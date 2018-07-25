implementation module iTasks.Internal.IWorld

from System.FilePath				import :: FilePath
from Data.Map						import :: Map
from Data.Maybe						import :: Maybe
from Data.Error 					import :: MaybeError(..), :: MaybeErrorString(..)
from System.Time					import :: Timestamp, time, :: Timespec
from Text.GenJSON						import :: JSONNode
from iTasks.WF.Definition           import :: TaskId, :: InstanceNo, :: TaskNo 
from iTasks.WF.Combinators.Core     import :: TaskListItem, :: ParallelTaskType
from iTasks.Extensions.DateTime     import :: Time, :: Date, :: DateTime, toTime, toDate
from iTasks.Internal.TaskEval       import :: TaskTime
from iTasks.Engine                  import :: EngineOptions(..)
import Data.Integer

import iTasks.SDS.Combinators.Common

from StdFile import class FileSystem(..)
from StdFile import instance FileSystem World
from StdFunc import const, o, seqList, :: St
from StdMisc import abort
from StdOrdList import sortBy

from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

import System.Time, StdList, Text.Encodings.Base64, _SystemArray, StdBool, StdTuple, Text.GenJSON, Data.Error, Math.Random
import iTasks.Internal.TaskStore, iTasks.Internal.Util
import iTasks.Internal.Serialization
import iTasks.Internal.SDS
import qualified Data.Map as DM
import Data.Func, Data.Tuple, Data.List, iTasks.SDS.Definition


import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory

from Data.Set import :: Set, newSet
from Sapl.Linker.LazyLinker import generateLoaderState, :: LoaderStateExt, :: LoaderState, :: FuncTypeMap, :: LineType
from Sapl.Linker.SaplLinkerShared import :: SkipSet
from Sapl.Target.Flavour import :: Flavour, toFlavour
from Sapl.Target.CleanFlavour import cleanFlavour
from Sapl.SaplParser import :: ParserState

//The following modules are excluded by the SAPL -> Javascript compiler
//because they contain functions implemented in ABC code that cannot
//be compiled to javascript anyway. Handwritten Javascript overrides need
//to be provided for them.
JS_COMPILER_EXCLUDES :==
	["iTasks.Internal.Client.Override"
	,"dynamic_string"
	,"graph_to_string_with_descriptors"
	,"graph_to_sapl_string"
	,"Text.Encodings.Base64"
	,"Sapl.LazyLinker"
	,"Sapl.Target.JS.CodeGeneratorJS"
	,"System.Pointer"
	,"System.File"
	,"System.Directory"
	]

createIWorld :: !EngineOptions !*World -> *IWorld
createIWorld options world
	# (ts=:{tv_nsec=seed}, world)	= nsTime world
	= {IWorld
	  |options = options 
      ,clock = ts
      ,current =
	    {TaskEvalState
        |taskTime				= 0
	    ,taskInstance		    = 0
        ,sessionInstance        = Nothing
        ,attachmentChain        = []
	    ,nextTaskNo			    = 0
        }
      ,sdsNotifyRequests    = 'DM'.newMap
      ,sdsNotifyReqsByTask  = 'DM'.newMap
      ,memoryShares         = 'DM'.newMap
      ,readCache            = 'DM'.newMap
      ,writeCache           = 'DM'.newMap
	  ,exposedShares		= 'DM'.newMap
	  ,jsCompilerState		= Nothing
	  ,shutdown				= Nothing
      ,ioTasks              = {done = [], todo = []}
      ,ioStates             = 'DM'.newMap
	  ,world				= world
      ,resources            = []
      ,random               = genRandInt seed
      ,onClient				= False
	  }

initJSCompilerState :: *IWorld -> *(!MaybeErrorString (), !*IWorld)
initJSCompilerState iworld=:{IWorld|world,options={EngineOptions|saplDirPath}}
	# ((lst, ftmap, _), world)  = generateLoaderState [saplDirPath] [] JS_COMPILER_EXCLUDES world
    # jsCompilerState = { loaderState = lst, functionMap = ftmap, flavour = cleanFlavour, parserState = Nothing, skipMap = 'DM'.newMap}
    = (Ok (), {iworld & jsCompilerState = Just jsCompilerState, world = world})

// Determines the server executables path
determineAppPath :: !*World -> (!FilePath, !*World)
determineAppPath world
	# ([arg:_],world) = getCommandLine world 
	| dropDirectory arg <> "ConsoleClient.exe"	= toCanonicalPath arg world
	//Using dynamic linker:	
	# (res, world)				= getCurrentDirectory world	
	| isError res				= abort "Cannot get current directory."	
	# currentDirectory			= fromOk res
	# (res, world)				= readDirectory currentDirectory world	
	| isError res				= abort "Cannot read current directory."	
	# batchfiles				= [f \\ f <- fromOk res | takeExtension f == "bat" ]
	| isEmpty batchfiles		= abort "No dynamic linker batch file found."	
	# (infos, world)			= seqList (map getFileInfo batchfiles) world	
	| any isError infos	 		= abort "Cannot get file information."	
	= (currentDirectory </> (fst o hd o sortBy cmpFileTime) (zip2 batchfiles infos), world)	
	where		
		cmpFileTime (_,Ok {FileInfo | lastModifiedTime = x})
					(_,Ok {FileInfo | lastModifiedTime = y}) = timeGm x > timeGm y

destroyIWorld :: !*IWorld -> *World
destroyIWorld iworld=:{IWorld|world} = world

iworldTimespec :: SDS (ClockParameter Timespec) Timespec Timespec
iworldTimespec = createReadWriteSDS "IWorld" "timespec" read write
where
    read _ iworld=:{IWorld|clock} = (Ok clock,iworld)
    write _ timestamp iworld = (Ok pred, {iworld & clock = timestamp})
	where
		pred reg p=:{start,interval}
			| timestamp < start = False // Start time has not passed
			= timestamp > iworldTimespecNextFire timestamp reg p

iworldTimespecNextFire :: Timespec Timespec (ClockParameter Timespec) -> Timespec
iworldTimespecNextFire now reg {start,interval}
	| interval == zero = now
	# start = toI start
	  interval = toI interval
	  reg = toI reg
	  passed = reg - start
	= toT (start + ((passed / interval + one) * interval))
where
	toI x = toInteger x.tv_sec * toInteger 1000000000 + toInteger x.tv_nsec
	toT x = {tv_sec=toInt (x/toInteger 1000000000), tv_nsec=toInt (x rem toInteger 1000000000)}

iworldTimestamp :: SDS (ClockParameter Timestamp) Timestamp Timestamp
iworldTimestamp = mapReadWrite (timespecToStamp, const o Just o timestampToSpec)
	$ sdsTranslate "iworldTimestamp translation" (\{start,interval}->{start=timestampToSpec start,interval=timestampToSpec interval}) iworldTimespec

iworldLocalDateTime :: ReadOnlyShared DateTime
iworldLocalDateTime = SDSParallel (createReadOnlySDS \_ -> iworldLocalDateTime`) (sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp) sdsPar
where
    // ignore value, but use notifications for 'iworldTimestamp'
    sdsPar = { SDSParallel
             | name   = "iworldLocalDateTime"
             , param  = \p -> (p,p)
             , read   = fst
             , writel = SDSWriteConst \_ _ -> Ok Nothing
             , writer = SDSWriteConst \_ _ -> Ok Nothing
             }

iworldLocalDateTime` :: !*IWorld -> (!DateTime, !*IWorld)
iworldLocalDateTime` iworld=:{clock={tv_sec}, world}
    # (tm, world) = toLocalTime (Timestamp tv_sec) world
    = (tmToDateTime tm, {iworld & world = world})

iworldResource :: (*Resource -> (Bool, *Resource)) *IWorld -> (*[*Resource], *IWorld)
iworldResource f iworld=:{IWorld|resources}
# (matches, resources) = splitWithUnique f resources
= (matches, {iworld & resources=resources})
where
	splitWithUnique f [] = ([], [])
	splitWithUnique f [r:rs]
	# (ok, r) = f r
	| ok = let (ms, xs) = splitWithUnique f rs in ([r:ms], xs)
	= let (ms, xs) = splitWithUnique f rs in (ms, [r:xs])

//Wrapper instance for file access
instance FileSystem IWorld
where
	fopen filename mode iworld=:{IWorld|world}
		# (ok,file,world) = fopen filename mode world
		= (ok,file,{IWorld|iworld & world = world})
	fclose file iworld=:{IWorld|world}
		# (ok,world) = fclose file world
		= (ok,{IWorld|iworld & world = world})
	stdio iworld=:{IWorld|world}
		# (io,world) = stdio world
		= (io,{IWorld|iworld & world = world})
	sfopen filename mode iworld=:{IWorld|world}
		# (ok,file,world) = sfopen filename mode world
		= (ok,file,{IWorld|iworld & world = world})
