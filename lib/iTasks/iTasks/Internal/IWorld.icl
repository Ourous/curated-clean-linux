implementation module iTasks.Internal.IWorld

import StdEnv
from StdFunc import seqList, :: St

from ABC.Interpreter import prepare_prelinked_interpretation, :: PrelinkedInterpretationEnvironment
from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

import Data.Func
import Data.Integer
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Maybe
import Math.Random
import System.CommandLine
import System.Directory
import System.File
import System.FilePath
import System.Signal

import iTasks.Engine
import iTasks.Extensions.DateTime
import iTasks.Internal.Task
import iTasks.Internal.TaskEval
import iTasks.Internal.Util
import iTasks.SDS.Combinators.Common
import iTasks.WF.Definition
import iTasks.WF.Derives

createIWorld :: !EngineOptions !*World -> Either (!String, !*World) *IWorld
createIWorld options world
	# (ts=:{tv_nsec=seed}, world) = nsTime world
	# (mbAbcEnv,           world) = prepare_prelinked_interpretation options.byteCodePath world
	= case mbAbcEnv of
		Just abcEnv = Right
			{IWorld
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
			,abcInterpreterEnv    = abcEnv
			,shutdown             = Nothing
			,ioTasks              = {done = [], todo = []}
			,ioStates             = 'DM'.newMap
			,world                = world
			,signalHandlers       = []
			,resources            = []
			,random               = genRandInt seed
			,onClient             = False
			}
		Nothing =
			Left ("Failed to parse bytecode, is ByteCode set in the project file?", world)

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

iworldTimespec :: SDSSource (ClockParameter Timespec) Timespec Timespec
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

iworldTimestamp :: SDSLens (ClockParameter Timestamp) Timestamp Timestamp
iworldTimestamp = mapReadWrite (timespecToStamp, \w r. Just (timestampToSpec w)) (Just \_ s. Ok (timespecToStamp s)) 
	$ sdsTranslate "iworldTimestamp translation" (\{start,interval}->{start=timestampToSpec start,interval=timestampToSpec interval}) iworldTimespec

iworldLocalDateTime :: SDSParallel () DateTime ()
iworldLocalDateTime = SDSParallel (createReadOnlySDS \_ -> iworldLocalDateTime`) (sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp) sdsPar
where
    // ignore value, but use notifications for 'iworldTimestamp'
    sdsPar = { SDSParallelOptions
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

instance FileEnv IWorld
where
	accFiles accfun iworld=:{IWorld|world}
		# (x, world) = accFiles accfun world
		= (x, {IWorld | iworld & world=world})
	appFiles appfun iworld=:{IWorld|world}
		# world = appFiles appfun world
		= {IWorld | iworld & world=world}
