implementation module iTasks.Engine

import StdMisc, StdArray, StdList, StdOrdList, StdTuple, StdChar, StdFile, StdBool, StdEnum
import iTasks.WF.Combinators.Common
import iTasks.WF.Tasks.System
from StdFunc import o, seqList, ::St, const, id
from Data.Map import :: Map
from Data.Queue import :: Queue(..)
import qualified Data.Map as DM
import Data.List, Data.Error, Data.Func, Data.Tuple, Math.Random, Text 
import System.Time, System.CommandLine, System.Environment, System.OSError, System.File, System.FilePath, System.Directory
import iTasks.Internal.Util, iTasks.Internal.HtmlUtil
import iTasks.Internal.IWorld, iTasks.Internal.WebService, iTasks.Internal.SDSService
import qualified iTasks.Internal.SDS as SDS
import iTasks.UI.Layout, iTasks.UI.Layout.Default

from iTasks.WF.Combinators.Tune import class tune(..), instance tune ApplyLayout Task, :: ApplyLayout(..)
from iTasks.SDS.Combinators.Common import sdsFocus

import StdInt, StdChar, StdString
import tcp
import Internet.HTTP, System.Time, System.CommandLine, Data.Func

import iTasks.Internal.IWorld, iTasks.Internal.TaskEval, iTasks.Internal.TaskStore
import iTasks.Internal.Util
import iTasks.Internal.TaskServer
import iTasks.Internal.EngineTasks

from iTasks.Extensions.DateTime import toDate, toTime, instance == Date, instance == Time

from Data.Set import :: Set, newSet
from Sapl.Linker.LazyLinker import generateLoaderState, :: LoaderStateExt
from Sapl.Linker.SaplLinkerShared import :: SkipSet
from Sapl.Target.Flavour import :: Flavour, toFlavour

from System.OS import IF_POSIX_OR_WINDOWS
import System.GetOpt
import Data.Functor

MAX_EVENTS 		        :== 5

defaultEngineOptions :: !*World -> (!EngineOptions,!*World)
defaultEngineOptions world
	# (appPath,world)    = determineAppPath world	
	# (appVersion,world) = determineAppVersion appPath world
	# appDir             = takeDirectory appPath
	# appName            = (dropExtension o dropDirectory) appPath
	# options =	
		{ appName			= appName
		, appPath			= appPath
        , appVersion        = appVersion
		, serverPort		= IF_POSIX_OR_WINDOWS 8080 80
        , serverUrl         = "http://localhost/"
		, keepaliveTime     = {tv_sec=300,tv_nsec=0} // 5 minutes
		, sessionTime       = {tv_sec=60,tv_nsec=0}  // 1 minute, (the client pings every 10 seconds by default)
        , persistTasks      = False
		, autoLayout        = True
		, timeout			= Just 500
		, webDirPath 		= appDir </> appName +++ "-www"
		, storeDirPath      = appDir </> appName +++ "-data" </> "stores"
		, tempDirPath       = appDir </> appName +++ "-data" </> "tmp"
		, saplDirPath 	    = appDir </> appName +++ "-sapl"
		}
	= (options,world)

defaultEngineCLIOptions :: [String] EngineOptions -> (!Maybe EngineOptions,![String])
defaultEngineCLIOptions [argv0:argv] defaults
	# (settings, positionals, errs) = getOpt Permute opts argv
	| not (errs =: []) = (Nothing, errs)
	| not (positionals =: []) = (Nothing, ["Positional arguments not allowed"])
	= case foldl (o) id settings (Just defaults) of
		Nothing = (Nothing, [usageInfo ("Usage " +++ argv0 +++ "[OPTIONS]") opts])
		Just settings = (Just settings,
			["*** " +++ settings.appName +++ " HTTP server ***"
			,""
			,"Running at http://localhost" +++ if (settings.serverPort == 80) "/" (":" +++ toString settings.serverPort +++ "/")])
where
	opts :: [OptDescr ((Maybe EngineOptions) -> Maybe EngineOptions)]
	opts =
		[ Option ['?'] ["help"] (NoArg $ const Nothing)
			"Display this message"
		, Option ['p'] ["port"] (ReqArg (\p->fmap \o->{o & serverPort=toInt p}) "PORT")
			("Specify the HTTP port (default: " +++ toString defaults.serverPort)
		, Option [] ["timeout"] (OptArg (\mp->fmap \o->{o & timeout=fmap toInt mp}) "MILLISECONDS")
			"Specify the timeout in ms (default: 500)\nIf not given, use an indefinite timeout."
		, Option [] ["keepalive"] (ReqArg (\p->fmap \o->{o & keepaliveTime={tv_sec=toInt p,tv_nsec=0}}) "SECONDS")
			"Specify the keepalive time in seconds (default: 300)"
		, Option [] ["sessiontime"] (ReqArg (\p->fmap \o->{o & sessionTime={tv_sec=toInt p,tv_nsec=0}}) "SECONDS")
			"Specify the expiry time for a session in seconds (default: 60)"
		, Option [] ["autolayout"] (NoArg (fmap \o->{o & autoLayout=True}))
			"Enable autolayouting (default)"
		, Option [] ["no-autolayout"] (NoArg (fmap \o->{o & autoLayout=False}))
			"Disable autolayouting"
		, Option [] ["persist-tasks"] (NoArg (fmap \o->{o & persistTasks=True}))
			"Enable the persistence of tasks"
		, Option [] ["no-persist-tasks"] (NoArg (fmap \o->{o & persistTasks=False}))
			"Disable the persistence of tasks (default)"
		, Option [] ["webdir"] (ReqArg (\p->fmap \o->{o & webDirPath=p}) "PATH")
			("Specify the folder containing static web content\ndefault: " +++ defaults.webDirPath)
		, Option [] ["storedir"] (ReqArg (\p->fmap \o->{o & storeDirPath=p}) "PATH")
			("Specify the folder containing the data stores\ndefault: " +++ defaults.storeDirPath)
		, Option [] ["tempdir"] (ReqArg (\p->fmap \o->{o & tempDirPath=p}) "PATH")
			("Specify the folder containing the temporary files\ndefault: " +++ defaults.tempDirPath)
		, Option [] ["sapldir"] (ReqArg (\p->fmap \o->{o & saplDirPath=p}) "PATH")
			("Specify the folder containing the sapl files\ndefault: " +++ defaults.saplDirPath)
		]

startEngine :: a !*World -> *World | Publishable a
startEngine publishable world = startEngineWithOptions defaultEngineCLIOptions publishable world

startEngineWithOptions :: ([String] EngineOptions -> (!Maybe EngineOptions,![String])) a !*World -> *World | Publishable a
startEngineWithOptions initFun publishable world
	# (cli,world)			= getCommandLine world
	# (options,world)       = defaultEngineOptions world
	# (mbOptions,msg)       = initFun cli options
	# world                 = show msg world
	= case mbOptions of
		Nothing = world
		Just options
 			# iworld				= createIWorld (fromJust mbOptions) world
 			# (res,iworld) 			= initJSCompilerState iworld
		 	| res =:(Error _) 		= show ["Fatal error: " +++ fromError res] (destroyIWorld iworld)
			# iworld				= serve [] (tcpTasks options.serverPort options.keepaliveTime) engineTasks (timeout options.timeout) iworld
			= destroyIWorld iworld
where
	tcpTasks serverPort keepaliveTime = [(serverPort,httpServer serverPort keepaliveTime (engineWebService publishable) taskOutput)]
	engineTasks =
 		[BackgroundTask updateClock
		,BackgroundTask (processEvents MAX_EVENTS)
		,BackgroundTask removeOutdatedSessions
		,BackgroundTask flushWritesWhenIdle]

runTasks :: a !*World -> *World | Runnable a
runTasks tasks world = runTasksWithOptions (\c o -> (Just o,[])) tasks world

runTasksWithOptions :: ([String] EngineOptions -> (!Maybe EngineOptions,![String])) a !*World -> *World | Runnable a
runTasksWithOptions initFun runnable world
	# (cli,world)			= getCommandLine world
	# (options,world)       = defaultEngineOptions world
	# (mbOptions,msg)       = initFun cli options
	# world                 = show msg world
	| mbOptions =: Nothing  = world
	# (Just options)		= mbOptions
 	# iworld				= createIWorld options world
 	# (res,iworld) 			= initJSCompilerState iworld
 	| res =:(Error _) 		= show ["Fatal error: " +++ fromError res] (destroyIWorld iworld)
	# iworld				= serve (toRunnable runnable) [] systemTasks (timeout options.timeout) iworld
	= destroyIWorld iworld
where
	systemTasks =
 		[BackgroundTask updateClock
		,BackgroundTask (processEvents MAX_EVENTS)
		,BackgroundTask stopOnStable]

show :: ![String] !*World -> *World
show lines world
	# (console,world)	= stdio world
	# console			= seqSt (\s c -> fwrites (s +++ "\n") c) lines console
	# (_,world)			= fclose console world
	= world

// The iTasks engine consist of a set of HTTP WebService 
engineWebService :: publish -> [WebService (Map InstanceNo TaskOutput) (Map InstanceNo TaskOutput)] | Publishable publish
engineWebService publishable = [taskUIService published, documentService, sdsService, staticResourceService [url \\ {PublishedTask|url} <- published]]
where
	published = publishAll publishable 

publish :: String (HTTPRequest -> Task a) -> PublishedTask | iTask a
publish url task = {url = url, task = WebTaskWrapper task}

instance Publishable (Task a) | iTask a
where
	publishAll task = [publish "/" (const task)]

instance Publishable (HTTPRequest -> Task a) | iTask a
where
	publishAll task = [publish "/" task]
	
instance Publishable [PublishedTask]
where
	publishAll list = list

class Runnable a
where
	toRunnable :: !a -> [TaskWrapper] 

instance Runnable (Task a) | iTask a
where
	toRunnable task = [TaskWrapper task]

instance Runnable [TaskWrapper]
where
	toRunnable list = list

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

//By default, we use the modification time of the applaction executable as version id
determineAppVersion :: !FilePath!*World -> (!String,!*World)	
determineAppVersion appPath world
	# (res,world)       = getFileInfo appPath world
	| res =: (Error _)  = ("unknown",world) 
	# tm				= (fromOk res).lastModifiedTime
	# version           = strfTime "%Y%m%d-%H%M%S" tm
	= (version,world)

