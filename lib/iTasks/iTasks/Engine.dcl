definition module iTasks.Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which worfklow specifications can be executed.
*/

from Data.Maybe      import :: Maybe
from System.FilePath import :: FilePath
from Internet.HTTP   import :: HTTPRequest
from System.Time     import :: Timespec

import iTasks.WF.Definition

:: EngineOptions =
	{ appName 		:: String
	, appPath		:: FilePath // Location of the application's executable
	, appVersion    :: String
	, serverPort	:: Int
    , serverUrl     :: String
	, keepaliveTime :: Timespec
    , sessionTime   :: Timespec
    , persistTasks  :: Bool
	, autoLayout    :: Bool
	, timeout       :: Maybe Int // The timeout
	, webDirPath 	:: FilePath  // Location of public files that are served by the iTask webserver
	, storeDirPath 	:: FilePath  // Location of the application's persistent data files 
	, tempDirPath 	:: FilePath  // Location for temporary files used in tasks
	, saplDirPath   :: FilePath  // Location of the application's sapl files (client-side code)
	}

/**
* Starts the task engine with a list of published task definitions.
*
* @param Tasks to start
* @param The world
* @return The world
*/
startEngine :: a !*World -> *World | Publishable a

/**
* Starts the task engine with options and a list of published task definitions.
*
* @param Tasks to start
* @param An initialization function to set the engine options with:
      @param The command line arguments
      @param The default options
	  @return Maybe the engine options, in case of Nothing, the engine is not started
      @return A message that is printed to the console when the engine is started
* @param The world
* @return The world
*/
startEngineWithOptions :: ([String] EngineOptions -> (!Maybe EngineOptions,![String])) a !*World -> *World | Publishable a

/**
* The function that takes the 'standard' command line options of an itask engine and
* shows the default help and startup message
*
* Essentially: startEngine = startEngineWithOptions defaultEngineCLIOptions 

* @param The command line arguments
* @param The default options
* @return Maybe the engine options, in case of Nothing, the engine is not started
* @return A message that is printed to the console when the engine is started
*/
defaultEngineCLIOptions :: [String] EngineOptions -> (!Maybe EngineOptions,![String])

/**
* Determines the default options for an application
*/
defaultEngineOptions :: !*World -> (!EngineOptions,!*World)

/**
* Start a stripped task engine (without an HTTP server) with a list of tasks to be created 
*/
runTasks :: a !*World -> *World | Runnable a

runTasksWithOptions :: ([String] EngineOptions -> (!Maybe EngineOptions,![String])) a !*World -> *World | Runnable a


// === Wrapping interactive tasks for use with the builtin iTask webserver ===

:: PublishedTask =
	{ url			:: String
	, task			:: WebTaskWrapper
	}

:: WebTaskWrapper = E.a: WebTaskWrapper (HTTPRequest -> Task a) & iTask a
:: TaskWrapper = E.a: TaskWrapper (Task a) & iTask a

/**
* Wraps a task together with a url to make it publishable by the engine
*/
publish :: String (HTTPRequest -> Task a) -> PublishedTask | iTask a

class Publishable a
where
	publishAll :: !a -> [PublishedTask]

instance Publishable (Task a) | iTask a
instance Publishable (HTTPRequest -> Task a) | iTask a
instance Publishable [PublishedTask]

// === Wrapping non-interactive tasks for running on the command line ===

class Runnable a
where
	toRunnable :: !a -> [TaskWrapper] 

instance Runnable (Task a) | iTask a
instance Runnable [TaskWrapper]
