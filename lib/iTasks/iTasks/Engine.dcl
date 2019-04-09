definition module iTasks.Engine
/**
* This module provides the iTasks engine.
* This is the primary function that creates the complete
* environment in which tasks can be executed.
*/

from Data.Maybe      import :: Maybe
from Data.Error      import :: MaybeError
from System.FilePath import :: FilePath
from System.Time     import :: Timespec
from Internet.HTTP   import :: HTTPRequest

import iTasks.WF.Definition

/*
* There are two ways tasks can be started:
* Interactively when a user requests it through the web,
* or directly when the application (server) is started,
*/
:: StartableTask
  = WebTask !WebTask
  | StartupTask !StartupTask

:: WebTask =
	{ path :: !String
	, task :: !WebTaskWrapper
	}

:: StartupTask =
	{ attributes :: !TaskAttributes 
	, task       :: !TaskWrapper
	}

:: WebTaskWrapper = E.a: WebTaskWrapper (HTTPRequest -> Task a) & iTask a
:: TaskWrapper = E.a: TaskWrapper (Task a) & iTask a

//Utility functions for creating collections of startable tasks
//Common cases:
onStartup :: (Task a) -> StartableTask | iTask a
onRequest :: String (Task a) -> StartableTask | iTask a

//Less common cases:
onStartupWithAttributes :: (Task a) TaskAttributes -> StartableTask | iTask a
onRequestFromRequest :: String (HTTPRequest -> Task a) -> StartableTask | iTask a

publish :== onRequestFromRequest //Backwards compatibility

class Startable a
where
	toStartable :: !a -> [StartableTask]

instance Startable (Task a) | iTask a //Default as web task
instance Startable (HTTPRequest -> Task a) | iTask a //As web task
instance Startable StartableTask
instance Startable [StartableTask]
instance Startable (a,b) | Startable a & Startable b


:: EngineOptions =
	{ appName       :: String
	, appPath       :: FilePath // Location of the application's executable
	, appVersion    :: String
	, serverPort    :: Int
	, serverUrl     :: String
	, keepaliveTime :: Timespec
	, sessionTime   :: Timespec
	, persistTasks  :: Bool
	, autoLayout    :: Bool
	, maxEvents     :: Int
	, timeout       :: Maybe Int // The timeout
	, distributed   :: Bool
	, sdsPort       :: Int
	, webDirPath    :: FilePath  // Location of public files that are served by the iTask webserver
	, storeDirPath  :: FilePath  // Location of the application's persistent data files 
	, tempDirPath   :: FilePath  // Location for temporary files used in tasks
	, saplDirPath   :: FilePath  // Location of the application's sapl files (client-side code)
	}

/**
* Executes the task framework with a collection of startable task definitions.
*
* @param Tasks to start
* @param The world
* @return The world
*/
doTasks :: a !*World -> *World | Startable a
startEngine :== doTasks //Backwards compatibility

/**
* Starts the task engine with options and a list of published task definitions.
*
* @param Tasks to start
* @param An initialization function to set the engine options with:
      @param The command line arguments
      @param The default options
      @return When Ok the engine options the engine is not started,
              when Error, a message is printed to the console
* @param The world
* @return The world
*/
doTasksWithOptions :: ([String] EngineOptions -> MaybeError [String] EngineOptions) a !*World
	-> *World | Startable a

startEngineWithOptions :== doTasksWithOptions

/**
* The function that takes the 'standard' command line options of an itask engine and
* shows the default help and startup message
*
* Essentially: doTasks = doTasksWithOptions defaultEngineCLIOptions 

* @param The command line arguments
* @param The default options
* @return When Ok the engine options the engine is not started,
*         when Error, the message to printed to the console
*/
defaultEngineCLIOptions :: [String] EngineOptions -> MaybeError [String] EngineOptions 

/**
* Determines the default options for an application
*/
defaultEngineOptions :: !*World -> (!EngineOptions,!*World)
