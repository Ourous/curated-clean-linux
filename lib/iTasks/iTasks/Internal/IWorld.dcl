definition module iTasks.Internal.IWorld

from System.FilePath		import :: FilePath
from Data.Map				import :: Map
from Data.Maybe				import :: Maybe
from Data.Error 			import :: MaybeError(..), :: MaybeErrorString(..)
from Data.Set               import :: Set
from Data.Queue             import :: Queue
from StdFile			                import class FileSystem		
from System.Time				        import :: Timestamp, :: Timespec
from Text.GenJSON				            import :: JSONNode
from iTasks.Engine                      import :: EngineOptions
from iTasks.UI.Definition				import :: UI, :: UIType
from iTasks.Internal.TaskState		import :: ParallelTaskState, :: TIMeta, :: DeferredJSON
from iTasks.Internal.Task             import :: ConnectionTask, :: BackgroundTask
from iTasks.Internal.TaskEval         import :: TaskTime

from iTasks.WF.Definition import :: TaskValue, :: Event, :: TaskId, :: InstanceNo, :: TaskNo
from iTasks.WF.Combinators.Core import :: ParallelTaskType, :: TaskListItem 
from iTasks.SDS.Definition import :: SDS, :: RWShared, :: ReadWriteShared, :: Shared, :: ReadOnlyShared
from iTasks.Internal.SDS import :: SDSNotifyRequest, :: JSONShared, :: DeferredWrite, :: SDSIdentity
from iTasks.Extensions.DateTime import :: Time, :: Date, :: DateTime

from Sapl.Linker.LazyLinker import :: LoaderState
from Sapl.Linker.SaplLinkerShared import :: LineType, :: FuncTypeMap
from Sapl.Target.Flavour import :: Flavour
from Sapl.SaplParser import :: ParserState
from TCPIP import :: TCP_Listener, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_, :: TCP_DuplexChannel, :: DuplexChannel, :: IPAddress, :: ByteSeq

CLEAN_HOME_VAR	:== "CLEAN_HOME"

:: *IWorld		=	{ options               :: !EngineOptions                                   // Engine configuration
                    , clock                 :: !Timespec                                        // Server side clock
                    , current               :: !TaskEvalState                                   // Shared state during task evaluation

                    , random                :: [Int]                                            // Infinite random stream

                    , sdsNotifyRequests     :: !Map SDSIdentity (Map SDSNotifyRequest Timespec) // Notification requests from previously read sds's
                    , sdsNotifyReqsByTask   :: !Map TaskId (Set SDSIdentity)                    // Allows to efficiently find notification by taskID for clearing notifications
                    , memoryShares          :: !Map String Dynamic                              // Run-time memory shares
                    , readCache             :: !Map (String,String) Dynamic                     // Cached share reads
                    , writeCache            :: !Map (String,String) (Dynamic,DeferredWrite)     // Cached deferred writes
					, exposedShares			:: !Map String (Dynamic, JSONShared)                // Shared source
					, jsCompilerState 		:: !Maybe JSCompilerState 					        // Sapl to Javascript compiler state

	                , ioTasks               :: !*IOTasks                                        // The low-level input/output tasks
                    , ioStates              :: !IOStates                                        // Results of low-level io tasks, indexed by the high-level taskid that it is linked to

					, world					:: !*World									        // The outside world

                    //Experimental database connection cache
                    , resources             :: *[*Resource]
                    , onClient				:: !Bool									// "False" on the server, "True" on the client
					, shutdown				:: !Maybe Int                               // Signals the server function to shut down, the int will be set as exit code
					}

:: JSCompilerState =
	{ loaderState 			:: !LoaderState							// State of the lazy loader
	, functionMap 			:: !FuncTypeMap 						// Function name -> source code mapping
 	, flavour 				:: !Flavour 							// Clean flavour for JS compilation
	, parserState 			:: !Maybe ParserState 					// Some information collected by the parser for the code generator
	, skipMap 				:: !Map InstanceNo (Set String) 		// Per client information of the names of the already generated functions
	}

:: TaskEvalState =
    { taskTime				 :: !TaskTime							// The 'virtual' time for the task. Increments at every event
	, taskInstance		     :: !InstanceNo							// The current evaluated task instance
    , sessionInstance        :: !Maybe InstanceNo                   // If we are evaluating a task in response to an event from a session
    , attachmentChain        :: ![TaskId]                           // The current way the evaluated task instance is attached to other instances
    , nextTaskNo			 :: !TaskNo								// The next task number to assign
    }

:: *IOTasks =
    { done :: !*[IOTaskInstance]
    , todo :: !*[IOTaskInstance]
    }

:: *IOTaskInstance
    = ListenerInstance        !ListenerInstanceOpts !*TCP_Listener
    | ConnectionInstance      !ConnectionInstanceOpts !*TCP_DuplexChannel
    | BackgroundInstance      !BackgroundInstanceOpts !BackgroundTask

:: ListenerInstanceOpts =
    { taskId                :: !TaskId          //Reference to the task that created the listener
    , nextConnectionId      :: !ConnectionId    
    , port                  :: !Int
    , connectionTask        :: !ConnectionTask
    , removeOnClose         :: !Bool            //If this flag is set, states of connections accepted by this listener are removed when the connection is closed
    }

:: ConnectionInstanceOpts =
    { taskId                :: !TaskId          //Reference to the task that created the connection
    , connectionId          :: !ConnectionId    //Unique connection id (per listener/outgoing connection)
    , remoteHost            :: !IPAddress      
    , connectionTask        :: !ConnectionTask  //The io task definition that defines how the connection is handled
    , removeOnClose         :: !Bool            //If this flag is set, the connection state is removed when the connection is closed
    }

:: ConnectionId             :== Int

:: BackgroundInstanceOpts =
    { bgInstId              :: !BackgroundTaskId
    }

:: BackgroundTaskId         :== Int

:: IOStates :== Map TaskId IOState
:: IOState
    = IOActive      !(Map ConnectionId (!Dynamic,!Bool)) // Bool: stability
    | IODestroyed   !(Map ConnectionId (!Dynamic,!Bool)) // Bool: stability
    | IOException   !String
:: IOConnectionState =
    { connectionTaskState   :: !Dynamic //The persisted local state of the connection task that handles the connection
    , closed                :: !Bool
    }

:: *Resource = Resource | .. //Extensible resource type for caching database connections etc...

//Creation and destruction of the iworld
/**
* Creates and initializes the IWorld state
*
* @param The engine options
* @param The world
*
* @return An initialized iworld
*/
createIWorld :: !EngineOptions !*World -> *IWorld

/**
* Initialize the SAPL->JS compiler state
* 
*/
initJSCompilerState :: *IWorld -> *(!MaybeErrorString (), !*IWorld)

/**
* Destroys the iworld state
*/
destroyIWorld :: !*IWorld -> *World

//Internally used clock share
// (UTC time can be derived from timestamp, local time requires *World to determine time zone)
:: ClockParameter a =
	{ start :: a
	, interval :: a
	}

iworldTimespec         :: SDS (ClockParameter Timespec) Timespec Timespec
/*
 * Calculate the next fire for the given timespec
 *
 * @param now
 * @param registration time
 * @param clock parameter
 * @result time to fire next
 */
iworldTimespecNextFire :: Timespec Timespec (ClockParameter Timespec) -> Timespec
iworldTimestamp        :: SDS (ClockParameter Timestamp) Timestamp Timestamp
iworldLocalDateTime    :: ReadOnlyShared DateTime

iworldLocalDateTime` :: !*IWorld -> (!DateTime, !*IWorld)

/*
 * Gives you possibly a matching resource while adhering to the uniqueness
 * constraints. Note that this does remove it from the IWorld
 *
 * @param Function that classifies the resource whether it matches
 */
iworldResource :: (*Resource -> (Bool, *Resource)) *IWorld -> (*[*Resource], *IWorld)

instance FileSystem IWorld
