definition module iTasks.Internal.TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from iTasks.WF.Definition           import :: Task, :: TaskResult, :: TaskException, :: TaskValue, :: TaskAttributes, :: Event, :: TaskId, :: InstanceNo
from iTasks.WF.Combinators.Core     import :: TaskListItem
from iTasks.Internal.IWorld		import :: IWorld
import iTasks.Internal.SDS
from iTasks.Internal.TaskState import :: DeferredJSON
from Text.GenJSON import :: JSONNode
from Data.Maybe import :: Maybe
from Data.Map import :: Map
from Data.Error import :: MaybeErrorString, :: MaybeError
from Data.CircularStack import :: CircularStack

//* External evaluation passed to the task under execution
:: TaskEvalOpts	=
	{ noUI     :: !Bool
	//* Whether to generate a UI
	, taskId   :: !TaskId
	//* The id of the task
	, lastEval :: !TaskTime
	//* The last evaluation
	}

mkEvalOpts :: TaskEvalOpts

//* External information passed from the task
:: TaskEvalInfo =
	{ lastEvent    :: !TaskTime	        //When was the last event in this task
    , removedTasks :: ![(TaskId,TaskId)]   //Which embedded parallel tasks were removed (listId,taskId)
	}

:: TaskTime :== Int

/**
 * Get the next TaskId
 */
getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)

/**
* Dequeues events from the event queue and evaluates the tasks instances
* @param Maximum amount of events to process at once
*/
processEvents :: !Int *IWorld -> *(!MaybeError TaskException (), !*IWorld)

/**
* Evaluate a task instance
*
* @param The instance id
* @param The event to process
* @param The IWorld state
*
* @return The result of the targeted main task or an error
* @return The IWorld state
*/
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (TaskValue DeferredJSON),!*IWorld)

//Update the I/O information for task instances
updateInstanceLastIO        ::          ![InstanceNo]       !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceConnect       :: !String  ![InstanceNo]       !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceDisconnect    ::          ![InstanceNo]       !*IWorld -> *(!MaybeError TaskException (), !*IWorld)

//Shares providing access to the evaluation information (constants from an evaluation point of view)
currentInstanceShare :: SDSSource () InstanceNo ()
