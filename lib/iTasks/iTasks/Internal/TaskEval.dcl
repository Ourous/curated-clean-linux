definition module iTasks.Internal.TaskEval
/**
* This module provides functions for creation, evaluation and removal of task/workflow instances.
*/

from iTasks.WF.Definition           import :: Task, :: TaskResult, :: TaskException, :: TaskValue, :: Event, :: TaskId, :: InstanceNo
from iTasks.WF.Combinators.Core     import :: TaskListItem
from iTasks.Internal.IWorld		import :: IWorld
from iTasks.Internal.SDS          import :: SDS, :: Shared, :: ReadOnlyShared
from iTasks.Internal.Tonic        import :: ExprId
from iTasks.Internal.TaskState import :: DeferredJSON
from Text.GenJSON import :: JSONNode
from Data.Maybe         import :: Maybe
from Data.Error import :: MaybeErrorString, :: MaybeError
from Data.CircularStack import :: CircularStack

//Extra types used during evaluation

//Additional options to pass down the tree when evaluating a task
:: TaskEvalOpts	=
	{ noUI              :: Bool
    , tonicOpts         :: TonicOpts
	}

:: TonicOpts =
  { inAssignNode            :: Maybe ExprId
  , inParallel              :: Maybe TaskId
  , captureParallel         :: Bool
  , currBlueprintModuleName :: String
  , currBlueprintFuncName   :: String
  , currBlueprintTaskId     :: TaskId
  , currBlueprintExprId     :: ExprId
  , callTrace               :: CircularStack TaskId
  }

mkEvalOpts :: TaskEvalOpts
defaultTonicOpts :: TonicOpts

//Additional information passed up from the tree when evaluating a task
:: TaskEvalInfo =
	{ lastEvent			:: !TaskTime	        //When was the last edit, action or focus event in this task
    , removedTasks      :: ![(TaskId,TaskId)]   //Which embedded parallel tasks were removed (listId,taskId)
	, refreshSensitive	:: !Bool		        //Can refresh events change the value or ui of this task (e.g. because shared data is read)
	}

:: TaskTime			:== Int

/**
* Extend the call trace with the current task number
*/
extendCallTrace :: !TaskId !TaskEvalOpts -> TaskEvalOpts

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
currentInstanceShare        :: ReadOnlyShared InstanceNo
