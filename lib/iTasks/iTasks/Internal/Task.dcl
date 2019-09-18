definition module iTasks.Internal.Task
/**
* This module provides types for the definition of tasks.
*/

import iTasks.WF.Definition
from iTasks.WF.Tasks.IO import :: ConnectionHandlers

import  iTasks.SDS.Definition
from iTasks.UI.Definition import :: UIChange
from iTasks.Internal.IWorld import :: ConnectionId
from Data.Map			import :: Map
from Data.Maybe         import :: Maybe
from Data.CircularStack import :: CircularStack
from Data.Error         import :: MaybeError, :: MaybeErrorString
from System.OSError		import :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage

derive JSONEncode		Task
derive JSONDecode		Task
derive gDefault			Task
derive gText	        Task
derive gEditor			Task
derive gEq				Task

//Low-level tasks that handle network connections
:: ConnectionTask = ConnectionTask !(ConnectionHandlersIWorld Dynamic Dynamic Dynamic) !(SDSLens () Dynamic Dynamic)

//Version of connection handlers with IWorld side-effects that is still necessary for built-in framework handlers
:: ConnectionHandlersIWorld l r w =
	{ onConnect     :: !(ConnectionId String r   *IWorld -> *(MaybeErrorString l, Maybe w, [String], Bool, *IWorld))
	, onData        :: !(             String l r *IWorld -> *(MaybeErrorString l, Maybe w, [String], Bool, *IWorld))
	, onShareChange :: !(                    l r *IWorld -> *(MaybeErrorString l, Maybe w, [String], Bool, *IWorld))
	, onTick        :: !(                    l r *IWorld -> *(MaybeErrorString l, Maybe w, [String], Bool, *IWorld))
	, onDisconnect  :: !(                    l r *IWorld -> *(MaybeErrorString l, Maybe w,                 *IWorld))
	, onDestroy     :: !(                    l   *IWorld -> *(MaybeErrorString l,          [String],       *IWorld))
	}

/**
* Wraps a set of connection handlers and a shared source as a connection task
*/
wrapConnectionTask :: (ConnectionHandlers l r w) (sds () r w) -> ConnectionTask | TC l & TC r & TC w & RWShared sds
wrapIWorldConnectionTask :: (ConnectionHandlersIWorld l r w) (sds () r w) -> ConnectionTask | TC l & TC r & TC w & RWShared sds

/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (MaybeError TaskException a,*IWorld)) -> Task a | iTask a

/**
 * Apply a function on the task continuation of the task result
 * @type ((Task a) -> (Event TaskEvalOpts !*IWorld -> *(TaskResult a, !*IWorld))) !(TaskResult a) -> TaskResult a
 */
wrapTaskContinuation tf val :== case val of
	(ValueResult val tei ui newtask) = ValueResult val tei ui (Task (tf newtask))
	a = a

/**
 * Unwrap the task to reveal the evaluation function
 * @type (Task a) -> (Event TaskEvalOpts !*IWorld -> *(TaskResult a, !*IWorld))
 */
unTask (Task t) :== t

nopTask :: Task a
