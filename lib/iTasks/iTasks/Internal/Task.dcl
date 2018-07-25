definition module iTasks.Internal.Task
/**
* This module provides types for the definition of tasks.
*/

import iTasks.WF.Definition
from iTasks.Internal.Tonic.AbsSyn import :: ExprId (..)
from iTasks.WF.Tasks.IO import :: ConnectionHandlers

from iTasks.Internal.TaskState			import :: TaskTree
from iTasks.SDS.Definition import :: SDS, :: RWShared
from iTasks.UI.Definition import :: UIChange
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
:: ConnectionTask = ConnectionTask !(ConnectionHandlersIWorld Dynamic Dynamic Dynamic) !(RWShared () Dynamic Dynamic)

//Definition of low-level network interaction
/*
:: ConnectionHandlers l r w = 
    { onConnect         :: !(String r   -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onData            :: !(String l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onShareChange     :: !(       l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onDisconnect      :: !(       l r -> (!MaybeErrorString l, Maybe w                  ))
	}
*/
//Version of connection handlers with IWorld side-effects that is still necessary for built-in framework handlers
:: ConnectionHandlersIWorld l r w =
    { onConnect     :: !(String r   *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onData        :: !(String l r *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onShareChange :: !(       l r *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onTick        :: !(       l r *IWorld -> *(!MaybeErrorString l, Maybe w, ![String], !Bool, !*IWorld))
    , onDisconnect  :: !(       l r *IWorld -> *(!MaybeErrorString l, Maybe w,                   !*IWorld))
    }

//Background computation tasks
:: BackgroundTask = BackgroundTask !(*IWorld -> *(!MaybeError TaskException (), !*IWorld))

/**
* Wraps a set of connection handlers and a shared source as a connection task
*/
wrapConnectionTask :: (ConnectionHandlers l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w
wrapIWorldConnectionTask :: (ConnectionHandlersIWorld l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w


/**
* Create a task that finishes instantly
*/
mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a

