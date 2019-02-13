implementation module iTasks.Extensions.Distributed._Evaluation

from iTasks.WF.Definition import :: Task(..), :: Event(ResetEvent), :: TaskEvalOpts, class iTask, :: TaskResult(..), :: TaskException, :: TaskValue(..), :: Stability, :: InstanceNo, :: TaskId
from iTasks.Internal.TaskState import :: TaskTree(TCInit,TCDestroy)
import iTasks.Internal.TaskEval
import iTasks.UI.Definition
from iTasks.WF.Combinators.Common import @!, @?, whileUnchanged, ||-
from iTasks.UI.Definition import :: UIType(UIEmpty)
from iTasks.Internal.IWorld import :: IWorld
import iTasks.SDS.Definition
import iTasks.Internal.SDS
from iTasks.SDS.Sources.System import currentTaskInstanceNo
from iTasks.UI.Definition import :: UIChange(..), :: UIChildChange(..), ui
from iTasks.Internal.Store import memoryStore, :: StoreName, :: StoreNamespace
from iTasks.WF.Tasks.SDS import get
from iTasks.SDS.Combinators.Common import sdsFocus
from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenEq import generic gEq

from iTasks.WF.Combinators.Overloaded import class TMonad(..), instance TMonad Task, instance Functor Task, instance TApplicative Task, class TApplicative
from Data.Functor import class Functor

from Data.Map import :: Map, newMap
from Data.Maybe import :: Maybe(..)
from Data.Error import :: MaybeError(..)
from StdFunc import const
from StdString import instance toString Int, instance +++ {#Char}
import StdOverloaded

evalRemoteTask :: (Task a) ((TaskValue a) -> Task ()) -> Task a | iTask a
evalRemoteTask task handleValue
	= get currentTaskInstanceNo
	>>= \taskid -> let share = taskValueShare taskid in
		(customEval share task ||- whileUnchanged share (changeTask handleValue))
where
	changeTask :: ((TaskValue a) -> Task ()) (TaskValue a) -> Task a | iTask a
	changeTask handleValue value=:(Value v True)
		= handleValue value @! v
	changeTask handleValue value
		= handleValue value @? const NoValue

proxyTask :: (Shared sds (TaskValue a)) (*IWorld -> *IWorld) -> (Task a) | iTask a & RWShared sds
proxyTask value_share onDestroy = Task (eval value_share)
where
    eval :: (Shared sds (TaskValue a)) Event TaskEvalOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld) | iTask a & RWShared sds
    eval value_share event evalOpts tree=:(TCInit taskId ts) iworld
            # (val,iworld)  = readRegister taskId value_share iworld
            = case val of
                  Ok (ReadingDone val)            = (ValueResult val {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes=newMap} (rep event) tree, iworld)
                  Error e           = (ExceptionResult e,iworld)
    eval value_share event repAs (TCDestroy _) iworld
            # iworld = onDestroy iworld
            = (DestroyedResult,iworld)

    rep ResetEvent = ReplaceUI (ui UIEmpty)
    rep _          = NoChange

taskValueShare :: Int ->  SimpleSDSLens (TaskValue a) | iTask a
taskValueShare taskid = sdsFocus store_name (memoryStore store_name (Just NoValue))
where
	store_name = "taskValueShare_" +++ (toString taskid)

customEval :: (Shared sds (TaskValue a)) (Task a) -> (Task a) | iTask a & RWShared sds
customEval value_share (Task eval) = Task eval`
        where
        eval` event evalOpts state iworld
                = case eval event evalOpts state iworld of
                        v=:(ValueResult value info rep tree, iworld) -> storeValue v
                        (ExceptionResult te, iworld) -> (ExceptionResult te, iworld)
                        (DestroyedResult, iworld) -> (DestroyedResult, iworld)

        storeValue (ValueResult task_value info rep tree, iworld)
                # (res, iworld) = write task_value value_share EmptyContext iworld
                = case res of
                        Ok _    = (ValueResult task_value info rep tree, iworld)
                        Error _ = (ValueResult task_value info rep tree, iworld)

