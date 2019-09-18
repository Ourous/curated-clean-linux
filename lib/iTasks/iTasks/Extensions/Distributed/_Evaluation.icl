implementation module iTasks.Extensions.Distributed._Evaluation

import Data.Error

import iTasks.Internal.SDS
import iTasks.Internal.TaskEval
import iTasks.Internal.Util

import iTasks.SDS.Combinators.Common
import iTasks.SDS.Definition
import iTasks.SDS.Sources.Store
import iTasks.SDS.Sources.System
import iTasks.UI.Definition
import iTasks.WF.Combinators.Common
import iTasks.WF.Definition
import iTasks.WF.Tasks.SDS

evalRemoteTask :: (Task a) ((TaskValue a) -> Task ()) -> Task a | iTask a
evalRemoteTask task handleValue
	= get currentTaskInstanceNo
	>>- \taskid -> let share = taskValueShare taskid in
		(customEval share task ||- whileUnchanged share (changeTask handleValue))
where
	changeTask :: ((TaskValue a) -> Task ()) (TaskValue a) -> Task a | iTask a
	changeTask handleValue value=:(Value v True)
		= handleValue value @! v
	changeTask handleValue value
		= handleValue value @? \_->NoValue

proxyTask :: (Shared sds (TaskValue a)) (*IWorld -> *IWorld) -> (Task a) | iTask a & RWShared sds
proxyTask value_share onDestroy = Task eval
where
	eval DestroyEvent evalOpts iworld
		= (DestroyedResult, onDestroy iworld)
	eval event {taskId,lastEval} iworld
		# (val,iworld) = readRegister taskId value_share iworld
		= case val of
			Ok (ReadingDone val) = (ValueResult val (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (Task eval), iworld)
			Error e = (ExceptionResult e,iworld)

taskValueShare :: Int ->  SimpleSDSLens (TaskValue a) | iTask a
taskValueShare taskid = sdsFocus store_name (memoryStore store_name (Just NoValue))
where
	store_name = "taskValueShare_" +++ (toString taskid)

customEval :: (Shared sds (TaskValue a)) (Task a) -> (Task a) | iTask a & RWShared sds
customEval value_share (Task inner) = Task eval
where
	eval event evalOpts iworld
		= case inner event evalOpts iworld of
			v=:(ValueResult value info rep newtask, iworld) = storeValue v
			(ExceptionResult te, iworld) = (ExceptionResult te, iworld)
			(DestroyedResult, iworld) = (DestroyedResult, iworld)

	storeValue (ValueResult task_value info rep newtask, iworld)
		# (res, iworld) = write task_value value_share EmptyContext iworld
		= case res of
			Ok _    = (ValueResult task_value info rep newtask, iworld)
			Error _ = (ValueResult task_value info rep newtask, iworld)
