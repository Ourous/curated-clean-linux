implementation module iTasks.WF.Tasks.SDS

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import qualified iTasks.Internal.SDS as SDS
import StdString, Data.Func, Data.Error
import qualified Data.Set as DS

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException
	
get :: !(ReadWriteShared a w) -> Task a | iTask a
get shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime}}
		# (val,iworld) = 'SDS'.read shared iworld
		= case val of
			Ok val		= (Ok val,iworld)
			Error e		= (Error e, iworld)
	
set :: !a !(ReadWriteShared r a)  -> Task a | iTask a & TC r
set val shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (res,iworld)	='SDS'.write val shared iworld
		= case res of
			Ok _	= (Ok val, iworld)
			Error e	= (Error e, iworld)

upd :: !(r -> w) !(ReadWriteShared r w) -> Task w | iTask r & iTask w
upd fun shared = mkInstantTask eval
where
	eval taskId iworld=:{current={taskTime,taskInstance}}
		# (er, iworld)	= 'SDS'.read shared iworld
		= case er of
			Error e		= (Error e, iworld)
			Ok r	
				# w				= fun r
				# (er, iworld)	=  'SDS'.write w shared iworld
				= case er of
					Ok _	= (Ok w, iworld)
					Error e = (Error e, iworld)
					
watch :: !(ReadWriteShared r w) -> Task r | iTask r
watch shared = Task eval
where
	eval event evalOpts (TCInit taskId=:(TaskId instanceNo _) ts) iworld
		# (val,iworld)	= 'SDS'.readRegister taskId shared iworld
		# res = case val of
			Ok val		= ValueResult (Value val False) {TaskEvalInfo|lastEvent=ts,removedTasks=[],refreshSensitive=True} (rep event) (TCInit taskId ts)
			Error e		= ExceptionResult e
		= (res,iworld)
	eval event repAs ttree=:(TCDestroy _) iworld
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton $ fromOk $ taskIdFromTaskTree ttree) iworld
		= (DestroyedResult,iworld)

	rep ResetEvent  = ReplaceUI (ui UIEmpty) 
	rep _ 			= NoChange
