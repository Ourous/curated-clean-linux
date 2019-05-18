implementation module iTasks.WF.Tasks.SDS

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import qualified iTasks.Internal.SDS as SDS
import StdString, Data.Func, Data.Error, StdBool
import qualified Data.Set as DS
import qualified Data.Map as DM

instance toString SharedException
where
	toString (SharedException err) = "Error performing operation on shared:" +++ err

derive class iTask SharedException

get :: !(sds () a w) -> Task a | iTask a & Readable sds & TC w
get shared = Task (eval shared)
where
	eval :: (sds () a w) Event TaskEvalOpts TaskTree !*IWorld -> (TaskResult a, !*IWorld) | TC w & TC a & Readable sds & iTask a
	eval _ DestroyEvent opts tree iworld=:{sdsEvalStates}
	# sdsEvalStates = 'DM'.del (fromOk (taskIdFromTaskTree tree)) sdsEvalStates
	= (DestroyedResult, {iworld & sdsEvalStates = sdsEvalStates})

	eval shared event opts tree=:(TCInit taskId ts) iworld=:{sdsEvalStates}
	= case 'SDS'.read shared ('SDS'.TaskContext taskId) iworld of
		// Remote read is queued, enter AwaitRead state and show loading UI.
		(Ok (Reading sds), iworld)
			# ui = ReplaceUI (uia UIProgressBar (textAttr "Getting data"))
			# newState = TCAwait Read taskId ts tree
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.read sds ('SDS'.TaskContext taskId))) sdsEvalStates
			= (ValueResult NoValue (tei ts) ui newState, {iworld & sdsEvalStates = sdsEvalStates})
		// Remote read not necessary, return result directly.
		(Ok (ReadingDone val), iworld)
		# tree = TCStable taskId ts (DeferredJSON val)
		# result = ValueResult (Value val True) (tei ts) (ReplaceUI (ui UIEmpty)) tree
		= (result, iworld)
		(Error e, iworld) 				= (ExceptionResult e, iworld)

	eval shared event opts tree=:(TCAwait Read taskId ts subtree) iworld=:{IWorld|sdsEvalStates}
	= case 'DM'.get taskId sdsEvalStates of
		Nothing 				= (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val) 				= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncRead a^ w^), iworld)
			= case res of
				(ReadingDone val) = (ValueResult (Value val True) (tei ts) (ReplaceUI (ui UIEmpty)) subtree, iworld)
				(Reading sds)
				# ui = NoChange
				# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.read sds ('SDS'.TaskContext taskId))) sdsEvalStates
				= (ValueResult NoValue (tei ts) ui tree, {iworld & sdsEvalStates = sdsEvalStates})

	eval _ event opts s=:(TCStable taskId ts enc) iworld
	= case fromDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],attributes='DM'.newMap} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

set :: !a !(sds () r a)  -> Task a | iTask a & TC r & Writeable sds
set val shared = Task (eval val shared)
where
	eval :: a (sds () r a) Event TaskEvalOpts TaskTree *IWorld -> (TaskResult a, !*IWorld) | iTask a & TC r & Writeable sds
	eval _ _ DestroyEvent _ tree iworld=:{sdsEvalStates}
	# sdsEvalStates = 'DM'.del (fromOk (taskIdFromTaskTree tree)) sdsEvalStates
	= (DestroyedResult, {iworld & sdsEvalStates = sdsEvalStates})

	eval val shared event _ tree=:(TCAwait Write taskId ts st) iworld=:{sdsEvalStates}
	# evalInfo = {lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
	= case 'DM'.get taskId sdsEvalStates of
		Nothing = (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just f) = case f iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncWrite r^ a^), iworld) = case res of
				WritingDone = (ValueResult (Value val True) evalInfo (ReplaceUI (ui UIEmpty)) (TCStable taskId ts (DeferredJSON val)), {iworld & sdsEvalStates = 'DM'.del taskId sdsEvalStates})
				Writing sds = (ValueResult NoValue evalInfo NoChange tree, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.write val sds ('SDS'.TaskContext taskId))) sdsEvalStates})

	eval val shared event _ tree=:(TCInit taskId ts) iworld=:{sdsEvalStates}
	# evalInfo = {lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
	= case 'SDS'.write val shared ('SDS'.TaskContext taskId) iworld of
		(Error e, iworld) 		= (ExceptionResult e, iworld)
		(Ok (Writing sds), iworld)
			# ui = ReplaceUI (uia UIProgressBar (textAttr "Writing data"))
			# tree = TCAwait Write taskId ts (TCInit taskId ts)
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.write val sds ('SDS'.TaskContext taskId))) sdsEvalStates
			= (ValueResult NoValue evalInfo ui tree, {iworld & sdsEvalStates = sdsEvalStates})
		(Ok WritingDone, iworld) 			= (ValueResult (Value val True) evalInfo (rep event) (TCStable taskId ts (DeferredJSON val)), iworld)

	eval val shared event _ s=:(TCStable taskId ts enc) iworld = case fromDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],attributes='DM'.newMap} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

upd :: !(r -> w) !(sds () r w) -> Task w | iTask r & iTask w & RWShared sds
upd fun shared = Task (eval fun shared)
where
	eval :: (r -> w) (sds () r w) Event TaskEvalOpts TaskTree *IWorld -> (TaskResult w, !*IWorld) | iTask r & iTask w & RWShared sds
	eval fun shared DestroyEvent _ tree iworld=:{sdsEvalStates}
	# sdsEvalStates = 'DM'.del (fromOk (taskIdFromTaskTree tree)) sdsEvalStates
	= (DestroyedResult, {iworld & sdsEvalStates = sdsEvalStates})

	eval fun shared event _ tree=:(TCInit taskId ts) iworld=:{sdsEvalStates}
	= case 'SDS'.modify fun shared ('SDS'.TaskContext taskId) iworld of
		(Error (d, s), iworld) 						= (ExceptionResult (d, s), iworld)
		(Ok (ModifyingDone w), iworld)
		# result = ValueResult (Value w True) (tei ts) (rep event) (TCStable taskId ts (DeferredJSON w))
		= (result, iworld)
		(Ok (Modifying sds _), iworld)
		# ui = ReplaceUI (uia UIProgressBar (textAttr "Getting data"))
		# tree = TCAwait Modify taskId ts (TCInit taskId ts)
		# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.modify fun sds ('SDS'.TaskContext taskId))) sdsEvalStates
		= (ValueResult NoValue (tei ts) ui tree, {iworld & sdsEvalStates = sdsEvalStates})

	eval fun shared event _ tree=:(TCAwait Modify taskId ts subtree) iworld=:{sdsEvalStates} =  case 'DM'.get taskId sdsEvalStates of
		Nothing 				= (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val) 				= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncModify r^ w^), iworld) = case res of
				ModifyingDone w
				# result = ValueResult (Value w True) (tei ts) NoChange (TCStable taskId ts (DeferredJSON w))
				= (result, iworld)
				Modifying sds f
				# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.modify f sds ('SDS'.TaskContext taskId))) sdsEvalStates
				# result = ValueResult NoValue (tei ts) NoChange tree
				= (result, {iworld & sdsEvalStates = sdsEvalStates})

	eval fun shared event _ s=:(TCStable taskId ts enc) iworld = case fromDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,removedTasks=[],attributes='DM'.newMap} (rep event) s, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

watch :: !(sds () r w) -> Task r | iTask r & TC w & Readable, Registrable sds
watch shared = Task (eval shared)
where
	eval :: (sds () r w) Event TaskEvalOpts TaskTree *IWorld
		-> (TaskResult r, !*IWorld) | iTask r & TC w & Readable, Registrable sds
	eval _ DestroyEvent _ ttree iworld=:{sdsEvalStates}
		# taskId = fromOk $ taskIdFromTaskTree ttree
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton $ taskId) iworld
		# iworld = {iworld & sdsEvalStates = 'DM'.del taskId sdsEvalStates}
		= (DestroyedResult,iworld)

	eval shared event _ tree=:(TCInit taskId ts) iworld=:{sdsEvalStates}
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok (ReadingDone val), iworld)
			# tree = TCBasic taskId ts (DeferredJSON val) False
			= (ValueResult (Value val False) (tei ts) (rep event) tree, iworld)
		(Ok (Reading sds), iworld)
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates
			# result = ValueResult NoValue (tei ts) (rep event) (TCAwait Read taskId ts tree)
			= (result, {iworld & sdsEvalStates = sdsEvalStates})

	eval shared event _ tree=:(TCBasic taskId ts val stable) iworld=:{sdsEvalStates}
	| not (isRefreshForTask event tree) = case fromDeferredJSON val of
		Nothing = (ExceptionResult (exception "Corrupt task result"), iworld)
		Just v = (ValueResult (Value v False) (tei ts) NoChange tree, iworld)
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok (ReadingDone val), iworld)
			# tree = TCBasic taskId ts (DeferredJSON val) False
			= (ValueResult (Value val False) (tei ts) (rep event) tree, iworld)
		(Ok (Reading sds), iworld) = case fromDeferredJSON val of
			Nothing = (ExceptionResult (exception "Corrupt task result"), iworld)
			Just v
			# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates
			# result = ValueResult (Value v False) (tei ts) (rep event) (TCAwait Read taskId ts tree)
			= (result, {iworld & sdsEvalStates = sdsEvalStates})

	eval _ event=:(RefreshEvent taskIds reason) _ tree=:(TCAwait Read taskId ts subtree) iworld=:{sdsEvalStates}
	# oldValue = case subtree of
		(TCInit _ _) = NoValue
		(TCBasic _ _ val _) = case fromDeferredJSON val of
			Nothing = NoValue
			Just v = Value v False
	| not (isRefreshForTask event tree) = (ValueResult oldValue (tei ts) NoChange tree, iworld)
	= case 'DM'.get taskId sdsEvalStates of
		Nothing = (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		Just val = case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncRead r^ w^), iworld) = case res of
				ReadingDone v
					# sdsEvalStates = 'DM'.del taskId sdsEvalStates
					# result = ValueResult (Value v False) (tei ts) NoChange (TCBasic taskId ts (DeferredJSON v) False)
					= (result, {iworld & sdsEvalStates = sdsEvalStates})
				Reading sds
					# sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates
					# result = ValueResult oldValue (tei ts) NoChange tree
					= (result, {iworld & sdsEvalStates = sdsEvalStates})

tei ts = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}

rep ResetEvent  = ReplaceUI (ui UIEmpty)
rep _ 			= NoChange

isRefreshForTask (RefreshEvent taskIds _) tree = 'DS'.member (fromOk (taskIdFromTaskTree tree)) taskIds
isRefreshForTask ResetEvent _ = True
isRefreshForTask _ _ = False
