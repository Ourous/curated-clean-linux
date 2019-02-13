implementation module iTasks.WF.Tasks.Core

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.UI.Prompt
import iTasks.SDS.Definition
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import qualified iTasks.Internal.SDS as SDS
import qualified iTasks.Internal.AsyncSDS as ASDS

import Data.Error, Data.Maybe, Data.Func, Data.Either
import Text.GenJSON
import StdString, StdBool, StdInt, StdMisc
import qualified Data.Set as DS
import qualified Data.Map as DM

derive JSONEncode Event,Set
derive gText Event, Set

treturn :: !a -> (Task a) | iTask a
treturn a  = mkInstantTask (\taskId iworld-> (Ok a, iworld))

throw :: !e -> Task a | iTask a & iTask, toString e
throw e = mkInstantTask (\taskId iworld -> (Error (dynamic e,toString e), iworld))

appWorld :: !(*World -> *World) -> Task ()
appWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		= (Ok (), {IWorld|iworld & world = fun world})

accWorld :: !(*World -> *(!a,!*World)) -> Task a | iTask a
accWorld fun = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|world}
		# (res,world) = fun world
		= (Ok res, {IWorld|iworld & world = world})

accWorldError :: !(*World -> (!MaybeError e a, !*World)) !(e -> err) -> Task a | iTask a & TC, toString err
accWorldError fun errf = mkInstantTask eval
where
	eval taskId iworld=:{IWorld|current={taskTime},world}
		# (res,world)	= fun world
		= case res of
			Error e
				# err = errf e
				= (Error (dynamic err,toString err), {IWorld|iworld & world = world})
			Ok v
				= (Ok v, {IWorld|iworld & world = world})

accWorldOSError :: !(*World -> (!MaybeOSError a, !*World)) -> Task a | iTask a
accWorldOSError fun = accWorldError fun OSException

instance toString OSException
where
	toString (OSException (_,err)) = "Error performing OS operation: " +++ err

interact :: !d !(sds () r w) (EditInteractionHandlers l r w v) (Editor v) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
interact prompt shared handlers editor
=  Task (eval prompt shared handlers editor)
where
	eval :: !d (sds () r w) (EditInteractionHandlers l r w v) (Editor v) Event TaskEvalOpts TaskTree *IWorld -> *(TaskResult (l,v), *IWorld) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
	eval _ _ _ _ event evalOpts tt=:(TCDestroy _) iworld
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton $ fromOk $ taskIdFromTaskTree tt) iworld
		= (DestroyedResult, iworld)

	eval prompt shared handlers editor (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Read taskId ts tree) iworld=:{sdsEvalStates, current={taskTime}}
	| not ('DS'.member taskId taskIds) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, iworld)
	= case 'DM'.get taskId sdsEvalStates of
		Nothing = (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val)
		= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncRead r^ w^), iworld) = case res of
				ReadingDone r
					# (l, mode) = handlers.onInit r
					# mbV = case mode of
							Enter    = Nothing
							Update x = Just x
							View x   = Just x
					= withVSt taskId (\vst. case editor.Editor.genUI 'DM'.newMap [] (uniqueMode mode) vst of
						(Error e, vst)		= (ExceptionResult (exception e), vst)
						(Ok (ui, st), vst)
							# change 	= ReplaceUI (uic UIInteract [toPrompt prompt, ui])
					        # info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
                			# value 	= maybe NoValue (\v -> Value (l, v) False) mbV
					        = (ValueResult value info change (TCInteract taskId ts (DeferredJSON l) (DeferredJSON mbV) st (mode =: View _)), vst)) iworld
				Reading sds = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates})
			(_, iworld) = (ExceptionResult (exception "Dynamic type mismatch"), iworld)

	eval prompt shared handlers editor (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Modify _ _ (TCInteract taskId ts encl encv st viewmode)) iworld=:{sdsEvalStates, current={taskTime}}
	| not ('DS'.member taskId taskIds) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, iworld)
	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
	= case 'DM'.get taskId sdsEvalStates of
		Nothing 				= (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val) 				= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncModify r^ w^), iworld) = case res of
				// We already have the result from executing the modify function, it happened on this machine.
				ModifyingDone _
					# value = (Value ((fromJust (fromDeferredJSON encl)), (fromJust (fromDeferredJSON encv))) False)
					= (ValueResult value evalInfo NoChange (TCInteract taskId ts encl encv st viewmode), {iworld & sdsEvalStates = 'DM'.del taskId sdsEvalStates })
				Modifying sds f
				= (ValueResult NoValue evalInfo NoChange t, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.modify f sds (TaskContext taskId))) sdsEvalStates})
			(Ok (dyn), iworld)							= (ExceptionResult (exception ("Dynamic type mismatch, type was " +++ toString (typeCodeOfDynamic dyn))), iworld)

    // Ignore all other events when waiting on an async operation.
	eval _ _ _ _  _ _ t=:(TCAwait _ taskId ts tree) iworld = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, iworld)

	// Handle all other events normally
	eval prompt shared handlers editor event evalOpts tree iworld=:{current={taskTime}, sdsEvalStates}
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok ('SDS'.ReadingDone r),iworld)
						# (l, mode) = handlers.onInit r
						# v = case mode of
							Enter    = Nothing
							Update x = Just x
							View x   = Just x
						= case initEditorState taskId mode editor iworld of
							(Ok st,iworld) = (Ok (Left (taskId,ts,l,v,st, mode =: View _)),iworld)
							(Error e,iworld) = (Error e,iworld)
					(Ok ('SDS'.Reading sds), iworld)
						= (Ok (Right (taskId, ts, sds)),{iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates})
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv st viewMode)
				//Just decode the initially stored values
				= case (fromDeferredJSON encl, fromDeferredJSON encv) of
					(Just l,Just v) = (Ok (Left (taskId,ts,l,v,st, viewMode)),iworld)
					_				= (Error (exception ("Failed to decode stored model and view in interact: '" +++ toString encl +++ "', '"+++toString encv+++"'")),iworld)
		| mbd =:(Error _) = (ExceptionResult (fromError mbd), iworld)
		| mbd =:(Ok (Right _)) = case mbd of
			(Ok (Right (taskId, ts, sds))) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} (ReplaceUI (uia UIProgressBar (textAttr "Getting data"))) (TCAwait Read taskId taskTime tree), iworld)
		# (Left (taskId,ts,l,v,st,viewMode)) = fromOk mbd
		# (mbRes, iworld) = case event of
			EditEvent eTaskId name edit | eTaskId == taskId =
				applyEditEvent_ name edit taskId editor taskTime shared handlers.EditInteractionHandlers.onEdit l v st iworld
			ResetEvent
				# resetMode = case (viewMode, v) of
					(True, Just v) = View v
					(True, _)      = abort "view mode without value"
					(_, Nothing)   = Enter
					(_, Just v)    = Update v
				= withVSt taskId
					( \vst -> case editor.Editor.genUI 'DM'.newMap [] resetMode vst of
						(Ok (ui,st),vst) = (Ok (Left (l,editor.Editor.valueFromState st,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),st,taskTime)), vst)
						(Error e, vst)  = (Error (exception e), vst)
					)
					iworld
			RefreshEvent taskIds _ | 'DS'.member taskId taskIds
				= refreshView_ taskId editor shared handlers.EditInteractionHandlers.onRefresh l v st taskTime iworld
			FocusEvent fTaskId | fTaskId == taskId = (Ok (Left (l,editor.Editor.valueFromState st,NoChange,st,taskTime)),iworld)
			_ = (Ok (Left (l,editor.Editor.valueFromState st,NoChange,st,ts)),iworld)
		= case mbRes of
		   Error e = (ExceptionResult e, iworld)
		   // An EditEvent can lead to an asynchronous update of a share. However, we do not
		   // care about the result of this update so we do not show the loading bar. We do
		   // want to wait for the result of the modify (otherwise we send multiple requests which may interfere),
		   // so we transition to the TCAwait state
		   Ok (Right (type, sdsf, l, v, st, change))
			   	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
			   	# tree = TCAwait type taskId taskTime (TCInteract taskId taskTime (DeferredJSON l) (DeferredJSON v) st viewMode)
			   	= (ValueResult NoValue evalInfo NoChange tree, {iworld & sdsEvalStates = 'DM'.put taskId sdsf iworld.sdsEvalStates})
		   Ok (Left (l,mbV,change,st,ts))
                //Construct the result
                # v     = maybe v Just mbV // use previous view state of editor is in invalid state
                # value = maybe NoValue (\v -> Value (l, v) False) mbV
                # info  = {TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
                = (ValueResult value info change (TCInteract taskId ts (DeferredJSON l) (DeferredJSON v) st viewMode), iworld)

interactView :: !d (sds () r w) (ViewInteractionHandlers l r w v) (Editor v) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
interactView prompt shared handlers editor
= Task (eval prompt shared handlers editor)
where
	eval :: !d (sds () r w) (ViewInteractionHandlers l r w v) (Editor v) Event TaskEvalOpts TaskTree *IWorld -> *(TaskResult (l,v), *IWorld) | toPrompt d & iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
	eval _ _ _ _ event evalOpts tt=:(TCDestroy _) iworld
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton $ fromOk $ taskIdFromTaskTree tt) iworld
		= (DestroyedResult, iworld)

	eval prompt shared handlers editor (RefreshEvent taskIds reason) evalOpts t=:(TCAwait Read taskId ts tree) iworld=:{sdsEvalStates, current={taskTime}}
	| not ('DS'.member taskId taskIds) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, iworld)
	= case 'DM'.get taskId sdsEvalStates of
		Nothing = (ExceptionResult (exception ("No SDS state found for task " +++ toString taskId)), iworld)
		(Just val)
		= case val iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok (res :: AsyncRead r^ w^), iworld) = case res of
				ReadingDone r
					# (l, mode) = handlers.onInitView r
					# mbV = case mode of
							Enter    = Nothing
							Update x = Just x
							View x   = Just x
					= withVSt taskId (\vst. case editor.Editor.genUI 'DM'.newMap [] (uniqueMode mode) vst of
						(Error e, vst)		= (ExceptionResult (exception e), vst)
						(Ok (ui, st), vst)
							# change 	= ReplaceUI (uic UIInteract [toPrompt prompt, ui])
					        # info      = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
                			# value 	= maybe NoValue (\v -> Value (l, v) False) mbV
					        = (ValueResult value info change (TCInteract taskId ts (DeferredJSON l) (DeferredJSON mbV) st (mode =: View _)), vst)) iworld
				Reading sds = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, {iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates})
			(_, iworld) = (ExceptionResult (exception "Dynamic type mismatch"), iworld)

	eval _ _ _ _  _ _ t=:(TCAwait _ taskId ts tree) iworld = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} NoChange t, iworld)

	eval prompt shared handlers editor event evalOpts tree iworld=:{current={taskTime}, sdsEvalStates}
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok ('SDS'.ReadingDone r),iworld)
						# (l, mode) = handlers.onInitView r
						# v = case mode of
							Enter    = Nothing
							Update x = Just x
							View x   = Just x
						= case initEditorState taskId mode editor iworld of
							(Ok st,iworld) = (Ok (Left (taskId,ts,l,v,st, mode =: View _)),iworld)
							(Error e,iworld) = (Error e,iworld)
					(Ok ('SDS'.Reading sds), iworld)
						= (Ok (Right (taskId, ts, sds)),{iworld & sdsEvalStates = 'DM'.put taskId (dynamicResult ('SDS'.readRegister taskId sds)) sdsEvalStates})
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv st viewMode)
				//Just decode the initially stored values
				= case (fromDeferredJSON encl, fromDeferredJSON encv) of
					(Just l,Just v) = (Ok (Left (taskId,ts,l,v,st, viewMode)),iworld)
					_				= (Error (exception ("Failed to decode stored model and view in interact: '" +++ toString encl +++ "', '"+++toString encv+++"'")),iworld)
		| mbd =:(Error _) = (ExceptionResult (fromError mbd), iworld)
		| mbd =:(Ok (Right _)) = case mbd of
			(Ok (Right (taskId, ts, sds))) = (ValueResult NoValue {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap} (ReplaceUI (uia UIProgressBar (textAttr "Getting data"))) (TCAwait Read taskId taskTime tree), iworld)
		# (Left (taskId,ts,l,v,st,viewMode)) = fromOk mbd
		# (mbRes, iworld) = case event of
			ResetEvent
				# resetMode = case (viewMode, v) of
					(True, Just v) = View v
					(True, _)      = abort "view mode without value"
					(_, Nothing)   = Enter
					(_, Just v)    = Update v
				= withVSt taskId
					( \vst -> case editor.Editor.genUI 'DM'.newMap [] resetMode vst of
						(Ok (ui,st),vst) = (Ok (Left (l,editor.Editor.valueFromState st,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),st,taskTime)), vst)
						(Error e, vst)  = (Error (exception e), vst)
					)
					iworld
			RefreshEvent taskIds _ | 'DS'.member taskId taskIds
				= refresh taskId editor shared handlers.ViewInteractionHandlers.onRefreshView l v st taskTime iworld
			_ = (Ok (Left (l,editor.Editor.valueFromState st,NoChange,st,ts)),iworld)
		= case mbRes of
		   Error e = (ExceptionResult e, iworld)
		   // An EditEvent can lead to an asynchronous update of a share. However, we do not
		   // care about the result of this update so we do not show the loading bar. We do
		   // want to wait for the result of the modify (otherwise we send multiple requests which may interfere),
		   // so we transition to the TCAwait state
		   Ok (Right (type, sdsf, l, v, st, change))
			   	# evalInfo = {TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes='DM'.newMap}
			   	# tree = TCAwait type taskId taskTime (TCInteract taskId taskTime (DeferredJSON l) (DeferredJSON v) st viewMode)
			   	= (ValueResult NoValue evalInfo NoChange tree, {iworld & sdsEvalStates = 'DM'.put taskId sdsf iworld.sdsEvalStates})
		   Ok (Left (l,mbV,change,st,ts))
                //Construct the result
                # v     = maybe v Just mbV // use previous view state of editor is in invalid state
                # value = maybe NoValue (\v -> Value (l, v) False) mbV
                # info  = {TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
                = (ValueResult value info change (TCInteract taskId ts (DeferredJSON l) (DeferredJSON v) st viewMode), iworld)
    where
    	refresh :: TaskId (Editor v) (sds () r w) (r l (Maybe v) -> (l, v, Maybe (r -> w))) l (Maybe v) EditState TaskTime !*IWorld
             -> (!MaybeError TaskException (Either (!l, !Maybe v, !UIChange, !EditState, !TaskTime) (!AsyncAction, !!*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld), !l, !Maybe v, !EditState, !UIChange)), !*IWorld)
             | TC r & TC w & Registrable sds
        refresh taskId editor shared onRefresh l ov st taskTime iworld
		= case 'SDS'.readRegister taskId shared iworld of
			(Error e,iworld) = (Error e,iworld)
			(Ok ('SDS'.Reading sds), iworld) = (Ok (Right (Read, dynamicResult ('SDS'.readRegister taskId sds), l, ov, st, NoChange)), iworld)
			(Ok ('SDS'.ReadingDone r),iworld)
				# (l,v,mbf) = onRefresh r l ov
				# (res, iworld) = withVSt taskId (editor.Editor.onRefresh [] v st) iworld
				= case res of
					Error e = (Error (exception e), iworld)
					Ok (change,st)
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					= (Ok (Left (l,Just v,change,st,taskTime)), iworld)

initEditorState :: TaskId (EditMode v) (Editor v) !*IWorld -> (MaybeError TaskException EditState, !*IWorld)
initEditorState taskId mode editor iworld = withVSt taskId
	( \vst -> case editor.Editor.genUI 'DM'.newMap [] (uniqueMode mode) vst of
		(Ok (_, st), vst) = (Ok st,               vst)
		(Error e,    vst) = (Error $ exception e, vst)
	)
	iworld

applyEditEvent_ :: String JSONNode TaskId (Editor v) TaskTime (sds () r w) (v l (Maybe v) -> (l, v, Maybe (r -> w))) l (Maybe v) EditState !*IWorld
		-> (!MaybeError TaskException (Either (!l, !Maybe v, !UIChange, !EditState, !TaskTime) (!AsyncAction, !*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld), !l, !Maybe v, !EditState, !UIChange)), !*IWorld)
		| TC r & TC w & RWShared sds
applyEditEvent_ name edit taskId editor taskTime shared onEdit l ov st iworld
	# (res, iworld) = withVSt taskId (editor.Editor.onEdit [] (s2dp name,edit) st) iworld
	= case res of
		Ok (change, st)
			# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
			= case editor.Editor.valueFromState st of
				Just v
					# (l, v, mbf) = onEdit v l ov
					= case mbf of
						Just f = case 'SDS'.modify f shared ('SDS'.TaskContext taskId) iworld of
							(Ok ('SDS'.ModifyingDone _),iworld)   = (Ok (Left (l,Just v,change,st,taskTime)),iworld)
							(Ok ('SDS'.Modifying sds _), iworld) = (Ok (Right (Modify, dynamicResult ('SDS'.modify f sds ('SDS'.TaskContext taskId)), l, Just v, st, change)),iworld)
							(Error e,iworld) = (Error e,iworld)
						_ = (Ok (Left (l,Just v,change,st,taskTime)),iworld)
				_ = (Ok (Left (l,Nothing,change,st,taskTime)),iworld)
        Error e = (Error (exception e), iworld)

refreshView_ :: TaskId (Editor v) (sds () r w) (r l (Maybe v) -> (l, v, Maybe (r -> w))) l (Maybe v) EditState TaskTime !*IWorld
             -> (!MaybeError TaskException (Either (!l, !Maybe v, !UIChange, !EditState, !TaskTime) (!AsyncAction, !!*IWorld -> *(MaybeError TaskException Dynamic, !*IWorld), !l, !Maybe v, !EditState, !UIChange)), !*IWorld)
             | TC r & TC w & RWShared sds
refreshView_ taskId editor shared onRefresh l ov st taskTime iworld
	//Read the shared source and refresh the editor
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok ('SDS'.Reading sds), iworld) = (Ok (Right (Read, dynamicResult ('SDS'.readRegister taskId sds), l, ov, st, NoChange)), iworld)
		(Ok ('SDS'.ReadingDone r),iworld)
			# (l,v,mbf) = onRefresh r l ov
			# (res, iworld) = withVSt taskId (editor.Editor.onRefresh [] v st) iworld
			= case res of
				Ok (change,st)
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					//Update the share if necessary
					= case mbf of
						Just f = case 'SDS'.modify f shared ('SDS'.TaskContext taskId) iworld of
							(Ok ('SDS'.ModifyingDone _),iworld) = (Ok (Left (l,Just v,change,st,taskTime)), iworld)
							(Ok ('SDS'.Modifying sds _), iworld) = (Ok (Right (Modify, dynamicResult ('SDS'.modify f sds ('SDS'.TaskContext taskId)), l, Just v, st, change)), iworld)
							(Error e,iworld) = (Error e,iworld)
						Nothing = (Ok (Left (l,Just v,change,st,taskTime)), iworld)
				Error e = (Error (exception e), iworld)

uniqueMode :: (EditMode a) -> *(EditMode a)
uniqueMode mode = case mode of
	Enter    = Enter
	Update x = Update x
	View x   = View x
