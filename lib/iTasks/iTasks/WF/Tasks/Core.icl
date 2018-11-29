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

import Data.Error, Data.Maybe, Data.Func
import Text.GenJSON
import StdString, StdBool, StdMisc
import qualified Data.Set as DS
import qualified Data.Map as DM

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

interact :: !d !(SDS () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v) | toPrompt d & iTask l & iTask r & iTask v & TC w
interact prompt shared {onInit,onEdit,onRefresh} editor = Task eval
where
	eval event evalOpts tt=:(TCDestroy _) iworld
		# iworld = 'SDS'.clearTaskSDSRegistrations ('DS'.singleton $ fromOk $ taskIdFromTaskTree tt) iworld
		= (DestroyedResult, iworld)

	eval event evalOpts tree iworld=:{current={taskTime}}
		//Decode or initialize state
		# (mbd,iworld) = case tree of
			(TCInit taskId ts)
				= case 'SDS'.readRegister taskId shared iworld of
					(Ok r,iworld)
						# (l, mode) = onInit r
						# v = case mode of
							Enter    = Nothing
							Update x = Just x
							View x   = Just x
						= case initEditorState taskId mode editor iworld of
							(Ok st,iworld) = (Ok (taskId,ts,l,v,st, mode =: View _),iworld)
							(Error e,iworld) = (Error e,iworld)
					(Error e,iworld)  = (Error e,iworld)
			(TCInteract taskId ts encl encv st viewMode)
				//Just decode the initially stored values
				= case (fromDeferredJSON encl, fromDeferredJSON encv) of
					(Just l,Just v) = (Ok (taskId,ts,l,v,st, viewMode),iworld)
					_				= (Error (exception ("Failed to decode stored model and view in interact: '" +++ toString encl +++ "', '"+++toString encv+++"'")),iworld)
		| mbd =:(Error _) = (ExceptionResult (fromError mbd), iworld)
		# (taskId,ts,l,v,st,viewMode) = fromOk mbd
        # (mbRes, iworld) = case event of
            EditEvent eTaskId name edit | eTaskId == taskId =
                applyEditEvent_ name edit taskId editor taskTime shared onEdit l v st iworld
            ResetEvent
				# resetMode = case (viewMode, v) of
					(True, Just v) = View v
					(True, _)      = abort "view mode without value"
					(_, Nothing)   = Enter
					(_, Just v)    = Update v
				= withVSt taskId
					( \vst -> case editor.Editor.genUI [] resetMode vst of
						(Ok (ui,st),vst) = (Ok (l,editor.Editor.valueFromState st,ReplaceUI (uic UIInteract [toPrompt prompt,ui]),st,taskTime), vst)
						(Error e, vst)  = (Error (exception e), vst)
					)
					iworld
            RefreshEvent taskIds _ | 'DS'.member taskId taskIds
                = refreshView_ taskId editor shared onRefresh l v st taskTime iworld
            FocusEvent fTaskId | fTaskId == taskId = (Ok (l,editor.Editor.valueFromState st,NoChange,st,taskTime),iworld)
            _ = (Ok (l,editor.Editor.valueFromState st,NoChange,st,ts),iworld)
        = case mbRes of
		   Error e = (ExceptionResult e, iworld)
		   Ok (l,mbV,change,st,ts)
                //Construct the result
                # v     = maybe v Just mbV // use previous view state of editor is in invalid state
                # value = maybe NoValue (\v -> Value (l, v) False) mbV
                # info  = {TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]}
                = (ValueResult value info change (TCInteract taskId ts (DeferredJSON l) (DeferredJSON v) st viewMode), iworld)

initEditorState :: TaskId (EditMode v) (Editor v) !*IWorld -> (MaybeError TaskException EditState, !*IWorld)
initEditorState taskId mode editor iworld = withVSt taskId
	( \vst -> case editor.Editor.genUI [] uniqueMode vst of
		(Ok (_, st), vst) = (Ok st,               vst)
		(Error e,    vst) = (Error $ exception e, vst)
	)
	iworld
where
	uniqueMode = case mode of
		Enter    = Enter
		Update x = Update x
		View x   = View x

applyEditEvent_ :: String JSONNode TaskId (Editor v) TaskTime (SDS () r w) (v l (Maybe v) -> (l, v, Maybe (r -> w))) l (Maybe v) EditState !*IWorld
                -> (!MaybeError TaskException (!l, !Maybe v, !UIChange, !EditState, !TaskTime), !*IWorld)
                | TC r & TC w
applyEditEvent_ name edit taskId editor taskTime shared onEdit l ov st iworld
	# (res, iworld) = withVSt taskId (editor.Editor.onEdit [] (s2dp name,edit) st) iworld
	= case res of
		Ok (change, st)
			# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
			= case editor.Editor.valueFromState st of
				Just v
					# (l, v, mbf) = onEdit v l ov
					= case mbf of
						Just f = case 'SDS'.modify (\r -> ((),f r)) shared iworld of
							(Ok (),iworld)   = (Ok (l,Just v,change,st,taskTime),iworld)
							(Error e,iworld) = (Error e,iworld)
						_
							= (Ok (l,Just v,change,st,taskTime),iworld)
				_ = (Ok (l,Nothing,change,st,taskTime),iworld)
        Error e = (Error (exception e), iworld)

refreshView_ :: TaskId (Editor v) (SDS () r w) (r l (Maybe v) -> (l, v, Maybe (r -> w))) l (Maybe v) EditState TaskTime !*IWorld
             -> (!MaybeError TaskException (!l, !Maybe v, !UIChange, !EditState, !TaskTime), !*IWorld)
             | TC r & TC w
refreshView_ taskId editor shared onRefresh l ov st taskTime iworld
	//Read the shared source and refresh the editor
	= case 'SDS'.readRegister taskId shared iworld of
		(Error e,iworld) = (Error e,iworld)
		(Ok r,iworld)
			# (l,v,mbf) = onRefresh r l ov
			# (res, iworld) = withVSt taskId (editor.Editor.onRefresh [] v st) iworld
			= case res of
				Ok (change,st)
					# change = case change of NoChange = NoChange; _ = ChangeUI [] [(1,ChangeChild change)]
					//Update the share if necessary
					= case mbf of
						Just f = case 'SDS'.modify (\r -> ((),f r)) shared iworld of
							(Ok (),iworld) = (Ok (l,Just v,change,st,taskTime), iworld)
							(Error e,iworld) = (Error e,iworld)
						Nothing
							= (Ok (l,Just v,change,st,taskTime), iworld)
				Error e = (Error (exception e), iworld)
