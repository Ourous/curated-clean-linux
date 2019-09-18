implementation module iTasks.Internal.Task

from StdFunc import const, id
import StdClass, StdArray, StdTuple, StdInt, StdList, StdBool, StdMisc, Data.Func
from Data.Map import :: Map
import qualified Data.Map as DM
import Text.HTML, Internet.HTTP, Data.Error, Data.Functor, Text.GenJSON
import iTasks.Internal.IWorld, iTasks.UI.Definition, iTasks.Internal.Util, iTasks.Internal.DynamicUtil
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage

import iTasks.WF.Definition
import iTasks.WF.Tasks.IO
import iTasks.WF.Tasks.Core
from   iTasks.WF.Combinators.Core import :: AttachmentStatus
import iTasks.UI.Editor, iTasks.UI.Editor.Common
import iTasks.Internal.SDS
from iTasks.UI.Layout import :: LUI, :: LUIMoves, :: LUIMoveID, :: LUIEffectStage, :: LUINo

from iTasks.Internal.TaskState		import :: DeferredJSON(..), :: TIMeta(..) , :: TIType(..), :: AsyncAction
import iTasks.Internal.TaskEval
from iTasks.SDS.Combinators.Common import toDynamic
from iTasks.Internal.Serialization    import JSONEncode, JSONDecode, dynamicJSONEncode, dynamicJSONDecode

fromJSONOfDeferredJSON :: !DeferredJSON -> Maybe a | TC a & JSONDecode{|*|} a
fromJSONOfDeferredJSON (DeferredJSON v)
	= case make_dynamic v of
		(v :: a^)
			-> Just v
fromJSONOfDeferredJSON (DeferredJSONNode json)
	= fromJSON json

make_dynamic v = dynamic v

JSONEncode{|Task|} _ _ tt = [dynamicJSONEncode tt]
JSONDecode{|Task|} _ _ [tt:c] = (dynamicJSONDecode tt,c)
JSONDecode{|Task|} _ _ c = (Nothing,c)

gText{|Task|} _ _ _ = ["<Task>"]
gEditor{|Task|} _ _ tj fj =
	emptyEditorWithErrorInEnterMode_  (JSONEncode{|* -> *|} tj) (JSONDecode{|* -> *|} fj) "Tasks cannot be entered."

gEq{|Task|} _ _ _			= True // tasks are always equal??

gDefault{|Task|} gDefx = Task (\_ -> abort error)
where
	error = "Creating default task functions is impossible"

wrapConnectionTask :: (ConnectionHandlers l r w) (sds () r w) -> ConnectionTask | TC l & TC r & TC w & RWShared sds
wrapConnectionTask ch=:{ConnectionHandlers|onConnect,onData,onShareChange,onDisconnect,onDestroy} sds
	= ConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect`,onData=onData`,onShareChange=onShareChange`,onTick=onTick`,onDisconnect=onDisconnect`,onDestroy=onDestroy`} (toDynamic sds)
where
	onConnect` connId host (r :: r^) env
		# (mbl, mbw, out, close) = onConnect connId host r
		= (toDyn <$> mbl, toDyn <$> mbw, out, close, env)
	onConnect` _ _ val env = abort ("onConnect does not match with type " +++ toString (typeCodeOfDynamic val))

	onData` data (l :: l^) (r :: r^) env
		# (mbl, mbw, out, close) = onData data l r
		= (toDyn <$> mbl, toDyn <$> mbw, out, close, env)
	onData` _ _ val env = abort ("onData does not match with type " +++ toString (typeCodeOfDynamic val))

	onShareChange` (l :: l^) (r :: r^) env
		# (mbl, mbw, out, close) = onShareChange l r
		= (toDyn <$> mbl, toDyn <$> mbw, out, close, env)
	onShareChange` l r env = abort ("onShareChange does not match with type l=" +++ toString (typeCodeOfDynamic l) +++ ", r=" +++ toString (typeCodeOfDynamic r) +++ ". Expected l=" +++ toString (typeCodeOfDynamic (dynamic ch)))

	// do nothing
	onTick` l _ env
		= (Ok l, Nothing, [], False, env)

	onDisconnect` (l :: l^) (r :: r^) env
		# (mbl, mbw) = onDisconnect l r
		= (toDyn <$> mbl, toDyn <$> mbw, env)
	onDisconnect` l r env = abort ("onDisconnect does not match with type l=" +++ toString (typeCodeOfDynamic l) +++ ", r=" +++ toString (typeCodeOfDynamic r))
	onDestroy` (l :: l^) env
		# (mbl, out) = onDestroy l
		= (toDyn <$> mbl, out, env)
	onDestroy` l env = abort ("onDestroy does not match with type l=" +++ toString (typeCodeOfDynamic l))

wrapIWorldConnectionTask :: (ConnectionHandlersIWorld l r w) (sds () r w) -> ConnectionTask | TC l & TC r & TC w & RWShared sds
wrapIWorldConnectionTask {ConnectionHandlersIWorld|onConnect,onData,onShareChange,onTick,onDisconnect,onDestroy} sds
	= ConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect`,onData=onData`,onShareChange=onShareChange`,onTick=onTick`,onDisconnect=onDisconnect`,onDestroy=onDestroy`} (toDynamic sds)
where
	onConnect` connId host (r :: r^) env
		# (mbl, mbw, out, close, env) = onConnect connId host r env
		= (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

	onConnect` _ _ val env = abort ("onConnect does not match with type " +++ toString (typeCodeOfDynamic val))

	onData` data (l :: l^) (r :: r^) env
		# (mbl, mbw, out, close, env) = onData data l r env
		= (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

	onData` _ _ val env = abort ("onData does not match with type " +++ toString (typeCodeOfDynamic val))

	onShareChange` (l :: l^) (r :: r^) env
		# (mbl, mbw, out, close, env) = onShareChange l r env
		= (toDyn <$> mbl, toDyn <$> mbw, out, close, env)
	onShareChange` l r env = abort ("onShareChange does not match with type l=" +++ toString (typeCodeOfDynamic l) +++ ", r=" +++ toString (typeCodeOfDynamic r))

	onTick` (l :: l^) (r :: r^) env
		# (mbl, mbw, out, close, env) = onTick l r env
		= (toDyn <$> mbl, toDyn <$> mbw, out, close, env)
	onTick` l r env = abort ("onTick does not match with type l=" +++ toString (typeCodeOfDynamic l) +++ ", r=" +++ toString (typeCodeOfDynamic r))

	onDisconnect` (l :: l^) (r :: r^) env
		# (mbl, mbw, env) = onDisconnect l r env
		= (toDyn <$> mbl, toDyn <$> mbw, env)
	onDisconnect` l r env = abort ("onDisconnect does not match with type l=" +++ toString (typeCodeOfDynamic l) +++ ", r=" +++ toString (typeCodeOfDynamic r))
	onDestroy` (l :: l^) env
		# (mbl, out, env) = onDestroy l env
		= (toDyn <$> mbl, out, env)
	onDestroy` l env = abort ("onDestroy does not match with type l=" +++ toString (typeCodeOfDynamic l))

mkInstantTask :: (TaskId *IWorld -> (MaybeError TaskException a,*IWorld)) -> Task a | iTask a
mkInstantTask iworldfun = Task eval
where
	eval DestroyEvent _ iworld = (DestroyedResult, iworld)
	eval event {taskId,lastEval} iworld
		= case iworldfun taskId iworld of
			(Ok a,iworld)     = (ValueResult (Value a True) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (treturn a), iworld)
			(Error e, iworld) = (ExceptionResult e, iworld)

nopTask :: Task a
nopTask = Task eval
where
	eval DestroyEvent _ iworld = (DestroyedResult, iworld)
	eval event {lastEval} iworld
		= (ValueResult NoValue (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (Task eval), iworld)
