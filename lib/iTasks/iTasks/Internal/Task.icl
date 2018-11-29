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
from   iTasks.WF.Combinators.Core import :: AttachmentStatus
import iTasks.UI.Editor, iTasks.UI.Editor.Common
from iTasks.UI.Layout import :: LUI, :: LUIMoves, :: LUIMoveID, :: LUIEffectStage, :: LUINo

from iTasks.Internal.TaskState		import :: TaskTree(..), :: DeferredJSON(..), :: TIMeta(..)
from iTasks.Internal.TaskEval         import :: TaskEvalInfo(..)
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

wrapConnectionTask :: (ConnectionHandlers l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w
wrapConnectionTask {ConnectionHandlers|onConnect,onData,onShareChange,onDisconnect} sds
    = ConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect`,onData=onData`,onShareChange=onShareChange`,onTick=onTick`,onDisconnect=onDisconnect`} (toDynamic sds)
where
    onConnect` host (r :: r^) env
        # (mbl, mbw, out, close) = onConnect host r
        = (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

    onData` data (l :: l^) (r :: r^) env
        # (mbl, mbw, out, close) = onData data l r
        = (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

    onShareChange` (l :: l^) (r :: r^) env
        # (mbl, mbw, out, close) = onShareChange l r
        = (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

    // do nothing
    onTick` l _ env
        = (Ok l, Nothing, [], False, env)

    onDisconnect` (l :: l^) (r :: r^) env
        # (mbl, mbw) = onDisconnect l r
        = (toDyn <$> mbl, toDyn <$> mbw, env)

wrapIWorldConnectionTask :: (ConnectionHandlersIWorld l r w) (RWShared () r w) -> ConnectionTask | TC l & TC r & TC w
wrapIWorldConnectionTask {ConnectionHandlersIWorld|onConnect,onData,onShareChange,onTick,onDisconnect} sds
    = ConnectionTask {ConnectionHandlersIWorld|onConnect=onConnect`,onData=onData`,onShareChange=onShareChange`,onTick=onTick`,onDisconnect=onDisconnect`} (toDynamic sds)
where
    onConnect` host (r :: r^) env
        # (mbl, mbw, out, close, env) = onConnect host r env
        = (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

    onData` data (l :: l^) (r :: r^) env
        # (mbl, mbw, out, close, env) = onData data l r env
        = (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

    onShareChange` (l :: l^) (r :: r^) env
        # (mbl, mbw, out, close, env) = onShareChange l r env
        = (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

    onTick` (l :: l^) (r :: r^) env
        # (mbl, mbw, out, close, env) = onTick l r env
        = (toDyn <$> mbl, toDyn <$> mbw, out, close, env)

    onDisconnect` (l :: l^) (r :: r^) env
        # (mbl, mbw, env) = onDisconnect l r env
        = (toDyn <$> mbl, toDyn <$> mbw, env)

mkInstantTask :: (TaskId *IWorld -> (!MaybeError (Dynamic,String) a,!*IWorld)) -> Task a | iTask a
mkInstantTask iworldfun = Task (evalOnce iworldfun)
where
	evalOnce f event repOpts (TCInit taskId ts) iworld = case f taskId iworld of	
		(Ok a,iworld)							= (ValueResult (Value a True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) (TCStable taskId ts (DeferredJSON a)), iworld)
		(Error e, iworld)					    = (ExceptionResult e, iworld)

	evalOnce f event repOpts state=:(TCStable taskId ts enc) iworld = case fromJSONOfDeferredJSON enc of
		Just a	= (ValueResult (Value a True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) state, iworld)
		Nothing	= (ExceptionResult (exception "Corrupt task result"), iworld)

	evalOnce f _ _ (TCDestroy _) iworld	= (DestroyedResult,iworld)

	rep ResetEvent  = ReplaceUI (ui UIEmpty)
	rep _ 			= NoChange	

