implementation module iTasks.Internal.TaskState

import Text.GenJSON, StdString, Data.Func, Data.GenEq, Data.Maybe, Data.Functor, Data.Map.GenJSON
import iTasks.UI.Definition, iTasks.UI.Layout
import iTasks.WF.Definition
from iTasks.WF.Combinators.Core import :: AttachmentStatus

from iTasks.Internal.Task	import exception
from iTasks.Internal.TaskEval import :: TaskTime, :: TaskEvalInfo(..)
import iTasks.Internal.Serialization, iTasks.Internal.Generic.Visualization
import Data.CircularStack
import Data.Error, Data.Either
import iTasks.WF.Derives

derive JSONEncode TIMeta, TIType, TIValue, TIReduct, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, CircularStack, AsyncAction
derive JSONDecode TIMeta, TIType, TIValue, TIReduct, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, CircularStack, AsyncAction

derive gDefault TIMeta, InstanceProgress, TIType, TaskId, ValueStatus

instance toString DeferredJSON where
    toString (DeferredJSON x)        = toString $ toJSON x
    toString (DeferredJSONNode json) = toString json

fromDeferredJSON :: !DeferredJSON -> Maybe a | TC, JSONDecode{|*|} a
fromDeferredJSON (DeferredJSON x) = case dynamic x of
    (x :: a^) -> Just x
    _         -> Nothing
fromDeferredJSON (DeferredJSONNode json)  = fromJSON json

JSONEncode{|DeferredJSON|} _ (DeferredJSON a)
	= JSONEncode{|*|} False a
JSONEncode{|DeferredJSON|} _ (DeferredJSONNode json)
	= [json]

JSONDecode{|DeferredJSON|} _ []
	= (Just (DeferredJSONNode JSONNull), [])
JSONDecode{|DeferredJSON|} _ [x:xs]
	= ((Just (DeferredJSONNode x)), xs)
JSONDecode{|DeferredJSON|} _ l
	= (Nothing, l)

gEq{|DeferredJSON|} x y = toJSON x === toJSON y
gText{|DeferredJSON|} f djson = gText{|*|} f $ toJSON <$> djson
