implementation module iTasks.Internal.TaskState

import Text.GenJSON, StdString, Data.Func, Data.GenEq, Data.Maybe, Data.Functor
import iTasks.UI.Definition, iTasks.UI.Layout
import iTasks.WF.Definition
from iTasks.WF.Combinators.Core import :: AttachmentStatus

from iTasks.Internal.Task	import exception
from iTasks.Internal.TaskEval import :: TaskTime, :: TaskEvalInfo(..), :: TonicOpts(..)
from iTasks.Internal.Tonic.AbsSyn import :: ExprId (..)
import iTasks.Internal.Serialization, iTasks.Internal.Generic.Visualization
import Data.CircularStack
import Data.Error, Data.Either

derive JSONEncode TIMeta, TIType, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, TonicOpts, CircularStack, AsyncAction
derive JSONDecode TIMeta, TIType, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, TonicOpts, CircularStack, AsyncAction

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

taskIdFromTaskTree :: TaskTree -> MaybeError TaskException TaskId
taskIdFromTaskTree (TCInit          taskId _)         = Ok taskId
taskIdFromTaskTree (TCAwait 		_ taskId _ _ )    = Ok taskId
taskIdFromTaskTree (TCBasic         taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCInteract      taskId _ _ _ _ _) = Ok taskId
taskIdFromTaskTree (TCStep          taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCParallel      taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCShared        taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCAttach        taskId _ _ _ _)   = Ok taskId
taskIdFromTaskTree (TCStable        taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCLayout        _ tt)             = taskIdFromTaskTree tt
taskIdFromTaskTree (TCAttribute     taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCNop)                            = Error (exception "Unable to obtain TaskId from TaskTree (TCNop)")
taskIdFromTaskTree (TCDestroy       tt)               = taskIdFromTaskTree tt
