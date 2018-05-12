implementation module iTasks.Internal.TaskState

import Text.GenJSON, StdString
import iTasks.UI.Definition
import iTasks.WF.Definition
from iTasks.WF.Combinators.Core import :: AttachmentStatus

from iTasks.Internal.Task	import exception
from iTasks.Internal.TaskEval import :: TaskTime, :: TaskEvalInfo(..), :: TonicOpts(..)
from iTasks.Internal.Tonic.AbsSyn import :: ExprId (..)
import iTasks.Internal.Serialization
import Data.CircularStack
import Data.Error, Data.Either

derive JSONEncode TIMeta, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, TonicOpts, CircularStack
derive JSONDecode TIMeta, TIValue, TIReduct, TaskTree, ParallelTaskState, ParallelTaskChange, TaskResult, TaskEvalInfo, TonicOpts, CircularStack

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

taskIdFromTaskTree :: TaskTree -> MaybeError TaskException TaskId
taskIdFromTaskTree (TCInit                  taskId _)         = Ok taskId
taskIdFromTaskTree (TCBasic                 taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCInteract              taskId _ _ _ _)   = Ok taskId
taskIdFromTaskTree (TCProject               taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCStep                  taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCParallel              taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCShared                taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCAttach                taskId _ _ _ _)   = Ok taskId
taskIdFromTaskTree (TCExposedShared         taskId _ _ _)     = Ok taskId
taskIdFromTaskTree (TCStable                taskId _ _)       = Ok taskId
taskIdFromTaskTree (TCLayout                _ tt)             = taskIdFromTaskTree tt
taskIdFromTaskTree (TCNop)                                    = Error (exception "Unable to obtain TaskId from TaskTree (TCNop)")
taskIdFromTaskTree (TCDestroy               tt)               = taskIdFromTaskTree tt

