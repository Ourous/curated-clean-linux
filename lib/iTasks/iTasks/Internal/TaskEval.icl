implementation module iTasks.Internal.TaskEval

import StdList, StdBool, StdTuple, StdMisc, StdString
import Data.Error, Data.Func, Data.Tuple, Data.Either, Data.Functor, Data.List, Text, Text.GenJSON
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.TaskState
import iTasks.Internal.Store, iTasks.Internal.TaskStore, iTasks.Internal.Util
import iTasks.UI.Layout
import iTasks.Internal.SDSService
import iTasks.Internal.Util

from iTasks.WF.Combinators.Core import :: SharedTaskList
from iTasks.WF.Combinators.Core import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map as DM				        import qualified newMap, fromList, toList, get, put, del
from Data.Queue import :: Queue (..)
from Data.Queue as DQ					import qualified newQueue, enqueue, dequeue, empty
from iTasks.Internal.SDS as SDS       import qualified read, write, modify
from iTasks.SDS.Combinators.Common      import sdsFocus, >+|, mapReadWrite, mapReadWriteError
from StdFunc import const
import qualified Data.CircularStack as DCS
from Data.CircularStack import :: CircularStack
from iTasks.Internal.Tonic.AbsSyn import :: ExprId (..)

derive gEq TIMeta

mkEvalOpts :: TaskEvalOpts
mkEvalOpts =
  { TaskEvalOpts
  | noUI        = False
  , tonicOpts   = defaultTonicOpts
  }

defaultTonicOpts :: TonicOpts
defaultTonicOpts = { TonicOpts
                   | inAssignNode            = Nothing
                   , inParallel              = Nothing
                   , captureParallel         = False
                   , currBlueprintModuleName = ""
                   , currBlueprintFuncName   = ""
                   , currBlueprintTaskId     = TaskId 0 0
                   , currBlueprintExprId     = []
                   , callTrace               = 'DCS'.newStack 1024
                   }

extendCallTrace :: !TaskId !TaskEvalOpts -> TaskEvalOpts
extendCallTrace taskId repOpts=:{TaskEvalOpts|tonicOpts = {callTrace = xs}}
  = case 'DCS'.peek xs of
      Just topTaskId
        | taskId == topTaskId = repOpts
      _ = {repOpts & tonicOpts = {repOpts.tonicOpts & callTrace = 'DCS'.push taskId repOpts.tonicOpts.callTrace}}


getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
    = (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

processEvents :: !Int *IWorld -> *(!MaybeError TaskException (), !*IWorld)
processEvents max iworld
	| max <= 0 = (Ok (), iworld)
	| otherwise
		= case dequeueEvent iworld of 
			(Nothing,iworld) = (Ok (),iworld)
			(Just (instanceNo,event),iworld)
				= case evalTaskInstance instanceNo event iworld of
					(Ok taskValue,iworld)
						= processEvents (max - 1) iworld
					(Error msg,iworld=:{IWorld|world})
						= (Ok (),{IWorld|iworld & world = world})

//Evaluate a single task instance
evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (TaskValue DeferredJSON),!*IWorld)
evalTaskInstance instanceNo event iworld
    # iworld            = mbResetUIState instanceNo event iworld
    # (res,iworld)      = evalTaskInstance` instanceNo event iworld
    = (res,iworld)
where
    evalTaskInstance` instanceNo event iworld=:{clock,current}
    # (constants, iworld)       = 'SDS'.read (sdsFocus instanceNo taskInstanceConstants) iworld
	| isError constants         = exitWithException instanceNo ((\(Error (e,msg)) -> msg) constants) iworld
	# constants=:{InstanceConstants|session,listId} = fromOk constants
	# (oldReduct, iworld)		= 'SDS'.read (sdsFocus instanceNo taskInstanceReduct) iworld
	| isError oldReduct			= exitWithException instanceNo ((\(Error (e,msg)) -> msg) oldReduct) iworld
	# oldReduct=:{TIReduct|task=Task eval,tree,nextTaskNo=curNextTaskNo,nextTaskTime,tasks,tonicRedOpts} = fromOk oldReduct
    # (oldProgress,iworld)      = 'SDS'.read (sdsFocus instanceNo taskInstanceProgress) iworld
	| isError oldProgress       = exitWithException instanceNo ((\(Error (e,msg)) -> msg) oldProgress) iworld
    # oldProgress=:{InstanceProgress|value,attachedTo} = fromOk oldProgress
    //Check exeption
    | value =: (Exception _)
		# (Exception description) = value
		= exitWithException instanceNo description iworld
	//Eval instance
    # (currentSession,currentAttachment) = case (session,attachedTo) of
        (True,_)                                  = (Just instanceNo,[])
        (_,[])                                    = (Nothing,[])
        (_,attachment=:[TaskId sessionNo _:_])    = (Just sessionNo,attachment)
	//Update current process id & eval stack in iworld
	# taskId					= TaskId instanceNo 0
	# iworld					= {iworld & current =
                                        { taskInstance = instanceNo
                                        , sessionInstance = currentSession
                                        , attachmentChain = currentAttachment
										, taskTime = oldReduct.TIReduct.nextTaskTime
                                        , nextTaskNo = oldReduct.TIReduct.nextTaskNo
										}}
	//Apply task's eval function and take updated nextTaskId from iworld
	# (newResult,iworld=:{current})	= eval event {mkEvalOpts & tonicOpts = tonicRedOpts} tree iworld
    # tree                      = case newResult of
        (ValueResult _ _ _ newTree)  = newTree
        _                            = tree
    //Reset necessary 'current' values in iworld
    # iworld = {IWorld|iworld & current = {TaskEvalState|current & taskInstance = 0}}
    // Check if instance was deleted by trying to reread the instance constants share
	# (deleted,iworld) = appFst isError ('SDS'.read (sdsFocus instanceNo taskInstanceConstants) iworld)
    // Write the updated progress
	# (mbErr,iworld) = if (updateProgress clock newResult oldProgress === oldProgress)
		(Ok (),iworld)	//Only update progress when something changed
   		('SDS'.modify (\p -> ((),updateProgress clock newResult p)) (sdsFocus instanceNo taskInstanceProgress) iworld)
    = case mbErr of
        Error (e,description)          
			= exitWithException instanceNo description iworld
        Ok _
            //Store updated reduct
            # (nextTaskNo,iworld)		= getNextTaskNo iworld
            # (_,iworld)                = 'SDS'.modify (\r -> ((),{TIReduct|r & tree = tree, nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1}))
                                                (sdsFocus instanceNo taskInstanceReduct) iworld
												//FIXME: Don't write the full reduct (all parallel shares are triggered then!)
            //Store update value
            # newValue                  = case newResult of
                (ValueResult val _ _ _)     = TIValue val
                (ExceptionResult (e,str))   = TIException e str
            # (mbErr,iworld)            = if deleted (Ok (),iworld) ('SDS'.write newValue (sdsFocus instanceNo taskInstanceValue) iworld)
            = case mbErr of
                Error (e,description) = exitWithException instanceNo description iworld
                Ok _
                	= case newResult of
                    	(ValueResult value _ change _)	
							| deleted
								= (Ok value,iworld)
							//Only queue UI changes if something interesting is changed
							= case compactUIChange change of
								NoChange = (Ok value,iworld)
								change
									# iworld = queueUIChange instanceNo change iworld
									= (Ok value, iworld)
                    	(ExceptionResult (e,description))
							= exitWithException instanceNo description iworld

	exitWithException instanceNo description iworld
		# iworld = queueException instanceNo description iworld
		= (Error description, iworld)

	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}}	    = (nextTaskNo,iworld)

	updateProgress now result progress
        # attachedTo = case progress.InstanceProgress.attachedTo of //Release temporary attachment after first evaluation
            (Just (_,[]))   = Nothing
            attachment      = attachment
		# progress = {InstanceProgress|progress
					 &firstEvent = Just (fromMaybe now progress.InstanceProgress.firstEvent)
					 ,lastEvent = Just now
					 }
		= case result of
			(ExceptionResult (_,msg))             = {InstanceProgress|progress & value = Exception msg}
			(ValueResult (Value _ stable) _  _ _) = {InstanceProgress|progress & value = if stable Stable Unstable}
			_                                     = {InstanceProgress|progress & value = Unstable }

    mbResetUIState instanceNo ResetEvent iworld 
		# (_,iworld) = 'SDS'.write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceOutput) iworld 
		//Remove all js compiler state for this instance
		# iworld=:{jsCompilerState=jsCompilerState} = iworld
		# jsCompilerState = fmap (\state -> {state & skipMap = 'DM'.del instanceNo state.skipMap}) jsCompilerState
		# iworld = {iworld & jsCompilerState = jsCompilerState}
		= iworld

    mbResetUIState _ _ iworld = iworld

updateInstanceLastIO ::![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceLastIO [] iworld = (Ok (),iworld)
updateInstanceLastIO [instanceNo:instanceNos] iworld=:{IWorld|clock}
    = case 'SDS'.modify (\io -> ((),fmap (appSnd (const clock)) io)) (sdsFocus instanceNo taskInstanceIO) iworld of
    	(Ok (),iworld) = updateInstanceLastIO instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceConnect client [] iworld = (Ok (),iworld)
updateInstanceConnect client [instanceNo:instanceNos] iworld=:{IWorld|clock}
    = case 'SDS'.write (Just (client,clock)) (sdsFocus instanceNo taskInstanceIO) iworld of
		(Ok (),iworld) = updateInstanceConnect client instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceDisconnect [] iworld = (Ok (),iworld)
updateInstanceDisconnect [instanceNo:instanceNos] iworld=:{IWorld|clock}
    = case 'SDS'.modify (\io -> ((),fmap (appSnd (const clock)) io)) (sdsFocus instanceNo taskInstanceIO) iworld of
		(Ok (),iworld) = updateInstanceDisconnect instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

currentInstanceShare :: ReadOnlyShared InstanceNo
currentInstanceShare = createReadOnlySDS (\() iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))
