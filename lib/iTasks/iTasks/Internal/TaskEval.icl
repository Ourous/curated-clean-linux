implementation module iTasks.Internal.TaskEval

import StdList, StdBool, StdTuple, StdMisc, StdString
import Data.Error, Data.Func, Data.Tuple, Data.Either, Data.Functor, Data.List, Text, Text.GenJSON
import iTasks.Internal.IWorld, iTasks.Internal.Task, iTasks.Internal.TaskState, iTasks.Internal.SDS, iTasks.Internal.AsyncSDS
import iTasks.Internal.Store, iTasks.Internal.TaskStore, iTasks.Internal.Util
import iTasks.UI.Layout
import iTasks.Internal.SDSService
import iTasks.Internal.Util
import iTasks.Internal.EngineTasks

from iTasks.WF.Combinators.Core import :: SharedTaskList
import iTasks.WF.Derives
from iTasks.WF.Combinators.Core import :: ParallelTaskType(..), :: ParallelTask(..)
from Data.Map as DM				        import qualified newMap, fromList, toList, get, put, del
from Data.Queue import :: Queue (..)
from Data.Queue as DQ					import qualified newQueue, enqueue, dequeue, empty

import qualified iTasks.Internal.SDS as SDS
from iTasks.SDS.Combinators.Common      import sdsFocus, >*|, mapReadWrite, mapReadWriteError
from StdFunc import const, o
import qualified Data.CircularStack as DCS
from Data.CircularStack import :: CircularStack

derive gEq TIMeta, TIType

mkEvalOpts :: TaskEvalOpts
mkEvalOpts =
	{ TaskEvalOpts
	| noUI     = False
	, taskId   = TaskId 0 0
	, lastEval = 0
	}

getNextTaskId :: *IWorld -> (!TaskId,!*IWorld)
getNextTaskId iworld=:{current=current=:{TaskEvalState|taskInstance,nextTaskNo}}
	= (TaskId taskInstance nextTaskNo, {IWorld|iworld & current = {TaskEvalState|current & nextTaskNo = nextTaskNo + 1}})

processEvents :: !Int *IWorld -> *(!MaybeError TaskException (), !*IWorld)
processEvents max iworld
	| max <= 0 = (Ok (), iworld)
	| otherwise
		= case dequeueEvent iworld of
			(Error e, iworld) = (Error e, iworld)
			(Ok Nothing, iworld) = (Ok (), iworld)
			(Ok (Just (instanceNo,event)), iworld)
				= case evalTaskInstance instanceNo event iworld of
					(Ok taskValue,iworld)
						= processEvents (max - 1) iworld
					(Error msg,iworld=:{IWorld|world})
						= (Ok (),{IWorld|iworld & world = world})

evalTaskInstance :: !InstanceNo !Event !*IWorld -> (!MaybeErrorString (TaskValue DeferredJSON),!*IWorld)
evalTaskInstance instanceNo event iworld
	# iworld            = mbResetUIState instanceNo event iworld
	# (res,iworld)      = evalTaskInstance` instanceNo event (event =: DestroyEvent) iworld
	= (res,iworld)
where
	evalTaskInstance` instanceNo event destroy iworld=:{clock,current}
	// Read the task reduct. If it does not exist, the task has been deleted.
	# (curReduct, iworld)		= 'SDS'.read (sdsFocus instanceNo taskInstanceReduct) EmptyContext iworld
	| isError curReduct			= exitWithException instanceNo ((\(Error (e,msg)) -> msg) curReduct) iworld
	# curReduct = directResult (fromOk curReduct)
	| curReduct =: Nothing      = exitWithException instanceNo ("Task instance does not exist" <+++ instanceNo) iworld
	# curReduct=:{TIReduct|task=(Task eval),nextTaskNo=curNextTaskNo,nextTaskTime,tasks} = fromJust curReduct
	// Determine the task type (startup,session,local) 
	# (type,iworld)             = determineInstanceType instanceNo iworld
	// Determine the progress of the instance
	# (curProgress=:{InstanceProgress|value,attachedTo},iworld) = determineInstanceProgress instanceNo iworld
	//Check exception
	| value =: (Exception _)
		# (Exception description) = value
		= exitWithException instanceNo description iworld
	//Evaluate instance
    # (currentSession,currentAttachment) = case (type,attachedTo) of
        (SessionInstance,_)                       = (Just instanceNo,[])
        (_,[])                                    = (Nothing,[])
        (_,attachment=:[TaskId sessionNo _:_])    = (Just sessionNo,attachment)
	//Update current process id & eval stack in iworld
	# taskId = TaskId instanceNo 0
	# iworld =
		{iworld & current =
			{ taskInstance = instanceNo
			, sessionInstance = currentSession
			, attachmentChain = currentAttachment
			, taskTime = curReduct.TIReduct.nextTaskTime
			, nextTaskNo = curReduct.TIReduct.nextTaskNo
		}}
	//Apply task's eval function and take updated nextTaskId from iworld
	# (newResult,iworld=:{current})	= eval event {mkEvalOpts & lastEval=curReduct.TIReduct.nextTaskTime, taskId=taskId} iworld
	# newTask = case newResult of
		(ValueResult _ _ _ newTask) = newTask
		_                           = Task eval
	# destroyed = newResult =: DestroyedResult
	//Reset necessary 'current' values in iworld
	# iworld = {IWorld|iworld & current = {TaskEvalState|current & taskInstance = 0}}
	// Write the updated progress
	# (mbErr,iworld) = if (destroyed || updateProgress clock newResult curProgress === curProgress)
		(Ok (),iworld)	//Only update progress when something changed
		(case (modify (updateProgress clock newResult) (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld) of
		  (Error e, iworld) = (Error e, iworld)
		  (Ok _, iworld) = (Ok (), iworld) )
	= case mbErr of
		Error (e,description)           = exitWithException instanceNo description iworld
		Ok ()
			//Store or remove reduct
			# (nextTaskNo,iworld)		= getNextTaskNo iworld
			# (_,iworld)                =
				 (modify (maybe Nothing (\r -> if destroyed Nothing (Just {TIReduct|r & task = newTask, nextTaskNo = nextTaskNo, nextTaskTime = nextTaskTime + 1})))
					(sdsFocus instanceNo taskInstanceReduct) EmptyContext iworld)
					//FIXME: Don't write the full reduct (all parallel shares are triggered then!)
			//Store or delete value
			# newValue                  = case newResult of
				ValueResult val _ _ _     = Just (TIValue val)
				ExceptionResult (e,str)   = Just (TIException e str)
				DestroyedResult           = Nothing
			# (mbErr,iworld) = write newValue (sdsFocus instanceNo taskInstanceValue) EmptyContext iworld
			= case mbErr of
				Error (e,description) = exitWithException instanceNo description iworld
				Ok _
					= case newResult of
						ValueResult value _ change _
							| destroyed = (Ok value,iworld)
							| otherwise = case compactUIChange change of
								//Only queue UI changes if something interesting is changed
								NoChange = (Ok value,iworld)
								change   = (Ok value, queueUIChange instanceNo change iworld)
						ExceptionResult (e,description)
							# iworld = if (type =: StartupInstance)
								(printStdErr description {iworld & shutdown=Just 1})
								 iworld
							= exitWithException instanceNo description iworld
						DestroyedResult
							= (Ok NoValue, iworld)

	exitWithException instanceNo description iworld
		# iworld = queueException instanceNo description iworld
		= (Error description, iworld)

	determineInstanceType instanceNo iworld
		# (constants, iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceConstants) EmptyContext iworld
		| isError constants = (SessionInstance,iworld)
		# {InstanceConstants|type} = directResult (fromOk constants)
		= (type,iworld)

	determineInstanceProgress instanceNo iworld
		# (progress,iworld)      = 'SDS'.read (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
		| isOk progress	= (directResult (fromOk progress),iworld)
		| otherwise     = ({InstanceProgress|value=Unstable,instanceKey=Nothing,attachedTo=[],firstEvent=Nothing,lastEvent=Nothing},iworld)

	getNextTaskNo iworld=:{IWorld|current={TaskEvalState|nextTaskNo}} = (nextTaskNo,iworld)

	updateProgress now result progress
		# attachedTo = case progress.InstanceProgress.attachedTo of //Release temporary attachment after first evaluation
			(Just (_,[]))   = Nothing
			attachment      = attachment
		# progress = {InstanceProgress
                     | progress
					 & firstEvent = Just (fromMaybe now progress.InstanceProgress.firstEvent)
					 , lastEvent = Just now
					 }
		= case result of
			(ExceptionResult (_,msg))             = {InstanceProgress|progress & value = Exception msg}
			(ValueResult (Value _ stable) _  _ _) = {InstanceProgress|progress & value = if stable Stable Unstable}
			_                                     = {InstanceProgress|progress & value = Unstable }

	mbResetUIState instanceNo ResetEvent iworld
		# (_,iworld) = write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceOutput) EmptyContext iworld
		= iworld

	mbResetUIState _ _ iworld = iworld
/*
	//TODO: Move remove to Taskeval after a destroy
	//Delete all states on disk
	# (mbe,iworld)    = 'SDS'.write Nothing (sdsFocus instanceNo taskInstanceReduct) 'SDS'.EmptyContext iworld
	| mbe =: (Error _) = (toWE mbe,iworld)
	# (mbe,iworld)    = 'SDS'.write Nothing (sdsFocus instanceNo taskInstanceValue) 'SDS'.EmptyContext iworld
	| mbe =: (Error _) = (toWE mbe,iworld)
	# (mbe,iworld)    = 'SDS'.write Nothing (sdsFocus instanceNo taskInstanceShares) 'SDS'.EmptyContext iworld
	| mbe =: (Error _) = (toWE mbe,iworld)
	# (mbe,iworld)    = 'SDS'.write Nothing (sdsFocus instanceNo taskInstanceParallelTaskLists) 'SDS'.EmptyContext iworld
	| mbe =: (Error _) = (toWE mbe,iworld)
	= (Ok (),iworld)
*/

updateInstanceLastIO ::![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceLastIO [] iworld = (Ok (),iworld)
updateInstanceLastIO [instanceNo:instanceNos] iworld=:{IWorld|clock}
	= case modify (\io -> fmap (appSnd (const clock)) io) (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
		(Ok (ModifyingDone _),iworld) = updateInstanceLastIO instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

updateInstanceConnect :: !String ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceConnect client [] iworld = (Ok (),iworld)
updateInstanceConnect client [instanceNo:instanceNos] iworld=:{IWorld|clock}
	= case write (Just (client,clock)) (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
		(Ok _,iworld) = updateInstanceConnect client instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

updateInstanceDisconnect :: ![InstanceNo] !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateInstanceDisconnect [] iworld = (Ok (),iworld)
updateInstanceDisconnect [instanceNo:instanceNos] iworld=:{IWorld|clock}
	= case modify (\io -> fmap (appSnd (const clock)) io) (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
		(Ok (ModifyingDone _),iworld) = updateInstanceDisconnect instanceNos iworld
		(Error e,iworld) = (Error e,iworld)

currentInstanceShare :: SDSSource () InstanceNo ()
currentInstanceShare = createReadOnlySDS (\() iworld=:{current={TaskEvalState|taskInstance}} -> (taskInstance,iworld))
