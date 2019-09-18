implementation module iTasks.WF.Combinators.Core

import iTasks.WF.Tasks.Core
import StdEnv

import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition

import iTasks.Engine
import iTasks.Internal.EngineTasks
import iTasks.Internal.DynamicUtil
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import iTasks.Internal.Util
import iTasks.Internal.AsyncSDS

from iTasks.SDS.Combinators.Common import sdsFocus, sdsSplit, sdsTranslate, toReadOnly, mapRead, mapReadWriteError, mapSingle, removeMaybe
import iTasks.WF.Combinators.Common
from iTasks.Internal.SDS import write, read, readRegister, modify

import iTasks.WF.Tasks.System

import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Queue as DQ

import Data.Maybe, Data.Either, Data.Error, Data.Func
import Text.GenJSON
from Data.Functor import <$>, class Functor(fmap)
from Data.Map import qualified instance Functor (Map k)

derive gEq ParallelTaskChange

:: Action	= Action !String //Locally unique identifier for actions

:: ParallelTaskType
	= Embedded                                    //Simplest embedded
	| Detached !TaskAttributes !Bool              //Management meta and flag whether the task should be started at once

:: ParallelTask a	:== (SharedTaskList a) -> Task a

// Data available to parallel sibling tasks
:: TaskList a :== (!TaskId,![TaskListItem a])
:: SharedTaskList a :== SDSLens TaskListFilter (!TaskId,![TaskListItem a]) [(TaskId,TaskAttributes)]

:: TaskListItem a =
	{ taskId			:: !TaskId
    , listId            :: !TaskId
    , detached          :: !Bool
    , self              :: !Bool
	, value				:: !TaskValue a
	, attributes        :: !TaskAttributes
	, progress		    :: !Maybe InstanceProgress //Only possible for detached tasks
	}

:: TaskListFilter =
    //Which rows to filter
    { onlyIndex         :: !Maybe [Int]
    , onlyTaskId        :: !Maybe [TaskId]
    , onlySelf          :: !Bool
    //What to include
    , includeValue      :: !Bool
    , includeAttributes :: !Bool
    , includeProgress   :: !Bool
    }
derive gDefault TaskListFilter, TaskId

instance toString AttachException
where
	toString InstanceNotFound	= "Cannot find task instance to attach"
	toString InstanceEvalError	= "Error in attached task instance "

derive class iTask AttachException

transformError :: ((TaskValue a) -> MaybeError TaskException (TaskValue b)) !(Task a) -> Task b
transformError f task = Task (eval task)
where
	eval (Task task) event evalOpts iworld = case task event evalOpts iworld of
		(ValueResult val lastEvent rep task, iworld) = case f val of
			Error e = (ExceptionResult e, iworld)
			Ok v = (ValueResult v lastEvent rep (Task (eval task)), iworld)
		(ExceptionResult e, iworld)                  = (ExceptionResult e, iworld)
		(DestroyedResult, iworld)                    = (DestroyedResult, iworld)

removeDupBy :: (a a -> Bool) [a] -> [a]
removeDupBy eq [x:xs] = [x:removeDupBy eq (filter (not o eq x) xs)]
removeDupBy _ [] = []

step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | TC, JSONEncode{|*|} a
step lhs lhsValFun conts = Task evalinit
where
	//Initial setup:
	//Destroyed before first evaluation
	//evalinit :: !Event !TaskEvalOpts !*IWorld -> *(TaskResult a, !*IWorld)
	evalinit DestroyEvent evalOpts iworld
		= (DestroyedResult,iworld)
	//Check for duplicates
	evalinit event evalOpts iworld
		# iworld = if (length (removeDupBy actionEq conts) == length conts)
			iworld
			(printStdErr "Duplicate actions in step" iworld)
		# (taskIda, iworld) = getNextTaskId iworld
		= evalleft lhs [] taskIda event evalOpts iworld
	where
		actionEq (OnAction (Action a) _) (OnAction (Action b) _) = a == b
		actionEq _ _ = False

	//Evaluating the lhs
	//Destroyed when executing the lhs
	//evalleft :: (Task a) [String] TaskId Event TaskEvalOpts !*IWorld -> *(TaskResult a, IWorld)
	evalleft (Task lhs) prevEnabledActions leftTaskId DestroyEvent evalOpts iworld
		= case lhs DestroyEvent {TaskEvalOpts|evalOpts&taskId=leftTaskId} iworld of
			(DestroyedResult, iworld)    = (DestroyedResult, iworld)
			(ExceptionResult e, iworld)  = (ExceptionResult e, iworld)
			(ValueResult _ _ _ _,iworld) = (ExceptionResult (exception "Failed destroying lhs in step"), iworld)
	//Execute lhs
	evalleft (Task lhs) prevEnabledActions leftTaskId event evalOpts=:{lastEval,taskId} iworld
		# mbAction = matchAction taskId event
		# (res, iworld) = lhs event {TaskEvalOpts|evalOpts&taskId=leftTaskId} iworld
		// Right  is a step
		# (mbCont, iworld) = case res of
			ValueResult val info rep lhs
				= case searchContValue val mbAction conts of
					//No match
					Nothing
						# info = {TaskEvalInfo|info & lastEvent = max lastEval info.TaskEvalInfo.lastEvent}
						# value = maybe NoValue (\v -> Value v False) (lhsValFun (case val of Value v _ = Just v; _ = Nothing))
						# actions = contActions taskId val conts
						# curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
						# sl = wrapStepUI taskId evalOpts event actions prevEnabledActions val rep
						= (Left (ValueResult
							value
							info
							sl
							(Task (evalleft lhs curEnabledActions leftTaskId))
							)
						, iworld)
					//A match
					Just rewrite
						//Send a destroyevent to the lhs
						# (_, iworld) = (unTask lhs) DestroyEvent {TaskEvalOpts|evalOpts&taskId=leftTaskId} iworld
						= (Right (rewrite, info.TaskEvalInfo.lastEvent, info.TaskEvalInfo.removedTasks), iworld)
			ExceptionResult e
				= case searchContException e conts of
					//No match
					Nothing      = (Left (ExceptionResult e), iworld)
					//A match
					Just rewrite = (Right (rewrite, lastEval, []), iworld)
		= case mbCont of
			//No match, just pass through
			Left res = (res, iworld)
			//A match, continue with the matched rhs
			Right ((_, (Task rhs), _), lastEvent, removedTasks)
				//Execute the rhs with a reset event
				# (resb, iworld)        = rhs ResetEvent evalOpts iworld
				= case resb of
					ValueResult val info change=:(ReplaceUI _) (Task rhs)
						# info = {TaskEvalInfo|info & lastEvent = max lastEvent info.TaskEvalInfo.lastEvent, removedTasks = removedTasks ++ info.TaskEvalInfo.removedTasks}
						= (ValueResult
							val
							info
							change
							//Actually rewrite to the rhs
							(Task rhs)
						,iworld)
					ValueResult _ _ change _
						= (ExceptionResult (exception ("Reset event of task in step failed to produce replacement UI: ("+++ toString (toJSON change)+++")")), iworld)
					ExceptionResult e = (ExceptionResult e, iworld)

	wrapStepUI taskId evalOpts event actions prevEnabled val change
		| actionUIs =: []
			= case (event,change) of
				(ResetEvent,ReplaceUI (UI type attributes items)) //Mark the ui as a step
					= ReplaceUI (UI type (addClassAttr "step" attributes) items)
				_
					= change
		| otherwise	//Wrap in a container
			= case (event,change) of
				(ResetEvent,ReplaceUI ui) //On reset generate a new step UI
					= ReplaceUI (uiac UIContainer (classAttr ["step-actions"]) [ui:actionUIs])
				_  //Otherwise create a compound change definition
					= ChangeUI [] [(0,ChangeChild change):actionChanges]
	where
		actionUIs = contActions taskId val conts
		actionChanges = [(i,ChangeChild (switch (isEnabled ui) (actionId ui))) \\ ui <- actions & i <- [1..]]
		where
			switch True name = if (isMember name prevEnabled) NoChange (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])
			switch False name = if (isMember name prevEnabled) (ChangeUI [SetAttribute "enabled" (JSONBool False)] []) NoChange

matchAction :: TaskId Event -> Maybe String
matchAction taskId (ActionEvent matchId action)
	| matchId == taskId = Just action
matchAction taskId _ = Nothing

isEnabled (UI _ attr _) = maybe False (\(JSONBool b) -> b) ('DM'.get "enabled" attr)
actionId (UI _ attr _) = maybe "" (\(JSONString s) -> s) ('DM'.get "actionId" attr)

contActions :: TaskId (TaskValue a) [TaskCont a b]-> [UI]
contActions taskId val conts = [actionUI (isJust (taskbf val)) action\\ OnAction action taskbf <- conts]
where
	actionUI enabled action=:(Action actionId)
		= uia UIAction ('DM'.unions [enabledAttr enabled, taskIdAttr (toString taskId), actionIdAttr actionId])

searchContValue :: (TaskValue a) (Maybe String) [TaskCont a b] -> Maybe (!Int, !b, !DeferredJSON) | TC a & JSONEncode{|*|} a
searchContValue val mbAction conts
	= search val mbAction 0 Nothing conts
where
	search _ _ _ mbMatch []							= mbMatch		//No matching OnValue steps were found, return the potential match
	search val mbAction i mbMatch [OnValue f:cs]
		= case f val of
			Just cont	= Just (i, cont, DeferredJSON val)			//Don't look any further, first matching trigger wins
			Nothing		= search val mbAction (i + 1) mbMatch cs	//Keep search
	search val mbAction=:(Just actionEvent) i Nothing [OnAction (Action actionName) f:cs]
		| actionEvent == actionName
			= case f val of
				Just cont = search val mbAction (i + 1) (Just (i, cont, DeferredJSON val)) cs 	//We found a potential winner (if no OnValue values are in cs)
				Nothing = search val mbAction (i + 1) Nothing cs								//Keep searching
		= search val mbAction (i + 1) Nothing cs								//Keep searching
	search val mbAction i mbMatch [_:cs]			= search val mbAction (i + 1) mbMatch cs		//Keep searching

searchContException :: (Dynamic,String) [TaskCont a b] -> Maybe (Int, !b, !DeferredJSON)
searchContException (dyn,str) conts = search dyn str 0 Nothing conts
where
	search _ _ _ catchall []                        = catchall                                                        //Return the maybe catchall
	search dyn str i catchall [OnException f:cs]    = case (match f dyn) of
		Just (taskb,enca) = Just (i, taskb, enca)                                            //We have a match
		_                 = search dyn str (i + 1) catchall cs                            //Keep searching
	search dyn str i Nothing [OnAllExceptions f:cs] = search dyn str (i + 1) (Just (i, f str, DeferredJSON str)) cs //Keep searching (at least we have a catchall)
	search dyn str i mbcatchall [_:cs]              = search dyn str (i + 1) mbcatchall cs                            //Keep searching

	match :: (e -> b) Dynamic -> Maybe (b, DeferredJSON) | iTask e
	match f (e :: e^)    = Just (f e, DeferredJSON e)
	match _ _            = Nothing

// Parallel composition
parallel :: ![(ParallelTaskType,ParallelTask a)] [TaskCont [(Int,TaskValue a)] (ParallelTaskType,ParallelTask a)] -> Task [(Int,TaskValue a)] | iTask a
parallel initTasks conts = Task evalinit
where
	//Destroyed before initial execution
	evalinit DestroyEvent _ iworld
		= (DestroyedResult, iworld)
	//Initialize the task list
	evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld
	//Create the states for the initial tasks
		= case initParallelTasks evalOpts taskId 0 initTasks iworld of
			(Ok (taskList,embeddedTasks),iworld)
				//Write the local task list
				# taskListFilter = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
				# (e,iworld) = (write taskList (sdsFocus (taskId, taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld)
				| isError e = (ExceptionResult (fromError e),iworld)
				//Write the local embedded tasks
				# (e,iworld) = writeAll embeddedTasks taskInstanceEmbeddedTask iworld
				| isError e = (ExceptionResult (fromError e),iworld)
				//Evaluate the parallel
				= eval (length embeddedTasks) [] event evalOpts iworld
			(Error err,iworld)
				= (ExceptionResult err, iworld)
	where
		writeAll [] sds iworld = (Ok WritingDone,iworld)
		writeAll [(f,w):ws] sds iworld
			= case (write w (sdsFocus f sds) EmptyContext iworld) of
				(Ok WritingDone, iworld) = writeAll ws sds iworld
				(Ok _, iworld) = (Error (exception "Asynchronous tasklist share???"), iworld)
				err = err

		initParallelTasks _ _ _ [] iworld = (Ok ([],[]),iworld)
		initParallelTasks evalOpts listId index [(parType,parTask):parTasks] iworld
			# (mbStateMbTask, iworld) = initParallelTask evalOpts listId index parType parTask iworld
			= case mbStateMbTask of
					Ok (state,mbTask)
						# (mbStateTasks, iworld) = initParallelTasks evalOpts listId (index + 1) parTasks iworld
						= case mbStateTasks of
								Ok (states,tasks)
									= (Ok ([state:states], maybe tasks (\task -> [task:tasks]) mbTask), iworld)
								err = (err, iworld)
					err = (liftError err, iworld)

	eval _ _ DestroyEvent {TaskEvalOpts|taskId} iworld
		= destroyParallelTasks taskId iworld

	//Evaluate the task list
	eval prevNumBranches prevEnabledActions event evalOpts=:{TaskEvalOpts|taskId} iworld
		//Evaluate all branches of the parallel set
		= case evalParallelTasks event evalOpts conts [] [] iworld of
			(Ok results, iworld)
				//Construct the result
				# results   = reverse results //(the results are returned in reverse order)
				# value     = genParallelValue results
				# evalInfo  = genParallelEvalInfo results
				# actions   = contActions taskId value conts
				# rep       = genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
				# curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
				# curNumBranches = length [()\\(ValueResult _ _ _ _)<-results]
				= (ValueResult value evalInfo rep (Task (eval curNumBranches curEnabledActions)), iworld)
			//Stopped because of an unhandled exception
			(Error e, iworld)
				//Clean up before returning the exception
				# (res,iworld) = destroyParallelTasks taskId iworld
				= (exceptionResult res e,iworld)
	where
		exceptionResult :: (TaskResult [(Int,TaskValue a)]) TaskException -> (TaskResult [(Int,TaskValue a)])
		exceptionResult DestroyedResult e = ExceptionResult e
		exceptionResult (ExceptionResult _) e = ExceptionResult e

		genParallelEvalInfo :: [TaskResult a] -> TaskEvalInfo
		genParallelEvalInfo results = foldr addResult {TaskEvalInfo|lastEvent=0,removedTasks=[]} results
		where
			addResult (ValueResult _ i1 _ _) i2
				# lastEvent = max i1.TaskEvalInfo.lastEvent i2.TaskEvalInfo.lastEvent
				# removedTasks = i1.TaskEvalInfo.removedTasks ++ i2.TaskEvalInfo.removedTasks
				= {TaskEvalInfo|lastEvent=lastEvent,removedTasks=removedTasks}
			addResult _ i = i


initParallelTask ::
	!TaskEvalOpts
	!TaskId
	!Int
	!ParallelTaskType
	!(ParallelTask a)
	!*IWorld
	->
	(!MaybeError TaskException (ParallelTaskState, Maybe (TaskId,Task a)), !*IWorld)
	| iTask a
initParallelTask evalOpts listId index parType parTask iworld=:{current={taskTime}}
	# (mbTaskStuff,iworld) = case parType of
		Embedded                                 = mkEmbedded 'DM'.newMap iworld
		Detached           attributes evalDirect = mkDetached attributes evalDirect iworld
	= case mbTaskStuff of
		Ok (taskId,attributes,mbTask)
			# state =
				{ ParallelTaskState
				| taskId      = taskId
				, index       = index
				, detached    = isNothing mbTask
				, implicitAttributes = 'DM'.newMap
				, explicitAttributes = fmap (\x -> (x,True)) attributes
				, value       = NoValue
				, createdAt   = taskTime
				, lastEvent   = taskTime
				, change      = Nothing
				, initialized = False
				}
			= (Ok (state,mbTask),iworld)
		err = (liftError err, iworld)
where
	mkEmbedded attributes iworld
		# (taskId,iworld) = getNextTaskId iworld
		# task            = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
		= (Ok (taskId, attributes, Just (taskId,task)), iworld)
	mkDetached attributes evalDirect iworld
		# (mbInstanceNo,iworld) = newInstanceNo iworld
		= case mbInstanceNo of
			Ok instanceNo
				# isTopLevel        = listId == TaskId 0 0
				# listShare         = if isTopLevel topLevelTaskList (sdsTranslate "setTaskAndList" (\listFilter -> (listId,TaskId instanceNo 0,listFilter)) parallelTaskList)
				# (mbTaskId,iworld) = createDetachedTaskInstance (parTask listShare) isTopLevel evalOpts instanceNo attributes listId evalDirect iworld
				= case mbTaskId of
					Ok taskId = (Ok (taskId, attributes, Nothing), iworld)
					err       = (liftError err, iworld)
			err = (liftError err, iworld)

evalParallelTasks :: !Event !TaskEvalOpts
	[TaskCont [(TaskTime,TaskValue a)] (ParallelTaskType,ParallelTask a)]
	[(TaskId, TaskResult a)] [ParallelTaskState] !*IWorld
	->
	(MaybeError TaskException [TaskResult a],!*IWorld) | iTask a
evalParallelTasks event evalOpts=:{TaskEvalOpts|taskId=listId} conts completed [] iworld
	//(re-)read the tasklist to check if it contains items we have not yet evaluated
	# taskListFilter        = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
	# (mbList,iworld)       = read (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
	| mbList =:(Error _)    = (Error (fromError mbList),iworld)
	= case drop (length completed) (directResult (fromOk mbList)) of
		//We are done, unless we have continuations that extend the set
		[]  = case searchContValue (genParallelValue (reverse (map snd completed))) (matchAction listId event) conts of
			Nothing //We have evaluated all branches and nothing is added
				//Remove all entries that are marked as removed from the list, they have been cleaned up by now
				# taskListFilter      = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=True,includeProgress=False}

                # (mbError,iworld)      = modify (\l -> [clearExplicitAttributeChange x \\ x <- l | not (isRemoved x)])
											(sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
				| mbError =:(Error _) = (Error (fromError mbError),iworld)
				//Bit of a hack... find updated attributes
				# completed = reverse [(t,addAttributeChanges explicitAttributes c) \\ (t,c) <- reverse completed & {ParallelTaskState|explicitAttributes} <- directResult (fromOk mbList) ]
                = (Ok (map snd completed),iworld)

			Just (_,(type,task),_) //Add extension
				# (mbStateMbTask, iworld) = initParallelTask evalOpts listId 0 type task iworld
				= case mbStateMbTask of
					Ok (state,mbTask)
					  //Update the task list (TODO, be specific about what we are writing here)
					  # taskListFilter            = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
					  # (mbError,iworld)          = modify (\states -> states ++ [{ParallelTaskState|state & index = length states}]) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
					  | mbError =:(Error _)       = (liftError mbError,iworld)
					  # taskId                    = state.ParallelTaskState.taskId
					  //Store the task function
					  # (mbError,iworld)          = (write (snd (fromJust mbTask)) (sdsFocus taskId taskInstanceEmbeddedTask) EmptyContext iworld)
					  | mbError =:(Error _)       = (liftError mbError,iworld)
					  = evalParallelTasks ResetEvent evalOpts conts completed [state] iworld //Continue
					err = (liftError err, iworld)
		//There is more work to do:
		todo    = evalParallelTasks event evalOpts conts completed todo iworld
where
	isRemoved {ParallelTaskState|change=Just RemoveParallelTask} = True
	isRemoved _ = False

	addAttributeChanges explicitAttributes (ValueResult val evalInfor rep tree)
		//Add the explicit attributes
		# rep = case rep of
			ReplaceUI (UI type attr items)
				# expAtt = 'DM'.fromList [(k,v) \\ (k,(v,True)) <- 'DM'.toList explicitAttributes]
				= (ReplaceUI (UI type ('DM'.union expAtt attr) items))
			ChangeUI attrChanges itemChanges
				# expChanges = [SetAttribute k v \\ (k,(v,True)) <- 'DM'.toList explicitAttributes]
				= (ChangeUI (attrChanges ++ expChanges) itemChanges)
			NoChange = case [SetAttribute k v \\ (k,(v,True)) <- 'DM'.toList explicitAttributes] of
				[] = NoChange
				attrChanges = (ChangeUI attrChanges [])
		= (ValueResult val evalInfor rep tree)
	addAttributeChanges explicitAttributes c = c

	clearExplicitAttributeChange pts=:{ParallelTaskState|explicitAttributes} = {pts & explicitAttributes = fmap (\(v,_) -> (v,False)) explicitAttributes}

//Evaluate an embedded parallel task
evalParallelTasks event evalOpts=:{TaskEvalOpts|taskId=listId} conts completed [t=:{ParallelTaskState|taskId=taskId=:(TaskId _ taskNo)}:todo] iworld
	= case evalParallelTask listId event evalOpts t iworld of
		(Error e, iworld) = (Error e,iworld)
		(Ok (ExceptionResult e), iworld) = (Error e,iworld) //Stop on exceptions
		(Ok result=:(ValueResult val {TaskEvalInfo|lastEvent,removedTasks} rep task), iworld)
			//Add the current result before checking for removals
			# completed = [(taskId, result):completed]
			//Check if in the branch tasks from this list were removed but that were already evaluated
			# removed = [t \\ (l,t=:(TaskId _ n)) <- removedTasks | l == listId && n <= taskNo]
			# (completed,iworld) = destroyRemoved listId removed completed iworld
			= evalParallelTasks event evalOpts conts completed todo iworld
		(Ok result=:DestroyedResult, iworld)
			= evalParallelTasks event evalOpts conts [(taskId, result):completed] todo iworld
where
	evalParallelTask :: TaskId !Event !TaskEvalOpts ParallelTaskState !*IWorld
		-> *(MaybeError TaskException (TaskResult a), !*IWorld) | iTask a
	evalParallelTask listId event evalOpts taskState=:{ParallelTaskState|detached} iworld
		| detached  = evalDetachedParallelTask listId event evalOpts taskState iworld
		            = evalEmbeddedParallelTask listId event evalOpts taskState iworld

	evalEmbeddedParallelTask listId event evalOpts
		{ParallelTaskState|taskId,createdAt,value,change,initialized} iworld=:{current={taskTime}}
		//Lookup task evaluation function and task evaluation state
		# (mbTask,iworld) = read (sdsFocus taskId taskInstanceEmbeddedTask) EmptyContext iworld
		| mbTask =:(Error _) = (Error (fromError mbTask),iworld)
		# (Task evala) = directResult (fromOk mbTask)
		//Evaluate or destroy branch
		| change === Just RemoveParallelTask
			# (result, iworld) = destroyEmbeddedParallelTask listId taskId iworld
			= case result of
				(Ok res) = (Ok res,iworld)
				(Error e) = (Error (exception (ExceptionList e)), iworld)
		//Evaluate new branches with a reset event, other with the event
		= case evala (if initialized event ResetEvent) {TaskEvalOpts|evalOpts&taskId=taskId} iworld of
			//If an exception occured, check if we can handle it at this level
			(ExceptionResult e, iworld)
				//TODO Check exception
				//If the exception can not be handled, don't continue evaluating just stop
				= (Ok (ExceptionResult e),iworld)
			(ValueResult val evalInfo=:{TaskEvalInfo|lastEvent,removedTasks} rep task, iworld)
				# result = ValueResult val evalInfo rep task
				# implicitAttributeUpdate = case rep of
					ReplaceUI (UI _ attributes _) = const attributes
					ChangeUI changes _ = \a -> foldl (flip applyUIAttributeChange) a changes
					_ = id
				//Check if the value changed
				# valueChanged = val =!= decode value
				//Write the new reduct
				# (mbError, iworld) = write task (sdsFocus taskId taskInstanceEmbeddedTask) EmptyContext iworld
				| mbError =:(Error _) = (Error (fromError mbError), iworld)
                //Write updated value
                # (mbError,iworld) = if valueChanged
                    (modify
						(\pts -> {ParallelTaskState|pts & value = encode val,
							implicitAttributes = implicitAttributeUpdate pts.ParallelTaskState.implicitAttributes,initialized = True})
                        (sdsFocus (listId,taskId,True) taskInstanceParallelTaskListItem)
						EmptyContext iworld)
                    (modify
						(\pts -> {ParallelTaskState|pts &
							implicitAttributes = implicitAttributeUpdate pts.ParallelTaskState.implicitAttributes, initialized = True})
                        (sdsFocus (listId,taskId,False) taskInstanceParallelTaskListItem)
						EmptyContext iworld)
				| mbError =:(Error _) = (Error (fromError mbError),iworld)
					= (Ok result,iworld)
	where
		encode NoValue      = NoValue
		encode (Value v s)  = Value (DeferredJSON v) s
	
		decode NoValue     = NoValue
		decode (Value v s) = Value (fromMaybe (abort "invalid parallel task state\n") $ fromDeferredJSON v) s
	
		(TaskId instanceNo taskNo)   = taskId
	
	//Retrieve result of detached parallel task
	evalDetachedParallelTask :: !TaskId !Event !TaskEvalOpts !ParallelTaskState !*IWorld -> *(MaybeError TaskException (TaskResult a), *IWorld) | iTask a
	evalDetachedParallelTask listId event evalOpts {ParallelTaskState|taskId=taskId=:(TaskId instanceNo _)} iworld
		= case readRegister listId (sdsFocus instanceNo (removeMaybe Nothing taskInstanceValue)) iworld of
			(Error e,iworld) = (Error e,iworld)
			(Ok (ReadingDone (TIException dyn msg)),iworld) = (Ok (ExceptionResult (dyn,msg)),iworld)
			(Ok (ReadingDone (TIValue encValue)),iworld)
				//Decode value value
				# mbValue = case encValue of
					NoValue           = Just NoValue
					Value json stable = (\dec -> Value dec stable) <$> fromDeferredJSON json
				//TODO: use global tasktime to be able to compare event times between instances
				# evalInfo = {TaskEvalInfo|lastEvent=0,removedTasks=[]}
				# result = maybe (ExceptionResult (exception "Could not decode task value of detached task"))
					(\val -> ValueResult val evalInfo NoChange nopTask) mbValue
				= (Ok result,iworld)

destroyParallelTasks :: !TaskId !*IWorld -> *(TaskResult [(Int,TaskValue a)], *IWorld) | iTask a
destroyParallelTasks listId=:(TaskId instanceNo _) iworld
	// Unlink registrations for all detached tasks
	# iworld = clearTaskSDSRegistrations ('DS'.singleton listId) iworld
	= case read (sdsFocus (listId, minimalTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld of
		(Error e,iworld) = (ExceptionResult e, iworld)
		(Ok (ReadingDone taskStates),iworld)
			// Destroy all child tasks (`result` is always `DestroyedResult` but passed to solve overloading
			# (result,exceptions,iworld) = foldl (destroyParallelTask listId) (DestroyedResult, [], iworld) taskStates
			// Remove the (shared) tasklist
			# (exceptions,iworld) = case modify (fmap (\m -> 'DM'.del listId m)) (sdsFocus instanceNo taskInstanceParallelTaskLists) EmptyContext iworld of
				(Ok (ModifyingDone _),iworld) = (exceptions,iworld)
				(Error e,iworld) = ([e:exceptions],iworld)
			| exceptions =: []
				= (destroyResult result, iworld)
			= (ExceptionResult (exception (ExceptionList exceptions)), iworld)
where
	minimalTaskListFilter = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False
	                        ,includeValue=False,includeAttributes=False,includeProgress=False}

	destroyParallelTask listId (_,exceptions,iworld) {ParallelTaskState|taskId,detached}
		= case (if detached destroyDetachedParallelTask destroyEmbeddedParallelTask) listId taskId iworld of
			(Error e, iworld) = (DestroyedResult, e ++ exceptions,iworld)
			(Ok res, iworld) = (res, exceptions,iworld)

	destroyResult :: (TaskResult a) -> (TaskResult [(Int,TaskValue a)])
	destroyResult DestroyedResult = DestroyedResult
	destroyResult (ExceptionResult e) = ExceptionResult e
	destroyResult (ValueResult _ _ _ _) = ExceptionResult (exception "Valueresult in a destroy?")

destroyEmbeddedParallelTask :: TaskId TaskId *IWorld -> *(MaybeError [TaskException] (TaskResult a), *IWorld) | iTask a
destroyEmbeddedParallelTask listId=:(TaskId instanceNo _) taskId iworld=:{current={taskTime}}
	# (errs,destroyResult,iworld) = case read (sdsFocus taskId taskInstanceEmbeddedTask) EmptyContext iworld of
		(Error e,iworld) = ([e], DestroyedResult,iworld)
		(Ok (ReadingDone (Task eval)),iworld)
			= case eval DestroyEvent {mkEvalOpts & noUI = True, taskId=taskId} iworld of
				(res=:(DestroyedResult),iworld) = ([],res,iworld)
				(res=:(ExceptionResult e),iworld) = ([e],DestroyedResult,iworld)
				(res,iworld) = ([exception "destroyEmbeddedParallelTask: unexpected result"],DestroyedResult,iworld)
	// 2. Remove the task evaluation function
	# (errs,iworld) = case modify (fmap (\(r=:{TIReduct|tasks}) -> {TIReduct|r & tasks = 'DM'.del taskId tasks}))
	                              (sdsFocus instanceNo taskInstanceReduct) EmptyContext iworld of
		(Error e,iworld) = ([e:errs],iworld)
		(Ok (ModifyingDone _),iworld) = (errs,iworld)
	= (Ok destroyResult, iworld)

destroyDetachedParallelTask :: TaskId TaskId *IWorld -> *(MaybeError [TaskException] (TaskResult a), *IWorld) | iTask a
destroyDetachedParallelTask listId=:(TaskId instanceNo _) taskId iworld
	//TODO: Detached parallel tasks should be marked that their parent no longer needs their result
	//      That way attach combinators can be programmed to notify the user or simply stop the task
	= (Ok DestroyedResult, iworld)

destroyRemoved :: TaskId [TaskId] [(TaskId, TaskResult a)] *IWorld -> ([(TaskId, TaskResult a)], *IWorld) | iTask a
destroyRemoved listId removed [] iworld = ([],iworld)
destroyRemoved listId removed [r=:(taskId, _):rs] iworld
	| isMember taskId removed
		= case destroyEmbeddedParallelTask listId taskId iworld of
			(Error e, iworld) = ([(taskId, ExceptionResult (exception (ExceptionList e))):rs], iworld)
			(Ok tr, iworld)
				# (rs,iworld) = destroyRemoved listId removed rs iworld
				= ([(taskId, tr):rs],iworld)
	# (rs,iworld) = destroyRemoved listId removed rs iworld
	= ([r:rs],iworld)

genParallelValue :: [TaskResult a] -> TaskValue [(TaskTime,TaskValue a)]
genParallelValue results = Value [(lastEvent,val) \\ ValueResult val {TaskEvalInfo|lastEvent} _ _ <- results] False

genParallelRep :: !TaskEvalOpts !Event [UI] [String] [TaskResult a] Int -> UIChange
genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
	= case event of
		ResetEvent
			= ReplaceUI (uiac UIContainer (classAttr [className]) ([def \\ ValueResult _ _ (ReplaceUI def) _ <- results] ++ actions))
		_
			# (idx,iChanges) = itemChanges 0 prevNumBranches results
			# aChanges       = actionChanges idx
			= ChangeUI [] (iChanges ++ aChanges)
where
	className = if (actions =: []) "parallel" "parallel-actions"

	itemChanges i numExisting [] = (i,[])
	itemChanges i numExisting [ValueResult _ _ change _:rs]
		| i < numExisting
			# (i`,changes) = itemChanges (i + 1) numExisting rs
			= (i`,[(i,ChangeChild change):changes]) 	//Update an existing branch
		| otherwise			= case change of
			(ReplaceUI def)
				# (i`,changes) = itemChanges (i + 1) (numExisting + 1) rs
				= (i`,[(i,InsertChild def):changes]) 	//Add a new branch
			_
				= itemChanges (i + 1) (numExisting + 1) rs //Skip if we don't get a blank UI

	itemChanges i numExisting [DestroyedResult:rs]
		| i < numExisting
			# (i`,changes) = itemChanges i (numExisting - 1) rs
			= (i`,[(i,RemoveChild):changes])
		| otherwise
			= itemChanges i numExisting rs //No need to destroy a branch that was not yet in the UI

	itemChanges i numExisting [ExceptionResult e:rs]
		| i < numExisting
			# (i`,changes) = itemChanges i (numExisting - 1) rs
			= (i`,[(i,RemoveChild):changes])
		| otherwise
			= itemChanges i numExisting rs

	actionChanges startIdx = [(i,ChangeChild (switch (isEnabled ui) (actionId ui))) \\ ui <- actions & i <- [startIdx..]]
	where
		switch True name = if (isMember name prevEnabledActions) NoChange (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])
		switch False name = if (isMember name prevEnabledActions) (ChangeUI [SetAttribute "enabled" (JSONBool False)] []) NoChange

genParallelEvalInfo :: [TaskResult a] -> TaskEvalInfo
genParallelEvalInfo results = foldr addResult {TaskEvalInfo|lastEvent=0,removedTasks=[]} results
where
    addResult (ValueResult _ i1 _ _) i2
        # lastEvent = max i1.TaskEvalInfo.lastEvent i2.TaskEvalInfo.lastEvent
        # removedTasks = i1.TaskEvalInfo.removedTasks ++ i2.TaskEvalInfo.removedTasks
        = {TaskEvalInfo|lastEvent=lastEvent,removedTasks=removedTasks}
    addResult _ i = i

readListId :: (SharedTaskList a) *IWorld -> (MaybeError TaskException TaskId,*IWorld) | TC a
readListId slist iworld = case read (sdsFocus taskListFilter slist) EmptyContext iworld of
	(Ok e,iworld)	= (Ok (fst (directResult e)), iworld)
	(Error e, iworld)	    = (Error e, iworld)
where
    taskListFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}

appendTask :: !ParallelTaskType !(ParallelTask a) !(SharedTaskList a) -> Task TaskId | iTask a
appendTask parType parTask slist = mkInstantTask eval
where
	eval _ iworld=:{current={taskTime}}
		# (mbListId,iworld) = readListId slist iworld
		| mbListId =:(Error _) = (mbListId,iworld)
		# listId = fromOk mbListId
		//Check if someone is trying to add an embedded task to the topLevel list
		| listId == TaskId 0 0 && parType =:(Embedded)
			= (Error (exception "Embedded tasks can not be added to the top-level task list"),iworld)
		# (mbStateMbTask, iworld) = initParallelTask mkEvalOpts listId 0 parType parTask iworld
		= case mbStateMbTask of
			Ok (state,mbTask)
				# taskId = state.ParallelTaskState.taskId
				| listId == TaskId 0 0 //For the top-level list, we don't need to do anything else
					//TODO: Make sure we don't lose the attributes!
					= (Ok taskId, iworld)
			  	//Update the task list
				# taskListFilter      = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
				# (mbError,iworld)    =  modify (\states -> states ++ [{ParallelTaskState|state & index = nextIndex states}]) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
				| mbError =:(Error _) = (liftError mbError,iworld)
				//If the task is an embedded one, we also need to store the task function
				| mbTask =:(Just _)
					# (mbError,iworld) = (write (snd (fromJust mbTask)) (sdsFocus taskId taskInstanceEmbeddedTask) EmptyContext iworld)
					| mbError =:(Error _) = (liftError mbError,iworld)
					= (Ok taskId, iworld)
				= (Ok taskId, iworld)
			err = (liftError err, iworld)
	where
		//To determine the next index we need to disregard states that are marked as removed
		nextIndex states = length [p\\p=:{ParallelTaskState|change} <- states | not (change =: (Just RemoveParallelTask))]

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task () | TC a
removeTask removeId=:(TaskId instanceNo taskNo) slist = Task eval
where
	eval DestroyEvent _ iworld = (DestroyedResult, iworld)
	eval event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld
		# (mbListId,iworld) = readListId slist iworld
		| mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
		# listId = fromOk mbListId
		//If we are removing from the top-level task list, just remove the instance
		| listId == TaskId 0 0
			# (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
			= (ValueResult
				(Value () True)
				(mkTaskEvalInfo lastEval)
				(mkUIIfReset event (ui UIEmpty))
				(treturn ()), iworld)
		//Mark the task as removed, and update the indices of the tasks afterwards
		# taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
		# (mbError,iworld)      = modify (markAsRemoved removeId) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
		| mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
		//If it is a detached task, remove the detached instance, if it is embedded, pass notify the currently evaluating parallel
		| taskNo == 0 //(if the taskNo equals zero the instance is embedded)
			# (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
			= (ValueResult (Value () True) {lastEvent=lastEval,removedTasks=[]} (mkUIIfReset event (ui UIEmpty)) (treturn ()), iworld)
		//Pass removal information up
		= (ValueResult (Value () True) {lastEvent=lastEval,removedTasks=[(listId,removeId)]} (mkUIIfReset event (ui UIEmpty)) (treturn ()), iworld)

	//When a task is marked as removed, the index of the tasks after that are decreased
	markAsRemoved removeId [] = []
	markAsRemoved removeId [s=:{ParallelTaskState|taskId}:ss]
		| taskId == removeId = [{ParallelTaskState|s & change = Just RemoveParallelTask}
		                       :[{ParallelTaskState|s` & index = index - 1} \\ s`=:{ParallelTaskState|index} <- ss]]
		| otherwise          = [s:markAsRemoved removeId ss]

replaceTask :: !TaskId !(ParallelTask a) !(SharedTaskList a) -> Task () | iTask a
replaceTask replaceId=:(TaskId instanceNo taskNo) parTask slist = Task eval
where
	eval DestroyEvent _ iworld = (DestroyedResult, iworld)
	eval event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld
		# (mbListId,iworld) = readListId slist iworld
		| mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
		# listId = fromOk mbListId
		//Replace the full instance task
		| listId == TaskId 0 0
			= case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
				(Ok (), iworld)
					= (ValueResult (Value () True) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (treturn ()), iworld)
				(Error e, iworld)
					= (ExceptionResult e,iworld)
		//If it is a detached task, replacee the detached instance, if it is embedded schedule the change in the parallel task state
		| taskNo == 0 //(if the taskNo equals zero the instance is embedded)
			= case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
				(Ok (), iworld)
					= (ValueResult (Value () True) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (treturn ()), iworld)
				(Error e, iworld)
					= (ExceptionResult e,iworld)
		//Schedule the change in the parallel task state
		# task                  = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
		# taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
		# (mbError,iworld)      = modify (scheduleReplacement replaceId task) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
		| mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
		= (ValueResult (Value () True) (mkTaskEvalInfo lastEval) (mkUIIfReset event (ui UIEmpty)) (treturn ()), iworld)

	scheduleReplacement replaceId task [] = []
	scheduleReplacement replaceId task [s=:{ParallelTaskState|taskId}:ss]
		| taskId == replaceId   = [{ParallelTaskState|s & change = Just (ReplaceParallelTask (dynamic task :: Task a^))}:ss]
		| otherwise             = [s:scheduleReplacement replaceId task ss]

attach :: !InstanceNo !Bool -> Task AttachmentStatus
attach instanceNo steal = Task evalinit
where
	evalinit DestroyEvent _ iworld = (DestroyedResult, iworld)
	evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld=:{current={attachmentChain}}
		# (mbConstants,iworld)		= read (sdsFocus instanceNo taskInstanceConstants) EmptyContext iworld
		| mbConstants =: (Error _)  = (ExceptionResult (fromError mbConstants),iworld)
		# (mbProgress,iworld)		= read (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
		| mbProgress =: (Error _)   = (ExceptionResult (fromError mbProgress),iworld)
		# (Ok (ReadingDone {InstanceConstants|build})) = mbConstants
		# (Ok (ReadingDone progress=:{InstanceProgress|instanceKey,value,attachedTo})) = mbProgress
		//Check if the task is already in use
		| (not (attachedTo =: [])) && (not steal)
			= eval (ASInUse (hd attachedTo)) build instanceKey event evalOpts  iworld
		//Take over the instance. We generate a new key, so the other instance will no longer have access
		# (newKey,iworld) = newInstanceKey iworld
        # progress      = {InstanceProgress|progress & instanceKey = Just newKey, attachedTo = [taskId:attachmentChain]}
		# (_,iworld)	= write progress (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
		//Clear all input and output of that instance
		# (_,iworld)    = write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceOutput) EmptyContext iworld 
		# (_,iworld)    = modify (\('DQ'.Queue a b) -> 'DQ'.Queue [(i,e) \\(i,e)<- a| i <> instanceNo][(i,e) \\(i,e)<- b| i <> instanceNo]) taskEvents EmptyContext iworld 
		= eval (ASAttached (value =: Stable)) build (Just newKey) event evalOpts iworld

	eval _ _ _ DestroyEvent evalOpts=:{TaskEvalOpts|taskId} iworld
		# iworld     = clearTaskSDSRegistrations ('DS'.singleton taskId) iworld
		# (_,iworld) = modify release (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
        = (DestroyedResult, iworld)
	where
		release progress=:{InstanceProgress|attachedTo=[t:_]}
			| t == taskId = {InstanceProgress|progress & attachedTo=[]} //Only release if the instance is still attached to this 'attach' task
			= progress
		release progress = progress

	eval prevStatus build instanceKey event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld=:{options={appVersion},current={taskInstance}}
		//Load instance
		# (progress,iworld) = readRegister taskId (sdsFocus instanceNo taskInstanceProgress) iworld
		//Determine state of the instance
		# curStatus = case progress of
			(Ok (ReadingDone progress=:{InstanceProgress|attachedTo=[attachedId:_],value}))
			    | build <> appVersion    = ASIncompatible
				| value =: (Exception _) = ASExcepted "unable to read progress"
				| attachedId <> taskId   = ASInUse attachedId	
				                         = ASAttached (value =: Stable)
			_                            = ASDeleted
		//Determine UI change
		# change = determineUIChange event curStatus prevStatus instanceNo instanceKey
		# stable = (curStatus =: ASDeleted) || (curStatus =: ASExcepted _)
		= (ValueResult (Value curStatus stable)
			(mkTaskEvalInfo lastEval) change
			(Task (eval curStatus build instanceKey)), iworld)

	determineUIChange event curStatus prevStatus instanceNo instanceKey
		| curStatus === prevStatus && not (event =: ResetEvent) = NoChange
		| curStatus =: (ASInUse _)    = ReplaceUI inuse
		| curStatus =: (ASExcepted _)    = ReplaceUI exception
		| curStatus =: ASIncompatible || instanceKey =: Nothing = ReplaceUI incompatible
		| otherwise     		      = ReplaceUI viewport
	where
		inuse        = stringDisplay "This task is already in use"
		exception    = stringDisplay "An exception occurred in this task"
		incompatible = stringDisplay "This task can no longer be evaluated"
		viewport  =	(uia UIViewport ('DM'.unions [sizeAttr FlexSize FlexSize, instanceNoAttr instanceNo, instanceKeyAttr (fromJust instanceKey)]))

withCleanupHook :: (Task a) (Task b) -> Task b | iTask a & iTask b
withCleanupHook patch orig
	= appendTopLevelTask 'DM'.newMap False patch
	>>- \x->Task (eval x orig)
where
	eval tosignal (Task orig) DestroyEvent opts iw
		# (tr, iw) = orig DestroyEvent opts iw
		= (tr, queueRefresh [(tosignal, "Cleanup")] iw)
	eval tosignal (Task orig) ev opts iw
		# (val, iw) = orig ev opts iw
		= (wrapTaskContinuation (eval tosignal) val, iw)
