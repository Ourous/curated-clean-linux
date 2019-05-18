implementation module iTasks.WF.Combinators.Core

import iTasks.WF.Definition
import iTasks.UI.Definition
import iTasks.SDS.Definition

import iTasks.Engine
import iTasks.Internal.DynamicUtil
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import iTasks.Internal.Tonic.Shares
import iTasks.Internal.AsyncSDS

from iTasks.SDS.Combinators.Common import sdsFocus, sdsSplit, sdsTranslate, toReadOnly, mapRead, mapReadWriteError, mapSingle, removeMaybe
import iTasks.WF.Combinators.Common
from iTasks.Internal.SDS import write, read, readRegister, modify

import iTasks.WF.Tasks.System

import StdList, StdBool, StdTuple, StdString, Data.Maybe, Data.Tuple, StdMisc
from StdFunc import o
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Queue as DQ

import Data.Maybe, Data.Either, Data.Error, Data.Func
import Text.GenJSON
from Data.Functor import <$>, class Functor(fmap)

derive gEq ParallelTaskChange

:: Action	= Action !String //Locally unique identifier for actions

:: ParallelTaskType
	= Embedded                                    //Simplest embedded
    | NamedEmbedded !String                       //Embedded with name
	| Detached !TaskAttributes !Bool              //Management meta and flag whether the task should be started at once
    | NamedDetached !String !TaskAttributes !Bool //Detached with name

:: ParallelTask a	:== (SharedTaskList a) -> Task a

// Data available to parallel sibling tasks
:: TaskList a :== (!TaskId,![TaskListItem a])
:: SharedTaskList a	:== SDSLens TaskListFilter (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)]

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

instance toString AttachException
where
	toString InstanceNotFound	= "Cannot find task instance to attach"
	toString InstanceEvalError	= "Error in attached task instance "

derive class iTask AttachException

transformError :: ((TaskValue a) -> MaybeError TaskException (TaskValue b)) !(Task a) -> Task b
transformError f (Task evala) = Task eval
where
	eval event evalOpts tree iworld = case evala event evalOpts tree iworld of
		(ValueResult val lastEvent rep tree,iworld)	= case f val of
			Error e = (ExceptionResult e, iworld)
			Ok v = (ValueResult v lastEvent rep tree, iworld)
		(ExceptionResult e, iworld)				    = (ExceptionResult e, iworld)
		(DestroyedResult, iworld)					= (DestroyedResult, iworld)

//TODO: Move this check to the TCInit of the step
step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | TC a & JSONDecode{|*|} a & JSONEncode{|*|} a
step task fun c
= if (length conts <> length c)
	(step` (traceValue "Duplicate actions in step") (\_->Nothing) [OnValue (ifStable \_->step` task fun conts)])
	(step` task fun conts)
where
	conts = removeDupBy actionEq c

	actionEq (OnAction (Action a) _) (OnAction (Action b) _) = a == b
	actionEq _ _ = False

removeDupBy :: (a a -> Bool) [a] -> [a]
removeDupBy eq [x:xs] = [x:removeDupBy eq (filter (not o eq x) xs)]
removeDupBy _ [] = []

step` :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | TC a & JSONDecode{|*|} a & JSONEncode{|*|} a
step` (Task evala) lhsValFun conts = Task eval
where
	//Cleanup
    eval DestroyEvent evalOpts (TCInit _ _) iworld
        = (DestroyedResult,iworld) //Removed before first evaluation...

	eval DestroyEvent evalOpts (TCStep taskId ts (Left (treea,_))) iworld
		= case evala DestroyEvent (extendCallTrace taskId evalOpts) treea iworld of
			(DestroyedResult,iworld)		= (DestroyedResult,iworld)
			(ExceptionResult e,iworld)	    = (ExceptionResult e,iworld)
			(ValueResult _ _ _ _,iworld)	= (ExceptionResult (exception "Destroy failed in step"),iworld)

	eval DestroyEvent evalOpts (TCStep taskId ts (Right (enca,sel,treeb))) iworld
		= case restoreTaskB sel enca of
			Just (Task evalb)	= evalb DestroyEvent (extendCallTrace taskId evalOpts) treeb iworld
			Nothing				= (ExceptionResult (exception "Corrupt task value in step"), iworld)

	eval event evalOpts (TCInit taskId ts) iworld
		# (taskIda,iworld)	= getNextTaskId iworld
		= eval event evalOpts (TCStep taskId ts (Left (TCInit taskIda ts,[]))) iworld

	//Eval left-hand side
	eval event evalOpts (TCStep taskId ts (Left (treea,prevEnabledActions))) iworld=:{current={taskTime}}
		# (resa, iworld) 	= evala event (extendCallTrace taskId evalOpts) treea iworld
        # mbAction          = matchAction taskId event
		# mbCont			= case resa of
			ValueResult val info rep ntreea = case searchContValue val mbAction conts of
				Nothing
					# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
                    # value = maybe NoValue (\v -> Value v False) (lhsValFun (case val of Value v _ = Just v; _ = Nothing))
					# actions = contActions taskId val conts
					# curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
					= Left (ValueResult value info (doBeforeStepLayout taskId evalOpts event actions prevEnabledActions rep val)
								(TCStep taskId info.TaskEvalInfo.lastEvent (Left (ntreea,curEnabledActions))))
				Just rewrite	= Right (rewrite,Just ntreea, info.TaskEvalInfo.lastEvent,info.TaskEvalInfo.removedTasks)
			ExceptionResult e = case searchContException e conts of
				Nothing			= (Left (ExceptionResult e))
				Just rewrite	= (Right (rewrite,Nothing,ts,[]))
		= case mbCont of
			Left res = (res,iworld)
			Right ((sel,Task evalb,d_json_a),mbTreeA, lastEvent,removedTasks)
				//Cleanup state of left-hand side
				# iworld	= case mbTreeA of
					Nothing		= iworld
					//TODO: Check for exceptions during cleanup
					Just treea	= snd (evala DestroyEvent (extendCallTrace taskId evalOpts) treea iworld)
				# (taskIdb,iworld)	= getNextTaskId iworld
				# (resb,iworld)		= evalb ResetEvent (extendCallTrace taskId evalOpts) (TCInit taskIdb lastEvent) iworld
				= case resb of
					ValueResult val info change=:(ReplaceUI _) nstateb
						# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent, removedTasks = removedTasks ++ info.TaskEvalInfo.removedTasks}
						= (ValueResult val info (doAfterStepLayout ResetEvent change) (TCStep taskId info.TaskEvalInfo.lastEvent (Right (d_json_a,sel,nstateb))),iworld)
					ValueResult val info change nstateb
						= (ExceptionResult (exception ("Reset event of task in step failed to produce replacement UI: ("+++ toString (toJSON change)+++")")), iworld)
					ExceptionResult e = (ExceptionResult e, iworld)

	//Eval right-hand side
	eval event evalOpts (TCStep taskId ts (Right (enca,sel,treeb))) iworld=:{current={taskTime}}
		= case restoreTaskB sel enca of
			Just (Task evalb)
				# (resb, iworld)	= evalb event (extendCallTrace taskId evalOpts) treeb iworld
				= case resb of
					ValueResult val info change ntreeb
						# info = {TaskEvalInfo|info & lastEvent = max ts info.TaskEvalInfo.lastEvent}
						= (ValueResult val info (doAfterStepLayout event change) (TCStep taskId info.TaskEvalInfo.lastEvent (Right (enca,sel,ntreeb))), iworld)
					ExceptionResult e = (ExceptionResult e, iworld)
			Nothing
				= (ExceptionResult (exception "Corrupt task value in step"), iworld)

	//Incorrect state
	eval event _ state iworld
		= (ExceptionResult (exception ("Corrupt task state in step:" +++ (toString (toJSON state)))), iworld)

	restoreTaskB sel d_json_a = case conts !! sel of
		(OnValue taskbf)			= callWithDeferredJSONTaskValue taskbf d_json_a
		(OnAction _ taskbf)			= callWithDeferredJSONTaskValue taskbf d_json_a
		(OnException taskbf)		= callWithDeferredJSON taskbf d_json_a
		(OnAllExceptions taskbf)	= callWithDeferredJSON taskbf d_json_a

	doBeforeStepLayout taskId evalOpts event actions prevEnabled change val
		= case (event,change) of
			//On reset generate a new step UI
			(ResetEvent,ReplaceUI rui)
				= ReplaceUI (uic UIStep [rui:contActions taskId val conts])
			//Otherwise create a compound change definition
			_
				= ChangeUI [] [(0,ChangeChild change):actionChanges]
	where
		actionChanges = [(i,ChangeChild (switch (isEnabled ui) (actionId ui))) \\ ui <- actions & i <- [1..]]
		where
			switch True name = if (isMember name prevEnabled) NoChange (ChangeUI [SetAttribute "enabled" (JSONBool True)] [])
			switch False name = if (isMember name prevEnabled) (ChangeUI [SetAttribute "enabled" (JSONBool False)] []) NoChange

	doAfterStepLayout event change = change

	callWithDeferredJSONTaskValue :: ((TaskValue a) -> (Maybe (Task .b))) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
	callWithDeferredJSONTaskValue f_tva_tb d_json_tva=:(DeferredJSON tva)
        = f_tva_tb (cast_to_TaskValue tva)

	callWithDeferredJSONTaskValue f_tva_tb (DeferredJSONNode json)
        = case fromJSON json of
            Just a ->  f_tva_tb a
            Nothing -> Nothing

	callWithDeferredJSON :: (a -> Task .b) DeferredJSON -> Maybe (Task .b) | TC a & JSONDecode{|*|} a
    callWithDeferredJSON f_tva_tb d_json_tva=:(DeferredJSON tva)
      = Just (f_tva_tb (cast tva))

    callWithDeferredJSON f_tva_tb (DeferredJSONNode json)
        = case fromJSON json of
            Just a ->  Just (f_tva_tb a)
            Nothing -> Nothing

isEnabled (UI _ attr _) = maybe False (\(JSONBool b) -> b) ('DM'.get "enabled" attr)
actionId (UI _ attr _) = maybe "" (\(JSONString s) -> s) ('DM'.get "actionId" attr)

matchAction :: TaskId Event -> Maybe String
matchAction taskId (ActionEvent matchId action)
    | matchId == taskId     = Just action
                            = Nothing
matchAction taskId _        = Nothing

contActions :: TaskId (TaskValue a) [TaskCont a b]-> [UI]
contActions taskId val conts = [actionUI (isJust (taskbf val)) action\\ OnAction action taskbf <- conts]
where
	actionUI enabled action=:(Action actionId)
		= uia UIAction ('DM'.unions [enabledAttr enabled, taskIdAttr (toString taskId), actionIdAttr actionId])

searchContValue :: (TaskValue a) (Maybe String) [TaskCont a b] -> Maybe (!Int, !b, !DeferredJSON) | TC a & JSONEncode{|*|} a
searchContValue val mbAction conts = search val mbAction 0 Nothing conts
where
    search _ _ _ mbMatch []							= mbMatch		//No matching OnValue steps were found, return the potential match
	search val mbAction i mbMatch [OnValue f:cs]
	    = case f val of
			Just cont	= Just (i, cont, DeferredJSON val)			//Don't look any further, first matching trigger wins
			Nothing		= search val mbAction (i + 1) mbMatch cs	//Keep search
    search val mbAction=:(Just actionEvent) i Nothing [OnAction (Action actionName) f:cs]
	    | actionEvent == actionName
		    = case f val of
                Just cont	= search val mbAction (i + 1) (Just (i, cont, DeferredJSON val)) cs 	//We found a potential winner (if no OnValue values are in cs)
                Nothing		= search val mbAction (i + 1) Nothing cs								//Keep searching
        | otherwise
                            = search val mbAction (i + 1) Nothing cs								//Keep searching
    search val mbAction i mbMatch [_:cs]			= search val mbAction (i + 1) mbMatch cs		//Keep searching

searchContException :: (Dynamic,String) [TaskCont a b] -> Maybe (Int, !b, !DeferredJSON)
searchContException (dyn,str) conts = search dyn str 0 Nothing conts
where
    search _ _ _ catchall []					= catchall														//Return the maybe catchall
    search dyn str i catchall [OnException f:cs] = case (match f dyn) of
        Just (taskb,enca)						= Just (i, taskb, enca)											//We have a match
        _										= search dyn str (i + 1) catchall cs							//Keep searching
    search dyn str i Nothing [OnAllExceptions f:cs]	= search dyn str (i + 1) (Just (i, f str, DeferredJSON str)) cs //Keep searching (at least we have a catchall)
    search dyn str i mbcatchall [_:cs]			= search dyn str (i + 1) mbcatchall cs							//Keep searching

    match :: (e -> b) Dynamic -> Maybe (b, DeferredJSON) | iTask e
    match f (e :: e^)	= Just (f e, DeferredJSON e)
    match _ _			= Nothing

// Parallel composition
parallel :: ![(!ParallelTaskType,!ParallelTask a)] [TaskCont [(!Int,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] -> Task [(!Int,!TaskValue a)] | iTask a
parallel initTasks conts = Task eval
where
    //Cleanup
    eval DestroyEvent evalOpts (TCParallel taskId ts taskTrees _) iworld
		= destroyParallelTasks taskId ('DM'.fromList taskTrees) iworld

    //Create initial task list
    eval event evalOpts (TCInit taskId ts) iworld
      //Create the states for the initial tasks
      = case initParallelTasks (setParallel taskId (extendCallTrace taskId evalOpts)) taskId 0 initTasks iworld of
          (Ok (taskList,embeddedTasks),iworld)
            //Write the local task list
            # taskListFilter = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
            # (e,iworld) = (write taskList (sdsFocus (taskId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld)
            | isError e = (ExceptionResult (fromError e),iworld)
            //Write the local embedded tasks
            # (e,iworld) = writeAll embeddedTasks taskInstanceEmbeddedTask iworld
            | isError e = (ExceptionResult (fromError e),iworld)
            //Evaluate the parallel
            = eval event (setParallel taskId (extendCallTrace taskId evalOpts)) (TCParallel taskId ts [] []) iworld
          (Error err,iworld)
			= (ExceptionResult err, iworld)
      where
      	writeAll [] sds iworld = (Ok WritingDone,iworld)
      	writeAll [(f,w):ws] sds iworld = case (write w (sdsFocus f sds) EmptyContext iworld) of
          (Ok _,iworld) = writeAll ws sds iworld
          err = err

    //Evaluate the task list
    eval event evalOpts (TCParallel taskId ts taskTrees prevEnabledActions) iworld=:{current=current=:{taskTime}}
        //Evaluate all branches of the parallel set
        = case evalParallelTasks taskId ('DM'.fromList taskTrees) event evalOpts conts [] [] iworld of
            (Ok results,iworld)
                //Construct the result
                # results   = reverse results //(the results are returned in reverse order)
                # value     = genParallelValue results
                # evalInfo  = genParallelEvalInfo results
				# actions 	= contActions taskId value conts
                # rep       = genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
                # taskTrees = [(fromOk (taskIdFromTaskTree tree),tree) \\ ValueResult _ _ _ tree <- results | isOk (taskIdFromTaskTree tree)]
                # curEnabledActions = [actionId action \\ action <- actions | isEnabled action]
                = (ValueResult value evalInfo rep (TCParallel taskId ts taskTrees curEnabledActions),iworld)
			//Stopped because of an unhandled exception
            (Error e, iworld)
				//Clean up before returning the exception
				# (res,iworld) = destroyParallelTasks taskId ('DM'.fromList taskTrees) iworld
				= (exceptionResult res e,iworld)
	where
		//We need to know how many branches there are before evaluation to be
		//able to determine the correct UI update instructions
		prevNumBranches = length taskTrees

		exceptionResult :: (TaskResult [(!Int,!TaskValue a)]) TaskException -> (TaskResult [(!Int,!TaskValue a)])
		exceptionResult DestroyedResult e = ExceptionResult e
		exceptionResult (ExceptionResult _) e = ExceptionResult e

    //Fallback
    eval _ _ _ iworld
    	= (ExceptionResult (exception "Corrupt task state in parallel"), iworld)

//Parallel helper functions
setParallel taskId evalOpts = {evalOpts & tonicOpts = {evalOpts.tonicOpts & inParallel = Just taskId}}

initParallelTasks ::
	!TaskEvalOpts
	!TaskId
	!Int
	![(!ParallelTaskType,!ParallelTask a)]
	!*IWorld
	->
	(!MaybeError TaskException ([ParallelTaskState]
	,[(TaskId,Task a)])
	,!*IWorld)
	| iTask a
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

initParallelTask ::
	!TaskEvalOpts
	!TaskId
	!Int
	!ParallelTaskType
	!(ParallelTask a)
	!*IWorld
	->
	(!MaybeError TaskException (ParallelTaskState
	,Maybe (TaskId,Task a))
	,!*IWorld)
	| iTask a
initParallelTask evalOpts=:{tonicOpts = {callTrace}} listId index parType parTask iworld=:{current={taskTime}}
  # (mbTaskStuff,iworld) = case parType of
                             Embedded           = mkEmbedded 'DM'.newMap iworld
                             NamedEmbedded name = mkEmbedded ('DM'.singleton "name" name) iworld
                             Detached           attributes evalDirect = mkDetached attributes evalDirect iworld
                             NamedDetached name attributes evalDirect = mkDetached ('DM'.put "name" name attributes) evalDirect iworld
  = case mbTaskStuff of
      Ok (taskId,attributes,mbTask)
        # state       = { ParallelTaskState
                        | taskId     = taskId
                        , index      = index
                        , detached   = isNothing mbTask
                        , attributes = attributes
                        , value      = NoValue
                        , createdAt  = taskTime
                        , lastFocus  = Nothing
                        , lastEvent  = taskTime
                        , change     = Nothing
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
              Ok taskId
                = (Ok (taskId, attributes, Nothing), iworld)
              err       = (liftError err, iworld)
        err = (liftError err, iworld)

evalParallelTasks :: TaskId (Map TaskId TaskTree) !Event !TaskEvalOpts
	[TaskCont [(!TaskTime,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)]
	[TaskResult a] [ParallelTaskState] !*IWorld
	->
	(MaybeError TaskException [TaskResult a],!*IWorld) | iTask a
evalParallelTasks listId taskTrees event evalOpts conts completed [] iworld
    //(re-)read the tasklist to check if it contains items we have not yet evaluated
    # taskListFilter         = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
    # (mbList,iworld)       = read (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
    | mbList =:(Error _)    = (Error (fromError mbList),iworld)
    = case drop (length completed) (directResult (fromOk mbList)) of
        //We are done, unless we have continuations that extend the set
        []  = case searchContValue (genParallelValue (reverse completed)) (matchAction listId event) conts of
            Nothing //We have evaluated all branches and nothing is added
                //Remove all entries that are marked as removed from the list, they have been cleaned up by now
                # taskListFilter        = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
                # (mbError,iworld)      = modify (\l -> [x \\ x <- l | not (isRemoved x)])
											(sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
                | mbError =:(Error _)    = (Error (fromError mbError),iworld)
                = (Ok completed,iworld)

            Just (_,(type,task),_) //Add extension
                # (mbStateMbTask,iworld)     = initParallelTask evalOpts listId 0 type task iworld
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
                      = evalParallelTasks listId taskTrees ResetEvent evalOpts conts completed [state] iworld //Continue
                    err = (liftError err, iworld)
        todo    = evalParallelTasks listId taskTrees event evalOpts conts completed todo iworld     //Evaluate the remaining items
where
	isRemoved {ParallelTaskState|change=Just RemoveParallelTask} = True
	isRemoved _ = False

//Evaluate an embedded parallel task
evalParallelTasks listId taskTrees event evalOpts conts completed [t=:{ParallelTaskState|taskId=TaskId _ taskNo}:todo] iworld
	= case evalParallelTask listId taskTrees event evalOpts t iworld of
		(Error e, iworld) = (Error e,iworld)
		(Ok (ExceptionResult e), iworld) = (Error e,iworld) //Stop on exceptions
		(Ok result=:(ValueResult val evalInfo=:{TaskEvalInfo|lastEvent,removedTasks} rep tree), iworld)
			//Add the current result before checking for removals
			# completed = [result:completed]
			//Check if in the branch tasks from this list were removed but that were already evaluated
			# removed = [t \\ (l,t=:(TaskId _ n)) <- removedTasks | l == listId && n <= taskNo]
			# (completed,iworld) = destroyRemoved listId removed completed iworld
			= evalParallelTasks listId taskTrees event evalOpts conts completed todo iworld
		(Ok result=:DestroyedResult, iworld)
			= evalParallelTasks listId taskTrees event evalOpts conts [result:completed] todo iworld

evalParallelTask listId taskTrees event evalOpts taskState=:{ParallelTaskState|detached} iworld
	| detached  = evalDetachedParallelTask listId taskTrees event evalOpts taskState iworld
	| otherwise = evalEmbeddedParallelTask listId taskTrees event evalOpts taskState iworld

evalEmbeddedParallelTask listId taskTrees event evalOpts
	{ParallelTaskState|taskId,detached=False,createdAt,lastFocus,value,change} iworld=:{current={taskTime}}
    //Lookup task evaluation function and task evaluation state
    # (mbTask,iworld) = read (sdsFocus taskId taskInstanceEmbeddedTask) EmptyContext iworld
    | mbTask =:(Error _) = (Error (fromError mbTask),iworld)
    # (Task evala) = directResult (fromOk mbTask)
    # (tree,newBranch) = maybe (TCInit taskId taskTime,True) (\tree -> (tree,False)) ('DM'.get taskId taskTrees)
    //Evaluate or destroy branch
    | change === Just RemoveParallelTask
		# (result,taskTrees,iworld) = destroyEmbeddedParallelTask listId taskId taskTrees iworld
		= case result of
			(Ok res) = (Ok res,iworld)
			(Error [e]) = (Error e, iworld)
			(Error _) = (Error (exception "evalEmbeddedParallelTask: multiple exceptions during destruction"), iworld)
    | otherwise
        # evalOpts =
			{evalOpts & tonicOpts =
				{evalOpts.tonicOpts
				& captureParallel = evalOpts.tonicOpts.inParallel == Just listId
				, inParallel      = Just listId
				}
			}
		//Evaluate new branches with a reset event
		# (result,iworld) = evala (if newBranch ResetEvent event)
			(setParallel listId (extendCallTrace taskId evalOpts)) tree iworld
		//Tonic hook
		# iworld = if (evalOpts.tonicOpts.captureParallel
						&& evalOpts.tonicOpts.currBlueprintExprId <> []
						&& evalOpts.tonicOpts.currBlueprintTaskId <> TaskId 0 0)
							(storeTaskOutputViewer result evalOpts.tonicOpts.currBlueprintExprId
								evalOpts.tonicOpts.currBlueprintTaskId taskId iworld)
							iworld
        = case result of
            //If an exception occured, check if we can handle it at this level
            ExceptionResult e
                //TODO Check exception
                //If the exception can not be handled, don't continue evaluating just stop
                = (Ok (ExceptionResult e),iworld)
            ValueResult val evalInfo=:{TaskEvalInfo|lastEvent,attributes,removedTasks} rep tree
                //Check for a focus event targeted at this branc
                # mbNewFocus= case event of
                    (FocusEvent focusId)  = if (focusId == taskId) (Just taskTime) Nothing
                    _                       = Nothing
                # lastFocus     = maybe lastFocus Just mbNewFocus
                # result = ValueResult val evalInfo rep tree
                //Check if the value changed
                # valueChanged = val =!= decode value
                //Write updated value, and optionally the new lastFocus time to the tasklist
                # (mbError,iworld) = if valueChanged
                    (modify
						(\pts -> {ParallelTaskState|pts & value = encode val, lastFocus = maybe pts.ParallelTaskState.lastFocus Just mbNewFocus, attributes = attributes})
                        (sdsFocus (listId,taskId,True) taskInstanceParallelTaskListItem)
						EmptyContext iworld)
                    (modify
						(\pts -> {ParallelTaskState|pts & lastFocus = maybe pts.ParallelTaskState.lastFocus Just mbNewFocus, attributes = attributes})
                        (sdsFocus (listId,taskId,False) taskInstanceParallelTaskListItem)
						EmptyContext
						iworld)
                | mbError =:(Error _) = (Error (fromError mbError),iworld)
					= (Ok result,iworld)
where
    encode NoValue      = NoValue
    encode (Value v s)  = Value (DeferredJSON v) s

    decode NoValue     = NoValue
    decode (Value v s) = Value (fromMaybe (abort "invalid parallel task state\n") $ fromDeferredJSON v) s

    (TaskId instanceNo taskNo)   = taskId

//Retrieve result of detached parallel task
evalDetachedParallelTask listId taskTrees event evalOpts {ParallelTaskState|taskId=taskId=:(TaskId instanceNo _),detached=True} iworld
    = case readRegister listId (sdsFocus instanceNo (removeMaybe Nothing taskInstanceValue)) iworld of
        (Error e,iworld)
            = (Error e,iworld)
        (Ok (ReadingDone (TIException dyn msg)),iworld)
            = (Ok (ExceptionResult (dyn,msg)),iworld)
        (Ok (ReadingDone (TIValue encValue)),iworld)
            //Decode value value
            # mbValue = case encValue of
                NoValue           = Just NoValue
                Value json stable = (\dec -> Value dec stable) <$> fromDeferredJSON json
            //TODO: use global tasktime to be able to compare event times between instances
            # evalInfo = {TaskEvalInfo|lastEvent=0,attributes='DM'.newMap,removedTasks=[]}
            # result = maybe (ExceptionResult (exception "Could not decode task value of detached task"))
                (\val -> ValueResult val evalInfo NoChange TCNop) mbValue
			= (Ok result,iworld)

destroyParallelTasks listId=:(TaskId instanceNo _) taskTrees iworld
	//Unlink registrations for all detached tasks
	# iworld = clearTaskSDSRegistrations ('DS'.singleton listId) iworld
	= case read (sdsFocus (listId,minimalTaskListFilter) taskInstanceParallelTaskList) EmptyContext iworld of
		(Error e,iworld)
			//TODO: Still try to cleanup as much as possible based on the taskIds in the taskTrees
			= (ExceptionResult e, iworld)
		(Ok (ReadingDone taskStates),iworld)
			//1. Destroy all child tasks (`result` is always `DestroyedResult` but passed to solve overloading
			# (result,exceptions,iworld) = foldl (destroyParallelTask listId taskTrees) (DestroyedResult,[],iworld) taskStates
			//2. Remove the (shared) tasklist
			# (exceptions,iworld) = case modify (fmap (\m -> 'DM'.del listId m))
			                             (sdsFocus instanceNo taskInstanceParallelTaskLists) EmptyContext iworld of
				(Ok (ModifyingDone _),iworld) = (exceptions,iworld)
				(Error e,iworld) = ([e:exceptions],iworld)
			| exceptions =: []
				= (destroyResult result, iworld)
			| otherwise
				= (ExceptionResult (exception "Multiple exceptions in destroyParallelTasks"),iworld)
where
	minimalTaskListFilter = {TaskListFilter|onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False
	                        ,includeValue=False,includeAttributes=False,includeProgress=False}

	destroyParallelTask listId taskTrees (_,exceptions,iworld) {ParallelTaskState|taskId,detached}
		= case (if detached destroyDetachedParallelTask destroyEmbeddedParallelTask) listId taskId taskTrees iworld of
			(Error e,_,iworld) = (DestroyedResult, e ++ exceptions,iworld)
			(Ok res,_,iworld) = (res,exceptions,iworld)

	destroyResult :: (TaskResult a) -> (TaskResult [(!Int,!TaskValue a)])
	destroyResult DestroyedResult = DestroyedResult
	destroyResult (ExceptionResult e) = ExceptionResult e

destroyEmbeddedParallelTask :: TaskId TaskId (Map TaskId TaskTree) *IWorld -> *(MaybeError [TaskException] (TaskResult a),Map TaskId TaskTree,*IWorld) | iTask a
destroyEmbeddedParallelTask listId=:(TaskId instanceNo _) taskId taskTrees iworld=:{current={taskTime}}
	// In this fuction we continue as much as possible, even when we encounter exceptions
	// The returned taskresult is always `DestroyedResult` but is needed to solve overloading
	// Evaluate with a DestroyEvent to destroy subtasks
	# (errs,destroyResult,iworld) = case read (sdsFocus taskId taskInstanceEmbeddedTask) EmptyContext iworld of
		(Error e,iworld) = ([e],DestroyedResult,iworld)
		(Ok (ReadingDone (Task eval)),iworld)
    		# taskTree = fromMaybe (TCInit taskId taskTime) ('DM'.get taskId taskTrees)
			= case eval DestroyEvent {mkEvalOpts & noUI = True} taskTree iworld of
				(res=:(DestroyedResult),iworld) = ([],res,iworld)
				(res=:(ExceptionResult e),iworld) = ([e],DestroyedResult,iworld)
				(res,iworld) = ([exception "destroyEmbeddedParallelTask: unexpected result"],DestroyedResult,iworld)
	// 2. Remove the task tree
	# taskTrees = 'DM'.del taskId taskTrees
	// 3. Remove the task evaluation function
	# (errs,iworld) = case modify (fmap (\(r=:{TIReduct|tasks}) -> {TIReduct|r & tasks = 'DM'.del taskId tasks}))
	                              (sdsFocus instanceNo taskInstanceReduct) EmptyContext iworld of
		(Error e,iworld) = ([e:errs],iworld)
		(Ok (ModifyingDone _),iworld) = (errs,iworld)
	= (Ok destroyResult,taskTrees,iworld)

destroyDetachedParallelTask listId=:(TaskId instanceNo _) taskId taskTrees iworld
	//TODO: Detached parallel tasks should be marked that their parent no longer needs their result
	//      That way attach combinators can be programmed to notify the user or simply stop the task
	= (Ok DestroyedResult,taskTrees,iworld)

destroyRemoved listId removed [] iworld = ([],iworld)
destroyRemoved listId removed [r=:(ValueResult _ _ _ tree):rs] iworld
  = case taskIdFromResult r of
	  Ok taskId
		| isMember taskId removed
			# (mbRes,_,iworld) = destroyEmbeddedParallelTask listId taskId ('DM'.fromList [(taskId,tree)]) iworld
			| mbRes =:(Error _) = ([ExceptionResult (hd (fromError mbRes)):rs],iworld)
			# (rs,iworld)        = destroyRemoved listId removed rs iworld
			= ([fromOk mbRes:rs],iworld)
		| otherwise
			# (rs,iworld) = destroyRemoved listId removed rs iworld
			= ([r:rs],iworld)
	  _
		# (rs,iworld) = destroyRemoved listId removed rs iworld
		= ([r:rs],iworld)
destroyRemoved listId removed [r:rs] iworld
	# (rs,iworld) = destroyRemoved listId removed rs iworld
	= ([r:rs],iworld)

taskIdFromResult (ValueResult _ _ _ tree)   = taskIdFromTaskTree tree
taskIdFromResult _                          = Error (exception "No ValueResult in taskIdFromResult")

genParallelValue :: [TaskResult a] -> TaskValue [(!TaskTime,!TaskValue a)]
genParallelValue results = Value [(lastEvent,val) \\ ValueResult val {TaskEvalInfo|lastEvent} _ _ <- results] False

genParallelRep :: !TaskEvalOpts !Event [UI] [String] [TaskResult a] Int -> UIChange
genParallelRep evalOpts event actions prevEnabledActions results prevNumBranches
	= case event of
		ResetEvent
			= ReplaceUI (uic UIParallel ([def \\ ValueResult _ _ (ReplaceUI def) _ <- results] ++ actions))
		_
			# (idx,iChanges) = itemChanges 0 prevNumBranches results
			# aChanges       = actionChanges idx
			= ChangeUI [] (iChanges ++ aChanges)
where
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
genParallelEvalInfo results = foldr addResult {TaskEvalInfo|lastEvent=0,attributes='DM'.newMap,removedTasks=[]} results
where
    addResult (ValueResult _ i1 _ _) i2
        # lastEvent = max i1.TaskEvalInfo.lastEvent i2.TaskEvalInfo.lastEvent
        # removedTasks = i1.TaskEvalInfo.removedTasks ++ i2.TaskEvalInfo.removedTasks
        = {TaskEvalInfo|lastEvent=lastEvent,attributes='DM'.newMap,removedTasks=removedTasks}
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
        | listId == TaskId 0 0 && (parType =:(Embedded) || parType =:(NamedEmbedded _))
            = (Error (exception "Embedded tasks can not be added to the top-level task list"),iworld)
        # (mbStateMbTask,iworld)  = initParallelTask mkEvalOpts listId 0 parType parTask iworld
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
              | otherwise
                  = (Ok taskId, iworld)
            err = (liftError err, iworld)
    where
        //To determine the next index we need to disregard states that are marked as removed
        nextIndex states = length [p\\p=:{ParallelTaskState|change} <- states | change =!= Just RemoveParallelTask]

/**
* Removes (and stops) a task from a task list
*/
removeTask :: !TaskId !(SharedTaskList a) -> Task () | TC a
removeTask removeId=:(TaskId instanceNo taskNo) slist = Task eval
where
    eval DestroyEvent _ _ iworld = (DestroyedResult,iworld)

    eval event evalOpts (TCInit taskId ts) iworld
        # (mbListId,iworld) = readListId slist iworld
        | mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
        # listId = fromOk mbListId
        //If we are removing from the top-level task list, just remove the instance
        | listId == TaskId 0 0
            # (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
            = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
        //Mark the task as removed, and update the indices of the tasks afterwards
        # taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
        # (mbError,iworld)      = modify (markAsRemoved removeId) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
        | mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
        //If it is a detached task, remove the detached instance, if it is embedded, pass notify the currently evaluating parallel
        | taskNo == 0 //(if the taskNo equals zero the instance is embedded)
            # (mbe,iworld) = deleteTaskInstance instanceNo iworld
			| mbe =: (Error _) = (ExceptionResult (fromError mbe),iworld)
            = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
        | otherwise
            //Pass removal information up
            = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[(listId,removeId)]} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
    eval event evalOpts state=:(TCStable taskId ts _) iworld
        = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) state, iworld)

	rep ResetEvent = ReplaceUI (ui UIEmpty)
	rep _          = NoChange

    //When a task is marked as removed, the index of the tasks after that are decreased
    markAsRemoved removeId [] = []
    markAsRemoved removeId [s=:{ParallelTaskState|taskId}:ss]
        | taskId == removeId = [{ParallelTaskState|s & change = Just RemoveParallelTask}
                               :[{ParallelTaskState|s` & index = index - 1} \\ s`=:{ParallelTaskState|index} <- ss]]
        | otherwise          = [s:markAsRemoved removeId ss]

replaceTask :: !TaskId !(ParallelTask a) !(SharedTaskList a) -> Task () | iTask a
replaceTask replaceId=:(TaskId instanceNo taskNo) parTask slist = Task eval
where
    eval DestroyEvent _ _ iworld
        = (DestroyedResult,iworld)
	eval event evalOpts (TCInit taskId ts) iworld
        # (mbListId,iworld) = readListId slist iworld
        | mbListId =:(Error _) = (ExceptionResult (fromError mbListId),iworld)
        # listId = fromOk mbListId
        //Replace the full instance task
        | listId == TaskId 0 0
            = case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
                (Ok (), iworld)
                    = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
                (Error e, iworld)
                    = (ExceptionResult e,iworld)
        //If it is a detached task, replacee the detached instance, if it is embedded schedule the change in the parallel task state
        | taskNo == 0 //(if the taskNo equals zero the instance is embedded)
            = case replaceTaskInstance instanceNo (parTask topLevelTaskList) iworld of
                (Ok (), iworld)
                    = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
                (Error e, iworld)
                    = (ExceptionResult e,iworld)
        //Schedule the change in the parallel task state
        | otherwise
            # task                  = parTask (sdsTranslate "setTaskAndList" (\listFilter -> (listId,taskId,listFilter)) parallelTaskList)
            # taskListFilter        = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}
            # (mbError,iworld)      = modify (scheduleReplacement replaceId task) (sdsFocus (listId,taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
            | mbError =:(Error _)   = (ExceptionResult (fromError mbError),iworld)
            = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) (TCStable taskId ts (DeferredJSONNode JSONNull)), iworld)
    eval event evalOpts state=:(TCStable taskId ts _) iworld
        = (ValueResult (Value () True) {lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} (rep event) state, iworld)

	rep ResetEvent = ReplaceUI (ui UIEmpty)
	rep _          = NoChange

    scheduleReplacement replaceId task [] = []
    scheduleReplacement replaceId task [s=:{ParallelTaskState|taskId}:ss]
        | taskId == replaceId   = [{ParallelTaskState|s & change = Just (ReplaceParallelTask (dynamic task :: Task a^))}:ss]
        | otherwise             = [s:scheduleReplacement replaceId task ss]


focusTask :: !TaskId !(SharedTaskList a) -> Task () | iTask a
focusTask focusId slist = mkInstantTask eval
where
    eval taskId iworld=:{IWorld|current={taskTime}}
        # (mbListId,iworld)     = readListId slist iworld
        | mbListId =:(Error _)  = (liftError mbListId, iworld)
        # listId                = fromOk mbListId
        | listId == TaskId 0 0
            = (Ok (), iworld)
        # (mbError,iworld)      = modify (\pts -> {ParallelTaskState|pts & lastFocus = Just taskTime}) (sdsFocus (listId,focusId,False) taskInstanceParallelTaskListItem) EmptyContext iworld
        | mbError =:(Error _)   = (liftError mbError, iworld)
        = (Ok (), iworld)

attach :: !InstanceNo !Bool -> Task AttachmentStatus
attach instanceNo steal = Task eval
where
	eval DestroyEvent evalOpts ttree=:(TCAttach taskId _ _ _ _) iworld
		# iworld = clearTaskSDSRegistrations ('DS'.singleton $ fromOk $ taskIdFromTaskTree ttree) iworld
		# (_,iworld)	    = modify release (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
        = (DestroyedResult,iworld)
	where
		release progress=:{InstanceProgress|attachedTo=[t:_]}
			| t == taskId = {InstanceProgress|progress & attachedTo=[]} //Only release if the instance is still attached to this 'attach' task
						  = progress
		release progress = progress

	eval event evalOpts (TCInit taskId ts) iworld=:{current={attachmentChain}}
		# (mbConstants,iworld)		= read (sdsFocus instanceNo taskInstanceConstants) EmptyContext iworld
		| mbConstants =: (Error _)  = (ExceptionResult (fromError mbConstants),iworld)
		# (mbProgress,iworld)		= read (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
		| mbProgress =: (Error _)   = (ExceptionResult (fromError mbProgress),iworld)
		# (Ok (ReadingDone {InstanceConstants|build})) = mbConstants
		# (Ok (ReadingDone progress=:{InstanceProgress|instanceKey,value,attachedTo})) = mbProgress
		//Check if the task is already in use
		| (not (attachedTo =: [])) && (not steal)
			= eval event evalOpts (TCAttach taskId ts (ASInUse (hd attachedTo)) build instanceKey) iworld
		| otherwise
		//Take over the instance. We generate a new key, so the other instance will no longer have access
		# (newKey,iworld) = newInstanceKey iworld
        # progress      = {InstanceProgress|progress & instanceKey = Just newKey, attachedTo = [taskId:attachmentChain]}
		# (_,iworld)	= write progress (sdsFocus instanceNo taskInstanceProgress) EmptyContext iworld
		//Clear all input and output of that instance
		# (_,iworld)    = write 'DQ'.newQueue (sdsFocus instanceNo taskInstanceOutput) EmptyContext iworld 
		# (_,iworld)    = modify (\('DQ'.Queue a b) -> 'DQ'.Queue [(i,e) \\(i,e)<- a| i <> instanceNo][(i,e) \\(i,e)<- b| i <> instanceNo]) taskEvents EmptyContext iworld 
		= eval event evalOpts (TCAttach taskId ts (ASAttached (value =: Stable)) build (Just newKey)) iworld

	eval event evalOpts tree=:(TCAttach taskId ts prevStatus build instanceKey) iworld=:{options={appVersion},current={taskInstance}}
		//Load instance
		# (progress,iworld)	    = readRegister taskId (sdsFocus instanceNo taskInstanceProgress) iworld
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
		= (ValueResult (Value curStatus stable) {TaskEvalInfo|lastEvent=ts,attributes='DM'.newMap,removedTasks=[]} change (TCAttach taskId ts curStatus build instanceKey), iworld)

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
withCleanupHook patch (Task orig)
	= appendTopLevelTask 'DM'.newMap False patch
	>>- Task o eval
where
	eval tosignal DestroyEvent opts tree iw
		# (tr, iw) = orig DestroyEvent opts tree iw
		= (tr, queueRefresh [(tosignal, "Cleanup")] iw)
	eval tosignal ev opts tree iw = orig ev opts tree iw
