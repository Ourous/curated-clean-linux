definition module iTasks.WF.Combinators.Core
/**
* This module provides the core builtin combinators for composing tasks into workflows
*/
import iTasks.WF.Definition
from iTasks.SDS.Definition import :: SDS
from Data.Maybe import :: Maybe

//* Next task actions
:: Action	= Action !String //Locally unique identifier for actions

//* Common action constants with predefined options
ActionOk		:== Action "Ok"
ActionCancel	:==	Action "Cancel"
ActionYes		:== Action "Yes"
ActionNo		:== Action "No"
ActionNext		:== Action "Next"
ActionPrevious	:== Action "Previous"
ActionFinish	:== Action "Finish"
ActionContinue	:==	Action "Continue"
ActionOpen		:== Action "/File/Open"
ActionSave		:== Action "/File/Save"
ActionSaveAs 	:== Action "/File/Save as"
ActionQuit		:== Action "/File/Quit"
ActionHelp		:==	Action "/Help/Help"
ActionAbout		:== Action "/Help/About"
ActionFind		:== Action "/Edit/Find"
ActionNew		:== Action "New"
ActionEdit		:== Action "Edit"
ActionDelete	:== Action "Delete"
ActionRefresh	:== Action "Refresh"
ActionClose		:==	Action "Close"
	
:: ParallelTaskType	
	= Embedded                                    //Simplest embedded
    | NamedEmbedded !String                       //Embedded with name
	| Detached !TaskAttributes !Bool              //Management meta and flag whether the task should be started at once
    | NamedDetached !String !TaskAttributes !Bool //Detached with name

:: ParallelTask a	:== (SharedTaskList a) -> Task a

// Data available to parallel sibling tasks
:: TaskList a :== (!TaskId,![TaskListItem a])
:: SharedTaskList a	:==	SDS TaskListFilter (!TaskId,![TaskListItem a]) [(!TaskId,!TaskAttributes)]

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

/**
* State of another task instance.
*/
:: AttachmentStatus
    = ASAttached !Stability //* the task instance is currently attached to this task
    | ASInUse !TaskId 		//* the task instance is already attached to another task
    | ASExcepted            //* the task instance had an uncaught exception
    | ASDeleted             //* the task instance does not exist anymore
    | ASIncompatible        //* the task instance can not be executed in this is version of the program (it was created by an older version)

:: AttachException		= InstanceNotFound | InstanceEvalError 

derive class iTask AttachException
instance toString AttachException
/**
* Adds a result transformation function to a task.
* The resulting task is still considered a single step in the workflow.
*
* @param Function: The transformation function. It works on maybe's to also map over instable tasks.
* @param Task: The task to which the transformation function is added
*
* @return The transformed task
*/
transform :: ((TaskValue a) -> TaskValue b) !(Task a) -> Task b

/**
* The generic sequential combinator.
* It does a task followed by one out of a given list of continuations.
* Once the transition to the continuation has been made it cannot be reversed.
*
* @param Task: The first step in the sequence
* @param Value before step function: Computes the value of the composition before a step is taken
* @param Continuations: A set of continuation definitions from which one is selected to make the step
*   -OnValue: inspect the value, step if the predicate matches
*	-OnAction: enable an action if the predicate matches, step if the actions is chosen
*	-OnException: Provides an exception handler for exceptions of type e
*	-OnAllExceptions: Provides an exception handler that catches all exceptions
*
*	@return The combined task
*/
step :: !(Task a) ((Maybe a) -> (Maybe b)) [TaskCont a (Task b)] -> Task b | TC a & JSONDecode{|*|} a & JSONEncode{|*|} a

:: TaskCont a b
    =       OnValue             ((TaskValue a)  -> Maybe b)
    |       OnAction    Action  ((TaskValue a)  -> Maybe b)
    | E.e:  OnException         (e              -> b)           & iTask e
    |       OnAllExceptions     (String         -> b)

/**
* Parallel task creation
*
* @param Initial tasks: The initial list of tasks to run in parallel, each task is given
*        a view on the status of all tasks in the list
* @param Continuations: A set of continuation definitions with which the list of tasks
*        can be extended
*   -OnValue:         Inspect the value, add if the predicate matches
*	-OnAction:        Enable an action if the predicate matches, add if the actions is chosen
*	-OnException:     Provides an exception handler for exceptions of type e
*                     The task in the parallel set that raised the exception is replaced
*                     with the continuation
*	-OnAllExceptions: Provides an exception handler that catches all exceptions
*                     The task in the parallel set that raised the exception is replaced
*                     with the continuation
* @return The sum of all results
* @gin False
*/
parallel :: ![(!ParallelTaskType,!ParallelTask a)] [TaskCont [(!Int,!TaskValue a)] (!ParallelTaskType,!ParallelTask a)] -> Task [(!Int,!TaskValue a)] | iTask a

//Task list manipulation
/**
* Appends a task to a task list
*/
appendTask  :: !ParallelTaskType !(ParallelTask a)	!(SharedTaskList a) -> Task TaskId | iTask a
/**
* Removes (and stops) a task from a task list
*/
removeTask  :: !TaskId								!(SharedTaskList a)	-> Task () | TC a
/**
* Replaces a task in a list and resets its execution state.
* All meta-data is kept
*/
replaceTask :: !TaskId !(ParallelTask a)            !(SharedTaskList a) -> Task () | iTask a
/**
* Focuses a task in a task list
*/
focusTask   :: !TaskId                              !(SharedTaskList a) -> Task () | iTask a

/**
* Attaches a a detached task.
*
* @param Identification of the task instance to attach
* @param Steal/takeover if this is true this attach will takeover the instance when it was already attached somewhere else
* @return The state of the task to work on
*/
attach :: !InstanceNo !Bool -> Task AttachmentStatus
