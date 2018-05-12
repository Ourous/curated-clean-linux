definition module iTasks.WF.Definition
/*
* This provides the types that define the core concepts for specifying workflow
*/

from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.TaskState import :: TaskTree
from iTasks.Internal.TaskEval import :: TaskEvalOpts, :: TaskEvalInfo
from iTasks.UI.Definition import :: UIChange
from Text.GenJSON import :: JSONNode
from Data.Maybe import :: Maybe
from Data.Map import :: Map(..)
from Data.Set import :: Set
from Data.Functor import class Functor
from System.Time import :: Timestamp, :: Timespec

from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.GenJSON import generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq
from StdString import class toString, class fromString
from StdClass import class <
from StdOverloaded import class ==

// Task definition:
:: Task a = Task !(Event TaskEvalOpts TaskTree *IWorld -> *(!TaskResult a, !*IWorld))

:: Event	= EditEvent		!TaskId !String !JSONNode //Update something in an interaction: Task id, edit name, value
			| ActionEvent	!TaskId !String           //Progress in a step combinator: Task id, action id
			| FocusEvent	!TaskId                   //Update last event time without changing anything: Task id
			| RefreshEvent	!(Set TaskId) !String     //Recalcalutate the tasks with given IDs,
                                                      //using the current SDS values (the string is the reason for the refresh)
			| ResetEvent                              //Nop event, recalculate the entire task and reset output stream

:: TaskResult a
   //If all goes well, a task computes its current value, a ui effect and a new task state
   = ValueResult !(TaskValue a) !TaskEvalInfo !UIChange !TaskTree   
   //If something went wrong, a task produces an exception value
   | ExceptionResult !TaskException
   //If a task finalizes and cleaned up it gives this result
   | DestroyedResult

//* Task results
:: TaskValue a		= NoValue				
					| Value !a !Stability 
:: Stability		:== Bool

StableValue   a :== Value a True
UnstableValue a :== Value a False

:: TaskException    :== (!Dynamic,!String) //The dynamic contains the actual exception which can be matched, the string is an error message

instance Functor TaskValue
/**
* Creates an exception
*/
exception :: !e -> TaskException | TC, toString e

// Task instantiation:

//* Each task instance can be identified by two numbers:
// - A unique number identifying the top-level state
// - A unique number the task within the the state
:: TaskId		= TaskId !InstanceNo !TaskNo
:: InstanceNo	:== Int
:: TaskNo		:== Int

:: TaskAttributes :== Map String String
:: InstanceKey  :== String

instance toString	TaskId
instance fromString	TaskId
instance ==			TaskId
instance <			TaskId

class toInstanceNo t :: t -> InstanceNo
instance toInstanceNo InstanceNo
instance toInstanceNo TaskId

// Instance data which does not change after creation (except when a task is replaced)
:: InstanceConstants =
    { listId        :: !TaskId              //* Reference to parent tasklist
    , session       :: !Bool                //* True for sessions (instances that automatically get garbage collected)
    , build         :: !String              //* Application build version when the instance was created
    , issuedAt		:: !Timespec           //* When was the task created
    }

:: InstanceProgress =
	{ value             :: !ValueStatus     //* Status of the task value
    , attachedTo        :: ![TaskId]        //* Chain of tasks through which this instance was attached
	, instanceKey       :: !InstanceKey     //* Random token that a client gets to have (temporary) access to the task instance
	, firstEvent		:: !Maybe Timespec //* When was the first work done on this task
	, lastEvent		    :: !Maybe Timespec //* When was the latest event on this task (excluding Refresh events)
	}

:: ValueStatus
    = Unstable
    | Stable
    | Exception !String

//The iTask context restriction contains all generic functions that need to
//be available for a type to be used in tasks
class iTask a
	//Interaction
	| gEditor{|*|}
	//Visualization
	, gText{|*|}
	//Serialization
	, JSONEncode{|*|}
	, JSONDecode{|*|}
	//Data
	, gDefault{|*|}
	, gEq{|*|}
	, TC a

