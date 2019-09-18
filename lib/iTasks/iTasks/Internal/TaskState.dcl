definition module iTasks.Internal.TaskState

from iTasks.Internal.TaskEval import :: TaskTime

from iTasks.WF.Definition import :: Task, :: TaskResult, :: TaskValue, :: TaskException, :: TaskNo, :: TaskId, :: TaskAttributes, :: Event
from iTasks.WF.Definition import :: InstanceNo, :: InstanceKey, :: InstanceProgress
from iTasks.WF.Combinators.Core import :: AttachmentStatus
from iTasks.UI.Definition import :: UIChange
from iTasks.UI.Editor import :: EditState
from iTasks.UI.Layout import :: LUI, :: LUIMoves, :: LUIMoveID, :: LUINo, :: LUIEffectStage
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenDefault import generic gDefault
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Data.Queue import :: Queue
from Data.Error import :: MaybeError
from Data.Either import :: Either
from System.Time import :: Timestamp, :: Timespec
from Data.GenEq import generic gEq
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat

derive JSONEncode TIMeta, TIType, TIReduct
derive JSONDecode TIMeta, TIType, TIReduct

//Persistent context of active tasks
//Split up version of task instance information

:: TIMeta =
    //Static information
	{ instanceNo	:: !InstanceNo			//Unique global identification
	, instanceType  :: !TIType              //There are 3 types of tasks: startup tasks, sessions, and persistent tasks
    , build         :: !String              //Application build version when the instance was created
    , issuedAt      :: !Timespec
    //Evaluation information
	, progress		:: !InstanceProgress
    //Identification and classification information
	, attributes    :: !TaskAttributes      //Arbitrary meta-data
	}
derive gDefault TIMeta

:: TIType
	= TIStartup
	| TISession !InstanceKey
	| TIPersistent !InstanceKey !(Maybe TaskId)

:: TIReduct =
	{ task			:: !Task DeferredJSON               //Main task definition
	, nextTaskNo	:: !TaskNo                          //Local task number counter
	, nextTaskTime	:: !TaskTime                        //Local task time (incremented at every evaluation)
    // TODO Remove from reduct!
	, tasks			:: !Map TaskId Dynamic				//Task functions of embedded parallel tasks
	}

:: TIValue
   = TIValue !(TaskValue DeferredJSON)
   | TIException !Dynamic !String

:: AsyncAction = Read | Write | Modify

:: DeferredJSON
	= E. a:	DeferredJSON !a & TC a & JSONEncode{|*|} a
	|		DeferredJSONNode !JSONNode

instance toString DeferredJSON
fromDeferredJSON :: !DeferredJSON -> Maybe a | TC, JSONDecode{|*|} a
derive JSONEncode DeferredJSON
derive JSONDecode DeferredJSON
derive gEq        DeferredJSON
derive gText      DeferredJSON

:: ParallelTaskState =
	{ taskId             :: !TaskId                       //Identification
	, index              :: !Int                          //Explicit index (when shares filter the list, you want to keep access to the index in the full list)
	, detached           :: !Bool
	, implicitAttributes :: !TaskAttributes               //Attributes that reflect the latest attributes from the task UI
	, explicitAttributes :: !Map String (!JSONNode,!Bool) //Attributes that are explicitly written to the list and shadow the implicit attributes
	, value             :: !TaskValue DeferredJSON        //Value (only for embedded tasks)
	, createdAt			:: !TaskTime                      //Time the entry was added to the set (used by layouts to highlight new items)
	, lastEvent			:: !TaskTime                      //Last modified time
	, change            :: !Maybe ParallelTaskChange      //Changes like removing or replacing a parallel task are only done when the
	                                                      //parallel is evaluated. This field is used to schedule such changes.
	, initialized       :: !Bool
	}

:: ParallelTaskChange
    = RemoveParallelTask                            //Mark for removal from the set on the next evaluation
    | ReplaceParallelTask !Dynamic                  //Replace the task on the next evaluation
