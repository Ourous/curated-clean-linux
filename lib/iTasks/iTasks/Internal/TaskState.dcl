definition module iTasks.Internal.TaskState

from iTasks.Internal.TaskEval import :: TonicOpts, :: TaskTime

from iTasks.WF.Definition import :: Task, :: TaskResult, :: TaskValue, :: TaskException, :: TaskNo, :: TaskId, :: TaskAttributes
from iTasks.WF.Definition import :: InstanceNo, :: InstanceKey, :: InstanceProgress
from iTasks.WF.Combinators.Core import :: AttachmentStatus
from iTasks.UI.Definition import :: UIChange
from iTasks.UI.Editor import :: EditMask
from iTasks.UI.Layout import :: LUI, :: LUIMoves, :: LUIMoveID, :: LUINo, :: LUIEffectStage
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Data.Queue import :: Queue
from Data.Error import :: MaybeError
from Data.Either import :: Either
from System.Time import :: Timestamp, :: Timespec
from Data.GenEq import generic gEq
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat

derive JSONEncode TIMeta, TIReduct, TaskTree
derive JSONDecode TIMeta, TIReduct, TaskTree

//Persistent context of active tasks
//Split up version of task instance information

:: TIMeta =
    //Static information
	{ instanceNo	:: !InstanceNo			//Unique global identification
    , instanceKey   :: !InstanceKey         //Random string that a client needs to provide to access the task instance
	, listId        :: !TaskId              //Reference to parent tasklist
    , session       :: !Bool                //Is this a session
    , build         :: !String              //Application build version when the instance was created
    , issuedAt      :: !Timespec
    //Evaluation information
	, progress		:: !InstanceProgress
    //Identification and classification information
	, attributes    :: !TaskAttributes      //Arbitrary meta-data
	}

:: TIReduct =
	{ task			:: !Task DeferredJSON               //Main task definition
    , tree          :: !TaskTree                        //Main task state
    , tonicRedOpts  :: !TonicOpts                       //Tonic data
	, nextTaskNo	:: !TaskNo                          //Local task number counter
	, nextTaskTime	:: !TaskTime                        //Local task time (incremented at every evaluation)
    // TODO Remove from reduct!
	, tasks			:: !Map TaskId Dynamic				//Task functions of embedded parallel tasks
	}

:: TIValue
   = TIValue !(TaskValue DeferredJSON)
   | TIException !Dynamic !String

// UI State
:: TIUIState
	= UIDisabled 									//The UI is disabled (e.g. when nobody is viewing the task)
	| UIEnabled !Int !UIChange  					//The UI is enabled, a version number and the previous task rep are stored for comparision //FIXME
	| UIException !String 							//An unhandled exception occurred and the UI should only show the error message

:: TaskTree
	= TCInit		            !TaskId !TaskTime													//Initial state for all tasks
	| TCBasic		            !TaskId !TaskTime !DeferredJSON !Bool 									//Encoded value and stable indicator
	| TCInteract	            !TaskId !TaskTime !DeferredJSON !DeferredJSON !EditMask
	| TCStep					!TaskId !TaskTime !(Either (!TaskTree, ![String]) (!DeferredJSON, !Int, !TaskTree))
	| TCParallel				!TaskId !TaskTime ![(!TaskId,!TaskTree)] ![String] //Subtrees of embedded tasks and enabled actions
	| TCShared					!TaskId !TaskTime !TaskTree
	| TCAttach                  !TaskId !TaskTime !AttachmentStatus !String !String
	| TCExposedShared			!TaskId !TaskTime !String !TaskTree	// +URL //TODO: Remove
	| TCStable					!TaskId !TaskTime !DeferredJSON
	| TCLayout					!(!LUI,!LUIMoves) !TaskTree
	| TCNop			
	| TCDestroy					!TaskTree	//Marks a task state as garbage that must be destroyed (TODO: replace by explicit event

taskIdFromTaskTree :: TaskTree -> MaybeError TaskException TaskId

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
	{ taskId			:: !TaskId					//Identification
    , index             :: !Int                     //Explicit index (when shares filter the list, you want to keep access to the index in the full list)
    , detached          :: !Bool
    , attributes        :: !TaskAttributes
    , value             :: !TaskValue DeferredJSON //Value (only for embedded tasks)
	, createdAt			:: !TaskTime				//Time the entry was added to the set (used by layouts to highlight new items)
    , lastFocus         :: !Maybe TaskTime          //Time the entry was last explicitly focused
	, lastEvent			:: !TaskTime				//Last modified time
	, change            :: !Maybe ParallelTaskChange //Changes like removing or replacing a parallel task are only done when the
	}                                                //parallel is evaluated. This field is used to schedule such changes.

:: ParallelTaskChange
    = RemoveParallelTask                            //Mark for removal from the set on the next evaluation
    | ReplaceParallelTask !Dynamic                  //Replace the task on the next evaluation


