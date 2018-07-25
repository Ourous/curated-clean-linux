definition module iTasks.Internal.SDS

import Data.GenEq
import System.FilePath, Data.Maybe, Data.Either, Data.Error, System.Time, Text.GenJSON
from Data.Set import :: Set
from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.Generic.Visualization import :: TextFormat

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: TaskException, :: TaskId, :: InstanceNo
from iTasks.UI.Editor import :: Editor, :: EditMask, :: Masked
import iTasks.SDS.Definition

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
    { reqTaskId 	:: !TaskId		 //Id of the task that read the SDS. This Id also connects a chain of notify requests that were registered together
    , reqSDSId      :: !SDSIdentity  //Id of the actual SDS used to create this request (may be a derived one)

    , cmpParam      :: !Dynamic      //Parameter we are saving for comparison
    , cmpParamText  :: !String       //String version of comparison parameter for tracing
    }
:: SDSIdentity  :== String

instance < SDSNotifyRequest

:: DeferredWrite = E. p r w: DeferredWrite !p !w !(SDS p r w) & iTask p & TC r & TC w

//Internal creation functions:

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	ROShared p r

createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	ROShared p r

//Internal access functions

//Just read an SDS
read			::						    !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r
//Read an SDS and register a taskId to be notified when it is written
readRegister	:: !TaskId                  !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r
//Write an SDS (and queue evaluation of those task instances which contained tasks that registered for notification)
write			:: !w					    !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)	| TC r & TC w
//Read followed by write. The 'a' typed value is a result that is returned
modify          :: !(r -> (!a,!w))          !(RWShared () r w) !*IWorld -> (!MaybeError TaskException a, !*IWorld) | TC r & TC w

//Force notify (queue evaluation of task instances that registered for notification)
notify          ::                          !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)

//Clear all registrations for the given tasks.
//This is normally called by the queueRefresh functions, because once a task is queued
//for evaluation anyway, it no longer make sense to notify it again.
clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld

//List all current registrations (for debugging purposes)
listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
formatSDSRegistrationsList :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String

//Flush all deffered/cached writes of
flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)

:: JSONShared :== RWShared JSONNode JSONNode JSONNode

//Exposing shares for external nodes
toJSONShared    :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & iTask p & TC r & TC w
fromJSONShared  :: JSONShared -> RWShared p r w | JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w
newURL 		    :: !*IWorld -> (!String, !*IWorld)
getURLbyId 	    :: !String !*IWorld -> (!String, !*IWorld)

