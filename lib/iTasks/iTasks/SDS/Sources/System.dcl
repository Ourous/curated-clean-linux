definition module iTasks.SDS.Sources.System
/**
* This module exposes system information from an itask application
*/

from iTasks.SDS.Definition import :: SDS
from iTasks.WF.Definition import :: TaskId, :: TaskNo, :: InstanceNo, :: InstanceKey, :: TaskAttributes, :: ValueStatus
from iTasks.WF.Combinators.Core import :: TaskList, :: SharedTaskList, :: TaskListFilter, :: TaskListItem 
from iTasks.Extensions.DateTime import :: DateTime, :: Date, :: Time 
from iTasks.Engine import :: EngineOptions

from System.Time import :: Timespec, :: Timestamp
from System.FilePath import :: FilePath
from Data.Map import :: Map
from Data.Maybe import :: Maybe

//* Types to view the server's internal table of running task instances
:: TaskInstance =
	{ instanceNo	    :: !InstanceNo			//* Unique global identification
    , instanceKey       :: !InstanceKey         //* Random string that a client needs to provide to access the task instance
    , session           :: !Bool                //* Is this a session
	, listId            :: !TaskId              //* Reference to parent tasklist
    , build             :: !String              //* Application build version when the instance was created
    , issuedAt			:: !Timespec           //* When was the task created
	, attributes        :: !TaskAttributes      //* Arbitrary meta-data
	, value             :: !ValueStatus         //* Status of the task value
	, firstEvent		:: !Maybe Timespec     //*When was the first work done on this task
	, lastEvent		    :: !Maybe Timespec     //* When was the last event on this task	
	}

// Date & time (in task server's local timezone)
currentDateTime			:: SDS () DateTime ()
currentTime				:: SDS () Time ()
currentDate				:: SDS () Date ()

// Date & time (in UTC)
currentUTCDateTime      :: SDS () DateTime ()
currentUTCTime          :: SDS () Time ()
currentUTCDate          :: SDS () Date ()

//Unix timestamp
currentTimestamp 		:: SDS () Timestamp ()
currentTimespec 		:: SDS () Timespec ()

// Processes
topLevelTasks 			:: SharedTaskList () 

currentSessions 		:: SDS () [TaskListItem ()] ()
currentProcesses		:: SDS () [TaskListItem ()] ()

// Session
currentTopTask			:: SDS () TaskId ()

//Task instances
currentTaskInstanceNo           :: SDS () InstanceNo ()
currentTaskInstanceAttributes   :: SDS () TaskAttributes TaskAttributes
allTaskInstances                :: SDS () [TaskInstance] ()
detachedTaskInstances	        :: SDS () [TaskInstance] () //Exclude sessions
taskInstanceByNo                :: SDS InstanceNo TaskInstance TaskAttributes
taskInstanceAttributesByNo      :: SDS InstanceNo TaskAttributes TaskAttributes
taskInstancesByAttribute		:: SDS (!String,!String) [TaskInstance] () //Parameter is (key,value)

// Application
applicationName			:: SDS () String ()          // Application name
applicationVersion      :: SDS () String ()          // Application build identifier
applicationDirectory	:: SDS () FilePath ()        // Directory in which the applicaton resides
applicationOptions      :: SDS () EngineOptions ()   //Full engine options

