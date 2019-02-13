definition module iTasks.SDS.Sources.System
/**
* This module exposes system information from an itask application
*/

import iTasks.SDS.Definition
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
    , instanceKey       :: !Maybe InstanceKey   //* Random string that a client needs to provide to access the task instance
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
currentDateTime 		:: SDSParallel () DateTime ()
currentTime				:: SDSLens () Time ()
currentDate				:: SDSLens () Date ()

// Date & time (in UTC)
currentUTCDateTime      :: SDSLens () DateTime ()
currentUTCTime          :: SDSLens () Time ()
currentUTCDate          :: SDSLens () Date ()

//Unix timestamp
currentTimestamp 		:: SDSLens () Timestamp ()
currentTimespec 		:: SDSLens () Timespec ()

// Processes
topLevelTasks 			:: SharedTaskList ()

currentSessions 		:: SDSLens () [TaskListItem ()] ()
currentProcesses		:: SDSLens () [TaskListItem ()] ()

// Session
currentTopTask :: SDSLens () TaskId ()

//Task instances
currentTaskInstanceNo           :: SDSSource () InstanceNo ()
currentTaskInstanceAttributes :: SimpleSDSSequence TaskAttributes
allTaskInstances                :: SDSLens () [TaskInstance] ()
detachedTaskInstances	        :: SDSLens () [TaskInstance] () //Exclude sessions
taskInstanceByNo                :: SDSLens InstanceNo TaskInstance TaskAttributes
taskInstanceAttributesByNo      :: SDSLens InstanceNo TaskAttributes TaskAttributes
taskInstancesByAttribute		:: SDSLens (!String,!String) [TaskInstance] () //Parameter is (key,value)

// Application
applicationName			:: SDSSource () String ()         // Application name
applicationVersion      :: SDSSource () String ()          // Application build identifier
applicationDirectory	:: SDSSource () FilePath ()       // Directory in which the applicaton resides
applicationOptions      :: SDSSource () EngineOptions ()   //Full engine options

