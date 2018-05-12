implementation module iTasks.SDS.Sources.System

import iTasks.SDS.Definition
import iTasks.SDS.Combinators.Core
import iTasks.SDS.Combinators.Common
import iTasks.Extensions.DateTime
import System.Time

import iTasks.Engine
import iTasks.Internal.SDS
import iTasks.Internal.IWorld
import iTasks.Internal.Util

import iTasks.Internal.TaskStore
import StdTuple, StdList, StdString
from iTasks.Internal.TaskEval  import currentInstanceShare
from StdFunc import id, o

NS_SYSTEM_DATA :== "SystemData"

currentDateTime :: SDS () DateTime ()
currentDateTime = iworldLocalDateTime

currentTime :: SDS () Time ()
currentTime = mapRead toTime iworldLocalDateTime
		
currentDate :: SDS () Date ()
currentDate = mapRead toDate iworldLocalDateTime

currentUTCDateTime :: SDS () DateTime ()
currentUTCDateTime = mapRead timestampToGmDateTime currentTimestamp

currentUTCTime :: SDS () Time ()
currentUTCTime = mapRead (toTime o timestampToGmDateTime) currentTimestamp

currentUTCDate :: SDS () Date ()
currentUTCDate = mapRead (toDate o timestampToGmDateTime) currentTimestamp

currentTimestamp :: SDS () Timestamp ()
currentTimestamp = toReadOnly (sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp)

currentTimespec :: SDS () Timespec ()
currentTimespec = toReadOnly (sdsFocus {start=zero,interval=zero} iworldTimespec)


// Workflow processes
topLevelTasks :: SharedTaskList ()
topLevelTasks = topLevelTaskList

currentSessions :: SDS () [TaskListItem ()] ()
currentSessions
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just True,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

currentProcesses :: SDS () [TaskListItem ()] ()
currentProcesses
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

toTaskListItem :: !InstanceData -> TaskListItem a
toTaskListItem (instanceNo,Just {InstanceConstants|listId},Just progress, Just attributes) //TODO Set self for current evaluating instance
	= {TaskListItem|taskId = TaskId instanceNo 0, listId = listId, detached = True, self = False, value = NoValue, progress = Just progress, attributes = attributes}

taskInstanceFromInstanceData :: InstanceData -> TaskInstance
taskInstanceFromInstanceData (instanceNo,Just {InstanceConstants|session,listId,build,issuedAt},Just progress=:{InstanceProgress|value,instanceKey,firstEvent,lastEvent},Just attributes)
    = {TaskInstance|instanceNo = instanceNo, instanceKey = instanceKey, session = session, listId = listId, build = build
      ,attributes = attributes, value = value, issuedAt = issuedAt, firstEvent = firstEvent, lastEvent = lastEvent}

currentTaskInstanceNo :: SDS () InstanceNo ()
currentTaskInstanceNo = createReadOnlySDS (\() iworld=:{current={taskInstance}} -> (taskInstance,iworld))

currentTaskInstanceAttributes :: SDS () TaskAttributes TaskAttributes
currentTaskInstanceAttributes
	= sdsSequence "currentTaskInstanceAttributes" 
		id
		(\_ no -> no) 
		(\_ _ -> Right snd)
		(SDSWriteConst (\_ _ -> Ok Nothing))  (SDSWriteConst (\no w -> (Ok (Just w))))
		currentTaskInstanceNo
		taskInstanceAttributes

allTaskInstances :: SDS () [TaskInstance] ()
allTaskInstances
    = (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

detachedTaskInstances :: SDS () [TaskInstance] ()
detachedTaskInstances
    =  (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Just False,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

taskInstanceByNo :: SDS InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem)
      (sdsTranslate "taskInstanceByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True}

    readItem [i]    = Ok (taskInstanceFromInstanceData i)
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstanceAttributesByNo :: SDS InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributesByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem)
      (sdsTranslate "taskInstanceAttributesByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Nothing,includeConstants=False,includeProgress=False,includeAttributes=True}

    readItem [(_,_,_,Just a)]    = Ok a
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstancesByAttribute :: SDS (!String,!String) [TaskInstance] ()
taskInstancesByAttribute 
    = 
      (sdsProject (SDSLensRead readInstances) SDSNoWrite
       (sdsTranslate "taskInstancesByAttribute" (\p -> {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,onlySession=Nothing,matchAttribute=Just p,includeConstants=True,includeProgress=True,includeAttributes=True}) filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

currentTopTask :: SDS () TaskId ()
currentTopTask = mapRead (\currentInstance -> TaskId currentInstance 0) currentInstanceShare
		
applicationName :: SDS () String ()
applicationName = createReadOnlySDS appName
where
	appName () iworld=:{IWorld|options={EngineOptions|appName}} = (appName,iworld)

applicationVersion :: SDS () String ()
applicationVersion = createReadOnlySDS appBuild
where
	appBuild () iworld=:{IWorld|options={EngineOptions|appVersion}} = (appVersion,iworld)

applicationDirectory :: SDS () FilePath ()
applicationDirectory = createReadOnlySDS appDir
where
	appDir () iworld=:{IWorld|options={EngineOptions|appPath}} = (takeDirectory appPath,iworld)

applicationOptions :: SDS () EngineOptions ()
applicationOptions = createReadOnlySDS options
where
	options () iworld=:{IWorld|options} = (options,iworld)

