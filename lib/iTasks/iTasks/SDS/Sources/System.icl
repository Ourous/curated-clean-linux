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
from StdFunc import id, o, const

NS_SYSTEM_DATA :== "SystemData"

currentDateTime :: SDSParallel () DateTime ()
currentDateTime = iworldLocalDateTime

currentTime :: SDSLens () Time ()
currentTime = mapRead toTime iworldLocalDateTime

currentDate :: SDSLens () Date ()
currentDate = mapRead toDate iworldLocalDateTime

currentUTCDateTime :: SDSLens () DateTime ()
currentUTCDateTime = mapRead timestampToGmDateTime currentTimestamp

currentUTCTime :: SDSLens () Time ()
currentUTCTime = mapRead (toTime o timestampToGmDateTime) currentTimestamp

currentUTCDate :: SDSLens () Date ()
currentUTCDate = mapRead (toDate o timestampToGmDateTime) currentTimestamp

currentTimestamp :: SDSLens () Timestamp ()
currentTimestamp = toReadOnly (sdsFocus {start=Timestamp 0,interval=Timestamp 1} iworldTimestamp)

currentTimespec :: SDSLens () Timespec ()
currentTimespec = toReadOnly (sdsFocus {start=zero,interval=zero} iworldTimespec)

// Workflow processes
topLevelTasks :: SharedTaskList ()
topLevelTasks = topLevelTaskList

currentSessions :: SDSLens () [TaskListItem ()] ()
currentSessions
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,includeSessions=True,includeDetached=False,includeStartup=False,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

currentProcesses :: SDSLens () [TaskListItem ()] ()
currentProcesses
    = mapRead (map toTaskListItem) (toReadOnly (sdsFocus filter filteredInstanceIndex))
where
    filter = {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,includeSessions=False,includeDetached=True,includeStartup=False,matchAttribute=Nothing
             ,includeConstants=True,includeProgress=True,includeAttributes=True}

toTaskListItem :: !InstanceData -> TaskListItem a
toTaskListItem (instanceNo,Just {InstanceConstants|type},Just progress, Just attributes) //TODO Set self for current evaluating instance
	# listId = case type of
		(PersistentInstance (Just listId)) = listId
		_ = (TaskId 0 0)
	= {TaskListItem|taskId = TaskId instanceNo 0, listId = listId, detached = True, self = False, value = NoValue, progress = Just progress, attributes = attributes}

taskInstanceFromInstanceData :: InstanceData -> TaskInstance
taskInstanceFromInstanceData (instanceNo,Just {InstanceConstants|type,build,issuedAt},Just progress=:{InstanceProgress|value,instanceKey,firstEvent,lastEvent},Just attributes)
	# session = (type =: SessionInstance)
	# listId = case type of
		(PersistentInstance (Just listId)) = listId
		_ = (TaskId 0 0)
    = {TaskInstance|instanceNo = instanceNo, instanceKey = instanceKey, session = session, listId = listId, build = build
      ,attributes = attributes, value = value, issuedAt = issuedAt, firstEvent = firstEvent, lastEvent = lastEvent}

currentTaskInstanceNo :: SDSSource () InstanceNo ()
currentTaskInstanceNo = createReadOnlySDS (\() iworld=:{current={taskInstance}} -> (taskInstance,iworld))

currentTaskInstanceAttributes :: SimpleSDSSequence TaskAttributes
currentTaskInstanceAttributes
	= sdsSequence "currentTaskInstanceAttributes"
		id
		(\_ no -> no)
		(\_ _ -> Right snd)
		(SDSWriteConst (\_ _ -> Ok Nothing))
    (SDSWrite (\no r w -> (Ok (Just w))))
		currentTaskInstanceNo
		taskInstanceAttributes

allTaskInstances :: SDSLens () [TaskInstance] ()
allTaskInstances
    = (sdsProject (SDSLensRead readInstances) (SDSBlindWrite \_. Ok Nothing) Nothing
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

detachedTaskInstances :: SDSLens () [TaskInstance] ()
detachedTaskInstances
    =  (sdsProject (SDSLensRead readInstances) (SDSBlindWrite \_. Ok Nothing) Nothing
       (sdsFocus {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,includeSessions=False,includeDetached=True,includeStartup=False,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True} filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

taskInstanceByNo :: SDSLens InstanceNo TaskInstance TaskAttributes
taskInstanceByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem) Nothing
      (sdsTranslate "taskInstanceByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing,includeConstants=True,includeProgress=True,includeAttributes=True}

    readItem [i]    = Ok (taskInstanceFromInstanceData i)
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstanceAttributesByNo :: SDSLens InstanceNo TaskAttributes TaskAttributes
taskInstanceAttributesByNo
    = sdsProject (SDSLensRead readItem) (SDSLensWrite writeItem) Nothing
      (sdsTranslate "taskInstanceAttributesByNo" filter filteredInstanceIndex)
where
    filter no = {InstanceFilter|onlyInstanceNo=Just [no],notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Nothing,includeConstants=False,includeProgress=False,includeAttributes=True}

    readItem [(_,_,_,Just a)]    = Ok a
    readItem _      = Error (exception "Task instance not found")

    writeItem [(n,c,p,_)] a = Ok (Just [(n,c,p,Just a)])
    writeItem _ _   = Error (exception "Task instance not found")

taskInstancesByAttribute :: SDSLens (!String,!String) [TaskInstance] ()
taskInstancesByAttribute
    =
      (sdsProject (SDSLensRead readInstances) (SDSBlindWrite \_. Ok Nothing) Nothing
       (sdsTranslate "taskInstancesByAttribute" (\p -> {InstanceFilter|onlyInstanceNo=Nothing,notInstanceNo=Nothing,includeSessions=True,includeDetached=True,includeStartup=True,matchAttribute=Just p,includeConstants=True,includeProgress=True,includeAttributes=True}) filteredInstanceIndex))
where
    readInstances is = Ok (map taskInstanceFromInstanceData is)

currentTopTask :: SDSLens () TaskId ()
currentTopTask = mapRead (\currentInstance -> TaskId currentInstance 0) currentInstanceShare

applicationName :: SDSSource () String ()
applicationName = createReadOnlySDS appName
where
	appName () iworld=:{IWorld|options={EngineOptions|appName}} = (appName,iworld)

applicationVersion :: SDSSource () String ()
applicationVersion = createReadOnlySDS appBuild
where
	appBuild () iworld=:{IWorld|options={EngineOptions|appVersion}} = (appVersion,iworld)

applicationDirectory :: SDSSource () FilePath ()
applicationDirectory = createReadOnlySDS appDir
where
	appDir () iworld=:{IWorld|options={EngineOptions|appPath}} = (takeDirectory appPath,iworld)

applicationOptions :: SDSSource () EngineOptions ()
applicationOptions = createReadOnlySDS options
where
	options () iworld=:{IWorld|options} = (options,iworld)


