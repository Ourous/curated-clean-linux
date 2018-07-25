implementation module iTasks.Internal.Client.RunOnClient

import StdMisc, Data.Func
import iTasks
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.IWorld
import iTasks.UI.Definition
import qualified iTasks.Internal.SDS as SDS

from Data.Map import qualified newMap, toList, fromList, get
from Data.List import find
from Data.Queue as DQ import qualified newQueue, dequeue

import iTasks.Extensions.DateTime
import System.Time, Math.Random
import Text.GenJSON

:: TaskState a = 
			{ instanceNo :: !InstanceNo
			, sessionId  :: !String
			, taskId     :: !Maybe TaskId
			, task		 :: !Task a			
			, value		 :: !Maybe (TaskValue DeferredJSON)
			}

runOnClient :: !(Task m) -> Task m | iTask m
runOnClient task = task
/*
	# roc_tasklet =
		{ Tasklet 
		| genUI				= roc_generator task
		, resultFunc		= gen_res
		, tweakUI			= id
		}
 
	= mkTask roc_tasklet
*/
gen_res {TaskState|value=Nothing} = NoValue
gen_res {TaskState|value=Just NoValue} = NoValue
gen_res {TaskState|value=Just (Value json stability)} = Value (fromJust (fromDeferredJSON json)) stability

/*
roc_generator :: !(Task m) !TaskId (Maybe (TaskState m)) !*IWorld -> *(!TaskletGUI (TaskState m), !TaskState m, !*IWorld) | iTask m
roc_generator task (TaskId instanceNo _) _ iworld=:{current={sessionInstance=Just currentInstance}}
    # currentSession = "SESSIONID-" +++ toString currentInstance
	# gui = TaskletTUI {TaskletTUI|instanceNo = instanceNo, controllerFunc = controllerFunc}
	# state = 	{ TaskState
				| instanceNo = instanceNo
				, sessionId  = currentSession
				, taskId 	 = Nothing
				, task		 = task
				, value 	 = Nothing}
	= (gui, state, iworld)
*/
// Init
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Nothing} Nothing Nothing Nothing iworld
	# (mbTaskId, iworld) = createClientTaskInstance task sessionId instanceNo iworld
    = case mbTaskId of
        Ok taskId
	      # (mbResult,iworld)  = evalTaskInstance instanceNo ResetEvent iworld
	      = case mbResult of
	      	Ok _ 
	      				= (Nothing, {TaskState | st & taskId = Just taskId}, iworld)
	      	_			= (Nothing, {TaskState | st & taskId = Just taskId}, iworld)
        _ = (Nothing, st, iworld)
/* FIXME
// Refresh
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Just t} Nothing Nothing Nothing iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (RefreshEvent "Client refresh") iworld
	= case mbResult of
		Ok (_,value)
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort ("controllerFunc: " +++ msg)
// Focus
controllerFunc _ st=:{TaskState | sessionId, instanceNo, task, taskId = Just t} Nothing Nothing Nothing iworld
	# iworld = trace_n "c_focus" iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (FocusEvent t) iworld
	= case mbResult of
		Ok (_,value)
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort ("controllerFunc: " +++ msg)
*/
// Edit
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} Nothing (Just name) (Just jsonval) iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (EditEvent taskId name (fromString jsonval)) iworld
	= case mbResult of
		Ok value
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort ("controllerFunc: " +++ msg)
// Action
controllerFunc taskId st=:{TaskState | sessionId, instanceNo} Nothing (Just name) Nothing iworld
	# (mbResult,iworld)	= evalTaskInstance instanceNo (ActionEvent taskId name) iworld
	= case mbResult of
		Ok value
					= (Nothing, {TaskState | st & value = Just value}, iworld)
		Error msg	= abort ("controllerFunc: " +++ msg)

newWorld :: *World
newWorld = undef

getUIUpdates :: !*IWorld -> (!Maybe [(InstanceNo, [String])], *IWorld)
getUIUpdates iworld
	= case 'SDS'.read taskOutput iworld of
		(Ok output,iworld)
			= case 'Data.Map'.toList output of
				[] = (Nothing,iworld)
				output
					# (_,iworld) = 'SDS'.write 'Data.Map'.newMap taskOutput iworld
					= (Just (map getUpdates output), iworld)
		(_,iworld)
			= (Nothing, iworld)
where
	getUpdates (instanceNo,upds) = (instanceNo, [toString (encodeUIChanges [c \\ TOUIChange c <- toList upds])])
	toList q = case 'DQ'.dequeue q of //TODO SHOULD BE IN Data.Queue
		(Nothing,q) 	= []
		(Just x,q) 		= [x:toList q]

createClientIWorld :: !String !InstanceNo -> *IWorld
createClientIWorld serverURL currentInstance
        # world = newWorld
        # (timestamp=:{tv_sec=seed},world) = nsTime world
		= {IWorld
		  |options =  { appName = "application"
	                    , appPath = locundef "appDirectory"
 	                    , appVersion = locundef "appVersion"
 	                    , serverPort = 80
                        , serverUrl = locundef "serverUrl"
	                    , keepaliveTime = locundef "keepaliveTime"
                        , sessionTime = locundef "sessionTime"
                        , persistTasks = False
						, autoLayout = True
						, timeout = Just 100
	                    , webDirPath  = locundef "webDirectory"
	                    , storeDirPath = locundef "dataDirectory"
	                    , tempDirPath = locundef "tempDirectory"
	                    , saplDirPath = locundef "saplDirectory"}				
          ,clock = timestamp
          ,current =
            {taskTime			= 0
		    ,taskInstance	    = currentInstance
		    ,sessionInstance	= Just currentInstance
		    ,attachmentChain    = []
		    ,nextTaskNo			= 6666
          }
          ,sdsNotifyRequests    = 'Data.Map'.newMap
          ,sdsNotifyReqsByTask  = 'Data.Map'.newMap
          ,memoryShares         = 'Data.Map'.newMap
          ,readCache            = 'Data.Map'.newMap
          ,writeCache           = 'Data.Map'.newMap
		  ,exposedShares		= 'Data.Map'.newMap
		  ,jsCompilerState		= locundef "jsCompilerState"
		  ,shutdown				= Nothing
          ,random               = genRandInt seed
          ,ioTasks              = {done=[],todo=[]}
		  ,ioStates             = 'Data.Map'.newMap
		  ,world				= world
		  ,resources			= []
		  ,onClient				= True
		  }
where
	locundef var = abort ("IWorld structure is not avalaible at client side. Reference: "+++var)
