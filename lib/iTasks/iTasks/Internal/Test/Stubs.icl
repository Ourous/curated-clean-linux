implementation module iTasks.Internal.Test.Stubs
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor
import System.Time
import Data.Maybe
import StdMisc
import qualified Data.Map as DM

//TEST STUBS
toStubIWorld :: *World -> *IWorld
toStubIWorld world
  = {IWorld
  |options = {EngineOptions|appName="STUB",appPath="./",appVersion="STUB",serverPort=80,serverUrl="/127.0.0.1:80/",keepaliveTime={tv_sec=0,tv_nsec=0},sessionTime={tv_sec=0,tv_nsec=0}
             ,persistTasks=False,autoLayout=False,webDirPath="./STUB/",storeDirPath="./STUB/",tempDirPath="./STUB/",saplDirPath="./STUB",timeout=Nothing}
  ,clock = {tv_sec=0,tv_nsec=0}
  ,current ={TaskEvalState|taskTime= 0,taskInstance= 0,sessionInstance = Nothing,attachmentChain = [] ,nextTaskNo = 0}
  ,sdsNotifyRequests = [], memoryShares = 'DM'.newMap, readCache = 'DM'.newMap, writeCache = 'DM'.newMap, exposedShares = 'DM'.newMap
  ,jsCompilerState = Nothing ,shutdown = Nothing ,ioTasks = {done = [], todo = []},ioStates = 'DM'.newMap
  ,world = world
  ,resources = [], random = [], onClient = False }
	
fromStubIWorld :: *IWorld -> *World
fromStubIWorld iworld=:{IWorld|world} = world

toStubVSt :: *IWorld -> *VSt
toStubVSt iworld = {VSt| selectedConsIndex = -1, taskId = "STUB", mode = Enter, optional = False, iworld = iworld}

fromStubVSt :: *VSt -> *IWorld
fromStubVSt vst=:{VSt|iworld} = iworld
