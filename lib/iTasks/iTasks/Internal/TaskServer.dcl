definition module iTasks.Internal.TaskServer

from Data.Maybe 		import :: Maybe
from StdFile			import class FileSystem
from TCPIP				import class ChannelEnv, :: IPAddress, :: Timeout
from Internet.HTTP		import :: HTTPRequest, :: HTTPResponse
from System.FilePath    import :: FilePath

from Data.Error               import :: MaybeError
from iTasks.WF.Definition     import :: TaskId
from iTasks.Internal.Task     import :: ConnectionTask, :: TaskException
from iTasks.Internal.IWorld   import :: IWorld
from iTasks.Internal.IWorld	  import :: IWorld, :: BackgroundTaskId
from iTasks.Internal.Task     import :: ConnectionTask, :: BackgroundTask, :: TaskException
from iTasks.Engine            import :: StartupTask

//Core task server loop
serve :: ![StartupTask] ![(!Int,!ConnectionTask)] ![BackgroundTask] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld

//Dynamically add a listener
addListener :: !TaskId !Int !Bool !ConnectionTask !*IWorld -> (!MaybeError TaskException (),!*IWorld)

//Dynamically add a connection
addConnection :: !TaskId !String !Int !ConnectionTask !*IWorld -> (!MaybeError TaskException Dynamic,!*IWorld)

//Dynamically add a background task
addBackgroundTask :: !BackgroundTask !*IWorld -> (!MaybeError TaskException BackgroundTaskId,!*IWorld)

//Dynamically remove a background task
removeBackgroundTask :: !BackgroundTaskId !*IWorld -> (!MaybeError TaskException (),!*IWorld)
