definition module iTasks.Internal.TaskServer

from iTasks.Internal.IWorld import :: ConnectionId
from iTasks.Internal.SDS import :: SDSSource

from Data.Maybe 		import :: Maybe
from StdFile			import class FileSystem
from TCPIP				import class ChannelEnv, :: IPAddress, :: Timeout
from Internet.HTTP		import :: HTTPRequest, :: HTTPResponse
from System.FilePath    import :: FilePath

from Data.Error               import :: MaybeError
from Data.Map                 import :: Map
from iTasks.WF.Definition     import :: TaskId
from iTasks.Internal.IWorld	  import :: IWorld
from iTasks.Internal.Task     import :: ConnectionTask, :: TaskException
from iTasks.Internal.IWorld   import :: IWorld, :: IOStates, :: IOState
from iTasks.Internal.IWorld	  import :: IWorld
from iTasks.Internal.Task     import :: ConnectionTask, :: TaskException
from iTasks.Engine            import :: StartupTask

//Core task server loop
serve :: ![StartupTask] ![(Int,ConnectionTask)] (*IWorld -> (Maybe Timeout,*IWorld)) *IWorld -> *IWorld

//Dynamically add a listener
addListener :: !TaskId !Int !Bool !(ConnectionTask) !*IWorld -> (!MaybeError TaskException (),!*IWorld)

//Dynamically add a connection
addConnection :: !TaskId !String !Int !ConnectionTask !*IWorld -> (!MaybeError TaskException (ConnectionId, Dynamic),!*IWorld)

ioStateString :: !IOStates -> String

//Ticks every time the server loops once
tick :: SDSSource () () ()
