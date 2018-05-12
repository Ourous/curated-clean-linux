definition module iTasks.WF.Tasks.IO
/**
* This modules provides tasks that support interaction with other systems.
* Either by running external programs, creating network clients and servers, or exchanging files
*/
import iTasks.WF.Definition
from iTasks.SDS.Definition import :: RWShared, :: SDS
from iTasks.UI.Prompt import class toPrompt
from System.FilePath import :: FilePath
from System.Process import :: ProcessPtyOptions
from Data.Error import :: MaybeError, :: MaybeErrorString

:: ConnectionHandlers l r w = 
    { onConnect         :: !(String r   -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onData            :: !(String l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onShareChange     :: !(       l r -> (!MaybeErrorString l, Maybe w, ![String], !Bool))
    , onDisconnect      :: !(       l r -> (!MaybeErrorString l, Maybe w                  ))
	}

:: ExitCode = ExitCode !Int
:: ExternalProcessHandlers l r w =
    { onStartup     :: !(           r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onOutData     :: !(String   l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onErrData     :: !(String   l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onShareChange :: !(         l r -> (!MaybeErrorString l, !Maybe w, ![String], !Bool))
    , onExit        :: !(ExitCode l r -> (!MaybeErrorString l, !Maybe w                  ))
    }

/**
* Execute an external process. This task's value becomes stable when the process is terminated.
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @param A reference to shared data the task has access to
* @param A flag whether to open a pseudotty
* @param The event handler functions
* @param Optionally the pseudotty settings
* @param An editor for visualizing the local state
*/
externalProcess :: !d !FilePath ![String] !(Maybe FilePath) !(SDS () r w) !(ExternalProcessHandlers l r w) !(Maybe ProcessPtyOptions) !(Editor l) -> Task l | toPrompt d & iTask l & TC r & TC w

/**
* Connect to an external system using TCP. This task's value becomes stable when the connection is closed
* @param Hostname
* @param Port
* @param A reference to shared data the task has access to
* @param The event handler functions
*/
tcpconnect :: !String !Int !(RWShared () r w) (ConnectionHandlers l r w) -> Task l | iTask l & iTask r & iTask w
/**
* Listen for connections from external systems using TCP.
* @param Port
* @param Remove closed connections. If this is true, closed connections are removed from the task value, if not they are kept in the list
* @param A reference to shared data the task has access to
* @param Initialization function: function that is called when a new connection is established
* @param Communication function: function that is called when data arrives, the connection is closed or the observed share changes.
*/
tcplisten :: !Int !Bool !(RWShared () r w) (ConnectionHandlers l r w) -> Task [l] | iTask l & iTask r & iTask w
