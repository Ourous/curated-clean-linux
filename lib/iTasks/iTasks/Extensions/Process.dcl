definition module iTasks.Extensions.Process

import iTasks.WF.Definition
from iTasks.UI.Prompt import class toPrompt
from iTasks.WF.Tasks.Interaction import :: ViewOption

from System.FilePath import :: FilePath
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage 
from System.Process import :: ProcessPtyOptions

//* External (operating system) process status
:: ProcessInformation =
    { executable :: String
    , arguments  :: [String]
    , stdout     :: String
    , stderr     :: String
    , status     :: ProcessStatus
    }

:: ProcessStatus
	= RunningProcess
	| CompletedProcess !Int

:: CallException = CallFailed !OSError

derive class iTask ProcessInformation, ProcessStatus, CallException
instance toString CallException

/**
* Calls an external executable. The call is non-blocking.
*
* @param Task description
* @param Executable: path to the executable
* @param Arguments: a list of command-line arguments
* @param Optional startup directory
* @param Run with pseudo terminal options
* @return return-code of the process
* @throws CallException
* 
* @gin-title Start executable
* @gin-icon executable
*/
callProcess :: !d ![ViewOption ProcessInformation] !FilePath ![String] !(Maybe FilePath) (Maybe ProcessPtyOptions) -> Task ProcessInformation | toPrompt d

/**
* Calls an external executable. This call blocks task computation, only use when process is known to terminate fast.
* @param Executable: path to the executable
* @param Arguments: a list of command-line arguments
* @param Optional startup directory
* @return return-code of the process
* @throws CallException
*/
callInstantProcess :: !FilePath ![String] !(Maybe FilePath)-> Task Int
