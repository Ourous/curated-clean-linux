definition module System.Process

import Data.Maybe, Data.Either
import System.OSError, System.FilePath
from System._Posix import exit
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

/*
Not yet implemented:
- Passsing environment, i.e. [(!String,!String)], to created process
*/

:: ProcessHandle = { pid :: !Int
				   }

:: ProcessIO = { stdIn  :: !WritePipe
               , stdOut :: !ReadPipe
               , stdErr :: !ReadPipe
               }

:: WritePipe
:: ReadPipe

derive JSONEncode WritePipe, ReadPipe
derive JSONDecode WritePipe, ReadPipe

/**
* runs a new process
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Process handle to the process
*/
runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)

/**
* runs a new process and opens pipes for IO
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Process handle to the process and pipes for IO
*/
runProcessIO :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError (ProcessHandle, ProcessIO), *World)

/**
* Runs a new process with a pseudo-terminal (pty) for IO
*
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Process handle to the process and pipes for IO
*/
runProcessPty :: !FilePath ![String] !(Maybe String) !ProcessPtyOptions !*World -> (MaybeOSError (ProcessHandle, ProcessIO), *World)

/*
 * Options for the pseudoterminal
 * By default set all options to False
 *
 * Should the child process create a new terminal session (see man setsid for more info)
 * Should the child process control the terminal (check man ioctl/TIOCSCTTY for more info)
 * Should the terminal do nothing with the IO (check cfmakeraw or termios)
 */
:: ProcessPtyOptions =
	{ childInNewSession :: !Bool
	, childControlsTty  :: !Bool
	, useRawIO          :: !Bool
	}

defaultPtyOptions :: ProcessPtyOptions

/**
* Check if a process is still running
* @param Process handle to the process
* @return Return code if the process has finished, Nothing if the process is still running
*/
checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)

/**
* Wait for a process to terminate, closes the handle and returns the exit code
* @param Process handle to the process
* @return Exit code of the process
*/
waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)

/**
* runs a new process and wait for it to terminate
* @param Path to the executable
* @param a list of command-line arguments
* @param (optional) startup directory
* @return Exit code of the process
*/
callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)

/**
* read the currently available string from the pipe
* without blocking if no data is available
* @param the pipe to read from
* @return the data read from the pipe
*/
readPipeNonBlocking   :: !ReadPipe   !*World -> (!MaybeOSError String,   !*World)

/**
* read the currently available string from the pipe
* and blocks until some data is available
* @param the pipe to read from
* @return the data read from the pipe (at least one character)
*/
readPipeBlocking      :: !ReadPipe   !*World -> (!MaybeOSError String,   !*World)

/**
* read the currently available string from a number of pipes
* and blocks until some data is available for at least one pipe
* @param the pipes to read from
* @return the data read from the pipes (at least one character for at least one pipe)
*/
readPipeBlockingMulti :: ![ReadPipe] !*World -> (!MaybeOSError [String], !*World)

/**
* writes data to a pipe. may block if buffer is full
* @param the data to write
* @param the pipes to write to
* @return ()
*/
writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)

/**
* terminates the process if it is still running and releases the process handle resources.
* @param the process handle.
* @return ()
*/
terminateProcess :: !ProcessHandle !*World -> (!MaybeOSError (), !*World)

/**
* closes the IO channels of the process.
* @param the io channels
* @return ()
*/
closeProcessIO :: !ProcessIO !*World -> (!MaybeOSError (), !*World)

/**
* closes an individual IO channel
* @param the channel to close
* @return ()
*/
class closePipe a :: !a !*World -> (!MaybeOSError (), !*World)

instance closePipe WritePipe
instance closePipe ReadPipe
