implementation module System.Process

//StdEnv
import StdArray
import StdBool
import StdClass
import StdInt
import StdList
import StdString
import StdMisc
import StdFunc

//Data
import Data.Maybe
from Data.List import maximum

//System
import System.FilePath
import System.File
import System.OSError
import System._Pointer
import System._Posix

import Text.GenJSON

:: WritePipe = WritePipe !Int
:: ReadPipe  = ReadPipe  !Int

derive JSONEncode WritePipe, ReadPipe
derive JSONDecode WritePipe, ReadPipe

runProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError ProcessHandle, *World)
runProcess path args mCurrentDirectory world = runProcessFork
    (runProcessChildProcessExec path args mCurrentDirectory)
    runProcessParentProcessCheckError
    world

runProcessIO :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError (ProcessHandle, ProcessIO), *World)
runProcessIO path args mCurrentDirectory world
    // StdIn
    # (pipeStdIn, world) = openPipe world
    | isError pipeStdIn = (liftError pipeStdIn, world)
    # (pipeStdInOut, pipeStdInIn) = fromOk pipeStdIn
    // StdOut
    # (pipeStdOut, world) = openPipe world
    | isError pipeStdOut = (liftError pipeStdOut, world)
    # (pipeStdOutOut, pipeStdOutIn) = fromOk pipeStdOut
    // StdErr
    # (pipeStdErr, world) = openPipe world
    | isError pipeStdErr = (liftError pipeStdErr, world)
    # (pipeStdErrOut, pipeStdErrIn) = fromOk pipeStdErr
    = runProcessFork (childProcess  pipeStdInOut pipeStdInIn pipeStdOutOut pipeStdOutIn pipeStdErrOut pipeStdErrIn)
                     (parentProcess pipeStdInOut pipeStdInIn pipeStdOutOut pipeStdOutIn pipeStdErrOut pipeStdErrIn)
                     world
where
    childProcess :: !Int !Int!Int !Int!Int !Int !Int !Int !*World -> (!MaybeOSError (!ProcessHandle, !ProcessIO), !*World)
    childProcess pipeStdInOut pipeStdInIn pipeStdOutOut pipeStdOutIn pipeStdErrOut pipeStdErrIn pipeExecErrorOut pipeExecErrorIn world
        //redirect stdin/out/err to pipes
        # (res, world) = dup2 pipeStdInOut STDIN_FILENO world
        | res == -1    = getLastOSError world
        # (res, world) = close pipeStdInIn world
        | res == -1    = getLastOSError world

        # (res, world) = dup2 pipeStdOutIn STDOUT_FILENO world
        | res == -1    = getLastOSError world
        # (res, world) = close pipeStdOutOut world
        | res == -1    = getLastOSError world

        # (res, world) = dup2 pipeStdErrIn STDERR_FILENO world
        | res == -1    = getLastOSError world
        # (res, world) = close pipeStdErrOut world
        | res == -1    = getLastOSError world
		# (_, world)   = runProcessChildProcessExec path args mCurrentDirectory pipeExecErrorOut pipeExecErrorIn world
        // this is never executed as 'childProcessExec' never returns
        = (undef, world)

    parentProcess :: !Int !Int!Int !Int!Int !Int !Int !Int !Int !*World -> (!MaybeOSError (!ProcessHandle, !ProcessIO), !*World)
    parentProcess pipeStdInOut pipeStdInIn pipeStdOutOut pipeStdOutIn pipeStdErrOut pipeStdErrIn pid pipeExecErrorOut pipeExecErrorIn world
        # (res, world)       = close pipeStdInOut world
        | res == -1          = getLastOSError world
        # (res, world)       = close pipeStdOutIn world
        | res == -1          = getLastOSError world
        # (res, world)       = close pipeStdErrIn world
        | res == -1          = getLastOSError world
        # (mbPHandle, world) = runProcessParentProcessCheckError pid pipeExecErrorOut pipeExecErrorIn world
        | isError mbPHandle  = (liftError mbPHandle, world)
		= ( Ok ( fromOk mbPHandle
               , { stdIn  = WritePipe pipeStdInIn
                 , stdOut = ReadPipe  pipeStdOutOut
                 , stdErr = ReadPipe  pipeStdErrOut
                 }
               )
          , world
          )

defaultPtyOptions :: ProcessPtyOptions
defaultPtyOptions =
	{ProcessPtyOptions
	|childInNewSession = True
	,childControlsTty  = True
	,useRawIO          = False
	}

runProcessPty :: !FilePath ![String] !(Maybe String) !ProcessPtyOptions !*World -> (MaybeOSError (ProcessHandle, ProcessIO), *World)
runProcessPty path args mCurrentDirectory opts world
	# (masterPty, world) = posix_openpt (O_RDWR bitor O_NOCTTY) world
	| masterPty == -1    = getLastOSError world
	# (slavePty, world)  = grantpt masterPty world
	| slavePty == -1     = getLastOSError world
	# (slavePty, world)  = unlockpt masterPty world
	| slavePty == -1     = getLastOSError world
	# (slavePty, world)  = ptsname masterPty world
	| slavePty == 0      = getLastOSError world
	# (slavePty, world)  = open slavePty (O_RDWR bitor O_NOCTTY) world
	| slavePty == -1     = getLastOSError world
	= runProcessFork (childProcess  slavePty masterPty)
	                 (parentProcess slavePty masterPty)
	                 world
where
	childProcess :: !Int !Int !Int !Int !*World -> (!MaybeOSError (!ProcessHandle, !ProcessIO), !*World)
	childProcess slavePty masterPty pipeExecErrorOut pipeExecErrorIn world
		//Disable echo
		//sizeof(struct termios) on linux gives 60, lets play safe
		# termios      = malloc 64
		| termios == 0 = abort "malloc failed"
		# (res, world) = tcgetattr slavePty termios world
		| res == -1    = getLastOSError world

		//Apply the termios transformation
		# world        = (if opts.useRawIO cfmakeraw (flip const)) termios world
		# (res, world) = tcsetattr slavePty TCSANOW termios world
		| res == -1    = getLastOSError world
		# world        = freeSt termios world

		//Close the master side
		# (res, world) = close masterPty world
		| res == -1    = getLastOSError world
		
		//Connect the pty to the stdio
		# (res, world) = dup2 slavePty STDIN_FILENO world
		| res == -1    = getLastOSError world
		# (res, world) = dup2 slavePty STDOUT_FILENO world
		| res == -1    = getLastOSError world
		# (res, world) = dup2 slavePty STDERR_FILENO world
		| res == -1    = getLastOSError world

		//Set the correct ioctl settings
		# world        = (if opts.childInNewSession setsid id) world
		# (res, world) = if opts.childControlsTty (0, world)
			(ioctl TCSANOW TIOCSCTTY 1 world)
		| res == -1    = getLastOSError world
		//Start
		# (_, world)   = runProcessChildProcessExec path args mCurrentDirectory pipeExecErrorOut pipeExecErrorIn world
		// this is never executed as 'childProcessExec' never returns
		= (undef, world)

	parentProcess :: !Int !Int !Int !Int !Int !*World -> (!MaybeOSError (!ProcessHandle, !ProcessIO), !*World)
	parentProcess slavePty masterPty pid pipeExecErrorOut pipeExecErrorIn world
		//sizeof(struct termios) on linux gives 60, lets play safe
		# termios          = malloc 64
		| termios == 0     = abort "malloc failed"
		# (res, world)     = tcgetattr masterPty termios world
		| res == -1        = getLastOSError world

		//Apply the termios transformation
		# world            = (if opts.useRawIO cfmakeraw (flip const)) termios world
		# (res, world)     = tcsetattr slavePty TCSANOW termios world
		| res == -1        = getLastOSError world

		//Close the slave side
		# (res, world)     = close slavePty world
		| res == -1        = getLastOSError world
		# world            = freeSt termios world
		//Start
		# (mbPHandle, world) = runProcessParentProcessCheckError pid pipeExecErrorOut pipeExecErrorIn world
		| isError mbPHandle  = (liftError mbPHandle, world)
		= ( Ok ( fromOk mbPHandle
		       , { stdIn  = WritePipe masterPty
		         , stdOut = ReadPipe masterPty
		         , stdErr = ReadPipe masterPty
		         }
		       )
		  , world)

runProcessFork :: !(    Int Int *World -> (!MaybeOSError a, !*World))
                  !(Int Int Int *World -> (!MaybeOSError a, !*World))
                  !*World
               -> (!MaybeOSError a, !*World)
runProcessFork childProcess parentProcess world
    // create pipe to pass errors of 'execvp' from child to parent
    # (pipeExecError, world) = openPipe world
    | isError pipeExecError = (liftError pipeExecError, world)
    # (pipeExecErrorOut, pipeExecErrorIn) = fromOk pipeExecError
	//Fork
	# (pid, world) = fork world
    | pid == 0  = childProcess      pipeExecErrorOut pipeExecErrorIn world
    | pid > 0   = parentProcess pid pipeExecErrorOut pipeExecErrorIn world
    | otherwise = getLastOSError world

// this function never returns, as the process is replaced by 'execvp'
// all errors before 'execvp' succeeds are passed on to the parent process
runProcessChildProcessExec :: !FilePath ![String] !(Maybe String) !Int !Int !*World -> (!MaybeOSError ProcessHandle, !*World)
runProcessChildProcessExec path args mCurrentDirectory pipeExecErrorOut pipeExecErrorIn world
    # (res, world) = close pipeExecErrorOut world
    | res == -1    = passLastOSErrorToParent pipeExecErrorIn world
    // set FD_CLOEXEC such that parent is informed if 'execvp' succeeds
    # (res, world) = fcntlArg pipeExecErrorIn F_SETFD FD_CLOEXEC world
    | res == -1    = passLastOSErrorToParent pipeExecErrorIn world
	//Chdir
	# (res,world) = case mCurrentDirectory of
		Just dir -> chdir (packString dir) world
		Nothing  -> (0, world)
	| res <> 0 = passLastOSErrorToParent pipeExecErrorIn world
	//Exec
	# (argv, world) = runProcessMakeArgv [path:args] world
	# (res, world)  = execvp (path +++ "\0") argv world
    // this part is only executed if 'execvp' failed
    // in this case the error is passed to the parent
    = passLastOSErrorToParent pipeExecErrorIn world
where
    passLastOSErrorToParent :: !Int !*World -> (MaybeOSError ProcessHandle, *World)
    passLastOSErrorToParent pipe world
        # (errno, world) = errno world
        # (_, world)     = writePipe (toString errno) (WritePipe pipe) world
        // potential error of 'writePipe' cannot be handled properly
	    = exit errno world

runProcessParentProcessCheckError :: !Int !Int !Int !*World -> (!MaybeOSError ProcessHandle, !*World)
runProcessParentProcessCheckError pid pipeExecErrorOut pipeExecErrorIn world
        # (res, world)     = close pipeExecErrorIn world
        | res == -1        = getLastOSError world
        // this blocks until either an error is written to the pipe or 'execvp' succeeds
        # (mbErrno, world) = readPipeBlocking (ReadPipe pipeExecErrorOut) world
        | isError mbErrno  = (liftError mbErrno, world)
        # errno            = fromOk mbErrno
        | errno <> ""      = (Error (osErrorCodeToOSError (toInt errno)), world)
        # (res, world)     = close pipeExecErrorOut world
        | res == -1        = getLastOSError world
		= (Ok {ProcessHandle| pid = pid}, world)

runProcessMakeArgv :: [String] *World -> (!{#Pointer}, *World)
runProcessMakeArgv argv_list world
	# args_size = argvLength argv_list 0
	  args_string = createArgsString args_size argv_list
	  args_memory = malloc args_size
	| args_memory == 0
		= abort "malloc failed"
	# args_memory = memcpy_string_to_pointer args_memory args_string args_size
	# (argv, args_memory) = readP (createArgv argv_list) args_memory
	= (argv, world)
where
	argvLength [a:as] l
		= argvLength as (l+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4)))
	argvLength [] l
		= l

	createArgsString args_size argv_list
		# s = createArray args_size '\0'
		= copyArgs argv_list 0 s
	where
		copyArgs [a:as] i s
			# s = copyChars 0 a i s
			= copyArgs as (i+((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))) s
		copyArgs [] i s
			= s

		copyChars :: !Int !{#Char} !Int !*{#Char} -> *{#Char}
		copyChars ai a si s
			| ai<size a
				# s = {s & [si]=a.[ai]}
				= copyChars (ai+1) a (si+1) s
			= s

	createArgv argv_list args_memory
		# n_args = length argv_list
		# argv = createArray (n_args+1) 0;
		= fillArgv 0 argv_list argv args_memory 
	where
		fillArgv :: !Int ![{#Char}] !*{#Pointer} !Int -> *{#Pointer}
		fillArgv arg_n [a:as] argv args_memory
			# argv = {argv & [arg_n]=args_memory}
			  args_memory = args_memory + ((size a +(IF_INT_64_OR_32 8 4)) bitand (IF_INT_64_OR_32 -8 -4))
			= fillArgv (arg_n+1) as argv args_memory
		fillArgv arg_n [] argv args_memory
			= {argv & [arg_n]=0}

openPipe :: !*World -> (MaybeOSError (Int, Int), !*World)
openPipe world
    # ptr = malloc 8
    | ptr == 0 = abort "malloc failed"
    # (res, world) = pipe ptr world
    | res == -1
        # world = freeSt ptr world
        = getLastOSError world
    # (rEnd, ptr)  = readP (\ptr -> readInt4S ptr 0) ptr
    # (wEnd, ptr)  = readP (\ptr -> readInt4S ptr 4) ptr
    # world = freeSt ptr world
    = (Ok (rEnd, wEnd), world)

checkProcess :: !ProcessHandle !*World -> (MaybeOSError (Maybe Int), *World)
checkProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status WNOHANG world //Non-blocking wait :)
	| ret == 0
		= (Ok Nothing, world)	
	| ret == pid	
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok (Just exitCode), world)
	| otherwise
		= getLastOSError world

waitForProcess :: !ProcessHandle !*World -> (!MaybeOSError Int, !*World)
waitForProcess {pid} world
	# status		= createArray 1 0
	# (ret,world)	= waitpid pid status 0 world //Blocking wait
	| ret == pid
		# exitCode = (status.[0] >> 8) bitand 0xFF
		= (Ok exitCode, world)
	| otherwise
		= getLastOSError world

	
callProcess :: !FilePath ![String] !(Maybe String) !*World -> (MaybeOSError Int, *World)
callProcess path args mCurrentDirectory world
	# (res, world) = runProcess path args mCurrentDirectory world
	= case res of 
		Ok handle	= waitForProcess handle world
		Error e		= (Error e, world)

readPipeNonBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)
readPipeNonBlocking (ReadPipe fd) world
    # ptr           = malloc 4
    # (res, world) = ioctl fd FIONREAD ptr world
    | res == -1
        # world = freeSt ptr world
        = getLastOSError world
    # (n, ptr)      = readP (\ptr -> readInt4Z ptr 0) ptr
    # world         = freeSt ptr world
    | n == 0        = (Ok "", world)
    # buffer        = malloc n
    # (res, world) = read fd buffer n world
    | res == -1
        # world = freeSt buffer world
        = getLastOSError world
    # (str, buffer) = readP (\ptr -> derefCharArray ptr n) buffer
    # world = freeSt buffer world
    = (Ok str, world)

readPipeBlocking :: !ReadPipe !*World -> (!MaybeOSError String, !*World)
readPipeBlocking pipe=:(ReadPipe fd) world
    # readfds = malloc 128
    // init array
    # readfds = seq [\ptr -> writeIntElemOffset ptr i 0 \\ i <- [0..IF_INT_64_OR_32 15 31]] readfds
    // set bit for fd
    # readfds = readPipeBlockingSetFdBit fd readfds
    // wait
    # (res, world) = select_ (fd + 1) readfds 0 0 0 world
    # world = freeSt readfds world
    | res == -1 = getLastOSError world
    = readPipeNonBlocking pipe world

readPipeBlockingMulti :: ![ReadPipe] !*World -> (!MaybeOSError [String], !*World)
readPipeBlockingMulti pipes world
    #readfds = malloc 128
    // init array
    #readfds = seq [\ptr -> writeIntElemOffset ptr i 0 \\ i <- [0..IF_INT_64_OR_32 15 31]] readfds
    // set bits for fds
    #readfds = seq [readPipeBlockingSetFdBit fd \\ ReadPipe fd <- pipes] readfds
    // wait
    # (res, world) = select_ (maxFd + 1) readfds 0 0 0 world
    # world = freeSt readfds world
    | res == -1 = getLastOSError world
    = seq [ \(res, world) -> case res of
                Ok res`
                    #(r, world) = readPipeNonBlocking pipe world
                    = (seqErrors r (\r` -> Ok [r`:res`]), world)
                error =  (error, world)
            \\ pipe <- reverse pipes
          ]
          (Ok [], world)
where
    maxFd = maximum [fd \\ ReadPipe fd <- pipes]

readPipeBlockingSetFdBit :: !Int !Pointer -> Pointer
readPipeBlockingSetFdBit fd ptr
    # offset  = fromInt fd / IF_INT_64_OR_32 64 32
    # val = (readIntElemOffset ptr offset) bitor (1 << (fd rem IF_INT_64_OR_32 64 32))
    = writeIntElemOffset ptr offset val

writePipe :: !String !WritePipe !*World -> (!MaybeOSError (), !*World)
writePipe str (WritePipe fd) world
    #(res, world) = write fd str (size str) world
    | res == -1 = getLastOSError world
    = (Ok (), world)

terminateProcess :: !ProcessHandle !*World -> (!MaybeOSError (), !*World)
terminateProcess pHandle=:{pid} world
    # (res, world) = kill pid 15 world // Termination signal
    | res == -1    = getLastOSError world
    // otherwise process will remain as zombie
    # status       = createArray 1 0
    # (res, world) = waitpid pid status 0 world
    | res == -1    = getLastOSError world
    = (Ok (), world)

closeProcessIO :: !ProcessIO !*World -> (!MaybeOSError (), !*World)
closeProcessIO {stdIn = WritePipe fdStdIn, stdOut = ReadPipe fdStdOut, stdErr = ReadPipe fdStdErr} world
    # (res, world) = close fdStdIn world
    | res == -1    = getLastOSError world
    // if 'runProcessPty' is used, the same file descriptor is used for stdIn, stdOut & stdErr
    | fdStdIn == fdStdOut = (Ok (), world)
    # (res, world) = close fdStdOut world
    | res == -1    = getLastOSError world
    # (res, world) = close fdStdErr world
    | res == -1    = getLastOSError world
    = (Ok (), world)

instance closePipe WritePipe
where
    closePipe :: !WritePipe !*World -> (!MaybeOSError (), !*World)
    closePipe (WritePipe pipe) world = closePipe` pipe world

instance closePipe ReadPipe
where
    closePipe :: !ReadPipe !*World -> (!MaybeOSError (), !*World)
    closePipe (ReadPipe pipe) world = closePipe` pipe world

closePipe` :: !Int !*World -> (!MaybeOSError (), !*World)
closePipe` pipe world
	# (res, world) = close pipe world
	| res <> 0     = getLastOSError world
	= (Ok (), world)
