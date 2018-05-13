implementation module System.CommandLine

import StdInt, StdList, StdEnum
import System._Pointer, System.OS

getCommandLine :: !*World -> (![String],!*World)
getCommandLine world
	# argc = readInt4Z global_argc 0
	# argv = derefInt global_argv
	= ([derefString (readInt argv (i << (IF_INT_64_OR_32 3 2)) ) \\ i <- [0..argc - 1]], world)
where
	//The pushLc ABC instruction should work on all platforms / architectures
	//Since it does not always work properly we use a fallback to pushL in some cases
	//Fallback currently neccessary on:	
	// - 64 bit windows, 

	//Global argc pointer
	global_argc :: Pointer
	global_argc = IF_POSIX_OR_WINDOWS global_argclc (IF_INT_64_OR_32 global_argcl global_argclc)
	
	global_argclc :: Pointer
	global_argclc = code {
		pushLc global_argc
	}
	global_argcl :: Pointer
	global_argcl = code {
		pushL global_argc
	}
	//Global argv pointer
	global_argv :: Pointer
	global_argv = IF_POSIX_OR_WINDOWS global_argvlc (IF_INT_64_OR_32 global_argvl global_argvlc)
	
	global_argvlc :: Pointer
	global_argvlc = code {
		pushLc global_argv
	}
	
	global_argvl :: Pointer
	global_argvl = code {
		pushL global_argv
	}

setReturnCode :: !Int !*World -> *World
setReturnCode i world = code {
    pushLc return_code
    :xxx
|    mov %eax,(%ebx)
    instruction 137
    instruction 3
    pop_b 2
    fill_a 0 1
    pop_a 1
}

