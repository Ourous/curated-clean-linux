implementation module System.Environment
/**
* Module for accessing environment variables
*/
import StdOverloaded, StdInt
import Data.Maybe, System._Pointer

getEnvironmentVariable :: !String !*World -> (Maybe String, *World)
getEnvironmentVariable name world
	# ptr = getenvC (packString name)
	| ptr == 0	= (Nothing, world)
				= (Just (derefString ptr), world)
	where
		getenvC :: !{#Char} -> Pointer
		getenvC a0 = code {
			ccall getenv "s:p"
		}

setEnvironmentVariable :: !String !String !*World -> *World
setEnvironmentVariable name value world
	# (_,world) = setenvC (packString name) (packString value) 1 world
	= world
	where
		setenvC :: !{#Char} !{#Char} !Int !*World -> (!Int, !*World)
		setenvC a0 a1 a2 a3 = code {
			ccall setenv "ssI:I:A"
		}

unsetEnvironmentVariable :: !String !*World -> *World
unsetEnvironmentVariable name world
	# (_,world) = unsetenvC (packString name) world
	= world
	where
		unsetenvC :: !{#Char} !*World -> (!Int, !*World)
		unsetenvC a0 a1 = code {
			ccall unsetenv "s:I:A"
		}
