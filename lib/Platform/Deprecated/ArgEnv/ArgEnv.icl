implementation module ArgEnv

import qualified System.CommandLine as CL
import qualified System.Environment as EV

import System._Unsafe
import Data.Maybe
import StdEnv

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable
getEnvironmentVariable s = case accUnsafe ('EV'.getEnvironmentVariable s) of
	Nothing = EnvironmentVariableUndefined
	(Just s) = EnvironmentVariable {c\\c<-:s}

getCommandLine :: {.{#Char}}
getCommandLine = {{c\\c<-:a}\\a<-accUnsafe 'CL'.getCommandLine}
