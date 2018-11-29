implementation module ArgEnv

import qualified System.CommandLine
import qualified System.Environment

import System._Unsafe
import Data.Maybe
import StdEnv

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable
getEnvironmentVariable s = case accUnsafe ('System.Environment'.getEnvironmentVariable s) of
	Nothing = EnvironmentVariableUndefined
	(Just s) = EnvironmentVariable {c\\c<-:s}

getCommandLine :: {.{#Char}}
getCommandLine = {{c\\c<-:a}\\a<-accUnsafe 'System.CommandLine'.getCommandLine}
