definition module System.Environment
/**
* Module for accessing environment variables
*/
import Data.Maybe

getEnvironmentVariable :: !String !*World -> (Maybe String, *World)

setEnvironmentVariable :: !String !String !*World -> *World

unsetEnvironmentVariable :: !String !*World -> *World
