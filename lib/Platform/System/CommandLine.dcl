definition module System.CommandLine
/**
* This module provides access to the commandline arguments of
* the program and enables setting a return code.
*/

/**
* Read the command line arguments from the world.
*
* @param The world
* @return The command line arguments
* @return The world
*/
getCommandLine :: !*World -> (![String],!*World)

/**
* Sets the return code of the program
*/
setReturnCode :: !Int !*World -> *World
