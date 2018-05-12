definition module iTasks.WF.Tasks.System
/**
* This module provides some tasks that influence the iTask system directly
*/

import iTasks.WF.Definition
/**
* Write a value to the server console output for tracing
*/
traceValue :: a -> Task a | iTask a

/**
* Terminates a running task server
*
* @param The exit code of the server process
*/
shutDown :: Int -> Task ()

