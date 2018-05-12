/*
	Version 1.0.3
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
definition module ArgEnv

:: EnvironmentVariable
	=	EnvironmentVariableUndefined
	|	EnvironmentVariable !.{#Char}

// get the value of an environment variable
getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable

// get the command line, first element is the command name,
//   arguments that are interpreted by the run-time system
//   (for example to set the heap size) are excluded
getCommandLine :: {.{#Char}}
