definition module ArgEnv

:: EnvironmentVariable
	=	EnvironmentVariableUndefined
	|	EnvironmentVariable !.{#Char}

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable

getCommandLine :: {.{#Char}}
