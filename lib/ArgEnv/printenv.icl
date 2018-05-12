/*
	Version 1.0.3
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
module printenv

import StdEnv, ArgEnv

Start
	| argc == 1
		=	format (getEnvironmentVariable argv.[1])
	| otherwise
		=	"usage: " +++ argv.[0] +++ " <variable>\n"
	where
		argc
			=	size argv - 1
		argv
			=	getCommandLine

		format EnvironmentVariableUndefined
			=	""
		format (EnvironmentVariable value)
			=	value +++ "\n"
