/*
	Version 1.0.3
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/
implementation module ArgEnv

import StdEnv

import code from "ArgEnvC.o"

:: CString :== Int
NULL :== 0

:: EnvironmentVariable
    =	EnvironmentVariableUndefined
    |   EnvironmentVariable !.{#Char}

getEnv es :== IF_INT_64_OR_32 (getEnv64 es) (getEnv32 es);

getEnv64 :: !{#Char} -> (!Int, !CString)
getEnv64 _
	= code inline {
			ccall ArgEnvGetEnvironmentVariableC "S-pp"
	}

getEnv32 :: !{#Char} -> (!Int, !CString)
getEnv32 _
	= code inline {
			ccall ArgEnvGetEnvironmentVariableC "S-II"
	}

getEnvironmentVariable :: !{#Char} -> *EnvironmentVariable
getEnvironmentVariable name
	| cString == NULL
		=	EnvironmentVariableUndefined
	| otherwise
		=	EnvironmentVariable (copy size cString)
	where
		(size, cString)
			=	getEnv (name +++ "\0")

copy length cString :== IF_INT_64_OR_32 (copy64 length cString) (copy32 length cString)

copy64 :: !Int !CString -> {#.Char}
copy64 length cString
	= code inline {
			create_array_	CHAR 0 1

			push_a	0
			ccall	ArgEnvCopyCStringToCleanStringC "pS-I"
			pop_b	1
	}

copy32 :: !Int !CString -> {#.Char}
copy32 length cString
	= code inline {
			create_array_	CHAR 0 1

			push_a	0
			ccall	ArgEnvCopyCStringToCleanStringC "IS-I"
			pop_b	1
	}

getCommandLineCount :: Int
getCommandLineCount 
	= code inline {
			ccall ArgEnvGetCommandLineCountC "-I"
	}

getCommandLineArgument n = IF_INT_64_OR_32 (getCommandLineArgument64 n) (getCommandLineArgument32 n)

getCommandLineArgument64 :: !Int -> (!Int, !Int)
getCommandLineArgument64 _
	= code inline {
			ccall ArgEnvGetCommandLineArgumentC "I-pp"
	}

getCommandLineArgument32 :: !Int -> (!Int, !Int)
getCommandLineArgument32 _
	= code inline {
			ccall ArgEnvGetCommandLineArgumentC "I-II"
	}

getArg :: !Int -> {#.Char}
getArg i
	=	copy size cString
	where
		(size, cString)
			=	getCommandLineArgument i

getCommandLine :: {.{#Char}}
getCommandLine
	=	{getArg i \\ i <- [0 .. getCommandLineCount-1]}
