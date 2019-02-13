/*
	module owner: Ronny Wichers Schreur

	This module contains some file functions that are not in StdEnv
	It uses the object file from Directory 1.1, but with a different
	(stripped down) interface.
*/
definition module filesystem

from StdFile import class FileSystem,::Files

// return last modified time (local time) as "yyyymmddhhmmss" or "" on error
fmodificationtime :: {#Char} !*env -> (!{#Char}, !*env) | FileSystem env

// create a directory, if it doesn't exist already
ensureDirectoryExists :: !{#Char} !*env -> (!Bool, !*env) | FileSystem env
