/*
	module owner: Ronny Wichers Schreur

	This module contains some file functions that are not in StdEnv
	It uses the object file from Directory 1.1, but with a different
	(stripped down) interface.
*/
implementation module filesystem

import StdEnv

// the import code is in CoclSystemDependent, because it is system dependent

// BEGIN copied from Directory.icl
createDirectoryC :: !String !*env -> (!Int, !*env)
createDirectoryC _ _
	= code
		{
			ccall createDirectoryC "S:I:A"
		}

findSingleFileC	::	!String !*env	-> (!ErrCode, !*env)
findSingleFileC _ _
	= code
		{
			ccall findSingleFileC "S:I:A"
		}

::	ErrCode			:== Int	// <>0 <=> error
::	DateTimeTuple	:== (!DateTuple, !TimeTuple)
::	DateTuple		:== (!Int, !Int, !Int, !Int)
::	TimeTuple		:== (!Int, !Int, !Int)

getCommonFileInfoC	::	!Bool !*env
				->	(!(!String, !(!Int, !Int), !DateTimeTuple, !Bool, !Bool), !*env)
getCommonFileInfoC _ _
	= code
		{
			ccall getCommonFileInfoC "I:VSIIIIIIIIIII:A"
		}

// createDirectoryC returns the following error codes:
M_NoDirError		:==  0
M_OtherDirError		:== -1
M_DoesntExist		:== -2
M_BadName			:== -3
M_NotEnoughSpace	:== -4
M_AlreadyExists		:== -5
M_NoPermission		:== -6

// END copied from Directory.icl

// return last modified time (local time) as "yyyymmddhhmmss" or "" on error
fmodificationtime :: {#Char} !*env -> (!{#Char}, !*env) | FileSystem env
fmodificationtime path env
	# (result, env)
		= findSingleFileC (path+++"\0") env
	| result <> 0
		=	("", env)
	#	((_, _, lastModifiedTuple, _, _), env)
			= getCommonFileInfoC False env
	=	(dateTimeTupleToString lastModifiedTuple, env)

dateTimeTupleToString :: DateTimeTuple -> {#Char}
dateTimeTupleToString ((year, month, day, _), (hours, minutes, seconds))
	=	string 4 year +++ string 2 month +++ string 2 day
			+++ string 2 hours +++ string 2 minutes +++ string 2 seconds
	where
		string :: !Int !Int -> {#Char}
		string minSize n
			=	pad (minSize - size s) +++ s
			where
				s
					=	toString n

				pad :: Int -> {#Char}
				pad n
					|	n > 0
						=	createArray n '0'
					// otherwise
						=	""
ensureDirectoryExists :: !{#Char} !*env -> (!Bool, !*env) | FileSystem env
// returned bool: now there is such a subfolder
ensureDirectoryExists path env
	# path_c_string = path +++ "\0"
	  (err_code, env) = createDirectoryC path_c_string env
	= (err_code==M_NoDirError || err_code==M_AlreadyExists, env)


