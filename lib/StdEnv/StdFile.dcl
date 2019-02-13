system module StdFile

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

//	File modes synonyms

FReadText	:== 0	//	Read from a text file
FWriteText	:== 1	//	Write to a text file
FAppendText	:== 2	//	Append to an existing text file
FReadData	:== 3	//	Read from a data file
FWriteData	:== 4	//	Write to a data file
FAppendData	:== 5	//	Append to an existing data file

//	Seek modes synonyms

FSeekSet	:== 0	//	New position is the seek offset
FSeekCur	:== 1	//	New position is the current position plus the seek offset
FSeekEnd	:== 2	//	New position is the size of the file plus the seek offset

::	*Files

//	Acces to the FileSystem (Files)

class FileSystem f where
	fopen :: !{#Char} !Int !*f -> (!Bool,!*File,!*f)
			/*	Opens a file for the first time in a certain mode (read, write or append, text or data).
				The boolean output parameter reports success or failure. */
	fclose :: !*File !*f -> (!Bool,!*f)
			/* Closes a file */
	stdio  :: !*f -> (!*File,!*f)
			/*	Open the 'Console' for reading and writing. */
	sfopen :: !{#Char} !Int !*f -> (!Bool,!File,!*f)
			/*	With sfopen a file can be opened for reading more than once.
				On a file opened by sfopen only the operations beginning with sf can be used.
				The sf... operations work just like the corresponding f... operations.
				They can't be used for files opened with fopen or freopen. */

instance FileSystem Files
instance FileSystem World

class FileEnv env where
	accFiles :: !.(*Files -> (.x,*Files)) !*env -> (!.x,!*env)
	appFiles :: !.(*Files -> *Files) !*env -> *env

instance FileEnv World

// openfiles	:: !*World -> (!*Files,!*World) // no longer supported
// closefiles	:: !*Files !*World -> *World    // no longer supported

freopen		:: !*File !Int -> (!Bool,!*File)
/*	Re-opens an open file in a possibly different mode.
	The boolean indicates whether the file was successfully closed before reopening. */

//	Reading from a File:

freadc		:: !*File -> (!Bool,!Char,!*File)
/*	Reads a character from a text file or a byte from a datafile.
	The boolean indicates succes or failure */

freadi		:: !*File -> (!Bool,!Int,!*File)
/*	Reads an Integer from a textfile by skipping spaces, tabs and newlines and
	then reading digits, which may be preceeded by a plus or minus sign.
	From a datafile freadi will just read four bytes (a Clean Int). */

freadr		:: !*File -> (!Bool,!Real,!*File)
/*	Reads a Real from a textfile by skipping spaces, tabs and newlines and then
	reading a character representation of a Real number.
	From a datafile freadr will just read eight bytes (a Clean Real). */

freads		:: ! *File !Int -> (!*{#Char},!*File)
/*	Reads n characters from a text or data file, which are returned as a String.
	If the file doesn't contain n characters the file will be read to the end
	of the file. An empty String is returned if no characters can be read. */

freadsubstring :: !Int !Int !*{#Char} !*File -> (!Int,!*{#Char},!*File)
	/*
	Reads n characters from a text or data file, which are returned in the string
	arg3 at positions arg1..arg1+arg2-1. If the file doesn't contain arg2 characters
	the file will be read to the end of the file, and the part of the string arg3 that
	could not be read will not be changed. The number of characters read, the modified
	string and the file are returned.
	*/

freadline	:: !*File -> (!*{#Char},!*File)
/*	Reads a line from a textfile. (including a newline character, except for the last
	line) freadline cannot be used on data files. */

//	Writing to a File:

fwritec		:: !Char !*File -> *File
/*	Writes a character to a textfile.
	To a datafile fwritec writes one byte (a Clean Char). */

fwritei		:: !Int !*File -> *File
/*	Writes an Integer (its textual representation) to a text file.
	To a datafile fwritei writes four bytes (a Clean Int). */

fwriter		:: !Real !*File -> *File
/*	Writes a Real (its textual representation) to a text file.
	To a datafile fwriter writes eight bytes (a Clean Real). */

fwrites		:: !{#Char} !*File -> *File
/*	Writes a String to a text or data file. */

fwritesubstring :: !Int !Int !{#Char} !*File -> *File
/*	Writes the characters at positions arg1..arg1+arg2-1 of string arg3 to
	a text or data file. */

class (<<<) infixl a :: !*File !a -> *File
/*	Overloaded write to file */

instance <<< Int
instance <<< Char
instance <<< {#Char}
instance <<< Real

//	Testing:

fend		:: !*File -> (!Bool,!*File)
/*	Tests for end-of-file. */

ferror		:: !*File -> (!Bool,!*File)
/*	Has an error occurred during previous file I/O operations? */

fposition	:: !*File -> (!Int,!*File)
/*	returns the current position of the file poInter as an Integer.
	This position can be used later on for the fseek function. */

fseek		:: !*File !Int !Int -> (!Bool,!*File)
/*	Move to a different position in the file, the first Integer argument is the offset,
	the second argument is a seek mode. (see above). True is returned if successful. */

//	Predefined files.

stderr		:: *File
/*	Open the 'Errors' file for writing only. May be opened more than once. */

//	Opening and reading Shared Files:

sfreadc		:: !File -> (!Bool,!Char,!File)
sfreadi		:: !File -> (!Bool,!Int,!File)
sfreadr		:: !File -> (!Bool,!Real,!File)
sfreads		:: !File !Int -> (!*{#Char},!File)
sfreadline	:: !File -> (!*{#Char},!File)
sfseek		:: !File !Int !Int -> (!Bool,!File)

sfend		:: !File -> Bool
sfposition	:: !File -> Int
/*	The functions sfend and sfposition work like fend and fposition, but don't return a
	new file on which other operations can continue. They can be used for files opened
	with sfopen or after fshare, and in guards for files opened with fopen or freopen. */

//	Convert a *File into:

fshare		:: !*File -> File
/*	Change a file so that from now it can only be used with sf... operations. */

fflush :: !*File -> (!Bool,!*File)
