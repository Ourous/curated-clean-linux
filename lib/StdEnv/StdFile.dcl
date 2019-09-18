system module StdFile

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
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

freopen		:: !*File !Int -> (!Bool,!*File)							:== code { .d 0 3 f i ; jsr reopenF ; .o 0 3 b f }
/*	Re-opens an open file in a possibly different mode.
	The boolean indicates whether the file was successfully closed before reopening. */

//	Reading from a File:

freadc		:: !*File -> (!Bool,!Char,!*File)							:== code { .d 0 2 f ; jsr readFC ; .o 0 4 b c f }
/*	Reads a character from a text file or a byte from a datafile.
	The boolean indicates succes or failure */

freadi		:: !*File -> (!Bool,!Int,!*File)							:== code { .d 0 2 f ; jsr readFI ; .o 0 4 b i f }
/*	Reads an Integer from a textfile by skipping spaces, tabs and newlines and
	then reading digits, which may be preceeded by a plus or minus sign.
	From a datafile freadi will just read four bytes (a Clean Int). */

freadr		:: !*File -> (!Bool,!Real,!*File)							:== code { .d 0 2 f ; jsr readFR ; .o 0 4 b r f }
/*	Reads a Real from a textfile by skipping spaces, tabs and newlines and then
	reading a character representation of a Real number.
	From a datafile freadr will just read eight bytes (a Clean Real). */

freads		:: ! *File !Int -> (!*{#Char},!*File)						:== code { .d 0 3 f i ; jsr readFS ; .o 1 2 f }
/*	Reads n characters from a text or data file, which are returned as a String.
	If the file doesn't contain n characters the file will be read to the end
	of the file. An empty String is returned if no characters can be read. */

freadsubstring :: !Int !Int !*{#Char} !*File -> (!Int,!*{#Char},!*File)	:== code { .d 1 4 i i f ; jsr readFString ; .o 1 3 i f }
	/*
	Reads n characters from a text or data file, which are returned in the string
	arg3 at positions arg1..arg1+arg2-1. If the file doesn't contain arg2 characters
	the file will be read to the end of the file, and the part of the string arg3 that
	could not be read will not be changed. The number of characters read, the modified
	string and the file are returned.
	*/

freadline	:: !*File -> (!*{#Char},!*File)								:== code { .d 0 2 f ; jsr readLineF ; .o 1 2 f }
/*	Reads a line from a textfile. (including a newline character, except for the last
	line) freadline cannot be used on data files. */

//	Writing to a File:

fwritec		:: !Char !*File -> *File									:== code { .d 0 3 c f ; jsr writeFC ; .o 0 2 f }
/*	Writes a character to a textfile.
	To a datafile fwritec writes one byte (a Clean Char). */

fwritei		:: !Int !*File -> *File										:== code { .d 0 3 i f ; jsr writeFI ; .o 0 2 f }
/*	Writes an Integer (its textual representation) to a text file.
	To a datafile fwritei writes four bytes (a Clean Int). */

fwriter		:: !Real !*File -> *File									:== code { .d 0 3 r f ; jsr writeFR ; .o 0 2 f }
/*	Writes a Real (its textual representation) to a text file.
	To a datafile fwriter writes eight bytes (a Clean Real). */

fwrites		:: !{#Char} !*File -> *File									:== code { .d 1 2 f ; jsr writeFS ; .o 0 2 f }
/*	Writes a String to a text or data file. */

fwritesubstring :: !Int !Int !{#Char} !*File -> *File					:== code { .d 1 4 i i f ; jsr writeFString ; .o 0 2 f }
/*	Writes the characters at positions arg1..arg1+arg2-1 of string arg3 to
	a text or data file. */

class (<<<) infixl a :: !*File !a -> *File
/*	Overloaded write to file */

instance <<< Int 		:: !*File !Int -> *File							:== code { push_b 2 ; update_b 2 3 ; update_b 1 2 ; updatepop_b 0 1 ; .d 0 3 i f ; jsr writeFI ; .o 0 2 f }
instance <<< Char		:: !*File !Char -> *File						:== code { push_b 2 ; update_b 2 3 ; update_b 1 2 ; updatepop_b 0 1 ; .d 0 3 c f ; jsr writeFC ; .o 0 2 f }
instance <<< {#Char}	:: !*File !{#Char} -> *File						:== code { .d 1 2 f ; jsr writeFS ; .o 0 2 f }
instance <<< Real		:: !*File !Real -> *File						:== code { push_b 2 ; update_b 2 3 ; update_b 1 2 ; updatepop_b 0 1 ; .d 0 3 r f ; jsr writeFR ; .o 0 2 f }

//	Testing:

fend		:: !*File -> (!Bool,!*File)									:== code { .d 0 2 f ; jsr endF ; .o 0 3 b f }
/*	Tests for end-of-file. */

ferror		:: !*File -> (!Bool,!*File)									:== code { .d 0 2 f ; jsr errorF ; .o 0 3 b f }
/*	Has an error occurred during previous file I/O operations? */

fposition	:: !*File -> (!Int,!*File)									:== code { .d 0 2 f ; jsr positionF ; .o 0 3 i f }
/*	returns the current position of the file poInter as an Integer.
	This position can be used later on for the fseek function. */

fseek		:: !*File !Int !Int -> (!Bool,!*File)						:== code { .d 0 4 f i i ; jsr seekF ; .o 0 3 b f }
/*	Move to a different position in the file, the first Integer argument is the offset,
	the second argument is a seek mode. (see above). True is returned if successful. */

//	Predefined files.

stderr		:: *File													:== code { .d 0 0 ; jsr	stderrF ; .o 0 2 f }
/*	Open the 'Errors' file for writing only. May be opened more than once. */

//	Opening and reading Shared Files:

sfreadc		:: !File -> (!Bool,!Char,!File)								:== code { .d 0 2 f ; jsr readSFC ; .o 0 4 b c f }
sfreadi		:: !File -> (!Bool,!Int,!File)								:== code { .d 0 2 f ; jsr readSFI ; .o 0 4 b i f }
sfreadr		:: !File -> (!Bool,!Real,!File)								:== code { .d 0 2 f ; jsr readSFR ; .o 0 4 b r f }
sfreads		:: !File !Int -> (!*{#Char},!File)							:== code { .d 0 3 f i ; jsr readSFS ; .o 1 2 f }
sfreadline	:: !File -> (!*{#Char},!File)								:== code { .d 0 2 f ; jsr readLineSF ; .o 1 2 f }
sfseek		:: !File !Int !Int -> (!Bool,!File)							:== code { .d 0 4 f i i ; jsr seekSF ; .o 0 3 b f }

sfend		:: !File -> Bool											:== code { .d 0 2 f ; jsr endSF ; .o 0 1 b }
sfposition	:: !File -> Int												:== code { .d 0 2 f ; jsr positionSF ; .o 0 1 i }
/*	The functions sfend and sfposition work like fend and fposition, but don't return a
	new file on which other operations can continue. They can be used for files opened
	with sfopen or after fshare, and in guards for files opened with fopen or freopen. */

//	Convert a *File into:

fshare		:: !*File -> File											:== code { .d 0 2 f ; jsr shareF ; .o 0 2 f }
/*	Change a file so that from now it can only be used with sf... operations. */

fflush :: !*File -> (!Bool,!*File)										:== code { .d 0 2 f ; jsr flushF ; .o 0 3 bf }
