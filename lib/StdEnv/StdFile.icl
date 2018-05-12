implementation module StdFile

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1994 University of Nijmegen
// ****************************************************************************************

//	File modes synonyms

import	StdClass, StdMisc, StdArray, StdInt

//	File modes

FReadText	:== 0	//	read from a text file
FWriteText	:== 1	//	write to a text file
FAppendText :== 2	//	append to an existing text file
FReadData	:== 3	//	read from a data file
FWriteData	:== 4	//	write to a data file
FAppendData :== 5	//	append to an existing data file

//	Seek modes

FSeekSet :== 0		//	new position is the seek offset
FSeekCur :== 1		//	new position is the current position plus the seek offset
FSeekEnd :== 2		//	new position is the size of the file plus the seek offset

::	* Files = Files;

class FileSystem f where
	fopen		:: !{#Char} !Int !*f -> (!Bool,!*File,!*f)
	/*	Opens a file for the first time in a certain mode (read, write or append, text or data).
		The boolean output parameter reports success or failure. */
	fclose		:: !*File !*f -> (!Bool,!*f)
	stdio		:: !*f -> (!*File,!*f)
	/*	Open the 'Console' for reading and writing. */
	sfopen		:: !{#Char} !Int !*f -> (!Bool,!File,!*f)
	/*	With sfopen a file can be opened for reading more than once.
		On a file opened by sfopen only the operations beginning with sf can be used.
		The sf... operations work just like the corresponding f... operations.
		They can't be used for files opened with fopen or freopen. */

instance FileSystem Files
where
	fopen :: !{#Char} !Int !*Files -> (!Bool,!*File,!*Files)
	fopen s i w
		# (b,f) = fopen_ s i;
		= (b,f,w);

	fclose :: !*File !*Files -> (!Bool,!*Files)
	fclose f w
		# b = fclose_ f
		= (b,w);

	stdio :: !*Files -> (!*File,!*Files)
	stdio w
		# f = stdio_
		= (f,w);

	sfopen :: !{#Char} !Int !*Files -> (!Bool,!File,!*Files)
	sfopen s i w
		# (b,f) = sfopen_ s i
		= (b,f,w)

instance FileSystem World
where
	fopen::!{#Char} !Int !*World -> (!Bool,!*File,!*World)
	fopen s i w
		# (b,f) = fopen_ s i;
		= (b,f,w);

	fclose :: !*File !*World -> (!Bool,!*World)
	fclose f w
		# b = fclose_ f
		= (b,w);

	stdio::!*World -> (!*File,!*World)
	stdio w
		# f = stdio_
		= (f,w);
	sfopen::!{#Char} !Int !*World -> (!Bool,!File,!*World)
	sfopen s i w
		# (b,f) = sfopen_ s i
		= (b,f,w)

fopen_ ::!{#Char} !Int -> (!Bool,!*File)
fopen_ s i = code inline {
	.d 1 1 i
		jsr	openF
	.o 0 3 b f
	}

fclose_ :: !*File -> Bool
fclose_ f = code inline {
	.d 0 2 f
		jsr	closeF
	.o 0 1 b
	}

/*	Open the 'Console' for reading and writing. */
stdio_ :: *File
stdio_ = code inline {
	.d 0 0
		jsr	stdioF
	.o 0 2 f
	}

sfopen_ ::!{#Char} !Int -> (!Bool,!File)
sfopen_ s i
= code inline {
	.d 1 1 i
		jsr	openSF
	.o 0 3 b f
	}

/*
	openfiles::!*World -> (!*Files,!*World)
	openfiles world
	| (1 bitand w) == 0
		=	OpenFiles2 (StoreWorld (w bitor 1) world)
		=	abort "openfiles: This world doesn't contain files"
			where  w = LoadWorld world

	OpenFiles2::!*World -> (!*Files,!*World)
	OpenFiles2 w
	= code inline {
		pushI 0
	}

	LoadWorld :: !World -> Int;
	LoadWorld w = code inline {
		pushI_a 0
		pop_a 1
	};

	StoreWorld :: !Int !World -> *World;
	StoreWorld i w = code inline {
		fillI_b 0 1
		pop_b 1
		pop_a 1
	};

	closefiles::!*Files !*World -> *World
	closefiles f world
	=	CloseFiles2 f (StoreWorld ((LoadWorld world) bitand (-2)) world)

	CloseFiles2::!*Files !*World -> *World
	CloseFiles2 f w
	= code inline {
		pop_b 1
		fill_a 0 1
		pop_a 1
	}
*/

freopen::!*File !Int -> (!Bool,!*File)
/*	Re-opens an open file in a possibly different mode.
	The boolean indicates whether the file was successfully closed before reopening. */
freopen f m 
	= code inline {
		.d 0 3 f i
			jsr reopenF
		.o 0 3 b f
	}

//	Input. The boolean output parameter reports success or failure of the operations.

freadc::!*File -> (!Bool,!Char,!*File)
/*	Reads a character from a text file or a byte from a datafile. */
freadc f
	= code inline {
		.d 0 2 f
			jsr	readFC
		.o 0 4 b c f
	}

freadi::!*File -> (!Bool,!Int,!*File)
/*	Reads an integer from a textfile by skipping spaces, tabs and newlines and
	then reading digits, which may be preceeded by a plus or minus sign.
	From a datafile freadi will just read four bytes (a Clean Int). */
freadi f
	= code inline {
		.d 0 2 f
			jsr	readFI
		.o 0 4 b i f
	}

freadr::!*File -> (!Bool,!Real,!*File)
/*	Reads a real from a textfile by skipping spaces, tabs and newlines and then
	reading a character representation of a real number.
	From a datafile freadr will just read eight bytes (a Clean Real). */
freadr f
	= code inline {
		.d 0 2 f
			jsr	readFR
		.o 0 4 b r f
	}

freads::!*File !Int -> (!*{#Char},!*File)
/*	Reads n characters from a text or data file, which are returned as a {#Char}.
	If the file doesn't contain n characters the file will be read to the end
	of the file. An empty {#Char} is returned if no characters can be read. */
freads f l
	= code inline {
		.d 0 3 f i
			jsr readFS
		.o 1 2 f
	}

freadsubstring :: !Int !Int !*{#Char} !*File -> (!Int,!*{#Char},!*File)
	/*
	Reads n characters from a text or data file, which are returned in the string s
	at positions i..i+n-1. If the file doesn't contain n characters the file will
	be read to the end of the file, and the part of the string s that could not be
	read will not be changed. The number of characters read, the modified string
	and the file are returned.
	*/
freadsubstring i n s f
	= code {
		.inline freadsubstring
		.d 1 4 i i f
			jsr readFString
		.o 1 3 i f
		.end
	}

freadline::!*File -> (!*{#Char},!*File)
/*	Reads a line from a textfile. (including a newline character, except for the last
	line) freadline cannot be used on data files. */
freadline f
	= code inline {
		.d 0 2 f
			jsr readLineF
		.o 1 2 f
	}

//	Output. Use FError to check for write errors.

fwritec::!Char !*File -> *File
/*	Writes a character to a textfile.
	To a datafile fwritec writes one byte (a Clean Char). */
fwritec c f
	= code inline {
		.d 0 3 c f
			jsr writeFC
		.o 0 2 f
	}

fwritei::!Int !*File -> *File
/*	Writes an integer (its textual representation) to a text file.
	To a datafile fwritec writes four bytes (a Clean Int). */
fwritei i f 
	= code inline {
		.d 0 3 i f
			jsr writeFI
		.o 0 2 f
	}

fwriter::!Real !*File -> *File
/*	Writes a real (its textual representation) to a text file.
	To a datafile fwriter writes eight bytes (a Clean Real). */
fwriter r f
	= code inline {
		.d 0 3 r f
			jsr writeFR
		.o 0 2 f
	}

fwrites::!{#Char} !*File -> *File
/*	Writes a {#Char} to a text or data file. */
fwrites s f 
	= code inline {
		.d 1 2 f
			jsr writeFS
		.o 0 2 f
	}

fwritesubstring :: !Int !Int !{#Char} !*File -> *File
/*	Writes the characters at positions i..i+n-1 of string s to a text or data file. */
fwritesubstring i n s f 
	= code {
		.inline fwritesubstring
		.d 1 4 i i f
			jsr writeFString
		.o 0 2 f
		.end
	}

//	Tests

fend::!*File -> (!Bool,!*File)
/*	Tests for end-of-file. */
fend f 
	= code inline {
		.d 0 2 f
			jsr endF
		.o 0 3 b f
	}

ferror::!*File -> (!Bool,!*File)
/*	Has an error occurred during previous file I/O operations? */
ferror f 
	= code inline {
		.d 0 2 f
			jsr errorF
		.o 0 3 b f
	}

fposition::!*File -> (!Int,!*File)
/*	returns the current position of the file pointer as an integer.
	This position can be used later on for the fseek function. */
fposition f 
	= code inline {
		.d 0 2 f
			jsr positionF
		.o 0 3 i f
	}

fseek::!*File !Int !Int -> (!Bool,!*File)
/*	Move to a different position in the file, the first integer argument is the offset,
	the second argument is a seek mode. (see above). True is returned if successful. */
fseek f p m 
	= code inline {
		.d 0 4 f i i
			jsr seekF
		.o 0 3 b f
	}


//	Predefined files.

stderr::   *File
/*	Open the 'Errors' file for writing only. May be opened more than once. */
stderr 
	= code inline {
		.d 0 0
			jsr	stderrF
		.o 0 2 f
	}

sfreadc::!File -> (!Bool,!Char,!File)
sfreadc f 
	= code inline {
		.d 0 2 f
			jsr	readSFC
		.o 0 4 b c f
	}

sfreadi::!File -> (!Bool,!Int,!File)
sfreadi f
	= code inline {
		.d 0 2 f
			jsr	readSFI
		.o 0 4 b i f
	}

sfreadr::!File -> (!Bool,!Real,!File)
sfreadr f 
	= code inline {
		.d 0 2 f
			jsr	readSFR
		.o 0 4 b r f
	}

sfreads::!File !Int -> (!*{#Char},!File)
sfreads f i 
	= code inline {
		.d 0 3 f i
			jsr readSFS
		.o 1 2 f
	}

sfreadline::!File -> (!*{#Char},!File)
sfreadline f 
	= code inline {
		.d 0 2 f
			jsr readLineSF
		.o 1 2 f
	}

sfseek::!File !Int !Int -> (!Bool,!File)
sfseek f i1 i2 
	= code inline {
		.d 0 4 f i i
			jsr seekSF
		.o 0 3 b f
	}

/*	Change a file so that from now it can only be used with sfF... operations. */
fshare::!*File -> File
fshare f 
	= code inline {
		.d 0 2 f
			jsr shareF
		.o 0 2 f
	}

/*	The functions sfend and sfposition work like fend and fposition, but don't return a
	new file on which other operations can continue. They can be used for files opened
	with sfopen or after fshare, and in guards for files opened with fopen or freopen. */
sfend::!File -> Bool
sfend f
	= code inline {
		.d 0 2 f
			jsr endSF
		.o 0 1 b
	}

sfposition::!File -> Int
sfposition f
	= code inline {
		.d 0 2 f
			jsr positionSF
		.o 0 1 i
	}

class (<<<) infixl a :: !*File !a -> *File

instance <<< Int where
//  (<<<) file i = fwritei i file
 (<<<) file i = code inline {
		push_b 2
		update_b 2 3
		update_b 1 2
		update_b 0 1
		pop_b 1
	.d 0 3 i f
		jsr writeFI
	.o 0 2 f
  }

instance <<< Char where
//  (<<<) file c = fwritec c file
 (<<<) file c = code inline {
		push_b 2
		update_b 2 3
		update_b 1 2
		update_b 0 1
		pop_b 1
	.d 0 3 c f
		jsr writeFC
	.o 0 2 f
  }

instance <<< {#Char} where
//  (<<<) file s = fwrites s file
 (<<<) file s = code inline {
	.d 1 2 f
		jsr writeFS
	.o 0 2 f
  }

instance <<< Real where
//  (<<<) file r = fwriter r file
 (<<<) file r = code inline {
		push_b 2
		update_b 2 3
		update_b 1 2
		update_b 0 1
		pop_b 1
	.d 0 3 r f
		jsr writeFR
	.o 0 2 f
  }

//	Access to the file system:

class FileEnv env where
	accFiles :: !.(*Files -> (.x,*Files)) !*env -> (!.x,!*env)
	appFiles :: !.(*Files -> *Files)      !*env -> *env

instance FileEnv World where
	accFiles :: !.(*Files -> (.x,*Files)) !*World -> (!.x,!*World)
	accFiles accfun world
		#! files=create_files
		   (r,files) = accfun files
		= do_files2 files r world
		where
			do_files2 :: !*Files !.x !*World -> (!.x,!*World)
			do_files2 filesRWS r world
			 = (r,world)

	appFiles :: !.(*Files -> *Files) !*World -> *World
	appFiles appfun world
		#! files1=create_files
// RWS ...		#! files=appfun files
		   files=appfun files1
// .. RWS
		=  do_files files world
		where
			do_files :: !*Files !*World -> *World
			do_files filesRWS world
			 = code inline {
				fill_a 1 2
				pop_a 2
			 }

create_files :== Files;
