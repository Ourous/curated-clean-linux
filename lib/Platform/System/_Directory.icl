implementation module System._Directory

import StdArray, StdBool, StdClass, StdInt, StdChar, StdString

import System.FilePath
import System.OSError

import System._Posix
import System._Pointer

createDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
createDirectory path world
	# (ret,world)	= mkdir (packString path) 493 world // 493 = 0755 in octal
	| ret == 0
		= (Ok (), world)
	| otherwise
		= getLastOSError world

removeDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
removeDirectory path world
	# (ret,world)	= rmdir (packString path) world
	| ret == 0
		= (Ok (), world)
	| otherwise
		= getLastOSError world

readDirectory :: !FilePath !*w -> (!MaybeOSError [FilePath], !*w)
readDirectory path world
	# (dirptr,world)	= opendir (packString path) world
	| dirptr == 0
		= getLastOSError world
	# (entries,world)	= readEntries dirptr world
	# (ret,world)		= closedir dirptr world
	| ret == 0
		= (Ok entries, world)
	| otherwise
		= getLastOSError world
where
	readEntries :: !Pointer !*w -> (![String],!*w)
	readEntries dirptr world
		# (entryptr,world)	= readdir dirptr world
		| entryptr == 0
			= ([],world)
		# (entry,world)		= readEntry entryptr world
		# (entries,world)	= readEntries dirptr world
		= ([entry:entries],world)
	
	readEntry :: !Pointer !*w -> (!String,!*w) 
	readEntry entryptr world
		= (derefString (entryptr + DIRENT_D_NAME_OFFSET), world)

getCurrentDirectory :: !*w -> (!MaybeOSError FilePath, !*w)
getCurrentDirectory world
	# buf			= createArray MAXPATHLEN '\0'
	# (ptr,world)	= getcwd buf MAXPATHLEN world
	| ptr == 0
		= getLastOSError world
	| otherwise
		= (Ok {c \\ c <-: buf | c <> '\0'},world)

setCurrentDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
setCurrentDirectory path world 
	# (ret,world)	= chdir (packString path) world
	| ret == 0
		= (Ok (), world)
	| otherwise
		= getLastOSError world
