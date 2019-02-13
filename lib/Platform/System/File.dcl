definition module System.File

from StdFile import class FileSystem
from StdClass import class toString

from System.Time import ::Tm
from Data.Error import ::MaybeError
from System.OSError import ::MaybeOSError, ::OSError, ::OSErrorCode, ::OSErrorMessage

:: FileError = CannotOpen | CannotClose | IOError

instance toString FileError

/**
* Given a filename, reads the contents of the file to a String
* @param Path to the file to read
* @return contents of the file
*/
readFile :: !String !*env -> (!MaybeError FileError String, !*env) | FileSystem env

/**
* Given a filename, reads the contents of the file by lines to a [String]
* @param Path to the file to read
* @return contents of the file
*/
readFileLines :: !String !*env -> (!MaybeError FileError [String], !*env) | FileSystem env

/**
* Read all contents of a *File by lines to a [String].
* @precondition The file must be opened in read mode
* @param Path to the file to read
* @return contents of the file
*/
readAllLines :: !*File -> (!MaybeError FileError [String], !*File)

/**
* Read all contents of a *File to a String.
* @precondition The file must be opened in read mode
* @param Path to the file to read
* @return contents of the file
*/
readAll :: !*File -> (!MaybeError FileError String, !*File)

/**
* Writes a string to a file
* @param Path to the file to read
* @param contents of the file
*/
writeFile :: !String !String !*env -> (!MaybeError FileError (), !*env) | FileSystem env

/**
* Performs a file operation on a given filename.
* The file is opened and closed by the withFile function.
* @param Path to the file
* @param file operation function
* @return file operation result
*/
withFile :: !String !Int (*File -> (!MaybeError FileError a,!*File)) !*env
			-> (!MaybeError FileError a, !*env) | FileSystem env

/**
* Checks if a file exists
* @param Path to the file
* @return file exists
*/
fileExists ::  !String !*World -> (!Bool, !*World)

/**
* Deletes a file from disk
* @param Path to the file
* @return delete succeeded
*/
deleteFile :: !String !*World -> (!MaybeOSError (), !*World)

:: FileInfo =
	{ directory         :: !Bool
	, creationTime      :: !Tm
	, lastModifiedTime  :: !Tm
	, lastAccessedTime  :: !Tm
	, sizeHigh          :: !Int
	, sizeLow           :: !Int
	}

/**
* Retrieves file information
* @param Path to the file
* @return FileInfo structure
*/
getFileInfo :: !String !*World -> (!MaybeOSError FileInfo, !*World)

/**
* Moves or renames a file
* @param Path to the current file
* @param Path to the new file
*/
moveFile :: !String !String !*World -> (!MaybeOSError (), !*World)
