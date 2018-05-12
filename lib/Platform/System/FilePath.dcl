definition module System.FilePath
/**
* Module for manipulation of file and directory paths
*/

from Data.Error import :: MaybeError
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage, :: MaybeOSError

:: FilePath :== String

/**
* Returns the default platform path separator
*/
pathSeparator :: Char

/**
* Returns a list of all allowed platform path separators
*/
pathSeparators :: [Char]

/**
* Returns the default file extension separator
*/
extSeparator :: Char

/**
* Concatenates two paths
*/
(</>) infixr 5 :: !FilePath !FilePath -> FilePath

/**
* Split a FilePath into filename and extension. The result does not include the extension separator (.).
*/
splitExtension :: !FilePath -> (String, String)

/**
* Take the extension of a FilePath, excluding the separator
*/
takeExtension :: !FilePath -> String

/**
* Remove the extension and extension separator of a FilePath
*/
dropExtension :: !FilePath -> String

/**
* Add an extension to a FilePath
*/
addExtension :: !FilePath !String -> FilePath

/**
* Replace the extension of a FilePath
*/
replaceExtension :: !FilePath !String -> FilePath

/**
* Take the directory part of a FilePath. If the FilePath is a directory, 
* the result is the parent directory.
*/
takeDirectory :: !FilePath -> FilePath

/**
* Drop the directory part of a FilePath. Keep only the filename.
*/
dropDirectory :: !FilePath -> String

/**
* Split a filename into directory and file.
*/
splitFileName :: !FilePath -> (String, String)

/**
* Get the file name.
*/
takeFileName :: !FilePath -> FilePath

/**
* Set the filename.
*/
replaceFileName :: !FilePath !String -> FilePath

/**
* Drop the filename.
*/
dropFileName :: !FilePath -> FilePath

/**
 * Get the full path name, without '.', '..' or symbolic links.
 */
getFullPathName :: !FilePath !*World -> (!MaybeOSError FilePath, !*World)

