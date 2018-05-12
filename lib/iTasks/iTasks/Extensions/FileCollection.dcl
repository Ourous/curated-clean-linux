definition module iTasks.Extensions.FileCollection
/**
* This extension provides a set of SDS functions to map
* somewhat complex data structures to a directory tree structure with text files on disk.
*/
import iTasks
from Data.Map import :: Map
from System.FilePath import :: FilePath

//Determine if a path is part of the colleciton based on the relative path and whether it is a directory 
:: FileFilter :== FilePath Bool -> Bool

:: FileCollection :== Map String FileCollectionItem
:: FileCollectionItem
	= FileContent String 
	| FileCollection FileCollection

derive class iTask FileCollectionItem

/**
* Writes a map of key/value pairs to a directory with one file per key/value
* It will ignore all files in the directory that don't match the filter

* @param The filter that specifies which files and directories are part of the collection
* @param Delete flag: When this is true, files on disk that are not in the collection, but match the filter are deleted during a write.
                      If it is false, entries on that are removed are only marked in a file called 'exclude.txt' but not deleted.
*/
fileCollection :: FileFilter Bool -> SDS FilePath FileCollection FileCollection

//Access utilities:
getStringContent:: String FileCollection -> Maybe String
setStringContent:: String String FileCollection -> FileCollection

getIntContent :: String FileCollection -> Maybe Int
setIntContent :: String Int FileCollection -> FileCollection

toPaths :: FileCollection -> [FilePath]
