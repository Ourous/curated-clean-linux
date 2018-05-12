implementation module iTasks.Extensions.FileCollection
/**
* This extension provides a set of SDS functions to map
* somewhat complex data structures to a directory tree structure with very simple
* plain text files on disk.
*/
import iTasks
import iTasks.Internal.Util

import StdFile
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Functor, Data.Maybe, Text
import System.Directory, System.File, System.FilePath, System.OS

derive class iTask FileCollectionItem

EXCLUDE_FILE :== "exclude.txt"

//Writes a map of key/value pairs to a directory with one file per key/value
//It will ignore all files in the directory that don't match the filter
fileCollection :: FileFilter Bool -> SDS FilePath FileCollection FileCollection
fileCollection isFileInCollection deleteRemovedFiles = worldShare (read isFileInCollection) (write isFileInCollection)
where
	read isFileInCollection dir world = case readDirectory dir world of
		(Error (_,msg),world) = (Error msg,world) 
		(Ok files,world) = case (if deleteRemovedFiles (Ok [],world) (readExcludeList dir world)) of 
			(Error e, world) = (Error e,world)
			(Ok excludes,world) = case readFiles isFileInCollection excludes dir files world of
				(Error e, world) = (Error e,world)
				(Ok collection,world) = (Ok ('DM'.fromList collection), world)
	
	readFiles isFileInCollection excludes dir [] world = (Ok [],world)
	readFiles isFileIncollection excludes dir [f:fs] world
		| f == "." || f == ".." || (not deleteRemovedFiles && isMember f excludes) = readFiles isFileInCollection excludes dir fs world 
		| otherwise = case getFileInfo (dir </> f) world of
			(Error (_,msg),world) = (Error msg,world)
			(Ok {FileInfo|directory},world) 
				//Skip files that don't match the filter
				| not (isFileInCollection f directory)
					= readFiles isFileInCollection excludes dir fs world 
				//Read a subcollection
				| directory = case read (\x -> isFileInCollection (f </> x)) (dir </> f) world of 
					(Error e,world) = (Error e,world)
					(Ok fcollection,world) = case readFiles isFileInCollection excludes dir fs world of
						(Error e,world) = (Error e,world)
						(Ok collection,world) = (Ok [(f,FileCollection fcollection):collection], world)
				//Read the file content
				| otherwise = case readFile (dir </> f) world of
                    (Error e,world) = (Error (toString e),world)
					(Ok fcontent,world) = case readFiles isFileInCollection excludes dir fs world of
						(Error e,world) = (Error e,world)
						(Ok collection,world) = (Ok [(f,FileContent fcontent):collection], world)

	readExcludeList dir world = case readFileLines (dir </> EXCLUDE_FILE) world of
		(Ok lines,world)         = (Ok [EXCLUDE_FILE:lines],world) //the exclude file itself should also be excluded
		(Error CannotOpen,world) = (Ok [EXCLUDE_FILE],world)
		(Error e,world)          = (Error (toString e),world)

	write isFileInCollection dir collection world = case readDirectory dir world of 
		//We need to know the current content of the directory to be able to delete removed entries
		(Error (_,msg),world) = (Error msg,world) 
		(Ok curfiles,world) = case writeFiles ('DM'.toList collection) isFileInCollection dir world of
			(Error e,world) = (Error e,world)
			(Ok newfiles,world) = cleanupRemovedFiles curfiles newfiles dir world
		
	writeFiles [] isFileInCollection dir world = (Ok [],world)
	writeFiles [(name,FileContent content):fs] isFileInCollection dir world
		| not (isFileInCollection name False) = writeFiles fs isFileInCollection dir world //Don't write files that don't match the filter
		| otherwise = case writeFile (dir </> name) content world of
			(Error e,world) = (Error (toString e),world)	
			(Ok (),world) = case writeFiles fs isFileInCollection dir world of
				(Error e,world) = (Error e,world)
				(Ok curfiles,world) = (Ok [name:curfiles],world)

	writeFiles [(name,FileCollection collection):fs] isFileInCollection dir world 
		| not (isFileInCollection name True) = writeFiles fs isFileInCollection dir world //Don't write files that don't match the filter
		| otherwise = case ensureDirectory (dir </> name) world of
			(Error e,world) = (Error e,world)
			(Ok (),world) = case write (\x -> isFileInCollection (name </> x)) (dir </> name) collection world  of
				(Error e,world) = (Error e,world)
				(Ok (),world) = case writeFiles fs isFileInCollection dir world of
					(Error e,world) = (Error e,world)
					(Ok curfiles,world) = (Ok [name:curfiles],world)
			
	ensureDirectory path world = case getFileInfo path world of
		(Ok {FileInfo|directory},world) 
			| directory = (Ok (),world)
			| otherwise = (Error ("Can't create directory " +++ path), world)
		(Error _, world)
			= case createDirectory path world of	
				(Ok (),world) = (Ok (),world)
				(Error (_,msg),world) = (Error msg,world)

	//Check if files that existed before, are not in the newly written set.
	//If they match the filter they 'belong' to the collection and should be removed.
	//Otherwise they will be included on the next read of the collection
	cleanupRemovedFiles filesInDirectory filesInCollection dir world
		| deleteRemovedFiles = deleteFiles filesToRemove dir world
		| otherwise          = excludeFiles filesToRemove dir world
	where
		filesToRemove = [f \\ f <- filesInDirectory | f <> "." && f <> ".." && f <> EXCLUDE_FILE && not (isMember f filesInCollection)]

		excludeFiles files dir world = case writeFile (dir </> EXCLUDE_FILE) (join OS_NEWLINE files) world of
			(Error e, world) = (Error (toString e),world)
			(Ok (),world)    = (Ok (),world)

		deleteFiles [] dir world = (Ok (),world) 
		deleteFiles [f:fs] dir world = case recursiveDelete (dir </> f) world of
			(Ok (),world) = deleteFiles fs dir world
			(Error (_,msg),world) = (Error msg,world)

getStringContent :: String FileCollection -> Maybe String
getStringContent key collection = case 'DM'.get key collection of
	(Just (FileContent content)) = Just content
	_							 = Nothing

setStringContent:: String String FileCollection -> FileCollection
setStringContent key value collection = 'DM'.put key (FileContent value) collection

getIntContent :: String FileCollection -> Maybe Int
getIntContent key collection = fmap (toInt o trim) (getStringContent key collection)

setIntContent :: String Int FileCollection -> FileCollection
setIntContent key value collection = 'DM'.put key (FileContent (toString value)) collection

toPaths :: FileCollection -> [FilePath]
toPaths collection = flatten (map toPath ('DM'.toList collection)) 
where
	toPath (name,FileContent _) = [name]
	toPath (name,FileCollection collection) = [name:[name </> path \\ path <- toPaths collection]]

