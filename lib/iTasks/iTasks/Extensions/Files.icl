implementation module iTasks.Extensions.Files

import iTasks
import System.FilePath
import qualified System.File as SF
import qualified System.Directory as SD
from System.File import instance toString FileError
from System.File import :: FileInfo{directory}, :: FileError
import Data.Error, Text
import StdFile, StdArray

deleteFile :: !FilePath -> Task ()
deleteFile path = accWorldError ('SF'.deleteFile path) snd 

moveFile :: !FilePath !FilePath -> Task ()
moveFile srcPath dstPath = accWorldError ('SF'.moveFile srcPath dstPath) snd

copyFile :: !FilePath !FilePath -> Task ()
copyFile srcPath dstPath = accWorldError (copyFile` srcPath dstPath) id

//TODO: This is a very stupid way of copying files, should be replaced by a better way
copyFile` srcPath dstPath world = case 'SF'.readFile srcPath world of
	(Error e,world) = (Error e,world)
	(Ok content,world) = 'SF'.writeFile dstPath content world
		
createDirectory :: !FilePath !Bool -> Task ()
createDirectory path False = accWorldError ('SD'.createDirectory path) snd
createDirectory path True = accWorldError (createWithParents path) id 
where
	createWithParents path world = create [] (split {pathSeparator} path) world

	create _ [] world = (Ok (),world)
	create [] ["":rest] world = create [""] rest world //Special case for absolute paths
	create base [dir:rest] world
		# next = base ++ [dir]
		# path = join {pathSeparator} next
		# (exists,world) = 'SF'.fileExists path world
		| exists = create next rest world //This part exists, continue
		| otherwise = case 'SD'.createDirectory path world of
			(Error e,world) = (Error (snd e),world) 
			(Ok (),world) = create next rest world

deleteDirectory :: !FilePath !Bool -> Task ()
deleteDirectory path False = accWorldError ('SD'.removeDirectory path) snd
deleteDirectory path True = accWorldError (deleteDirectoryRecursive path) id

deleteDirectoryRecursive path world = case 'SD'.readDirectory path world of
	(Error e,world) = (Error (snd e), world)
	(Ok content,world) = case deleteContent content world of
		(Error e,world) = (Error e,world)
		(Ok (),world) = case 'SD'.removeDirectory path world of 
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = (Ok (),world)
where
	deleteContent [] world = (Ok (),world)
	deleteContent [".":rest] world = deleteContent rest world
	deleteContent ["..":rest] world = deleteContent rest world
	deleteContent [entry:rest] world = case 'SF'.getFileInfo (path </> entry) world of
		(Error e,world) = (Error (snd e), world)
		(Ok {FileInfo|directory},world) 
		| directory = case deleteDirectoryRecursive (path </> entry) world of
			(Error e,world) = (Error e,world)
			(Ok (),world) = deleteContent rest world
		| otherwise = case 'SF'.deleteFile (path </> entry) world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = deleteContent rest world

copyDirectory :: !FilePath !FilePath -> Task ()
copyDirectory  srcPath dstPath = accWorldError (copyDirectory` srcPath dstPath) id

copyDirectory` srcPath dstPath world = case 'SD'.readDirectory srcPath world of
		(Error e,world) = (Error (snd e), world)
		(Ok content,world) = case 'SD'.createDirectory dstPath world of
			(Error e,world) = (Error (snd e),world)
			(Ok (),world) = copyContent content world
where
	copyContent [] world  = (Ok (),world)
	copyContent [".":rest] world = copyContent rest world
	copyContent ["..":rest] world = copyContent rest world
	copyContent [entry:rest] world = case 'SF'.getFileInfo (srcPath </> entry) world of
		(Error e,world) = (Error (snd e), world)
		(Ok {FileInfo|directory},world) 
			| directory = case copyDirectory` (srcPath </> entry) (dstPath </> entry) world of
				(Error e,world) = (Error e,world)
				(Ok (),world) = copyContent rest world
			| otherwise = case copyFile` (srcPath </> entry) (dstPath </> entry) world of
				(Error e,world) = (Error (toString e), world)
				(Ok (),world) = copyContent rest world
