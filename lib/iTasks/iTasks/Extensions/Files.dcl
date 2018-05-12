definition module iTasks.Extensions.Files
/**
* This module provides various tasks for managing files
*/
import iTasks
from System.FilePath import :: FilePath

//Managing files

/**
* Deletes a file
* @param The path of the file
*/
deleteFile :: !FilePath -> Task ()

/**
* Moves/renames a file
* @param Source path 
* @param Destination path 
*/
moveFile :: !FilePath !FilePath -> Task ()

/**
* Copies a file
* @param Source path 
* @param Destination path 
*/
copyFile :: !FilePath !FilePath -> Task ()

//Managing directories

/**
* Creates a directory
*
* @param The path of the new directory
* @param Create parent directories if necessary
*/
createDirectory :: !FilePath !Bool -> Task ()
/**
* Deletes a directory
*
* @param The path of the directory
* @param Recursively delete the content of the directory
*/
deleteDirectory :: !FilePath !Bool -> Task ()
/**
* Copy a directory and all of its content
*
* @param Source path 
* @param Destination path 
*/
copyDirectory :: !FilePath !FilePath -> Task ()

