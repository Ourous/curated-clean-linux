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

/**
 * A file selection task.
 * If your file structure is big or contains cyclic links, choose {{`selectFileLazyTree`}}
 *
 * @param Start with all directories expanded
 * @param Flag for multiple selection
 * @param Root directory to select from
 * @param Initial selection
 */
selectFileTree :: !Bool !Bool !FilePath [FilePath]-> Task [FilePath]

/**
 * Browse for a file in a lazy tree structure.
 *
 * @param Multiple selection allowed
 * @param Path to start in
 * @result Filepaths picked
 */
selectFileTreeLazy :: !Bool !FilePath -> Task [FilePath]
