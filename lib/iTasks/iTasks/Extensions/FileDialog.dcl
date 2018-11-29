definition module iTasks.Extensions.FileDialog
/**
* This module provides a simple dialog-based task for entering/updating file paths.
* entering or, it is parameterized with a title and action so it can 
* be used both for opening files and saving files.
*/
import iTasks

editFilePath :: String Action (Maybe FilePath) -> Task (Maybe FilePath)
