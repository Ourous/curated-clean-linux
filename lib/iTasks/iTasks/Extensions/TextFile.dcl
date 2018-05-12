definition module iTasks.Extensions.TextFile

import iTasks
from System.FilePath import :: FilePath

/**
* Import the content of  a text file on the server's filesystem.
*
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException
* 
* @gin-icon page_white_text
*/
importTextFile		:: !FilePath -> Task String

/**
* Export a string as text file to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Text: The content to export
*
* @return The exported content
* @throws FileException
* 
* @gin-icon page_white_text
*/
exportTextFile		:: !FilePath !String -> Task String
