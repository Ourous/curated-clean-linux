definition module iTasks.Extensions.TextFile

import iTasks
from System.FilePath            import :: FilePath
from iTasks.Extensions.Document import :: Document

/**
* Import the content of  a text file on the server's filesystem.
*
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException
*/
importTextFile		:: !FilePath -> Task String

/**
* Import the content of a text file document.
*
* @param Document: The document to import
*
* @return The imported content
* @throws FileException
*/
importTextDocument :: !Document -> Task String

/**
* Export a string as text file to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Text: The content to export
*
* @return The exported content
* @throws FileException
*/
exportTextFile		:: !FilePath !String -> Task String
