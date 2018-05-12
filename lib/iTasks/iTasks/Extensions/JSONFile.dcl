definition module iTasks.Extensions.JSONFile

import iTasks
import iTasks.Extensions.Document
from System.FilePath import :: FilePath

:: JSONParseException = CannotParse !String
instance toString JSONParseException

/**
* Import and parse a JSON datafile on the server's filesystem.
*
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException 
* 
* @gin-icon page_white_json
*/
importJSONFile		:: !FilePath -> Task a | iTask a
importJSONDocument  :: !Document -> Task a | iTask a
/**
* Import and parse a JSON datafile on the server's filesystem using
* a custom parse function.
*
* @param Decoder function: The JSON decoder function
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException 
* 
* @gin False
*/
importJSONFileWith	:: !(JSONNode -> Maybe a) !FilePath -> Task a | iTask a
/**
* Encode a value as JSON and create a document.
*
* @param File name: A name of the created JSONfile
* @param Value: The content to encode as JSON using the generic JSON encoder
*
* @return The exported content as a document
*/
createJSONFile      :: !String a -> Task Document | iTask a
/**
* Encode and export a JSON datafile to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Value: The content to encode as JSON using the generic JSON encoder
*
* @return The exported content
* 
* @gin-icon page_white_json
*/
exportJSONFile		:: !FilePath a -> Task a | iTask a
/**
* Encode and export a JSON datafile to the server's filesystem using a custom encode function.
* 
* @param Encoder function: The JSON encoder function
* @param File path: The path of the exported file
* @param Value: The content to encode as JSON
* 
* @return The exported content
* @throws FileException
* 
* @gin False
*/
exportJSONFileWith	:: !(a -> JSONNode) !FilePath a -> Task a | iTask a
