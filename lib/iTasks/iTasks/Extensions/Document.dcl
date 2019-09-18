definition module iTasks.Extensions.Document

import iTasks.WF.Definition
from System.FilePath import :: FilePath
from System.File import :: FileError

//* Documents
:: Document =
	{ documentId	:: !DocumentId				//*A unique identifier of the document
	, contentUrl	:: !String					//*A url to where the document can be downloaded
	, name			:: !String					//*The filename of a document
	, mime			:: !String					//*The mime type of the document
	, size			:: !Int						//*The filesize in bytes
	}
:: DocumentId	:== String

:: FileException		= FileException !FilePath !FileError

instance toString	Document
instance ==			Document
instance toString	FileException

//Necessary generics to be able to handle documents in tasks
derive JSONEncode		Document
derive JSONDecode		Document
derive gEq				Document
derive gText	        Document
derive gEditor			Document

derive class iTask FileException

/**
* Import a file on the server's filesystem as a Document
*
* @param File path: The path of the file to import
*
* @return The imported document
* @throws FileException
*/
importDocument		:: !FilePath -> Task Document

/**
* Export a document to the server's filesystem.
*
* @param File path: The path of the exported file
* @param Document: The document to export
*
* @return The exported document
* @throws FileException
*/
exportDocument		:: !FilePath !Document -> Task Document

