definition module iTasks.Extensions.CSVFile 

import iTasks
import iTasks.Extensions.Document
from System.FilePath import :: FilePath

/**
* Import a comma separated vector (CSV) file on the server's filesystem.
*
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException
* 
* @gin-icon page_white_csv
*/
importCSVFile		:: !FilePath -> Task [[String]]
importCSVDocument	:: !Document -> Task [[String]]
/**
* Import a comma separated vector (CSV) file on the server's filesystem using
* custom separator characters.
*
* @param Separator: The field separator
* @param Quote character: The string quote character
* @param Escape character : The escape character
* @param File path: The path of the file to import
*
* @return The imported content
* @throws FileException
*
* @gin False
*/
importCSVFileWith		:: !Char !Char !Char !FilePath -> Task [[String]]
importCSVDocumentWith	:: !Char !Char !Char !Document -> Task [[String]]

/**
* Export a list of rows of fields to a comma separated vector (CSV) document.
*
* @param File name: A name of the created CSV file
* @param Cells: The content to export as a list of rows of lists of fields
*
* @return The exported content as a document
* 
* @gin-icon page_white_csv
*/
createCSVFile		:: !String ![[String]] -> Task Document
/**
* Export a list of rows of fields to a comma separated vector (CSV) file on the server's filesystem.
*
* @param File path: The path of the exported file
* @param Cells: The content to export as a list of rows of lists of fields
*
* @return The exported content
* @throws FileException
* 
* @gin-icon page_white_csv
*/
exportCSVFile		:: !FilePath ![[String]] -> Task [[String]]
/**
* Export a list of rows of fields to a comma separated vector (CSV) file on the server's filesystem
* using custom separator characters.
*
* @param Separator: The field separator
* @param Quote character: The string quote character
* @param Escape character: The escape character
* @param File path: The path of the exported file
* @param Cells: The content to export as a list of rows of lists of fields
*
* @return The exported content
* @throws FileException
* 
* @gin False
*/
exportCSVFileWith	:: !Char !Char !Char !FilePath ![[String]] -> Task [[String]]
