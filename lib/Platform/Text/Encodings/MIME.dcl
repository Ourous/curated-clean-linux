definition module Text.Encodings.MIME
/**
* This module provides functions for working with MIME messages and MIME types.
* 
*/
:: MIMEBoundary	:== String
:: MIMEHeader	:== (!String,!String)
:: MIMEBody		:== String
:: MIMEPart		:== (![MIMEHeader],!MIMEBody)
:: MIMEType		:== String

/**
* Encode a multi-part MIME message
*/
encodeMimeMultipart :: !MIMEBoundary ![MIMEPart] -> String
/**
* Decode a multi-part MIME message
*/
decodeMimeMultipart :: !MIMEBoundary !String -> [MIMEPart]
/**
* Give the MIME type of common file extension
*/
extensionToMimeType :: !String -> MIMEType
