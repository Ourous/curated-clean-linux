definition module Text.Encodings.UrlEncoding
/**
* This module provides encode and decode functions for encoding strings for embedding in urls.
*
* For more info see:
* - http://en.wikipedia.org/wiki/Percent-encoding
* - http://tools.ietf.org/html/rfc3986
*/

/**
* Converts an ASCII string to a url-encoded string.
*/
urlEncode :: !String -> String
/**
* Converts an url-encoded string to an ASCII string.
*/
urlDecode :: !String -> String
/**
* Converts a list of name/value pairs to url encoded string of the form foo=bar&baz=42.
*/
urlEncodePairs :: ![(String,String)] -> String
/**
* Converts a url encoded list of name/value pairs to list of name/value tuples.
*/
urlDecodePairs :: !String -> [(String,String)]
