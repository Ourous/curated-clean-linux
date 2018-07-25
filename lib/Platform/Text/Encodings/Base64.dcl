definition module Text.Encodings.Base64

/*
 *	Base64 and Base64URL encoding/decoding according to RFC4648. More info:  
 *	- http://tools.ietf.org/html/rfc4648
 *  - http://en.wikipedia.org/wiki/Base64
 */
 
:: Length :== Int
:: Alphabet :== {#Char}
:: Offset :== Int
:: Padding :== Int
 
/**
 * Converts a String to a Base64-encoded String.
 */
base64Encode :: !.String -> .String

/**
 * Converts a String to a Base64-encoded String given a maximum line length.
 */
base64EncodeLen :: !.String !Length -> .String

/**
 * Converts a String to an URL-safe Base64-encoded String.
 */
base64URLEncode :: !.String -> .String

/**
 * Converts a String to an URL-safe Base64-encoded String given a maximum line length.
 */
base64URLEncodeLen :: !.String !Length -> .String

/**
 * Converts a Base64-encoded String to a String.
 */
base64Decode :: !.String -> .String

/**
 * Converts an URL-safe Base64-encoded String to String.
 */
base64URLDecode :: !.String -> .String
