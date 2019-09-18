definition module Text.Encodings.Base64

/**
 * Base64 and Base64URL encoding/decoding according to RFC4648. More info:
 * - http://tools.ietf.org/html/rfc4648
 * - http://en.wikipedia.org/wiki/Base64
 *
 * @property-bootstrap
 *     import StdEnv
 *     import Text
 */
 
:: Length :== Int
:: Alphabet :== {#Char}
:: Offset :== Int
:: Padding :== Int
 
/**
 * Converts a String to a Base64-encoded String.
 * @property inverse: A.x :: String:
 *     base64Decode (base64Encode x) == x
 */
base64Encode :: !.String -> .String

/**
 * Converts a String to a Base64-encoded String given a maximum line length.
 * @property inverse: A.len :: Int; x :: String:
 *     len > 0 ==> base64Decode (base64EncodeLen x len) == x
 * @property max length: A.len :: Int; x :: String:
 *     len > 0 ==> all ((>=) len o size) (split "\n" (base64EncodeLen x len))
 */
base64EncodeLen :: !.String !Length -> .String

/**
 * Converts a String to an URL-safe Base64-encoded String.
 * @property inverse: A.x :: String:
 *     base64URLDecode (base64URLEncode x) == x
 */
base64URLEncode :: !.String -> .String

/**
 * Converts a String to an URL-safe Base64-encoded String given a maximum line length.
 * @property inverse: A.len :: Int; x :: String:
 *     len > 0 ==> base64URLDecode (base64URLEncodeLen x len) == x
 * @property max length: A.len :: Int; x :: String:
 *     len > 0 ==> all ((>=) len o size) (split "\n" (base64URLEncodeLen x len))
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

/**
 * Decodes a Base64-encoded String in place.
 * @property inverse: A.x :: String:
 *     base64DecodeUnique (base64Encode x) == x
 * @property inverse with linebreaks: A.len :: Int; x :: String:
 *     len > 0 ==> base64DecodeUnique (base64EncodeLen x len) == x
 */
base64DecodeUnique :: !*String -> .String

/**
 * Decodes a URL-safe Base64-encoded String in place.
 * @property inverse: A.x :: String:
 *     base64URLDecodeUnique (base64URLEncode x) == x
 * @property inverse with linebreaks: A.len :: Int; x :: String:
 *     len > 0 ==> base64URLDecodeUnique (base64URLEncodeLen x len) == x
 */
base64URLDecodeUnique :: !*String -> .String
