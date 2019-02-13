definition module Crypto.Hash.MD5
/**
* Pure Clean MD5 implementation.
*/

from StdString import class toString
from StdFile import class FileSystem
from System.FilePath import :: FilePath
from Data.Error import :: MaybeError
from System.File import :: FileError

:: MD5Digest = MD5Digest {#Char} // 128 bit (16byte) message digest

/**
* Convenient shorthand for computing the MD5 digest of a string and
* returning the ascii hexadecimal representation
*
* @param The message for which to compute the digest
* @return The ascii hex representation of the message digest
*/
md5 :: !String -> String

/**
* Compute the MD5 digest of a string
*
* @param The message for which to compute the digest
* @return The raw 128 bit digest
*/
md5StringDigest :: !String -> MD5Digest

/**
* Compute the MD5 digest of the contents of a file
*
* @param The path of the file to get compute the digest of
* @param A filesystem environment
* @return The raw 128 bit digest
* @return The environment
*/
md5FileDigest :: !FilePath !*env -> (!MaybeError FileError MD5Digest,!*env) | FileSystem env

/**
* Print an MD5 digest as ascii hexadecimal form
*/
instance toString MD5Digest
