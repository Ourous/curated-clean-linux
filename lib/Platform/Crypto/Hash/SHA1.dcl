definition module Crypto.Hash.SHA1
/**
* Simple pure Clean SHA1 implementation.
* Probably not the fastest, but without any dependencies
* Relatively straightforward implementation of FIPS 180-1
*/

from StdString import class toString
from StdFile import class FileSystem
from System.FilePath import :: FilePath
from Data.Error import :: MaybeError
from System.File import :: FileError

:: SHA1Digest = SHA1Digest {#Char} // 160 bit (20byte) message digest

/**
* Convenient shorthand for computing the SHA1 digest of a string and
* returning the ascii hexadecimal representation
*
* @param The message for which to compute the digest
* @return The ascii hex representation of the message digest
*/
sha1 :: !String -> String

/**
* Compute the SHA1 digest of a string
*
* @param The message for which to compute the digest
* @return The raw 160 bit digest
*/
sha1StringDigest :: !String -> SHA1Digest

/**
* Compute the SHA1 digest of the contents of a file
*
* @param The path of the file to get compute the digest of
* @param A filesystem environment
* @return The raw 160 bit digest
* @return The environment
*/
sha1FileDigest :: !FilePath !*env -> (!MaybeError FileError SHA1Digest, !*env) | FileSystem env

/**
* Print a SHA1 digest as ascii hexadecimal form
*/
instance toString SHA1Digest
