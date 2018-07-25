definition module Codec.Compression.Snappy

/**
 * Clean bindings for Snappy (https://github.com/google/snappy/).
 * To build a program with this library, include the `snappy` library. With
 * `clm`, add `-l -lsnappy` to the command line arguments.
 */

/**
 * Compress a String
 */
snappy_compress :: !.String -> .String

/**
 * Uncompress a String
 */
snappy_uncompress :: !.String -> .String
