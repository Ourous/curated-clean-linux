definition module Codec.Compression.Snappy.Graph

/**
 * Compress an arbitrary Clean expression.
 * This uses GraphCopy's copy_to_string, so the result can only be uncompressed
 * by the same application.
 */
snappy_compress_a :: !.a -> .String

/**
 * Uncompress an arbitrary Clean expression.
 * This uses GraphCopy's copy_from_string, so this only works for expressions
 * compressed by the same application.
 */
snappy_uncompress_a :: !.String -> .a
