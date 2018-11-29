implementation module Codec.Compression.Snappy

import StdEnv

import System._Pointer, Data._Array
import Text

snappy_max_compressed_length :: !Int -> Int
snappy_max_compressed_length len = code {
	ccall snappy_max_compressed_length "I:I"
}

snappy_uncompressed_length :: !String -> Int
snappy_uncompressed_length s
#! (r,len) = snappy_uncompressed_length s (size s)
| r <> 0 = abort ("Invalid return status of snappy_uncompressed_length: " <+ r <+ "\n")
= len
where
	snappy_uncompressed_length :: !String !Int -> (!Int,!Int)
	snappy_uncompressed_length s len = code {
		ccall snappy_uncompressed_length "sI:II"
	}

snappy_compress :: !.String -> .String
snappy_compress s
#! n = snappy_max_compressed_length (size s)
#! c = unsafeCreateArray (n+1)
#! (r,len) = compress s (size s) c
| r <> 0 = abort ("Invalid return status of snappy_compress: " <+ r <+ "\n")
= {c \\ c <-: c & i <- [0..len-1]}
where
	compress :: !String !Int !String -> (!Int,!Int)
	compress i len o = code {
		ccall snappy_compress "sIs:II"
	}

snappy_uncompress :: !.String -> .String
snappy_uncompress s
#! n = snappy_uncompressed_length s
#! u = unsafeCreateArray (n+1)
#! (r,len) = uncompress s (size s) u
| r <> 0 = abort ("Invalid return status of snappy_uncompress: " <+ r <+ "\n")
= {c \\ c <-: u & i <- [0..len-1]}
where
	uncompress :: !String !Int !String -> (!Int, !Int)
	uncompress i len o = code {
		ccall snappy_uncompress "sIs:II"
	}
