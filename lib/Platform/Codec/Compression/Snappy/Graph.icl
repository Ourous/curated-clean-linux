implementation module Codec.Compression.Snappy.Graph

import Codec.Compression.Snappy

import dynamic_string

snappy_compress_a :: !.a -> .String
snappy_compress_a x
#! s = copy_to_string x
= snappy_compress s

snappy_uncompress_a :: !.String -> .a
snappy_uncompress_a s
# s     = snappy_uncompress s
# (x,_) = copy_from_string s
= x
