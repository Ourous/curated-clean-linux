definition module Data._Array

from StdArray import class Array

unsafeCreateArray :: !.Int -> u:(a v:b) | Array a b, [u<=v]
unsafeUselect :: !u:(a v:b) !Int -> *(v:b,!u:(a v:b)) | Array a b, [u<=v]
