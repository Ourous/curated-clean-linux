implementation module Data._Array

import StdEnv

unsafeCreateArray :: !.Int -> u:(a v:b) | Array a b, [u<=v]
unsafeCreateArray size = code {
	updatepop_a 0 7
	.d 1 1 i
	jmp_i 1
}

unsafeUselect :: !u:(a v:b) !Int -> *(v:b,!u:(a v:b)) | Array a b, [u<=v]
unsafeUselect arr index = code {
	updatepop_a 6 7
	.d 2 1 i
	jmp_i 2
}
