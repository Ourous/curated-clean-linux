implementation module Data._Array

import StdEnv

unsafeCreateArray :: .Int -> u:(a v:b) | Array a b, [u<=v]
unsafeCreateArray size = code {
		updatepop_a 0 7
		jmp_ap 1
	}

unsafeUselect :: u:(a v:b) Int -> *(v:b,u:(a v:b)) | Array a b, [u<=v]
unsafeUselect arr index = code {
		update_a 3 7
		pop_a 7
		jsr_ap 2
		repl_args 2 2
	}
