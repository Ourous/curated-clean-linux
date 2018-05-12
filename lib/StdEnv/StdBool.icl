implementation module StdBool

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import StdOverloaded

instance == Bool
where
 (==) :: !Bool !Bool -> Bool
 (==) a b
	= code inline {
			eqB
	}

instance toBool Bool
where
 toBool :: !Bool -> Bool
 toBool a
	= code inline {
			no_op
	}

instance toString Bool
where
 toString :: !Bool -> {#Char}
 toString a
	= code inline {
		.d 0 1 b
			jsr BtoAC
		.o 1 0
	}


instance fromBool Bool
where
 fromBool :: !Bool -> Bool
 fromBool a
	= code inline {
			no_op
	}

instance fromBool {#Char}
where
 fromBool :: !Bool -> {#Char}
 fromBool a
	= code inline {
		.d 0 1 b
			jsr BtoAC
		.o 1 0
	}

//	Logical Operators:
    
not :: !Bool -> Bool
not	a
	= code inline {
			notB
	}

(&&) infixr 3 :: !Bool Bool -> Bool
(&&) a b
	= code {
		push_b 0
		jmp_false l1
		pop_b 1
		jsr_eval 0
		pushB_a 0
		pop_a 1
	.d 0 1 b
		rtn
	:l1
		pop_a 1
	.d 0 1 b
		rtn
	}

(||) infixr 2 :: !Bool Bool -> Bool
(||) a b
	= code {
		push_b 0
		jmp_true l2
		pop_b 1
		jsr_eval 0
		pushB_a 0
		pop_a 1
	.d 0 1 b
		rtn
	:l2
		pop_a 1
	.d 0 1 b
		rtn
	}

