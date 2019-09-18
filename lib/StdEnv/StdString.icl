implementation module StdString

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance == {#Char}
where
 (==) :: !{#Char} !{#Char} -> Bool
 (==) a b
	= code inline {
		.d 2 0
			jsr eqAC
		.o 0 1 b	
	}

instance < {#Char}
where
 (<) :: !{#Char} !{#Char} -> Bool
 (<) a b
	= code inline {
		.d 2 0
			jsr cmpAC
		.o 0 1 i
			pushI 0
			gtI
	}

instance toString Int
where
 toString :: !Int -> {#Char}
 toString a
	= code inline {
		.d 0 1 i
			jsr ItoAC
		.o 1 0
	}

instance toString Char
where
 toString :: !Char -> {#Char}
 toString a
	= code inline {
			CtoAC
	}

instance toString Real
where
 toString :: !Real -> {#Char}
 toString a
	= code inline {
		.d 0 1 r
			jsr RtoAC
		.o 1 0
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

instance toString {#Char}
where
 toString :: !{#Char} -> {#Char} //	dummy
 toString a
	= code inline {
		no_op
	}

instance fromString {#Char}
where
 fromString :: !{#Char} -> {#Char} //	dummy
 fromString a
	= code inline {
		no_op
	}

instance % {#Char}
where
 (%) ::!{#Char} !(!Int,!Int) -> {#Char}
 (%) str (a,b)
	= code inline {
		.d 1 2 ii
			jsr sliceAC
		.o 1 0
	}

instance +++ {#Char}
where
 (+++) :: !{#Char} !{#Char} -> {#Char}
 (+++) a b
	= code inline {
		.d 2 0
			jsr catAC
		.o 1 0
	}

(+++.) infixr 5 :: !{#Char} !{#Char} -> .{#Char}
(+++.) a b
	= code inline {
		.d 2 0
			jsr catAC
		.o 1 0
	}

(:=) infixl 9 :: !{#Char} !(!Int,!Char) -> {#Char}	//	update i-th element
(:=) s (i,c)
	= code inline {
		.d 1 2 ic
			jsr updateAC
		.o 1 0
	}
