implementation module StdInt

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

import StdOverloaded
from StdMisc import abort
from StdBool import &&
import StdChar // import <
from StdClass import <=,not,class Ord
import StdArray

instance + Int
where
	(+) :: !Int !Int -> Int
	(+) a b
		= code inline {
			addI
		}

instance - Int
where
	(-) :: !Int !Int -> Int
	(-) a b
		= code inline {
			subI
		}

instance zero Int
where
 zero:: Int
 zero
	= code inline {
			pushI 0
	}

instance * Int
where
 (*) :: !Int !Int -> Int
 (*) a b
	= code inline {
			mulI
	}

instance / Int
where
 (/) :: !Int !Int -> Int
 (/) a b
	= code inline {
			divI
	}

instance one Int
where
 one :: Int
 one
	= code inline {
			pushI 1
	}

instance ^ Int
where
 (^) :: !Int !Int -> Int
 (^) x n
	|	n < 0
		= abort "^ (Int) called with negative power argument"
	// otherwise
		= pow x n
	where
		pow :: !Int !Int -> Int
		pow _ 0
			=	1
		pow a b
			| isEven b
				=	x * x
			// otherwise
				=	a * x * x
			where
				x
					= pow a (b >> 1)

 

instance abs Int
where
 abs::!Int -> Int
 abs x
 	| x<0
 		=	0 - x
 	// otherwise
 		=	x

instance sign Int
where
 sign::!Int -> Int
 sign x | x == 0	= 0
		| x < 0		= -1
					= 1


instance ~ Int
where
 ~ :: !Int -> Int
 ~ x
	= code inline {
			negI
	}

instance ==	Int
where
 (==) :: !Int !Int -> Bool
 (==) a b
	= code inline {
			eqI
	}

instance < Int
where
 (<) :: !Int !Int -> Bool
 (<) a b
	= code inline {
			ltI
	}

instance toInt Int
where
 toInt :: !Int -> Int //	dummy
 toInt a
	= code inline {
			no_op
	}

instance toInt Char
where
 toInt::!Char -> Int
 toInt a
	= code inline {
			CtoI
	}

instance toInt Real
where
 toInt :: !Real -> Int
 toInt a
	= code inline {
			 RtoI
	}

instance fromInt Int
where
 fromInt :: !Int -> Int
 fromInt a
	= code inline {
			no_op
	}

instance fromInt Char
where
 fromInt :: !Int -> Char
 fromInt a
	= code inline {
			ItoC
	}

instance fromInt Real
where
 fromInt :: !Int -> Real
 fromInt a
	= code inline {
			ItoR
	}

instance fromInt {#Char}
where
 fromInt :: !Int -> {#Char}
 fromInt a
	= code inline {
		.d 0 1 i
			jsr ItoAC
		.o 1 0
	}

// Additional functions for integer arithmetic: 

instance rem Int
where
	(rem) :: !Int !Int -> Int
	(rem) a b
	= code inline {
			remI
	  }

instance gcd Int
where
	gcd x y    = gcdnat (abs x) (abs y)
	where
		gcdnat x 0 = x
	    gcdnat x y = gcdnat y (x rem y)


instance lcm Int
where
	lcm _ 0    = 0
	lcm 0 _    = 0
	lcm x y    = abs ((x / gcd x y) * y)

//	Test on Integers:

instance isEven Int
where
	isEven :: !Int -> Bool
	isEven a
		= code inline {
			pushI 1
			and%
			pushI 0
			eqI
		}

instance isOdd Int
where
	isOdd :: !Int -> Bool
	isOdd a
		= code inline {
			pushI 1
			and%
			pushI 0
			eqI
			notB
		}

//	Operators on Bits:

(bitor) infixl 6 :: !Int !Int -> Int
(bitor) a b
	= code inline {
			or%
	}

(bitand) infixl 6 ::	!Int !Int -> Int
(bitand) a b
	= code inline {
			and%
	}

(bitxor) infixl 6 :: !Int !Int -> Int
(bitxor) a b
	= code inline {
			xor%
	}

(<<) infix 7 :: !Int !Int -> Int
(<<) a b
	= code inline {
			shiftl%
	}

(>>) infix 7 :: !Int !Int -> Int
(>>) a b
	= code inline {
			shiftr%
	}

instance toInt {#Char}
where
 toInt::!{#Char} -> Int
 toInt s
	| len==0
		=	0
	| neg
		=	~signedval
	| pos
		=	signedval
	// otherwise
		=	other
 where
	len	=	size s
	neg	=	s.[0] == '-'
	pos	=	s.[0] == '+'

	signedval
			=	toInt2 1 0 s
	other
			=	toInt2 0 0 s

	toInt2:: !Int !Int !{#Char} -> Int
	toInt2 posn val s
		|	len==posn		=	val
		|	isdigit			=	toInt2 (posn+1) (n+val*10) s
		//	otherwise
							=	0
	where
		n 		=	toInt (s.[posn]) - toInt '0'
		isdigit	=	0<=n && n<= 9

bitnot :: !Int -> Int
bitnot a
	= code inline {
			not%
	}
