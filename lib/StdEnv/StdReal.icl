implementation module StdReal

// ********************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ********************************************************
import StdClass
import StdOverloaded,StdInt,StdArray
from StdBool import &&,||,not
from StdChar import instance == Char, instance < Char, class ==(..)

instance + Real
where
 (+) :: !Real !Real -> Real
 (+) a b
	= code inline {
			addR
	}

instance - Real
where
 (-) :: !Real !Real -> Real
 (-) a b
	= code inline {
			subR
	}

instance zero Real
where
 zero:: Real
 zero
	= code inline {
			pushR 0.0
	}

instance * Real
where
 (*) :: !Real !Real -> Real
 (*) a b
	= code inline {
			mulR
	}

instance / Real
where
 (/) :: !Real !Real -> Real
 (/) a b
	= code inline {
			divR
	}

instance one Real
where
 one:: Real
 one
	= code inline {
			pushR 1.0
	}

instance ^ Real
where
 (^) :: !Real !Real -> Real
 (^) a b
	= code inline {
			powR
	}

instance abs Real
where
 abs::!Real -> Real
 abs x
	= code inline {
			absR
	}

instance sign Real
where
 sign::!Real -> Int
 sign x | x == 0.0	= 0
		| x < 0.0	= -1
 					= 1

instance ~ Real
where
 ~ :: !Real -> Real
 ~ x
	= code inline {
			negR
	}

instance == Real
where
 (==) :: !Real !Real -> Bool
 (==) a b
	= code inline {
			eqR
	}

instance < Real
where
 (<) :: !Real !Real -> Bool
 (<) a b
	= code inline {
			 ltR
	}

instance ln Real
where
	ln a
		= code inline {
				lnR
		}

instance log10 Real
where
	log10 a
		= code inline {
				log10R
		}

instance exp  Real
where exp a
		= code inline {
				expR
		}

instance sqrt Real
where sqrt a
		= code inline {
				sqrtR
		}

instance sin Real
where
	sin a
		= code inline {
				sinR
		}

instance cos Real
where
	cos a
		= code inline {
				cosR
		}

instance tan Real
where 
	tan a
		= code inline {
				tanR
		}
		
instance asin Real
where
	asin a
		= code inline {
				asinR
		}

instance acos Real
where
	acos a
		= code inline {
				acosR
		}

instance atan Real
where
	atan a
		= code inline {
				atanR
		}

instance sinh Real
where
	sinh x = (exp x - exp (~ x)) * 0.5

instance cosh Real
where
	cosh x =  (exp x + exp (~ x)) * 0.5

instance tanh Real
where
	tanh x = (expx - expmx) / (expx + expmx)
	where
		expx = exp x
		expmx = exp (~ x)

instance asinh Real
where
	asinh x = ln (x + sqrt (x*x + 1.0))

instance acosh Real
where
	acosh x = ln (x + sqrt (x*x - 1.0))  // only the positive value is taken

instance atanh Real
where
	atanh x = ln ((1.0 + x)/(1.0 - x)) * 0.5

instance toReal	Int
where
 toReal :: !Int -> Real
 toReal a
	= code inline {
			ItoR
	}

instance toReal Real
where
 toReal :: !Real -> Real
 toReal a
	= code inline {
			 no_op
	}

instance fromReal Int
where
 fromReal :: !Real -> Int
 fromReal a
	= code inline {
			RtoI
	}

instance fromReal Real
where
 fromReal :: !Real -> Real
 fromReal a
	= code inline {
			 no_op
	}

instance fromReal {#Char}
where
 fromReal :: !Real -> {#Char}
 fromReal a
	= code inline {
		.d 0 1 r
			jsr RtoAC
		.o 1 0
	}

instance toReal {#Char}
where
 toReal :: !{#Char} -> Real
 toReal s
	|	len == 0
			=	0.0
	|	first  == '-'
			= 	~ signedval
	|	first  == '+'
			=	signedval
	//	otherwise
			= 	val
 where
	len = size s
	first = s.[0]
	signedval = toReal1 s 1 0.0
	val = toReal1 s 0 0.0

	toReal1 :: !{#Char} !Int !Real -> Real
	toReal1 s posn val
		| posn == len
			= 	val
		| digit
			= 	toReal1 s (posn+1) (toReal n + 10.0*val)
		| c == '.'
			= 	toRealWithDot s (posn+1) val 0
		| c== 'e' || c== 'E'
			| posn<len-2 && s.[posn+1] == '-'
				= 	toRealWithExp s (posn+2) val 0 (-1) 0
			| posn<len-2 && s.[posn+1] == '+'
				= 	toRealWithExp s (posn+2) val 0 (+1) 0
			| posn<len-1
				= 	toRealWithExp s (posn+1) val 0 1 0 
				= 	0.0
			= 	0.0
		where
			c		=	s.[posn]
			n		=	toInt c  -  toInt '0' 
			digit	=	c>='0' && c<='9'

	toRealWithDot :: !{#Char} !Int !Real !Int -> Real
	toRealWithDot s posn val dn
		| posn == len
			| dn==0
				= 	val
				= 	val / (10.0 ^ toReal dn)
		| digit
				= 	toRealWithDot s (posn+1) (toReal n + 10.0*val) (dn+1)
		| c== 'e' || c== 'E'
			| posn<len-2 && s.[posn+1] == '-'
				= 	toRealWithExp s (posn+2) val dn (-1) 0
			| posn<len-2 && s.[posn+1] == '+'
				= 	toRealWithExp s (posn+2) val dn (+1) 0
			| posn<len-1
				= 	toRealWithExp s (posn+1) val dn 1 0 
				= 	0.0
			= 	0.0
		where
			c		=	s.[posn]
			n		=	toInt c  -  toInt '0' 
			digit	=	c>='0' && c<='9'

	toRealWithExp :: !{#Char} !Int !Real !Int !Int !Int -> Real
	toRealWithExp s posn val dn eneg eval
		| posn == len
			# e = eneg*eval-dn
			| e>=0
				=	val * 10.0 ^ toReal e
			| e>= -308
				=	val / 10.0 ^ toReal (~e)
				=	(val / 5.0 ^ toReal (~e)) / 2.0 ^ toReal (~e)
		| digit
			= 	toRealWithExp s (posn+1) val dn eneg (n + 10*eval)
			= 	0.0
		where
			c		=	s.[posn]
			n		=	toInt c  -  toInt '0' 
			digit	=	c>='0' && c<='9'

entier :: !Real -> Int
entier a
	= code inline {
			entierR
	}
