implementation module StdChar

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded, StdBool, StdInt, StdClass

instance + Char
where
	(+) :: !Char !Char -> Char
	(+) a b
		= code inline {
			addI
			ItoC
		}

instance - Char
where
	(-) :: !Char !Char -> Char
	(-) a b
		= code inline {
			subI
			ItoC
		}

instance zero Char
where
	zero :: Char
	zero
		= code inline {
			pushI 0
			ItoC
		}

instance one Char
where
	one :: Char
	one
		= code inline {
			pushI 1
			ItoC
		}

instance ==	Char
where
 (==) ::!Char !Char -> Bool
 (==) a b
	= code inline {
			eqC
	}

instance <	Char
where
 (<) ::!Char !Char -> Bool
 (<) a b
	= code inline {
			ltC
	}

instance toChar Char
where
 toChar::!Char -> Char
 toChar a
	= code inline {
			no_op
	}

instance toChar Int
where
 toChar :: !Int -> Char
 toChar a
	= code inline {
			ItoC
	}

instance fromChar Int
where
 fromChar::!Char -> Int
 fromChar a
	= code inline {
			CtoI
	}


instance fromChar {#Char}
where
 fromChar::!Char -> {#Char}
 fromChar a
	= code {
			CtoAC
	}

instance fromChar Char
where
 fromChar::!Char -> Char
 fromChar a
	= code inline {
			no_op
	}
//	Convert Character into:

toUpper::!Char -> Char
toUpper c
	| IsLower c	= clear_lowercase_bit c
    			= c

toLower::!Char -> Char
toLower c
	| IsUpper c	= set_lowercase_bit c
				= c

digitToInt ::!Char -> Int
digitToInt c = toInt c - 48 // toInt '0'

//	Test on Characters:

IsLower c :== c >= 'a' && c <= 'z'
IsUpper c :== c >= 'A' && c <= 'Z'
IsDigit c :== c >= '0'&& c <= '9'
IsAlpha c :== IsUpper (clear_lowercase_bit c)

clear_lowercase_bit :: !Char -> Char
clear_lowercase_bit c = code inline {
		pushI 223
		and%
	}

set_lowercase_bit :: !Char -> Char
set_lowercase_bit c = code inline {
		pushI 32
		or%
	}

isAscii::!Char -> Bool
isAscii c	=  toInt c < 128

isControl::!Char -> Bool
isControl c	=	c < ' '	||	c == '\177'

isPrint::!Char -> Bool
isPrint c	=	c >= ' ' &&	c <= '~'

isSpace::!Char -> Bool
isSpace c	=	c == ' ' || c == '\t' || c == '\n' || c ==  '\r' || c == '\f' || c == '\v'

isUpper::!Char -> Bool
isUpper c = IsUpper c

isLower::!Char -> Bool
isLower c = IsLower c

isAlpha::!Char -> Bool
isAlpha c = IsAlpha c

isDigit::!Char -> Bool
isDigit c = IsDigit c

isOctDigit::!Char -> Bool
isOctDigit c = c >= '0' &&  c <= '7'

isHexDigit::!Char -> Bool
isHexDigit c
	# uc = clear_lowercase_bit c
	= IsDigit c || (uc>='A' &&  uc<='F')

isAlphanum::!Char -> Bool
isAlphanum c = IsAlpha c || IsDigit c
