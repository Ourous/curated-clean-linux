implementation module _SystemEnum

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import StdClass,StdInt,StdChar
from StdBool import not 

_from :: a -> .[a] | IncDec, Ord a
_from n
	= [n : _from (inc n)]

_from_to :: !a !a -> .[a] | Enum a
_from_to n e
	| n <= e
		= [n : _from_to (inc n) e]
		= []

_from_then :: a a -> .[a] | Enum a
_from_then n1 n2
	= [n1 : _from_by n2 (n2-n1)]
where
	_from_by :: a a -> .[a] | Enum a
	_from_by n s
		= [n : _from_by (n+s) s]

_from_then_to :: !a !a !a -> .[a] | Enum a
_from_then_to n1 n2 e
	| n1 <= n2
		= _from_by_to n1 (n2-n1) e
		= _from_by_down_to n1 (n2-n1) e
where
	_from_by_to :: !a !a !a -> .[a] | Enum a
	_from_by_to n s e
		| n<=e
			= [n : _from_by_to (n+s) s e]
			= []
	_from_by_down_to :: !a !a !a -> .[a] | Enum a
	_from_by_down_to n s e
		| n>=e
			= [n : _from_by_down_to (n+s) s e]
			= []
