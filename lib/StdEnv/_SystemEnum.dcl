definition module _SystemEnum

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

/*
	This module must be imported if dotdot expressions are used

		[from .. ]			-> _from from
		[from .. to]		-> _from_to from to
		[from, then .. ]	-> _from_then from then
		[from, then .. to]	-> _from_then_to from then to
*/

from StdClass import class Enum (..), class IncDec (..), class Ord (..),<=,inc,dec
from StdBool import not 
import StdInt,StdChar

_from			::  a		-> .[a] | IncDec , Ord a	special a=Int; a=Char
_from_to		:: !a !a	-> .[a] | Enum a			special a=Int; a=Char
_from_then		::  a  a	-> .[a] | Enum a			special a=Int; a=Char
_from_then_to	:: !a !a !a	-> .[a] | Enum a			special a=Int; a=Char
