/*
	module owner: Ronny Wichers Schreur
*/
definition module backendsupport

from StdArray import class Array(size,usize)
from StdInt import class +,class ==
from StdFunc import `bind`

import utilities

identity
	:==	\x -> x

// binding sugar
(==>) infix
(==>) f g
	:== f `bind` g

// o` :== flip o
(o`) infixr
(o`) f g
	:==	\x -> g (f x)

(:-) infixl
(:-) a f
	:== f a


foldState function list
	:== foldState list
	where
		foldState []
			=	identity
		foldState [hd:tl]
			=	function hd
			o`	foldState tl

foldStateA function array
	:== foldStateA 0
	where
		arraySize
			=	size array
		foldStateA index
			| index == arraySize
				=	identity
			// otherwise
				=	function array.[index]
				o`	foldStateA (index+1)

foldStateWithIndexA function array
	:== foldStateWithIndexA 0
	where
		arraySize
			=	size array
		foldStateWithIndexA index
			| index == arraySize
				=	identity
			// otherwise
				=	function index array.[index]
				o`	foldStateWithIndexA (index+1)

foldrA function result array
	:== foldrA 0
	where
		arraySize
			=	size array
		foldrA index
			| index == arraySize
				=	result
			// otherwise
				=	function array.[index] (foldrA (index+1))
