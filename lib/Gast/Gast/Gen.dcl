definition module Gast.Gen

/*
	GAST: A Generic Automatic Software Test-system
	
	gen: generic generation of values of a type

	Pieter Koopman, 2004, 2017
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdGeneric
from Data.Set import :: Set
from Data.Map import :: Map

:: RandomStream :== [Int]
aStream :: RandomStream
splitRandomStream :: !RandomStream -> (RandomStream,RandomStream)
randomize :: ![a] [Int] Int ([Int] -> [a]) -> [a]

generic ggen a :: !GenState -> [a]

:: GenState =
	{ depth                 :: !Int //* depth
	, maxDepth              :: !Int
	, path                  :: ![ConsPos] //* path to nonrecursive constructor
	, skewl                 :: !Int
	, skewr                 :: !Int
	, recInfo               :: !Map TypeName (Set TypeName)
	, pairTree              :: !PairTree
	, recFieldValueNrLimits :: !Map (TypeName, RecFieldName) Int //* Restricts the number of values generated for record fields
	}

:: TypeName	    :== String
:: RecFieldName :== String
:: PairTree       = PTLeaf | PTNode PairTree Bool Bool PairTree

genState :: GenState

derive ggen Int, Bool, Real, Char, String
derive ggen UNIT, PAIR, EITHER, CONS of gcd, OBJECT of gtd, FIELD of d, RECORD of grd
derive ggen (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
derive ggen [], [!], [ !], [!!]
derive ggen {}, {!}

maxint	:: Int
minint	:: Int
StrLen	:== 80
