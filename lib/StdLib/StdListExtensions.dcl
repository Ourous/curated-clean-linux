definition module StdListExtensions

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

// extensions for StdList

from StdOverloaded import class <(..), class one(..), class -(..), class +(..), class ==(..);
from StdClass import class Ord(..), >, >=, dec, inc;
from StdBool import not;
from StdMaybe import :: Maybe;

// support for state handling

//maplSt :: (.a -> .(.st -> (.b,.st))) [.a] .st -> (.[.b],.st)
	// (maplSt f l st) applies "f" to each element of list "l" (left-to-right) and 
	// the state argument "st"
maplSt f l st
	:== mapl_st l st
  where
	mapl_st [] st =  ([], st)
	mapl_st [x:xs] st
		#! (y, st) = f x st
		   (ys, st) = mapl_st xs st
		=  ([y:ys], st)

//maprSt :: (.a -> .(.st -> (.b,.st))) [.a] .st -> (.[.b],.st)
	// (maprSt f l st) applies "f" to each element of list "l" (right-to-left) and
	// the state argument "st"
maprSt f l st
	:== mapr_st l st
  where
	mapr_st [] st =  ([], st)
	mapr_st [x:xs] st
		#! (ys, st) = mapr_st xs st
		   (y, st) = f x st
		=  ([y:ys], st)

// foldlSt :: !(.a -> .(.st -> .st)) ![.a] !.st -> .st
// folding left-to-right with a state
foldlSt op l st :== foldl_st l st
	where
		foldl_st [] st		= st
		foldl_st [a:as] st	= foldl_st as (op a st)

// foldrSt :: !(.a -> .(.st -> .st)) ![.a] !.st -> .st
// folding right-to-left with a state
foldrSt op l st :== foldr_st l
	where
		foldr_st [] = st
		foldr_st [a:as]
			= op a (foldr_st as)

// incFoldSt :: (Int -> .(.b -> .b)) !Int !Int .b -> .b
// incFoldSt f fr to st = foldlSt f [fr..to-1] st
incFoldSt op fr to st :== fold_st fr to st
	where
		fold_st fr to st
			| fr >= to
				= st
				= fold_st (inc fr) to (op fr st)

// decFoldSt :: (Int -> .(.b -> .b)) !Int !Int .b -> .b
// decFoldSt f fr to st = foldlSt f (reverse [fr..to-1]) st
decFoldSt op fr to st :== fold_st fr (dec to) st
	where
		fold_st fr to st
			| fr > to
				= st
				= fold_st fr (dec to) (op to st)

// the remaining definitions are taken from Haskell's standard module "List"

elemIndex :: a ![a] -> .Maybe Int | == a
	// returns the index of first occurance
elemIndices :: a ![a] -> .[Int] | == a
	// returns the indices of all occurances
find :: (a -> Bool) ![a] -> .Maybe a
	// returns element for which the predicate holds
findIndex :: (a -> Bool) ![a] -> .Maybe Int
	// similar to elemIndex
findIndices :: (a -> Bool) ![a] -> .[Int]
	// similar to elemIndices
intersperse :: a ![a] -> .[a]
	// insert 1st arg inbetween every list element,
	// e.g. intersperse ',' ['abc'] == ['a,b,c']
transpose :: ![[a]] -> [.[a]]
	// e.g. transpose [[1,2],[3,4]] == [[1,3],[2,4]]
partition :: (a -> Bool) !.[a] -> (.[a],.[a])
	// partition p l = (filter p l, filter (not o p) l)
group :: ![a] -> [.[a]] | == a
	// = groupBy (==)
groupBy :: (a a -> Bool) ![a] -> [.[a]]
	// groupBy splits its list argument into a list of lists of equal,
	// adjacent elements, e.g. groupBy (==) ['abbc'] = [['a'],['bb'],['c']]
inits :: ![a] -> [.[a]]
	// e.g. inits ['abc'] == [[],['a'],['ab'],['abc]]
tails :: ![a] -> .[[a]]
	// e.g. tails ['abc'] == [['abc],['bc'],['c'],[]]
isPrefixOf :: ![a] ![a] -> Bool | == a
	// whether 2nd arg begins with 1st arg
isSuffixOf :: ![a] ![a] -> Bool | == a
	// whether 2nd arg ends with 1st arg

zip3 :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip4 :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip5 :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]

zipWith :: (.a -> .(.b -> .h))
			![.a] [.b] -> [.h]
zipWith3 :: (.a -> .(.b -> .(.c -> .h)))
			![.a] [.b] [.c] -> [.h]
zipWith4 :: (.a -> .(.b -> .(.c -> .(.d -> .h))))
			![.a] [.b] [.c] [.d] -> [.h]
zipWith5 :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
			![.a] [.b] [.c] [.d] [.e] -> [.h]

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
