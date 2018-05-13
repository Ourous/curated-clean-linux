definition module StdListExtensions

import Data.List

from StdOverloaded import class <(..), class one(..), class -(..), class +(..), class ==(..);
from StdClass import class Ord(..), >, >=, dec, inc;
from StdBool import not;

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
