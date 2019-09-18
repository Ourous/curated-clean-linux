definition module StdArrayExtensions

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

// extensions for StdArray

from StdInt import class -(..), instance - Int, class <(..), instance < Int, 
 	instance + Int, class +(..)
from StdArray	import class Array(..)
from StdClass import class Ord, >=
from StdBool	import ||, not

//------ creating arrays ---------------------------------

createStrictArr :: !Int !a -> .{!a}
createLazyArr :: !Int !a -> .{a}
createString :: !Int !Char -> .String
createUnboxedIntArr :: !Int !Int -> .{#Int}
createUnboxedRealArr :: !Int !Real -> .{#Real}

/**
 * `createStrictArrIncFoldSt size init_elt st` creates and initializes a strict
 * array of size "size". The init_elt funtion is parametrized with integers
 * from `0` to `size-1` and calculates each initial array element. A state `st`
 * is shuffled through the computation. E.g.:
 *
 * `createStrictArrIncFoldSt 2 (\i st->(i+st, st*st)) 2
 *    = ({0+2, 1+(2*2)}, (2*2)*(2*2))) = ({2,5}, 16)`
 */
createStrictArrIncFoldSt :: !Int !(Int .st -> (.a, .st)) !.st -> (!.{!.a}, !.st)

/**
 * Like {{`createStrictArrIncFoldSt`}}, counting just happens in the reverse
 * direction.
 */
createStrictArrDecFoldSt :: !Int !(Int .st -> (.a, .st)) !.st -> (!.{!.a}, !.st)

//------ applying functions on one array element ---------

/**
 * `updateArrElt f index array = { array & [index] = f array.[index] }`
 */
class updateArrElt a :: !(.e -> .e) !Int !*(a .e) -> *(a .e)

instance updateArrElt {}
instance updateArrElt {!}

/**
 * `accArrElt f index array
 *   = (x, { array & [index] = elt` })
 * where (x, elt`) = f array.[index]`
 */
class accArrElt a :: !(.e -> (.x, .e)) !Int !*(a .e) -> (!.x, !*(a .e))

instance accArrElt {}
instance accArrElt {!}

//------ searching in an array ---------------------------

/**
 * `findlArrElt pred array i` increments `i` until `pred s.[i]` holds or the
 * end of the array is reached. `i` should be non-negative. For efficiency this
 * function is defined as a macro.
 * @type (elt -> Bool) (array elt) Int -> Int | Array array elt
 */
findlArrElt pred array i
	:== findl array i
  where
	findl array i
		| i>=size array || pred array.[i]
			= i
		= findl array (i+1)

/**
 * `findrArrElt pred array i` decrements `i` until `p s.[i]` holds or `i<0`.
 * `i<size s` should hold. For efficiency this function is defined as a macro.
 * @type (elt -> Bool) (array elt) Int -> Int | Array array elt
 */
findrArrElt pred array i
	:== findr array i
  where
	findr array i
		| i<0 || pred array.[i]
			= i
		= findr array (i-1)
