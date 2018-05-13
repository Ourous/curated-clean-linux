definition module Data.Set

from StdOverloaded	import class ==, class < (..)
from StdClass import class Ord (..), <=, >
from Data.Maybe		import :: Maybe
from Data.List import foldl, map
from StdBool import not, &&
from Data.GenEq import generic gEq
from Data.GenLexOrd import generic gLexOrd, :: LexOrd
from Data.Foldable import class Foldable

// This module is ported from Haskell Data.Set by László Domoszlai. 2013.sep.6

/*-----------------------------------------------------------------------------
 * |
 * Module      :  Data.Set
 * Copyright   :  (c) Daan Leijen 2002
 * License     :  BSD-style
 * Maintainer  :  libraries@haskell.org
 * Stability   :  provisional
 * Portability :  portable

 * An efficient implementation of sets.
 *
 * Since many function names (but not the type name) clash with
 * "Prelude" names, this module is usually imported @qualified@, e.g.
 *
 * >  import Data.Set (Set)
 * >  import qualified Data.Set as Set
 *
 * The implementation of 'Set' is based on /size balanced/ binary trees (or
 * trees of /bounded balance/) as described by:
 *
 *    * Stephen Adams, \"/Efficient sets: a balancing act/\",
 *	Journal of Functional Programming 3(4):553-562, October 1993,
 *	<http://www.swiss.ai.mit.edu/~adams/BB/>.
 *
 *    * J. Nievergelt and E.M. Reingold,
 *	\"/Binary search trees of bounded balance/\",
 *	SIAM journal of computing 2(1), March 1973.
 *
 * Note that the implementation is /left-biased/ -- the elements of a
 * first argument are always preferred to the second, for example in
 * 'union' or 'insert'.  Of course, left-biasing can only be observed
 * when equality is an equivalence relation instead of structural
 * equality.
 *---------------------------------------------------------------------------*/

:: Set a = Tip
         | Bin !Int !a !(Set a) !(Set a)

instance == (Set a) | == a
instance < (Set a) | < a
derive gEq Set
derive gLexOrd Set
instance Foldable Set

/**
 * True iff this is the empty set.
 * @type (Set a) -> Bool
 */
null s :== case s of
             Tip -> True
             (Bin sz _ _ _) -> False

/**
 * The number of elements in the set.
 * @type (Set a) -> Int
 */
size s :== case s of
             Tip -> 0
             (Bin sz _ _ _) -> sz
/**
 * Is the element in the set?
 */
member    :: !a !(Set a) -> Bool | < a

/**
 * Checks if an element is not in the set.
 * @type a (Set a) -> Bool | <, == a
 */
notMember x t :== not (member x t)

/**
 * Is t1 a subset of t2?
 * @type (Set a) (Set a) -> Bool | <, == a
 */
isSubsetOf t1 t2 :== (size t1 <= size t2) && (isSubsetOfX t1 t2)

isSubsetOfX :: !(Set a) !(Set a) -> Bool | < a

/**
 * Is t1 a proper subset of t2?
 * @type (Set a) (Set a) -> Bool | <, == a
 */
isProperSubsetOf s1 s2 :== (size s1 < size s2) && (isSubsetOf s1 s2)

/**
 * The empty set.
 */
newSet :: Set a

/**
 * Create a singleton set.
 */
singleton :: !u:a -> w:(Set u:a), [w <= u]
/**
 * Insert an element in a set.
 * If the set already contains an element equal to the given value, it is replaced with the new value.
 */
insert :: !a !.(Set a) -> Set a | < a

/**
 * Delete an element from a set.
 */
delete :: !a !.(Set a) -> Set a | < a

/**
 * The minimal element of a set.
 */
findMin :: !(Set a) -> a

/**
 * The maximal element of a set.
 */
findMax :: !(Set a) -> a

/**
 * Delete the minimal element.
 */
deleteMin :: !.(Set a) -> Set a

/**
 * Delete the maximal element.
 */
deleteMax :: !.(Set a) -> Set a

/**
 * deleteFindMin set = (findMin set, deleteMin set)
 */
deleteFindMin :: !.(Set a) -> (!a, !Set a)

/**
 * deleteFindMax set = (findMax set, deleteMax set)
 */
deleteFindMax :: !.(Set a) -> (!a, !Set a)

/**
 * Retrieves the minimal key of the set, and the set stripped of that element,
 * or 'Nothing' if passed an empty set.
 */
minView :: !.(Set a) -> .(Maybe (!a, !Set a))

/**
 * Retrieves the maximal key of the set, and the set stripped of that element,
 * or 'Nothing' if passed an empty set.
 */
maxView :: !.(Set a) -> .(Maybe (!a, !Set a))

/**
 * The union of two sets, preferring the first set when equal elements are
 * encountered.
 */
union :: !u:(Set a) !u:(Set a) -> Set a | < a & == a

/**
 * The union of a list of sets.
 * @type !u:[v:(Set a)] -> Set a | < a & == a, [u <= v]
 */
unions ts :== foldl union newSet ts

/**
 * Difference of two sets.
 */
difference :: !(Set a) !(Set a) -> Set a | < a & == a

/**
 * The intersection of two sets.
 * Elements of the result come from the first set
 */
intersection :: !(Set a) !(Set a) -> Set a | < a & == a

/**
 * The intersection of a list of sets.
 * Elements of the result come from the first set
 */
intersections :: ![Set a] -> Set a | < a & == a

/**
 * Filter all elements that satisfy the predicate.
 */
filter :: !(a -> Bool) !(Set a) -> Set a | < a

/**
 * Partition the set into two sets, one with all elements that satisfy the
 * predicate and one with all elements that don't satisfy the predicate.
 */
partition :: !(a -> Bool) !(Set a) -> (!Set a, !Set a) | < a

/**
 * Split a set in elements less and elements greater than a certain pivot.
 *
 * @param The pivot.
 * @param The set.
 * @return A tuple of two sets containing small and large values.
 */
split :: !a !(Set a) -> (!Set a, !Set a) | < a

/**
 * Performs a 'split' but also returns whether the pivot element was found in
 * the original set.
 */
splitMember :: !a !(Set a) -> (!Set a, !Bool, !Set a) | < a

/**
 * O(n) Post-order fold.
 */
fold :: !(a -> .b -> .b) !.b !.(Set a) -> .b

/**
 * Convert the set to an ascending list of elements.
 * @type (Set a) -> [a]
 */
toList s :== toAscList s

/**
 * Same as toList.
 * @type (Set a) -> [a]
 */
toAscList t :== fold (\a as -> [a:as]) [] t

/**
 * Create a set from a list of elements.
 */
fromList :: ![a] -> Set a | < a

/**
 * Map a function to all elements in a set.
 * @type (a -> b) (Set a) -> Set b | <, == a & <, == b
 */
mapSet f s :== fromList (map f (toList s))

/**
 * Map a set without converting it to and from a list.
 */
mapSetMonotonic :: !(a -> b) !(Set a) -> Set b
