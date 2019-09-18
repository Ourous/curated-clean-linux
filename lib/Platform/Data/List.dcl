definition module Data.List

from StdClass import class Ord, class Eq, class IncDec
from StdOverloaded import class ==, class <, class length, class %, class toString, class toChar, class fromString, class fromChar, class +, class *, class /, class *, class /, class *, class /, class *, class /, class zero, class one, class -
import StdList

from Data.GenEq import generic gEq
from Data.Functor import class Functor
from Data.Maybe import :: Maybe
from Data.Monoid import class Semigroup, class Monoid
from Data.Foldable import class Foldable
from Data.Traversable import class Traversable
from Control.Applicative import class pure, class <*>, class Applicative, class Alternative
from Control.Monad import class Monad, class MonadPlus

instance Functor []
instance pure []
instance <*> []
instance Alternative []
instance Monad []
instance MonadPlus []

instance Semigroup [a]
instance Monoid [a]
instance Foldable []
instance Traversable []

/**
 * An element in the list, or Nothing if it does not exist.
 */
(!?) infixl 9   :: ![.a] !Int -> Maybe .a

/**
 * The first element of the list.
 */
head            :: ![.a] -> .a

/**
 * Everything except the first element of the list.
 */
tail            :: !u:[.a] -> u:[.a]

/**
 * Check if a list is empty.
 */
isnull          :: ![.a] -> Bool

/**
 * Keep a number of elements at the end of the list.
 *
 * @param The number of elements to retain
 * @param The list
 * @result A list with the (|list| - n) last elements of the original list
 */
keep            :: !Int ![a] -> [a]

/**
 * Unzip a list of three-tuples to a three-tuple of lists.
 */
unzip3          :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])

/**
 * Unzip a list of four-tuples to a four-tuple of lists.
 */
unzip4          :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])

/**
 * Unzip a list of five-tuples to a five-tuple of lists.
 */
unzip5          :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])

/**
 * Replace elements in a list when a condition matches.
 *
 * @param The condition p.
 *   The first parameter is the replacement; the second, the old element.
 * @param The replacement r
 * @param The original list
 * @result The original list, with all elements x for which p r x holds replaced by r
 */
replaceInList   :: !(a a -> Bool) !a ![a] -> [a]
sortByIndex     :: ![(Int,a)] -> [a]
intersperse     :: !a ![a] -> [a]
intercalate     :: !.[a] ![.[a]] -> .[a]
transpose       :: ![[a]] -> [.[a]]
subsequences    :: .[a] -> .[[a]]
permutations    :: [a] -> .[[a]]
concatMap       :: (.a -> [.b]) ![.a] -> [.b]
getItems        :: ![a] ![Int] -> [a]
scanl           :: (a -> .(.b -> a)) a [.b] -> .[a]
scanl1          :: (a -> .(a -> a)) !.[a] -> .[a]
replicate       :: !.Int a -> .[a]
cycle           :: !.[a] -> [a]
unfoldr         :: !(.a -> Maybe (.b,.a)) .a -> [.b]
break           :: (a -> .Bool) !.[a] -> .([a],[a])
stripPrefix     :: !.[a] u:[a] -> Maybe v:[a] | == a, [u <= v]
group           :: .(.[a] -> [.[a]]) | == a
groupBy         :: (a -> a -> .Bool) !.[a] -> [.[a]]
inits           :: .[a] -> [.[a]]
tails           :: [a] -> .[[a]]
isPrefixOf      :: !.[a] .[a] -> .Bool | == a
isSuffixOf      :: !.[a] .[a] -> .Bool | == a
isInfixOf       :: .[a] .[a] -> Bool | == a
levenshtein     :: !.[a] !.[a] -> Int | == a
elem            :: a !.[a] -> .Bool | == a
notElem         :: a !.[a] -> .Bool | == a
lookup          :: a ![(a,.b)] -> Maybe .b | == a
find            :: (a -> .Bool) -> .(.[a] -> .(Maybe a))
partition       :: !(a -> .Bool) !.[a] -> (!.[a], !.[a])
elemIndex       :: a -> .(.[a] -> .(Maybe Int)) | == a
elemIndices     :: a -> .(.[a] -> .[Int]) | == a
findIndex       :: (.a -> .Bool) -> .([.a] -> .(Maybe Int))
findIndices     :: (.a -> .Bool) ![.a] -> .[Int]
zip3            :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip4            :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip5            :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zipSt           :: (.a -> .(.b -> (.st -> .st))) ![.a] [.b] .st -> .st
zipWith         :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWithSt       :: (.a -> .(.b -> (.st -> .(.h, .st)))) ![.a] [.b] .st -> .([.h], .st)
zipWith3        :: (.a -> .(.b -> .(.c -> .h))) ![.a] [.b] [.c] -> [.h]
zipWith4        :: (.a -> .(.b -> .(.c -> .(.d -> .h)))) ![.a] [.b] [.c] [.d] -> [.h]
zipWith5        :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
                   ![.a] [.b] [.c] [.d] [.e] -> [.h]
nub             :: !.[a] -> .[a] | == a
nubBy           :: (a -> .(a -> .Bool)) !.[a] -> .[a]
elem_by         :: (a -> .(.b -> .Bool)) a ![.b] -> .Bool
delete          :: u:(a -> v:(w:[a] -> x:[a])) | == a, [v <= u,w <= x]
deleteBy        :: (a -> .(b -> .Bool)) a !u:[b] -> v:[b], [u <= v]
deleteFirstsBy  :: (a -> .(b -> .Bool)) -> u:(v:[b] -> w:(.[a] -> x:[b])), [w <= u,w v <= x]
difference      :: u:(v:[a] -> w:(.[a] -> x:[a])) | == a, [w <= u,w v <= x]
differenceBy 	:: (a -> a -> .Bool) !u:[a] !.[a] -> v:[a], [u <= v]
intersect       :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
intersectBy     :: (a -> b -> .Bool) !.[a] !.[b] -> .[a]
union           :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
unionBy         :: (a -> .(a -> .Bool)) !.[a] .[a] -> .[a]

/** hasDup @as = True:
       @as has at least one element that occurs twice.
    hasDup @as = False:
       @as has no elements that occur twice.
*/
hasDup :: ![a] -> Bool | Eq a

isMemberGen :: !a !.[a] -> Bool | gEq{|*|} a

strictTRMapRev      :: !(.a -> .b) ![.a] -> [.b]
strictTRMapAcc      :: !(u:a -> v:b) !w:[u:a] !x:[v:b] -> y:[v:b], [w <= u,y <= v,x <= y]
strictTRMap         :: !(.a -> .b) ![.a] -> [.b]
reverseTR           :: ![.a] -> [.a]
flattenTR           :: ![[a]] -> [a]
strictTRMapSt       :: !(a .st -> (b, .st)) ![a] !.st -> (![b], !.st)
strictTRMapStAcc    :: !(a .st -> (b, .st)) ![a] ![b] !.st -> (![b], !.st)
strictTRZipWith     :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWithRev  :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWithAcc  :: !(a b -> c) ![a] ![b] ![c] -> [c]
strictTRZip4        :: ![a] ![b] ![c] ![d] -> [(a, b, c, d)]
strictTRZip4Rev     :: ![a] ![b] ![c] ![d] -> [(a, b, c, d)]
strictTRZip4Acc     :: ![a] ![b] ![c] ![d] ![(a, b, c, d)] -> [(a, b, c, d)]
strictTRZip2        :: ![a] ![b]-> [(a, b)]
strictTRZip2Rev     :: ![a] ![b]-> [(a, b)]
strictTRZip2Acc     :: ![a] ![b] ![(a, b)] -> [(a, b)]
strictTRZipWith3    :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3Rev :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3Acc :: !(a b c -> d) ![a] ![b] ![c] ![d] -> [d]

/**
 * Left-associative fold of a list.
 * Variant that use a queue instead of a fixed size list.
 *
 * @param Function that generates new elements, appended to the end of the list,
 *        for each list element and accumulator value
 * @param Function that updates the accumulator value for each list element
 * @param The initial accumulator value
 * @param The initial list
 * @result The final accumulator value
 */
qfoldl :: (a -> b -> [b]) (a -> b -> a) a ![b] -> a
/**
 * Right-associative fold of a list.
 * Variant that use a queue instead of a fixed size list.
 *
 * @param Function that generates new elements, appended to the end of the list,
 *        for each list element and accumulator value
 * @param Function that updates the accumulator value for each list element
 * @param The initial accumulator value
 * @param The initial list
 * @result The final accumulator value
 */
qfoldr :: (a -> b -> [b]) (b -> a -> a) a ![b] -> a

