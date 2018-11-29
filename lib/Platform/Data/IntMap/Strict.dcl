definition module Data.IntMap.Strict

from Data.Maybe		import :: Maybe (..)
from StdClass		import class Eq, class Ord
from StdOverloaded	import class ==, class <
from StdBool        import not
from StdFunc        import id
from Text.GenJSON   import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenEq import generic gEq
from Data.Monoid    import class Monoid, class Semigroup
from Data.List import foldr
from Data.Functor import class Functor (..)
from Data.IntMap.Base import :: IntMap (..), :: Mask, :: Prefix, minViewWithKey, maxViewWithKey, empty, lookup, instance == (IntMap a), equal

null :: !(IntMap a) -> Bool

/**
* Create an empty Map
*
* @return An empty map
*/
newMap      :: w:(IntMap u:v), [ w <= u]

singleton   :: !Int !.a -> .(IntMap .a)

mapSize     :: !(IntMap v) -> Int

/**
* Adds or replaces the value for a given key.
*
* @param The key value to add/update
* @param The value to add/update at the key position
* @param The original mapping
* @return The modified mapping with the added value
*/
put :: !Int !u:a !v:(IntMap u:a) -> w:(IntMap u:a), [w <= u,v <= w]

// | /O(min(n,W))/. Insert a new key\/value pair in the map.
// If the key is already present in the map, the associated value is
// replaced with the supplied value, i.e. 'insert' is equivalent to
// @'insertWith' 'const'@.
//
// > insert 5 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'x')]
// > insert 7 'x' (fromList [(5,'a'), (3,'b')]) == fromList [(3, 'b'), (5, 'a'), (7, 'x')]
// > insert 5 'x' empty                         == singleton 5 'x'
insert :: !Int !u:a !v:(IntMap u:a) -> w:(IntMap u:a), [w <= u,v <= w]
// right-biased insertion, used by 'union'
// | /O(min(n,W))/. Insert with a combining function.
// @'insertWith' f key value mp@
// will insert the pair (key, value) into @mp@ if key does
// not exist in the map. If the key does exist, the function will
// insert @f new_value old_value@.
//
// > insertWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
// > insertWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
// > insertWith (++) 5 "xxx" empty                         == singleton 5 "xxx"
insertWith :: !(a a -> a) !Int !a !(IntMap a) -> IntMap a

// | /O(min(n,W))/. Adjust a value at a specific key. When the key is not
// a member of the map, the original map is returned.
//
// > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
// > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > adjust ("new " ++) 7 empty                         == empty
adjust :: !(a -> a) !Int !(IntMap a) -> IntMap a
/**
* Searches for a value at a given key position. Works only for non-unique
* mappings.
*
* @param The key to look for
* @param The orginal mapping
* @return When found, the value at the key position, if not: Nothing
*/

get :: !Int !.(IntMap .a) -> Maybe .a

getU :: !Int !*(IntMap a) -> *(.(Maybe a), *(IntMap a))

/**
* Removes the value at a given key position. The mapping itself can be spine unique.
*
* @param The key to remove
* @param The original mapping
* @return The modified mapping with the value/key removed
*/
del :: !Int !(IntMap a) -> IntMap a
delete :: !Int !(IntMap a) -> IntMap a

keys  :: !.(IntMap a) -> [Int]
elems :: !.(IntMap a) -> [a]

derive JSONEncode IntMap
derive JSONDecode IntMap
derive gEq IntMap

size :: !(IntMap a) -> Int

member :: !Int !(IntMap a) -> Bool

/**
 * @type !Int !(IntMap a) -> Bool
 */
notMember k m :== not (member k m)

find :: !Int !(IntMap a) -> a

findWithDefault :: a !Int !(IntMap a) -> a

alter :: !((Maybe a) -> Maybe a) !Int !(IntMap a) -> IntMap a

unionWith :: !(a a -> a) !(IntMap a) !(IntMap a) -> IntMap a
unionsWith :: !(a a -> a) ![IntMap a] -> IntMap a

mergeWithKey :: !(Int a b -> Maybe c) !((IntMap a) -> IntMap c) !((IntMap b) -> IntMap c)
             !(IntMap a) !(IntMap b) -> IntMap c

foldlStrict :: !(a b -> a) !a ![b] -> a

foldrWithKey :: !(Int .a -> .(.b -> .b)) !.b !.(IntMap .a) -> .b

union :: !(IntMap a) !(IntMap a) -> IntMap a

unions :: ![IntMap a] -> IntMap a

instance Functor IntMap

// | /O(n)/. Map a function over all values in the map.
//
// > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]
map :: !(a -> b) !(IntMap a) -> IntMap b

// | /O(n)/. Map a function over all values in the map.
//
// > let f key x = (show key) ++ ":" ++ x
// > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]
mapWithKey :: !(Int a -> b) !(IntMap a) -> IntMap b

mapSt :: !(a *st -> *(!b, !*st)) !.(IntMap a) !*st -> *(!IntMap b, !*st)

toList :: !(IntMap a) -> [(!Int, !a)]

toAscList :: !(IntMap a) -> [(!Int, !a)]

fromList :: ![(!Int, !a)] -> IntMap a

// | /O(n*min(n,W))/. Create a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
//
// > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
// > fromListWith (++) [] == empty
fromListWith :: !(a a -> a) ![(!Int, !a)] -> IntMap a
