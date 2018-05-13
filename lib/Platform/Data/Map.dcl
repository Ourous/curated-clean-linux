definition module Data.Map
/**
 * This module provides a dynamic Map type for creating mappings from keys to values
 * Internally it uses an AVL tree to organize the key-value pairs stored in the mapping
 * such that lookup, insert and delete operations can be performed in O(log n).
 */

from Data.Maybe		import :: Maybe (..)
from StdOverloaded	import class ==, class <
from StdBool        import not
from StdFunc        import id
from Text.JSON      import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Generics.GenEq import generic gEq
from Data.Generics.GenLexOrd import generic gLexOrd, :: LexOrd
from Data.Monoid    import class Monoid, class Semigroup
import qualified StdList as SL
from Data.List import foldr
from Data.Functor import class Functor (..)
from StdOverloaded import class < (..)
import StdClass

/**
 * The abstract Map type provides the mapping.
 * For example "Map Int String" is a mapping "from" integers "to" strings.
 *
 * @var The key type on which the data structure is indexed.
 * @var The type of the values stored in the mapping.
 */
:: Map k v
  = Bin !Int !k !v !(Map k v) !(Map k v)
  | Tip

instance Monoid (Map k v) | < k

instance == (Map k v) | Eq k  & Eq v
instance <  (Map k v) | Ord k & Ord v

//Basic functions

/**
 * Check if a Map is empty.
 * @type (Map k a) -> Bool
 */
null mp :== case mp of
              Tip -> True
              _   -> False

/**
 * Create an empty Map.
 * @return An empty map
 */
newMap      :: w:(Map k u:v), [ w <= u]

/**
 * Create a Map with one element.
 */
singleton   :: !k !v -> Map k v

/**
 * The number of elements in a Map.
 */
mapSize     :: !(Map k v) -> Int

/**
 * Adds or replaces the value for a given key.
 *
 * @param The key value to add/update
 * @param The value to add/update at the key position
 * @param The original mapping
 * @return The modified mapping with the added value
 */
put :: !k !a !(Map k a) -> Map k a | < k

/**
 * Searches for a value at a given key position. Works only for non-unique
 * mappings.
 *
 * @type k (Map k v) -> Maybe v | < k
 * @param The key to look for
 * @param The orginal mapping
 * @return When found, the value at the key position, if not: Nothing
 */
get k m :== get` k m
  where
  get` _ Tip              = Nothing
  get` k (Bin _ kx x l r) = if (k < kx)
                              (get` k l)
                              (if (k > kx)
                                 (get` k r)
                                 (Just x))

/**
 * Searches for a value at a given key position. Works also for unique mappings.
 */
getU :: !k !w:(Map k v) -> x:(!Maybe v, !y:(Map k v)) | == k & < k, [ x <= y, w <= y]

/**
* Removes the value at a given key position. The mapping itself can be spine unique.
*
* @param The key to remove
* @param The original mapping
* @return The modified mapping with the value/key removed
*/
del :: !k !(Map k a) -> Map k a | < k

/**
 * Removes the value at a given key position. The mapping can be unique.
 */
delU :: !a !.(Map a b) -> u:(!v:(Maybe b), !Map a b) | == a & < a, [u <= v] // !k !w:(Map k u:v) -> x:(Maybe u:v, !y:(Map k u:v)) | == k & < k, [ w y <= u, x <= y, w <= y]

foldrWithKey :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a
foldlWithKey :: !(u:a k v -> u:a) !u:a !(Map k v) -> u:a

/**
 * @type (v a -> a) a (Map k v) -> a
 */
foldrNoKey f x m :== foldrWithKey (\_ v acc -> f v acc) x m

/**
 * @type (a v -> a) a (Map k v) -> a
 */
foldlNoKey f x m :== foldlWithKey (\acc _ v -> f acc v) x m

/**
 * Filter elements in a Map.
 *
 * @param The predicate function.
 * @param The Map.
 * @return A new Map that contains exactly those pairs (k,v) from the original Map for which p k v holds.
 */
filterWithKey :: !(k v -> Bool) !(Map k v) -> Map k v

/**
 * A list of the keys in a Map.
 * @type (Map k v) -> [k]
 */
keys m :== foldrWithKey (\k _ ks -> [k : ks]) [] m

/**
 * A list of the elements in a Map.
 * @type (Map k v) -> [v]
 */
elems m :== foldrNoKey (\x xs -> [x:xs]) [] m

//Conversion functions

/**
 * Converts a mapping to a list of key value pairs.
 * Because of the internal ordering of the mapping the resulting
 * list is sorted ascending on the key part of the tuple.
 *
 * @type (Map k v) -> [(k,v)]
 * @param The original mapping
 * @return A list of key/value tuples in the mapping
 */
toList m :== toAscList m

/**
 * Same as toList.
 * @type (Map k v) -> [(k,v)]
 */
toAscList m :== foldrWithKey (\k x xs -> [(k,x):xs]) [] m

/**
 * Converts a list of key/value tuples to a mapping.
 *
 * @param A list of key/value tuples
 * @return A mapping containing all the tuples in the list
 */
fromList :: !u:[v:(!a, !b)] -> Map a b | == a & < a, [u <= v]

derive JSONEncode Map
derive JSONDecode Map
derive gEq Map
derive gLexOrd Map

/**
 * Check if a key exists in a Map.
 */
member :: !k !(Map k a) -> Bool | < k

/**
 * Checks if a key is not a member of a Map.
 * @type k (Map k v) -> Bool | < k
 */
notMember k m :== not (member k m)

/**
 * Find an element in a Map.
 * Aborts when the element is not found.
 */
find :: !k !(Map k a) -> a | < k

/**
 * Find an element in a Map.
 * When the key does not exist, return a default.
 *
 * @param The default.
 * @param The key to look up.
 */
findWithDefault :: !a !k !(Map k a) -> a | < k

alter :: !((Maybe a) -> Maybe a) !k !(Map k a) -> Map k a | < k

/**
 * Get the index of a key in a Map so that it can be retrieved with
 * {{`elemAt`}}.
 */
getIndex :: !k !(Map k a) -> Maybe Int | < k

/**
 * Get the entry at a certain index. To get an index for a certain key, see
 * {{`getIndex`}}.
 */
elemAt :: !Int !(Map k a) -> Maybe (!k, !a)

/**
 * Update an entry at a certain index. To get an index for a certain key, see
 * {{`getIndex`}}.
 *
 * @param The update function
 * @param The index
 * @param The map
 * @result The new map, or Maybe in case of an index out of range
 */
updateAt :: !(k a -> Maybe a) !Int !(Map k a) -> Maybe (Map k a)

findMin :: !(Map k a) -> (!k, !a)

findMax :: !(Map k a) -> (!k, !a)

unions :: ![Map k a] -> Map k a | < k

unionsWith :: !(a a -> a) ![Map k a] -> Map k a | < k

unionWith :: !(a a -> a) !(Map k a) !(Map k a) -> Map k a | < k

unionWithKey :: !(k a a -> a) !(Map k a) !(Map k a) -> Map k a | < k

intersection :: !(Map k a) !(Map k b) -> Map k a | < k

intersectionWith :: !(a b -> c) !(Map k a) !(Map k b) -> Map k c | < k

intersectionWithKey :: !(k a b -> c) !(Map k a) !(Map k b) -> Map k c | < k

union :: !(Map k a) !(Map k a) -> Map k a | < k

mergeWithKey :: !(k a b -> Maybe c) !((Map k a) -> Map k c) !((Map k b) -> Map k c)
             !(Map k a) !(Map k b) -> Map k c | < k

foldlStrict :: !(a b -> a) !a ![b] -> a

/**
 * Removes the values at given key positions. The mapping itself can be spine unique.
 *
 * @type [a] (Map a b) -> Map a b | <, == a
 * @param The list of keys to remove
 * @param The original mapping
 * @return The modified mapping with the values/keys removed
 */
delList xs m :== 'SL'.foldr (\k m -> del k m) m xs

/**
 * Adds or replaces a list of key/value pairs.
 *
 * @type [(a, b)] (Map a b) -> Map a b | ==, < a
 * @param A list of key/value tuples
 * @param The original mapping
 * @return The modified mapping with the added values
 */
putList xs m :== union (fromList xs) m

instance Functor (Map k)

difference :: !(Map k a) !(Map k b) -> Map k a | < k
mapWithKey :: !(k a -> b) !(Map k a) -> Map k b
isSubmapOf :: !(Map k a) !(Map k a) -> Bool | < k & Eq a
isSubmapOfBy :: !(a b -> Bool) !(Map k a) !(Map k b) -> Bool | < k
