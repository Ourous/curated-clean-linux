definition module Data.Map

/**
 * This module provides a dynamic Map type for creating mappings from keys to values
 * Internally it uses an AVL tree to organize the key-value pairs stored in the mapping
 * such that lookup, insert and delete operations can be performed in O(log n).
 *
 * @property-bootstrap
 *   import StdBool, StdChar, StdInt, StdTuple
 *   from StdList import all, isMember, removeDup, reverse, instance length []
 *   from Data.Func import on, `on`
 *   import Data.GenDefault
 *   from Data.List import nubBy
 *
 *   :: Predicate a = ConstTrue | IsMember [a]
 *
 *   pred :: (Predicate a) a -> Bool | Eq a
 *   pred ConstTrue     _ = True
 *   pred (IsMember cs) c = isMember c cs
 *
 *   :: GMap k v =
 *     { gma  :: !v
 *     , gmb  :: !v
 *     , gmc  :: !v
 *     , gmd  :: !v
 *     , gme  :: !v
 *     , gmf  :: !v
 *     , rest :: ![(k,v)]
 *     }
 *
 *   class Key k
 *   where keya :: k; keyb :: k; keyc :: k; keyd :: k; keye :: k; keyf :: k
 *
 *   instance Key Char
 *   where keya = 'a'; keyb = 'b'; keyc = 'c'; keyd = 'd'; keye = 'e'; keyf = 'f'
 *
 *   derive class Gast GMap, Predicate
 *   derive genShow Map, Maybe
 *   derive gPrint Map, Maybe
 *
 *   kvs :: (GMap k v) -> [(k,v)] | Key k
 *   kvs gm =
 *     [ (keya,gm.gma)
 *     , (keyb,gm.gmb)
 *     , (keyc,gm.gmc)
 *     , (keyd,gm.gmd)
 *     , (keye,gm.gme)
 *     , (keyf,gm.gmf)
 *     : gm.rest
 *     ]
 *
 *   all_present :: [(k,v)] (Map k v) -> Bool | <, == k & == v
 *   all_present kvs m = all (\(k,v) -> get k m == Just v) kvs`
 *   where
 *     kvs` = nubBy ((==) `on` fst) (reverse kvs) // Remove duplicate keys, assuming the last takes precedence
 *
 *   all_from :: (Map k v) [(k,v)] -> Bool | Eq k & Eq v
 *   all_from Tip _ = True
 *   all_from (Bin _ k v l r) kvs = isMember (k,v) kvs && all_from l kvs && all_from r kvs
 *
 * @property-test-with k = Char
 * @property-test-with v = Char
 *
 * @property-test-generator (GMap k v) -> Map k v | Key, <, == k
 *   gen gm = fromList (kvs gm)
 */

from Data.Maybe		import :: Maybe (..)
from StdOverloaded	import class ==, class <
from StdBool        import not
from StdFunc        import id
from Data.GenEq import generic gEq
from Data.GenLexOrd import generic gLexOrd, :: LexOrd
from Data.Monoid    import class Monoid, class Semigroup
import qualified StdList
from Data.Functor import class Functor (..)
from Data.Set import :: Set
from StdOverloaded import class < (..)
import StdClass

/**
 * The abstract Map type provides the mapping.
 * For example "Map Int String" is a mapping "from" integers "to" strings.
 *
 * @var The key type on which the data structure is indexed.
 * @var The type of the values stored in the mapping.
 *
 * @invariant integrity: A.m :: Map k v:
 *   log_size m /\
 *   sizes_correct m
 *
 * @invariant log_size: A.m :: Map k v:
 *   check (<) nelem (2 ^ depth m)
 *   where
 *     nelem = mapSize m
 *
 *     depth :: (Map a b) -> Int
 *     depth Tip = 0
 *     depth (Bin _ _ _ l r) = 1 + (max `on` depth) l r
 *
 * @invariant sizes_correct: A.m :: Map k v:
 *   case m of
 *     Tip                -> prop True
 *     b=:(Bin _ _ _ l r) ->
 *       mapSize b =.= 1 + mapSize l + mapSize r /\
 *       sizes_correct l /\
 *       sizes_correct r
 */
:: Map k v
  = Bin !Int !k !v !(Map k v) !(Map k v)
  | Tip

instance Monoid (Map k v) | < k
instance Semigroup (Map k v) | < k

instance == (Map k v) | Eq k  & Eq v
instance <  (Map k v) | Ord k & Ord v

//Basic functions

/**
 * Check if a Map is empty.
 * @type (Map k a) -> Bool
 * @property equivalence with size 0: A.m :: Map k v:
 *   mapSize m == 0 <==> null m
 * @property equivalence with newMap: A.m :: Map k v:
 *   m == newMap <==> null m
 * @complexity O(1)
 */
null mp :== case mp of
              Tip -> True
              _   -> False

/**
 * Create an empty Map.
 * @result An empty map
 * @property is null:
 *   null newMap
 * @complexity O(1)
 */
newMap      :: w:(Map k u:v), [ w <= u]

/**
 * Create a Map with one element.
 * @complexity O(1)
 */
singleton   :: !k !v -> Map k v

/**
 * The number of elements in a Map.
 * @property correctness: A.m :: Map k v:
 *   mapSize m =.= length (removeDup (keys m))
 */
mapSize     :: !(Map k v) -> Int

/**
 * Adds or replaces the value for a given key.
 *
 * @param The key value to add/update
 * @param The value to add/update at the key position
 * @param The original mapping
 * @result The modified mapping with the added value
 * @property correctness: A.m :: Map k v; k :: k; v :: v:
 *   get k m` =.= Just v /\                                           // Correctly put
 *     check all_present [kv \\ kv=:(k`,_) <- toList m | k <> k`] m` /\ // Other elements untouched
 *     integrity m`
 *   where
 *     m` = put k v m
 * @complexity O(log n)
 */
put :: !k !a !(Map k a) -> Map k a | < k

/**
 * Searches for a value at a given key position. Works only for non-unique
 * mappings.
 *
 * @type k (Map k v) -> Maybe v | < k
 * @param The key to look for
 * @param The orginal mapping
 * @result When found, the value at the key position, if not: Nothing
 * @complexity O(log n)
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
 * @result The modified mapping with the value/key removed
 * @property correctness: A.m :: Map k v; k :: k:
 *   get k m` =.= Nothing /\                                            // Correctly deleted
 *     check all_present [kv \\ kv=:(k`,_) <- toList m | k <> k`] m` /\ // Other elements untouched
 *     integrity m`
 *   where
 *     m` = del k m
 */
del :: !k !(Map k a) -> Map k a | < k

/**
 * Removes the value at a given key position. The mapping can be unique.
 */
delU :: !a !.(Map a b) -> u:(!v:(Maybe b), !Map a b) | == a & < a, [u <= v] // !k !w:(Map k u:v) -> x:(Maybe u:v, !y:(Map k u:v)) | == k & < k, [ w y <= u, x <= y, w <= y]

foldrWithKey :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a
foldlWithKey :: !(.a -> .(k -> .(v -> .a))) !.a !(Map k v) -> .a

//* @type (v a -> a) a (Map k v) -> a
foldrNoKey f x m :== foldrWithKey (\_ v acc -> f v acc) x m

//* @type (a v -> a) a (Map k v) -> a
foldlNoKey f x m :== foldlWithKey (\acc _ v -> f acc v) x m

/**
 * Filter elements in a Map.
 *
 * @param The predicate function.
 * @param The Map.
 * @result A new Map that contains exactly those pairs (k,v) from the original Map for which p k v holds.
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
 * @result A list of key/value tuples in the mapping
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
 * @result A mapping containing all the tuples in the list
 * @property correctness: A.elems :: [(k,v)]:
 *   check all_present elems m /\ // All elements put
 *     check all_from m elems /\  // No other elements
 *     integrity m
 *   where
 *     m = fromList elems
 * @complexity O(n*log n)
 */
fromList :: !u:[v:(!a, !b)] -> Map a b | == a & < a, [u <= v]

/**
 * The keys of all keys of a map.
 * @complexity log(n)
 */
keysSet :: !(Map k a) -> Set k

/**
 * Build a map from a set of keys and a function which for each key computes its value.
 * @complexity log(n)
 */
fromSet :: !(k -> a) !(Set k) -> Map k a

derive gEq Map
derive gLexOrd Map

/**
 * Check if a key exists in a Map.
 * @property correctness: A.k :: k; m :: Map k v:
 *   member k m <==> isMember k (keys m)
 * @complexity O(log n)
 */
member :: !k !(Map k a) -> Bool | < k

/**
 * Checks if a key is not a member of a Map.
 * @type k (Map k v) -> Bool | < k
 * @property correctness: A.k :: k; m :: Map k v:
 *   notMember k m <==> not (isMember k (keys m))
 */
notMember k m :== not (member k m)

/**
 * Find an element in a Map.
 * Aborts when the element is not found.
 * @property correctness: A.k :: k; v :: v; m :: Map k v:
 *   find k (put k v m) =.= v
 * @complexity O(log n)
 */
find :: !k !(Map k a) -> a | < k

/**
 * Find an element in a Map.
 * When the key does not exist, return a default.
 *
 * @param The default.
 * @param The key to look up.
 * @property correctness: A.k :: k; v :: v; m :: Map k v:
 *   findWithDefault default k (put k v m) =.= v /\
 *     findWithDefault default k (del k m) =.= default
 *   where default = gDefault{|*|}
 * @complexity O(log n)
 */
findWithDefault :: !a !k !(Map k a) -> a | < k


/**
 * Find the (Just key) of an element in a Map.
 * When the element does not exist, return Nothing.
 *
 * @param The element you're looking for.
 * @property correctness: A.v :: v; m :: Map k v:
 *   case [k \\ (k,v`) <- toList m | v == v`] of
 *     []    -> findKey v m =.= Nothing
 *     [k:_] -> findKey v m =.= Just k
 * @complexity O(n)
 */
findKey :: !a !(Map k a) -> Maybe k | == a

/**
 * Find a (Just key) of an element in a Map, for which the function yields True.
 * When the element does not exist, return Nothing.
 *
 * @param The search function for checking values in the Map.
 * @property correctness: A.p :: Predicate v; m :: Map k v:
 *   case [k \\ (k,v) <- toList m | pred p v] of
 *     []    -> findKeyWith (pred p) m =.= Nothing
 *     [k:_] -> findKeyWith (pred p) m =.= Just k
 * @complexity O(n)
 */
findKeyWith :: !(a -> Bool) !(Map k a) -> Maybe k

/**
 * Find the key of an element in a Map.
 * If the element is not a member, return the first parameter.
 *
 * @param The result if the second parameter does not occur as a value in the Map.
 * @param The element you're looking for.
 * @property correctness: A.v :: v; m :: Map k v:
 *   case findKey v m of
 *     Nothing -> findKeyWithDefault default v m =.= default
 *     Just k  -> findKeyWithDefault default v m =.= k
 *   where default = gDefault{|*|}
 * @complexity O(n)
 */
findKeyWithDefault :: !k !a !(Map k a) -> k | == a

/**
 * Find the key of an element in a Map, for which the function yields True.
 * If the element is not a member, return the second parameter.
 *
 * @param The search function for checking values in the Map.
 * @param The result when all values in the Map check as False.
 * @property correctness: A.p :: Predicate v; m :: Map k v:
 *   case findKeyWith (pred p) m of
 *     Nothing -> findKeyWithDefaultWith (pred p) default m =.= default
 *     Just k  -> findKeyWithDefaultWith (pred p) default m =.= k
 *   where default = gDefault{|*|}
 * @complexity O(n)
 */
findKeyWithDefaultWith :: !(a -> Bool) !k !(Map k a) -> k


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
 * @result The modified mapping with the values/keys removed
 */
delList xs m :== 'StdList'.foldr (\k m -> del k m) m xs

/**
 * Adds or replaces a list of key/value pairs.
 *
 * @type [(a, b)] (Map a b) -> Map a b | ==, < a
 * @param A list of key/value tuples
 * @param The original mapping
 * @result The modified mapping with the added values
 */
putList xs m :== union (fromList xs) m

instance Functor (Map k)
where
	fmap :: !(a -> b) !(Map k a) -> Map k b

difference :: !(Map k a) !(Map k b) -> Map k a | < k
mapWithKey :: !(k a -> b) !(Map k a) -> Map k b
isSubmapOf :: !(Map k a) !(Map k a) -> Bool | < k & Eq a
isSubmapOfBy :: !(a b -> Bool) !(Map k a) !(Map k b) -> Bool | < k
