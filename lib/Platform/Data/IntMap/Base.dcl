definition module Data.IntMap.Base

/**
 * @property-bootstrap
 *     import StdEnv, Data.Func
 *     // TODO: this is a bad dependency as IntMap.Base should not depend on IntMap.Strict
 *     from Data.IntMap.Strict import fromList, toList
 *
 *     derive genShow IntMap
 *     derive gPrint  IntMap
 *
 * @property-test-with a = ()
 */
from StdOverloaded import class ==
from Data.Maybe import :: Maybe

// A map of integers to values @a@.

:: IntMap a
  = Nil
  | Tip !Int a
  | Bin !Prefix
        !Mask
        !(IntMap a)
        !(IntMap a)

:: Prefix :== Int
:: Mask   :== Int

instance == (IntMap a) | == a

equal :: !(IntMap a) !(IntMap a) -> Bool | == a

bin :: !Prefix !Mask !(IntMap a) !(IntMap a) -> IntMap a

nomatch :: !Int !Prefix !Mask -> Bool

empty :: IntMap a

foldrWithKey :: (Int a b -> b) b !(IntMap a) -> b

/**
 * @property correctness: A.list :: [(Int, a)]:
 *     fromDistinctAscList distinctAscList =.= fromList distinctAscList
 *     where
 *         distinctAscList = sort $ removeDup list
 */
fromDistinctAscList :: ![(Int, a)] -> IntMap a

union :: !(IntMap a) !(IntMap a) -> IntMap a

unions :: ![IntMap a] -> IntMap a

mask :: !Int !Mask -> Prefix

shorter :: !Mask !Mask -> Bool

branchMask :: !Prefix !Prefix -> Mask

// | /O(min(n,W))/. Retrieves the minimal (key,value) pair of the map, and
// the map stripped of that element, or 'Nothing' if passed an empty map.
//
// > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
// > minViewWithKey empty == Nothing
minViewWithKey :: !(IntMap a) -> Maybe ((Int, a), IntMap a)

// | /O(min(n,W))/. Retrieves the maximal (key,value) pair of the map, and
// the map stripped of that element, or 'Nothing' if passed an empty map.
//
// > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
// > maxViewWithKey empty == Nothing
maxViewWithKey :: !(IntMap a) -> Maybe ((Int, a), IntMap a)

// | /O(min(n,W))/. Lookup the value at a key in the map. See also 'Data.Map.lookup'.
lookup :: !Int !(IntMap a) -> Maybe a
