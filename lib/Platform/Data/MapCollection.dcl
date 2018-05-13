definition module Data.MapCollection

from Data.Map import :: Map
from Data.Set import :: Set
import StdOverloaded

/** singletonMapSet key value:
		generates a singleton Map of @key to the singleton Set containing @value.
*/
singletonMapSet :: !k !v -> Map k (Set v)

/** addToMapSet key value m = m`:
        if @key is already member of @m, then @value is added to the values associated with @key in @m;
        if @key is not a member of @m, then @key and @value are added as new element to @m.
*/
addToMapSet :: !k !v !(Map k (Set v)) -> Map k (Set v) | < k & <, == v

/** mergeMapToSets m1 m2 = m:
		for every key k for which ('Data.Map'.member k @m1 && 'Data.Map'.member k @m2), @m has the 'Data.Set'.union of their sets
		                                                                       ('Data.Map'.find k @m1 and 'Data.Map'.find k @m2);
		for every key k in @m1 but not in @m2, @m has 'Data.Map'.find k @m1;
		for every key k in @m2 but not in @m1, @m has 'Data.Map'.find k @m2.
*/		
mergeMapToSets   :: !(Map k (Set v)) !(Map k (Set v)) -> Map k (Set v) | < k & <, == v

/** mergeMapToSetss ms:
		merges all maps in @ms via mergeMapToSets.
*/
mergeMapToSetss  :: ![Map k (Set v)] -> Map k (Set v) | < k & <, == v

/** invertToMapSet m = m`:
		all (k_1,v) ... (k_n,v) pairs in @m are transformed to (v,{k_1,...,k_n}) in @m`.
*/
invertToMapSet :: !(Map k v) -> Map v (Set k) | < v & <, == k

/** singletonMapList key value:
		generates a singleton Map of @key to the singleton list containing @value.
*/
singletonMapList :: !k v -> Map k [v]

/** mergeMapToListss m1 m2 = m:
		for every key k for which ('Data.Map'.member k @m1 && 'Data.Map'.member k @m2), @m has the (++) of their lists
		                                                            ('Data.Map'.find k @m1 and 'Data.Map'.find k @m2);
		for every key k in @m1 but not in @m2, @m has 'Data.Map'.find k @m1;
		for every key k in @m2 but not in @m1, @m has 'Data.Map'.find k @m2.
*/
mergeMapToLists  :: !(Map k [v]) !(Map k [v]) -> Map k [v] | < k

/** mergeMapToListss ms:
		merges all maps in @ms via mergeMapToLists.
*/
mergeMapToListss :: ![Map k [v]] -> Map k [v] | < k

/** invertToMapList m = m`:
		all (k_1,v) ... (k_n,v) in @m are transformed to (v,[k_1,...,k_n]) in @m` (actual order not defined).
*/
invertToMapList :: !(Map k v) -> Map v [k] | < v & <, == k
