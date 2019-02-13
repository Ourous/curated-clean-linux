implementation module Data.MapCollection

import StdList
import Data.List
from Data.Maybe import :: Maybe (..), fromJust, maybeToList
from Data.Map import :: Map
from Data.Set import :: Set
import qualified Data.Map
import qualified Data.Set

singletonMapSet :: !k !v -> Map k (Set v)
singletonMapSet k v = 'Data.Map'.singleton k ('Data.Set'.singleton v)

addToMapSet :: !k !v !(Map k (Set v)) -> Map k (Set v) | < k & <, == v
addToMapSet k v m = 'Data.Map'.alter (merge_kv v) k m
where
	merge_kv :: !v !(Maybe (Set v)) -> Maybe (Set v) | < v
	merge_kv v (Just vs) = Just ('Data.Set'.insert v vs)
	merge_kv v nothing   = Just ('Data.Set'.singleton v)

mergeMapToSets :: !(Map k (Set v)) !(Map k (Set v)) -> Map k (Set v) | < k & <, == v
mergeMapToSets a b
| 'Data.Map'.mapSize a <= 'Data.Map'.mapSize b = foldl merge b ('Data.Map'.toList a)
| otherwise                        = foldl merge a ('Data.Map'.toList b)
where
	merge :: !(Map k (Set v)) !(!k,!Set v) -> Map k (Set v) | < k & <, == v
	merge a (k,addSet) = 'Data.Map'.put k newSet a
	where
		newSet = case 'Data.Map'.get k a of
	                Just oldSet = 'Data.Set'.union addSet oldSet
	                nothing     = addSet

mergeMapToSetss :: ![Map k (Set v)] -> Map k (Set v) | < k & <, == v
mergeMapToSetss [ts : tss] = foldl mergeMapToSets ts tss
mergeMapToSetss _          = 'Data.Map'.newMap

invertToMapSet :: !(Map k v) -> Map v (Set k) | < v & <, == k
invertToMapSet m = mergeMapToSetss [singletonMapSet v k \\ (k,v) <- 'Data.Map'.toList m]

singletonMapList :: !k v -> Map k [v]
singletonMapList k v = 'Data.Map'.singleton k [v]

mergeMapToLists :: !(Map k [v]) !(Map k [v]) -> Map k [v] | < k
mergeMapToLists a b
| 'Data.Map'.mapSize a <= 'Data.Map'.mapSize b = foldl merge b ('Data.Map'.toList a)
| otherwise                        = foldl merge a ('Data.Map'.toList b)
where
	merge :: !(Map k [v]) !(k,[v]) -> Map k [v] | < k
	merge a (k,add) = 'Data.Map'.put k new a
	where
		new = case 'Data.Map'.get k a of
		         Just old = add ++ old
		         nothing  = add

mergeMapToListss :: ![Map k [v]] -> Map k [v] | < k
mergeMapToListss [hs : hss] = foldl mergeMapToLists hs hss
mergeMapToListss _          = 'Data.Map'.newMap

invertToMapList :: !(Map k v) -> Map v [k] | < v & <, == k
invertToMapList m = mergeMapToListss [singletonMapList v k \\ (k,v) <- 'Data.Map'.toList m]
