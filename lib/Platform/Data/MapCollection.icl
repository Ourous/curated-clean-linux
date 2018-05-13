implementation module Data.MapCollection

import Data.List
from Data.Maybe import :: Maybe (..), fromJust, maybeToList
from Data.Map import :: Map
from Data.Set import :: Set
import qualified Data.Map as DM
import qualified Data.Set as DS

singletonMapSet :: !k !v -> Map k (Set v)
singletonMapSet k v = 'DM'.singleton k ('DS'.singleton v)

addToMapSet :: !k !v !(Map k (Set v)) -> Map k (Set v) | < k & <, == v
addToMapSet k v m = 'DM'.alter (merge_kv v) k m
where
	merge_kv :: !v !(Maybe (Set v)) -> Maybe (Set v) | < v
	merge_kv v (Just vs) = Just ('DS'.insert v vs)
	merge_kv v nothing   = Just ('DS'.singleton v)

mergeMapToSets :: !(Map k (Set v)) !(Map k (Set v)) -> Map k (Set v) | < k & <, == v
mergeMapToSets a b
| 'DM'.mapSize a <= 'DM'.mapSize b = foldl merge b ('DM'.toList a)
| otherwise                        = foldl merge a ('DM'.toList b)
where
	merge :: !(Map k (Set v)) !(!k,!Set v) -> Map k (Set v) | < k & <, == v
	merge a (k,addSet) = 'DM'.put k newSet a
	where
		newSet = case 'DM'.get k a of
	                Just oldSet = 'DS'.union addSet oldSet
	                nothing     = addSet

mergeMapToSetss :: ![Map k (Set v)] -> Map k (Set v) | < k & <, == v
mergeMapToSetss [ts : tss] = foldl mergeMapToSets ts tss
mergeMapToSetss _          = 'DM'.newMap

invertToMapSet :: !(Map k v) -> Map v (Set k) | < v & <, == k
invertToMapSet m = mergeMapToSetss [singletonMapSet v k \\ (k,v) <- 'DM'.toList m]

singletonMapList :: !k v -> Map k [v]
singletonMapList k v = 'DM'.singleton k [v]

mergeMapToLists :: !(Map k [v]) !(Map k [v]) -> Map k [v] | < k
mergeMapToLists a b
| 'DM'.mapSize a <= 'DM'.mapSize b = foldl merge b ('DM'.toList a)
| otherwise                        = foldl merge a ('DM'.toList b)
where
	merge :: !(Map k [v]) !(k,[v]) -> Map k [v] | < k
	merge a (k,add) = 'DM'.put k new a
	where
		new = case 'DM'.get k a of
		         Just old = add ++ old
		         nothing  = add

mergeMapToListss :: ![Map k [v]] -> Map k [v] | < k
mergeMapToListss [hs : hss] = foldl mergeMapToLists hs hss
mergeMapToListss _          = 'DM'.newMap

invertToMapList :: !(Map k v) -> Map v [k] | < v & <, == k
invertToMapList m = mergeMapToListss [singletonMapList v k \\ (k,v) <- 'DM'.toList m]
