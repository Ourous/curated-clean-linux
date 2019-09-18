implementation module Data.Map

import StdEnv
import Data.Either
import Data.GenLexOrd
import Data.Maybe
import Data.Monoid
import Data.Functor
import Data.List
import Control.Applicative
import Control.Monad

import qualified Data.Set
from Data.Set import :: Set

// Ported from Haskell`s Data.Map by Jurriën Stutterheim, 10-09-2014

instance Semigroup (Map k v) | < k where
	mappend :: !(Map k v) !(Map k v) -> Map k v | < k
    mappend x y = union x y

instance Monoid (Map k v) | < k where
    mempty = newMap

mapSize :: !(Map k a) -> Int
mapSize Tip              = 0
mapSize (Bin sz _ _ _ _) = sz

lexOrd x y :== if (x < y) LT (if (x > y) GT EQ)

member :: !k !(Map k a) -> Bool | < k
member _ Tip              = False
member k (Bin _ kx _ l r) = if (k < kx)
                              (member k l)
                              (if (k > kx)
                                 (member k r)
                                 True)

find :: !k !(Map k a) -> a | < k
find _ Tip              = abort "Map.!: given key is not an element in the map"
find k (Bin _ kx x l r) = if (k < kx)
                              (find k l)
                              (if (k > kx)
                                 (find k r)
                                 x)

findWithDefault :: !a !k !(Map k a) -> a | < k
findWithDefault def _ Tip              = def
findWithDefault def k (Bin _ kx x l r) = if (k < kx)
                                           (findWithDefault def k l)
                                           (if (k > kx)
                                              (findWithDefault def k r)
                                              x)

findKey :: !a !(Map k a) -> Maybe k | == a
findKey a m = findKeyWith ((==) a) m

findKeyWith :: !(a -> Bool) !(Map k a) -> Maybe k
findKeyWith select m = listToMaybe [k` \\ (k`,v) <- toList m | select v]

findKeyWithDefault :: !k !a !(Map k a) -> k | == a
findKeyWithDefault k a m = findKeyWithDefaultWith ((==) a) k m

findKeyWithDefaultWith :: !(a -> Bool) !k !(Map k a) -> k
findKeyWithDefaultWith compare k m = fromMaybe k (findKeyWith compare m)

// | /O(log n)/. Find largest key smaller than the given one and return the
// corresponding (key, value) pair.
//
// > getLT 3 (fromList [(3,'a`), (5,'b`)]) == Nothing
// > getLT 4 (fromList [(3,'a`), (5,'b`)]) == Just (3, 'a`)
getLT :: !k !(Map k v) -> Maybe (!k, !v) | < k
getLT k m = goNothing k m
  where
  goNothing :: !k !(Map k v) -> Maybe (!k, !v) | < k
  goNothing _ Tip = Nothing
  goNothing k (Bin _ kx x l r)
    | k <= kx   = goNothing k l
    | otherwise = goJust k kx x r

  goJust :: !k !k !v !(Map k v) -> Maybe (!k, !v) | < k
  goJust _ kx` x` Tip = Just (kx`, x`)
  goJust k kx` x` (Bin _ kx x l r)
    | k <= kx   = goJust k kx` x` l
    | otherwise = goJust k kx x r

// | /O(log n)/. Find smallest key greater than the given one and return the
// corresponding (key, value) pair.
//
// > getGT 4 (fromList [(3,'a`), (5,'b`)]) == Just (5, 'b`)
// > getGT 5 (fromList [(3,'a`), (5,'b`)]) == Nothing
getGT :: !k !(Map k v) -> Maybe (!k, !v) | < k
getGT k m = goNothing k m
  where
  goNothing :: !k !(Map k v) -> Maybe (!k, !v) | < k
  goNothing _ Tip = Nothing
  goNothing k (Bin _ kx x l r)
    | k < kx    = goJust k kx x l
    | otherwise = goNothing k r

  goJust :: !k !k !v !(Map k v) -> Maybe (!k, !v) | < k
  goJust _ kx` x` Tip = Just (kx`, x`)
  goJust k kx` x` (Bin _ kx x l r)
    | k < kx    = goJust k kx x l
    | otherwise = goJust k kx` x` r

// | /O(log n)/. Find largest key smaller or equal to the given one and return
// the corresponding (key, value) pair.
//
// > getLE 2 (fromList [(3,'a`), (5,'b`)]) == Nothing
// > getLE 4 (fromList [(3,'a`), (5,'b`)]) == Just (3, 'a`)
// > getLE 5 (fromList [(3,'a`), (5,'b`)]) == Just (5, 'b`)
getLE :: !k !(Map k v) -> Maybe (!k, !v) | < k
getLE k m = goNothing k m
  where
  goNothing :: !k !(Map k v) -> Maybe (!k, !v) | < k
  goNothing _ Tip              = Nothing
  goNothing k (Bin _ kx x l r) = if (k < kx)
                                   (goNothing k l)
                                   (if (k > kx)
                                      (goJust k kx x r)
                                      (Just (kx, x)))

  goJust :: !k !k !v !(Map k v) -> Maybe (!k, !v) | < k
  goJust _ kx` x` Tip              = Just (kx`, x`)
  goJust k kx` x` (Bin _ kx x l r) = if (k < kx)
                                       (goJust k kx` x` l)
                                       (if (k > kx)
                                          (goJust k kx x r)
                                          (Just (kx, x)))

// | /O(log n)/. Find smallest key greater or equal to the given one and return
// the corresponding (key, value) pair.
//
// > getGE 3 (fromList [(3,'a`), (5,'b`)]) == Just (3, 'a`)
// > getGE 4 (fromList [(3,'a`), (5,'b`)]) == Just (5, 'b`)
// > getGE 6 (fromList [(3,'a`), (5,'b`)]) == Nothing
getGE :: !k !(Map k v) -> Maybe (!k, !v) | < k
getGE k m = goNothing k m
  where
  goNothing :: !k !(Map k v) -> Maybe (!k, !v) | < k
  goNothing _ Tip              = Nothing
  goNothing k (Bin _ kx x l r) = case lexOrd k kx of
                                   LT -> goJust k kx x l
                                   EQ -> Just (kx, x)
                                   GT -> goNothing k r

  goJust :: !k !k !v !(Map k v) -> Maybe (!k, !v) | < k
  goJust _ kx` x` Tip              = Just (kx`, x`)
  goJust k kx` x` (Bin _ kx x l r) = case lexOrd k kx of
                                       LT -> goJust k kx x l
                                       EQ -> Just (kx, x)
                                       GT -> goJust k kx` x` r

newMap :: w:(Map k u:v), [ w <= u]
newMap = Tip

singleton :: !k !a -> Map k a
singleton k x = Bin 1 k x Tip Tip

// See Note: Type of local 'go' function
put :: !k !a !(Map k a) -> Map k a | < k
put kx x Tip               = singleton kx x
put kx x (Bin sz ky y l r) =
  if (kx < ky)
    (balanceL ky y (put kx x l) r)
    (if (kx > ky)
       (balanceR ky y l (put kx x r))
       (Bin sz kx x l r))

// Insert a new key and value in the map if it is not already present.
// Used by `union`.

// See Note: Type of local 'go' function
putR :: !k !a !(Map k a) -> Map k a | < k
putR kx x Tip = singleton kx x
putR kx x t=:(Bin _ ky y l r) =
  if (kx < ky)
    (balanceL ky y (putR kx x l) r)
    (if (kx > ky)
       (balanceR ky y l (putR kx x r))
       t)

// | /O(log n)/. Insert with a function, combining new value and old value.
// @'putWith' f key value mp@
// will put the pair (key, value) into @mp@ if key does
// not exist in the map. If the key does exist, the function will
// put the pair @(key, f new_value old_value)@.
//
// > putWith (++) 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "xxxa")]
// > putWith (++) 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
// > putWith (++) 5 "xxx" newMap                         == singleton 5 "xxx"

putWith :: !(a a -> a) !k !a !(Map k a) -> Map k a | < k
putWith f k v m = putWithKey (\_ x` y` -> f x` y`) k v m

// | /O(log n)/. Insert with a function, combining key, new value and old value.
// @'putWithKey` f key value mp@
// will put the pair (key, value) into @mp@ if key does
// not exist in the map. If the key does exist, the function will
// put the pair @(key,f key new_value old_value)@.
// Note that the key passed to f is the same key passed to 'putWithKey`.
//
// > let f key new_value old_value = (show key) ++ ":" ++ new_value ++ "|" ++ old_value
// > putWithKey f 5 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:xxx|a")]
// > putWithKey f 7 "xxx" (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "xxx")]
// > putWithKey f 5 "xxx" newMap                         == singleton 5 "xxx"

// See Note: Type of local 'go' function
putWithKey :: !(k a a -> a) !k !a !(Map k a) -> Map k a | < k
putWithKey _ kx x Tip = singleton kx x
putWithKey f kx x (Bin sy ky y l r) =
  if (kx < ky)
    (balanceL ky y (putWithKey f kx x l) r)
    (if (kx > ky)
       (balanceR ky y l (putWithKey f kx x r))
       (Bin sy kx (f kx x y) l r))

del :: !k !(Map k a) -> Map k a | < k
del _ Tip = Tip
del k (Bin _ kx x l r) =
  if (k < kx)
    (balanceR kx x (del k l) r)
    (if (k > kx)
       (balanceL kx x l (del k r))
       (glue l r))

// | /O(log n)/. Update a value at a specific key with the result of the provided function.
// When the key is not
// a member of the map, the original map is returned.
//
// > adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
// > adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > adjust ("new " ++) 7 newMap                         == newMap

adjust :: !(a -> a) !k !(Map k a) -> Map k a | < k
adjust f k m = adjustWithKey (\_ x -> f x) k m

// | /O(log n)/. Adjust a value at a specific key. When the key is not
// a member of the map, the original map is returned.
//
// > let f key x = (show key) ++ ":new " ++ x
// > adjustWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
// > adjustWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > adjustWithKey f 7 newMap                         == newMap

adjustWithKey :: !(k a -> a) !k !(Map k a) -> Map k a | < k
adjustWithKey f k m = updateWithKey (\k` x` -> Just (f k` x`)) k m

// | /O(log n)/. The expression (@'update' f k map@) updates the value @x@
// at @k@ (if it is in the map). If (@f x@) is 'Nothing', the element is
// deleted. If it is (@'Just` y@), the key @k@ is bound to the new value @y@.
//
// > let f x = if x == "a" then Just "new a" else Nothing
// > update f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
// > update f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > update f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

update :: !(a -> Maybe a) !k !(Map k a) -> Map k a | < k
update f k m = updateWithKey (\_ x -> f x) k m

// | /O(log n)/. The expression (@'updateWithKey` f k map@) updates the
// value @x@ at @k@ (if it is in the map). If (@f k x@) is 'Nothing',
// the element is deleted. If it is (@'Just` y@), the key @k@ is bound
// to the new value @y@.
//
// > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
// > updateWithKey f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "5:new a")]
// > updateWithKey f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > updateWithKey f 3 (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

// See Note: Type of local 'go' function
updateWithKey :: !(k a -> Maybe a) !k !(Map k a) -> Map k a | < k
updateWithKey _ _ Tip = Tip
updateWithKey f k (Bin sx kx x l r) =
  if (k < kx)
    (balanceR kx x (updateWithKey f k l) r)
    (if (k > kx)
       (balanceL kx x l (updateWithKey f k r))
       (case f kx x of
          Just x` -> Bin sx kx x` l r
          Nothing -> glue l r))

// | /O(log n)/. Lookup and update. See also 'updateWithKey`.
// The function returns changed value, if it is updated.
// Returns the original key value if the map entry is deleted.
//
// > let f k x = if x == "a" then Just ((show k) ++ ":new a") else Nothing
// > updateLookupWithKey f 5 (fromList [(5,"a"), (3,"b")]) == (Just "5:new a", fromList [(3, "b"), (5, "5:new a")])
// > updateLookupWithKey f 7 (fromList [(5,"a"), (3,"b")]) == (Nothing,  fromList [(3, "b"), (5, "a")])
// > updateLookupWithKey f 3 (fromList [(5,"a"), (3,"b")]) == (Just "b", singleton 5 "a")

// See Note: Type of local 'go' function
updateLookupWithKey :: !(k a -> Maybe a) !k !(Map k a) -> (Maybe a,Map k a) | < k
updateLookupWithKey _ _ Tip = (Nothing,Tip)
updateLookupWithKey f k (Bin sx kx x l r) =
          case lexOrd k kx of
               LT -> let (found,l`) = updateLookupWithKey f k l in (found,balanceR kx x l` r)
               GT -> let (found,r`) = updateLookupWithKey f k r in (found,balanceL kx x l r`)
               EQ -> case f kx x of
                       Just x` -> (Just x`,Bin sx kx x` l r)
                       Nothing -> (Just x,glue l r)

// | /O(log n)/. The expression (@'alter` f k map@) alters the value @x@ at @k@, or absence thereof.
// 'alter` can be used to put, delete, or update a value in a 'Map'.
// In short : @'get' k ('alter` f k m) = f ('get' k m)@.
//
// > let f _ = Nothing
// > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
// > alter f 5 (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
// >
// > let f _ = Just "c"
// > alter f 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a"), (7, "c")]
// > alter f 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "c")]

// See Note: Type of local 'go' function
alter :: !((Maybe a) -> Maybe a) !k !(Map k a) -> Map k a | < k
alter f k Tip = case f Nothing of
                   Nothing -> Tip
                   Just x  -> singleton k x

alter f k (Bin sx kx x l r) = case lexOrd k kx of
               LT -> balance kx x (alter f k l) r
               GT -> balance kx x l (alter f k r)
               EQ -> case f (Just x) of
                       Just x` -> Bin sx kx x` l r
                       Nothing -> glue l r

//////////////////////////////////////////////////////////////////////
//  Indexing
//////////////////////////////////////////////////////////////////////
// | /O(log n)/. Lookup the /index/ of a key, which is its zero-based index in
// the sequence sorted by keys. The index is a number from /0/ up to, but not
// including, the 'mapSize' of the map.
//
// > isJust (getIndex 2 (fromList [(5,"a"), (3,"b")]))   == False
// > fromJust (getIndex 3 (fromList [(5,"a"), (3,"b")])) == 0
// > fromJust (getIndex 5 (fromList [(5,"a"), (3,"b")])) == 1
// > isJust (getIndex 6 (fromList [(5,"a"), (3,"b")]))   == False

// See Note: Type of local 'go' function
getIndex :: !k !(Map k a) -> Maybe Int | < k
getIndex k m = go 0 k m
  where
    go :: !Int !k !(Map k a) -> Maybe Int | < k
    go _   _ Tip  = Nothing
    go idx k (Bin _ kx _ l r) = case lexOrd k kx of
      LT -> go idx k l
      GT -> go (idx + mapSize l + 1) k r
      EQ -> Just (idx + mapSize l)

// | /O(log n)/. Retrieve an element by its /index/, i.e. by its zero-based
// index in the sequence sorted by keys. If the /index/ is out of range (less
// than zero, greater or equal to 'mapSize' of the map), 'Nothing` is returned.
//
// > elemAt 0 (fromList [(5,"a"), (3,"b")]) == Just (3,"b")
// > elemAt 1 (fromList [(5,"a"), (3,"b")]) == Just (5, "a")
// > elemAt 2 (fromList [(5,"a"), (3,"b")]) == Nothing

elemAt :: !Int !(Map k a) -> Maybe (!k, !a)
elemAt _ Tip = Nothing
elemAt i (Bin _ kx x l r)
  #! mapSizeL = mapSize l
  = case lexOrd i mapSizeL of
      LT -> elemAt i l
      GT -> elemAt (i - mapSizeL - 1) r
      EQ -> Just (kx,x)

// | /O(log n)/. Update the element at /index/, i.e. by its zero-based index in
// the sequence sorted by keys. If the /index/ is out of range (less than zero,
// greater or equal to 'mapSize' of the map), 'Nothing` is returned.
//
// > updateAt (\ _ _ -> Just "x") 0    (fromList [(5,"a"), (3,"b")]) == Just (fromList [(3, "x"), (5, "a")])
// > updateAt (\ _ _ -> Just "x") 1    (fromList [(5,"a"), (3,"b")]) == Just (fromList [(3, "b"), (5, "x")])
// > updateAt (\ _ _ -> Just "x") 2    (fromList [(5,"a"), (3,"b")]) == Nothing
// > updateAt (\ _ _ -> Just "x") (-1) (fromList [(5,"a"), (3,"b")]) == Nothing
// > updateAt (\_ _  -> Nothing)  0    (fromList [(5,"a"), (3,"b")]) == Just (singleton 5 "a")
// > updateAt (\_ _  -> Nothing)  1    (fromList [(5,"a"), (3,"b")]) == Just (singleton 3 "b")
// > updateAt (\_ _  -> Nothing)  2    (fromList [(5,"a"), (3,"b")]) == Nothing
// > updateAt (\_ _  -> Nothing)  (-1) (fromList [(5,"a"), (3,"b")]) == Nothing

updateAt :: !(k a -> Maybe a) !Int !(Map k a) -> Maybe (Map k a)
updateAt f i t =
  case t of
    Tip = Nothing
    Bin sx kx x l r
      #! mapSizeL = mapSize l
      = case lexOrd i mapSizeL of
          LT -> flip (balanceR kx x) r <$> updateAt f i l
          GT -> balanceL kx x l <$> updateAt f (i-mapSizeL-1) r
          EQ -> case f kx x of
                  Just x` -> Just (Bin sx kx x` l r)
                  Nothing -> Just (glue l r)


//////////////////////////////////////////////////////////////////////
//  Minimal, Maximal
//////////////////////////////////////////////////////////////////////
// | /O(log n)/. The minimal key of the map. Calls 'abort` if the map is newMap.
//
// > findMin (fromList [(5,"a"), (3,"b")]) == (3,"b")
// > findMin newMap                            Error: newMap map has no minimal element

findMin :: !(Map k a) -> (!k, !a)
findMin (Bin _ kx x Tip _)  = (kx,x)
findMin (Bin _ _  _ l _)    = findMin l
findMin Tip                 = abort "Map.findMin: newMap map has no minimal element"

// | /O(log n)/. The maximal key of the map. Calls 'abort` if the map is newMap.
//
// > findMax (fromList [(5,"a"), (3,"b")]) == (5,"a")
// > findMax newMap                            Error: newMap map has no maximal element

findMax :: !(Map k a) -> (!k, !a)
findMax (Bin _ kx x _ Tip)  = (kx,x)
findMax (Bin _ _  _ _ r)    = findMax r
findMax Tip                 = abort "Map.findMax: newMap map has no maximal element"

// | /O(log n)/. Delete the minimal key. Returns an newMap map if the map is newMap.
//
// > deleteMin (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(5,"a"), (7,"c")]
// > deleteMin newMap == newMap

deleteMin :: !(Map k a) -> Map k a
deleteMin (Bin _ _  _ Tip r)  = r
deleteMin (Bin _ kx x l r)    = balanceR kx x (deleteMin l) r
deleteMin Tip                 = Tip

// | /O(log n)/. Delete the maximal key. Returns an newMap map if the map is newMap.
//
// > deleteMax (fromList [(5,"a"), (3,"b"), (7,"c")]) == fromList [(3,"b"), (5,"a")]
// > deleteMax newMap == newMap

deleteMax :: !(Map k a) -> Map k a
deleteMax (Bin _ _  _ l Tip)  = l
deleteMax (Bin _ kx x l r)    = balanceL kx x l (deleteMax r)
deleteMax Tip                 = Tip

// | /O(log n)/. Update the value at the minimal key.
//
// > updateMin (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "Xb"), (5, "a")]
// > updateMin (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMin :: !(a -> Maybe a) !(Map k a) -> Map k a
updateMin f m
  = updateMinWithKey (\_ x -> f x) m

// | /O(log n)/. Update the value at the maximal key.
//
// > updateMax (\ a -> Just ("X" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "Xa")]
// > updateMax (\ _ -> Nothing)         (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMax :: !(a -> Maybe a) !(Map k a) -> Map k a
updateMax f m = updateMaxWithKey (\_ x -> f x) m


// | /O(log n)/. Update the value at the minimal key.
//
// > updateMinWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"3:b"), (5,"a")]
// > updateMinWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

updateMinWithKey :: !(k a -> Maybe a) !(Map k a) -> Map k a
updateMinWithKey _ Tip                 = Tip
updateMinWithKey f (Bin sx kx x Tip r) = case f kx x of
                                           Nothing -> r
                                           Just x` -> Bin sx kx x` Tip r
updateMinWithKey f (Bin _ kx x l r)    = balanceR kx x (updateMinWithKey f l) r

// | /O(log n)/. Update the value at the maximal key.
//
// > updateMaxWithKey (\ k a -> Just ((show k) ++ ":" ++ a)) (fromList [(5,"a"), (3,"b")]) == fromList [(3,"b"), (5,"5:a")]
// > updateMaxWithKey (\ _ _ -> Nothing)                     (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"

updateMaxWithKey :: !(k a -> Maybe a) !(Map k a) -> Map k a
updateMaxWithKey _ Tip                 = Tip
updateMaxWithKey f (Bin sx kx x l Tip) = case f kx x of
                                           Nothing -> l
                                           Just x` -> Bin sx kx x` l Tip
updateMaxWithKey f (Bin _ kx x l r)    = balanceL kx x l (updateMaxWithKey f r)

// | /O(log n)/. Retrieves the minimal (key,value) pair of the map, and
// the map stripped of that element, or 'Nothing' if passed an newMap map.
//
// > minViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((3,"b"), singleton 5 "a")
// > minViewWithKey newMap == Nothing

minViewWithKey :: !(Map k a) -> Maybe (!(!k, !a), !Map k a)
minViewWithKey Tip = Nothing
minViewWithKey x   = Just (deleteFindMin x)

// | /O(log n)/. Retrieves the maximal (key,value) pair of the map, and
// the map stripped of that element, or 'Nothing' if passed an newMap map.
//
// > maxViewWithKey (fromList [(5,"a"), (3,"b")]) == Just ((5,"a"), singleton 3 "b")
// > maxViewWithKey newMap == Nothing

maxViewWithKey :: !(Map k a) -> Maybe (!(!k, !a), !Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey x   = Just (deleteFindMax x)

// | /O(log n)/. Retrieves the value associated with minimal key of the
// map, and the map stripped of that element, or 'Nothing' if passed an
// newMap map.
//
// > minView (fromList [(5,"a"), (3,"b")]) == Just ("b", singleton 5 "a")
// > minView newMap == Nothing

minView :: !(Map k a) -> Maybe (!a, !Map k a)
minView Tip = Nothing
minView x   = Just (first snd (deleteFindMin x))

// | /O(log n)/. Retrieves the value associated with maximal key of the
// map, and the map stripped of that element, or 'Nothing' if passed an
//
// > maxView (fromList [(5,"a"), (3,"b")]) == Just ("a", singleton 3 "b")
// > maxView newMap == Nothing

maxView :: !(Map k a) -> Maybe (!a, !Map k a)
maxView Tip = Nothing
maxView x   = Just (first snd (deleteFindMax x))

// Update the 1st component of a tuple (special case of Control.Arrow.first)
first :: !(a -> b) !(!a, !c) -> (!b, !c)
first f (x,y) = (f x, y)

//////////////////////////////////////////////////////////////////////
//  Union.
//////////////////////////////////////////////////////////////////////
// | The union of a list of maps:
//   (@'unions' == 'Prelude.foldl` 'union' 'newMap`@).
//
// > unions [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
// >     == fromList [(3, "b"), (5, "a"), (7, "C")]
// > unions [(fromList [(5, "A3"), (3, "B3")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "a"), (3, "b")])]
// >     == fromList [(3, "B3"), (5, "A3"), (7, "C")]

//unions :: ![Map k a] -> Map k a | < k

// | The union of a list of maps, with a combining operation:
//   (@'unionsWith' f == 'Prelude.foldl` ('unionWith' f) 'newMap`@).
//
// > unionsWith (++) [(fromList [(5, "a"), (3, "b")]), (fromList [(5, "A"), (7, "C")]), (fromList [(5, "A3"), (3, "B3")])]
// >     == fromList [(3, "bB3"), (5, "aAA3"), (7, "C")]

//unionsWith :: !(a a -> a) ![Map k a] -> Map k a | < k

// | /O(n+m)/.
// The expression (@'union' t1 t2@) takes the left-biased union of @t1@ and @t2@.
// It prefers @t1@ when duplicate keys are encountered,
// i.e. (@'union' == 'unionWith' 'const`=:).
// The implementation uses the efficient /hedge-union/ algorithm.
//
// > union (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "a"), (7, "C")]

union :: !(Map k a) !(Map k a) -> Map k a | < k
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = hedgeUnion Nothing Nothing t1 t2

unions :: ![Map k a] -> Map k a | < k
unions ts = foldl union newMap ts

unionsWith :: !(a a -> a) ![Map k a] -> Map k a | < k
unionsWith f ts = foldl (unionWith f) newMap ts

unionWith :: !(a a -> a) !(Map k a) !(Map k a) -> Map k a | < k
unionWith f m1 m2 = unionWithKey (appUnion f) m1 m2
  where
  appUnion :: !(a a -> a) k !a !a -> a
  appUnion f _ x y = f x y

unionWithKey :: !(k a a -> a) !(Map k a) !(Map k a) -> Map k a | < k
unionWithKey f t1 t2 = mergeWithKey (appUnion f) id id t1 t2
  where
  appUnion :: !(k a a -> a) !k !a !a -> Maybe a
  appUnion f k x y = Just (f k x y)

// left-biased hedge union
hedgeUnion :: !(Maybe a) !(Maybe a) !(Map a b) !(Map a b) -> Map a b | < a
hedgeUnion _   _   t1  Tip = t1
hedgeUnion blo bhi Tip (Bin _ kx x l r) = link kx x (filterGt blo l) (filterLt bhi r)
hedgeUnion _   _   t1  (Bin _ kx x Tip Tip) = putR kx x t1  // According to benchmarks, this special case increases
                                                              // performance up to 30%. It does not help in difference or intersection.
hedgeUnion blo bhi (Bin _ kx x l r) t2
  #! bmi = Just kx
  = link kx x (hedgeUnion blo bmi l (trim blo bmi t2))
              (hedgeUnion bmi bhi r (trim bmi bhi t2))
hedgeUnion _ _ _ _ = abort "error in hedgeUnion\n"

//////////////////////////////////////////////////////////////////////
//  Union with a combining function
//////////////////////////////////////////////////////////////////////
// | /O(n+m)/. Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
//
// > unionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "aA"), (7, "C")]

//unionWith :: !(a a -> a) !(Map k a) !(Map k a) -> Map k a | < k

// | /O(n+m)/.
// Union with a combining function. The implementation uses the efficient /hedge-union/ algorithm.
//
// > let f key left_value right_value = (show key) ++ ":" ++ left_value ++ "|" ++ right_value
// > unionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == fromList [(3, "b"), (5, "5:a|A"), (7, "C")]

//unionWithKey :: !(k a a -> a) !(Map k a) !(Map k a) -> Map k a | < k

//////////////////////////////////////////////////////////////////////
//  Difference
//////////////////////////////////////////////////////////////////////
// | /O(n+m)/. Difference of two maps.
// Return elements of the first map not existing in the second map.
// The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
//
// > difference (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 3 "b"

difference :: !(Map k a) !(Map k b) -> Map k a | < k
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff Nothing Nothing t1 t2

hedgeDiff :: !(Maybe a) !(Maybe a) !(Map a b) !(Map a c) -> Map a b | < a
hedgeDiff _   _   Tip              _ = Tip
hedgeDiff blo bhi (Bin _ kx x l r) Tip = link kx x (filterGt blo l) (filterLt bhi r)
hedgeDiff blo bhi t (Bin _ kx _ l r)
  #! bmi = Just kx
  = merge (hedgeDiff blo bmi (trim blo bmi t) l) (hedgeDiff bmi bhi (trim bmi bhi t) r)
hedgeDiff _ _ _ _ = abort "error in hedgeDiff\n"

// | /O(n+m)/. Difference with a combining function.
// When two equal keys are
// encountered, the combining function is applied to the values of these keys.
// If it returns 'Nothing', the element is discarded (proper set difference). If
// it returns (@'Just` y@), the element is updated with a new value @y@.
// The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
//
// > let f al ar = if al == "b" then Just (al ++ ":" ++ ar) else Nothing
// > differenceWith f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (7, "C")])
// >     == singleton 3 "b:B"

differenceWith :: !(a b -> Maybe a) !(Map k a) !(Map k b) -> Map k a | < k
differenceWith f m1 m2
  = differenceWithKey (\_ x y -> f x y) m1 m2

// | /O(n+m)/. Difference with a combining function. When two equal keys are
// encountered, the combining function is applied to the key and both values.
// If it returns 'Nothing', the element is discarded (proper set difference). If
// it returns (@'Just` y@), the element is updated with a new value @y@.
// The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
//
// > let f k al ar = if al == "b" then Just ((show k) ++ ":" ++ al ++ "|" ++ ar) else Nothing
// > differenceWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (3, "B"), (10, "C")])
// >     == singleton 3 "3:b|B"

differenceWithKey :: !(k a b -> Maybe a) !(Map k a) !(Map k b) -> Map k a | < k
differenceWithKey f t1 t2 = mergeWithKey f id (const Tip) t1 t2


//////////////////////////////////////////////////////////////////////
//  Intersection
//////////////////////////////////////////////////////////////////////
// | /O(n+m)/. Intersection of two maps.
// Return data in the first map for the keys existing in both maps.
// (@'intersection' m1 m2 == 'intersectionWith' 'const` m1 m2@).
// The implementation uses an efficient /hedge/ algorithm comparable with
// /hedge-union/.
//
// > intersection (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "a"

intersection :: !(Map k a) !(Map k b) -> Map k a | < k
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1 t2 = hedgeInt Nothing Nothing t1 t2

hedgeInt :: !(Maybe k) !(Maybe k) !(Map k a) !(Map k b) -> Map k a | < k
hedgeInt _ _ _   Tip = Tip
hedgeInt _ _ Tip _   = Tip
hedgeInt blo bhi (Bin _ kx x l r) t2
  #! bmi = Just kx
  #! l` = hedgeInt blo bmi l (trim blo bmi t2)
  #! r` = hedgeInt bmi bhi r (trim bmi bhi t2)
  | member kx t2 = link kx x l` r`
  | otherwise    = merge l` r`

// | /O(n+m)/. Intersection with a combining function.  The implementation uses
// an efficient /hedge/ algorithm comparable with /hedge-union/.
//
// > intersectionWith (++) (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "aA"

intersectionWith :: !(a b -> c) !(Map k a) !(Map k b) -> Map k c | < k
intersectionWith f m1 m2
  = intersectionWithKey (\_ x y -> f x y) m1 m2

// | /O(n+m)/. Intersection with a combining function.  The implementation uses
// an efficient /hedge/ algorithm comparable with /hedge-union/.
//
// > let f k al ar = (show k) ++ ":" ++ al ++ "|" ++ ar
// > intersectionWithKey f (fromList [(5, "a"), (3, "b")]) (fromList [(5, "A"), (7, "C")]) == singleton 5 "5:a|A"


intersectionWithKey :: !(k a b -> c) !(Map k a) !(Map k b) -> Map k c | < k
intersectionWithKey f t1 t2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const Tip) (const Tip) t1 t2


//////////////////////////////////////////////////////////////////////
//  MergeWithKey
//////////////////////////////////////////////////////////////////////

// | /O(n+m)/. A high-performance universal combining function. This function
// is used to define 'unionWith', 'unionWithKey`, 'differenceWith',
// 'differenceWithKey`, 'intersectionWith', 'intersectionWithKey` and can be
// used to define other custom combine functions.
//
// Please make sure you know what is going on when using 'mergeWithKey`,
// otherwise you can be surprised by unexpected code growth or even
// corruption of the data structure.
//
// When 'mergeWithKey` is given three arguments, it is inlined to the call
// site. You should therefore use 'mergeWithKey` only to define your custom
// combining functions. For example, you could define 'unionWithKey`,
// 'differenceWithKey` and 'intersectionWithKey` as
//
// > myUnionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) id id m1 m2
// > myDifferenceWithKey f m1 m2 = mergeWithKey f id (const newMap) m1 m2
// > myIntersectionWithKey f m1 m2 = mergeWithKey (\k x1 x2 -> Just (f k x1 x2)) (const newMap) (const newMap) m1 m2
//
// When calling @'mergeWithKey` combine only1 only2@, a function combining two
// 'IntMap's is created, such that
//
// * if a key is present in both maps, it is passed with both corresponding
//   values to the @combine@ function. Depending on the result, the key is either
//   present in the result with specified value, or is left out;
//
// * a nonnewMap subtree present only in the first map is passed to @only1@ and
//   the output is added to the result;
//
// * a nonnewMap subtree present only in the second map is passed to @only2@ and
//   the output is added to the result.
//
// The @only1@ and @only2@ methods /must return a map with a subset (possibly newMap) of the keys of the given map/.
// The values can be modified arbitrarily. Most common variants of @only1@ and
// @only2@ are 'id' and @'const` 'newMap`@, but for example @'map' f@ or
// @'filterWithKey` f@ could be used for any @f@.

mergeWithKey :: !(k a b -> Maybe c) !((Map k a) -> Map k c) !((Map k b) -> Map k c)
             !(Map k a) !(Map k b) -> Map k c | < k
mergeWithKey f g1 g2 Tip t2 = g2 t2
mergeWithKey f g1 g2 t1 Tip = g1 t1
mergeWithKey f g1 g2 t1 t2 = hedgeMerge f g1 g2 Nothing Nothing t1 t2

hedgeMerge :: !(a b c -> Maybe d) !((Map a b) -> Map a d) !((Map a c) -> Map a d)
              !(Maybe a) !(Maybe a) !(Map a b) !(Map a c) -> Map a d | < a
hedgeMerge f g1 g2 _   _   t1  Tip = g1 t1
hedgeMerge f g1 g2 blo bhi Tip (Bin _ kx x l r) = g2 (link kx x (filterGt blo l) (filterLt bhi r))
hedgeMerge f g1 g2 blo bhi (Bin _ kx x l r) t2
  #! bmi              = Just kx
  #! l`               = hedgeMerge f g1 g2 blo bmi l (trim blo bmi t2)
  #! (found, trim_t2) = trimLookupLo kx bhi t2
  #! r`               = hedgeMerge f g1 g2 bmi bhi r trim_t2
  = case found of
      Nothing -> case g1 (singleton kx x) of
                   Tip -> merge l` r`
                   (Bin _ _ x` Tip Tip) -> link kx x` l` r`
                   _ -> abort "mergeWithKey: Given function only1 does not fulfil required conditions (see documentation)"
      Just x2 -> case f kx x x2 of
                   Nothing -> merge l` r`
                   Just x` -> link kx x` l` r`
hedgeMerge _ _ _ _ _ _ _ = abort "error in hedgeMerge\n"

//////////////////////////////////////////////////////////////////////
//  Submap
//////////////////////////////////////////////////////////////////////
// | /O(n+m)/.
// This function is defined as (@'isSubmapOf' = 'isSubmapOfBy` (==)@).
//
isSubmapOf :: !(Map k a) !(Map k a) -> Bool | < k & Eq a
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2

/* | /O(n+m)/.
 The expression (@'isSubmapOfBy` f t1 t2@) returns 'True' if
 all keys in @t1@ are in tree @t2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

 > isSubmapOfBy (==) (fromList [('a`,1)]) (fromList [('a`,1),('b`,2)])
 > isSubmapOfBy (<=) (fromList [('a`,1)]) (fromList [('a`,1),('b`,2)])
 > isSubmapOfBy (==) (fromList [('a`,1),('b`,2)]) (fromList [('a`,1),('b`,2)])

 But the following are all 'False':

 > isSubmapOfBy (==) (fromList [('a`,2)]) (fromList [('a`,1),('b`,2)])
 > isSubmapOfBy (<)  (fromList [('a`,1)]) (fromList [('a`,1),('b`,2)])
 > isSubmapOfBy (==) (fromList [('a`,1),('b`,2)]) (fromList [('a`,1)])

*/
isSubmapOfBy :: !(a b -> Bool) !(Map k a) !(Map k b) -> Bool | < k
isSubmapOfBy f t1 t2
  = (mapSize t1 <= mapSize t2) && (submap` f t1 t2)

submap` :: !(b c -> Bool) !(Map a b) !(Map a c) -> Bool | < a
submap` _ Tip _ = True
submap` _ _ Tip = False
submap` f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y  -> f x y && submap` f l lt && submap` f r gt
  where
    (lt,found,gt) = splitLookup kx t
submap` _ _ _ = abort "error in submap`\n"

// | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
// Defined as (@'isProperSubmapOf' = 'isProperSubmapOfBy` (==)@).
isProperSubmapOf :: !(Map k a) !(Map k a) -> Bool | < k & Eq a
isProperSubmapOf m1 m2
  = isProperSubmapOfBy (==) m1 m2

/* | /O(n+m)/. Is this a proper submap? (ie. a submap but not equal).
 The expression (@'isProperSubmapOfBy` f m1 m2@) returns 'True' when
 @m1@ and @m2@ are not equal,
 all keys in @m1@ are in @m2@, and when @f@ returns 'True' when
 applied to their respective values. For example, the following
 expressions are all 'True':

  > isProperSubmapOfBy (==) (fromList [(1,1)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (<=) (fromList [(1,1)]) (fromList [(1,1),(2,2)])

 But the following are all 'False':

  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1),(2,2)])
  > isProperSubmapOfBy (==) (fromList [(1,1),(2,2)]) (fromList [(1,1)])
  > isProperSubmapOfBy (<)  (fromList [(1,1)])       (fromList [(1,1),(2,2)])


*/
isProperSubmapOfBy :: !(a b -> Bool) !(Map k a) !(Map k b) -> Bool | < k
isProperSubmapOfBy f t1 t2
  = (mapSize t1 < mapSize t2) && (submap` f t1 t2)

//////////////////////////////////////////////////////////////////////
//  Filter and partition
//////////////////////////////////////////////////////////////////////
// | /O(n)/. Filter all values that satisfy the predicate.
//
// > filter (> "a") (fromList [(5,"a"), (3,"b")]) == singleton 3 "b"
// > filter (> "x") (fromList [(5,"a"), (3,"b")]) == newMap
// > filter (< "a") (fromList [(5,"a"), (3,"b")]) == newMap

filter :: !(a -> Bool) !(Map k a) -> Map k a
filter p m = filterWithKey (\_ x -> p x) m

// | /O(n)/. Filter all keys\/values that satisfy the predicate.
//
// > filterWithKey (\k _ -> k > 4) (fromList [(5,"a"), (3,"b")]) == singleton 5 "a"

// TODO : Optimize?
filterWithKey :: !(k a -> Bool) !(Map k a) -> Map k a
filterWithKey _ Tip = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x    = link kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)

// | /O(n)/. Partition the map according to a predicate. The first
// map contains all elements that satisfy the predicate, the second all
// elements that fail the predicate. See also 'split`.
//
// > partition (> "a") (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
// > partition (< "x") (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], newMap)
// > partition (> "x") (fromList [(5,"a"), (3,"b")]) == (newMap, fromList [(3, "b"), (5, "a")])

partition :: !(a -> Bool) !(Map k a) -> (!Map k a, !Map k a)
partition p m
  = partitionWithKey (\_ x -> p x) m

// | /O(n)/. Partition the map according to a predicate. The first
// map contains all elements that satisfy the predicate, the second all
// elements that fail the predicate. See also 'split`.
//
// > partitionWithKey (\ k _ -> k > 3) (fromList [(5,"a"), (3,"b")]) == (singleton 5 "a", singleton 3 "b")
// > partitionWithKey (\ k _ -> k < 7) (fromList [(5,"a"), (3,"b")]) == (fromList [(3, "b"), (5, "a")], newMap)
// > partitionWithKey (\ k _ -> k > 7) (fromList [(5,"a"), (3,"b")]) == (newMap, fromList [(3, "b"), (5, "a")])

partitionWithKey :: !(k a -> Bool) !(Map k a) -> (!Map k a, !Map k a)
partitionWithKey _ Tip = (Tip, Tip)
partitionWithKey p (Bin _ kx x l r)
  #! (l1, l2) = partitionWithKey p l
  #! (r1, r2) = partitionWithKey p r
  | p kx x    = (link kx x l1 r1, merge l2 r2)
  | otherwise = (merge l1 r1, link kx x l2 r2)

// | /O(n)/. Map values and collect the 'Just` results.
//
// > let f x = if x == "a" then Just "new a" else Nothing
// > mapMaybe f (fromList [(5,"a"), (3,"b")]) == singleton 5 "new a"

mapMaybe :: !(a -> Maybe b) !(Map k a) -> Map k b
mapMaybe f m = mapMaybeWithKey (\_ x -> f x) m

// | /O(n)/. Map keys\/values and collect the 'Just` results.
//
// > let f k _ = if k < 5 then Just ("key : " ++ (show k)) else Nothing
// > mapMaybeWithKey f (fromList [(5,"a"), (3,"b")]) == singleton 3 "key : 3"

mapMaybeWithKey :: !(k a -> Maybe b) !(Map k a) -> Map k b
mapMaybeWithKey _ Tip = Tip
mapMaybeWithKey f (Bin _ kx x l r) = case f kx x of
                                       Just y  -> link kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
                                       Nothing -> merge (mapMaybeWithKey f l) (mapMaybeWithKey f r)

// | /O(n)/. Map values and separate the 'Left` and 'Right` results.
//
// > let f a = if a < "c" then Left a else Right a
// > mapEither f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (fromList [(3,"b"), (5,"a")], fromList [(1,"x"), (7,"z")])
// >
// > mapEither (\ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (newMap, fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])

mapEither :: !(a -> Either b c) !(Map k a) -> (!Map k b, !Map k c)
mapEither f m = mapEitherWithKey (\_ x -> f x) m

// | /O(n)/. Map keys\/values and separate the 'Left` and 'Right` results.
//
// > let f k a = if k < 5 then Left (k * 2) else Right (a ++ a)
// > mapEitherWithKey f (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (fromList [(1,2), (3,6)], fromList [(5,"aa"), (7,"zz")])
// >
// > mapEitherWithKey (\_ a -> Right a) (fromList [(5,"a"), (3,"b"), (1,"x"), (7,"z")])
// >     == (newMap, fromList [(1,"x"), (3,"b"), (5,"a"), (7,"z")])

mapEitherWithKey :: !(k a -> Either b c) !(Map k a) -> (!Map k b, !Map k c)
mapEitherWithKey _ Tip = (Tip, Tip)
mapEitherWithKey f (Bin _ kx x l r)
  #! (l1, l2) = mapEitherWithKey f l
  #! (r1, r2) = mapEitherWithKey f r
  = case f kx x of
      Left y  -> (link kx y l1 r1, merge l2 r2)
      Right z -> (merge l1 r1, link kx z l2 r2)

//////////////////////////////////////////////////////////////////////
//  Mapping
//////////////////////////////////////////////////////////////////////
// | /O(n)/. Map a function over all values in the map.
//
// > map (++ "x") (fromList [(5,"a"), (3,"b")]) == fromList [(3, "bx"), (5, "ax")]

map :: !(a -> b) !(Map k a) -> Map k b
map _ Tip = Tip
map f (Bin sx kx x l r) = Bin sx kx (f x) (map f l) (map f r)

// | /O(n)/. Map a function over all values in the map.
//
// > let f key x = (show key) ++ ":" ++ x
// > mapWithKey f (fromList [(5,"a"), (3,"b")]) == fromList [(3, "3:b"), (5, "5:a")]

mapWithKey :: !(k a -> b) !(Map k a) -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin sx kx x l r) = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

// | /O(n)/.
// @'traverseWithKey` f s == 'fromList` <$> 'traverse' (\(k, v) -> (,) k <$> f k v) ('toList` m)@
// That is, behaves exactly like a regular 'traverse' except that the traversing
// function also has access to the key associated with a value.
//
// > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(1, 'a`), (5, 'e')]) == Just (fromList [(1, 'b`), (5, 'f')])
// > traverseWithKey (\k v -> if odd k then Just (succ v) else Nothing) (fromList [(2, 'c')])           == Nothing
traverseWithKey :: !(k a -> t b) !(Map k a) -> t (Map k b) | Applicative t
traverseWithKey _ Tip = pure Tip
traverseWithKey f (Bin 1 k v _ _) = (\v` -> Bin 1 k v` Tip Tip) <$> f k v
traverseWithKey f (Bin s k v l r) = flip (Bin s k) <$> traverseWithKey f l <*> f k v <*> traverseWithKey f r

// | /O(n)/. The function 'mapAccum' threads an accumulating
// argument through the map in ascending order of keys.
//
// > let f a b = (a ++ b, b ++ "X")
// > mapAccum f "Everything: " (fromList [(5,"a"), (3,"b")]) == ("Everything: ba", fromList [(3, "bX"), (5, "aX")])

mapAccum :: !(a b -> (a, c)) !a !(Map k b) -> (!a, !Map k c)
mapAccum f a m = mapAccumWithKey (\a` _ x` -> f a` x`) a m

// | /O(n)/. The function 'mapAccumWithKey` threads an accumulating
// argument through the map in ascending order of keys.
//
// > let f a k b = (a ++ " " ++ (show k) ++ "-" ++ b, b ++ "X")
// > mapAccumWithKey f "Everything:" (fromList [(5,"a"), (3,"b")]) == ("Everything: 3-b 5-a", fromList [(3, "bX"), (5, "aX")])

mapAccumWithKey :: !(a k b -> (a, c)) !a !(Map k b) -> (!a, !Map k c)
mapAccumWithKey f a t = mapAccumL f a t

// | /O(n)/. The function 'mapAccumL' threads an accumulating
// argument through the map in ascending order of keys.
mapAccumL :: !(a k b -> (a, c)) !a !(Map k b) -> (!a, !Map k c)
mapAccumL _ a Tip               = (a,Tip)
mapAccumL f a (Bin sx kx x l r)
  #! (a1,l`) = mapAccumL f a l
  #! (a2,x`) = f a1 kx x
  #! (a3,r`) = mapAccumL f a2 r
  = (a3, Bin sx kx x` l` r`)

// | /O(n)/. The function 'mapAccumR' threads an accumulating
// argument through the map in descending order of keys.
mapAccumRWithKey :: !(a k b -> (a, c)) !a !(Map k b) -> (!a, !Map k c)
mapAccumRWithKey _ a Tip = (a,Tip)
mapAccumRWithKey f a (Bin sx kx x l r)
  #! (a1,r`) = mapAccumRWithKey f a r
  #! (a2,x`) = f a1 kx x
  #! (a3,l`) = mapAccumRWithKey f a2 l
  = (a3, Bin sx kx x` l` r`)

// | /O(n*log n)/.
// @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
//
// The mapSize of the result may be smaller if @f@ maps two or more distinct
// keys to the same new key.  In this case the value at the greatest of the
// original keys is retained.
//
// > mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])                        == fromList [(4, "b"), (6, "a")]
// > mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "c"
// > mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "c"

mapKeys :: !(k1 -> k2) !(Map k1 a) -> Map k2 a | < k1 & < k2 & == k1 & == k2
mapKeys f m = fromList (foldrWithKey (\k x xs -> [(f k, x) : xs]) [] m)

// | /O(n*log n)/.
// @'mapKeysWith' c f s@ is the map obtained by applying @f@ to each key of @s@.
//
// The mapSize of the result may be smaller if @f@ maps two or more distinct
// keys to the same new key.  In this case the associated values will be
// combined using @c@.
//
// > mapKeysWith (++) (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 1 "cdab"
// > mapKeysWith (++) (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")]) == singleton 3 "cdab"

mapKeysWith :: !(a a -> a) !(k1 -> k2) !(Map k1 a) -> Map k2 a | < k1 & < k2
mapKeysWith c f m = fromListWith c (foldrWithKey (\k x xs -> [(f k, x) : xs]) [] m)


// | /O(n)/.
// @'mapKeysMonotonic' f s == 'mapKeys' f s@, but works only when @f@
// is strictly monotonic.
// That is, for any values @x@ and @y@, if @x@ < @y@ then @f x@ < @f y@.
// /The precondition is not checked./
// Semi-formally, we have:
//
// > and [x < y ==> f x < f y | x <- ls, y <- ls]
// >                     ==> mapKeysMonotonic f s == mapKeys f s
// >     where ls = keys s
//
// This means that @f@ maps distinct original keys to distinct resulting keys.
// This function has better performance than 'mapKeys'.
//
// > mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")]) == fromList [(6, "b"), (10, "a")]
// > valid (mapKeysMonotonic (\ k -> k * 2) (fromList [(5,"a"), (3,"b")])) == True
// > valid (mapKeysMonotonic (\ _ -> 1)     (fromList [(5,"a"), (3,"b")])) == False

mapKeysMonotonic :: !(k1 -> k2) !(Map k1 a) -> Map k2 a
mapKeysMonotonic _ Tip = Tip
mapKeysMonotonic f (Bin sz k x l r) =
    Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

//////////////////////////////////////////////////////////////////////
//  Folds
//////////////////////////////////////////////////////////////////////

// | /O(n)/. Fold the values in the map using the given right-associative
// binary operator, such that @'foldr` f z == 'Prelude.foldr` f z . 'elems'@.
//
// For example,
//
// > elems map = foldr (:) [] map
//
// > let f a len = len + (length a)
// > foldr f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
foldr :: !(a b -> b) !b (Map k a) -> b
foldr f z` Tip             = z`
foldr f z` (Bin _ _ x l r) = foldr f (f x (foldr f z` r)) l

// | /O(n)/. A strict version of 'foldr`. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldr` :: !(a b -> b) !b !(Map k a) -> b
foldr` f z` Tip             = z`
foldr` f z` (Bin _ _ x l r) = foldr` f (f x (foldr` f z` r)) l

// | /O(n)/. Fold the values in the map using the given left-associative
// binary operator, such that @'foldl` f z == 'Prelude.foldl` f z . 'elems'@.
//
// For example,
//
// > elems = reverse . foldl (flip (:)) []
//
// > let f len a = len + (length a)
// > foldl f 0 (fromList [(5,"a"), (3,"bbb")]) == 4
/*foldl :: !(a b -> a) !a !(Map k b) -> a
foldl f z` Tip             = z`
foldl f z` (Bin _ _ x l r) = foldl f (f (foldl f z` l) x) r*/

// | /O(n)/. A strict version of 'foldl`. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldl` :: !(a b -> a) !a (Map k b) -> a
foldl` f z` Tip             = z`
foldl` f z` (Bin _ _ x l r) = foldl` f (f (foldl` f z` l) x) r

// | /O(n)/. Fold the keys and values in the map using the given right-associative
// binary operator, such that
// @'foldrWithKey` f z == 'Prelude.foldr` ('uncurry` f) z . 'toAscList`=:.
//
// For example,
//
// > keys map = foldrWithKey (\k x ks -> k:ks) [] map
//
// > let f k a result = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
// > foldrWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (5:a)(3:b)"
foldrWithKey :: !(k v u:a -> u:a) !u:a !(Map k v) -> u:a
foldrWithKey f z` Tip              = z`
foldrWithKey f z` (Bin _ kx x l r) = foldrWithKey f (f kx x (foldrWithKey f z` r)) l

// | /O(n)/. A strict version of 'foldrWithKey`. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldrWithKey` :: !(k a b -> b) !b !(Map k a) -> b
foldrWithKey` f z` Tip              = z`
foldrWithKey` f z` (Bin _ kx x l r) = foldrWithKey` f (f kx x (foldrWithKey` f z` r)) l

// | /O(n)/. Fold the keys and values in the map using the given left-associative
// binary operator, such that
// @'foldlWithKey` f z == 'Prelude.foldl` (\\z` (kx, x) -> f z` kx x) z . 'toAscList`=:.
//
// For example,
//
// > keys = reverse . foldlWithKey (\ks k x -> k:ks) []
//
// > let f result k a = result ++ "(" ++ (show k) ++ ":" ++ a ++ ")"
// > foldlWithKey f "Map: " (fromList [(5,"a"), (3,"b")]) == "Map: (3:b)(5:a)"
foldlWithKey :: !(.a -> .(k -> .(v -> .a))) !.a !(Map k v) -> .a
foldlWithKey f z` Tip              = z`
foldlWithKey f z` (Bin _ kx x l r) = foldlWithKey f (f (foldlWithKey f z` l) kx x) r

// | /O(n)/. A strict version of 'foldlWithKey`. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldlWithKey` :: !(a k b -> a) !a !(Map k b) -> a
foldlWithKey` f z` Tip              = z`
foldlWithKey` f z` (Bin _ kx x l r) = foldlWithKey` f (f (foldlWithKey` f z` l) kx x) r

// | /O(n)/. Fold the keys and values in the map using the given monoid, such that
//
// @'foldMapWithKey` f = 'Prelude.fold' . 'mapWithKey` f@
//
// This can be an asymptotically faster than 'foldrWithKey` or 'foldlWithKey` for some monoids.
foldMapWithKey :: !(k a -> m) !(Map k a) -> m | Monoid m
foldMapWithKey f Tip             = mempty
foldMapWithKey f (Bin 1 k v _ _) = f k v
foldMapWithKey f (Bin _ k v l r) = mappend (foldMapWithKey f l) (mappend (f k v) (foldMapWithKey f r))

//////////////////////////////////////////////////////////////////////
//  List variations
//////////////////////////////////////////////////////////////////////
// | /O(n)/.
// Return all elements of the map in the ascending order of their keys.
// Subject to list fusion.
//
// > elems (fromList [(5,"a"), (3,"b")]) == ["b","a"]
// > elems newMap == []

//elems :: !(Map k a) -> [a]

// | /O(n)/. Return all keys of the map in ascending order. Subject to list
// fusion.
//
// > keys (fromList [(5,"a"), (3,"b")]) == [3,5]
// > keys newMap == []

//keys :: !(Map k a) -> [k]

// | /O(n)/. An alias for 'toAscList`. Return all key\/value pairs in the map
// in ascending key order. Subject to list fusion.
//
// > assocs (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
// > assocs newMap == []

assocs :: !(Map k a) -> [(k, a)]
assocs m = toAscList m

keysSet :: !(Map k a) -> Set k
keysSet Tip = 'Data.Set'.Tip
keysSet (Bin sz kx _ l r) = 'Data.Set'.Bin sz kx (keysSet l) (keysSet r)

fromSet :: !(k -> a) !(Set k) -> Map k a
fromSet _ 'Data.Set'.Tip            = Tip
fromSet f ('Data.Set'.Bin sz x l r) = Bin sz x (f x) (fromSet f l) (fromSet f r)

fromList :: !u:[v:(a, b)] -> Map a b | == a & < a, [u <= v]
fromList [] = Tip
fromList [(kx, x)] = Bin 1 kx x Tip Tip
fromList [(kx0, x0) : xs0]
  | not_ordered kx0 xs0 = fromList` (Bin 1 kx0 x0 Tip Tip) xs0
  | otherwise = go 1 (Bin 1 kx0 x0 Tip Tip) xs0
  where
    not_ordered :: !a !u:[v:(a, b)] -> Bool | == a & < a, [u <= v]
    not_ordered _ [] = False
    not_ordered kx [(ky,_) : _] = kx >= ky

    fromList` :: !(Map a b) !u:[v:(a, b)] -> Map a b | == a & < a, [u <= v]
    fromList` t0 xs = foldl ins t0 xs
      where ins t (k,x) = put k x t

    go :: !Int !(Map a b) !u:[v:(a, b)] -> Map a b | == a & < a, [u <= v]
    go _ t [] = t
    go _ t [(kx, x)] = putMax kx x t
    go s l xs=:[(kx, x) : xss]
      | not_ordered kx xss = fromList` l xs
      | otherwise = case create s xss of
                      (r, ys, []) -> go (s << 1) (link kx x l r) ys
                      (r, _,  ys) -> fromList` (link kx x l r) ys

    // The create is returning a triple (tree, xs, ys). Both xs and ys
    // represent not yet processed elements and only one of them can be nonnewMap.
    // If ys is nonnewMap, the keys in ys are not ordered with respect to tree
    // and must be puted using fromList`. Otherwise the keys have been
    // ordered so far.
    create :: !Int !u:[v:(a, b)] -> (!Map a b, ![(a, b)], ![(a, b)]) | == a & < a, [u <= v]
    create _ [] = (Tip, [], [])
    create s xs=:[xp : xss]
      | s == 1 = case xp of (kx, x) | not_ordered kx xss -> (Bin 1 kx x Tip Tip, [], xss)
                                    | otherwise -> (Bin 1 kx x Tip Tip, xss, [])
      | otherwise = case create (s >> 1) xs of
                      res=:(_, [], _) -> res
                      (l, [(ky, y)], zs) -> (putMax ky y l, [], zs)
                      (l, ys=:[(ky, y):yss], _) | not_ordered ky yss -> (l, [], ys)
                                               | otherwise -> case create (s >> 1) yss of
                                                   (r, zs, ws) -> (link ky y l r, zs, ws)

// | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWith'.
//
// > fromListWith (++) [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "ab"), (5, "aba")]
// > fromListWith (++) [] == newMap

//fromListWith :: !(a a -> a) ![(k, a)] -> Map k a | < k
fromListWith f xs :== fromListWithKey (\_ x y -> f x y) xs

// | /O(n*log n)/. Build a map from a list of key\/value pairs with a combining function. See also 'fromAscListWithKey`.
//
// > let f k a1 a2 = (toString k) ++ a1 ++ a2
// > fromListWithKey f [(5,"a"), (5,"b"), (3,"b"), (3,"a"), (5,"a")] == fromList [(3, "3ab"), (5, "5a5ba")]
// > fromListWithKey f [] == newMap

fromListWithKey :: !(k a a -> a) ![(k, a)] -> Map k a | < k
fromListWithKey f xs = foldl (ins f) newMap xs
  where
  ins :: !(k a a -> a) !(Map k a) !(!k, !a) -> Map k a | < k
  ins f t (k, x) = putWithKey f k x t

// | /O(n)/. Convert the map to a list of key\/value pairs. Subject to list fusion.
//
// > toList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]
// > toList newMap == []

//toList :: !(Map k a) -> [(k, a)]
//toList m = toAscList m

// | /O(n)/. Convert the map to a list of key\/value pairs where the keys are
// in ascending order. Subject to list fusion.
//
// > toAscList (fromList [(5,"a"), (3,"b")]) == [(3,"b"), (5,"a")]

//toAscList :: !(Map k a) -> [(k, a)]

// | /O(n)/. Convert the map to a list of key\/value pairs where the keys
// are in descending order. Subject to list fusion.
//
// > toDescList (fromList [(5,"a"), (3,"b")]) == [(5,"a"), (3,"b")]

toDescList :: !(Map k a) -> [(k, a)]
toDescList m = foldlWithKey (\xs k x -> [(k,x):xs]) [] m

//////////////////////////////////////////////////////////////////////
//  Building trees from ascending/descending lists can be done in linear time.
//
//  Note that if [xs] is ascending that:
//    fromAscList xs       == fromList xs
//    fromAscListWith f xs == fromListWith f xs
//////////////////////////////////////////////////////////////////////
// | /O(n)/. Build a map from an ascending list in linear time.
// /The precondition (input list is ascending) is not checked./
//
// > fromAscList [(3,"b"), (5,"a")]          == fromList [(3, "b"), (5, "a")]
// > fromAscList [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "b")]
// > valid (fromAscList [(3,"b"), (5,"a"), (5,"b")]) == True
// > valid (fromAscList [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscList :: ![(k, a)] -> Map k a | == k
fromAscList xs = fromAscListWithKey (\_ x _ -> x) xs

// | /O(n)/. Build a map from an ascending list in linear time with a combining function for equal keys.
// /The precondition (input list is ascending) is not checked./
//
// > fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")] == fromList [(3, "b"), (5, "ba")]
// > valid (fromAscListWith (++) [(3,"b"), (5,"a"), (5,"b")]) == True
// > valid (fromAscListWith (++) [(5,"a"), (3,"b"), (5,"b")]) == False

fromAscListWith :: !(a a -> a) ![(k, a)] -> Map k a | == k
fromAscListWith f xs = fromAscListWithKey (\_ x y -> f x y) xs

// | /O(n)/. Build a map from an ascending list in linear time with a
// combining function for equal keys.
// /The precondition (input list is ascending) is not checked./
//
// > let f k a1 a2 = (toString k) ++ ":" ++ a1 ++ a2
// > fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")] == fromList [(3, "b"), (5, "5:b5:ba")]
// > valid (fromAscListWithKey f [(3,"b"), (5,"a"), (5,"b"), (5,"b")]) == True
// > valid (fromAscListWithKey f [(5,"a"), (3,"b"), (5,"b"), (5,"b")]) == False

fromAscListWithKey :: !(k a a -> a) ![(k, a)] -> Map k a | == k
fromAscListWithKey f xs = fromDistinctAscList (combineEq f xs)
  where
  // [combineEq f xs] combines equal elements with function [f] in an ordered list [xs]
  combineEq :: !(k a a -> a) ![(k, a)] -> [(k, a)] | == k
  combineEq f xs`
    = case xs` of
        []     -> []
        [x]    -> [x]
        [x:xx] -> combineEq` f x xx

  combineEq` :: !(k a a -> a)  !(!k, !a) ![(k, a)] -> [(k, a)] | == k
  combineEq` _ z [] = [z]
  combineEq` f z=:(kz,zz) [x=:(kx,xx):xs`]
    | kx == kz
      #! yy = f kx xx zz
      = combineEq` f (kx, yy) xs`
    | otherwise = [z : combineEq` f x xs`]

// | /O(n)/. Build a map from an ascending list of distinct elements in linear time.
// /The precondition is not checked./
//
// > fromDistinctAscList [(3,"b"), (5,"a")] == fromList [(3, "b"), (5, "a")]
// > valid (fromDistinctAscList [(3,"b"), (5,"a")])          == True
// > valid (fromDistinctAscList [(3,"b"), (5,"a"), (5,"b")]) == False

// For some reason, when 'singleton' is used in fromDistinctAscList or in
// create, it is not inlined, so we inline it manually.
fromDistinctAscList :: ![(k, a)] -> Map k a
fromDistinctAscList [] = Tip
fromDistinctAscList [(kx0, x0) : xs0] = go 1 (Bin 1 kx0 x0 Tip Tip) xs0
  where
  go :: !Int !(Map a b) ![(a, b)] -> Map a b
  go _ t [] = t
  go s l [(kx, x) : xs] = case create s xs of
                            (r, ys) -> go (s << 1) (link kx x l r) ys

  create :: !Int ![(a, b)] -> (!Map a b, ![(a, b)])
  create _ [] = (Tip, [])
  create s xs=:[x` : xs`]
    | s == 1 = case x` of (kx, x) -> (Bin 1 kx x Tip Tip, xs`)
    | otherwise = case create (s >> 1) xs of
                    res=:(_, []) -> res
                    (l, [(ky, y):ys]) -> case create (s >> 1) ys of
                      (r, zs) -> (link ky y l r, zs)


//////////////////////////////////////////////////////////////////////
//  Utility functions that return sub-ranges of the original
//  tree. Some functions take a `Maybe value` as an argument to
//  allow comparisons against infinite values. These are called `blow`
//  (Nothing is -\infty) and `bhigh` (here Nothing is +\infty).
//  We use Maybe value, which is a Maybe strict in the Just case.
//
//  [trim blow bhigh t]   A tree that is either newMap or where [x > blow]
//                        and [x < bhigh] for the value [x] of the root.
//  [filterGt blow t]     A tree where for all values [k]. [k > blow]
//  [filterLt bhigh t]    A tree where for all values [k]. [k < bhigh]
//
//  [split k t]           Returns two trees [l] and [r] where all keys
//                        in [l] are <[k] and all keys in [r] are >[k].
//  [splitLookup k t]     Just like [split] but also returns whether [k]
//                        was found in the tree.
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
//  [trim blo bhi t] trims away all subtrees that surely contain no
//  values between the range [blo] to [bhi]. The returned tree is either
//  newMap or the key of the root is between @blo@ and @bhi@.
//////////////////////////////////////////////////////////////////////
trim :: !(Maybe k) !(Maybe k) !(Map k a) -> Map k a | < k
trim Nothing   Nothing   t = t
trim (Just lk) Nothing   t = greater lk t
  where
  greater :: !k !(Map k a) -> Map k a | < k
  greater lo (Bin _ k _ _ r) | k <= lo = greater lo r
  greater _  t` = t`
trim Nothing   (Just hk) t = lesser hk t
  where
  lesser :: !k !(Map k a) -> Map k a | < k
  lesser hi (Bin _ k _ l _) | k >= hi = lesser hi l
  lesser _  t` = t`
trim (Just lk) (Just hk) t = middle lk hk t
  where
  middle :: !k !k !(Map k a) -> Map k a | < k
  middle lo hi (Bin _ k _ _ r) | k <= lo = middle lo hi r
  middle lo hi (Bin _ k _ l _) | k >= hi = middle lo hi l
  middle _  _  t` = t`

// Helper function for 'mergeWithKey`. The @'trimLookupLo' lk hk t@ performs both
// @'trim' (Just lk) hk t@ and @'get' lk t@.

trimLookupLo :: !k !(Maybe k) !(Map k a) -> (!Maybe a, !Map k a) | < k
trimLookupLo lk Nothing t = greater lk t
      where greater :: !k !(Map k a) -> (!Maybe a, !Map k a) | < k
            greater lo t`=:(Bin _ kx x l r) =
              if (lo < kx)
                (get lo l, t`)
                (if (lo > kx)
                   (greater lo r)
                   (Just x, r))
            greater _ Tip = (Nothing, Tip)
trimLookupLo lk (Just hk) t = middle lk hk t
      where middle :: !k !k !(Map k a) -> (!Maybe a, !Map k a) | < k
            middle lo hi t`=:(Bin _ kx x l r) =
              if (lo < kx)
                (if (kx < hi) (get lo l, t`) (middle lo hi l))
                (if (lo > kx)
                   (middle lo hi r)
                   (Just x, lesser hi r))
            middle _ _ Tip = (Nothing, Tip)

            lesser :: !k (Map k a) -> Map k a | < k
            lesser hi (Bin _ k _ l _) | k >= hi = lesser hi l
            lesser _ t` = t`


//////////////////////////////////////////////////////////////////////
//  [filterGt b t] filter all keys >[b] from tree [t]
//  [filterLt b t] filter all keys <[b] from tree [t]
//////////////////////////////////////////////////////////////////////
filterGt :: !(Maybe k) !(Map k v) -> Map k v | < k
filterGt Nothing t = t
filterGt (Just b) t = filter` b t
  where
  filter` :: !k !(Map k a) -> Map k a | < k
  filter` _   Tip = Tip
  filter` b` (Bin _ kx x l r) = if (b` < kx)
                                  (link kx x (filter` b` l) r)
                                  (if (b` > kx)
                                     (filter` b` r)
                                     r)

filterLt :: !(Maybe k) !(Map k v) -> Map k v | < k
filterLt Nothing t = t
filterLt (Just b) t = filter` b t
  where
  filter` :: !k !(Map k a) -> Map k a | < k
  filter` _   Tip = Tip
  filter` b` (Bin _ kx x l r) =
    if (kx < b`)
      (link kx x l (filter` b` r))
      (if (kx > b`)
         (filter` b` l)
         l)

//////////////////////////////////////////////////////////////////////
//  Split
//////////////////////////////////////////////////////////////////////
// | /O(log n)/. The expression (@'split` k map@) is a pair @(map1,map2)@ where
// the keys in @map1@ are smaller than @k@ and the keys in @map2@ larger than @k@.
// Any key equal to @k@ is found in neither @map1@ nor @map2@.
//
// > split 2 (fromList [(5,"a"), (3,"b")]) == (newMap, fromList [(3,"b"), (5,"a")])
// > split 3 (fromList [(5,"a"), (3,"b")]) == (newMap, singleton 5 "a")
// > split 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", singleton 5 "a")
// > split 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", newMap)
// > split 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], newMap)

split :: !k !(Map k a) -> (!Map k a, !Map k a) | < k
split k Tip = (Tip, Tip)
split k (Bin _ kx x l r) = case lexOrd k kx of
                             LT
                               #! (lt, gt) = split k l
                               = (lt, link kx x gt r)
                             GT
                               #! (lt, gt) = split k r
                               = (link kx x l lt, gt)
                             EQ
                               = (l, r)

// | /O(log n)/. The expression (@'splitLookup' k map@) splits a map just
// like 'split` but also returns @'get' k map@.
//
// > splitLookup 2 (fromList [(5,"a"), (3,"b")]) == (newMap, Nothing, fromList [(3,"b"), (5,"a")])
// > splitLookup 3 (fromList [(5,"a"), (3,"b")]) == (newMap, Just "b", singleton 5 "a")
// > splitLookup 4 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Nothing, singleton 5 "a")
// > splitLookup 5 (fromList [(5,"a"), (3,"b")]) == (singleton 3 "b", Just "a", newMap)
// > splitLookup 6 (fromList [(5,"a"), (3,"b")]) == (fromList [(3,"b"), (5,"a")], Nothing, newMap)

splitLookup :: !k !(Map k a) -> (!Map k a, !Maybe a, !Map k a) | < k
splitLookup k t =
  case t of
    Tip            -> (Tip,Nothing,Tip)
    Bin _ kx x l r -> case lexOrd k kx of
      LT
        #! (lt,z,gt) = splitLookup k l
        #! gt` = link kx x gt r
        = (lt,z,gt`)
      GT
        #! (lt,z,gt) = splitLookup k r
        #! lt` = link kx x l lt
        = (lt`,z,gt)
      EQ
        = (l,Just x,r)

//////////////////////////////////////////////////////////////////////
//  Utility functions that maintain the balance properties of the tree.
//  All constructors assume that all values in [l] < [k] and all values
//  in [r] > [k], and that [l] and [r] are valid trees.
//
//  In order of sophistication:
//    [Bin sz k x l r]  The type constructor.
//    [bin k x l r]     Maintains the correct mapSize, assumes that both [l]
//                      and [r] are balanced with respect to each other.
//    [balance k x l r] Restores the balance and mapSize.
//                      Assumes that the original tree was balanced and
//                      that [l] or [r] has changed by at most one element.
//    [link k x l r]    Restores balance and mapSize.
//
//  Furthermore, we can construct a new tree from two trees. Both operations
//  assume that all values in [l] < all values in [r] and that [l] and [r]
//  are valid:
//    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
//                      [r] are already balanced with respect to each other.
//    [merge l r]       Merges two trees and restores balance.
//
//  Note: in contrast to Adam's paper, we use (<=) comparisons instead
//  of (<) comparisons in [link], [merge] and [balance].
//  Quickcheck (on [difference]) showed that this was necessary in order
//  to maintain the invariants. It is quite unsatisfactory that I haven't
//  been able to find out why this is actually the case! Fortunately, it
//  doesn't hurt to be a bit more conservative.
//////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
//  Link
//////////////////////////////////////////////////////////////////////
link :: !k !a !(Map k a) !(Map k a) -> Map k a
link kx x Tip r  = putMin kx x r
link kx x l Tip  = putMax kx x l
link kx x l=:(Bin mapSizeL ky y ly ry) r=:(Bin mapSizeR kz z lz rz)
  | delta*mapSizeL < mapSizeR  = balanceL kz z (link kx x l lz) rz
  | delta*mapSizeR < mapSizeL  = balanceR ky y ly (link kx x ry r)
  | otherwise                  = bin kx x l r
link _ _ _ _ = abort "error in link\n"


// putMin and putMax don't perform potentially expensive comparisons.
putMax :: !k !a !(Map k a) -> Map k a
putMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceR ky y l (putMax kx x r)

putMin :: !k !a !(Map k a) -> Map k a
putMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r
          -> balanceL ky y (putMin kx x l) r

////////////////////////////////////////////////////////////////////
//  [merge l r]: merges two trees.
////////////////////////////////////////////////////////////////////
merge :: !(Map k a) !(Map k a) -> Map k a
merge Tip r   = r
merge l Tip   = l
merge l=:(Bin mapSizeL kx x lx rx) r=:(Bin mapSizeR ky y ly ry)
  | delta*mapSizeL < mapSizeR = balanceL ky y (merge l ly) ry
  | delta*mapSizeR < mapSizeL = balanceR kx x lx (merge rx r)
  | otherwise                 = glue l r
merge _ _ = abort "error in merge\n"

////////////////////////////////////////////////////////////////////
//  [glue l r]: glues two trees together.
//  Assumes that [l] and [r] are already balanced with respect to each other.
////////////////////////////////////////////////////////////////////
glue :: !(Map k a) !(Map k a) -> Map k a
glue Tip r = r
glue l Tip = l
glue l r
  | mapSize l > mapSize r
      #! ((km, m), l`) = deleteFindMax l
      = balanceR km m l` r
  | otherwise
      #! ((km, m), r`) = deleteFindMin r
      = balanceL km m l r`


// | /O(log n)/. Delete and find the minimal element.
//
// > deleteFindMin (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((3,"b"), fromList[(5,"a"), (10,"c")])
// > deleteFindMin                                            Error: can not return the minimal element of an newMap map

deleteFindMin :: !(Map k a) -> (!(!k, !a), !Map k a)
deleteFindMin t
  = case t of
      Bin _ k x Tip r
        = ((k, x), r)
      Bin _ k x l r
        #! (km,l`) = deleteFindMin l
        = (km, balanceR k x l` r)
      Tip
        = (abort "Map.deleteFindMin: can not return the minimal element of an newMap map", Tip)

// | /O(log n)/. Delete and find the maximal element.
//
// > deleteFindMax (fromList [(5,"a"), (3,"b"), (10,"c")]) == ((10,"c"), fromList [(3,"b"), (5,"a")])
// > deleteFindMax newMap                                      Error: can not return the maximal element of an newMap map

deleteFindMax :: !(Map k a) -> (!(!k, !a), !Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip
        = ((k, x), l)
      Bin _ k x l r
        #! (km,r`) = deleteFindMax r
        = (km, balanceL k x l r`)
      Tip
        = (abort "Map.deleteFindMax: can not return the maximal element of an newMap map", Tip)


////////////////////////////////////////////////////////////////////
//  [balance l x r] balances two trees with value x.
//  The mapSizes of the trees should balance after decreasing the
//  mapSize of one of them. (a rotation).
//
//  [delta] is the maximal relative difference between the mapSizes of
//          two trees, it corresponds with the [w] in Adams' paper.
//  [ratio] is the ratio between an outer and inner sibling of the
//          heavier subtree in an unbalanced setting. It determines
//          whether a double or single rotation should be performed
//          to restore balance. It is corresponds with the inverse
//          of $\alpha$ in Adam's article.
//
//  Note that according to the Adam's paper:
//  - [delta] should be larger than 4.646 with a [ratio] of 2.
//  - [delta] should be larger than 3.745 with a [ratio] of 1.534.
//
//  But the Adam's paper is erroneous:
//  - It can be proved that for delta=2 and delta>=5 there does
//    not exist any ratio that would work.
//  - Delta=4.5 and ratio=2 does not work.
//
//  That leaves two reasonable variants, delta=3 and delta=4,
//  both with ratio=2.
//
//  - A lower [delta] leads to a more 'perfectly` balanced tree.
//  - A higher [delta] performs less rebalancing.
//
//  In the benchmarks, delta=3 is faster on put operations,
//  and delta=4 has slightly better deletes. As the put speedup
//  is larger, we currently use delta=3.
//
////////////////////////////////////////////////////////////////////
//delta :: Int
delta :== 3

//ratio :: Int
ratio :== 2

// The balance function is equivalent to the following:
//
//   balance :: k -> a -> Map k a -> Map k a -> Map k a
//   balance k x l r
//     | mapSizeL + mapSizeR <= 1    = Bin mapSizeX k x l r
//     | mapSizeR > delta*mapSizeL   = rotateL k x l r
//     | mapSizeL > delta*mapSizeR   = rotateR k x l r
//     | otherwise             = Bin mapSizeX k x l r
//     where
//       mapSizeL = mapSize l
//       mapSizeR = mapSize r
//       mapSizeX = mapSizeL + mapSizeR + 1
//
//   rotateL :: a -> b -> Map a b -> Map a b -> Map a b
//   rotateL k x l r=:(Bin _ _ _ ly ry) | mapSize ly < ratio*mapSize ry = singleL k x l r
//                                     | otherwise               = doubleL k x l r
//
//   rotateR :: a -> b -> Map a b -> Map a b -> Map a b
//   rotateR k x l@(Bin _ _ _ ly ry) r | mapSize ry < ratio*mapSize ly = singleR k x l r
//                                     | otherwise               = doubleR k x l r
//
//   singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
//   singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)  = bin k2 x2 (bin k1 x1 t1 t2) t3
//   singleR k1 x1 (Bin _ k2 x2 t1 t2) t3  = bin k2 x2 t1 (bin k1 x1 t2 t3)
//
//   doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
//   doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4) = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
//   doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4 = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
//
// It is only written in such a way that every node is pattern-matched only once.

balance :: !k !a !(Map k a) !(Map k a) -> Map k a
balance k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr=:(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl=:(Bin rls rlk rlx rll rlr) rr=:(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+mapSize rll) k x Tip rll) (Bin (1+rrs+mapSize rlr) rk rx rlr rr)

  (Bin ls lk lx ll lr) -> case r of
           Tip -> case (ll, lr) of
                    (Tip, Tip) -> Bin 2 k x l Tip
                    (Tip, (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
                    ((Bin _ _ _ _ _), Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
                    ((Bin lls _ _ _ _), (Bin lrs lrk lrx lrl lrr))
                      | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
                      | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+mapSize lrl) lk lx ll lrl) (Bin (1+mapSize lrr) k x lrr Tip)
           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+mapSize rll) k x l rll) (Bin (1+rrs+mapSize rlr) rk rx rlr rr)
                   (_, _) -> abort "Failure in Data.Map.balance"
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+mapSize lrl) lk lx ll lrl) (Bin (1+rs+mapSize lrr) k x lrr r)
                   (_, _) -> abort "Failure in Data.Map.balance"
              | otherwise -> Bin (1+ls+rs) k x l r

// Functions balanceL and balanceR are specialised versions of balance.
// balanceL only checks whether the left subtree is too big,
// balanceR only checks whether the right subtree is too big.

// balanceL is called when left subtree might have been puted to or when
// right subtree might have been deleted from.
balanceL :: !k !a !(Map k a) !(Map k a) -> Map k a
balanceL k x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x l Tip
           (Bin _ lk lx Tip (Bin _ lrk lrx _ _)) -> Bin 3 lrk lrx (Bin 1 lk lx Tip Tip) (Bin 1 k x Tip Tip)
           (Bin _ lk lx ll=:(Bin _ _ _ _ _) Tip) -> Bin 3 lk lx ll (Bin 1 k x Tip Tip)
           (Bin ls lk lx ll=:(Bin lls _ _ _ _) lr=:(Bin lrs lrk lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lk lx ll (Bin (1+lrs) k x lr Tip)
             | otherwise -> Bin (1+ls) lrk lrx (Bin (1+lls+mapSize lrl) lk lx ll lrl) (Bin (1+mapSize lrr) k x lrr Tip)

  (Bin rs _ _ _ _) -> case l of
           Tip -> Bin (1+rs) k x Tip r

           (Bin ls lk lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _ _, Bin lrs lrk lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lk lx ll (Bin (1+rs+lrs) k x lr r)
                     | otherwise -> Bin (1+ls+rs) lrk lrx (Bin (1+lls+mapSize lrl) lk lx ll lrl) (Bin (1+rs+mapSize lrr) k x lrr r)
                   (_, _) -> abort "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) k x l r

// balanceR is called when right subtree might have been puted to or when
// left subtree might have been deleted from.
balanceR :: !k !a !(Map k a) !(Map k a) -> Map k a
balanceR k x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 k x Tip Tip
           (Bin _ _ _ Tip Tip) -> Bin 2 k x Tip r
           (Bin _ rk rx Tip rr=:(Bin _ _ _ _ _)) -> Bin 3 rk rx (Bin 1 k x Tip Tip) rr
           (Bin _ rk rx (Bin _ rlk rlx _ _) Tip) -> Bin 3 rlk rlx (Bin 1 k x Tip Tip) (Bin 1 rk rx Tip Tip)
           (Bin rs rk rx rl=:(Bin rls rlk rlx rll rlr) rr=:(Bin rrs _ _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rk rx (Bin (1+rls) k x Tip rl) rr
             | otherwise -> Bin (1+rs) rlk rlx (Bin (1+mapSize rll) k x Tip rll) (Bin (1+rrs+mapSize rlr) rk rx rlr rr)

  (Bin ls _ _ _ _) -> case r of
           Tip -> Bin (1+ls) k x l Tip

           (Bin rs rk rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlk rlx rll rlr, Bin rrs _ _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rk rx (Bin (1+ls+rls) k x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlk rlx (Bin (1+ls+mapSize rll) k x l rll) (Bin (1+rrs+mapSize rlr) rk rx rlr rr)
                   (_, _) -> abort "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) k x l r


////////////////////////////////////////////////////////////////////
//  The bin constructor maintains the mapSize of the tree
////////////////////////////////////////////////////////////////////
bin :: !k !a !(Map k a) !(Map k a) -> Map k a
bin k x l r = Bin (mapSize l + mapSize r + 1) k x l r

////////////////////////////////////////////////////////////////////
//  Eq converts the tree to a list. In a lazy setting, this
//  actually seems one of the faster methods to gLexOrd{|*|} two trees
//  and it is certainly the simplest :-)
////////////////////////////////////////////////////////////////////
instance == (Map k a) | Eq k & Eq a where
  (==) t1 t2  = (mapSize t1 == mapSize t2) && (toAscList t1 == toAscList t2)

instance < (Map k v) | Ord k & Ord v where
    (<) t1 t2 = toAscList t1 < toAscList t2

instance Functor (Map k)
where
	fmap :: !(a -> b) !(Map k a) -> Map k b
	fmap f m  = map f m

// TODO
//instance Traversable (Map k) where
  //traverse f = traverseWithKey (\_ -> f)

//instance Foldable (Map k) where
  //fold t = go t
    //where go Tip = mnewMap
          //go (Bin 1 _ v _ _) = v
          //go (Bin _ _ v l r) = mappend (go l) (mappend v (go r))
  //foldr = foldr
  //foldl = foldl
  //foldMap f t = go t
    //where go Tip = mnewMap
          //go (Bin 1 _ v _ _) = f v
          //go (Bin _ _ v l r) = mappend (go l) (mappend (f v) (go r))

instance toString (Map k a) | toString k & toString a where
  toString m  = "" // TODO showParen False (showString "fromList " o shows (toList m))

// | /O(n)/. Show the tree that implements the map. The tree is shown
// in a compressed, hanging format. See 'showTreeWith'.
showTree :: !(Map k a) -> String | toString k & toString a
showTree m
  = showTreeWith showElem True False m
  where
  showElem k x  = toString k +++ ":=" +++ toString x

shows :: !a -> (String -> String) | toString a
shows x = showsPrec 0 x

showString :: !String -> (String -> String)
showString str = \str` -> str +++ str`

showParen :: !Bool !(String -> String) -> (String -> String)
showParen b p = if b (showChar '(' o p o showChar ')') p

showChar :: !Char -> (String -> String)
showChar x = \str -> toString x +++ str

showsPrec _ x s = toString x +++ s

/* | /O(n)/. The expression (@'showTreeWith' showelem hang wide map@) shows
 the tree that implements the map. Elements are shown using the @showElem@ function. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

>  Map> let t = fromDistinctAscList [(x,()) | x <- [1..5]]
>  Map> putStrLn $ showTreeWith (\k x -> toString (k,x)) True False t
>  (4,())
>  +//(2,())
>  |  +//(1,())
>  |  +//(3,())
>  +//(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> toString (k,x)) True True t
>  (4,())
>  |
>  +//(2,())
>  |  |
>  |  +//(1,())
>  |  |
>  |  +//(3,())
>  |
>  +//(5,())
>
>  Map> putStrLn $ showTreeWith (\k x -> toString (k,x)) False True t
>  +//(5,())
>  |
>  (4,())
>  |
>  |  +//(3,())
>  |  |
>  +//(2,())
>     |
>     +//(1,())
*/
showTreeWith :: !(k a -> String) !Bool !Bool !(Map k a) -> String
showTreeWith showelem hang wide t
  | hang      = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""

showsTree :: !(k a -> String) !Bool ![String] ![String] !(Map k a) -> (String -> String)
showsTree showelem wide lbars rbars t
  = case t of
      Tip -> showsBars lbars o showString "|\n"
      Bin _ kx x Tip Tip
          -> showsBars lbars o showString (showelem kx x) o showString "\n"
      Bin _ kx x l r
          -> showsTree showelem wide (withBar rbars) (withEmpty rbars) r o
             showWide wide rbars o
             showsBars lbars o showString (showelem kx x) o showString "\n" o
             showWide wide lbars o
             showsTree showelem wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: !(k a -> String) !Bool ![String] !(Map k a) -> (String -> String)
showsTreeHang showelem wide bars t
  = case t of
      Tip -> showsBars bars o showString "|\n"
      Bin _ kx x Tip Tip
          -> showsBars bars o showString (showelem kx x) o showString "\n"
      Bin _ kx x l r
          -> showsBars bars o showString (showelem kx x) o showString "\n" o
             showWide wide bars o
             showsTreeHang showelem wide (withBar bars) l o
             showWide wide bars o
             showsTreeHang showelem wide (withEmpty bars) r

showWide :: !Bool ![String] !String -> String
showWide wide bars s
  | wide      = "" // TODO showString (foldr (+++) "" (reverse bars)) (showString "|\n" s)
  | otherwise = s

showsBars :: ![String] -> (String -> String)
showsBars bars
  = case bars of
      [] -> id
      _  -> id // TODO showString (foldr (+++) "" (reverse (tail bars))) o showString node

node :: String
node           = "+//"

withBar :: ![String] -> [String]
withBar bars   = ["|  ":bars]

withEmpty :: ![String] -> [String]
withEmpty bars = ["   ":bars]

////////////////////////////////////////////////////////////////////
//  Assertions
////////////////////////////////////////////////////////////////////
// | /O(n)/. Test if the internal map structure is valid.
//
// > valid (fromAscList [(3,"b"), (5,"a")]) == True
// > valid (fromAscList [(5,"a"), (3,"b")]) == False

valid :: !(Map k a) -> Bool | < k
valid t = balanced t && ordered t && validmapSize t

ordered :: !(Map a b) -> Bool | < a
ordered t
  = bounded (const True) (const True) t
  where
    bounded :: !(a -> .Bool) !(a -> .Bool) !(Map a b) -> Bool | < a
    bounded lo hi t`
      = case t` of
          Tip              -> True
          Bin _ kx _ l r  -> (lo kx) && (hi kx) && bounded lo (\x -> x < kx) l && bounded (\x -> x > kx) hi r

balanced :: !(Map k a) -> Bool
balanced Tip = True
balanced (Bin _ _ _ l r) = (mapSize l + mapSize r <= 1 || (mapSize l <= delta*mapSize r && mapSize r <= delta*mapSize l)) &&
                            balanced l && balanced r

validmapSize :: !(Map a b) -> Bool
validmapSize t
  = (realmapSize t == Just (mapSize t))
  where
  realmapSize :: !(Map a b) -> Maybe Int
  realmapSize Tip = Just 0
  realmapSize (Bin sz _ _ l r) = case (realmapSize l, realmapSize r) of
                                   (Just n,Just m)  | n+m+1 == sz  -> Just sz
                                   _                               -> Nothing

// | /O(1)/.  Decompose a map into pieces based on the structure of the underlying
// tree.  This function is useful for consuming a map in parallel.
//
// No guarantee is made as to the mapSizes of the pieces; an internal, but
// deterministic process determines this.  However, it is guaranteed that the pieces
// returned will be in ascending order (all elements in the first submap less than all
// elements in the second, and so on).
//
// Examples:
//
// > splitRoot (fromList (zip [1..6] ['a`..])) ==
// >   [fromList [(1,'a`),(2,'b`),(3,'c')],fromList [(4,'d')],fromList [(5,'e'),(6,'f')]]
//
// > splitRoot newMap == []
//
//  Note that the current implementation does not return more than three submaps,
//  but you should not depend on this behaviour because it can change in the
//  future without notice.
splitRoot :: !(Map k b) -> [Map k b]
splitRoot Tip = []
splitRoot (Bin _ k v l r) = [l, singleton k v, r]

getU :: !k !w:(Map k v) -> x:(!Maybe v, !y:(Map k v)) | == k & < k, [ x <= y, w <= y]
getU k Tip = (Nothing, Tip)
getU k (Bin h nk nv left right)
  | k == nk  = (Just nv, Bin h nk nv left right)
  | k < nk
    #! (mbv, left) = getU k left
    = (mbv, Bin h nk nv left right)
  | otherwise
    #! (mbv, right) = getU k right
    = (mbv, Bin h nk nv left right)

delU :: !a !.(Map a b) -> u:(!v:(Maybe b), !Map a b) | == a & < a, [u <= v]
delU k Tip = (Nothing, Tip)              //Do nothing
delU k (Bin h nk nv Tip Tip)            //A node with just leaves as children can be safely removed
  | k == nk = (Just nv, Tip)
             = (Nothing, Bin h nk nv Tip Tip)
delU k (Bin h nk nv Tip right)            //A node without smaller items
  | k == nk = (Just nv, right)            //When found, just remove
  | k < nk   = (Nothing, Bin h nk nv Tip right)  //Do nothing, k is not in the mapping
  | otherwise
    #! (mbv, right)    = delU k right
    #! (hright, right) = height right
    = (mbv, balance nk nv Tip right)
delU k (Bin h nk nv left Tip)						//A node without larger items
	| k == nk	= (Just nv, left)						//When found just remove
	| k < nk	
		#! (mbv,left)		= delU k left
		#! (hleft,left)		= height left
		= (mbv, balance nk nv left Tip)
	| otherwise
				= (Nothing, Bin h nk nv left Tip)	//Do nothing, k is not in hte mapping

delU k (Bin h nk nv left right)						//A node with both larger and smaller items
	| k == nk	
		#! (left,k,v)		= takeMax left
		#! (h,left,right)	= parentHeight left right
		= (Just nv, balance k v left right)	//Replace with the largest of the smaller items and rebalance
	| k < nk	
		#! (mbv, left)		= delU k left
		#! (h,left,right)	= parentHeight left right
		= (mbv, balance nk nv left right)
	| otherwise
		#! (mbv, right)		= delU k right
		#! (h,left,right)	= parentHeight left right
		= (mbv, balance nk nv left right)
where // TODO
	//Takes the k and v values from the maximum node in the tree and removes that node
    takeMax :: !(Map a b) -> (!Map a b, !a, !b)
	takeMax Tip = abort "takeMax of leaf evaluated" 
	takeMax (Bin _ nk nv left Tip)	= (left, nk, nv)
	takeMax (Bin _ nk nv left right)
      #! (right,k,v)    = takeMax right
      #! (hleft,left)   = height left
      #! (hright,right) = height right
      = (balance nk nv left right, k, v)

	//Determines the height of the parent node of two sub trees
    parentHeight :: !(Map a b) !(Map c d) -> (!Int, !Map a b, !Map c d)
	parentHeight left right
		#! (hleft,left)   = height left
		#! (hright,right) = height right
		#! h              = (max hleft hright) + 1
		= (h, left, right)

height :: !u:(Map k w:v) -> x:(!Int, !y:(Map k w:v)), [u y <= w, x <= y, u <= y]
height Tip                    = (0, Tip)
height (Bin h k v left right) = (h, Bin h k v left right)

gEq{|Map|} fk fv mx my = mapSize mx == mapSize my && and [fk kx ky && fv vx vy \\ (kx,vx) <- toList mx & (ky,vy) <- toList my]

gLexOrd{|Map|} kLexOrd vLexOrd x y = gLexOrd{|* -> *|} (gLexOrd{|* -> * -> *|} kLexOrd vLexOrd) (toAscList x) (toAscList y)

