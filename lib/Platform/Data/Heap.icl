implementation module Data.Heap

from Control.Applicative import class pure(..), class Applicative, :: WrappedMonad (..), unwrapMonad,
	instance Functor (WrappedMonad m), instance pure (WrappedMonad m), instance <*> (WrappedMonad m)
from Control.Monad import liftM, class Monad
import Data.Monoid
from Data.Func import on
from Data.Functor import class Functor (..)
from Data.List import instance Functor [], instance pure [], instance <*> [],
	instance Semigroup [a], instance Monoid [a], instance Foldable [], instance Traversable []
import qualified Data.Traversable
from Data.Traversable import class Traversable (..)
from Data.Foldable import class Foldable (..)
from StdFunc import o, id, flip
from StdOverloaded import class < (..), class == (..), class + (..), class isEven
from StdInt import instance isEven Int, instance + Int, instance == Int, instance < Int, instance - Int
//import StdClass
import StdClass
from StdBool import &&
from StdMisc import abort
import Data.Maybe
import Data.Tuple
from StdList import repeatn

import qualified Data.Foldable
import qualified Data.List
import qualified StdList

// The implementation of 'Heap' must internally hold onto the dictionary entry for ('<='),
// so that it can be made 'Foldable'. Confluence in the absence of incoherent instances
// is provided by the fact that we only ever build these from instances of 'Ord' a (except in the case of 'groupBy')


// A min-heap of values of type @a@.
:: Heap a
  = Empty
  | Heap !Int (a -> a -> Bool) !(Tree a)

instance == (Heap a) where
  (==) Empty Empty = True
  (==) Empty _ = False
  (==) _ Empty = False
  (==) a=:(Heap s1 leq _) b=:(Heap s2 _ _) = s1 == s2 && go leq ('Data.Foldable'.toList a) ('Data.Foldable'.toList b)
    where
    go f [x:xs] [y:ys] = f x y && f y x && go f xs ys
    go _ []     []     = True
    go _ _      _      = False
  (==) _ _ = False

instance < (Heap a) where
  < Empty Empty = False
  < Empty _     = True
  < _     Empty = False
  < a=:(Heap _ leq _) b = go leq ('Data.Foldable'.toList a) ('Data.Foldable'.toList b)
    where
    go f [x:xs] [y:ys] =
        if (f x y)
          (if (f y x)
             (go f xs ys)
             True)
          False
    go f []    []    = False
    go f []    [_:_] = True
    go f [_:_] []    = False
  < _     _ = False


// /O(1)/. Is the heap empty?
//
// >>> null empty
// True
//
// >>> null (singleton "hello")
// False
null :: (Heap a) -> Bool
null Empty = True
null _     = False

// /O(1)/. The number of elements in the heap.
//
// >>> size empty
// 0
// >>> size (singleton "hello")
// 1
// >>> size (fromList [4,1,2])
// 3
size :: (Heap a) -> Int
size Empty        = 0
size (Heap s _ _) = s

// /O(1)/. The empty heap
//
// @'empty' ≡ 'fromList' []@
//
// >>> size empty
// 0
//empty :: Heap a
empty :== Empty

// /O(1)/. A heap with a single element
//
// @
// 'singleton' x ≡ 'fromList' [x]
// 'singleton' x ≡ 'insert' x 'empty'
// @
//
// >>> size (singleton "hello")
// 1
//singleton :: a -> Heap a | Ord a
singleton x :== singletonWith (<=) x

//singletonWith :: (a a -> Bool) a -> Heap a
singletonWith f a :== Heap 1 f (Node 0 a Nil)

// /O(1)/. Insert a new value into the heap.
//
// >>> insert 2 (fromList [1,3])
// fromList [1,2,3]
//
// @
// 'insert' x 'empty' ≡ 'singleton' x
// 'size' ('insert' x xs) ≡ 1 + 'size' xs
// @
//insert :: a (Heap a) -> (Heap a) | Ord a
insert :== insertWith (<=)

insertWith :: (a a -> Bool) a (Heap a) -> Heap a
insertWith leq x Empty = singletonWith leq x
insertWith leq x (Heap s _ t=:(Node _ y f))
  | leq x y   = Heap (s + 1) leq (Node 0 x (Cons t Nil))
  | otherwise = Heap (s + 1) leq (Node 0 y (skewInsert leq (Node 0 x Nil) f))

// /O(1)/. Meld the values from two heaps into one heap.
//
// >>> union (fromList [1,3,5]) (fromList [6,4,2])
// fromList [1,2,6,4,3,5]
// >>> union (fromList [1,1,1]) (fromList [1,2,1])
// fromList [1,1,1,2,1,1]
union :: (Heap a) (Heap a) -> Heap a
union Empty q = q
union q Empty = q
union (Heap s1 leq t1=:(Node _ x1 f1)) (Heap s2 _ t2=:(Node _ x2 f2))
  | leq x1 x2 = Heap (s1 + s2) leq (Node 0 x1 (skewInsert leq t2 f1))
  | otherwise = Heap (s1 + s2) leq (Node 0 x2 (skewInsert leq t1 f2))
union _ _ = abort "error in union\n"

// /O(log n)/. Create a heap consisting of multiple copies of the same value.
//
// >>> replicate 'a' 10
// fromList "aaaaaaaaaa"
replicate :: a Int -> Heap a | Ord a
replicate x0 y0 = fromList (repeatn y0 x0)
  //| y0 < 0 = abort "Heap.replicate: negative length"
  //| y0 == 0 = mempty
  //| otherwise = f (singleton x0) y0
  //where
    //f x y
        //| isEven y = f (union x x) (quot y 2)
        //| y == 1 = x
        //| otherwise = g (union x x) (quot (y - 1) 2) x
    //g x y z
        //| isEven y = g (union x x) (quot y 2) z
        //| y == 1 = union x z
        //| otherwise = g (union x x) (quot (y - 1) 2) (union x z)

// Provides both /O(1)/ access to the minimum element and /O(log n)/ access to the remainder of the heap.
// This is the same operation as 'viewMin'
//
// >>> uncons (fromList [2,1,3])
// Just (1,fromList [2,3])
uncons :: (Heap a) -> Maybe (a, Heap a) | Ord a
uncons l=:(Heap _ _ t) = Just (root t, deleteMin l)
uncons _               = Nothing

// Same as 'uncons'
//viewMin :: (Heap a) -> Maybe (a, Heap a) | Ord a
viewMin :== uncons

// /O(1)/. Assumes the argument is a non-'null' heap.
//
// >>> minimum (fromList [3,1,2])
// 1
minimum :: (Heap a) -> a
minimum (Heap _ _ t) = root t
minimum _            = abort "Heap.minimum: empty heap"

trees :: (Forest a) -> [Tree a]
trees (Cons a as) = [a : trees as]
trees _           = []

// /O(log n)/. Delete the minimum key from the heap and return the resulting heap.
//
// >>> deleteMin (fromList [3,1,2])
// fromList [2,3]
deleteMin :: (Heap a) -> Heap a
deleteMin Empty                      = Empty
deleteMin (Heap _ _ (Node _ _ Nil))  = Empty
deleteMin (Heap s leq (Node _ _ f0)) = Heap (s - 1) leq (Node 0 x f3)
  where
  (Node r x cf, ts2) = getMin leq f0
  (zs, ts1, f1) = splitForest r Nil Nil cf
  f2 = skewMeld leq (skewMeld leq ts1 ts2) f1
  f3 = foldr (skewInsert leq) f2 (trees zs)

// /O(log n)/. Adjust the minimum key in the heap and return the resulting heap.
//
// >>> adjustMin (+1) (fromList [1,2,3])
// fromList [2,2,3]
adjustMin :: (a -> a) (Heap a) -> Heap a
adjustMin _ Empty = Empty
adjustMin f (Heap s leq (Node r x xs)) = Heap s leq (heapify leq (Node r (f x) xs))

:: ForestZipper a :== (Forest a, Forest a)

//zipper :: (Forest a) -> ForestZipper a
zipper xs :== (Nil, xs)

//emptyZ :: ForestZipper a
emptyZ :== (Nil, Nil)

// leftZ :: (ForestZipper a) -> ForestZipper a
// leftZ (x :> path, xs) = (path, x :> xs)

//rightZ :: (ForestZipper a) -> ForestZipper a
rightZ (path, Cons x xs) :== (Cons x path, xs)

adjustZ :: ((Tree a) -> Tree a) (ForestZipper a) -> ForestZipper a
adjustZ f (path, Cons x xs) = (path, Cons (f x) xs)
adjustZ _ z                 = z

rezip :: (ForestZipper a) -> Forest a
rezip (Nil, xs)         = xs
rezip (Cons x path, xs) = rezip (path, Cons x xs)

// assumes non-empty zipper
rootZ :: (ForestZipper a) -> a
rootZ (_ , Cons x _) = root x
rootZ _              = abort "Heap.rootZ: empty zipper"

minZ :: (a a -> Bool) (Forest a) -> ForestZipper a
minZ _ Nil = emptyZ
minZ f xs = minZ` f z z
  where z = zipper xs

minZ` :: (a a -> Bool) (ForestZipper a) (ForestZipper a) -> ForestZipper a
minZ` _   lo (_, Nil) = lo
minZ` leq lo z        = minZ` leq (if (leq (rootZ lo) (rootZ z)) lo z) (rightZ z)

heapify :: (a a -> Bool) (Tree a) -> Tree a
heapify _   n=:(Node _ _ Nil) = n
heapify leq n=:(Node r a as)
  | leq a a`  = n
  | otherwise = Node r a` (rezip (left, Cons (heapify leq (Node r` a as`)) right))
  where
  (left, Cons (Node r` a` as`) right) = minZ leq as

// /O(n)/. Build a heap from a list of values.
//
// @
// 'fromList' '.' 'toList' ≡ 'id'
// 'toList' '.' 'fromList' ≡ 'sort'
// @

// >>> size (fromList [1,5,3])
// 3
//fromList :: [a] -> Heap a | Ord a
fromList xs :== 'StdList'.foldr insert mempty xs

//fromListWith :: (a a -> Bool) [a] -> Heap a
fromListWith f xs :== 'StdList'.foldr (insertWith f) mempty xs

// /O(n log n)/. Perform a heap sort
//sort :: [a] -> [a] | Ord a
sort xs = 'Data.Foldable'.toList (fromList xs)

instance Semigroup (Heap a) where
  mappend l r = union l r

instance Monoid (Heap a) where
  mempty = empty

// /O(n)/. Returns the elements in the heap in some arbitrary, very likely unsorted, order.
//
// >>> toUnsortedList (fromList [3,1,2])
// [1,3,2]
//
// @'fromList' '.' 'toUnsortedList' ≡ 'id'@
toUnsortedList :: (Heap a) -> [a]
toUnsortedList Empty        = []
toUnsortedList (Heap _ _ t) = foldMap pure t

instance Foldable Heap where
  foldMap _ Empty = mempty
  foldMap f l=:(Heap _ _ t) = mappend (f (root t)) (foldMap f (deleteMin l))
  fold x = foldMap id x
  foldr f z t = appEndo (foldMap (Endo o f) t) z
  foldr` f z0 xs = foldl f` id xs z0
    where f` k x z = k (f x z)
  foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
  foldl` f z0 xs = foldr f` id xs z0
    where f` x k z = k (f z x)
  foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                  (foldr mf Nothing xs)
    where
      mf x Nothing = Just x
      mf x (Just y) = Just (f x y)
  foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                  (foldl mf Nothing xs)
    where
      mf Nothing y = Just y
      mf (Just x) y = Just (f x y)

// /O(n)/. Map a function over the heap, returning a new heap ordered appropriately for its fresh contents
//
// >>> map negate (fromList [3,1,2])
// fromList [-3,-1,-2]
map :: (a -> b) (Heap a) -> Heap b | Ord b
map _ Empty = Empty
map f (Heap _ _ t) = foldMap (singleton o f) t

// /O(n)/. Map a monotone increasing function over the heap.
// Provides a better constant factor for performance than 'map', but no checking is performed that the function provided is monotone increasing. Misuse of this function can cause a Heap to violate the heap property.
//
// >>> map (+1) (fromList [1,2,3])
// fromList [2,3,4]
// >>> map (*2) (fromList [1,2,3])
// fromList [2,4,6]
mapMonotonic :: (a -> b) (Heap a) -> Heap b | Ord b
mapMonotonic _ Empty = Empty
mapMonotonic f (Heap s _ t) = Heap s (<=) (fmap f t)

// * Filter

// /O(n)/. Filter the heap, retaining only values that satisfy the predicate.
//
// >>> filter (>'a') (fromList "ab")
// fromList "b"
// >>> filter (>'x') (fromList "ab")
// fromList []
// >>> filter (<'a') (fromList "ab")
// fromList []
filter :: (a -> Bool) (Heap a) -> Heap a
filter _ Empty = Empty
filter p (Heap _ leq t) = foldMap f t
  where
  f x | p x = singletonWith leq x
      | otherwise = Empty

// /O(n)/. Partition the heap according to a predicate. The first heap contains all elements that satisfy the predicate, the second all elements that fail the predicate. See also 'split'.
//
// >>> partition (>'a') (fromList "ab")
// (fromList "b",fromList "a")
partition :: (a -> Bool) (Heap a) -> (Heap a, Heap a)
partition _ Empty = (Empty, Empty)
partition p (Heap _ leq t) = foldMap f t
  where
  f x | p x       = (singletonWith leq x, mempty)
      | otherwise = (mempty, singletonWith leq x)

// /O(n)/. Partition the heap into heaps of the elements that are less than, equal to, and greater than a given value.
//
// >>> split 'h' (fromList "hello")
// (fromList "e",fromList "h",fromList "llo")
split :: a (Heap a) -> (Heap a, Heap a, Heap a)
split a Empty = (Empty, Empty, Empty)
split a (Heap s leq t) = foldMap f t
  where
  f x = if (leq x a)
          (if (leq a x)
             (mempty, singletonWith leq x, mempty)
             (singletonWith leq x, mempty, mempty))
          (mempty, mempty, singletonWith leq x)

// * Subranges

// /O(n log n)/. Return a heap consisting of the least @n@ elements of a given heap.
//
// >>> take 3 (fromList [10,2,4,1,9,8,2])
// fromList [1,2,2]
//take :: Int (Heap a) -> Heap a
take :== withList o 'Data.List'.take

// /O(n log n)/. Return a heap consisting of all members of given heap except for the @n@ least elements.
//drop :: Int (Heap a) -> Heap a
drop :== withList o 'Data.List'.drop

// /O(n log n)/. Split a heap into two heaps, the first containing the @n@ least elements, the latter consisting of all members of the heap except for those elements.
//splitAt :: Int (Heap a) -> (Heap a, Heap a)
splitAt :== splitWithList o 'Data.List'.splitAt

// /O(n log n)/. 'break' applied to a predicate @p@ and a heap @xs@ returns a tuple where the first element is a heap consisting of the
// longest prefix the least elements of @xs@ that /do not satisfy/ p and the second element is the remainder of the elements in the heap.
//
// >>> break (\x -> x `mod` 4 == 0) (fromList [3,5,7,12,13,16])
// (fromList [3,5,7],fromList [12,13,16])
//
// 'break' @p@ is equivalent to @'span' ('not' . p)@.
//break :: (a -> Bool) (Heap a) -> (Heap a, Heap a)
break :== splitWithList o 'Data.List'.break

// /O(n log n)/. 'span' applied to a predicate @p@ and a heap @xs@ returns a tuple where the first element is a heap consisting of the
// longest prefix the least elements of xs that satisfy @p@ and the second element is the remainder of the elements in the heap.
//
// >>> span (\x -> x `mod` 4 == 0) (fromList [4,8,12,14,16])
// (fromList [4,8,12],fromList [14,16])
//
// 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile p xs)@
//span :: (a -> Bool) (Heap a) -> (Heap a, Heap a)
span :== splitWithList o 'Data.List'.span

// /O(n log n)/. 'takeWhile' applied to a predicate @p@ and a heap @xs@ returns a heap consisting of the
// longest prefix the least elements of @xs@ that satisfy @p@.
//
// >>> takeWhile (\x -> x `mod` 4 == 0) (fromList [4,8,12,14,16])
// fromList [4,8,12]
//takeWhile :: (a -> Bool) (Heap a) -> Heap a
takeWhile :== withList o 'Data.List'.takeWhile

// /O(n log n)/. 'dropWhile' @p xs@ returns the suffix of the heap remaining after 'takeWhile' @p xs@.
//
// >>> dropWhile (\x -> x `mod` 4 == 0) (fromList [4,8,12,14,16])
// fromList [14,16]
//dropWhile :: (a -> Bool) (Heap a) -> Heap a
dropWhile :== withList o 'Data.List'.dropWhile

// /O(n log n)/. Remove duplicate entries from the heap.
//
// >>> nub (fromList [1,1,2,6,6])
// fromList [1,2,6]
nub :: (Heap a) -> Heap a
nub Empty             = Empty
nub h=:(Heap _ leq t) = insertWith leq x (nub zs)
  where
  x = root t
  xs = deleteMin h
  zs = dropWhile (\a -> leq a x) xs

// /O(n)/. Construct heaps from each element in another heap, and union them together.
//
// >>> concatMap (\a -> fromList [a,a+1]) (fromList [1,4])
// fromList [1,4,5,2]
concatMap :: (a -> Heap b) (Heap a) -> Heap b | Ord b
concatMap _ Empty = Empty
concatMap f h=:(Heap _ _ t) = foldMap f t

// /O(n log n)/. Group a heap into a heap of heaps, by unioning together duplicates.
//
// >>> group (fromList "hello")
// fromList [fromList "e",fromList "h",fromList "ll",fromList "o"]
group :: (Heap a) -> Heap (Heap a)
group Empty             = Empty
group h=:(Heap _ leq _) = groupBy (flip leq) h

// /O(n log n)/. Group using a user supplied function.
groupBy :: (a a -> Bool) (Heap a) -> Heap (Heap a)
groupBy f Empty = Empty
groupBy f h=:(Heap _ leq t) = insert (insertWith leq x ys) (groupBy f zs)
  where
  x = root t
  xs = deleteMin h
  (ys,zs) = span (f x) xs

// /O(n log n + m log m)/. Intersect the values in two heaps, returning the value in the left heap that compares as equal
intersect :: (Heap a) (Heap a) -> Heap a
intersect Empty _ = Empty
intersect _ Empty = Empty
intersect a=:(Heap _ leq _) b = go leq ('Data.Foldable'.toList a) ('Data.Foldable'.toList b)
  where
  go leq` xxs=:[x:xs] yys=:[y:ys] =
      if (leq` x y)
        (if (leq` y x)
           (insertWith leq` x (go leq` xs ys))
           (go leq` xs yys))
        (go leq` xxs ys)
  go _ [] _ = empty
  go _ _ [] = empty
  go _ _ _  = abort "error in go\n"
intersect _ _ = abort "error in intersect\n"

/// /O(n log n + m log m)/. Intersect the values in two heaps using a function to generate the elements in the right heap.
intersectWith :: (a a -> b) (Heap a) (Heap a) -> Heap b | Ord b
intersectWith _ Empty _ = Empty
intersectWith _ _ Empty = Empty
intersectWith f a=:(Heap _ leq _) b = go leq f ('Data.Foldable'.toList a) ('Data.Foldable'.toList b)
  where
  go :: (a a -> Bool) (a a -> b) [a] [a] -> Heap b | Ord b
  go leq` f` xxs=:[x:xs] yys=:[y:ys]
      | leq` x y =
          if (leq` y x)
            (insert (f` x y) (go leq` f` xs ys))
            (go leq` f` xs yys)
      | otherwise = go leq` f` xxs ys
  go _ _ [] _ = empty
  go _ _ _ [] = empty
  go _ _ _ _  = abort "error in go\n"
intersectWith _ _ _ = abort "error in intersectWith\n"

// /O(n log n)/. Traverse the elements of the heap in sorted order and produce a new heap using 'Applicative' side-effects.
//traverse :: (a -> t b) (Heap a) -> t (Heap b) | Applicative t & Ord b
traverseHeap f h :== fmap fromList ('Data.Traversable'.traverse f ('Data.Foldable'.toList h))

// /O(n log n)/. Traverse the elements of the heap in sorted order and produce a new heap using 'Monad'ic side-effects.
//mapM :: (a -> m b) (Heap a) -> m (Heap b) | Monad m & Ord b
//mapM f h = liftM fromList ('Traversable'.mapM f (toList h))
mapMHeap f h :== liftM fromList ('Data.Traversable'.mapM f ('Data.Foldable'.toList h))

//both :: (a -> b) (a, a) -> (b, b)
both f (a, b) :== (f a, f b)

// we hold onto the children counts in the nodes for /O(1)/ 'size'
:: Tree a = Node Int a !(Forest a)

rank (Node x _ _) = x
root (Node _ x _) = x
forest (Node _ _ x) = x

:: Forest a
  = Cons !(Tree a) !(Forest a)
  | Nil

instance Functor Tree where
  fmap f (Node r a as) = Node r (f a) (fmap f as)

instance Functor Forest where
  fmap f (Cons a as) = Cons (fmap f a) (fmap f as)
  fmap _ Nil = Nil

// internal foldable instances that should only be used over commutative monoids
instance Foldable Tree where
  foldMap f (Node _ a as) = mappend (f a) (foldMap f as)
  fold x = foldMap id x
  foldr f z t = appEndo (foldMap (Endo o f) t) z
  foldr` f z0 xs = foldl f` id xs z0
    where f` k x z = k (f x z)
  foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
  foldl` f z0 xs = foldr f` id xs z0
    where f` x k z = k (f z x)
  foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                  (foldr mf Nothing xs)
    where
      mf x Nothing = Just x
      mf x (Just y) = Just (f x y)
  foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                  (foldl mf Nothing xs)
    where
      mf Nothing y = Just y
      mf (Just x) y = Just (f x y)

// internal foldable instances that should only be used over commutative monoids
instance Foldable Forest where
  foldMap f (Cons a as) = mappend (foldMap f a) (foldMap f as)
  foldMap _ Nil = mempty
  fold x = foldMap id x
  foldr f z t = appEndo (foldMap (Endo o f) t) z
  foldr` f z0 xs = foldl f` id xs z0
    where f` k x z = k (f x z)
  foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
  foldl` f z0 xs = foldr f` id xs z0
    where f` x k z = k (f z x)
  foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                  (foldr mf Nothing xs)
    where
      mf x Nothing = Just x
      mf x (Just y) = Just (f x y)
  foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                  (foldl mf Nothing xs)
    where
      mf Nothing y = Just y
      mf (Just x) y = Just (f x y)

link :: (a a -> Bool) (Tree a) (Tree a) -> Tree a
link f t1=:(Node r1 x1 cf1) t2=:(Node r2 x2 cf2) // assumes r1 == r2
  | f x1 x2   = Node (r1 + 1) x1 (Cons t2 cf1)
  | otherwise = Node (r2 + 1) x2 (Cons t1 cf2)

skewLink :: (a a -> Bool) (Tree a) (Tree a) (Tree a) -> Tree a
skewLink f t0=:(Node _ x0 cf0) t1=:(Node r1 x1 cf1) t2=:(Node r2 x2 cf2)
  | f x1 x0 && f x1 x2 = Node (r1 + 1) x1 (Cons t0 (Cons t2 cf1))
  | f x2 x0 && f x2 x1 = Node (r2 + 1) x2 (Cons t0 (Cons t1 cf2))
  | otherwise          = Node (r1 + 1) x0 (Cons t1 (Cons t2 cf0))

ins :: (a a -> Bool) (Tree a) (Forest a) -> Forest a
ins _ t Nil = Cons t Nil
ins f t (Cons t` ts) // assumes rank t <= rank t'
  | rank t < rank t` = Cons t (Cons t` ts)
  | otherwise = ins f (link f t t`) ts

uniqify :: (a a -> Bool) (Forest a) -> Forest a
uniqify _ Nil         = Nil
uniqify f (Cons t ts) = ins f t ts

unionUniq :: (a a -> Bool) (Forest a) (Forest a) -> Forest a
unionUniq _ Nil ts = ts
unionUniq _ ts Nil = ts
unionUniq f tts1=:(Cons t1 ts1) tts2=:(Cons t2 ts2)
  | r1 < r2   = Cons t1 (unionUniq f ts1 tts2)
  | r1 == r2  = ins f (link f t1 t2) (unionUniq f ts1 ts2)
  | otherwise = Cons t2 (unionUniq f tts1 ts2)
  where
  r1 = rank t1
  r2 = rank t2
unionUniq _ _ _ = abort "error in unionUniq\n"

skewInsert :: (a a -> Bool) (Tree a) (Forest a) -> Forest a
skewInsert f t ts=:(Cons t1 (Cons t2 rest))
  | rank t1 == rank t2 = Cons (skewLink f t t1 t2) rest
  | otherwise = Cons t ts
skewInsert _ t ts = Cons t ts

//skewMeld :: (a a -> Bool) (Forest a) (Forest a) -> Forest a
skewMeld f ts ts` :== unionUniq f (uniqify f ts) (uniqify f ts`)

getMin :: (a a -> Bool) (Forest a) -> (Tree a, Forest a)
getMin _ (Cons t Nil) = (t, Nil)
getMin f (Cons t ts)
  | f (root t) (root t`) = (t, ts)
  | otherwise            = (t`, Cons t ts`)
  where (t`, ts`) = getMin f ts
getMin _ Nil = abort "Heap.getMin: empty forest"

splitForest :: Int (Forest a) (Forest a) (Forest a) -> (Forest a, Forest a, Forest a)
//splitForest a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
splitForest 0 zs ts f = (zs, ts, f)
splitForest 1 zs ts (Cons t Nil) = (zs, Cons t ts, Nil)
splitForest 1 zs ts (Cons t1 (Cons t2 f))
  // rank t1 == 0
  | rank t2 == 0 = (Cons t1 zs, Cons t2 ts, f)
  | otherwise    = (zs, Cons t1 ts, Cons t2 f)
splitForest r zs ts (Cons t1 (Cons t2 cf))
  // r1 = r - 1 or r1 == 0
  | r1 == r2  = (zs, Cons t1 (Cons t2 ts), cf)
  | r1 == 0   = splitForest (r - 1) (Cons t1 zs) (Cons t2 ts) cf
  | otherwise = splitForest (r - 1) zs (Cons t1 ts) (Cons t2 cf)
  where
  r1 = rank t1
  r2 = rank t2
splitForest _ _ _ _ = abort "Heap.splitForest: invalid arguments"

withList :: ([a] -> [a]) (Heap a) -> Heap a
withList _ Empty = Empty
withList f hp=:(Heap _ leq _) = fromListWith leq (f ('Data.Foldable'.toList hp))

splitWithList :: ([a] -> ([a], [a])) (Heap a) -> (Heap a, Heap a)
splitWithList _ Empty = (Empty, Empty)
splitWithList f hp=:(Heap _ leq _) = both (fromListWith leq) (f ('Data.Foldable'.toList hp))

// explicit priority/payload tuples

priority :: (Entry p a) -> p
priority (Entry x _) = x

payload :: (Entry p a) -> a
payload (Entry _ x) = x

instance Functor (Entry p) where
  fmap f (Entry p a) = Entry p (f a)

instance Foldable (Entry p) where
  foldMap f (Entry _ a) = f a
  fold x = foldMap id x
  foldr f z t = appEndo (foldMap (Endo o f) t) z
  foldr` f z0 xs = foldl f` id xs z0
    where f` k x z = k (f x z)
  foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
  foldl` f z0 xs = foldr f` id xs z0
    where f` x k z = k (f z x)
  foldr1 f xs = fromMaybe (abort "foldr1: empty structure")
                  (foldr mf Nothing xs)
    where
      mf x Nothing = Just x
      mf x (Just y) = Just (f x y)
  foldl1 f xs = fromMaybe (abort "foldl1: empty structure")
                  (foldl mf Nothing xs)
    where
      mf Nothing y = Just y
      mf (Just x) y = Just (f x y)

instance Traversable (Entry p) where
  traverse f (Entry p a) = fmap (\x -> Entry p x) (f a)
  sequenceA f = traverse id f
  mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
  sequence x = mapM id x

instance == (Entry p a) | == p where
  (==) e1 e2 = on (==) priority e1 e2

instance < (Entry p a) | < p where
  (<) e1 e2 = on (<) priority e1 e2
