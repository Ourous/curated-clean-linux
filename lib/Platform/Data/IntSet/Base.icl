// --------------------------------------------------------------------------
// |
// Module      :  Data.IntSet.Base
// Copyright   :  (c) Daan Leijen 2002
//                (c) Joachim Breitner 2011
// License     :  BSD-style
// Maintainer  :  libraries@haskell.org
// Stability   :  provisional
// Portability :  portable
//
// = WARNING
//
// This module is considered __internal__.
//
// The Package Versioning Policy __does not apply__.
//
// This contents of this module may change __in any way whatsoever__
// and __without any warning__ between minor versions of this package.
//
// Authors importing this module are expected to track development
// closely.
//
// = Description
//
// An efficient implementation of integer sets.
//
// These modules are intended to be imported qualified, to avoid name
// clashes with Prelude functions, e.g.
//
// >  import Data.IntSet (IntSet)
// >  import qualified Data.IntSet as IntSet
//
// The implementation is based on /big-endian patricia trees/.  This data
// structure performs especially well on binary operations like 'union'
// and 'intersection'.  However, my benchmarks show that it is also
// (much) faster on insertions and deletions when compared to a generic
// size-balanced set implementation (see "Data.Set").
//
//    * Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
//      Workshop on ML, September 1998, pages 77-86,
//      <http://citeseer.ist.psu.edu/okasaki98fast.html>
//
//    * D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve
//      Information Coded In Alphanumeric/\", Journal of the ACM, 15(4),
//      October 1968, pages 514-534.
//
// Additionally, this implementation places bitmaps in the leaves of the tree.
// Their size is the natural size of a machine word (32 or 64 bits) and greatly
// reduce memory footprint and execution times for dense sets, e.g. sets where
// it is likely that many values lie close to each other. The asymptotics are
// not affected by this optimization.
//
// Many operations have a worst-case complexity of /O(min(n,W))/.
// This means that the operation can become linear in the number of
// elements with a maximum of /W/ -- the number of bits in an 'Int`
// (32 or 64).
// --------------------------------------------------------------------------

// [Note: INLINE bit fiddling]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~
// It is essential that the bit fiddling functions like mask, zero, branchMask
// etc are inlined. If they do not, the memory allocation skyrockets. The GHC
// usually gets it right, but it is disastrous if it does not. Therefore we
// explicitly mark these functions INLINE.


// [Note: Local 'go' functions and capturing]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Care must be taken when using 'go' function which captures an argument.
// Sometimes (for example when the argument is passed to a data constructor,
// as in insert), GHC heap-allocates more than necessary. Therefore C-- code
// must be checked for increased allocation when creating and modifying such
// functions.


// [Note: Order of constructors]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The order of constructors of IntSet matters when considering performance.
// Currently in GHC 7.0, when type has 3 constructors, they are matched from
// the first to the last -- the best performance is achieved when the
// constructors are ordered by frequency.
// On GHC 7.0, reordering constructors from Nil | Tip | Bin to Bin | Tip | Nil
// improves the benchmark by circa 10%.

implementation module Data.IntSet.Base

import StdInt, StdBool, StdFunc, StdMisc, StdOverloaded, StdClass, StdTuple
import Data.Generics.GenLexOrd
import qualified Data.List as DL
import Data.Maybe
import Data.Monoid
//import Data.Semigroup
//import Data.Utils.BitUtil
//import Data.Utils.StrictFold
//import Data.Utils.StrictPair

// A "Nat" is a natural machine word (an unsigned Int)
:: Nat :== Int

/* ------------------------------------------------------------------
  Operators
*/
// | /O(n+m)/. See 'difference'.
//(\\) infixl 9 :: IntSet -> IntSet -> IntSet
//(\\) m1 m2 = difference m1 m2

/* ------------------------------------------------------------------
  Types
*/


instance Monoid IntSet where
    mempty  = empty

instance Semigroup IntSet where
    mappend x y = union x y

/* ------------------------------------------------------------------
  Query
*/
// | /O(1)/. Is the set empty?
null :: IntSet -> Bool
null Nil = True
null _   = False
//  INLINE null #*/

// | /O(n)/. Cardinality of the set.
size :: IntSet -> Int
size (Bin _ _ l r) = size l + size r
size (Tip _ bm) = bitcount 0 bm
size Nil = 0

// | /O(min(n,W))/. Is the value a member of the set?

// See Note: Local 'go' functions and capturing]
member :: !Key IntSet -> Bool
member x is = go is
  where
    go (Bin p m l r)
      | nomatch x p m = False
      | zero x m      = go l
      | otherwise     = go r
    go (Tip y bm) = prefixOf x == y && bitmapOf x bitand bm <> 0
    go Nil = False

// | /O(min(n,W))/. Is the element not in the set?
notMember :: !Key IntSet -> Bool
notMember k is = not (member k is)

// | /O(log n)/. Find largest element smaller than the given one.
//
// > lookupLT 3 (fromList [3, 5]) == Nothing
// > lookupLT 5 (fromList [3, 5]) == Just 3

// See Note: Local 'go' functions and capturing.
lookupLT :: !Key IntSet -> Maybe Key
lookupLT x t = case t of
    Bin _ m l r | m < 0 -> if (x >= 0) (go r l) (go Nil r)
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if (x < p) (unsafeFindMax def) (unsafeFindMax r)
                         | zero x m  = go def l
                         | otherwise = go l r
    go def (Tip kx bm) | prefixOf x > kx = Just (kx + highestBitSet bm)
                       | prefixOf x == kx && maskLT <> 0 = Just (kx + highestBitSet maskLT)
                       | otherwise = unsafeFindMax def
                       where maskLT = (bitmapOf x - 1) bitand bm
    go def Nil = unsafeFindMax def


// | /O(log n)/. Find smallest element greater than the given one.
//
// > lookupGT 4 (fromList [3, 5]) == Just 5
// > lookupGT 5 (fromList [3, 5]) == Nothing

// See Note: Local 'go' functions and capturing.
lookupGT :: !Key IntSet -> Maybe Key
lookupGT x t = case t of
    Bin _ m l r | m < 0 -> if (x >= 0) (go Nil l) (go l r)
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if (x < p) (unsafeFindMin l) (unsafeFindMin def)
                         | zero x m  = go r l
                         | otherwise = go def r
    go def (Tip kx bm) | prefixOf x < kx = Just (kx + lowestBitSet bm)
                       | prefixOf x == kx && maskGT <> 0 = Just (kx + lowestBitSet maskGT)
                       | otherwise = unsafeFindMin def
                       where maskGT = (0 - ((bitmapOf x) << 1)) bitand bm
    go def Nil = unsafeFindMin def


// | /O(log n)/. Find largest element smaller or equal to the given one.
//
// > lookupLE 2 (fromList [3, 5]) == Nothing
// > lookupLE 4 (fromList [3, 5]) == Just 3
// > lookupLE 5 (fromList [3, 5]) == Just 5

// See Note: Local 'go' functions and capturing.
lookupLE :: !Key IntSet -> Maybe Key
lookupLE x t = case t of
    Bin _ m l r | m < 0 -> if (x >= 0) (go r l) (go Nil r)
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if (x < p) (unsafeFindMax def) (unsafeFindMax r)
                         | zero x m  = go def l
                         | otherwise = go l r
    go def (Tip kx bm) | prefixOf x > kx = Just (kx + highestBitSet bm)
                       | prefixOf x == kx && maskLE <> 0 = Just (kx + highestBitSet maskLE)
                       | otherwise = unsafeFindMax def
                       where maskLE = (((bitmapOf x) << 1) - 1) bitand bm
    go def Nil = unsafeFindMax def


// | /O(log n)/. Find smallest element greater or equal to the given one.
//
// > lookupGE 3 (fromList [3, 5]) == Just 3
// > lookupGE 4 (fromList [3, 5]) == Just 5
// > lookupGE 6 (fromList [3, 5]) == Nothing

// See Note: Local 'go' functions and capturing.
lookupGE :: !Key IntSet -> Maybe Key
lookupGE x t = case t of
    Bin _ m l r | m < 0 -> if (x >= 0) (go Nil l) (go l r)
    _ -> go Nil t
  where
    go def (Bin p m l r) | nomatch x p m = if (x < p) (unsafeFindMin l) (unsafeFindMin def)
                         | zero x m  = go r l
                         | otherwise = go def r
    go def (Tip kx bm) | prefixOf x < kx = Just (kx + lowestBitSet bm)
                       | prefixOf x == kx && maskGE <> 0 = Just (kx + lowestBitSet maskGE)
                       | otherwise = unsafeFindMin def
                       where maskGE = (0 - (bitmapOf x)) bitand bm
    go def Nil = unsafeFindMin def



// Helper function for lookupGE and lookupGT. It assumes that if a Bin node is
// given, it has m > 0.
unsafeFindMin :: IntSet -> Maybe Key
unsafeFindMin Nil = Nothing
unsafeFindMin (Tip kx bm) = Just (kx + lowestBitSet bm)
unsafeFindMin (Bin _ _ l _) = unsafeFindMin l

// Helper function for lookupLE and lookupLT. It assumes that if a Bin node is
// given, it has m > 0.
unsafeFindMax :: IntSet -> Maybe Key
unsafeFindMax Nil = Nothing
unsafeFindMax (Tip kx bm) = Just (kx + highestBitSet bm)
unsafeFindMax (Bin _ _ _ r) = unsafeFindMax r

/* ------------------------------------------------------------------
  Construction
*/
// | /O(1)/. The empty set.
empty :: IntSet
empty
  = Nil
//  INLINE empty #*/

// | /O(1)/. A set of one element.
singleton :: Key -> IntSet
singleton x
  = Tip (prefixOf x) (bitmapOf x)
//  INLINE singleton #*/

/* ------------------------------------------------------------------
  Insert
*/
// | /O(min(n,W))/. Add a value to the set. There is no left- or right bias for
// IntSets.
insert :: !Key IntSet -> IntSet
insert x s = insertBM (prefixOf x) (bitmapOf x) s

// Helper function for insert and union.
insertBM :: !Prefix !BitMap IntSet -> IntSet
insertBM kx bm t=:(Bin p m l r)
  | nomatch kx p m = link kx (Tip kx bm) p t
  | zero kx m      = Bin p m (insertBM kx bm l) r
  | otherwise      = Bin p m l (insertBM kx bm r)
insertBM kx bm t=:(Tip kx` bm`)
  | kx` == kx = Tip kx` (bm bitor bm`)
  | otherwise = link kx (Tip kx bm) kx` t
insertBM kx bm Nil = Tip kx bm

// | /O(min(n,W))/. Delete a value in the set. Returns the
// original set when the value was not present.
delete :: Key IntSet -> IntSet
delete x s = deleteBM (prefixOf x) (bitmapOf x) s

// Deletes all values mentioned in the BitMap from the set.
// Helper function for delete and difference.
deleteBM :: Prefix BitMap IntSet -> IntSet
deleteBM kx bm t=:(Bin p m l r)
  | nomatch kx p m = t
  | zero kx m      = bin p m (deleteBM kx bm l) r
  | otherwise      = bin p m l (deleteBM kx bm r)
deleteBM kx bm t=:(Tip kx` bm`)
  | kx` == kx = tip kx (bm` bitand bitnot bm)
  | otherwise = t
deleteBM _ _ Nil = Nil


/* ------------------------------------------------------------------
  Union
*/
// | The union of a list of sets.
unions :: [IntSet] -> IntSet
unions xs
  = 'DL'.foldl union empty xs


// | /O(n+m)/. The union of two sets.
union :: IntSet IntSet -> IntSet
union t1=:(Bin p1 m1 l1 r1) t2=:(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = link p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = link p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = link p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union t=:(Bin _ _ _ _) (Tip kx bm) = insertBM kx bm t
union t=:(Bin _ _ _ _) Nil = t
union (Tip kx bm) t = insertBM kx bm t
union Nil t = t


/* ------------------------------------------------------------------
  Difference
*/
// | /O(n+m)/. Difference between two sets.
difference :: IntSet IntSet -> IntSet
difference t1=:(Bin p1 m1 l1 r1) t2=:(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (difference l1 t2) r1
                | otherwise         = bin p1 m1 l1 (difference r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = difference t1 l2
                | otherwise         = difference t1 r2

difference t=:(Bin _ _ _ _) (Tip kx bm) = deleteBM kx bm t
difference t=:(Bin _ _ _ _) Nil = t

difference t1=:(Tip kx bm) t2 = differenceTip t2
  where differenceTip (Bin p2 m2 l2 r2) | nomatch kx p2 m2 = t1
                                        | zero kx m2 = differenceTip l2
                                        | otherwise = differenceTip r2
        differenceTip (Tip kx2 bm2) | kx == kx2 = tip kx (bm bitand bitnot bm2)
                                    | otherwise = t1
        differenceTip Nil = t1

difference Nil _     = Nil



/* ------------------------------------------------------------------
  Intersection
*/
// | /O(n+m)/. The intersection of two sets.
intersection :: IntSet IntSet -> IntSet
intersection t1=:(Bin p1 m1 l1 r1) t2=:(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersection l1 l2) (intersection r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersection l1 t2
                  | otherwise         = intersection r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersection t1 l2
                  | otherwise         = intersection t1 r2

intersection t1=:(Bin _ _ _ _) (Tip kx2 bm2) = intersectBM t1
  where intersectBM (Bin p1 m1 l1 r1) | nomatch kx2 p1 m1 = Nil
                                      | zero kx2 m1       = intersectBM l1
                                      | otherwise         = intersectBM r1
        intersectBM (Tip kx1 bm1) | kx1 == kx2 = tip kx1 (bm1 bitand bm2)
                                  | otherwise = Nil
        intersectBM Nil = Nil

intersection (Bin _ _ _ _) Nil = Nil

intersection (Tip kx1 bm1) t2 = intersectBM t2
  where intersectBM (Bin p2 m2 l2 r2) | nomatch kx1 p2 m2 = Nil
                                      | zero kx1 m2       = intersectBM l2
                                      | otherwise         = intersectBM r2
        intersectBM (Tip kx2 bm2) | kx1 == kx2 = tip kx1 (bm1 bitand bm2)
                                  | otherwise = Nil
        intersectBM Nil = Nil

intersection Nil _ = Nil

/* ------------------------------------------------------------------
  Subset
*/
// | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: IntSet IntSet -> Bool
isProperSubsetOf t1 t2
  = case subsetCmp t1 t2 of
      LT -> True
      _  -> False

subsetCmp :: IntSet IntSet -> LexOrd
subsetCmp t1=:(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = case subsetCmpLt of
                       GT -> GT
                       _  -> LT
  | p1 == p2       = subsetCmpEq
  | otherwise      = GT  // disjoint
  where
    subsetCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = subsetCmp t1 l2
                | otherwise         = subsetCmp t1 r2
    subsetCmpEq = case (subsetCmp l1 l2, subsetCmp r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

subsetCmp (Bin _ _ _ _) _  = GT
subsetCmp (Tip kx1 bm1) (Tip kx2 bm2)
  | kx1 <> kx2                  = GT // disjoint
  | bm1 == bm2                  = EQ
  | bm1 bitand bitnot bm2 == 0 = LT
  | otherwise                   = GT
subsetCmp t1=:(Tip kx _) (Bin p m l r)
  | nomatch kx p m = GT
  | zero kx m      = case subsetCmp t1 l of GT -> GT ; _ -> LT
  | otherwise      = case subsetCmp t1 r of GT -> GT ; _ -> LT
subsetCmp (Tip _ _) Nil = GT // disjoint
subsetCmp Nil Nil = EQ
subsetCmp Nil _   = LT

// | /O(n+m)/. Is this a subset?
// =:(s1 `isSubsetOf` s2)=: tells whether =:s1=: is a subset of =:s2=:.

isSubsetOf :: IntSet IntSet -> Bool
isSubsetOf t1=:(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if (zero p1 m2) (isSubsetOf t1 l2)
                                                        (isSubsetOf t1 r2))
  | otherwise      = (p1==p2) && isSubsetOf l1 l2 && isSubsetOf r1 r2
isSubsetOf (Bin _ _ _ _) _  = False
isSubsetOf (Tip kx1 bm1) (Tip kx2 bm2) = kx1 == kx2 && bm1 bitand bitnot bm2 == 0
isSubsetOf t1=:(Tip kx _) (Bin p m l r)
  | nomatch kx p m = False
  | zero kx m      = isSubsetOf t1 l
  | otherwise      = isSubsetOf t1 r
isSubsetOf (Tip _ _) Nil = False
isSubsetOf Nil _         = True


/* ------------------------------------------------------------------
  Filter
*/
// | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (Key -> Bool) IntSet -> IntSet
filter predicate t
  = case t of
      Bin p m l r
        -> bin p m (filter predicate l) (filter predicate r)
      Tip kx bm
        -> tip kx (foldl`Bits 0 (bitPred kx) 0 bm)
      Nil -> Nil
  where bitPred kx bm bi | predicate (kx + bi) = bm bitor bitmapOfSuffix bi
                         | otherwise           = bm
        /*# INLINE bitPred #*/

// | /O(n)/. partition the set according to some predicate.
partition :: (Key -> Bool) IntSet -> (IntSet,IntSet)
partition predicate0 t0 = go predicate0 t0
  where
    go predicate t
      = case t of
          Bin p m l r
            -> let (l1, l2) = go predicate l
                   (r1, r2) = go predicate r
               in (bin p m l1 r1, bin p m l2 r2)
          Tip kx bm
            -> let bm1 = foldl`Bits 0 (bitPred kx) 0 bm
               in  (tip kx bm1, tip kx (bm bitxor bm1))
          Nil -> (Nil, Nil)
      where bitPred kx bm bi | predicate (kx + bi) = bm bitor bitmapOfSuffix bi
                             | otherwise           = bm
            /*# INLINE bitPred #*/


// | /O(min(n,W))/. The expression (=:'split` x set=:) is a pair =:(set1,set2)=:
// where =:set1=: comprises the elements of =:set=: less than =:x=: and =:set2=:
// comprises the elements of =:set=: greater than =:x=:.
//
// > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: Key IntSet -> (IntSet,IntSet)
split x t =
  case t of
      Bin _ m l r
          | m < 0 -> if (x >= 0)  // handle negative numbers.
                       (case go x l of (lt, gt) -> let lt` = union lt r
                                                   in (lt`, gt))
                       (case go x r of (lt, gt) -> let gt` = union gt l
                                                   in (lt, gt`))
      _ -> go x t
  where
    go x` t`=:(Bin p m l r)
        | match x` p m = if (zero x` m)
                           (case go x` l of
                             (lt, gt) -> (lt, union gt r))
                           (case go x` r of
                             (lt, gt) -> (union lt l, gt))
        | otherwise   = if (x` < p) (Nil, t`) (t`, Nil)
    go x` t`=:(Tip kx` bm)
        | kx` > x`          = (Nil, t`)
          // equivalent to kx` > prefixOf x`
        | kx` < prefixOf x` = (t`, Nil)
        | otherwise = (tip kx` (bm bitand lowerBitmap), tip kx` (bm bitand higherBitmap))
            where lowerBitmap = bitmapOf x` - 1
                  higherBitmap = bitnot (lowerBitmap + bitmapOf x`)
    go _ Nil = (Nil, Nil)

// | /O(min(n,W))/. Performs a 'split` but also returns whether the pivot
// element was found in the original set.
splitMember :: Key IntSet -> (IntSet,Bool,IntSet)
splitMember x t =
  case t of
      Bin _ m l r | m < 0 -> if (x >= 0)
                               (case go x l of
                                 (lt, fnd, gt) -> let lt` = union lt r
                                                  in (lt`, fnd, gt))
                               (case go x r of
                                 (lt, fnd, gt) -> let gt` = union gt l
                                                  in (lt, fnd, gt`))
      _ -> go x t
  where
    go x` t`=:(Bin p m l r)
        | match x` p m = if (zero x` m)
                           (case go x` l of
                             (lt, fnd, gt) -> (lt, fnd, union gt r))
                           (case go x` r of
                             (lt, fnd, gt) -> (union lt l, fnd, gt))
        | otherwise   = if (x` < p) (Nil, False, t`) (t`, False, Nil)
    go x` t`=:(Tip kx` bm)
        | kx` > x`          = (Nil, False, t`)
          // equivalent to kx` > prefixOf x`
        | kx` < prefixOf x` = (t`, False, Nil)
        | otherwise = let lt = tip kx` (bm bitand lowerBitmap)
                          found = (bm bitand bitmapOfx`) <> 0
                          gt = tip kx` (bm bitand higherBitmap)
                      in (lt, found, gt)
            where bitmapOfx` = bitmapOf x`
                  lowerBitmap = bitmapOfx` - 1
                  higherBitmap = bitnot (lowerBitmap + bitmapOfx`)
    go _ Nil = (Nil, False, Nil)

/* --------------------------------------------------------------------
  Min/Max
*/

// | /O(min(n,W))/. Retrieves the maximal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
maxView :: IntSet -> Maybe (Key, IntSet)
maxView t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go l of (result, l`) -> Just (result, bin p m l` r)
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go r of (result, r`) -> (result, bin p m l r`)
    go (Tip kx bm) = case highestBitSet bm of bi -> (kx + bi, tip kx (bm bitand bitnot (bitmapOfSuffix bi)))
    go Nil = abort "maxView Nil"

// | /O(min(n,W))/. Retrieves the minimal key of the set, and the set
// stripped of that element, or 'Nothing' if passed an empty set.
minView :: IntSet -> Maybe (Key, IntSet)
minView t =
  case t of Nil -> Nothing
            Bin p m l r | m < 0 -> case go r of (result, r`) -> Just (result, bin p m l r`)
            _ -> Just (go t)
  where
    go (Bin p m l r) = case go l of (result, l`) -> (result, bin p m l` r)
    go (Tip kx bm) = case lowestBitSet bm of bi -> (kx + bi, tip kx (bm bitand bitnot (bitmapOfSuffix bi)))
    go Nil = abort "minView Nil"

// | /O(min(n,W))/. Delete and find the minimal element.
//
// > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: IntSet -> (Key, IntSet)
deleteFindMin s = (fromMaybe (abort "deleteFindMin: empty set has no minimal element") o minView) s

// | /O(min(n,W))/. Delete and find the maximal element.
//
// > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: IntSet -> (Key, IntSet)
deleteFindMax s = (fromMaybe (abort "deleteFindMax: empty set has no maximal element") o maxView) s


// | /O(min(n,W))/. The minimal element of the set.
findMin :: IntSet -> Key
findMin Nil = abort "findMin: empty set has no minimal element"
findMin (Tip kx bm) = kx + lowestBitSet bm
findMin (Bin _ m l r)
  |   m < 0   = find r
  | otherwise = find l
    where find (Tip kx bm) = kx + lowestBitSet bm
          find (Bin _ _ l` _) = find l`
          find Nil            = abort "findMin Nil"

// | /O(min(n,W))/. The maximal element of a set.
findMax :: IntSet -> Key
findMax Nil = abort "findMax: empty set has no maximal element"
findMax (Tip kx bm) = kx + highestBitSet bm
findMax (Bin _ m l r)
  |   m < 0   = find l
  | otherwise = find r
    where find (Tip kx bm) = kx + highestBitSet bm
          find (Bin _ _ _ r`) = find r`
          find Nil            = abort "findMax Nil"


// | /O(min(n,W))/. Delete the minimal element. Returns an empty set if the set is empty.
//
// Note that this is a change of behaviour for consistency with 'Data.Set.Set` &#8211;
// versions prior to 0.5 threw an error if the 'IntSet` was already empty.
deleteMin :: IntSet -> IntSet
deleteMin s = (maybe Nil snd o minView) s

// | /O(min(n,W))/. Delete the maximal element. Returns an empty set if the set is empty.
//
// Note that this is a change of behaviour for consistency with 'Data.Set.Set` &#8211;
// versions prior to 0.5 threw an error if the 'IntSet` was already empty.
deleteMax :: IntSet -> IntSet
deleteMax s = (maybe Nil snd o maxView) s

/* --------------------------------------------------------------------
  Map
*/

// | /O(n*min(n,W))/.
// =:'map' f s=: is the set obtained by applying =:f=: to each element of =:s=:.
//
// It`s worth noting that the size of the result may be smaller if,
// for some =:(x,y)=:, =:x \<> y && f x == f y=:

map :: (Key -> Key) IntSet -> IntSet
map f s = (fromList o 'DL'.map f o toList) s

/* ------------------------------------------------------------------
  Fold
*/
// | /O(n)/. Fold the elements in the set using the given right-associative
// binary operator. This function is an equivalent of 'foldr` and is present
// for compatibility only.
//
// /Please note that fold will be deprecated in the future and removed./
fold :: (Key b -> b) b IntSet -> b
fold f z s = foldr f z s
//  INLINE fold #*/

// | /O(n)/. Fold the elements in the set using the given right-associative
// binary operator, such that =:'foldr` f z == 'Prelude.foldr` f z . 'toAscList`=:.
//
// For example,
//
// > toAscList set = foldr (:) [] set
foldr :: (Key b -> b) b -> IntSet -> b
foldr f z = \t ->      // Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z l) r // put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip kx bm)   = foldrBits kx f z` bm
    go z` (Bin _ _ l r) = go (go z` r) l
//  INLINE foldr #*/

// | /O(n)/. A strict version of 'foldr`. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldr` :: (Key b -> b) b -> IntSet -> b
foldr` f z = \t ->      // Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z l) r // put negative numbers before
                        | otherwise -> go (go z r) l
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip kx bm)   = foldr`Bits kx f z` bm
    go z` (Bin _ _ l r) = go (go z` r) l
//  INLINE foldr` #*/

// | /O(n)/. Fold the elements in the set using the given left-associative
// binary operator, such that =:'foldl` f z == 'Prelude.foldl` f z . 'toAscList`=:.
//
// For example,
//
// > toDescList set = foldl (flip (:)) [] set
foldl :: (a Key -> a) a -> IntSet -> a
foldl f z = \t ->      // Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z r) l // put negative numbers before
                        | otherwise -> go (go z l) r
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip kx bm)   = foldlBits kx f z` bm
    go z` (Bin _ _ l r) = go (go z` l) r
//  INLINE foldl #*/

// | /O(n)/. A strict version of 'foldl`. Each application of the operator is
// evaluated before using the result in the next application. This
// function is strict in the starting value.
foldl` :: (a Key -> a) a -> IntSet -> a
foldl` f z = \t ->      // Use lambda t to be inlinable with two arguments only.
  case t of Bin _ m l r | m < 0 -> go (go z r) l // put negative numbers before
                        | otherwise -> go (go z l) r
            _ -> go z t
  where
    go z` Nil           = z`
    go z` (Tip kx bm)   = foldl`Bits kx f z` bm
    go z` (Bin _ _ l r) = go (go z` l) r
//  INLINE foldl` #*/

/* ------------------------------------------------------------------
  List variations
*/
// | /O(n)/. An alias of 'toAscList`. The elements of a set in ascending order.
// Subject to list fusion.
elems :: IntSet -> [Key]
elems s = toAscList s

/* ------------------------------------------------------------------
  Lists
*/

// | /O(n)/. Convert the set to a list of elements. Subject to list fusion.
toList :: IntSet -> [Key]
toList s = toAscList s

// | /O(n)/. Convert the set to an ascending list of elements. Subject to list
// fusion.
toAscList :: IntSet -> [Key]
toAscList s = foldr (\x xs -> [x:xs]) [] s

// | /O(n)/. Convert the set to a descending list of elements. Subject to list
// fusion.
toDescList :: IntSet -> [Key]
toDescList s = foldl (\xs x -> [x:xs]) [] s

// | /O(n*min(n,W))/. Create a set from a list of integers.
fromList :: [Key] -> IntSet
fromList xs
  = 'DL'.foldl ins empty xs
  where
    ins t x  = insert x t

// | /O(n)/. Build a set from an ascending list of elements.
// /The precondition (input list is ascending) is not checked./
fromAscList :: [Key] -> IntSet
fromAscList [] = Nil
fromAscList [x0 : xs0] = fromDistinctAscList (combineEq x0 xs0)
  where
    combineEq x` [] = [x`]
    combineEq x` [x:xs]
      | x==x`     = combineEq x` xs
      | otherwise = [x` : combineEq x xs]

// | /O(n)/. Build a set from an ascending list of distinct elements.
// /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [Key] -> IntSet
fromDistinctAscList []         = Nil
fromDistinctAscList [z0 : zs0] = work (prefixOf z0) (bitmapOf z0) zs0 Nada
  where
    // 'work' accumulates all values that go into one tip, before passing this Tip
    // to 'reduce'
    work kx bm []     stk = finish kx (Tip kx bm) stk
    work kx bm [z:zs] stk | kx == prefixOf z = work kx (bm bitor bitmapOf z) zs stk
    work kx bm [z:zs] stk = reduce z zs (branchMask z kx) kx (Tip kx bm) stk

    reduce z zs _ px tx Nada = work (prefixOf z) (bitmapOf z) zs (Push px tx Nada)
    reduce z zs m px tx stk=:(Push py ty stk`) =
        let mxy = branchMask px py
            pxy = mask px mxy
        in  if (shorter m mxy)
                 (reduce z zs m pxy (Bin pxy mxy ty tx) stk`)
                 (work (prefixOf z) (bitmapOf z) zs (Push px tx stk))

    finish _  t  Nada = t
    finish px tx (Push py ty stk) = finish p (link py ty px tx) stk
        where m = branchMask px py
              p = mask px m

:: Stack = Push !Prefix !IntSet !Stack | Nada


/* ------------------------------------------------------------------
  Eq
*/
instance == IntSet where
  (==) t1 t2  = equal t1 t2

equal :: IntSet IntSet -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx1 bm1) (Tip kx2 bm2)
  = kx1 == kx2 && bm1 == bm2
equal Nil Nil = True
equal _   _   = False

nequal :: IntSet IntSet -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 <> m2) || (p1 <> p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx1 bm1) (Tip kx2 bm2)
  = kx1 <> kx2 || bm1 <> bm2
nequal Nil Nil = False
nequal _   _   = True

/* ------------------------------------------------------------------
  Ord TODO
*/

//instance Ord IntSet where
    //compare s1 s2 = compare (toAscList s1) (toAscList s2)
    // tentative implementation. See if more efficient exists.

/* ------------------------------------------------------------------
  Show TODO
*/
//instance Show IntSet where
  //showsPrec p xs = showParen (p > 10) $
    //showString "fromList " . shows (toList xs)

/* ------------------------------------------------------------------
  Debugging
*/
// | /O(n)/. Show the tree that implements the set. The tree is shown
// in a compressed, hanging format.
//showTree :: IntSet -> String
//showTree s = showTreeWith True False s


/* | /O(n)/. The expression (=:'showTreeWith' hang wide map=:) shows
 the tree that implements the set. If =:hang=: is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 =:wide=: is 'True', an extra wide version is shown.
*/
//showTreeWith :: Bool Bool IntSet -> String
//showTreeWith hang wide t
  //| hang      = (showsTreeHang wide [] t) ""
  //| otherwise = (showsTree wide [] [] t) ""

//showsTree :: Bool [String] [String] IntSet -> ShowS
//showsTree wide lbars rbars t
  //= case t of
      //Bin p m l r
          //-> showsTree wide (withBar rbars) (withEmpty rbars) r o
             //showWide wide rbars o
             //showsBars lbars o showString (showBin p m) o showString "\n" .
             //showWide wide lbars .
             //showsTree wide (withEmpty lbars) (withBar lbars) l
      //Tip kx bm
          //-> showsBars lbars o showString " " o shows kx o showString " + " o
                                                //showsBitMap bm o showString "\n"
      //Nil -> showsBars lbars o showString "|\n"

//showsTreeHang :: Bool [String] IntSet -> ShowS
//showsTreeHang wide bars t
  //= case t of
      //Bin p m l r
          //-> showsBars bars o showString (showBin p m) o showString "\n" o
             //showWide wide bars o
             //showsTreeHang wide (withBar bars) l o
             //showWide wide bars o
             //showsTreeHang wide (withEmpty bars) r
      //Tip kx bm
          //-> showsBars bars o showString " " o shows kx o showString " + " o
                                               //showsBitMap bm o showString "\n"
      //Nil -> showsBars bars i showString "|\n"

//showBin :: Prefix Mask -> String
//showBin _ _
  //= "*" -- ++ show (p,m)

//showWide :: Bool [String] String -> String
//showWide wide bars
  //| wide      = showString (concat (reverse bars)) o showString "|\n"
  //| otherwise = id

//showsBars :: [String] -> ShowS
//showsBars [] = id
//showsBars bars = showString (concat (reverse (tail bars))) o showString node

//showsBitMap :: Word -> ShowS
//showsBitMap = showString o showBitMap

//showBitMap :: Word -> String
//showBitMap w = show (foldrBits 0 (\x xs -> [x:xs]) [] w)

//node :: String
//node           = "+--"

//withBar, withEmpty :: [String] -> [String]
//withBar bars   = "|  ":bars
//withEmpty bars = "   ":bars


/* ------------------------------------------------------------------
  Helpers
*/
/* ------------------------------------------------------------------
  Link
*/
link :: Prefix IntSet Prefix IntSet -> IntSet
link p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m
//  INLINE link #*/

/* ------------------------------------------------------------------
  =:bin=: assures that we never have empty trees within a tree.
*/
bin :: Prefix Mask IntSet IntSet -> IntSet
bin _ _ l Nil = l
bin _ _ Nil r = r
bin p m l r   = Bin p m l r
//  INLINE bin #*/

/* ------------------------------------------------------------------
  =:tip=: assures that we never have empty bitmaps within a tree.
*/
tip :: Prefix BitMap -> IntSet
tip _ 0 = Nil
tip kx bm = Tip kx bm
//  INLINE tip #*/


/* --------------------------------------------------------------------
  Functions that generate Prefix and BitMap of a Key or a Suffix.
*/

//suffixBitMask :: Int
suffixBitMask :== IF_INT_64_OR_32 64 32

//prefixBitMask :: Int
prefixBitMask :== bitnot suffixBitMask

prefixOf :: Int -> Prefix
prefixOf x = x bitand prefixBitMask
//  INLINE prefixOf #*/

suffixOf :: Int -> Int
suffixOf x = x bitand suffixBitMask
//  INLINE suffixOf #*/

bitmapOfSuffix :: Int -> BitMap
bitmapOfSuffix s = 1 << s
//  INLINE bitmapOfSuffix #*/

bitmapOf :: Int -> BitMap
bitmapOf x = bitmapOfSuffix (suffixOf x)
//  INLINE bitmapOf #*/


/* ------------------------------------------------------------------
  Endian independent bit twiddling
*/
zero :: Int Mask -> Bool
zero i m = (i) bitand (m) == 0
//  INLINE zero #*/

nomatch :: Int Prefix Mask -> Bool
nomatch i p m
  = (mask i m) <> p
//  INLINE nomatch #*/

match :: Int Prefix Mask -> Bool
match i p m
  = (mask i m) == p
//  INLINE match #*/

// Suppose a is largest such that 2^a divides 2*m.
// Then mask i m is i with the low a bits zeroed out.
mask :: Int Mask -> Prefix
mask i m = maskW (i) (m)
//  INLINE mask #*/

/* ------------------------------------------------------------------
  Big endian operations
*/
maskW :: Nat Nat -> Prefix
maskW i m =  (i bitand (bitnot (m-1) bitxor m))
//  INLINE maskW #*/

shorter :: Mask Mask -> Bool
shorter m1 m2 = (m1) > (m2)
//  INLINE shorter #*/

branchMask :: Prefix Prefix -> Mask
branchMask p1 p2 =  (highestBitMask (p1 bitxor p2))
//  INLINE branchMask #*/

/* --------------------------------------------------------------------
  To get best performance, we provide fast implementations of
  lowestBitSet, highestBitSet and fold[lr][l]Bits for GHC.
  If the intel bsf and bsr instructions ever become GHC primops,
  this code should be reimplemented using these.

  Performance of this code is crucial for folds, toList, filter, partition.

  The signatures of methods in question are placed after this comment.
*/


//  INLINE lowestBitSet #*/
//  INLINE highestBitSet #*/
//  INLINE foldlBits #*/
//  INLINE foldl`Bits #*/
//  INLINE foldrBits #*/
//  INLINE foldr`Bits #*/

/* --------------------------------------------------------------------
  In general case we use logarithmic implementation of
  lowestBitSet and highestBitSet, which works up to bit sizes of 64.

  Folds are linear scans.
*/

lowestBitSet n0 =
    let (n1,b1) = if (n0 bitand 0xFFFFFFFF <> 0) (n0, 0)  (n0 >> 32, 32)
        (n2,b2) = if (n1 bitand 0xFFFF <> 0    ) (n1, b1) (n1 >> 16, 16 + b1)
        (n3,b3) = if (n2 bitand 0xFF <> 0      ) (n2, b2) (n2 >> 8,  8  + b2)
        (n4,b4) = if (n3 bitand 0xF <> 0       ) (n3, b3) (n3 >> 4,  4  + b3)
        (n5,b5) = if (n4 bitand 0x3 <> 0       ) (n4, b4) (n4 >> 2,  2  + b4)
        b6      = if (n5 bitand 0x1 <> 0       )      b5  (          1  + b5)
    in b6

highestBitSet n0 =
    let (n1,b1) = if (n0 bitand 0xFFFFFFFF00000000 <> 0) (n0 >> 32, 32)      (n0,0)
        (n2,b2) = if (n1 bitand 0xFFFF0000 <> 0        ) (n1 >> 16, 16 + b1) (n1,b1)
        (n3,b3) = if (n2 bitand 0xFF00 <> 0            ) (n2 >> 8,  8  + b2) (n2,b2)
        (n4,b4) = if (n3 bitand 0xF0 <> 0              ) (n3 >> 4,  4  + b3) (n3,b3)
        (n5,b5) = if (n4 bitand 0xC <> 0               ) (n4 >> 2,  2  + b4) (n4,b4)
        b6      = if (n5 bitand 0x2 <> 0               ) (          1  + b5)     b5
    in b6

foldlBits prefix f z bm = let lb = lowestBitSet bm
                          in  go (prefix+lb) z (bm >> lb)
  where go _ acc 0 = acc
        go bi acc n | testBit n 0 = go (bi + 1) (f acc bi) (n >> 1)
                    | otherwise     = go (bi + 1)    acc     (n >> 1)

foldl`Bits prefix f z bm = let lb = lowestBitSet bm
                           in  go (prefix+lb) z (bm >> lb)
  where go _ acc 0 = acc
        go bi acc n | testBit n 0 = go (bi + 1) (f acc bi) (n >> 1)
                    | otherwise     = go (bi + 1)    acc     (n >> 1)

foldrBits prefix f z bm = let lb = lowestBitSet bm
                          in  go (prefix+lb) (bm >> lb)
  where go _ 0 = z
        go bi n | testBit n 0 = f bi (go (bi + 1) (n >> 1))
                | otherwise     =       go (bi + 1) (n >> 1)

foldr`Bits prefix f z bm = let lb = lowestBitSet bm
                           in  go (prefix+lb) (bm >> lb)
  where
        go _ 0 = z
        go bi n | testBit n 0 = f bi (go (bi + 1) (n >> 1))
                | otherwise     =         go (bi + 1) (n >> 1)


/* --------------------------------------------------------------------
  [bitcount] as posted by David F. Place to haskell-cafe on April 11, 2006,
  based on the code on
  http://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetKernighan,
  where the following source is given:
    Published in 1988, the C Programming Language 2nd Ed. (by Brian W.
    Kernighan and Dennis M. Ritchie) mentions this in exercise 2-9. On April
    19, 2006 Don Knuth pointed out to me that this method "was first published
    by Peter Wegner in CACM 3 (1960), 322. (Also discovered independently by
    Derrick Lehmer and published in 1964 in a book edited by Beckenbach.)"
-------------------------------------------------------------------- */

bitcount :: Int Int -> Int
bitcount a0 x0 = go a0 x0
  where go a 0 = a
        go a x = go (a + 1) (x bitand (x-1))
//  INLINE bitcount #*/

// TODO Test
testBit x i = (x bitand i) <> 0

// The highestBitMask implementation is based on
// http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
// which has been put in the public domain.

// | Return a word where only the highest bit is set.
highestBitMask :: Int -> Int
highestBitMask x1 = let x2 = x1 bitor x1 >> 1
                        x3 = x2 bitor x2 >> 2
                        x4 = x3 bitor x3 >> 4
                        x5 = x4 bitor x4 >> 8
                        x6 = x5 bitor x5 >> 16
                     in x6 bitxor  (x6 >> 1)

/* ------------------------------------------------------------------
  Utilities
------------------------------------------------------------------- */

// | /O(1)/.  Decompose a set into pieces based on the structure of the underlying
// tree.  This function is useful for consuming a set in parallel.
//
// No guarantee is made as to the sizes of the pieces; an internal, but
// deterministic process determines this.  However, it is guaranteed that the
// pieces returned will be in ascending order (all elements in the first submap
// less than all elements in the second, and so on).
//
// Examples:
//
// > splitRoot (fromList [1..120]) == [fromList [1..63],fromList [64..120]]
// > splitRoot empty == []
//
//  Note that the current implementation does not return more than two subsets,
//  but you should not depend on this behaviour because it can change in the
//  future without notice. Also, the current version does not continue
//  splitting all the way to individual singleton sets -- it stops at some
//  point.
splitRoot :: IntSet -> [IntSet]
splitRoot Nil = []
// NOTE: we don't currently split below Tip, but we could.
splitRoot x=:(Tip _ _) = [x]
splitRoot (Bin _ m l r) | m < 0 = [r, l]
                        | otherwise = [l, r]
//  INLINE splitRoot #*/

