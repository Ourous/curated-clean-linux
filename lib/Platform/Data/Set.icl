implementation module Data.Set

import StdClass, StdMisc, StdBool, StdFunc, StdInt, StdTuple
import Data.Maybe, Data.GenEq, Data.GenLexOrd, Data.Monoid
from Data.Foldable import class Foldable (..)
import Data.Func
import qualified StdList
from StdList import instance == [a]

mapSetMonotonic :: !(a -> b) !(Set a) -> Set b
mapSetMonotonic _ Tip = Tip
mapSetMonotonic f (Bin n x l r) = Bin n (f x) (mapSetMonotonic f l) (mapSetMonotonic f r)

/*
 * Sets are size balanced trees.
 * A set of values @a@.
 */
:: Set a = Tip
         | Bin !Int !a !(Set a) !(Set a)

instance == (Set a) | == a where
  (==) :: !(Set a) !(Set a) -> Bool | == a
  (==) t1 t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

instance < (Set a) | < a
where
	(<) :: !(Set a) !(Set a) -> Bool | < a
	(<) s1 s2 = compare (toAscList s1) (toAscList s2)
	where
		compare :: ![a] ![a] -> Bool | < a
		compare []     [] = False
		compare []     _  = True
		compare [_:_]  [] = False
		compare [a:as] [b:bs]
			| a < b     = True
			| a > b     = False
			| otherwise = compare as bs

gEq{|Set|} eEq x y = (size x == size y) && gEq{|* -> *|} eEq (toAscList x) (toAscList y)
gLexOrd{|Set|} eLexOrd x y = gLexOrd{|* -> *|} eLexOrd (toAscList x) (toAscList y)

instance Foldable Set where
	foldr _ z Tip           = z
	foldr f z (Bin _ x l r) = foldr f (f x (foldr f z r)) l

/*--------------------------------------------------------------------
 * Query
 *--------------------------------------------------------------------*/

member :: !a !(Set a) -> Bool | < a
member x Tip = False
member x (Bin _ y l r)
  | x < y     = member x l
  | x > y     = member x r
  | otherwise = True

/*--------------------------------------------------------------------
 * Construction
 *--------------------------------------------------------------------*/
 
newSet :: Set a
newSet = Tip

singleton :: !u:a -> w:(Set u:a), [w <= u]
singleton x = Bin 1 x Tip Tip

/*--------------------------------------------------------------------
 * Insertion, Deletion
 *--------------------------------------------------------------------*/

insert :: !a !.(Set a) -> Set a | < a
insert x Tip = singleton x
insert x (Bin sz y l r)
  | x < y     = balanceL y (insert x l) r
  | x > y     = balanceR y l (insert x r)
  | otherwise = Bin sz x l r

insertR :: !a !(Set a) -> Set a | < a
insertR x Tip = singleton x
insertR x t=:(Bin _ y l r)
  | x < y     = balanceL y (insertR x l) r
  | x > y     = balanceR y l (insertR x r)
  | otherwise = t

delete :: !a !.(Set a) -> Set a | < a
delete x Tip = Tip
delete x (Bin _ y l r)
  | x < y     = balanceR y (delete x l) r
  | x > y     = balanceL y l (delete x r)
  | otherwise = glue l r

/*--------------------------------------------------------------------
 * Subset
 *--------------------------------------------------------------------*/

isSubsetOfX :: !(Set a) !(Set a) -> Bool | < a
isSubsetOfX Tip _ = True
isSubsetOfX _ Tip = False
isSubsetOfX (Bin _ x l r) t
  #! (lt, found, gt) = splitMember x t
  = found && isSubsetOfX l lt && isSubsetOfX r gt
isSubsetOfX _ _ = abort "error in isSubsetOfX\n"

/*--------------------------------------------------------------------
 * Minimal, Maximal
 *--------------------------------------------------------------------*/
 
findMin :: !(Set a) -> a
findMin (Bin _ x Tip _) = x
findMin (Bin _ _ l _)   = findMin l
findMin Tip             = abort "Set.findMin: empty set has no minimal element"

findMax :: !(Set a) -> a
findMax (Bin _ x _ Tip)  = x
findMax (Bin _ _ _ r)    = findMax r
findMax Tip              = abort "Set.findMax: empty set has no maximal element"

deleteMin :: !.(Set a) -> Set a
deleteMin (Bin _ _ Tip r) = r
deleteMin (Bin _ x l r)   = balanceR x (deleteMin l) r
deleteMin Tip             = Tip

deleteMax :: !.(Set a) -> Set a
deleteMax (Bin _ _ l Tip) = l
deleteMax (Bin _ x l r)   = balanceL x l (deleteMax r)
deleteMax Tip             = Tip

/*--------------------------------------------------------------------
 * Union. 
 *--------------------------------------------------------------------*/

union :: !u:(Set a) !u:(Set a) -> Set a | < a & == a
union t1 Tip = t1
union t1 (Bin _ x Tip Tip) = insertR x t1
union (Bin _ x Tip Tip) t2 = insert x t2
union Tip t2 = t2
union t1=:(Bin _ x l1 r1) t2 = link x l1l2 r1r2
where
	(l2,r2) = splitS x t2
	l1l2 = union l1 l2
	r1r2 = union r1 r2

splitS :: !a !(Set a) -> (!Set a, !Set a) | <, == a
splitS _ Tip = (Tip,Tip)
splitS x (Bin _ y l r)
| x < y     = let (lt,gt) = splitS x l in (lt, link y gt r)
| x > y     = let (lt,gt) = splitS x r in (link y l lt, gt)
| otherwise = (l,r)

/*--------------------------------------------------------------------
 * Difference
 *--------------------------------------------------------------------*/
 
difference :: !(Set a) !(Set a) -> Set a | < a & == a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 (Bin _ x l2 r2) = case split  x t1 of
	(l1, r1)
		| size l1l2 + size r1r2 == size t1 -> t1
		| otherwise -> merge l1l2 r1r2
	where
		l1l2 = difference l1 l2
		r1r2 = difference r1 r2

/*--------------------------------------------------------------------
 * Intersection
 *--------------------------------------------------------------------*/

intersections :: ![Set a] -> Set a | < a & == a
intersections [t] = t
intersections [t:ts] = 'StdList'.foldl intersection t ts
intersections [] = abort "intersections called with []\n"

intersection :: !(Set a) !(Set a) -> Set a | < a & == a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1 t2 = hedgeInt NothingS NothingS t1 t2

hedgeInt :: !(MaybeS a) !(MaybeS a) !(Set a) !(Set a) -> Set a | < a & == a
hedgeInt _ _ _   Tip = Tip
hedgeInt _ _ Tip _   = Tip
hedgeInt blo bhi (Bin _ x l r) t2
  #! bmi = JustS x
  #! l` = hedgeInt blo bmi l (trim blo bmi t2)
  #! r` = hedgeInt bmi bhi r (trim bmi bhi t2)
  = if (member x t2)
      (link x l` r`)
      (merge l` r`)

/*--------------------------------------------------------------------
 * Filter and partition
 *--------------------------------------------------------------------*/

filter :: !(a -> Bool) !(Set a) -> Set a | < a
filter _ Tip = Tip
filter p (Bin _ x l r)
  | p x       = link x (filter p l) (filter p r)
  | otherwise = merge (filter p l) (filter p r)

partition :: !(a -> Bool) !(Set a) -> (!Set a, !Set a) | < a
partition _ Tip = (Tip,Tip)
partition p (Bin _ x l r)
  #! (l1,l2) = partition p l
  #! (r1,r2) = partition p r
  | p x       = (link x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,link x l2 r2)

/*--------------------------------------------------------------------
 * Lists 
 *--------------------------------------------------------------------*/

fromList :: ![a] -> Set a | < a
fromList xs = 'StdList'.foldl ins newSet xs
  where
  ins :: !(Set a) !a -> Set a | < a
  ins t x = insert x t

/*--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a comparison function as argument to
  allow comparisons against infinite values. A function [cmplo x]
  should be read as [compare lo x].

  [trim cmplo cmphi t]  A tree that is either empty or where [cmplo x == LT]
                        and [cmphi x == GT] for the value [x] of the root.
  [filterGt cmp t]      A tree where for all values [k]. [cmp k == LT]
  [filterLt cmp t]      A tree where for all values [k]. [cmp k == GT]

  [split k t]           Returns two trees [l] and [r] where all values
                        in [l] are <[k] and all keys in [r] are >[k].
  [splitMember k t]     Just like [split] but also returns whether [k]
                        was found in the tree.
--------------------------------------------------------------------*/

:: MaybeS a = NothingS | JustS !a

/*--------------------------------------------------------------------
  [trim lo hi t] trims away all subtrees that surely contain no
  values between the range [lo] to [hi]. The returned tree is either
  empty or the key of the root is between @lo@ and @hi@.
--------------------------------------------------------------------*/

trim :: !(MaybeS a) !(MaybeS a) !(Set a) -> Set a | < a & == a
trim NothingS   NothingS   t = t
trim (JustS lx) NothingS   t = greater lx t
  where
  greater lo (Bin _ x _ r) | x <= lo = greater lo r
  greater _  t` = t`
trim NothingS   (JustS hx) t = lesser hx t
  where
  lesser  hi (Bin _ x l _) | x >= hi = lesser  hi l
  lesser  _  t` = t`
trim (JustS lx) (JustS hx) t = middle lx hx t
  where
  middle lo hi (Bin _ x _ r) | x <= lo = middle lo hi r
  middle lo hi (Bin _ x l _) | x >= hi = middle lo hi l
  middle _  _  t` = t`

/*--------------------------------------------------------------------
 * [filterGt x t] filter all values >[x] from tree [t]
 * [filterLt x t] filter all values <[x] from tree [t]
 *--------------------------------------------------------------------*/

filterGt :: !(MaybeS a) !(Set a) -> Set a | < a & == a
filterGt NothingS t = t
filterGt (JustS b) t = filter` b t
  where filter` _   Tip = Tip
        filter` b` (Bin _ x l r)
          | b` < x    = link x (filter` b` l) r
          | b` > x    = r
          | otherwise = filter` b` r

filterLt :: !(MaybeS a) !(Set a) -> Set a | < a & == a
filterLt NothingS t = t
filterLt (JustS b) t = filter` b t
  where filter` _   Tip = Tip
        filter` b` (Bin _ x l r)
          | x < b`    = link x l (filter` b` r)
          | x > b`    = l
          | otherwise = filter` b` l

/*--------------------------------------------------------------------
 * Split
 *--------------------------------------------------------------------*/

split :: !a !(Set a) -> (!Set a, !Set a) | < a
split _ Tip = (Tip,Tip)
split x (Bin _ y l r)
  | x < y
    #! (lt, gt) = split x l
    = (lt, link y gt r)
  | x > y
    #! (lt,gt) = split x r
    = (link y l lt,gt)
  | otherwise = (l, r)

splitMember :: !a !(Set a) -> (!Set a, !Bool, !Set a) | < a
splitMember _ Tip = (Tip, False, Tip)
splitMember x (Bin _ y l r)
  | x < y
    #! (lt, found, gt) = splitMember x l
    = (lt, found, link y gt r)
  | x > y
    #! (lt, found, gt) = splitMember x r
    = (link y l lt, found, gt)
  | otherwise = (l, True, r)

/*--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [x] and all values
  in [r] > [x], and that [l] and [r] are valid trees.
  
  In order of sophistication:
    [Bin sz x l r]    The type constructor.
    [bin x l r]       Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance x l r]   Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [join x l r]      Restores balance and size. 

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.

  Note: in contrast to Adam's paper, we use (<=) comparisons instead
  of (<) comparisons in [join], [merge] and [balance]. 
  Quickcheck (on [difference]) showed that this was necessary in order 
  to maintain the invariants. It is quite unsatisfactory that I haven't 
  been able to find out why this is actually the case! Fortunately, it 
  doesn't hurt to be a bit more conservative.
--------------------------------------------------------------------*/

/*--------------------------------------------------------------------
 * Join 
 *--------------------------------------------------------------------*/
link :: !a !(Set a) !(Set a) -> Set a
link x Tip r  = insertMin x r
link x l Tip  = insertMax x l
link x l=:(Bin sizeL y ly ry) r=:(Bin sizeR z lz rz)
  | delta*sizeL < sizeR  = balanceL z (link x l lz) rz
  | delta*sizeR < sizeL  = balanceR y ly (link x ry r)
  | otherwise            = bin x l r
link _ _ _ = abort "error in link\n"

// insertMin and insertMax don't perform potentially expensive comparisons.
insertMax :: !a !(Set a) -> Set a
insertMax x Tip = singleton x
insertMax x (Bin _ y l r) = balanceR y l (insertMax x r)

insertMin :: !a !(Set a) -> Set a
insertMin x Tip = singleton x
insertMin x (Bin _ y l r) = balanceL y (insertMin x l) r
         
/*--------------------------------------------------------------------
 * [merge l r]: merges two trees.
 *--------------------------------------------------------------------*/
merge :: !(Set a) !(Set a) -> Set a
merge Tip r   = r
merge l Tip   = l
merge l=:(Bin sizeL x lx rx) r=:(Bin sizeR y ly ry)
  | delta*sizeL < sizeR = balanceL y (merge l ly) ry
  | delta*sizeR < sizeL = balanceR x lx (merge rx r)
  | otherwise           = glue l r
merge _ _ = abort "error in merge\n"

/*--------------------------------------------------------------------
 * [glue l r]: glues two trees together.
 * Assumes that [l] and [r] are already balanced with respect to each other.
 *--------------------------------------------------------------------*/
glue :: !.(Set a) !.(Set a) -> Set a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r
      #! (m, l`) = deleteFindMax l
      = balanceR m l` r
  | otherwise
      #! (m, r`) = deleteFindMin r
      = balanceL m l r`

deleteFindMin :: !.(Set a) -> (!a, !Set a)
deleteFindMin (Bin _ x Tip r) = (x, r)
deleteFindMin (Bin _ x l r)
  #! (xm, l`) = deleteFindMin l
  = (xm, balanceR x l` r)
deleteFindMin Tip = (abort "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

deleteFindMax :: !.(Set a) -> (!a, !Set a)
deleteFindMax (Bin _ x l Tip ) = (x, l)
deleteFindMax (Bin _ x l r)
  #! (xm, r`) = deleteFindMax r
  = (xm, balanceL x l r`)
deleteFindMax Tip = (abort "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

minView :: !.(Set a) -> .(Maybe (!a, !Set a))
minView Tip = Nothing
minView x = Just (deleteFindMin x)

maxView :: !.(Set a) -> .(Maybe (!a, !Set a))
maxView Tip = Nothing
maxView x = Just (deleteFindMax x)

/*--------------------------------------------------------------------
  [balance x l r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper,
          or equivalently, [1/delta] corresponds with the $\alpha$
          in Nievergelt's paper. Adams shows that [delta] should
          be larger than 3.745 in order to garantee that the
          rotations can always restore balance.         

  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is correspondes with the inverse
          of $\alpha$ in Adam's article.

  Note that:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.
  
  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  - Balancing is automatic for random data and a balancing
    scheme is only necessary to avoid pathological worst cases.
    Almost any choice will do in practice
    
  - Allthough it seems that a rather large [delta] may perform better 
    than smaller one, measurements have shown that the smallest [delta]
    of 4 is actually the fastest on a wide range of operations. It
    especially improves performance on worst-case scenarios like
    a sequence of ordered insertions.

  Note: in contrast to Adams' paper, we use a ratio of (at least) 2
  to decide whether a single or double rotation is needed. Allthough
  he actually proves that this ratio is needed to maintain the
  invariants, his implementation uses a (invalid) ratio of 1. 
  He is aware of the problem though since he has put a comment in his 
  original source code that he doesn't care about generating a 
  slightly inbalanced tree since it doesn't seem to matter in practice. 
  However (since we use quickcheck :-) we will stick to strictly balanced 
  trees.
--------------------------------------------------------------------*/
delta :== 4
ratio :== 2

// Functions balanceL and balanceR are specialised versions of balance.
// balanceL only checks whether the left subtree is too big,
// balanceR only checks whether the right subtree is too big.

// balanceL is called when left subtree might have been inserted to or when
// right subtree might have been deleted from.
balanceL :: !a !(Set a) !(Set a) -> Set a
balanceL x l r = case r of
  Tip -> case l of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x l Tip
           (Bin _ lx Tip (Bin _ lrx _ _)) -> Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
           (Bin _ lx ll=:(Bin _ _ _ _) Tip) -> Bin 3 lx ll (Bin 1 x Tip Tip)
           (Bin ls lx ll=:(Bin lls _ _ _) lr=:(Bin lrs lrx lrl lrr))
             | lrs < ratio*lls -> Bin (1+ls) lx ll (Bin (1+lrs) x lr Tip)
             | otherwise -> Bin (1+ls) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+size lrr) x lrr Tip)

  (Bin rs _ _ _) -> case l of
           Tip -> Bin (1+rs) x Tip r

           (Bin ls lx ll lr)
              | ls > delta*rs  -> case (ll, lr) of
                   (Bin lls _ _ _, Bin lrs lrx lrl lrr)
                     | lrs < ratio*lls -> Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
                     | otherwise -> Bin (1+ls+rs) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+rs+size lrr) x lrr r)
                   (_, _) -> abort "Failure in Data.Map.balanceL"
              | otherwise -> Bin (1+ls+rs) x l r

// balanceR is called when right subtree might have been inserted to or when
// left subtree might have been deleted from.
balanceR :: !a !(Set a) !(Set a) -> Set a
balanceR x l r = case l of
  Tip -> case r of
           Tip -> Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) -> Bin 2 x Tip r
           (Bin _ rx Tip rr=:(Bin _ _ _ _)) -> Bin 3 rx (Bin 1 x Tip Tip) rr
           (Bin _ rx (Bin _ rlx _ _) Tip) -> Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
           (Bin rs rx rl=:(Bin rls rlx rll rlr) rr=:(Bin rrs _ _ _))
             | rls < ratio*rrs -> Bin (1+rs) rx (Bin (1+rls) x Tip rl) rr
             | otherwise -> Bin (1+rs) rlx (Bin (1+size rll) x Tip rll) (Bin (1+rrs+size rlr) rx rlr rr)

  (Bin ls _ _ _) -> case r of
           Tip -> Bin (1+ls) x l Tip

           (Bin rs rx rl rr)
              | rs > delta*ls  -> case (rl, rr) of
                   (Bin rls rlx rll rlr, Bin rrs _ _ _)
                     | rls < ratio*rrs -> Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
                     | otherwise -> Bin (1+ls+rs) rlx (Bin (1+ls+size rll) x l rll) (Bin (1+rrs+size rlr) rx rlr rr)
                   (_, _) -> abort "Failure in Data.Map.balanceR"
              | otherwise -> Bin (1+ls+rs) x l r

// rotate
rotateL :: !a !(Set a) !(Set a) -> Set a
rotateL x l r=:(Bin _ _ ly ry)
  | size ly < ratio*size ry = singleL x l r
  | otherwise               = doubleL x l r
rotateL _ _ Tip = abort "rotateL Tip"

rotateR :: !a !(Set a) !(Set a) -> Set a
rotateR x l=:(Bin _ _ ly ry) r
  | size ry < ratio*size ly = singleR x l r
  | otherwise               = doubleR x l r
rotateR _ Tip _ = abort "rotateL Tip"

// basic rotations
singleL :: !a !(Set a) !(Set a) -> Set a
singleL x1 t1 (Bin _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
singleL _  _  Tip               = abort "singleL"

singleR :: !a !(Set a) !(Set a) -> Set a
singleR x1 (Bin _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
singleR _  Tip              _   = abort "singleR"

doubleL :: !a !(Set a) !(Set a) -> Set a
doubleL x1 t1 (Bin _ x2 (Bin _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleL _ _ _ = abort "doubleL"

doubleR :: !a !(Set a) !(Set a) -> Set a
doubleR x1 (Bin _ x2 t1 (Bin _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
doubleR _ _ _ = abort "doubleR"

/*--------------------------------------------------------------------
 * The bin constructor maintains the size of the tree
 *--------------------------------------------------------------------*/
//bin :: !a !(Set a) !(Set a) -> Set a
bin x l r :== Bin (size l + size r + 1) x l r
