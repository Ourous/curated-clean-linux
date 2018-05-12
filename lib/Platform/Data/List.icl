implementation module Data.List

import StdBool
import StdEnum
import StdFunc
import StdList
import StdOrdList
import StdTuple

import Data.Functor
import Data.GenEq
import Data.Maybe
import Data.Monoid
from Data.Foldable import class Foldable(foldMap)
from Data.Traversable import class Traversable
import qualified Data.Traversable as T
import Control.Applicative
import Control.Monad

instance Functor []
where
	fmap f l = [f e \\ e <- l]

instance Applicative [] where
	pure x      = [x]
	(<*>) fs xs = [f x\\f<-fs, x<-xs]

instance Alternative [] where
	empty        = []
	(<|>) fa fa` = fa ++ fa`

instance Monad []
where
	bind m k = foldr ((++) o k) [] m

instance MonadPlus []
where
	mzero        = []
	mplus xs ys = xs ++ ys

instance Semigroup [a]
where
	mappend xs ys  = xs ++ ys

instance Monoid [a]
where
	mempty = []

instance Foldable []
where
	fold x = foldMap id x
	foldMap f x = foldr (mappend o f) mempty x
	foldr f x y = foldr f x y
	foldr` f z0 xs = foldl f` id xs z0
	where f` k x z = k (f x z)
	foldl f x y = foldl f x y
	foldl` f x y = foldl f x y
	foldr1 f x = foldr1 f x
	foldl1 f x = foldl1 f x

instance Traversable []
where
	traverse f x = foldr cons_f (pure []) x
	where cons_f x ys = (\x xs -> [x:xs]) <$> f x <*> ys
	mapM f x = mapM f x
	sequenceA f = 'T'.traverse id f
	sequence x = 'T'.mapM id x

(!?) infixl 9   :: ![.a] !Int -> Maybe .a
(!?) [x:_]  0 = Just x
(!?) [_:xs] i = xs !? (i-1)
(!?) _      _ = Nothing

// Haskell Data.List compat

head :: ![.a] -> .a
head xs = hd xs

tail :: !u:[.a] -> u:[.a]
tail xs = tl xs

isnull :: ![.a] -> Bool
isnull xs = isEmpty xs

product :: !.[a] -> a | * , one  a
product xs = prod xs

// End Haskell Data.List compat

keep :: !Int ![a] -> [a]
keep n xs = drop (length xs - n) xs

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip3 []        = ([], [], [])
unzip3 [(x,y,z) : xyzs]  = ([x : xs],[y : ys],[z : zs])
where
  (xs,ys,zs) = unzip3 xyzs

unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip4 []          = ([], [], [], [])
unzip4 [(w,x,y,z) : wxyzs]  = ([w : ws],[x : xs],[y : ys],[z : zs])
where
  (ws,xs,ys,zs) = unzip4 wxyzs
unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
unzip5 []            = ([], [], [], [], [])
unzip5 [(v,w,x,y,z) : vwxyzs]  = ([v : vs],[w : ws],[x : xs],[y : ys],[z : zs])
where
  (vs,ws,xs,ys,zs) = unzip5 vwxyzs

replaceInList :: !(a a -> Bool) !a ![a] -> [a]
replaceInList _ _ []         = []
replaceInList cond new [x:xs]
    | cond new x            = [new : xs]
    | otherwise             = [x : replaceInList cond new xs]

splitWith :: !(a -> Bool) ![a] -> (![a],![a])
splitWith f [] = ([],[])
splitWith f [x:xs]
  | f x  = let (y,n) = splitWith f xs in ([x:y],n)
      = let (y,n)  = splitWith f xs in (y,[x:n])

sortByIndex :: ![(!Int,!a)] -> [a]
sortByIndex l = map snd (sortBy (\(a,_) (b,_) -> a < b) l)

intersperse :: !a ![a] -> [a]
intersperse i []      = []
intersperse i [x]     = [x]
intersperse i [x:xs]  = [x,i:intersperse i xs]

intercalate :: !.[a] ![.[a]] -> .[a]
intercalate xs xss = flatten (intersperse xs xss)

transpose :: ![[a]] -> [.[a]]
transpose []  = []
transpose [[]     : xss] = transpose xss
transpose [[x:xs] : xss] = [[x : [h \\ [h:t] <- xss]] : transpose [xs : [t \\ [h:t] <- xss]]]

subsequences :: .[a] -> .[[a]]
subsequences xs = [[] : nonEmptySubsequences xs]

nonEmptySubsequences :: .[a] -> .[[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences [x:xs]  =  [[x] : foldr f [] (nonEmptySubsequences xs)]
  where f ys r = [ys : [x : ys] : r]

permutations :: [a] -> .[[a]]
permutations xs0        =  [xs0 : perms xs0 []]
  where
    perms []     _  = []
    perms [t:ts] is = foldr interleave (perms ts [t:is]) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave` id xs r in zs
            interleave` _ []     r = (ts, r)
            interleave` f [y:ys] r = let (us,zs) = interleave` (f o (\xs -> [y:xs])) ys r
                                     in  ([y:us], [f [t:y:us] : zs])

foldl1 :: (.a -> .(.a -> .a)) ![.a] -> .a
foldl1 f [x:xs]         =  foldl f x xs

concatMap :: (.a -> [.b]) ![.a] -> [.b]
concatMap f ls = flatten (map f ls)

maximum :: !.[a] -> a | < a
maximum [x]     = x
maximum [x:xs]  = max x (maximum xs)

minimum :: !.[a] -> a | Ord a
minimum xs =  foldl1 min xs

getItems :: ![a] ![Int] -> [a]
getItems list indexes = [x \\ x <- list & idx <- [0..] | isMember idx indexes]

scanl :: (a -> .(.b -> a)) a [.b] -> .[a]
scanl f q ls            =  [q : (case ls of
                                  []     -> []
                                  [x:xs] -> scanl f (f q x) xs)]

scanl1 :: (a -> .(a -> a)) !.[a] -> .[a]
scanl1 f [x:xs]         =  scanl f x xs
scanl1 _ []             =  []

foldr1 :: (.a -> .(.a -> .a)) ![.a] -> .a
foldr1 _ [x]            =  x
foldr1 f [x:xs]         =  f x (foldr1 f xs)

replicate :: !.Int a -> .[a]
replicate n x           =  take n (repeat x)

cycle :: !.[a] -> [a]
cycle xs                = xs`
  where xs` = xs ++ xs`

unfoldr :: !(.a -> Maybe (.b,.a)) .a -> [.b]
unfoldr f b  =
  case f b of
   Just (a,new_b) -> [a : unfoldr f new_b]
   Nothing        -> []

break :: (a -> .Bool) !.[a] -> .([a],[a])
break _ xs=:[]           =  (xs, xs)
break p xs=:[x:xs`]
           | p x        =  ([],xs)
           | otherwise  =  let (ys,zs) = break p xs` in ([x:ys],zs)

stripPrefix :: !.[a] u:[a] -> Maybe v:[a] | == a, [u <= v]
stripPrefix [] ys = Just ys
stripPrefix [x:xs] [y:ys]
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

group :: .(.[a] -> [.[a]]) | == a
group                   =  groupBy (==)

groupBy :: (a -> a -> .Bool) !.[a] -> [.[a]]
groupBy _  []           =  []
groupBy eq [x:xs]       =  [[x:ys] : groupBy eq zs]
                           where (ys,zs) = span (eq x) xs

inits :: .[a] -> [.[a]]
inits xs                =  [[] : case xs of
                                  []        -> []
                                  [x : xs`] -> map (\ys -> [x : ys]) (inits xs`)]

tails :: [a] -> .[[a]]
tails xs                =  [xs : case xs of
                                  []        -> []
                                  [_ : xs`] -> tails xs`]

isPrefixOf :: !.[a] .[a] -> .Bool | == a
isPrefixOf [] _          =  True
isPrefixOf _  []         =  False
isPrefixOf [x:xs] [y:ys] =  x == y && isPrefixOf xs ys

isSuffixOf :: !.[a] .[a] -> .Bool | == a
isSuffixOf x y          =  isPrefixOf (reverse x) (reverse y)

isInfixOf :: .[a] .[a] -> Bool | == a
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

// Ported from https://rosettacode.org/wiki/Levenshtein_distance#Haskell
levenshtein :: !.[a] !.[a] -> Int | == a
levenshtein xs ys = last (foldl transform [0..length xs] ys)
where
	transform ns=:[n:ns`] c = scan (calc c) (n+1) (zip3 xs ns ns`)
	calc c z (c`, x, y) = minList [y+1, z+1, if (c`<>c) 1 0 + x]

elem :: a !.[a] -> .Bool | == a
elem _ []       = False
elem x [y:ys]   = x == y || elem x ys

notElem :: a !.[a] -> .Bool | == a
notElem _ []     =  True
notElem x [y:ys] =  x <> y && notElem x ys

lookup :: a ![(a,.b)] -> Maybe .b | == a
lookup  _   []          =  Nothing
lookup  key [(x,y):xys]
    | key == x          =  Just y
    | otherwise         =  lookup key xys

find :: (a -> .Bool) -> .(.[a] -> .(Maybe a))
find p          = listToMaybe o filter p

partition :: !(a -> .Bool) !.[a] -> (!.[a], !.[a])
partition p xs = foldr` (select p) ([],[]) xs

select :: !.(a -> .Bool) !a !(!u:[a], !v:[a]) -> (!w:[a], !x:[a]), [u <= w,v <= x]
select p x (ts, fs)
  | p x       = ([x:ts], fs)
  | otherwise = (ts, [x:fs])

foldr` :: !(a .b -> .b) !.b !.[a] -> .b
foldr` _ acc []       = acc
foldr` f acc [x : xs]
  #! tmp = foldr` f acc xs
  = f x tmp

elemIndex :: a -> .(.[a] -> .(Maybe Int)) | == a
elemIndex x     = findIndex (\y -> x==y)

elemIndices :: a -> .(.[a] -> .[Int]) | == a
elemIndices x   = findIndices (\y -> x==y)

findIndex :: (.a -> .Bool) -> .([.a] -> .(Maybe Int))
findIndex p     = listToMaybe o findIndices p

findIndices :: (.a -> .Bool) ![.a] -> .[Int]
findIndices p xs = [ i \\ (x,i) <- zip2 xs [0..] | p x]

zip3 :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip3 [a:as] [b:bs] [c:cs]
  = [(a, b, c): zip3 as bs cs]
zip3 _ _ _
  = []

zip4 :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip4 [a:as] [b:bs] [c:cs] [d:ds]
  = [(a, b, c, d): zip4 as bs cs ds]
zip4 _ _ _ _
  = []

zip5 :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zip5 [a:as] [b:bs] [c:cs] [d:ds] [e:es]
  = [(a, b, c, d, e): zip5 as bs cs ds es]
zip5 _ _ _ _ _
  = []

zipWith :: (.a -> .(.b -> .h)) ![.a] [.b] -> [.h]
zipWith z [a:as] [b:bs]
                   = [ z a b : zipWith z as bs]
zipWith _ _ _ = []

zipSt :: (.a -> .(.b -> (.st -> .st))) ![.a] [.b] .st -> .st
zipSt z [a:as] [b:bs] st
  # st = z a b st
  = zipSt z as bs st
zipSt _ _ _ st = st

zipWithSt :: (.a -> .(.b -> (.st -> .(.h, .st)))) ![.a] [.b] .st -> .([.h], .st)
zipWithSt z [a:as] [b:bs] st
  # (x, st)  = z a b st
  # (xs, st) = zipWithSt z as bs st
  = ([x : xs], st)
zipWithSt _ _ _ st = ([], st)

zipWith3 :: (.a -> .(.b -> .(.c -> .h))) ![.a] [.b] [.c] -> [.h]
zipWith3 z [a:as] [b:bs] [c:cs]
                   = [ z a b c : zipWith3 z as bs cs]
zipWith3 _ _ _ _ = []

zipWith4 :: (.a -> .(.b -> .(.c -> .(.d -> .h))))
      ![.a] [.b] [.c] [.d] -> [.h]
zipWith4 z [a:as] [b:bs] [c:cs] [d:ds]
                   = [ z a b c d : zipWith4 z as bs cs ds]
zipWith4 _ _ _ _ _ = []

zipWith5 :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
      ![.a] [.b] [.c] [.d] [.e] -> [.h]
zipWith5 z [a:as] [b:bs] [c:cs] [d:ds] [e:es]
                   = [ z a b c d e : zipWith5 z as bs cs ds es]
zipWith5 _ _ _ _ _ _ = []

nub :: !.[a] -> .[a] | == a
nub l                   = nub` l []
  where
    nub` [] _           = []
    nub` [x:xs] ls
        | elem x  ls    = nub` xs ls
        | otherwise     = [x : nub` xs [x:ls]]

nubBy :: (a -> .(a -> .Bool)) !.[a] -> .[a]
nubBy eq l              = nubBy` l []
  where
    nubBy` [] _         = []
    nubBy` [y:ys] xs
       | elem_by eq y xs = nubBy` ys xs
       | otherwise       = [y : nubBy` ys [y:xs]]

elem_by :: (a -> .(.b -> .Bool)) a ![.b] -> .Bool
elem_by _  _ []         =  False
elem_by eq y [x:xs]     =  eq y x || elem_by eq y xs

delete :: u:(a -> v:(w:[a] -> x:[a])) | == a, [v <= u,w <= x]
delete                  =  deleteBy (==)

deleteBy :: (a -> .(b -> .Bool)) a !u:[b] -> v:[b], [u <= v]
deleteBy _  _ []        = []
deleteBy eq x [y:ys]    = if (eq x y) ys [y : deleteBy eq x ys]

deleteFirstsBy :: (a -> .(b -> .Bool)) -> u:(v:[b] -> w:(.[a] -> x:[b])), [w <= u,w v <= x]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))

difference :: u:(v:[a] -> w:(.[a] -> x:[a])) | == a, [w <= u,w v <= x]
difference                    =  differenceBy (==)

differenceBy :: (a -> a -> .Bool) !u:[a] !.[a] -> v:[a], [u <= v]
differenceBy eq as bs             =  foldl (flip (deleteBy eq)) as bs

intersect :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
intersect               =  intersectBy (==)

intersectBy :: (a -> b -> .Bool) !.[a] !.[b] -> .[a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x \\ x <- xs | any (eq x) ys]

union :: u:(.[a] -> v:(.[a] -> .[a])) | == a, [v <= u]
union                   = unionBy (==)

unionBy :: (a -> .(a -> .Bool)) !.[a] .[a] -> .[a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

hasDup :: ![a] -> Bool | Eq a
hasDup []     = False
hasDup [x:xs] = isMember x xs || hasDup xs

isMemberGen :: !a !.[a] -> Bool | gEq{|*|} a
isMemberGen x [hd:tl]	= hd === x || isMemberGen x tl
isMemberGen x []		= False

strictFoldr :: !(.a -> .(.b -> .b)) !.b ![.a] -> .b
strictFoldr _ b []     = b
strictFoldr f b [x:xs] = f x (strictFoldr f b xs)

strictFoldrSt       :: !(.a -> .(.b -> .(.st -> .(.b, .st)))) !.b ![.a] .st -> .(.b, .st)
strictFoldrSt _ b []     st = (b, st)
strictFoldrSt f b [x:xs] st
  #! (acc, st) = strictFoldrSt f b xs st
  #! (r, st)   = f x acc st
  = (r, st)

strictFoldlSt       :: !(.a -> .(.b -> .(.st -> .(.a, .st)))) !.a ![.b] .st -> .(.a, .st)
strictFoldlSt _ b [] st = (b, st)
strictFoldlSt f b [x:xs] st
  #! (r, st) = f b x st
  = strictFoldlSt f r xs st

strictFoldl :: !(.a -> .(.b -> .a)) !.a ![.b] -> .a
strictFoldl _ b [] = b
strictFoldl f b [x:xs]
  #! r = f b x
  = strictFoldl f r xs

strictTRMapRev :: !(.a -> .b) ![.a] -> [.b]
strictTRMapRev f xs = strictTRMapAcc f xs []

strictTRMapAcc :: !(u:a -> v:b) !w:[u:a] !x:[v:b] -> y:[v:b], [w <= u,y <= v,x <= y]
strictTRMapAcc f []     acc = acc
strictTRMapAcc f [x:xs] acc = strictTRMapAcc f xs [f x : acc]

strictTRMap :: !(.a -> .b) ![.a] -> [.b]
strictTRMap f xs = reverseTR (strictTRMapAcc f xs [])

reverseTR :: ![.a] -> [.a]
reverseTR xs = rev` xs []
  where
  rev` :: !u:[v:a] !w:[v:a] -> x:[v:a], [x u <= v,w <= x]
  rev` [] acc = acc
  rev` [x:xs] acc = rev` xs [x:acc]

flattenTR :: ![[a]] -> [a]
flattenTR xss = reverseTR (flattenTRAcc xss [])

flattenTRAcc :: ![[a]] [a] -> [a]
flattenTRAcc [] acc = acc
flattenTRAcc [xs:xss] acc
  #! r = reverseTR xs ++ acc
  = flattenTRAcc xss r

strictTRMapSt :: !(a .st -> (!b, !.st)) ![a] !.st -> (![b], !.st)
strictTRMapSt f xs st
  #! (rs, st) = strictTRMapStAcc f xs [] st
  = (reverseTR rs, st)

strictTRMapStAcc :: !(a .st -> (!b, !.st)) ![a] ![b] !.st -> (![b], !.st)
strictTRMapStAcc f []     acc st = (acc, st)
strictTRMapStAcc f [x:xs] acc st
  #! (r, st) = f x st
  = strictTRMapStAcc f xs [r : acc] st

strictTRZipWith :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWith f as bs = reverseTR (strictTRZipWithRev f as bs)

strictTRZipWithRev :: !(a b -> c) ![a] ![b] -> [c]
strictTRZipWithRev f as bs = strictTRZipWithAcc f as bs []

strictTRZipWithAcc :: !(a b -> c) ![a] ![b] ![c] -> [c]
strictTRZipWithAcc f [a:as] [b:bs] acc
  = strictTRZipWithAcc f as bs [f a b : acc]
strictTRZipWithAcc _ _ _ acc = acc

strictTRZip4 :: ![a] ![b] ![c] ![d] -> [(!a, !b, !c, !d)]
strictTRZip4 as bs cs ds = reverseTR (strictTRZip4Rev as bs cs ds)

strictTRZip4Rev :: ![a] ![b] ![c] ![d] -> [(!a, !b, !c, !d)]
strictTRZip4Rev as bs cs ds = strictTRZip4Acc as bs cs ds []

strictTRZip4Acc :: ![a] ![b] ![c] ![d] ![(!a, !b, !c, !d)] -> [(!a, !b, !c, !d)]
strictTRZip4Acc [a:as] [b:bs] [c:cs] [d:ds] acc
  = strictTRZip4Acc as bs cs ds [(a, b, c, d):acc]
strictTRZip4Acc _ _ _ _ acc = acc

strictTRZip2 :: ![a] ![b]-> [(!a, !b)]
strictTRZip2 as bs = reverseTR (strictTRZip2Rev as bs)

strictTRZip2Rev :: ![a] ![b]-> [(!a, !b)]
strictTRZip2Rev as bs = strictTRZip2Acc as bs []

strictTRZip2Acc :: ![a] ![b] ![(!a, !b)] -> [(!a, !b)]
strictTRZip2Acc [a:as] [b:bs] acc
  = strictTRZip2Acc as bs [(a, b):acc]
strictTRZip2Acc _ _ acc = acc

strictTRZipWith3 :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3 f as bs cs = reverseTR (strictTRZipWith3Rev f as bs cs)

strictTRZipWith3Rev :: !(a b c -> d) ![a] ![b] ![c] -> [d]
strictTRZipWith3Rev f as bs cs = strictTRZipWith3Acc f as bs cs []

strictTRZipWith3Acc :: !(a b c -> d) ![a] ![b] ![c] ![d] -> [d]
strictTRZipWith3Acc f [a:as] [b:bs] [c:cs] acc
  = strictTRZipWith3Acc f as bs cs [f a b c : acc]
strictTRZipWith3Acc _ _ _ _ acc = acc

qfoldl :: (a -> b -> [b]) (a -> b -> a) a ![b] -> a
qfoldl _ _ a []
	= a
qfoldl f g a [b:bs]
	= let a` = g a b in qfoldl f g a` (bs ++ f a` b)

qfoldr :: (a -> b -> [b]) (b -> a -> a) a ![b] -> a
qfoldr _ _ a []
	= a
qfoldr f g a [b:bs]
	= let a` = g b a in qfoldr f g a` (bs ++ f a` b)

