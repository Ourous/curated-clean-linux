implementation module Data.Tree

// Ported from Haskell's Data.Tree by Jurriën Stutterheim
from Data.Functor import class Functor (..), <$>
from Control.Applicative import class pure(..), class <*>(..), class Applicative
from Control.Monad import class Monad (..), liftM, `b`, mapM
from Data.Monoid import class Monoid (..), class Semigroup
from StdList import map, ++
from StdOverloaded import class +++ (..)
from StdFunc import o
import StdMisc
from Data.List import zipWith, iterate, foldr, repeat, concatMap, takeWhile, isEmpty
from StdBool import not
import StdString

rootLabel :: (RTree a) -> a
rootLabel (RNode x _) = x

subRForest :: (RTree a) -> RForest a
subRForest (RNode _ xs) = xs

instance Functor RTree where
  fmap f t = fmapRTree f t

fmapRTree :: (a -> b) (RTree a) -> RTree b
fmapRTree f (RNode x ts) = RNode (f x) (map (fmapRTree f) ts)

instance pure RTree
where
	pure x = RNode x []

instance <*> RTree
where
	(<*>) (RNode f tfs) tx=:(RNode x txs) =
		RNode (f x) (map (\x -> f <$> x) txs ++ map (\x -> x <*> tx) tfs)

instance Monad RTree where
  bind (RNode x ts) f
    # (RNode x` ts`) = f x
    = RNode x` (ts` ++ map (\x -> bind x f) ts)

mergeForestsByChoice :: (a a -> Bool) (a a -> a) (RForest a) (RForest a) -> RForest a
mergeForestsByChoice _ _ [] ys = ys
mergeForestsByChoice _ _ xs [] = xs
mergeForestsByChoice pred choose [xn=:(RNode x xs) : xss] [yn=:(RNode y ys) : yss]
  | pred x y  = [RNode (choose x y) (mergeForestsByChoice pred choose xs ys) : mergeForestsByChoice pred choose xss yss]
  | otherwise = [xn : yn : mergeForestsByChoice pred choose xss yss]
mergeForestsByChoice _ _ _ _ = abort "error in mergeForestsByChoice\n"

mergeForestsBy :: (a a -> Bool) (RForest a) (RForest a) -> RForest a
mergeForestsBy f xs ys = mergeForestsByChoice f (\x _ -> x) xs ys

mergeForests :: (RForest a) (RForest a) -> RForest a | == a
mergeForests l r = mergeForestsBy (==) l r

//instance Traversable RTree where
  //traverse f (RNode x ts) = RNode <$> f x <*> traverse (traverse f) ts

//instance Foldable RTree where
    //foldMap f (RNode x ts) = mappend (f x) (foldMap (foldMap f) ts)
unlines :: [String] -> String
unlines xs = foldr (\x acc -> x +++ "\n" +++ acc) "" xs

// | Neat 2-dimensional drawing of a tree.
drawRTree :: (RTree String) -> String
drawRTree x = unlines (draw x)

// | Neat 2-dimensional drawing of a forest.
drawRForest :: (RForest String) -> String
drawRForest x = unlines (map drawRTree x)

draw :: (RTree String) -> [String]
draw (RNode x ts0) = [x : drawSubRTrees ts0]
  where
  drawSubRTrees [] = []
  drawSubRTrees [t] =
      ["|" : shift "`- " "   " (draw t)]
  drawSubRTrees [t:ts] =
      ["|" : shift "+- " "|  " (draw t) ++ drawSubRTrees ts]

  shift first other xs = zipWith (+++) [first : repeat other] xs

// | The elements of a tree in pre-order.
flatten :: (RTree a) -> [a]
flatten t = squish t []
  where
  squish (RNode x ts) xs = [x : foldr squish xs ts]

// | Lists of nodes at each level of the tree.
levels :: (RTree a) -> [[a]]
levels t =
    map (map rootLabel) (
        takeWhile (not o isEmpty) (
        iterate (concatMap subRForest) [t]))

leafs :: (RTree a) -> [a]
leafs (RNode x []) = [x]
leafs (RNode x xs) = concatMap leafs xs

// | Build a tree from a seed value
unfoldRTree :: (b -> (a, [b])) b -> RTree a
unfoldRTree f b
  # (a, bs) = f b
  = RNode a (unfoldRForest f bs)

// | Build a forest from a list of seed values
unfoldRForest :: (b -> (a, [b])) [b] -> RForest a
unfoldRForest f xs = map (unfoldRTree f) xs

// | Monadic tree builder, in depth-first order
unfoldRTreeM :: (b -> m (a, [b])) b -> m (RTree a) | Monad m
unfoldRTreeM f b =
                  f b
  `b` \(a, bs) -> unfoldRForestM f bs
  `b` \ts      -> pure (RNode a ts)

// | Monadic forest builder, in depth-first order
unfoldRForestM :: (b -> m (a, [b])) [b] -> m (RForest a) | Monad m
unfoldRForestM f xs = mapM (unfoldRTreeM f) xs

foldTree :: (a [b] -> b) (RTree a) -> b
foldTree f t = go t
  where
  go (RNode x ts) = f x (map go ts)
