// (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
// | Inward directed trees as lists of paths.
implementation module Data.Graph.Inductive.Internal.RootPath

import Data.Graph.Inductive.Graph
import Data.List
import StdFunc, StdTuple

:: LRTree a :== [LPath a]
:: RTree    :== [Path]

first :: ([a] -> Bool) [[a]] -> [a]
first p xss  = case filter p xss of
                 []    -> []
                 [x:_] -> x

// | Find the first path in a tree that starts with the given node
findP :: Node (LRTree a) -> [LNode a]
findP _ []                                 = []
findP v [LP []:ps]                         = findP v ps
findP v [LP (p=:[(w,_):_]):ps] | v==w      = p
                               | otherwise = findP v ps

getPath :: Node RTree -> Path
getPath v t = (reverse o first (\w->hd w==v)) t

getLPath :: Node (LRTree a) -> LPath a
getLPath v t = (LP o reverse o findP v) t

getDistance :: Node (LRTree a) -> a
getDistance v t = (snd o head o findP v) t

getLPathNodes :: Node (LRTree a) -> Path
getLPathNodes v t = ((\(LP p)->map fst p) o getLPath v) t
