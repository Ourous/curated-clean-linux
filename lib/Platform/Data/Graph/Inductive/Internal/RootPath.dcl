// (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
// | Inward directed trees as lists of paths.
definition module Data.Graph.Inductive.Internal.RootPath

import Data.Graph.Inductive.Graph

:: LRTree a :== [LPath a]
:: RTree    :== [Path]

first :: ([a] -> Bool) [[a]] -> [a]

// | Find the first path in a tree that starts with the given node
findP :: Node (LRTree a) -> [LNode a]

getPath :: Node RTree -> Path

getLPath :: Node (LRTree a) -> LPath a

getDistance :: Node (LRTree a) -> a

getLPathNodes :: Node (LRTree a) -> Path

