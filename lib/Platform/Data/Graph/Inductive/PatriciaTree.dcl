// |An efficient implementation of 'Data.Graph.Inductive.Graph.Graph'
// using big-endian patricia tree (i.e. "Data.IntMap").
//
// This module provides the following specialised functions to gain
// more performance, using GHC's RULES pragma:
//
// * 'Data.Graph.Inductive.Graph.insNode'
//
// * 'Data.Graph.Inductive.Graph.insEdge'
//
// * 'Data.Graph.Inductive.Graph.gmap'
//
// * 'Data.Graph.Inductive.Graph.nmap'
//
// * 'Data.Graph.Inductive.Graph.emap'
definition module Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Graph
from Data.IntMap.Strict import :: IntMap

//--------------------------------------------------------------------
// GRAPH REPRESENTATION
//--------------------------------------------------------------------

:: Gr a b = Gr (GraphRep a b)

:: GraphRep a b :== IntMap (Context` a b)
:: Context` a b :== (IntMap [b], a, IntMap [b])

:: UGr :== Gr () ()

//--------------------------------------------------------------------
// CLASS INSTANCES
//--------------------------------------------------------------------

// TODO
//instance == (Gr a b) | == a & == b & Ord a & Ord b

instance Graph Gr

instance DynGraph Gr

matchGr :: Node (Gr a b) -> Decomp Gr a b

//--------------------------------------------------------------------
// OVERRIDING FUNCTIONS
//--------------------------------------------------------------------

fastInsNode :: (LNode a) !(Gr a b) -> Gr a b

fastInsEdge :: (LEdge b) !(Gr a b) -> Gr a b

fastGMap :: ((Context a b) -> Context c d) (Gr a b) -> Gr c d

fastNMap :: (a -> c) (Gr a b) -> Gr c b

fastEMap :: (b -> c) (Gr a b) -> Gr a c

fastNEMap :: (a -> c) (b -> d) (Gr a b) -> Gr c d

//--------------------------------------------------------------------
// UTILITIES
//--------------------------------------------------------------------

toAdj :: (IntMap [b]) -> Adj b

fromAdj :: (Adj b) -> IntMap [b]

toContext :: Node (Context` a b) -> Context a b

fromContext :: (Context a b) -> Context` a b

swap :: (a, b) -> (b, a)

// A version of @++@ where order isn`t important, so @xs ++ [x]@
// becomes @x:xs@.  Used when we have to have a function of type @[a]
// -> [a] -> [a]@ but one of the lists is just going to be a single
// element (and it isn`t possible to tell which).
addLists :: [a] [a] -> [a]

addSucc :: (GraphRep a b) Node [(b, Node)] -> GraphRep a b

addPred :: (GraphRep a b) Node [(b, Node)] -> GraphRep a b

clearSucc :: (GraphRep a b) Node [Node] -> GraphRep a b

clearPred :: (GraphRep a b) Node [Node] -> GraphRep a b
