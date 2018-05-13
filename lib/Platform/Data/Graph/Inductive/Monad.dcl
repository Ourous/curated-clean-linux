// (c) 2002 by Martin Erwig [see file COPYRIGHT]
// | Monadic Graphs
definition module Data.Graph.Inductive.Monad

import Data.Graph.Inductive.Graph
from Control.Monad import class Monad
from Control.Applicative import class Applicative
from Data.Functor import class Functor

//--------------------------------------------------------------------
// MONADIC GRAPH CLASS
//--------------------------------------------------------------------

//
// Currently, we define just one monadic graph class:
//
//   GraphM:    static, decomposable graphs
//              static means that a graph itself cannot be changed
//
// Later we might also define DynGraphM for dynamic, extensible graphs
//



// Monadic Graph
//
class GraphM m gr | Monad m where
  emptyM     :: m (gr a b)

  isEmptyM   :: (m (gr a b)) -> m Bool

  matchM     :: Node (m (gr a b)) -> m (Decomp gr a b)

  mkGraphM   :: [LNode a] [LEdge b] -> m (gr a b)

  labNodesM  :: (m (gr a b)) -> m [LNode a]

  matchAnyM  :: (m (gr a b)) -> m (GDecomp gr a b)

  noNodesM   :: (m (gr a b)) -> m Int

  nodeRangeM :: (m (gr a b)) -> m (Node,Node)

  labEdgesM  :: (m (gr a b)) -> m [LEdge b]

defLabEdgesM  :: (m (gr a b)) -> m [LEdge b] | GraphM m gr

defMatchAnyM  :: (m (gr a b)) -> m (GDecomp gr a b) | GraphM m gr

defNoNodesM   :: (m (gr a b)) -> m Int | GraphM m gr

defNodeRangeM :: (m (gr a b)) -> m (Node,Node) | GraphM m gr

// composing a monadic function with a non-monadic one
//
(>>.) :: ((m a) -> m b) (b -> c) (m a) -> m c | Monad m


//--------------------------------------------------------------------
// DERIVED GRAPH OPERATIONS
//--------------------------------------------------------------------

// graph folds and maps
//

// | graph fold
ufoldM :: ((Context a b) c -> c) c (m (gr a b)) -> m c | GraphM m gr

// (additional) graph projection
// [noNodes, nodeRange, labNodes, labEdges are defined in class Graph]
//
nodesM :: (m (gr a b)) -> m [Node] | GraphM m gr

edgesM :: (m (gr a b)) -> m [Edge] | GraphM m gr

newNodesM :: Int (m (gr a b)) -> m [Node] | GraphM m gr


// graph construction & destruction
//
delNodeM :: Node (m (gr a b)) -> m (gr a b) | GraphM m gr

delNodesM :: [Node] (m (gr a b)) -> m (gr a b) | GraphM m gr

mkUGraphM :: [Node] [Edge] -> m (gr () ()) | GraphM m gr

labUEdges :: [Edge] -> [LEdge ()]

labUNodes :: [Node] -> [LNode ()]


// graph inspection (for a particular node)
//
onMatch :: ((Context a b) -> c) c (m (gr a b)) Node -> m c | GraphM m gr

contextM :: (m (gr a b)) Node -> m (Context a b) | GraphM m gr

labM :: (m (gr a b)) Node -> m (Maybe a) | GraphM m gr
