// (c) 2002 by Martin Erwig [see file COPYRIGHT]
// | Monadic Graphs
implementation module Data.Graph.Inductive.Monad

import Data.Graph.Inductive.Graph
from Control.Applicative import class Applicative
import Control.Monad
import Data.Functor
from StdFunc import o, id
import StdList, StdTuple, StdOrdList, StdMisc, StdEnum, StdString
import Data.Maybe

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
defLabEdgesM  :: (m (gr a b)) -> m [LEdge b] | GraphM m gr
defLabEdgesM m = ufoldM (\(p,v,_,s)->(\xs -> (map (fi v) p ++ map (fo v) s)++xs)) [] m
    where
      fo v = \(l,w)->(v,w,l)
      fi v = \(l,w)->(w,v,l)

defMatchAnyM  :: (m (gr a b)) -> m (GDecomp gr a b) | GraphM m gr
defMatchAnyM g = labNodesM g >>= \vs ->
                   case vs of
                     []        -> abort "Match Exception, Empty Graph"
                     [(v,_):_] -> matchM v g >>= \t->case t of
                         (Just c,g`) -> pure (c,g`)
                         _ -> abort "No Match"

defNoNodesM   :: (m (gr a b)) -> m Int | GraphM m gr
defNoNodesM m = (labNodesM >>. length) m

defNodeRangeM :: (m (gr a b)) -> m (Node,Node) | GraphM m gr
defNodeRangeM g = isEmptyM g >>= \isE ->
                    if isE
                       (abort "nodeRangeM of empty graph")
                       (nodesM g >>= \vs -> pure (minList vs,maxList vs))

// composing a monadic function with a non-monadic one
//
(>>.) :: ((m a) -> m b) (b -> c) (m a) -> m c | Monad m
(>>.) f g m = ((\x -> x >>= pure o g) o f) m


//--------------------------------------------------------------------
// DERIVED GRAPH OPERATIONS
//--------------------------------------------------------------------

// graph folds and maps
//

// | graph fold
ufoldM :: ((Context a b) c -> c) c (m (gr a b)) -> m c | GraphM m gr
ufoldM f u g = isEmptyM g >>= \b ->
                  if b (pure u)
                       (matchAnyM g >>= \(c, g`) -> ufoldM f u (pure g`) >>= \x -> pure (f c x))


// (additional) graph projection
// [noNodes, nodeRange, labNodes, labEdges are defined in class Graph]
//
nodesM :: (m (gr a b)) -> m [Node] | GraphM m gr
nodesM m = (labNodesM >>. map fst) m

edgesM :: (m (gr a b)) -> m [Edge] | GraphM m gr
edgesM m = (labEdgesM >>. map (\(v,w,_)->(v,w))) m

newNodesM :: Int (m (gr a b)) -> m [Node] | GraphM m gr
newNodesM i g = isEmptyM g >>= \isE ->
                   if isE
                      (pure [0..i-1])
                      (nodeRangeM g >>= \(_, n) -> pure [n+1..n+i])


// graph construction & destruction
//
delNodeM :: Node (m (gr a b)) -> m (gr a b) | GraphM m gr
delNodeM v m = delNodesM [v] m

delNodesM :: [Node] (m (gr a b)) -> m (gr a b) | GraphM m gr
delNodesM []     g = g
delNodesM [v:vs] g = matchM v g >>= \(_, g`) -> delNodesM vs (pure g`)

mkUGraphM :: [Node] [Edge] -> m (gr () ()) | GraphM m gr
mkUGraphM vs es = mkGraphM (labUNodes vs) (labUEdges es)

labUEdges :: [Edge] -> [LEdge ()]
labUEdges es = map (\x -> toLEdge x ()) es

labUNodes :: [Node] -> [LNode ()]
labUNodes ns = map (\v->(v,())) ns


// graph inspection (for a particular node)
//
onMatch :: ((Context a b) -> c) c (m (gr a b)) Node -> m c | GraphM m gr
onMatch f u g v = matchM v g >>= \(x, _) ->
                     pure (case x of
                             Nothing -> u
                             Just c -> f c)

contextM :: (m (gr a b)) Node -> m (Context a b) | GraphM m gr
contextM g v = onMatch id (abort ("Match Exception, Node: "+++toString v)) g v

labM :: (m (gr a b)) Node -> m (Maybe a) | GraphM m gr
labM m n = onMatch (Just o lab`) Nothing m n

