// (c) 1999 - 2002 by Martin Erwig [see file COPYRIGHT]
// | Basic Graph Algorithms
definition module Data.Graph.Inductive.Basic

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Thread

// | Reverse the direction of all edges.
grev :: (gr a b) -> gr a b | DynGraph gr

// | Make the graph undirected, i.e. for every edge from A to B, there
// exists an edge from B to A.
undir :: (gr a b) -> gr a b | DynGraph gr & Eq b

// | Remove all labels.
unlab :: (gr a b) -> gr () () | DynGraph gr

// | Return all 'Context's for which the given function returns 'True'.
gsel :: ((Context a b) -> Bool) (gr a b) -> [Context a b] | Graph gr

// filter operations
//
// efilter  : filter based on edge property
// elfilter : filter based on edge label property
//

// | Filter based on edge property.
efilter :: ((LEdge b) -> Bool) (gr a b) -> gr a b | DynGraph gr

// | Filter based on edge label property.
elfilter :: (b -> Bool) (gr a b) -> gr a b | DynGraph gr

// some predicates and classifications
//

// | 'True' if the graph has any edges of the form (A, A).
hasLoop :: (gr a b) -> Bool | Graph gr

// | The inverse of 'hasLoop'.
isSimple :: (gr a b) -> Bool | Graph gr

threadGraph :: ((Context a b) r -> t)
               (Split (gr a b) (Context a b) r)
            -> SplitM (gr a b) Node t | Graph gr

// gfold1 f d b u = threadGraph (\c->d (labNode' c)) (\c->gfoldn f d b u (f c))
gfold1 :: (((Context a b) -> [Node])) ((Context a b) r -> t) (Collect (Maybe t) r)
       -> SplitM (gr a b) Node t | Graph gr

gfoldn :: ((Context a b) -> [Node]) ((Context a b) r -> t)
          (Collect (Maybe t) r) [Node] (gr a b) -> (r, gr a b) | Graph gr

// gfold :: ((Context a b) -> [Node]) -> ((Node,a) -> c -> d) ->
//          (Maybe d -> c -> c) -> c -> [Node] -> Graph a b -> c
// gfold f d b u l g = fst (gfoldn f d b u l g)

// type Dir a b    = (Context a b) -> [Node]  -- direction of fold
// type Dagg a b c = (Node,a) -> b -> c       -- depth aggregation
// type Bagg a b   = (Maybe a -> b -> b,b)    -- breadth/level aggregation
//
// gfold :: (Dir a b) -> (Dagg a c d) -> (Bagg d c) -> [Node] -> Graph a b -> c
// gfold f d (b,u) l g = fst (gfoldn f d b u l g)

// | Directed graph fold.
gfold :: ((Context a b) -> [Node]) // ^ direction of fold
         ((Context a b) c -> d)      // ^ depth aggregation
         ((Maybe d) c -> c, c)    // ^ breadth\/level aggregation
         [Node]
         (gr a b)
      -> c | Graph gr

// not finished yet ...
//
// undirBy :: (b -> b -> b) -> Graph a b -> Graph a b
// undirBy = gmap (\(p,v,l,s)->let ps = nub (p++s) in (ps,v,l,ps))
