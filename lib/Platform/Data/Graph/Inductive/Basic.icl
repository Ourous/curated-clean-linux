// (c) 1999 - 2002 by Martin Erwig [see file COPYRIGHT]
// | Basic Graph Algorithms
implementation module Data.Graph.Inductive.Basic

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Thread

import Data.List
from StdFunc import o
import StdTuple

// | Reverse the direction of all edges.
grev :: (gr a b) -> gr a b | DynGraph gr
grev gr = gmap (\(p,v,l,s)->(s,v,l,p)) gr

// | Make the graph undirected, i.e. for every edge from A to B, there
// exists an edge from B to A.
undir :: (gr a b) -> gr a b | DynGraph gr & Eq b
undir g = gmap (\(p,v,l,s)->let ps = nub (p++s) in (ps,v,l,ps)) g
// this version of undir considers edge lables and keeps edges with
// different labels, an alternative is the definition below:
//   undir = gmap (\(p,v,l,s)->
//           let ps = nubBy (\x y->snd x==snd y) (p++s) in (ps,v,l,ps))

// | Remove all labels.
unlab :: (gr a b) -> gr () () | DynGraph gr
unlab g = gmap (\(p,v,_,s)->(unlabAdj p,v,(),unlabAdj s)) g
        where unlabAdj = map (\(_,v)->((),v))
// alternative:
//    unlab = nmap (\_->()) o emap (\_->())

// | Return all 'Context's for which the given function returns 'True'.
gsel :: ((Context a b) -> Bool) (gr a b) -> [Context a b] | Graph gr
gsel p g = ufold (\c cs->if (p c) [c:cs] cs) [] g


// filter operations
//
// efilter  : filter based on edge property
// elfilter : filter based on edge label property
//

// | Filter based on edge property.
efilter :: ((LEdge b) -> Bool) (gr a b) -> gr a b | DynGraph gr
efilter f g = ufold cfilter emptyGraph g
            where cfilter (p,v,l,s) g = (p`,v,l,s`) <&> g
                   where p` = filter (\(b,u)->f (u,v,b)) p
                         s` = filter (\(b,w)->f (v,w,b)) s

// | Filter based on edge label property.
elfilter :: (b -> Bool) (gr a b) -> gr a b | DynGraph gr
elfilter f g = efilter (\(_,_,b)->f b) g


// some predicates and classifications
//

// | 'True' if the graph has any edges of the form (A, A).
hasLoop :: (gr a b) -> Bool | Graph gr
hasLoop gr = (not o isEmpty o gsel (\c-> elem (node` c) (suc` c))) gr

// | The inverse of 'hasLoop'.
isSimple :: (gr a b) -> Bool | Graph gr
isSimple g = (not o hasLoop) g

threadGraph :: ((Context a b) r -> t)
               (Split (gr a b) (Context a b) r)
            -> SplitM (gr a b) Node t | Graph gr
threadGraph f c = threadMaybe f c match

// gfold1 f d b u = threadGraph (\c->d (labNode' c)) (\c->gfoldn f d b u (f c))
gfold1 :: (((Context a b) -> [Node])) ((Context a b) r -> t) (Collect (Maybe t) r)
       -> SplitM (gr a b) Node t | Graph gr
gfold1 f d b = threadGraph d (gfoldn f d b o f)

gfoldn :: ((Context a b) -> [Node]) ((Context a b) r -> t)
          (Collect (Maybe t) r) [Node] (gr a b) -> (r, gr a b) | Graph gr
gfoldn f d b xs g = threadList b (gfold1 f d b) xs g

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
gfold f d b l g = fst (gfoldn f d b l g)

// not finished yet ...
//
// undirBy :: (b -> b -> b) -> Graph a b -> Graph a b
// undirBy = gmap (\(p,v,l,s)->let ps = nub (p++s) in (ps,v,l,ps))
