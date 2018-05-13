// | Maximum Flow algorithm
//
// We are given a flow network @G=(V,E)@ with source @s@ and sink @t@
// where each edge @(u,v)@ in @E@ has a nonnegative capacity
// @c(u,v)>=0@, and we wish to find a flow of maximum value from @s@
// to @t@.
//
// A flow in @G=(V,E)@ is a real-valued function @f:VxV->R@ that
// satisfies:
//
// @
// For all u,v in V, f(u,v)\<=c(u,v)
// For all u,v in V, f(u,v)=-f(v,u)
// For all u in V-{s,t}, Sum{f(u,v):v in V } = 0
// @
//
// The value of a flow f is defined as @|f|=Sum {f(s,v)|v in V}@, i.e.,
// the total net flow out of the source.
//
// In this module we implement the Edmonds-Karp algorithm, which is
// the Ford-Fulkerson method but using the shortest path from @s@ to
// @t@ as the augmenting path along which the flow is incremented.

definition module Data.Graph.Inductive.Query.MaxFlow

import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph
from StdOverloaded import class ~, class +, class -, class zero
from StdClass import class Ord

// |
// @
//                 i                                 0
// For each edge a--->b this function returns edge b--->a .
//          i
// Edges a\<--->b are ignored
//          j
// @
getRevEdges :: [Edge] -> [LEdge b] | zero b

// |
// @
//                 i                                  0
// For each edge a--->b insert into graph the edge a\<---b o Then change the
//                            i         (i,0,i)
// label of every edge from a---->b to a------->b
// @
//
// where label (x,y,z)=(Max Capacity, Current flow, Residual capacity)
augmentGraph :: (gr a b) -> gr a (b,b,b) | DynGraph gr & zero b

// | Given a successor or predecessor list for node @u@ and given node @v@, find
//   the label corresponding to edge @(u,v)@ and update the flow and
//   residual capacity of that edge's label. Then return the updated
//   list.
updAdjList:: (Adj (b,b,b)) Node b Bool -> Adj (b,b,b) | < b & Ord b & Eq b & zero b & ~ b & + b & - b

// | Update flow and residual capacity along augmenting path from @s@ to @t@ in
//   graph @@G. For a path @[u,v,w,...]@ find the node @u@ in @G@ and
//   its successor and predecessor list, then update the corresponding
//   edges @(u,v)@ and @(v,u)@ on those lists by using the minimum
//   residual capacity of the path.
updateFlow :: Path b (gr a (b,b,b)) -> gr a (b,b,b) | DynGraph gr & < b & Ord b & Eq b & zero b & ~ b & + b & - b

// | Compute the flow from @s@ to @t@ on a graph whose edges are labeled with
//   @(x,y,z)=(max capacity,current flow,residual capacity)@ and all
//   edges are of the form @a\<---->b@. First compute the residual
//   graph, that is, delete those edges whose residual capacity is
//   zero. Then compute the shortest augmenting path from @s@ to @t@,
//   and finally update the flow and residual capacity along that path
//   by using the minimum capacity of that path. Repeat this process
//   until no shortest path from @s@ to @t@ exist.
mfmg :: (gr a (b,b,b)) Node Node -> gr a (b,b,b) | DynGraph gr & < b & Ord b & Eq b & zero b & ~ b & + b & - b

// | Compute the flow from s to t on a graph whose edges are labeled with
//   @x@, which is the max capacity and where not all edges need to be
//   of the form a\<---->b. Return the flow as a grap whose edges are
//   labeled with (x,y,z)=(max capacity,current flow,residual
//   capacity) and all edges are of the form a\<---->b
mf :: (gr a b) Node Node -> gr a (b,b,b) | DynGraph gr & < b & Ord b & Eq b & zero b & ~ b & + b & - b

// | Compute the maximum flow from s to t on a graph whose edges are labeled
//   with x, which is the max capacity and where not all edges need to
//   be of the form a\<---->b. Return the flow as a graph whose edges
//   are labeled with (y,x) = (current flow, max capacity).
maxFlowgraph :: (gr a b) Node Node -> gr a (b,b) | DynGraph gr & < b & Ord b & Eq b & zero b & ~ b & + b & - b

// | Compute the value of a maximumflow
maxFlow :: (gr a b) Node Node -> b | DynGraph gr & < b & Ord b & Eq b & zero b & ~ b & + b & - b

//----------------------------------------------------------------------------
// Some test cases: clr595 is from the CLR textbook, page 595. The value of
// the maximum flow for s=1 and t=6 (23) coincides with the example but the
// flow itself is slightly different since the textbook does not compute the
// shortest augmenting path from s to t, but just any path. However remember
// that for a given flow graph the maximum flow is not unique.
// (gr595 is defined in GraphData.hs)
//----------------------------------------------------------------------------
