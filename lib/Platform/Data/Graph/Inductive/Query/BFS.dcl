// (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
// | Breadth-First Search Algorithms

definition module Data.Graph.Inductive.Query.BFS

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Queue
import Data.Graph.Inductive.Internal.RootPath

// bfs (node list ordered by distance)
//
bfsnInternal :: ((Context a b) -> c) (Queue Node) (gr a b) -> [c] | Graph gr

bfsnWith :: ((Context a b) -> c) [Node] (gr a b) -> [c] | Graph gr

bfsn :: [Node] (gr a b) -> [Node] | Graph gr

bfsWith :: ((Context a b) -> c) Node (gr a b) -> [c] | Graph gr

bfs :: Node (gr a b) -> [Node] | Graph gr


// level (extension of bfs giving the depth of each node)
//
level :: Node (gr a b) -> [(Node,Int)] | Graph gr

suci :: (Context a b) Int -> [(Node, Int)]

leveln :: [(Node,Int)] (gr a b) -> [(Node,Int)] | Graph gr


// bfe (breadth first edges)
// remembers predecessor information
//
bfenInternal :: (Queue Edge) (gr a b) -> [Edge] | Graph gr

bfen :: [Edge] (gr a b) -> [Edge] | Graph gr

bfe :: Node (gr a b) -> [Edge] | Graph gr

outU :: (Context a b) -> [Edge]


// bft (breadth first search tree)
// here: with inward directed trees
//
// bft :: Node -> gr a b -> IT.InTree Node
// bft v g = IT.build $ map swap $ bfe v g
//           where swap (x,y) = (y,x)
//
// sp (shortest path wrt to number of edges)
//
// sp :: Node -> Node -> gr a b -> [Node]
// sp s t g = reverse $ IT.rootPath (bft s g) t


// faster shortest paths
// here: with root path trees
//
bft :: Node (gr a b) -> RTree | Graph gr

bf :: (Queue Path) (gr a b) -> RTree | Graph gr

esp :: Node Node (gr a b) -> Path | Graph gr


// lesp is a version of esp that returns labeled paths
// Note that the label of the first node in a returned path is meaningless;
// all other nodes are paired with the label of their incoming edge.
//
lbft :: Node (gr a b) -> LRTree b | Graph gr

lbf :: (Queue (LPath b)) (gr a b) -> LRTree b | Graph gr

lesp :: Node Node (gr a b) -> LPath b | Graph gr
