// (c) 2000-2005 by Martin Erwig [see file COPYRIGHT]
// | Breadth-First Search Algorithms

implementation module Data.Graph.Inductive.Query.BFS

import StdMisc,StdBool, StdFunc
import Data.List
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Internal.Queue
import Data.Graph.Inductive.Internal.RootPath

// bfs (node list ordered by distance)
//
bfsnInternal :: ((Context a b) -> c) (Queue Node) (gr a b) -> [c] | Graph gr
bfsnInternal f q g | queueEmpty q || isEmptyGraph g = []
                   | otherwise                 =
       let (v,q`) = queueGet q in
       case match v g of
          (Just c, g`)  -> [f c:bfsnInternal f (queuePutList (suc` c) q`) g`]
          (Nothing, g`) -> bfsnInternal f q` g`

bfsnWith :: ((Context a b) -> c) [Node] (gr a b) -> [c] | Graph gr
bfsnWith f vs g = bfsnInternal f (queuePutList vs mkQueue) g

bfsn :: [Node] (gr a b) -> [Node] | Graph gr
bfsn ns g = bfsnWith node` ns g

bfsWith :: ((Context a b) -> c) Node (gr a b) -> [c] | Graph gr
bfsWith f v g = bfsnInternal f (queuePut v mkQueue) g

bfs :: Node (gr a b) -> [Node] | Graph gr
bfs n g = bfsWith node` n g


// level (extension of bfs giving the depth of each node)
//
level :: Node (gr a b) -> [(Node,Int)] | Graph gr
level v g = leveln [(v,0)] g

suci :: (Context a b) Int -> [(Node, Int)]
suci c i = zip2 (suc` c) (repeat i)

leveln :: [(Node,Int)] (gr a b) -> [(Node,Int)] | Graph gr
leveln []         _                  = []
leveln _          g | isEmptyGraph g = []
leveln [(v,j):vs] g = case match v g of
                        (Just c,g`)  -> [(v,j):leveln (vs++suci c (j+1)) g`]
                        (Nothing,g`) -> leveln vs g`
leveln _ _ = abort "Shouldn't happen"

// bfe (breadth first edges)
// remembers predecessor information
//
bfenInternal :: (Queue Edge) (gr a b) -> [Edge] | Graph gr
bfenInternal q g | queueEmpty q || isEmptyGraph g = []
                 | otherwise                      =
      let ((u,v),q`) = queueGet q in
      case match v g of
        (Just c, g`)  -> [(u,v):bfenInternal (queuePutList (outU c) q`) g`]
        (Nothing, g`) -> bfenInternal q` g`

bfen :: [Edge] (gr a b) -> [Edge] | Graph gr
bfen vs g = bfenInternal (queuePutList vs mkQueue) g

bfe :: Node (gr a b) -> [Edge] | Graph gr
bfe v g = bfen [(v,v)] g

outU :: (Context a b) -> [Edge]
outU c = map toEdge (out` c)


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
bft v g = bf (queuePut [v] mkQueue) g

bf :: (Queue Path) (gr a b) -> RTree | Graph gr
bf q g
| queueEmpty q || isEmptyGraph g = []
= case queueGet q of
	(p=:[v:_],q`) = case match v g of
		(Just c, g`)  = [p:bf (queuePutList (map (\x -> [x:p]) (suc` c)) q`) g`]
		(Nothing, g`) = bf q` g`
	(_,_) = abort "shouldn't happen"

esp :: Node Node (gr a b) -> Path | Graph gr
esp s t g = (getPath t o bft s) g

// lesp is a version of esp that returns labeled paths
// Note that the label of the first node in a returned path is meaningless;
// all other nodes are paired with the label of their incoming edge.
//
lbft :: Node (gr a b) -> LRTree b | Graph gr
lbft v g = case out g v of
             []           -> [LP []]
             [(v`,_,l):_] -> lbf (queuePut (LP [(v`,l)]) mkQueue) g


lbf :: (Queue (LPath b)) (gr a b) -> LRTree b | Graph gr
lbf q g
| queueEmpty q || isEmptyGraph g = []
= case queueGet q of
	(LP (p=:[(v,_):_]),q`) = case match v g of
		(Just c, g`) =
			[LP p:lbf (queuePutList (map (\v` -> LP [v`:p]) (lsuc` c)) q`) g`]
		(Nothing, g`) = lbf q` g`
	_ = abort "Shouldn't happen"

lesp :: Node Node (gr a b) -> LPath b | Graph gr
lesp s t g = (getLPath t o lbft s) g
