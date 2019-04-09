implementation module Data.Graph

import StdBool
import StdFunc
import StdInt
from StdList import ++, filter, hd, isEmpty, isMember, map, reverse, tl,
	instance length [], foldr
import StdOrdList
import StdTuple

import Data.Map, Data.Map.GenJSON
import Data.Maybe
import Data.Functor
import Text.GenJSON
from Data.GenEq import generic gEq

:: Graph n e = 
	{ nodes		:: !Map NodeIndex (Node n)
	, edges		:: !Map EdgeIndex e
	, lastId	:: !Int
	}

:: Node n =
	{ data			:: n
	, predecessors	:: ![NodeIndex]
	, successors 	:: ![NodeIndex]
	}

derive JSONEncode Graph, Node
derive JSONDecode Graph, Node
derive gEq Graph, Node

emptyGraph :: .(Graph n e)
emptyGraph = 
	{ Graph
	| nodes = newMap
	, edges = newMap
	, lastId = 0
	}

emptyGraphWithLastId :: Int -> .(Graph n e)
emptyGraphWithLastId n = 
	{ Graph
	| nodes = newMap
	, edges = newMap
	, lastId = n
	}

getLastId :: .(Graph n e) -> Int
getLastId g = g.lastId

setLastId :: .(Graph n e) Int -> .Graph n e
setLastId g n = {g & lastId = n}

addNode :: n .(Graph n e) -> .(NodeIndex, .(Graph n e))
addNode n graph = addNodeWithIndex (\_ -> n) graph

addNodeWithIndex :: (NodeIndex -> n) .(Graph n e) -> .(NodeIndex, .(Graph n e))
addNodeWithIndex fn graph 
	# newId = inc graph.lastId
	# node =	{ Node
				| data			= fn newId
				, predecessors	= []
				, successors	= []
				}
	=	(	newId
		,	{ Graph
			| graph
			& nodes = put newId node graph.nodes
			, lastId = newId
			}
		)
		
addEdge :: e EdgeIndex u:(Graph n e) -> v:(Graph n e), [u <= v]
addEdge e (fromIndex,toIndex) graph
	| edgeExists (fromIndex,toIndex) graph = graph
	# mFromNode = get fromIndex graph.nodes
	| isNothing mFromNode = graph
	# fromNode = fromJust mFromNode
	# fromNode =	{ Node
					| fromNode
					& successors = [toIndex:fromNode.successors]
					}
	# mToNode = get toIndex graph.nodes
	| isNothing mToNode = graph
	# toNode = fromJust mToNode
	# toNode = 		{ Node
					| toNode
					& predecessors = [fromIndex:toNode.predecessors]
					}
	=	{ Graph
		| graph
		& nodes = put toIndex toNode (put fromIndex fromNode graph.nodes)
		, edges = put (fromIndex,toIndex) e graph.edges
		}

removeNode :: NodeIndex u:(Graph a b) -> v:(Graph a b), [u <= v]
removeNode nodeIndex graph
	# mNode = getNode nodeIndex graph
	| isNothing mNode = graph
	# node = fromJust mNode
	# graph = foldr removeEdge graph [ (nodeIndex,n) \\ n <- node.successors ]
	# graph = foldr removeEdge graph [ (n,nodeIndex) \\ n <- node.predecessors ]
	=	{ Graph
		| graph
		& nodes = del nodeIndex graph.nodes
		}

removeEdge :: EdgeIndex u:(Graph a b) -> v:(Graph a b), [u <= v]
removeEdge (fromNodeIndex,toNodeIndex) graph
	# mFromNode = getNode fromNodeIndex graph
	  mToNode = getNode toNodeIndex graph
	| isNothing mFromNode || isNothing mToNode = graph
	# fromNode = fromJust mFromNode
	  toNode = fromJust mToNode
	# fromNode = { Node | fromNode & successors = filter ((<>) toNodeIndex) fromNode.successors }
      toNode   = { Node | toNode   & predecessors = filter ((<>) fromNodeIndex) toNode.predecessors }
    =	{ Graph
		| graph
		& nodes = put toNodeIndex toNode (put fromNodeIndex fromNode graph.nodes)
		, edges = del (fromNodeIndex,toNodeIndex) graph.edges
		}
		
setNodeData :: NodeIndex a u:(Graph a b) -> v:(Graph a b), [u <= v]
setNodeData nodeIndex data graph
	# mNode = getNode nodeIndex graph
	| isNothing mNode = graph
	# node = {Node | fromJust mNode & data = data}
	= { Graph
	  | graph
	  & nodes = put nodeIndex node graph.nodes
	  }

trivialGraph :: n -> .(NodeIndex, .(Graph n e))
trivialGraph node = addNode node emptyGraph

isEmptyGraph :: !.(Graph n e) -> Bool
isEmptyGraph graph = isEmpty (toList graph.nodes)

isTrivialGraph :: !.(Graph n e) -> Bool
isTrivialGraph graph = case toList graph.nodes of
	[] = False
	[_] = isEmpty (toList graph.edges)
	_ = False

nodeIndices :: !.(Graph n e) -> [NodeIndex]
nodeIndices graph = (map fst (toList graph.nodes))

edgeIndices :: !.(Graph n e) -> [EdgeIndex]
edgeIndices graph = map fst (toList graph.edges)

getNode :: Int (Graph n e) -> Maybe (Node n)
getNode nodeIndex graph = get nodeIndex graph.nodes

nodeExists :: !NodeIndex !.(Graph n e) -> Bool
nodeExists nodeIndex graph = isJust (get nodeIndex graph.nodes)

edgeExists :: !EdgeIndex !.(Graph n e) -> Bool
edgeExists edgeIndex graph = isJust (get edgeIndex graph.edges)

nodeCount :: !.(Graph n e) -> Int
nodeCount graph = length (toList graph.nodes)

edgeCount :: !.(Graph n e) -> Int
edgeCount graph = length (toList graph.edges)

directPredecessors :: !NodeIndex !.(Graph n e) -> [NodeIndex]
directPredecessors nodeIndex graph = 
	case get nodeIndex graph.nodes of
		Nothing = []
		Just n = n.predecessors
		
directSuccessors :: !NodeIndex !.(Graph n e) -> [NodeIndex]
directSuccessors nodeIndex graph = 
	case get nodeIndex graph.nodes of
		Nothing = []
		Just n = n.successors

predecessorEdges :: !EdgeIndex !.(Graph n e) -> [EdgeIndex]
predecessorEdges startEdge graph = tl (predecessors` [startEdge] [])
where
	predecessors` :: [EdgeIndex] [EdgeIndex] -> [EdgeIndex]
	predecessors` [] visited = reverse visited
	predecessors` [edge=:(fromNode,toNode):edges] visited
		| isMember edge visited = predecessors` edges visited
		| not (nodeExists fromNode graph) = predecessors` edges visited
		= predecessors` (edges ++ [(i,fromNode) \\ i <- directPredecessors fromNode graph]) [edge:visited]
		
successorEdges :: !EdgeIndex !.(Graph n e) -> [EdgeIndex]
successorEdges startEdge graph  = tl (successors` [startEdge] [])
where
	successors` :: [EdgeIndex] [EdgeIndex] -> [EdgeIndex]
	successors` [] visited = reverse visited
	successors` [edge=:(fromNode,toNode):edges] visited
		| isMember edge visited = successors` edges visited
		| not (nodeExists toNode graph) = successors` edges visited
		= successors` (edges ++ [(toNode,i) \\ i <- directSuccessors toNode graph]) [edge:visited]
		
		
		
		
getNodeData :: !NodeIndex !.(Graph n e) -> Maybe n
getNodeData nodeIndex graph = 
	case get nodeIndex graph.nodes of
		Nothing = Nothing
		Just n = Just n.Node.data

getEdgeData :: !EdgeIndex !.(Graph n e) -> Maybe e
getEdgeData edgeIndex graph = get edgeIndex graph.edges

filterNodes :: !([NodeIndex] [NodeIndex] n -> Bool) !.(Graph n e) -> [NodeIndex]
filterNodes pred graph = [ i \\ (i,n) <- toList graph.nodes | pred n.predecessors n.successors n.data]

mapNodes :: !(a -> b) !.(Graph a e) -> .(Graph b e)
mapNodes f graph = { Graph | graph & nodes = mapMap (fmap f) graph.nodes }
		
mapEdges :: !(a -> b) !.(Graph n a) -> .(Graph n b)
mapEdges f graph = { Graph | graph & edges = mapMap f graph.edges }

instance Functor Node
where
	fmap f node = { Node | node & data = f node.Node.data }

mapMap :: (a -> b) (Map k a) -> (Map k b) | Eq k & Ord k
mapMap f m = (fromList o map (app2 (id,f)) o toList) m

mapIndices :: ![(!NodeIndex,!NodeIndex)] !.(Graph n e) -> .(Graph n e)
mapIndices updates { nodes, edges } 
	# updMap = fromList updates
	= { Graph
	| nodes = fromList [ (fromJust (get k updMap),updateNode updMap v) \\ (k,v) <- toList nodes ]
	, edges = fromList [ ((fromJust (get k updMap),fromJust (get l updMap)),v) \\ ((k,l),v) <- toList edges ]
	, lastId = maxList (map snd (toList updMap))
	}
	where	
	updateNode :: (Map NodeIndex NodeIndex) (Node n) -> Node n
	updateNode updMap { data, predecessors, successors } =
		{ Node
		| data = data
		, predecessors = [ fromJust (get k updMap) \\ k <- predecessors ]
		, successors   = [ fromJust (get k updMap) \\ k <- successors   ]
		}
	
//--------------------------------------------------------------------------------
//Folding
foldrNodes :: (NodeIndex (Node n) .a -> .a) .a .(Graph n e) -> .a
foldrNodes f b g = foldrWithKey f b g.nodes

foldlNodes :: (.a NodeIndex (Node n) -> .a) .a .(Graph n e) -> .a
foldlNodes f b g = foldlWithKey f b g.nodes

foldrEdges :: (EdgeIndex e .a -> .a) .a .(Graph n e) -> .a
foldrEdges f b g = foldrWithKey f b g.edges

foldlEdges :: (.a EdgeIndex e -> .a) .a .(Graph n e) -> .a
foldlEdges f b g = foldlWithKey f b g.edges

//--------------------------------------------------------------------------------
//Connectivity
components :: !.(Graph n e) -> .[.Graph n e]
components graph = fst (components` graph)
where
    components` :: .(Graph a b) -> u:(v:[w:(Graph a b)],Graph a b), [u <= v,v <= w]
	components` graph | isEmptyGraph graph = ([], graph)
	components` graph 
		# (comp,graph`) = findComponentNodes [(fst o hd o toList) graph.nodes] [] graph
		# comp = nodeListToSubgraph comp graph
		# (comps,graph`) = components` graph`
		= ([comp:comps],graph`)
		
    findComponentNodes :: [Int] [Int] .(Graph a b) -> .([Int],Graph a b)
	findComponentNodes [] acc graph = (acc, graph)
	findComponentNodes [n:ns] acc graph =
		case get n graph.nodes of
			Nothing = findComponentNodes ns acc graph
			Just node = let adjacents = node.predecessors ++ node.successors
			            in findComponentNodes (adjacents ++ ns) [n:acc] (removeNode n graph)

	nodeListToSubgraph :: [Int] !.(Graph n e) -> .(Graph n e).
	nodeListToSubgraph nodeIndices graph = 
		{ Graph
		| nodes = fromList [ (n, filterEdges (fromJust (getNode n graph))) \\ n <- nodeIndices ]
		, edges = (fromList o filter (\((a,b),_) -> isMember a nodeIndices && isMember b nodeIndices) o toList) graph.edges
		, lastId = foldr max 0 nodeIndices
		}
		where
			filterEdges :: (Node n) -> Node n
			filterEdges node =	{ Node
								| node
								& predecessors = filter (flip isMember nodeIndices) node.predecessors
								, successors   = filter (flip isMember nodeIndices) node.successors
								}

isConnected :: !.(Graph n e) -> Bool
isConnected graph = case components graph of 
	[]     = True
	[c]    = True
	_	   = False

//--------------------------------------------------------------------------------
// Get all nodes without successors
leafNodes :: !.(Graph n e) -> [NodeIndex]
leafNodes graph = filterNodes (\_ successors _ -> isEmpty successors) graph

//--------------------------------------------------------------------------------
// For Two-terminal graphs
sourceNode :: !.(Graph n e) -> Maybe NodeIndex
sourceNode graph = case filterNodes (\predecessors _ _ -> isEmpty predecessors) graph of
	[n] = Just n
	_   = Nothing

sinkNode :: !.(Graph n e) -> Maybe NodeIndex
sinkNode graph = case filterNodes (\_ successors _ -> isEmpty successors) graph of
	[n] = Just n
	_   = Nothing
