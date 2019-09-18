definition module Data.Graph

from Data.Maybe import ::Maybe
from Data.Map import :: Map
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.GenEq import generic gEq

//:: Graph n e
:: Graph n e = 
	{ nodes		:: !Map NodeIndex (Node n)
	, edges		:: !Map EdgeIndex e
	, lastId	:: !Int
	}

:: Node n =
	{ data         :: n
	, predecessors :: ![NodeIndex]
	, successors   :: ![NodeIndex]
	}

:: NodeIndex :== Int
:: EdgeIndex :== (!NodeIndex, !NodeIndex)

derive JSONEncode Graph, Node
derive JSONDecode Graph, Node
derive gEq Graph, Node

//Initialization
emptyGraph :: .(Graph n e)
emptyGraphWithLastId :: Int -> .(Graph n e)
trivialGraph :: n -> .(NodeIndex, .(Graph n e))

//Query
getLastId :: .(Graph n e) -> Int
setLastId :: .(Graph n e) Int -> .Graph n e
nodeIndices :: !.(Graph n e) -> [NodeIndex]
edgeIndices :: !.(Graph n e) -> [EdgeIndex]
nodeCount :: !.(Graph n e) -> Int
edgeCount :: !.(Graph n e) -> Int
directPredecessors :: !NodeIndex !.(Graph n e) -> [NodeIndex]
directSuccessors :: !NodeIndex !.(Graph n e) -> [NodeIndex]
predecessorEdges :: !EdgeIndex !.(Graph n e) -> [EdgeIndex]
successorEdges :: !EdgeIndex !.(Graph n e) -> [EdgeIndex]
getNodeData :: !NodeIndex !.(Graph n e) -> Maybe n
getEdgeData :: !EdgeIndex !.(Graph n e) -> Maybe e
filterNodes :: !([NodeIndex] [NodeIndex] n -> Bool) !.(Graph n e) -> [NodeIndex]

//Predicates
isEmptyGraph :: !.(Graph n e) -> Bool
isTrivialGraph :: !.(Graph n e) -> Bool
nodeExists :: !NodeIndex !.(Graph n e) -> Bool
edgeExists :: !EdgeIndex !.(Graph n e) -> Bool

//Graph manipulation
addNode :: n .(Graph n e) -> .(NodeIndex, .(Graph n e))
addNodeWithIndex :: (NodeIndex -> n) .(Graph n e) -> .(NodeIndex, .(Graph n e))
addEdge :: e EdgeIndex u:(Graph n e) -> v:(Graph n e), [u <= v]
removeNode :: NodeIndex u:(Graph a b) -> v:(Graph a b), [u <= v]
removeEdge :: EdgeIndex u:(Graph a b) -> v:(Graph a b), [u <= v]
setNodeData :: NodeIndex a u:(Graph a b) -> v:(Graph a b), [u <= v]

//Mapping
mapNodes :: !(a -> b) !.(Graph a e) -> .(Graph b e)
mapEdges :: !(a -> b) !.(Graph n a) -> .(Graph n b)
mapIndices :: ![(NodeIndex,NodeIndex)] !.(Graph n e) -> .(Graph n e)

//Folding
foldrNodes :: (NodeIndex (Node n) .a -> .a) .a .(Graph n e) -> .a
foldlNodes :: (.a NodeIndex (Node n) -> .a) .a .(Graph n e) -> .a
foldrEdges :: (EdgeIndex e .a -> .a) .a .(Graph n e) -> .a
foldlEdges :: (.a EdgeIndex e -> .a) .a .(Graph n e) -> .a

//Connectivity
components :: !.(Graph n e) -> .[.(Graph n e)]
isConnected :: !.(Graph n e) -> Bool

//Leafs
leafNodes :: !.(Graph n e) -> [NodeIndex]

//Two-terminal graphs
sourceNode :: !.(Graph n e) -> Maybe NodeIndex
sinkNode :: !.(Graph n e) -> Maybe NodeIndex
