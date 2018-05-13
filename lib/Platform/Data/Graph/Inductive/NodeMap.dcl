definition module Data.Graph.Inductive.NodeMap

from Data.Map import :: Map
from StdOverloaded import class ==
from StdClass import class Ord
import Data.Graph.Inductive.Graph

:: NodeMap a =
  { map :: Map a Node
  , key :: Int
  }

instance == (NodeMap a) | Eq a

// | Create a new, empty mapping.
new :: NodeMap a

// LNode = (Node, a)

// | Generate a mapping containing the nodes in the given graph.
fromGraph :: (g a b) -> NodeMap a | Ord a & Graph g

// | Generate a labelled node from the given label.  Will return the same node
// for the same label.
mkNode :: (NodeMap a) a -> (LNode a, NodeMap a) | Ord a

// | Generate a labelled node and throw away the modified `NodeMap`.
mkNode_ :: (NodeMap a) a -> LNode a | Ord a

// | Generate a `LEdge` from the node labels.
mkEdge :: (NodeMap a) (a, a, b) -> Maybe (LEdge b) | Ord a

// | Generates a list of `LEdge`s.
mkEdges :: (NodeMap a) [(a, a, b)] -> Maybe [LEdge b] | Ord a

// | Construct a list of nodes.
mkNodes :: (NodeMap a) [a] -> ([LNode a], NodeMap a) | Ord a

map` :: (a b -> (c, a)) a [b] -> ([c], a)

// | Construct a list of nodes and throw away the modified `NodeMap`.
mkNodes_ :: (NodeMap a) [a] -> [LNode a] | Ord a

insMapNode :: (NodeMap a) a (g a b) -> (g a b, NodeMap a, LNode a) | Ord a & DynGraph g

insMapNode_ :: (NodeMap a) a (g a b) -> g a b | Ord a & DynGraph g

insMapEdge :: (NodeMap a) (a, a, b) (g a b) -> g a b | Ord a & DynGraph g

delMapNode :: (NodeMap a) a (g a b) -> g a b | Ord a & DynGraph g

delMapEdge :: (NodeMap a) (a, a) (g a b) -> g a b | Ord a & DynGraph g

insMapNodes :: (NodeMap a) [a] (g a b) -> (g a b, NodeMap a, [LNode a]) | Ord a & DynGraph g

insMapNodes_ :: (NodeMap a) [a] (g a b) -> g a b | Ord a & DynGraph g

insMapEdges :: (NodeMap a) [(a, a, b)] (g a b) -> g a b | Ord a & DynGraph g

delMapNodes :: (NodeMap a) [a] (g a b) -> g a b | Ord a & DynGraph g

delMapEdges :: (NodeMap a) [(a, a)] (g a b) -> g a b | Ord a & DynGraph g

mkMapGraph :: [a] [(a, a, b)] -> (g a b, NodeMap a) | Ord a & DynGraph g

// | Graph construction monad; handles passing both the `NodeMap` and the
// `Graph`.
// TODO
//:: NodeMapM a b g r :== State (NodeMap a, g a b) r

// | Run a construction; return the value of the computation, the modified
// `NodeMap`, and the modified `Graph`.
//run :: (g a b) (NodeMapM a b g r) -> (r, (NodeMap a, g a b)) | Ord a & DynGraph g

// | Run a construction and only return the `Graph`.
//run` :: (g a b) (NodeMapM a b g r) -> g a b | Ord a & DynGraph g

//liftN2 :: ((NodeMap a) c -> (d, NodeMap a)) c -> NodeMapM a b g d

//liftN2` :: ((NodeMap a) c -> d) c -> NodeMapM a b g d

//liftM1 :: ((NodeMap a) c (g a b) -> g a b) c -> NodeMapM a b g ()

//liftM1` :: ((NodeMap a) c (g a b) -> (g a b, NodeMap a, d)) c -> NodeMapM a b g d

// | Monadic node construction.
//mkNodeM :: a -> NodeMapM a b g (LNode a) | Ord a

//mkNodesM :: [a] -> NodeMapM a b g [LNode a] | Ord a

//mkEdgeM :: (a, a, b) -> NodeMapM a b g (Maybe (LEdge b)) | Ord a

//mkEdgesM :: [(a, a, b)] -> NodeMapM a b g (Maybe [LEdge b]) | Ord a

//insMapNodeM :: a -> NodeMapM a b g (LNode a) | Ord a & DynGraph g

//insMapEdgeM :: (a, a, b) -> NodeMapM a b g () | Ord a & DynGraph g

//delMapNodeM :: a -> NodeMapM a b g () | Ord a & DynGraph g

//delMapEdgeM :: (a, a) -> NodeMapM a b g () | Ord a & DynGraph g

//insMapNodesM :: [a] -> NodeMapM a b g [LNode a] | Ord a & DynGraph g

//insMapEdgesM :: [(a, a, b)] -> NodeMapM a b g () | Ord a & DynGraph g

//delMapNodesM :: [a] -> NodeMapM a b g () | Ord a & DynGraph g

//delMapEdgesM :: [(a, a)] -> NodeMapM a b g () | Ord a & DynGraph g
