// | Utility methods to automatically generate and keep track of a mapping
// between node labels and `Node`s.
implementation module Data.Graph.Inductive.NodeMap

//import           Control.Monad.Trans.State // TODO Implement monadic interface
import Control.Monad, Control.Applicative, Data.Functor
import StdOverloaded, StdBool, StdClass, StdTuple, StdInt
import Data.Maybe
//import           Data.Graph.Inductive.Graph
from Data.Graph.Inductive.Graph import class Graph, class DynGraph, :: LNode, :: Node, :: LEdge, :: Edge
import qualified Data.Graph.Inductive.Graph
from             Data.Map import :: Map, instance == (Map k v)
import qualified Data.Map
import qualified Data.List
import Data.Func
import StdEnv

:: NodeMap a =
  { map :: Map a Node
  , key :: Int
  }

instance == (NodeMap a) | Eq a where
  (==) {map, key} {map = map`, key = key`} = key == key` && map == map`

// | Create a new, empty mapping.
new :: NodeMap a
new = { map = 'Data.Map'.newMap, key = 0 }

// LNode = (Node, a)

// | Generate a mapping containing the nodes in the given graph.
fromGraph :: (g a b) -> NodeMap a | Ord a & Graph g
fromGraph g =
    let ns = 'Data.Graph.Inductive.Graph'.labNodes g
        aux (n, a) (m`, k`) = ('Data.Map'.put a n m`, max n k`)
        (m, k) = 'Data.List'.foldr aux ('Data.Map'.newMap, 0) ns
    in {NodeMap | map = m, key = k+1 }

// | Generate a labelled node from the given label.  Will return the same node
// for the same label.
mkNode :: (NodeMap a) a -> (LNode a, NodeMap a) | Ord a
mkNode m=:{NodeMap | map = mp, key = k} a =
    case 'Data.Map'.get a mp of
        Just i        -> ((i, a), m)
        Nothing        ->
            let m` = { NodeMap | map = 'Data.Map'.put a k mp, key = k+1 }
            in ((k, a), m`)

// | Generate a labelled node and throw away the modified `NodeMap`.
mkNode_ :: (NodeMap a) a -> LNode a | Ord a
mkNode_ m a = fst (mkNode m a)

// | Generate a `LEdge` from the node labels.
mkEdge :: (NodeMap a) (a, a, b) -> Maybe (LEdge b) | Ord a
mkEdge {NodeMap | map = m} (a1, a2, b) = 'Data.Map'.get a1 m >>= \n1 -> 'Data.Map'.get a2 m >>= \n2 -> pure (n1, n2, b)

// | Generates a list of `LEdge`s.
mkEdges :: (NodeMap a) [(a, a, b)] -> Maybe [LEdge b] | Ord a
mkEdges m xs = mapM (mkEdge m) xs

// | Construct a list of nodes.
mkNodes :: (NodeMap a) [a] -> ([LNode a], NodeMap a) | Ord a
mkNodes m xs = map` mkNode m xs

map` :: (a b -> (c, a)) a [b] -> ([c], a)
map` _ a [] = ([], a)
map` f a [b:bs] =
    let (c, a`) = f a b
        (cs, a``) = map` f a` bs
    in ([c:cs], a``)

// | Construct a list of nodes and throw away the modified `NodeMap`.
mkNodes_ :: (NodeMap a) [a] -> [LNode a] | Ord a
mkNodes_ m as = fst (mkNodes m as)

insMapNode :: (NodeMap a) a (g a b) -> (g a b, NodeMap a, LNode a) | Ord a & DynGraph g
insMapNode m a g =
    let (n, m`) = mkNode m a
    in ('Data.Graph.Inductive.Graph'.insNode n g, m`, n)

insMapNode_ :: (NodeMap a) a (g a b) -> g a b | Ord a & DynGraph g
insMapNode_ m a g =
    let (g`, _, _) = insMapNode m a g
    in g`

insMapEdge :: (NodeMap a) (a, a, b) (g a b) -> g a b | Ord a & DynGraph g
insMapEdge m e g =
    let e` = maybe (abort "shouldn't happen") id $ mkEdge m e
    in 'Data.Graph.Inductive.Graph'.insEdge e` g

delMapNode :: (NodeMap a) a (g a b) -> g a b | Ord a & DynGraph g
delMapNode m a g =
    let (n, _) = mkNode_ m a
    in 'Data.Graph.Inductive.Graph'.delNode n g

delMapEdge :: (NodeMap a) (a, a) (g a b) -> g a b | Ord a & DynGraph g
delMapEdge m (n1, n2) g =
    let (n1`, n2`, _) = maybe (abort "shouldn't happen") id $ mkEdge m (n1, n2, ())
    in 'Data.Graph.Inductive.Graph'.delEdge (n1`, n2`) g

insMapNodes :: (NodeMap a) [a] (g a b) -> (g a b, NodeMap a, [LNode a]) | Ord a & DynGraph g
insMapNodes m as g =
    let (ns, m`) = mkNodes m as
    in ('Data.Graph.Inductive.Graph'.insNodes ns g, m`, ns)

insMapNodes_ :: (NodeMap a) [a] (g a b) -> g a b | Ord a & DynGraph g
insMapNodes_ m as g =
    let (g`, _, _) = insMapNodes m as g
    in g`

insMapEdges :: (NodeMap a) [(a, a, b)] (g a b) -> g a b | Ord a & DynGraph g
insMapEdges m es g =
    let es` = maybe (abort "Shouldn't happen") id $ mkEdges m es
    in 'Data.Graph.Inductive.Graph'.insEdges es` g

delMapNodes :: (NodeMap a) [a] (g a b) -> g a b | Ord a & DynGraph g
delMapNodes m as g =
    let ns = 'Data.List'.map fst (mkNodes_ m as)
    in 'Data.Graph.Inductive.Graph'.delNodes ns g

delMapEdges :: (NodeMap a) [(a, a)] (g a b) -> g a b | Ord a & DynGraph g
delMapEdges m ns g =
    let ns` =  maybe (abort "shouldn't happen") id $ mkEdges m ('Data.List'.map (\(a, b) -> (a, b, ())) ns)
        ns`` = 'Data.List'.map (\(a, b, _) -> (a, b)) ns`
    in 'Data.Graph.Inductive.Graph'.delEdges ns`` g

mkMapGraph :: [a] [(a, a, b)] -> (g a b, NodeMap a) | Ord a & DynGraph g
mkMapGraph ns es =
    let (ns`, m`) = mkNodes new ns
        es` = maybe (abort "shouldn't happen") id $ mkEdges m` es
    in ('Data.Graph.Inductive.Graph'.mkGraph ns` es`, m`)

// | Graph construction monad; handles passing both the `NodeMap` and the
// `Graph`.
// TODO
//:: NodeMapM a b g r :== State (NodeMap a, g a b) r

// | Run a construction; return the value of the computation, the modified
// `NodeMap`, and the modified `Graph`.
//run :: (g a b) (NodeMapM a b g r) -> (r, (NodeMap a, g a b)) | Ord a & DynGraph g
//run g m = runState m (fromGraph g, g)

// | Run a construction and only return the `Graph`.
//run` :: (g a b) (NodeMapM a b g r) -> g a b | Ord a & DynGraph g
//run` g m = (snd o snd) (run g m)

//liftN2 :: ((NodeMap a) c -> (d, NodeMap a)) c -> NodeMapM a b g d
//liftN2 f c = get >>= \(m, g) -> let (r, m`) = f m c in put (m`, g) >>| pure r

//liftN2` :: ((NodeMap a) c -> d) c -> NodeMapM a b g d
//liftN2` f c = get >>= \(m, _) -> pure (f m c)

//liftM1 :: ((NodeMap a) c (g a b) -> g a b) c -> NodeMapM a b g ()
//liftM1 f c = get >>= \(m, g) -> let g` = f m c g in put (m, g`)

//liftM1` :: ((NodeMap a) c (g a b) -> (g a b, NodeMap a, d)) c -> NodeMapM a b g d
//liftM1` f c = get >>= \(m, g) -> let (g`, m`, r) = f m c g in put (m`, g`) >>| pure r

// | Monadic node construction.
//mkNodeM :: a -> NodeMapM a b g (LNode a) | Ord a
//mkNodeM x = liftN2 mkNode x

//mkNodesM :: [a] -> NodeMapM a b g [LNode a] | Ord a
//mkNodesM xs = liftN2 mkNodes xs

//mkEdgeM :: (a, a, b) -> NodeMapM a b g (Maybe (LEdge b)) | Ord a
//mkEdgeM x = liftN2` mkEdge x

//mkEdgesM :: [(a, a, b)] -> NodeMapM a b g (Maybe [LEdge b]) | Ord a
//mkEdgesM xs = liftN2` mkEdges xs

//insMapNodeM :: a -> NodeMapM a b g (LNode a) | Ord a & DynGraph g
//insMapNodeM x = liftM1` insMapNode x

//insMapEdgeM :: (a, a, b) -> NodeMapM a b g () | Ord a & DynGraph g
//insMapEdgeM x = liftM1 insMapEdge x

//delMapNodeM :: a -> NodeMapM a b g () | Ord a & DynGraph g
//delMapNodeM x = liftM1 delMapNode x

//delMapEdgeM :: (a, a) -> NodeMapM a b g () | Ord a & DynGraph g
//delMapEdgeM x = liftM1 delMapEdge x

//insMapNodesM :: [a] -> NodeMapM a b g [LNode a] | Ord a & DynGraph g
//insMapNodesM xs = liftM1` insMapNodes xs

//insMapEdgesM :: [(a, a, b)] -> NodeMapM a b g () | Ord a & DynGraph g
//insMapEdgesM xs = liftM1 insMapEdges xs

//delMapNodesM :: [a] -> NodeMapM a b g () | Ord a & DynGraph g
//delMapNodesM xs = liftM1 delMapNodes xs

//delMapEdgesM :: [(a, a)] -> NodeMapM a b g () | Ord a & DynGraph g
//delMapEdgesM xs = liftM1 delMapEdges xs
