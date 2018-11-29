// |An efficient implementation of `Data.Graph.Inductive.Graph.Graph`
// using big-endian patricia tree (i.e. "Data.IntMap").
//
// This module provides the following specialised functions to gain
// more performance, using GHC`s RULES pragma:
//
// * `Data.Graph.Inductive.Graph.insNode`
//
// * `Data.Graph.Inductive.Graph.insEdge`
//
// * `Data.Graph.Inductive.Graph.gmap`
//
// * `Data.Graph.Inductive.Graph.nmap`
//
// * `Data.Graph.Inductive.Graph.emap`

implementation module Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Graph

from Control.Monad import class Monad (..), >>=
from           Control.Applicative import liftA2, class pure(..), class Applicative
import Data.Functor
from           Data.IntMap.Strict         import :: IntMap, instance == (IntMap a), instance Functor IntMap
import qualified Data.IntMap.Strict
from Data.List import instance Functor [], instance pure [], instance <*> [], instance Monad []
import qualified Data.List
import StdList, StdTuple, StdMisc, StdOrdList
import StdClass, StdFunctions, StdOverloaded
import Data.Maybe

import Control.Arrow

//--------------------------------------------------------------------
// GRAPH REPRESENTATION
//--------------------------------------------------------------------

:: Gr a b = Gr (GraphRep a b)

:: GraphRep a b :== IntMap (Context` a b)
:: Context` a b :== (IntMap [b], a, IntMap [b])

:: UGr :== Gr () ()

//--------------------------------------------------------------------
// CLASS INSTANCES
//--------------------------------------------------------------------

// TODO
//instance == (Gr a b) | == a & == b & Ord a & Ord b where
  //(==) (Gr g1) (Gr g2) = 'Data.IntMap.Strict'.map sortAdj g1 == 'Data.IntMap.Strict'.map sortAdj g2
    //where
      //sortAdj :: (Context` a b) -> Context` a b | == a & == b & Ord a & Ord b
      //sortAdj (p,n,s) = ('Data.IntMap.Strict'.map sort p,n,'Data.IntMap.Strict'.map sort s)

//instance (Show a, Show b) => Show (Gr a b) where
  //showsPrec d g = showParen (d > 10) $
                    //showString "mkGraph "
                    //. shows (labNodes g)
                    //. showString " "
                    //. shows (labEdges g)

instance Graph Gr where
  emptyGraph           = Gr 'Data.IntMap.Strict'.empty

  isEmptyGraph (Gr g)  = 'Data.IntMap.Strict'.null g

  match x y          = matchGr x y

  mkGraph vs es   = (insEdges es
                    o Gr
                    o 'Data.IntMap.Strict'.fromList
                    o 'Data.List'.map (second (\l -> ('Data.IntMap.Strict'.empty,l,'Data.IntMap.Strict'.empty))))
                    vs

  labNodes (Gr g) = [ (node, label)
                          \\ (node, (_, label, _)) <- 'Data.IntMap.Strict'.toList g ]

  noNodes   (Gr g) = 'Data.IntMap.Strict'.size g

  nodeRange (Gr g) = fromMaybe (abort "nodeRange of empty graph")
                     (liftA2 (\x y -> (x, y)) (ix ('Data.IntMap.Strict'.minViewWithKey g))
                                  (ix ('Data.IntMap.Strict'.maxViewWithKey g)))
    where
      ix = fmap (fst o fst)

  labEdges (Gr g) =                       'Data.IntMap.Strict'.toList g
                >>= \(node, (_, _, s)) -> 'Data.IntMap.Strict'.toList s
                >>= \(next, labels)    -> labels
                >>= \label             -> pure (node, next, label)

  matchAny g = defMatchAny g

instance DynGraph Gr where
  <&> (p, v, l, s) (Gr g)
        #! g1 = 'Data.IntMap.Strict'.insert v (fromAdj p, l, fromAdj s) g
        #! g2 = addSucc g1 v p
        #! g3 = addPred g2 v s
        = Gr g3

matchGr :: Node (Gr a b) -> Decomp Gr a b
matchGr node (Gr g)
    = case 'Data.IntMap.Strict'.lookup node g of
        Nothing
            = (Nothing, Gr g)

        Just (p, label, s)
            #! g1 = 'Data.IntMap.Strict'.delete node g
            #! p` = 'Data.IntMap.Strict'.delete node p
            #! s` = 'Data.IntMap.Strict'.delete node s
            #! g2 = clearPred g1 node ('Data.IntMap.Strict'.keys s`)
            #! g3 = clearSucc g2 node ('Data.IntMap.Strict'.keys p`)
            = (Just (toAdj p`, node, label, toAdj s), Gr g3)

//--------------------------------------------------------------------
// OVERRIDING FUNCTIONS
//--------------------------------------------------------------------

fastInsNode :: (LNode a) !(Gr a b) -> Gr a b
fastInsNode (v, l) (Gr g) = Gr g`
  where
    g` = 'Data.IntMap.Strict'.insert v ('Data.IntMap.Strict'.empty, l, 'Data.IntMap.Strict'.empty) g

fastInsEdge :: (LEdge b) !(Gr a b) -> Gr a b
fastInsEdge (v, w, l) (Gr g)
  #! g1 = 'Data.IntMap.Strict'.adjust addSucc` v g
  #! g2 = 'Data.IntMap.Strict'.adjust addPred` w g1
  = Gr g2
  where
  addSucc` (ps, l`, ss) = (ps, l`, 'Data.IntMap.Strict'.insertWith addLists w [l] ss)
  addPred` (ps, l`, ss) = ('Data.IntMap.Strict'.insertWith addLists v [l] ps, l`, ss)

fastGMap :: ((Context a b) -> Context c d) (Gr a b) -> Gr c d
fastGMap f (Gr g) = Gr ('Data.IntMap.Strict'.mapWithKey f` g)
  where
    //f` :: Node (Context` a b) -> Context` c d
    f` n c = ((\x -> (fromContext o f) o x) o toContext) n c

fastNMap :: (a -> c) (Gr a b) -> Gr c b
fastNMap f (Gr g) = Gr ('Data.IntMap.Strict'.map f` g)
  where
    //f` :: (Context` a b) -> Context` c b
    f` (ps, a, ss) = (ps, f a, ss)

fastEMap :: (b -> c) (Gr a b) -> Gr a c
fastEMap f (Gr g) = Gr ('Data.IntMap.Strict'.map f` g)
  where
    //f` :: (Context` a b) -> Context` a c
    f` (ps, a, ss) = ('Data.IntMap.Strict'.map ('Data.List'.map f) ps, a, 'Data.IntMap.Strict'.map ('Data.List'.map f) ss)

fastNEMap :: (a -> c) (b -> d) (Gr a b) -> Gr c d
fastNEMap fn fe (Gr g) = Gr ('Data.IntMap.Strict'.map f g)
  where
    //f :: (Context` a b) -> Context` c d
    f (ps, a, ss) = ('Data.IntMap.Strict'.map ('Data.List'.map fe) ps, fn a, 'Data.IntMap.Strict'.map ('Data.List'.map fe) ss)

//--------------------------------------------------------------------
// UTILITIES
//--------------------------------------------------------------------

toAdj :: (IntMap [b]) -> Adj b
toAdj m = ('Data.List'.concatMap expand o 'Data.IntMap.Strict'.toList) m
  where
    expand (n,ls) = 'Data.List'.map (flip (\x y -> (x,y)) n) ls

fromAdj :: (Adj b) -> IntMap [b]
fromAdj a = ('Data.IntMap.Strict'.fromListWith addLists o 'Data.List'.map (second pure o swap)) a

toContext :: Node (Context` a b) -> Context a b
toContext v (ps, a, ss) = (toAdj ps, v, a, toAdj ss)

fromContext :: (Context a b) -> Context` a b
fromContext (ps, _, a, ss) = (fromAdj ps, a, fromAdj ss)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

// A version of @++@ where order isn`t important, so @xs ++ [x]@
// becomes @x:xs@.  Used when we have to have a function of type @[a]
// -> [a] -> [a]@ but one of the lists is just going to be a single
// element (and it isn`t possible to tell which).
addLists :: [a] [a] -> [a]
addLists [a] as  = [a : as]
addLists as  [a] = [a : as]
addLists xs  ys  = xs ++ ys

addSucc :: (GraphRep a b) Node [(b, Node)] -> GraphRep a b
addSucc g _ []              = g
addSucc g v [(l, p) : rest] = addSucc g` v rest
    where
      g` = 'Data.IntMap.Strict'.adjust f p g
      f (ps, l`, ss) = (ps, l`, 'Data.IntMap.Strict'.insertWith addLists v [l] ss)


addPred :: (GraphRep a b) Node [(b, Node)] -> GraphRep a b
addPred g _ []              = g
addPred g v [(l, s) : rest] = addPred g` v rest
  where
    g` = 'Data.IntMap.Strict'.adjust f s g
    f (ps, l`, ss) = ('Data.IntMap.Strict'.insertWith addLists v [l] ps, l`, ss)


clearSucc :: (GraphRep a b) Node [Node] -> GraphRep a b
clearSucc g _ []       = g
clearSucc g v [p:rest] = clearSucc g` v rest
  where
    g` = 'Data.IntMap.Strict'.adjust f p g
    f (ps, l, ss) = (ps, l, 'Data.IntMap.Strict'.delete v ss)


clearPred :: (GraphRep a b) Node [Node] -> GraphRep a b
clearPred g _ []       = g
clearPred g v [s:rest] = clearPred g` v rest
  where
    g` = 'Data.IntMap.Strict'.adjust f s g
    f (ps, l, ss) = ('Data.IntMap.Strict'.delete v ps, l, ss)
