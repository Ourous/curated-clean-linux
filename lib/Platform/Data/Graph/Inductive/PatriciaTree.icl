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
from           Control.Applicative import liftA2, class Applicative (..)
import Data.Functor
from           Data.IntMap.Strict         import :: IntMap, instance == (IntMap a), instance Functor IntMap
import qualified Data.IntMap.Strict       as IM
from Data.List import instance Functor [], instance Applicative [], instance Monad []
import qualified Data.List as DL
import StdList, StdTuple, StdMisc, StdOrdList
import StdClass, StdFunc, StdOverloaded
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
  //(==) (Gr g1) (Gr g2) = 'IM'.map sortAdj g1 == 'IM'.map sortAdj g2
    //where
      //sortAdj :: (Context` a b) -> Context` a b | == a & == b & Ord a & Ord b
      //sortAdj (p,n,s) = ('IM'.map sort p,n,'IM'.map sort s)

//instance (Show a, Show b) => Show (Gr a b) where
  //showsPrec d g = showParen (d > 10) $
                    //showString "mkGraph "
                    //. shows (labNodes g)
                    //. showString " "
                    //. shows (labEdges g)

instance Graph Gr where
  emptyGraph           = Gr 'IM'.empty

  isEmptyGraph (Gr g)  = 'IM'.null g

  match x y          = matchGr x y

  mkGraph vs es   = (insEdges es
                    o Gr
                    o 'IM'.fromList
                    o 'DL'.map (second (\l -> ('IM'.empty,l,'IM'.empty))))
                    vs

  labNodes (Gr g) = [ (node, label)
                          \\ (node, (_, label, _)) <- 'IM'.toList g ]

  noNodes   (Gr g) = 'IM'.size g

  nodeRange (Gr g) = fromMaybe (abort "nodeRange of empty graph")
                     (liftA2 (\x y -> (x, y)) (ix ('IM'.minViewWithKey g))
                                  (ix ('IM'.maxViewWithKey g)))
    where
      ix = fmap (fst o fst)

  labEdges (Gr g) =                       'IM'.toList g
                >>= \(node, (_, _, s)) -> 'IM'.toList s
                >>= \(next, labels)    -> labels
                >>= \label             -> pure (node, next, label)

  matchAny g = defMatchAny g

instance DynGraph Gr where
  <&> (p, v, l, s) (Gr g)
        #! g1 = 'IM'.insert v (fromAdj p, l, fromAdj s) g
        #! g2 = addSucc g1 v p
        #! g3 = addPred g2 v s
        = Gr g3

matchGr :: Node (Gr a b) -> Decomp Gr a b
matchGr node (Gr g)
    = case 'IM'.lookup node g of
        Nothing
            = (Nothing, Gr g)

        Just (p, label, s)
            #! g1 = 'IM'.delete node g
            #! p` = 'IM'.delete node p
            #! s` = 'IM'.delete node s
            #! g2 = clearPred g1 node ('IM'.keys s`)
            #! g3 = clearSucc g2 node ('IM'.keys p`)
            = (Just (toAdj p`, node, label, toAdj s), Gr g3)

//--------------------------------------------------------------------
// OVERRIDING FUNCTIONS
//--------------------------------------------------------------------

fastInsNode :: (LNode a) !(Gr a b) -> Gr a b
fastInsNode (v, l) (Gr g) = Gr g`
  where
    g` = 'IM'.insert v ('IM'.empty, l, 'IM'.empty) g

fastInsEdge :: (LEdge b) !(Gr a b) -> Gr a b
fastInsEdge (v, w, l) (Gr g)
  #! g1 = 'IM'.adjust addSucc` v g
  #! g2 = 'IM'.adjust addPred` w g1
  = Gr g2
  where
  addSucc` (ps, l`, ss) = (ps, l`, 'IM'.insertWith addLists w [l] ss)
  addPred` (ps, l`, ss) = ('IM'.insertWith addLists v [l] ps, l`, ss)

fastGMap :: ((Context a b) -> Context c d) (Gr a b) -> Gr c d
fastGMap f (Gr g) = Gr ('IM'.mapWithKey f` g)
  where
    //f` :: Node (Context` a b) -> Context` c d
    f` n c = ((\x -> (fromContext o f) o x) o toContext) n c

fastNMap :: (a -> c) (Gr a b) -> Gr c b
fastNMap f (Gr g) = Gr ('IM'.map f` g)
  where
    //f` :: (Context` a b) -> Context` c b
    f` (ps, a, ss) = (ps, f a, ss)

fastEMap :: (b -> c) (Gr a b) -> Gr a c
fastEMap f (Gr g) = Gr ('IM'.map f` g)
  where
    //f` :: (Context` a b) -> Context` a c
    f` (ps, a, ss) = ('IM'.map ('DL'.map f) ps, a, 'IM'.map ('DL'.map f) ss)

fastNEMap :: (a -> c) (b -> d) (Gr a b) -> Gr c d
fastNEMap fn fe (Gr g) = Gr ('IM'.map f g)
  where
    //f :: (Context` a b) -> Context` c d
    f (ps, a, ss) = ('IM'.map ('DL'.map fe) ps, fn a, 'IM'.map ('DL'.map fe) ss)

//--------------------------------------------------------------------
// UTILITIES
//--------------------------------------------------------------------

toAdj :: (IntMap [b]) -> Adj b
toAdj m = ('DL'.concatMap expand o 'IM'.toList) m
  where
    expand (n,ls) = 'DL'.map (flip (\x y -> (x,y)) n) ls

fromAdj :: (Adj b) -> IntMap [b]
fromAdj a = ('IM'.fromListWith addLists o 'DL'.map (second pure o swap)) a

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
      g` = 'IM'.adjust f p g
      f (ps, l`, ss) = (ps, l`, 'IM'.insertWith addLists v [l] ss)


addPred :: (GraphRep a b) Node [(b, Node)] -> GraphRep a b
addPred g _ []              = g
addPred g v [(l, s) : rest] = addPred g` v rest
  where
    g` = 'IM'.adjust f s g
    f (ps, l`, ss) = ('IM'.insertWith addLists v [l] ps, l`, ss)


clearSucc :: (GraphRep a b) Node [Node] -> GraphRep a b
clearSucc g _ []       = g
clearSucc g v [p:rest] = clearSucc g` v rest
  where
    g` = 'IM'.adjust f p g
    f (ps, l, ss) = (ps, l, 'IM'.delete v ss)


clearPred :: (GraphRep a b) Node [Node] -> GraphRep a b
clearPred g _ []       = g
clearPred g v [s:rest] = clearPred g` v rest
  where
    g` = 'IM'.adjust f s g
    f (ps, l, ss) = ('IM'.delete v ps, l, ss)
