implementation module Clean.Types.Tree

import StdBool
import StdInt
import StdFunctions
import StdOrdList

import Clean.Types
import Clean.Types.Unify
import Clean.Types.Util
from Data.Func import $
import Data.GenLexOrd
import Data.Graphviz
import Data.List
import Data.Tuple
from Text import class Text(concat), instance Text String, <+
import Text.GenJSON

:: TypeTree v = Node Type [v] [TypeTree v]

instance zero (TypeTree v) where zero = Node (Var "ra") [] []

instance < (TypeTree v) where < (Node a _ _) (Node b _ _) = a < b
derive gLexOrd Type, Maybe, TypeRestriction
instance < Type where < t u = (t =?= u) =: LT

derive JSONEncode TypeTree, Type, TypeRestriction
derive JSONDecode TypeTree, Type, TypeRestriction

typeTreeNodes :: !(TypeTree v) -> Int
typeTreeNodes (Node _ _ cs) = 1 + sum (map typeTreeNodes cs)

typeTreeSize :: !(TypeTree v) -> Int
typeTreeSize (Node _ vs cs) = length vs + sum (map typeTreeSize cs)

typeTreeDepth :: !(TypeTree v) -> Int
typeTreeDepth (Node _ _ cs) = maxList [0:map ((+) 1 o typeTreeDepth) cs]

addType :: !Type !v !(TypeTree v) -> TypeTree v
addType t v tree=:(Node n vs children)
| t generalises n
	| n generalises t = Node n [v:vs] children
	| otherwise       = Node t [v] [tree]
// A type may end up in different places when there are multiple types that
// generalise it. We sort on the matching types here to avoid that as much as
// is easily possible, because we want the tree to be as small as possible.
| otherwise = case appFst sort $ partition (\(Node t` _ _) -> t` generalises t) children of
	([],_) -> Node n vs [Node t [v] yes:no]
		with (yes,no) = partition (\(Node c _ _) -> t generalises c) children
	([g:gs],rest) -> Node n vs ([addType t v g:gs] ++ rest)

findUnifying :: !Type !(TypeTree v) -> [(Type,Unifier,[v])]
findUnifying t tree=:(Node n ls cs) = case unify t n of
	Nothing -> []
	Just tvas -> [(n,finish_unification [] tvas,ls):concatMap (findUnifying t) cs]

allTypes :: (TypeTree v) -> [(Type,[v],[TypeTree v])]
allTypes (Node t vs cs) = [(t,vs,cs):concatMap allTypes cs]

allValues :: (TypeTree v) -> [v]
allValues (Node _ ls cs) = ls ++ concatMap allValues cs

typeTreeToGraphviz :: !(TypeTree v) -> Digraph
typeTreeToGraphviz tree = Digraph
	"Type tree"
	[GAttRankDir RDLR]
	[NodeDef i []
		[NAttLabel $ concat $ print False n]
		[(c, []) \\ Node t` _ _ <- cs, c <- [i \\ (t,_) <- nodes & i <- [0..] | t == t`]]
		\\ (n,cs) <- nodes & i <- [0..]]
	Nothing
where
	nodes = [(t,cs) \\ (t,vs,cs) <- allTypes tree | not (isEmpty vs) || not (isEmpty cs)]
