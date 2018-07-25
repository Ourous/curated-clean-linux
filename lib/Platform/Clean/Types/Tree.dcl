definition module Clean.Types.Tree

from StdOverloaded import class zero

from Clean.Types import :: Type, :: Unifier
from Data.Graphviz import :: Digraph
from Data.Maybe import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: TypeTree v

instance zero (TypeTree v)
derive JSONEncode TypeTree
derive JSONDecode TypeTree

typeTreeNodes :: !(TypeTree v) -> Int
typeTreeSize :: !(TypeTree v) -> Int
typeTreeDepth :: !(TypeTree v) -> Int
addType :: !Type !v !(TypeTree v) -> TypeTree v
findUnifying :: !Type !(TypeTree v) -> [(Type,Unifier,[v])]
typeTreeToGraphviz :: !(TypeTree v) -> Digraph
