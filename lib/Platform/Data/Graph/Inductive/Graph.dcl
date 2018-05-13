// (c) 1999-2005 by Martin Erwig [see file COPYRIGHT]
// | Static and Dynamic Inductive Graphs
definition module Data.Graph.Inductive.Graph

from Data.Maybe import :: Maybe
from StdOverloaded import class <, class ==
from StdClass import class Eq
from Data.GenLexOrd import generic gLexOrd, :: LexOrd

// | Unlabeled node
:: Node   :== Int
// | Labeled node
:: LNode a :== (Node,a)
// | Quasi-unlabeled node
:: UNode   :== LNode ()

// | Unlabeled edge
:: Edge   :== (Node,Node)
// | Labeled edge
:: LEdge b :== (Node,Node,b)
// | Quasi-unlabeled edge
:: UEdge   :== LEdge ()

// | Unlabeled path
:: Path    :== [Node]
// | Labeled path
:: LPath a = LP [LNode a]

unLPath :: (LPath a) -> [LNode a]

//instance toString (LPath a) | toString a

instance == (LPath a) | == a
instance < (LPath a) | gLexOrd{|*|} a

// TODO gLexOrd?
//instance (Ord a) => Ord (LPath a) where
  //compare (LP [])        (LP [])        = EQ
  //compare (LP ((_,x):_)) (LP ((_,y):_)) = compare x y
  //compare _ _ = abort "LPath: cannot compare two empty paths"

// | Quasi-unlabeled path
:: UPath   :== [UNode]

// | Labeled links to or from a 'Node'.
:: Adj b        :== [(b,Node)]
// | Links to the 'Node', the 'Node' itself, a label, links from the 'Node'.
:: Context a b  :== (Adj b,Node,a,Adj b) // Context a b "=" Context' a b "+" Node
:: MContext a b :== Maybe (Context a b)
// | 'Graph' decomposition - the context removed from a 'Graph', and the rest
// of the 'Graph'.
:: Decomp g a b :== (MContext a b,g a b)
// | The same as 'Decomp', only more sure of itself.
:: GDecomp g a b  :== (Context a b,g a b)

// | Unlabeled context.
:: UContext     :== ([Node],Node,[Node])
// | Unlabeled decomposition.
:: UDecomp g    :== (Maybe UContext,g)

// | Minimum implementation: 'empty', 'isEmptyGraph', 'match', 'mkGraph', 'labNodes'
class Graph gr where
  // | An empty 'Graph'.
  emptyGraph     :: gr a b

  // | True if the given 'Graph' is empty.
  isEmptyGraph   :: (gr a b) -> Bool

  // | Decompose a 'Graph' into the 'MContext' found for the given node and the
  // remaining 'Graph'.
  match     :: Node (gr a b) -> Decomp gr a b

  // | Create a 'Graph' from the list of 'LNode's and 'LEdge's.
  //
  //   For graphs that are also instances of 'DynGraph', @mkGraph ns
  //   es@ should be equivalent to @('insEdges' es . 'insNodes' ns)
  //   'empty'@.
  mkGraph   :: [LNode a] [LEdge b] -> gr a b

  // | A list of all 'LNode's in the 'Graph'.
  labNodes  :: (gr a b) -> [LNode a]

  // | Decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
  // and the remaining 'Graph'.
  matchAny  :: (gr a b) -> GDecomp gr a b

  // | The number of 'Node's in a 'Graph'.
  noNodes   :: (gr a b) -> Int

  // | The minimum and maximum 'Node' in a 'Graph'.
  nodeRange :: (gr a b) -> (Node,Node)

  // | A list of all 'LEdge's in the 'Graph'.
  labEdges  :: (gr a b) -> [LEdge b]

class DynGraph gr | Graph gr where
  // | Merge the 'Context' into the 'DynGraph'.
  //
  //   Contexts should only refer to either a Node already in a graph
  //   or the node in the Context itself (for loops).
  (<&>) :: (Context a b) (gr a b) -> gr a b


// | The number of nodes in the graph.  An alias for 'noNodes'.
order :: (gr a b) -> Int | Graph gr

// | The number of edges in the graph.
//
//   Note that this counts every edge found, so if you are
//   representing an unordered graph by having each edge mirrored this
//   will be incorrect.
//
//   If you created an unordered graph by either mirroring every edge
//   (including loops!) or using the @undir@ function in
//   "Data.Graph.Inductive.Basic" then you can safely halve the value
//   returned by this.
size :: (gr a b) -> Int | Graph gr

// | Fold a function over the graph.
ufold :: ((Context a b) c -> c) c (gr a b) -> c | Graph gr

// | Map a function over the graph.
gmap :: ((Context a b) -> Context c d) (gr a b) -> gr c d | DynGraph gr

// | Map a function over the 'Node' labels in a graph.
nmap :: (a -> c) (gr a b) -> gr c b | DynGraph gr

// | Map a function over the 'Edge' labels in a graph.
emap :: (b -> c) (gr a b) -> gr a c | DynGraph gr

// | Map functions over both the 'Node' and 'Edge' labels in a graph.
nemap :: (a -> c) (b -> d) (gr a b) -> gr c d | DynGraph gr

// | List all 'Node's in the 'Graph'.
nodes :: (gr a b) -> [Node] | Graph gr

// | List all 'Edge's in the 'Graph'.
edges :: (gr a b) -> [Edge] | Graph gr

// | Drop the label component of an edge.
toEdge :: (LEdge b) -> Edge

// | Add a label to an edge.
toLEdge :: Edge b -> LEdge b

// | The label in an edge.
edgeLabel :: (LEdge b) -> b

// | List N available 'Node's, i.e. 'Node's that are not used in the 'Graph'.
newNodes :: Int (gr a b) -> [Node] | Graph gr

// | 'True' if the 'Node' is present in the 'Graph'.
gelem :: Node (gr a b) -> Bool | Graph gr

// | Insert a 'LNode' into the 'Graph'.
insNode :: (LNode a) (gr a b) -> gr a b | DynGraph gr

// | Insert a 'LEdge' into the 'Graph'.
insEdge :: (LEdge b) (gr a b) -> gr a b | DynGraph gr

// | Remove a 'Node' from the 'Graph'.
delNode :: Node (gr a b) -> gr a b | Graph gr

// | Remove an 'Edge' from the 'Graph'.
//
//   NOTE: in the case of multiple edges, this will delete /all/ such
//   edges from the graph as there is no way to distinguish between
//   them.  If you need to delete only a single such edge, please use
//   'delLEdge'.
delEdge :: Edge (gr a b) -> gr a b | DynGraph gr

// | Remove an 'LEdge' from the 'Graph'.
//
//   NOTE: in the case of multiple edges with the same label, this
//   will only delete the /first/ such edge.  To delete all such
//   edges, please use 'delAllLedge'.
delLEdge :: (LEdge b) (gr a b) -> gr a b | DynGraph gr & Eq b

// | Remove all edges equal to the one specified.
delAllLEdge :: (LEdge b) (gr a b) -> gr a b | DynGraph gr & Eq b

delLEdgeBy :: ((b,Node) (Adj b) -> Adj b) (LEdge b) (gr a b) -> gr a b | DynGraph gr

// | Insert multiple 'LNode's into the 'Graph'.
insNodes   :: [LNode a] (gr a b) -> gr a b | DynGraph gr

// | Insert multiple 'LEdge's into the 'Graph'.
insEdges :: [LEdge b] (gr a b) -> gr a b | DynGraph gr

// | Remove multiple 'Node's from the 'Graph'.
delNodes :: [Node] (gr a b) -> gr a b | Graph gr

// | Remove multiple 'Edge's from the 'Graph'.
delEdges :: [Edge] (gr a b) -> gr a b | DynGraph gr

// | Build a 'Graph' from a list of 'Context's.
//
//   The list should be in the order such that earlier 'Context's
//   depend upon later ones (i.e. as produced by @'ufold' (:) []@).
buildGr :: [Context a b] -> gr a b | DynGraph gr

// | Build a quasi-unlabeled 'Graph'.
mkUGraph :: [Node] [Edge] -> gr () () | Graph gr

// | Build a graph out of the contexts for which the predicate is
// true.
gfiltermap :: ((Context a b) -> MContext c d) (gr a b) -> gr c d | DynGraph gr

// | Returns the subgraph only containing the labelled nodes which
// satisfy the given predicate.
labnfilter :: ((LNode a) -> Bool) (gr a b) -> gr a b | Graph gr

// | Returns the subgraph only containing the nodes which satisfy the
// given predicate.
nfilter :: (Node -> Bool) (gr a b) -> gr a b | DynGraph gr

// | Returns the subgraph only containing the nodes whose labels
// satisfy the given predicate.
labfilter :: (a -> Bool) (gr a b) -> gr a b | DynGraph gr

// | Returns the subgraph induced by the supplied nodes.
subgraph :: [Node] (gr a b) -> gr a b | DynGraph gr

// | Find the context for the given 'Node'.  Causes an error if the 'Node' is
// not present in the 'Graph'.
context :: (gr a b) Node -> Context a b | Graph gr

// | Find the label for a 'Node'.
lab :: (gr a b) Node -> Maybe a | Graph gr

// | Find the neighbors for a 'Node'.
neighbors :: (gr a b) Node -> [Node] | Graph gr

// | Find the labelled links coming into or going from a 'Context'.
lneighbors :: (gr a b) Node -> Adj b | Graph gr

// | Find all 'Node's that have a link from the given 'Node'.
suc :: (gr a b) Node -> [Node] | Graph gr

// | Find all 'Node's that link to to the given 'Node'.
pre :: (gr a b) Node -> [Node] | Graph gr

// | Find all 'Node's that are linked from the given 'Node' and the label of
// each link.
lsuc :: (gr a b) Node -> [(Node,b)] | Graph gr

// | Find all 'Node's that link to the given 'Node' and the label of each link.
lpre :: (gr a b) Node -> [(Node,b)] | Graph gr

// | Find all outward-bound 'LEdge's for the given 'Node'.
out :: (gr a b) Node -> [LEdge b] | Graph gr

// | Find all inward-bound 'LEdge's for the given 'Node'.
inn :: (gr a b) Node -> [LEdge b] | Graph gr

// | The outward-bound degree of the 'Node'.
outdeg :: (gr a b) Node -> Int | Graph gr

// | The inward-bound degree of the 'Node'.
indeg :: (gr a b) Node -> Int | Graph gr

// | The degree of the 'Node'.
deg :: (gr a b) Node -> Int | Graph gr

// | The 'Node' in a 'Context'.
node` :: (Context a b) -> Node

// | The label in a 'Context'.
lab` :: (Context a b) -> a

// | The 'LNode' from a 'Context'.
labNode` :: (Context a b) -> LNode a

// | All 'Node's linked to or from in a 'Context'.
neighbors` :: (Context a b) -> [Node]

// | All labelled links coming into or going from a 'Context'.
lneighbors` :: (Context a b) -> Adj b

// | All 'Node's linked to in a 'Context'.
suc` :: (Context a b) -> [Node]

// | All 'Node's linked from in a 'Context'.
pre` :: (Context a b) -> [Node]

// | All 'Node's linked from in a 'Context', and the label of the links.
lsuc` :: (Context a b) -> [(Node,b)]

// | All 'Node's linked from in a 'Context', and the label of the links.
lpre` :: (Context a b) -> [(Node,b)]

// | All outward-directed 'LEdge's in a 'Context'.
out` :: (Context a b) -> [LEdge b]

// | All inward-directed 'LEdge's in a 'Context'.
inn` :: (Context a b) -> [LEdge b]

// | The outward degree of a 'Context'.
outdeg` :: (Context a b) -> Int

// | The inward degree of a 'Context'.
indeg` :: (Context a b) -> Int

// | The degree of a 'Context'.
deg` :: (Context a b) -> Int

// | Checks if there is a directed edge between two nodes.
hasEdge :: (gr a b) Edge -> Bool | Graph gr

// | Checks if there is an undirected edge between two nodes.
hasNeighbor :: (gr a b) Node Node -> Bool | Graph gr

// | Checks if there is a labelled edge between two nodes.
hasLEdge :: (gr a b) (LEdge b) -> Bool | Graph gr & Eq b

// | Checks if there is an undirected labelled edge between two nodes.
hasNeighborAdj :: (gr a b) Node (b,Node) -> Bool | Graph gr & Eq b

//--------------------------------------------------------------------
// GRAPH EQUALITY
//--------------------------------------------------------------------

//slabNodes :: (gr a b) -> [LNode a] | Graph gr

//glabEdges :: (gr a b) -> [GroupEdges b] | Graph gr

//equal :: (gr a b) (gr a b) -> Bool | Graph gr & == a & == b

// This assumes that nodes aren't repeated (which shouldn't happen for
// sane graph instances).  If node IDs are repeated, then the usage of
// slabNodes cannot guarantee stable ordering.

// Newtype wrapper just to test for equality of multiple edges.  This
// is needed because without an Ord constraint on `b' it is not
// possible to guarantee a stable ordering on edge labels.
:: GroupEdges b = GEs (LEdge [b])

instance == (GroupEdges b) | Eq b

eqLists :: [a] [a] -> Bool | Eq a

//--------------------------------------------------------------------
// UTILITIES
//--------------------------------------------------------------------

// auxiliary functions used in the implementation of the
// derived class members
//
(.:) :: (c -> d) (a -> b -> c) a b -> d

flip2 :: (a,b) -> (b,a)

// projecting on context elements
//
context1l :: (gr a b) Node -> Adj b | Graph gr

context4l :: (gr a b) Node -> Adj b | Graph gr

mcontext :: (gr a b) Node -> MContext a b | Graph gr

context1l` :: (Context a b) -> Adj b

context4l` :: (Context a b) -> Adj b

//--------------------------------------------------------------------
// PRETTY PRINTING
//--------------------------------------------------------------------

// | Pretty-print the graph.  Note that this loses a lot of
//   information, such as edge inverses, etc.
//prettify :: (gr a b) -> String | DynGraph gr & Show a & Show b
prettify :: (gr a b) -> String | DynGraph gr & toString a & toString b

//--------------------------------------------------------------------
// Ordered Graph
//--------------------------------------------------------------------

// | OrdGr comes equipped with an Ord instance, so that graphs can be
//   used as e.g. Map keys.
:: OrdGr gr a b = OrdGr (gr a b)

unOrdGr :: (OrdGr gr a b) -> gr a b

// TODO
//instance (Graph gr, Ord a, Ord b) => == (OrdGr gr a b) where
  //g1 == g2 = compare g1 g2 == EQ

//instance (Graph gr, Ord a, Ord b) => Ord (OrdGr gr a b) where
  //compare (OrdGr g1) (OrdGr g2) =
    //(compare `on` sort . labNodes) g1 g2
    //`mappend` (compare `on` sort . labEdges) g1 g2

defMatchAny  :: (gr a b) -> GDecomp gr a b | Graph gr

// | The number of 'Node's in a 'Graph'.
defNoNodes   :: (gr a b) -> Int | Graph gr

// | The minimum and maximum 'Node' in a 'Graph'.
defNodeRange :: (gr a b) -> (Node,Node) | Graph gr

// | A list of all 'LEdge's in the 'Graph'.
defLabEdges  :: (gr a b) -> [LEdge b] | Graph gr
