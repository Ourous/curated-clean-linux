// (c) 1999-2005 by Martin Erwig [see file COPYRIGHT]
// | Static and Dynamic Inductive Graphs
implementation module Data.Graph.Inductive.Graph

import Control.Arrow
//import           Data.Function (on)
import qualified Data.IntSet as IntSet
//import           Data.List     (delete, foldl, groupBy, sort, sortBy, (\\))
import qualified Data.List as DL
//import           Data.Maybe    (fromMaybe, isJust)
//import Data.Monoid (mappend)
import StdBool, StdTuple, StdFunc, StdMisc, StdEnum, StdString, StdOverloaded, StdClass
import Data.List
import Data.Maybe
import Data.Functor
import Data.Generics.GenLexOrd

unLPath :: (LPath a) -> [LNode a]
unLPath (LP xs) = xs

//TODO
//instance toString (LPath a) | toString a where
  //toString (LP xs) = foldr (\x xs -> toString x +++ " " +++ xs) "" xs

instance == (LPath a) | == a where
  == (LP [])        (LP [])        = True
  == (LP [(_,x):_]) (LP [(_,y):_]) = x==y
  == (LP _)         (LP _)         = False

instance < (LPath a) | gLexOrd{|*|} a where
  < (LP [(_,x):_]) (LP [(_,y):_]) = case x =?= y of
                                      LT -> True
                                      _  -> False
  < _ _ = False

// | Decompose a graph into the 'Context' for an arbitrarily-chosen 'Node'
// and the remaining 'Graph'.
defMatchAny  :: (gr a b) -> GDecomp gr a b | Graph gr
defMatchAny g = case labNodes g of
                  []        -> abort "Match Exception, Empty Graph"
                  [(v,_):_] -> (c,g`)
                    where
                      (Just c,g`) = match v g

// | The number of 'Node's in a 'Graph'.
defNoNodes   :: (gr a b) -> Int | Graph gr
defNoNodes g = length (labNodes g)

// | The minimum and maximum 'Node' in a 'Graph'.
defNodeRange :: (gr a b) -> (Node,Node) | Graph gr
defNodeRange g
  | isEmptyGraph g = abort "nodeRange of empty graph"
  | otherwise = (minimum vs, maximum vs)
  where
    vs = nodes g

// | A list of all 'LEdge's in the 'Graph'.
defLabEdges  :: (gr a b) -> [LEdge b] | Graph gr
defLabEdges g = ufold (\(_,v,_,s) xs -> map (\(l,w)->(v,w,l)) s ++ xs) [] g

// | The number of nodes in the graph.  An alias for 'noNodes'.
order :: (gr a b) -> Int | Graph gr
order g = noNodes g

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
size g = length (labEdges g)

// | Fold a function over the graph.
ufold :: ((Context a b) c -> c) c (gr a b) -> c | Graph gr
ufold f u g
  | isEmptyGraph g = u
  | otherwise = f c (ufold f u g`)
  where
    (c,g`) = matchAny g

// | Map a function over the graph.
gmap :: ((Context a b) -> Context c d) (gr a b) -> gr c d | DynGraph gr
gmap f g = ufold (\c x -> f c <&> x) emptyGraph g

// | Map a function over the 'Node' labels in a graph.
nmap :: (a -> c) (gr a b) -> gr c b | DynGraph gr
nmap f g = gmap (\(p,v,l,s)->(p,v,f l,s)) g

// | Map a function over the 'Edge' labels in a graph.
emap :: (b -> c) (gr a b) -> gr a c | DynGraph gr
emap f g = gmap (\(p,v,l,s)->(map1 f p,v,l,map1 f s)) g
  where
    map1 g = map (first g)

// | Map functions over both the 'Node' and 'Edge' labels in a graph.
nemap :: (a -> c) (b -> d) (gr a b) -> gr c d | DynGraph gr
nemap fn fe g = gmap (\(p,v,l,s) -> (fe` p,v,fn l,fe` s)) g
  where
    fe` = map (first fe)

// | List all 'Node's in the 'Graph'.
nodes :: (gr a b) -> [Node] | Graph gr
nodes g = map fst (labNodes g)

// | List all 'Edge's in the 'Graph'.
edges :: (gr a b) -> [Edge] | Graph gr
edges g = map toEdge (labEdges g)

// | Drop the label component of an edge.
toEdge :: (LEdge b) -> Edge
toEdge (v,w,_) = (v,w)

// | Add a label to an edge.
toLEdge :: Edge b -> LEdge b
toLEdge (v,w) l = (v,w,l)

// | The label in an edge.
edgeLabel :: (LEdge b) -> b
edgeLabel (_,_,l) = l

// | List N available 'Node's, i.e. 'Node's that are not used in the 'Graph'.
newNodes :: Int (gr a b) -> [Node] | Graph gr
newNodes i g
  | isEmptyGraph g = [0..i-1]
  | otherwise = [n+1..n+i]
  where
    (_,n) = nodeRange g

// | 'True' if the 'Node' is present in the 'Graph'.
gelem :: Node (gr a b) -> Bool | Graph gr
gelem v g = isJust (fst (match v g))

// | Insert a 'LNode' into the 'Graph'.
insNode :: (LNode a) (gr a b) -> gr a b | DynGraph gr
insNode (v,l) gr = ([],v,l,[]) <&> gr

// | Insert a 'LEdge' into the 'Graph'.
insEdge :: (LEdge b) (gr a b) -> gr a b | DynGraph gr
insEdge (v,w,l) g = (pr,v,la,[(l,w):su]) <&> g`
  where
    (mcxt,g`) = match v g
    (pr,_,la,su) = fromMaybe
                     (abort ("insEdge: cannot add edge from non-existent vertex" +++ toString v))
                     mcxt

// | Remove a 'Node' from the 'Graph'.
delNode :: Node (gr a b) -> gr a b | Graph gr
delNode v gr = delNodes [v] gr

// | Remove an 'Edge' from the 'Graph'.
//
//   NOTE: in the case of multiple edges, this will delete /all/ such
//   edges from the graph as there is no way to distinguish between
//   them.  If you need to delete only a single such edge, please use
//   'delLEdge'.
delEdge :: Edge (gr a b) -> gr a b | DynGraph gr
delEdge (v,w) g = case match v g of
                    (Nothing,_)          -> g
                    (Just (p,v`,l,s),g`) -> (p,v`,l,filter ((\x -> x <> w) o snd) s) <&> g`

// | Remove an 'LEdge' from the 'Graph'.
//
//   NOTE: in the case of multiple edges with the same label, this
//   will only delete the /first/ such edge.  To delete all such
//   edges, please use 'delAllLedge'.
delLEdge :: (LEdge b) (gr a b) -> gr a b | DynGraph gr & Eq b
delLEdge e g = delLEdgeBy 'DL'.delete e g

// | Remove all edges equal to the one specified.
delAllLEdge :: (LEdge b) (gr a b) -> gr a b | DynGraph gr & Eq b
delAllLEdge e g = delLEdgeBy (filter o (<>)) e g

delLEdgeBy :: ((b,Node) (Adj b) -> Adj b) (LEdge b) (gr a b) -> gr a b | DynGraph gr
delLEdgeBy f (v,w,b) g = case match v g of
                           (Nothing,_)          -> g
                           (Just (p,v`,l,s),g`) -> (p,v`,l,f (b,w) s) <&> g`

// | Insert multiple 'LNode's into the 'Graph'.
insNodes   :: [LNode a] (gr a b) -> gr a b | DynGraph gr
insNodes vs g = foldl (flip insNode) g vs

// | Insert multiple 'LEdge's into the 'Graph'.
insEdges :: [LEdge b] (gr a b) -> gr a b | DynGraph gr
insEdges es g = foldl (flip insEdge) g es

// | Remove multiple 'Node's from the 'Graph'.
delNodes :: [Node] (gr a b) -> gr a b | Graph gr
delNodes vs g = foldl (snd .: flip match) g vs

// | Remove multiple 'Edge's from the 'Graph'.
delEdges :: [Edge] (gr a b) -> gr a b | DynGraph gr
delEdges es g = foldl (flip delEdge) g es

// | Build a 'Graph' from a list of 'Context's.
//
//   The list should be in the order such that earlier 'Context's
//   depend upon later ones (i.e. as produced by @'ufold' (:) []@).
buildGr :: [Context a b] -> gr a b | DynGraph gr
buildGr cs = foldr (<&>) emptyGraph cs

// | Build a quasi-unlabeled 'Graph'.
mkUGraph :: [Node] [Edge] -> gr () () | Graph gr
mkUGraph vs es = mkGraph (labUNodes vs) (labUEdges es)
   where
     labUEdges = map (\x -> toLEdge x ())
     labUNodes = map ((\x y -> (y, x)) ()) // TODO Double check

// | Build a graph out of the contexts for which the predicate is
// true.
gfiltermap :: ((Context a b) -> MContext c d) (gr a b) -> gr c d | DynGraph gr
gfiltermap f gr = ufold (maybe id (<&>) o f) emptyGraph gr

// | Returns the subgraph only containing the labelled nodes which
// satisfy the given predicate.
labnfilter :: ((LNode a) -> Bool) (gr a b) -> gr a b | Graph gr
labnfilter p gr = delNodes (map fst (filter (not o p) (labNodes gr))) gr

// | Returns the subgraph only containing the nodes which satisfy the
// given predicate.
nfilter :: (Node -> Bool) (gr a b) -> gr a b | DynGraph gr
nfilter f gr = labnfilter (f o fst) gr

// | Returns the subgraph only containing the nodes whose labels
// satisfy the given predicate.
labfilter :: (a -> Bool) (gr a b) -> gr a b | DynGraph gr
labfilter f gr = labnfilter (f o snd) gr

// | Returns the subgraph induced by the supplied nodes.
subgraph :: [Node] (gr a b) -> gr a b | DynGraph gr
subgraph vs gr = let vs` = 'IntSet'.fromList vs
              in nfilter (\x -> 'IntSet'.member x vs`) gr

// | Find the context for the given 'Node'.  Causes an error if the 'Node' is
// not present in the 'Graph'.
context :: (gr a b) Node -> Context a b | Graph gr
context g v = fromMaybe (abort ("Match Exception, Node: " +++ toString v))
                        (fst (match v g))

// | Find the label for a 'Node'.
lab :: (gr a b) Node -> Maybe a | Graph gr
lab g v = lab` <$> (fst (match v g))

// | Find the neighbors for a 'Node'.
neighbors :: (gr a b) Node -> [Node] | Graph gr
neighbors gr n = (map snd .: lneighbors) gr n

// | Find the labelled links coming into or going from a 'Context'.
lneighbors :: (gr a b) Node -> Adj b | Graph gr
lneighbors gr n = (maybe [] lneighbors` .: mcontext) gr n

// | Find all 'Node's that have a link from the given 'Node'.
suc :: (gr a b) Node -> [Node] | Graph gr
suc gr n = (map snd .: context4l) gr n

// | Find all 'Node's that link to to the given 'Node'.
pre :: (gr a b) Node -> [Node] | Graph gr
pre gr n = (map snd .: context1l) gr n

// | Find all 'Node's that are linked from the given 'Node' and the label of
// each link.
lsuc :: (gr a b) Node -> [(Node,b)] | Graph gr
lsuc gr n = (map flip2 .: context4l) gr n

// | Find all 'Node's that link to the given 'Node' and the label of each link.
lpre :: (gr a b) Node -> [(Node,b)] | Graph gr
lpre gr n = (map flip2 .: context1l) gr n

// | Find all outward-bound 'LEdge's for the given 'Node'.
out :: (gr a b) Node -> [LEdge b] | Graph gr
out g v = map (\(l,w)->(v,w,l)) (context4l g v)

// | Find all inward-bound 'LEdge's for the given 'Node'.
inn :: (gr a b) Node -> [LEdge b] | Graph gr
inn g v = map (\(l,w)->(w,v,l)) (context1l g v)

// | The outward-bound degree of the 'Node'.
outdeg :: (gr a b) Node -> Int | Graph gr
outdeg gr n = (length .: context4l) gr n

// | The inward-bound degree of the 'Node'.
indeg :: (gr a b) Node -> Int | Graph gr
indeg gr n = (length .: context1l) gr n

// | The degree of the 'Node'.
deg :: (gr a b) Node -> Int | Graph gr
deg gr n = (deg` .: context) gr n

// | The 'Node' in a 'Context'.
node` :: (Context a b) -> Node
node` (_,v,_,_) = v

// | The label in a 'Context'.
lab` :: (Context a b) -> a
lab` (_,_,l,_) = l

// | The 'LNode' from a 'Context'.
labNode` :: (Context a b) -> LNode a
labNode` (_,v,l,_) = (v,l)

// | All 'Node's linked to or from in a 'Context'.
neighbors` :: (Context a b) -> [Node]
neighbors` (p,_,_,s) = map snd p++map snd s

// | All labelled links coming into or going from a 'Context'.
lneighbors` :: (Context a b) -> Adj b
lneighbors` (p,_,_,s) = p ++ s

// | All 'Node's linked to in a 'Context'.
suc` :: (Context a b) -> [Node]
suc` c = map snd (context4l` c)

// | All 'Node's linked from in a 'Context'.
pre` :: (Context a b) -> [Node]
pre` c = map snd (context1l` c)

// | All 'Node's linked from in a 'Context', and the label of the links.
lsuc` :: (Context a b) -> [(Node,b)]
lsuc` c = map flip2 (context4l` c)

// | All 'Node's linked from in a 'Context', and the label of the links.
lpre` :: (Context a b) -> [(Node,b)]
lpre` c = map flip2 (context1l` c)

// | All outward-directed 'LEdge's in a 'Context'.
out` :: (Context a b) -> [LEdge b]
out` c=:(_,v,_,_) = map (\(l,w)->(v,w,l)) (context4l` c)

// | All inward-directed 'LEdge's in a 'Context'.
inn` :: (Context a b) -> [LEdge b]
inn` c=:(_,v,_,_) = map (\(l,w)->(w,v,l)) (context1l` c)

// | The outward degree of a 'Context'.
outdeg` :: (Context a b) -> Int
outdeg` c = length (context4l` c)

// | The inward degree of a 'Context'.
indeg` :: (Context a b) -> Int
indeg` c = length (context1l` c)

// | The degree of a 'Context'.
deg` :: (Context a b) -> Int
deg` (p,_,_,s) = length p+length s

// | Checks if there is a directed edge between two nodes.
hasEdge :: (gr a b) Edge -> Bool | Graph gr
hasEdge gr (v,w) = 'DL'.elem w (suc gr v)

// | Checks if there is an undirected edge between two nodes.
hasNeighbor :: (gr a b) Node Node -> Bool | Graph gr
hasNeighbor gr v w = 'DL'.elem w (neighbors gr v)

// | Checks if there is a labelled edge between two nodes.
hasLEdge :: (gr a b) (LEdge b) -> Bool | Graph gr & Eq b
hasLEdge gr (v,w,l) = 'DL'.elem (w,l) (lsuc gr v)

// | Checks if there is an undirected labelled edge between two nodes.
hasNeighborAdj :: (gr a b) Node (b,Node) -> Bool | Graph gr & Eq b
hasNeighborAdj gr v a = 'DL'.elem a (lneighbors gr v)

//--------------------------------------------------------------------
// GRAPH EQUALITY
//--------------------------------------------------------------------

// TODO
//slabNodes :: (gr a b) -> [LNode a] | Graph gr
//slabNodes g = sortBy (compare `on` fst) (labNodes g)

//glabEdges :: (gr a b) -> [GroupEdges b] | Graph gr
//glabEdges g = (map (GEs o groupLabels)
            //o groupBy ((==) `on` toEdge)
            //o sortBy (compare `on` toEdge)
            //o labEdges) g
  //where
    //groupLabels les = toLEdge (toEdge (head les)) (map edgeLabel les)

//equal :: (gr a b) (gr a b) -> Bool | Graph gr & == a & == b
//equal g g` = slabNodes g == slabNodes g` && glabEdges g == glabEdges g`
// This assumes that nodes aren't repeated (which shouldn't happen for
// sane graph instances).  If node IDs are repeated, then the usage of
// slabNodes cannot guarantee stable ordering.

// Newtype wrapper just to test for equality of multiple edges.  This
// is needed because without an Ord constraint on `b' it is not
// possible to guarantee a stable ordering on edge labels.
:: GroupEdges b = GEs (LEdge [b])

instance == (GroupEdges b) | Eq b where
  == (GEs (v1,w1,bs1)) (GEs (v2,w2,bs2)) = v1 == v2
                                           && w1 == w2
                                           && eqLists bs1 bs2

eqLists :: [a] [a] -> Bool | Eq a
eqLists xs ys = False // TODO FIXME null (xs \\ ys) && null (ys \\ xs)
// OK to use \\ here as we want each value in xs to cancel a *single*
// value in ys.

//--------------------------------------------------------------------
// UTILITIES
//--------------------------------------------------------------------

// auxiliary functions used in the implementation of the
// derived class members
//
(.:) :: (c -> d) (a -> b -> c) a b -> d
// f .: g = \x y->f (g x y)
// f .: g = (f .) . g
// (.:) f = ((f .) .)
// (.:) = (.) (.) (.)
(.:) f g x y = ((o) o (o)) f g x y

flip2 :: (a,b) -> (b,a)
flip2 (x,y) = (y,x)

// projecting on context elements
//
context1l :: (gr a b) Node -> Adj b | Graph gr
context1l g n = (maybe [] context1l` .: mcontext) g n

context4l :: (gr a b) Node -> Adj b | Graph gr
context4l g n = (maybe [] context4l` .: mcontext) g n

mcontext :: (gr a b) Node -> MContext a b | Graph gr
mcontext g n = (fst .: flip match) g n

context1l` :: (Context a b) -> Adj b
context1l` (p,v,_,s) = p++filter ((\x -> x == v) o snd) s

context4l` :: (Context a b) -> Adj b
context4l` (p,v,_,s) = s++filter ((\x -> x == v) o snd) p

//--------------------------------------------------------------------
// PRETTY PRINTING
//--------------------------------------------------------------------

// | Pretty-print the graph.  Note that this loses a lot of
//   information, such as edge inverses, etc.
prettify :: (gr a b) -> String | DynGraph gr & toString a & toString b
prettify g = foldr (showsContext o context g) "" (nodes g)
  where
  showsContext (_,n,l,s) acc = toString n +++ " -> " +++ foldr (\(l, n) xs -> "(" +++ toString l +++ ", " +++ toString n +++ "); " +++ xs) "" s +++ "\n" +++ acc
//prettify g = "TODO Prettify" [> foldr (showsContext o context g) id (nodes g) ""
  //where
    //showsContext (_,n,l,s) sg = shows n o (\xs -> ":" +++ xs) o shows l
                                //o showString "->" o shows s
                                //o (\xs -> "\n" +++ xs) o sg */

//--------------------------------------------------------------------
// Ordered Graph
//--------------------------------------------------------------------

// | OrdGr comes equipped with an Ord instance, so that graphs can be
//   used as e.g. Map keys.
:: OrdGr gr a b = OrdGr (gr a b)

unOrdGr :: (OrdGr gr a b) -> gr a b
unOrdGr (OrdGr g) = g

// TODO
//instance (Graph gr, Ord a, Ord b) => == (OrdGr gr a b) where
  //g1 == g2 = compare g1 g2 == EQ

//instance (Graph gr, Ord a, Ord b) => Ord (OrdGr gr a b) where
  //compare (OrdGr g1) (OrdGr g2) =
    //(compare `on` sort . labNodes) g1 g2
    //`mappend` (compare `on` sort . labEdges) g1 g2

