implementation module Sapl.Transform.Let

import StdList, StdFunc, StdTuple, StdBool
import Sapl.SaplStruct

from Data.Set import qualified newSet, fromList, toList, member, difference, insert, filter, delete, null
from Data.Set import :: Set
from Data.Map import qualified fromList, get

instance == SaplVar
where
	(==) a b = eqVarByNameLevel a b

instance < SaplVar
where
	(<) a b = ltVarByNameLevel a b

// Generate the graph: edges and the start nodes (independent nodes)
genGraph :: !(Set SaplVar) ![SaplLetDef] -> (!Set (SaplVar,SaplVar), !Set SaplVar)
genGraph binds defs = foldl (\s (SaplLetDef (TypedVar bv _) body) -> gen binds bv s body) ('Data.Set'.newSet,binds) defs
where
	gen vs bv s (SSelect expr _ idx) = gen vs bv s expr
	gen vs bv s (SUpdate expr _ updates) = foldl (gen vs bv) (gen vs bv s expr) (map snd updates)
	gen vs bv s (SApplication (SVar f) as) = foldl (gen vs bv) s [SVar f:as]
	gen vs bv s (SApplication expr as) = foldl (gen vs bv) (gen vs bv s expr) as
	gen vs bv (es,is) (SVar v) 
		| 'Data.Set'.member v vs && v <> bv  = ('Data.Set'.insert (bv, v) es, 'Data.Set'.delete v is)
	gen _ _ s _ = s

// Kahn, Arthur B. (1962), "Topological sorting of large networks"
sortBindings :: ![SaplLetDef] -> Maybe [SaplLetDef]
sortBindings [d] = Just [d]
sortBindings defs 
	# (redges,rordbinds) = gen edges ('Data.Set'.toList startnodes)
	| 'Data.Set'.null redges
		= Just (map (\k -> fromJust ('Data.Map'.get k defmap)) (reverse rordbinds))
		= Nothing // cycle is detected
where
	(edges, startnodes) = genGraph binds defs 
	binds  = 'Data.Set'.fromList (map (removeTypeInfo o toNormalVar o unpackBindVar) defs)
	defmap = 'Data.Map'.fromList (map (\d=:(SaplLetDef bv body) -> (removeTypeInfo bv,d)) defs)	

	// Returns the renaming edges (if any) and the ordered list of bind vars (reversed order)
	gen edges [] = (edges, [])
	gen edges [n:ns] = let (redges,rout) = gen nedges (nns++ns) in (redges,[n:rout])
	where
		(nedges, nns) = foldl peredge (edges,[]) outedges
		outedges = filter (\e = fst e == n) ('Data.Set'.toList edges)
		
		peredge (edges,out) e=:(n,m)
			# edges = 'Data.Set'.delete e edges
			| 'Data.Set'.null ('Data.Set'.filter (\e = snd e == m) edges)
				= (edges, [m:out])
				= (edges, out)
				
				