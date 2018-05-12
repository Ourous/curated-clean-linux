implementation module Sapl.Optimization.StrictnessPropagation

import StdEnum, StdArray, StdTuple, StdBool
from StdList import map, foldl, !!, zip2, unzip, filter, reverse, instance length []
import Sapl.SaplParser, Sapl.Target.Flavour
from Data.Set import :: Set, newSet, fromList, member, insert, delete, union, unions, intersection, intersections, difference
from Data.Map import get, put
import Data.Maybe

isStrictArgFlavour :: !Flavour !ParserState !String !Int !Int -> Bool
isStrictArgFlavour {builtInFunctions, inlineFunctions} {ps_constructors, ps_functions} n nr_args i
	= checkCons
where
	checkCons = case get n ps_constructors of
					(Just cons) = if (nr_args < cons.nr_args || i >= cons.nr_args) False (isStrictVar (cons.args !! i))
								= checkFun 
								
	checkFun = case get n ps_functions of
					(Just args) = let largs = length args in if (nr_args < largs || i >= largs) False (isStrictVar (args !! i))
								= checkInline
								
	checkInline =  case get n inlineFunctions of
					(Just def) 	= if (nr_args < def.InlineFunDef.arity || i >= def.InlineFunDef.arity) False (def.strictness.[i] == '1')
							   	= False

doStrictnessPropagation :: !ParserState !IsStrictArgFun ![FuncType] -> (![FuncType], !ParserState)
doStrictnessPropagation ps isStrictArg funs 
	# (nfs, nps) = foldl (\(nfs,ps) f -> let (nf, nps) = propFunc ps isStrictArg f in ([nf:nfs], nps)) ([], ps) funs
	= (reverse nfs, nps)

// TODO: if strictness is given to the arguments the whole propogation stuff
//		 should be recomputed again and again until a fixpoint...
// 		 Expect: if the functions are in the good order which is the case if the code is linked
propFunc :: !ParserState !IsStrictArgFun !FuncType -> (!FuncType, !ParserState)
propFunc ps=:{ps_functions} isStrictArg (FTFunc name body args) 
	= (FTFunc name nbody nargs, {ps & ps_functions = put (unpackVar name) nargs ps_functions})
where
	(ds, nbody) = (propBody ps isStrictArg newSet body)
	nargs = map addStrictness args
	
	addStrictness var=:(TypedVar (GlobalVar _) _) = var
	addStrictness var=:(TypedVar (StrictVar _ _) _) = var
	addStrictness var=:(TypedVar (NormalVar vn _) _) = if (member vn ds) (toStrictVar var) var	

propFunc ps _ f = (f, ps)

propBody :: !ParserState !IsStrictArgFun !(Set String) !SaplTerm -> (!Set String, !SaplTerm)
propBody ps isStrictArg sd body = walk sd body
where
	walk sd t=:(SVar var) = (insert (unpackVar var) sd, t)

    // We can skip the new expr, cannot contain let definitions...
	walk sd t=:(SUpdate expr _ updates) 
		# (sd, _) = walk sd expr
		// TODO: updates
		= (sd, t)

    // We can skip the new expr, cannot contain let definitions...
	walk sd t=:(SSelect expr _ idx) 
		# (sd, _) = walk sd expr
		= (sd, t)

    // We can skip the new args, cannot contain let definitions...
	walk sd t=:(SApplication (SVar var) args)
		# nsds = map fst (map (walk newSet) strictArgs)
		= (unions [sd:nsds], t)
	where
		varName = unpackVar var
		nr_args = length args
		checkArg (arg, i) = isStrictArg ps varName nr_args i
		strictArgs = map fst (filter checkArg (zip2 args [0..]))

    // We can skip the new expr, cannot contain let definitions...
    // Args cannot be checked, we do not know the function definition...
	walk sd t=:(SApplication expr args)
		// expr is always strict
		# (sd, _) = walk sd expr
		= (sd, t)

	walk sd (SCase p cases) 
		# (sdp, np) = walk sd p
		# (sdcs, ncases) = unzip (map walkcase cases)
		= (union sdp (intersections sdcs), SCase np ncases)
	where
		walkcase (p, c) 
			# (sd, nc) = walk newSet c 
			= (difference sd (patternvars p), (p, nc))
		
		patternvars (PCons _ vars) = fromList (map unpackVar vars)
		patternvars _ = newSet 
	
	// It is supposed that bindings are topologically sorted
	walk sd (SLet body bnds)
		# (sdb, nbody) = walk newSet body
		# (sdl, nbnds) =  wbnds sdb (reverse bnds) [] // reverse is important
		= (union sd sdl, SLet nbody nbnds) 
	where
		wbnds sd [] nbnds = (sd, nbnds)
		wbnds sd [bnd:bnds] nbnds
			# nbnd = if (member vn sd) (toStrictBind bnd) bnd
			# nsd = walkbnd sd nbnd
			= wbnds nsd bnds [nbnd:nbnds]
		where
			vn = unpackVar (unpackBindVar bnd)

			// Delete itself, it doesn't need any more
			walkbnd sd (SaplLetDef (TypedVar (StrictVar vn _) _) body) = delete vn (fst (walk sd body)) // skip new body, it cannot be a let definition
			walkbnd sd (SaplLetDef (TypedVar (NormalVar vn _) _) body) = delete vn sd
			walkbnd sd (SaplLetDef (TypedVar (GlobalVar vn) _) body) = delete vn sd
		
	walk sd t = (sd, t)
			
	
