implementation module Sapl.Target.JS.Lifting

import StdEnv
import Sapl.SaplStruct
from Data.Map import :: Map
import qualified Data.Map as DM

inline :: !SaplTerm -> Bool
inline (SLet _ _) = False
inline (SUpdate _ _ _) = False
inline (SCase cond [(PLit (LBool True), case1),(PLit (LBool False), case2)]) = inline cond && inline case1 && inline case2
inline (SCase cond [(PLit (LBool False), case1),(PLit (LBool True), case2)]) = inline cond && inline case1 && inline case2
inline (SCase _ _) = False
inline _ = True

prepareFun :: (String Int Int -> Bool) !FuncType (Map String FuncType) -> (FuncType, Map String FuncType)
prepareFun sf (FTFunc name body args) genfuns
	# (body, genfuns) = prepareExpr sf body genfuns 
	= (FTFunc name body args,genfuns)
prepareFun sf (FTCAF name body) genfuns
	# (body, genfuns) = prepareExpr sf body genfuns
	= (FTCAF name body, genfuns)
prepareFun _ ftype genfuns = (ftype, genfuns)

:: LiftingState = {varidx :: Int, genfuns :: Map String FuncType}

genUpdateFun :: SaplTerm -> FuncType
genUpdateFun (SUpdate _ ty updates)
	= FTFunc (TypedVar (NormalVar funName 0) NoType) 
			 (SUpdate (SVar (NormalVar "e" 0)) ty
			 	[(idx, SVar (NormalVar ("a" +++ toString i) 0)) \\ i <- [1..length updates] & idx <- map fst updates])
			 [TypedVar (NormalVar "e" 0) NoType:[TypedVar (NormalVar ("a" +++ toString i) 0) NoType \\ i <- [1..length updates]]]
where
	funName = case ty of
				NoType = "update$" +++ toString (mask updates 0) 
				(Type tn) = "update$" +++ tn +++ "_" +++ toString (mask updates 0) 
				
	mask [] bits = bits	
	mask [(idx,_):us] bits = mask us ((1 << idx) bitor bits)
	
prepareExpr :: (String Int Int -> Bool) !SaplTerm (Map String FuncType) -> (SaplTerm, Map String FuncType)
prepareExpr sf t genfuns
	# (t, st, defs) = walkTerm t False True {varidx = 1, genfuns = genfuns} 
	= case defs of
		[] = (t, st.genfuns)
		defs = (SLet t defs, st.genfuns)
where
	walkTerm :: !SaplTerm !Bool !Bool !LiftingState -> (!SaplTerm, !LiftingState, ![SaplLetDef])
	walkTerm (SCase cond patterns) _ _ st | not (inline cond)
		# (cond, st, cdefs) = walkTerm cond True True st	
		# (letvar, st) = genVar st
		# casevar = SVar (removeTypeInfo letvar)
		# (patterns, st) = walkPatterns patterns st
		# defs = [SaplLetDef letvar cond:cdefs]
		= case defs of
			[] = (SCase casevar patterns, st, [])
			defs = (SLet (SCase casevar patterns) defs, st, [])
	where
	  genVar st=:{varidx} = (TypedVar (StrictVar ("$g"+++toString varidx) 0) (Type "B"), {st & varidx = varidx + 1})

	walkTerm c=:(SCase cond patterns) doNotLift _ st
		# (cond, st, cdefs) = walkTerm cond False True st
		# (patterns, st) = walkPatterns patterns st
		# defs = cdefs
		= case defs of
			[] = (SCase cond patterns, st, []) // TODO: move to pattern?
			defs = (SLet (SCase cond patterns) defs, st, [])

	walkTerm (SLet expr bindings) doNotLift _ st
		# (expr, st, edefs) = walkTerm expr False True st 
		# (bindings, st, bdefs) = walkBindings bindings st 	
		# defs = edefs ++ bdefs
		=  case defs of
			[] = (SLet expr (bindings ++ defs), st, [])  
			defs = (SLet expr (bindings ++ defs), st, [])  

	walkTerm (SSelect expr ty idx) doNotLift strictPosition st
		# (expr, st, defs) = walkTerm expr False strictPosition st 
		=  (SSelect expr ty idx, st, defs)  

	walkTerm (SUpdate expr ty updates) False True st
		# (letvar, st) = genVar st
		# updvar = SVar (removeTypeInfo letvar)	
		# (updates, st, udefs) = walkUpdates updates st
		# (expr, st, edefs) = walkTerm expr False True st 
		= (updvar, st, [SaplLetDef letvar (SUpdate expr ty updates):edefs++udefs])	
	where
		genVar st=:{varidx} = (TypedVar (StrictVar ("$g"+++toString varidx) 0) NoType, {st & varidx = varidx + 1})

	walkTerm (SUpdate expr ty updates) doNotLift False st
		# (expr, st, edefs) = walkTerm expr doNotLift False st 
		# (updates, st, udefs) = walkUpdates updates st
		// Generate new fun and lift it in the same time
		# (genfun, _) = prepareFun sf (genUpdateFun (SUpdate expr ty updates)) 'DM'.newMap 
		# funname = extractName genfun
		=  (SApplication (SVar funname) [expr:map snd updates], 
					{st & genfuns = 'DM'.put (unpackVar funname) genfun st.genfuns}, edefs ++ udefs)  
	where
		extractName (FTFunc (TypedVar name _) _ _) = name

	// TODO: is it a real option?
	walkTerm (SUpdate expr ty updates) True True st
		# (expr, st, edefs) = walkTerm expr False True st 
		# (updates, st, udefs) = walkUpdates updates st
		=  (SUpdate expr ty updates, st, edefs ++ udefs)  

	walkTerm (SApplication v=:(SVar name) args) doNotLift strictPosition st
		# (args, st, defs) = walkArgs [sf (unpackVar name) (length args) i \\ i <- [0..]] args st
		= (SApplication v args, st, defs)

	walkTerm (SApplication name args) doNotLift strictPosition st
		# (args, st, defs) = walkArgs (repeat False) args st
		= (SApplication name args, st, defs)

	walkTerm t _ _ st = (t, st, [])

	walkArgs _ [] st = ([], st, [])
	walkArgs [isStrict:si] [t:ts] st
		# (t, st, def) = walkTerm t False isStrict st 
		# (ts, st, defs) = walkArgs si ts st
		= ([t:ts], st, def ++ defs)
	
	walkPatterns [] st = ([], st)
	walkPatterns [(p, t):ps] st 
		# (t, st, defs) = walkTerm t False True st 
		# t = case defs of
			[]  = t
			defs = SLet t defs 	 
		# (ps, st) = walkPatterns ps st
		= ([(p,t):ps], st)	
	
	walkBindings [] st = ([], st, [])
	walkBindings [SaplLetDef var expr:bs] st 
		# (expr, st, def) = walkTerm expr True (isStrictVar var) st
		# (bs, st, defs) = walkBindings bs st
		= ([SaplLetDef var expr:bs], st, def ++ defs)

	walkUpdates [] st = ([], st, [])
	walkUpdates [(idx,expr):us] st 
		# (expr, st, def) = walkTerm expr True False st
		# (us, st, defs) = walkUpdates us st
		= ([(idx, expr):us], st, def ++ defs)


		
	



