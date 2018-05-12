implementation module Sapl.Transform.VarReferences

import StdList, StdFunc, StdTuple, StdBool
import Sapl.SaplStruct
from Data.Map import :: Map (..), newMap, put, get

fixReferences :: ![FuncType] -> [FuncType]
fixReferences fs = map fixOneFun fs   
			
createInitialMap :: [SaplTypedVar] -> Map String SaplVar 
createInitialMap vs = addVars newMap (map removeTypeInfo vs)  				
		
addVars :: (Map String SaplVar) [SaplVar] -> Map String SaplVar		
addVars vars vs = foldl addOne vars vs  				
where
  addOne map var = put (unpackVar var) var map
  					
fixOneFun (FTFunc name body args) = FTFunc name (fixRefs (createInitialMap [name:args]) body) args
fixOneFun (FTMacro name body args) = FTMacro name (fixRefs (createInitialMap [name:args]) body) args
fixOneFun (FTCAF name body) = FTCAF name (fixRefs (createInitialMap [name]) body)
fixOneFun f = f

fixRefs vars v1=:(SVar var) = 
	case get (unpackVar var) vars of
		(Just v2) = (SVar v2)
		Nothing = v1	  

fixRefs vars (SApplication f args) = SApplication (fixRefs vars f) (map (fixRefs vars) args)

fixRefs vars (SCase expr patterns) = SCase (fixRefs vars expr) (map fixPattern patterns)
where
  fixPattern (p=:(PCons cons vs), body) = (p, fixRefs newVars body)
  where
    newVars = addVars vars vs  				
    
  fixPattern (p, body) = (p, fixRefs vars body)

fixRefs vars (SLet body defs) = SLet (fixRefs vars body) (map fixLetDef defs)
where
  newVars = addVars vars (map (removeTypeInfo o unpackBindVar) defs)			
  fixLetDef (SaplLetDef var expr) = SaplLetDef var (fixRefs newVars expr)			
  
fixRefs vars (SSelect expr type field) = SSelect (fixRefs vars expr) type field 
fixRefs vars (SUpdate expr type updates) = SUpdate (fixRefs vars expr) type (map fixUpdate updates)
where			
  fixUpdate (field, expr) = (field, fixRefs vars expr)
  
fixRefs vars t = t