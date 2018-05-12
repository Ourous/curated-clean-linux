implementation module Sapl.Transform.AddSelectors

import Sapl.SaplStruct
import StdBool, StdList

// do not do that for embedded "case"s, fall back may be screwed up

instance addSelectors SaplTerm where
  addSelectors (SApplication v ts) = SApplication v (map addSelectors ts)

  addSelectors st=:(SCase t ps=:[(PCons cons vs, SVar x)])
    | foldr (\v acc -> acc || eqVarByName v x) False vs 
    	# (idx, _) = foldl (\(idx, cnt) v -> if (eqVarByName x v) (cnt, cnt) (idx, cnt + 1)) (0, 0) vs  
    	= SSelect t (Type cons)  idx
    | otherwise                                         
    	= SCase t ps
    
  addSelectors (SLet t lds) = SLet (addSelectors t) (map addSelectors lds)
  addSelectors st = st

instance addSelectors SaplLetDef where
  addSelectors (SaplLetDef v t) = SaplLetDef v (addSelectors t)
