implementation module Data.Maybe.Ord

import StdEnv, Data.Maybe

instance < (Maybe a) | < a where 
	< Nothing   Nothing   = False 
	< Nothing   _         = True 
	< (Just a1) (Just a2) = a1 < a2
	< _         _         = False
