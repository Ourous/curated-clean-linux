implementation module GenFMap 

import StdGeneric, StdEnv, StdMaybe, _Array, GenMonad

derive bimap (,), [] 

:: FMap v 
	= FMEmpty
	| FMValue v
	| FMEither 	.(FMap v) .(FMap v)
	| FMChar .[.(Char, .FMap v)]
	| FMInt  .[.(Int, .FMap v)]
	| FMReal .[.(Real, .FMap v)]

emptyFMap :: .FMap .v
emptyFMap = FMEmpty

lookupAssocList :: k v [(k,v)] -> v | == k
lookupAssocList key default_val [] = default_val
lookupAssocList key default_val [(k,v):xs]
	| key == k
		= v
		= lookupAssocList key default_val xs 
		
updateAssocList :: k v v [(k,v)] -> (v, [(k,v)]) | == k
updateAssocList key value default_val [] = (default_val, [(key, value)])
updateAssocList key value default_val [(k,v):xs]
	| k == key
		= (v, [(k, value):xs])
		#! (old_val, xs) = updateAssocList key value default_val xs
		= (old_val, [(k, v) : xs]) 
	
derive bimap FMap, Maybe
bimap{|{}|} bma = {map_to = mapArray bma.map_to, map_from = mapArray bma.map_from}

generic gLookupFMap key :: key (FMap value) -> FMap value
gLookupFMap{|Char|} key (FMChar xs) = lookupAssocList key FMEmpty xs
gLookupFMap{|Char|} key FMEmpty 	= FMEmpty

gLookupFMap{|Int|} key (FMInt xs) = lookupAssocList key FMEmpty xs
gLookupFMap{|Int|} key FMEmpty 	= FMEmpty

gLookupFMap{|Real|} key (FMReal xs) = lookupAssocList key FMEmpty xs
gLookupFMap{|Real|} key FMEmpty 	= FMEmpty

gLookupFMap{|Bool|} False (FMEither ls rs) 	= ls
gLookupFMap{|Bool|} True  (FMEither ls rs) 	= rs
gLookupFMap{|Bool|} key FMEmpty 			= FMEmpty

//gLookupFMap{|UNIT|} key (FMValue v)	= (FMValue v)
//gLookupFMap{|UNIT|} key FMEmpty		= FMEmpty
gLookupFMap{|UNIT|} key fm		= fm

gLookupFMap{|PAIR|} fx fy (PAIR kx ky) fm = fy ky (fx kx fm)

gLookupFMap{|EITHER|} fl fr (LEFT key) (FMEither ls rs) = fl key ls
gLookupFMap{|EITHER|} fl fr (RIGHT key) (FMEither ls rs) = fr key rs
gLookupFMap{|EITHER|} fl fr key FMEmpty 	= FMEmpty 

gLookupFMap{|CONS|} f (CONS key) fm = f key fm
gLookupFMap{|FIELD|} f (FIELD key) fm = f key fm
gLookupFMap{|OBJECT|} f (OBJECT key) fm = f key fm

derive gLookupFMap []

gLookupFMap{|String|} arr fm = gLookupFMap{|*|} [x\\x<-:arr] fm
gLookupFMap{|{}|} f arr fm = gLookupFMap{|*->*|} f [x\\x<-:arr] fm
gLookupFMap{|{!}|} f arr fm = gLookupFMap{|*->*|} f [x\\x<-:arr] fm

derive gLookupFMap (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

lookupFMap :: !k .(FMap v) -> .(Maybe v) | gLookupFMap{|*|} k & bimap{|*|} v
lookupFMap key fmap = case gLookupFMap{|*|} key fmap of
	FMValue v 	-> Just v
	FMEmpty		-> Nothing 
	_			-> abort "erroneous FMap"


//------------------------------------------------------------------------------------

generic gInsertFMap key :: key (FMap value, FMap value) -> (FMap value, FMap value)

gInsertFMap{|Char|} key (new_val, FMChar xs) 
	# (old_val, xs) = updateAssocList key new_val FMEmpty xs
	= (old_val, FMChar xs)
gInsertFMap{|Char|} key (new_val, FMEmpty) 	
	= (FMEmpty, FMChar [(key, new_val)])

gInsertFMap{|Int|} key (new_val, FMInt xs) 
	# (old_val, xs) = updateAssocList key new_val FMEmpty xs
	= (old_val, FMInt xs)
gInsertFMap{|Int|} key (new_val, FMEmpty) 	
	= (FMEmpty, FMInt [(key, new_val)])

gInsertFMap{|Real|} key (new_val, FMReal xs) 
	# (old_val, xs) = updateAssocList key new_val FMEmpty xs
	= (old_val, FMReal xs)
gInsertFMap{|Real|} key (new_val, FMEmpty) 	
	= (FMEmpty, FMReal [(key, new_val)])

gInsertFMap{|Bool|} False (v, FMEither ls rs)	= (ls, FMEither v rs)
gInsertFMap{|Bool|} False (v, FMEmpty) 			= (FMEmpty, FMEither v FMEmpty)
gInsertFMap{|Bool|} True  (v, FMEither ls rs)	= (rs, FMEither ls v)
gInsertFMap{|Bool|} True  (v, FMEmpty)			= (FMEmpty, FMEither FMEmpty v)
	
gInsertFMap{|UNIT|} key (x, y) 		= (y, x)

gInsertFMap{|PAIR|} fx fy (PAIR kx ky) (new_val, fmx) 
	#! (old_fmy, fmx1) = fx kx (FMEmpty, fmx)
	#! (old_val, new_fmy) = fy ky (new_val, old_fmy)   
	#! (empty_fmy, new_fmx) = fx kx (new_fmy, fmx) 
	= (old_val, new_fmx)

gInsertFMap{|EITHER|} fl fr (LEFT key) (v, FMEither ls rs) 
	# (old_val, new_ls) = fl key (v,ls)
	= (old_val, FMEither new_ls rs)
gInsertFMap{|EITHER|} fl fr (LEFT key) (v, FMEmpty)
	# (old_val, new_ls) = fl key (v,FMEmpty)
	= (FMEmpty, FMEither new_ls FMEmpty)
gInsertFMap{|EITHER|} fl fr (RIGHT key) (v, FMEither ls rs)
	# (old_val, new_rs) = fr key (v,rs)
	= (old_val, FMEither ls new_rs)
gInsertFMap{|EITHER|} fl fr (RIGHT key) (v, FMEmpty)
	# (old_val, new_rs) = fr key (v,FMEmpty)
	= (FMEmpty, FMEither FMEmpty new_rs)
	
gInsertFMap{|CONS|} f (CONS key) x = f key x
gInsertFMap{|FIELD|} f (FIELD key) x = f key x
gInsertFMap{|OBJECT|} f (OBJECT key) x = f key x

derive gInsertFMap []

gInsertFMap{|String|} xs fm = gInsertFMap{|*|} [x\\x<-:xs] fm
gInsertFMap{|{}|} f xs fm = gInsertFMap{|*->*|} f [x\\x<-:xs] fm
gInsertFMap{|{!}|} f xs fm = gInsertFMap{|*->*|} f [x\\x<-:xs] fm

derive gInsertFMap (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

(<<=) infixl 1 :: .(FMap v) !.(k,v) -> FMap v | gInsertFMap{|*|} k & bimap{|*|} v
(<<=) fmap (key, value)
	#! (old_val, fmap) = gInsertFMap{|*|} key (FMValue value, fmap)
	= fmap 

//-----------------------------------------------------------------------------
/* 
fmap = FMEmpty 
	<<= ("one", 1)
	<<= ("two", 2)
	<<= ("three", 3)
	<<= ("four", 4)

Start = lookupFMap "two" fmap 
*/
