implementation module StdMaybe

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

from StdOverloaded import class ==(..);

::	Maybe x
	=	Just x
	|	Nothing

isJust :: !(Maybe .x) -> Bool
isJust Nothing	= False
isJust _		= True

isNothing :: !(Maybe .x) -> Bool
isNothing Nothing	= True
isNothing _		= False

isJustU :: !u:(Maybe .x) -> (!Bool, !u:Maybe .x)
isJustU nothing=:Nothing
	= (False, nothing)
isJustU just
	= (True, just)

isNothingU :: !u:(Maybe .x) -> (!Bool, !u:Maybe .x)
isNothingU nothing=:Nothing
	= (True, nothing)
isNothingU just
	= (False,just)

fromJust :: !(Maybe .x) -> .x
fromJust (Just x) = x

mapMaybe :: .(.x -> .y) !(Maybe .x) -> Maybe .y
mapMaybe f (Just x) = Just (f x)
mapMaybe _ nothing  = Nothing

instance == (Maybe x) | == x where
	(==) Nothing  maybe	= case maybe of
							Nothing -> True
							just    -> False
	(==) (Just a) maybe	= case maybe of
							Just b  -> a==b
							nothing -> False

maybeToList :: !(Maybe .a) -> [.a];
maybeToList Nothing    =  []
maybeToList (Just a)   =  [a]

listToMaybe :: ![.a] -> .Maybe .a;
listToMaybe []         =  Nothing
listToMaybe [a:_]      =  Just a
 
catMaybes :: ![Maybe .a] -> .[.a];
catMaybes ms           =  [ m \\ Just m <- ms ]
