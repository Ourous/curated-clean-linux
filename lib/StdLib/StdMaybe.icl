implementation module StdMaybe

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

from StdFunc import :: St;
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

u_isJust :: !u:(Maybe .x) -> (!Bool, !u:Maybe .x)
u_isJust nothing=:Nothing
	= (False, nothing)
u_isJust just
	= (True, just)

u_isNothing :: !u:(Maybe .x) -> (!Bool, !u:Maybe .x)
u_isNothing nothing=:Nothing
	= (True, nothing)
u_isNothing just
	= (False,just)

fromJust :: !(Maybe .x) -> .x
fromJust (Just x) = x

accMaybe :: .(St .x .a) !u:(Maybe .x) -> (!Maybe .a,!u:Maybe .x)
accMaybe f (Just x)
	# (a,x) = f x
	= (Just a,Just x)
accMaybe _ nothing
	= (Nothing,nothing)

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
