definition module StdMaybe

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

from StdOverloaded import class ==(..);

::	Maybe x
	=	Just x
	|	Nothing

isJust		:: !(Maybe .x) -> Bool		// case @1 of (Just _) -> True; _ -> False
isNothing	:: !(Maybe .x) -> Bool		// not o isJust
fromJust	:: !(Maybe .x) -> .x		// \(Just x) -> x

// for possibly unique elements:
isJustU :: !u:(Maybe .x) -> (!Bool, !u:Maybe .x)
isNothingU :: !u:(Maybe .x) -> (!Bool, !u:Maybe .x)

mapMaybe	:: .(.x -> .y) !(Maybe .x) -> Maybe .y
// mapMaybe f (Just x) = Just (f x)
// mapMaybe f Nothing  = Nothing

instance == (Maybe x) | == x
//	Nothing==Nothing
//	Just a ==Just b <= a==b

maybeToList :: !(Maybe .a) -> [.a];
//	returns list with no or one element

listToMaybe :: ![.a] -> .Maybe .a;
//	returns Just head of list if possible

catMaybes :: ![Maybe .a] -> .[.a];
//	catMaybes ms =  [ m \\ Just m <- ms ]
