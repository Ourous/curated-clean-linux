implementation module GenZip

import StdGeneric
import StdEnv
import StdMaybe

derive bimap Maybe
	
generic gZip a b c :: .a .b -> .c
gZip{|Int|} x y 	= if (x == y) x (abort "zip Int failed\n")
gZip{|Bool|} x y 	= if (x == y) x (abort "zip Bool failed\n")
gZip{|Char|} x y 	= if (x == y) x (abort "zip Char failed\n")
gZip{|Real|} x y 	= if (x == y) x (abort "zip Real failed\n")
gZip{|String|} x y 	= if (x == y) x (abort "zip String failed\n")
gZip{|UNIT|} UNIT UNIT 					= UNIT
gZip{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = PAIR (fx x1 x2) (fy y1 y2)
gZip{|EITHER|} fl fr (LEFT x) (LEFT y) 	= LEFT (fl x y) 
gZip{|EITHER|} fl fr (RIGHT x) (RIGHT y) = RIGHT (fr x y) 
gZip{|EITHER|} fl fr _ _ 				= abort "gZip failed: EITHER does not match\n" 
gZip{|CONS|} f (CONS x) (CONS y) 		= CONS (f x y)
gZip{|FIELD|} f (FIELD x) (FIELD y) 	= FIELD (f x y)
gZip{|OBJECT|} f (OBJECT x) (OBJECT y) 	= OBJECT (f x y)
derive gZip [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
 
generic gMaybeZip a b c :: .a .b -> Maybe .c
gMaybeZip{|Int|} x y = if (x == y) (Just x) Nothing
gMaybeZip{|Bool|} x y = if (x == y) (Just x) Nothing
gMaybeZip{|Char|} x y = if (x == y) (Just x) Nothing
gMaybeZip{|Real|} x y = if (x == y) (Just x) Nothing
gMaybeZip{|String|} x y = if (x == y) (Just x) Nothing
gMaybeZip{|UNIT|} UNIT UNIT = Just UNIT
gMaybeZip{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = zipMaybe PAIR (fx x1 x2) (fy y1 y2)
gMaybeZip{|EITHER|} fl fr (LEFT x) (LEFT y) 	= mapMaybe LEFT (fl x y)
gMaybeZip{|EITHER|} fl fr (RIGHT x) (RIGHT y)  	= mapMaybe RIGHT (fr x y)
gMaybeZip{|EITHER|} fl fr _ _   				= Nothing
gMaybeZip{|CONS|} f (CONS x) (CONS y) 			= mapMaybe CONS (f x y)
gMaybeZip{|FIELD|} f (FIELD x) (FIELD y) 		= mapMaybe FIELD (f x y)
gMaybeZip{|OBJECT|} f (OBJECT x) (OBJECT y) 	= mapMaybe OBJECT (f x y)
derive gMaybeZip [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

zipMaybe :: .(.a -> .(.b -> .c)) !(Maybe .a) (Maybe .b) -> (Maybe .c)
zipMaybe f (Just x) (Just y) 	= Just (f x y)
zipMaybe f _ _ 					= Nothing

