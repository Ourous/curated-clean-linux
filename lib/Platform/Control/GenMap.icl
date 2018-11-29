implementation module Control.GenMap

import StdClass, StdArray, StdInt, StdFunc
import StdGeneric

generic gMap a b :: .a -> .b
gMap{|c|} x 					= x
gMap{|UNIT|} x 					= x
gMap{|PAIR|} fx fy (PAIR x y) 	= PAIR (fx x) (fy y) 
gMap{|EITHER|} fl fr (LEFT x) 	= LEFT (fl x)
gMap{|EITHER|} fl fr (RIGHT x) 	= RIGHT (fr x)
gMap{|CONS|} f (CONS x) 		= CONS (f x)
gMap{|FIELD|} f (FIELD x) 		= FIELD (f x)
gMap{|OBJECT|} f (OBJECT x) 	= OBJECT (f x)
gMap{|{}|} f xs 				= {f x\\x<-:xs}
gMap{|{!}|} f xs				= {f x\\x<-:xs}

derive gMap [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
