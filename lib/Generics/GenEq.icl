implementation module GenEq

import StdGeneric, StdEnv

generic gEq a  :: a a -> Bool
gEq{|Int|} 	x y 							= x == y
gEq{|Char|} x y 							= x == y
gEq{|Bool|} x y 							= x == y
gEq{|Real|} x y 							= x == y
gEq{|String|} x y 							= x == y
gEq{|UNIT|} UNIT UNIT 						= True
gEq{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 && fy y1 y2
gEq{|EITHER|} fl fr (LEFT x) (LEFT y) 		= fl x y
gEq{|EITHER|} fl fr (RIGHT x) (RIGHT y) 	= fr x y
gEq{|EITHER|} fl fr 	_ _ 				= False
gEq{|CONS|} f (CONS x) (CONS y) 			= f x y
gEq{|RECORD|} f (RECORD x) (RECORD y) 		= f x y
gEq{|FIELD|} f (FIELD x) (FIELD y) 			= f x y
gEq{|OBJECT|} f (OBJECT x) (OBJECT y) 		= f x y
gEq{|{}|} f xs ys 							= eqArray f xs ys
gEq{|{!}|} f xs ys 							= eqArray f xs ys

derive gEq [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)


(===) infix 4 :: a a -> Bool | gEq{|*|} a
(===) x y = gEq{|*|} x y

(=!=) infix 4 :: a a -> Bool | gEq{|*|} a
(=!=) x y = not (x === y)

eqArray f xs ys = size xs == size ys && eq 0 (size xs) xs ys
where
	eq i n xs ys
		| i == n 	= True		
		| i < n 	= f xs.[i] ys.[i] && eq (inc i) n xs ys
