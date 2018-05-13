definition module Data.Generics.GenEq

import StdGeneric
from StdBool import qualified &&, not

generic gEq a  :: !a !a -> Bool

// base cases
gEq{|OBJECT|} f (OBJECT x) (OBJECT y) 		= f x y
gEq{|CONS|} f (CONS x) (CONS y) 			= f x y
gEq{|RECORD|} f (RECORD x) (RECORD y) 		= f x y
gEq{|FIELD|} f (FIELD x) (FIELD y) 			= f x y

gEq{|EITHER|} fl fr (LEFT x) (LEFT y) 		= fl x y
gEq{|EITHER|} fl fr (LEFT _) (RIGHT _) 		= False
gEq{|EITHER|} fl fr (RIGHT x) (RIGHT y) 	= fr x y
gEq{|EITHER|} fl fr (RIGHT _) (LEFT _)		= False

gEq{|UNIT|} UNIT UNIT 						= True

gEq{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 'StdBool'. && fy y1 y2

derive gEq Int, Char, Bool, Real, String, {}, {!} 

// standard types
derive gEq [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)


/**
 * @type a a -> Bool | gEq{|*|} a
 */
(===) infix 4
(===) x y :== gEq{|*|} x y

/**
 * @type a a -> Bool | gEq{|*|} a
 */
(=!=) infix 4
(=!=) x y :== 'StdBool'.not (x === y)
