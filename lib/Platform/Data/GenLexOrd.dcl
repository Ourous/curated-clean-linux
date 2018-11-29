definition module Data.GenLexOrd

import StdGeneric, Data.GenEq

:: LexOrd = LT | EQ | GT
derive gEq LexOrd

generic gLexOrd a :: !a !a -> LexOrd

// base cases
gLexOrd{|UNIT|} UNIT UNIT = EQ
gLexOrd{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = case fx x1 x2 of
	EQ -> fy y1 y2
	LT -> LT
	GT -> GT

gLexOrd{|EITHER|} fl fr (LEFT x) (LEFT y) = fl x y
gLexOrd{|EITHER|} fl fr (LEFT x) (RIGHT y) = LT
gLexOrd{|EITHER|} fl fr (RIGHT x) (LEFT y) = GT
gLexOrd{|EITHER|} fl fr (RIGHT x) (RIGHT y) = fr x y

gLexOrd{|CONS|} f (CONS x) (CONS y) = f x y
gLexOrd{|FIELD|} f (FIELD x) (FIELD y) = f x y
gLexOrd{|OBJECT|} f (OBJECT x) (OBJECT y) = f x y
gLexOrd{|RECORD|} f (RECORD x) (RECORD y) = f x y

derive gLexOrd Char, Bool, Int, Real, String, [], {}, {!}

// standard types
derive gLexOrd (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

//* @type a a -> LexOrd | gLexOrd{|*|} a
(=?=) infix 4
(=?=) x y :== gLexOrd{|*|} x y
