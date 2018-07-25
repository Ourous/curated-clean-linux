definition module Gast.GenLibTest

/*
	GAST: A Generic Automatic Software Test-system
	
	Gast.GenLibTest: library for generic testing: showing and comparing values

	Pieter Koopman, 2004
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdGeneric, Data.GenEq

import StdClass
//instance + String

(@@) infixl 2 :: !(a->b)  a -> b
(@@!)infixl 2 :: !(a->b) !a -> b

generic genShow a :: !String !Bool !a ![String] -> [String]
generic gLess a  :: a a -> Bool

derive genShow	Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS of {gcd_name,gcd_arity},RECORD of {grd_name}, FIELD of {gfd_name}, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), (,,,,,,,,), (,,,,,,,,,), (->), {}, {!}
derive gLess    Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, OBJECT, CONS, FIELD, RECORD, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), (,,,,,,,,), (,,,,,,,,,) 

show  :: !a -> [String] | genShow{|*|} a
show1 :: !a ->  String  | genShow{|*|} a

(-<-) infix 4 :: !a !a -> Bool | gLess{|*|} a
(->-) infix 4 :: !a !a -> Bool | gLess{|*|} a
(-<=) infix 4 :: !a !a -> Bool | gLess{|*|} a
(=>-) infix 4 :: !a !a -> Bool | gLess{|*|} a

