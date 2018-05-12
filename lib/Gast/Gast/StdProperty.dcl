definition module Gast.StdProperty

/*
	GAST: A Generic Automatic Software Test-system
	
	stdProperty: opertors on logical properties

	Pieter Koopman, 2004
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import Gast.GenLibTest
import Gast.Testable
from Testing.TestEvents import :: Relation

class (\/) infixr 2 a b	:: !a b -> Property	//	Conditional or  of arg1 and arg2
class (/\) infixr 3	a b :: !a b -> Property	//	Conditional and of arg1 and arg2

instance /\ Bool     Bool
instance /\ Property Bool
instance /\ Bool     Property
instance /\ Property Property

instance \/ Bool     Bool
instance \/ Property Bool
instance \/ Bool     Property
instance \/ Property Property

(=.=) infix 4 :: !a !a -> Property | Eq, genShow{|*|}, JSONEncode{|*|} a // shows values x and y if x =.= y yields False
check :: !(a b -> Bool) !a !b -> Property | genShow{|*|}, JSONEncode{|*|} a & genShow{|*|}, JSONEncode{|*|} b

class (==>) infixr 1 b :: !b p -> Property | Testable p

instance ==> Bool
instance ==> Property

(<==>) infix 1 :: !a !b -> Property	| Testable a & Testable b		//	True if properties are equivalent

ExistsIn :: (x->p) [x] -> Property | Testable p & TestArg x 		// type is too restricive
Exists :: (x->p) -> Property | Testable p & TestArg x
ForAll :: !(x->p) -> Property | Testable p & TestArg x

ForEach :: ![x] !(x->p) -> Property | Testable p & TestArg x
(For) infixl 0 :: !(x->p) ![x] -> Property | Testable p & TestArg x
(ForAndGen) infixl 0 :: !(x->p) ![x] -> Property | Testable p & TestArg x

classify :: !Bool l !p -> Property | Testable p & genShow{|*|} l
label ::  !l !p -> Property | Testable p & genShow{|*|} l
name :: !n !p -> Property | Testable p & toString n
limitNrOfRecFieldValues :: !(Map (TypeName, RecFieldName) Int) !p -> Property | Testable p

instance ~ Bool
instance ~ Property
