definition module StdOverloaded

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

class (+)  infixl 6	a	:: !a	!a	->	a				//	Add arg1 to arg2

class (-)  infixl 6	a	:: !a	!a	->	a 				//	Subtract arg2 from arg1
class zero 			a	:: a							//	Zero (unit element for addition)

class (*)  infixl 7	a	:: !a	!a	->	a				//	Multiply arg1 with arg2
class (/)  infixl 7	a	:: !a	!a	->	a				//	Divide arg1 by arg2
class one 			a	:: a							//	One (unit element for multiplication)

class (==) infix  4	a	:: !a	!a	->	Bool			//	True if arg1 is equal to arg2
class (<)  infix  4	a	:: !a	!a	->	Bool	 		//	True if arg1 is less than arg2
class isEven a :: !a -> Bool;							// True if arg1 is an even number
class isOdd  a :: !a -> Bool;							// True if arg1 is an odd  number

class length m	:: !(m a) -> Int						//	Number of elements in arg
														//	used for list like structures (linear time)

class (%)  infixl 9	a	:: !a !(!Int,!Int)	-> a		//	Slice a part from arg1

class (+++) infixr 5	a 	:: !a	!a	-> a			//	Append args

class (^)  infixr 8	a	:: !a	!a 	->	a				//	arg1 to the power of arg2
class abs  			a	:: !a		->	a				//	Absolute value
class sign 			a	:: !a 		->	Int				//	1 (pos value) -1 (neg value) 0 (if zero)
class ~				a	:: !a 		->	a				//	-a1

class (mod) infix 7 a :: !a !a -> a						//	arg1 modulo arg2
class (rem)	infix 7	a :: !a !a -> a 					//	remainder after division
class gcd a :: !a !a -> a								//	Greatest common divider
class lcm a :: !a !a -> a								//	Least common multiple

class toInt			a	:: !a		->	Int				//	Convert into Int
class toChar		a	:: !a		->	Char			//	Convert into Char
class toBool		a	:: !a		->	Bool			//	Convert into Bool
class toReal		a	:: !a		->	Real			//	Convert into Real
class toString		a	:: !a		->	{#Char}			//	Convert into String

class fromInt		a	:: !Int			-> a			//	Convert from Int
class fromChar		a	:: !Char		-> a			//	Convert from Char
class fromBool		a	:: !Bool		-> a			//	Convert from Bool
class fromReal		a	:: !Real		-> a			//	Convert from Real
class fromString	a	:: !{#Char}		-> a			//	Convert from String

class ln			a	:: !a 		->	a				//	Logarithm base e
class log10			a	:: !a 		->	a				//	Logarithm base 10
class exp			a	:: !a 		->	a				//	e to to the power	
class sqrt			a	:: !a 		->	a				//	Square root

//	Trigonometrical Functions:

class sin			a	:: !a		->	a				//	Sine
class cos			a	:: !a		->	a				//	Cosine
class tan			a	:: !a		->	a				//	Tangent
class asin			a	:: !a		->	a				//	Arc Sine
class acos			a	:: !a		->	a				//	Arc Cosine
class atan			a	:: !a		->	a				//	Arc Tangent
class sinh			a	:: !a		->	a				//	Hyperbolic Sine
class cosh			a	:: !a		->	a				//	Hyperbolic Cosine
class tanh			a	:: !a		->	a				//	Hyperbolic Tangent
class asinh			a	:: !a		->	a				//	Arc Hyperbolic Sine
class acosh			a	:: !a		->	a				//	Arc Hyperbolic Cosine
class atanh			a	:: !a		->	a				//	Arc Hyperbolic Tangent
