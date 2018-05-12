system module StdInt 

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded


instance +				Int

instance -  			Int

instance zero			Int

instance *  			Int

instance /				Int
instance one			Int


instance ^				Int
instance abs			Int
instance sign			Int
instance ~				Int

instance ==				Int
instance <  			Int
instance isEven 		Int	// True if arg1 is an even number
instance isOdd			Int	// True if arg1 is an odd  number

instance toInt			Char
instance toInt			Int
instance toInt			Real
instance toInt			{#Char}

instance fromInt		Int
instance fromInt		Char
instance fromInt		Real
instance fromInt		{#Char}

// Additional functions for integer arithmetic: 

instance rem Int 	//	remainder after integer division
instance gcd Int	//	Greatest common divider
instance lcm Int	//	Least common multiple

//	Operators on Bits:

(bitor)	infixl  6	:: !Int !Int 	->	Int 	//	Bitwise Or of arg1 and arg2
(bitand) infixl 6	:: !Int !Int 	->	Int 	//	Bitwise And of arg1 and arg2
(bitxor) infixl 6	:: !Int !Int 	->	Int 	//	Exclusive-Or arg1 with mask arg2
(<<)	infix  7	:: !Int !Int 	->	Int 	//	Shift arg1 to the left arg2 bit places
(>>)	infix  7	:: !Int !Int 	->	Int 	//	Shift arg1 to the right arg2 bit places
bitnot				:: !Int 		->	Int 	//	One's complement of arg1

IF_INT_64_OR_32 int64 int32 :== int64;
