system module StdInt 

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance +				Int		:: !Int !Int -> Int					:== code { addI }

instance -  			Int		:: !Int !Int -> Int					:== code { subI }

instance zero			Int		:: Int								:== code { pushI 0 }

instance *  			Int		:: !Int !Int -> Int					:== code { mulI }

instance /				Int		:: !Int !Int -> Int					:== code { divI }
instance one			Int		:: Int								:== code { pushI 1 }

instance ^				Int
instance abs			Int
instance sign			Int
instance ~				Int		:: !Int -> Int						:== code { negI }

instance ==				Int		:: !Int !Int -> Bool				:== code { eqI }
instance <  			Int		:: !Int !Int -> Bool				:== code { ltI }
instance isEven 		Int		:: !Int -> Bool						:== code { pushI 1 ; and% ; pushI 0 ; eqI }
						// True if arg1 is an even number
instance isOdd			Int		:: !Int -> Bool						:== code { pushI 1 ; and% ; pushI 0 ; eqI ; notB }
						// True if arg1 is an odd  number

instance toInt			Char	:: !Char -> Int						:== code { CtoI }
instance toInt			Int		:: !Int -> Int						:== code { no_op }
instance toInt			Real	:: !Real -> Int						:== code { RtoI }
instance toInt			{#Char}

instance fromInt		Int		:: !Int -> Int						:== code { no_op }
instance fromInt		Char	:: !Int -> Char						:== code { ItoC }
instance fromInt		Real	:: !Int -> Real						:== code { ItoR }
instance fromInt		{#Char}	:: !Int -> {#Char}					:== code { .d 0 1 i ; jsr ItoAC ; .o 1 0 }

// Additional functions for integer arithmetic: 

instance rem Int		:: !Int !Int -> Int							:== code { remI }
				 	//	remainder after integer division
instance gcd Int	//	Greatest common divider
instance lcm Int	//	Least common multiple

//	Operators on Bits:

(bitor)	infixl  6	:: !Int !Int 	->	Int							:== code { or% }
					//	Bitwise Or of arg1 and arg2
(bitand) infixl 6	:: !Int !Int 	->	Int							:== code { and% }
					//	Bitwise And of arg1 and arg2
(bitxor) infixl 6	:: !Int !Int 	->	Int							:== code { xor% }
					//	Exclusive-Or arg1 with mask arg2
(<<)	infix  7	:: !Int !Int 	->	Int							:== code { shiftl% }
					//	Shift arg1 to the left arg2 bit places
(>>)	infix  7	:: !Int !Int 	->	Int							:== code { shiftr% }
					//	Shift arg1 to the right arg2 bit places
bitnot				:: !Int 		->	Int							:== code { not% }
					//	One's complement of arg1

IF_INT_64_OR_32 int64 int32 :== int64;
