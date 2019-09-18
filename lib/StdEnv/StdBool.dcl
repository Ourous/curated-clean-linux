system module StdBool

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance ==				Bool	:: !Bool !Bool -> Bool				:== code { eqB }

instance toBool			Bool	:: !Bool -> Bool					:== code { no_op }

instance fromBool		Bool	:: !Bool -> Bool					:== code { no_op }
instance fromBool		{#Char}	:: !Bool -> {#Char}					:== code { .d 0 1 b ; jsr BtoAC ; .o 1 0 }

//	Additional Logical Operators:

not					:: !Bool		->	Bool						:== code { notB }
												//	Not arg1
(||)	infixr 2	:: !Bool Bool	->	Bool	//	Conditional or  of arg1 and arg2
(&&)	infixr 3	:: !Bool Bool	->	Bool	//	Conditional and of arg1 and arg2

