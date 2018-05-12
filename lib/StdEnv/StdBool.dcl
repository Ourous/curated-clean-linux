system module StdBool

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance ==				Bool

instance toBool			Bool

instance fromBool		Bool
instance fromBool		{#Char}

//	Additional Logical Operators:

not					:: !Bool		->	Bool	//	Not arg1
(||)	infixr 2	:: !Bool Bool	->	Bool	//	Conditional or  of arg1 and arg2
(&&)	infixr 3	:: !Bool Bool	->	Bool	//	Conditional and of arg1 and arg2

