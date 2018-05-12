system module StdString

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance ==			{#Char}
instance <  		{#Char}

instance fromString	{#Char}

instance toString	Int

instance toString	Char

instance toString	Real

instance toString	Bool

instance toString	{#Char}

instance %			{#Char}

instance +++		{#Char}							// string concatenation

(+++.) infixr 5 :: !{#Char} !{#Char} -> .{#Char}	// string concatenation with unique result

(:=) infixl 9 :: !{#Char} !(!Int,!Char) -> {#Char}	//	update i-th element with char
