system module StdString

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance ==			{#Char}	:: !{#Char} !{#Char} -> Bool					:== code { .d 2 0 ; jsr eqAC ; .o 0 1 b	}
instance <  		{#Char} :: !{#Char} !{#Char} -> Bool					:== code { .d 2 0 ; jsr cmpAC ; .o 0 1 i ; pushI 0 ; gtI }

instance fromString	{#Char}	:: !{#Char} -> {#Char}							:== code { no_op }

instance toString	Int		:: !Int -> {#Char}								:== code { .d 0 1 i ; jsr ItoAC ; .o 1 0 }

instance toString	Char	:: !Char -> {#Char}								:== code { CtoAC }

instance toString	Real	:: !Real -> {#Char}								:== code { .d 0 1 r ; jsr RtoAC ; .o 1 0 }

instance toString	Bool	:: !Bool -> {#Char}								:== code { .d 0 1 b ; jsr BtoAC ; .o 1 0 }

instance toString	{#Char}	:: !{#Char} -> {#Char}							:== code { no_op }

instance %			{#Char}	:: !{#Char} !(!Int,!Int) -> {#Char}				:== code { .d 1 2 ii ; jsr sliceAC ; .o 1 0 }

instance +++		{#Char}	:: !{#Char} !{#Char} -> {#Char}					:== code { .d 2 0 ; jsr catAC ; .o 1 0 }
													// string concatenation

(+++.) infixr 5 :: !{#Char} !{#Char} -> .{#Char}							:== code { .d 2 0 ; jsr catAC ; .o 1 0 }
													// string concatenation with unique result

(:=) infixl 9 :: !{#Char} !(!Int,!Char) -> {#Char}							:== code { .d 1 2 ic ; jsr updateAC ; .o 1 0 }
													//	update i-th element with char
