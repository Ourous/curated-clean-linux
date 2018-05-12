definition module StdCharList

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

cjustify	:: !.Int ![.Char] -> .[Char] // Center [Char] in field with width arg1
ljustify	:: !.Int ![.Char] -> .[Char] // Left justify [Char] in field with width arg1
rjustify	:: !.Int ![.Char] -> [Char]	 // Right justify [Char] in field with width arg1

flatlines	:: ![[u:Char]] -> [u:Char]	 // Concatenate by adding newlines
mklines 	:: ![Char] -> [[Char]]		 // Split in lines removing newlines

spaces		:: !.Int -> .[Char]			 // Make [Char] containing n space characters
