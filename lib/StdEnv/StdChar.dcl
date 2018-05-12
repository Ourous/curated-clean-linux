system module StdChar

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance + 				Char
instance - 				Char
instance zero 			Char
instance one 			Char

instance ==				Char
instance <  			Char

instance toChar			Char
instance toChar			Int

instance fromChar		Int
instance fromChar		Char
instance fromChar		{#Char}

//	Additional conversions:

digitToInt		:: !Char -> Int		//	Convert Digit into Int
toUpper			:: !Char -> Char	//	Convert Char into an uppercase Char
toLower			:: !Char -> Char	//	Convert Char into a  lowercase Char

//	Tests on Characters:

isUpper			:: !Char -> Bool	//	True if arg1 is an uppercase character
isLower			:: !Char -> Bool	//	True if arg1 is a lowercase character
isAlpha			:: !Char -> Bool	//	True if arg1 is a letter
isAlphanum		:: !Char -> Bool	//	True if arg1 is an alphanumerical character
isDigit			:: !Char -> Bool	//	True if arg1 is a digit
isOctDigit		:: !Char -> Bool	//	True if arg1 is a digit
isHexDigit		:: !Char -> Bool	//	True if arg1 is a digit
isSpace			:: !Char -> Bool	//	True if arg1 is a space, tab etc
isControl		:: !Char -> Bool	//	True if arg1 is a control character
isPrint			:: !Char -> Bool	//	True if arg1 is a printable character
isAscii			:: !Char -> Bool	//	True if arg1 is a 7 bit ASCII character
