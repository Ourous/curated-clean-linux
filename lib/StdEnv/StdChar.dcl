system module StdChar

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2019 University of Nijmegen
// ****************************************************************************************

import	StdOverloaded

instance + 				Char	:: !Char !Char -> Char		:== code { addI ; ItoC }
instance - 				Char	:: !Char !Char -> Char		:== code { subI; ItoC }
instance zero 			Char	:: Char						:== code { pushI 0; ItoC }
instance one 			Char	:: Char						:== code { pushI 1; ItoC }

instance ==				Char	:: !Char !Char -> Bool		:== code { eqC }
instance <  			Char	:: !Char !Char -> Bool		:== code { ltC }

instance toChar			Char	:: !Char -> Char			:== code { no_op }
instance toChar			Int		:: !Int -> Char				:== code { ItoC }

instance fromChar		Int		:: !Char -> Int				:== code { CtoI }
instance fromChar		Char	:: !Char -> Char			:== code { no_op }
instance fromChar		{#Char}	:: !Char -> {#Char}			:== code { CtoAC }

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
