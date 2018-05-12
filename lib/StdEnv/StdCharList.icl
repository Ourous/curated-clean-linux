implementation module StdCharList

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1995 University of Nijmegen
// ****************************************************************************************

import StdList, StdInt, StdChar

cjustify::!.Int ![.Char] -> .[Char]
cjustify n s = spaces lmargin ++ s ++ spaces rmargin
where
	margin	= n - length s	
	lmargin	= margin / 2	
	rmargin	= margin - lmargin

ljustify::!.Int ![.Char] -> .[Char]
ljustify n s = s ++ spaces (n - length s)

rjustify::!.Int ![.Char] -> [Char]
rjustify n s = spaces (n - length s ) ++ s

flatlines::![[u:Char]] -> [u:Char]
flatlines [a:x]	= a ++ ['\n' : flatlines x]
flatlines []	= []

mklines::![Char] -> [[Char]]
mklines [] 					= []
mklines [a:x] | a == '\n'	= [[]:mklines x]
							= [[a:hd result]:tl result]
where 
	result	=	case x of
 					[]	->	[[]]
					n	->	mklines x
	
spaces::!.Int -> .[Char]
spaces n = repeatn n ' '
