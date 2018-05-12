definition module StdLibMisc

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

from StdInt import class +(..), instance + Int, class ~(..), instance ~ Int, 
 	instance - Int, class -(..), bitand;
from StdArray	import class Array (size, usize, select)
import StdInt

/* a generic map function. Any instance of gMap should apply it's function argument
   to every element contained in the second argument, e.g. for lists: gMap = map */

class gMap c :: (a -> b) !(c a) -> (c b)

instance gMap []
instance gMap {}
instance gMap {!}

// some types

::	Time
	=	{	hours	:: !Int		// hours		(0-23)
		,	minutes	:: !Int		// minutes		(0-59)
		,	seconds	:: !Int		// seconds		(0-59)
		}
::	Date
	=	{	year	:: !Int		// year
		,	month	:: !Int		// month		(1-12)
		,	day		:: !Int		// day			(1-31)
		,	dayNr	:: !Int		// day of week
								// convention:	(1-7, Sunday=1, Saturday=7)
		}

:: Either a b  = Left a | Right b

// extensions for StdReal

isFinite :: !Real -> Bool
	// only not-a-number and the two infinite values are not finite

// extensions for StdInt

roundupToMultiple s m :== (s + (m-1)) bitand (~m)
	// when m==2^n then this formula rounds s up to a multiple of m

// extensions for StdFunc

sseq :: ![.(.s -> .s)] !.s -> .s
	// same as seq, but strict
