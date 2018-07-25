definition module StdLibMisc

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

from StdInt import class +(..), instance + Int, class ~(..), instance ~ Int, 
 	instance - Int, class -(..), bitand;
from StdArray	import class Array (size, usize, select)
import StdInt

/**
 * A generic {{`map`}} function. Any instance of `gMap` should apply its
 * function argument to every element contained in the second argument, e.g.
 * for lists: `gMap = {{map}}`.
 */
class gMap c :: (a -> b) !(c a) -> (c b)

instance gMap []
instance gMap {}
instance gMap {!}

// some types

:: Time =
	{ hours   :: !Int //* Hours (0-23)
	, minutes :: !Int //* Minutes (0-59)
	, seconds :: !Int //* Seconds (0-59)
	}

:: Date =
	{ year  :: !Int //* Year
	, month :: !Int //* Month (1-12)
	, day   :: !Int //* Day (1-31)
	, dayNr :: !Int //* Day of week; convention: (1-7, Sunday=1, Saturday=7)
	}

// extensions for StdReal

/**
 * Only not-a-number and the two infinite values are not finite.
 */
isFinite :: !Real -> Bool

// extensions for StdInt

/**
 * When `m==2^n`, this formula rounds `s` up to a multiple of `m`
 * @type Int Int -> Int
 */
roundupToMultiple s m :== (s + (m-1)) bitand (~m)

// extensions for StdFunc

/**
 * Same as {{`seq`}}, but strict.
 */
sseq :: ![.(.s -> .s)] !.s -> .s
