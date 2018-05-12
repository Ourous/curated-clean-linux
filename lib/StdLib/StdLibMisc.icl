implementation module StdLibMisc

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

import StdEnv

::	Time
	=	{	hours	:: !Int		// hours		(0-23)
		,	minutes	:: !Int		// minutes		(0-59)
		,	seconds	:: !Int		// seconds		(0-59)
		}
::	Date
	=	{	year	:: !Int		// year
		,	month	:: !Int		// month		(1-12)
		,	day		:: !Int		// day			(1-31)
		,	dayNr	:: !Int		// day of week	(1-7, Sunday=1, Saturday=7)
		}

:: Either a b  = Left a | Right b

class gMap c :: (a -> b) !(c a) -> (c b)

instance gMap []
  where
	gMap f l = map f l

instance gMap {}
  where
	gMap f a = { f el \\ el<-:a }

instance gMap {!}
  where
	gMap f a = { f el \\ el<-:a }

inf =: 1.0/0.0
minus_inf =: (-1.0)/0.0

isFinite :: !Real -> Bool
isFinite r = minus_inf<r && r<inf

roundupToMultiple s m :== (s + (m-1)) bitand (~m)

// extensions for StdFunc

sseq :: ![.(.s -> .s)] !.s -> .s
sseq [f:fs] arg	=	sseq fs (f arg)
sseq [] arg		=	arg

