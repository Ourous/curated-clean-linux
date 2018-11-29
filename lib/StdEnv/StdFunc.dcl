definition module StdFunc

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2018 Radboud University
// ****************************************************************************************

import StdFunctions

//	Some handy functions for transforming unique states:

seq				:: ![.(.s -> .s)] .s -> .s					// fn-1 (..(f1 (f0 x))..)
seqList			:: ![St .s .a] .s -> ([.a],.s)				// fn-1 (..(f1 (f0 x))..)

:: St s a :== s -> *(a,s)

// monadic style:

(`bind`) infix 0 // :: w:(St .s .a) v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]
(`bind`) f g :== \st0 -> let (r,st1) = f st0 in g r st1

// return :: u:a -> u:(St .s u:a)
return r :== \s -> (r,s)
