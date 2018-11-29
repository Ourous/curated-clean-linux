implementation module StdFunc

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2018 University of Nijmegen
// ****************************************************************************************

import StdClass, StdMisc, StdInt

import StdFunctions

seq :: ![.(.s -> .s)] .s -> .s
seq [f:fs] arg	=	seq fs (f arg)
seq [] arg		=	arg

seqList::![St .s .a] .s -> ([.a],.s)
seqList [f:fs] io = ([a:as],iol)
where
	(as,iol) = seqList fs io1 
	(a,io1)  = f io
seqList [] io = ([],io)

//	for people who like state manipulation with monads

:: St s a :== s -> *(a,s)

(`bind`) infix 0; // :: w:(St .s .a) v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]
(`bind`) f g :== \st0 -> let (r,st1) = f st0 in g r st1

// return :: u:a -> u:(St .s u:a)
return r :== \s -> (r,s)
