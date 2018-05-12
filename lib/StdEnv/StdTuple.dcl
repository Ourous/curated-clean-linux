definition module StdTuple

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import StdClass

// fst		:: !(!.a,.b) -> .a								// t1 of (t1,t2)
fst tuple :== t1 where (t1, _) = tuple
// snd		:: !(.a,!.b) -> .b								// t2 of (t1,t2)
snd tuple :== t2 where (_, t2) = tuple

// fst3	:: !(!.a,.b,.c) -> .a								// t1 of (t1,t2,t3)
fst3 tuple :== t1 where (t1, _, _) = tuple
// snd3	:: !(.a,!.b,.c) -> .b								// t2 of (t1,t2,t3)
snd3 tuple :== t2 where (_, t2, _) = tuple
// thd3	:: !(.a,.b,!.c) -> .c								// t3 of (t1,t2,t3)
thd3 tuple :== t3 where (_, _, t3) = tuple

instance == (a,b)   | Eq a & Eq b
instance == (a,b,c) | Eq a & Eq b & Eq c

instance <  (a,b)   | Ord a & Ord b
instance <  (a,b,c) | Ord a & Ord b & Ord c

app2 	:: !(.(.a -> .b),.(.c -> .d)) !(.a,.c) -> (.b,.d)	// app2 (f,g) (a,b) = (f a,g b)
app3 	:: !(.(.a -> .b),.(.c -> .d),.(.e -> .f)) !(.a,.c,.e) -> (.b,.d,.f)
															// app3 (f,g,h) (a,b,c) = (f a,g b,h c)

curry	:: !.((.a,.b) -> .c) .a .b -> .c					// curry f a b	= f (a,b)
uncurry :: !.(.a -> .(.b -> .c)) !(.a,.b) -> .c				// uncurry f (a,b)	= f a b
