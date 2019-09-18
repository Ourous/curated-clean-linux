implementation module StdTuple

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1995 University of Nijmegen
// ****************************************************************************************

// Standard Functions on Tuples:

import StdClass, StdBool

// fst		:: !(!.a,.b) -> .a									// t1 of (t1,t2)
fst tuple :== t1 where (t1, _) = tuple
// snd		:: !(.a,!.b) -> .b									// t2 of (t1,t2)
snd tuple :== t2 where (_, t2) = tuple

// fst3	:: !(!.a,.b,.c) -> .a								// t1 of (t1,t2,t3)
fst3 tuple :== t1 where (t1, _, _) = tuple
// snd3	:: !(.a,!.b,.c) -> .b								// t2 of (t1,t2,t3)
snd3 tuple :== t2 where (_, t2, _) = tuple
// thd3	:: !(.a,.b,!.c) -> .c								// t3 of (t1,t2,t3)
thd3 tuple :== t3 where (_, _, t3) = tuple

instance == () where
	(==) :: !() !() -> Bool
	(==) _ _ = code inline { pop_a 2 ; pushB TRUE }

instance ==	(a,b) |	Eq a & Eq b
	where
	(==) ::!(!a,b) !(!a,b) -> Bool	|	Eq a & Eq b
	(==) (x1,y1) (x2,y2) = x1==x2 && y1==y2
	
instance == (a,b,c)	| Eq a & Eq b & Eq c
	where
	(==) ::!(!a,b,c) !(!a,b,c) -> Bool	|	Eq a & Eq b & Eq c
	(==) (x1,y1,z1) (x2,y2,z2) = x1==x2 && y1==y2 && z1==z2

instance < () where
	(<) :: !() !() -> Bool
	(<) _ _ = code inline { pop_a 2 ; pushB FALSE }

instance <	(a,b) |	Ord a & Ord b
	where
	(<) ::!(!a,b) !(!a,b) -> Bool	|	Ord a & Ord b
	(<) (x1,y1) (x2,y2)
		|	x1<x2
			=	True
		|	x1>x2
			=	False
		//	otherwise
			=	y1<y2
	
instance <	(a,b,c) | Ord a & Ord b & Ord c
	where
	(<) ::!(!a,b,c) !(!a,b,c) -> Bool	|	Ord a & Ord b & Ord c
	(<) (x1,y1,z1) (x2,y2,z2)
		|	x1<x2
			=	True
		|	x1>x2	
			=	False
		//	otherwise
			=	(y1,z1) < (y2,z2)
	
curry::!.((.a,.b) -> .c) .a .b -> .c
curry f x y     =  f (x,y)

uncurry :: !.(.a -> .(.b -> .c)) !(.a,.b) -> .c;
uncurry f (x,y) = f x y

app2 :: !(.(.a -> .b),.(.c -> .d)) !(.a,.c) -> (.b,.d)
app2 (f1, f2) (x1,x2) = (f1 x1,f2 x2)

app3 :: !(.(.a -> .b),.(.c -> .d),.(.e -> .f)) !(.a,.c,.e) -> (.b,.d,.f)
app3 (f1, f2, f3) (x1,x2,x3) = (f1 x1,f2 x2,f3 x3)
