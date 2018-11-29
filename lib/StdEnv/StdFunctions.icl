implementation module StdFunctions

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2018 Radbound University
// ****************************************************************************************

import StdClass, StdMisc, StdInt

id :: !.a -> .a
id x = x

const :: !.a .b -> .a
const x y = x

//flip::!.(.a -> .(.b -> .c)) .b .a -> .c
flip f a b :== f b a

(o) infixr 9; // :: u:(.a -> .b) u:(.c -> .a) -> u:(.c -> .b)
(o) f g :== \ x -> f (g x)

twice :: !(.a -> .a) .a -> .a
twice f x = f (f x)

while :: !(a -> .Bool) (a -> a) a -> a
while p f x	
	| p x	= 	while p f (f x)
			= 	x

until::!(a -> .Bool) (a -> a) a -> a
until p f x
	| p x 	= x
			= until p f (f x)

iter :: !Int (.a -> .a) .a -> .a
iter 0 f x	= 	x
iter n f x
	| n > 0	= 	iter (n-1) f (f x)
			= 	abort "Error: Negative index given to iter."
