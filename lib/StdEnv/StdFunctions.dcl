definition module StdFunctions

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 3.0
//	Copyright 2018 Radboud University
// ****************************************************************************************

id    :: !.a -> .a								// identity function
const :: !.a .b -> .a							// constant function

//flip  :: !.(.a -> .(.b -> .c)) .b .a -> .c		// Flip arguments
flip f a b :== f b a

(o) infixr  9 // ::  u:(.a -> .b) u:(.c -> .a) -> u:(.c -> .b) // Function composition
(o) f g :== \ x -> f (g x)

twice			:: !(.a -> .a)   .a             -> .a		// f (f x)
while			:: !(a -> .Bool) (a -> a) 	 a 	->  a		// while (p x) f (f x) 
until			:: !(a -> .Bool) (a -> a) 	 a 	->  a		// f (f x) until (p x)
iter			:: !Int 		 (.a -> .a) .a	-> .a		// f (f..(f x)..) 
