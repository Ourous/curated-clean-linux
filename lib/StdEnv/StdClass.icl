implementation module StdClass

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import StdOverloaded
from StdBool import not

//	Remark: derived class members are not implemented yet!
//	For the time-being, macro definitions are used for this purpose
//	This may cause misleading error messages in case of type errors 

class PlusMin a | + , - , zero a

class MultDiv a | * , / , one a

class Arith a 	| PlusMin , MultDiv , abs , sign , ~ a 

class IncDec a	| + , - , one , zero a
where
  inc :: !a -> a | + , one a
  inc x :== x + one

  dec :: !a -> a | - , one a
  dec x :== x - one

class Enum a | < , IncDec a

class Eq a | == a	
where
  (<>) infix  4 :: !a	!a	->	Bool | Eq a
  (<>) x y :== not (x == y)

class Ord a	| < a
where
  (>) infix  4 :: !a !a	-> Bool | Ord a
  (>) x y  :== y < x 

  (<=) infix 4 :: !a !a -> Bool | Ord a
  (<=) x y :== not (y<x)

  (>=) infix 4 :: !a !a -> Bool | Ord a
  (>=) x y :== not (x<y) 

  min::!a !a ->	a | Ord a
  min x y  :== case (x<y) of True = x; _ = y

  max::!a !a ->	a | Ord a
  max x y  :== case (x<y) of True = y; _ = x

