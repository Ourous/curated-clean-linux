definition module StdOrdList

//	****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
//	****************************************************************************************

import StdClass

sort    ::               !u:[a] -> u:[a] | Ord a			// Sort the list (mergesort)
	special
		a	= Char
		a	= Int
		a	= Real
sortBy  :: (a a -> Bool) !u:[a] -> u:[a] 					// Sort the list, arg1 is < function
merge   ::               !u:[a] !v:[a] -> w:[a]	
										 | Ord a,[u v <= w] // Merge two sorted lists giving a sorted list
	special
		a	= Char
		a	= Int
		a	= Real
mergeBy :: (a a -> Bool) !u:[a] !v:[a] -> w:[a]				// Merge two sorted lists giving a sorted list
										 ,[u v <= w]		// arg1 is < function
maxList	:: !.[a] 			-> a 		 | Ord a			// Maximum element of list
	special
		a	= Char
		a	= Int
		a	= Real
maxListBy :: (a a -> Bool) !.[a] -> a						// Maximum element of list, arg1 is < function
minList	:: !.[a] 			-> a 		 | Ord a			// Minimum element of list
	special
		a	= Char
		a	= Int
		a	= Real
minListBy :: (a a -> Bool) !.[a] -> a						// Minimum element of list, arg1 is < function
