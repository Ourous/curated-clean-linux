definition module StdList

//	****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.2
//	Copyright 1998-2006 University of Nijmegen
//	****************************************************************************************

import StdClass
import StdInt,StdChar,StdReal

//	Instances of overloaded functions:

instance ==	[a] | == a
						special a=Int
								a=Char
								a=Real

instance <	[a] | Ord a
						special a=Int
								a=Char
								a=Real

instance length	[]
instance %		[a]

instance toString 	[x] | toChar x	 // Convert [x]    via [Char] into String
									special x=Char
instance fromString [x] | fromChar x // Convert String via [Char] into [x]
									special x=Char

//	List Operators:

(!!) 	infixl 9	:: ![.a] !Int -> .a				//	Get nth element of the list
(++)	infixr 5	:: ![.a] u:[.a] -> u:[.a]		//	Append args
flatten				:: ![[.a]] -> [.a]				//	e0 ++ e1 ++ ... ++ e##
isEmpty				:: ![.a] -> Bool				//	[] ?

//	List breaking or permuting functions:

hd			:: ![.a] -> .a							//	Head of the list
tl			:: !u:[.a] -> u:[.a]					//	Tail of the list
last		:: ![.a] -> .a							//	Last element of the list
init	 	:: ![.a] -> [.a]						//	Remove last element of the list
take		:: !Int [.a] -> [.a]					//	Take first arg1 elements of the list
takeWhile	:: (a -> .Bool) !.[a] -> .[a]			//	Take elements while pred holds
drop		:: !Int !u:[.a] -> u:[.a]				//	Drop first arg1 elements from the list
dropWhile	:: (a -> .Bool) !u:[a] -> u:[a]			//	Drop elements while pred holds
span		:: (a -> .Bool) !u:[a] -> (.[a],u:[a])	//	(takeWhile list,dropWhile list)
filter		:: (a -> .Bool) !.[a] -> .[a]			//	Drop all elements not satisfying pred
reverse		:: ![.a] -> [.a]							//	Reverse the list
insert 		:: (a -> a -> .Bool) a !u:[a] -> u:[a]		//	Insert arg2 when pred arg2 elem holds   
insertAt	:: !Int .a u:[.a] -> u:[.a]				//	Insert arg2 on position arg1 in list
removeAt	:: !Int !u:[.a] -> u:[.a]				//	Remove arg2!!arg1 from list
updateAt 	:: !Int .a !u:[.a] -> u:[.a]			//	Replace list!!arg1 by arg2
splitAt		:: !Int u:[.a] -> ([.a],u:[.a])			//	(take n list,drop n list)

//	Creating lists:

map			:: (.a -> .b) ![.a] -> [.b]				//	[f e0,f e1,f e2,...
iterate		:: (a -> a) a -> .[a]					//	[a,f a,f (f a),...
indexList	:: !.[a] -> [Int]						//	[0..maxIndex list]
repeatn		:: !.Int a -> .[a]						//	[e0,e0,...,e0] of length n
repeat		:: a -> [a]								//	[e0,e0,...
unzip		::	![(.a,.b)] 		-> ([.a],[.b])		//	([a0,a1,...],[b0,b1,...])
zip2		:: ![.a] [.b] 		-> [(.a,.b)]		//	[(a0,b0),(a1,b1),...
zip			:: !(![.a],[.b]) 	-> [(.a,.b)]		//	[(a0,b0),(a1,b1),...
diag2		:: !.[a] .[b]		-> [.(a,b)]			//	[(a0,b0),(a1,b0),(a0,b1),...
diag3		:: !.[a] .[b] .[c]	-> [.(a,b,c)]		//	[(a0,b0,c0),(a1,b0,c0),...

//	Folding and scanning:

// for efficiency reasons, foldl and foldr are macros, so that applications of these functions will be inlined

// foldl :: (.a -> .(.b -> .a)) .a ![.b] -> .a	//	op(...(op (op (op r e0) e1)...e##)
foldl op r l :== foldl r l
	where
		foldl r []		= r
		foldl r [a:x]	= foldl (op r a) x

// foldr :: (.a -> .(.b -> .b)) .b ![.a] -> .b	//	op e0 (op e1(...(op e## r)...)
foldr op r l :== foldr l
	where
		foldr []	= r
		foldr [a:x]	= op a (foldr x)

scan		::  (a -> .(.b -> a)) a ![.b] -> .[a]	//	[r,op r e0,op (op r e0) e1,...

//	On Booleans

and			:: ![.Bool] -> Bool						//	e0 && e1 ... && e##
or			:: ![.Bool] -> Bool						//	e0 || e1 ... || e##
any			:: (.a -> .Bool) ![.a] -> Bool			//	True, if ei is True for some i
all			:: (.a -> .Bool) ![.a] -> Bool			//	True, if ei is True for all i

//	When equality is defined on list elements

isMember		::    a	 !.[a]	-> Bool 	| Eq a	//	Is element in list
													special a=Int
															a=Char
															a=Real
isAnyMember		:: !.[a]  !.[a] -> Bool 	| Eq a	//	Is one of arg1 an element arg2
													special a=Int
															a=Char
															a=Real
removeMember	:: a !u:[a] -> u:[a] 		| Eq a	//	Remove first occurrence of arg1 from list arg2
													special a=Int
															a=Char
															a=Real
removeMembers	:: !u:[a] !.[a]	-> u:[a] 	| Eq a	//	Remove first occurrences in arg2 from list arg1
													special a=Int
															a=Char
															a=Real
removeDup		:: !.[a] 		-> .[a] 	| Eq a	//	Remove all duplicates from list
													special a=Int
															a=Char
															a=Real
removeIndex 	:: !a !u:[a] -> (Int,u:[a])	| Eq a	//	"removeMember" returning index of removed element
													special a=Int
															a=Char
															a=Real
limit			:: !.[a] 		-> a 		| Eq a	//	find two succeeding elements that are equal
													//	e.g. limit [1,3,2,2,1] == 2
													special a=Int
															a=Char
															a=Real

//	When addition is defined on list elements

sum :: !.[a] -> a |  + , zero  a					//	sum of list elements, sum [] = zero
									special	a=Int
											a=Real

//	When multiplication and addition is defined on list elements

prod :: !.[a] -> a | * , one  a 					//	product of list elements, prod [] = one
									special	a=Int
											a=Real
avg :: !.[a] -> a | / , IncDec a					//	average of list elements, avg [] gives error!
									special	a=Int
											a=Real
														

