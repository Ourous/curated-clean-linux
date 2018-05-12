implementation module StdOrdList

// ****************************************************************************************
//	Concurrent Clean Standard Library Module Version 2.0
//	Copyright 1998 University of Nijmegen
// ****************************************************************************************

import StdClass
import StdInt,StdChar,StdReal
from StdMisc import abort
from StdList import hd

sort::!u:[a] -> u:[a] | Ord a
sort l = hd (msort (pair l)) // mergesort
where
	pair [x1,x2:xs]
		| x2<x1
			= [[x2,x1]:pair xs];
			= [[x1,x2]:pair xs];
	pair x = [x];

	msort [x1,x2:xs] = msort (merge_stage [x1,x2:xs]);
	msort x	= x;

	merge_stage [xs1,xs2:xxs] = [merge xs1 xs2 : merge_stage xxs];
	merge_stage x = x;

sortBy :: (a a -> Bool) !u:[a] -> u:[a]
sortBy less_f l = hd (msort (pair l)) // mergesort
where
	pair [x1,x2:xs]
		| less_f x2 x1
			= [[x2,x1]:pair xs];
			= [[x1,x2]:pair xs];
	pair x = [x];

	msort [x1,x2:xs] = msort (merge_stage [x1,x2:xs]);
	msort x	= x;

	merge_stage [xs1,xs2:xxs] = [mergeBy less_f xs1 xs2 : merge_stage xxs];
	merge_stage x = x;

merge :: !u:[a] !v:[a] -> w:[a] | Ord a, [u v <= w];
merge []  y = y
merge f=:[x:xs] [] = f
merge f=:[x:xs] s=:[y:ys]
	| y<x
		= [y:merge f ys]
		= [x:merge xs s]

mergeBy :: (a a -> Bool) !u:[a] !v:[a] -> w:[a], [u v <= w];
mergeBy less_f [] y = y
mergeBy less_f f=:[x:xs] []	= f
mergeBy less_f f=:[x:xs] s=:[y:ys]
	| less_f y x
		= [y:mergeBy less_f f ys]
		= [x:mergeBy less_f xs s]

maxList::!.[a] -> a | Ord a
maxList [a:x] = max1 a x
where
	max1:: a !.[a] -> a | Ord a
	max1 m [hd:tl]
		| hd<m
			= max1 m tl 
			= max1 hd tl
	max1 m []		= m
maxList []
	= abort "maxList of []"

maxListBy :: (a a -> Bool) !.[a] -> a
maxListBy less_f [a:x] = max1 a x
where
	max1 m [hd:tl]
		| less_f hd m
			= max1 m tl 
			= max1 hd tl
	max1 m []
		= m
maxListBy less_f []
	= abort "maxListBy of []"

minList::!.[a] -> a | Ord a
minList [a:x]	= min1 a x
where
	min1:: a !.[a] -> a | Ord a
	min1 m [hd:tl]
		| m<hd
			= min1 m tl 
			= min1 hd tl
	min1 m []
		= m
minList []
	= abort "minList of []"

minListBy :: (a a -> Bool) !.[a] -> a
minListBy less_f [a:x] = min1 a x
where
	min1 m [hd:tl]
		| less_f m hd
			= min1 m tl 
			= min1 hd tl
	min1 m []
		= m
minListBy less_f []
	= abort "minListBy of []"
