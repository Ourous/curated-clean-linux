implementation module Control.GenMapSt

import StdGeneric, StdEnv, Data.Array, Data._Array

generic gMapLSt a b :: .a .st -> (.b, .st)
gMapLSt{|c|} x st 				= (x, st)
gMapLSt{|UNIT|} _ st 				= (UNIT, st)
gMapLSt{|PAIR|} fx fy (PAIR x y) st
	# (x, st) = fx x st
	# (y, st) = fy y st
	= (PAIR x y, st)
gMapLSt{|EITHER|} fl fr x st 	= mapStEITHER fl fr x st
gMapLSt{|CONS|} f x st 			= mapStCONS f x st
gMapLSt{|FIELD|} f x st 		= mapStFIELD f x st
gMapLSt{|OBJECT|} f x st 		= mapStOBJECT f x st
gMapLSt{|{}|} f x st			= mapArrayLSt f x st
gMapLSt{|{!}|} f x st			= mapArrayLSt f x st

derive gMapLSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gMapRSt a b :: .a .st -> (.b, .st)
gMapRSt{|c|} x st = (x, st)
gMapRSt{|UNIT|} _ st 				= (UNIT, st)
gMapRSt{|PAIR|} fx fy (PAIR x y) st
	# (y, st) = fy y st
	# (x, st) = fx x st
	= (PAIR x y, st)
gMapRSt{|EITHER|} fx fy x st 	= mapStEITHER fx fy x st
gMapRSt{|CONS|} f x st 			= mapStCONS f x st
gMapRSt{|FIELD|} f x st 		= mapStFIELD f x st
gMapRSt{|OBJECT|} f x st 		= mapStOBJECT f x st
gMapRSt{|{}|} f x st			= mapArrayRSt f x st
gMapRSt{|{!}|} f x st			= mapArrayRSt f x st

derive gMapRSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

mapStEITHER fl fr (LEFT x) st
	# (x, st) = fl x st
	= (LEFT x, st)
mapStEITHER fl fr (RIGHT x) st
	# (x, st) = fr x st
	= (RIGHT x, st)
mapStCONS f (CONS x) st
	# (x, st) = f x st
	= (CONS x, st)
mapStFIELD f (FIELD x) st
	# (x, st) = f x st
	= (FIELD x, st)
mapStOBJECT f (OBJECT x) st
	# (x, st) = f x st
	= (OBJECT x, st)

mapArrayLSt f xs st
	#! (size_xs, xs) = usize xs
	#! (xs, ys, st) = map f 0 size_xs xs (unsafeCreateArray size_xs) st
	= (ys, st)
where
		map f i n xs ys st
		| i == n
			= (xs, ys, st)
		| otherwise
			#! (x, xs) = unsafeUselect xs i
			#! (y, st) = f x st
			#! ys = update ys i y
			= map f (inc i) n xs ys st

mapArrayRSt f xs st
	#! (size_xs, xs) = usize xs
	#! (xs, ys, st) = map f (size_xs - 1) xs (unsafeCreateArray size_xs) st
	= (ys, st)
where
		map f i xs ys st
		| i < 0
			= (xs, ys, st)
		| otherwise
			#! (x, xs) = unsafeUselect xs i
			#! (y, st) = f x st
			#! ys = update ys i y
			= map f (dec i) xs ys st
