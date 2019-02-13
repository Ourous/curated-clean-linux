definition module utilities
// compile with "reuse unique nodes"

from StdClass import class Eq, not, class Ord, class IncDec

import StdMisc, general, _aconcat

/*
	For Strings
*/

stringToCharList	:: !String -> [Char]
charListToString	:: ![Char] -> String
revCharListToString	:: !Int ![Char] -> String

isUpperCaseName :: ! String -> Bool
isLowerCaseName :: ! String -> Bool
isFunnyIdName 	:: ! String -> Bool
isSpecialChar	:: ! Char	-> Bool

/*
	For Lists
*/

isNotEmpty :: ![a] -> Bool

//mapSt :: !(.a -> (.st -> (.c,.st))) ![.a] !.st -> (![.c],!.st)

mapSt f l s :== map_st l s
where
	map_st [x : xs] s
	 	# (x, s) = f x s
		  (xs, s) = map_st xs s
		#! s = s
		= ([x : xs], s)
	map_st [] s
		#! s = s
	 	= ([], s)

mapSt2 f l s1 s2 :== map_st2 l s1 s2
where
	map_st2 [x : xs] s1 s2
	 	# (x, s1,s2) = f x s1 s2
		  (xs, s1,s2) = map_st2 xs s1 s2
		#! s1 = s1
		#! s2 = s2
		= ([x : xs], s1,s2)
	map_st2 [] s1 s2
	 	= ([], s1,s2)

mapY2St f l s :== map_y2_st l s
where
	map_y2_st [x : xs] s
	 	# (x, y, s) = f x s
		  (xs, ys, s) = map_y2_st xs s
		#! s = s
		= ([x : xs], [y : ys], s)
	map_y2_st [] s
		#! s = s
	 	= ([], [], s)

map2St f l1 l2 st :== map2_st l1 l2 st
  where
	map2_st [h1:t1] [h2:t2] st
		# (h, st) = f h1 h2 st
		  (t, st) = map2_st t1 t2 st
		#! st = st
		= ([h:t], st)
	map2_st _ _ st
		#! st = st
		= ([], st)

app2St :: !(!.(.a -> .(.st -> (.c,.st))),!.(.e -> .(.st -> (.f,.st)))) !(.a,.e) !.st -> (!(.c,.f),!.st)

mapAppendSt :: !(.a -> .(.b -> (.c,.b))) ![.a] !u:[.c] !.b -> (!u:[.c],!.b)

strictMap :: !(.a -> .b) ![.a] -> [.b]

strictMapAppend :: !(.a -> .b) ![.a] !u:[.b] -> v:[.b], [u <= v]

mapAppend :: !(.a -> .b) ![.a] !u:[.b] -> u:[.b]

//zip2Append :: [.a] [.b] u:[w:(.a,.b)] -> v:[x:(.a,.b)], [w <= x, u <= v]

eqMerge :: ![a] ![a] -> [a] | Eq a

// foldl2 :: !(.c -> .(.a -> .(.b -> .c))) !.c ![.a] ![.b] -> .c
foldl2 op r l1 l2
	:== foldl2 r l1 l2
where
	foldl2 r [x : xs] [y : ys]
		= foldl2 (op r x y) xs ys
	foldl2 r [] []
		= r
//foldr2 :: !(.a -> .(.b -> .(.c -> .c))) !.c ![.a] ![.b] -> .c

foldr2 op r l1 l2
	:== foldr2 r l1 l2
where
	foldr2 r [x : xs] [y : ys]
		= op x y (foldr2 r xs ys)	
	foldr2 r [] []
		= r

fold2St op l1 l2 st
	:== fold_st2 l1 l2 st
where
	fold_st2 [x : xs] [y : ys] st
		= op x y (fold_st2 xs ys st)	
	fold_st2 [] [] st
		= st
	fold_st2 [] ys st
		= abort ("fold_st2: second argument list contains more elements")
	fold_st2 xs [] st
		= abort ("fold_st2: first argument list contains more elements")

unsafeFold2St op l1 l2 st
	:== ufold_st2 l1 l2 st
where
	ufold_st2 [x : xs] [y : ys] st
		= ufold_st2 xs ys (op x y st)
	ufold_st2 _ _ st
		= st

unsafeFold3St op l1 l2 l3 st
	:== ufold_st3 l1 l2 l3 st
where
	ufold_st3 [x : xs] [y : ys] [z : zs] st
		= ufold_st3 xs ys zs (op x y z st)
	ufold_st3 _ _ _ st
		= st


// foldSt :: !(.a -> .(.st -> .st)) ![.a] !.st -> .st
foldSt op l st :== fold_st l st
	where
		fold_st [] st		= st
		fold_st [a:x] st	= fold_st x (op a st)

// iFoldSt :: (Int -> .(.b -> .b)) !Int !Int .b -> .b
iFoldSt op fr to st :== i_fold_st fr to st
	where
		i_fold_st fr to st
			| fr >= to
				= st
				= i_fold_st (inc fr) to (op fr st)

iterateSt op st :== iterate_st op st
	where
		iterate_st op st
			# (continue, st) = op st
			| continue
				= iterate_st op st
				= st

mapFilterYesSt f l st
	:== map_filter_yes_st l st
  where
	map_filter_yes_st [] st
		#! st = st
		= ([], st)
	map_filter_yes_st [h:t] st
		#! (opt_f_h , st) = f h st
		   (t2, st) = map_filter_yes_st t st
		   (f_h_t2, _) = optCons opt_f_h t2
		   st = st
		= (f_h_t2, st)
				
iMapFilterYesSt f fr to st 
	:== i_map_filter_yes_st fr to st
  where
	i_map_filter_yes_st fr to st
		#! st = st
		| fr >= to
			= ([], st)
		#! (opt_f_fr, st) = f fr st
		   (t, st) = i_map_filter_yes_st (inc fr) to st
		   (f_fr_t2, _) = optCons opt_f_fr t
		   st = st
		= (f_fr_t2, st)
				
foldlArrayStWithIndex f a st :== fold_a_st_i 0 a st
  where
	fold_a_st_i i a st
		| i==size a
			= st
		# (ai, a) = a![i]
		= fold_a_st_i (i+1) a (f i ai st)

foldlArraySt f a st :== fold_a_st 0 a st
  where
	fold_a_st i a st
		| i==size a
			= st
		# (ai, a) = a![i]
		= fold_a_st (i+1) a (f ai st)

foldrArraySt f a st
	:== foldr_a_st (size a-1) a st
  where
	foldr_a_st i a st
		| i==(-1)
			= st
		# (ai, a) = a![i]
		= foldr_a_st (i-1) a (f ai st)


firstIndex p l :== first_index l 0
  where
	first_index [] i
		= (i-i)-1
	first_index [h:t] i
		| p h
			= i
		= first_index t (i+1)


optCons :: !(Optional .a) !u:[.a] -> (!v:[.a], !Int) ,[u <= v]

revAppend	:: ![a] ![a] -> [a]	//	Reverse the list using the second argument as accumulator.
revMap :: !(.a -> .b) ![.a] !u:[.b] -> u:[.b]

:: Bag x = Empty | Single !x | Pair !(Bag x) !(Bag x)

uniqueBagToList :: !*(Bag x) -> [x] // exploits reuse of unique nodes (if compiled with that option)
bagToList :: !(Bag x) -> [x]
isEmptyBag :: !(Bag x) -> Bool


:: DAG =
	{	dag_nr_of_nodes		:: !Int
	,	dag_get_children	:: !Int -> [Int]
	}

partitionateDAG :: !DAG ![Int] -> [[Int]]
