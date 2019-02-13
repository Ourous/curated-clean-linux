implementation module utilities
// compile using the "reuse unique nodes option"

import	StdEnv, general

/*
		Utility routines.
*/
StringToCharList`	:: !String !Int !Int -> [Char]
StringToCharList` string 0 index
		= 	[]
StringToCharList` string length index
		= [string.[index] : StringToCharList` string (dec length) (inc index)]
		
stringToCharList	:: !String ->	[Char]
stringToCharList string = StringToCharList` string (size string) 0

charListToString	:: ![Char] -> String
charListToString [hd:tl] = toString hd +++ charListToString tl 
charListToString []      = ""

revCharListToString	:: !Int ![Char] -> String
revCharListToString max_index l
	# string = createArray (max_index + 1) '\0'
	= fill_string max_index l string
where
	fill_string :: !Int ![Char] !*String -> *String
	fill_string n [ char : rest] string
		= fill_string (n - 1) rest { string & [n] = char }
	fill_string (-1) [] string
		= string
		  
/*
revCharListToString [hd:tl] = revCharListToString tl +++ toString hd 
revCharListToString []      = ""
*/

skipUnderscores :: !Int !Int !String -> Char
skipUnderscores i size s
	| i < size
		#! c = s.[i]
		| c == '_'
			= skipUnderscores (i+1) size s
			= c
	// otherwise: i >= size
		= '_'

isUpperCaseName :: ! String -> Bool
isUpperCaseName id
	#! c = skipUnderscores 0 (size id) id 
	= 'A' <= c  &&  c <= 'Z'

isLowerCaseName :: ! String -> Bool
isLowerCaseName id
	#! c = skipUnderscores 0 (size id) id
	= 'a' <= c  &&  c <= 'z'

isFunnyIdName :: ! String -> Bool
isFunnyIdName id
	= isSpecialChar id.[0]

isSpecialChar	:: !Char -> Bool
isSpecialChar '~'	= True
isSpecialChar '@'	= True
isSpecialChar '#'	= True
isSpecialChar '$'	= True
isSpecialChar '%'	= True
isSpecialChar '^'	= True
isSpecialChar '?'	= True
isSpecialChar '!'	= True
isSpecialChar '+'	= True
isSpecialChar '-'	= True
isSpecialChar '*'	= True
isSpecialChar '<'	= True
isSpecialChar '>'	= True
isSpecialChar '\\'	= True
isSpecialChar '/'	= True
isSpecialChar '|'	= True
isSpecialChar '&'	= True
isSpecialChar '='	= True
isSpecialChar ':'	= True
isSpecialChar '.'	= True
isSpecialChar c	= False

isNotEmpty :: ![a] -> Bool
isNotEmpty [] = False
isNotEmpty _  = True

strictMap :: !(.a -> .b) ![.a] -> [.b]
strictMap f [x : xs]
		#!	head = f x
			tail = strictMap f xs
		=	[head : tail]
strictMap f xs
		= []

strictMapAppend :: !(.a -> .b) ![.a] !u:[.b] -> v:[.b], [u <= v]
strictMapAppend f [x : xs] tail
	#! x = f x
	   xs = strictMapAppend f xs tail
	= [x : xs]
strictMapAppend f [] tail
	= tail

mapAppend :: !(.a -> .b) ![.a] !u:[.b] -> u:[.b]
mapAppend f [x : xs] tail
	#  x = f x
	   xs = mapAppend f xs tail
	= [x : xs]
mapAppend f [] tail
	= tail


mapAppendSt :: !(.a -> .(.b -> (.c,.b))) ![.a] !u:[.c] !.b -> (!u:[.c],!.b)
mapAppendSt f [x : xs] tail s 
	# (x, s) = f x s
	  (xs, s) = mapAppendSt f xs tail s
	= ([x : xs], s)
mapAppendSt f [] tail s
	= (tail, s)

/*
mapSt :: !(.a -> (.st -> (.c,.st))) ![.a] !.st -> (![.c],!.st)
mapSt f [x : xs] s
 	# (x, s) = f x s
	  (xs, s) = mapSt f xs s
	= ([x : xs], s)
mapSt f [] s
 	= ([], s)
*/
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
app2St (f,g) (x,y) s
 	# (x, s) = f x s
	  (y, s) = g y s
	= ((x,y), s)


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
foldSt op r l :== fold_st r l
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
optCons No l
	= (l, 0)
optCons (Yes x) l
	= ([x:l], 0)


eqMerge :: ![a] ![a] -> [a] | Eq a
eqMerge [a : x] y
	| isMember a y
		= eqMerge x y
		= [a : eqMerge x y]
eqMerge x y
		= y

revAppend	:: ![a] ![a] -> [a]	//	Reverse the list using the second argument as accumulator.
revAppend [] acc = acc
revAppend [x : xs] acc = revAppend xs [x : acc]

revMap :: !(.a -> .b) ![.a] !u:[.b] -> u:[.b]
revMap f [] acc = acc
revMap f [x : xs] acc = revMap f xs [f x : acc]



/*		
zip2Append :: [.a] [.b] u:[w:(.a,.b)] -> v:[x:(.a,.b)], [w <= x, u <= v]
zip2Append [] [] tail
	= tail
zip2Append [x : xs] [y : ys] tail
	= [(x,y) : zip2Append xs ys tail]
*/

:: Bag x = Empty | Single !x | Pair !(Bag x) !(Bag x)

uniqueBagToList :: !*(Bag x) -> [x] // exploits reuse of unique nodes (if compiled with that option)
uniqueBagToList bag
	= accumulate_elements bag []
  where
	accumulate_elements :: !*(Bag x) [x] -> [x]
	accumulate_elements Empty accu
		= accu
	accumulate_elements (Single element) accu
		= [element : accu]
	accumulate_elements (Pair bag1 bag2) accu
		= accumulate_elements bag1 (accumulate_elements bag2 accu)
		
bagToList :: !(Bag x) -> [x]
bagToList bag
	= accumulate_elements bag []
  where
	accumulate_elements :: !(Bag x) [x] -> [x]
	accumulate_elements Empty accu
		= accu
	accumulate_elements (Single element) accu
		= [element : accu]
	accumulate_elements (Pair bag1 bag2) accu
		= accumulate_elements bag1 (accumulate_elements bag2 accu)
		
isEmptyBag :: !(Bag x) -> Bool
isEmptyBag Empty = True
isEmptyBag _ = False

:: DAG =
	{	dag_nr_of_nodes		:: !Int
	,	dag_get_children	:: !Int -> [Int]
	}

::	PartitioningState = 
	{	ps_marks :: 		!.{# Int}
	,	ps_next_num ::		!Int
	,	ps_groups ::		![[Int]]
	,	ps_deps ::			![Int]
	}

NotChecked :== -1	

partitionateDAG :: !DAG ![Int] -> [[Int]]
partitionateDAG pi=:{dag_nr_of_nodes} roots
	# partitioning_info
			= { ps_marks = createArray dag_nr_of_nodes NotChecked, ps_deps = [],
				ps_next_num = 0, ps_groups = [] }
	  {ps_groups}
			= foldSt (partitionate_node pi) roots partitioning_info
	= ps_groups
where
	partitionate_node :: !DAG !Int !*PartitioningState -> *PartitioningState
	partitionate_node pi node_index ps=:{ps_marks}
		| ps_marks.[node_index] == NotChecked
			= snd (partitionate_unvisited_node node_index pi ps)
		= ps

	partitionate_unvisited_node :: !Int !DAG !*PartitioningState
								-> (!Int, !*PartitioningState)
	partitionate_unvisited_node node_index pi ps=:{ps_next_num}
		# children
				= pi.dag_get_children node_index
		  (min_dep, ps)
		  		= visit_children children pi.dag_nr_of_nodes pi (push_on_dep_stack node_index ps)
		= try_to_close_group node_index ps_next_num min_dep pi ps

	push_on_dep_stack :: !Int !*PartitioningState -> *PartitioningState
	push_on_dep_stack node_index ps=:{ps_deps,ps_marks,ps_next_num}
		= { ps & ps_deps = [node_index : ps_deps], ps_marks = { ps_marks & [node_index] = ps_next_num},
			ps_next_num = inc ps_next_num}

	visit_children :: ![Int] !Int !DAG !*PartitioningState -> (!Int, !*PartitioningState)
	visit_children [child:children] min_dep pi ps=:{ps_marks} 
		#! mark = ps_marks.[child]
		| mark == NotChecked
			# (mark, ps) = partitionate_unvisited_node child pi ps
			= visit_children children (min min_dep mark) pi ps
			= visit_children children (min min_dep mark) pi ps
	visit_children [] min_dep nr_of_nodes ps
		= (min_dep, ps)
		

	try_to_close_group :: !Int !Int !Int !DAG !*PartitioningState -> (!Int, !*PartitioningState)
	try_to_close_group node_index next_num min_dep pi ps=:{ps_marks, ps_deps, ps_groups}
		| next_num <= min_dep
			# (ps_deps, ps_marks, group)
				= close_group node_index ps_deps ps_marks [] pi
			  ps = { ps & ps_deps = ps_deps, ps_marks = ps_marks, ps_groups = [group : ps_groups] }
			= (pi.dag_nr_of_nodes, ps)
			= (min_dep, ps)

	close_group :: !Int ![Int] !*{# Int} ![Int] !DAG -> (![Int], !*{# Int}, ![Int])
	close_group node_index [d:ds] marks group pi
		# marks = { marks & [d] = pi.dag_nr_of_nodes }
		| d == node_index
			= (ds, marks, [d : group])
			= close_group node_index ds marks [d : group] pi
