implementation module StdListExtensions

//	********************************************************************************
//	Clean StdLib library module, version 1.0
//	********************************************************************************

import StdEnv, StdMaybe

//maplSt :: (.a -> .(.st -> (.b,.st))) [.a] .st -> (.[.b],.st)
maplSt f l st
	:== mapl_st l st
  where
	mapl_st [] st =  ([], st)
	mapl_st [x:xs] st
		#! (y, st) = f x st
		   (ys, st) = mapl_st xs st
		=  ([y:ys], st)
                                 
//maprSt :: (.a -> .(.st -> (.b,.st))) [.a] .st -> (.[.b],.st)
maprSt f l st
	:== mapr_st l st
  where
	mapr_st [] st =  ([], st)
	mapr_st [x:xs] st
		#! (ys, st) = mapr_st xs st
		   (y, st) = f x st
		=  ([y:ys], st)

// foldlSt :: !(.a -> .(.st -> .st)) ![.a] !.st -> .st
foldlSt op l st :== foldl_st l st
	where
		foldl_st [] st		= st
		foldl_st [a:as] st	= foldl_st as (op a st)

// foldrSt :: !(.a -> .(.st -> .st)) ![.a] !.st -> .st
foldrSt op l st :== foldr_st l
	where
		foldr_st [] = st
		foldr_st [a:as]
			= op a (foldr_st as)

// incFoldSt :: (Int -> .(.b -> .b)) !Int !Int .b -> .b
incFoldSt op fr to st :== fold_st fr to st
	where
		fold_st fr to st
			| fr >= to
				= st
				= fold_st (inc fr) to (op fr st)

// decFoldSt :: (Int -> .(.b -> .b)) !Int !Int .b -> .b
decFoldSt op fr to st :== fold_st fr (dec to) st
	where
		fold_st fr to st
			| fr > to
				= st
				= fold_st fr (dec to) (op to st)

elemIndex :: a ![a] -> .Maybe Int | == a
elemIndex x l = listToMaybe (elemIndices x l)

elemIndices :: a ![a] -> .[Int] | == a
elemIndices x l = elem_indices 0 x l
  where
	elem_indices i x []
		= []
	elem_indices i x [y:ys]
		| x==y
			= [i : elem_indices (i+1) x ys]
		= elem_indices (i+1) x ys

find :: (a -> Bool) ![a] -> .Maybe a
find p l =  listToMaybe (filter p l)

findIndex :: (a -> Bool) ![a] -> .Maybe Int
findIndex p l = listToMaybe (findIndices p l)

findIndices :: (a -> Bool) ![a] -> .[Int]
findIndices p l = find_indices p 0 l
  where
	find_indices _ _ []
		= []
	find_indices p i [y:ys]
		| p y
			= [i : find_indices p (i+1) ys]
		= find_indices p (i+1) ys

intersperse :: a ![a] -> .[a]
intersperse sep []      =  []
intersperse sep [x]     =  [x]
intersperse sep [x:xs]  =  [x , sep : intersperse sep xs]

transpose :: ![[a]] -> [.[a]]
transpose []  = []
transpose [[]     : xss] = transpose xss
transpose [[x:xs] : xss] = [[x : [h \\ [h:t] <- xss]] : transpose [xs : [t \\ [h:t] <- xss]]]

partition :: (a -> Bool) !.[a] -> (.[a],.[a])
partition p []
	= ([], [])
partition p [x:xs]
	| p x
		= ([x:a], b)
	= (a, [x:b])
  where
	(a, b) = partition p xs

group :: ![a] -> [.[a]] | == a
group l                 =  groupBy (==) l

groupBy :: (a a -> Bool) ![a] -> [.[a]]
groupBy eq []           =  []
groupBy eq [x:xs]       =  [[x:ys] : groupBy eq zs]
                           where (ys,zs) = span (eq x) xs

inits :: ![a] -> [.[a]]
inits []                =  [[]]
inits [x:xs]            =  [[]:[[x:xs_init] \\ xs_init<-inits xs]]

tails :: ![a] -> .[[a]]
tails []                =  [[]]
tails xxs=:[_:xs]       =  [xxs : tails xs]

isPrefixOf :: ![a] ![a] -> Bool | == a
isPrefixOf [x:xs] [y:ys] =  x == y && isPrefixOf xs ys
isPrefixOf []     _      =  True
isPrefixOf _      []     =  False

isSuffixOf :: ![a] ![a] -> Bool | == a
isSuffixOf suffix x
	= let
		length_suffix = length suffix
		(n, lasts) = get_lasts length_suffix x
		// n == min length_suffix (length x)
	in if (n<length_suffix) False (lasts==suffix)
  where
	get_lasts :: !Int ![a] -> (!Int,![a])
	get_lasts length_suffix []
		= (0, [])
	get_lasts length_suffix x=:[h:t]
		= let
			(n, t`) = get_lasts length_suffix t
		  in if (n==length_suffix) (n, t`) (n+1, x)

zip3 :: ![.a] [.b] [.c] -> [(.a,.b,.c)]
zip3 [a:as] [b:bs] [c:cs]
	= [(a, b, c): zip3 as bs cs]
zip3 _ _ _
	= []

zip4 :: ![.a] [.b] [.c] [.d] -> [(.a,.b,.c,.d)]
zip4 [a:as] [b:bs] [c:cs] [d:ds]
	= [(a, b, c, d): zip4 as bs cs ds]
zip4 _ _ _ _
	= []

zip5 :: ![.a] [.b] [.c] [.d] [.e] -> [(.a,.b,.c,.d,.e)]
zip5 [a:as] [b:bs] [c:cs] [d:ds] [e:es]
	= [(a, b, c, d, e): zip5 as bs cs ds es]
zip5 _ _ _ _ _
	= []

zipWith :: (.a -> .(.b -> .h))
			![.a] [.b] -> [.h]
zipWith z [a:as] [b:bs]
                   = [ z a b : zipWith z as bs]
zipWith _ _ _ = []

zipWith3 :: (.a -> .(.b -> .(.c -> .h)))
			![.a] [.b] [.c] -> [.h]
zipWith3 z [a:as] [b:bs] [c:cs]
                   = [ z a b c : zipWith3 z as bs cs]
zipWith3 _ _ _ _ = []

zipWith4 :: (.a -> .(.b -> .(.c -> .(.d -> .h))))
			![.a] [.b] [.c] [.d] -> [.h]
zipWith4 z [a:as] [b:bs] [c:cs] [d:ds]
                   = [ z a b c d : zipWith4 z as bs cs ds]
zipWith4 _ _ _ _ _ = []

zipWith5 :: (.a -> .(.b -> .(.c -> .(.d -> .(.e -> .h)))))
			![.a] [.b] [.c] [.d] [.e] -> [.h]
zipWith5 z [a:as] [b:bs] [c:cs] [d:ds] [e:es]
                   = [ z a b c d e : zipWith5 z as bs cs ds es]
zipWith5 _ _ _ _ _ _ = []

unzip3 :: ![(.a,.b,.c)] -> ([.a],[.b],[.c])
unzip3 [] = ([],[],[])
unzip3 [(a, b, c): xs]
	= let (as, bs, cs) = unzip3 xs
	  in ([a:as], [b:bs], [c:cs])

unzip4 :: ![(.a,.b,.c,.d)] -> ([.a],[.b],[.c],[.d])
unzip4 [] = ([],[],[],[])
unzip4 [(a, b, c, d): xs]
	= let (as, bs, cs, ds) = unzip4 xs
	  in ([a:as], [b:bs], [c:cs], [d:ds])

unzip5 :: ![(.a,.b,.c,.d,.e)] -> ([.a],[.b],[.c],[.d],[.e])
unzip5 [] = ([],[],[],[],[])
unzip5 [(a, b, c, d, e): xs]
	= let (as, bs, cs, ds, es) = unzip5 xs
	  in ([a:as], [b:bs], [c:cs], [d:ds], [e:es])
