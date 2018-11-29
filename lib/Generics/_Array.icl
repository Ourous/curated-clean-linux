implementation module _Array

import _SystemArray, StdInt, StdClass


createArrayUnsafe :: .Int -> u:(a v:b) | Array a b, [u <= v]
createArrayUnsafe n = _createArray n


instance UnsafeArray {} where 
	unsafeCreate size =
		code
		{	
			create_array_ _ 1 0
		}
	unsafeUselect arr index =
		code
		{
			push_a 0
			select _ 1 0
		}

instance UnsafeArray {!} where 
	unsafeCreate size =
		code
		{	
			create_array_ _ 1 0
		}
	unsafeUselect arr index =
		code
		{
			push_a 0
			select _ 1 0
		}

//mapArray :: (u:a -> v:b) w:(c u:a) -> x:(d v:b) | UnsafeArray c a & UnsafeArray d b, [w <= u,x <= v]
mapArray :: (u:a -> v:b) w:(c u:a) -> x:(d v:b) | Array d b & UnsafeArray c & UnsafeArray d & Array c a, [w <= u,x <= v]
mapArray f xs
	#! (size_xs, xs) = usize xs
	#! (xs, ys) = map f 0 size_xs xs (unsafeCreate size_xs)
	= ys
where
	map f i n xs ys
		| i == n	
			= (xs, ys)
		| otherwise
			#! (x, xs) = unsafeUselect xs i
			#! ys = update ys i (f x) 
			= map f (inc i) n xs ys 

//mapArrayLSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | UnsafeArray d a & UnsafeArray e c, [w <= u,x <= v]
mapArrayLSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | Array e c & UnsafeArray d & UnsafeArray e & Array d a, [w <= u,x <= v]
mapArrayLSt f xs st
	#! (size_xs, xs) = usize xs
	#! (xs, ys, st) = map f 0 size_xs xs (unsafeCreate size_xs) st
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

//mapArrayRSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | UnsafeArray d a & UnsafeArray e c, [w <= u,x <= v]
mapArrayRSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | Array e c & UnsafeArray d & UnsafeArray e & Array d a, [w <= u,x <= v]
mapArrayRSt f xs st
	#! (size_xs, xs) = usize xs
	#! (xs, ys, st) = map f (size_xs - 1) xs (unsafeCreate size_xs) st
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

reduceArray :: ((.a -> u:(b -> b)) -> .(b -> .(c -> .a))) (.a -> u:(b -> b)) b .(d c) -> b | Array d c
reduceArray f op e xs 
	= reduce f 0 (size xs) op e xs
where
	reduce f i n op e xs
		| i == n 
			= e
		| otherwise
			= op (f op e xs.[i]) (reduce f (inc i) n op e xs) 

reduceArrayLSt :: (u:a -> .(.b -> .b)) v:(c u:a) .b -> .b | UnsafeArray c & Array c a, [v <= u]
reduceArrayLSt f xs st
	#! (size_xs, xs) = usize xs
	#! (xs, st) = reduce f 0 size_xs xs st
	= st
where
	reduce f i n xs st
		| i == n	
			= (xs, st)
		| otherwise
			#! (x, xs) = unsafeUselect xs i
			= reduce f (inc i) n xs (f x st)

reduceArrayRSt :: (u:a -> .(.b -> .b)) v:(c u:a) .b -> .b | UnsafeArray c & Array c a, [v <= u]
reduceArrayRSt f xs st
	#! (size_xs, xs) = usize xs
	#! (xs, st) = reduce f (dec size_xs) xs st
	= st
where
	reduce f i xs st
		| i < 0
			= (xs, st)
		| otherwise
			#! (x, xs) = unsafeUselect xs i
			= reduce f (dec i) xs (f x st)
