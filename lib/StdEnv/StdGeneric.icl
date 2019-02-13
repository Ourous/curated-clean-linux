implementation module StdGeneric

import StdInt, StdMisc, StdClass, StdFunc

generic bimap a b | bimap b a :: .a ->.b

bimap{|c|} x = x

bimap{|PAIR|} fx _ fy _ (PAIR x y) = PAIR (fx x) (fy y)

bimap{|EITHER|} fl _ fr _ (LEFT x) 	= LEFT (fl x)
bimap{|EITHER|} fl _ fr _ (RIGHT x)	= RIGHT (fr x)

bimap{|CONS|} fx _ (CONS x) = CONS (fx x)

bimap{|RECORD|} fx _ (RECORD x) = RECORD (fx x)

bimap{|FIELD|} fx _ (FIELD x) = FIELD (fx x)

bimap{|OBJECT|} fx _ (OBJECT x) = OBJECT (fx x)

bimap{|(->)|} _ ba fr _ f = comp3 fr f ba

comp3 :: !(.a -> .b) u:(.c -> .a) !(.d -> .c) -> u:(.d -> .b)
comp3 f g h
	| is_id f
		| is_id h
			= cast g
			= cast (\x -> g (h x))
		| is_id h
			= cast (\x -> f (g x))
			= \x -> f (g (h x))
where
	is_id :: !.(.a -> .b) -> Bool
	is_id f = code inline
	{
		eq_desc e_StdFunctions_did 0 0
		pop_a 1
	}
	
	cast :: !u:a -> u:b
	cast f = code inline
	{
		pop_a 0
	}

getConsPath :: !GenericConsDescriptor -> [ConsPos]
getConsPath {gcd_index, gcd_type_def={gtd_num_conses}}
	| gtd_num_conses==0
		= [] // for newtype
		= doit gcd_index gtd_num_conses
where
	doit i n
		| i >= n	
			= abort "getConsPath: cons index >= number of conses"
		| n == 1
			= []
		| i < (n>>1)
			= [ ConsLeft : doit i (n>>1) ]
		| otherwise
			= [ ConsRight : doit (i - (n>>1)) (n - (n>>1)) ]

bimapId :: Bimap .a .a	// deprecated, no longer used
bimapId = { map_to = id, map_from = id }
