implementation module GenCompress

import StdGeneric, StdEnv, StdMaybe, _Array

//--------------------------------------------------
// uncompressor monad

ret :: !.a !u:CompressSt -> (!Maybe .a,!u:CompressSt)
ret a st = (Just a, st)
(>>=) infixl 5 
(>>=) pa pb = bind pa pb
where
	bind pa pb st 
		#! (ma, st) = pa st
		= case ma of
			Nothing -> (Nothing, st)
			Just x  -> pb x st

//--------------------------------------------------

:: BitVector :== {#Int}
:: BitPos :== Int

:: CompressSt = { cs_pos :: !Int, cs_bits :: !.{#Int} }
mkCompressSt arr = { cs_pos = 0, cs_bits = arr}


:: Compress a :== a -> *CompressSt -> *CompressSt
:: Uncompress a :== .CompressSt -> .(.(Maybe a), .CompressSt)

compressBool :: !Bool !*CompressSt -> *CompressSt
compressBool bit {cs_pos = pos, cs_bits = bits}
	#! s = size bits
	#! int_pos = pos >> (IF_INT_64_OR_32 6 5)
	#! bit_pos = pos bitand (IF_INT_64_OR_32 63 31)
	| s == int_pos
		= abort "reallocate" 
		#! int = bits.[int_pos]
		#! bit_mask = 1 << bit_pos
		#! new_int = if bit (int bitor bit_mask) (int bitand (bitnot bit_mask))
		= {cs_pos = inc pos, cs_bits = {bits & [int_pos] = new_int}}

uncompressBool :: !u:CompressSt -> (.(Maybe Bool),v:CompressSt), [u <= v]
uncompressBool cs=:{cs_pos = pos, cs_bits = bits}
	#! s = size bits
	#! int_pos = pos >> (IF_INT_64_OR_32 6 5)
	#! bit_pos = pos bitand (IF_INT_64_OR_32 63 31)
	| s == int_pos
		= (Nothing, cs) 
		#! int = bits.[int_pos]
		#! bit_mask = 1 << bit_pos
		#! bit = (bit_mask bitand int) <> 0
		= (Just bit, {cs & cs_pos = inc pos})

compressIntB :: !.Int !.Int -> .(*CompressSt -> .CompressSt)
compressIntB num_bits int
	= compress 0 num_bits int
where
	compress i n int
		| i == n
			= id
		| otherwise
			= compress (inc i) n (int >> 1) 
			o compressBool ((int bitand 1) == 1)


compressInt = compressIntB (IF_INT_64_OR_32 64 32)
compressChar c = compressIntB 8 (toInt c)

uncompressIntB :: !.Int -> u:CompressSt -> (.(Maybe Int),v:CompressSt), [u <= v]
uncompressIntB num_bits
	= uncompress 0 num_bits 0
where
	uncompress i n int
		| i == n
			= ret int
		| otherwise
			=   	uncompressBool
			>>= 	\bit -> uncompress (inc i) n int 
			>>= 	\x -> ret ((if bit 1 0) + (x << 1))

uncompressInt :: (u:CompressSt -> (.(Maybe Int),v:CompressSt)), [u <= v]
uncompressInt = uncompressIntB (IF_INT_64_OR_32 64 32)

uncompressChar :: (u:CompressSt -> (.(Maybe Char),v:CompressSt)), [u <= v]
uncompressChar = uncompressIntB 8 >>= ret o toChar 

realToBinary32 :: !Real -> (!Int,!Int);
realToBinary32 _ = code {
    pop_b 0
    };

realToBinary64 :: !Real -> Int;
realToBinary64 _ = code {
    pop_b 0
    };

binaryToReal32 :: !(!Int,!Int) -> Real;
binaryToReal32 _ = code {
    pop_b 0
    };

binaryToReal64 :: !Int -> Real;
binaryToReal64 _ = code {
    pop_b 0
    };

compressReal real
	= IF_INT_64_OR_32
		(compressInt (realToBinary64 real))
		(let (i1, i2) = realToBinary32 real in compressInt i2 o compressInt i1)

uncompressReal :: (u:CompressSt -> (.(Maybe Real),v:CompressSt)), [u <= v]
uncompressReal
	= IF_INT_64_OR_32
		(uncompressInt
		>>= \i -> ret (binaryToReal64 i))
		(uncompressInt 
		>>= \i1 -> uncompressInt 
		>>= \i2 -> ret (binaryToReal32 (i1,i2)))

compressArray :: (a -> u:(v:CompressSt -> w:CompressSt)) !.(b a) -> x:(*CompressSt -> y:CompressSt) | Array b a, [x <= u,w <= v,w <= y]
compressArray f xs 
	= foldSt f [x \\ x <-: xs] o compressInt (size xs)

foldSt f [] = id
foldSt f [x:xs] = foldSt f xs o f x

uncompressArray :: (u:CompressSt -> ((Maybe v:a),w:CompressSt)) -> .(x:CompressSt -> ((Maybe y:(b v:a)),z:CompressSt)) | Array b a, [x w <= u,y <= v,x w <= z]
uncompressArray f 
	=	uncompressInt >>= \s -> uncompress_array 0 s (createArrayUnsafe s) 
where 
	uncompress_array i s arr
		| i == s
			= ret arr
			= f >>= \x -> uncompress_array (inc i) s {arr & [i] = x} 

compressList :: (a *CompressSt -> *CompressSt) ![a] -> *CompressSt -> *CompressSt
compressList c xs = compressArray c (list_to_arr xs)
where
	list_to_arr :: [b] -> {b} | Array {} b
	list_to_arr xs = {x \\ x <- xs}


uncompressList xs = uncompressArray xs >>= ret o arr_to_list
where
	arr_to_list :: {b} -> [b] | Array {} b
	arr_to_list xs = [x \\ x <-: xs] 
 
//--------------------------------------------------------------------------------------

generic gCompress a :: !a -> *CompressSt -> *CompressSt
gCompress{|Int|} x = compressInt x 
gCompress{|Real|} x = compressReal x 
gCompress{|Char|} x = compressChar x
gCompress{|Bool|} x = compressBool x
gCompress{|UNIT|} x = id
gCompress{|PAIR|} cx cy (PAIR x y) = cy y o cx x
gCompress{|EITHER|} cl cr (LEFT x) = cl x o compressBool False
gCompress{|EITHER|} cl cr (RIGHT x) = cr x o compressBool True
gCompress{|CONS|} c (CONS x) = c x
gCompress{|FIELD|} c (FIELD x) = c x
gCompress{|OBJECT|} c (OBJECT x) = c x
gCompress{|{}|} c xs = compressArray c xs
gCompress{|{!}|} c xs = compressArray c xs
gCompress{|String|} xs = compressArray compressChar xs
gCompress{|[]|} c xs = compressList c xs


generic gCompressedSize a :: a -> Int
gCompressedSize{|Int|} _ = IF_INT_64_OR_32 64 32
gCompressedSize{|Real|} _ = 64
gCompressedSize{|Char|} _ = 8
gCompressedSize{|Bool|} _ = 1
gCompressedSize{|UNIT|} _ = 0
gCompressedSize{|PAIR|} cx cy (PAIR x y) = cx x + cy y
gCompressedSize{|EITHER|} cl cr (LEFT x) = 1 + cl x
gCompressedSize{|EITHER|} cl cr (RIGHT x) = 1 + cr x
gCompressedSize{|CONS|} c (CONS x) = c x
gCompressedSize{|FIELD|} c (FIELD x) = c x
gCompressedSize{|OBJECT|} c (OBJECT x) = c x
gCompressedSize{|[]|} c xs = foldSt (\x st -> c x + st) xs (IF_INT_64_OR_32 64 32) 
gCompressedSize{|{}|} c xs = foldSt (\x st -> c x + st) [x\\x<-:xs] (IF_INT_64_OR_32 64 32) 
gCompressedSize{|{!}|} c xs = foldSt (\x st -> c x + st) [x\\x<-:xs] (IF_INT_64_OR_32 64 32) 
gCompressedSize{|String|} xs = (IF_INT_64_OR_32 64 32) + size xs * 8

generic gUncompress a :: (u:CompressSt -> ((Maybe a),u:CompressSt))
gUncompress{|Int|} = uncompressInt
gUncompress{|Real|} = uncompressReal
gUncompress{|Char|} = uncompressChar
gUncompress{|Bool|} = uncompressBool
gUncompress{|UNIT|} = ret UNIT
gUncompress{|PAIR|} fx fy = fx >>= \x -> fy >>= \y -> ret (PAIR x y)
gUncompress{|EITHER|} fl fr = uncompressBool >>= either
where
	either is_right 
		| is_right
			= fr >>= ret o RIGHT
			= fl >>= ret o LEFT
gUncompress{|CONS|} f = f >>= ret o CONS
gUncompress{|FIELD|} f = f >>= ret o FIELD
gUncompress{|OBJECT|} f = f >>= ret o OBJECT
gUncompress{|[]|} f = uncompressList f 
gUncompress{|{}|} f = uncompressArray f 
gUncompress{|{!}|} f = uncompressArray f 
gUncompress{|String|} = uncompressArray uncompressChar 


//-------------------------------------------------------------------------------------

uncompress :: (BitVector -> Maybe a) | gUncompress{|*|} a
uncompress = fst o gUncompress{|*|} o mkCompressSt

compress :: !a -> BitVector | gCompressedSize{|*|} a & gCompress{|*|} a
compress x 
	#! compressed_size = gCompressedSize{|*|} x
	#! arr_size = (compressed_size + (IF_INT_64_OR_32 63 31)) >> (IF_INT_64_OR_32 6 5)
	#! bits = createArray arr_size 0
	= (gCompress{|*|} x (mkCompressSt bits)).cs_bits
 
//-------------------------------------------------------------------------------------

/*
:: Tree a b = Tip a | Bin b (Tree a b) (Tree a b)
:: Color = Red | Green | Blue

derive bimap (,), (,,), Maybe
derive gCompress Tree, Color
derive gUncompress Tree, Color
derive gCompressedSize Tree, Color
		
//Start :: Maybe (Tree Color Color)
//Start = uncompress (compress (Bin Red (Bin Green (Tip Blue) (Tip Red)) (Tip Green)))
//Start = gCompressedSize{|*|} (Bin Red (Bin Green (Tip Blue) (Tip Red)) (Tip Green))

Start 
	= gCompressedSize{|*|} xs
*/