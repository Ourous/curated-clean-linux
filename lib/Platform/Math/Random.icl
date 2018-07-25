implementation module Math.Random

import StdArray, StdBool, StdEnum, StdInt, StdList, StdMisc, StdReal, StdString

n :== 624
m :== 397

a			:== 0x9908b0df
uppermask	:== 0x80000000
lowermask	:== 0x7fffffff

mask_b		:== 0x9d2c5680
mask_c		:== 0xefc60000 

u :== 11
s :== 7
t :== 15
l :== 18

shiftRight m n :== (m >> n) bitand (0x7FFFFFFF >> (n-1))

initrand :: Int *{# Int} -> *{# Int}
initrand seed mt 
	| seed==0 = abort "initrand: seed must not be 0\n"
	= init_i 0 { mt & [0] = seed bitand 0xffffffff}
where
	init_i i mt =: { [i] = mti}
		| ii==n = mt
		= init_i ii { mt & [ii] = (69069 * mti) bitand 0xffffffff}
	where
		ii = inc i
		
genRandInt :: !Int -> [Int]
genRandInt seed 
	#! first_mt = initrand seed (createArray n 0)
	= gr_mti 0 (new_mti 0 first_mt)
where
	gr_mti mti mt
		| mti==n = gr_mti 0 (new_mti 0 mt)
		#! (y, mt) = mt![mti]
		#! y = y bitxor (shiftRight y u)
		#! y = y bitxor ((y << s) bitand mask_b)
		#! y = y bitxor ((y << t) bitand mask_c)
		#! y = y bitxor (shiftRight y l)
		= [ y : gr_mti (inc mti) mt]
	
	new_mti :: Int *{# Int} -> *{# Int}
	new_mti k mt
		| k==n = mt 
		#! (mtk, mt) = mt![k]
		#! (mtkk, mt) = mt![(inc k) rem n] 
		#! y = (mtk bitand uppermask) bitor (mtkk bitand lowermask)
		#! (mtind, mt) = mt![ind k]
		#! mttmp = mtind bitxor (shiftRight y 1) bitxor ((y bitand 0x1) * a)
		= new_mti (inc k) { mt & [k] = mttmp}
	where
		ind :: Int -> Int
		ind k 
			| k<n-m = k+m
			= k+m-n

uint2Real i :== if (i >= 0) (toReal i) (4294967296.0 + toReal i)

genRandReal :: !Int -> [Real]
genRandReal seed = map (\y -> (uint2Real y) / 4294967295.0) (genRandInt seed)

