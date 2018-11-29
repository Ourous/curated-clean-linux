definition module _Array

import StdArray

createArrayUnsafe :: .Int -> u:(a v:b) | Array a b, [u <= v]


/*
class UnsafeArray a e | Array a e where
	unsafeCreate				:: !Int				-> *(a .e)
	unsafeUselect				:: !u:(a .e) !Int	-> *(.e, !u:(a .e))

instance UnsafeArray {} e, {!} e

mapArray :: (u:a -> v:b) w:(c u:a) -> x:(d v:b) | UnsafeArray c a & UnsafeArray d b, [w <= u,x <= v]
mapArrayLSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | UnsafeArray d a & UnsafeArray e c, [w <= u,x <= v]
mapArrayRSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | UnsafeArray d a & UnsafeArray e c, [w <= u,x <= v]
*/

class UnsafeArray a where
	unsafeCreate				:: !Int				-> *(a .e)
	unsafeUselect				:: !u:(a .e) !Int	-> *(.e, !u:(a .e))

instance UnsafeArray {}, {!}

mapArray :: (u:a -> v:b) w:(c u:a) -> x:(d v:b) | Array d b & UnsafeArray c & UnsafeArray d & Array c a, [w <= u,x <= v]
mapArrayLSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | Array e c & UnsafeArray d & UnsafeArray e & Array d a, [w <= u,x <= v]
mapArrayRSt :: (u:a -> .(.b -> (v:c,.b))) w:(d u:a) .b -> (x:(e v:c),.b) | Array e c & UnsafeArray d & UnsafeArray e & Array d a, [w <= u,x <= v]
reduceArray :: ((.a -> u:(b -> b)) -> .(b -> .(c -> .a))) (.a -> u:(b -> b)) b .(d c) -> b | Array d c
reduceArrayLSt :: (u:a -> .(.b -> .b)) v:(c u:a) .b -> .b | UnsafeArray c & Array c a, [v <= u]
reduceArrayRSt :: (u:a -> .(.b -> .b)) v:(c u:a) .b -> .b | UnsafeArray c & Array c a, [v <= u]
