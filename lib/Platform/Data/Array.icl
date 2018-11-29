implementation module Data.Array

import StdArray, StdInt, StdOverloaded, StdClass, StdFunctions
import Data.Functor, Control.Applicative, Control.Monad

mapArrSt :: !(.a -> .(*st -> *(!.a, !*st))) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a
mapArrSt f arr st
  #! (sz, arr) = usize arr
  = mapArrSt` sz 0 f arr st
  where
  mapArrSt` :: !Int !Int !(.a -> .(*st -> *(!.a, !*st))) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a
  mapArrSt` sz idx f arr st
    | idx == sz = (arr, st)
    | otherwise
        #! (e, arr) = arr![idx]
        #! (e, st)  = f e st
        #! arr      = {arr & [idx] = e}
        = mapArrSt` sz (idx + 1) f arr st

foldrArr :: !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a
foldrArr f b arr = foldrArrWithKey (\_ -> f) b arr

foldrArrWithKey :: !(Int a -> .(.b -> .b)) !.b !.(arr a) -> .b | Array arr a
foldrArrWithKey f b arr
  #! (arrSz, arr) = usize arr
  = foldrArr` arrSz 0 f b arr
  where
  foldrArr` :: !Int !Int !(Int a -> .(.b -> .b)) !.b !.(arr a) -> .b | Array arr a
  foldrArr` arrSz idx f b arr
    | idx == arrSz = b
    | otherwise
        #! (e, arr) = arr![idx]
        = f idx e (foldrArr` arrSz (idx + 1) f b arr)

foldrUArr :: !(a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
          -> *(.b, *(arr a)) | Array arr a
foldrUArr f b arr = foldrUArrWithKey (\_ -> f) b arr

foldrUArrWithKey :: !(Int a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
                 -> *(.b, *(arr a)) | Array arr a
foldrUArrWithKey f b arr
  # (sz, arr) = usize arr
  = foldUArr` sz 0 b arr
  where
  foldUArr` sz idx b arr
    | idx == sz = (b, arr)
    | otherwise
      #! (elem, arr) = uselect arr idx
      #! (res, arr)  = foldUArr` sz (idx + 1) b arr
      = f idx elem res arr

foldlArr :: !(.b a -> .b) !.b !.(arr a) -> .b | Array arr a
foldlArr f b arr = foldlArrWithKey (\_ -> f) b arr

foldlArrWithKey :: !(Int .b -> .(a -> .b)) !.b !.(arr a) -> .b | Array arr a
foldlArrWithKey f b arr
  #! (arrSz, arr) = usize arr
  = foldlArr` arrSz 0 f b arr
  where
  foldlArr` :: !Int !Int !(Int .b -> .(a -> .b)) !.b !.(arr a) -> .b | Array arr a
  foldlArr` arrSz idx f b arr
    | idx == arrSz = b
    | otherwise
        #! (e, arr) = arr![idx]
        #! b` = f idx b e
        = foldlArr` arrSz (idx + 1) f b` arr

reverseArr :: !.(arr a) -> .arr a | Array arr a
reverseArr arr
  #! sz = size arr
  = foldlArrWithKey (\idx acc e -> {acc & [sz - idx - 1] = e}) {x \\ x <-: arr} arr

takeArr :: !Int !.(arr a) -> .arr a | Array arr a
takeArr n arr
  | size arr > 0
    #! newArr = createArray n arr.[0]
    = copyArr n 0 arr newArr
  | otherwise = {x \\ x <-: arr}
  where
  copyArr sz i origArr newArr
    | i == sz = newArr
    | otherwise   = copyArr sz (i + 1) origArr {newArr & [i] = origArr.[i]}

mapArr :: !(a -> a) !(arr a) -> arr a | Array arr a
mapArr f arr
  #! arr = {a \\ a <-: arr}
  #! (sz, arr) = usize arr
  = mapArrSt` sz 0 f arr
  where
  mapArrSt` :: !Int !Int !(.a -> .a) !*(arr .a) -> *arr .a | Array arr a
  mapArrSt` sz idx f arr
    | idx == sz = arr
    | otherwise
        #! (e, arr) = arr![idx]
        #! e        = f e
        #! arr      = {arr & [idx] = e}
        = mapArrSt` sz (idx + 1) f arr

appendArr :: !(arr a) !(arr a) -> arr a | Array arr a
appendArr l r
  #! szl     = size l
  #! szr     = size r
  #! totalSz = szl + szr
  | totalSz < 1 = l
  | otherwise
    #! el     = if (szl > 0) l.[0] r.[0]
    #! newArr = createArray totalSz el
    #! newArr = addWithOffset totalSz 0 l newArr
    #! newArr = addWithOffset totalSz (szl - 1) r newArr
    = newArr
  where
  addWithOffset totalSz offset oldArr newArr
    = foldrArrWithKey (\idx oldEl newArr -> {newArr & [idx + offset] = oldEl}) newArr oldArr

instance +++ (arr a) | Array arr a where
  (+++) l r = appendArr l r

instance Functor {} where fmap f arr = {f a\\a<-:arr}
instance Functor {!} where fmap f arr = {f a\\a<-:arr}

instance pure {}
where
	pure x = {x}

instance <*> {}
where
	(<*>) fs xs = {f x\\f<-:fs, x<-:xs}

instance pure {!}
where
	pure x = {!x}

instance <*> {!}
where
	(<*>) fs xs = {!f x\\f<-:fs, x<-:xs}

instance Monad {} where bind m k = foldrArr ((+++) o k) {} m
instance Monad {!} where bind m k = foldrArr ((+++) o k) {} m

reduceArray :: ((.a -> u:(b -> b)) -> .(b -> .(c -> .a))) (.a -> u:(b -> b)) b .(d c) -> b | Array d c
reduceArray f op e xs 
	= reduce f 0 (size xs) op e xs
where
		reduce f i n op e xs
		| i == n 
			= e
		| otherwise
			= op (f op e xs.[i]) (reduce f (inc i) n op e xs)
