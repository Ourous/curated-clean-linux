definition module Data.Array

from StdArray import class Array
from StdOverloaded import class +++

mapArrSt :: !(.a -> .(*st -> *(!.a, !*st))) !*(arr .a) !*st -> *(!*(arr .a), !*st) | Array arr a

foldrArr :: !(a .b -> .b) !.b !.(arr a) -> .b | Array arr a

foldrArrWithKey :: !(Int a -> .(.b -> .b)) !.b !.(arr a) -> .b | Array arr a

foldrUArr :: !(a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
          -> *(.b, *(arr a)) | Array arr a

foldrUArrWithKey :: !(Int a -> .(.b -> .(*(arr a) -> *(.b, *(arr a))))) .b *(arr a)
                 -> *(.b, *(arr a)) | Array arr a

foldlArr :: !(.b a -> .b) !.b !.(arr a) -> .b | Array arr a

foldlArrWithKey :: !(Int .b -> .(a -> .b)) !.b !.(arr a) -> .b | Array arr a

reverseArr :: !.(arr a) -> .arr a | Array arr a

takeArr :: !Int !.(arr a) -> .arr a | Array arr a

mapArr :: !(a -> a) !(arr a) -> arr a | Array arr a

appendArr :: !(arr a) !(arr a) -> arr a | Array arr a

instance +++ (arr a) | Array arr a
