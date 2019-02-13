definition module general

from StdFile import instance <<< Int,class <<< (..)
from StdInt import instance + Int,class + (..),instance ~ Int,class ~ (..)

instance ~ Bool

instance <<< Bool
instance <<< (a,b) | <<< a & <<< b
instance <<< (a,b,c) | <<< a & <<< b & <<< c
instance <<< (a,b,c,d) | <<< a & <<< b & <<< c & <<< d
instance <<< (a,b,c,d,e) | <<< a & <<< b & <<< c & <<< d & <<< e
instance <<< (a,b,c,d,e,f) | <<< a & <<< b & <<< c & <<< d & <<< e & <<< f
instance <<< (a,b,c,d,e,f,g) | <<< a & <<< b & <<< c & <<< d & <<< e & <<< f & <<< g
instance <<< [a] | <<< a

::	Bind a b =
	{	bind_src :: !a
	,	bind_dst :: !b
	}	

::	Env a b :== [.Bind a b]

::	Optional x = Yes !x | No

hasOption :: (Optional x) -> Bool

(--->) infix :: .a !b -> .a | <<< b
(<---) infix :: !.a !b -> .a | <<< b
traceValue :: !String !String .a -> .a

(-?->) infix :: .a !(!Bool, !b) -> .a | <<< b

instance + {#Char}

cMAXINT :== 2147483647

::	BITVECT :== Int

