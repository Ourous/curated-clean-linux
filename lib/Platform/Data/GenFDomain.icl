implementation module Data.GenFDomain

import StdChar, StdEnum, StdInt, StdList
import StdGeneric
import Data.List
import Control.GenBimap

generic gFDomain a :: [a]
gFDomain{|Bool|}         = [False,True]
gFDomain{|Char|}         = map toChar [0..255]
gFDomain{|UNIT|}         = [UNIT]
gFDomain{|PAIR|}   dx dy = [PAIR x y\\x <- dx, y <- dy]
gFDomain{|EITHER|} dx dy = map LEFT dx ++ map RIGHT dy
gFDomain{|CONS|}   dx    = map CONS   dx
gFDomain{|FIELD|}  dx    = map (\x -> FIELD x) dx
gFDomain{|OBJECT|} dx    = map (\x -> OBJECT x) dx

derive gFDomain (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
