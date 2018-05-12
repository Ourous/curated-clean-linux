implementation module Data.Eq

import StdOverloaded, StdBool

(/=) infix 4 :: !a !a -> Bool | == a
(/=) x y = not (x == y)

