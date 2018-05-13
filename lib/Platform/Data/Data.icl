implementation module Data.Data

import StdOverloaded, StdCleanTypes, StdString

// Returns True if the two arguments are of the same type and have the same
// constructor. Constructor arguments are ignored. E.g.:
//
// Left True =+?= Left False == True
//
(=+?=) infix 6 :: a a -> Bool
(=+?=) l r = toString (CTToCons l) == toString (CTToCons r)

