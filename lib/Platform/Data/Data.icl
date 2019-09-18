implementation module Data.Data

import StdOverloaded, StdCleanTypes, StdString

// Returns True if the two arguments are of the same type and have the same
// constructor. Constructor arguments are ignored. E.g.:
//
// Left True =+?= Left False == True
//
// TODO: this can probably be: pushD_a 0; pushD_a 1; pop_a 2; eqI
(=+?=) infix 6 :: !a !a -> Bool
(=+?=) l r = toString (CTToCons l) == toString (CTToCons r)
