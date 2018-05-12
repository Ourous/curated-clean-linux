definition module Data.Generics.GenFDomain

import StdGeneric

// gFDomain generates *all* values of a finite domain
generic gFDomain a :: [a]
derive gFDomain Bool, Char, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT

derive gFDomain (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
