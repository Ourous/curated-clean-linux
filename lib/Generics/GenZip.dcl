definition module GenZip

import StdMaybe, StdGeneric

generic gZip a b c :: .a .b -> .c
derive gZip Int, Bool, Char, Real, String, UNIT, EITHER, PAIR, CONS, FIELD, OBJECT
derive gZip [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gMaybeZip a b c :: .a .b -> Maybe .c
derive gMaybeZip Int, Char, Bool, Real, String, UNIT, EITHER, PAIR, CONS, FIELD, OBJECT
derive gMaybeZip [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
