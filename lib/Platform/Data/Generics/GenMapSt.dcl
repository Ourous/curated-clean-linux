definition module Data.Generics.GenMapSt

import StdGeneric

generic gMapLSt a b :: .a .st -> (.b, .st)
derive gMapLSt c, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, {}, {!}
derive gMapLSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gMapRSt a b :: .a .st -> (.b, .st)
derive gMapRSt c, UNIT, PAIR, EITHER, FIELD, CONS, OBJECT, {}, {!} 
derive gMapRSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
