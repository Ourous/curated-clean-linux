definition module GenMapSt

import StdGeneric

generic gMapLSt a b :: .a .st -> (.b, .st)
derive gMapLSt c, PAIR, EITHER, FIELD, CONS, OBJECT, {}, {!}
derive gMapLSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gMapRSt a b :: .a .st -> (.b, .st)
derive gMapRSt c, PAIR, EITHER, FIELD, CONS, OBJECT, {}, {!} 
derive gMapRSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

