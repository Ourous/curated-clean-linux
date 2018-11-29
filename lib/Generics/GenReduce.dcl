definition module GenReduce

import StdGeneric

generic gReduce t :: (a a -> a) a  t -> a
derive gReduce c, PAIR, EITHER, CONS, FIELD, OBJECT
derive gReduce [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gReduceRSt t :: .t .st -> .st
derive gReduceRSt c, PAIR, EITHER, CONS, FIELD, OBJECT
derive gReduceRSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gReduceLSt t :: .t .st -> .st
derive gReduceLSt c, PAIR, EITHER, CONS, FIELD, OBJECT
derive gReduceLSt [], (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

 