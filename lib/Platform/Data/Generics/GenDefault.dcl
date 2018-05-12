definition module Data.Generics.GenDefault

import StdGeneric

generic gDefault a :: a

derive gDefault Int, Real, String, UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, RECORD
derive gDefault [], [!], [ !], [!!], {}, {!}
derive gDefault (), (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
