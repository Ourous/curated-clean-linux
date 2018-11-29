definition module GenFMap

import StdGeneric, StdMaybe

:: FMap v
derive bimap FMap

emptyFMap :: .FMap .v

generic gLookupFMap key :: key (FMap value) -> FMap value
derive gLookupFMap UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, Char, Int, Bool, Real, String, [], {}, {!}
derive gLookupFMap (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

generic gInsertFMap key :: key (FMap value, FMap value) -> (FMap value, FMap value)
derive gInsertFMap UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, Char, Int, Bool, Real, String, [], {}, {!}
derive gInsertFMap (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

lookupFMap :: !k .(FMap v) -> .(Maybe v) | gLookupFMap{|*|} k & bimap{|*|} v
(<<=) infixl 1 :: .(FMap v) !.(k,v) -> FMap v | gInsertFMap{|*|} k & bimap{|*|} v
