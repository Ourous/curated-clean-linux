implementation module Data.Generics

import StdGeneric

fromOBJECT :: !(OBJECT x) -> x
fromOBJECT (OBJECT x) = x

fromCONS :: !(CONS x) -> x
fromCONS (CONS x) = x

fromRECORD :: !(RECORD x) -> x
fromRECORD (RECORD x) = x

fromFIELD :: !(FIELD x) -> x
fromFIELD (FIELD x) = x

fromPAIRX :: !(PAIR x y) -> x
fromPAIRX (PAIR x _) = x

fromPAIRY :: !(PAIR x y) -> y
fromPAIRY (PAIR _ y) = y
