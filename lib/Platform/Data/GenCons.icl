implementation module Data.GenCons

import StdEnv, StdGeneric, Control.GenBimap, _SystemStrictLists
import Data.Functor
import Data.List
import Data.Maybe

consByName :: String -> Maybe a | conses{|*|}, consName{|*|} a
consByName a = let cs = conses{|*|}
	in ((!!) cs) <$> elemIndex a (map consName{|*|} cs)

generic consName a :: a -> String
consName{|CONS of {gcd_name}|} f x = gcd_name
consName{|UNIT|} _ = "UNIT"
consName{|PAIR|} f g (PAIR x y) = f x
consName{|EITHER|} f g (LEFT x) = f x
consName{|EITHER|} f g (RIGHT y) = g y
consName{|OBJECT|} f (OBJECT x) = f x
consName{|RECORD|} f (RECORD x) = f x
consName{|FIELD|} f (FIELD x) = f x
consName{|Int|} i = "INT"
consName{|Bool|} b = "BOOL"
consName{|Char|} c = "CHAR"
consName{|Real|} r = "REAL"
consName{|String|} s = "STRING"
consName{|File|} s = "FILE"
consName{|{}|} _ _ = "ARRAY"
consName{|{!}|} _ _ = "ARRAY"
consName{|(->)|} _ _ _ = "->"
derive consName (),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),[],[! ],[ !],[!!]

generic consIndex a :: a -> Int
consIndex{|CONS of {gcd_index}|} f x = gcd_index
consIndex{|UNIT|} _ = 0
consIndex{|PAIR|} f g (PAIR x y) = f x
consIndex{|EITHER|} f g (LEFT x) = f x
consIndex{|EITHER|} f g (RIGHT y) = g y
consIndex{|OBJECT|} f (OBJECT x) = f x
consIndex{|RECORD|} f (RECORD x) = f x
consIndex{|FIELD|} f (FIELD x) = f x
consIndex{|Int|} i = 0
consIndex{|Bool|} b = 0
consIndex{|Char|} c = 0
consIndex{|Real|} r = 0
consIndex{|String|} _ = 0
consIndex{|File|} _ = 0
consIndex{|[]|} _ _ = 0
consIndex{|[!]|} _ _ = 0
consIndex{|[ !]|} _ _ = 0
consIndex{|[!!]|} _ _ = 0
consIndex{|{}|} _ _ = 0
consIndex{|{!}|} _ _ = 0
consIndex{|(->)|} _ _ _ = 0
consIndex{|()|} _ = 0
consIndex{|(,)|} _ _ _ = 0
consIndex{|(,,)|}  _ _ _ _ = 0
consIndex{|(,,,)|}  _ _ _ _ _ = 0
consIndex{|(,,,,)|}  _ _ _ _ _ _ = 0
consIndex{|(,,,,,)|}  _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,)|}  _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,)|}  _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0
consIndex{|(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)|}  _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = 0

generic conses a :: [a]
conses{|CONS|} f = [CONS (hd f)]
conses{|UNIT|} = [UNIT]
conses{|PAIR|} f g = [PAIR x y \\ x <- f, y <- g] 
conses{|EITHER|} f g = map LEFT f ++ map RIGHT g
conses{|OBJECT|} f = map OBJECT f
conses{|RECORD|} f = map RECORD f
conses{|FIELD|} f = map FIELD f
conses{|Int|} = [0]
conses{|Bool|} = [True]
conses{|Char|} = ['\0']
conses{|Real|} = [0.0]
conses{|String|} = [""]
conses{|{}|} _ = [{}]
conses{|{!}|} _ = [{!}]
conses{|(->)|} _ _ = [const undef]
derive conses [],[!],[ !],[!!],(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)

generic consNum a :: a -> Int
consNum{|CONS|} _ _ = consNumError
consNum{|UNIT|} _ = consNumError
consNum{|PAIR|} _ _ _ = consNumError
consNum{|EITHER|} _ _ _ = consNumError
consNum{|OBJECT of {gtd_num_conses}|} f _ = gtd_num_conses
consNum{|RECORD|} _ _ = 1
consNum{|FIELD|} _ _ = consNumError
consNum{|Int|} _ = 1
consNum{|Bool|} _ = 1
consNum{|Char|} _ = 1
consNum{|Real|} _ = 1
consNum{|String|} _ = 1
consNum{|File|} _ = 1
consNum{|{}|} _ _ = 1
consNum{|{!}|} _ _ = 1
consNum{|(->)|} _ _ _ = 1
consNumError = abort "Impossible case in Data.GenCons.consNum\n"
derive consNum [],[!],[ !],[!!],(),(,),(,,),(,,,),(,,,,),(,,,,,),(,,,,,,),(,,,,,,,),(,,,,,,,,),(,,,,,,,,,),(,,,,,,,,,,),(,,,,,,,,,,,),(,,,,,,,,,,,,),(,,,,,,,,,,,,,),(,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,),(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,)
