definition module Text.GenPrint

import StdGeneric

class PrintOutput s where
	printOutput :: !Char !*s -> *s
	
:: *StringOutput

:: PrintState s

mkPrintState :: !*s -> PrintState *s | PrintOutput s
mkStringPrintState :: PrintState StringOutput
printToString :: !a -> String | gPrint{|*|} a

(<<-) infixl 0 :: !(PrintState *s) !a -> *(PrintState *s)
	| gPrint{|*|} a & PrintOutput s


instance PrintOutput StringOutput 
instance PrintOutput File

generic gPrint a :: !a !(PrintState *s) -> (PrintState *s) | PrintOutput s

derive gPrint Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, RECORD of {grd_name}, FIELD of {gfd_name}, CONS of d, OBJECT of {gtd_num_conses,gtd_conses}, [], {!}, {}, (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
//derive bimap PrintState
