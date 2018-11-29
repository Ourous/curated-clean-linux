definition module GenPrint

import StdGeneric

class PrintOutput s where
	printOutput :: Char *s -> *s
	
:: *StringOutput

:: PrintState s

mkPrintState :: *s -> PrintState *s | PrintOutput s
mkStringPrintState :: PrintState StringOutput
printToString :: a -> String | gPrint{|*|} a

(<<-) infixl 0 :: (PrintState *s) a -> *(PrintState *s) 
	| gPrint{|*|} a & PrintOutput s


instance PrintOutput StringOutput 
instance PrintOutput File

generic gPrint a :: a (PrintState *s) -> (PrintState *s) | PrintOutput s

derive gPrint Int, Real, Char, Bool, String, UNIT, PAIR, EITHER, RECORD of d, FIELD of d, CONS of d, OBJECT, [], {!}, {}
//derive bimap PrintState

