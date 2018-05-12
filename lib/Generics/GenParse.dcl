definition module GenParse

import StdGeneric, StdMaybe

class ParseInput s where
	parseInput :: s -> (Maybe Char, s)

:: StringInput = { si_str :: !String, si_pos :: !Int} 	
mkStringInput :: String -> StringInput 

instance ParseInput	StringInput 
instance ParseInput File

:: Expr
generic gParse a :: Expr -> Maybe a

derive gParse Int, Char, Bool, Real, String, UNIT, PAIR, EITHER, CONS of d, RECORD of d, FIELD of d, OBJECT, [], {!}, {}

parseString :: String -> Maybe a | gParse{|*|} a
parseFile :: File -> Maybe a | gParse{|*|} a
