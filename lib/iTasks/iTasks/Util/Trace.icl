implementation module iTasks.Util.Trace

import StdList, StdInt, StdString, StdGeneric, StdArray
import Text, System.OS
from Data.Map import toList, toAscList, foldrWithKey, :: Map
import Data.List

//Provides a generic pretty printer for easier debugging
generic gPrettyTrace a :: Int a -> [String]

gPrettyTrace{|Int|} level x  = [indent level (toString x)]
gPrettyTrace{|String|} level x = [indent level x]
gPrettyTrace{|Real|} level x = [indent level (toString x)]
gPrettyTrace{|Bool|} level x = [indent level (toString x)]

gPrettyTrace{|UNIT|} level _ = []
gPrettyTrace{|PAIR|} fx fy level (PAIR x y) = fx level x ++ fy level y
gPrettyTrace{|EITHER|} fx fy level (LEFT x) = fx level x
gPrettyTrace{|EITHER|} fx fy level (RIGHT y) = fy level y
gPrettyTrace{|OBJECT|} fx level (OBJECT x) = fx (level + 1) x
gPrettyTrace{|RECORD|} fx level (RECORD x) = [indent level "{"] ++ fx (level + 1) x ++ [indent level "}"]
gPrettyTrace{|CONS of d|} fx level (CONS x) = [indent level d.gcd_name:fx level x]
gPrettyTrace{|FIELD of d|} fx level (FIELD x) = [indent level d.gfd_name:fx level x]

gPrettyTrace{|[]|} fx level [] = [indent level "[]"]
gPrettyTrace{|[]|} fx level list
	| onelevel = [indent level ("[" +++ join ", " (flatten (map (fx 0) list)) +++ "]")]
	| otherwise = [indent level"[" : flatten items ] ++ [indent level "]"]
where
	items = map (fx (level + 1)) list
	onelevel = maximum (map length items) <= 1

gPrettyTrace{|()|} level _ = [indent level "()"]
gPrettyTrace{|(,)|} fx fy level (x,y) = [indent level "["] ++ fx (level + 1) x ++ fy (level + 1) y ++ [indent level "]"]
gPrettyTrace{|(,,)|} fx fy fz level (x,y,z) = [indent level "["] ++ fx (level + 1) x ++ fy (level + 1) y ++ fz (level + 1) z ++ [indent level "]"]
gPrettyTrace{|Dynamic|} level _ = [indent level "<Dynamic>"]

gPrettyTrace{|Map|} fk fv level m = [indent level "Map:": flatten [(fk (level + 1) k) ++ (fv (level + 1) v) \\ (k,v) <- toList m]]

indent :: Int String -> String
indent level s = {c \\ c <- repeatn level '-'} +++ s

prettyTrace :: a -> String | gPrettyTrace{|*|} a
prettyTrace x = join OS_NEWLINE (gPrettyTrace{|*|} 0 x)

sideBySideTrace :: (String, a) (String, a) -> String | gPrettyTrace{|*|} a
sideBySideTrace (leftTitle,left) (rightTitle,right)
	= join OS_NEWLINE (sideBySide [leftTitle:gPrettyTrace{|*|} 0 left] [rightTitle:gPrettyTrace{|*|} 0 right])
where
	sideBySide left right 
		| length left > length right
			= [showLine l r \\ l <- padded left & r <- padded (right ++ repeat "")]
		| otherwise
			= [showLine l r \\ l <- padded (left ++ repeat "") & r <- padded right]
	where
		maxLenLeft = maximum (map textSize left)
		padded = map (\l -> rpad l maxLenLeft ' ')
		showLine l r = (if (l <> r) "! " "  ") +++ l +++ " | " +++ r

