definition module iTasks.Util.Trace
import StdGeneric
from Data.Map import :: Map

//Provides a generic pretty printer for easier debugging
generic gPrettyTrace a :: Int a -> [String]

derive gPrettyTrace Int, String, Real, Bool
derive gPrettyTrace UNIT, PAIR, EITHER, OBJECT, CONS of d, RECORD, FIELD of d
derive gPrettyTrace [], (), (,), (,,), Dynamic
derive gPrettyTrace Map

prettyTrace :: a -> String | gPrettyTrace{|*|} a

sideBySideTrace :: (String, a) (String, a) -> String | gPrettyTrace{|*|} a
