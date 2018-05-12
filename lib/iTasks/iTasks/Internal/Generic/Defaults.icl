implementation module iTasks.Internal.Generic.Defaults

import StdGeneric, StdFunc
import Data.Maybe, Data.Either, Data.Error, Data.Map, Text.HTML, Text.GenJSON, System.Time
import Data.GenDefault

gDefault{|Bool|}  				= False
gDefault{|Char|}  				= '-'

gDefault{|(->)|} fa fb	    				    = const fb
gDefault{|Dynamic|}		    				    = dynamic 42
gDefault{|Maybe|} fa	    				    = Nothing

gDefault{|HtmlTag|}		    				    = Html ""
gDefault{|Map|} fa fb                           = newMap

//SCARY BUG: When 'Map' is derived programs segfault when used in 'update' task on a shared source
derive gDefault Either, MaybeError, /*Map,*/ JSONNode, Timestamp, Timespec

defaultValue :: a | gDefault{|*|} a
defaultValue = gDefault{|*|}
