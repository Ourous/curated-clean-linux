implementation module iTasks.Internal.Generic.Defaults

import StdGeneric, StdFunc
import Data.Maybe, Data.Either, Data.Error, Data.Map, Text.HTML, Text.GenJSON, System.Time
import Data.GenDefault

gDefault{|Maybe|} fa	    				    = Nothing

gDefault{|HtmlTag|}		    				    = Html ""
gDefault{|Map|} fa fb                           = newMap

//SCARY BUG: When 'Map' is derived programs segfault when used in 'update' task on a shared source
derive gDefault Either, MaybeError, /*Map,*/ JSONNode, Timestamp, Timespec

defaultValue :: a | gDefault{|*|} a
defaultValue = gDefault{|*|}
