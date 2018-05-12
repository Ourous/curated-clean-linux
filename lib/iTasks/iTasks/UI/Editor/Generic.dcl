definition module iTasks.UI.Editor.Generic
/**
* This module provides a generic function to create editors for arbitrary types
*/
import iTasks.UI.Editor
import StdGeneric
from Text.HTML import :: HtmlTag
from Data.Error import :: MaybeError
from System.Time import :: Timestamp
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat

/**
* Main eneric editor function
*/
generic gEditor a | gText a, gDefault a, JSONEncode a, JSONDecode a :: Editor a

derive gEditor
	UNIT,
	EITHER with ex _ dx _ _ ey _ dy _ _,
	PAIR with ex _ _ _ _ ey _ _ _ _,
	OBJECT of {gtd_num_conses,gtd_conses} with ex _ _ _ _,
	CONS of {gcd_index,gcd_arity} with ex _ _ _ _,
	RECORD of {grd_arity} with ex _ _ _ _,
	FIELD of {gfd_name} with ex _ _ _ _

derive gEditor Int, Real, Char, Bool, String, [], (), (,), (,,), (,,,), (,,,,), (,,,,,), (->), Dynamic
derive gEditor Maybe, Either, MaybeError, Map, JSONNode, HtmlTag, Timestamp

derive bimap Editor
