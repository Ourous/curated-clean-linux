definition module iTasks.UI.Tune
/**
* It is common to fine-tune the user interfaces of tasks.
* This module provides an overloaded way of annotating tasks or editors with attributes that
* change the behavior of the UI.
*/
from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import generic gEditor, generic gEq, generic gDefault, generic gText, generic JSONEncode, generic JSONDecode

from Data.Maybe import :: Maybe
from Text.GenJSON import :: JSONNode
from iTasks.UI.Editor import :: Editor
from iTasks.Internal.Generic.Visualization import :: TextFormat

class tune b f :: !b !(f a) -> f a
class tunev b a f | iTask a :: !(b a) !(f a) -> f a

/**
* Infix shorthands for the (overloaded) tune combinator.
*/
(<<@) infixl 2 :: !(f a) !b	-> f a | tune b f
(@>>) infixr 2 :: !b !(f a)	-> f a | tune b f
(<@@) infixl 2 :: !(f a) !(b a) -> f a | tunev b a f & iTask a
(@@>) infixr 2 :: !(b a) !(f a) -> f a | tunev b a f & iTask a
