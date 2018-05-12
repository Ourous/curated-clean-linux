definition module iTasks.Internal.Generic.Defaults

import Data.GenDefault

derive gDefault (->), Bool, Char, Maybe, Either, MaybeError, Map, JSONNode, HtmlTag, Timestamp, Timespec

from Text.GenJSON import :: JSONNode
from Text.HTML import :: HtmlTag
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Map import :: Map
from System.Time import :: Timestamp, :: Timespec

// Wrapper functions for updating
defaultValue :: a | gDefault{|*|} a
