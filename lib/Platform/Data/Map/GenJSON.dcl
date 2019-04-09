definition module Data.Map.GenJSON

from Data.Maybe   import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Map     import :: Map

derive JSONEncode Map
derive JSONDecode Map
