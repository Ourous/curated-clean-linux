definition module Database.Native.JSON

from Data.Maybe import :: Maybe
from Database.Native import :: Index
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode Index
derive JSONDecode Index
