definition module System.Time.GenJSON

from Data.Maybe   import :: Maybe
from System.Time  import :: Timestamp
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

derive JSONEncode Timestamp
derive JSONDecode Timestamp
