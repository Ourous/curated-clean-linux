implementation module Data.Set.GenJSON

import Data.Set, Text.GenJSON

derive JSONEncode Set
derive JSONDecode Set
