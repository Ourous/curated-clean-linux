implementation module Data.Map.GenJSON

import Data.Map, Text.GenJSON

derive JSONEncode Map
derive JSONDecode Map
