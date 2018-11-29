implementation module Data.Either.GenJSON

import Data.Either, Text.GenJSON

derive JSONEncode Either
derive JSONDecode Either
