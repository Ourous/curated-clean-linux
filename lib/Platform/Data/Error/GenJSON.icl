implementation module Data.Error.GenJSON

import Data.Error, Text.GenJSON

derive JSONEncode MaybeError
derive JSONDecode MaybeError
