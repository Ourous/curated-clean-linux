implementation module Database.Native.JSON

import Database.Native
import Text.GenJSON

JSONEncode{|Index|} _ (Index i) = [JSONInt i]
JSONDecode{|Index|} _ [JSONInt i:l] = (Just (Index i), l)
JSONDecode{|Index|} _ l             = (Nothing, l)
