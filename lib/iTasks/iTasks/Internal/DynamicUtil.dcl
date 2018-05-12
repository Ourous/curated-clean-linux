definition module iTasks.Internal.DynamicUtil

import Text.GenJSON

//derive JSONEncode TypeCode
//derive JSONDecode TypeCode

unpackType 		:: !Dynamic -> TypeCode
//typeCodeName 	:: !TypeCodeConstructor -> String
//unsafeCreateDynamic :: !a !TypeCode -> Dynamic

toDyn :: a -> Dynamic | TC a

