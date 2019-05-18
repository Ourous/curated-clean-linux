definition module iTasks.Internal.DynamicUtil

import Text.GenJSON
from iTasks.WF.Definition import :: Task, :: TaskValue

//derive JSONEncode TypeCode
//derive JSONDecode TypeCode

unpackType 		:: !Dynamic -> TypeCode
//typeCodeName 	:: !TypeCodeConstructor -> String
//unsafeCreateDynamic :: !a !TypeCode -> Dynamic

toDyn :: a -> Dynamic | TC a

cast :: a -> b | TC a & TC b
cast_to_TaskValue :: a -> TaskValue b | TC a & TC b

unwrapTask :: Dynamic -> Task a | TC a
