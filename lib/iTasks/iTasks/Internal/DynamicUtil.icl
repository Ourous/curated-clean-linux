implementation module iTasks.Internal.DynamicUtil

import _SystemDynamic
import Data.Maybe, Text.GenJSON
import iTasks.WF.Definition

:: MyTypeCodeConstructor = E.a: { my_tcc_cons :: !a }

:: MyDynamic = E.a:
	{	mydynamic_value :: a
	,	mydynamic_type	:: TypeCode
	}
	
unpackType :: !Dynamic -> TypeCode
unpackType dyn = (toMyDynamic dyn).mydynamic_type
where
	toMyDynamic :: !Dynamic -> MyDynamic
	toMyDynamic _ = code {
		pop_a 0
	}

/*
typeCodeName :: !TypeCodeConstructor -> String
typeCodeName tcc = descriptorToString (getDescriptor (toMyTypeCodeConstructor tcc).my_tcc_cons)
where
	toMyTypeCodeConstructor :: !TypeCodeConstructor -> MyTypeCodeConstructor
	toMyTypeCodeConstructor _ = code {
		pop_a 0
	}

unsafeCreateDynamic :: !a !TypeCode -> Dynamic
unsafeCreateDynamic val type = toDynamic {mydynamic_value = val, mydynamic_type=type}
where
	toDynamic :: !MyDynamic -> Dynamic
	toDynamic _ = code {
		pop_a 0
	}

descriptorToString :: !Int -> {#Char}
descriptorToString desc
	= code {
	.d 0 1 i
		jsr DtoAC
	.o 1 0
	}

getDescriptor :: !a -> Int
getDescriptor _
	= code {
		pushD_a	0
		pop_a	1
	}
*/

/*
JSONEncode{|TypeCode|} (TypeScheme n tc) = [JSONArray [JSONString "TypeScheme", JSONInt n, toJSON tc]]
JSONEncode{|TypeCode|} (TypeVar n) = [JSONArray [JSONString "TypeVar", JSONInt n]]
JSONEncode{|TypeCode|} (TypeCons tcc) = [JSONArray [JSONString "TypeConsName", JSONString (toString tcc)]]
JSONEncode{|TypeCode|} (TypeConsName cn) = [JSONArray [JSONString "TypeConsName", JSONString cn]]
JSONEncode{|TypeCode|} (TypeApp tc1 tc2) = [JSONArray [JSONString "TypeApp", toJSON tc1, toJSON tc2]]
JSONEncode{|TypeCode|} (TypeUnique tc) = [JSONArray [JSONString "TypeUnique", toJSON tc]]

JSONDecode{|TypeCode|} [JSONArray [JSONString "TypeScheme", JSONInt n, tc]:ts] 
		= case fromJSON tc of
			(Just tc) = (Just (TypeScheme n tc), ts)		
			Nothing   = (Nothing, ts)

JSONDecode{|TypeCode|} [JSONArray [JSONString "TypeVar", JSONInt n]:ts] 
		= (Just (TypeVar n), ts)

JSONDecode{|TypeCode|} [JSONArray [JSONString "TypeConsName", JSONString cn]:ts] 
		= (Just (TypeConsName cn), ts)

JSONDecode{|TypeCode|} [JSONArray [JSONString "TypeApp", tc1, tc2]:ts] 
		= case (fromJSON tc1, fromJSON tc2) of
			(Just tc1, Just tc2) 
					= (Just (TypeApp tc1 tc2), ts)		
					= (Nothing, ts)

JSONDecode{|TypeCode|} [JSONArray [JSONString "TypeUnique", tc]:ts] 
		= case fromJSON tc of
			(Just tc) = (Just (TypeUnique tc), ts)		
			Nothing   = (Nothing, ts)

JSONDecode{|TypeCode|} ts = (Nothing, ts)
*/

toDyn :: a -> Dynamic | TC a
toDyn x = dynamic x

cast :: a -> b | TC a & TC b
cast a = case toDyn a of (a::b^) -> a

cast_to_TaskValue :: a -> TaskValue b | TC a & TC b
cast_to_TaskValue a = case toDyn a of (a::TaskValue b^) -> a

unwrapTask :: Dynamic -> Task a | TC a
unwrapTask (task :: Task a^) = task
