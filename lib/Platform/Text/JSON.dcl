definition module Text.JSON
/*
* This module provides functions to encode and decode any Clean data type
* to JSON format. It provides two generic functions JSONEncode and JSONDecode
* which must be derived for concrete types. Then toJSON and fromJSON may be
* used to convert any value to and from JSON.
*
* For more info about JSON see: http://www.json.org/
*/

import StdGeneric
from StdFile import class <<<
from StdOverloaded import class fromString, class toString, class ==(..)
from StdString import instance == {#Char}
from Data.List import !?
from Data.Maybe import :: Maybe(..)

:: JSONNode	= JSONNull
			| JSONBool !Bool
			| JSONInt !Int
			| JSONReal !Real
			| JSONString !String
			| JSONArray ![JSONNode]
			| JSONObject ![(!String,!JSONNode)]
			| JSONRaw !String
			| JSONError
/**
* Serializing JSON structures is done with a toString instance
*/
instance toString JSONNode
/**
* Deserializing JSON structures is done with a fromString instance
*/
instance fromString JSONNode

/**
* Serialize a JSON structure and write to a File
*/
instance <<< JSONNode

/**
* Encodes any value to JSON format.
* @param The value to encode
* @return The JSON encoded value
*/
toJSON        :: !a -> JSONNode | JSONEncode{|*|} a

toJSONInField :: !a -> JSONNode | JSONEncode{|*|} a

/**
* Tries to parse a JSON encoded string.
* When parsing fails, the result is Nothing.
*
* @param The JSON encoded input
* @return Just the result, when parsing succeeds
*/
fromJSON	:: !JSONNode	-> Maybe a	| JSONDecode{|*|} a

/**
* Escapes a string for manual JSON construction
*
* @param The unescaped string
* @return A properly escaped string
*/
jsonEscape	:: !String	-> String

/**
* Unescapes a string that is escaped for use in a serialized JSON string
*
* @param The escaped string
* @return An unescaped string
*/
jsonUnescape :: !String -> String

/**
* Simple query-by-path function that enables searching of JSON structures
*
* @param The query path separated by '/'. Objects are indexed by fieldname
*        and arrays by their array index.
*        Example paths: 'node1/node3' 'node1/node2/23'
*
* @return The value if a value of the right type is at that path.
*/
jsonQuery :: !String !JSONNode -> Maybe a | JSONDecode{|*|} a

/**
* Generic encoding function. This function should not be used
* directly but always through the toJSON function. It must be derived
* for each type you want to encode in JSON format.
*/
generic JSONEncode t :: !Bool !t -> [JSONNode]
derive  JSONEncode Int, Real, Char, Bool, String, UNIT, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), {}, {!}, Maybe, JSONNode,
	EITHER, CONS of {gcd_name}, OBJECT

JSONEncode{|RECORD of {grd_fields}|} fx _ (RECORD x)
  = [JSONObject [(name, o) \\ o <- fx False x & name <- grd_fields | isNotNull o]]
  where
  isNotNull :: !JSONNode -> Bool
  isNotNull JSONNull = False
  isNotNull _ = True

JSONEncode{|FIELD|} fx _ (FIELD x) = fx True x

JSONEncode{|PAIR|} fx fy _ (PAIR x y) = fx False x ++ fy False y
where
	(++) infixr 5::![.a] !u:[.a] -> u:[.a]
	(++) [hd:tl]	list	= [hd:tl ++ list]
	(++) nil 		list	= list

/**
* Generic decoding function. This function should not be used
* directly, but always through the fromJSON function. It must be derived
* for each type you want to parse from JSON format.
*/
generic JSONDecode t :: !Bool ![JSONNode] -> (!Maybe t,![JSONNode])
derive  JSONDecode Int, Real, Char, Bool, String, UNIT, EITHER, CONS of {gcd_name}, OBJECT, [], (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,), {}, {!}, Maybe, JSONNode

JSONDecode{|PAIR|} fx fy _ l = d1 fy (fx False l) l
  where
  d1 :: !(Bool [JSONNode] -> (!Maybe b, ![JSONNode])) !(!Maybe a, ![JSONNode]) ![JSONNode]
     -> (!Maybe (PAIR a b), ![JSONNode])
  d1 fy (Just x,xs)  l = d2 x (fy False xs) l
  d1 _  (Nothing, _) l = (Nothing, l)

  d2 :: !a !(!Maybe b, ![JSONNode]) ![JSONNode] -> (!Maybe (PAIR a b), ![JSONNode])
  d2 x (Just y, ys) l = (Just (PAIR x y), ys)
  d2 x (Nothing, _) l = (Nothing, l)

JSONDecode{|RECORD|} fx _ l=:[obj=:JSONObject fields : xs] = d (fx False [obj]) xs l
  where
  d :: !(Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSONDecode{|RECORD|} fx _ l=:[obj=:JSONArray fields : xs] = d (fx False [obj]) xs l
  where
  d :: !(Maybe a, b) ![JSONNode] ![JSONNode] -> (!Maybe (RECORD a), ![JSONNode])
  d (Just x, _)  xs l = (Just (RECORD x),xs)
  d (Nothing, _) xs l = (Nothing, l)
JSONDecode{|RECORD|} fx _ l = (Nothing,l)

JSONDecode{|FIELD of {gfd_name}|} fx _ l =:[JSONObject fields]
  #! field = findField gfd_name fields
  = case fx True field of
      (Just x, _) = (Just (FIELD x), l)
      (_, _)      = (Nothing, l)
  where
  findField :: !String ![(!String, !JSONNode)] -> [JSONNode]
  findField match [(l,x):xs]
    | l == match = [x]
    | otherwise  = findField match xs
  findField match [] = []
JSONDecode{|FIELD of {gfd_index}|} fx _ l =:[JSONArray fields]
	= case fields !? gfd_index of
		Nothing    = (Nothing, l)
		Just field = case fx True [field] of
			(Just x, _) = (Just (FIELD x), l)
			(_, _)      = (Nothing, l)
JSONDecode{|FIELD|} fx _ l = (Nothing, l)

/**
* Equality of JSON nodes.
* JSON Reals are considered equal if their string representation is equal.
* JSON Objects are considered equal if they contain the same non-null fields.
*/
instance == JSONNode

/**
* Pretty printed string encoding of JSON nodes.
* This function uses indenting and newlines to make the serialized JSON representation
* more readable than the standard toString instance, which uses minimal whitespace.
*/
jsonPrettyPrint :: !JSONNode -> String


