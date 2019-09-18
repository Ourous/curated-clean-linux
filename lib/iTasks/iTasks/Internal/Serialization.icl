implementation module iTasks.Internal.Serialization

import StdEnv
import dynamic_string

import Text.Encodings.Base64
import Data.Error
import Text.GenJSON
import Data.Maybe

import ABC.Interpreter

import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.UI.Editor

serialize :: !a -> *String
serialize value = copy_to_string value

deserialize	:: !*String -> MaybeErrorString a
deserialize str = let (a,_) = (copy_from_string str) in (Ok a)

serializeDynamic :: !Dynamic -> *String
serializeDynamic dyn = dynamic_to_string dyn

deserializeDynamic :: !*String -> Dynamic
deserializeDynamic str = string_to_dynamic str

JSONEncode{|Dynamic|} _ dyn = [JSONArray [JSONString "_DYNAMIC_", JSONString (base64URLEncode (serializeDynamic dyn))]]

JSONEncode{|(->)|} _ _ _ f = [JSONArray [JSONString "_FUNCTION_", JSONString (base64URLEncode (serialize f))]]

JSONDecode{|Dynamic|} _ [JSONArray [JSONString "_DYNAMIC_",JSONString string]:c]	= (Just (deserializeDynamic (base64URLDecode string)), c)
JSONDecode{|Dynamic|} _ c												= (Nothing, c)

JSONDecode{|(->)|} _ _ _ [JSONArray [JSONString "_FUNCTION_",JSONString string]:c] = (Just (fst(copy_from_string {s` \\ s` <-: base64URLDecode string})) ,c)
JSONDecode{|(->)|} _ _ _ c											= (Nothing,c)

functionFree :: !JSONNode -> Bool
functionFree (JSONString "_FUNCTION_") = False
functionFree (JSONString "_DYNAMIC_") = False
functionFree (JSONString "_DYNAMICENCODE_") = False
functionFree (JSONArray items) = and (map functionFree items)
functionFree (JSONObject fields) = and (map (functionFree o snd) fields)
functionFree _ = True

dynamicJSONEncode :: !a -> JSONNode
dynamicJSONEncode f = JSONArray [JSONString "_DYNAMICENCODE_",JSONString (base64URLEncode (copy_to_string f))]

dynamicJSONDecode :: !JSONNode -> Maybe a
dynamicJSONDecode (JSONArray [JSONString "_DYNAMICENCODE_",JSONString str]) = Just (fst (copy_from_string (base64URLDecode str)))
dynamicJSONDecode _					= Nothing

serializeForClient :: a !*VSt -> *(!String, !*VSt)
serializeForClient graph vst=:{VSt| abcInterpreterEnv}
	# serialized = serialize_for_prelinked_interpretation graph abcInterpreterEnv
	= (base64Encode serialized, vst)
