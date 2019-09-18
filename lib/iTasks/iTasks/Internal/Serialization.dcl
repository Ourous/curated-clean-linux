definition module iTasks.Internal.Serialization

from Text.GenJSON import generic JSONEncode, generic JSONDecode, ::JSONNode
from Data.Error import ::MaybeError, ::MaybeErrorString
from Data.Maybe import ::Maybe

from iTasks.Internal.IWorld import :: IWorld
from iTasks.UI.Editor import :: VSt

serialize :: !a -> *String
deserialize	:: !*String -> MaybeErrorString a
serializeDynamic :: !Dynamic -> *String
deserializeDynamic :: !*String -> Dynamic

derive JSONEncode Dynamic, (->)
derive JSONDecode Dynamic, (->)

//Check if a JSON serialization contains encoded functions or dynamics
functionFree		:: !JSONNode -> Bool

dynamicJSONEncode	:: !a -> JSONNode
dynamicJSONDecode	:: !JSONNode -> Maybe a

/**
 * Serialize a Clean value to send to a browser client running Clean in the
 * WebAssembly interpreter. The values are serialized using GraphCopy after
 * which the descriptors are replaced by the descriptors of the client.
 * @param The expression to serialize.
 * @result The serialized string in base64 encoding.
 */
serializeForClient :: a !*VSt -> *(!String, !*VSt)
