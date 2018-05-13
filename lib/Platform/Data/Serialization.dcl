definition module Data.Serialization

// FIXME: remove this when the compiler no longer translates
// :: Dynamic to :: DynamicTemp
from _SystemDynamic import :: DynamicTemp

from Data.Error import ::MaybeError, ::MaybeErrorString

/**
* pack a value into a dynamic and serialize it as a SYSDYN string
*/
serialize :: !a -> String | TC a

/**
* Deserialize a SYSDYN string to a dynamic and unpack it.
* If string is incorrect or pattern match fails, return Error
*/
deserialize :: !String -> MaybeErrorString a | TC a
/**
* pack a value into a dynamic and serialize it as a SYSDYN string
*/
serializeDynamic :: !Dynamic -> String

/**
* Deserialize a SYSDYN string to a dynamic
* If string is incorrect, return Error
*/
deserializeDynamic :: !String -> MaybeErrorString Dynamic
 
