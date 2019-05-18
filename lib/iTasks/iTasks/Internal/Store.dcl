definition module iTasks.Internal.Store
/**
* This module provides a simple generic store.
* It is used to store the internal databases with users, sessions and workflow processes
* and for storage of intermediate task results.
*
* Values can be stored either by generic serialization to plain text, or by writing a dynamic
* to disk.
* Dynamics are generally more expensive, so only when really necessary (for example to store tasks or
* functions) should they be used.
*/
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.Maybe import :: Maybe
from Data.Error import :: MaybeError, :: MaybeErrorString
from System.FilePath import :: FilePath
import iTasks.SDS.Definition, iTasks.SDS.Sources.Store
from iTasks.Internal.IWorld import :: IWorld
from iTasks.WF.Definition import class iTask
from iTasks.UI.Editor import :: Editor, :: EditState, :: VSt
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks.Internal.Generic.Defaults		import generic gDefault
from Data.GenEq import generic gEq

/**
* Deletes the value with given key from the store
*/
deleteValue             :: !StoreNamespace !StoreName !*IWorld -> *(MaybeErrorString (),*IWorld)

/**
* Deletes all values that start with the prefix from the store
*/
deleteValues 			:: !StoreNamespace !StorePrefix !*IWorld -> *(MaybeErrorString (),*IWorld)

/**
* List the namespaces in the store
*/
listStoreNamespaces     ::                                          !*IWorld -> (![StoreNamespace], !*IWorld)
/**
* List the keys for a given namespace
*/
listStoreNames          :: !StoreNamespace                          !*IWorld -> (!MaybeErrorString [StoreName], !*IWorld)

/**
* Delete all values in the store
*/
emptyStore :: !*IWorld -> *IWorld
