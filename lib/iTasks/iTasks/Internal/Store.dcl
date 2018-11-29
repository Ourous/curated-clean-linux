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
from iTasks.SDS.Definition import :: SDS, :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Internal.IWorld import :: IWorld
from iTasks.WF.Definition import class iTask
from iTasks.UI.Editor import :: Editor, :: EditState, :: VSt
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks.Internal.Generic.Defaults		import generic gDefault
from Data.GenEq import generic gEq

:: StoreNamespace	:== String
:: StoreName		:== String
:: StorePrefix		:== String
:: BuildID          :== String

// Predefined namespaces
NS_TASK_INSTANCES		:== "task-instances"
NS_DOCUMENT_CONTENT		:== "document-data"
NS_APPLICATION_SHARES	:== "application-data"

:: StoreReadError
    = StoreReadMissingError !StoreName      //When there is no file on disk for this
    | StoreReadDataError !StoreName         //When there is a problem reading data from disk
    | StoreReadTypeError !StoreName         //When the data cannot be decoded based on the type
    | StoreReadBuildVersionError !StoreName //When there is a stored value but it has the wrong build version

instance toString StoreReadError
derive class iTask StoreReadError

/**
* Creates a store in memory. Values in this store are lost when the server shuts down.
*
* @param The namespace in the store
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
memoryStore   :: !StoreNamespace !(Maybe a) -> RWShared StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

/**
* Extends a fullFileStore with JSON encoding/decoding such that arbitrary values can be stored.
* It also adds optional buildID checking to make sure that JSONEncoded functions and dynamics are
* not decoded if the versions don't match.
*
* @param The namespace in the store
* @param Check the build versions to protect against deserializing outdated functions stored by older versions
* @param Automatically reset the the store if an error occurs
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
jsonFileStore :: !StoreNamespace !Bool !Bool !(Maybe a) -> RWShared StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

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
