definition module iTasks.SDS.Sources.Store
/**
* This module provides access to the generic document store where
* iTask applications store their data by default
*/

import iTasks.SDS.Definition
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from System.FilePath import :: FilePath
from Data.Maybe import :: Maybe

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

/*
* Creates a reference to a store identified by a string identifier.
* If no data is store the default value given as second argument is given as result.
*/
sharedStore :: !String !a -> SimpleSDSLens a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedDynamicStore :: !String !a -> SimpleSDSLens a | TC a

// Generic access to the store
storeNamespaces :: SDSSource () [String] ()
storeNames      :: SDSSource String [String] () // List the stores in a given namespace

:: StorageType
  = InMemory      //When the data is disposable. It will be gone when the application shuts down
  | InJSONFile    //When the data should be persisted between different versions of an application
  | InDynamicFile //When the data contains functions, dynamics or otherwise

/**
* Creates a store in memory. Values in this store are lost when the server shuts down.
*
* @param The namespace in the store
* @param Optionally a default content to be used on first read. If nothing is given an error will occur when reading before writing.
*/
memoryStore :: !StoreNamespace !(Maybe a) -> SDSSequence StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

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
jsonFileStore :: !StoreNamespace !Bool !Bool !(Maybe a) -> SDSSequence StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a

// Generic Store access
storeShare :: !String !Bool !StorageType !(Maybe a) -> (SDSSequence String a a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
mbStoreShare :: !String !Bool !StorageType -> (SDSSequence String (Maybe a) (Maybe a)) | JSONEncode{|*|}, JSONDecode{|*|}, TC a

// Data blob storage access
blobStoreShare :: !String !Bool !(Maybe {#Char}) -> SDSSequence String {#Char} {#Char}

remoteShare :: (sds p r w) SDSShareOptions -> SDSRemoteSource p r w | RWShared sds

remoteService :: (WebServiceShareOptions p r w) -> SDSRemoteService p r w

debugShare :: String (sds p r w) -> SDSDebug p r w | RWShared sds

