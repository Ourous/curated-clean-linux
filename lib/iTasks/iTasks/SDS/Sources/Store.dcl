definition module iTasks.SDS.Sources.Store
/**
* This module provides access to the generic document store where
* itasks applications store their data by default
*/
import iTasks.SDS.Definition
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from System.FilePath import :: FilePath
from Data.Maybe import :: Maybe

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

// Generic Store access
storeShare :: !String !Bool !StorageType !(Maybe a) -> (SDSSequence String a a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a

// Data blob storage access
blobStoreShare :: !String !Bool !(Maybe {#Char}) -> SDSSequence String {#Char} {#Char}

remoteShare :: (sds p r w) SDSShareOptions -> SDSRemoteSource p r w | RWShared sds

remoteService :: (WebServiceShareOptions p r w) -> SDSRemoteService p r w

debugShare :: String (sds p r w) -> SDSDebug p r w | RWShared sds
