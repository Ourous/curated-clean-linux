definition module iTasks.SDS.Sources.Store
/**
* This module provides access to the generic document store where
* itasks applications store their data by default
*/
from iTasks.SDS.Definition import :: SDS
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from System.FilePath import :: FilePath
from Data.Maybe import :: Maybe

/*
* Creates a reference to a store identified by a string identifier.
* If no data is store the default value given as second argument is given as result.
*/
sharedStore        :: !String !a -> SDS () a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedDynamicStore :: !String !a -> SDS () a a | TC a

// Generic access to the store
storeNamespaces    :: SDS () [String] ()     // List the namespaces in the store 
storeNames         :: SDS String [String] () // List the stores in a given namespace

:: StorageType
  = InMemory      //When the data is disposable. It will be gone when the application shuts down
  | InJSONFile    //When the data should be persisted between different versions of an application
  | InDynamicFile //When the data contains functions, dynamics or otherwise

// Generic Store access
storeShare :: !String !Bool !StorageType !(Maybe a) -> (SDS String a a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a

// Data blob storage access
blobStoreShare :: !String !Bool !(Maybe {#Char}) -> SDS String {#Char} {#Char}
