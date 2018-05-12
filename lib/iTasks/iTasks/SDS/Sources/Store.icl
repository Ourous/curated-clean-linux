implementation module iTasks.SDS.Sources.Store

import iTasks.SDS.Definition
import iTasks.SDS.Sources.Core
import iTasks.SDS.Sources.System
import iTasks.SDS.Combinators.Core
import iTasks.SDS.Combinators.Common

import iTasks.Engine
import iTasks.Internal.SDS
import iTasks.Internal.Store
import iTasks.Internal.Task
import iTasks.Internal.IWorld
import iTasks.Internal.Serialization
import System.FilePath
import StdTuple, StdFunc, StdArray, StdBool, StdChar, StdInt, StdString

sharedDynamicStore :: !String !a -> SDS () a a | TC a
sharedDynamicStore storeId defaultV
	= mapReadWriteError (read, write) (sharedStore storeId (dynamic defaultV))
where
	read (r :: a^) = r
	read x = Error (exception "Dynamic types mismatched?")

	write _ w = Ok (Just (dynamic w))

sharedStore :: !String !a -> SDS () a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV
	= sdsFocus storeId (storeShare NS_APPLICATION_SHARES True InJSONFile (Just defaultV))

storeNamespaces :: SDS () [String] ()
storeNamespaces = createReadOnlySDS read
where
    read () iworld = listStoreNamespaces iworld

storeNames :: SDS String [String] ()
storeNames = createReadOnlySDSError read
where
    read namespace iworld = case listStoreNames namespace iworld of
        (Ok names,iworld) = (Ok names,iworld)
        (Error e,iworld) = (Error (exception e),iworld)

:: StorageType
  = InMemory      //When the data is disposable. It will be gone when the application shuts down
  | InJSONFile    //When the data should be persisted between different versions of an application
  | InDynamicFile //When the data contains functions, dynamics or otherwise

derive class iTask StorageType

storeShare :: !String !Bool !StorageType !(Maybe a) -> (SDS String a a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
storeShare namespace versionSpecific prefType defaultV = sdsSequence "storeShare"
	(\key -> ())
	//Compute the filepath in the store from the key
	//And decide if the store should be memory-only or can use a persistent version
	(\key {EngineOptions|appVersion,storeDirPath,persistTasks} ->
		(storeDirPath </> namespace </> (if versionSpecific (appVersion </> safeName key) (safeName key))
		,if (namespace == NS_TASK_INSTANCES && not persistTasks) InMemory prefType))
	(\_ _ -> Right snd)
	(SDSWriteConst (\_ _ -> Ok Nothing))
	(SDSWriteConst (\_ w -> Ok (Just w)))
	applicationOptions
	(storageLocation defaultV)

blobStoreShare :: !String !Bool !(Maybe {#Char}) -> SDS String {#Char} {#Char}
blobStoreShare namespace versionSpecific defaultV = sdsSequence "storeShare"
	(\key -> ())
	(\key {storeDirPath,appVersion} -> storeDirPath </> namespace </> (if versionSpecific (appVersion </> safeName key) (safeName key)))
	(\_ _ -> Right snd)
	(SDSWriteConst (\_ _ -> Ok Nothing))
	(SDSWriteConst (\_ w -> Ok (Just w)))
	applicationOptions
	(removeMaybe defaultV fileShare)

storageLocation :: !(Maybe a) -> SDS (FilePath,StorageType) a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
storageLocation defaultV = sdsSelect "storageLocation" choice
	(SDSNotifyConst (\_ _ -> const (const False))) (SDSNotifyConst (\_ _ -> const (const False)))
	(memoryLoc defaultV) (fileLoc defaultV)
where
	choice (path,InMemory) = (Left path)
	choice (path,type)     = (Right (path,type))

	fileLoc defaultV = sdsSelect "fileLoc" choice (SDSNotifyConst (\ _ _ -> const (const False))) (SDSNotifyConst (\_ _->const (const False)))
		(jsonLoc defaultV) (graphLoc defaultV)
	where
		choice (path,InJSONFile) = Left (addExtension path "json")
		choice (path,_ )         = Right (addExtension path "bin")

	memoryLoc :: !(Maybe a) -> SDS FilePath a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
	memoryLoc defaultV = removeMaybe defaultV memoryShare

	jsonLoc :: !(Maybe a) -> SDS FilePath a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
	jsonLoc defaultV = removeMaybe defaultV (sdsCache (\_ _ _ w -> (Just w,WriteDelayed)) jsonFileShare)

	graphLoc :: !(Maybe a) -> SDS FilePath a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
	graphLoc  defaultV = removeMaybe defaultV (sdsCache (\_ _ _ w -> (Just w,WriteDelayed)) graphFileShare)

//Utility function to make sure we don't use names that escape the file path
safeName :: !String -> String
safeName s = copy 0 (createArray len '\0')
where
	len = size s
	copy :: !Int !*String -> String
	copy i n
		| i == len	= n
		| isAlphanum s.[i] || s.[i] == '-'  = copy (i + 1) {n & [i] = s.[i]}
							                = copy (i + 1) {n & [i] = '_'}

