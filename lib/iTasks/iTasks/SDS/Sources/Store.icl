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

instance toString StoreReadError
where
    toString (StoreReadMissingError name)      = "Stored data not in store: " +++ name
    toString (StoreReadDataError name)         = "Failed to read store data: " +++ name
    toString (StoreReadTypeError name)         = "Stored data is of incorrect type: " +++ name
    toString (StoreReadBuildVersionError name) = "Stored data contains functions from an older executable that can no longer be evaluated: " +++ name

derive class iTask StoreReadError

sharedDynamicStore :: !String !a -> SimpleSDSLens a | TC a
sharedDynamicStore storeId defaultV
	= mapReadWriteError (read, write) (Just reducer) (sharedStore storeId (dynamic defaultV))
where
	read (r :: a^) = r
	read x = Error (exception "Dynamic types mismatched?")

	write _ w = Ok (Just (dynamic w))

	reducer p d = read d

sharedStore :: !String !a -> SimpleSDSLens a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
sharedStore storeId defaultV
	= sdsFocus storeId (storeShare NS_APPLICATION_SHARES True InJSONFile (Just defaultV))

storeNamespaces :: SDSSource () [String] ()
storeNamespaces = createReadOnlySDS read
where
    read () iworld = listStoreNamespaces iworld

storeNames :: SDSSource String [String] ()
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

//Temporary memory storage
memoryStore :: !StoreNamespace !(Maybe a) -> SDSSequence StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
memoryStore namespace defaultV = storeShare namespace False InMemory defaultV

//Convenient derived store which checks version
jsonFileStore :: !StoreNamespace !Bool !Bool !(Maybe a) -> SDSSequence StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
jsonFileStore namespace check reset defaultV = storeShare namespace True InJSONFile defaultV

storeShare :: !String !Bool !StorageType !(Maybe a) -> (SDSSequence String a a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
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
	(removeMaybe defaultV storageLocation)

mbStoreShare :: !String !Bool !StorageType -> (SDSSequence String (Maybe a) (Maybe a)) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
mbStoreShare namespace versionSpecific prefType = sdsSequence "mbStoreShare"
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
	storageLocation

blobStoreShare :: !String !Bool !(Maybe {#Char}) -> SDSSequence String {#Char} {#Char}
blobStoreShare namespace versionSpecific defaultV = sdsSequence "storeShare"
	(\key -> ())
	(\key {storeDirPath,appVersion} -> storeDirPath </> namespace </> (if versionSpecific (appVersion </> safeName key) (safeName key)))
	(\_ _ -> Right snd)
	(SDSWriteConst (\_ _ -> Ok Nothing))
	(SDSWriteConst (\_ w -> Ok (Just w)))
	applicationOptions
	(removeMaybe defaultV fileShare)

storageLocation :: SDSSelect (FilePath,StorageType) (Maybe a) (Maybe a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
storageLocation = sdsSelect "storageLocation" choice
	(SDSNotifyConst (\_ _ -> const (const False))) (SDSNotifyConst (\_ _ -> const (const False))) memoryShare fileLoc
where
	choice (path,InMemory) = (Left path)
	choice (path,type)     = (Right (path,type))

	fileLoc = sdsSelect "fileLoc" choice (SDSNotifyConst (\ _ _ -> const (const False))) (SDSNotifyConst (\_ _->const (const False)))
		jsonLoc graphLoc
	where
		choice (path,InJSONFile) = Left (addExtension path "json")
		choice (path,_ )         = Right (addExtension path "bin")

	jsonLoc :: SDSCache FilePath (Maybe a) (Maybe a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
	jsonLoc = sdsCache (\_ _ _ w -> (Just w,WriteNow)) jsonFileShare

	graphLoc :: SDSCache FilePath (Maybe a) (Maybe a) | JSONEncode{|*|}, JSONDecode{|*|}, TC a
	graphLoc = sdsCache (\_ _ _ w -> (Just w,WriteNow)) graphFileShare

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

remoteShare :: (sds p r w) SDSShareOptions -> SDSRemoteSource p r w | RWShared sds
remoteShare sds opts = SDSRemoteSource sds Nothing opts

remoteService :: (WebServiceShareOptions p r w) -> SDSRemoteService p r w
remoteService opts = SDSRemoteService Nothing opts

debugShare :: String (sds p r w) -> SDSDebug p r w | RWShared sds
debugShare name sds = SDSDebug name sds

