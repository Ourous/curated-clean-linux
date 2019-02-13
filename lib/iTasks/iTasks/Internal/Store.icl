implementation module iTasks.Internal.Store

import StdEnv
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Maybe, Data.Functor, Data.Error
import System.File, System.Directory, System.OSError, System.FilePath
import Text, Text.GenJSON, iTasks.Internal.Serialization

import iTasks.Internal.Client.JSStore
import iTasks.Internal.SDS

from iTasks.Engine import :: EngineOptions(..)
from iTasks.Internal.IWorld		import :: IWorld {options,onClient,memoryShares,world}, :: Resource
from iTasks.Internal.Task		    import exception
from iTasks.Internal.TaskState		import :: DeferredJSON(..)
from iTasks.Internal.TaskEval import :: TaskTime

from iTasks.WF.Definition				import class iTask
from iTasks.Internal.Generic.Visualization	import generic gText, :: TextFormat(..), toMultiLineText
from iTasks.Internal.Generic.Defaults		import generic gDefault
from iTasks.Internal.Serialization import serialize, deserialize, functionFree
from iTasks.UI.Editor.Generic import generic gEditor

from iTasks.WF.Definition import :: TaskId, :: TaskNo, :: InstanceNo
import iTasks.SDS.Sources.Core
import iTasks.SDS.Sources.Store
import iTasks.SDS.Combinators.Core
import iTasks.SDS.Combinators.Common
from System.Time 					import :: Timestamp(..), instance < Timestamp, instance toInt Timestamp
from Data.GenEq import generic gEq

instance toString StoreReadError
where
    toString (StoreReadMissingError name)      = "Stored data not in store: " +++ name
    toString (StoreReadDataError name)         = "Failed to read store data: " +++ name
    toString (StoreReadTypeError name)         = "Stored data is of incorrect type: " +++ name
    toString (StoreReadBuildVersionError name) = "Stored data contains functions from an older executable that can no longer be evaluated: " +++ name

derive class iTask StoreReadError

//Temporary memory storage
memoryStore :: !StoreNamespace !(Maybe a) -> SDSSequence StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
memoryStore namespace defaultV = storeShare namespace False InMemory defaultV

//Convenient derived store which checks version
jsonFileStore :: !StoreNamespace !Bool !Bool !(Maybe a) -> SDSSequence StoreName a a | JSONEncode{|*|}, JSONDecode{|*|}, TC a
jsonFileStore namespace check reset defaultV = storeShare namespace True InJSONFile defaultV

deleteValue :: !StoreNamespace !StoreName !*IWorld -> *(MaybeErrorString (),*IWorld)
deleteValue namespace delKey iworld=:{onClient=True}
	= (Ok (), jsDeleteValue namespace delKey iworld)
deleteValue namespace delKey iworld = deleteValues` namespace delKey (==) filterFuncDisk iworld
where
	// compare key with filename without extension
	filterFuncDisk delKey key = dropExtension key == delKey

deleteValues :: !StoreNamespace !StorePrefix !*IWorld -> *(MaybeErrorString (),*IWorld)
deleteValues namespace delKey iworld = deleteValues` namespace delKey startsWith startsWith iworld

deleteValues` :: !String !String !(String String -> Bool) !(String String -> Bool) !*IWorld -> *(MaybeErrorString (),*IWorld)
deleteValues` namespace delKey filterFuncCache filterFuncDisk iworld=:{options={storeDirPath},world}
	//Delete items from disk
	# (res,world) = deleteFromDisk world
	= (res,{iworld & world = world})
where
	deleteFromDisk world
		# storeDir		= storeDirPath</> namespace
		# (res, world)	= readDirectory storeDir world
		= case res of
			(Ok _) = deleteFiles storeDir (fromOk res) world
			(Error (errNo,errMsg))
				| errNo == 2 //The store directory doesn't exist -> nothing to do
					= (Ok (), world)
				| otherwise
					= (Error ("Problem reading store directory " +++ storeDir +++ ": " +++ toString errNo +++ " " +++ errMsg),world)

	deleteFiles _ [] world
		= (Ok (),world)
	deleteFiles dir [f:fs] world
		| filterFuncDisk (safeName delKey) f
			# (res,world) = deleteFile (dir </> f) world
			= case res of
				(Ok _) = deleteFiles dir fs world
				(Error (errNo,errMsg))
					| errNo == 2 // The file doesn't exits -> nothing to do
						= deleteFiles dir fs world
					| otherwise
						= (Error ("Problem deleting store file " +++ (dir </> f) +++ ": " +++ toString errNo +++ " " +++ errMsg), world)
		| otherwise //Skip
			= deleteFiles dir fs world

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

listStoreNamespaces :: !*IWorld -> (![StoreNamespace], !*IWorld)
listStoreNamespaces iworld=:{options={storeDirPath},world}
    # (res,world)   = readDirectory storeDirPath world
    = case res of
        Error e     = ([], {iworld & world = world})
        Ok files    = ([f \\ f <- files | not (f == "." || f == "..")], {iworld & world = world})

listStoreNames :: !StoreNamespace !*IWorld -> (!MaybeErrorString [StoreName], !*IWorld)
listStoreNames namespace iworld
    # (namespaces,iworld=:{options={storeDirPath},world})
                    = listStoreNamespaces iworld
    | not (isMember namespace namespaces)
                    = (Error ("Namespace " +++ namespace +++ " does not exist"), {iworld & world = world})
    # storeDir		= storeDirPath </> namespace
    # (res,world)   = readDirectory storeDir world
    = case res of
        Error e     = (Error (snd e), {iworld & world = world})
        Ok keys     = (Ok [dropExtension k \\ k <- keys | not (k == "." || k == "..")], {iworld & world = world})

emptyStore :: !*IWorld -> *IWorld
emptyStore iworld
	# (namespaces,iworld) = listStoreNamespaces iworld
	= foldl (\w ns -> emptyNamespace ns w) iworld namespaces
where
	emptyNamespace ns iworld = case listStoreNames ns iworld of
		(Ok stores,iworld) = foldl (\w s -> snd (deleteValue ns s w)) iworld stores
		(Error _,iworld) = iworld
