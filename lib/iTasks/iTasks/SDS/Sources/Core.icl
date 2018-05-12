implementation module iTasks.SDS.Sources.Core

import iTasks.SDS.Definition
import iTasks.Internal.SDS
import iTasks.Internal.IWorld
import iTasks.Internal.Serialization
import System.FilePath, System.Directory, System.File
import Text, Text.GenJSON
import StdFile, StdTuple, StdArray, StdBool, StdList, StdString
import qualified Data.Map as DM

from StdFunc import const
from iTasks.Internal.Task import exception

constShare :: !a -> SDS p a ()
constShare v = createReadOnlySDS (\_ env -> (v, env))

nullShare :: SDS p () a
nullShare = createReadWriteSDS "_core_" "nullShare" (\_ env -> (Ok (), env)) (\_ _ env -> (Ok (const (const False)), env))

unitShare :: SDS () () ()
unitShare = nullShare

worldShare :: (p *World -> *(MaybeErrorString r,*World)) (p w *World -> *(MaybeErrorString (),*World)) -> SDS p r w
worldShare read write = createReadWriteSDS "_core_" "worldShare" read` write`
where
	read` p iworld=:{IWorld|world} = case read p world of
		(Ok r,world) = (Ok r, {IWorld|iworld & world = world})
		(Error e,world) = (Error (exception e), {IWorld|iworld & world = world})

	write` p w iworld=:{IWorld|world} = case write p w world of
		(Ok (),world) = (Ok (const (const False)), {IWorld|iworld & world = world})
		(Error e,world) = (Error (exception e), {IWorld|iworld & world = world})

// Random source
randomInt :: SDS () Int ()
randomInt = createReadOnlySDS randomInt
where
	randomInt () iworld=:{IWorld|random=[i:is]}
		= (i, {IWorld|iworld & random = is})

memoryShare :: SDS String (Maybe a) (Maybe a) | TC a
memoryShare = createReadWriteSDS "_core_" "memoryShare" read write
where
	read key iworld=:{IWorld|memoryShares}
		= case 'DM'.get key memoryShares of
			(Just (val :: a^))  = (Ok (Just val),iworld)
			Nothing             = (Ok Nothing, iworld)
			(Just _)            = (Error (exception ("Read shared memory with incorrect type " +++ key)), iworld)

	write key (Just val) iworld=:{IWorld|memoryShares}
       = (Ok (const ((===) key)),{IWorld|iworld & memoryShares = 'DM'.put key (dynamic val :: a^) memoryShares})
	write key Nothing iworld=:{IWorld|memoryShares}
       = (Ok (const ((===) key)),{IWorld|iworld & memoryShares = 'DM'.del key memoryShares})

fileShare :: SDS FilePath (Maybe String) (Maybe String)
fileShare = createReadWriteSDS "_core_" "fileShare" (fileRead fromFile) (fileWrite toFile)
where
	fromFile path content = Ok content

	toFile path content = content

jsonFileShare :: SDS FilePath (Maybe a) (Maybe a) | JSONEncode{|*|}, JSONDecode{|*|} a
jsonFileShare = createReadWriteSDS "_core_" "jsonFileShare" (fileRead fromFile) (fileWrite toFile)
where
	fromFile path content = case fromJSON (fromString content) of
		(Just value) = Ok value
		Nothing      = Error (exception ("Could not parse json file " +++ path))

	toFile path content = toString (toJSON content)

// Share that maps to a file that holds a serialized graph representation of the value
graphFileShare :: SDS FilePath (Maybe a) (Maybe a)
graphFileShare = createReadWriteSDS "_core_" "graphFileShare" (fileRead fromFile) (fileWrite toFile)
where
	fromFile path content = case deserialize {c \\ c <-: content} of
	    Ok val  = Ok val
		Error e = Error (exception e)

	toFile path content = serialize content

fileRead fromFile path iworld=:{world}
	# (ok,file,world)			= fopen path FReadData iworld.world
	| not ok					= (Ok Nothing, {IWorld|iworld & world = world})
	# (res,file)				= readAll file
	# (ok,world)				= fclose file world
	| not ok					= (Error (exception CannotClose) ,{IWorld|iworld & world = world})
	= case res of
	   	Error e                 = (Error (exception e), {IWorld|iworld & world = world})
        Ok content = case fromFile path content of
			(Ok value) = (Ok (Just value), {IWorld|iworld & world = world})
			(Error e) = (Error e,{IWorld|iworld & world = world})

fileWrite toFile path (Just content) iworld=:{IWorld|world}
	# (ok,file,world)			= fopen path FWriteData world
	| ok
		= writeContent file {IWorld|iworld & world = world}
	| not ok					
		//Check parent dirs...
		# (ok,world) = ensureParentDirs path world
		| not ok = (Error (exception CannotOpen), {IWorld|iworld & world = world})
		//.. and try again
		# (ok,file,world)			= fopen path FWriteData world
		| ok
			= writeContent file {IWorld|iworld & world = world}
		//Really can't open
			= (Error (exception CannotOpen), {IWorld|iworld & world = world})
	where
		writeContent file iworld=:{IWorld|world}
			# file						= fwrites (toFile path content) file
			# (ok,world)				= fclose file world
			| not ok					= (Error (exception CannotClose) ,{IWorld|iworld & world = world})
			= (Ok (const ((==) path)), {IWorld|iworld & world = world})

fileWrite toFile path Nothing iworld
	= (Error (exception "Removing files through fileShare SDS not yet supported"),iworld)

//Create all parent directories of a file if they don't exist
ensureParentDirs :: FilePath *World -> (!Bool,*World)
ensureParentDirs path world = let [b:p] = split {pathSeparator} path in create [b] p world
where
	create _ [] world = (True,world)
	create _ [file] world = (True,world) 
	create base [dir:rest] world
		# next = base ++ [dir]
		# path = join {pathSeparator} next
		# (exists,world) = fileExists path world
		| exists = create next rest world //This part exists, continue
		# (res, world) = createDirectory path world 
		| isError res = (False,world) //Can't create the directory
		= create next rest world //Created the directory, continue

directoryListing :: SDS FilePath [String] ()
directoryListing = createReadOnlySDSError read
where
	read path iworld = case readDirectory path iworld of
		(Ok files,iworld) = (Ok [f \\ f <- files | f <> "." && f <> ".."],iworld)
		(Error (_,e),iworld) = (Error (exception e),iworld)

