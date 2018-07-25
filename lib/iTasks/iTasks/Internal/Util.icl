implementation module iTasks.Internal.Util

import StdBool, StdChar, StdList, StdFile, StdMisc, StdArray, StdString, StdTuple, StdFunc, StdGeneric, StdOrdList
import Data.Maybe, Data.Tuple, Data.Func, System.Time, System.OS, Text, System.FilePath, System.Directory, Text.GenJSON, Data.Error, Data.GenEq
import Data.Error, System.OSError, System.File
import iTasks.Internal.IWorld
import iTasks.WF.Definition
from iTasks.Internal.IWorld 		import :: IWorld{current}, :: TaskEvalState
from iTasks.Extensions.DateTime import :: Date{..}, :: Time{..}, :: DateTime(..)
import qualified Control.Monad as M
import qualified Data.Map as DM
from Data.Map import :: Map
	
show :: ![String] !*World -> *World
show lines world
	# (console,world)	= stdio world
	# console			= seqSt (\s c -> fwrites (s +++ "\n") c) lines console
	# (_,world)			= fclose console world
	= world
	
tmToDateTime :: !Tm -> DateTime
tmToDateTime tm
	= {DateTime| day = tm.Tm.mday, mon = 1 + tm.Tm.mon, year = 1900 + tm.Tm.year
	  ,hour = tm.Tm.hour, min = tm.Tm.min, sec= tm.Tm.sec}

toCanonicalPath	:: !FilePath !*World -> (!FilePath,!*World)
toCanonicalPath path world
	| isAbsolute path = (canonicalize path,world)
	| otherwise
		= case getCurrentDirectory world of
			(Ok curDir,world)	= (canonicalize (curDir</>path), world)
			(_,world)		= (canonicalize path,world)
where
	isAbsolute path = IF_POSIX_OR_WINDOWS (startsWith {pathSeparator} path) (size path >= 2 && isUpper path.[0] && path.[1] == ':')

	canonicalize path = join {pathSeparator} (undot [] (split {pathSeparator} path))

	undot acc []				= reverse acc
	undot []  ["..":ds]			= undot [] ds
	undot [_:acc] ["..":ds]			= undot acc ds
	undot acc [".":ds]			= undot acc ds
	undot [] ["":ds]			= undot [""] ds //Only allowed at the beginning
	undot acc ["":ds]			= undot acc ds
	undot acc [d:ds] 			= undot [d:acc] ds

recursiveDelete :: FilePath *World -> *(MaybeOSError (), *World)
recursiveDelete fp w
	# (mfi, w) = getFileInfo fp w
	| isError mfi = (liftError mfi, w)
	| (fromOk mfi).directory
		# (mdir, w) = readDirectory fp w
		| isError mdir = (liftError mdir, w)
		# (merr, w) = mapSt (\c->recursiveDelete (fp </> c))
			(filter (\r->r <> "." && r <> "..") (fromOk mdir)) w
		# merr = 'M'.sequence merr
		| isError merr = (liftError merr, w)
		= removeDirectory fp w
	= deleteFile fp w

ensureDir :: FilePath *World -> (!Bool,*World)
ensureDir path world = let [b:p] = split {pathSeparator} path in create [b] p world
where
	create _ [] world = (True,world)
	create base [dir:rest] world
		# next = base ++ [dir]
		# path = join {pathSeparator} next
		# (exists,world) = fileExists path world
		| exists = create next rest world //This part exists, continue
		# (res, world) = createDirectory path world 
		| isError res = (False,world) //Can't create the directory
		= create next rest world //Created the directory, continue

(>-=) infixl 1 :: (*env -> *(MaybeError e a, *env)) (a -> *(*env -> (MaybeError e b, *env))) *env -> (MaybeError e b, *env)
(>-=) a b w
	# (mca, w) = a w
	= case mca of
		Error e = (Error e, w)
		Ok a = (b a) w

liftIWorld :: (*World -> *(.a, *World)) *IWorld -> *(.a, *IWorld)
liftIWorld f iworld
# (a, world) = f iworld.world
= (a, {iworld & world=world})

apIWTransformer :: *env (*env -> *(MaybeError TaskException (TaskResult a), *env)) -> *(TaskResult a, *env)
apIWTransformer iw f = case f iw of
	(Error e, iw) = (ExceptionResult e, iw)
	(Ok tv, iw) = (tv, iw)
