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

iShow :: ![String] !*IWorld -> *IWorld
iShow lines iworld = {iworld & world = show lines iworld.world}

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

generateRandomString :: !Int !*IWorld -> (!String, !*IWorld)
generateRandomString length iworld=:{IWorld|random}
	= (toString (take length [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld & random = drop length random})
