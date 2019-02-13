definition module iTasks.Internal.Util

from iTasks.WF.Definition import :: TaskResult
from iTasks.WF.Definition import :: TaskException
from StdClass import class Eq
from Data.Error import :: MaybeErrorString, :: MaybeError
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from System.FilePath import :: FilePath
from iTasks.Extensions.DateTime import :: DateTime
from iTasks.Internal.IWorld import :: IWorld
from StdOverloaded import class <
from System.Time import :: Tm
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage, :: MaybeOSError

show :: ![String] !*World -> *World

iShow :: ![String] !*IWorld -> *IWorld

tmToDateTime :: !Tm -> DateTime

//Path conversion
toCanonicalPath	:: !FilePath !*World -> (!FilePath,!*World)

//Bind a possibly failing iworld function to another
(>-=) infixl 1 :: (*env -> *(MaybeError e a, *env)) (a -> *(*env -> (MaybeError e b, *env))) *env -> (MaybeError e b, *env)

//Lift a world function to an iworld function
liftIWorld :: (*World -> *(.a, *World)) *IWorld -> *(.a, *IWorld)

//Apply an IWorld transformer and transform the result to a taskresult
apIWTransformer :: *env (*env -> *(MaybeError TaskException (TaskResult a), *env)) -> *(TaskResult a, *env)

generateRandomString :: !Int !*IWorld -> (!String, !*IWorld)
