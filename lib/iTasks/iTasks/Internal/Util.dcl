definition module iTasks.Internal.Util

from StdClass import class Eq
from Data.Error import :: MaybeErrorString, :: MaybeError
from Data.Map import :: Map
from Data.Maybe import :: Maybe
from System.FilePath import :: FilePath
from iTasks.Extensions.DateTime import :: DateTime
from StdOverloaded import class <
from System.Time import :: Tm
from System.OSError import :: OSError, :: OSErrorCode, :: OSErrorMessage, :: MaybeOSError

show :: ![String] !*World -> *World

tmToDateTime :: !Tm -> DateTime

//Path conversion
toCanonicalPath	:: !FilePath !*World -> (!FilePath,!*World)

//Recursive delete of files
recursiveDelete :: FilePath *World -> *(MaybeOSError (), *World)
//Create a directory and its parent directories
ensureDir :: FilePath *World -> (!Bool,*World)

