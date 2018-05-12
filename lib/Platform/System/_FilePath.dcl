definition module System._FilePath

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

getFullPathName :: !String !*World -> (!MaybeOSError String, !*World)
