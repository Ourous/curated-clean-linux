definition module System.Directory

from System.FilePath import :: FilePath
from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

createDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)

removeDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)

readDirectory :: !FilePath !*w -> (!MaybeOSError [FilePath], !*w)

getCurrentDirectory :: !*w -> (!MaybeOSError FilePath, !*w)

setCurrentDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
