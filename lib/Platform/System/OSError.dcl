definition module System.OSError

import Data.Error

:: OSErrorCode :== Int
:: OSErrorMessage :== String

:: OSError :== (OSErrorCode, OSErrorMessage)
:: MaybeOSError a :== MaybeError OSError a
:: MaybeOSErrorCode a :== MaybeError OSErrorCode a

getLastOSError :: *w -> (MaybeOSError .a, *w)

getLastOSErrorCode :: *w -> (MaybeOSErrorCode .a, *w)

osErrorCodeToOSError :: OSErrorCode -> OSError

