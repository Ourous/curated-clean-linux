definition module Data.Word8

import StdClass

:: Word8

// Modulo 0xFF to avoid run-time error!
instance fromInt Word8
instance toInt Word8

instance fromChar Word8

instance == Word8
instance < Word8

stringToBytes :: !String -> [Word8]
bytesToString :: ![Word8] -> String