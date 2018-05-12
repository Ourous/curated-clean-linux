definition module Text.Unicode.Encodings.UTF8

// Ported to Clean from GHC by László Domoszlai, 2013-09-27
// http://hackage.haskell.org/package/utf8-string-0.3.6/docs/src/Codec-Binary-UTF8-String.html
//
// Module      :  Codec.Binary.UTF8.String
// Copyright   :  (c) Eric Mertens 2007
// License     :  BSD3-style (see LICENSE)
// 
// Maintainer:    emertens@galois.com
// Stability   :  experimental
// Portability :  portable
//
// Support for encoding UTF8 Strings to and from @[Word8]@

import StdClass, Text.Unicode

:: UTF8

instance fromUnicode UTF8 
instance toUnicode UTF8 

instance fromString UTF8 
instance toString UTF8 

