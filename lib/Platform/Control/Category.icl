//---------------------------------------------------------------------------
// |
// Module      :  Control.Category
// Copyright   :  (c) Ashley Yakeley 2007
// License     :  BSD-style (see the LICENSE file in the distribution)
//
// Maintainer  :  ashley@semantic.org
// Stability   :  experimental
// Portability :  portable

// http://ghc.haskell.org/trac/ghc/ticket/1773

implementation module Control.Category

import StdFunc

instance Category (->) where
    cid = \x -> x
    (O) f g = f o g

// | Right-to-left composition
(<<<) infixr 1 :: (cat b c) (cat a b) -> cat a c | Category cat
(<<<) f g = f O g

// | Left-to-right composition
(>>>) infixr 1 :: (cat a b) (cat b c) -> cat a c | Category cat
(>>>) f g = g O f
