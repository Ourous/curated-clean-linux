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

definition module Control.Category

// | A class for categories.
//   cid and (O) must form a monoid.
class Category cat where
    // | the identity morphism
    cid :: cat a a

    // | morphism composition
    (O) infixr 9 :: (cat b c) (cat a b) -> cat a c

instance Category (->)

// | Right-to-left composition
(<<<) infixr 1 :: (cat b c) (cat a b) -> cat a c | Category cat

// | Left-to-right composition
(>>>) infixr 1 :: (cat a b) (cat b c) -> cat a c | Category cat
