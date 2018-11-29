implementation module Data.Functor.Identity

import Data.Functor
import Control.Applicative
import Control.Monad

:: Identity a = Identity a

runIdentity :: (Identity .a) -> .a
runIdentity (Identity a) = a

instance Functor Identity where
  fmap f (Identity m) = Identity (f m)

instance pure Identity
where
	pure x = Identity x

instance <*> Identity
where
	(<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  bind (Identity m) k  = k m

