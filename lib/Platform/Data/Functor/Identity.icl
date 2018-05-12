implementation module Data.Functor.Identity

from Data.Functor import class Functor
from Control.Applicative import class Applicative
from Control.Monad import class Monad

:: Identity a = Identity a

runIdentity :: (Identity .a) -> .a
runIdentity (Identity a) = a

instance Functor Identity where
  fmap f (Identity m) = Identity (f m)

instance Applicative Identity where
  pure x = Identity x
  (<*>) (Identity f) (Identity x) = Identity (f x)

instance Monad Identity where
  bind (Identity m) k  = k m

