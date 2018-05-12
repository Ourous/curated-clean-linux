definition module Data.Functor.Identity

from Data.Functor import class Functor
from Control.Monad import class Monad
from Control.Applicative import class Applicative

:: Identity a = Identity a

runIdentity :: (Identity .a) -> .a

instance Functor Identity
instance Applicative Identity
instance Monad Identity

