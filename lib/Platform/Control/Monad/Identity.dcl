definition module Control.Monad.Identity

from Control.Applicative import class pure, class <*>, class Applicative
from Control.Monad import class Monad
from Control.Monad.Fix import class MonadFix
from Data.Functor import class Functor
from Data.Functor.Identity import :: Identity, instance Functor Identity,
	instance pure Identity, instance <*> Identity, instance Monad Identity

instance MonadFix Identity
