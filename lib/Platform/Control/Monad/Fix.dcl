definition module Control.Monad.Fix

from Control.Monad import class Monad
from Control.Applicative import class pure, class <*>, class Applicative
from Data.Functor import class Functor
from Data.Maybe import :: Maybe

class MonadFix m | Monad m where
  mfix :: (a -> m a) -> m a

instance MonadFix Maybe where
  mfix :: !(a -> Maybe a) -> Maybe a

instance MonadFix []
