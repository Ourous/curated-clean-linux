definition module Control.Monad.Trans

from Control.Monad import class Monad
from Control.Applicative import class pure, class <*>, class Applicative
from Data.Functor import class Functor

class MonadTrans t where
  liftT :: (m a) -> t m a | Monad m
