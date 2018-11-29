implementation module Control.Monad.Trans

from Data.Functor import class Functor
import Control.Applicative
from Control.Monad import class Monad

class MonadTrans t where
  liftT :: (m a) -> t m a | Monad m
