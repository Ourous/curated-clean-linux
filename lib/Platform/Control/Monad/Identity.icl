implementation module Control.Monad.Identity

from StdFunc import o
import Data.Func
import Data.Functor
import Data.Functor.Identity
import Control.Monad
import Control.Monad.Fix

instance MonadFix Identity where
  mfix f = Identity (fix (runIdentity o f))
