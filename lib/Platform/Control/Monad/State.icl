implementation module Control.Monad.State

import StdTuple
import Control.Monad
import Data.Functor
import Control.Applicative
import Data.Functor.Identity
import Control.Monad.Trans

from StdFunc import o
from StdTuple import fst, snd

instance Functor (StateT s m) | Monad m where
  fmap f m = StateT (\s -> fmap (\(a, s`) -> (f a, s`)) (runStateT m s))

instance Applicative (StateT s m) | Monad m where
  pure a = state (\s -> (a, s))
  (<*>) sf sa = ap sf sa

instance Alternative (StateT s m) | Alternative m where
  empty = StateT (const empty)
  (<|>) fa fb = StateT \cs->runStateT fa cs <|> runStateT fb cs

instance Monad (StateT s m) | Monad m where
  bind m k = StateT (\s -> (runStateT m s >>= \(a, s`) -> runStateT (k a) s`))

instance MonadTrans (StateT s) where
  liftT m = StateT (\s -> m >>= \a -> pure (a, s))

state :: (s -> .(a, s)) -> StateT s m a | Monad m
state f = StateT (\s -> pure (f s))

getState :: StateT s m s | Monad m
getState = state (\s -> (s, s))

put :: s -> StateT s m () | Monad m
put s = state (\_ -> ((), s))

modify :: (s -> s) -> StateT s m () | Monad m
modify f = state (\s -> ((), f s))

gets :: (s -> a) -> StateT s m a | Monad m
gets f = state (\s -> (f s, s))

runState :: .(StateT s Identity a) s -> (a, s)
runState m s = runIdentity (runStateT m s)

runStateT :: u:(StateT v:s m a) v:s -> m w:(a, v:s), [w <= v,u <= w]
runStateT (StateT f) s = f s

evalState :: .(StateT s Identity a) s -> a
evalState m s = fst (runState m s)

evalStateT :: .(StateT s m a) s -> m a | Monad m
evalStateT m s = runStateT m s >>= \(a, _) -> pure a

execState :: .(StateT s Identity a) s -> s
execState m s = snd (runState m s)

execStateT :: .(StateT s m a) s -> m s | Monad m
execStateT m s = runStateT m s >>= \(_, s`) -> pure s`

mapState :: ((a, s) -> (b, s)) .(StateT s Identity a) -> StateT s Identity b
mapState f st = mapStateT (Identity o f o runIdentity) st

mapStateT :: ((m (a, s)) -> (m` (b, s))) .(StateT s m a) -> StateT s m` b
mapStateT f m = StateT (f o runStateT m)

withState :: (s -> s) .(StateT s m c) -> StateT s m c
withState f m = withStateT f m

withStateT :: (s -> s) .(StateT s m c) -> StateT s m c
withStateT f m = StateT (runStateT m o f)

transformStateT :: (s2 -> s1) (a s1 -> (a, s2)) (StateT s1 m a) -> (StateT s2 m a) | Functor m
transformStateT to fro s = StateT (fmap (uncurry fro) o runStateT s o to)
