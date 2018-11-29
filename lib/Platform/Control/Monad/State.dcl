definition module Control.Monad.State

from Control.Monad import class Monad
from Control.Applicative import class pure, class <*>, class Applicative, class *>, class <*, class Alternative
from Data.Functor import class Functor
from Data.Functor.Identity import :: Identity
from Control.Monad.Trans import class MonadTrans

:: StateT s m a = StateT (s -> m .(a, s))

:: State s a :== StateT s Identity a

state      :: (s -> .(a, s)) -> StateT s m a | pure m
getState   :: StateT s m s | Monad m
put        :: s -> StateT s m () | Monad m
modify     :: (s -> s) -> StateT s m () | Monad m
gets       :: (s -> a) -> StateT s m a | Monad m
runState   :: .(StateT s Identity a) s -> (a, s)
runStateT  :: !u:(StateT v:s m a) v:s -> m w:(a, v:s), [w <= v,u <= w]
evalState  :: .(StateT s Identity a) s -> a
evalStateT :: !.(StateT s m a) s -> m a | Monad m
execState  :: .(StateT s Identity a) s -> s
execStateT :: !.(StateT s m a) s -> m s | Monad m
mapState   :: ((a, s) -> (b, s)) .(StateT s Identity a) -> StateT s Identity b
mapStateT  :: ((m (a, s)) -> (m` (b, s))) .(StateT s m a) -> StateT s m` b
withState  :: (s -> s) .(StateT s m c) -> StateT s m c
withStateT :: (s -> s) .(StateT s m c) -> StateT s m c

transformStateT :: (s2 -> s1) (a s1 -> (a, s2)) (StateT s1 m a) -> (StateT s2 m a) | Functor m

instance Functor (StateT s m) | Functor m
instance pure (StateT s m) | pure m
instance <*> (StateT s m) | Monad m
instance *> (StateT s m) | Monad m
instance <* (StateT s m) | Monad m
instance Alternative (StateT s m) | Alternative m
instance Monad (StateT s m) | Monad m
instance MonadTrans (StateT s)
