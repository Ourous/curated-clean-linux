definition module Control.Monad.RWST

from Control.Applicative import class pure, class <*>, class Applicative
from Control.Monad import class Monad
from Control.Monad.Trans import class MonadTrans
from Data.Functor import class Functor
from Data.Functor.Identity import :: Identity
from Data.Monoid import class Monoid, class Semigroup

// The RWS monad
:: RWS r w s a :== RWST r w s Identity a

// The RWST monad transformer
:: RWST r w s m a = RWST (r s -> m (a, s, w))

instance Functor (RWST r w s m) | Monad m & Monoid w
instance pure (RWST r w s m) | pure m & Monoid w
instance <*> (RWST r w s m) | Monad m & Monoid w
instance Monad (RWST r w s m) | Monad m & Monoid w
instance MonadTrans (RWST r w s) | Monoid w

rws :: (r -> s -> (a, s, w)) -> RWS r w s a
runRWS :: (RWS r w s a) r s -> (a, s, w)
evalRWS :: (RWS r w s a) r s -> (a, w)
execRWS :: (RWS r w s a) r s -> (s, w)
mapRWS :: ((a, s, w) -> (b, s, w`)) (RWS r w s a) -> RWS r w` s b
withRWS :: (r` s -> (r, s)) (RWS r w s a) -> RWS r` w s a

runRWST :: (RWST r w s m a) r s -> m (a, s, w)
evalRWST :: (RWST r w s m a) r s -> m (a, w) | Monad m
execRWST :: (RWST r w s m a) r s -> m (s, w) | Monad m
mapRWST :: ((m (a, s, w)) -> n (b, s, w`)) (RWST r w s m a) -> RWST r w` s n b
withRWST :: (r` -> s -> (r, s)) (RWST r w s m a) -> RWST r` w s m a

// Reader operations
ask :: RWST r w s m r | Monoid w & Monad m 
local :: (r -> r) (RWST r w s m a) -> RWST r w s m a | Monoid w & Monad m
asks :: (r -> a) -> RWST r w s m a | Monoid w & Monad m

// Writer operations
tell :: w -> RWST r w s m () | Monoid w & Monad m
listen :: (RWST r w s m a) -> RWST r w s m (a, w) | Monoid w & Monad m
pass :: (RWST r w s m (a, w -> w)) -> RWST r w s m a | Monoid w & Monad m
listens :: (w -> b) (RWST r w s m a) -> RWST r w s m (a, b)| Monoid w & Monad m
censor :: (w -> w) (RWST r w s m a) -> RWST r w s m a | Monoid w & Monad m

// State operations
get :: RWST r w s m s | Monoid w & Monad m
put :: s -> RWST r w s m () | Monoid w & Monad m
modify :: (s -> s) -> RWST r w s m () | Monoid w & Monad m
gets :: (s -> a) -> RWST r w s m a | Monoid w & Monad m

// Lifting other operations
liftCallCC :: ((((a,s,w) -> m (b,s,w)) -> m (a,s,w)) -> m (a,s,w)) ((a -> RWST r w s m b) -> RWST r w s m a) -> RWST r w s m a | Monoid w
liftCallCC` :: ((((a,s,w) -> m (b,s,w)) -> m (a,s,w)) -> m (a,s,w)) ((a -> RWST r w s m b) -> RWST r w s m a) -> RWST r w s m a | Monoid w
liftCatch :: ((m (a,s,w)) (e -> m (a,s,w)) -> m (a,s,w)) (RWST l w s m a) (e -> RWST l w s m a) -> RWST l w s m a
