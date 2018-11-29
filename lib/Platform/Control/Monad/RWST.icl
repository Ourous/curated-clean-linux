implementation module Control.Monad.RWST

import StdTuple

from Data.Func import $
from StdFunc import o
import Data.Functor.Identity
import Data.Functor
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Applicative

instance Functor (RWST r w s m) | Monad m & Monoid w where
  fmap f m = liftM f m

instance pure (RWST r w s m) | pure m & Monoid w
where
	pure a = RWST \_ s -> pure (a,s,mempty)

instance <*> (RWST r w s m) | Monad m & Monoid w
where
	(<*>) mf mx = ap mf mx

instance Monad (RWST r w s m) | Monad m & Monoid w where
  bind m k
    = RWST (   \r s          -> runRWST m r s
           >>= \(a, s`, w)   -> runRWST (k a) r s`
           >>= \(b, s``, w`) -> pure (b, s``, mappend w w`))

instance MonadTrans (RWST r w s) | Monoid w where
  liftT m = RWST \_ s->m >>= \a->pure (a, s, mempty)

rws :: (r -> s -> (a, s, w)) -> RWS r w s a
rws f = RWST (\r s -> Identity (f r s))

runRWS :: (RWS r w s a) r s -> (a, s, w)
runRWS m r s = runIdentity (runRWST m r s)

evalRWS :: (RWS r w s a) r s -> (a, w)
evalRWS m r s
  # (a, _, w) = runRWS m r s
  = (a, w)

execRWS :: (RWS r w s a) r s -> (s, w)
execRWS m r s
  # (_, s`, w) = runRWS m r s
  = (s`, w)

mapRWS :: ((a, s, w) -> (b, s, w`)) (RWS r w s a) -> RWS r w` s b
mapRWS f m = mapRWST (Identity o f o runIdentity) m

withRWS :: (r` s -> (r, s)) (RWS r w s a) -> RWS r` w s a
withRWS f m = withRWST f m

// The RWST monad transformer
:: RWST r w s m a = RWST (r s -> m (a, s, w))

runRWST :: (RWST r w s m a) r s -> m (a, s, w)
runRWST (RWST f) r s = f r s

evalRWST :: (RWST r w s m a) r s -> m (a, w) | Monad m
evalRWST m r s
  =                 runRWST m r s
  >>= \(a, _, w) -> pure (a, w)

execRWST :: (RWST r w s m a) r s -> m (s, w) | Monad m
execRWST m r s
  =                  runRWST m r s
  >>= \(_, s`, w) -> pure (s`, w)

mapRWST :: ((m (a, s, w)) -> n (b, s, w`)) (RWST r w s m a) -> RWST r w` s n b
mapRWST f m = RWST (\r s -> f (runRWST m r s))

withRWST :: (r` -> s -> (r, s)) (RWST r w s m a) -> RWST r` w s m a
withRWST f m = RWST (\r s -> uncurry (runRWST m) (f r s))

// Reader operations
ask :: RWST r w s m r | Monoid w & Monad m
ask = RWST (\r s -> pure (r, s, mempty))

local :: (r -> r) (RWST r w s m a) -> RWST r w s m a | Monoid w & Monad m
local f m = RWST (\r s -> runRWST m (f r) s)

asks :: (r -> a) -> RWST r w s m a | Monoid w & Monad m
asks f
  =         ask
  >>= \r -> pure (f r)

// Writer operations
tell :: w -> RWST r w s m () | Monoid w & Monad m
tell w = RWST \_ s->pure ((), s,w)

listen :: (RWST r w s m a) -> RWST r w s m (a, w) | Monoid w & Monad m
listen m
  = RWST (   \r s ->        runRWST m r s
         >>= \(a, s`, w) -> pure ((a, w), s`, w))

pass :: (RWST r w s m (a, w -> w)) -> RWST r w s m a | Monoid w & Monad m
pass m
  = RWST (   \r s ->             runRWST m r s
         >>= \((a, f), s`, w) -> pure (a, s`, f w))

listens :: (w -> b) (RWST r w s m a) -> RWST r w s m (a, b)| Monoid w & Monad m
listens f m
  =              listen m
  >>= \(a, w) -> pure (a, f w)

censor :: (w -> w) (RWST r w s m a) -> RWST r w s m a | Monoid w & Monad m
censor f m = pass (m >>= \a -> pure (a, f))

// State operation
get :: RWST r w s m s | Monoid w & Monad m
get = RWST (\_ s -> pure (s, s, mempty))

put :: s -> RWST r w s m () | Monoid w & Monad m
put s = RWST \_ _->pure ((), s, mempty)

modify :: (s -> s) -> RWST r w s m () | Monoid w & Monad m
modify f
  =         get
  >>= \s -> put (f s)

gets :: (s -> a) -> RWST r w s m a | Monoid w & Monad m
gets f
  =         get
  >>= \s -> pure (f s)

// Lifting other operations
liftCallCC :: ((((a,s,w) -> m (b,s,w)) -> m (a,s,w)) -> m (a,s,w)) ((a -> RWST r w s m b) -> RWST r w s m a) -> RWST r w s m a | Monoid w
liftCallCC callCC f = RWST (\r s -> callCC (\c -> runRWST (f (\a -> RWST (\_ _ -> c (a, s, mempty)))) r s))

liftCallCC` :: ((((a,s,w) -> m (b,s,w)) -> m (a,s,w)) -> m (a,s,w)) ((a -> RWST r w s m b) -> RWST r w s m a) -> RWST r w s m a | Monoid w
liftCallCC` callCC f = RWST (\r s -> callCC (\c -> runRWST (f (\a -> (RWST \_ s` -> c (a, s`, mempty)))) r s))

liftCatch :: ((m (a,s,w)) (e -> m (a,s,w)) -> m (a,s,w)) (RWST l w s m a) (e -> RWST l w s m a) -> RWST l w s m a
liftCatch catchError m h = RWST (\r s -> catchError (runRWST m r s) (\e -> runRWST (h e) r s))
