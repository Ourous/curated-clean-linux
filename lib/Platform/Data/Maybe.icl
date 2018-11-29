implementation module Data.Maybe

import StdMaybe
import StdBool
import StdFunctions
import StdMisc
import Data.Functor
import Data.Monoid
import Data.Func
from Data.Foldable import class Foldable(..)
from Data.Traversable import class Traversable(traverse)
import qualified Data.Traversable
import Control.Applicative
import Control.Monad, Control.Monad.Trans
import Data.GenEq

instance Functor Maybe where fmap f m = mapMaybe f m

instance pure Maybe where pure x = Just x

instance <*> Maybe
where
	(<*>) Nothing  _  = Nothing
	(<*>) (Just f) ma = fmap f ma

instance *> Maybe
where
	(*>) (Just _) m = m
	(*>) _        _ = Nothing

instance <* Maybe
where
	(<*) Nothing _  = Nothing
	(<*) m (Just _) = m
	(<*) _ _        = Nothing

instance Alternative Maybe
where
	empty           = Nothing
	(<|>) Nothing r = r
	(<|>) l       _ = l

instance Monad Maybe
where
	bind (Just x) k = k x
	bind Nothing  _ = Nothing

instance MonadPlus Maybe
where
	mzero = Nothing
	mplus Nothing ys = ys
	mplus xs      _  = xs

instance Semigroup (Maybe a) | Semigroup a
where
	mappend :: !(Maybe a) !(Maybe a) -> Maybe a | Semigroup a
	mappend Nothing   m         = m
	mappend m         Nothing   = m
	mappend (Just m1) (Just m2) = Just (mappend m1 m2)
	mappend _         _         = abort "impossible case in mappend_Maybe\n"

instance Monoid (Maybe a)
where
	mempty = Nothing

instance Foldable Maybe where
	fold x = foldMap id x
	foldMap f x = foldr (mappend o f) mempty x
	foldr _ z Nothing = z
	foldr f z (Just x) = f x z
	foldr` f z0 xs = foldl f` id xs z0
	where f` k x z = k (f x z)

	foldl _ z Nothing = z
	foldl f z (Just x) = f z x
	foldl` f z0 xs = foldr f` id xs z0
	where f` x k z = k (f z x)
	foldr1 f xs = fromMaybe (abort "foldr1: empty structure") (foldr mf Nothing xs)
	where
		mf x Nothing = Just x
		mf x (Just y) = Just (f x y)
	foldl1 f xs = fromMaybe (abort "foldl1: empty structure") (foldl mf Nothing xs)
	where
		mf Nothing y = Just y
		mf (Just x) y = Just (f x y)

instance Traversable Maybe
where
	traverse _ Nothing = pure Nothing
	traverse f (Just x) = Just <$> f x
	sequenceA f = traverse id f
	mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
	sequence x = 'Data.Traversable'.mapM id x

derive gEq Maybe

mapMaybe :: .(.x -> .y) !(Maybe .x) -> Maybe .y
mapMaybe f (Just x) = Just (f x)
mapMaybe _ _        = Nothing

maybe :: w:b v:(.a -> w:b) !.(Maybe .a) -> w:b
maybe x _ Nothing  = x
maybe _ f (Just x) = f x

maybeSt :: *st (.a *st -> *st) !(Maybe .a) -> *st
maybeSt st _ Nothing  = st
maybeSt st f (Just x) = f x st

fromMaybe :: .a !(Maybe .a) -> .a
fromMaybe x mb = maybe x id mb

runMaybeT :: !(MaybeT m a) -> m (Maybe a)
runMaybeT (MaybeT f) = f

mapMaybeT :: !((m (Maybe a)) -> n (Maybe b)) !(MaybeT m a) -> MaybeT n b
mapMaybeT f m = MaybeT $ f $ runMaybeT m

instance Functor (MaybeT m) | Functor m where
	fmap f m = mapMaybeT (fmap $ fmap f) m

instance pure (MaybeT m) | pure m where pure x = MaybeT (pure (Just x))

instance <*> (MaybeT m) | Monad m where
	(<*>) mf mx = MaybeT $
		runMaybeT mf >>= \mb_f ->
		case mb_f of
			Nothing = pure Nothing
			Just f  =
				runMaybeT mx >>= \mb_x ->
				case mb_x of
					Nothing = pure Nothing
					Just x  = pure $ Just $ f x

instance Alternative (MaybeT m) | Monad m where
	empty = MaybeT $ return Nothing

	<|> x y = MaybeT $
		runMaybeT x >>= \v ->
		case v of
			Nothing -> runMaybeT y
			Just _  -> return v

instance Monad (MaybeT m) | Monad m where
	bind x f = MaybeT $
		runMaybeT x >>= \v ->
		case v of
			Nothing -> return Nothing
			Just y  -> runMaybeT $ f y

instance MonadTrans MaybeT
where
	liftT :: !(a b) -> MaybeT a b | Monad a
	liftT m = MaybeT $ liftM Just m
