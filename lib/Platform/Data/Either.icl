implementation module Data.Either

from StdEnv import flip, id, o
from StdMisc import abort
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.Maybe
import Data.Monoid
from Data.Foldable import class Foldable(foldMap,foldl,foldr)
from Data.Traversable import class Traversable(traverse)
import qualified Data.Traversable as T
import Data.Bifunctor

instance Functor (Either a) where
  fmap f (Left l)  = Left l
  fmap f (Right r) = Right (f r)

instance Applicative (Either e) where
  pure x        = Right x
  (<*>) (Left  e) _ = Left e
  (<*>) (Right f) r = fmap f r

instance *> (Either e)
where
	*> (Right _) e = e
	*> (Left l)  _ = Left l

instance <* (Either e)
where
	<* (Left l)  _         = Left l
	<* _         (Left l)  = Left l
	<* x         _         = x

instance Monad (Either e)
where
	bind (Left  l) _ = Left l
	bind (Right r) k = k r

instance Foldable (Either a)
where
	foldMap _ (Left _) = mempty
	foldMap f (Right y) = f y
	fold x = foldMap id x

	foldr _ z (Left _) = z
	foldr f z (Right y) = f y z
	foldr` f z0 xs = foldl f` id xs z0
	where f` k x z = k (f x z)
	foldl f z t = appEndo (getDual (foldMap (Dual o Endo o flip f) t)) z
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

instance Traversable (Either a)
where
	traverse _ (Left x) = pure (Left x)
	traverse f (Right y) = Right <$> f y
	sequenceA f = traverse id f
	mapM f x = unwrapMonad (traverse (WrapMonad o f) x)
	sequence x = 'T'.mapM id x

instance Bifunctor Either
where
	bifmap f _ (Left a) = Left (f a)
	bifmap _ g (Right b) = Right (g b)
	first f d = bifmap f id d
	second g d = bifmap id g d

either :: .(.a -> .c) .(.b -> .c) !(Either .a .b) -> .c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y
