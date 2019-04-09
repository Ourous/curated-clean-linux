implementation module Data.Either

from StdEnv import flip, id, o, const, class == (..)
from StdMisc import abort
import Control.Applicative
import Data.Monoid
import Data.Functor
import Data.Maybe
import Data.Monoid
from Data.Foldable import class Foldable(foldMap,foldl,foldr)
from Data.Traversable import class Traversable(..)
import Data.Bifunctor
import Data.GenEq

instance == (Either a b) | == a & == b where
	== (Left x)  (Left y)  = x == y
	== (Right x) (Right y) = x == y
	== _         _         = False

instance Functor (Either a) where
	fmap f (Left l)  = Left l
	fmap f (Right r) = Right (f r)

instance pure (Either e)
where
	pure x = Right x

instance <*> (Either e)
where
	(<*>) (Left  e) _ = Left e
	(<*>) (Right f) r = fmap f r

instance *> (Either e)
where
	(*>) (Right _) e = e
	(*>) (Left l)  _ = Left l

instance <* (Either e)
where
	(<*) (Left l)  _         = Left l
	(<*) _         (Left l)  = Left l
	(<*) x         _         = x

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

instance Bifunctor Either
where
	bifmap :: (a -> c) (b -> d) !(Either a b) -> Either c d
	bifmap f _ (Left a) = Left (f a)
	bifmap _ g (Right b) = Right (g b)

instance Alternative (Either m) | Monoid m
where
	empty = Left mempty
	(<|>) fa fb = either (\e->either (Left o mappend e) Right fb) Right fa

derive gEq Either

either :: .(.a -> .c) .(.b -> .c) !(Either .a .b) -> .c
either f _ (Left x)     =  f x
either _ g (Right y)    =  g y

lefts :: !.[Either .a .b] -> .[.a]
lefts l = [l\\(Left l)<-l]

rights :: !.[Either .a .b] -> .[.b]
rights l = [l\\(Right l)<-l]

fromLeft :: .a !(Either .a .b) -> .a
fromLeft a e = either id (const a) e

fromRight :: .b !(Either .a .b) -> .b
fromRight a e = either (const a) id e
