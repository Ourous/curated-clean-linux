implementation module Data.Tuple

import StdFunc
import StdMisc
import Data.Bifunctor
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Traversable
import Control.Applicative

tuple  :: .a .b -> .(.a,.b)
tuple a b = (a,b)

tuple3 :: .a .b .c -> .(.a,.b,.c)
tuple3 a b c = (a,b,c)

appFst:: .(.a -> .c) !(.a,.b) -> (.c,.b)
appFst f (a,b) = (f a,b)

appSnd :: .(.b -> .c) !(.a,.b) -> (.a,.c)
appSnd f (a,b) = (a,f b)

appFst3 :: .(.a -> .d) !(.a,.b,.c) -> (.d,.b,.c)
appFst3 f (a,b,c) = (f a,b,c)

appSnd3 :: .(.b -> .d) !(.a,.b,.c) -> (.a,.d,.c)
appSnd3 f (a,b,c) = (a,f b,c)

appThd3 :: .(.c -> .d) !(.a,.b,.c) -> (.a,.b,.d)
appThd3 f (a,b,c) = (a,b,f c)

swap :: !.(.a, .b) -> .(.b, .a)
swap (a,b) = (b,a)

instance Functor ((,) a)
where
	fmap f (x, y) = (x, f y)

instance Functor ((,,) a b)
where
	fmap f (x, y, z) = (x, y, f z)

instance Functor ((,,,) a b c)
where
	fmap f (x, y, z, a) = (x, y, z, f a)

instance Functor ((,,,,) a b c d)
where
	fmap f (x, y, z, a, b) = (x, y, z, a, f b)

instance Functor ((,,,,,) a b c d e)
where
	fmap f (x, y, z, a, b, c) = (x, y, z, a, b, f c)

instance Semigroup (a, b) | Semigroup a & Semigroup b
where
	mappend :: !(a,b) !(a,b) -> (a,b) | Semigroup a & Semigroup b
	mappend (a1, b1) (a2, b2)  = (mappend a1 a2, mappend b1 b2)

instance Monoid (a, b) | Monoid a & Monoid b
where
	mempty = (mempty, mempty)

instance Semigroup (a, b, c) | Semigroup a & Semigroup b & Semigroup c
where
	mappend :: !(a,b,c) !(a,b,c) -> (a,b,c) | Semigroup a & Semigroup b & Semigroup c
	mappend (a1, b1, c1) (a2, b2, c2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2)

instance Monoid (a, b, c) | Monoid a & Monoid b & Monoid c
where
	mempty = (mempty, mempty, mempty)

instance Semigroup (a, b, c, d) | Semigroup a & Semigroup b & Semigroup c & Semigroup d
where
	mappend :: !(a,b,c,d) !(a,b,c,d) -> (a,b,c,d) | Semigroup a & Semigroup b & Semigroup c & Semigroup d
	mappend (a1, b1, c1, d1) (a2, b2, c2, d2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2, mappend d1 d2)

instance Monoid (a, b, c, d) | Monoid a & Monoid b & Monoid c & Monoid d
where
	mempty = (mempty, mempty, mempty, mempty)

instance Semigroup (a, b, c, d, e) | Semigroup a & Semigroup b & Semigroup c & Semigroup d & Semigroup e
where
	mappend :: !(a,b,c,d,e) !(a,b,c,d,e) -> (a,b,c,d,e) | Semigroup a & Semigroup b & Semigroup c & Semigroup d & Semigroup e
	mappend (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2)  = (mappend a1 a2, mappend b1 b2, mappend c1 c2, mappend d1 d2, mappend e1 e2)

instance Monoid (a, b, c, d, e) | Monoid a & Monoid b & Monoid c & Monoid d & Monoid e
where
	mempty = (mempty, mempty, mempty, mempty, mempty)

instance Foldable ((,) a)
where
	foldMap :: !(a -> b) !(c,a) -> b | Monoid b
	foldMap f (_, y) = f y
	fold x = foldMap id x

	foldr :: !(a -> .b -> .b) .b !(c,a) -> .b
	foldr f z (_, y) = f y z
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

instance Traversable ((,) a)
where
	traverse :: !(a -> b c) !(d,a) -> b (d,c) | Applicative b
	traverse f (x, y) = tuple x <$> f y

instance Bifunctor (,)
where
	bifmap f g t = let (a, b) = t in (f a, g b)

instance Bifunctor ((,,) x)
where
	bifmap f g t = let (x, a, b) = t in (x, f a, g b)

instance Bifunctor ((,,,) x y)
where
	bifmap f g t = let (x, y, a, b) = t in (x, y, f a, g b)

instance Bifunctor ((,,,,) x y z)
where
	bifmap f g t = let (x, y, z, a, b) = t in (x, y, z, f a, g b)
