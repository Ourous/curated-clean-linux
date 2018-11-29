implementation module Data.Traversable

/**
 * Ported from Haskell's Data.Traversable by Jurriën Stutterheim 15-08-2014
 */

import Control.Applicative
import Data.Foldable
import Data.Functor
from Data.List import instance Functor []
from Control.Monad import class Monad
from Data.Monoid import class Monoid
import Data.Tuple
from StdFunc import o, id, flip

// TODO Cleanify
//instance Ix i => Traversable (Array i) where
    //traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

// general functions

// 'for' is 'traverse' with its arguments flipped.
for :: !(t a) (a -> f b) -> f (t b) | Traversable t & Applicative f
for x f = flip traverse x f

// 'forM' is 'mapM' with its arguments flipped.
forM :: !(t a) (a -> m b) -> m (t b) | Traversable t & Monad m
forM x f = flip mapM x f

/// left-to-right state transformer
:: StateL s a = StateL .(s -> *(a, s))

runStateL (StateL f) = f

instance Functor (StateL *s) where
    fmap f (StateL k) = StateL g
      where
      g s
        # (v, s) = k s
        = (f v, s)

instance pure (StateL *s)
where
    pure x = StateL (\s -> (x, s))

instance <*> (StateL *s)
where
    (<*>) (StateL kf) (StateL kv) = StateL f
      where
      f s
        # (f, s) = kf s
        # (v, s) = kv s
        = (f v, s)

// The 'mapAccumL' function behaves like a combination of 'fmap'
// and 'foldl'; it applies a function to each element of a structure,
// passing an accumulating parameter from left to right, and returning
// a final value of this accumulator together with the new structure.
mapAccumL :: (b -> (*s -> *(c, *s))) !(t b) *s -> *(t c, *s) | Traversable t
mapAccumL f t s = runStateL (traverse (StateL o f) t) s

// right-to-left state transformer
:: StateR s a = StateR .(s -> *(a, s))

runStateR (StateR f) = f

instance Functor (StateR *s) where
    fmap f (StateR k) = StateR g
      where
      g s
        # (v, s) = k s
        = (f v, s)

instance pure (StateR *s) where pure x = StateR \s -> (x, s)

instance <*> (StateR *s)
where
    (<*>) (StateR kf) (StateR kv) = StateR f
      where
      f s
        # (v, s) = kv s
        # (f, s) = kf s
        = (f v, s)

// The 'mapAccumR' function behaves like a combination of 'fmap'
// and 'foldr'; it applies a function to each element of a structure,
// passing an accumulating parameter from right to left, and returning
// a final value of this accumulator together with the new structure.
mapAccumR :: (b -> (*s -> *(c, *s))) !(t b) *s -> *(t c, *s) | Traversable t
mapAccumR f t s = runStateR (traverse (StateR o f) t) s

// This function may be used as a value for `fmap` in a `Functor`
// instance, provided that 'traverse' is defined. (Using
// `fmapDefault` with a `Traversable` instance defined only by
// 'sequenceA' will result in infinite recursion.)
fmapDefault :: (a -> b) !(t a) -> t b | Traversable t
fmapDefault f x = getId (traverse (Id o f) x)

// This function may be used as a value for `Data.Foldable.foldMap`
// in a `Foldable` instance.
foldMapDefault :: (a -> m) !(t a) -> m | Traversable t & Monoid m
foldMapDefault f x = getConst (traverse (Const o f) x)

// local instances

:: Id a = Id a

getId (Id x) =x

instance Functor Id where
    fmap f (Id x) = Id (f x)

instance pure Id where pure x = Id x
instance <*> Id where (<*>) (Id f) (Id x) = Id (f x)
