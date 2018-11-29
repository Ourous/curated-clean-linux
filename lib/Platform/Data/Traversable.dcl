definition module Data.Traversable

from Control.Applicative import class pure, class <*>, class Applicative
from Control.Monad import class Monad
from Data.Functor import class Functor
from Data.Foldable import class Foldable
from Data.Monoid import class Monoid, class Semigroup

// Functors representing data structures that can be traversed from left to
// right.
//
// Minimal complete definition: 'traverse' or 'sequenceA'.
//
// A definition of 'traverse' must satisfy the following laws:
//
// [/naturality/]
//   @t . 'traverse' f = 'traverse' (t . f)@
//   for every applicative transformation @t@
//
// [/identity/]
//   @'traverse' Identity = Identity@
//
// [/composition/]
//   @'traverse' (Compose . 'fmap' g . f) = Compose . 'fmap' ('traverse' g) . 'traverse' f@
//
// A definition of 'sequenceA' must satisfy the following laws:
//
// [/naturality/]
//   @t . 'sequenceA' = 'sequenceA' . 'fmap' t@
//   for every applicative transformation @t@
//
// [/identity/]
//   @'sequenceA' . 'fmap' Identity = Identity@
//
// [/composition/]
//   @'sequenceA' . 'fmap' Compose = Compose . 'fmap' 'sequenceA' . 'sequenceA'@
//
// where an /applicative transformation/ is a function
//
// @t :: (Applicative f, Applicative g) => f a -> g a@
//
// preserving the 'Applicative' operations, i.e.
//
//  * @t ('pure' x) = 'pure' x@
//
//  * @t (x '<*>' y) = t x '<*>' t y@
//
// and the identity functor @Identity@ and composition of functors @Compose@
// are defined as
//
// >   newtype Identity a = Identity a
// >
// >   instance Functor Identity where
// >     fmap f (Identity x) = Identity (f x)
// >
// >   instance Applicative Indentity where
// >     pure x = Identity x
// >     Identity f <*> Identity x = Identity (f x)
// >
// >   newtype Compose f g a = Compose (f (g a))
// >
// >   instance (Functor f, Functor g) => Functor (Compose f g) where
// >     fmap f (Compose x) = Compose (fmap (fmap f) x)
// >
// >   instance (Applicative f, Applicative g) => Applicative (Compose f g) where
// >     pure x = Compose (pure (pure x))
// >     Compose f <*> Compose x = Compose ((<*>) <$> f <*> x)
//
// (The naturality law is implied by parametricity.)
//
// Instances are similar to 'Functor', e.g. given a data type
//
// > data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
//
// a suitable instance would be
//
// > instance Traversable Tree where
// >    traverse f Empty = pure Empty
// >    traverse f (Leaf x) = Leaf <$> f x
// >    traverse f (Node l k r) = Node <$> traverse f l <*> f k <*> traverse f r
//
// This is suitable even for abstract types, as the laws for '<*>'
// imply a form of associativity.
//
// The superclass instances should satisfy the following:
//
//  * In the 'Functor' instance, 'fmap' should be equivalent to traversal
//    with the identity applicative functor ('fmapDefault').
//
//  * In the 'Foldable' instance, 'Data.Foldable.foldMap' should be
//    equivalent to traversal with a constant applicative functor
//    ('foldMapDefault').
//
class Traversable t | Functor t & Foldable t where
    // Map each element of a structure to an action, evaluate
    // these actions from left to right, and collect the results.
    traverse :: (a -> f b) !(t a) -> f (t b) | Applicative f

    // Evaluate each action in the structure from left to right,
    // and collect the results.
    sequenceA :: !(t (f a)) -> f (t a) | Applicative f

    // Map each element of a structure to a monadic action, evaluate
    // these actions from left to right, and collect the results.
    mapM :: (a -> m b) !(t a) -> m (t b) | Monad m

    // Evaluate each monadic action in the structure from left to right,
    // and collect the results.
    sequence :: !(t (m a)) -> m (t a) | Monad m

for :: !(t a) (a -> f b) -> f (t b) | Traversable t & Applicative f
forM :: !(t a) (a -> m b) -> m (t b) | Traversable t & Monad m
mapAccumL :: (b -> (*s -> *(c, *s))) !(t b) *s -> *(t c, *s) | Traversable t
mapAccumR :: (b -> (*s -> *(c, *s))) !(t b) *s -> *(t c, *s) | Traversable t
fmapDefault :: (a -> b) !(t a) -> t b | Traversable t
foldMapDefault :: (a -> m) !(t a) -> m | Traversable t & Monoid m
