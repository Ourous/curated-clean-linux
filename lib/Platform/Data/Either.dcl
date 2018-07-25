definition module Data.Either
/**
* This module defines the "Either" type to represent binary choice.
* Clean's generics define a similar type EITHER, but this should only be
* used inside generic functions, since most generic functions treat this
* type in a special way which may lead to strange behavior.
*/
from Control.Applicative import class Applicative, class *>, class <*, class Alternative
from Control.Monad import class Monad
from Data.Functor import class Functor
from Data.Monoid import class Monoid, class Semigroup
from Data.Foldable import class Foldable
from Data.Traversable import class Traversable
from Data.Bifunctor import class Bifunctor

:: Either a b = Left a | Right b

instance Functor (Either a)

instance Applicative (Either e)

// Making use of the type information allows for faster sequencing operators.
// See the documentation on *> in Control.Applicative.
instance *> (Either e)
instance <* (Either e)

instance Monad (Either e)

instance Foldable (Either a)
instance Traversable (Either a)

instance Bifunctor Either

instance Alternative (Either m) | Monoid m

either    :: .(.a -> .c) .(.b -> .c) !(Either .a .b) -> .c
lefts     :: .[Either .a .b] -> .[.a]
rights    :: .[Either .a .b] -> .[.b]
fromLeft :: .a (Either .a .b) -> .a
fromRight :: .b (Either .a .b) -> .b
