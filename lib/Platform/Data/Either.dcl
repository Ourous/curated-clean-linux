definition module Data.Either
/**
* This module defines the "Either" type to represent binary choice.
* Clean's generics define a similar type EITHER, but this should only be
* used inside generic functions, since most generic functions treat this
* type in a special way which may lead to strange behavior.
*/
from StdOverloaded import class ==
from Control.Applicative import class pure, class <*>, class Applicative, class *>, class <*, class Alternative
from Control.Monad import class Monad
from Data.Functor import class Functor
from Data.Monoid import class Monoid, class Semigroup
from Data.Foldable import class Foldable
from Data.Traversable import class Traversable
from Data.Bifunctor import class Bifunctor
from Data.GenEq import generic gEq

:: Either a b = Left a | Right b

instance == (Either a b) | == a & == b
instance Functor (Either a)
instance pure (Either e)
instance <*> (Either e)
instance *> (Either e)
instance <* (Either e)
instance Monad (Either e)

instance Foldable (Either a)
instance Traversable (Either a)

instance Bifunctor Either
where
	bifmap :: (a -> c) (b -> d) !(Either a b) -> Either c d

instance Alternative (Either m) | Monoid m

derive gEq Either

either    :: .(.a -> .c) .(.b -> .c) !(Either .a .b) -> .c
lefts     :: !.[Either .a .b] -> .[.a]
rights    :: !.[Either .a .b] -> .[.b]
fromLeft  :: .a !(Either .a .b) -> .a
fromRight :: .b !(Either .a .b) -> .b
