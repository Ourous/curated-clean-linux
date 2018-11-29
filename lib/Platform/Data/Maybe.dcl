definition module Data.Maybe
/**
* This module extends the basic functions on Maybe values from StdMaybe 
*/

import StdMaybe 

from StdOverloaded import class ==
from Data.Functor import class Functor
from Control.Applicative import class pure, class <*>, class Applicative, class *>, class <*, class Alternative
from Control.Monad import class Monad, class MonadPlus
from Control.Monad.Trans import class MonadTrans
from Data.Monoid import class Semigroup, class Monoid
from Data.Foldable import class Foldable
from Data.Traversable import class Traversable
from Data.GenEq import generic gEq

instance Functor Maybe
instance pure Maybe
instance <*> Maybe
instance *> Maybe
instance <* Maybe
instance Alternative Maybe
instance Monad Maybe
instance MonadPlus Maybe

instance Semigroup (Maybe a) | Semigroup a
where
	mappend :: !(Maybe a) !(Maybe a) -> Maybe a | Semigroup a
instance Monoid (Maybe a)
instance Foldable Maybe
instance Traversable Maybe

derive gEq Maybe

/**
 * Apply a function to the the contents of a Just value and directly return
 * the result, or return a default value if the argument is a Nothing value.
 */
maybe :: w:b v:(.a -> w:b) !.(Maybe .a) -> w:b

/**
 * Apply a function to the the contents of a Just value and the state, and
 * directly return the result and a new state. Return the state immediately
 * if the argument is a Nothing value.
 */
maybeSt :: *st (.a *st -> *st) !(Maybe .a) -> *st

/**
 * Directly return a Just value or return a default value if the argument is a Nothing value.
 */
fromMaybe :: .a !(Maybe .a) -> .a

/**
 * The Maybe monad transformer.
 */
:: MaybeT m a = MaybeT !(m (Maybe a))

/**
 * Runs a MaybeT as the monad wrapped inside the transformer.
 */
runMaybeT :: !(MaybeT m a) -> m (Maybe a)

/**
 * Transforms the computation inside a transformer.
 *
 * @param The computation transformation.
 * @param The transformer to be transformed.
 * @result The transformed transformer.
 */
mapMaybeT :: !((m (Maybe a)) -> n (Maybe b)) !(MaybeT m a) -> MaybeT n b

instance Functor (MaybeT m) | Functor m
instance pure (MaybeT m) | pure m
instance <*> (MaybeT m) | Monad m
instance Alternative (MaybeT m) | Monad m
instance Monad (MaybeT m) | Monad m
instance MonadTrans MaybeT
where
	liftT :: !(a b) -> MaybeT a b | Monad a
