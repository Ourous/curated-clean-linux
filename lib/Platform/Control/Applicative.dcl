definition module Control.Applicative

from Control.Monad import class Monad, class MonadPlus
from Data.Functor  import class Functor
from Data.Maybe    import :: Maybe
from Data.Monoid   import class Monoid, class Semigroup

:: Const a b = Const a
:: WrappedMonad m a = WrapMonad (m a)

unwrapMonad :: !(WrappedMonad m a) -> m a

getConst :: !(Const a b) -> a

class Applicative f | Functor f
where
	pure           :: a -> f a
	(<*>) infixl 4 :: !(f (a -> b)) (f a) -> f b

class Alternative f | Applicative f
where
	empty          :: f a
	(<|>) infixl 3 :: !(f a) (f a) -> f a

instance Functor (Const m)
instance Functor (WrappedMonad m) | Monad m
instance Applicative (Const m) | Monoid m
instance Applicative (WrappedMonad m) | Monad m
instance Monad (WrappedMonad m) | Monad m

instance Alternative (WrappedMonad m) | MonadPlus m

instance Semigroup (Const a b) | Semigroup a
instance Monoid (Const a b) | Monoid a

some :: (f a) -> f [a] | Alternative f

many :: (f a) -> f [a] | Alternative f

/**
 * Sequence actions and take the value of the right argument.
 * Previously, this was a normal function with the type context Applicative f
 * and an implementation similar to the instance for f now. However, for some
 * types there are more efficient possibilities. Making this a class with a
 * default implementation allows overriding the instance in such cases, like
 * for Maybe here.
 * Be aware that the execution order has to be correct: the left hand side must
 * be evaluated before the right hand side.
 */
class (*>) infixl 4 f :: !(f a) (f b) -> f b | Applicative f
instance *> f

/**
 * Sequence actions and take the value of the left argument.
 * For the reason behind making this a class rather than a normal function, see
 * the documentation on *>.
 * Be aware that the execution order has to be correct: the left hand side must
 * be evaluated before the right hand side.
 */
class (<*) infixl 4 f :: !(f a) (f b) -> f a | Applicative f
instance <* f

(<**>) infixl 4 :: (f a) (f (a -> b)) -> f b | Applicative f

lift :: a -> f a | Applicative f

liftA :: (a -> b) (f a) -> f b | Applicative f

liftA2 :: (a b -> c) (f a) (f b) -> f c | Applicative f

liftA3 :: (a b c -> d) (f a) (f b) (f c) -> f d | Applicative f

optional :: (f a) -> f (Maybe a) | Alternative f
