definition module Control.Applicative

from Control.Monad import class Monad, class MonadPlus
from Data.Functor  import class Functor
from Data.Maybe    import :: Maybe
from Data.Monoid   import class Monoid, class Semigroup

:: Const a b = Const a
:: WrappedMonad m a = WrapMonad (m a)

unwrapMonad :: !(WrappedMonad m a) -> m a

getConst :: !(Const a b) -> a

class pure f :: a -> f a
class (<*>) infixl 4 f :: !(f (a -> b)) (f a) -> f b

class Applicative f | Functor, pure, <*> f

class <* f | Applicative f
where
	(<*) infixl 4 :: !(f a) (f b) -> f a
	(<*) fa fb = pure (\x _->x) <*> fa <*> fb

class *> f | Applicative f
where
	(*>) infixl 4 :: !(f a) (f b) -> f b
	(*>) fa fb = pure (\_ x->x) <*> fa <*> fb

class Alternative f | Applicative f
where
	empty          :: f a
	(<|>) infixl 3 :: !(f a) (f a) -> f a

instance Functor (Const m)
instance Functor (WrappedMonad m) | Monad m
instance pure (Const m) | Monoid m
instance pure (WrappedMonad m) | pure m
instance <*> (Const m) | Monoid m
where
	(<*>) :: !(Const m (a -> b)) !(Const m a) -> Const m b | Monoid m
instance <*> (WrappedMonad m) | Monad m
where
	(<*>) :: !(WrappedMonad m (a -> b)) !(WrappedMonad m a) -> WrappedMonad m b | Monad m
instance Monad (WrappedMonad m) | Monad m

instance Alternative (WrappedMonad m) | MonadPlus m
where
	(<|>) :: !(WrappedMonad m a) !(WrappedMonad m a) -> WrappedMonad m a | MonadPlus m

instance Semigroup (Const a b) | Semigroup a
where
	mappend :: !(Const a b) !(Const a b) -> Const a b | Semigroup a
instance Monoid (Const a b) | Monoid a

some :: !(f a) -> f [a] | Alternative f
	special f=[]; f=Maybe

many :: !(f a) -> f [a] | Alternative f
	special f=[]; f=Maybe

(<**>) infixl 4 :: !(f a) (f (a -> b)) -> f b | Applicative f
	special f=[]; f=Maybe

lift :: a -> f a | pure f
	special f=[]; f=Maybe

liftA :: (a -> b) (f a) -> f b | Applicative f
	special f=[]; f=Maybe

liftA2 :: (a b -> c) !(f a) (f b) -> f c | Applicative f
	special f=[]; f=Maybe

liftA3 :: (a b c -> d) !(f a) (f b) (f c) -> f d | Applicative f
	special f=[]; f=Maybe

optional :: !(f a) -> f (Maybe a) | Alternative f
	special f=[]; f=Maybe

/**
 * Conditional execution of Applicative expressions. For example,
 *
 *     when debug (putStrLn "Debugging")
 *
 * will output the string Debugging if the Boolean value debug is True, and otherwise do nothing.
 *
 * @type Bool (f ()) -> f () | Applicative f
 */
when p s :== if p s (pure ())

/**
 * The reverse of `when`
 * @type Bool (f ()) -> f () | Applicative f
 */
unless p s :== if p (pure ()) s
