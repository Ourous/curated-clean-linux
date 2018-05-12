definition module iTasks.WF.Combinators.Overloaded

import iTasks.WF.Definition

from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Functor import class Functor

class TApplicative f | Functor f where
    (<#>)  :: (f (a -> b)) (f a) -> f b | iTask a & iTask b
    return :: a -> f a | iTask a

class TMonad m | TApplicative m where
    (>>=) infixl 1 :: (m a) (a -> m b) -> m b | iTask a & iTask b
    (>>|) infixl 1 :: (m a) (     m b) -> m b | iTask a & iTask b

instance Functor Task
instance TApplicative Task
instance TMonad Task

instance TApplicative Maybe
instance TMonad Maybe

instance TApplicative []
instance TMonad []

instance TApplicative (Either e)
instance TMonad (Either e)

