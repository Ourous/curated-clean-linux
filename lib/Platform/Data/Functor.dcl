definition module Data.Functor

class Functor f
where
    fmap :: (a -> b) !(f a) -> f b

    (<$>) infixl 4 :: (a -> b) !(f a) -> f b | Functor f
    (<$>) f fa :== fmap f fa

    (<$) infixl 4 :: a !(f b) -> f a | Functor f
    (<$) x fa :== fmap (\_ -> x) fa

    ($>) infixl 4 :: !(f b) a -> f a | Functor f
    ($>) fa x :== x <$ fa

    void :: !(f a) -> f () | Functor f
    void x :== () <$ x
