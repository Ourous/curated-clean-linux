definition module Control.Monad

from Control.Applicative  import class pure(pure), class <*>, class Applicative
from Data.Functor         import class Functor

class Monad m | Applicative m
where
    bind :: !(m a) (a -> m b) -> m b

    (>>=) infixl 1 :: (m a) (a -> m b) -> m b | Monad m
    (>>=) ma a2mb :== bind ma a2mb

    (`b`) infixl 1 :: (m a) (a -> m b) -> m b | Monad m
    (`b`) ma a2mb :== bind ma a2mb

    (>>|) infixl 1 :: (m a) (m b) -> m b | Monad m
    (>>|) ma mb :== ma >>= \_ -> mb

    (=<<) infixr 1 :: (a -> m b) (m a) -> m b | Monad m
    (=<<) f x :== x >>= f

	return :: a -> m a | pure m
	return x :== pure x

class MonadPlus m | Monad m
where
	mzero :: m a
	mplus :: !(m a) (m a) -> m a

sequence          :: !.[a b] -> a [b] | Monad a
sequence_         :: !.[a b] -> a () | Monad a
mapM              :: (.a -> b c) ![.a] -> b [c] | Monad b
mapM_             :: (.a -> b c) ![.a] -> b () | Monad b
forM              :: u:([v:a] -> w:((v:a -> b c) -> b [c])) | Monad b, [w <= u,w <= v]
forM_             :: u:([v:a] -> w:((v:a -> b c) -> b ())) | Monad b, [w <= u,w <= v]
forever           :: !(a b) -> a c | Monad a
join              :: !(a (a b)) -> a b | Monad a
zipWithM          :: (.a -> .(.b -> c d)) ![.a] [.b] -> c [d] | Monad c
foldM             :: (a -> .(b -> c a)) a ![b] -> c a | Monad c
mapStM            :: (a st -> m (b, st)) ![a] !st -> m (![b], st) | Monad m
replicateM        :: !.Int (a b) -> a [b] | Monad a
(>=>) infixr 1    :: u:(.a -> b c) (c -> b d) -> v:(.a -> b d) | Monad b, [v <= u]
(<=<) infixr 1    :: u:((a -> b c) -> v:(w:(.d -> b a) -> x:(.d -> b c))) | Monad b, [v <= u,x <= w]
liftM             :: (a -> b) !(c a) -> c b | Monad c
liftM2            :: (a -> .(b -> c)) !(d a) (d b) -> d c | Monad d
liftM3            :: (a -> .(b -> .(c -> d))) !(e a) (e b) (e c) -> e d | Monad e
liftM4            :: (a -> .(b -> .(c -> .(d -> e)))) !(f a) (f b) (f c) (f d) -> f e | Monad f
liftM5            :: (a -> .(b -> .(c -> .(d -> .(e -> f))))) !(g a) (g b) (g c) (g d) (g e) -> g f | Monad g
ap                :: u:((a (b -> c)) -> v:((a b) -> a c)) | Monad a, [v <= u]
