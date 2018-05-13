implementation module Control.Monad

from Control.Applicative  import class Applicative (..), lift
from Data.Functor         import class Functor (..)
from Data.List            import map, zipWith, replicate
from StdList              import foldr
from StdFunc              import flip, id, o, const
from StdInt               import class +, instance + Int

sequence :: !.[a b] -> a [b] | Monad a
sequence ms = foldr k (lift []) ms
  where
    k m m` = m >>= \x -> m` >>= \xs -> lift [x:xs]

sequence_ :: !.[a b] -> a () | Monad a
sequence_ ms = foldr (>>|) (lift ()) ms

mapM :: (.a -> b c) ![.a] -> b [c] | Monad b
mapM f as = sequence (map f as)

mapM_ :: (.a -> b c) ![.a] -> b () | Monad b
mapM_ f as = sequence_ (map f as)

forM :: u:([v:a] -> w:((v:a -> b c) -> b [c])) | Monad b, [w <= u,w <= v]
forM = flip mapM

forM_ :: u:([v:a] -> w:((v:a -> b c) -> b ())) | Monad b, [w <= u,w <= v]
forM_ = flip mapM_

forever :: (a b) -> a c | Monad a
forever a = let a` = a >>| a` in a`

join :: (a (a b)) -> a b | Monad a
join x = x >>= id

zipWithM :: (.a -> .(.b -> c d)) ![.a] [.b] -> c [d] | Monad c
zipWithM f xs ys = sequence (zipWith f xs ys)

foldM :: (a -> .(b -> c a)) a ![b] -> c a | Monad c
foldM _ a []      = lift a
foldM f a [x:xs]  = f a x >>= \fax -> foldM f fax xs

replicateM :: !.Int (a b) -> a [b] | Monad a
replicateM n x = sequence (replicate n x)

(>=>) infixr 1 :: u:(.a -> b c) (c -> b d) -> v:(.a -> b d) | Monad b, [v <= u]
(>=>) f g = \x -> f x >>= g

(<=<) infixr 1 :: u:((a -> b c) -> v:(w:(.d -> b a) -> x:(.d -> b c))) | Monad b, [v <= u,x <= w]
(<=<) = flip (>=>)

liftM :: (a -> b) (c a) -> c b | Monad c
liftM f m1 = m1 >>= \x1 -> lift (f x1)

liftM2 :: (a -> .(b -> c)) (d a) (d b) -> d c | Monad d
liftM2 f m1 m2 = m1 >>= \x1 -> m2 >>= \x2 -> lift (f x1 x2)

liftM3 :: (a -> .(b -> .(c -> d))) (e a) (e b) (e c) -> e d | Monad e
liftM3 f m1 m2 m3 = m1 >>= \x1 -> m2 >>= \x2 -> m3 >>= \x3 -> lift (f x1 x2 x3)

liftM4 :: (a -> .(b -> .(c -> .(d -> e)))) (f a) (f b) (f c) (f d) -> f e | Monad f
liftM4 f m1 m2 m3 m4 = m1 >>= \x1 -> m2 >>= \x2 -> m3 >>= \x3 -> m4 >>= \x4 -> lift (f x1 x2 x3 x4)

liftM5 :: (a -> .(b -> .(c -> .(d -> .(e -> f))))) (g a) (g b) (g c) (g d) (g e) -> g f | Monad g
liftM5 f m1 m2 m3 m4 m5 = m1 >>= \x1 -> m2 >>= \x2 -> m3 >>= \x3 -> m4 >>= \x4 -> m5 >>= \x5 -> lift (f x1 x2 x3 x4 x5)

ap :: u:((a (b -> c)) -> v:((a b) -> a c)) | Monad a, [v <= u]
ap = liftM2 id
