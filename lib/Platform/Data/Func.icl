implementation module Data.Func

from StdFunc import const, o
import Data.Functor
import Data.Monoid
import Control.Applicative
import Control.Monad

instance Functor ((->) r)
where
	fmap f g = \x -> (f o g) x

instance pure ((->) r)
where
	pure :: !a -> b -> a
	pure x = const x

instance <*> ((->) r)
where
	(<*>) f g = \x -> f x (g x)

instance Monad ((->) r)
where
	bind ma a2mb = \r -> a2mb (ma r) r

instance Semigroup (a -> b) | Semigroup b
where
	mappend f g = \x -> mappend (f x) (g x)

instance Monoid (a -> b) | Monoid b
where
	mempty = \_ -> mempty

seqSt :: !(a .st -> .st) ![a] !.st -> .st
seqSt f [] st = st
seqSt f [x:xs] st = seqSt f xs (f x st)

mapSt :: !(a .st -> (!b,!.st)) ![a] !.st -> (![b],.st)
mapSt f [] st = ([], st)
mapSt f [x:xs] st
  # (y, st)  = f x st
  # (ys, st) = mapSt f xs st
  = ([y:ys], st)

fix :: !(a -> a) -> a
fix f = let x = f x in x

on :: (b b -> c) (a -> b) -> (a a -> c)
on f g = \x y -> f (g x) (g y)

hyperstrict :: !.a -> .a
hyperstrict a = code {
		push_a 0
		.d 1 0
		jsr _eval_to_nf
		.o 0 0
	}
