// --------------------------------------------------------------------------
// |
// Module      :  Control.Arrow
// Copyright   :  (c) Ross Paterson 2002
// License     :  BSD-style (see the LICENSE file in the distribution)
//
// Maintainer  :  libraries@haskell.org
// Stability   :  provisional
// Portability :  portable
//
// Basic arrow definitions, based on
//
//  * /Generalising Monads to Arrows/, by John Hughes,
//    /Science of Computer Programming/ 37, pp67-111, May 2000.
//
// plus a couple of definitions ('pureA' and 'loop') from
//
//  * /A New Notation for Arrows/, by Ross Paterson, in /ICFP 2001/,
//    Firenze, Italy, pp229-240.
//
// These papers and more information on arrows can be found at
// <http://www.haskell.org/arrows/>.

implementation module Control.Arrow

import StdTuple
from StdFunc import o, const
import Data.Either
import Control.Monad.Fix
import Control.Category
import Control.Monad
import Control.Applicative


// Ordinary functions are arrows.

instance Arrow (->) where
    arr f = f
    first x = x *** cid
    second x = cid *** x
    (***) f g = \t -> let (x,y) = t in (f x, g y)
    (&&&) f g = arr (\b -> (b,b)) >>> f *** g

// | Kleisli arrows of a monad.
:: Kleisli m a b = Kleisli (a -> m b)

runKleisli :: (Kleisli m a b) -> (a -> m b)
runKleisli (Kleisli f) = f

instance Category (Kleisli m) | Monad m where
    cid = Kleisli pure
    (O) (Kleisli f) (Kleisli g) = Kleisli (\b -> g b >>= f)

instance Arrow (Kleisli m) | Monad m where
    arr f = Kleisli (pure o f)
    first (Kleisli f) = Kleisli (\t -> let (b, d) = t in f b >>= \c -> pure (c,d))
    second (Kleisli f) = Kleisli (\t -> let (d,b) = t in f b >>= \c -> pure (d,c))
    (***) f g = first f >>> arr swap >>> first g >>> arr swap
      where swap t = let (x,y) = t in (y,x)
    (&&&) f g = arr (\b -> (b,b)) >>> f *** g

// | The identity arrow, which plays the role of 'pure' in arrow notation.
pureA :: a b b | Arrow a
pureA = arr cid

// | Precomposition with a pure function.
(^>>) infixr 1 :: (b -> c) (a c d) -> a b d | Arrow a
(^>>) f a = arr f >>> a

// | Postcomposition with a pure function.
(>>^) infixr 1 :: (a b c) (c -> d) -> a b d | Arrow a
(>>^) a f = a >>> arr f

// | Precomposition with a pure function (right-to-left variant).
(<<^) infixr 1 :: (a c d) (b -> c) -> a b d | Arrow a
(<<^) a f = a <<< arr f

// | Postcomposition with a pure function (right-to-left variant).
(^<<) infixr 1 :: (c -> d) (a b c) -> a b d | Arrow a
(^<<) f a = arr f <<< a

// TODO
//instance ArrowZero (Kleisli m) | MonadPlus m where
    //zeroArrow = Kleisli (\_ -> mzero)

// TODO
//instance ArrowPlus (Kleisli m) | MonadPlus m where
    //Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)

instance ArrowChoice (->) where
    left f = f <+++> cid
    right f = cid <+++> f
    (<+++>) f g = (Left o f) ||| (Right o g)
    (|||) x y = either x y

instance ArrowChoice (Kleisli m) | Monad m where
    left f = f <+++> arr cid
    right f = arr cid <+++> f
    (<+++>) f g = (f >>> arr Left) ||| (g >>> arr Right)
    (|||) (Kleisli f) (Kleisli g) = Kleisli (either f g)

instance ArrowApply (->) where
    app = \(f,x) -> f x

instance ArrowApply (Kleisli m) | Monad m where
    app = Kleisli (\(Kleisli f, x) -> f x)

instance Functor (ArrowMonad a) | Arrow a where
    fmap f (ArrowMonad m) = ArrowMonad (m >>> arr f)

instance pure (ArrowMonad a) | Arrow a
where
	pure x = ArrowMonad (arr (const x))

instance <*> (ArrowMonad a) | Arrow a
where
   (<*>) (ArrowMonad f) (ArrowMonad x) = ArrowMonad (f &&& x >>> arr (uncurry cid))

instance Monad (ArrowMonad a) | ArrowApply a where
   bind (ArrowMonad m) f = ArrowMonad (
        m >>> arr (\x -> let (ArrowMonad h) = f x in (h, ())) >>> app)

instance Alternative (ArrowMonad a) | ArrowPlus a where
   empty = ArrowMonad zeroArrow
   (<|>) (ArrowMonad x) (ArrowMonad y) = ArrowMonad (x <+> y)

// TODO
//instance MonadPlus (ArrowMonad a) | ArrowApply a & ArrowPlus a where
   //mzero = ArrowMonad zeroArrow
   //ArrowMonad x `mplus` ArrowMonad y = ArrowMonad (x <+> y)

// | Any instance of 'ArrowApply' can be made into an instance of
//   'ArrowChoice' by defining 'left' = 'leftApp'.

leftApp :: (a b c) -> a (Either b d) (Either c d) | ArrowApply a
leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
             (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app


//instance ArrowLoop (->) where
    //loop f b = let (c,d) = f (b,d) in c

// | Beware that for many monads (those for which the '>>=' operation
// is strict) this instance will /not/ satisfy the right-tightening law
// required by the 'ArrowLoop' class.
// TODO
//instance MonadFix m => ArrowLoop (Kleisli m) where
    //loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
      //where f' x y = f (x, snd y)
