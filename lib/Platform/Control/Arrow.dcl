definition module Control.Arrow

import Control.Category
from Control.Monad import class Monad
from Control.Applicative import class Applicative, class Alternative
from Data.Functor import class Functor
from Data.Either import :: Either


// | The basic arrow class.
//
// Instances should satisfy the following laws:
//
//  * @'arr' cid = 'id'@
//
//  * @'arr' (f >>> g) = 'arr' f >>> 'arr' g@
//
//  * @'first' ('arr' f) = 'arr' ('first' f)@
//
//  * @'first' (f >>> g) = 'first' f >>> 'first' g@
//
//  * @'first' f >>> 'arr' 'fst' = 'arr' 'fst' >>> f@
//
//  * @'first' f >>> 'arr' ('id' *** g) = 'arr' ('id' *** g) >>> 'first' f@
//
//  * @'first' ('first' f) >>> 'arr' 'assoc' = 'arr' 'assoc' >>> 'first' f@
//
// where
//
// > assoc ((a,b),c) = (a,(b,c))
//
// The other combinators have sensible default definitions,
// which may be overridden for efficiency.

class Arrow a | Category a where
    // | Lift a function to an arrow.
    arr :: (b -> c) -> a b c

    // | Send the first component of the input through the argument
    //   arrow, and copy the rest unchanged to the output.
    first :: (a b c) -> a (b,d) (c,d)

    // | A mirror image of 'first'.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    second :: (a b c) -> a (d,b) (d,c)

    // | Split the input between the two argument arrows and combine
    //   their output.  Note that this is in general not a functor.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (***) infixr 3 :: (a b c) (a b` c`) -> a (b,b`) (c,c`)

    // | Fanout: send the input to both argument arrows and combine
    //   their output.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (&&&) infixr 3 :: (a b c) (a b c`) -> a b (c,c`)

// Ordinary functions are arrows.
instance Arrow (->)

// | Kleisli arrows of a monad.
:: Kleisli m a b = Kleisli (a -> m b)

runKleisli :: (Kleisli m a b) -> (a -> m b)

instance Category (Kleisli m) | Monad m

instance Arrow (Kleisli m) | Monad m

// | The identity arrow, which plays the role of 'pure' in arrow notation.
pureA :: a b b | Arrow a

// | Precomposition with a pure function.
(^>>) infixr 1 :: (b -> c) (a c d) -> a b d | Arrow a

// | Postcomposition with a pure function.
(>>^) infixr 1 :: (a b c) (c -> d) -> a b d | Arrow a

// | Precomposition with a pure function (right-to-left variant).
(<<^) infixr 1 :: (a c d) (b -> c) -> a b d | Arrow a

// | Postcomposition with a pure function (right-to-left variant).
(^<<) infixr 1 :: (c -> d) (a b c) -> a b d | Arrow a

class ArrowZero a | Arrow a where
    zeroArrow :: a b c

// TODO
//instance ArrowZero (Kleisli m) | MonadPlus m

// | A monoid on arrows.
class ArrowPlus a | ArrowZero a where
    // | An associative operation with identity 'zeroArrow'.
    (<+>) infixr 5 :: (a b c) (a b c) -> a b c

// TODO
//instance ArrowPlus (Kleisli m) | MonadPlus m

// | Choice, for arrows that support it.  This class underlies the
// @if@ and @case@ constructs in arrow notation.
//
// Instances should satisfy the following laws:
//
//  * @'left' ('arr' f) = 'arr' ('left' f)@
//
//  * @'left' (f >>> g) = 'left' f >>> 'left' g@
//
//  * @f >>> 'arr' 'Left' = 'arr' 'Left' >>> 'left' f@
//
//  * @'left' f >>> 'arr' ('id' <+++> g) = 'arr' ('id' <+++> g) >>> 'left' f@
//
//  * @'left' ('left' f) >>> 'arr' 'assocsum' = 'arr' 'assocsum' >>> 'left' f@
//
// where
//
// > assocsum (Left (Left x)) = Left x
// > assocsum (Left (Right y)) = Right (Left y)
// > assocsum (Right z) = Right (Right z)
//
// The other combinators have sensible default definitions, which may
// be overridden for efficiency.

class ArrowChoice a | Arrow a where
    // | Feed marked inputs through the argument arrow, passing the
    //   rest through unchanged to the output.
    left :: (a b c) -> a (Either b d) (Either c d)

    // | A mirror image of 'left'.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    right :: (a b c) -> a (Either d b) (Either d c)

    // | Split the input between the two argument arrows, retagging
    //   and merging their outputs.
    //   Note that this is in general not a functor.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (<+++>) infixr 2 :: (a b c) (a b` c`) -> a (Either b b`) (Either c c`)

    // | Fanin: Split the input between the two argument arrows and
    //   merge their outputs.
    //
    //   The default definition may be overridden with a more efficient
    //   version if desired.
    (|||) infixr 2 :: (a b d) (a c d) -> a (Either b c) d

instance ArrowChoice (->)

instance ArrowChoice (Kleisli m) | Monad m

// | Some arrows allow application of arrow inputs to other inputs.
// Instances should satisfy the following laws:
//
//  * @'first' ('arr' (\\x -> 'arr' (\\y -> (x,y)))) >>> 'app' = 'id'@
//
//  * @'first' ('arr' (g >>>)) >>> 'app' = 'second' g >>> 'app'@
//
//  * @'first' ('arr' (>>> h)) >>> 'app' = 'app' >>> h@
//
// Such arrows are equivalent to monads (see 'ArrowMonad').

class ArrowApply a | Arrow a where
    app :: a (a b c, b) c

instance ArrowApply (->)

instance ArrowApply (Kleisli m) | Monad m

// | The 'ArrowApply' class is equivalent to 'Monad': any monad gives rise
//   to a 'Kleisli' arrow, and any instance of 'ArrowApply' defines a monad.

:: ArrowMonad a b = ArrowMonad (a () b)

instance Functor (ArrowMonad a) | Arrow a

instance Applicative (ArrowMonad a) | Arrow a

instance Monad (ArrowMonad a) | ArrowApply a

instance Alternative (ArrowMonad a) | ArrowPlus a

// TODO
//instance MonadPlus (ArrowMonad a) | ArrowApply a & ArrowPlus a

// | Any instance of 'ArrowApply' can be made into an instance of
//   'ArrowChoice' by defining 'left' = 'leftApp'.

leftApp :: (a b c) -> a (Either b d) (Either c d) | ArrowApply a

// | The 'loop' operator expresses computations in which an output value
// is fed back as input, although the computation occurs only once.
// It underlies the @rec@ value recursion construct in arrow notation.
// 'loop' should satisfy the following laws:
//
// [/extension/]
//      @'loop' ('arr' f) = 'arr' (\\ b -> 'fst' ('fix' (\\ (c,d) -> f (b,d))))@
//
// [/left tightening/]
//      @'loop' ('first' h >>> f) = h >>> 'loop' f@
//
// [/right tightening/]
//      @'loop' (f >>> 'first' h) = 'loop' f >>> h@
//
// [/sliding/]
//      @'loop' (f >>> 'arr' ('id' *** k)) = 'loop' ('arr' ('id' *** k) >>> f)@
//
// [/vanishing/]
//      @'loop' ('loop' f) = 'loop' ('arr' unassoc >>> f >>> 'arr' assoc)@
//
// [/superposing/]
//      @'second' ('loop' f) = 'loop' ('arr' assoc >>> 'second' f >>> 'arr' unassoc)@
//
// where
//
// > assoc ((a,b),c) = (a,(b,c))
// > unassoc (a,(b,c)) = ((a,b),c)
//
//class ArrowLoop a | Arrow a where
    //loop :: a (b,d) (c,d) -> a b c

//instance ArrowLoop (->)

// | Beware that for many monads (those for which the '>>=' operation
// is strict) this instance will /not/ satisfy the right-tightening law
// required by the 'ArrowLoop' class.
// TODO
//instance ArrowLoop (Kleisli m) | MonadFix m
