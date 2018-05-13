definition module Data.Foldable

from Control.Applicative import class Applicative (..), :: Const, class Alternative (..), class *>
from Control.Monad import class Monad (..), >>=, class MonadPlus (..)
from Data.Functor import class Functor (..)
from Data.Monoid import class Monoid (..), class Semigroup (..)
from Data.Maybe import :: Maybe
from StdOverloaded import class +, class one, class *, class zero, class <, class ==
from StdClass import class Ord
from StdFunc import flip

/**
 * Ported from Haskell's Data.Foldable by Jurriën Stutterheim 15-08-2014
 */

/**
 * Data structures that can be folded.
 *
 * For example, given a data type
 *
 * > :: Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
 *
 * a suitable instance would be
 *
 * > instance Foldable Tree where
 * >     foldMap f Empty = mempty
 * >     foldMap f (Leaf x) = f x
 * >     foldMap f (Node l k r) = foldMap f l <++> f k <++> foldMap f r
 *
 * This is suitable even for abstract types, as the monoid is assumed
 * to satisfy the monoid laws.  Alternatively, one could define foldr:
 *
 * > instance Foldable Tree where
 * >     foldr f z Empty = z
 * >     foldr f z (Leaf x) = f x z
 * >     foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l
 */
class Foldable t where
	/**
	 * Combine the elements of a structure using a monoid.
	 */
    fold :: !(t m) -> m | Monoid m

	/**
	 * Map each element of the structure to a monoid, and combine the results.
	 */
    foldMap :: (a -> m) !(t a) -> m | Monoid m

	/**
	 * Right-associative fold of a structure.
	 * `foldr f z = 'StdList'.{{foldr}} f z {{o}} {{toList}}`
	 */
    foldr :: (a b -> b) b !(t a) -> b

	/**
	 * Right-associative fold of a structure, but with strict application of
	 * the operator.
	 */
    foldr` :: (a b -> b) !b !(t a) -> b

	/**
	 * Left-associative fold of a structure.
	 * `foldl f z = 'StdList'.{{foldl}} f z o {{toList}}`
	 */
    foldl :: (b a -> b) b !(t a) -> b

	/**
	 * Left-associative fold of a structure, but with strict application of the
	 * operator.
	 */
    foldl` :: (b a -> b) !b !(t a) -> b

	/**
	 * A variant of {{foldr}} that has no base case, and thus may only be
	 * applied to non-empty structures.
	 * `foldr1 f = 'Data.List'.{{foldr1}} f o {{toList}}`
	 */
    foldr1 :: (a a -> a) !(t a) -> a

	/**
	 * A variant of {{foldl}} that has no base case, and thus may only be
	 * applied to non-empty structures.
	 * `foldl1 f = 'Data.List'.{{foldl1}} f o {{toList}}`
	 */
    foldl1 :: (a a -> a) !(t a) -> a

// TODO Cleanify
//instance Ix i => Foldable (Array i)

instance Foldable (Const m)

/**
 * Monadic fold over the elements of a structure, associating to the right,
 * i.e. from right to left.
 */
foldrM :: (a b -> m b) b (t a) -> m b | Foldable t & Monad m

/**
 * Monadic fold over the elements of a structure, associating to the left, i.e.
 * from left to right.
 */
foldlM :: (b a -> m b) b (t a) -> m b | Foldable t & Monad m

/**
 * Map each element of a structure to an action, evaluate these actions from
 * left to right, and ignore the results.
 */
traverse_ :: (a -> f b) (t a) -> f () | Foldable t & Applicative, *> f

/**
 * `for_` is {{`traverse_`}} with its arguments flipped.
 * @type (t a) (a -> f b) -> f () | Foldable, Applicative f
 */
for_ :== flip traverse_

/**
 * Map each element of a structure to a monadic action, evaluate these actions
 * from left to right, and ignore the results.
 */
mapM_ :: (a -> m b) (t a) -> m () | Foldable t & Monad m

/**
 * `forM_` is {{`mapM_`}} with its arguments flipped.
 * @type (t a) (a -> m b) -> m () | Foldable t & Monad m
 */
forM_ :== flip mapM_

/**
 * Evaluate each action in the structure from left to right, and ignore the
 * results.
 */
sequenceA_ :: (t (f a)) -> f () | Foldable t & Applicative, *> f

/**
 * Evaluate each monadic action in the structure from left to right, and ignore
 * the results.
 * @type (t (m a)) -> m () | Foldable t & Monad m
 */
sequence_ :== foldr (\ma mb -> ma >>= \_ -> mb) (pure ())

/**
 * The sum of a collection of actions, generalizing {{`concat`}}.
 * @type (t (f a)) -> f a | Foldable t & Alternative f
 */
asum :== foldr (<|>) empty

/**
 * The sum of a collection of actions, generalizing {{`concat`}}.
 * @type (t (m a)) -> m a | Foldable t & MonadPlus m
 */
msum :== foldr mplus mzero

// These use foldr rather than foldMap to avoid repeated concatenation.

/**
 * List of elements of a structure.
 * @type (t a) -> [a] | Foldable t
 */
toList t :== build (\c n -> foldr c n t)

/**
 * @type ((a b -> b) b -> b) -> [a]
 */
build g :== g (\x xs -> [x:xs]) []


/**
 * The concatenation of all the elements of a container of lists.
 */
concat :: (t [a]) -> [a] | Foldable t

/**
 * Map a function over all the elements of a container and concatenate the
 * resulting lists.
 */
concatMap :: (a -> [b]) (t a) -> [b] | Foldable t

/**
 * `and` returns the conjunction of a container of {{`Bool`}}s. For the result
 * to be {{`True`}}, the container must be finite; {{`False`}}, however,
 * results from a {{`False`}} value finitely far from the left end.
 */
and :: (t Bool) -> Bool | Foldable t

/**
 * `or` returns the disjunction of a container of {{`Bool`}}s. For the result
 * to be {{`False`}}, the container must be finite; {{`True`}}, however,
 * results from a {{`True`}} value finitely far from the left end.
 */
or :: (t Bool) -> Bool | Foldable t

/**
 * Determines whether any element of the structure satisfies the predicate.
 */
any :: (a -> Bool) (t a) -> Bool | Foldable t

/**
 * Determines whether all elements of the structure satisfy the predicate.
 */
all :: (a -> Bool) (t a) -> Bool | Foldable t

/**
 * The `sum` function computes the sum of the numbers of a structure.
 */
sum :: (t a) -> a | Foldable t & + a & zero a

/**
 * The `product` function computes the product of the numbers of a structure.
 */
product :: (t a) -> a | Foldable t & * a & one a

/**
 * The largest element of a non-empty structure.
 */
maximum :: (t a) -> a | Foldable t & Ord a

/**
 * The largest element of a non-empty structure with respect to the given
 * lesser-than function.
 */
maximumBy :: (a a -> Bool) (t a) -> a | Foldable t

/**
 * The least element of a non-empty structure.
 */
minimum :: (t a) -> a | Foldable t & Ord a

/**
 * The least element of a non-empty structure with respect to the given
 * lesser-than function.
 */
minimumBy :: (a a -> Bool) (t a) -> a | Foldable t

/**
 * Does the element occur in the structure?
 */
elem :: a (t a) -> Bool | Foldable t & == a

/**
 * `notElem` is the negation of {{`elem`}}.
 */
notElem ::  a (t a) -> Bool | Foldable t & == a

/**
 * The `find` function takes a predicate and a structure and returns the
 * leftmost element of the structure matching the predicate, or {{`Nothing`}}
 * if there is no such element.
 */
find :: (a -> Bool) (t a) -> Maybe a | Foldable t
