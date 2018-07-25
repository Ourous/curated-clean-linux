definition module Clean.Types.Unify

/**
 * Functions to unify Clean types.
 */

import Clean.Types
from Data.Map import :: Map
from Data.Maybe import :: Maybe

/**
 * Check whether a unification result indicates that the left type generalised
 * the right type.
 */
isGeneralisingUnifier :: ![TVAssignment] -> Bool

/**
 * Check whether a unification result indicates that the unified types are
 * isomorphic.
 */
isIsomorphicUnifier :: ![TVAssignment] -> Bool

/**
 * `True` iff the first type is more general or equal to the second type.
 */
(generalises) infix 4 :: !Type !Type -> Bool

/**
 * `True` iff the first type is more specific or equal to the second type.
 */
(specialises) infix 4 :: !Type !Type -> Bool

/**
 * `True` if two types are isomorphic to each other.
 */
(isomorphic_to) infix 4 :: !Type !Type -> Bool

/**
 * Prepare a type for unification. Unification always happens between a 'left'
 * and a 'right' type. Unification of two left or two right types may yield
 * unexpected results.
 *
 * @param True if this is the left type
 * @param A predicate indicating if a type is always unique, like, e.g., World
 * @param Known type definitions to use for resolving synonyms
 * @param The type to prepare
 * @result The type synonyms used and the type after preparation
 */
prepare_unification :: !Bool (String -> Bool) (Map String [TypeDef]) !Type -> ([TypeDef], Type)

/**
 * Finish unification, yielding a unifier.
 *
 * @param The used type synonyms
 * @param The variable assignments
 * @result The unifier
 */
finish_unification :: ![TypeDef] ![TVAssignment] -> Unifier

/**
 * Core of the unification. An implementation of Martelli and Montanari, 'An
 * Efficient Unification Algorithm'. ACM Transactions on Programming Languages
 * and Systems, Vol. 4, No. 2, April 1982, pp. 258-282.
 * It has been modified slightly to deal with constructor variables, universal
 * quantifiers and uniqueness.
 *
 * @param The left type
 * @param The right type
 * @result A list of type variable assignments, or Nothing if unification failed
 */
unify :: !Type !Type -> Maybe [TVAssignment]
