definition module Data.GenDefault

import StdGeneric
from StdList import hd
from Data.Maybe import :: Maybe(Just)

/**
 * Generates some value of the type. The value may not have a finite representation.
 *
 * @result A default value
 */
generic gDefault a :: a
derive gDefault Bool, Char, Int, Real, String, (->), UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, RECORD
derive gDefault [], [!], [ !], [!!], {}, {!}
derive gDefault (), (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

/**
 * Gives a default value which is guaranteed to be final.
 * The function does not terminate in case the type does not contain any finite value.
 *
 * @result A finite default value
 * @type a | gFiniteDefault{|*|} a
 **/
finiteDefaultValue :== hd [d \\ Just d <- gFiniteDefault{|*|}]

/**
 * Generates a list with the following properties:
 *   1. Each Just value is finite.
 *   2. If the types contains any finite value, the list will contain a Just values.
 *
 * @result The list of optional values with the properties given above
 **/
generic gFiniteDefault a :: [Maybe a]
derive gFiniteDefault Bool, Char, Int, Real, String, (->), UNIT, PAIR, EITHER, CONS, FIELD, OBJECT, RECORD
derive gFiniteDefault [], [!], [ !], [!!], {}, {!}
derive gFiniteDefault (), (,), (,,),  (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)
