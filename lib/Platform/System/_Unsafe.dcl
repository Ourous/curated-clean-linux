definition module System._Unsafe

/**
* Execute impure functions as side effect of evaluating pure expressions.
*
* WARNING:
* This module provides unsafe and impure functions that can really mess up
* your program when used incorrectly.
* Only use these if you understand the risks of these low-level operations.
*/

/**
* Execute a *World->*World function as side effect of evaluating a pure expression.
*/
appUnsafe :: !(*World -> *World) !.a -> .a

/**
* Execute a function with side effect and use its result as a pure expression.
*/
accUnsafe :: !*(*World -> *(.a, !*World)) -> .a

/**
 * Changes the type of the given argument.
 * This is only safe if both types have the same internal representation, e.g.
 * in case they differ only in phantom types or `a` is a newtype of `b` (or
 * vice versa). The function can be used to prevent reconstructing objects with
 * the same value, but different phantom type.
 *
 * @param The value to be converted
 * @result The converted value
 */
unsafeCoerce :: !.a -> .b
