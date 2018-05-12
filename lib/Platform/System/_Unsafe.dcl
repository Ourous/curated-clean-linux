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

