definition module Data.Stack
from Data.Maybe import :: Maybe
from StdOverloaded import class length

:: Stack a =: Stack [a]

/**
 * Create a new, empty Stack.
 */
newStack :: Stack a

/**
 * Check whether a Stack is empty.
 * @type (Stack a) -> Bool
 */
empty a :== case a of
	Stack [] -> True
	_	 -> False

instance length Stack

/**
 * Add an element to the Stack.
 */
push :: a !(Stack a) -> Stack a

/**
 * Remove element from the Stack and return it if the Stack is not empty.
 */
pop :: !(Stack a) -> (Maybe a, Stack a)

/**
 * Peek the top of the Stack.
 */
peek :: !(Stack a) -> Maybe a
