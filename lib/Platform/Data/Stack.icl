implementation module Data.Stack
import Data.Maybe
import StdList, StdOverloaded

newStack :: Stack a
newStack = Stack []

instance length Stack
where
	length (Stack a) = length a

push :: a !(Stack a) -> Stack a
push a (Stack as) = Stack [a : as]

pop :: !(Stack a) -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, newStack)
pop (Stack [a : as]) = (Just a, Stack as)

peek :: !(Stack a) -> Maybe a
peek (Stack []) = Nothing
peek (Stack [a : _]) = Just a
