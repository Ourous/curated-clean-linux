implementation module Data.Queue
import Data.Maybe
import StdList, StdOverloaded
/**
* This module provides a straightforward FIFO queue.
* It is implemented using two list based on Chris Okasaki's example in Purely Functional Data Structures.
*/

newQueue :: Queue a
newQueue = Queue [] []

instance length Queue where length (Queue front rear) = length front + length rear

/**
* Add an element to the queue
*/
enqueue :: a !(Queue a) -> Queue a
enqueue x (Queue front rear) = Queue front [x:rear]

/**
* Take an element from the queue (if the queue is not empty)
*/
dequeue :: !(Queue a) -> (!Maybe a, !Queue a)
dequeue (Queue [] [] ) = (Nothing, Queue [] [])
dequeue (Queue [x:xs] rear) = (Just x, Queue xs rear)
dequeue (Queue [] rear) = let [x:xs] = reverse rear in (Just x, Queue xs [])
