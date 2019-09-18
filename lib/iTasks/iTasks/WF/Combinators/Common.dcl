definition module iTasks.WF.Combinators.Common
/**
* This module contains a collection of useful iTasks combinators defined in terms of the basic iTask combinators.
*/
import iTasks.SDS.Definition
import iTasks.WF.Combinators.Core

from StdBool					import not
from Data.Map				    import :: Map
from Data.Either				import :: Either

/**
* Infix shorthand for step combinator
*
* @param Task: The task for which continuations are defined
* @param The possible continuations
* @return The continuation's result
*/
(>>*) infixl 1 :: !(Task a) ![TaskCont a (Task b)] -> Task b | iTask a & iTask b

//Standard monadic operations:

/**
* Combines two tasks sequentially. The first task is executed first. When it has a value
* the user may continue to the second task, which is executed with the result of the first task as parameter.
* If the first task becomes stable, the second task is started automatically.
*
* @param First: The first task to be executed
* @param Second: The second task, which receives the result of the first task
* @return The combined task
*/
tbind :: !(Task a) !(a -> Task b) 			-> Task b		| iTask a & iTask b
/**
* Combines two tasks sequentially but explicitly waits for user input to confirm the completion of
* the first task.
*
* @param First: The first task to be executed
* @param Second: The second task, which receives the result of the first task
* @return The combined task
*/
(>>!) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
/**
* Combines two tasks sequentially but continues only when the first task has a stable value.
*
* @param First: The first task to be executed
* @param Second: The second task, which receives the result of the first task
* @return The combined task
*/
(>>-) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
/**
* Combines two tasks sequentially but continues only when the first task has a stable value.
*
* @param First: The first task to be executed
* @param Second: The second task
* @return The combined task
* @type (Task a) (Task b) -> Task b | iTask a & iTask b
*/
(>-|) infixl 1
(>-|) x y :== x >>- \_ -> y
/**
* Combines two tasks sequentially but continues only when the first task has a value.
*
* @param First: The first task to be executed
* @param Second: The second task, which receives the result of the first task
* @return The combined task
*/
(>>~) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
/**
* Combines two tasks sequentially just as >>=, but the result of the second task is disregarded.
*
* @param First: The first task to be executed
* @param Second: The second task to be executed
*
* @return The combined task
*/
(>>^) infixl 1 :: !(Task a) (Task b) -> Task a| iTask a & iTask b
/**
* Infix shorthand for transform combinator
*
* @param Task: The task on which the transform should be applied
* @param The transformation function to apply
* @return The transformed task
*/
(@?) infixl 1 :: !(Task a) !((TaskValue a) -> TaskValue b) -> Task b
/**
* Infix shorthand for transform combinator which only deals which transforms valid results
*
* @param Task: The task on which the transform should be applied
* @param The transformation function to apply
* @return The transformed task
*/
(@) infixl 1 :: !(Task a) !(a -> b) -> Task b
/**
* Infix shorthand for transform combinator which maps results to a constant value
* Stability of the task is propagated from the original value
*
* @param Task: The task on which the transform should be applied
* @param The constant task value
* @return The transformed task
*/
(@!) infixl 1 :: !(Task a) !b -> Task b
/**
* 'Sidestep' combinator. This combinator has a similar signature as the core 'step'
* combinator, but instead of moving forward to a next step, the selected step is executed
* in parallel with the first task. When the chosen task step becomes stable, it is removed
* and the actions are enabled again.
*/
(>^*) infixl 1 :: !(Task a) ![TaskCont a (Task b)] -> Task a | iTask a & iTask b
sideStep       :: !(Task a) ![TaskCont a (Task b)] -> Task a | iTask a & iTask b

/**
* Exception combinator.
*
* @param Task: The normal task which will possibly raise an exception of type e
* @param Handler: The exception handling task which gets the exception as parameter
* @return The combined task
*/
try 		:: !(Task a) (e -> Task a) 			-> Task a 	| iTask a & iTask, toString e
/**
* Catches all exceptions.
*
* @param Task: The normal task which will possibly raise an exception of any type
* @param Handler: The exception handling task
*/
catchAll	:: !(Task a) (String -> Task a)		-> Task a | iTask a

/**
* Execute a Maybe task that you expect to always return Just.
* Throw an exception if it returns nothing
*
* @param The task that could in theory return Nothing
* @return The result of the task
*/
justdo	:: !(Task (Maybe a)) -> Task a | iTask a

/**
* Execute the list of tasks one after another.
*
* @param Tasks: The list of tasks to be executed sequentially
* @return The combined task
*/
sequence	:: ![Task a] 						-> Task [a]		| iTask a

/**
* Repeats a task until a given predicate holds. The predicate is tested as soon as the
* given task is finished. When it does not hold, the task is restarted.
*
* @param Task: The task to be looped
* @param Predicate: The predicate over the result of the task to determine if the combination is finished
* @return The combined task
*/
(<!) infixl 6 :: (Task a) (a -> Bool) -> Task a | iTask a

/**
* Repeats a task while carrying a state while a predicate holds
*
* @param Predicate: The predicate that has to hold
* @param State: The initial state
* @param Task: The task that has to be repeate
* @return The combined task
*/
foreverStIf :: (a -> Bool) a !(a -> Task a) -> Task a | iTask a

/**
* Repeats a task while a predicate holds
*
* @param Predicate: The predicate that has to hold
* @param Task: The task that has to be repeate
* @return The combined task
*/
foreverIf :: (a -> Bool) !(Task a) -> Task a | iTask a

/**
* Repeats a task indefinitely while carrying a state
*
* @param State: The initial state
* @param Task: The task that has to be repeate
* @return The combined task
*/
foreverSt :: a !(a -> Task a) -> Task a | iTask a

/**
* Repeats a task indefinitely
*
* @param Task: The task that has to be repeate
* @return The combined task
*/
forever :: (Task a) -> Task a | iTask a

/**
* Group two tasks in parallel, return the result of the first completed task.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
*/
(-||-) infixr 3 	:: !(Task a) !(Task a) 	-> Task a 				| iTask a

/**
* Group two tasks in parallel, return the result of the right task
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
*/
(||-)  infixr 3		:: !(Task a) !(Task b)	-> Task b				| iTask a & iTask b

/**
* Group two tasks in parallel, return the result of the left task
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the task that is completed first
*/
(-||)  infixl 3		:: !(Task a) !(Task b)	-> Task a				| iTask a & iTask b

/**
* Group two tasks in parallel that both need to be completed.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The results of both tasks
*/
(-&&-) infixr 4 	:: !(Task a) !(Task b) 	-> Task (a,b) 			| iTask a & iTask b

/**
* Feed the result of one task as read-only shared to another
*/
feedForward :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task b | iTask a & iTask b

//Infix version of feedForward
(>&>) infixl 1  :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task b | iTask a & iTask b

//Same as feedForward, but with the value of the lefthand side
feedSideways :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task a | iTask a & iTask b

//Infix version of feedSideways
(>&^) infixl 1 :: (Task a) ((SDSLens () (Maybe a) ()) -> Task b) -> Task a | iTask a & iTask b

/**
 * Feed the result of one task as read-only shared to another one and vice versa.
 */
feedBidirectionally :: !((SDSLens () (Maybe b) ()) -> Task a) !((SDSLens () (Maybe a) ()) -> Task b)
                    -> Task (a, b) | iTask a & iTask b

/**
 * Infix version of `feedBidirectionally`.
 * @type ((ReadOnlyShared (Maybe b)) -> Task a) ((ReadOnlyShared (Maybe a)) -> Task b) -> Task (a, b) | iTask a & iTask b
 */
(<&>) infixl 1
(<&>) x y :== feedBidirectionally x y

/**
* Group a list of tasks in parallel.
* The group stops as soon as one result is available which is returned.
*
* @param Tasks: The list of tasks
*
* @return The first result
*/
anyTask				:: ![Task a]			-> Task a				| iTask a

/**
* Group a list of tasks in parallel.
* The group stops when all tasks are completed.
*
* @param Tasks: The list of tasks
*
* @return The list of results
*/
allTasks			:: ![Task a]			-> Task [a]				| iTask a

/**
* Group two tasks in parallel of which only one needs to be completed.
* The tasks can have different types. The 'Either' results indicates which task completed.
*
* @param Left: The left task
* @param Right: The right task
*
* @return The result of the first completed task wrapped in an 'Either'.
*/
eitherTask			:: !(Task a) !(Task b) 	-> Task (Either a b)	| iTask a & iTask b

/**
* Randomly selects one item from a list.
*
* @param Options: The list of options
*
* @return The chosen item
*/
randomChoice		:: ![a]										-> Task a				| iTask a

/**
* Do a task as long while monitoring that a shared state remains unchanged.
* When the share changes the task is restarted
*/
whileUnchanged :: !(sds () r w) (r -> Task b) -> Task b | iTask r & iTask b & Registrable sds & TC w
whileUnchangedWith :: !(r r -> Bool) !(sds () r w) (r -> Task b) -> Task b | iTask r & TC w & iTask b & Registrable sds

/**
* Do a task when there is a Just value in the share
* Do a default task when there is a Nothing value
*/
withSelection :: (Task c) (a -> Task b) (sds () (Maybe a) ()) -> Task b | iTask a & iTask b & iTask c & RWShared sds

/**
* Append a task to the set of top level tasks
*
*/
appendTopLevelTask :: !TaskAttributes !Bool !(Task a) -> Task TaskId | iTask a

/**
 * Do a blocking computation with a pure function while showing a loader
 * @param String to show under the loader
 * @param The value to return
 *
 * @return The return value
 */
compute :: !String a -> Task a | iTask a

//Utility functions for defining task steps
always 		:: b					    (TaskValue a) -> Maybe b
never 		:: b 					    (TaskValue a) -> Maybe b

hasValue	:: (a -> b) 				(TaskValue a) -> Maybe b
ifStable 	:: (a -> b) 				(TaskValue a) -> Maybe b
ifUnstable 	:: (a -> b) 				(TaskValue a) -> Maybe b

ifValue 	:: (a -> Bool) 	(a -> b) 	(TaskValue a) -> Maybe b
ifCond 		:: Bool b 				    (TaskValue a) -> Maybe b

withoutValue :: (Maybe b)      (TaskValue a) -> Maybe b

withValue    :: (a -> Maybe b) (TaskValue a) -> Maybe b

withStable   :: (a -> Maybe b) (TaskValue a) -> Maybe b

withUnstable :: (a -> Maybe b) (TaskValue a) -> Maybe b

//Utility functions for transforming task values
tvHd         :: (TaskValue [a]) -> TaskValue a
tvFst 		 :: (TaskValue (a,b)) -> TaskValue a
tvSnd        :: (TaskValue (a,b)) -> TaskValue b
tvFromMaybe  :: (TaskValue (Maybe a)) -> TaskValue a
tvToMaybe    :: (TaskValue a) -> TaskValue (Maybe a)


