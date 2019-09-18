definition module iTasks.WF.Tasks.Core
/**
* This module provis the builtin basic tasks
*/
import iTasks.WF.Definition
import iTasks.SDS.Definition

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage
from iTasks.UI.Editor import :: EditMode

/**
* Lifts a value to the task domain. The task finishes immediately and yields its parameter
* as result of the task.
*
* @param Value: The value to be returned
*				@default ()
* @return A task that will return the value defined by the parameter
*/
treturn :: !a -> Task a | iTask a

/**
* Exception throwing. This will throw an exception of arbitrary type e which has to be caught
* by a higher level exception handler combinator.
*
* @param Value: The exception value
* @return The combined task
*/
throw :: !e -> Task a | iTask a & iTask, toString e

/**
* Evaluate a "World" function that does not yield any result once.
*
* @param World function: The function to evaluate
* @return A () task that evaluates the function
*/
appWorld :: !(*World -> *World)			-> Task ()

/**
* Evaluate a "World" function that also returns a value once.
*
* @param World function: The function to evaluate
* @return A task that evaluates the function and yield a
*/
accWorld :: !(*World -> *(a,*World))	-> Task a | iTask a

/**
* Evaluate a "World" function that also returns a MaybeError value.
* If the MaybeError value is Error, the error is transformed.
* @param World function: The function to evaluate
* @param Error function: Error transformation function
*
* @return A  task that evaluates the function
*/
accWorldError   :: !(*World -> (MaybeError e a, *World)) !(e -> err) -> Task a | iTask a & TC, toString err

/**
* Evaluate a "World" function that also returns a MaybeOSError value.
* If the MaybeError value is Error, the error is transformed.
* @param World function: The function to evaluate
* @param Error function: Error transformation function
*
* @return A task that evaluates the function
*/
accWorldOSError :: !(*World -> (MaybeOSError a, *World))             -> Task a | iTask a

:: OSException			= OSException !OSError
instance toString OSException

/**
* Core interaction task. All other interaction tasks are derived from this one.
* There are two almost identical versions:
* The `interactRW` version can update the given sds.
* The `interactR` version only reads, which means it can also be used for sds's that are not writable.
*/
:: InteractionHandlers l r w v =
    { onInit    :: !(r -> (l, EditMode v))
    , onEdit    :: !(v l -> (l, Maybe (r -> w)))
    , onRefresh :: !(r l (Maybe v) -> (l, v, Maybe (r -> w)))
	}

//Version which can write shared data
interactRW :: !(sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v) | iTask l & iTask r & iTask v & TC r & TC w & RWShared sds
//Version which does not write shared data
interactR :: (sds () r w) (InteractionHandlers l r w v) (Editor v) -> Task (l,v) | iTask l & iTask r & iTask v & TC r & TC w & Registrable sds
