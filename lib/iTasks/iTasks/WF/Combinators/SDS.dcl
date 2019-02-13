definition module iTasks.WF.Combinators.SDS
/**
* This module provides task combinators that give tasks access to shared sources
*/
import iTasks.WF.Definition
import iTasks.SDS.Definition
from System.FilePath import :: FilePath

/**
* Provide a local read/write shared for a task to work on.
*
* @param The initial value of the shared variable
* @param The task which uses the shared variable
*/
withShared :: !b !((SimpleSDSLens b) -> Task a) -> Task a | iTask a & iTask b

/**
* Expose the taskId during execution
*/
withTaskId :: (Task a) -> Task (a, TaskId)

/**
* Creates a temporary directory on the server's file system for the duration of a task
*
* @param The task that gets the temporary directory's location as argument
*
* @return The result of the task
*/
withTemporaryDirectory :: (FilePath -> Task a) -> Task a | iTask a
