definition module iTasks.Extensions.Distributed._Evaluation

import iTasks

/*
 * Evaluate a remote task.
 *
 * @param Remote task.
 * @param Task that handles a value change.
 * @return Task result of remote task.
 */
evalRemoteTask :: (Task a) ((TaskValue a) -> Task ()) -> Task a | iTask a

proxyTask :: (Shared sds (TaskValue a)) (*IWorld -> *IWorld) -> (Task a) | iTask a & RWShared sds

taskValueShare :: Int ->  SimpleSDSLens (TaskValue a) | iTask a
