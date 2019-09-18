definition module iTasks.Internal.AsyncSDS

from iTasks.Internal.TaskState import :: AsyncAction
import iTasks.SDS.Definition
from iTasks.WF.Definition import :: TaskId, :: TaskValue, :: Event, :: TaskEvalOpts, :: TaskResult
from iTasks.Internal.IWorld import :: IOState, :: IOStates
from iTasks.Internal.SDS import :: SDSIdentity, :: SDSNotifyRequest

:: SDSRequest p r w = E. sds: SDSReadRequest !(sds p r w) p & gText{|*|} p & TC p & TC r & TC w & Readable sds
	/*
	 * sds: SDS to read
	 * p: parameter with which to read the SDS
	 * reqSDSId: id of the original SDS read
	 * remoteSDSId: id of the current remote sds, so that refresh events can be sent using this identity.
	 * taskId: taskId of the task on the current instance
	 * port: Port which to send refresh messages on
	 */
	| E. sds: SDSRegisterRequest !(sds p r w) !p !SDSIdentity !SDSIdentity !TaskId !Int & gText{|*|} p & TC p & TC r & TC w & Registrable sds & Readable sds
	| E. sds: SDSWriteRequest !(sds p r w) !p !w & gText{|*|} p & TC p & TC r & TC w & Writeable sds
	| E. sds: SDSModifyRequest !(sds p r w) !p (r -> MaybeError TaskException w) & gText{|*|} p & TC p & TC r & TC w & Modifiable sds
	| SDSRefreshRequest TaskId SDSIdentity

/**
 * Queue a read or register operation.
 * @param the sds
 * @param the parameter
 * @param the taskId which queues the operation
 * @param should we register?
 * @param the identity of the original sds
 */
queueRead :: !(SDSRemoteSource p r w) p !TaskId !Bool !SDSIdentity !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w

/**
 * Queue a read to an external service.
 * @param the sds
 * @param the parameter
 * @param the taskId which queues the operation
 */
queueServiceRequest :: !(SDSRemoteService p r w) p !TaskId !Bool !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r

queueServiceWriteRequest :: !(SDSRemoteService p r w) !p !w !TaskId !*IWorld -> (MaybeError TaskException (Maybe ConnectionId), !*IWorld) | TC p & TC w

/**
 * Queue that a task on a remote service should refresh itself.
 * @param Remote notify requests
 */
queueRemoteRefresh :: ![(TaskId, RemoteNotifyOptions)] !*IWorld -> *IWorld

/**
 * Queue a write operation to a remote sds.
 * @param the value to be written
 * @param the sds
 * @param the parameter
 * @param the taskId which queues the operation
 */
queueWrite :: !w !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w

/**
 * Queue a modify operation to a remote sds.
 * @param the modify function
 * @param the sds
 * @param the parameter
 * @param the taskId which queues the operation
 */
queueModify :: !(r -> MaybeError TaskException w) !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w

/**
 * Queries IOStates to see whether a read operation has yielded a result.
 * @param the share, required for typing reasons.
 * @param taskId which is evaluating the share.
 * @param connectionId the connection to check for a value.
 * @param container for io states to check.
 * @returns
 * 	Left: No IO State for task or the connection id is not valid.
 * 	Right: IO state is found.
 *		Nothing: No value yet, asynchronous action is still pending.
 *		Just: A value of type r is found.
 */
getAsyncReadValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe r) | TC r

getAsyncServiceValue :: !(SDSRemoteService p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe r) | TC r & TC w & TC p

getAsyncServiceWriteValue :: !(SDSRemoteService p r w) !TaskId !ConnectionId !IOStates -> MaybeError TaskException (Maybe (SDSNotifyPred p)) | TC p & TC w & TC r

/**
 * Queries IOStates to see whether a write operation has yielded a result.
 * @param the share, required for typing reasons.
 * @param taskId which is evaluating the share.
 * @param connectionId the connection to check for a value.
 * @param container for io states to check.
 * @returns
 * 	Left: No IO State for task or the connection id is not valid.
 * 	Right: IO state is found.
 *		Nothing: No value yet, asynchronous action is still pending.
 *		Just: A value of type w is found.
 */
getAsyncWriteValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe ()) | TC w

/**
 * Queries IOStates to see whether a modify operation has yielded a result.
 * @param the share, required for typing reasons.
 * @param taskId which is evaluating the share.
 * @param connectionId the connection to check for a value.
 * @param container for io states to check.
 * @result
 * 	Left: No IO State for task or the connection id is not valid.
 * 	Right: IO state is found.
 *		Nothing: No value yet, asynchronous action is still pending.
 *		Just: A value of type (r,w) is found.
 */
getAsyncModifyValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe (r,w)) | TC w & TC r

/**
 * The default UI during the loading of an asynchronous SDS
 */
asyncSDSLoaderUI :: !AsyncAction -> UI

/**
 * Completely load an sds and continue with the continuation
 *
 * @param sds
 * @param value during loading
 * @param continuation
 * @param event
 * @param taskevalopts
 * @param iworld
 * @result taskresult and iworld with the continuation embedded
 */
readCompletely :: (sds () r w) (TaskValue a) (r Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld
	-> *(TaskResult a, *IWorld) | Readable sds & TC r & TC w

/**
 * Completely write an sds and continue with the continuation
 *
 * @param value to write
 * @param sds
 * @param value during loading
 * @param continuation
 * @param event
 * @param taskevalopts
 * @param iworld
 * @result taskresult and iworld with the continuation embedded
 */
writeCompletely :: w (sds () r w) (TaskValue a) (Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld
	-> *(TaskResult a, *IWorld) | Writeable sds & TC r & TC w

/**
 * Completely modify an sds and continue with the continuation
 *
 * @param modification function
 * @param sds
 * @param value during loading
 * @param ui during loading
 * @param continuation
 * @param event
 * @param taskevalopts
 * @param iworld
 * @result taskresult and iworld with the continuation embedded
 */
modifyCompletely :: (r -> w) (sds () r w) (TaskValue a) (Event -> UIChange) (w Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld -> *(TaskResult a, *IWorld) | TC r & TC w & Modifiable sds

/**
 * Completely readRegister an sds and continue with the continuation
 *
 * @param sds
 * @param value during loading
 * @param ui during loading
 * @param continuation
 * @param event
 * @param taskevalopts
 * @param iworld
 * @result taskresult and iworld with the continuation embedded
 */
readRegisterCompletely :: (sds () r w) (TaskValue a) (Event -> UIChange) (r Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld
	-> *(TaskResult a, *IWorld) | TC r & TC w & Registrable sds
