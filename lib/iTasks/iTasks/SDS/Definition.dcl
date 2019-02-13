definition module iTasks.SDS.Definition
/**
* This module provides the types that define a shared data source
*/
from iTasks.WF.Definition import :: TaskException, class iTask, :: TaskId
from iTasks.Internal.IWorld import :: IWorld, :: ConnectionId

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import Data.GenEq, Internet.HTTP, Data.Maybe.Ord

from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from Data.Set import :: Set

derive gText SDSNotifyRequest, RemoteNotifyOptions

:: SDSIdentity  :== String

:: TaskContext = EmptyContext // Used in the internals of the iTasks system
               | TaskContext TaskId // Used when a local task is reading from a share
               // Used when a remote task is reading from a share locally
               | RemoteTaskContext !TaskId  // The id of the original task reading the share
                                   !TaskId // The id of the current task handling the request
                                   !SDSIdentity // The id of the share on the remote server
                                   !String // The host to which to send a refresh notification
                                   !Int  // The port to which to send a refresh notification

:: ReadResult p r w =
	/**
	 * Reading from the share has yielded a result. Where applicable, all asynchronous operations have finished.
	 */
	E. sds: ReadResult !r !(sds p r w) & RWShared sds & TC r & TC w
	/**
	 * Reading from the share has not yet yielded a result because some asynchronous operation has not finished.
	 * We return a new version of the share, which MUST be used for the next read operation.
	 */
	| E. sds: AsyncRead !(sds p r w) & RWShared sds & TC r & TC w

:: WriteResult p r w =
	/**
	 * Writing to the share has succeeded. Where applicable, all asynchronous operations have finished.
	 */
	E. sds: WriteResult !(Set (!TaskId, !Maybe RemoteNotifyOptions)) !(sds p r w) & TC r & TC w & RWShared sds
	/**
	 * Denotes that writing to a SDS had lead to some asynchronous action.
	 * We return a new version of the share, which MUST be used for the next write operation.
	 * The SDS is required to be a Readable AND Writeable, because writing to a SDS may require reading from another.
	 */
	| E. sds: AsyncWrite !(sds p r w) & RWShared sds & TC r & TC w

:: ModifyResult p r w =
	/**
	 * Modifying the share has succeeded, all asynchronous operations have finished.
	 */
	E.sds: ModifyResult !(Set (!TaskId, !Maybe RemoteNotifyOptions)) !r !w !(sds p r w) & TC r & TC w & RWShared sds
	/**
	 * Modifying has not yet succeeded because some asynchronous operation has not finished.
	 * We return a new version of the share, which MUST be used for the next modify operation.
	 * TODO: We include the modify function so that async operations can be resumed later. This should
	 * 		 not be necessary.
	 */
	| E. sds: AsyncModify !(sds p r w) !(r -> MaybeError TaskException w) & RWShared sds

//Notification requests are stored in the IWorld
:: SDSNotifyRequest =
	{ reqTaskId     :: !TaskId       //* Id of the task that read the SDS. This Id also connects a chain of notify requests that were registered together
	, reqSDSId      :: !SDSIdentity  //* Id of the actual SDS used to create this request (may be a derived one)
	, cmpParam      :: !Dynamic      //* Parameter we are saving for comparison
	, cmpParamText  :: !String       //* String version of comparison parameter for tracing
	, remoteOptions :: !Maybe RemoteNotifyOptions //* When the notify request is made from another client, this field
												  //* include the information to send a refresh event to that client.
	}

instance < SDSNotifyRequest, RemoteNotifyOptions

:: RemoteNotifyOptions =
	{ hostToNotify :: !String
	, portToNotify :: !Int
	, remoteSdsId  :: !String
	}

class Identifiable sds
where
	/**
	 * Identify the shared datasource
	 */
	nameSDS :: !(sds p r w) ![String] -> [String]

class Readable sds | Identifiable sds
where
	/**
	 * Read from a sds
	 * @param sds to read from.
	 * @param parameter used for reading
	 * @param context in which to read. Async sdss use the context to retrieve the task id.
	 */
	readSDS :: !(sds p r w) !p !TaskContext !*IWorld
	        -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class Registrable sds | Readable sds
where
	/**
	 * Register to a sds. Reads the value and registers the task to get a refresh event when the sds is changed.
	 * @param sds to read from and register to.
	 * @param parameter used for reading
	 * @param context in which to read. Async sds's use the context to retrieve the task id.
	 * @param taskId which registers itself for changes to the sds.
	 * @param Identity of the sds to read at the top of the tree, can be different from the sds given as parameter.
	 */
	readRegisterSDS :: !(sds p r w) !p !TaskContext !TaskId !SDSIdentity !*IWorld
	                -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class Writeable sds | Identifiable sds
where
	/**
	 * Write a value directly to a sds.
	 * @param sds to write to.
	 * @param parameter used for writing
	 * @param context in which to write. Async sdss use the context to retrieve the task id.
	 * @param value which to write to the sds.
	 */
	writeSDS :: !(sds p r w) !p !TaskContext !w !*IWorld
	         -> *(!MaybeError TaskException (WriteResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class Modifiable sds | Readable, Writeable sds
where
	/**
	 * Modify the SDS with the given function
	 * @param Function to apply to the SDS value
	 * @param the sds to modify
	 * @param parameter
	 * @param The context in which to read/write to the SDS
	 */
	modifySDS :: !(r -> MaybeError TaskException w) !(sds p r w) !p !TaskContext !*IWorld
	          -> *(!MaybeError TaskException (ModifyResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w

class RWShared sds | Modifiable, Registrable sds

/**
 * A SDS with no parameters and equal read and write types.
 */
:: Shared sds a :== sds () a a

:: SDSShareOptions =
	{ domain :: !String
	, port   :: !Int
	}

instance toString (WebServiceShareOptions p r w)

//For notification we need a predicate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== Timespec p -> Bool

/**
 * A SDSSource with no parameter and equal read and write type.
 */
:: SimpleSDSSource a :== SDSSource () a a

//Sources provide direct access to a data source
:: SDSSource p r w = SDSSource !(SDSSourceOptions p r w)

	// Allows for some keeping of local state. Writing to a SDS may require reading from that SDS.
	// In the case that this reading is asynchronous, writing could also be asynchronous. This
	// option allows to temporarily store the read result, so that we can start rewriting in order
	//  to write to the SDS, using the stored read value.
	| E. sds: SDSValue !Bool !r !(sds p r w) & RWShared sds & TC p & TC r & TC w

:: SDSSourceOptions p r w =
	{ name  :: !String
	, read  :: !p *IWorld -> *(!MaybeError TaskException r, !*IWorld)
	, write :: !p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld)
	}

/**
 * A SDSLens with no parameter and equal read and write type.
 */
:: SimpleSDSLens a :== SDSLens () a a

//Lenses select and transform data
:: SDSLens p r w  = E. ps rs ws sds: SDSLens !(sds ps rs ws) !(SDSLensOptions p r w ps rs ws)
                  & RWShared sds & gText{|*|} ps & TC ps & TC rs & TC ws

:: SDSLensOptions p r w ps rs ws =
	{ name    :: !String
	, param   :: !p -> ps
	, read    :: !SDSLensRead p r rs
	, write   :: !SDSLensWrite p w rs ws
	, notify  :: !SDSLensNotify p p w rs
	, reducer :: !Maybe (SDSReducer p ws w)
	}
/**
A SDSLens may possibly have a reducer. ONLY when a reducer is present is it possible to atomically
modify an underlying asynchronous share when modifying the lens.

A reducer is needed because we can no longer read the value first, apply the modification function,
and write the value. The underlying share may not be a local share and when this is the case anyone
else could change the share between reading and writing. To deal with this we need to transform the
modification function from r -> w to rs -> ws. This can be done using the existing read and write
functions. However, modifying the underlying share yield a value of type ws which is not of the
required type w. The reducer has the job to turn this ws into w.

*/
:: SDSReducer p ws w :== p ws -> MaybeError TaskException w

:: SDSLensRead p r rs
	= SDSRead      !(p rs -> MaybeError TaskException r) //* Read original source and transform
	| SDSReadConst !(p -> r)                             //* No need to read the original source

:: SDSLensWrite p w rs ws
	= SDSWrite      !(p rs w  -> MaybeError TaskException (Maybe ws)) //* Read original source, and write updated version
	| SDSWriteConst !(p w     -> MaybeError TaskException (Maybe ws)) //* No need to read the original source

:: SDSLensNotify pw pq w rs
	= SDSNotify      !(pw rs w -> SDSNotifyPred pq)
	| SDSNotifyConst !(pw w    -> SDSNotifyPred pq)

/**
 * A SDSSelect with no parameter and equal read and write type.
 */
:: SimpleSDSSelect a :== SDSSelect () a a

//Merge two sources by selecting one based on the parameter
:: SDSSelect p r w = E. p1 p2 sds1 sds2: SDSSelect !(sds1 p1 r w) !(sds2 p2 r w) !(SDSSelectOptions p r w p1 p2)
                   & RWShared sds1 & RWShared sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r & TC w

:: SDSSelectOptions p r w p1 p2 =
	{ name    :: !String
	, select  :: !p -> Either p1 p2
	, notifyl :: !SDSLensNotify p1 p2 w r
	, notifyr :: !SDSLensNotify p2 p1 w r
	}

/**
 * A SDSParallel with no parameter and equal read and write type.
 */
:: SimpleSDSParallel a :== SDSParallel () a a

//Read from and write to two independent SDS's
:: SDSParallel p r w
	= E. p1 r1 w1 p2 r2 w2 sds1 sds2:
	  SDSParallel !(sds1 p1 r1 w1) !(sds2 p2 r2 w2) !(SDSParallelOptions p1 r1 w1 p2 r2 w2 p r w)
	  & RWShared sds1 & RWShared sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w2
	| E. p1 r1 p2 r2 w2 sds1 sds2:
	  SDSParallelWriteLeft !(sds1 p1 r1 w) !(sds2 p2 r2 w2) !(SDSParallelOptions p1 r1 w p2 r2 w2 p r w)
	  & RWShared sds1 & Registrable sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w2 & TC w
	| E. p1 r1 w1 p2 r2 sds1 sds2:
	  SDSParallelWriteRight !(sds1 p1 r1 w1) !(sds2 p2 r2 w) !(SDSParallelOptions p1 r1 w1 p2 r2 w p r w)
	  & Registrable sds1 & RWShared sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w
	| E. p1 r1 w1 p2 r2 w2 sds1 sds2:
	  SDSParallelWriteNone !(sds1 p1 r1 w1) !(sds2 p2 r2 w2) !(SDSParallelOptions p1 r1 w1 p2 r2 w2 p r w)
	  & Registrable sds1 & Registrable sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w2

:: SDSParallelOptions p1 r1 w1 p2 r2 w2 p r w =
	{ name   :: !String
	, param  :: !p -> (!p1, !p2)
	, read   :: !(!r1, !r2) -> r
	, writel :: !SDSLensWrite p w r1 w1
	, writer :: !SDSLensWrite p w r2 w2
	}

/**
 * A SDSSequence with no parameter and equal read and write type.
 */
:: SimpleSDSSequence a :== SDSSequence () a a

//Read from and write to two dependent SDS's
//The read value from the first is used to compute the parameter for the second
:: SDSSequence p r w =
	E. p1 r1 w1 p2 r2 w2 sds1 sds2:
	SDSSequence !(sds1 p1 r1 w1)
	            !(sds2 p2 r2 w2)
	            !(SDSSequenceOptions p1 r1 w1 p2 r2 w2 p r w)
	& RWShared sds1 & RWShared sds2 & gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w2

:: SDSSequenceOptions p1 r1 w1 p2 r2 w2 p r w =
	{ name   :: !String
	, paraml :: !p -> p1
	, paramr :: !p r1 -> p2
	, read   :: !p r1 -> Either r ((r1,r2) -> r)
	, writel :: !SDSLensWrite p w r1 w1
	, writer :: !SDSLensWrite p w r2 w2
	}

/**
 * A SDSCache with no parameter and equal read and write type.
 */
:: SimpleSDSCache a :== SDSCache () a a

// TODO: For some reason, gText{|*|} p & TC p is not sufficient and causes overloading errors in the implementation of Readable and Writeable for SDSCache. iTask p seems to solve this for unknown reasons.
:: SDSCache p r w = E. sds: SDSCache !(SDSSource p r w) !(SDSCacheOptions p r w) & iTask p & TC r & TC w
:: SDSCacheOptions p r w  =
	{ write :: !p (Maybe r) (Maybe w) w -> (!Maybe r, !SDSCacheWrite)
	}

:: SDSCacheWrite = WriteNow | WriteDelayed | NoWrite

/**
 * A SDSRemoteSource with no parameter and equal read and write type.
 */
:: SimpleSDSRemoteSource a :== SDSRemoteSource () a a

/**
 * A SDSRemoteSource is a share tree living on another system. Evaluating it will cause an
 * asynchronous message to be sent to the other server to retrieve the value for the
 * specified operation.
 */
:: SDSRemoteSource p r w = E. sds: SDSRemoteSource !(sds p r w) !(Maybe ConnectionId) !SDSShareOptions & RWShared sds

:: WebServiceShareOptions p r w = HTTPShareOptions !(HTTPHandlers p r w)
                                | TCPShareOptions !(TCPHandlers p r w)

:: HTTPHandlers p r w =
	{ host          :: !String
	, port          :: !Int
	, createRequest :: !p -> HTTPRequest
	, fromResponse  :: !HTTPResponse p -> MaybeErrorString r
	, writeHandlers :: !Maybe (!p w -> HTTPRequest, !p HTTPResponse -> MaybeErrorString (SDSNotifyPred p))
	}

:: TCPHandlers p r w =
	{ host                 :: !String
	, port                 :: !Int
	, createMessage        :: !p -> String
	, fromTextResponse     :: !String p Bool -> MaybeErrorString (!Maybe r, !Maybe String)
	, writeMessageHandlers :: !Maybe (!p w -> String, !p String -> MaybeErrorString (Maybe (SDSNotifyPred p)))
	}

/**
 * A SDSRemoteService with no parameter and equal read and write type.
 */
:: SimpleSDSRemoteService a :== SDSRemoteService () a a

/**
 * A SDSRemoteServive is a share which allows you to connect to the outside world.
 * For now it just allows you to send HTTP messages and receive responses asynchronously.
 */
:: SDSRemoteService p r w = SDSRemoteService !(Maybe ConnectionId) !(WebServiceShareOptions p r w)

:: SDSDebug p r w = E. sds: SDSDebug !String !(sds p r w) & RWShared sds
