definition module iTasks.Internal.SDS

import Data.GenEq
import System.FilePath, Data.Maybe, Data.Either, Data.Error, System.Time, Text.GenJSON
from Data.Set import :: Set
from iTasks.Internal.IWorld import :: IWorld, :: ConnectionId
from iTasks.Internal.Generic.Visualization import :: TextFormat

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: TaskException, :: TaskId, :: InstanceNo
import iTasks.SDS.Definition

instance Identifiable SDSSource
instance Readable SDSSource
instance Writeable SDSSource
instance Modifiable SDSSource
instance Registrable SDSSource

instance Identifiable SDSLens
instance Readable SDSLens
instance Writeable SDSLens
instance Modifiable SDSLens
instance Registrable SDSLens

instance Identifiable SDSCache
instance Readable SDSCache
instance Writeable SDSCache
instance Modifiable SDSCache
instance Registrable SDSCache

instance Identifiable SDSSequence
instance Readable SDSSequence
instance Writeable SDSSequence
instance Modifiable SDSSequence
instance Registrable SDSSequence

instance Identifiable SDSSelect
instance Readable SDSSelect
instance Writeable SDSSelect
instance Modifiable SDSSelect
instance Registrable SDSSelect

instance Identifiable SDSParallel
instance Readable SDSParallel
instance Writeable SDSParallel
instance Modifiable SDSParallel
instance Registrable SDSParallel

instance Identifiable SDSRemoteService
instance Readable SDSRemoteService
instance Writeable SDSRemoteService
instance Modifiable SDSRemoteService
instance Registrable SDSRemoteService

instance Identifiable SDSRemoteSource
instance Readable SDSRemoteSource
instance Writeable SDSRemoteSource
instance Modifiable SDSRemoteSource
instance Registrable SDSRemoteSource

instance Identifiable SDSDebug
instance Readable SDSDebug
instance Writeable SDSDebug
instance Modifiable SDSDebug
instance Registrable SDSDebug

:: DeferredWrite = E. p r w sds: DeferredWrite !p !w !(sds p r w) & iTask p & TC r & TC w & RWShared sds

//Internal creation functions:

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	SDSSource p r w

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	SDSSource p r ()

createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	SDSSource p r ()

// Helper to immediately get the read value from a share. Use only when reading in an EmptyContext.
directResult :: (AsyncRead r w) -> r

//Internal access functions
sdsIdentity :: !(sds p r w) -> SDSIdentity | Identifiable sds

:: AsyncRead r w = ReadingDone r
	| E. sds: Reading (sds () r w) & TC r & TC w & Readable sds & Registrable sds

/*
 * Read the SDS. TaskContext is used to determine whether a read is done in the
 * context of a task. The read is performed asynchronously when there is a task
 * context and the share is a asynchronous share.
 *
 * @return A ReadResult, either Queued (a asynchronous read is queued and the
 *	task will be notified when it is ready), or a direct result in the case of
 *	a blocking read.
 */
read 			:: !(sds () r w) 			!TaskContext !*IWorld -> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Readable sds

//Read an SDS and register a taskId to be notified when it is written
readRegister	:: !TaskId                  !(sds () r w) !*IWorld -> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Readable, Registrable sds

:: AsyncWrite r w = WritingDone
	| E. sds: Writing (sds () r w) & Writeable sds & TC r & TC w

//Write an SDS (and queue evaluation of those task instances which contained tasks that registered for notification)
write			:: !w					    !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncWrite r w), !*IWorld)	| TC r & TC w & Writeable sds


:: AsyncModify r w = ModifyingDone w & TC w
	| E. sds: Modifying (sds () r w) (r -> w) & Modifiable sds & TC r & TC w

//Read followed by write. The 'a' typed value is a result that is returned
modify :: !(r -> w)          !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncModify r w), !*IWorld) | TC r & TC w & Modifiable sds

//Clear all registrations for the given tasks.
//This is normally called by the queueRefresh functions, because once a task is queued
//for evaluation anyway, it no longer make sense to notify it again.
clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld

queueNotifyEvents :: !String !(Set (!TaskId, !Maybe RemoteNotifyOptions)) !*IWorld -> *IWorld

//List all current registrations (for debugging purposes)
listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)

formatSDSRegistrationsList :: [SDSNotifyRequest] -> String

//Flush all deffered/cached writes of
flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)

// Used to turn any read/write/modified operation (with all arguments except the environment
// curried in) into one which returns a dynamic. Use to store sdsEvalStates in the environment.
dynamicResult :: (*IWorld -> (MaybeError TaskException a, !*IWorld)) !*IWorld -> (MaybeError TaskException Dynamic, !*IWorld) | TC a
