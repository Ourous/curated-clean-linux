implementation module iTasks.Internal.SDS

import StdString, StdTuple, StdMisc, StdBool, StdInt, StdChar, StdFunctions, StdArray
from StdList import flatten, map, take, drop, instance toString [a], instance length []
from Text import class Text, instance Text String
import qualified Text
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Func, Data.Tuple, System.OS, System.Time, Text.GenJSON, Data.Foldable
from Data.Set import instance Foldable Set, instance < (Set a)
import qualified Data.Set as Set
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task, iTasks.Internal.TaskStore, iTasks.Internal.TaskEval

import iTasks.SDS.Sources.Core
import iTasks.WF.Tasks.IO
import Text.GenJSON
import iTasks.Internal.AsyncSDS
import iTasks.Internal.Util
from Text import instance + String

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	SDSSource p r w
createReadWriteSDS ns id read write
	= createSDS ns id read write

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	SDSSource p r ()
createReadOnlySDS read
	= createReadOnlySDSError (\p iworld -> appFst Ok (read p iworld))

createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	SDSSource p r ()
createReadOnlySDSError read
	= createSDS "readonly" "readonly" read (\_ _ iworld -> (Ok (const (const True)), iworld))

createSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	SDSSource p r w
createSDS ns id read write = SDSSource
	{ SDSSourceOptions
	| name = ns +++ ":" +++ id
	, read = read
	, write = write
	}

//Construct the identity of an sds
sdsIdentity :: !(sds p r w) -> SDSIdentity | Identifiable sds
sdsIdentity s = 'Text'.concat (nameSDS s [])

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p iworld = (npred p, iworld)

read            :: !(sds () r w) !TaskContext !*IWorld
	-> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Readable sds
read sds c iworld = case readSDS sds () c iworld of
	(Error e, iworld) = (Error e, iworld)
	(Ok (ReadResult r sds), iworld) = (Ok (ReadingDone r), iworld)
	(Ok (AsyncRead sds), iworld) = (Ok (Reading sds), iworld)

readRegister    :: !TaskId !(sds () r w) !*IWorld
	-> (!MaybeError TaskException (AsyncRead r w), !*IWorld) | TC r & TC w & Readable, Registrable sds
readRegister taskId sds iworld = case readRegisterSDS sds () (TaskContext taskId) taskId (sdsIdentity sds) iworld of
	(Error e, iworld) = (Error e, iworld)
	(Ok (ReadResult r sds), iworld) = (Ok (ReadingDone r), iworld)
	(Ok (AsyncRead sds), iworld) = (Ok (Reading sds), iworld)

mbRegister :: !p (sds p r w) !(Maybe (!TaskId, !SDSIdentity)) !TaskContext !*IWorld -> *IWorld | gText{|*|} p & TC p & Identifiable sds
// When a remote requests a register, we do not have a local task id rather a remote task context which we use to record the request.
mbRegister _ _ Nothing _ iworld = iworld
mbRegister p sds (Just (taskId, reqSDSId)) context iworld=:{IWorld|sdsNotifyRequests, sdsNotifyReqsByTask, world}
	# (ts, world) = nsTime world
	# req = buildRequest context taskId reqSDSId p
	# sdsId = sdsIdentity sds
	= { iworld
	  & world = world
	  , sdsNotifyRequests = 'DM'.alter (Just o maybe ('DM'.singleton req ts) ('DM'.put req ts))
									   sdsId
									   sdsNotifyRequests
	  , sdsNotifyReqsByTask = case context of
			// We do not store remote requests in the tasks map, the task ID's are not local to this instance.
			(RemoteTaskContext _ _ _ _ _)   = sdsNotifyReqsByTask
			_ 								= ('DM'.alter (Just o maybe ('Set'.singleton sdsId) ('Set'.insert sdsId)) taskId sdsNotifyReqsByTask)
	  }
where
	buildRequest (RemoteTaskContext reqTaskId currTaskId remoteSDSId host port) _ reqSDSId p
		= buildRequest` reqTaskId reqSDSId p (Just {hostToNotify=host, portToNotify=port, remoteSdsId=remoteSDSId})
	buildRequest (TaskContext taskId) _ reqSDSId p
		= buildRequest` taskId reqSDSId p Nothing
	buildRequest EmptyContext taskId reqSDSId p
		= buildRequest` taskId reqSDSId p Nothing

	buildRequest` taskId reqSDSId p mbRemoteOptions =
		{ reqTaskId=taskId
		, reqSDSId=reqSDSId
		, cmpParam=dynamic p
		, cmpParamText=toSingleLineText p
		, remoteOptions = mbRemoteOptions}

write :: !w !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncWrite r w), !*IWorld) | TC r & TC w & Writeable sds
write w sds c iworld
= case writeSDS sds () c w iworld of
		(Ok (WriteResult notify _), iworld)
		= (Ok WritingDone, queueNotifyEvents (sdsIdentity sds) notify iworld)
		(Ok (AsyncWrite sds), iworld) = (Ok (Writing sds), iworld)
		(Error e,iworld)    = (Error e,iworld)

directResult :: (AsyncRead r w) -> r
directResult (ReadingDone r) = r
directResult _ = abort "No direct result!"

//Check the registrations and find the set of id's for which the current predicate holds
//and for which id's it doesn't
checkRegistrations :: !SDSIdentity !(SDSNotifyPred p) !*IWorld
                   -> (!Set (!TaskId, !Maybe RemoteNotifyOptions), !Set (!TaskId, !Maybe RemoteNotifyOptions), !*IWorld)
                    | TC p
checkRegistrations sdsId pred iworld
	# (registrations, iworld) 	= lookupRegistrations sdsId iworld
	# (match,nomatch) 			= matchRegistrations pred registrations
	= (match,nomatch,iworld)
where
	//Find all notify requests for the given share id
	lookupRegistrations :: String !*IWorld -> (![(!SDSNotifyRequest, !Timespec)], !*IWorld)
	lookupRegistrations sdsId iworld=:{sdsNotifyRequests} =
		('DM'.toList $ 'DM'.findWithDefault 'DM'.newMap sdsId sdsNotifyRequests, iworld)

	//Match the notify requests against the predicate to determine two sets:
	//The registrations that matched the predicate, and those that did not match the predicate
	matchRegistrations pred [] = ('Set'.newSet,'Set'.newSet)
	matchRegistrations pred [(req=:{SDSNotifyRequest|cmpParam}, reqTimespec):regs]
		# (match,nomatch) = matchRegistrations pred regs
		= case cmpParam of
			(p :: p^) = if (pred reqTimespec p)
						   ('Set'.insert (req.reqTaskId, req.remoteOptions) match,nomatch)
						   (match, 'Set'.insert (req.reqTaskId, req.remoteOptions) nomatch)
			//In case of a type mismatch, just ignore (should not happen)
			_                        = abort "Not matching!"

modify :: !(r -> w)          !(sds () r w) !TaskContext !*IWorld -> (!MaybeError TaskException (AsyncModify r w), !*IWorld) | TC r & TC w & Modifiable sds
modify f sds context iworld
= case modifySDS (\r. Ok (f r)) sds () context iworld of
	(Error e, iworld) 					= (Error e, iworld)
	(Ok (AsyncModify sds _), iworld) 		= (Ok (Modifying sds f), iworld)
	(Ok (ModifyResult notify r w _), iworld)
	# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
	= (Ok (ModifyingDone w), iworld)

queueNotifyEvents :: !String !(Set (!TaskId, !Maybe RemoteNotifyOptions)) !*IWorld -> *IWorld
queueNotifyEvents sdsId notify iworld
    # remotes = [(reqTaskId, remoteOptions) \\ (reqTaskId, Just remoteOptions) <- 'Set'.toList notify]
    # locals = [reqTaskId \\ (reqTaskId, Nothing) <- 'Set'.toList notify]
    # iworld = queueRefresh [(reqTaskId,"Notification for write of " +++ sdsId) \\ reqTaskId <- locals] iworld
    = queueRemoteRefresh remotes iworld

clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld
clearTaskSDSRegistrations taskIds iworld=:{IWorld|sdsNotifyRequests, sdsNotifyReqsByTask}
	# sdsIdsToClear = foldl
		(\sdsIdsToClear taskId -> 'Set'.union ('DM'.findWithDefault 'Set'.newSet taskId sdsNotifyReqsByTask) sdsIdsToClear)
		'Set'.newSet
		taskIds
	= { iworld
	  & sdsNotifyRequests   = foldl clearRegistrationRequests sdsNotifyRequests sdsIdsToClear
	  , sdsNotifyReqsByTask = foldl (flip 'DM'.del) sdsNotifyReqsByTask taskIds
	  }
where
	clearRegistrationRequests :: (Map SDSIdentity (Map SDSNotifyRequest Timespec))
								 SDSIdentity
							  -> Map SDSIdentity (Map SDSNotifyRequest Timespec)
	clearRegistrationRequests requests sdsId
		| 'DM'.null filteredReqsForSdsId = 'DM'.del sdsId requests
		| otherwise                      = 'DM'.put sdsId filteredReqsForSdsId requests
	where
		reqsForSdsId         = fromJust $ 'DM'.get sdsId requests
		filteredReqsForSdsId = 'DM'.filterWithKey (\req _ -> not $ 'Set'.member req.reqTaskId taskIds) reqsForSdsId

listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
listAllSDSRegistrations iworld=:{IWorld|sdsNotifyRequests} = ('DM'.toList ('DM'.foldrWithKey addRegs 'DM'.newMap sdsNotifyRequests),iworld)
where
	addRegs cmpSDSId reqs list = 'DM'.foldlWithKey addReg list reqs
	where
		addReg list {SDSNotifyRequest|reqTaskId=reqTaskId=:(TaskId taskInstance _)} _
			= 'DM'.put taskInstance [(reqTaskId,cmpSDSId):fromMaybe [] ('DM'.get taskInstance list)] list

formatSDSRegistrationsList :: [SDSNotifyRequest] -> String
formatSDSRegistrationsList list = 'Text'.join "\n" lines
where
	lines = [ "Task id " +++ toString reqTaskId +++ ": " +++ reqSDSId +++ " (" +++ cmpParamText +++ ")" \\ {reqTaskId, reqSDSId, cmpParamText} <- list]

formatRegistrations :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String
formatRegistrations list = 'Text'.join "\n" lines
where
	lines = [toString instanceNo +++ " -> " +++
				('Text'.join "\n\t" [toString tId +++ ":" +++ sdsId \\ (tId, sdsId) <- requests])
			\\ (instanceNo, requests) <- list]

flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)
flushDeferredSDSWrites iworld=:{writeCache}
	# (errors,iworld) = flushAll ('DM'.toList writeCache) iworld
	| errors =: [] = (Ok (), {iworld & writeCache = 'DM'.newMap})
	# msg = 'Text'.join OS_NEWLINE ["Could not flush all deferred SDS writes, some data may be lost":map snd errors]
	= (Error (exception msg),{iworld & writeCache = 'DM'.newMap})
where
	flushAll [] iworld = ([],iworld)
	flushAll [(_,(_,DeferredWrite p w sds)):rest] iworld
		= case writeSDS sds p EmptyContext w iworld of
			(Ok (WriteResult notify _),iworld)
				# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
				= flushAll rest iworld
			(Error e,iworld)
				# (errors,iworld) = flushAll rest iworld
				= ([e:errors],iworld)

dynamicResult :: (*IWorld -> (MaybeError TaskException a, !*IWorld)) !*IWorld -> (MaybeError TaskException Dynamic, !*IWorld) | TC a
dynamicResult f iworld = case f iworld of
	(Error e, iworld)   = (Error e, iworld)
	(Ok a, iworld)      = (Ok (dynamic a), iworld)

instance Identifiable SDSSource
where
	nameSDS (SDSSource {SDSSourceOptions|name}) acc = ["$", name, "$":acc]
	nameSDS (SDSValue done mr sds) acc = nameSDS sds acc

instance Readable SDSSource
where
	readSDS sds p c iworld = readSDSSource sds p c Nothing iworld

instance Writeable SDSSource
where
	writeSDS sds=:(SDSSource {SDSSourceOptions|write,name}) p _ w iworld
	= case write p w iworld of
		(Error e, iworld)   = (Error e, iworld)
		(Ok npred, iworld)
			# (match,nomatch, iworld) = checkRegistrations (sdsIdentity sds) npred iworld
			= (Ok (WriteResult match sds), iworld)

	writeSDS (SDSValue False val sds) p c w iworld = case writeSDS sds p c w iworld of
		(Error e, iworld)                   = (Error e, iworld)
		(Ok (AsyncWrite ssds), iworld)      = (Ok (AsyncWrite (SDSValue False val ssds)), iworld)
		(Ok (WriteResult r ssds), iworld)   = (Ok (WriteResult r (SDSValue True val ssds)), iworld)

	writeSDS (SDSValue True val sds) p c w iworld = (Ok (WriteResult 'Set'.newSet sds), iworld)

instance Modifiable SDSSource where
	modifySDS f sds=:(SDSSource {SDSSourceOptions|name}) p context iworld
	= case readSDS sds p context iworld of
		(Error e, iworld)               = (Error e, iworld)
		(Ok (ReadResult r ssds), iworld)     =  case f r of
			Error e                         = (Error e, iworld)
			Ok w                            = case writeSDS ssds p context w iworld of
				(Error e, iworld)               = (Error e, iworld)
				(Ok (WriteResult n ssds), iworld)    = (Ok (ModifyResult n r w ssds), iworld)

	modifySDS f (SDSValue False v sds) p c iworld = case modifySDS f sds p c iworld of
		(Error e, iworld) = (Error e, iworld)
		(Ok (ModifyResult notify r w ssds), iworld) = (Ok (ModifyResult notify r w (SDSValue False v ssds)), iworld)
		(Ok (AsyncModify ssds f), iworld) = (Ok (AsyncModify (SDSValue True v ssds) f), iworld)

	modifySDS f (SDSValue True r sds) p c iworld = case f r of
		Error e = (Error e, iworld)
		Ok w = (Ok (ModifyResult 'Set'.newSet r w (SDSValue True r sds)), iworld)

instance Registrable SDSSource
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSSource sds p c (Just (taskId, reqSDSId)) iworld

readSDSSource :: !(SDSSource p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
              -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSSource sds=:(SDSSource {SDSSourceOptions|read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	= case read p iworld of
		(Error e, iworld) = (Error e, iworld)
		(Ok r, iworld) = (Ok (ReadResult r sds), iworld)

readSDSSource sds=:(SDSValue done v _) _ _ _ iworld = (Ok (ReadResult v sds), iworld)

instance Identifiable SDSLens
where
	nameSDS (SDSLens sds {SDSLensOptions|name}) acc = nameSDS sds ["/[", name, "]":acc]

instance Readable SDSLens
where
	readSDS sds p c iworld = readSDSLens sds p c Nothing iworld

instance Writeable SDSLens
where
	writeSDS sds=:(SDSLens sds1 opts=:{SDSLensOptions|param,write,notify,name}) p c w iworld
	# ps = param p
	= case (write,notify) of
		//Special case: we don't need to read the base SDS
		(SDSWriteConst writef,SDSNotifyConst notifyf)
			//Check which registrations the current parameter matches
			# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) (notifyf p w) iworld
			= case writef p w of
				Error e = (Error e, iworld)
				Ok Nothing
					//We need to decide based on the current parameter if we need to notify or not
					= (Ok (WriteResult match sds), iworld)
				Ok (Just ws) = case writeSDS sds1 ps c ws iworld of
					(Error e, iworld) = (Error e, iworld)
					(Ok (AsyncWrite sds), iworld) = (Ok (AsyncWrite (SDSLens sds opts)), iworld)
					(Ok (WriteResult notify ssds), iworld)
						//Remove the registrations that we can eliminate based on the current parameter
						# notify = 'Set'.difference notify ('Set'.difference nomatch match)
						= (Ok (WriteResult notify (SDSLens ssds opts)), iworld)
		//General case: read base SDS before writing
		_ = case readSDS sds1 ps c iworld of
				(Error e, iworld) = (Error e, iworld)
				(Ok (AsyncRead sds), iworld) = (Ok (AsyncWrite (SDSLens sds opts)), iworld)
				(Ok (ReadResult rs ssds), iworld)
					# ws = case write of
						SDSWrite writef = writef p rs w
						SDSWriteConst writef = writef p w
					# notifyf = case notify of
						SDSNotify notifyf = notifyf p rs w
						SDSNotifyConst notifyf = notifyf p w
					//Check which registrations the current parameter matches
					# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) notifyf iworld
					= case ws of
						Error e         = (Error e, iworld)
						Ok Nothing      = (Ok (WriteResult match (SDSLens ssds opts)), iworld)
						Ok (Just ws) = case writeSDS ssds ps c ws iworld of
							(Error e, iworld) = (Error e, iworld)
							(Ok (AsyncWrite sds), iworld) = (Ok (AsyncWrite (SDSLens sds opts)), iworld)
							(Ok (WriteResult notify ssds), iworld)
								//Remove the registrations that we can eliminate based on the current parameter
								# notify = 'Set'.difference notify ('Set'.difference nomatch match)
								= (Ok (WriteResult notify (SDSLens ssds opts)), iworld)

instance Modifiable SDSLens where
	modifySDS f sds=:(SDSLens sds1 opts=:{SDSLensOptions|param, read, write, reducer, notify, name}) p context iworld
	= case reducer of
		Nothing = case readSDS sds p context iworld of
			(Error e, iworld)               = (Error e, iworld)
			(Ok (AsyncRead sds), iworld)    = (Ok (AsyncModify sds f), iworld)
			(Ok (ReadResult r ssds), iworld)    = case f r of
				Error e                             = (Error e, iworld)
				Ok w                                = case writeSDS ssds p context w iworld of
					(Error e, iworld)                        = (Error e, iworld)
					(Ok (AsyncWrite sds), iworld)            = (Ok (AsyncModify sds f), iworld)
					(Ok (WriteResult notify ssds), iworld)
					= (Ok (ModifyResult notify r w ssds), iworld)

		Just reducer = case modifySDS sf sds1 (param p) context iworld of
			(Error e, iworld)                           = (Error e, iworld)
			(Ok (AsyncModify sds _), iworld)            = (Ok (AsyncModify (SDSLens sds opts) f), iworld)
			(Ok (ModifyResult toNotify rs ws ssds), iworld) = case reducer p ws of
				Error e                                     = (Error e, iworld)
				Ok w
				# notf = case notify of
					SDSNotify f         = f p rs w
					SDSNotifyConst f    = f p w
				= case doRead read p rs of
					Error e = (Error e, iworld)
					Ok r
					# (match, nomatch, iworld) = checkRegistrations (sdsIdentity sds) notf iworld
					# notify = 'Set'.difference toNotify ('Set'.difference nomatch match)
					= (Ok (ModifyResult notify r w (SDSLens ssds opts)), iworld)
		where
			sf rs
			# readV = doRead read p rs
			= case readV of
				(Error e) = (Error e)
				(Ok r) = case f r of
					(Error e) = (Error e)
					(Ok w) = case doWrite write p rs w of
						Error e = Error e
						Ok (Just ws) = Ok ws
						_ = abort "Contact not satisfied: write yields Nothing while there is a reducer"

			doRead readf p rs = case readf of
				(SDSRead rf) = rf p rs
				(SDSReadConst rf) = Ok (rf p)

			doWrite writef p rs w = case writef of
				(SDSWrite wf) = wf p rs w
				(SDSWriteConst wf) = wf p w

instance Registrable SDSLens
where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSLens sds p c (Just (taskId, reqSDSId)) iworld

readSDSLens :: !(SDSLens p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
            -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSLens sds=:(SDSLens sds1 opts=:{SDSLensOptions|param,read}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	= case read of
		SDSRead f = case readAndMbRegisterSDS sds1 (param p) c mbNotify iworld of
			(Error e, iworld)  = (Error e, iworld)
			(Ok (ReadResult r ssds), iworld)     = case f p r of
				Error e = (Error e, iworld)
				Ok r = (Ok (ReadResult r (SDSLens ssds opts)), iworld)
			(Ok (AsyncRead sds), iworld) = (Ok (AsyncRead (SDSLens sds opts)), iworld)
		SDSReadConst f = (Ok (ReadResult (f p) sds), iworld)

// SDSCache
instance Identifiable SDSCache where
	nameSDS (SDSCache sds _) acc = ["$": nameSDS sds ["$":acc]]

instance Readable SDSCache where
	readSDS sds p c iworld = readSDSCache sds p c Nothing iworld

instance Writeable SDSCache where
	writeSDS sds=:(SDSCache sds1 opts=:{SDSCacheOptions|write}) p c w iworld=:{IWorld|readCache,writeCache}
	# key = (sdsIdentity sds, toSingleLineText p)
	//Check cache
	# mbr = case 'DM'.get key readCache of
		Just (val :: r^) = Just val
		_                = Nothing
	# mbw = case 'DM'.get key writeCache of
		Just (val :: w^,_) = Just val
		_                  = Nothing
	//Determine what to do
	# (mbr,policy) = write p mbr mbw w
	//Update read cache
	# readCache = case mbr of
		Just r = 'DM'.put key (dynamic r :: r^) readCache
		Nothing  = 'DM'.del key readCache
	= case policy of
		NoWrite = (Ok (WriteResult 'Set'.newSet sds), {iworld & readCache = readCache})
		WriteNow = case writeSDS sds1 p c w {iworld & readCache = readCache} of
			(Error e, iworld) = (Error e, iworld)
			(Ok (WriteResult r ssds), iworld) = (Ok (WriteResult r sds), iworld)
		WriteDelayed
			//FIXME: Even though write is delayed, the notification should still happen
			# writeCache = 'DM'.put key (dynamic w :: w^, DeferredWrite p w sds1) writeCache
			= (Ok (WriteResult 'Set'.newSet sds), {iworld & readCache = readCache, writeCache = writeCache})

instance Modifiable SDSCache where
	modifySDS f sds=:(SDSCache _ opts) p context iworld
	= case readSDS sds p context iworld of
		(Error e, iworld)               = (Error e, iworld)
		(Ok (AsyncRead sds), iworld)    = (Ok (AsyncModify sds f), iworld)
		(Ok (ReadResult r ssds), iworld)     = case f r of
			(Error e)   = (Error e, iworld)
			(Ok w)      = case writeSDS ssds p context w iworld of
				(Error e, iworld)   = (Error e, iworld)
				(Ok (WriteResult notify ssds), iworld) = (Ok (ModifyResult notify r w sds), iworld)

instance Registrable SDSCache where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSCache sds p c (Just (taskId, reqSDSId)) iworld

readSDSCache :: !(SDSCache p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
             -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSCache sds=:(SDSCache sds1 opts) p c mbNotify iworld=:{readCache}
	# iworld = mbRegister p sds mbNotify c iworld
	# key = (sdsIdentity sds,toSingleLineText p)
	//First check cache
	= case 'DM'.get key readCache of
		Just (val :: r^)
			# iworld = mbRegister p sds1 mbNotify c iworld
			= (Ok (ReadResult val sds),iworld)
		Just _           = (Error (exception "Cached value of wrong type"), iworld)
		Nothing = case readAndMbRegisterSDS sds1 p c mbNotify iworld of
			(Error e,iworld) = (Error e, iworld)
			//Read and add to cache
			(Ok (ReadResult val ssds),iworld)  = (Ok (ReadResult val sds), {iworld & readCache = 'DM'.put key (dynamic val :: r^) iworld.readCache})

// SDSSequence
instance Identifiable SDSSequence where
	nameSDS (SDSSequence sds1 sds2 {SDSSequenceOptions|name}) acc = ["<",name:nameSDS sds1 [",":nameSDS sds2 [">":acc]]]

instance Readable SDSSequence where
	readSDS sds p c iworld = readSDSSequence sds p c Nothing iworld

instance Writeable SDSSequence where
	writeSDS sds=:(SDSSequence sds1 sds2 opts=:{SDSSequenceOptions|paraml,paramr,writel,writer,name}) p c w iworld=:{IWorld|readCache,writeCache}
	= case readSDS sds1 (paraml p) c iworld of
		(Error e, iworld)  = (Error e, iworld)
		(Ok (AsyncRead asds), iworld)  = (Ok (AsyncWrite (SDSSequence asds sds2 opts)), iworld)
		(Ok (ReadResult r1 ssds), iworld)
			//Write sds1 if necessary
			# (npreds1,iworld) = case writel of
				(SDSWrite f)  = case f p r1 w of
					Error e             = (Error e, iworld)
					Ok Nothing   		= (Ok (WriteResult 'Set'.newSet ssds), iworld)
					Ok (Just w1)     	= writeSDS ssds (paraml p) c w1 iworld
				(SDSWriteConst f) = case f p w of
					Error e             = (Error e, iworld)
					Ok Nothing   		= (Ok (WriteResult 'Set'.newSet ssds), iworld)
					Ok (Just w1)     	= writeSDS ssds (paraml p) c w1 iworld
			| npreds1 =:(Error _) = (liftError npreds1, iworld)
			//Read/write sds2 if necessary
			# (npreds2,iworld) = case writer of
				(SDSWrite f)                    = case readSDS sds2 (paramr p r1) c iworld of //Also read sds2
					(Error e, iworld)               = (Error e, iworld)
					(Ok (ReadResult r2 ssds),iworld)     = case f p r2 w of
						Error e                         	= (Error e, iworld)
						Ok Nothing               			= (Ok (WriteResult 'Set'.newSet ssds), iworld)
						Ok (Just w2)                 		= writeSDS sds2 (paramr p r1) c w2 iworld
				(SDSWriteConst f)               = case f p w of
					Error e                         = (Error e, iworld)
					Ok Nothing               		= (Ok (WriteResult 'Set'.newSet sds2), iworld)
					Ok (Just w2)                 	= writeSDS sds2 (paramr p r1) c w2 iworld
			| npreds2 =:(Error _) = (liftError npreds2, iworld)
			= case (npreds1, npreds2) of
				(Ok (WriteResult notify1 ssds1), Ok (WriteResult notify2 ssds2))        = (Ok (WriteResult ('Set'.union notify1 notify2) (SDSSequence ssds1 ssds2 opts)), iworld)
				(Ok (WriteResult notify1 ssds1), Ok (AsyncWrite sds2))            = (Ok (AsyncWrite (SDSSequence ssds sds2 opts)), queueNotifyEvents (sdsIdentity sds1) notify1 iworld)

instance Modifiable SDSSequence where
	modifySDS f sds p context iworld
	= case readSDS sds p context iworld of
		(Error e, iworld)               = (Error e, iworld)
		(Ok (AsyncRead sds), iworld)    = (Error (exception "SDSSequence cannot be modified asynchronously in the left SDS."), iworld)
		(Ok (ReadResult r ssds), iworld)     = case f r of
			Error e                         = (Error e, iworld)
			Ok w                            = case writeSDS sds p context w iworld of
				(Error e, iworld)                   = (Error e, iworld)
				(Ok (AsyncWrite _), iworld)         = (Error (exception "SDSSequence cannot be modified asynchronously"), iworld)
				(Ok (WriteResult notify ssds), iworld)   = (Ok (ModifyResult notify r w sds), iworld)

instance Registrable SDSSequence where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSSequence sds p c (Just (taskId, reqSDSId)) iworld

readSDSSequence :: !(SDSSequence p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
                -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSSequence sds=:(SDSSequence sds1 sds2 opts=:{SDSSequenceOptions|paraml,paramr,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	= case readAndMbRegisterSDS sds1 (paraml p) c mbNotify iworld of
		(Error e, iworld) = (Error e, iworld)
		(Ok (AsyncRead sds), iworld) = (Ok (AsyncRead (SDSSequence sds sds2 opts)), iworld)
		(Ok (ReadResult r1 ssds1), iworld) = case read p r1 of
			Left r = (Ok (ReadResult r (SDSSequence ssds1 sds2 opts)), iworld)
			Right read2 = case readAndMbRegisterSDS sds2 (paramr p r1) c mbNotify iworld of
				(Error e, iworld) = (Error e, iworld)
				(Ok (ReadResult r2 ssds2), iworld) = (Ok (ReadResult (read2 (r1,r2)) (SDSSequence ssds1 ssds2 opts)), iworld)
				(Ok (AsyncRead sds2), iworld) = (Ok (AsyncRead (SDSSequence ssds1 sds2 opts)), iworld)

// SDSSelect
instance Identifiable SDSSelect where
	nameSDS (SDSSelect sds1 sds2 {SDSSelectOptions|name}) acc = ["{", name:nameSDS sds1 [",":nameSDS sds2 ["}":acc]]]

instance Readable SDSSelect where
	readSDS sds p c iworld = readSDSSelect sds p c Nothing iworld

instance Writeable SDSSelect where
	writeSDS sds=:(SDSSelect sds1 sds2 opts=:{SDSSelectOptions|select,notifyl,notifyr,name}) p c w iworld=:{IWorld|readCache,writeCache}
	= case select p of
		Left p1 = case notifyl of
			(SDSNotify f)  = case readSDS sds1 p1 c iworld of
				(Error e, iworld)  = (Error e, iworld)
				(Ok (AsyncRead ssds), iworld) = (Ok (AsyncWrite (SDSSelect ssds sds2 opts)), iworld)
				(Ok (ReadResult r1 ssds), iworld)    = case writeSDS ssds p1 c w iworld of
					(Error e, iworld) = (Error e, iworld)
					(Ok (AsyncWrite ssds), iworld) = (Ok (AsyncWrite (SDSSelect ssds sds2 opts)), iworld)
					(Ok (WriteResult notify ssds), iworld)
						# npred = (\ts pq -> case select pq of Right p2 = f p1 r1 w ts p2; _ = False)
						# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
						= (Ok (WriteResult notify (SDSSelect ssds sds2 opts)), iworld)
			(SDSNotifyConst f) = case writeSDS sds1 p1 c w iworld of
				(Error e, iworld) = (Error e, iworld)
				(Ok (AsyncWrite ssds), iworld) = (Ok (AsyncWrite (SDSSelect ssds sds2 opts)), iworld)
				(Ok (WriteResult notify ssds), iworld)
					# npred = (\ts pq -> case select pq of Right p2 = f p1 w ts p2; _ = False)
					# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
					# notify = 'Set'.union notify match
					= (Ok (WriteResult notify (SDSSelect ssds sds2 opts)), iworld)
		Right p2 = case notifyr of
			(SDSNotify f) = case readSDS sds2 p2 c iworld of
				(Error e, iworld)  = (Error e, iworld)
				(Ok (AsyncRead ssds), iworld) = (Ok (AsyncWrite (SDSSelect sds1 ssds opts)), iworld)
				(Ok (ReadResult r2 ssds), iworld)    = case writeSDS ssds p2 c w iworld of
					(Error e, iworld) = (Error e,iworld)
					(Ok (AsyncWrite ssds), iworld) = (Ok (AsyncWrite (SDSSelect sds1 ssds opts)), iworld)
					(Ok (WriteResult notify ssds), iworld)
						# npred = (\ts pq -> case select pq of Left p1 = f p2 r2 w ts p1 ; _ = False)
						# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
						= (Ok (WriteResult notify (SDSSelect sds1 ssds opts)), iworld)
			(SDSNotifyConst f) = case writeSDS sds2 p2 c w iworld of
				(Error e, iworld) = (Error e,iworld)
				(Ok (AsyncWrite ssds), iworld) = (Ok (AsyncWrite (SDSSelect sds1 ssds opts)), iworld)
				(Ok (WriteResult notify ssds), iworld)
					# npred = (\ts pq -> case select pq of Left p1 = f p2 w ts p1 ; _ = False)
					# (match,nomatch,iworld) = checkRegistrations (sdsIdentity sds) npred iworld
					//Add the matching registrations for the 'other' SDS
					# notify = 'Set'.union notify match
					= (Ok (WriteResult notify (SDSSelect sds1 ssds opts)), iworld)

instance Modifiable SDSSelect where
	modifySDS f sds=:(SDSSelect sds1 sds2 opts=:{select}) p context iworld
	= case select p of
		(Left p1)       = case modifySDS f sds1 p1 context iworld of
			(Error e, iworld)                   		= (Error e, iworld)
			(Ok (AsyncModify sds f), iworld)    		= (Ok (AsyncModify (SDSSelect sds sds2 opts) f), iworld)
			// TODO: Use applicable notify function.
			(Ok (ModifyResult notify r w ssds), iworld) = (Ok (ModifyResult notify r w (SDSSelect ssds sds2 opts)), iworld)
		(Right p2)      = case modifySDS f sds2 p2 context iworld of
			(Error e, iworld)                  			= (Error e, iworld)
			(Ok (AsyncModify sds f), iworld)    		= (Ok (AsyncModify (SDSSelect sds1 sds opts) f), iworld)
			// TODO: Use applicable notify function.
			(Ok (ModifyResult notify r w ssds), iworld) = (Ok (ModifyResult notify r w (SDSSelect sds1 ssds opts)), iworld)

instance Registrable SDSSelect where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSSelect sds p c (Just (taskId, reqSDSId)) iworld

readSDSSelect :: !(SDSSelect p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
              -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSSelect sds=:(SDSSelect sds1 sds2 opts=:{SDSSelectOptions|select,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	= case select p of
		Left p1     = case readAndMbRegisterSDS sds1 p1 c mbNotify iworld of
			(Error e, iworld)                   = (Error e, iworld)
			(Ok (ReadResult r ssds), iworld)    = (Ok (ReadResult r (SDSSelect ssds sds2 opts)), iworld)
			(Ok (AsyncRead sds), iworld)        = (Ok (AsyncRead (SDSSelect sds sds2 opts)), iworld)
		Right p2    = case readAndMbRegisterSDS sds2 p2 c mbNotify iworld of
			(Error e, iworld)                   = (Error e, iworld)
			(Ok (ReadResult r ssds), iworld)    = (Ok (ReadResult r (SDSSelect sds1 ssds opts)), iworld)
			(Ok (AsyncRead sds), iworld)        = (Ok (AsyncRead (SDSSelect sds1 sds opts)), iworld)

// SDSParallel
instance Identifiable SDSParallel where
	nameSDS sds acc = case sds of
        SDSParallel           sds1 sds2 opts = parallelName sds1 sds2 opts
        SDSParallelWriteLeft  sds1 sds2 opts = parallelName sds1 sds2 opts
        SDSParallelWriteRight sds1 sds2 opts = parallelName sds1 sds2 opts
        SDSParallelWriteNone  sds1 sds2 opts = parallelName sds1 sds2 opts
    where
        parallelName sds1 sds2 opts = ["|",opts.SDSParallelOptions.name:nameSDS sds1 [",":nameSDS sds2 ["|":acc]]]

instance Readable SDSParallel where
	readSDS sds p c iworld = readSDSParallel sds p c Nothing iworld

instance Writeable SDSParallel where
	writeSDS sds=:(SDSParallel sds1 sds2 opts=:{SDSParallelOptions|param,writel,writer,name}) p c w iworld
	# (p1,p2) = param p
	//Read/write sds1
	# (npreds1,iworld) = case writel of
		(SDSWrite f) = case readSDS sds1 p1 c iworld of
			(Error e, iworld)  = (Error e, iworld)
			(Ok (AsyncRead ssds), iworld) = (Ok (AsyncWrite ssds), iworld)
			(Ok (ReadResult r1 ssds),iworld)     = case f p r1 w of
				Error e                 = (Error e, iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet ssds), iworld)
				Ok (Just w1)            = writeSDS ssds p1 c w1 iworld
		(SDSWriteConst f) = case f p w of
				Error e                 = (Error e,iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet sds1),iworld)
				Ok (Just w1)            = writeSDS sds1 p1 c w1 iworld
	| npreds1 =:(Error _) = (liftError npreds1, iworld)
	//Read/write sds2
	# (npreds2,iworld) = case writer of
		(SDSWrite f) = case readSDS sds2 p2 c iworld of
			(Error e, iworld)  = (Error e, iworld)
			(Ok (AsyncRead ssds), iworld) = (Ok (AsyncWrite ssds), iworld)
			(Ok (ReadResult r2 ssds),iworld)     = case f p r2 w of
				Error e                 = (Error e, iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet ssds), iworld)
				Ok (Just w2)         = writeSDS ssds p2 c w2 iworld
		(SDSWriteConst f) = case f p w of
				Error e                 = (Error e,iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet sds2), iworld)
				Ok (Just w2)            = writeSDS sds2 p2 c w2 iworld
	| npreds2 =:(Error _) = (liftError npreds2, iworld)
	= case (npreds1, npreds2) of
		(Ok (WriteResult n1 ssds1), Ok (WriteResult n2 ssds2)) = (Ok (WriteResult ('Set'.union n1 n2) (SDSParallel ssds1 ssds2 opts)), iworld)
		(Ok (WriteResult n1 ssds1), Ok (AsyncWrite sds2)) = (Ok (AsyncWrite (SDSParallel ssds1 sds2 opts)), queueNotifyEvents (sdsIdentity sds1) n1 iworld)
		(Ok (AsyncWrite sds1), Ok (WriteResult n2 ssds2)) = (Ok (AsyncWrite (SDSParallel sds1 ssds2 opts)), queueNotifyEvents (sdsIdentity sds2) n2 iworld)
		(Ok (AsyncWrite sds1), Ok (AsyncWrite sds2)) = (Ok (AsyncWrite (SDSParallel sds1 sds2 opts)), iworld)

	writeSDS sds=:(SDSParallelWriteLeft sds1 sds2 opts=:{SDSParallelOptions|param,writel,name}) p c w iworld
	# p1 = fst (param p)
	//Read/write sds1
	# (npreds1,iworld) = case writel of
		(SDSWrite f) = case readSDS sds1 p1 c iworld of
			(Error e, iworld)  = (Error e, iworld)
			(Ok (AsyncRead ssds), iworld) = (Ok (AsyncWrite ssds), iworld)
			(Ok (ReadResult r1 ssds),iworld)     = case f p r1 w of
				Error e                 = (Error e, iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet ssds), iworld)
				Ok (Just w1)            = writeSDS ssds p1 c w1 iworld
		(SDSWriteConst f) = case f p w of
				Error e                 = (Error e,iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet sds1),iworld)
				Ok (Just w1)            = writeSDS sds1 p1 c w1 iworld
	= case npreds1 of
		Error e 					= (Error e, iworld)
		Ok (WriteResult n1 ssds1) 	= (Ok (WriteResult n1 (SDSParallelWriteLeft ssds1 sds2 opts)), iworld)
		Ok (AsyncWrite sds1) 		= (Ok (AsyncWrite (SDSParallelWriteLeft sds1 sds2 opts)), iworld)

	writeSDS sds=:(SDSParallelWriteRight sds1 sds2 opts=:{SDSParallelOptions|param,writer,name}) p c w iworld
	# p2 = snd (param p)
	//Read/write sds1
	# (npreds2,iworld) = case writer of
		(SDSWrite f) = case readSDS sds2 p2 c iworld of
			(Error e, iworld)  = (Error e, iworld)
			(Ok (AsyncRead ssds), iworld) = (Ok (AsyncWrite ssds), iworld)
			(Ok (ReadResult r2 ssds),iworld)     = case f p r2 w of
				Error e                 = (Error e, iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet ssds), iworld)
				Ok (Just w2)            = writeSDS ssds p2 c w2 iworld
		(SDSWriteConst f) = case f p w of
				Error e                 = (Error e,iworld)
				Ok Nothing              = (Ok (WriteResult 'Set'.newSet sds2),iworld)
				Ok (Just w2)            = writeSDS sds2 p2 c w2 iworld
	= case npreds2 of
		Error e 					= (Error e, iworld)
		Ok (WriteResult n2 ssds2) 	= (Ok (WriteResult n2 (SDSParallelWriteRight sds1 ssds2 opts)), iworld)
		Ok (AsyncWrite sds2) 		= (Ok (AsyncWrite (SDSParallelWriteRight sds1 sds2 opts)), iworld)

	writeSDS sds=:(SDSParallelWriteNone sds1 sds2 opts) p c w iworld
	= (Ok (WriteResult 'Set'.newSet sds), iworld)

instance Modifiable SDSParallel where
	modifySDS f sds p context iworld
	= case readSDS sds p context iworld of
		(Error e, iworld)               = (Error e, iworld)
		(Ok (AsyncRead sds), iworld)    = (Ok (AsyncModify sds f), iworld)
		(Ok (ReadResult r ssds), iworld)    = case f r of
			Error e                             = (Error e, iworld)
			Ok w                                = case writeSDS ssds p context w iworld of
				(Error e, iworld)                        = (Error e, iworld)
				(Ok (AsyncWrite sds), iworld)            = (Ok (AsyncModify sds f), iworld)
				(Ok (WriteResult notify ssds), iworld)   = (Ok (ModifyResult notify r w ssds), iworld)

instance Registrable SDSParallel where
	readRegisterSDS sds p c taskId reqSDSId iworld = readSDSParallel sds p c (Just (taskId, reqSDSId)) iworld

readSDSParallel :: !(SDSParallel p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
                -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSParallel sds=:(SDSParallel sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1 =:(Error _)
		= (liftError res1, iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2 =:(Error _)
		= (liftError res2, iworld)
	= case (fromOk res1, fromOk res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) 	= (Ok (ReadResult (read (r1, r2)) (SDSParallel ssds1 ssds2 opts)), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2) 		= (Ok (AsyncRead (SDSParallel sds1 ssds2 opts)), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2) 		= (Ok (AsyncRead (SDSParallel ssds1 sds2 opts)), iworld)
		(AsyncRead sds1, AsyncRead sds2) 			= (Ok (AsyncRead (SDSParallel sds1 sds2 opts)), iworld)

readSDSParallel sds=:(SDSParallelWriteLeft sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1 =:(Error _)
		= (liftError res1, iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2 =:(Error _)
		= (liftError res2, iworld)
	= case (fromOk res1, fromOk res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) 	= (Ok (ReadResult (read (r1, r2)) (SDSParallelWriteLeft ssds1 ssds2 opts)), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2) 		= (Ok (AsyncRead (SDSParallelWriteLeft sds1 ssds2 opts)), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2) 		= (Ok (AsyncRead (SDSParallelWriteLeft ssds1 sds2 opts)), iworld)
		(AsyncRead sds1, AsyncRead sds2) 			= (Ok (AsyncRead (SDSParallelWriteLeft sds1 sds2 opts)), iworld)

readSDSParallel sds=:(SDSParallelWriteRight sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1 =:(Error _)
		= (liftError res1, iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2 =:(Error _)
		= (liftError res2, iworld)
	= case (fromOk res1, fromOk res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) 	= (Ok (ReadResult (read (r1, r2)) (SDSParallelWriteRight ssds1 ssds2 opts)), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2) 		= (Ok (AsyncRead (SDSParallelWriteRight sds1 ssds2 opts)), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2) 		= (Ok (AsyncRead (SDSParallelWriteRight ssds1 sds2 opts)), iworld)
		(AsyncRead sds1, AsyncRead sds2) 			= (Ok (AsyncRead (SDSParallelWriteRight sds1 sds2 opts)), iworld)

readSDSParallel sds=:(SDSParallelWriteNone sds1 sds2 opts=:{SDSParallelOptions|param,read,name}) p c mbNotify iworld
	# iworld = mbRegister p sds mbNotify c iworld
	# (p1,p2) = param p
	# (res1, iworld) = readAndMbRegisterSDS sds1 p1 c mbNotify iworld
	| res1 =:(Error _)
		= (liftError res1, iworld)
	# (res2, iworld) = readAndMbRegisterSDS sds2 p2 c mbNotify iworld
	| res2 =:(Error _)
		= (liftError res2, iworld)
	= case (fromOk res1, fromOk res2) of
		(ReadResult r1 ssds1, ReadResult r2 ssds2) 	= (Ok (ReadResult (read (r1, r2)) (SDSParallelWriteNone ssds1 ssds2 opts)), iworld)
		(AsyncRead sds1, ReadResult r2 ssds2) 		= (Ok (AsyncRead (SDSParallelWriteNone sds1 ssds2 opts)), iworld)
		(ReadResult r1 ssds1, AsyncRead sds2) 		= (Ok (AsyncRead (SDSParallelWriteNone ssds1 sds2 opts)), iworld)
		(AsyncRead sds1, AsyncRead sds2) 			= (Ok (AsyncRead (SDSParallelWriteNone sds1 sds2 opts)), iworld)

optionsS :: SDSShareOptions -> String
optionsS o = o.SDSShareOptions.domain +++ ":" +++ toString o.SDSShareOptions.port

instance Identifiable SDSRemoteSource where
	nameSDS (SDSRemoteSource sds _ options) acc = ["REMOTE%" +++ optionsS options +++ "%" : nameSDS sds acc]

instance Readable SDSRemoteSource where
	readSDS sds p c iworld = readSDSRemoteSource sds p c Nothing iworld

instance Writeable SDSRemoteSource where
	writeSDS sds p EmptyContext value iworld = (Error (exception "cannot write remote SDS without task id"), iworld)

	writeSDS (SDSRemoteSource sds (Just connectionId) opts) p (TaskContext taskId) value iworld=:{ioStates}
	= case getAsyncWriteValue sds taskId connectionId ioStates of
		Error (_, error)
			# errorString = "SDSRemoteSourceQueued write get value<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		Ok Nothing = (Ok (AsyncWrite (SDSRemoteSource sds (Just connectionId) opts)), iworld)
		Ok (Just v) = (Ok (WriteResult 'Set'.newSet (SDSRemoteSource sds Nothing opts)), iworld)

	writeSDS sds=:(SDSRemoteSource sds1 Nothing opts) p (TaskContext taskId) value iworld
	= case queueWrite value sds p taskId iworld of
		(Error (_, error), iworld)
			# errorString = "SDSRemoteSource write queue<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		(Ok connectionId, iworld)              = (Ok (AsyncWrite (SDSRemoteSource sds (Just connectionId) opts)), iworld)

instance Modifiable SDSRemoteSource where
	modifySDS _ _ _ EmptyContext iworld = (Error (exception "SDSRemoteSource modify: Cannot modify with empty context"), iworld)

	modifySDS f sds=:(SDSRemoteSource subsds (Just connectionId) opts) p (TaskContext taskId) iworld=:{ioStates}
	= case getAsyncModifyValue subsds taskId connectionId ioStates of
		Error (_, error)
			# errorString = "SDSRemoteSourceQueued modify get value<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		Ok Nothing = (Ok (AsyncModify sds f), iworld)
		Ok (Just (r, w)) = (Ok (ModifyResult 'Set'.newSet r w (SDSRemoteSource subsds Nothing opts)), iworld)

	modifySDS f sds=:(SDSRemoteSource subsds Nothing opts) p (TaskContext taskId) iworld
	= case queueModify f sds p taskId iworld of
		(Error (_, error), iworld)
			# errorString = "SDSRemoteSource modify queue<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		(Ok connectionId, iworld) = (Ok (AsyncModify (SDSRemoteSource sds (Just connectionId) opts) f), iworld)

instance Registrable SDSRemoteSource where
	readRegisterSDS sds p context taskId reqSDSId iworld =
		readSDSRemoteSource sds p context (Just (taskId, reqSDSId)) iworld

readSDSRemoteSource :: !(SDSRemoteSource p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
                     -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSRemoteSource _ _ EmptyContext _ iworld = (Error (exception "Cannot read remote SDS without task id"), iworld)

readSDSRemoteSource (SDSRemoteSource sds (Just connectionId) opts) p context register iworld=:{ioStates}
	# taskId = case context of
		(TaskContext taskId ) = taskId
		(RemoteTaskContext reqTaskId currTaskId _ _ _ ) = currTaskId
	= case getAsyncReadValue sds taskId connectionId ioStates of
		Error (_, error)
			# errorString = "SDSRemoteSourceQueued read get value<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		Ok Nothing       	= (Ok (AsyncRead (SDSRemoteSource sds (Just connectionId) opts)), iworld)
		Ok (Just value)  	= (Ok (ReadResult value (SDSValue False value (SDSRemoteSource sds Nothing opts))), iworld)

readSDSRemoteSource sds=:(SDSRemoteSource _ Nothing opts) p context register iworld
	# iworld = mbRegister p sds register context iworld
	# taskId = case context of
		(TaskContext taskId ) = taskId
		(RemoteTaskContext reqTaskId currTaskId _ _ _ ) = currTaskId
	= case queueRead sds p taskId (isJust register) (sdsIdentity sds) iworld of
		(Error (_, error), iworld)
			# errorString = "SDSRemoteSource read queu<br>Remote to " +++ optionsS opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		(Ok connectionId, iworld)          = (Ok (AsyncRead (SDSRemoteSource sds (Just connectionId) opts)), iworld)

// Remote services
instance Identifiable SDSRemoteService where
	nameSDS (SDSRemoteService mbConnId opts) acc = ["SERVICE%" +++ toString opts +++ "%" : acc]

instance Readable SDSRemoteService where
	readSDS sds p c iworld = readSDSRemoteService sds p c Nothing iworld

instance Writeable SDSRemoteService where
	writeSDS sds p EmptyContext value iworld = (Error (exception "cannot write remote service without task id"), iworld)

	writeSDS sds=:(SDSRemoteService (Just connectionId) opts) p (TaskContext taskId) value iworld=:{ioStates}
	= case getAsyncServiceWriteValue sds taskId connectionId ioStates of
		Error (_, error)
			# errorString = "Remote service write queued error<br>Service " +++ toString opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		Ok Nothing = (Ok (AsyncWrite sds), iworld)
		Ok (Just pred)
			# (match,nomatch, iworld) = checkRegistrations (sdsIdentity sds) pred iworld
			= (Ok (WriteResult match (SDSRemoteService Nothing opts)), iworld)

	writeSDS sds=:(SDSRemoteService Nothing opts) p (TaskContext taskId) value iworld
	= case queueServiceWriteRequest sds p value taskId iworld of
		(Error (_, error), iworld)
			# errorString = "Remote service write error<br>Service " +++ toString opts +++ ": " +++ error
			= (Error $ exception errorString, iworld)
		(Ok Nothing, iworld) = (Ok $ WriteResult 'Set'.newSet sds, iworld)
		(Ok (Just connectionId), iworld) = (Ok $ AsyncWrite $ SDSRemoteService (Just connectionId) opts, iworld)

instance Modifiable SDSRemoteService where
	modifySDS _ _ _ _  iworld = (Error (exception "modifying remote services not possible"), iworld)

/**
 * Registering a remote service consists of keeping the connection open after a response is
 * received, so that the server may send other messages. Only applicable for TCP connections.
 */
instance Registrable SDSRemoteService where
	readRegisterSDS (SDSRemoteService _ (HTTPShareOptions _)) _ _ _ _ iworld = (Error (exception "registering HTTP services not possible"), iworld)
	readRegisterSDS sds p context taskId reqSDSId iworld =
		readSDSRemoteService sds p context (Just (taskId, reqSDSId)) iworld

readSDSRemoteService :: !(SDSRemoteService p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
                     -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSRemoteService _ _ EmptyContext _ iworld = (Error (exception "Cannot read remote service without task id"), iworld)
readSDSRemoteService sds=:(SDSRemoteService (Just connectionId) opts) p (TaskContext taskId) _ iworld=:{ioStates}
	= case getAsyncServiceValue sds taskId connectionId ioStates of
		Error (_, error)
			# errorString = "Remote service queued error<br>Service " +++ toString opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		Ok (Nothing)  				= (Ok (AsyncRead sds), iworld)
		Ok (Just value)  			= (Ok (ReadResult value (SDSValue False value (SDSRemoteService Nothing opts))), iworld)
readSDSRemoteService sds=:(SDSRemoteService Nothing opts) p (TaskContext taskId) mbRegister iworld
	= case queueServiceRequest sds p taskId (isJust mbRegister) iworld of
		(Error (_,error), iworld)
			# errorString = "Remote service error<br>Service  " +++ toString opts +++ ": " +++ error
			= (Error (exception errorString), iworld)
		(Ok connectionId, iworld)          = (Ok (AsyncRead (SDSRemoteService (Just connectionId) opts)), iworld)

instance == RemoteNotifyOptions where
	(==) left right = (left.hostToNotify, left.portToNotify, left.remoteSdsId) == (right.hostToNotify, right.portToNotify, right.remoteSdsId)

instance Identifiable SDSDebug where
	nameSDS (SDSDebug name sds) acc = nameSDS sds acc

instance Readable SDSDebug where
	readSDS sds p c iworld = readSDSDebug sds p c Nothing iworld

instance Writeable SDSDebug where
	writeSDS (SDSDebug name sds) p context w iworld=:{sdsNotifyRequests}
		# iworld = iShow ["Writing to share " +++ name +++ "(identity=" +++ sdsIdentity sds +++ ")"] iworld
		# iworld = iShow [(maybe "" ('Text'.join "\n" o map toSingleLineText o 'DM'.keys) ('DM'.get (sdsIdentity sds) sdsNotifyRequests))] iworld
		= db (writeSDS sds p context w iworld)
		where
			db (Error e, iworld) = (Error e, iShow [snd e] iworld)
			db (Ok (WriteResult notify sds), iworld)
				= (Ok (WriteResult notify (SDSDebug name sds)),
					iShow ["WriteResult from share " + name + " notifying: " +++ 'Text'.join " " (map toString ('Set'.toList notify))] iworld)
			db (Ok (AsyncWrite sds), iworld)
				= (Ok (AsyncWrite (SDSDebug name sds)), iShow ["AsyncWrite from share " +++ name] iworld)

instance Registrable SDSDebug where
	readRegisterSDS (SDSDebug name sds) p context taskId reqSDSId iworld
		# iworld = iShow ["Registering to share " +++ name] iworld
		= readAndMbRegisterSDS sds p context (Just (taskId, reqSDSId)) iworld

instance Modifiable SDSDebug where
	modifySDS f (SDSDebug name sds) p context iworld=:{sdsNotifyRequests}
		# iworld = iShow ["Modifying share " +++ name +++ "(identity=" +++ sdsIdentity sds +++ ")"] iworld
		# (regs, iworld) = listAllSDSRegistrations iworld
		# iworld = iShow [formatRegistrations regs] iworld
		= db (modifySDS f sds p context iworld)
	where
		db (Error e, iworld) = (Error e, iShow [snd e] iworld)
		db (Ok (ModifyResult notify r w sds), iworld)
			= (Ok (ModifyResult notify r w (SDSDebug name sds)),
				iShow ["ModifyResult from share " + name + " notifying: " + 'Text'.join ", " (map toString ('Set'.toList notify))] iworld)
		db (Ok (AsyncModify sds f), iworld) = (Ok (AsyncModify (SDSDebug name sds) f), iShow ["AsyncModify from share " + name] iworld)

readSDSDebug :: !(SDSDebug p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
             -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld) | gText{|*|} p & TC p & TC r & TC w
readSDSDebug (SDSDebug name sds) p context mbRegister iworld
		# iworld = iShow ["Reading from share " +++ name] iworld
		= db (readAndMbRegisterSDS sds p context mbRegister iworld)
	where
		db (Error e, iworld) = (Error e, iShow [snd e] iworld)
		db (Ok (ReadResult v sds), iworld)
			= (Ok (ReadResult v (SDSDebug name sds)),
				iShow ["ReadResult from share " +++ name] iworld)
		db (Ok (AsyncRead sds), iworld)
			= (Ok (AsyncRead (SDSDebug name sds)), iShow ["AsyncRead " +++ name] iworld)

// toString instances for SDSDebug
instance toString (TaskId, Maybe RemoteNotifyOptions) where
	toString (taskId, Nothing) = "local " +++ toString taskId
	toString (taskId, (Just remote)) = "remote " +++ toString taskId +++ " " +++ toString remote

instance toString RemoteNotifyOptions where
	toString {hostToNotify, portToNotify, remoteSdsId} = hostToNotify +++ ":" +++ toString portToNotify +++ "@" +++ remoteSdsId

instance toString (Maybe RemoteNotifyOptions) where
	toString Nothing = ""
	toString (Just options) = toString options

readAndMbRegisterSDS :: !(sds p r w) !p !TaskContext !(Maybe (!TaskId, !SDSIdentity)) !*IWorld
	                 -> *(!MaybeError TaskException (ReadResult p r w), !*IWorld)
                     | Registrable sds & gText{|*|} p & TC p & TC r & TC w
readAndMbRegisterSDS sds p c mbRegister iworld = case mbRegister of
	Just (regTaskId, reqSDSId) = readRegisterSDS sds p c regTaskId reqSDSId iworld
	Nothing                    = readSDS sds p c iworld
