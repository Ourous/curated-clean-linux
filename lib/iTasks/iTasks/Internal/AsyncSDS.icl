implementation module iTasks.Internal.AsyncSDS

import Data.Maybe, Data.Either, Data.List, Data.Func
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskState
import Text, Text.GenJSON
import StdMisc, StdArray, StdBool
import Internet.HTTP

import iTasks.Engine
import iTasks.Internal.Distributed.Symbols
import iTasks.Internal.IWorld
import iTasks.Internal.SDS
import iTasks.Internal.Task
import iTasks.Internal.Util
import iTasks.SDS.Definition
import iTasks.UI.Definition
import iTasks.WF.Tasks.IO
import iTasks.WF.Derives
import iTasks.Internal.Serialization

import iTasks.Extensions.Distributed._Formatter

from iTasks.Internal.TaskServer import addConnection
from iTasks.SDS.Sources.Core import unitShare
import iTasks.Internal.SDSService

import qualified Data.Map as DM
import qualified Data.Set as DS

derive JSONEncode SDSNotifyRequest, RemoteNotifyOptions

createRequestString req = serializeToBase64 req

onConnect reqq connId _ _ = (Ok (Left []), Nothing, [createRequestString reqq +++ "\n"], False)

onData data (Left acc) _ = (Ok (Left (acc ++ [data])), Nothing, [], False)

onShareChange acc _ = (Ok acc, Nothing, [], False)

onDestroy s = (Ok s, [])

queueSDSRequest :: !(SDSRequest p r w) !String !Int !TaskId !{#Symbol} !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | TC r
queueSDSRequest req host port taskId symbols iworld
= case addConnection taskId host port connectionTask iworld of
	(Error e, iworld)  		= (Error e, iworld)
	(Ok (id, _), iworld)     	= (Ok id, iworld)
where
	connectionTask = wrapConnectionTask (handlers req) unitShare

	handlers :: (SDSRequest p r w) -> ConnectionHandlers (Either [String] (MaybeError TaskException r)) () () | TC r
	handlers _ = {ConnectionHandlers| onConnect = onConnect req,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect,
		onDestroy = onDestroy}

	onDisconnect (Left acc) _
	# textResponse = concat acc
	| size textResponse < 1 = (Error ("queueSDSRequest: Server " +++ host +++ " disconnected without responding"), Nothing)
	= (Ok $ Right $ deserializeFromBase64 textResponse symbols, Nothing)

queueModifyRequest :: !(SDSRequest p r w) !String !Int !TaskId !{#Symbol} !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | TC r & TC w
queueModifyRequest req=:(SDSModifyRequest p r w) host port taskId symbols iworld = case addConnection taskId host port connectionTask iworld of
	(Error e, iworld)          = (Error e, iworld)
	(Ok (id, _), iworld)       = (Ok id, iworld)
where
	connectionTask = wrapConnectionTask (handlers req) unitShare

	handlers :: (SDSRequest p r w) -> ConnectionHandlers (Either [String] (MaybeError TaskException (r, w))) () () | TC r & TC w
	handlers _ = {ConnectionHandlers| onConnect = onConnect req,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect,
		onDestroy=onDestroy}

	onDisconnect (Left acc) _
	# textResponse = concat acc
	| size textResponse == 0 = (Error ("queueModifyRequest: Server" +++ host +++ " disconnected without responding"), Nothing)
	= (Ok $ Right $ deserializeFromBase64 textResponse symbols, Nothing)

queueWriteRequest :: !(SDSRequest p r w) !String !Int !TaskId !{#Symbol} !*IWorld ->  (!MaybeError TaskException ConnectionId, !*IWorld) | TC r & TC w
queueWriteRequest req=:(SDSWriteRequest sds p w) host port taskId symbols iworld = case addConnection taskId host port connectionTask iworld of
	(Error e, iworld)          = (Error e, iworld)
	(Ok (id, _), iworld)       = (Ok id, iworld)
where
	connectionTask = wrapConnectionTask (handlers req) unitShare

	handlers :: (SDSRequest p r w) -> ConnectionHandlers (Either [String] (MaybeError TaskException ())) () () | TC r & TC w
	handlers req = {ConnectionHandlers| onConnect = onConnect req,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect,
		onDestroy = onDestroy}

	onDisconnect (Left acc) _
	# textResponse = concat acc
	| size textResponse == 0 = (Error ("queueWriteRequest: Server" +++ host +++ " disconnected without responding"), Nothing)
	= (Ok $ Right $ deserializeFromBase64 textResponse symbols, Nothing)

queueServiceRequest :: !(SDSRemoteService p r w) p !TaskId !Bool !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r
queueServiceRequest (SDSRemoteService (Just _) _) _ _ _ iworld = (Error (exception "SDSRemoteService queing request while still a connection id"), iworld)
queueServiceRequest service=:(SDSRemoteService _ (HTTPShareOptions {host, port, createRequest, fromResponse})) p taskId _ iworld
= case addConnection taskId host port connectionTask iworld of
	(Error e, iworld) = (Error e, iworld)
	(Ok (id, _), iworld) = (Ok id, iworld)
where
	connectionTask = wrapConnectionTask (handlers service) unitShare

	handlers req = {ConnectionHandlers| onConnect = onConnect,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect,
		onDestroy = onDestroy}

	onConnect _ _ _
	# req = createRequest p
	# sreq = toString {HTTPRequest|req & req_headers = 'DM'.put "Connection" "Close" req.HTTPRequest.req_headers}
	= (Ok (Left []), Nothing, [sreq], False)

	onData data (Left acc) _ = (Ok (Left (acc ++ [data])), Nothing, [], False)

	onShareChange acc _ = (Ok acc, Nothing, [], False)

	onDisconnect (Left []) _ = (Error ("queueServiceRequest: Server" +++ host +++ ":" +++ toString port +++ " disconnected without responding"), Nothing)
	onDisconnect (Left acc) _
	# textResponse = concat acc
	= case parseResponse textResponse of
		Nothing = (Error ("Unable to parse HTTP response, got: " +++ textResponse), Nothing)
		(Just parsed) = case fromResponse parsed p of
			(Error error) = (Error error, Nothing)
			(Ok a) = (Ok (Right a), Nothing)

queueServiceRequest service=:(SDSRemoteService _ (TCPShareOptions {host, port, createMessage, fromTextResponse})) p taskId register iworld
= case addConnection taskId host port connectionTask iworld of
	(Error e, iworld) = (Error e, iworld)
	(Ok (id, _), iworld) = (Ok id, iworld)
where
	connectionTask = wrapConnectionTask handlers unitShare
	handlers = {ConnectionHandlers| onConnect = onConnect,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect,
		onDestroy = onDestroy}

	onConnect connId _ _	= (Ok (Nothing, []), Nothing, [createMessage p +++ "\n"], False)

	onData data (previous, acc) _
	# newacc = acc ++ [data]
	// If already a result, and we are registering, then we have received a refresh notification from the server.
	| register && isJust previous = (Ok (previous, newacc), Nothing, [], True)
	= case fromTextResponse (concat newacc) p register of
		Error e = (Error e, Nothing, [], True)
		// No full response yet, keep the old value.
		Ok (Nothing,response) 	= (Ok (previous, newacc), Nothing, maybe [] (\resp. [resp +++ "\n"]) response, False)
		Ok (Just r, Just resp) 	= (Ok (Just r, []), Nothing, [resp +++ "\n"], False)
		// Only close the connection when we have a value and when we are not registering.
		Ok (Just r, Nothing) 	= (Ok (Just r, []), Nothing, [], not register)

	onShareChange state _ = (Ok state, Nothing, [], False)
	onDisconnect state _ = (Ok state, Nothing)

queueServiceWriteRequest :: !(SDSRemoteService p r w) !p !w !TaskId !*IWorld -> (MaybeError TaskException (Maybe ConnectionId), !*IWorld) | TC p & TC w
queueServiceWriteRequest service=:(SDSRemoteService (Just _) _) _ _ _ iworld = (Error (exception "SDSRemoteService queing write request while still containing a connection id"), iworld)
queueServiceWriteRequest service=:(SDSRemoteService _ (HTTPShareOptions {host, port, writeHandlers})) p w taskId iworld
| isNothing writeHandlers = (Ok Nothing, iworld) // Writing not supported for this share.
= case addConnection taskId host port connectionTask iworld of
	(Error e, iworld) = (Error e, iworld)
	(Ok (id, _), iworld) = (Ok (Just id), iworld)
where
	(toWriteRequest, fromWriteResponse) = fromJust writeHandlers
	connectionTask = wrapConnectionTask handlers unitShare
	handlers = { ConnectionHandlers|onConnect = onConnect
				, onData 		= onData
				, onShareChange = onShareChange
				, onDisconnect 	= onDisconnect
				, onDestroy  	= onDestroy
				}
	onConnect connId _ _
	# req = toWriteRequest p w
	# sreq = toString {HTTPRequest|req & req_headers = 'DM'.put "Connection" "Close" req.HTTPRequest.req_headers}
	= (Ok (Left []), Nothing, [sreq], False)

	onData data (Left acc) _ = (Ok $ Left $ acc ++ [data], Nothing, [], False)

	onShareChange acc _ = (Ok acc, Nothing, [], False)

	onDisconnect (Left []) _ = (Error ("queueServiceWriteRequest: Server" +++ host +++ ":" +++ toString port +++ " disconnected without responding"), Nothing)
	onDisconnect (Left acc) _
	# textResponse = concat acc
	= case parseResponse textResponse of
		Nothing = (Error ("Unable to parse HTTP response, got: " +++ textResponse), Nothing)
		Just parsed = case fromWriteResponse p parsed of
			Error e = (Error e, Nothing)
			Ok pred = (Ok (Right pred), Nothing)

queueServiceWriteRequest service=:(SDSRemoteService _ (TCPShareOptions {host, port, writeMessageHandlers})) p w taskId iworld
| isNothing writeMessageHandlers = (Ok Nothing, iworld)
= case addConnection taskId host port connectionTask iworld of
	(Error e, iworld) = (Error e, iworld)
	(Ok (id, _), iworld) = (Ok (Just id), iworld)
where
	(toWriteMessage, fromWriteMessage) = fromJust writeMessageHandlers
	connectionTask = wrapConnectionTask handlers unitShare

	handlers = {ConnectionHandlers| onConnect = onConnect,
		onData = onData,
		onShareChange = onShareChange,
		onDisconnect = onDisconnect,
		onDestroy = onDestroy}

	onConnect connId _ _	= (Ok (Left ""), Nothing, [toWriteMessage p w +++ "\n"], False)

	onData data (Left acc) _
	# newacc = acc +++ data
	= case fromWriteMessage p newacc of
		Error e 		= (Error e, Nothing, [], True)
		Ok Nothing		= (Ok (Left newacc), Nothing, [], False)
		Ok (Just pred) 	= (Ok (Right pred), Nothing, [], True)
	onData data state _ = (Ok state, Nothing, [], True)

	onShareChange state _ = (Ok state, Nothing, [], False)
	onDisconnect state _ = (Ok state, Nothing)

queueRead :: !(SDSRemoteSource p r w) p !TaskId !Bool !SDSIdentity !*IWorld
	-> (!MaybeError TaskException ConnectionId, !*IWorld)
	| gText{|*|} p & TC p & TC r & TC w
queueRead rsds=:(SDSRemoteSource sds (Just _) _) _ _ _ _ iworld = (Error $ exception "queueRead while already a connection id", iworld)
queueRead rsds=:(SDSRemoteSource sds Nothing {SDSShareOptions|domain, port}) p taskId register reqSDSId iworld
# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
	(Ok (ReadingDone r), iworld) = (readSymbols r, iworld)
	_ = abort "Reading symbols failed!"
# (request, iworld) = buildRequest register iworld
= queueSDSRequest request domain port taskId symbols iworld
where
	buildRequest True iworld=:{options}= (SDSRegisterRequest sds p reqSDSId (sdsIdentity rsds) taskId options.sdsPort, iworld)
	buildRequest False iworld = (SDSReadRequest sds p, iworld)

queueRemoteRefresh :: ![(TaskId, RemoteNotifyOptions)] !*IWorld -> *IWorld
queueRemoteRefresh [] iworld = iworld
queueRemoteRefresh [(reqTaskId, remoteOpts) : reqs] iworld=:{options}
# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
	(Ok (ReadingDone r), iworld) = (readSymbols r, iworld)
# request = reqq reqTaskId remoteOpts.remoteSdsId
= case queueSDSRequest request remoteOpts.hostToNotify remoteOpts.portToNotify SDSSERVICE_TASK_ID symbols iworld of
	(_, iworld) = queueRemoteRefresh reqs iworld
where
	reqq :: !TaskId !SDSIdentity -> SDSRequest () String ()
	reqq taskId sdsId = SDSRefreshRequest taskId sdsId

queueWrite :: !w !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueWrite w rsds=:(SDSRemoteSource sds (Just _) _) _ _ iworld = (Error $ exception "queueWrite while already a connection id", iworld)
queueWrite w rsds=:(SDSRemoteSource sds Nothing share=:{SDSShareOptions|domain, port}) p taskId iworld
# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
	(Ok (ReadingDone r), iworld) = (readSymbols r, iworld)
# request = SDSWriteRequest sds p w
= queueWriteRequest request domain port taskId symbols iworld

queueModify :: !(r -> MaybeError TaskException w) !(SDSRemoteSource p r w) p !TaskId !*IWorld -> (!MaybeError TaskException ConnectionId, !*IWorld) | gText{|*|} p & TC p & TC r & TC w
queueModify f rsds=:(SDSRemoteSource sds (Just _)_) _ _ iworld = (Error $ exception "queueModify while already a connection id", iworld)
queueModify f rsds=:(SDSRemoteSource sds Nothing share=:{SDSShareOptions|domain, port}) p taskId iworld
# (symbols, iworld) = case read symbolsShare EmptyContext iworld of
	(Ok (ReadingDone r), iworld) = (readSymbols r, iworld)
# request = SDSModifyRequest sds p f
= queueModifyRequest request domain port taskId symbols iworld

getAsyncServiceValue :: !(SDSRemoteService p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe r) | TC r & TC w & TC p
getAsyncServiceValue service taskId connectionId ioStates
# getValue = case service of
	SDSRemoteService _ (HTTPShareOptions _) = getValueHttp
	SDSRemoteService _ (TCPShareOptions _) = getValueTCP
=  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		Just ioState                        = case ioState of
			IOException exc                   = Error (exception exc)
			IOActive connectionMap            = getValue connectionId connectionMap
			IODestroyed connectionMap         = getValue connectionId connectionMap
where
	getValueHttp connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		Just (value :: Either [String] r^, _) = case value of
			(Left _)                                = Ok Nothing
			(Right val)                     		= Ok (Just val)
		Just (dyn, _)
			# message = "Dynamic not of the correct service type, got: "
				+++ toString (typeCodeOfDynamic dyn)
				+++ ", required: "
				+++ toString (typeCodeOfDynamic (dynamic service))
			= Error (exception message)
		Nothing                             	= Ok Nothing

	getValueTCP connectionId connectionMap
	= case 'DM'.get connectionId connectionMap of
		Just (value :: (Maybe r^, [String]), _) = case value of
				(Nothing, _)                        = Ok Nothing
				(Just r,_)                     		= Ok (Just r)
		Just (dyn, _)
			# message = "Dynamic not of the correct service type, got: "
				+++ toString (typeCodeOfDynamic dyn)
				+++ ", required: "
				+++ toString (typeCodeOfDynamic (dynamic service))
			= Error (exception message)
		Nothing                             	= Ok Nothing

getAsyncServiceWriteValue :: !(SDSRemoteService p r w) !TaskId !ConnectionId !IOStates -> MaybeError TaskException (Maybe (SDSNotifyPred p)) | TC p & TC w & TC r
getAsyncServiceWriteValue service taskId connectionId ioStates
# getValue = case service of
	SDSRemoteService _ (HTTPShareOptions _) = getValueHttp
	SDSRemoteService _ (TCPShareOptions _) = getValueTCP
=  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		Just ioState                        = case ioState of
			IOException exc                   = Error (exception exc)
			IOActive connectionMap            = getValue connectionId connectionMap
			IODestroyed connectionMap         = getValue connectionId connectionMap
where
	getValueHttp connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		Just (value :: Either [String] (SDSNotifyPred p^), _) = case value of
			Left _                              = Ok Nothing
			Right pred                     		= Ok (Just pred)
		Just (dyn, _)
			# message = "Dynamic not of the correct service type, got: "
				+++ toString (typeCodeOfDynamic dyn)
				+++ ", required: "
				+++ toString (typeCodeOfDynamic (dynamic service))
			= Error (exception message)
		Nothing                             	= Ok Nothing

	getValueTCP connectionId connectionMap
	= case 'DM'.get connectionId connectionMap of
		Just (value :: Either String (SDSNotifyPred p^), _) = case value of
				Left _                       	= Ok Nothing
				Right pred                     	= Ok (Just pred)
		Just (dyn, _)
			# message = "Dynamic not of the correct service type, got: "
				+++ toString (typeCodeOfDynamic dyn)
				+++ ", required: "
				+++ toString (typeCodeOfDynamic (dynamic service))
			= Error (exception message)
		Nothing                             	= Ok Nothing

getAsyncReadValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe r) | TC r
getAsyncReadValue _ taskId connectionId ioStates
=  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValue connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] (MaybeError TaskException r^), _)) = case value of
			(Left _)                                = Ok Nothing
			(Right (Ok val))                        = Ok (Just val)
			(Right (Error e))						= Error e
		(Just (dyn, _))							= Error (exception ("Dynamic not of the correct read type, got" +++ toString (typeCodeOfDynamic dyn)))
		Nothing                             	= Ok Nothing

getAsyncWriteValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe ()) | TC w
getAsyncWriteValue _ taskId connectionId ioStates =  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValue connectionId connectionMap = case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] (MaybeError TaskException ()), _)) = case value of
			(Left _)                                    = Ok Nothing
			(Right (Ok val))                            = Ok (Just val)
			(Right (Error e))							= Error e
		(Just (dyn, _))						= Error (exception ("Dynamic not of the correct write type, got" +++ toString (typeCodeOfDynamic dyn)))
		Nothing                             = Ok Nothing

getAsyncModifyValue :: !(sds p r w) !TaskId !ConnectionId IOStates -> MaybeError TaskException (Maybe (r,w)) | TC w & TC r
getAsyncModifyValue _ taskId connectionId ioStates =  case 'DM'.get taskId ioStates of
		Nothing                             = Error (exception "No iostate for this task")
		(Just ioState)                      = case ioState of
			(IOException exc)                   = Error (exception exc)
			(IOActive connectionMap)            = getValue connectionId connectionMap
			(IODestroyed connectionMap)         = getValue connectionId connectionMap
where
	getValue connectionId connectionMap
	= case 'DM'.get connectionId connectionMap of
		(Just (value :: Either [String] (MaybeError TaskException (r^, w^)), _)) = case value of
			(Left _)						= Ok Nothing
			(Right (Ok val))				= Ok (Just val)
			(Right (Error e))				= Error e
		(Just (dyn, _))					= Error (exception ("Dynamic not of the correct modify type, got " +++ toString (typeCodeOfDynamic dyn)))
		Nothing 						= Ok Nothing

asyncSDSLoaderUI :: !AsyncAction -> UI
asyncSDSLoaderUI Read = uia UIProgressBar (textAttr "Reading data")
asyncSDSLoaderUI Write = uia UIProgressBar (textAttr "Writing data")
asyncSDSLoaderUI Modify = uia UIProgressBar (textAttr "Modifying data")

readCompletely :: (sds () r w) (TaskValue a) (r Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld
	-> *(TaskResult a, *IWorld) | Readable sds & TC r & TC w
readCompletely _ _ _ DestroyEvent _ iworld
	= (DestroyedResult, iworld)
readCompletely sds tv cont event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld
	= case read sds (TaskContext taskId) iworld of
		(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok (ReadingDone r), iworld)
			= cont r event evalOpts iworld
		(Ok (Reading sds), iworld)
			= (ValueResult tv (mkTaskEvalInfo lastEval) (mkUIIfReset event (asyncSDSLoaderUI Read)) (Task (readCompletely sds tv cont)), iworld)

writeCompletely :: w (sds () r w) (TaskValue a) (Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld
	-> *(TaskResult a, *IWorld) | Writeable sds & TC r & TC w
writeCompletely _ _ _ cont DestroyEvent evalOpts iworld
	= (DestroyedResult, iworld)
writeCompletely w sds tv cont event evalOpts=:{taskId,lastEval} iworld
	= case write w sds (TaskContext taskId) iworld of
		(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok (WritingDone), iworld)
			= cont event evalOpts iworld
		(Ok (Writing sds), iworld)
			= (ValueResult tv (mkTaskEvalInfo lastEval) (mkUIIfReset event (asyncSDSLoaderUI Write)) (Task (writeCompletely w sds tv cont)), iworld)

modifyCompletely :: (r -> w) (sds () r w) (TaskValue a) (Event -> UIChange) (w Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld
	-> *(TaskResult a, *IWorld) | TC r & TC w & Modifiable sds
modifyCompletely _ _ _ _ cont DestroyEvent evalOpts iworld
	= (DestroyedResult, iworld)
modifyCompletely modfun sds tv ui cont event evalOpts=:{taskId,lastEval} iworld
	= case modify modfun sds (TaskContext taskId) iworld of
		(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok (ModifyingDone w), iworld)
			= cont w event evalOpts iworld
		(Ok (Modifying sds modfun), iworld)
			= (ValueResult tv (mkTaskEvalInfo lastEval) (ui event) (Task (modifyCompletely modfun sds tv ui cont)), iworld)

readRegisterCompletely :: (sds () r w) (TaskValue a) (Event -> UIChange) (r Event TaskEvalOpts *IWorld -> *(TaskResult a, *IWorld)) Event TaskEvalOpts !*IWorld
	-> *(TaskResult a, *IWorld) | TC r & TC w & Registrable sds
readRegisterCompletely _ _ _ cont DestroyEvent evalOpts iworld
	= (DestroyedResult, iworld)
readRegisterCompletely sds tv ui cont event evalOpts=:{taskId,lastEval} iworld
	| not (isRefreshForTask event taskId)
		= (ValueResult tv (mkTaskEvalInfo lastEval) (ui event) (Task (readRegisterCompletely sds tv ui cont)), iworld)
	= case readRegister taskId sds iworld of
		(Error e, iworld) = (ExceptionResult e, iworld)
		(Ok (ReadingDone r), iworld)
			= cont r event evalOpts iworld
		(Ok (Reading sds), iworld)
			= (ValueResult
				tv
				(mkTaskEvalInfo lastEval)
				(ui event)
				(Task (readRegisterCompletely sds tv ui cont))
			, iworld)
