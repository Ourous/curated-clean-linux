implementation module iTasks.Internal.Distributed.Instance

import iTasks
from Text import class Text, instance Text String
import qualified Data.Map as DM
import Data.Map.GenJSON
import qualified Text as T
import Text.Encodings.Base64
import iTasks.Extensions.Distributed._Evaluation
import iTasks.Extensions.Distributed._Formatter
import iTasks.Extensions.Distributed._Util
import iTasks.Extensions.Distributed._Types
import iTasks.Internal.Distributed.Symbols
from iTasks.Extensions.Distributed.Task import :: Domain(..)
from iTasks.Extensions.Distributed._Util import repeatClient
from iTasks.SDS.Sources.System import topLevelTasks
from iTasks.Internal.Serialization import dynamicJSONEncode, dynamicJSONDecode
from iTasks.Internal.Distributed.Domain import Domain
from iTasks.UI.Editor.Common import emptyEditor
import iTasks.Internal.SDS
from Data.Maybe import fromMaybe, isNothing, fromJust, maybe, instance Functor Maybe

:: Source = Client Int InstanceNo
	  | Server Int InstanceNo

:: DistributedTaskInstance =
	{ id :: Int
	, key :: String
	, task :: String
	, attributes :: TaskAttributes
	, creator :: Source // Client that created this task (owner).
	, assignedTo :: Int // Client that is executing the task
	}

:: DistributedTaskInstances =
	{ lastId :: Int
	, instances :: [DistributedTaskInstance]
	}

derive class iTask DistributedTaskInstance, DistributedTaskInstances, Source

distributedInstances :: SimpleSDSLens DistributedTaskInstances
distributedInstances = sharedStore "distributedInstances" {DistributedTaskInstances|lastId = 0, instances = [] }

:: ClientFilters =
	{ id :: Int
	, viewFilter :: (TaskAttributes -> Bool)
	, clameFilter :: (TaskAttributes -> Bool)
	}

:: Communication =
	{ id :: Int
	, requests :: [String]
	, responses :: [String]
	, ack :: Int
	, pending :: [(Int, String)]
	}

:: InstanceServerShare =
	{ lastId :: Int
	, clients :: [Communication]
	}

:: InstanceServerState =
	{ id :: Int
	, buffer :: String
	}

gText{|ClientFilters|} _ _             = []
gEditor{|ClientFilters|}               = emptyEditor
JSONEncode{|ClientFilters|} _ c        = [dynamicJSONEncode c]
JSONDecode{|ClientFilters|} _ [c:r]    = (dynamicJSONDecode c,r)
JSONDecode{|ClientFilters|} _ r        = (Nothing,r)
gEq{|ClientFilters|} _ _               = True
gDefault{|ClientFilters|}              = { ClientFilters| id = -1, viewFilter = (const False), clameFilter = (const False) }

derive class iTask InstanceServerShare
derive class iTask InstanceServerState
derive class iTask Communication

instanceServerShared = sharedStore "instanceServerShare" {InstanceServerShare| lastId = 0, clients = [] }

instanceServerFilters = sharedStore "instanceServerFilters" []

instanceServer :: Int Domain -> Task ()
instanceServer port domain = tcplisten port True instanceServerShared {ConnectionHandlers
	| onConnect 		= onConnect
	, onData		= onData
	, onShareChange		= onShareChange
	, onDisconnect 		= onDisconnect
	, onDestroy= \s->(Ok s, [])
	} -|| (instanceClient` "127.0.0.1" port domain True) -|| (process instanceServerShared) @! ()
where
	onConnect :: ConnectionId String InstanceServerShare -> (MaybeErrorString InstanceServerState, Maybe InstanceServerShare, [String], Bool)
	onConnect connId host share=:{InstanceServerShare|lastId,clients}
		= ( Ok {InstanceServerState| id = -1, buffer = "" }, Nothing, [], False)

	onData :: String InstanceServerState InstanceServerShare -> (MaybeErrorString InstanceServerState, Maybe InstanceServerShare, [String], Bool)
	onData newData st=:{id,buffer} share=:{InstanceServerShare|lastId,clients}
		# (requests, newBuffer) = getRequests (buffer +++ newData)
		| id == -1 = case notConnectedClientRequest requests of
				(Just (-1, requests)) -> let clientId = lastId + 1 in
							 let (newack, input, pending) = extractAckMsgs 0 requests [] in
							  ( Ok {InstanceServerState| id = clientId, buffer = newBuffer }
							  , Just {InstanceServerShare| share & lastId = clientId, clients = clients ++ [{Communication| id = clientId, requests = input, responses = [], ack = newack, pending = pending }] }
							  , (["connected " +++ (toString clientId) +++ "\n"] ++ (if (newack <> 0) ["#ACK" +++ (toString newack) +++ "#\n"] []))
							  , False)
				(Just (oldid, requests)) -> (Ok {InstanceServerState| st & id = oldid, buffer = newBuffer}, Just {InstanceServerShare| share & clients = [ if (clientid == oldid) ({Communication| c & requests = orgrequests ++ requests}) c \\ c=:{Communication|id=clientid,requests=orgrequests} <- clients] }, [], False)
				Nothing	-> (Ok {InstanceServerState| st & buffer = (buffer +++ newData)}, Nothing, [], False)
		| otherwise
			# (ack, pending) = getClientAck id clients
			# (newack, input, pending) = extractAckMsgs ack requests pending
			= (Ok {InstanceServerState| st & buffer = newBuffer}, Just {InstanceServerShare| share & clients = [ if (clientid == id) ({Communication| c & requests = orgrequests ++ input, ack = newack, pending = pending}) c \\ c=:{Communication|id=clientid,requests=orgrequests} <- clients] }, ["#ACK" +++ (toString newack) +++ "#\n"], False)

	onShareChange state=:{InstanceServerState|id} share=:{InstanceServerShare|clients}
		# responses = flatten [ responses \\ c=:{Communication|id=clientid,responses} <- clients | clientid == id ]
		| isEmpty responses = (Ok state, Nothing, responses, False)
		# share = {InstanceServerShare| share & clients = [ if (clientid == id) {Communication| c & responses = []} c \\ c=:{Communication|id=clientid} <- clients ] }
		= (Ok state, Just share, [ r +++ "\n" \\ r <- responses ], False) // Only replay on requests.

	onDisconnect :: InstanceServerState InstanceServerShare -> (MaybeErrorString InstanceServerState, Maybe InstanceServerShare)
	onDisconnect state share
		= (Ok state, Just share)

	getClientAck :: Int [Communication] -> (Int, [(Int, String)])
	getClientAck clientid clients
		= case [(ack, pending) \\ {Communication|id,ack,pending} <- clients | id == clientid] of
			[x:_] -> x
			_     -> (0, [])

	notConnectedClientRequest :: [String] -> Maybe (Int, [String])
	notConnectedClientRequest [] = Nothing
	notConnectedClientRequest [request:rest]
		= case 'T'.split " " request of
			["connect"]               -> return (-1, rest)
			["reconnect", id]	  -> return (toInt id, rest)
			_			  -> case notConnectedClientRequest rest of
							(Just (id, reqs)) -> (Just (id, [request:reqs]))
							Nothing            -> Nothing

	process :: (Shared sds InstanceServerShare) -> Task () | RWShared sds
	process share
		= forever (watch share >>* [OnValue (ifValue hasRequests \_ -> changed)] @! ())
	where
		hasRequests :: InstanceServerShare -> Bool
		hasRequests {InstanceServerShare|clients} = not (isEmpty (flatten [requests \\ c=:{Communication|requests}<-clients | not (isEmpty requests)]))

		changed :: Task Bool
		changed
			= get share
			>>- \{InstanceServerShare|clients} -> processClients clients
			>>- \newClients -> upd (\s -> {InstanceServerShare| s & clients = newClients}) share
			>-| return True

		processClients :: [Communication] -> Task [Communication]
		processClients [] = return []
		processClients [c=:{Communication|id, requests}:rest]
			= case requests of
				[]		= processClients rest >>- \rest -> return [c:rest]
				data	= processClients rest >>- \rest -> appendTopLevelTask ('DM'.fromList []) True (handleClient id data) >-| return [{Communication| c & requests = []}:rest]

		handleClient :: Int [String] -> Task ()
		handleClient id requests
			= handleClientRequests id requests
			>>- \responses -> upd (\s=:{InstanceServerShare|clients} -> {InstanceServerShare| s & clients = [if (clientid == id) ({Communication| c & responses=orgresponses ++ responses}) c \\ c=:{Communication|id=clientid,responses=orgresponses} <- clients]}) share @! ()

		handleClientRequests :: Int [String] -> Task [String]
		handleClientRequests id []
			= return []
		handleClientRequests id [request:rest]
			= handleClientRequest id ('T'.split " " request)
			>>- \responses -> handleClientRequests id rest
			>>- \other -> return (responses ++ other)

		handleClientRequest :: Int [String] -> Task [String]
		handleClientRequest id ["instance", "add", clientId, data]
			= withSymbols (\symbols -> case deserializeFromBase64 data symbols of
				(Remote_Task _ attributes taskid)
					= addTask (Client id (toInt clientId)) data attributes
					>-| return []
				_
					= return [])
		handleClientRequest id ["instance", "destory", taskid]
			= destroyTask id (toInt taskid) @! []
		handleClientRequest id ["filter", "view", data]
			= withSymbols (\symbols -> let filter = deserializeFromBase64 data symbols in
				updateViewFilter id filter >-| return [])
		handleClientRequest id ["filter", "clame", data]
			= withSymbols (\symbols -> let filter = deserializeFromBase64 data symbols in
				updateClameFilter id filter >-| return [])
		handleClientRequest id ["instance", "get", instanceid]
			= getInstanceById id (toInt instanceid) False
		handleClientRequest id ["instance", "get-force", instanceid]
			= getInstanceById id (toInt instanceid) True
		handleClientRequest id request=:["value", instanceid : data]
			= forwardRequest id request @! []
		handleClientRequest _ request = return []

		forwardRequest :: Int [String] -> Task ()
		forwardRequest clientid request=:["value", ref, clientRef : data]
			= getCreatorAndAssignedTo (toInt ref)
			>>- \v -> case v of
					(Just (Client creatorClient creatorRef, _)) -> sendToClient creatorClient ('T'.join " " ["value", (toString creatorRef) : data])
					(Just (Server creatorServer instanceNo, _)) -> sendToInstanceServer creatorServer ('T'.join " " ["value", (toString instanceNo) : data])

/*
 * Update the clients filter AND notify about all the task that match this (new) filter.
 */
updateViewFilter :: Int (TaskAttributes -> Bool) -> Task ()
updateViewFilter clientid filter
	= get distributedInstances
	>>- \{DistributedTaskInstances|instances} -> let responses = [ getNotifyMessage id attributes \\ i=:{DistributedTaskInstance|id,attributes} <- instances | filter attributes ] in
		upd (\s=:{InstanceServerShare|clients} ->
			{InstanceServerShare| s & clients = [ if (id==clientid) (updateClient c responses) c \\ c=:{Communication|id} <- clients] }) instanceServerShared
		>-| upd (updateFilter clientid filter) instanceServerFilters @! ()
where
	updateClient :: Communication [String] -> Communication
	updateClient c=:{Communication|responses=r} responses
		= {Communication| c & responses = r ++ responses}

	updateFilter :: Int (TaskAttributes -> Bool) [ClientFilters] -> [ClientFilters]
	updateFilter clientid filter data
		| not (isEmpty [id \\ {ClientFilters|id} <- data | id == clientid])
			= [ if (id == clientid) {ClientFilters| f & viewFilter = filter } f \\ f=:{ClientFilters| id} <- data]
		= data ++ [{ClientFilters| id = clientid, viewFilter = filter, clameFilter = (const False)}]

/*
 * Update the clients filter for claming.
 */
updateClameFilter :: Int (TaskAttributes -> Bool) -> Task ()
updateClameFilter clientid filter
	= upd (updateFilter clientid filter) instanceServerFilters @! ()
where
	updateFilter :: Int (TaskAttributes -> Bool) [ClientFilters] -> [ClientFilters]
	updateFilter clientid filter data
		| not (isEmpty [id \\ {ClientFilters|id} <- data | id == clientid])
			= [ if (id == clientid) {ClientFilters| f & clameFilter = filter } f \\ f=:{ClientFilters| id} <- data]
		= data ++ [{ClientFilters| id = clientid, viewFilter = (const False), clameFilter = filter}]

sendToClient :: Int String -> Task ()
sendToClient clientid message
        = upd (\s=:{InstanceServerShare|clients} ->
                {InstanceServerShare| s & clients = [ if (id==clientid) (sendToClient c message) c \\ c=:{Communication|id} <- clients]
                }) instanceServerShared @! ()
where
        sendToClient :: Communication String -> Communication
        sendToClient c=:{Communication|responses} message
                = {Communication|c & responses = responses ++ [ message ] }

getCreatorAndAssignedTo :: InstanceNo -> Task (Maybe (Source,Int))
getCreatorAndAssignedTo instanceno
	= get distributedInstances
	>>- \{DistributedTaskInstances|instances} -> case [ (creator,assignedTo) \\ {DistributedTaskInstance|id,creator,assignedTo} <- instances | id == instanceno ] of
		[x:_] -> return (Just x)
		_     -> return Nothing

addTask :: Source String TaskAttributes -> Task InstanceNo
addTask creator task attributes
	= clameTask attributes
	>>- \clame -> let assignedTo = fromMaybe 0 clame in
	   upd (\s=:{DistributedTaskInstances|lastId,instances} ->
		{ s & lastId = lastId + 1
		, instances = instances ++ [pack (lastId + 1) task attributes assignedTo]
		}) distributedInstances
	>>- \{DistributedTaskInstances|lastId} -> notifyChange lastId attributes
	>-| if (assignedTo == 0) (return lastId) (sendDataToClient assignedTo task attributes lastId >-| return lastId)
where
	pack id task attributes assignedTo
		= { DistributedTaskInstance|id=id
		  , key=""
		  , task=task
		  , attributes=attributes
		  , creator=creator
		  , assignedTo=assignedTo
		  }

destroyTask :: Int InstanceNo -> Task ()
destroyTask clientId taskId
	= getTaskInfo clientId taskId
	>>- \id -> case id of
			Nothing 	-> return ()
			(Just (tid,at))	-> notifyDestory tid at
					   >-| upd (\s=:{DistributedTaskInstances|instances} ->
					        {s & instances = [ i \\ i=:{DistributedTaskInstance|id} <- instances | id <> tid]}) distributedInstances @! ()
where
	getTaskInfo :: Int InstanceNo -> Task (Maybe (Int,TaskAttributes))
	getTaskInfo clientid taskid
		= get distributedInstances
		>>- \s=:{DistributedTaskInstances|instances} ->
			case [ (id,attributes) \\ i=:{DistributedTaskInstance|id,attributes,creator} <- instances | (isTask clientid taskId creator) ] of
				[x:_]	-> return (Just x)
				_	-> return Nothing

	isTask :: Int InstanceNo Source -> Bool
	isTask clientid taskid (Client id tid)  = clientid == id && taskid == tid
	isTask _ _ _				= False

// Send instance-clame to client.
sendDataToClient :: Int String TaskAttributes InstanceNo -> Task ()
sendDataToClient clientid task attributes instanceno
	= upd (\s=:{InstanceServerShare|clients} ->
		{InstanceServerShare| s & clients = [ if (id == clientid) (sendToClient c instanceno attributes) c \\ c=:{Communication|id} <- clients]
		}) instanceServerShared @! ()
where
        sendToClient :: Communication InstanceNo TaskAttributes -> Communication
        sendToClient c=:{Communication|id,responses} instanceno attributes
                = {Communication|c & responses = responses ++ [ "instance-clame data " +++ (toString instanceno) +++ " " +++ task] }


clameTask :: TaskAttributes -> Task (Maybe Int)
clameTask attributes
	= get instanceServerFilters
	>>- \filters = case [ id \\ {ClientFilters|id,clameFilter} <- filters | clameFilter attributes ] of
			[x:_] -> return (Just x)
			_     -> return Nothing

getInstanceById :: Int InstanceNo Bool -> Task [String]
getInstanceById clientid instanceno force
	= upd (\i=:{DistributedTaskInstances|instances} -> {i & instances = [if (id==instanceno) { s & assignedTo = if (to==0 || force) clientid to } s \\ s=:{DistributedTaskInstance|id,assignedTo=to} <- instances] })  distributedInstances
	>>- \{DistributedTaskInstances|instances} -> case [i \\ i=:{DistributedTaskInstance|id}<- instances | id == instanceno ] of
		[{DistributedTaskInstance|id,task,assignedTo}:_] -> if (assignedTo==clientid) (return ["instance data " +++ (toString instanceno) +++ " " +++ task]) (return ["instance assigned " +++ (toString instanceno)])
		_ -> return []

notifyChange :: InstanceNo TaskAttributes -> Task ()
notifyChange instanceno attributes
	= get instanceServerFilters
	>>- \filters -> upd (\s=:{InstanceServerShare|clients} ->
                {InstanceServerShare| s & clients = [ notifyClient c instanceno attributes filters \\ c <- clients]
                }) instanceServerShared @! ()
where
	notifyClient :: Communication InstanceNo TaskAttributes [ClientFilters] -> Communication
	notifyClient c=:{Communication|id,responses} instanceno attributes filters
		| not (clientFilter id filters attributes) = c
		= {Communication|c & responses = responses ++ [  getNotifyMessage instanceno attributes] }

	clientFilter :: Int [ClientFilters] TaskAttributes -> Bool
	clientFilter clientid filters attrb
		= case [viewFilter \\ {ClientFilters|id,viewFilter} <- filters | id == clientid ] of
			[x:_] -> x attrb
			_     -> False

notifyDestory :: InstanceNo TaskAttributes -> Task ()
notifyDestory instanceno attributes
	= get instanceServerFilters
	>>- \filters -> upd (\s=:{InstanceServerShare|clients} ->
		{InstanceServerShare| s & clients = [ notifyClient c instanceno attributes filters \\ c <- clients]
		}) instanceServerShared @! ()
where
	notifyClient :: Communication InstanceNo TaskAttributes [ClientFilters] -> Communication
	notifyClient c=:{Communication|id,responses} instanceno attributes filters
		| not (clientFilter id filters attributes) = c
		= {Communication|c & responses = responses ++ [  getDestroyNotifyMessage instanceno ] }

	clientFilter :: Int [ClientFilters] TaskAttributes -> Bool
	clientFilter clientid filters attrb
		= case [viewFilter \\ {ClientFilters|id,viewFilter} <- filters | id == clientid ] of
			[x:_] -> x attrb
			_     -> False

	getDestroyNotifyMessage :: InstanceNo -> String
	getDestroyNotifyMessage id
		= "instance destory " +++ (toString id)

getNotifyMessage :: InstanceNo TaskAttributes -> String
getNotifyMessage instanceno attributes = "instance notify " +++ (toString instanceno) +++ " " +++ (serializeToBase64 attributes)

getRequests :: String -> ([String], String)
getRequests input
        | 'T'.indexOf "\n" input <> -1
                # splitpoint = 'T'.indexOf "\n" input
                # request = 'T'.subString 0 splitpoint input
                # rest = 'T'.dropChars (splitpoint + 1) input
                = let (req,data) = getRequests rest in ([request : req], data)
        = ([], input)

extractAckMsgs :: Int [String] [(Int, String)] -> (Int, [String], [(Int, String)])
extractAckMsgs ack input pending
	# newPending = merge pending (extract input)
	# ack` = newAck ack [nr \\ (nr,_) <- newPending]
	= (ack`, [s \\ (id,s) <- newPending | id == -1 || (id <= ack` && id > ack)], [i \\ i=:(id,_) <- newPending | id > ack`])
where
	extract :: [String] -> [(Int, String)]
	extract input
		= [extractItem i \\ i <- input]

	extractItem :: String -> (Int, String)
	extractItem input
		# splitpoint = 'T'.indexOf "#!#" input
		| splitpoint == -1 = (-1, input)
		# ack = 'T'.subString 0 splitpoint input
		# rest = 'T'.dropChars (splitpoint + 3) input
		= (toInt ack, rest)

	merge :: [(Int, String)] [(Int, String)] -> [(Int, String)]
	merge list [] = list
	merge list [(id,msg):xs]
		| isMember id [i \\ (i,_) <- list]   = merge list xs
		| otherwise                          = merge (list ++ [(id,msg)]) xs

	newAck :: Int [Int] -> Int
	newAck ack acks = if (isMember (ack + 1) acks) (newAck (ack + 1) acks) ack

extractAck :: Int [String] -> (Int, [String])
extractAck ack input
	# acks  = [extract i \\ i <- input | 'T'.indexOf "#ACK" i == 0]
	# input = [i \\ i <- input | 'T'.indexOf "#ACK" i <> 0]
	= case acks of
		[x:_] -> (x,input)
		_     -> (ack,input)
where
        extract input
                # input = 'T'.dropChars 4 input
                # splitpoint = 'T'.indexOf "#" input
                | splitpoint == -1 = -1
                # ack = 'T'.subString 0 splitpoint input
                = toInt ack

// ---- instance client.

:: ClientState :== String
:: ClientShare =
        { requests      :: [String]
        , responses     :: [String]
	, serverId	:: Int
	, outNr		:: Int
	, out		:: [(Int, String)]
	, ack		:: Int
        }

derive class iTask ClientShare

instanceClientShare clientId = sharedStore ("instanceClientShare" +++ (toString clientId)) {ClientShare| requests = [], responses = [], serverId = -1, outNr = 1, out = [], ack = 0}

getClientServerId :: Int -> Task (Maybe Int)
getClientServerId clientId
	=                              get (instanceClientShare clientId)
	>>- \{ClientShare|serverId} -> return (if (serverId == -1) Nothing (Just serverId))

:: ClientsShare =
	{ lastId 	:: Int
	, clients 	:: [(String, Int)]
	}

derive class iTask ClientsShare

clientsShare = sharedStore ("instanceClientsShare") {ClientsShare| lastId = 0, clients = [] }

getClientId :: Domain -> Task Int
getClientId (Domain domain)
	= upd (\s=:{ClientsShare|lastId,clients} -> let id = lastId + 1 in {ClientsShare| s & lastId = id, clients = clients ++ [(domain, id)] }) clientsShare
	>>- \{ClientsShare|lastId} -> return lastId

getClientIdByDomain :: Domain -> Task (Maybe Int)
getClientIdByDomain (Domain domain)
	= get clientsShare
	>>- \{ClientsShare|clients} -> case [ id \\ (d, id) <- clients | d == domain] of
					[x] -> return (Just x)
					_ -> return Nothing

instanceClient :: String Int Domain -> Task ()
instanceClient host port domain = instanceClient` host port domain False

instanceClient` :: String Int Domain Bool -> Task ()
instanceClient` host port domain local
	=                getClientId domain
	>>- \clientId -> (repeatClient (client clientId) @! ()) -|| (process (instanceClientShare clientId) clientId)
where
        client :: Int -> Task (Maybe ())
        client clientId
                = getClientServerId clientId
                >>- \serverId -> (tcpconnect host port (instanceClientShare clientId) { ConnectionHandlers
                                                      | onConnect      = (onConnect (maybe "connect" (\id -> ("reconnect " +++ (toString id))) serverId))
                                                      , onData         = onData
						      , onShareChange  = onShareChange
                                                      , onDisconnect   = onDisconnect
                                                      , onDestroy      = \s->(Ok s, [])
                                                      } @! Nothing)
                -||- (viewInformation [] () >>* [OnAction (Action "reset") (always (return Nothing))])

        onConnect :: String ConnectionId String ClientShare -> (MaybeErrorString ClientState, Maybe ClientShare, [String], Bool)
        onConnect helloMessage connId host store
                = (Ok "", Just store, [helloMessage +++ "\n"] ++ [(toString nr) +++ "#!#" +++ resp +++ "\n" \\ (nr,resp) <- store.out], False)

        onData :: String ClientState ClientShare -> (MaybeErrorString ClientState, Maybe ClientShare, [String], Bool)
        onData received data store=:{ClientShare|requests,ack}
                # received_data = data +++ received
                # (new_requests,other) = getRequests received_data
                | isEmpty new_requests = (Ok received_data, Nothing, [], False)
                # (newack, input) = extractAck ack new_requests
                = (Ok other, Just {ClientShare| store & requests = requests ++ input, out = [ i \\ i=:(nr,_) <- store.out | nr <= newack], ack = newack}, [], False)

        onShareChange state store=:{ClientShare|responses}
                | isEmpty responses = (Ok state, Nothing, [], False)
                # newOut = [(i, resp) \\ resp <- responses & i <- [store.outNr..]]
                # outNr = store.outNr + (length newOut)
                # out = store.out ++ newOut
                = (Ok state, Just {ClientShare| store & responses = [], outNr = outNr, out = out}, [ (toString nr) +++ "#!#" +++ resp +++ "\n" \\ (nr,resp) <- newOut], False)

        onDisconnect :: ClientState ClientShare -> (MaybeErrorString ClientState, Maybe ClientShare)
        onDisconnect state share
                = (Ok state, Just share)

        process :: (Shared sds ClientShare) Int -> Task () | RWShared sds
        process share clientId
                = forever (watch share >>* [OnValue (ifValue hasRequests \_ -> changed)] @! ())
        where
                hasRequests :: ClientShare -> Bool
                hasRequests {ClientShare|requests} = not (isEmpty requests)

                changed :: Task Bool
                changed
                        = get share
                        >>- \{ClientShare|requests} -> handleRequests requests
                        >>- \(newServerId, newResponses) -> upd (\s=:{ClientShare|serverId,responses=orgresponses} -> {ClientShare| s & requests = [], serverId = (fromMaybe serverId newServerId), responses = orgresponses ++ newResponses}) share
                        >-| return True

                handleRequests :: [String] -> Task (Maybe Int, [String])
                handleRequests []
                        = return (Nothing, [])
                handleRequests [request:rest]
                        = withSymbols (handleRequest ('T'.split " " request))
                        >>- \(serverId, responses) -> handleRequests rest
                        >>- \(serverIdOther, other) -> return (if (isNothing serverId) serverIdOther serverId, (responses ++ other))

                handleRequest :: [String] {#Symbol} -> Task (Maybe Int, [String])
                handleRequest ["instance", "notify", instanceno, attributes] symbols
                        # attributes = deserializeFromBase64 attributes symbols
                        = getTaskIdByAttribute "distributedInstanceId" (JSONString instanceno)
                        >>- \id -> if (isNothing id)
                                (appendTopLevelTask ('DM'.put "distributedInstanceServerId" (JSONString (toString clientId)) ('DM'.put "distributedInstanceId" (JSONString instanceno) attributes)) False (wrapperTask (toInt instanceno) clientId) @! ())
                                (return ())
                        >-| return (Nothing, [])
                handleRequest ["instance", "destory", instanceno] _
                        = getTaskIdByAttribute "distributedInstanceId" (JSONString instanceno)
                        >>- \id -> if (isNothing id)
                                (return ())
                                (removeTask (TaskId (fromJust id) 0) topLevelTasks @! ())
                        >-| return (Nothing, [])
                handleRequest ["instance-clame", "data", instanceno, data] _
                        = storeInPool clientId (toInt instanceno) data @! (Nothing, [])
                handleRequest ["instance", "data", instanceno, data] _
                        = callTaskHandler (toInt instanceno) data @! (Nothing, [])
                handleRequest ["instance", "assigned", instanceno] _
                        = callTaskHandler (toInt instanceno) "ASSIGNED" @! (Nothing, [])
                handleRequest request=:["value", ref, data] symbols
                        = case (deserializeFromBase64 data symbols) of
                              (Remote_TaskValue value) -> set value (taskValueShare (toInt ref)) @! (Nothing, [])
                handleRequest ["connected", id : rest] _
                        = return (Just (toInt id), [])
                handleRequest _ _
                        = return (Nothing, [])

storeInPool :: Int InstanceNo String -> Task ()
storeInPool serverId instanceno taskdata
	= withSymbols (\symbols -> case deserializeFromBase64 taskdata symbols of
		(Remote_Task _ attributes id) -> addTask (Server serverId instanceno) taskdata attributes @! ())

// Is local instance server (TODO: Fix this check).
hasLocalInstanceServer :: Task Bool
hasLocalInstanceServer
	= get instanceServerShared
	>>- \{InstanceServerShare|lastId} -> return (lastId == 0)

instanceFilter :: (TaskAttributes -> Bool) Domain -> Task ()
instanceFilter filter domain
	= getClientIdByDomain domain
	>>- \clientId -> upd (\s=:{ClientShare|responses} -> {ClientShare | s & responses = responses ++ [message]}) (instanceClientShare (fromMaybe 0 clientId)) @! ()
where
	message = "filter view " +++ (serializeToBase64 filter)

instanceClameFilter :: (TaskAttributes -> Bool) Domain -> Task ()
instanceClameFilter filter domain
	= getClientIdByDomain domain
	>>- \clientId -> upd (\s=:{ClientShare|responses} -> {ClientShare | s & responses = responses ++ [message]}) (instanceClientShare (fromMaybe 0 clientId)) @! ()
where
	 message = "filter clame " +++ (serializeToBase64 filter)

sendToInstanceServer :: Int String -> Task ()
sendToInstanceServer clientId message
	= upd (\s=:{ClientShare|responses=r} -> {ClientShare| s & responses = r ++ [message] }) (instanceClientShare clientId) @! ()

remoteTaskIdShare = sharedStore "remoteTaskIdShare" 0

newRemoteTaskId :: Task Int
newRemoteTaskId
        = upd ((+) 1) remoteTaskIdShare
        >>- return

sendDistributedInstance :: InstanceNo (Task a) TaskAttributes Domain -> Task a | iTask a
sendDistributedInstance _ task attributes domain
	= newRemoteTaskId
	>>- \id -> let valueShare = taskValueShare id in getClientIdByDomain domain
	>>- \clientId -> upd (\s=:{ClientShare|responses=or} -> {ClientShare| s & responses = or ++ ["instance add " +++ (toString id) +++ " " +++ (serializeToBase64 (Remote_Task task attributes id))]}) (instanceClientShare (fromMaybe 0 clientId))
	>-| proxyTask valueShare (onDestroy id (instanceClientShare (fromMaybe 0 clientId)))
where
	onDestroy :: InstanceNo (Shared sds ClientShare) *IWorld -> *IWorld | RWShared sds
	onDestroy id share iworld
		# (error,iworld) = modify (\s=:{ClientShare|responses=or} ->{ClientShare| s & responses = or ++ ["instance destory " +++ (toString id)]}) share EmptyContext iworld
		= iworld

sendRequestToInstanceServer :: Int String -> Task ()
sendRequestToInstanceServer clientId request
	= upd (\s=:{ClientShare|responses=or} -> {ClientShare| s & responses = or ++ [request]}) (instanceClientShare clientId) @! ()

getTaskIdByAttribute :: String JSONNode -> Task (Maybe InstanceNo)
getTaskIdByAttribute key value = get attrb
where
	attrb = mapRead find (sdsFocus (key,value) taskInstancesByAttribute)

	find instances = case [instanceNo \\ {TaskInstance|instanceNo,attributes} <- instances | hasValue key value attributes] of
		[i:_]   = Just i
		_	= Nothing

	hasValue key value attributes
		= maybe False ((==) value) ('DM'.get key attributes)

// ---- Wrapper task

wrapperTask :: InstanceNo Int -> Task ()
wrapperTask instanceno clientId
	= withSymbols (\symbols -> (withShared "" (loadTask instanceno False))
	>>- \task -> case deserializeFromBase64 task symbols of
		(Remote_Task task _ _) = evalRemoteTask task (valueChange instanceno) @! ())
where
	loadTask :: InstanceNo Bool (Shared sds String) -> Task String | RWShared sds
	loadTask instanceno force shared
		= Title "Loading task" @>> viewInformation [] "Please wait, the task is loaded ..."
		||- (addWrapperTaskHandler instanceno (handlerTask shared)
	 	     >-| sendRequestToInstanceServer clientId ("instance " +++ (if force "get-force " "get ") +++ toString instanceno)
                     >>| (watch shared >>* [OnValue (ifValue (\v -> not (v == "")) return)])
		) >>- \result -> if (result=="ASSIGNED") (assigned instanceno shared) (return result)

	handlerTask :: (Shared sds String) String -> Task () | RWShared sds
	handlerTask shared data = set data shared @! ()

	assigned :: InstanceNo (Shared sds String) -> Task String | RWShared sds
	assigned instanceno shared
		= Hint "Task is assigned to another node" @>> viewInformation []
			"You can takeover the task. Please take in mind that the progress at the other device maybe lost."
		>>* [OnAction (Action "Take over") (always (return ()))]
		>-| set "" shared
		>-| loadTask instanceno True shared

	valueChange :: InstanceNo (TaskValue a) -> Task () | iTask a
	valueChange instanceno value
		= sendRequestToInstanceServer clientId ("value " +++ (toString instanceno) +++ " none " +++ serializeToBase64 (Remote_TaskValue value))

:: WrapperTaskHandelers :== Map Int String

wrapperTaskHandelersShare :: SimpleSDSLens WrapperTaskHandelers
wrapperTaskHandelersShare = memoryShare_ "wrapper_task_handelers" 'DM'.newMap

addWrapperTaskHandler :: Int (String -> Task ()) -> Task ()
addWrapperTaskHandler taskid handlerTask
        = upd (\handelers -> 'DM'.put taskid handler handelers) wrapperTaskHandelersShare @! ()
where
	handler = serializeToBase64 handlerTask

callTaskHandler :: Int String -> Task ()
callTaskHandler instanceno data
        = get wrapperTaskHandelersShare
        >>- \handlers ->
                case 'DM'.get instanceno handlers of
                        (Just handler)     = withSymbols (\s -> callHandler (deserializeFromBase64 handler s) data)
                        _                  = return () // Not found.
where
	callHandler :: (String -> Task ()) String -> Task ()
	callHandler task data = task data
