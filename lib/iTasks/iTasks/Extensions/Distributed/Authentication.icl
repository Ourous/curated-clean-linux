implementation module iTasks.Extensions.Distributed.Authentication

import iTasks
import iTasks.Extensions.Admin.UserAdmin
from Text import class Text, instance Text String
import qualified Data.Map as DM
import qualified Text as T
import Text.Encodings.Base64
from iTasks.Internal.Distributed.Domain import :: Domain(..)
from iTasks.Extensions.Distributed._Util import repeatClient
from Data.Maybe import fromMaybe, isNothing, fromJust, maybe, instance Functor Maybe

:: Communication =
	{ id :: Int
	, requests :: [String]
	, responses :: [String]
	}

:: AuthShare =
	{ lastId :: Int
	, clients :: [Communication]
	}

:: AuthServerState =
	{ id :: Int
	, buffer :: String
	}

derive class iTask Communication
derive class iTask AuthShare
derive class iTask AuthServerState

authServerShare = sharedStore "authServerShare" {AuthShare| lastId = 0, clients = [] }

authServer :: Int -> Task ()
authServer port = tcplisten port True authServerShare {ConnectionHandlers
	| onConnect 		= onConnect
	, onData		= onData
	, onShareChange		= onShareChange
	, onDisconnect 		= onDisconnect
	} -|| (process authServerShare) @! ()
where
	onConnect :: ConnectionId String AuthShare -> (MaybeErrorString AuthServerState, Maybe AuthShare, [String], Bool)
	onConnect connId host share
		# clientId = share.lastId + 1
		= ( Ok {AuthServerState| id = clientId, buffer = "" }
		  , Just { share & lastId = clientId, clients = share.clients ++ [{Communication| id = clientId, requests = [], responses = []}] }
		  , []
		  , False
		  )

	onData :: String AuthServerState AuthShare -> (MaybeErrorString AuthServerState, Maybe AuthShare, [String], Bool)
	onData newData st=:{id,buffer} share
		= let (requests, newBuffer) = getRequests (buffer +++ newData) in
			(Ok {AuthServerState| st & buffer = newBuffer}, Just { share & clients = [ if (clientid == id) ({Communication| c & requests = c.requests ++ requests}) c \\ c=:{Communication|id=clientid} <- share.clients] }, [], False)

	onShareChange state=:{AuthServerState|id} share
		# responses = flatten [ c.responses \\ c=:{Communication|id=clientid} <- share.clients | clientid == id ]
		| isEmpty responses = (Ok state, Just share, responses, False)
		# share = {share & clients = [ if (clientid == id) {Communication| c & responses = []} c \\ c=:{Communication|id=clientid} <- share.clients ] }
		= (Ok state, Just share, [ r +++ "\n" \\ r <- responses ], False) // Only replay on requests.

	onDisconnect :: AuthServerState AuthShare -> (MaybeErrorString AuthServerState, Maybe AuthShare)
	onDisconnect state share
		= (Ok state, Just share)

	process :: (Shared sds AuthShare) -> Task () | RWShared sds
	process share
		= forever (watch share >>* [OnValue (ifValue hasRequests \_ -> changed)] @! ())
	where
		hasRequests :: AuthShare -> Bool
		hasRequests {AuthShare|clients} = not (isEmpty (flatten [requests \\ c=:{Communication|requests}<-clients | not (isEmpty requests)]))

		changed :: Task Bool
		changed
			= get share
			>>= \{AuthShare|clients} -> processClients clients
			>>= \newClients -> upd (\s -> {AuthShare| s & clients = newClients}) share
			>>| return True

		processClients :: [Communication] -> Task [Communication]
		processClients [] = return []
		processClients [c=:{Communication|id, requests}:rest]
			= case requests of
				[]		= processClients rest >>= \rest -> return [c:rest]
				data	= processClients rest >>= \rest -> appendTopLevelTask ('DM'.fromList []) True (handleClient id data) >>| return [{Communication| c & requests = []}:rest]

		handleClient :: Int [String] -> Task ()
		handleClient id requests
			= handleClientRequests id requests
			>>= \responses -> upd (\s -> {AuthShare| s & clients = [if (clientid == id) ({Communication| c & responses=responses}) c \\ c=:{Communication|id=clientid} <- s.clients]}) share @! ()

		handleClientRequests :: Int [String] -> Task [String]
		handleClientRequests id []
			= return []
		handleClientRequests id [request:rest]
			= handleClientRequest id ('T'.split " " request)
			>>= \responses -> handleClientRequests id rest
			>>= \other -> return (responses ++ other)

		handleClientRequest :: Int [String] -> Task [String]
		handleClientRequest id ["auth", username, password]
			# username = base64Decode username
			# password = base64Decode password
			= authenticateUser (Username username) (Password password)
			>>= \user -> return [(base64Encode (toString (toJSON user)))]
		handleClientRequest id ["users"]
			= get users
			>>= \users -> return [(base64Encode (toString (toJSON users)))]
		handleClientRequest _ _ = return []

remoteAuthenticateUser	:: !Username !Password	-> Task (Maybe User)
remoteAuthenticateUser (Username username) (Password password)
	# user = (toString (base64Encode username))
	# pass = (toString (base64Encode password))
	= get authServerInfoShare
	>>- \domain -> request domain DEFAULT_AUTH_PORT ("auth " +++ user +++ " " +++ pass)
	>>- \user -> return (fromMaybe Nothing user)

getUsers :: String Int -> Task [User]
getUsers host port
	= request host port "users"
	>>= \users -> return (fromMaybe [] users)

request	:: String Int String -> Task (Maybe a) | iTask a
request host port request
	= repeatClient client
where
	client :: Task (Maybe a) | iTask a
	client
		= ((tcpconnect host port (constShare ())
                        { ConnectionHandlers
                        | onConnect      = onConnect
                        , onData	 = onData
			, onShareChange  = onShareChange
                        , onDisconnect   = onDisconnect
                        }) @? taskResult)
		>>- \(resps,_) -> case resps of
					[resp:_]  -> return (fromJSON (fromString (base64Decode resp)))
					_         -> return Nothing

	onConnect :: ConnectionId String () -> (MaybeErrorString ([String], String, Bool), Maybe (), [String], Bool)
	onConnect connId host store
		= (Ok ([], "", False), Just store, [request +++ "\n"], False)

	onData :: String ([String], String,Bool) () -> (MaybeErrorString ([String], String, Bool), Maybe (), [String], Bool)
	onData received state=:(response,data,_) store
		# received_data = data +++ received
		# (new_requests,other) = getRequests received_data
		= (Ok (response ++ new_requests,data, not (isEmpty new_requests)), Just store, [], False)

	onShareChange state ()
		= (Ok state, Nothing, [], False)

	onDisconnect :: ([String], String, Bool) () -> (MaybeErrorString ([String], String, Bool), Maybe ())
	onDisconnect state share
                = (Ok state, Just share)

	taskResult (Value (r1,r2,True) _) = Value (r1,r2) True
	taskResult _                      = NoValue

getRequests :: String -> ([String], String)
getRequests input
	| 'T'.indexOf "\n" input <> -1
		# splitpoint = 'T'.indexOf "\n" input
		# request = 'T'.subString 0 splitpoint input
		# rest = 'T'.dropChars (splitpoint + 1) input
		= let (req,data) = getRequests rest in ([request : req], data)
	= ([], input)

DEFAULT_AUTH_PORT :: Int
DEFAULT_AUTH_PORT = 2018

domainAuthServer :: Task ()
domainAuthServer
	= authServer DEFAULT_AUTH_PORT

usersOf :: Domain -> Task [User]
usersOf (Domain domain)
	= request domain DEFAULT_AUTH_PORT "users"
	>>- \users -> return (fromMaybe [] users)

startAuthEngine :: Domain -> Task ()
startAuthEngine (Domain domain)
	= set domain authServerInfoShare @! ()

authServerInfoShare :: SimpleSDSLens String
authServerInfoShare = sharedStore "authServer" ""

currentDistributedUser :: SimpleSDSParallel (User,Domain)
currentDistributedUser = sdsParallel "communicationDetailsByNo" param read (SDSWriteConst writel) (SDSWriteConst writer) currentUser authServerInfoShare
where
	param p = (p,p)
	read (user,domain) = (user,Domain domain)
	writel _ (x,_) = Ok (Just x)
	writer _ (_, Domain y) = Ok (Just y)

currentDomain :: SDSLens () Domain ()
currentDomain = toReadOnly (mapRead (\domain -> Domain domain) authServerInfoShare)

enterDomain :: Task Domain
enterDomain
	= get authServerInfoShare
	>>- \domain -> updateInformation "Enter domain" [] (Domain domain)
