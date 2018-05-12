implementation module iTasks.Extensions.Web
import iTasks
import iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import Internet.HTTP, Text, Text.HTML, Text.URI, Text.Encodings.MIME, Text.Encodings.UrlEncoding, StdArray, Data.Either
import System.Time, System.FilePath

from iTasks.Internal.HttpUtil import http_addRequestData, http_parseArguments
import iTasks.Internal.HtmlUtil

import iTasks.Extensions.Document
import iTasks.Extensions.TextFile

import qualified Data.Map as DM
import qualified Data.List as DL

//* URL
gText{|URL|}	_ val	= [maybe "" toString val]

gEditor{|URL|} = selectByMode
		(comapEditorValue (\(URL s) -> ATag [HrefAttr s] [Text s]) htmlView)
		(bijectEditorValue (\(URL s) -> s) (\s -> URL s) (withDynamicHintAttributes "uniform resource locator (URL)" textField ))
		(bijectEditorValue (\(URL s) -> s) (\s -> URL s) (withDynamicHintAttributes "uniform resource locator (URL)" textField ))

derive JSONEncode		URL
derive JSONDecode		URL
derive gDefault			URL
derive gEq				URL

instance toString URL
where
	toString (URL url) = url

instance html URL
where
	html (URL url) = ATag [HrefAttr url] [Text url]


KEEPALIVE_TIME :== {tv_sec=5, tv_nsec=0}

:: HttpConnState
    = Idle String Timespec
    | ReadingRequest HttpReqState
	| AwaitingResponse String Int Bool

:: HttpReqState =
    { request       :: HTTPRequest
    , method_done   :: Bool
    , headers_done  :: Bool
    , data_done     :: Bool
    , error         :: Bool
    }

derive class iTask HttpConnState, HttpReqState, HTTPRequest, HTTPResponse, HTTPMethod, HTTPProtocol, HTTPUpload

serveWebService :: Int (HTTPRequest -> Task HTTPResponse) -> Task ()
serveWebService port handler 
	= withShared []
		\io ->
		manageConnections io -&&- handleRequests io
    @! ()
where
	manageConnections io
		= tcplisten port False (currentTimespec |+< io)
			{ConnectionHandlers|onConnect=onConnect,onData=onData,onShareChange=onShareChange,onDisconnect=onDisconnect}

    onConnect client_name (now,io)
		= (Ok (Idle client_name now), Nothing, [], False)

    onData data l=:(Idle client_name last) (now,io)
		# request = {newHTTPRequest & client_name = client_name, server_port = port}
	 	# (request, method_done, headers_done, data_done, error) = http_addRequestData request False False False data
		# reqs = {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
		= whileReadingRequest data reqs now io
	onData data l=:(ReadingRequest {HttpReqState|request, method_done, headers_done, data_done}) (now,io)
		# (request, method_done, headers_done, data_done, error) = http_addRequestData request method_done headers_done data_done (toString data)
		# reqs = {HttpReqState|request=request,method_done=method_done,headers_done=headers_done,data_done=data_done,error=error}
		= whileReadingRequest data reqs now io

	onShareChange l=:(AwaitingResponse client_name reqId keepalive) (now,io)
		= case getResponse reqId io of
			(Nothing,_) = (Ok l, Nothing, [], False)
			(Just response,io)
				//Add keep alive header if necessary
				# response	= if keepalive {HTTPResponse|response & rsp_headers = [("Connection","Keep-Alive"):response.HTTPResponse.rsp_headers]} response
				# reply		= encodeResponse True response
				= (Ok (Idle client_name now), Just io, [reply], keepalive)

	onShareChange l=:(Idle client_name last) (now,_) //Close idle connections if the keepalive time passed
		= (Ok l, Nothing, [], now - last > KEEPALIVE_TIME)

    onShareChange l (now,io)
		= (Ok l, Nothing, [], False)

	whileReadingRequest data reqs now io
		| reqs.HttpReqState.error
			//Sent bad request response and disconnect
			= (Ok (Idle reqs.HttpReqState.request.client_name now) , Nothing, ["HTTP/1.1 400 Bad Request\r\n\r\n"], True)
		| not reqs.HttpReqState.headers_done
			//Without headers we can't do anything yet
			= (Ok (ReadingRequest reqs), Nothing, [], False)
		| not reqs.HttpReqState.data_done	
			//For now only support full requests
			= (Ok (ReadingRequest reqs), Nothing, [], False)
		//Queue request to get a response
		# request	= http_parseArguments reqs.HttpReqState.request 
		//Determine if a  persistent connection was requested
		# keepalive	= isKeepAlive request
		//Add the request to be handled and wait
		# (reqId,io) = addRequest request io
		= (Ok (AwaitingResponse request.client_name reqId keepalive), Just io, [], False)

    onDisconnect l _        = (Ok l, Nothing)

	isKeepAlive request = maybe (request.HTTPRequest.req_version == "HTTP/1.1") (\h -> (toLowerCase h == "keep-alive")) ('DM'.get "Connection" request.HTTPRequest.req_headers)

    encodeResponse autoContentLength response=:{rsp_headers, rsp_data}
	    # rsp_headers = addDefault rsp_headers "Server" "iTasks HTTP Server"
	    # rsp_headers = addDefault rsp_headers "Content-Type" "text/html"
	    # rsp_headers = if autoContentLength
	    					(addDefault rsp_headers "Content-Length" (toString (size rsp_data)))
	    					rsp_headers
	    = toString {HTTPResponse|response & rsp_headers = rsp_headers}
    where		
    	addDefault headers hdr val = if (('DL'.lookup hdr headers) =: Nothing) [(hdr,val):headers] headers

	handleRequests io
		= 	forever (
				(watch io @ listRequests) 								 //Watch for unhandled requests
			>>* [OnValue (ifValue (not o isEmpty) (createResponses io))] //Handle the new requests and store responses
			)
	
	createResponses slist requests 
		= 	allTasks [handler req \\ (_,req) <- requests]
		>>- \responses ->
			upd (addResponses [(reqId,rsp) \\ (reqId,_) <- requests & rsp <- responses]) slist
		@! ()
	where
		addResponses [] list = list
		addResponses [(reqId,rsp):rest] list = addResponses rest (addResponse reqId rsp list)
		
//The data structure shared between the management of connections and the actual processing of requests
:: ConnectionList :== [(Int,Either HTTPRequest HTTPResponse)]
:: RequestId 	:== Int

addRequest :: HTTPRequest ConnectionList -> (RequestId,ConnectionList)
addRequest req list = addRequest` 0 req list
where
	addRequest` max req [] = let max` = max + 1 in (max`,[(max`,Left req)])
	addRequest` max req [x=:(i,_):xs] = let (id,xs`) = addRequest` (if (i > max) i max) req xs in (id,[x:xs`])

listRequests :: ConnectionList -> [(RequestId,HTTPRequest)]
listRequests list = [(i,req) \\ (i,Left req) <- list]

addResponse :: RequestId HTTPResponse ConnectionList -> ConnectionList
addResponse reqId rsp [] = []
addResponse reqId rsp [x=:(i,_):xs]
	| i == reqId 	= [(i,Right rsp):xs]
	| otherwise		= [x:addResponse reqId rsp xs]

getResponse :: RequestId ConnectionList -> (Maybe HTTPResponse,ConnectionList)
getResponse reqId [] = (Nothing,[])
getResponse reqId [x=:(i,Right rsp):xs]
	| i == reqId 	     = (Just rsp,xs)
	| otherwise  		 = let (mbrsp,xs`) = getResponse reqId xs in (mbrsp,[x:xs])
getResponse reqId [x:xs] = let (mbrsp,xs`) = getResponse reqId xs in (mbrsp,[x:xs])

serveFile :: [FilePath] HTTPRequest -> Task HTTPResponse
serveFile [] req = return (notFoundResponse req)
serveFile [d:ds] req=:{HTTPRequest|req_path}
	= 	try (importTextFile (d +++ filePath) @ toResponse)
		    (\(FileException _ _) -> serveFile ds req)
where
	//Translate a URL path to a filesystem path
	filePath = ((replaceSubString "/" {pathSeparator}) o (replaceSubString ".." "")) (urlDecode req_path)
	mimeType = extensionToMimeType (takeExtension filePath)

	toResponse content
	  = {HTTPResponse|okResponse
		& rsp_headers =
			[("Content-Type", mimeType)
			,("Content-Length", toString (size content))]
		, rsp_data = content
		}

callHTTP :: !HTTPMethod !URI !String !(HTTPResponse -> (MaybeErrorString a)) -> Task a | iTask a
callHTTP method url=:{URI|uriScheme,uriRegName=Just uriRegName,uriPort,uriPath,uriQuery,uriFragment} data parseFun
    =   tcpconnect uriRegName port (constShare ()) {ConnectionHandlers|onConnect=onConnect,onData=onData,onShareChange=onShareChange,onDisconnect=onDisconnect}
    @?  taskResult
where
    port = fromMaybe 80 uriPort
    path = uriPath +++ maybe "" (\q -> ("?"+++q)) uriQuery +++ maybe "" (\f -> ("#"+++f)) uriFragment
    //VERY SIMPLE HTTP 1.1 Request
    req = toString method +++ " " +++ path +++ " HTTP/1.1\r\nHost:"+++uriRegName+++"\r\nConnection: close\r\n\r\n"+++data

    onConnect _ _
        = (Ok (Left []),Nothing,[req],False)
    onData data (Left acc) _
        = (Ok (Left (acc ++ [data])),Nothing,[],False)
    onShareChange acc _
        = (Ok acc,Nothing,[],False)
    onDisconnect (Left acc) _
        = case parseResponse (concat acc) of
			Nothing    = (Error "Invalid response",Nothing)
            (Just rsp) = case parseFun rsp of
 				               	Ok a    = (Ok (Right a),Nothing)
                				Error e = (Error e,Nothing)

    taskResult (Value (Right a) _)  = Value a True
    taskResult _                    = NoValue

callHTTP _ url _ _
    = throw ("Invalid url: " +++ toString url)

callRPCHTTP :: !HTTPMethod !URI ![(String,String)] !(HTTPResponse -> a) -> Task a | iTask a
callRPCHTTP method url params transformResult
	= callHTTP method url (urlEncodePairs params) (Ok o transformResult)


