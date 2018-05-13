implementation module Internet.HTTP

import StdOverloaded, StdString, StdList, StdArray, StdFile, StdBool
import Data.Maybe, Data.List, Text, Text.Encodings.UrlEncoding, Text.Encodings.MIME
from Data.Map import get, put, :: Map (..), newMap, fromList, toList, toAscList, foldrWithKey

from StdFunc import id
from Data.Func import $, instance Functor ((->) r)
import Data.Error
import Text.URI
import Data.Functor

import Control.Applicative
from Control.Monad import class Monad
import qualified Control.Monad as CM
import TCPIP

doHTTPRequest :: !HTTPRequest Int !*World -> *(!MaybeErrorString HTTPResponse, !*World)
doHTTPRequest req timeout w
# (ip,w) = lookupIPAddress req.server_name w
| isNothing ip
	= (Error $ "DNS lookup for " + req.server_name + " failed.", w)
# (Just ip) = ip
# (rpt,chan,w) = connectTCP_MT (Just timeout) (ip, req.server_port) w
| rpt == TR_Expired
	= (Error $ "Connection to " + toString ip + " timed out.", w)
| rpt == TR_NoSuccess
	= (Error $ "Could not connect to " + req.server_name + ".", w)
# (Just {sChannel,rChannel}) = chan
# (rpt,i,sChannel,w) = send_MT (Just timeout) (toByteSeq req) sChannel w
| rpt <> TR_Success
	= (Error $ "Could not send request to " + req.server_name + ".", w)
# (rpt,resp,rChannel,w) = receive_MT (Just timeout) rChannel w
| rpt <> TR_Success
	= (Error $ "Did not receive a reply from " + req.server_name + ".", w)
# resp = 'CM'.join $ parseResponse <$> toString <$> resp
| isNothing resp
	# w = closeChannel sChannel (closeRChannel rChannel w)
	= (Error $ "Server did not respond with HTTP.", w)
# (resp,rChannel,w) = receiveRest (fromJust resp) rChannel w
# w = closeChannel sChannel (closeRChannel rChannel w)
= (resp,w)
where
	receiveRest resp chan w
	# (end,chan,w) = eom chan w
	| end = (Ok resp, chan, w)
	# (rpt,newresp,chan,w) = receive_MT (Just timeout) chan w
	| rpt <> TR_Success
		# (end,chan,w) = eom chan w
		| end = (Ok resp, chan, w)
		| otherwise = (Error $ req.server_name + " hung up during transmission", chan, w)
	= receiveRest {resp & rsp_data=resp.rsp_data + toString (fromJust newresp)} chan w

doHTTPRequestFollowRedirects :: !HTTPRequest Int !Int !*World -> *(!MaybeErrorString HTTPResponse, !*World)
doHTTPRequestFollowRedirects req timeout 0 w = (Error "Maximal redirect number exceeded", w)
doHTTPRequestFollowRedirects req timeout maxRedirects w
# (er, w) = doHTTPRequest req timeout w
| isError er = (er, w)
# resp = fromOk er
| isMember resp.HTTPResponse.rsp_code [301, 302, 303, 307, 308]
	= case lookup "Location" resp.HTTPResponse.rsp_headers of
		Nothing = (Error $ "Redirect given but no Location header", w)
		Just loc = case parseURI loc of
			Nothing = (Error $ "Redirect URI couldn't be parsed", w)
			Just uri = doHTTPRequestFollowRedirects {req 
				& server_name = maybe loc id uri.uriRegName
				, server_port = maybe 80 id uri.uriPort
				, req_path = uri.uriPath
				, req_query = maybe "" ((+++) "?") uri.uriQuery
				} timeout (maxRedirects-1) w
= (er, w)

newHTTPRequest :: HTTPRequest
newHTTPRequest 
			= {	req_method		= HTTP_GET
			  ,	req_path		= ""
			  ,	req_query		= ""
			  ,	req_version		= "HTTP/1.0"
			  ,	req_protocol	= HTTPProtoHTTP
			  ,	req_headers		= newMap
			  ,	req_data		= ""
			  ,	arg_get			= newMap
			  ,	arg_post		= newMap
			  ,	arg_cookies		= newMap
			  ,	arg_uploads		= newMap
			  ,	server_name		= ""
			  ,	server_port		= 80
			  ,	client_name		= ""
			  }
		
newHTTPResponse :: !Int !String -> HTTPResponse
newHTTPResponse rspcode reason 
	= {HTTPResponse | rsp_version = "HTTP/1.0", rsp_code = rspcode, rsp_reason = reason, rsp_headers = [], rsp_data = toString rspcode +++ " - " +++ reason}
		
okResponse :: HTTPResponse
okResponse = newHTTPResponse 200 "OK" 

notfoundResponse :: HTTPResponse
notfoundResponse = newHTTPResponse 404 "Not Found"

forbiddenResponse :: HTTPResponse
forbiddenResponse = newHTTPResponse 403 "Forbidden"

errorResponse :: !String -> HTTPResponse
errorResponse msg 
	= {newHTTPResponse 500 "Internal Server Error" & rsp_data = msg}	

badRequestResponse :: !String -> HTTPResponse
badRequestResponse msg 
	= {newHTTPResponse 400 "Bad Request" & rsp_data = msg}	

isOkResponse :: !HTTPResponse -> Bool
isOkResponse {HTTPResponse|rsp_code = 200} = True
isOkResponse _ = False

newHTTPUpload :: HTTPUpload
newHTTPUpload		= {	upl_name		= ""
					,	upl_filename	= ""
					,	upl_mimetype	= ""
					,	upl_content		= ""
					}

instance toString HTTPMethod
where
	toString HTTP_GET = "GET"
	toString HTTP_HEAD = "HEAD"
	toString HTTP_PUT = "PUT"
	toString HTTP_DELETE = "DELETE"
	toString HTTP_POST = "POST"
	toString HTTP_OPTIONS = "OPTIONS"
	toString HTTP_TRACE = "TRACE"
	toString HTTP_CONNECT = "CONNECT"
	toString (HTTP_CUSTOM str) = str

instance fromString HTTPMethod
where
	fromString str 
		= case lookup ustr pairs of
			(Just method) = method
						  = HTTP_CUSTOM ustr
	where
		ustr = toUpperCase str

		pairs = [("GET", HTTP_GET), 
				 ("HEAD", HTTP_HEAD),
				 ("PUT", HTTP_PUT),
				 ("DELETE", HTTP_DELETE),
				 ("POST", HTTP_POST),
				 ("OPTIONS", HTTP_OPTIONS),
				 ("TRACE", HTTP_TRACE),
				 ("CONNECT", HTTP_TRACE)]

instance toString HTTPRequest
where toString {req_method,req_path,req_query,req_version,req_headers,req_data,server_name}
		= toString req_method +++ " " +++ req_path +++ req_query +++
			" " +++ req_version +++ "\r\n" +++
			foldl (+++) "" [n +++ ": " +++ v +++ "\r\n" \\ (n,v) <- toList req_headers] +++
			"Host: " +++ server_name +++ "\r\n\r\n" +++
			req_data

instance toString HTTPResponse
where
	toString { rsp_version
			 , rsp_code
			 , rsp_reason	
			 , rsp_headers
			 , rsp_data
			 }
	= join "\r\n" (
		[rsp_version +++ " " +++ toString rsp_code +++ " " +++ rsp_reason] ++
		[(n +++ ": " +++ v) \\ (n,v) <- rsp_headers] ++
		["",rsp_data])
			   	
instance toString HTTPProtocol
where
	toString HTTPProtoHTTP = "Http"
	toString HTTPProtoHTTPS = "Https"

//Server utilities
parseRequestLine	:: !String																							-> Maybe (!String, !String, !String, !String)
parseRequestLine line
	# parts						= split " " line
	| length parts <> 3			= Nothing
	# [method,path,version:_]	= parts
	# qindex					= indexOf "?" path
	| qindex <> -1				= Just (method, path % (0, qindex - 1), path % (qindex + 1, size path), version)
								= Just (method, path, "", version)
	
parseHeader			:: !String																							-> Maybe (!String, !String)
parseHeader header
	# index					= indexOf ":" header
	| index < 1				= Nothing
	# name					= trim (header % (0, index - 1))
	# value					= trim (header % (index + 1, size header))
	= Just (name,value)

// TODO: fast solution, needs multipart handling and stuff
parseResponse :: !String -> Maybe HTTPResponse
parseResponse rsp | startsWith "HTTP/" rsp
	| length lines < 4
		= Nothing
	| length code_words < 2
		= Nothing
	= Just {rsp_version = rsp_version, rsp_code = rsp_code, rsp_reason = rsp_reason, rsp_headers = rsp_headers, rsp_data = rsp_data} 
where
	lines 		 = split "\n" rsp

	code_words 	 = split " " (hd lines)
	rsp_version  = hd code_words
	rsp_code     = toInt (hd (tl code_words))
	rsp_reason   = join " " (tl (tl code_words))
	
	header_lines = takeWhile ((<>) "\r") (tl lines)
	rsp_headers	 = map fromJust (filter isJust (map parseHeader header_lines))
	
	data_lines	 = tl (dropWhile ((<>) "\r") (tl lines))
	rsp_data	 = join "\n" data_lines
	
parseResponse rsp = Nothing

//Request utilities
parseRequest 		::	!HTTPRequest																					-> HTTPRequest
parseRequest req
	# req 							= {req & arg_get = parseGetArguments req}		//Parse get arguments
	# type							= case (get "Content-Type") req.HTTPRequest.req_headers of
		(Just ct)	= ct
		(Nothing)	= ""
	| type % (0,32) == "application/x-www-form-urlencoded"
		= {req & arg_post = parsePostArguments req}									//Parse post arguments
	| type % (0,18) == "multipart/form-data"
		# (post,uploads)			= parseMultiPartPostArguments req
		= {req & arg_post = fromList post, arg_uploads = fromList uploads}			//Parse post arguments + uploads
	| otherwise						= req
where	
	parseGetArguments :: !HTTPRequest -> Map String String
	parseGetArguments req
		| req.HTTPRequest.req_query == ""	= newMap
											= fromList (urlDecodePairs req.HTTPRequest.req_query)

	parsePostArguments :: !HTTPRequest -> Map String String
	parsePostArguments req		= fromList (urlDecodePairs req.req_data)

	parseMultiPartPostArguments :: !HTTPRequest -> ([(String,String)],[(String,HTTPUpload)])
	parseMultiPartPostArguments req
		# mimetype				= get "Content-Type" req.HTTPRequest.req_headers
		| isNothing	mimetype	= ([],[]) //Fail
		# mimetype				= fromJust mimetype
		# index					= indexOf "boundary=" mimetype
		| index == -1			= ([],[]) //Fail
		# boundary				= mimetype % (index + 9, size mimetype)
		# parts					= decodeMimeMultipart boundary req.HTTPRequest.req_data
		= parseParts parts [] []
		where
			parseParts [] arguments uploads	= (arguments, uploads)
			parseParts [(headers, body):xs] arguments uploads
				# disposition		= [v \\ (k,v) <- headers | k == "Content-Disposition" ]
				# type				= [v \\ (k,v) <- headers | k == "Content-Type" ]
				| isEmpty disposition || isEmpty type
					= parseParts xs arguments uploads
				# disposition		= hd disposition
				# type				= hd type
				# name				= getParam "name" disposition
				| name == ""		= parseParts xs arguments uploads
				# filename			= getParam "filename" disposition
				| filename == ""	= parseParts xs [(name,body):arguments] uploads
				| otherwise			= parseParts xs arguments [	(name,	{ newHTTPUpload
																		& upl_name		= name
																		, upl_filename	= filename
																		, upl_mimetype	= type
																		, upl_content	= body
																		}):uploads]
			getParam name header
				# index	= indexOf (name +++ "=") header
				| index == -1	= ""
				# header = header % (index + (size name) + 1, size header)
				# index	= indexOf ";" header
				| index == -1	= removequotes header
								= removequotes (header % (0, index - 1))

			removequotes s
				| size s < 2	= s
				# start	= if (s.[0] == '"') 1 0
				# end = if (s.[size s - 1] == '"') (size s - 2) (size s - 1)
				= s % (start, end) 

//Generating responses
staticResponse		:: !HTTPRequest !*World																				-> (!HTTPResponse, !*World)
staticResponse req world
	# filename				= req.HTTPRequest.req_path % (1, size req.HTTPRequest.req_path)		//Remove first slash
	# (type, world)			= fileMimeType filename world
	# (ok, content, world)	= fileContent filename world
	| not ok 				= (notfoundResponse, world)
							= ({HTTPResponse|okResponse & 
								rsp_headers = [("Content-Type", type),
											   ("Content-Length", toString (size content))]
							   ,rsp_data = content}, world)						
where
	fileContent :: !String !*World -> (!Bool, !String, !*World)
	fileContent filename world
		# (ok, file, world)	= fopen filename FReadData world
		| not ok			= (False, "Could not open file", world)
		# (ok, file)		= fseek file 0 FSeekEnd
		| not ok			= (False, "Seek to end of file does not succeed", world)
		# (pos, file)		= fposition file
		# (ok, file)		= fseek file (~pos) FSeekCur
		| not ok			= (False, "Seek to begin of file does not succeed", world)
		# (content, file)	= freads file pos
		# (ok, world)		= fclose file world
		= (True, content, world)

	fileMimeType :: !String !*World -> (!String, !*World)
	fileMimeType ".jpg" world = ("image/jpeg",world)
	fileMimeType ".png" world = ("image/png",world)
	fileMimeType ".gif" world = ("image/gif",world)
	fileMimeType ".bmp" world = ("image/bmp",world)
	fileMimeType ".htm" world = ("text/html",world)
	fileMimeType ".html" world = ("text/html",world)
	fileMimeType ".txt" world = ("text/plain",world)
	fileMimeType ".css" world = ("text/css",world)
	fileMimeType ".js" world = ("text/javascript",world)
	fileMimeType "" world = ("application/octet-stream",world)
	fileMimeType name world = fileMimeType (name % (1, size name)) world

customResponse		:: ![((String -> Bool),(HTTPRequest *World -> (HTTPResponse, *World)))] !Bool !HTTPRequest !*World	-> (!HTTPResponse, !*World)
customResponse [] fallback request world 											//None of the request handlers matched
	| fallback						= (staticResponse request world)				//Use the static response handler
									= (notfoundResponse, world)						//Raise an error
customResponse [(pred,handler):rest] fallback request world
	| (pred request.HTTPRequest.req_path)
									= handler request world							//Apply handler function
									= customResponse rest fallback request world	//Search the rest of the list

//Response utilities
encodeResponse :: !Bool !HTTPResponse !*World -> (!String,!*World)
encodeResponse withreply {rsp_headers = headers, rsp_data = data} world
	# reply = if withreply
		("HTTP/1.0 " +++ (default "200 OK" (lookup "Status" headers)) +++ "\r\n")
		("Status: " +++ (default "200 OK" (lookup "Status" headers)) +++ "\r\n")
	# reply = reply +++ ("Server: " +++ (default "Clean HTTP tools" (lookup "Server" headers)) +++ "\r\n") //Server identifier
	# reply = reply +++ ("Content-Type: " +++ (default "text/html" (lookup "Content-Type" headers)) +++ "\r\n") //Content type header
	# reply = reply +++ ("Content-Length: " +++ (toString (size data)) +++ "\r\n") //Content length header
	# reply = reply +++ (foldr (+++) "" [(n +++ ": " +++ v +++ "\r\n") \\ (n,v) <- headers | not (skipHeader n)]) //Additional headers
	# reply = reply +++ ("\r\n" +++ data) //Separator + data
	= (reply, world)
where
	//Do not add these headers two times
	default def mbval = case mbval of
		Nothing = def
		(Just val) = val
	skipHeader s = isMember s ["Status","Date","Server","Content-Type","Content-Length","Last-Modified"]
