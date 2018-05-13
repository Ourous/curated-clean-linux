implementation module Internet.HTTP.CGI

import StdFile, StdInt, StdBool, StdArray
import Data.Maybe, Data.Tuple, Internet.HTTP, System.Environment, Data.Map

//Http headers which should be polled in the environment
CGI_HEADERS :== [ ("Content-Type","CONTENT_TYPE")
				, ("Content-Length","CONTENT_LENGTH")
				, ("Content-Encoding","HTTP_CONTENT_ENCODING")
				, ("Accept","HTTP_ACCEPT")
				, ("User-Agent","HTTP_USER_AGENT")
				, ("Host", "HTTP_HOST")
				, ("Authorization","HTTP_AUTHORIZATION")
				, ("If-Modified-Since","HTTP_IF_MODIFIED_SINCE")
				, ("Referer","HTTP_REFERER")
				]

//Starts the CGI Wrapper
startCGI :: [CGIOption] [((String -> Bool),(HTTPRequest *World-> (HTTPResponse,*World)))] *World -> *World
startCGI options handlers world
	# (console, world)		= stdio world
	# (ok,console)			= freopen console FReadData
	# (datalength, world)	= getDataLength world
	# (data, console)		= getData datalength console											//Read post data
	# (req_method, world)	= appFst fromString (getFromEnv "REQUEST_METHOD" world) //Read environment data
	# (req_path, world)		= getFromEnv "SCRIPT_NAME" world
	# (req_query, world)	= getFromEnv "QUERY_STRING" world
	# (req_version, world)	= getFromEnv "SERVER_PROTOCOL" world
	# (req_headers, world)	= makeHeaders CGI_HEADERS world
	# (server_name, world)	= getFromEnv "SERVER_NAME" world
	# (server_port, world)	= getFromEnv "SERVER_PORT" world
	# (client_name, world)	= getClientName world
	# request				= {newHTTPRequest	&	req_method = req_method,		//Create the request
													req_path = req_path,
													req_query = req_query,
													req_version = req_version, 
													req_headers = req_headers,
													req_data = data,
													server_name = server_name,
													server_port = toInt server_port,
													client_name = client_name
							  }
	# request				= if (getParseOption options) (parseRequest request) request
	# (response,world)		= customResponse handlers (getStaticOption options) request world
	# (response,world)		= encodeResponse False response world
	# (ok,console)			= freopen console FWriteData
	# console				= fwrites response console
	# (ok,world)			= fclose console world
	= world

getDataLength :: *World -> (Int, *World)
getDataLength world
	# (mbLen,world)	= getEnvironmentVariable "CONTENT_LENGTH" world
	= case mbLen of
		Nothing		= (0, world)
		(Just len)	= (toInt len, world)

getData :: !Int !*File -> (!String, !*File)
getData len file = freads file len

getFromEnv :: String *World -> (String, *World)
getFromEnv name world
	# (mbValue,world)	= getEnvironmentVariable name world
	= case mbValue of
		Nothing		= ("",world)
		(Just v)	= (v,world)
						
getClientName :: *World -> (String, *World)
getClientName world
	# (name,world)	= getFromEnv "REMOTE_HOST" world
	| name == ""	= getFromEnv "REMOTE_ADDR" world
					= (name,world)
									
makeHeaders :: [(String,String)] *World -> (Map String String, *World)
makeHeaders cgihdrs world = case makeHeaders` cgihdrs world of (hdrs,world) = (fromList hdrs, world)
where
	makeHeaders` [] world = ([],world)
	makeHeaders` [(name,envname):xs] world
		# (mbValue, world)	= getEnvironmentVariable envname world
		= case mbValue of
			Nothing			= makeHeaders` xs world
			(Just value)	
				# (xs,world)	= makeHeaders` xs world
				= ([(name,value):xs],world)

getStaticOption :: [CGIOption] -> Bool
getStaticOption [] = False
getStaticOption [x:xs] = case x of	(CGIOptStaticFallback b) = b
									_							 = getStaticOption xs

getParseOption	:: [CGIOption] -> Bool
getParseOption [] = True
getParseOption [x:xs] = case x of (CGIOptParseArguments b)	= b
								  _								= getParseOption xs
