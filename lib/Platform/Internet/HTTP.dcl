definition module Internet.HTTP

// This library defines HTTP related types and functions
import StdString
import Data.Maybe
from Data.Map import :: Map
from Data.Error import :: MaybeErrorString, :: MaybeError

:: HTTPMethod = HTTP_GET 
			  | HTTP_HEAD 
			  | HTTP_PUT 
			  | HTTP_DELETE 
			  | HTTP_POST 
			  | HTTP_OPTIONS
			  | HTTP_TRACE
			  | HTTP_CONNECT
			  | HTTP_CUSTOM !String

/* Do an HTTP request and return the response
 *
 * @param The request
 * @param Timeout in milliseconds
 * @param The world
 */
doHTTPRequest :: !HTTPRequest Int !*World -> *(!MaybeErrorString HTTPResponse, !*World)

/* Do an HTTP request and follow redirects
 *
 * @param The request
 * @param Timeout in milliseconds
 * @param The maximum number of redirects
 * @param The world
 */
doHTTPRequestFollowRedirects :: !HTTPRequest Int !Int !*World -> *(!MaybeErrorString HTTPResponse, !*World)

instance toString   HTTPMethod
instance fromString HTTPMethod

//A raw request header
:: HTTPRequestHeader =
	{ req_method 	:: String  				// The HTTP request method (eg. GET, POST, HEAD)
	, req_path 		:: String  				// The requested location (eg. /foo)
	, req_query 	:: String 				// The query part of a location (eg. ?foo=bar&baz=42)
	, req_version 	:: String  				// The http version (eg. HTTP/1.0 or HTTP/1.1)
	, req_headers	:: [(String,String)] 	//Additional headers
	}

//A fully parsed HTTP request
:: HTTPRequest	= {	req_method		:: 	HTTPMethod				// The HTTP request method (eg. GET, POST, HEAD)
				,	req_path		::	String					// The requested location (eg. /foo)
				,	req_query		::	String					// The query part of a location (eg. ?foo=bar&baz=42)
				,	req_version		::	String					// The http version (eg. HTTP/1.0 or HTTP/1.1)
				,	req_protocol	::	HTTPProtocol			// Protocol info, http or https
				,	req_headers		::	Map String String		// The headers sent with the request parsed into name/value pairs
				,	req_data		::	String					// The raw data of the request (without the headers)
				,	arg_get			::	Map String String		// The arguments passed in the url 
				,	arg_post		::	Map String String		// The arguments passed via the POST method
				,	arg_cookies		::	Map String String		// The cookies in the set-cookie header
				,	arg_uploads		::	Map String HTTPUpload	// Uploads that are sent via the POST method
				,	server_name		::	String					// Server host name or ip address
				,	server_port		::	Int						// Server port
				,	client_name		::	String					// Client host name or ip address
				}


:: HTTPProtocol	= HTTPProtoHTTP | HTTPProtoHTTPS				// The protocol used for a request

:: HTTPResponse	=
	{ rsp_version   :: String  				// The http version (eg. HTTP/1.0 or HTTP/1.1)
	, rsp_code      ::  Int
	, rsp_reason    ::  String
	, rsp_headers 	::	[(String,String)]		// Extra return headers that should be sent (eg. ("Content-Type","text/plain"))
	, rsp_data		::	String					// The body of the response. (eg. html code or file data)
	}

//A raw response header
:: HTTPResponseHeader =
    { rsp_version 	:: String
    , rsp_code 		:: Int
    , rsp_reason    ::  String
    , rsp_headers   :: [(String,String)]
    }

:: HTTPUpload	= { upl_name		::	String					// The name of the file input in the form
				,	upl_filename	::	String					// The filename of the uploaded file
				,	upl_mimetype	::	String					// The MIME content type of the file
				,	upl_content		::	String					// The actual content of the file.
				}


//Construction functions 
newHTTPRequest	:: HTTPRequest
newHTTPUpload	:: HTTPUpload
newHTTPResponse :: !Int !String -> HTTPResponse

//String instances
instance toString HTTPRequest
instance toString HTTPResponse

//Server utilities
parseRequestLine	:: !String																							-> Maybe (!String, !String, !String, !String)
parseHeader			:: !String																							-> Maybe (!String, !String)
parseResponse 		:: !String 																							-> Maybe HTTPResponse

//Request utilities
parseRequest 		:: !HTTPRequest																					-> HTTPRequest

//Generating and cheking responses
okResponse			:: HTTPResponse
isOkResponse 		:: !HTTPResponse -> Bool

notfoundResponse	:: HTTPResponse
forbiddenResponse	:: HTTPResponse

errorResponse 		:: !String -> HTTPResponse
badRequestResponse 	:: !String -> HTTPResponse

staticResponse		:: !HTTPRequest !*World																				-> (!HTTPResponse, !*World)
customResponse		:: ![((String -> Bool),(HTTPRequest *World -> (HTTPResponse, *World)))] !Bool !HTTPRequest !*World	-> (!HTTPResponse, !*World)

encodeResponse :: !Bool !HTTPResponse !*World -> (!String,!*World)
