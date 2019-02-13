definition module iTasks.Extensions.Web
import iTasks
from Internet.HTTP import :: HTTPMethod, :: HTTPRequest, :: HTTPResponse
from Text.URI import :: URI
from Text.HTML import class html
/**
* This module provides support for building web applications.
*/

//* Uniform resource locators
:: URL = URL !String
instance toString	URL
instance html		URL

derive gEditor    URL
derive gText      URL
derive JSONEncode URL
derive JSONDecode URL
derive gDefault	  URL
derive gEq        URL

//Simple web server task
serveWebService :: Int (HTTPRequest -> Task HTTPResponse) -> Task ()

//Task for serving a static file
serveFile :: [FilePath] HTTPRequest -> Task HTTPResponse

/**
* Calls an external HTTP webservice.
*
* @param HTTP Method: the HTTP method (GET or POST) to use
* @param URL: The URL of the webservice
* @param Parameters: A list of name/value pairs
* @param Response handler: A parse function that parses the response
*
* @return The parsedd value
*
* @gin-title Call web service
* @gin-icon webservice
*/
callHTTP	:: !HTTPMethod !URI !String !(HTTPResponse -> (MaybeErrorString a)) -> Task a | iTask a
callRPCHTTP :: !HTTPMethod !URI ![(String,String)] !(HTTPResponse -> a) -> Task a | iTask a
