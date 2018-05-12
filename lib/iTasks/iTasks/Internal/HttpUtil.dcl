definition module iTasks.Internal.HttpUtil

import Internet.HTTP, StdFile

//General utility functions
http_splitMultiPart :: !String !String -> [([(String,String)], String)]

//Incremental construction of a request
http_addRequestData :: !HTTPRequest !Bool !Bool !Bool !String -> (HTTPRequest, Bool, Bool, Bool, Bool)

//Parsing of HTTP Request messages
http_parseRequestLine :: !String -> (!String, !String, !String, !String, !Bool)
http_parseHeader :: !String -> (!(String,String), !Bool)

http_parseArguments :: !HTTPRequest -> HTTPRequest
http_parseGetArguments :: !HTTPRequest -> Map String String
http_parsePostArguments :: !HTTPRequest -> Map String String
http_parseUrlEncodedArguments :: !String -> [(String,String)]
http_parseMultiPartPostArguments :: !HTTPRequest -> (Map String String, Map String HTTPUpload) 

//Construction of HTTP Response messages
http_makeResponse :: !HTTPRequest ![((String -> Bool),(HTTPRequest *st -> (HTTPResponse, *st)))] !Bool !*st -> (!HTTPResponse,!*st) | FileSystem st
http_addDateHeaders	:: !HTTPResponse !*World -> (!HTTPResponse,!*World)

//Static content
http_staticResponse :: !HTTPRequest !*st -> (!HTTPResponse, !*st) | FileSystem st
http_staticFileContent :: !String !*st -> (!Bool, !String, !*st) | FileSystem st
http_staticFileMimeType :: !String !*st -> (!String, !*st)

//Server control
http_serverControl :: !HTTPResponse -> String
