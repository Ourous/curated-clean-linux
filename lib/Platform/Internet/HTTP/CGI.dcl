definition module Internet.HTTP.CGI

import Internet.HTTP

:: CGIOption 	= CGIOptStaticFallback Bool // If all request handlers fail, should the static file handler be tried (default False)
				| CGIOptParseArguments Bool	// Should the query and body of the request be parsed (default True)

startCGI :: [CGIOption] [((String -> Bool),(HTTPRequest *World-> (HTTPResponse,*World)))] *World -> *World
