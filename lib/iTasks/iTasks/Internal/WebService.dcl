definition module iTasks.Internal.WebService
/**
* This module provides the web service that gives access to tasks via the web.
* It also provides access to upload/download of blob content.
*/
from Internet.HTTP				import :: HTTPRequest, :: HTTPResponse
from iTasks.Engine              import :: PublishedTask
from iTasks.Internal.IWorld		import :: IWorld
from iTasks.Internal.Task 	    import :: Task, :: ConnectionTask
from iTasks.Internal.TaskState 	import :: TIUIState
from iTasks.Internal.TaskStore  import :: TaskOutput, :: TaskOutputMessage
from iTasks.Internal.SDS 			import :: SDS, :: RWShared
from iTasks.UI.Definition           import :: UIChange
from iTasks.WF.Definition	        import :: InstanceNo
from Data.Queue 					import :: Queue
from Data.Maybe                     import :: Maybe
from Data.Map                       import :: Map
from System.Time                    import :: Timespec

:: ConnectionState :== (String, WebSockState,[(InstanceNo,String)])

:: WebSockState =
	{ cur_frame    :: !{#Char}   //The fram
	, message_text :: !Bool     // True -> text message, False -> binary
	, message_data :: ![String] // Message data from previous frames 
	}

:: WebSockEvent
	= WSTextMessage String //A UTF-8 text message was received completely
	| WSBinMessage String  //A binary message was received completely
	| WSClose String       //A close frame was received
	| WSPing String        //A ping frame was received

:: WebService r w =
    { urlMatchPred    :: !(String -> Bool)                                                                                              // checks whether the URL is served by this service
    , completeRequest :: !Bool                                                                                                          // wait for complete request before start serving request
    , onNewReq        :: !(HTTPRequest r                        *IWorld -> *(!HTTPResponse,!Maybe ConnectionState, !Maybe w, !*IWorld)) // is called for each new request
    , onData          :: !(HTTPRequest r String ConnectionState *IWorld -> *(![{#Char}], !Bool, !ConnectionState, !Maybe w, !*IWorld))  // on new data from client
    , onShareChange   :: !(HTTPRequest r        ConnectionState *IWorld -> *(![{#Char}], !Bool, !ConnectionState, !Maybe w, !*IWorld))  // on shared change
    , onTick          :: !(HTTPRequest r        ConnectionState *IWorld -> *(![{#Char}], !Bool, !ConnectionState, !Maybe w, !*IWorld))  // called on each iteration of main loop
    , onDisconnect    :: !(HTTPRequest r ConnectionState        *IWorld -> *(!Maybe w, !*IWorld))                                       // is called on disconnect
    }

httpServer :: !Int !Timespec ![WebService r w] (RWShared () r w) -> ConnectionTask | TC r & TC w

:: OutputQueues :== Map InstanceNo TaskOutput

taskUIService         :: ![PublishedTask] -> WebService OutputQueues OutputQueues
documentService       ::                     WebService r w
staticResourceService :: [String]         -> WebService r w

