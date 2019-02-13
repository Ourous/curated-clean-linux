implementation module iTasks.Extensions.Device._Common
import iTasks
from Text import class Text, instance Text String
import qualified Text as T

:: DeviceRequestState  = { buffer :: String, result :: String }

derive class iTask DeviceRequestState

deviceRequest :: String (String -> Bool) -> Task String
deviceRequest request close
	= tcpconnect "127.0.0.1" 20097 (constShare ())
		{ ConnectionHandlers
		| onConnect      = onConnect
		, onData	 = onData
		, onShareChange  = onShareChange
		, onDisconnect   = onDisconnect
		} 
	>>= \{DeviceRequestState|result} -> return result            
where
	onConnect :: ConnectionId String () -> (MaybeErrorString DeviceRequestState, Maybe (), [String], Bool)
	onConnect connId host _
		= ( Ok { buffer = "", result = "" }
		  , Nothing
      	  , [request +++ "\n"]
      	  , False
      	  )

	onData :: String DeviceRequestState () -> (MaybeErrorString DeviceRequestState, Maybe (), [String], Bool)
	onData newData state=:{buffer} _
		#! buffer = buffer +++ newData
		# splitpoint = 'T'.indexOf "\n" buffer
		| splitpoint <> -1 		
			# result = 'T'.subString 0 splitpoint buffer
			# incomplete = 'T'.dropChars (splitpoint + 1) buffer
			= (Ok {state & buffer = incomplete
			  , result = result}
			  , Nothing
			  , []
			  , close result
			  )
		= (Ok {state & buffer = buffer}, Nothing, [], False)

	onShareChange state _
		= (Ok state, Nothing, [], False)

	onDisconnect :: DeviceRequestState () -> (MaybeErrorString DeviceRequestState, Maybe ())
	onDisconnect state _
		= (Ok state, Nothing)
