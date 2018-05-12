implementation module iTasks.Extensions.Email
import iTasks
import Text

sendEmail :: ![EmailOpt] !String !String !String !String -> Task ()
sendEmail opts subject body sender recipient
	= tcpconnect server port (constShare ()) {ConnectionHandlers|onConnect=onConnect,whileConnected=whileConnected,onDisconnect=onDisconnect}
	@! ()
where
	server 	= getServerOpt opts
	port	= getPortOpt opts
	headers = getHeadersOpt opts
	//Sending the message with SMTP is essentially one-way communication
	//but we send it in parts. After each part we get a response with a status code.
	//After each message we check if it is a status code we expect.
	messages =
		[("",220) //Initially we don't send anything, but wait for the welcome message from the server
		,(smtpHelo, 250)
		,(smtpFrom sender, 250)
		,(smtpTo recipient, 250)
		,(smtpData, 354)
		,(smtpBody sender recipient headers subject body, 250)
		,(smtpQuit, 221)
		]

	//Send the first message
    onConnect _ _
        = (Ok messages,Nothing,[],False)
	//Response to last message: if ok, close connection
    whileConnected (Just data) [(_,expectedCode)] _
		| statusCode data == expectedCode
			= (Ok [],Nothing,[],True)
        	= (Error data,Nothing,[],False)
	//Response to other messages: if ok, send next message
    whileConnected (Just data) [(_,expectedCode):ms] _ 
		| statusCode data == expectedCode
        	= (Ok ms,Nothing,[fst (hd ms)],False)
        	= (Error data,Nothing,[],False)
	//All other cases: just wait
    whileConnected _ state _ 
        = (Ok state,Nothing,[],False)

	//We don't expect the server to disconnect before we close
	//the connection ourselves
    onDisconnect _ _
		= (Error "SMTP server disconnected unexpectedly",Nothing)

// SMTP messages
smtpHelo = "HELO localhost\r\n"
smtpFrom email_from = "MAIL FROM:<" +++ (cleanupEmailString email_from) +++ ">\r\n"
smtpTo email_to = "RCPT TO:<" +++ (cleanupEmailString email_to) +++ ">\r\n"
smtpData = "DATA\r\n"
smtpBody email_from email_to email_headers email_subject email_body 
	= concat [k+++":"+++ v +++ "\r\n" \\ (k,v) <-
				[("From",cleanupEmailString email_from)
				,("To",cleanupEmailString email_to)
				,("Subject",cleanupEmailString email_subject)
				:email_headers]
			 ]
	+++ "\r\n" +++ email_body +++ "\r\n.\r\n"
smtpQuit = "QUIT\r\n"

//Utility functions

//Parse the reply of the server into a status code
statusCode :: String -> Int
statusCode msg = toInt (msg % (0,2))

//Strip any newline chars and tabs from a string.
cleanupEmailString :: String -> String
cleanupEmailString s = toString (filter (\x -> not (isMember x ['\r\n\t'])) (fromString s))

getServerOpt [] 						= "localhost"
getServerOpt [EmailOptSMTPServer s:xs]	= s
getServerOpt [x:xs] 					= getServerOpt xs

getPortOpt [] 						= 25 
getPortOpt [EmailOptSMTPServerPort s:xs]	= s
getPortOpt [x:xs] 					= getPortOpt xs

getHeadersOpt [] 							= []
getHeadersOpt [EmailOptExtraHeaders s:xs]	= s
getHeadersOpt [x:xs] 						= getHeadersOpt xs
