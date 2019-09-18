implementation module iTasks.Extensions.Email

import iTasks
import Data.Functor, Data.Func
import Text, Text.HTML

sendEmail :: ![EmailOpt] !String ![String] !String !String -> Task ()
sendEmail opts sender recipients subject body
	= tcpconnect server port (constShare ()) {ConnectionHandlers|onConnect=onConnect,onData=onData,onDisconnect=onDisconnect,onShareChange = \l _ = (Ok l, Nothing, [], False), onDestroy= \s->(Ok s, [])}
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
			]
		++
			((\recipient -> (smtpTo recipient, 250)) <$> recipients)
		++
			[(smtpData, 354)
			,(smtpBody sender recipients headers subject body, 250)
			,(smtpQuit, 221)
			]

	//Send the first message
	onConnect :: !ConnectionId !String !() -> (!MaybeErrorString [(String, Int)], !Maybe (), ![String], !Bool)
    onConnect _ _ _
        = (Ok messages,Nothing,[],False)
	//Response to last message: if ok, close connection
	onData :: !String ![(String, Int)] !() -> (!MaybeErrorString [(String, Int)], !Maybe (), ![String], !Bool)
    onData data [(_,expectedCode)] _
		| statusCode data == expectedCode
			= (Ok [],Nothing,[],True)
        	= (Error data,Nothing,[],False)
	//Response to other messages: if ok, send next message
    onData data [(_,expectedCode):ms] _
		| statusCode data == expectedCode
        	= (Ok ms,Nothing,[fst (hd ms)],False)
        	= (Error data,Nothing,[],False)

	//We don't expect the server to disconnect before we close
	//the connection ourselves
    onDisconnect _ _
		= (Error "SMTP server disconnected unexpectedly",Nothing)

sendHtmlEmail :: ![EmailOpt] !String ![String] !String !HtmlTag -> Task ()
sendHtmlEmail opts sender recipients subject body =
	sendEmail [EmailOptExtraHeaders [("content-type", "text/html")]: opts] sender recipients subject htmlString
where
	// avoid too long lines (SMTP allows a max length of 1000 characters only)
	// by inserting a newline (\r\n is required for mails) after each tag
	htmlString = replaceSubString ">" ">\r\n" $ toString body

// SMTP messages
smtpHelo = "HELO localhost\r\n"
smtpFrom email_from = "MAIL FROM:<" +++ (cleanupEmailString email_from) +++ ">\r\n"
smtpTo email_to = "RCPT TO:<" +++ (cleanupEmailString email_to) +++ ">\r\n"
smtpData = "DATA\r\n"
smtpBody email_from email_to email_headers email_subject email_body 
	= concat [k+++":"+++ v +++ "\r\n" \\ (k,v) <-
				[("From",cleanupEmailString email_from)
				: (\email_to -> ("To",cleanupEmailString email_to)) <$> email_to
				] ++
				[("Subject",cleanupEmailString email_subject)
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
getHeadersOpt [EmailOptExtraHeaders s:xs]	= s ++ getHeadersOpt xs
getHeadersOpt [x:xs] 						= getHeadersOpt xs
