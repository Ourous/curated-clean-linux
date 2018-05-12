definition module iTasks.Extensions.Email
/**
* This module provides basic SMTP email support
*/
import iTasks

/**
* Send an e-mail message.
*
* @param Options: Mail server options, when left blank port 25 on localhost is used SMTP server
* @param Sender: The sender address
* @param Recipient: The recipient address
* @param Subject: The subject line of the e-mail message
* @param Body: The body of the e-mail message
*/
sendEmail :: ![EmailOpt] !String !String !String !String -> Task ()

//Options for sendEmail
:: EmailOpt
	= EmailOptSMTPServer !String 				//SMTP server to use. Default: localhost
	| EmailOptSMTPServerPort !Int 				//TCP port of the SMTP server to use. Default: 25
	| EmailOptExtraHeaders ![(!String,!String)] //Additional headers to add before the body
