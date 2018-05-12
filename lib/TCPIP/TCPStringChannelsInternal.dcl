definition module TCPStringChannelsInternal

from TCPDef import ::TCP_RChannel,::TCP_RChannel_,::ByteSeq

//	a StringChannel has three states:
::	*ReadPhase	= ReadingLength !Int 				// the channel is currently reading the length of the next
													// string. The integer is the length that has been read
													// up until now.
				| ReadingString !*{#Char} !Int !Int // the channel is currently receiving the next string
													// For (ReadingString toFill filled s) it holds:
													//	 s==size toFill=="the size that the next receivable string will have"
													// filled characters already have been received and written into toFill
				| EndOfMessages						// the underlying tcp channel is eom or a string that is too big is received


::	*StringRChannel_ a	
	=	{	tcp_rchan		::	!TCP_RChannel
		,	readPhase		::	!ReadPhase
		,	receivedStrings	::	![String]
		,	maxSize			::	!Int
		}

addString	::	!(!String, !Int) !ReadPhase !Int -> (![String], !ReadPhase)
