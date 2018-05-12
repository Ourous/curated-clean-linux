definition module TCPDef

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	StdTCPDef provides basic definitions for using TCP.
//	Author: Martin Wierich
//	Modified: 7 September 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

from	StdMaybe		import :: Maybe
from	StdOverloaded	import class ==, class toString

from	TCPChannelClass	import :: DuplexChannel
from	tcp				import :: IPAddress, :: TCP_Listener_, :: TCP_RChannel_, :: TCP_SChannel_

::	*TCP_SChannel		:==	TCP_SChannel_ ByteSeq
::	*TCP_RChannel		:==	TCP_RChannel_ ByteSeq
::	*TCP_Listener		:== TCP_Listener_ *(IPAddress, TCP_DuplexChannel)

::	Port				:== Int

::	*TCP_DuplexChannel	:== DuplexChannel *TCP_SChannel_ *TCP_RChannel_ ByteSeq

::	ByteSeq
//	A sequence of bytes

instance toString	ByteSeq
instance ==			ByteSeq
toByteSeq		::	!x			-> ByteSeq	| toString x
byteSeqSize		::	!ByteSeq	-> Int
//	byteSeqSize returns the size in bytes

instance toString IPAddress	
//	returns ip address in dotted decimal form

//	********************************************************************************
//	for multiplexing
//	********************************************************************************

:: *TCP_RChannels = TCP_RChannels *[TCP_RChannel]	
:: *TCP_SChannels = TCP_SChannels *[TCP_SChannel]
:: *TCP_Listeners = TCP_Listeners *[TCP_Listener]

::	*PrimitiveRChannel
 =	TCP_RCHANNEL TCP_RChannel
 |	TCP_LISTENER TCP_Listener

::	SelectResult
 =	SR_Available
 |	SR_EOM
 |	SR_Sendable
 |	SR_Disconnected
