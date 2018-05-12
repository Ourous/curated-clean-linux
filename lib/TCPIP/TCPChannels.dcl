definition module TCPChannels

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	StdTCPChannels provides instances to use TCP.
//	Author: Martin Wierich
//	Modified: 7 September 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdString
import	TCPDef, TCPChannelClass
from	tcp_bytestreams	import :: TCP_RCharStream_{..}, :: TCP_SCharStream_{..}

:: TCP_Void = TCP_Void
:: TCP_Pair a b = TCP_Pair a b

//	********************************************************************************
//	Listeners
//	********************************************************************************

instance Receive			TCP_Listener_
instance closeRChannel		TCP_Listener_
/*	Receiving on a listener will accept a TCP_DuplexChannel. eom never becomes True
	for listeners.
*/

//	********************************************************************************
//	TCP send channels
//	********************************************************************************

instance Send				TCP_SChannel_

//	********************************************************************************
//	TCP receive channels
//	********************************************************************************

instance Receive			TCP_RChannel_
instance closeRChannel		TCP_RChannel_
instance MaxSize			TCP_RChannel_

//	********************************************************************************
//	TCP char streams to receive
//	********************************************************************************

::	*TCP_RCharStream	:==	TCP_RCharStream_ Char
::	*TCP_RCharStreams	= 	TCP_RCharStreams *[TCP_RCharStream]

toRCharStream			:: !TCP_RChannel -> TCP_RCharStream

instance Receive			TCP_RCharStream_
instance closeRChannel		TCP_RCharStream_

//	********************************************************************************
//	TCP char streams to send
//	********************************************************************************

::	*TCP_SCharStream	:==	TCP_SCharStream_ Char
::	*TCP_SCharStreams	= 	TCP_SCharStreams *[TCP_SCharStream]

toSCharStream			::	!TCP_SChannel -> TCP_SCharStream

instance Send				TCP_SCharStream_

//	********************************************************************************
//	establishing connections
//	********************************************************************************

lookupIPAddress	:: !String !*env
				-> (!Maybe IPAddress, !*env)
				|  ChannelEnv env
connectTCP_MT	:: !(Maybe Timeout) !(!IPAddress,!Port) !*env
				-> (!TimeoutReport, !Maybe TCP_DuplexChannel, !*env) 
				|  ChannelEnv env
openTCP_Listener:: !Port !*env
				-> (!Bool, !Maybe TCP_Listener, !*env)
				|  ChannelEnv env
tcpPossible		:: !*env
				-> (!Bool, !*env)
				|  ChannelEnv env
/*	lookupIPAddress
		input String can be in dotted decimal form or alphanumerical. In the latter
		case the DNS is called.
	connectTCP
		tries to establish a TCP connection.
	openTCP_Listener
		to listen on a certain port.
	tcpPossible
		whether tcp can be started on this computer.
*/

//	********************************************************************************
//	multiplexing
//	********************************************************************************

selectChannel_MT:: !(Maybe Timeout)          !*r_channels !*s_channels !*env
				-> (![(!Int, !SelectResult)],!*r_channels,!*s_channels,!*env)
				|  SelectReceive r_channels & SelectSend s_channels & ChannelEnv env
/*	selectChannel_MT mbTimeout r_channels s_channels world
		determines the first channel on which "something happens". 
		If the result is an empty list, then the timeout expired, otherwise each 
		(who,what) element of the result identifies one channel in r_channels or 
		s_channels. The what value determines whether available/eom/disconnected 
		on the identified channel would have returned True. 
		what==SR_Sendable indicates that it is possible to send non blocking on the 
		identified channel. If r_channels contains r channels and if s_channels 
		contains s channels, then the following holds:
			isMember what [SR_Available,SR_EOM]          => 0<=who<r
			isMember what [SR_Sendable ,SR_Disconnected] => 0<=who<s
*/		
		
instance ==       SelectResult
instance toString SelectResult

/*	The following classes support the selectChannel_MT function:
*/
class SelectReceive channels where
	accRChannels	:: (PrimitiveRChannel -> (x, PrimitiveRChannel)) !*channels
					-> (![x], !*channels)
	getRState		:: !Int !*channels !*env
					-> (!Maybe SelectResult, !*channels, !*env) | ChannelEnv env
/*	accRChannels f channels
		applies a function on each channel in channels and returns a list which
		contains the result for each application.
	getRState
		applies available and eom on the channel which is identified by the Int
		parameter and returns SR_Available or SR_EOM or Nothing.
*/

class SelectSend channels where
	accSChannels	:: (TCP_SChannel -> *(.x, TCP_SChannel)) !*channels
					-> (![.x], !*channels)
	appDisconnected	:: !Int !*channels !*env
					-> (!Bool, !*channels, !*env) | ChannelEnv env
/*	accSChannels
		applies a function on each channel in channels and returns a list which
		contains the result for each application.
	appDisconnected
		returns whether disconnected is True for the channel which is identified by
		the Int parameter.
*/

class getNrOfChannels channels :: !*channels -> (!Int, !*channels)
/*	getNrOfChannels channels
		returns the number of channels in channels.
*/

instance SelectReceive TCP_RChannels,TCP_Listeners,TCP_RCharStreams,TCP_Void
instance SelectReceive (TCP_Pair *x *y)		| SelectReceive, getNrOfChannels x 
										& SelectReceive y
	
instance SelectSend TCP_SChannels,TCP_SCharStreams,TCP_Void
instance SelectSend (TCP_Pair *x *y)			| SelectSend, getNrOfChannels x
										& SelectSend y

instance getNrOfChannels TCP_RChannels,TCP_Listeners,TCP_RCharStreams,
						 TCP_SChannels,TCP_SCharStreams,TCP_Void
instance getNrOfChannels (TCP_Pair *x *y)	| getNrOfChannels x & getNrOfChannels y
