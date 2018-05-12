definition module TCPEvent

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	StdEventTCP provides functions for using event driven TCP
//	Author: Martin Wierich
//	Modified: 15 October 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	TCPChannelClass, TCPDef
from	tcp_bytestreams import :: TCP_SCharStream_

class accSChannel ch	::	(TCP_SChannel -> (x, TCP_SChannel)) *(ch .a)
					 	->	(x, *(ch .a))
/*	This overloaded function supports the openSendNotifier function. It applies an
	access function on the underlying TCP_SChannel
*/

instance accSChannel TCP_SChannel_
instance accSChannel TCP_SCharStream_
