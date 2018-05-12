implementation module tcp_bytestreams

import	TCPDef

::	*TCP_RCharStream_ char
	=	{	rbs_rchan	::	!TCP_RChannel
		,	rbs_buffer	::	!{#Char}
		,	rbs_index	::	!Int	// >=0
		}
	// rbs_buffer.[rbs_index] is the next character to receive. If rbs_index>=size rbs_buffer, then a new
	// buffer has to be received from rbs_rchan. rbs_buffer can be an empty string.

::	*TCP_SCharStream_ char
	=	{	sbs_schan	::	!TCP_SChannel
		}
