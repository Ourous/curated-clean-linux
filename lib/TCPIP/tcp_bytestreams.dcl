definition module tcp_bytestreams

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	Author: Martin Wierich
//	Modified: 7 September 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	TCPDef

::	*TCP_RCharStream_ char
	=	{	rbs_rchan	::	!TCP_RChannel
		,	rbs_buffer	::	!{#Char}
		,	rbs_index	::	!Int
		}

::	*TCP_SCharStream_ char
	=	{	sbs_schan	::	!TCP_SChannel
		}
