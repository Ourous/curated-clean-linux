definition module tcp

//	********************************************************************************
//	Clean Standard TCP library, version 1.2.2
//	
//	Author: Martin Wierich
//	Modified: 7 September 2001 for Clean 2.0 (Peter Achten)
//	********************************************************************************

import	StdString

class ChannelEnv env
where
	channelEnvKind	::	!*env	-> (!Int, !*env)
	mb_close_inet_receiver_without_id :: !Bool !(!Int, !Int) !*env -> *env
	/*
	::	!Bool  !(!EndpointRef, !InetReceiverCategory) !*env -> *env
	mb_close_inet_receiver_without_id:
		iff the Boolean is True, this function closes the receiver, which is identified through
		the (!EndpointRef, !InetReceiverCategory) pair
	*/
	channel_env_get_current_tick :: !*env -> (!Int, !*env)

//channelEnvKind can return the following values:
WORLD	:==	0
IOST	:==	1
PST		:==	2

IE_CONNECTREQUEST		:== 0x0001
IE_RECEIVED				:== 0x0004
IE_EOM					:== 0x0010
IE_SENDABLE				:== 0x0100
IE_DISCONNECTED			:== 0x0011
IE_IPADDRESSFOUND		:== 0x2000000F
IE_IPADDRESSNOTFOUND	:== 0x20000010
IE_ASYNCCONNECTCOMPLETE	:==	0x0002
IE_ASYNCCONNECTFAILED	:==	0x0003 

RCHANNEL				:==	0
LISTENER				:==	1

::	InetEvent	:== Int
::	EndpointRef	:==	Int
::	InetErrCode	:== Int


::	TCP_SChannel_ a
::	TCP_RChannel_ a
::	TCP_Listener_ a

::	 Buffered_SChan
		=	{	bEndpointRef	::	!EndpointRef
			,	bNormal			::	!Buffer
			,	bUnsent			::	!Int
			,	bId				::	!Int
			}
::	Buffer
		=	{	bPackets		::	![{#Char}]
			,	bBegin			::	!Int
			}		

::	IPAddress
			
:: InetReceiverCategory	:== Int	// with the following values

ListenerReceiver	:== 0
RChanReceiver		:== 1
SChanReceiver		:== 2
DNSReceiver			:== 3
ConnectReceiver		:== 4

pack_tcplistener	::	!EndpointRef			-> TCP_Listener_ .a
pack_tcpschan		::	!Buffered_SChan 		-> TCP_SChannel_ .a
pack_tcprchan		::	!(!EndpointRef,!Int) 	-> TCP_RChannel_ .a
pack_ipaddr			::	!Int					-> IPAddress

unpack_tcplistener	::	!.(TCP_Listener_ .a)	-> EndpointRef
unpack_tcpschan 	::	!.(TCP_SChannel_ .a) 	-> Buffered_SChan
unpack_tcprchan		::	!.(TCP_RChannel_ .a)	-> (!EndpointRef, !Int)
unpack_ipaddr		::	!IPAddress				-> Int

toDottedDecimal	::	!Int	->	String

lookupHost_syncC	::	!String !*env -> (!(!InetErrCode, !Int), !*env)

close_listener		:: !EndpointRef !*env	->	*env
close_tcprchan		:: !EndpointRef !*env	->	*env

openTCP_ListenerC	::	!Int !*env -> (!(!InetErrCode, !EndpointRef), !*env)
data_availableC		::	!EndpointRef !*env ->	(!Bool, !*env)
sendC				::	!EndpointRef !String !Int !Int !*env -> (!(!InetErrCode, !Int), !*env)
receiveC			::	!EndpointRef !Int !*env -> (!String, !*env)
acceptC				::	!EndpointRef !*env	->	(!(!InetErrCode, !Int, !EndpointRef),!*env) 
disconnectGracefulC	::	!EndpointRef !*env -> *env
disconnectBrutalC	::	!EndpointRef !*env -> *env		
garbageCollectEndpointC	::	!EndpointRef !*env -> *env

setEndpointData_no_new_notifiersC :: !EndpointRef !Int !Bool !Bool !Bool !*env -> *env
getEndpointDataC	::	!EndpointRef !*env -> (!(!Int, !Bool, !Bool, !Bool), !*env)
selectChC			::	!Int !Bool !(!Bool, !Int) !{#EndpointRef} !{#Int} !{#EndpointRef} !*env -> (!InetErrCode, !*env)
tcpPossibleC		::	!*env -> (!Bool, !*env)	
