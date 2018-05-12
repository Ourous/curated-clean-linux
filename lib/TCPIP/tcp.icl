implementation module tcp

import StdEnv
//from ostick import :: Tick

class ChannelEnv env
where
	channelEnvKind	::	!*env	-> (!Int, !*env)
	mb_close_inet_receiver_without_id :: !Bool !(!Int, !Int) !*env -> *env
	channel_env_get_current_tick :: !*env -> (!Int,	!*env)
	
//channelEnvKind can return the following values:
// (some C functions rely on these values)
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

::	 TCP_SChannel_ a		 =	TCP_SChannel_ Buffered_SChan
::	 TCP_RChannel_ a		 =	TCP_RChannel_ !(!EndpointRef, !Int)
		// the Int contains the maximum size, which is set via setMaxSize (StdSyncTCP)
		// this field is 0, if there is no limit
::	 TCP_Listener_ a	 = 	TCP_Listener_ !EndpointRef

::	 Buffered_SChan
		=	{	bEndpointRef	::	!EndpointRef
			,	bNormal			::	!Buffer
			,	bUnsent			::	!Int
			,	bId				::	!Int			// the toInt of a unique id for send channels. will be stored in ReceiverHandle
//			,	bId				::	!Id				// a unique id for send channels. will be strored in ReceiverHandle
			}
::	Buffer
		=	{	bPackets		::	![{#Char}]		// no element has size 0
			,	bBegin			::	!Int			// bBegin bytesfrom (hd bPackets) are sent, the rest is unsent
													// (bPackets==[]) => (bBegin==0)
													// bBegin<size (hd bPackets)
			}		
			
::	IPAddress			:== Int

:: InetReceiverCategory	:== Int	// with the following values (some C functions rely on these values)
ListenerReceiver	:== 0
RChanReceiver		:== 1
SChanReceiver		:== 2
DNSReceiver			:== 3
ConnectReceiver		:== 4

pack_tcplistener :: !EndpointRef -> TCP_Listener_ .a
pack_tcplistener endpointRef
	= TCP_Listener_ endpointRef
	
pack_tcpschan :: !Buffered_SChan -> TCP_SChannel_ .a
pack_tcpschan buffered_SChan
	= TCP_SChannel_ buffered_SChan

pack_tcprchan :: !(!EndpointRef,!Int) -> TCP_RChannel_ .a
pack_tcprchan x
	= TCP_RChannel_ x

pack_ipaddr :: !Int -> IPAddress
pack_ipaddr i = i

unpack_tcplistener :: !.(TCP_Listener_ .a) -> EndpointRef
unpack_tcplistener (TCP_Listener_ endpointRef)
	= endpointRef

unpack_tcpschan :: !.(TCP_SChannel_ .a) -> Buffered_SChan
unpack_tcpschan (TCP_SChannel_ buffered_SChan)
	= buffered_SChan

unpack_tcprchan :: !.(TCP_RChannel_ .a) -> (!EndpointRef, !Int)
unpack_tcprchan (TCP_RChannel_ x)
	= x
	
unpack_ipaddr :: !IPAddress -> Int
unpack_ipaddr i = i

close_listener :: !EndpointRef !*env -> *env
close_listener endpointRef env
	#	env	= setEndpointData_no_new_notifiersC endpointRef 0 False False True env
		env	= garbageCollectEndpointC endpointRef env
	= env

close_tcprchan :: !EndpointRef !*env -> *env
close_tcprchan endpointRef env
	#	((referenceCount,_,hs,aborted),env)
						= getEndpointDataC endpointRef env
		env = setEndpointData_no_new_notifiersC endpointRef (dec referenceCount) False hs aborted env
		env	= case (referenceCount, aborted) of
					(1, False)	-> disconnectGracefulC endpointRef env
					(1, _)		-> disconnectBrutalC endpointRef env
					_			-> env
		env	= garbageCollectEndpointC endpointRef env
	= env

toDottedDecimal :: !Int -> String
toDottedDecimal ip
	=	(toString ((ip>>24) bitand 255))	+++"."+++
		(toString ((ip>>16) bitand 255))	+++"."+++
		(toString ((ip>> 8) bitand 255))	+++"."+++
		(toString ( ip      bitand 255))

///////////////////////////// low level stuff //////////////////////

lookupHost_syncC :: !String !*env -> (!(!InetErrCode, !Int), !*env)
// returns ip address in host order; string can be in aplhanumerical or dotted decimal form; (null terminated)
// error code: 0 ok, otherwise error (also: addr doesn't exist)
lookupHost_syncC _ _
	= code {
		ccall lookupHost_syncC "S:VII:A"
	}

openTCP_ListenerC :: !Int !*env -> (!(!InetErrCode, !EndpointRef), !*env)
// installs a Listener. first param: portnum (host order); errCode: 0:ok;	otherwise:not ok
// also adds a new dictionary item with values (referencecount=1, hasSNotif=False, hasRNotif=False, aborted=False)
openTCP_ListenerC portNum e
	= IF_INT_64_OR_32 (openTCP_ListenerC64 portNum e) (openTCP_ListenerC32 portNum e);

openTCP_ListenerC64 :: !Int !*env -> (!(!InetErrCode, !EndpointRef), !*env)
openTCP_ListenerC64 portNum e
	= code inline {
		ccall openTCP_ListenerC "I:VII:A"
	}

openTCP_ListenerC32 :: !Int !*env -> (!(!InetErrCode, !EndpointRef), !*env)
openTCP_ListenerC32 portNum e
	= code inline {
		ccall openTCP_ListenerC "I:VII:A"
	}

data_availableC :: !EndpointRef !*env -> (!Bool, !*env)
// returns whether data is available
data_availableC er env
	# (avail,env) = IF_INT_64_OR_32 (data_availableC64 er env) (data_availableC32 er env);
	= (avail <> 0, env)
where
	data_availableC64 :: !EndpointRef !*env -> (!Int, !*env)
	data_availableC64 _ _
		= code {
			ccall data_availableC "p:I:A"
		}

	data_availableC32 :: !EndpointRef !*env -> (!Int, !*env)
	data_availableC32 _ _
		= code {
			ccall data_availableC "I:I:A"
		}

sendC :: !EndpointRef !String !Int !Int !*env -> (!(!InetErrCode, !Int), !*env)
/*	sendC epr data begin nBytes env 
		sends non blocking (data % (begin, begin+nBytes-1)) via the endpoint. 
		(isIOProg<>0)<=>evaluation happens within startS(N)(M)DI
		returns number of sent bytes (0 if errCode<>0)
		errCode: 0=ok, otherwise not ok
*/
sendC endpointRef data begin nBytes e
	= IF_INT_64_OR_32 (sendC64 endpointRef data begin nBytes e) (sendC32 endpointRef data begin nBytes e);

sendC64 :: !EndpointRef !String !Int !Int !*env -> (!(!InetErrCode, !Int), !*env)
sendC64 endpointRef data begin nBytes e
	= code inline {
		ccall sendC "pSII:VII:A"
	}

sendC32 :: !EndpointRef !String !Int !Int !*env -> (!(!InetErrCode, !Int), !*env)
sendC32 endpointRef data begin nBytes e
	= code inline {
		ccall sendC "ISII:VII:A"
	}

receiveC :: !EndpointRef !Int !*env -> (!String, !*env)
// receiveC endpointRef maxBytes: receive maximal maxBytes bytes on endpoint endpointRef
receiveC endpointRef maxSize e
	= IF_INT_64_OR_32 (receiveC64 endpointRef maxSize e) (receiveC32 endpointRef maxSize e);

receiveC64 :: !EndpointRef !Int !*env -> (!String, !*env)
receiveC64 endpointRef maxSize e
	= code {
		ccall receiveC "pI:VS:A"
	}

receiveC32 :: !EndpointRef !Int !*env -> (!String, !*env)
receiveC32 endpointRef maxSize e
	= code {
		ccall receiveC "II:VS:A"
	}

acceptC :: !EndpointRef !*env -> (!(!InetErrCode, !Int, !EndpointRef),!*env) 
// accept connection request on a listener, yielding the ip adress of the remote side in host order and
// a new endpointRef
// also adds a new dictionary item with values (referencecount=2, hasSNotif=False, hasRNotif=False, aborted=False)
// error code: 0: ok, otherwise: not ok
acceptC listener e
	= IF_INT_64_OR_32 (acceptC64 listener e) (acceptC32 listener e);

acceptC64 :: !EndpointRef !*env -> (!(!InetErrCode, !Int, !EndpointRef),!*env) 
acceptC64 listener e
	= code inline {
		ccall acceptC "p:VIII:A"
	}

acceptC32 :: !EndpointRef !*env -> (!(!InetErrCode, !Int, !EndpointRef),!*env) 
acceptC32 listener e
	= code inline {
		ccall acceptC "I:VIII:A"
	}

disconnectGracefulC	::	!EndpointRef !*env -> *env		
// disconnect graceful !
disconnectGracefulC endpointRef e
	= IF_INT_64_OR_32 (disconnectGracefulC64 endpointRef e) (disconnectGracefulC32 endpointRef e);

disconnectGracefulC64 :: !EndpointRef !*env -> *env		
disconnectGracefulC64 endpointRef e
	= code inline {
		ccall disconnectGracefulC "p:V:A"
	}

disconnectGracefulC32 :: !EndpointRef !*env -> *env		
disconnectGracefulC32 endpointRef e
	= code inline {
		ccall disconnectGracefulC "I:V:A"
	}

disconnectBrutalC :: !EndpointRef !*env -> *env		
// disconnect brutal !
disconnectBrutalC endpointRef e
	= IF_INT_64_OR_32 (disconnectBrutalC64 endpointRef e) (disconnectBrutalC32 endpointRef e);

disconnectBrutalC64 :: !EndpointRef !*env -> *env		
disconnectBrutalC64 endpointRef e
	= code inline {
		ccall disconnectBrutalC "p:V:A"
	}

disconnectBrutalC32 :: !EndpointRef !*env -> *env		
disconnectBrutalC32 endpointRef e
	= code inline {
		ccall disconnectBrutalC "I:V:A"
	}

garbageCollectEndpointC :: !EndpointRef !*env -> *env
// returns resources back to the system. uses reference count of dictionary item
garbageCollectEndpointC endpointRef e
	= IF_INT_64_OR_32 (garbageCollectEndpointC64 endpointRef e) (garbageCollectEndpointC32 endpointRef e);

garbageCollectEndpointC64 :: !EndpointRef !*env -> *env
garbageCollectEndpointC64 endpointRef e
	= code inline {
		ccall garbageCollectEndpointC "p:V:A"
	}

garbageCollectEndpointC32 :: !EndpointRef !*env -> *env
garbageCollectEndpointC32 endpointRef e
	= code inline {
		ccall garbageCollectEndpointC "I:V:A"
	}

// endpoint dictionary functions

setEndpointData_no_new_notifiersC :: !EndpointRef !Int !Bool !Bool !Bool !*env -> *env
// set the endpointRef data. parameters: endpointRef referenceCount hasReceiveNotifier hasSendableNotifier aborted
// the values of hasReceiveNotifier and hasSendableNotifier also have an effect on the set of internet events,
// that will reach Clean
// if the item is already deallocated by the C side, nothing will happen
setEndpointData_no_new_notifiersC endpointRef referenceCount hasReceiveNotifier hasSendableNotifier aborted e
	= IF_INT_64_OR_32
		(setEndpointData_no_new_notifiersC64 endpointRef referenceCount hasReceiveNotifier hasSendableNotifier aborted e)
		(setEndpointData_no_new_notifiersC32 endpointRef referenceCount hasReceiveNotifier hasSendableNotifier aborted e);

setEndpointData_no_new_notifiersC64 :: !EndpointRef !Int !Bool !Bool !Bool !*env -> *env
setEndpointData_no_new_notifiersC64 endpointRef referenceCount hasReceiveNotifier hasSendableNotifier aborted e
	= code inline {
		ccall setEndpointData_no_new_notifiersC "pIIII:V:A"
	}

setEndpointData_no_new_notifiersC32 :: !EndpointRef !Int !Bool !Bool !Bool !*env -> *env
setEndpointData_no_new_notifiersC32 endpointRef referenceCount hasReceiveNotifier hasSendableNotifier aborted e
	= code inline {
		ccall setEndpointData_no_new_notifiersC "IIIII:V:A"
	}

getEndpointDataC :: !EndpointRef !*env -> (!(!Int, !Bool, !Bool, !Bool), !*env)
// get the endpointRef data. result: referenceCount hasReceiveNotifier hasSendableNotifier aborted
// if the item is already deallocated by the C side, then the returned values are undefined
getEndpointDataC er env
	# ((a,b,c,d),env) = IF_INT_64_OR_32 (getEndpointDataC64 er env) (getEndpointDataC32 er env);
	= ((a,b<>0,c<>0,d<>0),env)
where
	getEndpointDataC64 :: !EndpointRef !*env -> (!(!Int, !Int, !Int, !Int), !*env)
	getEndpointDataC64 endpointRef e 
		= code {
			ccall getEndpointDataC "p:VIIII:A"
		}

	getEndpointDataC32 :: !EndpointRef !*env -> (!(!Int, !Int, !Int, !Int), !*env)
	getEndpointDataC32 endpointRef e 
		= code {
			ccall getEndpointDataC "I:VIIII:A"
		}

selectChC :: !Int !Bool !(!Bool, !Int) !{#EndpointRef} !{#Int} !{#EndpointRef} !*env -> (!InetErrCode, !*env)
/* selectChC isIOProg nonBlocking (doTimeout, stopTime) rcvEndpoints rcvKinds sndEndpoints
   calls the sockets select function. HAS A SIDEEFFECT: the contents of rcvEndpoints and sndEndpoints will
   be updated.
		(isIOProg<>0)<=>evaluation happens within startS(N)(M)DI
		(nonBlocking, doTimeout, stopTime)
				==	(True, True,_):			a timeout of zero should be used
					(False, True, _):	timeout should expire, when the system time in ticks reaches stopTime
					(False, False, _):	timeout should never expire
		rcvEndpoints: endpoints that are checked for readability or eom 
					  (listening endpoints or endpoints for receive channels)
		rcvKinds	: case rcvKinds.[i] of
						RCHANNEL	->	"rcvEndpoints.[i] is an endpoint for a receive channel"
						LISTENER	->	"rcvEndpoints.[i] is a listening endpoint"
					 (size rcvKinds)==(size rcvEndpoints)
		sndEndpoints: endpoints that are checked for sendability or disconnected
		result error code: 0=ok; 1=timeout expired, 3=other errors
		
	after function execution those elements of rcvEndpoints will be set to 0 which are readable or "eom"
	after function execution those elements of sndEndpoints will be set to 0 which are sendable or "disconnected"
*/
selectChC justForMac nonBlocking doTimeout_and_stopTime pRChannels justForMac2 pSChannels env
	= code {
		ccall selectChC "IIIIAAA:VI:A"
	}

tcpPossibleC :: !*env -> (!Bool, !*env)	
// whether tcp can be started up
tcpPossibleC env
	# (res,env)	= tcpPossibleC env
	= (res<>0,env)
where
	tcpPossibleC		::	!*env -> (!Int, !*env)	
	tcpPossibleC _
		= code {
			ccall tcpPossibleC ":I:A"
		}
