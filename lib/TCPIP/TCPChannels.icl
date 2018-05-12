implementation module TCPChannels

import	StdEnv
import	TCPDef, TCPChannelClass
import	tcp, ostcp, tcp_bytestreams

:: TCP_Void = TCP_Void
:: TCP_Pair a b = TCP_Pair a b

//////////////////////// Listeners ////////////////////////////////////

instance Receive	TCP_Listener_
  where
	receive_MT mbTimeout ch env
		# endpointRef					= unpack_tcplistener ch
		  (toReport, mbHostDuplexChan, env)	= receive_mb_Listener mbTimeout endpointRef env
		= (toReport, Cast mbHostDuplexChan, pack_tcplistener endpointRef, env)
	receiveUpTo max ch env
		= receiveUpToGeneral [] max ch env
	available ch env
		# endpointRef			= unpack_tcplistener ch
		  (cReqAvail, env)		= os_connectrequestavailable endpointRef env
		= (cReqAvail, pack_tcplistener endpointRef, env)
	eom ch env
		= (False, ch, env)

receiveUpToGeneral akku max ch env
	|	max<=0
		= (u_reverse akku, ch, env)
	#!	(tReport, mbData, ch, env)	= receive_MT (Just 0) ch env
	|	tReport<>TR_Success
		= (u_reverse akku, ch, env)
	= receiveUpToGeneral [fromJust mbData:akku] (dec max) ch env

instance closeRChannel	TCP_Listener_
  where
	closeRChannel ch env
		# endpointRef			= unpack_tcplistener ch
		= close_listener endpointRef env

receive_mb_Listener	::	!(Maybe Timeout) !EndpointRef !*env
						->	(!TimeoutReport, !Maybe (!IPAddress, !TCP_DuplexChannel), !*env) | ChannelEnv env
receive_mb_Listener mbTimeout endpointRef env
	|	isJust mbTimeout && (fromJust mbTimeout)<0
		= (TR_Expired, Nothing, env)
	#!	(cReqAvail, env)		= os_connectrequestavailable endpointRef env
	= if cReqAvail (accept endpointRef env) (wait endpointRef env)
  where
	wait endpointRef env
		#!	(isIOProg, env)		= channelEnvKind env
			(mbStopTime, env)	= getMbStopTime mbTimeout env
			(errCode, env)		= selectChC isIOProg False mbStopTime {endpointRef} {LISTENER} {} env
		| errCode<>0	// the timeout expired (or some fatal error)
			= (TR_Expired, Nothing, env)
		= accept endpointRef env
	accept endpointRef env
		# ((errCode,host,newEpRef), env)
					= acceptC endpointRef env	
		| errCode<>0
			= (TR_NoSuccess, Nothing, env)
		# (duplexChan, env)		= createDuplexChan newEpRef env
		= (TR_Success, Just (pack_ipaddr host, duplexChan), env)

		
/////////////////////// TCP channels /////////////////////////////////

instance Receive	TCP_RChannel_
  where
	receive_MT mbTimeout ch env
		# rchan						= unpack_tcprchan ch
		  (toReport, mbByteSeq, env)= receive_mb_TCP mbTimeout rchan env
		= (toReport, Cast mbByteSeq, pack_tcprchan rchan, env)
	receiveUpTo max ch env
		= receiveUpToGeneral [] max ch env
	available ch env
		# (rchan=:(endpointRef,_))	= unpack_tcprchan ch
		  (isAvailable, env)		= data_availableC endpointRef env
		= (isAvailable, pack_tcprchan rchan, env)
	eom ch env
		# (rchan=:(endpointRef,_))	= unpack_tcprchan ch
		  (isEom, env)				= os_eom endpointRef env
		= (isEom, pack_tcprchan rchan, env)

instance closeRChannel	TCP_RChannel_
  where
	closeRChannel ch env
		# (endpointRef, _)			= unpack_tcprchan ch
		= close_tcprchan endpointRef env

receive_mb_TCP ::	!(Maybe Timeout) !(!EndpointRef,!Int) !*env 
					->	(!TimeoutReport, !Maybe ByteSeq, !*env)
					| 	ChannelEnv env
receive_mb_TCP mbTimeout rchan=:(endpointRef,maxSize) env
	|	isJust mbTimeout && (fromJust mbTimeout)<0
		= (TR_Expired, Nothing, env)
	#!	(receivedData, env)	= receiveC endpointRef maxSize env	
	|	size receivedData>0
		= (TR_Success, Just (toByteSeq receivedData), env)
  	#!	(mbStopTime, env)	= getMbStopTime mbTimeout env
		(isIOProg, env)		= channelEnvKind env
		(errCode, env) = selectChC isIOProg False mbStopTime {endpointRef} {RCHANNEL} {} env
	|	errCode	== 1	// timeout expired
		= (TR_Expired, Nothing, env)
	|	errCode == 3	// some error
		= (TR_NoSuccess, Nothing, env)
	#!	(receivedData, env)	= receiveC endpointRef maxSize env	
	|	size receivedData>0
		= (TR_Success, Just (toByteSeq receivedData), env)
	= (TR_NoSuccess, Nothing, env)

instance Send		TCP_SChannel_
  where
	send_MT mbTimeout byteSeqPolymorphic ch env
		= nsend_MT mbTimeout [byteSeqPolymorphic] ch env
	nsend_MT mbTimeout byteSeqsPolymorphic ch env
		#!	byteSeqs			= castToByteSeqList byteSeqsPolymorphic
			chan				= unpack_tcpschan ch
			chan				= seq (map addToBuffer byteSeqs) chan
			(toExpired, sentBytes, bNormal, env)
								= flushBuffers	mbTimeout chan.bEndpointRef chan.bNormal env
			chan				= { chan & 	bNormal=bNormal, bUnsent=chan.bUnsent-sentBytes}
		= (toExpired, sentBytes, pack_tcpschan chan, env)
	flushBuffer_MT mbTimeout ch env 
		#!	chan						= unpack_tcpschan ch
			(toExpired, flushedBytes, bNormal, env)	= flushBuffers mbTimeout chan.bEndpointRef chan.bNormal env
		= (toExpired, flushedBytes, pack_tcpschan { chan & bNormal =bNormal, bUnsent=chan.bUnsent-flushedBytes}, env)
	closeChannel_MT mbTimeout ch env
		#!	chan				= unpack_tcpschan ch
			endpointRef			= chan.bEndpointRef
			(toExpired, sentBytes, _, env)
								= flushBuffers mbTimeout endpointRef chan.bNormal env
			((referenceCount,hr,hs,aborted),env)
							= getEndpointDataC endpointRef env
			env				= mb_close_inet_receiver_without_id	hs (endpointRef, SChanReceiver) env
			env = setEndpointData_no_new_notifiersC endpointRef (dec referenceCount) hr False aborted env
			env	= case referenceCount of
					1	->	disconnectGracefulC endpointRef env
					_	->	env
			env	= garbageCollectEndpointC endpointRef env
		= (toExpired, sentBytes, env)
	abortConnection ch env
		#	chan				= unpack_tcpschan ch
			endpointRef			= chan.bEndpointRef
			((referenceCount,hr,hs,_),env)
							= getEndpointDataC endpointRef env
			env				= mb_close_inet_receiver_without_id	hs (endpointRef, SChanReceiver) env
			env = setEndpointData_no_new_notifiersC endpointRef (dec referenceCount) hr False True env
			env	= case referenceCount of
						1	-> disconnectBrutalC endpointRef env
						_	-> env
			env	= garbageCollectEndpointC endpointRef env
		= env
	disconnected ch env
		# chan	= unpack_tcpschan ch
		  (isDisconnected, env)	= os_disconnected chan.bEndpointRef env
		= (isDisconnected, pack_tcpschan chan, env)
	bufferSize ch
		# chan				= unpack_tcpschan ch
		= (chan.bUnsent, pack_tcpschan chan)


addToBuffer :: !ByteSeq !u:Buffered_SChan -> v:Buffered_SChan, [u <= v]
addToBuffer byteSeq chan
	# data		= toString byteSeq
	| size data==0
		= chan 
	= { chan & bNormal=addPacket data chan.bNormal, bUnsent=chan.bUnsent+(size data)}
  where
	addPacket data {bPackets=[]}
		= { bPackets=[data], bBegin=0 }
	addPacket data buffer=:{bPackets}
		= { buffer & bPackets=bPackets++[data] }

flushBuffers mbTimeout endpointRef buffer env
	#!	(isIOProg,env)		= channelEnvKind env
		(mbStopTime, env)	= getMbStopTime mbTimeout env
		nonBlocking			= isJust mbTimeout && fromJust mbTimeout==0
	= flushBuffers_Loop nonBlocking mbStopTime isIOProg endpointRef buffer 0 env
	
flushBuffers_Loop	::	!Bool !(!Bool, !Int) !Int !EndpointRef !Buffer !Int !*env
				->	(!TimeoutReport, !Int, !Buffer, !*env)
				|	ChannelEnv env
flushBuffers_Loop _ _ _ _ buffer=:{bPackets=[]} akku env
	= (TR_Success, akku, buffer, env)
flushBuffers_Loop	nonBlocking mbStopTime isIOProg endpointRef 
				buffer=:{bPackets=bPackets=:[first_packet:packets], bBegin} akku env
	#!	(isDisconnected, env)	=  os_disconnected endpointRef env
	|	isDisconnected
		= (TR_NoSuccess, akku, buffer, env)
	#!	bytesToSend				= (size first_packet) - bBegin
		((errCode, bytesSent), env)		= sendC endpointRef first_packet bBegin bytesToSend env
	|	errCode<>0
		= (TR_NoSuccess, akku, buffer, env)
	|	bytesSent==bytesToSend
		= flushBuffers_Loop nonBlocking mbStopTime isIOProg endpointRef {bPackets=packets, bBegin=0} (bytesSent+akku) env
	|	nonBlocking
		= (TR_Expired, bytesSent+akku, {buffer & bBegin=bBegin+bytesSent}, env)
	#!	(errCode, env)			= selectChC isIOProg False mbStopTime {} {} {endpointRef} env
	|	errCode	== 1	// timeout expired
		= (TR_Expired, bytesSent+akku, {buffer & bBegin=bBegin+bytesSent}, env)
	|	errCode == 3	// some error
		= (TR_NoSuccess, bytesSent+akku, {buffer & bBegin=bBegin+bytesSent}, env)
	= flushBuffers_Loop	nonBlocking mbStopTime isIOProg endpointRef {buffer & bBegin=bBegin+bytesSent} 
						(bytesSent+akku) env
lookupIPAddress		::	!String !*env
				->	(!Maybe IPAddress, !*env)
				|	ChannelEnv env
lookupIPAddress inetAddr env
	# ((errCode, inetHost), env)	= lookupHost_syncC (inetAddr+++"\0") env
	| errCode<>0
		= (Nothing, env)
	= (Just (pack_ipaddr inetHost), env)

connectTCP_MT		::	!(Maybe Timeout) !(!IPAddress,!Port) !*env
					->	(!TimeoutReport, !Maybe TCP_DuplexChannel, !*env) 
					|	ChannelEnv env
connectTCP_MT mbTimeout (inetHost,inetPort) env
	#!	destination						= (unpack_ipaddr inetHost, inetPort)
		(chanEnvKind,env)				= channelEnvKind env
		(mbStopTime, env)				= getMbStopTime mbTimeout env
		((errCode,timeoutExpired,endpointRef), env)
			= os_connectTCP_sync chanEnvKind mbStopTime destination env
	| timeoutExpired
		= (TR_Expired, Nothing, env)
	| errCode<>0
		= (TR_NoSuccess, Nothing, env)
	#!	(duplexChan, env)					= createDuplexChan endpointRef env
	= (TR_Success, Just duplexChan, env)

openTCP_Listener	::	!Port !*env
					->	(!Bool, !Maybe TCP_Listener, !*env)
					|	ChannelEnv env
openTCP_Listener port env
	# ((errCode,endpointRef), env)		= openTCP_ListenerC port env
	| errCode<>0
		= (False, Nothing, env)
	= (True, Just (pack_tcplistener endpointRef), env)
	
instance MaxSize TCP_RChannel_
  where
	setMaxSize newMaxLength tcp_rchan
		# (endpointRef,_)	= unpack_tcprchan tcp_rchan
		= pack_tcprchan (endpointRef, max newMaxLength 0)
	getMaxSize tcp_rchan
		# (ch=:(_,maxSize))	= unpack_tcprchan tcp_rchan
		= (maxSize, pack_tcprchan ch)
	clearMaxSize tcp_rchan
		# (endpointRef,_)	= unpack_tcprchan tcp_rchan
		= pack_tcprchan (endpointRef, 0)

//////////////// helpers //////////////////////////

createDuplexChan	::	!EndpointRef !*env	->	(!TCP_DuplexChannel, !*env)	| ChannelEnv env
createDuplexChan endpointRef env
//	# (id, env)	= openId env
	# tcp_SChan	=	{	bEndpointRef	=	endpointRef
					,	bNormal			=	emptyBuffer
					,	bUnsent			=	0
					,	bId				=	0 // id
					}
	= ({sChannel= pack_tcpschan tcp_SChan, rChannel=pack_tcprchan (endpointRef, 0)}, env)

emptyBuffer = {bPackets=[], bBegin=0}

/*	Conversion functions:
	Cast contains abc code because it can't be typed conventionally. (JVG/RWS)
*/

castToByteSeqList :: !.a -> [ByteSeq]
castToByteSeqList x
	= Cast x

castToChList :: !.a -> .[Char]
castToChList x
	= Cast x

castToChar :: !.a -> .Char
castToChar x
	= Cast x

Cast :: !.a -> .b
Cast a
	= code
		{
			pop_a 0
		}

u_reverse::![.a] -> [.a]
u_reverse list = reverse_ list []
where 
	reverse_ [hd:tl] list	= reverse_ tl [hd:list]
	reverse_ [] list		= list

u_length l
	= u_length_ l [] 0
  where
	u_length_ [] akku n
		= (n, u_reverse akku)
	u_length_ [h:t] akku n
		= u_length_ t [h:akku] (inc n)
	
///////////////////// for selecting on channels ///////////////////////

class SelectReceive channels
  where
	accRChannels	:: (PrimitiveRChannel -> (x, PrimitiveRChannel)) !*channels 
					-> (![x], !*channels)
	getRState		:: !Int !*channels !*env
					-> (!Maybe SelectResult, !*channels, !*env) | ChannelEnv env

class SelectSend channels
  where
	accSChannels	:: (TCP_SChannel -> *(.x, TCP_SChannel)) !*channels
					-> (![.x], !*channels)
	appDisconnected	:: !Int !*channels !*env
					-> (!Bool, !*channels, !*env) | ChannelEnv env

class getNrOfChannels channels	:: !*channels
								-> (!Int, !*channels)

instance SelectSend (TCP_Pair *x *y)			| SelectSend, getNrOfChannels x & SelectSend y
  where
	accSChannels f (TCP_Pair l_channels r_channels)
		#!	(l, l_channels)	= accSChannels f l_channels
			(r, r_channels)	= accSChannels f r_channels
		= (l++r, TCP_Pair l_channels r_channels)
	appDisconnected n (TCP_Pair l_channels r_channels) env
		#!	(l_length, l_channels)	= getNrOfChannels l_channels
		|	n<l_length
			#!	(result, l_channels, env)	= appDisconnected n l_channels env
			= (result, TCP_Pair l_channels r_channels, env)
		#!	(result, r_channels, env)	= appDisconnected (n-l_length) r_channels env
		= (result, TCP_Pair l_channels r_channels, env)

instance SelectSend TCP_SChannels
  where
	accSChannels f (TCP_SChannels channels)
		#!	(l,channels)	=  unzip (map f channels)
		= (l, TCP_SChannels channels)
	appDisconnected n (TCP_SChannels channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(isDisconnected, channel, env)	= disconnected channel env
		= (isDisconnected, TCP_SChannels (l ++ [channel:r]), env)

instance SelectSend TCP_SCharStreams
  where
	accSChannels f (TCP_SCharStreams channels)
		= accSChannelsA f channels []
	  where
		accSChannelsA f [] akku
			#! (l, channels)	= unzip (u_reverse akku)
			= (l, TCP_SCharStreams channels)
		accSChannelsA f [{sbs_schan}:channels] akku
			#!	(x, sbs_schan)	= f sbs_schan
			= accSChannelsA f channels [(x,{sbs_schan=sbs_schan}): akku]
	appDisconnected n (TCP_SCharStreams channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(isDisconnected, channel, env)	= disconnected channel env
		= (isDisconnected, TCP_SCharStreams (l ++ [channel:r]), env)

instance SelectSend TCP_Void
  where
	accSChannels _ void
		= ([],void)
	appDisconnected _ _ _
		= abort "StdTCPChannels: error: tried to apply appDisconnected to an object of type TCP_Void"

instance SelectReceive (TCP_Pair *x *y)			| SelectReceive, getNrOfChannels x & SelectReceive y
  where
	accRChannels f (TCP_Pair l_channels r_channels)
		#!	(l, l_channels)	= accRChannels f l_channels
			(r, r_channels)	= accRChannels f r_channels
		= (l++r, TCP_Pair l_channels r_channels)
	getRState n (TCP_Pair l_channels r_channels) env
		#!	(l_length, l_channels)	= getNrOfChannels l_channels
		|	n<l_length
			#!	(result, l_channels, env)	= getRState n l_channels env
			= (result, TCP_Pair l_channels r_channels, env)
		#!	(result, r_channels, env)	= getRState (n-l_length) r_channels env
		= (result, TCP_Pair l_channels r_channels, env)

instance SelectReceive TCP_RChannels
  where
	accRChannels f (TCP_RChannels channels)
		#!	(l,channels)	=  unzip (map f (map TCP_RCHANNEL channels))
		= (l, TCP_RChannels (map fromTCP_RCHANNEL channels))
	  where
		fromTCP_RCHANNEL (TCP_RCHANNEL channel)
			= channel
	getRState n (TCP_RChannels channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(state, channel, env)	= getStateRchan channel env
		= (state, TCP_RChannels (l ++ [channel:r]), env)

instance SelectReceive TCP_Listeners
  where
	accRChannels f (TCP_Listeners channels)
		#!	(l,channels)	=  unzip (map f (map TCP_LISTENER channels))
		= (l, TCP_Listeners (map fromTCP_LISTENER channels))
	  where
		fromTCP_LISTENER (TCP_LISTENER channel)
			= channel
	getRState n (TCP_Listeners channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(state, channel, env)	= getState channel env
		= (state, TCP_Listeners (l ++ [channel:r]), env)
	  where
		getState listener env
			#!	endpointRef				= unpack_tcplistener listener
				(cReqAvail, env)		= os_connectrequestavailable endpointRef env
			= ( if cReqAvail (Just SR_Available) Nothing,
				pack_tcplistener endpointRef,
				env)

instance SelectReceive TCP_RCharStreams
  where
	accRChannels f (TCP_RCharStreams channels)
		= accRChannelsA f channels []
	  where
		accRChannelsA f [] akku
			#! (l, channels)	= unzip (u_reverse akku)
			= (l, TCP_RCharStreams channels)
		accRChannelsA f [rbs=:{rbs_rchan}:channels] akku
			#!	(x, TCP_RCHANNEL rbs_rchan)	= f (TCP_RCHANNEL rbs_rchan)
			= accRChannelsA f channels [(x,{rbs & rbs_rchan=rbs_rchan}): akku]
	getRState n (TCP_RCharStreams channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(state, channel, env)	= getStateRchan channel env
		= (state, TCP_RCharStreams (l ++ [channel:r]), env)

instance SelectReceive TCP_Void
  where
	accRChannels _ void
		= ([],void)
	getRState _ void env
		= (Nothing, void, env)
		
getStateRchan channel env
	#!	(isAvailable, channel, env) = available channel env
	|	isAvailable
		= (Just SR_Available, channel, env)
	#!	(isEom, channel, env) = eom channel env
	|	isEom
		= (Just SR_EOM, channel, env)
	= (Nothing, channel, env)

instance getNrOfChannels (TCP_Pair *x *y)			| getNrOfChannels x & getNrOfChannels y
  where
	getNrOfChannels (TCP_Pair l r)
		#!	(nl, l)	= 	getNrOfChannels l
			(nr, r)	= 	getNrOfChannels r
		= (nl+nr, TCP_Pair l r)
instance getNrOfChannels TCP_RChannels
  where
	getNrOfChannels (TCP_RChannels channels)
		#!	(n, channels)	= u_length channels
		= (n, TCP_RChannels channels)
instance getNrOfChannels TCP_Listeners
  where
	getNrOfChannels (TCP_Listeners channels)
		#!	(n, channels)	= u_length channels
		= (n, TCP_Listeners channels)
instance getNrOfChannels TCP_RCharStreams
  where
	getNrOfChannels (TCP_RCharStreams channels)
		#!	(n, channels)	= u_length channels
		= (n, TCP_RCharStreams channels)
instance getNrOfChannels TCP_SChannels
  where
	getNrOfChannels (TCP_SChannels channels)
		#!	(n, channels)	= u_length channels
		= (n, TCP_SChannels channels)
instance getNrOfChannels TCP_SCharStreams
  where
	getNrOfChannels (TCP_SCharStreams channels)
		#!	(n, channels)	= u_length channels
		= (n, TCP_SCharStreams channels)
instance getNrOfChannels TCP_Void
  where
	getNrOfChannels void
		= (0, void)

instance == SelectResult
  where
	(==) SR_Available x		= case x of	SR_Available	-> True
										_				-> False
	(==) SR_EOM x			= case x of	SR_EOM			-> True
										_				-> False
	(==) SR_Sendable x		= case x of	SR_Sendable 	-> True
										_				-> False
	(==) SR_Disconnected x	= case x of	SR_Disconnected	-> True
										_				-> False

instance toString SelectResult
  where
  	toString SR_Available		= "SR_Available"
  	toString SR_EOM				= "SR_EOM"
  	toString SR_Sendable		= "SR_Sendable"
  	toString SR_Disconnected	= "SR_Disconnected"

selectChannel_MT		:: !(Maybe Timeout) !*r_channels !*s_channels !*env
					-> (![(!Int, !SelectResult)], !*r_channels, !*s_channels, !*env) 
					|	SelectReceive r_channels & SelectSend s_channels & ChannelEnv env
selectChannel_MT mbTimeout r_channels s_channels env
	|	isJust mbTimeout && fromJust mbTimeout<0
		= ([], r_channels, s_channels, env)
	#!	(mbStopTime, env) 	= getMbStopTime mbTimeout env
		nonBlocking			= isJust mbTimeout && fromJust mbTimeout==0
		(rcvPairs,r_channels) = accRChannels getREndpointRef r_channels
		(sndEndpoints,s_channels) = accSChannels getSEndpointRef s_channels
	|	isEmpty rcvPairs && isEmpty sndEndpoints
		= abort "StdTCPChannels: error: selectChannel called with zero channels"
	#!	(rcvEndpoints, rcvKinds)	= unzip rcvPairs
	= selectLoop nonBlocking mbStopTime rcvEndpoints { kind \\ kind <- rcvKinds } sndEndpoints r_channels s_channels env
  where
	getREndpointRef (TCP_RCHANNEL tcp_RChan)
		#!	(rchan=:(endpointRef,_))	= unpack_tcprchan tcp_RChan
		= ((endpointRef,RCHANNEL),TCP_RCHANNEL (pack_tcprchan rchan))
	getREndpointRef (TCP_LISTENER tcp_Listener)
		#!	endpointRef	= unpack_tcplistener tcp_Listener
		= ((endpointRef, LISTENER),TCP_LISTENER (pack_tcplistener endpointRef))
	getSEndpointRef tcp_SChan
		#!	ch	= unpack_tcpschan tcp_SChan
		= (ch.bEndpointRef, pack_tcpschan ch)
  
selectLoop nonBlocking mbStopTime rcvEndpoints rcvKindArray sndEndpoints r_channels s_channels env
	#!	(isIOProg, env) = channelEnvKind env
		rcvEndpointArray	= { ep \\ ep <- rcvEndpoints }
		sndEndpointArray	= { ep \\ ep <- sndEndpoints }
		(errCode, env)	
			= selectChC isIOProg nonBlocking mbStopTime rcvEndpointArray rcvKindArray sndEndpointArray env
		// SIDEEFFECT: this will also alter the contents of rcvEndpointArray and sndEndpointArray
	|	errCode	== 1 || errCode==3	// timeout expired or some other error
		= ([], r_channels, s_channels, env)
	#!	readableIndices	= [ i \\ endpointRef<-:rcvEndpointArray & i<-[0..] | endpointRef==0]
		(rStates, r_channels, env)	= pollRState readableIndices [] r_channels env
		sendableIndices	= [ i \\ endpointRef<-:sndEndpointArray & i<-[0..] | endpointRef==0]
		(sStates, s_channels, env)	= pollSState sendableIndices [] s_channels env
		states	= rStates++sStates
	|	isEmpty states
		= case nonBlocking of
			False	-> selectLoop nonBlocking mbStopTime rcvEndpoints rcvKindArray sndEndpoints r_channels s_channels env
			_		-> ([], r_channels, s_channels, env)
	= (states,  r_channels, s_channels, env)

pollRState [] akku r_channels env
	= (reverse akku, r_channels, env)
pollRState [rcvIndex:rcvIndices] akku r_channels env
	#!	(mbResult, r_channels, env)	= getRState rcvIndex r_channels env
	|	isNothing mbResult
		= pollRState rcvIndices akku r_channels env
	#!	result	= fromJust mbResult
	| result<>SR_Available && result<>SR_EOM
		= abort (	"StdTCPChannels: error an instance of getRState delivered something else"
				 +++" than SR_Available or SR_EOM")
	= pollRState rcvIndices [(rcvIndex, result):akku] r_channels env

pollSState [] akku s_channels env
	= (reverse akku, s_channels, env)
pollSState [sndIndex:sndIndices] akku s_channels env
	#!	(isDisconnected, s_channels, env)	= appDisconnected sndIndex s_channels env
		sState	= if isDisconnected SR_Disconnected SR_Sendable
	= pollSState sndIndices [(sndIndex, sState):akku] s_channels env

////////////////////// TCP byte streams to receive ////////////////////////////////////

::	*TCP_RCharStream	:==		TCP_RCharStream_ Char
::	*TCP_RCharStreams	= 		TCP_RCharStreams *[TCP_RCharStream]

toRCharStream	::	!TCP_RChannel -> TCP_RCharStream
toRCharStream tcp_rchan
	=	{	rbs_rchan	= tcp_rchan
		,	rbs_buffer	= ""
		,	rbs_index	= 0
		}

instance Receive			TCP_RCharStream_
  where
	receive_MT mbTimeout rbs=:{ rbs_buffer, rbs_index } env
		|	isJust mbTimeout && (fromJust mbTimeout)<0
			= (TR_Expired, Nothing, rbs, env)
		|	rbs_index < (size rbs_buffer)
			= ( TR_Success, Cast (Just rbs_buffer.[rbs_index]), { rbs & rbs_index=inc rbs_index }, env)
		#!	rbs_rchan	= rbs.rbs_rchan
			(toReport, mbData, rbs_rchan, env)	= receive_MT mbTimeout rbs_rchan env
		|	toReport==TR_Success
			#!	data	= toString (fromJust mbData)
			= ( toReport, Cast (Just data.[0]),
			    { rbs & rbs_rchan=rbs_rchan, rbs_index=1, rbs_buffer=data }, env)
		= ( toReport, Nothing, { rbs & rbs_rchan=rbs_rchan, rbs_buffer="", rbs_index=0 }, env)
	receiveUpTo maxLength rbs=:{ rbs_buffer, rbs_index } env
		|	maxLength<=0
			=([], rbs, env)
		#!	nrOfCharsInBuffer	= max (size rbs_buffer - rbs_index) 0
		|	nrOfCharsInBuffer>=maxLength	// enough data is buffered in rbs_buffer
			= (Cast [rbs_buffer.[i] \\ i<-[rbs_index..rbs_index+maxLength-1]], { rbs & rbs_index=rbs_index+maxLength }, env)
		#!	(_, mbData, rbs_rchan, env)	= receive_MT (Just 0) rbs.rbs_rchan env
			charsInBuffer	= [rbs_buffer.[i] \\ i<-[rbs_index..(size rbs_buffer)-1]]
		|	isNothing mbData	// available is False->return the rbs_buffer
			= (	Cast charsInBuffer, { rbs & rbs_rchan=rbs_rchan, rbs_buffer="", rbs_index=0 }, env)
		#!	(l, ch, env)	= receiveUpTo	(maxLength-nrOfCharsInBuffer)
											{rbs & rbs_rchan=rbs_rchan, rbs_buffer=toString (fromJust mbData),
													rbs_index=0 }
											env
		= ((Cast charsInBuffer)++l, ch, env)
	available rbs=:{ rbs_buffer, rbs_index } env
		|	rbs_index < (size rbs_buffer)
			= (True, rbs, env)
		#!	rbs_rchan	= rbs.rbs_rchan
			(isAvailable, rbs_rchan, env)	= available rbs_rchan env
		= (isAvailable, { rbs & rbs_rchan=rbs_rchan }, env)
	eom rbs=:{ rbs_buffer, rbs_index } env
		|	rbs_index < (size rbs_buffer)
			= (False, rbs, env)
		#!	rbs_rchan	= rbs.rbs_rchan
			(isEom, rbs_rchan, env)	= eom rbs_rchan env
		= (isEom, { rbs & rbs_rchan=rbs_rchan }, env)

instance closeRChannel			TCP_RCharStream_
  where
	closeRChannel { rbs_rchan } env
		= closeRChannel rbs_rchan env

////////////////////// TCP byte streams to send ////////////////////////////////////

::	*TCP_SCharStream	:==		TCP_SCharStream_ Char
::	*TCP_SCharStreams	= 		TCP_SCharStreams *[TCP_SCharStream]

toSCharStream	::	!TCP_SChannel -> TCP_SCharStream
toSCharStream tcp_schan
	=	{	sbs_schan	= tcp_schan }

instance Send				TCP_SCharStream_
  where
	send_MT mbTimeout charPolymorphic sbs=:{ sbs_schan } env
		#!	char	= castToChar charPolymorphic
			(toReport, sentBytes, sbs_schan, env)	= send_MT mbTimeout (toByteSeq char) sbs_schan env
		= (toReport, sentBytes, {sbs & sbs_schan=sbs_schan}, env)
	nsend_MT mbTimeout chListPolymorphic sbs=:{ sbs_schan } env
		#!	(toReport, sentBytes, sbs_schan, env)	= send_MT mbTimeout (toByteSeq string) sbs_schan env
		= (toReport, sentBytes, {sbs & sbs_schan=sbs_schan}, env)
	  where
		chList	= castToChList chListPolymorphic
		string :: String
		string	= { ch \\ ch<-chList }
	flushBuffer_MT mbTimeout sbs=:{ sbs_schan } env
		#!	(tReport, sentBytes, sbs_schan, env)	= flushBuffer_MT mbTimeout sbs_schan env
		= (tReport, sentBytes, {sbs & sbs_schan=sbs_schan}, env)
	closeChannel_MT mbTimeout sbs=:{ sbs_schan } env
		= closeChannel_MT mbTimeout sbs_schan env
	abortConnection sbs=:{ sbs_schan } env
		= abortConnection sbs_schan env
	disconnected sbs=:{ sbs_schan } env
		#!	(isDisconnected, sbs_schan, env)	= disconnected sbs_schan env
		= (isDisconnected, {sbs & sbs_schan=sbs_schan}, env)
	bufferSize sbs=:{ sbs_schan }
		#!	(buffSize, sbs_schan)	= bufferSize sbs_schan
		= (buffSize, {sbs & sbs_schan=sbs_schan})

tcpPossible			::	!*env
					->	(!Bool, !*env)
					|	ChannelEnv env
tcpPossible env
	= tcpPossibleC env
