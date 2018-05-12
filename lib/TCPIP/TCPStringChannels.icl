implementation module TCPStringChannels

import	StdEnv,StdMaybe
import	TCPChannelClass, TCPDef, TCPChannels, TCPEvent, TCPStringChannelsInternal
from	tcp import class ChannelEnv (channel_env_get_current_tick)

////////////////////// StringChannels to receive ////////////////////////////////////

::	*StringRChannel		:== StringRChannel_ String
::	*StringRChannels	=	StringRChannels *[StringRChannel]

toStringRChannel	::	TCP_RChannel -> StringRChannel
toStringRChannel tcp_rchan
	= { tcp_rchan=tcp_rchan, readPhase=ReadingLength 0, receivedStrings=[], maxSize=0 }

instance Receive StringRChannel_ where
	receive_MT mbTimeout rchan=:{receivedStrings, maxSize} env
		|	isJust mbTimeout && (fromJust mbTimeout)<0
			= (TR_Expired, Nothing, rchan, env)
		|	not (isEmpty receivedStrings)
			= (TR_Success, Just (Cast (hd receivedStrings)), { rchan & receivedStrings=tl receivedStrings }, env)
		#!	(timeBeforeReceive, env)	= channel_env_get_current_tick env
			(toReport, mbByteSeq, tcp_rchan, env)	= receive_MT mbTimeout rchan.tcp_rchan env
		|	toReport==TR_Expired
			= (toReport, Nothing, { rchan & tcp_rchan=tcp_rchan }, env)
		|	toReport==TR_NoSuccess
			= (toReport, Nothing, { rchan & tcp_rchan=tcp_rchan, readPhase=EndOfMessages }, env)
		#!	(newStrings, readPhase)	= addString (toString (fromJust mbByteSeq), 0) rchan.readPhase maxSize
		|	not (isEmpty newStrings)
			= (	TR_Success, Just (Cast (hd newStrings))
			  ,	{ rchan & tcp_rchan=tcp_rchan, readPhase=readPhase, receivedStrings=tl newStrings }
			  , env
			  )
		#!	(timeAfterReceive, env)	= channel_env_get_current_tick env
			usedTime		= timeAfterReceive-timeBeforeReceive
			newMbTimeout	= if (isNothing mbTimeout) mbTimeout (Just ((fromJust mbTimeout)-usedTime))
		= receive_MT newMbTimeout { rchan & tcp_rchan=tcp_rchan, readPhase=readPhase } env
			
	receiveUpTo max ch env
		= receiveUpToGeneral [] max ch env
	
	available rchan=:{maxSize} env
		|	not (isEmpty rchan.receivedStrings)
			= (True, rchan, env)
		#!	(isAvailable, tcp_rchan, env)	= available rchan.tcp_rchan env
		|	not isAvailable
			= (False, { rchan & tcp_rchan=tcp_rchan }, env)
		#!	(byteSeq, tcp_rchan, env)	= receive tcp_rchan env
			string	= toString byteSeq
			(newStrings, readPhase)	= addString (string, 0) rchan.readPhase maxSize
		= (	not (isEmpty newStrings)
		  ,	{ rchan & tcp_rchan=tcp_rchan, readPhase=readPhase, receivedStrings=newStrings }
		  ,	env
		  )
	
	eom rchan env
		|	not (isEmpty rchan.receivedStrings)
			= (False, rchan, env)
		#!	(isEom, rchan)	= isEOM rchan
		| isEom
			= (True, rchan, env)
		#!	(isTCPEom, tcp_rchan, env)	= eom rchan.tcp_rchan env
		|	isTCPEom
			= (isTCPEom, { rchan & tcp_rchan=tcp_rchan, readPhase=EndOfMessages }, env)
		= (isTCPEom, { rchan & tcp_rchan=tcp_rchan }, env)
	  where
		isEOM rchan=:{readPhase=EndOfMessages}	= (True, rchan)
		isEOM rchan								= (False, rchan)

receiveUpToGeneral akku max ch env
	|	max<=0
		= (u_reverse akku, ch, env)
	#!	(tReport, mbData, ch, env)	= receive_MT (Just 0) ch env
	|	tReport<>TR_Success
		= (u_reverse akku, ch, env)
	= receiveUpToGeneral [fromJust mbData:akku] (dec max) ch env

instance closeRChannel			StringRChannel_
  where
	closeRChannel {tcp_rchan} env
		= closeRChannel tcp_rchan env


instance MaxSize			StringRChannel_
  where
	setMaxSize newMaxSize ch=:{readPhase=ReadingString _ _ currentStringSize, maxSize}
		|	currentStringSize>maxSize && maxSize>0
			= { ch & readPhase=EndOfMessages, maxSize=max newMaxSize 0}
	setMaxSize newMaxSize ch=:{maxSize}
		= { ch & maxSize=max newMaxSize 0 }
	getMaxSize ch=:{maxSize}
		= (maxSize, ch)
	clearMaxSize ch
		= { ch & maxSize=0 }
	
////////////////////// StringChannels to send ////////////////////////////////////

::	*StringSChannel_ a	=	StringSChannel_ TCP_SChannel
::	*StringSChannel		:== StringSChannel_ String
::	*StringSChannels	=	StringSChannels *[StringSChannel]

toStringSChannel	::	TCP_SChannel -> StringSChannel
toStringSChannel tcp_schan
	= StringSChannel_ tcp_schan

stringToByteSeq	::	!String -> ByteSeq
stringToByteSeq string
	= toByteSeq (toString (size string)+++" "+++string+++NL13)

NL13 :== "\xD"	// carriage return	

instance Send				StringSChannel_
  where
	send_MT mbTimeout stringPolymorph ch env
		= nsend_MT mbTimeout [stringPolymorph] ch env
	nsend_MT mbTimeout stringListPolymorph (StringSChannel_ tcp_schan) env
		#!	(toReport, sentBytes, tcp_schan, env)
				= nsend_MT mbTimeout (map stringToByteSeq (CastToStringList stringListPolymorph)) tcp_schan env
		= (toReport, sentBytes, StringSChannel_ tcp_schan, env)
	flushBuffer_MT mbTimeout (StringSChannel_ tcp_schan) env
		#!	(tReport, sentBytes, tcp_schan, env)
				= flushBuffer_MT mbTimeout tcp_schan env
		= (tReport, sentBytes, StringSChannel_ tcp_schan, env)
	closeChannel_MT mbTimeout (StringSChannel_ tcp_schan) env
		= closeChannel_MT mbTimeout tcp_schan env
	abortConnection (StringSChannel_ tcp_schan) env
		= abortConnection tcp_schan env
	
	disconnected (StringSChannel_ tcp_schan) env
		#!	(isDisconnected, tcp_schan, env)
				= disconnected tcp_schan env
		= (isDisconnected, StringSChannel_ tcp_schan, env)

	bufferSize (StringSChannel_ tcp_schan)
		#!	(buffSize, tcp_schan)
				= bufferSize tcp_schan
		= (buffSize, StringSChannel_ tcp_schan)

/////////////////////////////////////////////////////////////////////////////////

// for openSendNotifier, closeSendNotifier
instance accSChannel 		StringSChannel_
  where
	accSChannel f (StringSChannel_ tcp_schan)
		#!	(x, tcp_schan)	= f tcp_schan
		= (x, (StringSChannel_ tcp_schan))

// for selectChannel
instance SelectSend		StringSChannels
  where
	accSChannels f (StringSChannels channels)
		= accSChannelsA f channels []
	  where
		accSChannelsA f [] akku
			#! (l, channels)	= unzip (u_reverse akku)
			= (l, StringSChannels channels)
		accSChannelsA f [StringSChannel_ tcp_schan:channels] akku
			#!	(x, tcp_schan)	= f tcp_schan
			= accSChannelsA f channels [(x,StringSChannel_ tcp_schan): akku]
	appDisconnected n (StringSChannels channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(isDisconnected, channel, env)	= disconnected channel env
		= (isDisconnected, StringSChannels (l ++ [channel:r]), env)

instance SelectReceive			StringRChannels
  where
	accRChannels f (StringRChannels channels)
		= accRChannelsA f channels []
	  where
		accRChannelsA f [] akku
			#! (l, channels)	= unzip (u_reverse akku)
			= (l, StringRChannels channels)
		accRChannelsA f [chan=:{tcp_rchan}:channels] akku
			#!	(x, TCP_RCHANNEL tcp_rchan)	= f (TCP_RCHANNEL tcp_rchan)
			= accRChannelsA f channels [(x,{ chan & tcp_rchan=tcp_rchan}): akku]
	getRState n (StringRChannels channels) env
		#!	(l,[channel:r])			= splitAt n channels
			(state, channel, env)	= getState channel env
		= (state, StringRChannels (l ++ [channel:r]), env)
	  where
		getState channel env
			#!	(isAvailable, channel, env) = available channel env
			|	isAvailable
				= (Just SR_Available, channel, env)
			#!	(isEom, channel, env) = eom channel env
			|	isEom
				= (Just SR_EOM, channel, env)
			= (Nothing, channel, env)

instance getNrOfChannels 	StringRChannels
  where
	getNrOfChannels (StringRChannels channels)
		#!	(n, channels)	= u_length channels
		= (n, StringRChannels channels)

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

CastToStringList	::	!.a -> [String]
CastToStringList x 
	= Cast x

Cast :: !.a -> .b
Cast a
	= code
		{
			pop_a 0
		}
