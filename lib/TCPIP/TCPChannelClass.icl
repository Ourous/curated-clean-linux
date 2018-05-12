implementation module TCPChannelClass

import	StdEnv,StdMaybe
import	tcp
from ostcp import tcp_getcurrenttick

instance ChannelEnv World
  where
	channelEnvKind env
		= (WORLD, env)
	mb_close_inet_receiver_without_id _ _ world
		= world
	channel_env_get_current_tick env
//		= os_getcurrenttick env
		= tcp_getcurrenttick env

///////////////////////////////// receive channels /////////////////////////////////

class Receive ch where
	receive_MT		::	!(Maybe Timeout)			!*(ch .a)   !*env	
					->	(!TimeoutReport, !Maybe .a, !*(ch .a),  !*env)	
													| ChannelEnv  env
	receiveUpTo		::	!Int						!*(ch .a)   !*env	
					->	(![.a],						!*(ch .a),  !*env)	
													| ChannelEnv  env
	available		:: 								!*(ch .a)   !*env
					->	(!Bool,						!*(ch .a),  !*env)
													| ChannelEnv  env
	eom				:: 								!*(ch .a)   !*env
					->	(!Bool,						!*(ch .a),  !*env)
													| ChannelEnv  env
        
class closeRChannel	ch	:: !*(ch .a) !*env -> *env	| ChannelEnv env

//////////////////////////////// send channels /////////////////////////////////////
	
class Send ch
where
	send_MT			::	!(Maybe Timeout) !.a		!*(ch .a)  !*env
					->	(!TimeoutReport, !Int,		!*(ch .a), !*env)
													| ChannelEnv  env
	nsend_MT		:: 	!(Maybe Timeout) ![.a]		!*(ch .a)	!*env
					->	(!TimeoutReport, !Int,		!*(ch .a), !*env)
													| ChannelEnv  env
	flushBuffer_MT	::	!(Maybe Timeout) 			!*(ch .a)  !*env
					->	(!TimeoutReport, !Int,		!*(ch .a), !*env)
													| ChannelEnv  env
	closeChannel_MT	::	!(Maybe Timeout) 			!*(ch .a)  !*env
					->	(!TimeoutReport, !Int,					!*env)
													| ChannelEnv  env
	abortConnection	::								!*(ch .a)	!*env
					->											 *env
													| ChannelEnv  env
	disconnected	::								!*(ch .a)	!*env
					->	(!Bool,						!*(ch .a), !*env)
													| ChannelEnv  env
	bufferSize		::								!*(ch .a)
					->	(!Int, 						!*(ch .a))	

class MaxSize ch
  where
	setMaxSize		::	!Int !*(ch .a)	-> *(ch .a)
	getMaxSize		::		 !*(ch .a)	-> (!Int, !*(ch .a))
	clearMaxSize	::		 !*(ch .a)	-> *(ch .a)

:: DuplexChannel sChannel rChannel a
	=	{	sChannel	::	sChannel a
		,	rChannel	::	rChannel a
		}

::	TimeoutReport
	= 	TR_Expired
	|	TR_Success
	|	TR_NoSuccess

::	Timeout		:==	Int		// timeout in ticks

instance == TimeoutReport
  where
	(==) TR_Expired	x	= case x of TR_Expired	 	-> True
									_				-> False
	(==) TR_Success	x	= case x of TR_Success		-> True
									_				-> False
	(==) TR_NoSuccess x	= case x of TR_NoSuccess	-> True
									_				-> False

instance toString TimeoutReport
  where
	toString TR_Expired = "TR_Expired"
	toString TR_Success = "TR_Success"
	toString TR_NoSuccess = "TR_NoSuccess"
	
/////////////////// derived functions ////////////////////////////////////////////////////

nreceive_MT			::	!(Maybe Timeout) !Int		!*(ch .a)  !*env	
					->	(!TimeoutReport, ![.a],		!*(ch .a), !*env)	
													| Receive ch & ChannelEnv  env
nreceive_MT mbTimeout n ch env
//	#!	(before, env)	= getCurrentTick env
	#!	(before, env)	= channel_env_get_current_tick env
		(l, ch, env)	= receiveUpTo n ch env
		(length, l)		= u_length l
	|	length==n
		= (TR_Success, l, ch, env)
	#!	(tReport, mbData, ch, env)	= receive_MT mbTimeout ch env
	|	tReport<>TR_Success
		= (tReport, l, ch, env)
	|	n-length==1
		= (tReport, l++[fromJust mbData], ch, env)
//	#!	(after, env)	= getCurrentTick env
	#!	(after, env)	= channel_env_get_current_tick env
//	= nreceive_MT (mbSubtract mbTimeout (tickDifference after before)) (n-length-1) ch env
	= nreceive_MT (mbSubtract mbTimeout (after-before)) (n-length-1) ch env
  where
	mbSubtract Nothing _ 		= Nothing
	mbSubtract (Just timeout) i	= Just (timeout-i)

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
	


receive	:: 	!*(ch .a)  !*env -> (!.a, !*(ch .a), !*env)
			| 	ChannelEnv env & Receive ch
receive ch env
	#!	(timeoutReport, mbMessage, ch, env)	= receive_MT Nothing ch env
	| timeoutReport==TR_NoSuccess
		#!	(isEom, ch, env)	= eom ch env
		| isEom
			= abort "\nStdChannels: receive failed"
		= receive ch env
	= (fromJust mbMessage, ch, env)

nreceive		::	!Int !*(ch .a)  !*env -> (![.a], !*(ch .a), !*env)
				|	ChannelEnv env & Receive ch
nreceive n ch env
	#!	(timeoutReport, l, ch, env)	= nreceive_MT Nothing n ch env
	| timeoutReport==TR_NoSuccess
		#!	(isEom, ch, env)	= eom ch env
		| isEom
			= abort "\nStdChannels: nreceive failed"
		= nreceive n ch env
	= (l, ch, env)

send	 	:: !.a	!*(ch .a) !*env -> (!*(ch .a), !*env)
			| 	ChannelEnv env & Send ch
send msg ch env
	#!	(_,_,ch,env)	= send_MT Nothing msg ch env
	= (ch, env)

closeChannel:: !*(ch .a) !*env -> *env	| ChannelEnv env & Send ch
closeChannel ch env
	#!	(_,_,env)	= closeChannel_MT Nothing ch env
	= env

nsend 	:: ![.a]		!*(ch .a) !*env
			->			   (!*(ch .a), !*env)	| 	ChannelEnv env & Send ch
nsend msg ch env
	#!	(_,_,ch,env)	= nsend_MT Nothing msg ch env
	= (ch, env)

send_NB  	:: !.a	!*(ch .a) !*env -> (!*(ch .a), !*env)
			| 	ChannelEnv env & Send ch
send_NB msg ch env
	#!	(_,_,ch,env)	= send_MT (Just 0) msg ch env
	= (ch, env)

flushBuffer_NB	::				!*(ch .a)  !*env
				->			   (!*(ch .a), !*env)
				| 	ChannelEnv env & Send ch
flushBuffer_NB ch env
	#!	(_,_,ch,env)	= flushBuffer_MT (Just 0) ch env
	= (ch, env)
