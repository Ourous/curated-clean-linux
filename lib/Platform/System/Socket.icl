implementation module System.Socket

import StdEnv
import System._Socket => qualified socket, bind, listen, accept, close, connect, send, recv, hostToNetworkByteOrderLong, hostToNetworkByteOrderShort, networkToHostByteOrderShort, networkToHostByteOrderLong
import System.OSError

instance toInt SocketType where
	toInt SocketStream = SOCK_STREAM
	toInt SocketDataGram = SOCK_DGRAM

instance toInt SendFlag where
	toInt SendFlagOob = MSG_OOB
	toInt SendFlagDontRoute = MSG_DONTROUTE

instance toInt RecvFlag where
	toInt RecvFlagOob = MSG_OOB
	toInt RecvFlagWaitAll = MSG_WAITALL
	toInt RecvFlagPeek = MSG_PEEK

socket :: !SocketType !*env -> *(!MaybeOSError *(Socket sa), !*env) | SocketAddress sa
socket a b = 'System._Socket'.socket a b

bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
bind a b = 'System._Socket'.bind a b

listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
listen a b = 'System._Socket'.listen a b

accept :: !*(Socket sa) -> *(!MaybeOSError (!*Socket sa, !sa), !*(Socket sa)) | SocketAddress sa
accept a = 'System._Socket'.accept a

close :: !*(Socket sa) !*env -> *(!MaybeOSError (), !*env) | SocketAddress sa
close a b = 'System._Socket'.close a b

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
connect a b = 'System._Socket'.connect a b

send :: !String ![SendFlag] !*(Socket sa) -> *(!MaybeOSError Int, !*Socket sa)
send a b c = 'System._Socket'.send a b c

recv :: !Int ![RecvFlag] !*(Socket sa) -> *(!MaybeOSError String, !*Socket sa)
recv a b c = 'System._Socket'.recv a b c

networkToHostByteOrderShort :: !Int -> Int
networkToHostByteOrderShort a = 'System._Socket'.networkToHostByteOrderShort a

hostToNetworkByteOrderShort :: !Int -> Int
hostToNetworkByteOrderShort a = 'System._Socket'.hostToNetworkByteOrderShort a

networkToHostByteOrderLong :: !Int -> Int
networkToHostByteOrderLong a = 'System._Socket'.networkToHostByteOrderShort a

hostToNetworkByteOrderLong :: !Int -> Int
hostToNetworkByteOrderLong a = 'System._Socket'.hostToNetworkByteOrderLong a
