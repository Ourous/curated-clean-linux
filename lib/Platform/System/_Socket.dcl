definition module System._Socket

from Data.Error import :: MaybeError
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode
from System.Socket import :: SocketType, class SocketAddress, :: SendFlag, :: RecvFlag

:: *Socket a

AF_INET :== 2
AF_UNIX :== 1
AF_INET6 :== 10
AF_IPX :== 4
AF_APPLETALK :== 5
AF_IRDA :== 23

SOCK_STREAM :== 1
SOCK_DGRAM :== 2

MSG_DONTROUTE :== 4
MSG_OOB :== 1
MSG_PEEK :== 2
MSG_WAITALL :== 256

socket :: !SocketType !*env -> *(!MaybeOSError *(Socket sa), !*env) | SocketAddress sa
bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
accept :: !*(Socket sa) -> *(!MaybeOSError (!*Socket sa, !sa), !*Socket sa) | SocketAddress sa
close :: !*(Socket sa) !*env -> *(!MaybeOSError (), !*env) | SocketAddress sa

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa

send :: !String ![SendFlag] !*(Socket sa) -> *(!MaybeOSError Int, !*Socket sa)
recv :: !Int ![RecvFlag] !*(Socket sa) -> *(!MaybeOSError String, !*Socket sa)

networkToHostByteOrderShort :: !Int -> Int
hostToNetworkByteOrderShort :: !Int -> Int
networkToHostByteOrderLong :: !Int -> Int
hostToNetworkByteOrderLong :: !Int -> Int
