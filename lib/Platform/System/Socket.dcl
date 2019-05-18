definition module System.Socket

from StdOverloaded import class toInt
from Data.Error import :: MaybeError, :: MaybeErrorString
from System._Pointer import :: Pointer(..)
from System._Socket import :: Socket
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorMessage, :: OSErrorCode

:: SocketType = SocketStream | SocketDataGram
:: SendFlag = SendFlagOob | SendFlagDontRoute
:: RecvFlag = RecvFlagOob | RecvFlagWaitAll | RecvFlagPeek
instance toInt SocketType, SendFlag, RecvFlag

class SocketAddress sa where
	sa_length      :: !sa -> Int
	sa_serialize   :: !sa !Pointer !*env -> *(!Pointer, !*env)
	sa_deserialize :: !Pointer -> MaybeErrorString sa
	sa_domain      :: !sa -> Int
	sa_null        :: sa

/**
 * Register a socket with the given type
 *
 * @param type of communication semantics
 * @param environment
 * @return socket
 * @return new environment
 */
socket :: !SocketType !*env -> *(!MaybeOSError *(Socket sa), !*env) | SocketAddress sa

/**
 * Bind a socket to an address
 *
 * @param address
 * @param socket
 * @return error if something went wrong
 * @return new socket
 */
bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa

/**
 * Listen for connections on a socket
 *
 * @param maximum number of backlog connections
 * @param socket
 * @return error if something went wrong
 * @return new socket
 */
listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa

/**
 * Accept a connection from a listening socket
 *
 * @param socket
 * @return error if something went wrong or a connected socket with its address if it went okay
 * @return new socket
 */
accept :: !*(Socket sa) -> *(!MaybeOSError (!*Socket sa, !sa), !*Socket sa) | SocketAddress sa

/**
 * Close a socket
 *
 * @param socket
 * @param environment
 * @return error if something went wrong
 * @return new environmnt
 */
close :: !*(Socket sa) !*env -> *(!MaybeOSError (), !*env) | SocketAddress sa

/**
 * Connect to a listening address
 *
 * @param address
 * @param socket
 * @return error if something went wrong
 * @return new socket
 */
connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa

/**
 * Send data to a socket
 *
 * @param data
 * @param flags
 * @return error if something went wrong or the number of bytes sent otherwise
 * @return new socket
 */
send :: !String ![SendFlag] !*(Socket sa) -> *(!MaybeOSError Int, !*Socket sa)

/**
 * Receive data from a socket
 *
 * @param number of bytes to receive
 * @param flags
 * @return error if something went wrong or the data received otherwise
 * @return new socket
 */
recv :: !Int ![RecvFlag] !*(Socket sa) -> *(!MaybeOSError String, !*Socket sa)

/**
 * Convert a short in network order to a short in host order
 */
networkToHostByteOrderShort :: !Int -> Int

/**
 * Convert a short in network order to a short in host order
 */
hostToNetworkByteOrderShort :: !Int -> Int

/**
 * Convert a long in network order to a long in host order
 */
networkToHostByteOrderLong :: !Int -> Int

/**
 * Convert a long in network order to a long in host order
 */
hostToNetworkByteOrderLong :: !Int -> Int
