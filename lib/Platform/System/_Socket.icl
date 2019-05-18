implementation module System._Socket

import StdEnv
import Data.Error
import System.OSError
import System._Pointer
import System._Posix
import System.Socket => qualified socket, bind, listen, accept, close, connect, send, recv, networkToHostByteOrderLong, networkToHostByteOrderShort, hostToNetworkByteOrderLong, hostToNetworkByteOrderShort

:: *Socket a :== Int

socket :: !SocketType !*env -> *(!MaybeOSError *(Socket sa), !*env) | SocketAddress sa
socket type w
	# (sockfd, w) = socket` (sa_domain msa) (toInt type) 0 w
	# (fd, sockfd) = getFd sockfd
	| fd == -1 = getLastOSError w
	= (Ok (coerce sockfd msa), w)
where
	msa = sa_null

	coerce :: *(Socket sa) sa -> *(Socket sa)
	coerce x y = x

	socket` :: !Int !Int !Int !*env -> *(!*Int, !*env)
	socket` _ _ _ _ = code {
			ccall socket "III:I:A"
		}

bind :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
bind addr sockfd
	# (p, sockfd) = mallocSt (sa_length addr) sockfd
	| p == 0 = getLastOSError sockfd
	# (p, sockfd) = sa_serialize addr p sockfd
	# len = sa_length addr
	# (fd, sockfd) = getFd sockfd
	# (r, sockfd) = bind` fd p len sockfd
	# sockfd = freeSt p sockfd
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	bind` :: !Int !Pointer !Int !*env -> *(!Int, !*env)
	bind` _ _ _ _ = code {
			ccall bind "IpI:I:A"
		}

listen :: !Int !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
listen backlog sockfd
	#! r = listen` sockfd backlog
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	listen` :: !Int !Int -> Int
	listen` _ _ = code {
			ccall listen "II:I"
		}

accept :: !*(Socket sa) -> *(!MaybeOSError (!*Socket sa, !sa), !*Socket sa) | SocketAddress sa
accept sockfd
	# (fd, sockfd) = getFd sockfd
	# (p1, sockfd) = mallocSt 64 sockfd
	| p1 == 0 = getLastOSError sockfd
	# (p2, sockfd) = mallocSt 8 sockfd
	| p2 == 0 = getLastOSError (freeSt p2 sockfd)
	# p2 = writeInt p2 0 64
	= case accept` fd p1 p2 sockfd of
		(-1, sockfd)
			#! sockfd = freeSt p1 sockfd
			#! sockfd = freeSt p2 sockfd
			= getLastOSError sockfd
		(sock, sockfd)
			#! (merr, p1) = readP sa_deserialize p1
			#! sockfd = freeSt p1 sockfd
			#! sockfd = freeSt p2 sockfd
			| isError merr = (Error (0, fromError merr), sockfd)
			= (Ok (sock, fromOk merr), sockfd)
where
	accept` :: !Int !Pointer !Int !*env -> *(!*Int, !*env)
	accept` _ _ _ _ = code {
			ccall accept "IpI:I:A"
		}

connect :: !sa !*(Socket sa) -> *(!MaybeOSError (), !*Socket sa) | SocketAddress sa
connect addr sockfd
	# (p, sockfd) = mallocSt (sa_length addr) sockfd
	| p == 0 = getLastOSError sockfd
	# (p, sockfd) = sa_serialize addr p sockfd
	# (fd, sockfd) = getFd sockfd
	# (r, sockfd) = connect` fd p (sa_length addr) sockfd
	# sockfd = freeSt p sockfd
	| r == -1 = getLastOSError sockfd
	= (Ok (), sockfd)
where
	connect` :: !Int !Pointer !Int !*env -> *(!Int, !*env)
	connect` _ _ _ _ = code {
			ccall connect "IpI:I:A"
		}

send :: !String ![SendFlag] !*(Socket sa) -> *(!MaybeOSError Int, !*Socket sa)
send data flags sockfd
	# flags = foldr (bitor) 0 (map toInt flags)
	# (fd, sockfd) = getFd sockfd
	# (r, sockfd) = send` fd (packString data) (size data) flags sockfd
	| r == -1 = getLastOSError sockfd
	= (Ok r, sockfd)
where
	send` :: !Int !String !Int !Int !*env -> *(!Int, !*env)
	send` _ _ _ _ _ = code {
			ccall send "IsII:I:A"
		}

recv :: !Int ![RecvFlag] !*(Socket sa) -> *(!MaybeOSError String, !*Socket sa)
recv length flags sockfd
	# flags = foldr (bitor) 0 (map toInt flags)
	# (p, sockfd) = mallocSt length sockfd
	| p == 0 = getLastOSError sockfd
	# (fd, sockfd) = getFd sockfd
	# (r, sockfd) = recv` fd p length flags sockfd
	| r == -1 = getLastOSError (freeSt p sockfd)
	# (s, p) = readP derefString p
	# sockfd = freeSt p sockfd
	= (Ok s, sockfd)
	
where
	recv` :: !Int !Pointer !Int !Int !*env -> *(!Int, !*env)
	recv` _ _ _ _ _ = code {
			ccall recv "IpII:I:A"
		}

close :: !*(Socket sa) !*env -> *(!MaybeOSError (), !*env) | SocketAddress sa
close sock w
	# r = close` sock
	| r == -1 = getLastOSError w
	= (Ok (), w)
where
	close` :: !Int -> Int
	close` _ = code {
			ccall close "I:I"
		}

networkToHostByteOrderShort :: !Int -> Int
networkToHostByteOrderShort a = code {
		ccall ntohs "I:I"
	}

hostToNetworkByteOrderShort :: !Int -> Int
hostToNetworkByteOrderShort a = code {
		ccall htons "I:I"
	}

networkToHostByteOrderLong :: !Int -> Int
networkToHostByteOrderLong a = code {
		ccall ntohl "I:I"
	}

hostToNetworkByteOrderLong :: !Int -> Int
hostToNetworkByteOrderLong a = code {
		ccall htonl "I:I"
	}

getFd :: !*(Socket sa) -> *(!Int, !*Socket sa)
getFd s = code {
		push_b 0
	}
