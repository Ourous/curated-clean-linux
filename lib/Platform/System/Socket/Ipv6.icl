implementation module System.Socket.Ipv6

import StdEnv
import Data.Error
import Data.Maybe
import System.Socket
import System._Pointer
import Text.GenPrint
from System._Socket import AF_INET6

instance SocketAddress Ipv6SocketAddress where
	sa_serialize sa p w
		# p = writeInt2 p 0 (sa_domain sa)
		# p = writeInt2 p 2 (hostToNetworkByteOrderShort sa.ipv6_socket_port)
		# p = writeInt4 p 4 (sa.ipv6_socket_flowinfo)
		# p = writeCharArray (p+8) (pad16 (fromMaybe "::" sa.ipv6_socket_addr))
		# p = writeInt4 p 24 (sa.ipv6_socket_scope_id)
		= (p, w)
	where
		pad16 s = s +++ {'\0'\\_<-[0..16-1-size s]}
	sa_deserialize p = Ok
		{ ipv6_socket_port     = networkToHostByteOrderShort (readInt2Z p 2)
		, ipv6_socket_flowinfo = readInt4Z p 4
		, ipv6_socket_addr     = Just (derefCharArray (p+8) 16)
		, ipv6_socket_scope_id = readInt4Z p 24
		}
	sa_length _ = 28
	sa_domain _ = AF_INET6
	sa_null = {ipv6_socket_port=0,ipv6_socket_flowinfo=0,ipv6_socket_addr=Nothing,ipv6_socket_scope_id=0}

derive gPrint Ipv6SocketAddress, Maybe
instance toString Ipv6SocketAddress where toString s = printToString s
