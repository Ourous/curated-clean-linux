implementation module System.Socket.Ipv4

import StdEnv
import Network.IP
import Data.Error
import System.Socket
import System._Pointer
import Text.GenPrint

instance SocketAddress Ipv4SocketAddress where
	sa_serialize sa p w
		# p = writeInt2 p 0 (sa_domain sa)
		# p = writeInt2 p 2 (hostToNetworkByteOrderShort sa.ipv4_socket_port)
		# p = writeInt4 p 4 (maybe 0 toInt sa.ipv4_socket_addr)
		= (p, w)
	sa_deserialize p
		= Ok {ipv4_socket_port=networkToHostByteOrderShort (readInt2Z p 2),ipv4_socket_addr=Just (fromInt (readInt4Z p 4))}
	sa_length _ = 16
	sa_domain _ = 2
	sa_null = {ipv4_socket_port=0, ipv4_socket_addr=Nothing}

gPrint{|IPAddress|} a s = gPrint{|*|} (toString a) s
derive gPrint Ipv4SocketAddress, Maybe
instance toString Ipv4SocketAddress where toString s = printToString s
