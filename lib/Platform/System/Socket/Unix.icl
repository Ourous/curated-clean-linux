implementation module System.Socket.Unix

import StdEnv
import Data.Error
import System.FilePath
import System.Socket
import System._Pointer
from System._Socket import AF_UNIX

instance SocketAddress UnixSocketAddress where
	sa_serialize sa p w
		# p = writeInt2 p 0 (sa_domain sa)
		# p = writeCharArray (p+2) (packString sa.unix_socket_path)
		= (p, w)
	sa_deserialize p
		= Ok {unix_socket_path=derefString (p+2)}
	sa_length _ = 110
	sa_domain _ = AF_UNIX
	sa_null = {unix_socket_path="/"}

instance toString UnixSocketAddress where toString s = s.unix_socket_path
