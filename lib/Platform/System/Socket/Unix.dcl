definition module System.Socket.Unix

from System.FilePath import :: FilePath(..)
from StdOverloaded import class toString
from System.Socket import class SocketAddress

:: UnixSocketAddress =
	{ unix_socket_path :: !FilePath
	}
instance SocketAddress UnixSocketAddress
instance toString UnixSocketAddress
