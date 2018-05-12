implementation module System._FilePath

import _SystemArray
import StdInt

import Data.Error
import System.OSError
import System._Pointer
import System._Posix

getFullPathName :: !String !*World -> (!MaybeOSError String, !*World)
getFullPathName relp w
	# buf     = createArray MAXPATHLEN '\0'
	# (res,w) = realpath (packString relp) buf w
	| res == 0
		= getLastOSError w
	| otherwise
		= (Ok (unpackString buf), w)
where
	realpath :: !String !String !*World -> (!Pointer, !*World)
	realpath path buf w = code {
		ccall realpath "ss:p:A"
	}
