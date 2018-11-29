implementation module System.Directory

import StdBool
import StdString
import StdFunctions

import Data.Error
import Data.Func
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tree
import Data.Tuple
import Control.Monad
import qualified System._Directory
import System.File
import System.FilePath

ensureDirectoryExists :: !FilePath !*World -> (!MaybeOSError (), !*World)
ensureDirectoryExists fp w
# (err,w) = case takeDirectory fp of
	""     -> (Ok (), w)
	parent -> ensureDirectoryExists parent w
| isError err = (err,w)
# (exi,w) = fileExists fp w
| exi
	= (Ok (), w)
	= createDirectory fp w

recursiveDelete :: !FilePath !*World -> *(!MaybeOSError (), !*World)
recursiveDelete fp w
	# (mfi, w) = getFileInfo fp w
	| isError mfi = (liftError mfi, w)
	| (fromOk mfi).directory
		# (mdir, w) = readDirectory fp w
		| isError mdir = (liftError mdir, w)
		# (merr, w) = mapSt (\c->recursiveDelete (fp </> c))
			(filter (\r->r <> "." && r <> "..") (fromOk mdir)) w
		# merr = sequence merr
		| isError merr = (liftError merr, w)
		= removeDirectory fp w
	= deleteFile fp w

scanDirectory :: !(FilePath FileInfo .st *World -> *(.st, *World)) !.st !FilePath !*World -> *(![OSError], !.st, !*World)
scanDirectory upd st dir w = scan dir [] st w
where
	scan dir errs st w
	# (fi,w) = getFileInfo dir w
	# (errs,st,w) = case fi of
		Error e -> ([e:errs], st, w)
		Ok fi   -> (\(st,w) -> (errs,st,w)) $ upd dir fi st w
	| isError fi = (errs, st, w)
	# fi = fromOk fi
	| not fi.directory = (errs, st, w)
	# (contents,w) = readDirectory dir w
	| isError contents = ([fromError contents:errs], st, w)
	# contents = [dir </> fp \\ fp <- fromOk contents | fp <> "." && fp <> ".."]
	= seqSt3 scan contents errs st w

	seqSt3 f [] s1 s2 s3 = (s1, s2, s3)
	seqSt3 f [x:xs] s1 s2 s3
	# (s1,s2,s3) = f x s1 s2 s3
	= seqSt3 f xs s1 s2 s3

readDirectoryTree :: !FilePath !(Maybe Int) !*World -> *(RTree (FilePath, MaybeOSError FileInfo), !*World)
readDirectoryTree fp md w = scan (maybe -1 id md) fp "" w
where
	scan md acc fp w
	# fp = acc </> fp
	# (mfi, w) = getFileInfo fp w
	| isError mfi = (RNode (fp, liftError mfi) [], w)
	| md == 0 = (RNode (fp, mfi) [], w)
	# (Ok fi) = mfi
	| not fi.directory = (RNode (fp, mfi) [], w)
	# (mcs, w) = readDirectory fp w
	| isError mfi = (RNode (fp, liftError mcs) [], w)
	# (cs, w) = mapSt (scan (dec md) fp) (filter (\c->not (elem c [".", ".."])) (fromOk mcs)) w
	= (RNode (fp, Ok fi) cs, w)

createDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
createDirectory f w = 'System._Directory'.createDirectory f w

removeDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
removeDirectory f w = 'System._Directory'.removeDirectory f w

readDirectory :: !FilePath !*w -> (!MaybeOSError [FilePath], !*w)
readDirectory f w = 'System._Directory'.readDirectory f w

getCurrentDirectory :: !*w -> (!MaybeOSError FilePath, !*w)
getCurrentDirectory w = 'System._Directory'.getCurrentDirectory w

setCurrentDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
setCurrentDirectory f w = 'System._Directory'.setCurrentDirectory f w
