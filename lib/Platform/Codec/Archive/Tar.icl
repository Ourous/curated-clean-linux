implementation module Codec.Archive.Tar

import StdArray
import StdBool
import StdChar
import StdClass
import StdFile
from StdFunc import const, o
import StdInt
import StdList
import StdOverloaded
import StdString
import StdTuple

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Error
from Data.Func import $
import Data.GenEq
import Data.Functor
import Data.Maybe
import Data.Tuple
import System.Directory
import System.File
import System.FilePath

:: TarParser t :== StateT [Char] (MaybeError TarError) t

derive gEq TarFileType, TarError, FileError
instance == TarFileType where (==) a b = a === b
instance == TarError where (==) a b = a === b

parseTar :: ![Char] -> TarBall
parseTar cs
| length start <> 1024
	= [Error UnexpectedEOS]
| all ((==) '\0') start
	= []
| otherwise
	= case runStateT parse cs of
		(Error e)   = [Error e]
		(Ok (f,cs)) = [Ok f:parseTar cs]
where
	start = take 1024 cs

parse :: TarParser TarFile
parse = parseHeader >>= parseContent

parseContent :: TarFile -> TarParser TarFile
parseContent f = StateT $ \s -> let (cont,rest) = split f.tar_size [] s in pure
	( {f & tar_content = Just $ cont}
	, drop (if (f.tar_size rem 512 == 0) 0 (512 - f.tar_size rem 512)) rest)
where
	// Tail-recursive splitAt; otherwise we need O(n) stack size.
	split :: Int [a] [a] -> ([a],[a])
	split 0 xs ys     = (reverse xs,ys)
	split n xs []     = (reverse xs, [])
	split n xs [y:ys] = split (n-1) [y:xs] ys

parseHeader :: TarParser TarFile
parseHeader =
	parseString 100 >>= \pName ->
	parseOctal 8 >>= \pMode ->
	parseOctal 8 >>= \pOwner ->
	parseOctal 8 >>= \pGroup ->
	parseOctal 12 >>= \pSize ->
	parseOctal 12 >>= \pTime ->
	skip 8 *> // Checksum
	parseType >>= \pType ->
	parseString 100 >>= \pLinkedTo ->
	parseString 6 >>= \ustar ->
		let pIsUstar = ['ustar'] == takeWhile ((<>)' ') ustar in
	skip 2 *> // Null bytes
	parseString 32 >>= \pOwnerName ->
	parseString 32 >>= \pGroupName ->
	skip 183 *> // Device numbers, filename prefix & padding
	pure
	{ tar_name              = pName
	, tar_mode              = pMode
	, tar_owner_id          = pOwner
	, tar_group_id          = pGroup
	, tar_size              = pSize
	, tar_last_modification = pTime
	, tar_file_type         = pType
	, tar_link_to           = if (isEmpty pLinkedTo) Nothing (Just pLinkedTo)
	, tar_content           = Nothing
	, ustar                 = pIsUstar
	, ustar_owner_name      = if pIsUstar (Just pOwnerName) Nothing
	, ustar_group_name      = if pIsUstar (Just pGroupName) Nothing
	}

parseString :: Int -> TarParser [Char]
parseString n = StateT $ \s-> Ok $ appFst (takeWhile ((<>) '\0')) $ splitAt n s

parseOctal :: Int -> TarParser Int
parseOctal n = StateT $ \s ->
	let (h,t) = splitAt n s; s` = takeWhile (\c -> c <> '\0' && c <> ' ') h in
	if (all isOctDigit s`)
	(Ok (foldl ((+) o ((*) 8)) 0 $ dropWhile ((==) 0) $ map digitToInt s`,t))
	(Error InvalidNumeral)
where
	isOctDigit c = '0' <= c && c <= '7'

parseType :: TarParser TarFileType
parseType = StateT $ \cs -> case cs of
	[]     -> Error UnexpectedEOS
	[c:cs] -> case c of
		'0' = Ok (NormalFile, cs)
		'1' = Ok (HardLink, cs)
		'2' = Ok (SymLink, cs)
		'3' = Ok (CharSpecial, cs)
		'4' = Ok (BlockSpecial, cs)
		'5' = Ok (Directory, cs)
		'6' = Ok (FIFO, cs)
		'7' = Ok (Contiguous, cs)
		c   = Error $ UnsupportedFileTypeId c

skip :: Int -> TarParser ()
skip i = StateT $ \cs -> Ok ((), drop i cs)

readTar :: !FilePath !*env -> *(MaybeError FileError TarBall, *env) | FileSystem env
readTar f w = case readFile f w of
	(Ok s, w)    = (Ok $ parseTar s, w)
	(Error e, w) = (Error $ e, w)

readFile :: !FilePath !*env -> *(MaybeError FileError [Char], *env) | FileSystem env
readFile f w
# (ok,f,w) = fopen f FReadData w
| not ok = (Error CannotOpen, w)
# (cs,f) = readAll f
# (ok,w) = fclose f w
| not ok = (Error CannotClose, w)
= (cs, w)
where
	readAll :: *File -> *(MaybeError FileError [Char], *File)
	readAll f
	# (e,f) = fend f
	| e = (Ok [], f)
	# (s,f) = freads f 512
	| s == "" = (Error IOError, f)
	# (cs,f) = readAll f
	= (((++) (fromString s)) <$> cs, f)

unTar :: (FilePath -> FilePath) !TarBall !*env
	-> ([TarError], *env) | FileSystem env
unTar namef tb w = untarAll tb w
where
	untarAll :: TarBall *env -> *([TarError], *env) | FileSystem env
	untarAll [] w = ([], w)
	untarAll [Error e:tb] w
	# (es,w) = untarAll tb w
	= ([e:es],w)
	untarAll [Ok f:tb] w
	| f.tar_file_type == Directory
		# (e,w)  = createDirectory fname w
		= finish (if (isError e) [OtherError $ fname +++ ": " +++ (snd $ fromError e)] []) w
	| f.tar_file_type == NormalFile
		# (ok,fl,w) = fopen fname FWriteData w
		| not ok = finish [FileError CannotOpen] w
		# fl = writeAll (fromJust (f.tar_content <|> Just [])) fl
		# (ok,w) = fclose fl w
		| not ok = finish [FileError CannotClose] w
		= finish [] w
	| otherwise
		= finish [UnsupportedFileType f.tar_file_type] w
	where
		fname = namef $ toString f.tar_name

		finish :: [TarError] -> *env -> *([TarError], *env) | FileSystem env
		finish es = appFst ((++) es) o untarAll tb

		writeAll :: [Char] *File -> *File
		writeAll [] f = f
		writeAll cs f = writeAll t $ fwrites (toString h) f
		where (h,t) = splitAt 512 cs

unTarFile :: (FilePath -> FilePath) !FilePath !*env
	-> (MaybeError FileError [TarError], *env) | FileSystem env
unTarFile namef f w
# (tb,w) = readTar f w
| isError tb = (Error $ fromError tb, w)
# (err,w) = unTar namef (fromOk tb) w
= (Ok err, w)
