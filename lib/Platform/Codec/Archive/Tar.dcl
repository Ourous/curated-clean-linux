definition module Codec.Archive.Tar

/**
 * Functions to parse and untar tar archives.
 */

from StdFile import class FileSystem
from StdOverloaded import class ==

from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe

from System.File import :: FileError
from System.FilePath import :: FilePath

/**
 * A tarball is a list of tar files. The tar files may contain errors to allow
 * lazy parsing.
 */
:: TarBall :== [MaybeTarError TarFile]

/**
 ** A file in a tar archive.
 */
:: TarFile
	= { tar_name              :: [Char]
	  , tar_mode              :: Int
	  , tar_owner_id          :: Int
	  , tar_group_id          :: Int
	  , tar_size              :: Int
	  , tar_last_modification :: Int
	  , tar_file_type         :: TarFileType
	  , tar_link_to           :: Maybe [Char]
	  , tar_content           :: Maybe [Char]
	  , ustar                 :: Bool
	  , ustar_owner_name      :: Maybe [Char]
	  , ustar_group_name      :: Maybe [Char]
	  }

:: TarFileType
	= NormalFile
	| HardLink
	| SymLink
	| CharSpecial
	| BlockSpecial
	| Directory
	| FIFO
	| Contiguous
	| UnknownFileType

:: TarError
	= InvalidNumeral
	| UnsupportedFileTypeId Char
	| UnsupportedFileType TarFileType
	| FileError FileError
	| OtherError String
	| UnexpectedEOS

:: MaybeTarError a :== MaybeError TarError a

instance == TarFileType
instance == TarError

/**
 * Parse a tar archive from a list of characters.
 */
parseTar :: ![Char] -> TarBall

/**
 * Untar a tar archive.
 * @param A function to transform file names before untarring.
 */
unTar :: (FilePath -> FilePath) !TarBall !*env -> ([TarError], *env) | FileSystem env

/**
 * Read a tar archive from a file. This is basically a wrapper around
 * {{`parseTar`}} which reads a file from disk. The resulting {{`TarBall`}} may
 * still contain {{`Error`}}s.
 */
readTar :: !FilePath !*env -> *(MaybeError FileError TarBall, *env) | FileSystem env

/**
 * Untar a tar archive from a file to disk.
 * @param A function to transform file names before untarring.
 * @param The file name of the archive to untar.
 * */
unTarFile :: (FilePath -> FilePath) !FilePath !*env -> (MaybeError FileError [TarError], *env) | FileSystem env
