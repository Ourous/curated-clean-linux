definition module System.Directory

from Data.Tree import :: RTree
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from System.File import :: FileInfo
from System.FilePath import :: FilePath
from System.OSError import :: MaybeOSError, :: OSError, :: OSErrorCode, :: OSErrorMessage

/**
 * Ensure a directory exists by creating all non-existant parent directories.
 */
ensureDirectoryExists :: !FilePath !*World -> (!MaybeOSError (), !*World)

/**
 * Delete a file/directory and, if it is a directory, all its contents.
 */
recursiveDelete :: !FilePath !*World -> *(!MaybeOSError (), !*World)

/**
 * Recursively scan a directory and collect information about all files and
 * directories it contains.
 *
 * @param A function that is used to update a state (`st`) for every file or
 *   directory encountered. A directory is given before everything it contains.
 *   Otherwise, no guarantees w.r.t. order are made.
 * @param The initial state.
 * @param The directory to scan. The update function is also called with this
 *   directory as an argument.
 * @param The world.
 * @result All errors encountered.
 * @result The updated state.
 * @result The new world.
 */
scanDirectory :: !(FilePath FileInfo .st *World -> *(.st, *World)) !.st !FilePath !*World -> *(![OSError], !.st, !*World)

/**
 * Create an {{RTree}} from a root for a directory structure.
 * E.g. to use it in iTasks in a {{SelectFromTree}}
 * The filepaths are complete to make it easy to retrieve a path.
 *
 * @param Root directory to start in.
 * @param Maximum depth -1 for unbounded
 * @param The world.
 * @result A tree of filepaths with either an error or fileinfo
 * @result The new world.
 */
readDirectoryTree :: !FilePath !(Maybe Int) !*World -> *(RTree (FilePath, MaybeOSError FileInfo), !*World)

/**
 * Create a directory
 *
 * @param Path for the directory.
 * @param The world.
 * @result Maybe an error.
 * @result The new world.
 */
createDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)

/**
 * Remove a directory
 *
 * @param Path for the directory.
 * @param The world.
 * @result Maybe an error.
 * @result The new world.
 */
removeDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)

/**
 * Read a directory
 *
 * @param Path for the directory.
 * @param The world.
 * @result Maybe an error or a list of relative filepaths.
 * @result The new world.
 */
readDirectory :: !FilePath !*w -> (!MaybeOSError [FilePath], !*w)

/**
 * Retrieve the current working directory
 *
 * @param The world.
 * @result Maybe an error or the current directory.
 * @result The new world.
 */
getCurrentDirectory :: !*w -> (!MaybeOSError FilePath, !*w)

/**
 * Change the current working directory
 *
 * @param The new working directory.
 * @param The world.
 * @result Maybe an error.
 * @result The new world.
 */
setCurrentDirectory :: !FilePath !*w -> (!MaybeOSError (), !*w)
