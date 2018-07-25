definition module Clean.Parse.Comments

/**
 * Functions to deal with (documentation) comments in Clean programs.
 * You will need the Clean compiler in your path.
 */

from StdFile import class FileSystem

from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from System.File import :: FileError
from System.FilePath import :: FilePath

from syntax import :: Ident, :: Module, :: ParsedDefinition, :: ParsedModule

/**
 * A comment in a Clean program.
 */
:: CleanComment =
	{ line      :: !Int
	, column    :: !Int
	, level     :: !Maybe Int //* Nothing for single-line comments, otherwise the nesting level
	, content   :: !String //* All content except `//` or `/*` and `*/`
	, multiline :: !Bool
	}

/**
 * Scan all comments from a Clean program given the filename.
 * Also see {{`scanCommentsFile`}}.
 */
scanComments :: !FilePath !*env -> *(!MaybeError FileError [CleanComment], !*env) | FileSystem env

/**
 * Scan all comments from a Clean program given a readable {{`File`}}.
 * Also see {{`scanComments`}}.
 */
scanCommentsFile :: !*File -> *(!MaybeError FileError [CleanComment], !*File)

/**
 * Clean comments linked to the definitions in a {{`ParsedModule`}} of the
 * Clean compiler.
 */
:: CollectedComments

emptyCollectedComments :: CollectedComments

/**
 * Get the comment content for an identifier.
 */
getComment :: !Ident !CollectedComments -> Maybe String

/**
 * Match a list of comments (see {{`scanComments`}}) to a {{`ParsedModule`}}
 * (see {{`readModule`}} in {{`Clean.Parse`}}).
 */
collectComments :: ![CleanComment] !ParsedModule -> CollectedComments
