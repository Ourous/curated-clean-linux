definition module Clean.Parse

/**
 * A small wrapper around the parser of the Clean compiler.
 * You will need to have the source of the Clean compiler available in your path.
 */

from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe
from System.FilePath import :: FilePath

from hashtable import :: HashTable
from Heap import :: Heap
from syntax import :: Module, :: ParsedDefinition, :: ParsedModule

/**
 * Parse a Clean module.
 *
 * @param The path to the file to parse
 * @result
 *   The parsed module and the corresponding hash table.
 *   When the result is an {{`Error`}}, there is a descriptive error message.
 */
readModule :: !FilePath !*World -> *(!MaybeError String (ParsedModule, HashTable), !*World)
