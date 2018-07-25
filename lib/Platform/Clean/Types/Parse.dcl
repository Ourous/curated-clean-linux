definition module Clean.Types.Parse

/**
 * A parser for Clean types.
 */

from Clean.Types import :: Type
from Data.Maybe import :: Maybe

/**
 * Parse a Clean type.
 */
parseType :: ![Char] -> Maybe Type
