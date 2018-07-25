definition module Clean.PrettyPrint

/**
 * Pretty-printer for types in the Clean compiler.
 */

from syntax import
	:: AType,
	:: ParsedDefinition,
	:: ParsedExpr,
	:: Rhs,
	:: Type,
	:: TypeContext

/**
 * Pretty-printer.
 *
 * @var The type to print
 */
class cpp t where
	/**
	 * Normal pretty-printer.
	 * @param The value to print
	 * @result A string representation of the parameter
	 */
	cpp :: !t -> String

	/**
	 * Pretty-printer which places parentheses around the result if necessary.
	 * @param The value to print
	 * @result A string representation of the parameter
	 */
	cppp :: !t -> String

instance cpp
	AType,
	ParsedDefinition,
	ParsedExpr,
	Rhs,
	Type,
	TypeContext
