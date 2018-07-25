definition module Clean.PrettyPrint.Expression

from syntax import :: ParsedExpr, :: Rhs, :: OptGuardedAlts

from Clean.PrettyPrint.Util import class print

instance print ParsedExpr, Rhs

/**
 * `True` iff the right-hand side is a {{`GuardedAlts`}} or {{`UnguardedExpr`}}
 * with at least one {{`ewl_node`}}.
 */
compound_rhs :: !OptGuardedAlts -> Bool
