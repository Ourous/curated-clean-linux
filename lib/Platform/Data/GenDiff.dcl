definition module Data.GenDiff

import StdGeneric
from StdOverloaded import class ==

/**
 * The difference between two values.
 */
:: Diff =
	{ status   :: !DiffStatus //* Whether this node is common, added or removed.
	, value    :: !String     //* A String representation of the value.
	, children :: ![Diff]     //* Diffs on the childrens of this node.
	}

/**
 * The status of a node head in a {{`Diff`}}.
 */
:: DiffStatus
	= Common    //* The complete node is common to both values
	| Changed   //* The node head is common, but there are diffs in the children
	| OnlyRight //* The node only exists in the right argument
	| OnlyLeft  //* The node only exists in the left argument

instance == DiffStatus

/**
 * Recursively set the status in a Diff.
 */
setStatus :: DiffStatus Diff -> Diff

/**
 * Compute the {{`Diff`}} between two values.
 */
generic gDiff a :: a a -> [Diff]
derive gDiff UNIT, PAIR, EITHER, OBJECT, CONS of d, RECORD of d, FIELD of d
derive gDiff Int, Char, Bool, Real, String
derive gDiff [], [!], [ !], [!!], {}, {!}
derive gDiff (,), (,,), (,,,), (,,,,), (,,,,,), (,,,,,,), (,,,,,,,)

/**
 * A String representation of a {{`Diff`}} using ANSI escape codes.
 */
diffToConsole :: [Diff] -> String
