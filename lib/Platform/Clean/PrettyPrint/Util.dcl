definition module Clean.PrettyPrint.Util

/**
 * Utility functions for the pretty-printer. Normally, these need not be used
 * directly, as the cpp class in Clean.PrettyPrint is sufficient for most use
 * cases.
 */

from StdOverloaded import class zero, class +++(+++)

:: CPPState
	= { cpp_indent :: !Int
	  , cpp_parens :: !Bool
	  }

:: PrintList
	= PrintNil
	| E.t u: (:+:) infixl 0 !t !u & print t & print u

class print t where
	print :: !CPPState !t -> String

	printp :: !CPPState !t -> String | print t
	printp st x :== if st.cpp_parens ("(" +++ print st x +++ ")") (print {st & cpp_parens=True} x)

class Join e where
	join :: !CPPState t !e -> String | print t
	isNil :: !e -> Bool

	join_start :: !CPPState !t !e -> String | print t
	join_start st glue elems :== if (isNil elems) "" (print st glue) +++ join st glue elems

instance zero CPPState

instance print String, Int, [t] | print t, CPPState, PrintList

instance Join [u] | print u
