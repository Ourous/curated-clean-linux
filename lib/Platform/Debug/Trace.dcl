definition module Debug.Trace

/**
 * This module provides functions to trace values to output channels. Other
 * useful functions can be found in StdEnv's {{StdDebug}}.
 */

/**
 * Prints the value to stdout and returns it. Other than the functions in
 * {{StdDebug}}, this does not require {{`toString`}}.
 */
trace_stdout :: !.a -> .a
