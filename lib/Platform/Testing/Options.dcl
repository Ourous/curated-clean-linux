definition module Testing.Options

from Data.GenDefault import generic gDefault
from System.Options import :: Option

/**
 * Basic options of a test program.
 *
 * When `runs` is empty, the program should in principle run all tests.
 * However, `skip` overrides everything; tests with a name in `skip` should
 *   never be run.
 * When `list` is set, the program should list the names of the tests on stdout
 *   (one per line) and exit with 0 exit code.
 */
:: TestOptions =
	{ runs     :: ![TestRun] //* Specific tests to run
	, skip     :: ![String]  //* Tests to skip
	, list     :: !Bool      //* List all tests
	}

/**
 * A specific test to run.
 */
:: TestRun =
	{ name       :: !String   //* The name of the test
	, options    :: ![String] //* Extra options
	}

derive gDefault TestOptions, TestRun

/**
 * An Option descriptor for {{`parseOptions`}} to parse command line arguments
 * as a {{`TestOptions`}} record.
 */
testOptionDescription :: Option TestOptions
