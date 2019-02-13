definition module System.Options

from StdOverloaded import class toString

from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe

/**
 * The state of an {{`OptParser`}}.
 */
:: OptParserState =
	{ operands_state :: !ParsingOperandsState
	}

:: ParsingOperandsState
	= NoOperandsSeenYet
	| OnlyOperands
	| OperandsIncludingHyphens

/**
 * A parser for command line options.
 * The arguments are the list of command line arguments and the current
 * settings object. The result is:
 *  - `Nothing`, if the parser cannot be applied
 *  - `Just (Error es)`, if the parser finished with an error, where `es` is a
 *    list of errors/warning.
 *  - `Just (Ok (opts,state,args))`, if the parser succeeded, where `opts` is
 *    the new settings object, `state` the new {{`OptParserState`}} and `args`
 *    the list of the rest of the arguments.
 *
 * @var The settings object type
 */
:: OptParser opts
	= OptParser ([String] OptParserState opts -> Maybe (MaybeError [String] (opts, OptParserState, [String])))

/**
 * An element in the help text of a command line application.
 */
:: HelpText
	= OptionHelpText [String] [String] String [String]
		//* Help text for an option: the option variants, the meta variables, a description, additional lines
	| OperandHelpText String String [String]
		//* Help text for an operand: the meta variable, a description, additional lines

showHelpText :: ![HelpText] -> String

/**
 * Types instantiating this class can be combined to form new parsers.
 */
class OptionDescription t
where
	/**
	 * Create a parser from an option description.
	 * @param The option description
	 * @result The corresponding parser
	 */
	optParser :: !(t opts) -> OptParser opts

	/**
	 * The help text belonging to an option description.
	 * @param The option description
	 * @result The corresponding parser
	 */
	helpText :: !(t opts) -> [HelpText]

/**
 * Parse commnd line arguments.
 *
 * @param The option descriptions
 * @param The command line arguments (see {{`getCommandLine`}})
 * @param The default settings
 * @result Either a list of error/warning messages or the new settings object
 */
parseOptions :: !(t opts) ![String] !opts -> MaybeError [String] opts | OptionDescription t

/**
 * Basic command line options
 */
:: Option opts
	= Flag String (opts -> MaybeError [String] opts) String
		//* A flag is a command line option without arguments: flag, update function, help text
	| Option String (String opts -> MaybeError [String] opts) String String
		//* An option is a command line option with one argument: option, update function, meta variable, help text
	| Operand Bool (String opts -> Maybe (MaybeError [String] opts)) String String
		//* An operand follows all flags and options: accepts hyphens, update function, meta variable, help text
	| E.t: Shorthand String String (t opts) & OptionDescription t
		//* A shorthand translates a short option into a long one: short version, long version, child parser
	| E.t: Options [t opts] & OptionDescription t
		//* A collections of option descriptions
	| E.t: WithHelp Bool (t opts) & OptionDescription t
		//* Adds a --help option to display the help text. If the Bool is True, -h is used as well
	| E.a t: Biject (opts -> a) (opts a -> opts) (t a) & OptionDescription t
		//* Lift an option description to another domain (useful for combining different descriptions together)
	| E.t: AddHelpLines [String] (t opts) & OptionDescription t

instance OptionDescription Option

/**
 * Certain {{`Option`}} descriptors can override 'lower' option descriptors,
 * which can lead to incorrect information in {{`HelpText`}}s (for instance,
 * multiple options can have the same {{`Shorthand`}} if it is overridden. This
 * function removes these inconsistencies that can arise from combining
 * option descriptors together.
 */
cleanupHelpText :: ![HelpText] -> [HelpText]
