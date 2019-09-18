definition module System.GetOpt

/*

This library provides facilities for parsing the command-line options
in a standalone program. It is essentially a Clean port of the GNU 
@getopt@ library.
 
Ported from Haskell System.Console.GetOpt module to Clean by 
László Domoszlai <dlacko@pnyf.inf.elte.hu> Feb. 2013. 

http://www.haskell.org/ghc/docs/latest/html/libraries/base/src/System-Console-GetOpt.html

Originally written by Sven Panne:

Sven Panne <Sven.Panne@informatik.uni-muenchen.de> Oct. 1996 (small
changes Dec. 1997)

Two rather obscure features are missing: The Bash 2.0 non-option hack
(if you don't already know it, you probably don't want to hear about
it...) and the recognition of long options with a single dash
(e.g. '-help' is recognised as '--help', as long as there is no short
option 'h').

Other differences between GNU's getopt and this implementation:

* To enforce a coherent description of options and arguments, there
  are explanation fields in the option/argument descriptor.

* Error messages are now more informative, but no longer POSIX
  compliant... :-(

And a final Haskell advertisement: The GNU C implementation uses well
over 1100 lines, we need only 195 here, including a 46 line example! 
:-)

*/

import Data.Maybe

// What to do with options following non-options
:: ArgOrder a
  = RequireOrder                // ^ no option processing after first non-option
  | Permute                     // ^ freely intersperse options and non-options
  | ReturnInOrder (String -> a) // ^ wrap non-options into options

/*
 * Each 'OptDescr' describes a single option.
 * The arguments to 'Option' are: 
 * - list of short option characters
 * - list of long option strings (without \"--\")
 * - argument descriptor
 * - explanation of option for user
 */
:: OptDescr a =                 // description of a single options:
   Option [Char]                //    list of short option characters
          [String]              //    list of long option strings (without "--")
          (ArgDescr a)          //    argument descriptor
          String                //    explanation of option for user

// Describes whether an option takes an argument or not, and if so
// how the argument is injected into a value of type @a@.
:: ArgDescr a
	= NoArg                   a         // ^   no argument expected
    | ReqArg (String       -> a) String // ^   option requires argument
    | OptArg ((Maybe String) -> a) String // ^   optional argument

// | Return a string describing the usage of a command, derived from
// the header (first argument) and the options described by the 
// second argument.
usageInfo :: String                    // header
             [OptDescr a]              // option descriptors
          -> String                    // nicely formatted decription of options

/*
Process the command-line, and return the list of values that matched
(and those that didn\'t). The arguments are:

* The order requirements (see 'ArgOrder')

* The option descriptions (see 'OptDescr')

* The actual command line arguments (presumably got from 
  'System.Environment.getArgs').

'getOpt' returns a triple consisting of the option arguments, a list
of non-options, and a list of error messages.
*/
getOpt :: (ArgOrder a)                 // non-option handling
          [OptDescr a]                 // option descriptors
          [String]                     // the command-line arguments
       -> ([a],[String],[String])      // (options,non-options,error messages)

/*
This is almost the same as 'getOpt', but returns a quadruple
consisting of the option arguments, a list of non-options, a list of
unrecognized options, and a list of error messages.
*/
getOpt` :: (ArgOrder a)                       // non-option handling
           [OptDescr a]                       // option descriptors
          ![String]                           // the command-line arguments
        -> ([a],[String], [String] ,[String]) // (options,non-options,unrecognized,error messages)
