definition module Sapl.Target.JS.CodeGeneratorJS

/**
* SAPL to JS compiler.
*
* Two kinds: 
*   1. expression compiler (an expression doesn't contain any function definition),
*	2. full compiler
*
* Because for proper expression generation the code generator has to know whether
* the dependent functions have strict entry point, the exprGenerateJS has an optional
* argument of ParserState which contains this information. The necessary ParserState
* instance is made by generateJS which is supposed to generate the function definitions
* needed by the expression.
*/

import Text.StringAppender, Data.Error
import Sapl.Target.Flavour
from Sapl.SaplParser import :: ParserState

/**
* Convert SAPL function name to JS name
*
* @param function name prefix
* @param Sapl fn name
* @param String appender
* @return String appender
*/
escapeName :: !String !String !StringAppender -> StringAppender

/**
* Generates JS from Sapl source.
*
* @param Flavour
* @param Trampoline on/off
* @param Sapl source
* @param A global ParserState if any
* @return (JS source / error message, error)
*/
generateJS :: !Flavour !Bool !String !(Maybe ParserState) -> MaybeErrorString (StringAppender, ParserState)

/**
* Generates JS from Sapl source of sapl expression only. It may generate functions as well, what 
* is written to the output stream given as the last argument.
*
* @param Flavour
* @param Trampoline on/off
* @param souce of Sapl expression
* @param A global ParserState if any
* @param output stream for the (possibly) generated fucntions
* @return (JS source / error message, error)
*/
exprGenerateJS :: !Flavour !Bool !String !(Maybe ParserState) !StringAppender -> (MaybeErrorString (String, StringAppender, ParserState))

