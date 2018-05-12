definition module Sapl.Linker.SaplLinkerShared

/**
* Types and function definitions used by the different SAPL linker implementations
*/

import StdString, StdClass, Data.Maybe, Sapl.SaplTokenizer, Text.StringAppender
from Data.Map import :: Map
from Data.Set import :: Set

/**
* A line in a SAPL source file can be the following:
*/
:: LineType = LT_REDIRECT String 			  // e.g. names of data constructor redirect to their type 
            | LT_FUNC String DependencyType   
            | LT_MACRO String DependencyType

:: DependencyType = DT_NO_DEPENDENCY | DT_NEED_PROCESS [Token]

// Function name <-> LineType
:: FuncTypeMap :== Map String LineType

:: IdGenerator :== Int
:: Warnings    :== [String]

/**
* Loads a function by its name, that is: String -> Maybe LineType + side effects
*
* @param inner state
* @param function name
* @param FuncTypeMap
* ...
*/
:: LoaderFunction st :== st String FuncTypeMap *World -> *(Maybe LineType, FuncTypeMap, st, *World)
:: Loader st         :== (LoaderFunction st, st)

// Skip the functions in generate_source whose names are in the set
:: SkipSet :== Set String

instance toString LineType

/**
* Appends the names of function dependencies to the list of second argument
* by the token stream of the first argument.
*
* @param token stream
* @param the list the dependencies to be appended
* @return extended dependency list
*/
generate_dependencies :: [Token] [String] -> [String]

/**
* Reads a modules, given by its name, and generates mappings and 
* possibly some warning messages (e.g. one of the modules is not found).
*
* @param module name
* @param initial FuncTypeMap (can be empty)
* @param initial message list 
* @param a numeric id which is unique between the calls of this function
* @param *World to access files 
* @return extended FuncTypeMap
* @return new numeric id to pass to the same function on next call
* @return possibly extended message list
* @return *World
*/
read_module :: !String FuncTypeMap Warnings IdGenerator !*World -> (FuncTypeMap, IdGenerator, Warnings, *World)

/**
* Generates SAPL source of a given function (by name) including its dependencies.
* This is a general function, the concrete loader logic is done by the function given in its second argument.
*
* @param initial FuncTypeMap
* @param loader function (and its current state)
* @param the function name
* @param *World
* @param output stream
* @return new FuncTypeMap (loader my changed it)
* @return loader function (and its new state)
* @return updated output stream
* @return *World
*/
generate_source :: !FuncTypeMap !SkipSet !(Loader lst) !String !StringAppender !*World -> *(!FuncTypeMap, !SkipSet, !(Loader lst), !StringAppender, !*World)

/**
* Substitute macros in a given expression.
*
* @param initial FuncTypeMap
* @param dependencies of the expression
* @param loader function (and its current state)
* @param the expression
* @param *World
* @param output stream
* @return new FuncTypeMap (loader my changed it)
* @return loader function (and its new state)
* @return *World
* @return updated output stream
*/
substitute_macros :: !FuncTypeMap ![String] !(Loader st) !String !StringAppender !*World -> (!FuncTypeMap, !(Loader st), !StringAppender, !*World)

				 

