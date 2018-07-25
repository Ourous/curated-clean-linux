definition module Sapl.Linker.LazyLinker

import Sapl.Linker.SaplLinkerShared

/**
* Lazy SAPL linker
*
* Starting from a initial expression, generates the necessary dependency
* functions recursively.
* It can use a LoaderState. Passing the same LoaderState to the subsequent
* calls, multiple referenced functions will be linked only once.

* It's lazy, because only those modules touched to which have reference
* from some functions.
*/

import StdString, Text.StringAppender

:: LoaderState
:: LoaderStateExt :== (LoaderState, FuncTypeMap, SkipSet)

/**
* Generate loader state:
* It looks for modules in the given directories hierarchically.
* Modules given in "_override_" directories are handled with priority compared to the same
* module in the parent directory.
*
* @param Directory list to look up modules
* @param Individual modules given in a list (must be top level)
* @param Module names to be excluded
*/
generateLoaderState :: ![String] ![String] ![String] !*World -> *(LoaderStateExt, !*World)

/**
* Link an expression using a LoaderState
*
* @param LoaderState
* @param StringAppender as the output stream
* @param the expression to link for
* @param *IWorld for accessing referenced modules
* @return new LoaderState
* @return new output stream of generated source code with dependencies
* @return the original expression after macro substitution 
* @return *IWorld
*/
linkByExpr :: !LoaderStateExt !StringAppender !String !*World -> *(!LoaderStateExt, !StringAppender, !String, !*World)

/**
* Extract warnings from loader state
*
* @param LoaderState
* @return List of Warning messages if any
*/
getWarnings :: !LoaderStateExt -> [String]

