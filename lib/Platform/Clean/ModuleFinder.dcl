definition module Clean.ModuleFinder

/**
 * This module provides functionality to search for Clean modules in the file
 * system.
 */

from System.FilePath import :: FilePath
from System.Options import :: Option
from System.OSError import :: OSError, :: OSErrorMessage, :: OSErrorCode

/**
 * Options to tweak the searching for Clean modules.
 */
:: ModuleFindingOptions =
	{ include_paths        :: ![FilePath] //* Complete paths to search in (clm's `-I`)
	, include_libraries    :: ![String]   //* Libraries to search in (combined with CLEAN_HOME; clm's `-IL`)
	, clean_home           :: !FilePath   //* Override CLEAN_HOME
	, include_applications :: !Bool       //* Whether to include modules that do not have a definition module or not
	}

/**
 * Get the default {{`ModuleFindingOptions`}}. This requires the World because
 * {{`clean_home`}} needs to be set correctly, for which the `CLEAN_HOME`
 * environment variable is read.
 */
defaultModuleFindingOptions :: !*World -> *(!ModuleFindingOptions, !*World)

/**
 * An option description ({{System.Options}}) for {{`ModuleFindingOptions`}},
 * supporting clm's `-I` and `-IL`, as well as long forms, possibility to
 * override `CLEAN_HOME`, etc.
 */
moduleFindingOptionDescription :: Option ModuleFindingOptions

/**
 * Find a specific module in the file system.
 *
 * @param The module name
 * @param The options to search for the module
 * @result A list of all matching file paths
 */
findModule :: !String !ModuleFindingOptions !*World -> *(![FilePath], !*World)

/**
 * Find all modules in the file system.
 *
 * @param The options to search for modules
 * @result {{`OSError`}}s that occurred during searching
 * @result File paths of all modules found
 */
findAllModules :: !ModuleFindingOptions !*World -> *(![OSError], ![FilePath], !*World)
