implementation module Clean.ModuleFinder

import StdArray
import StdList
import StdString

import Clean.Parse.ModuleName
import Data.Error
from Data.Func import $, mapSt
import System.Directory
import System.Environment
import System.File
import System.FilePath
import System.Options
import System.OS
import Text

defaultModuleFindingOptions :: !*World -> *(!ModuleFindingOptions, !*World)
defaultModuleFindingOptions w
# (home,w) = getEnvironmentVariable "CLEAN_HOME" w
# home = fromMaybe (IF_WINDOWS "C:\\Clean" "/opt/clean") home
# opts =
	{ include_paths        = []
	, include_libraries    = ["StdEnv"]
	, clean_home           = home
	, include_applications = False
	}
= (opts, w)

moduleFindingOptionDescription :: Option ModuleFindingOptions
moduleFindingOptionDescription = Options
	[ Shorthand "-I" "--include" $ Option
		"--include"
		(\dir opts -> Ok {opts & include_paths=opts.include_paths ++ [dir]})
		"DIR"
		"Add DIR to the include path"
	, Shorthand "-IL" "--include-library" $ Option
		"--include-library"
		(\lib opts -> Ok {opts & include_libraries=opts.include_libraries ++ [lib]})
		"LIB"
		"Add CLEAN_HOME/lib/LIB to the include path"
	, Shorthand "-H" "--clean-home" $ Option
		"--clean-home"
		(\h opts -> Ok {opts & clean_home=h})
		"PATH"
		"Set CLEAN_HOME to PATH (used to find libraries)"
	, Flag
		"--include-applications"
		(\opts -> Ok {opts & include_applications=True})
		"Include modules for which no definition module exists"
	]

baseDirectories :: !ModuleFindingOptions -> [FilePath]
baseDirectories opts =
	opts.include_paths ++
	[opts.clean_home </> "lib" </> lib \\ lib <- opts.include_libraries]

findModule :: !String !ModuleFindingOptions !*World -> *(![FilePath], !*World)
findModule mod opts w
# (exis,w) = mapSt fileExists candidates w
= ([p \\ p <- candidates & exi <- exis | exi], w)
where
	moddir = {if (c == '.') pathSeparator c \\ c <-: mod}

	candidates = [dir </> moddir +++ ext \\ dir <- baseDirectories opts]
	with ext = if opts.include_applications ".icl" ".dcl"

findAllModules :: !ModuleFindingOptions !*World -> *(![OSError], ![FilePath], !*World)
findAllModules opts w
# (errs,(paths,w)) = mapSt (\d -> scanDirectory` (collect d) d) (baseDirectories opts) ([], w)
= (flatten errs,paths,w)
where
	scanDirectory` f dir (st,w) = (err, (st`,w`)) where (err, st`, w`) = scanDirectory f st dir w

	collect dir fp fi seen w
	| endsWith (if opts.include_applications ".icl" ".dcl") fp
		# (modname, w) = guessModuleName fp w
		| isError modname = (seen, w)
		# modname = fromOk modname
		| isNothing modname = (seen, w)
		# modname = fromJust modname
		# expected = {if (c == pathSeparator) '.' c \\ c <-: fp % (size dir`, size fp - 5)}
			with dir` = dir </> ""
		| modname == expected
			= ([fp:seen], w)
			= (seen, w)
	| otherwise
		= (seen, w)
