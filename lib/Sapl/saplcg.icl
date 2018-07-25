module saplcg

import StdFile
import StdList

from Data.Func import $, mapSt, seqSt
from Data.Map import fromList, newMap
import System.CommandLine
import System.Environment
import System.FilePath
import System.Options
import Text

import Sapl.Target.CleanFlavour
import Sapl.Target.JS.CodeGeneratorJS

:: Options =
	{ home       :: !Maybe FilePath
	, paths      :: ![FilePath]
	, libraries  :: ![String]
	, modules    :: ![String]
	, ignore     :: ![String]
	, trampoline :: !Bool
	}

defaultOptions :: Options
defaultOptions =
	{ home       = Nothing
	, paths      = ["."]
	, libraries  = ["StdEnv"]
	, modules    = []
	, ignore     = ["StdBool", "StdChar", "StdFile", "StdInt", "StdMisc", "StdReal", "StdString"]
	, trampoline = False
	}

defaultParserState :: ParserState
defaultParserState =
	{ ps_level        = 0
	, ps_constructors = fromList
		[ ("_predefined._Unit",   {index=0,nr_cons=1,nr_args=0,args=[]})
		, ("_predefined._Nil",    nil)
		, ("_predefined._|Nil",   nil)
		, ("_predefined._!Nil",   nil)
		, ("_predefined._Nil!",   nil)
		, ("_predefined._!Nil!",  nil)
		, ("_predefined._Cons",   cons)
		, ("_predefined._|Cons",  cons)
		, ("_predefined._!Cons",  cons)
		, ("_predefined._Cons!",  cons)
		, ("_predefined._!Cons!", cons)
		]
	, ps_functions    = newMap
	, ps_CAFs         = newMap
	, ps_genFuns      = []
	}
where
	nil = {index=0,nr_cons=2,nr_args=0,args=[]}
	cons = {index=1,nr_cons=2,nr_args=2,args=[TypedVar (NormalVar "x" 0) NoType, TypedVar (NormalVar "xs" 0) NoType]}

Start w
# ([prog:args],w) = getCommandLine w
# noUsage = Nothing
# usage = Just ("Usage: " +++ prog +++ " [OPTIONS] MOD [MOD..]")

# opts = defaultOptions
# (home,w) = getEnvironmentVariable "CLEAN_HOME" w
# opts = parseOptions optionDescription args {opts & home=home}
| isError opts = error noUsage (join "\n" $ fromError opts) w
# opts = fromOk opts

# opts & modules = removeMembers opts.modules opts.ignore
| isEmpty opts.modules = error usage "No modules given" w

# (files,(pst,w)) = mapSt (parseModule opts) opts.modules (defaultParserState,w)
| any isNothing files = error Nothing "Parsing failed" w

# (out,w) = stdio w
# (out,_,w) = genCode opts (join "\n" [f \\ Just f <- files]) (out,pst,w)
# (_,w) = fclose out w

= w
where
	optionDescription :: Option Options
	optionDescription = WithHelp True $ Options
		[ Shorthand "-H" "--clean-home" $ Option
			"--clean-home"
			(\h opts -> Ok {opts & home=Just h})
			"DIR"
			"Clean installation directory (default: $CLEAN_HOME)"
		, Shorthand "-IL" "--include-lib" $ Option
			"--include-lib"
			(\l opts -> Ok {opts & libraries=opts.libraries ++ [l]})
			"LIB"
			"Library to include when searching for modules"
		, Shorthand "-I" "--include" $ Option
			"--include"
			(\d opts -> Ok {opts & paths=opts.paths ++ [d]})
			"DIR"
			"Directory to include when searching for modules"
		, Shorthand "-t" "--trampoline" $ Flag
			"--trampoline"
			(\opts -> Ok {opts & trampoline=True})
			"Turn on trampoline code"
		, Operand False
			(\m opts -> Just $ Ok {opts & modules=opts.modules ++ [m]})
			"MODULE"
			"Modules to generate code for"
		]

error :: !(Maybe String) !String !*World -> *World
error usage s w
# io = stderr
# io = io <<< s <<< "\n"
# io = case usage of
	Nothing -> io
	Just u  -> io <<< u <<< "\n"
# (_,w) = fclose io w
# w = setReturnCode 1 w
= w

parseModule :: !Options !String !*(!ParserState,!*World) -> *(!Maybe String, !*(!ParserState,!*World))
parseModule opts mod (pst,w)
#! (fp,w) = findModule opts.paths opts.libraries w
| isNothing fp = (Nothing, (pst, error Nothing ("Could not find " +++ mod) w))
#! fp = fromJust fp
#! (f,w) = readFile fp w
| isError f = (Nothing, (pst, error Nothing (fromError f <+ " " +++ fp) w))
#! f = fromOk f
#! parseRes = parse (tokensWithPositions f)
| isError parseRes = (Nothing, (pst, error Nothing (mod +++ ": " <+ fromError parseRes) w))
#! (_,pst`) = fromOk parseRes
= (Just f, (mergeParserStates pst` (Just pst),w))
where
	modparts = split "." mod
	modPath = foldl (</>) "" (init modparts)
	modBasename = last modparts

	findModule :: ![FilePath] ![String] !*World -> *(!Maybe FilePath, !*World)
	findModule [] [lib:libs] w | isJust opts.home
	# (e,w) = fileExists filename w
	= if e (Just filename, w) (findModule [] libs w)
	where
		filename = (fromJust opts.home </> "lib" </> lib </> modPath </> modBasename +++ ".sapl")
	findModule [path:paths] libs w
	# (e,w) = fileExists filename w
	= if e (Just filename, w) (findModule paths libs w)
	where
		filename = (path </> modPath </> modBasename +++ ".sapl")
	findModule _ _ w = (Nothing, w)

genCode :: !Options !String !*(!*File,!ParserState,!*World) -> *(!*File,!ParserState,!*World)
genCode opts sapl (out,pst,w)
#! genResult = generateJS cleanFlavour opts.trampoline sapl (Just pst)
| isError genResult = (out, pst, error Nothing (fromError genResult) w)
#! (res,pst) = fromOk genResult
#! (mbError,out) = intoFile res out
#! out = out <<< "\n"
#! w = if (isError mbError) (error Nothing "Error while writing output\n" w) w
= (out,pst,w)
