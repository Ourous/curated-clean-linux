implementation module Gast.CommandLine

import StdBool
from StdFunc import flip, o
import StdList
import StdString
import StdTuple

import Control.Monad => qualified join
import Data.Either
import Data.Error
from Data.Func import $
import Text.GenParse
import Text.GenPrint
import Data.List
import Data.Tuple
import System.CommandLine
import System.Options
import System.File
import Testing.Options
import Text

import Gast

instance getOptions a where getOptions _ = []
instance getPrintOptions a where getPrintOptions _ = []

instance Testable (o1, o2, a) | Testable a
where
	evaluate (_,_,p) g a = evaluate p g a
	testname (_,_,p) = testname p

instance getOptions ([Testoption], a, b) where getOptions (opts,_,_) = opts
instance getPrintOptions (a, [PrintOption], b) where getPrintOptions (_,opts,_) = opts

instance Testable ExposedProperty
where
	evaluate (EP p) g a = evaluate p g a
	testname (EP p) = testname p

instance getOptions ExposedProperty where getOptions (EP p) = getOptions p

exposeProperties :: ![PrintOption] ![Testoption] ![a] !*World -> *World | Testable, getOptions a
exposeProperties printopts globopts ps w
# ([_:opts], w) = getCommandLine w
# opts = parseOptions optionDescription opts gDefault{|*|}
| isError opts = error (join "\n" $ fromError opts) w
# opts = fromOk opts
# (io,w) = stdio w
| opts.test_options.list
	# io = foldl (<<<) io [testname p +++ "\n" \\ p <- ps]
	# (_,w) = fclose io w
	= w
# ps = case opts.test_options.runs of
	[] -> map (tuple3 [] []) ps
	rs -> [ let opts = map (fromOk o parseOpt) r.options in
		( [o \\ Left  o <- opts]
		, [o \\ Right o <- opts]
		, p
		) \\ r <- rs, p <- ps | r.TestRun.name == testname p]
# ps = filter (\p -> not (isMember (testname p) opts.test_options.skip)) ps
# (io,w) = seqSt (test opts.global_print_options opts.global_options) ps io w
# (_,w) = fclose io w
= w
where
	error :: !String !*World -> *World
	error s w
	# io = stderr
	# io = io <<< s <<< "\n"
	# (_,w) = fclose io w
	# w = setReturnCode 1 w
	= w

	// Specialized for the type of `test`
	seqSt :: !(a .st1 -> .(.st2 -> .(.st1,.st2))) ![a] !.st1 !.st2 -> .(!.st1, !.st2)
	seqSt f [x:xs] st1 st2
	# (st1,st2) = f x st1 st2
	# (st1,st2) = seqSt f xs st1 st2
	= (st1,st2)
	seqSt _ [] st1 st2 = (st1, st2)

	test :: ![PrintOption] ![Testoption] !p !*File !*World -> *(!*File, !*World) | getOptions, getPrintOptions, Testable p
	test popts opts p io w
	# events = Test (globopts ++ opts ++ getOptions p) p
	= stream (toPrintConfig (printopts ++ popts ++ getPrintOptions p)) events io w
	where
		stream :: !PrintConfig ![GastEvent] !*File !*World -> *(!*File, !*World)
		stream pc [ge:ges] io w
		# io = foldl (\io ev -> snd $ fflush $ io <<< ev) io $ printEvents pc [ge]
		# w = case ge of
			GE_TestFinished _ {resultType=CounterExpls _ _ _} _ _ -> setReturnCode 1 w
			GE_TestFinished _ {resultType=Undefined _}        _ _ -> setReturnCode 1 w
			_                                                     -> w
		= stream pc ges io w
		stream _  [] io w = (io,w)

	optionDescription :: Option Options
	optionDescription = WithHelp True $ Options
		[ Shorthand "-O" "--option" $ AddHelpLines
			[ "Tests N:            the maximum number of tests to run"
			, "Fails N:            the maximum number of failing test cases to collect"
			, "Args N:             the maximum number of arguments to generate"
			, "RandomSeed N:       a custom random seed"
			, "Skew N:             0 for symmetric test generation; positive for right-skewn generation; negative for left-skewn generation"
			, "MaxDepth N:         the maximum tree depth in generated test cases"
			, "Output options:"
			, "- Quiet:            only show the end result"
			, "- Concise N:        show a test counter for every N tests"
			, "- Verbose:          show every test case, then hide it again"
			, "- Trace:            show every test case"
			, "- OutputTestEvents: output JSON test events as in Testing.TestEvents"
			] $ Option
			"--option"
			// When no runs are given yet, this gives global options
			(\opt opts -> parseOpt opt >>= \opt -> case opts.test_options.runs of
				[] -> case opt of
					Left  o -> Ok {opts & global_options=opts.global_options ++ [o]}
					Right o -> Ok {opts & global_print_options=opts.global_print_options ++ [o]}
				rs -> let r = last rs in
					Ok {opts & test_options.runs=init rs ++ [{r & options=r.options ++ [print opt]}]}
				with
					print (Left o)  = printToString o
					print (Right o) = printToString o
			)
			"OPT"
			"Add OPT to the options of the previously added test, where OPT is one of:"
		, Shorthand "-r" "--run" $ Option
			"--run"
			// Remove tests previously added with the same name, to make it possible to add --run after --run-all
			(\r opts -> if (isMember r allnames)
				(Ok {opts & test_options.runs=[r` \\ r` <- opts.test_options.runs | r`.TestRun.name <> r]++ [{name=r, options=[]}]})
				(Error ["No test with the name '" +++ r +++ "' is known."]))
			"NAME"
			"Run test NAME (see --list for a list of names)"
		, Shorthand "-R" "--run-all" $ Flag
			"--run-all"
			// Only add those tests which do not exist yet in the runs list
			(\opts -> let existing = [r.TestRun.name \\ r <- opts.test_options.runs] in Ok
				{opts & test_options.runs=opts.test_options.runs
				++ [{name=r,options=[]} \\ r <- allnames | not (isMember r existing)]})
			"Run all tests (which have not been mentioned with --run yet)"
		, Biject (\r->r.test_options) (\old r -> {old & test_options=r}) testOptionDescription
		]
	where
		allnames = map testname ps

:: Options =
	{ test_options         :: !TestOptions
	, global_options       :: ![Testoption]
	, global_print_options :: ![PrintOption]
	}

derive gDefault Options, Testoption, GenType, PrintOption
derive gParse Testoption, GenType, PrintOption
derive gPrint Testoption, GenType, PrintOption

parseOpt :: !String -> MaybeError [String] (Either Testoption PrintOption)
parseOpt s = case parseString s of
	Just o  -> Ok (Left o)
	Nothing -> case parseString s of
		Just o  -> Ok (Right o)
		Nothing -> Error ["Could not parse '" +++ s +++ "' as test option"]
