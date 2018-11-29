implementation module Testing.Options

import StdList

import Data.Error
from Data.Func import $
import Data.GenDefault
import Data.Maybe
import System.Options

derive gDefault TestOptions, TestRun

testOptionDescription :: Option TestOptions
testOptionDescription = WithHelp True $ Options
	[ Shorthand "-l" "--list" $ Flag
		"--list"
		(\opts -> Ok {opts & list=True})
		"List all available tests"
	, Shorthand "-O" "--option" $ Option
		"--option"
		(\opt opts -> case opts.runs of
			[] -> Error ["--option used before --run"]
			rs -> let r = last rs in
				Ok {opts & runs=init rs ++ [{r & options=r.options ++ [opt]}]})
		"OPT"
		"Add OPT to the options of the previously added test"
	, Shorthand "-r" "--run"  $ Option
		"--run"
		(\r opts -> Ok {opts & runs=opts.runs ++ [{name=r, options=[]}]})
		"NAME"
		"Run test NAME"
	, Shorthand "-s" "--skip" $ Option
		"--skip"
		(\r opts -> Ok {opts & skip=[r:opts.skip]})
		"NAME"
		"Skip test NAME"
	]
