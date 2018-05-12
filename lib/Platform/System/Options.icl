implementation module System.Options

import StdArray
import StdClass
from StdFunc import flip, o
import StdList
import StdOrdList
import StdString

import Data.Error
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
from Text import class Text(join,rpad), instance Text String

instance toString HelpText
where
	toString (OptionHelpText opts args help additional) = join " "
		[ join "/" opts
		, join " " args
		, join "\n  " [help:additional]
		]

showHelpText :: [HelpText] -> String
showHelpText help = join "\n" $ map (\(OptionHelpText opts args help add) ->
		rpad (join "/" opts +++ " " +++ join " " args) (maxLeftWidth + 2) ' '
		+++ join "\n  " [help:add]) help
where
	maxLeftWidth = maxList $ map leftWidth help

	leftWidth :: HelpText -> Int
	leftWidth (OptionHelpText opts args _ _) =
		sum (map size opts) + length opts - 1 + 1 +
		sum (map size args) + length args - 1

parseOptions :: (t opts) [String] opts -> MaybeError [String] opts | OptionDescription t
parseOptions p args opts = parse (optParser p) args opts
where
	parse (OptParser p) []   opts = Ok opts
	parse (OptParser p) args opts = case p args opts of
		Nothing               -> Error ["Unknown option '" +++ hd args +++ "'"]
		Just (Error es)       -> Error es
		Just (Ok (opts,rest)) -> parse (OptParser p) rest opts

instance OptionDescription Option
where
	optParser :: (Option opts) -> OptParser opts
	optParser (Flag f upd _) = OptParser \args opts -> case args of
		[arg:args] -> if (arg <> f)
			Nothing
			(Just $ flip tuple args <$> upd opts)
		[] -> Nothing
	optParser (Option opt upd _ _) = OptParser \args opts -> case args of
		[arg:args] -> if (arg <> opt)
			Nothing
			(case args of
				[arg:args] -> Just $ flip tuple args <$> upd arg opts
				[]         -> Just $ Error ["'" +++ opt +++ "' requires an argument"])
		[] -> Nothing
	optParser (Shorthand short long child) = OptParser \args opts ->
		let (OptParser p`) = optParser child in
		p` (map (\arg -> if (arg == short) long arg) args) opts
	optParser (Options ps) = OptParser \args opts -> case catMaybes [p args opts \\ p <- optps] of
		[]      -> Nothing
		[res:_] -> Just res
	where
		optps = [p \\ OptParser p <- map optParser ps]
	optParser wh=:(WithHelp short p) = OptParser \args opts -> case args of
		[h:args] | isMember h ["--help":if short ["-h"] []]
			-> Just $ Error [showHelpText $ cleanupHelpText $ helpText wh]
		_   -> let (OptParser p`) = optParser p in p` args opts
	optParser (Biject fr to optp) = OptParser \args opts ->
		fmap (appFst (to opts)) <$> p args (fr opts)
	where (OptParser p) = optParser optp
	optParser (AddHelpLines _ p) = optParser p

	helpText (Flag a _ h) = [OptionHelpText [a] [] h []]
	helpText (Option o _ n h) = [OptionHelpText [o] [n] h []]
	helpText (Shorthand short long child) = map upd (helpText child)
	where
		upd :: HelpText -> HelpText
		upd oht=:(OptionHelpText opts args help add)
		| isMember long opts = OptionHelpText (opts ++ [short]) args help add
		| otherwise          = oht
	helpText (Options ps) = concatMap helpText ps
	helpText (WithHelp short p) =
		[ OptionHelpText ["--help":if short ["-h"] []] [] "Show this help text" []
		: helpText p
		]
	helpText (Biject _ _ p) = helpText p
	helpText (AddHelpLines lines p) = case helpText p of
		[OptionHelpText opts args help add:rest] -> [OptionHelpText opts args help (add ++ lines):rest]
		[]                                       -> []

cleanupHelpText :: [HelpText] -> [HelpText]
cleanupHelpText [oht=:OptionHelpText opts _ _ _:rest] = [oht:cleanupHelpText $ catMaybes $ map cleanup rest]
where
	cleanup :: HelpText -> Maybe HelpText
	cleanup (OptionHelpText opts` args help add) = case [o \\ o <- opts` | not (isMember o opts)] of
		[]   -> Nothing
		opts -> Just $ OptionHelpText opts args help add
cleanupHelpText [] = []
