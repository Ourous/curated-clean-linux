implementation module System.Options

import StdArray
import StdBool
import StdClass
from StdFunc import flip, o
import StdList
import StdOrdList
import StdString

import Control.Applicative
import Data.Error
from Data.Func import $
import Data.Functor
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Foldable
from Text import class Text(join,rpad), instance Text String

instance Alternative (MaybeError [String])
where
	empty = Error ["Unknown error"]

	(<|>) r=:(Ok _) _ = r
	(<|>) (Error e) (Error _) = Error e
	(<|>) (Error _) r = r

showHelpText :: ![HelpText] -> String
showHelpText help = join "\n" $ map show help
where
	maxLeftWidth = maxList $ map leftWidth help

	leftWidth :: HelpText -> Int
	leftWidth (OptionHelpText opts args _ _) =
		sum (map size opts) + length opts - 1 + 1 +
		sum (map size args) + length args - 1
	leftWidth (OperandHelpText var _ _) = size var + 1

	show (OptionHelpText opts args help add) =
		rpad (join "/" opts +++ " " +++ join " " args) (maxLeftWidth + 2) ' ' +++
		join "\n  " [help:add]
	show (OperandHelpText var help add) =
		rpad var (maxLeftWidth + 2) ' ' +++ join "\n  " [help:add]

parseOptions :: !(t opts) ![String] !opts -> MaybeError [String] opts | OptionDescription t
parseOptions p args opts = parse (optParser p) args defaultState opts
where
	defaultState =
		{ operands_state = NoOperandsSeenYet
		}

	parse _ [] st opts = Ok opts
	parse p ["--":args] st opts = parse p args {st & operands_state=OperandsIncludingHyphens} opts
	parse optp=:(OptParser p) args st opts = case p args st opts of
		Nothing                  -> Error ["Unknown option '" +++ hd args +++ "'"]
		Just (Error es)          -> Error es
		Just (Ok (opts,st,rest)) -> parse optp rest st opts

instance OptionDescription Option
where
	optParser :: !(Option opts) -> OptParser opts
	optParser (Flag f upd _) = OptParser \args st opts -> case args of
		[arg:args] | arg == f -> case st.operands_state of
			NoOperandsSeenYet -> Just $ (\opts -> (opts,st,args)) <$> upd opts
			_ -> Just $ Error ["Flag " +++ f +++ " not allowed after operands"]
		_ -> Nothing
	optParser (Option opt upd _ _) = OptParser \args st opts -> case args of
		[arg:args] | arg == opt -> case st.operands_state of
			NoOperandsSeenYet -> case args of
				[arg:args] -> Just $ (\opts -> (opts,st,args)) <$> upd arg opts
				[]         -> Just $ Error ["'" +++ opt +++ "' requires an argument"]
			_ -> Just $ Error ["Option " +++ opt +++ " not allowed after operands"]
		_ -> Nothing
	optParser (Operand acceptsHyphens upd _ _) = OptParser \args st opts = case args of
		[arg:args]
			| arg.[0] == '-' && not acceptsHyphens && not (st.operands_state=:OperandsIncludingHyphens) -> Nothing
			| otherwise -> case upd arg opts of
				Nothing        -> Nothing
				Just (Error e) -> Just (Error e)
				Just (Ok opts) -> Just $ Ok (opts, upd_st, args)
				with
					upd_st = case st.operands_state of
						NoOperandsSeenYet -> {st & operands_state=OnlyOperands}
						_                 -> st
		[] -> Nothing
	optParser (Shorthand short long child) = OptParser \args st opts ->
		let (OptParser p`) = optParser child in
		p` (map (\arg -> if (arg == short) long arg) args) st opts
	optParser (Options ps) = OptParser \args st opts -> case catMaybes [p args st opts \\ p <- optps] of
		[]  -> Nothing
		res -> Just ('Data.Foldable'.foldr1 (<|>) res)
	where
		optps = [p \\ OptParser p <- map optParser ps]
	optParser wh=:(WithHelp short p) = OptParser \args st opts -> case args of
		[h:args] | isMember h ["--help":if short ["-h"] []]
			-> Just $ Error [showHelpText $ cleanupHelpText $ helpText wh]
		_   -> let (OptParser p`) = optParser p in p` args st opts
	optParser (Biject fr to optp) = OptParser \args st opts ->
		fmap (appFst3 (to opts)) <$> p args st (fr opts)
	where (OptParser p) = optParser optp
	optParser (AddHelpLines _ p) = optParser p

	helpText (Flag a _ h) = [OptionHelpText [a] [] h []]
	helpText (Option o _ n h) = [OptionHelpText [o] [n] h []]
	helpText (Operand _ _ v h) = [OperandHelpText v h []]
	helpText (Shorthand short long child) = map upd (helpText child)
	where
		upd :: HelpText -> HelpText
		upd oht=:(OptionHelpText opts args help add)
		| isMember long opts = OptionHelpText (opts ++ [short]) args help add
		| otherwise          = oht
		upd oht=:(OperandHelpText var help add) = oht
	helpText (Options ps) = concatMap helpText ps
	helpText (WithHelp short p) =
		[ OptionHelpText ["--help":if short ["-h"] []] [] "Show this help text" []
		: helpText p
		]
	helpText (Biject _ _ p) = helpText p
	helpText (AddHelpLines lines p) = case helpText p of
		[OptionHelpText opts args help add:rest] -> [OptionHelpText opts args help (add ++ lines):rest]
		[OperandHelpText var help add:rest]      -> [OperandHelpText var help (add ++ lines):rest]
		[]                                       -> []

cleanupHelpText :: ![HelpText] -> [HelpText]
cleanupHelpText [oht=:OptionHelpText opts _ _ _:rest] = [oht:cleanupHelpText $ catMaybes $ map cleanup rest]
where
	cleanup :: HelpText -> Maybe HelpText
	cleanup (OptionHelpText opts` args help add) = case [o \\ o <- opts` | not (isMember o opts)] of
		[]   -> Nothing
		opts -> Just $ OptionHelpText opts args help add
	cleanup ht = Just ht
cleanupHelpText [oht=:OperandHelpText _ _ _:rest] = [oht:cleanupHelpText rest]
cleanupHelpText [] = []
