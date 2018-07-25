implementation module Gast.Testable

/*
	GAST: A Generic Automatic Software Test-system
	
	testable: the test algorithm for logical properties

	Pieter Koopman, 2002-2010
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

import StdEnv

from Data.Func import $
import Data.Functor
from Data.List import instance Functor [], concatMap
import qualified Data.Map as Map
import Math.Random
import Testing.TestEvents
import Text
import Text.GenJSON
import Text.GenPrint
import Text.Language

import Gast.Gen
import Gast.GenLibTest
import Gast.ThunkNames

derive gLess Result
instance == Result where (==) x y = x===y

newAdmin :: Admin
newAdmin = { res=Undef, labels=[], args=[], argsRepresentation = [], failedAssertions = []
           , namePath = []
           , recFieldValueNrLimits = 'Map'.newMap
           }

instance Testable Bool where
    evaluate b genState result=:{Admin| args, argsRepresentation} =
        [{result & args = reverse args, argsRepresentation = reverse argsRepresentation, res = if b OK CE}]
	testname b = "Bool"

instance Testable Result where
    evaluate r genState result=:{Admin| args, argsRepresentation} =
        [{result & args = reverse args, argsRepresentation = reverse argsRepresentation, res = r}]
	testname r = "Result"

instance Testable Property
where
	evaluate (Prop _ p) genState result = p genState result
	testname (Prop n _) = n

instance Testable (a->b) | Testable b & genShow{|*|} a & ggen{|*|} a & TestArg a  
where
	evaluate f genState admin = forAll f (generateAll genState`) genState` admin
	where
		genState` = {GenState| genState & recFieldValueNrLimits = admin.Admin.recFieldValueNrLimits}
	testname f = thunk_name_to_string f

instance Testable [a] | Testable a  
where
	evaluate list genState admin = diagonal [ evaluate x genState admin \\ x<-list ] // copy the genState
	testname xs = "[" +++ join "," (map testname xs) +++ "]"

prop :: a -> Property | Testable a
prop p = Prop (testname p) (evaluate p)

forAll :: !(a->b) ![a] GenState !Admin -> [Admin] | Testable b & TestArg a
forAll f []   genState r=:{Admin| args} = [{r & args = reverse args, res = OK}] // to handle empty sets of values
forAll f list genState r = diagonal [apply f a genState r \\ a<-list ] // copy the genState

apply :: !(a->b) a GenState !Admin -> [Admin] | Testable b & TestArg a
apply f a genState r =
    evaluate (f a) genState {Admin| r & args = [show1 a:r.Admin.args], argsRepresentation = [printToString a : r.Admin.argsRepresentation]}

diagonal :: ![[a]] -> [a]
diagonal list = f 1 2 list []
where
	f n m [] [] = []
	f 0 m xs ys = f m (m+1) (rev ys xs) []
	f n m [] ys = f m (m+1) (rev ys []) []
	f n m [[x:r]:xs] ys = [x: f (n-1) m xs [r:ys]]
	f n m [[]:xs] ys = f (n-1) m xs ys
	
	rev []    accu = accu
	rev [x:r] accu = rev r [x:accu]

generateAll :: !GenState -> [a] | ggen{|*|} a // & genType{|*|} a
generateAll genState = l where l = ggen{|*|} genState // {genState & currType = hd (genType{|*|} [] [] l) }

derive gEq Result
derive bimap [], (,), (,,), (,,,), (,,,,), (,,,,,)

//--- testing ---//

:: Config =
	{ maxTests :: Int
	, maxArgs  :: Int
	, fails    :: Int
	, randoms  :: [Int]
	, genState :: GenState
	}

printEvents :: PrintConfig [GastEvent] -> [String]
printEvents pc [ge:ges] = case s of
	"" -> printEvents pc ges
	s  -> [s:printEvents pc ges]
where
	s = case ge of
		GE_TestStarted n               -> pc.beforeStartOutput n
		GE_TestFinished n r ces labels -> pc.resultOutput n r ces labels
		GE_CounterExample ce           -> pc.counterExampleOutput ce
		GE_Tick n adm                  -> pc.everyOutput n adm
printEvents _ [] = []

defaultTestConfig =
	{ maxTests = 1000
	, maxArgs  = 2000
	, fails    = 1
	, randoms  = aStream
	, genState = genState
	}

verbosePrintConfig =
	{ everyOutput          = verboseEvery
	, counterExampleOutput = humanReadableCEOutput True False
	, beforeStartOutput    = noBeforeOutput
	, resultOutput         = humanReadableResOutput True
	}
verboseEvery n r = blank <+ n <+ ":" <+ join " " r.Admin.args

tracePrintConfig =
	{ everyOutput          = traceEvery
	, counterExampleOutput = humanReadableCEOutput False False
	, beforeStartOutput    = noBeforeOutput
	, resultOutput         = humanReadableResOutput True
	}
traceEvery n r = n <+ ":" <+ join " " r.Admin.args <+ "\n"

blank :: String
blank =: { createArray len ' ' & [0] = '\r', [len-1] = '\r' } where len = 81

countPrintConfig n =
	{ everyOutput          = countEvery n
	, counterExampleOutput = humanReadableCEOutput True True
	, beforeStartOutput    = noBeforeOutput
	, resultOutput         = humanReadableResOutput True
	}
countEvery :: !Int !Int Admin -> String
countEvery steps n r
| n rem steps == 0 = toString n +++ "\r"
| otherwise        = ""

quietPrintConfig =
	{ everyOutput          = noEveryOutput
	, counterExampleOutput = noCounterExampleOutput
	, beforeStartOutput    = noBeforeOutput
	, resultOutput         = humanReadableResOutput False
	}

testEventsPrintConfig =
	{ everyOutput          = noEveryOutput
	, counterExampleOutput = noCounterExampleOutput
	, beforeStartOutput    = jsonEventStart
	, resultOutput         = jsonEventEnd
	}

noCounterExampleOutput :: CounterExampleRes -> String
noCounterExampleOutput _ = ""

noBeforeOutput :: !String -> String
noBeforeOutput _ = ""

noEveryOutput :: !Int Admin -> String
noEveryOutput n _ = ""

humanReadableCEOutput :: Bool Bool CounterExampleRes -> String
humanReadableCEOutput newLine showArgs {maxTests, nTests, nE, args, name, failedAssertions} = concat $
	if showArgs [(maxTests-nTests+1) <+ ":" <+ join " " args] [] ++
	if newLine ["\n"] [] ++
	[ showName True name
	, "Counterexample "
	, toString (nE+1)
	, " found after "
	, pluralisen English (maxTests-nTests+1) "test"
	, ": "
	, join " " args
	, "\n"
	: concatMap showFailedAssertion failedAssertions
	]
where
	showFailedAssertion :: !(!FailedAssertion, !String, !String) -> [String]
	showFailedAssertion (ExpectedRelation _ rel _, x, y) = ["not (", x, " ", toString rel, " ", y, ")\n"]

humanReadableResOutput :: Bool String TestsResult [CounterExampleRes] [(String, Int)] -> String
humanReadableResOutput addWhite name {maxTests, nRej, resultType} _ labels = withBlank $ showName True name +++ resStr
where
	resStr = case resultType of
		Proof nTests -> "Proof: " +++ msgStr +++ conclude nTests 0 labels
		with
			msgStr = if (nRej == 0) "success for all arguments" "success for all non-rejected arguments"
		PassedTest maxArgs nTests nUnd allArgsGenerated -> msgStr +++ conclude nTests nUnd labels
		with
			msgStr
			| allArgsGenerated = "Passed: success for arguments"
			| nTests == 0      = "Passed"
			| otherwise        = "Passed: maximum number of arguments (" <+ maxArgs <+ ") generated"
		CounterExpls nTests nUnd nE -> pluralisen English nE "counterexample" +++ " found" +++ conclude nTests nUnd labels
		Undefined nUnd -> "Undefined: no success nor counterexample found, all tests rejected or undefined" +++ conclude maxTests nUnd labels
		NoTests maxArgs nTests nUnd -> "No tests performed, maximum number of arguments (" <+ maxArgs <+ ") generated" +++ conclude nTests nUnd labels

	withBlank x
	| addWhite  = blank +++ x
	| otherwise = x

	conclude :: Int Int [(String, Int)] -> String
	conclude ntests nund labels
		# n    = maxTests-ntests
		# rest = showLabels n (sort labels)
		# rest = case nRej of
			0 -> rest
			n -> [", ", pluralisen English n "case", " rejected": rest]
		# rest = case nund of
			0 -> rest
			n -> [", ", pluralisen English n "case", " undefined": rest]
		| n==0
			= concat rest
			= concat [" after ",pluralisen English n "test":rest]

	showLabels :: !Int ![(String,Int)] -> [String]
	showLabels ntests [] = ["\n"]
	showLabels 0      [(lab,n):rest] = ["\n",lab,": ",toString n:showLabels 0 rest]
	showLabels ntests [(lab,n):rest] = ["\n",lab,": ",toString n," (",toString (toReal (n*100)/toReal ntests),"%)":showLabels ntests rest]

jsonEventStart :: !String -> String
jsonEventStart name = toString (toJSON {StartEvent | name=name}) +++ "\n"

jsonEventEnd :: String TestsResult [CounterExampleRes] [(String, Int)] -> String
jsonEventEnd name res counterExamples labels = toString (toJSON endEvent) +++ "\n"
where
	endEvent =
		{ name    = showName False name
		, event   = eventType
		, message = concat
			[ humanReadableResOutput False name res counterExamples labels
			: map (humanReadableCEOutput False False) counterExamples
			]
		}

	eventType = case res.resultType of
		Proof _            -> Passed
		PassedTest _ _ _ _ -> Passed
		CounterExpls _ _ _ -> Failed $ Just $ CounterExamples $
			(\ce ->
				{ counterExample   = map GPrint ce.CounterExampleRes.argsRepresentation
				, failedAssertions = fst3 <$> ce.CounterExampleRes.failedAssertions
				}
			) <$> counterExamples
		Undefined _        -> Failed Nothing
		NoTests _ _ _      -> Failed Nothing

showName :: Bool String -> String
showName quoteName l = if quoteName ("\"" <+ l <+ "\" ") l

toPrintConfig :: ([PrintOption] -> PrintConfig)
toPrintConfig = foldl handleOption verbosePrintConfig
where
	handleOption pc Verbose          = verbosePrintConfig
	handleOption pc Trace            = tracePrintConfig
	handleOption pc (Concise n)      = countPrintConfig n
	handleOption pc Quiet            = quietPrintConfig
	handleOption pc OutputTestEvents =
		{ pc
		& everyOutput          = noEveryOutput
		, counterExampleOutput = noCounterExampleOutput
		, resultOutput         = jsonEventEnd
		, beforeStartOutput    = jsonEventStart
		}

derive genShow Testoption, GenType

Test :: ![Testoption] !p -> [GastEvent] | Testable p
Test options p = testConfig config.randoms {config & randoms = []} p
where
	config = foldl handleOption defaultTestConfig options

	handleOption c (Tests i)      = {c & maxTests = i, maxArgs = 2*i}
	handleOption c (Fails i)      = {c & fails = i}
	handleOption c (Args i)       = {c & maxArgs = i}
	handleOption c (RandomSeed i) = {c & randoms = genRandInt i}
	handleOption c (RandomList r) = {c & randoms = r}
	handleOption c (MaxDepth i)   = {c & genState = {c.genState & maxDepth = i}}
	handleOption c (Skew s)
	| s > 0     = {c & genState = {c.genState & skewl = 1, skewr = s}}
	| s < 0     = {c & genState = {c.genState & skewl = ~s, skewr = 1}}
	| otherwise = {c & genState = {c.genState & skewl = 1, skewr = 1}}
	handleOption _ o = abort ("Test: unknown option \"" +++ show1 o +++ "\"\n")

TestList :: ![Testoption] ![p] -> [GastEvent] | Testable p
TestList options ps = flatten (map (Test options) ps)

test :: !p -> [String] | Testable p
test p = testn 1000 p

testn :: !Int !p -> [String] | Testable p
testn n p = verbosen n aStream p

testnm :: !Int !Int !p -> [String] | Testable p
testnm n m p = printEvents verbosePrintConfig $ testConfig aStream { defaultTestConfig & maxTests = n, maxArgs = 100*n, fails = m } p

ttestn :: !Int !p -> [String] | Testable p
ttestn n p = printEvents verbosePrintConfig $ testConfig aStream { defaultTestConfig & maxTests = n, maxArgs = 100*n } p

ttestnm :: !Int !Int !p -> [String] | Testable p
ttestnm n m p = printEvents verbosePrintConfig $ testConfig aStream { defaultTestConfig & maxTests = n, maxArgs = 100*n, fails = m } p

verbose  :: !RandomStream !p -> [String] | Testable p
verbose rs p = printEvents verbosePrintConfig $ testConfig rs defaultTestConfig p

verbosen :: !Int !RandomStream !p -> [String] | Testable p
verbosen n rs p = printEvents verbosePrintConfig $ testConfig rs { defaultTestConfig & maxTests = n, maxArgs = 100*n } p

concise :: !RandomStream !p -> [String] | Testable p
concise rs p = printEvents (countPrintConfig 100) $ testConfig rs defaultTestConfig p

concisen   :: !Int !RandomStream !p -> [String] | Testable p
concisen n rs p = printEvents (countPrintConfig 100) $ testConfig rs { defaultTestConfig & maxTests = n, maxArgs = 100*n } p

quiet :: !RandomStream !p -> [String] | Testable p
quiet rs p = printEvents quietPrintConfig $ testConfig rs defaultTestConfig p

quietn   :: !Int !RandomStream !p -> [String] | Testable p
quietn n rs p = printEvents quietPrintConfig $ testConfig rs { defaultTestConfig & maxTests = n, maxArgs = 100*n } p

quietnm   :: !Int !Int !RandomStream !p -> [String] | Testable p
quietnm n m rs p = printEvents quietPrintConfig $ testConfig rs { defaultTestConfig & maxTests = n, maxArgs = 100*n, fails = m } p

testEvents :: !RandomStream !p -> [String] | Testable p
testEvents rs p = printEvents testEventsPrintConfig $ testConfig rs defaultTestConfig p

testEventsn :: !Int !RandomStream !p -> [String] | Testable p
testEventsn n rs p = printEvents testEventsPrintConfig $ testConfig rs { defaultTestConfig & maxTests = n, maxArgs = 100*n } p

testConfig :: RandomStream Config p -> [GastEvent] | Testable p
testConfig rs {maxTests,maxArgs,fails,genState} p
	# res = evaluate p genState newAdmin
	= [GE_TestStarted (testname p):analyse res maxTests maxArgs 0 0 0 [] []]
where
	analyse :: ![.Admin] !Int !Int !Int !Int !Int [CounterExampleRes] ![(String,Int)] -> [GastEvent]
	analyse results nTests nArgs nRej nUnd nE counterExamples labels =
		case analyse` results nTests nArgs nRej nUnd nE of
			// testing of property finished
			Just resType -> [GE_TestFinished
				(testname p)
				{maxTests = maxTests, nRej = nRej, resultType = resType}
				counterExamples
				labels]
			// continue with testing property
			Nothing ->
				let [res:rest] = results in
				[GE_Tick (maxTests-nTests+1) res:case res.res of
					OK   -> analyse rest (nTests-1) (nArgs-1) nRej nUnd nE counterExamples (admin res.labels labels)
					Pass -> analyse rest (nTests-1) (nArgs-1) nRej nUnd nE counterExamples (admin res.labels labels) // NOT YET CORRECT ?
					CE   -> [GE_CounterExample counterExample:more]
					with
						counterExample =
							{ maxTests           = maxTests
							, nTests             = nTests
							, nE                 = nE
							, args               = res.Admin.args
							, argsRepresentation = res.Admin.argsRepresentation
							, name               = let n = testname p in join "." [n:dropWhile ((==) n) (reverse res.namePath)]
							, failedAssertions   = res.Admin.failedAssertions
							}
						more | nE+1<fails
							= analyse rest (nTests-1) (nArgs-1) nRej nUnd (nE+1) [counterExample: counterExamples] (admin res.labels labels)
							= [GE_TestFinished
								(testname p)
								{ maxTests   = maxTests
								, nRej       = nRej
								, resultType = CounterExpls (nTests - 1) nUnd (nE + 1)
								}
								[counterExample: counterExamples]
								(admin res.labels labels)]
					Rej   -> analyse rest nTests (nArgs-1) (nRej+1) nUnd     nE counterExamples labels
					Undef -> analyse rest nTests (nArgs-1) nRej     (nUnd+1) nE counterExamples labels
					_     -> abort "Error in Gast: analyse; missing case for result\n"
				]

	// there should always be a result for empty result lists!
	analyse` :: ![.Admin] !Int !Int !Int !Int !Int -> Maybe ResultType
	analyse` [] ntests nargs nrej 0    0  = Just $ Proof ntests
	analyse` [] ntests nargs nrej nund 0
	| ntests==maxTests                    = Just $ Undefined nund
	| otherwise                           = Just $ PassedTest maxArgs ntests nund True
	analyse` [] ntests nargs nrej nund ne = Just $ CounterExpls ntests nund ne
	analyse` _  0      nargs nrej nund 0  = Just $ PassedTest maxArgs 0 nund False
	analyse` _  0      nargs nrej nund ne = Just $ CounterExpls 0 nund ne
	analyse` _  ntests 0     nrej nund 0
	| ntests == maxTests                  = Just $ NoTests maxArgs ntests nund
	| otherwise                           = Just $ PassedTest maxArgs ntests nund False
	analyse` _  ntests 0     nrej nund ne = Just $ CounterExpls ntests nund ne
	analyse` _  _      _     _    _    _  = Nothing

	admin :: ![String] ![(String,Int)] -> [(String,Int)]
	admin [] accu = accu
	admin [label:rest] accu = admin rest (insert label accu)

	insert :: !String ![(String,Int)] -> [(String,Int)]
	insert label [] = [(label,1)]
	insert label [this=:(old,n):rest]
	 | label==old
		= [(old,n+1):rest]
		= [this:insert label rest]
