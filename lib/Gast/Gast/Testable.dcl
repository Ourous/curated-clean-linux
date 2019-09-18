definition module Gast.Testable

/*
	GAST: A Generic Automatic Software Test-system
	
	testable: the test algorithm for logical properties

	Pieter Koopman, 2002-2012
	Radboud Universty, Nijmegen
	The Netherlands
	pieter@cs.ru.nl
*/

from StdMaybe import :: Maybe(Nothing)
import Gast.GenLibTest 
from Gast.StdProperty import ::Property // for instance of testable
import Gast.Gen
from Testing.TestEvents import :: TestLocation, :: CounterExample, :: FailedAssertion
from Text.GenPrint import class PrintOutput, :: PrintState, generic gPrint

//--- basics --//

:: Admin =
	{ labels                :: ![String]
	, args                  :: ![String]
	, argsRepresentation    :: ![String]
	, namePath              :: ![String]
	, res                   :: !Result
	, failedAssertions      :: ![(FailedAssertion, String, String)] //* Failed assertion & string representation of args
	, recFieldValueNrLimits :: !Map (TypeName, RecFieldName) Int    //* Restricts the number of values generated for record fields
	}
:: Result = Undef | Rej | Pass | OK | CE
newAdmin :: Admin

derive gLess Result
instance == Result

:: Property = Prop String (Maybe TestLocation) (GenState Admin -> [Admin])

prop :: a -> Property | Testable a

class TestArg a | genShow{|*|}, ggen{|*|}, gPrint{|*|} a

class Testable a
where
	evaluate :: !a GenState !Admin -> [Admin]
	testname :: a -> String

	testlocation :: a -> Maybe TestLocation
	testlocation _ = Nothing

instance Testable Bool
instance Testable Result
instance Testable Property
///instance Testable (a->b) | Testable b & genShow{|*|} a & ggen{|*|} a //TestArg a  
instance Testable (a->b) | Testable b & genShow{|*|} a & ggen{|*|} a & TestArg a  
instance Testable [a] | Testable a  

//derive bimap [], (,), (,,), (,,,), (,,,,), (,,,,,)

MaxExists	:== 1000

//--- for generating lists of elements ---//

//aStream :: RandomStream
//split :: !RandomStream -> (RandomStream,RandomStream)

//--- for implementation of properties ---//

diagonal :: ![[a]] -> [a]
forAll :: !(a->b) ![a] GenState !Admin -> [Admin] | Testable b & TestArg a
//split :: !RandomStream -> (RandomStream,RandomStream)
//generateAll :: !RandomStream -> [a] | ggen{|*|} a
generateAll :: !GenState -> [a] | ggen{|*|} a //& genType{|*|} a

//--- testing --//

:: Testoption
	= Tests           !Int
	| Fails           !Int
	| Args            !Int
	| RandomSeed      !Int
	| RandomList      ![Int]
	| Skew            !Int
	| Bent
	| MaxDepth        !Int
	| MaxStringLength !Int
	| ArgTypes        ![GenType]

/**
 * The combined results of all tests for a single property.
 * This is in contrast to {{Result}} which represents the result of a single test.
 */
:: TestsResult = { maxTests   :: !Int        //* Maximum number of tests
                 , nRej       :: !Int        //* Rejected test arguments
                 , resultType :: !ResultType //* Type of the result
                 }
/**
 * The type of the combined result, together with information
 * specific to that type of result.
 */
:: ResultType = Proof        !Int                 //* Proof by exhaustive testing: nTests
              | PassedTest   !Int !Int !Int !Bool //* Passed test: maxArgs, nTests, nUnd, all possible args generated?
              | CounterExpls !Int !Int !Int       //* Counterexamples found: nTests nUnd nCounterExamples
              | Undefined    !Int                 //* Undefined result: nUnd
              | NoTests      !Int !Int !Int       //* No tests performed: maxArgs nTests nUnd

/**
 * A counter example.
 */
:: CounterExampleRes =
	{ maxTests           :: !Int      //* Maximal number of tests for run in which counter example is found
	, nTests             :: !Int      //* maxTests MINUS number of test at which counter example is found
	, nE                 :: !Int      //* Number of counter example
	, args               :: ![String] //* Arguments used for test (string representation)
	, argsRepresentation :: ![String] //* Arguments used for test ({{`gPrint`}} encoding)
	, name               :: !String   //* Name of property
	, failedAssertions   :: ![(FailedAssertion, String, String)] //* Failed assertions leading to counter example & string representation of arguments
	}

:: GastEvent
	= GE_TestStarted !(Maybe TestLocation) !String
	| GE_TestFinished !(Maybe TestLocation) !String !TestsResult ![CounterExampleRes] ![(String,Int)]
	| GE_CounterExample !CounterExampleRes
	| GE_Tick !Int !Admin

:: PrintOption
	= Verbose
	| Trace
	| Concise Int //* The Int tells how often a test count should be displayed
	| Quiet
	| OutputTestEvents //* output test results as event specified in clean-platform {{Testing.TestEvents}}

:: PrintConfig =
	{ everyOutput          :: Int Admin -> String
	, counterExampleOutput :: CounterExampleRes -> String
	, beforeStartOutput    :: (Maybe TestLocation) String -> String
	, resultOutput         :: (Maybe TestLocation) String TestsResult [CounterExampleRes] [(String, Int)] -> String
	}

printEvents :: PrintConfig [GastEvent] -> [String]
toPrintConfig :: ([PrintOption] -> PrintConfig)

Test     :: ![Testoption] !p   -> [GastEvent] | Testable p
TestList :: ![Testoption] ![p] -> [GastEvent] | Testable p

verbose     ::      !RandomStream !p -> [String] | Testable p
verbosen    :: !Int !RandomStream !p -> [String] | Testable p
concise     ::      !RandomStream !p -> [String] | Testable p
concisen    :: !Int !RandomStream !p -> [String] | Testable p
quiet       ::      !RandomStream !p -> [String] | Testable p
quietn      :: !Int !RandomStream !p -> [String] | Testable p
quietnm     :: !Int !Int !RandomStream !p -> [String] | Testable p
testEvents  ::      !RandomStream !p -> [String] | Testable p
testEventsn :: !Int !RandomStream !p -> [String] | Testable p

test :: !p -> [String] | Testable p              // test p 1000 times
testn :: !Int !p -> [String] | Testable p        // maxnumber of tests
ttestn :: !Int !p -> [String] | Testable p       // maxnumber of tests, trace all arguments
testnm :: !Int !Int !p -> [String] | Testable p  // maxnumber of tests, max number of errors
ttestnm :: !Int !Int !p -> [String] | Testable p // maxnumber of tests, max number of errors
