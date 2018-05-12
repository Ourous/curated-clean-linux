implementation module iTasks.Internal.Test.Definition
import iTasks, StdFile, StdMisc
import iTasks.Extensions.Image
import iTasks.UI.Editor, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Common, iTasks.UI.Definition
import iTasks.Extensions.Editors.Ace
import iTasks.Internal.Serialization
import Text, Text.HTML, System.CommandLine
import qualified Data.Map as DM
import iTasks.Extensions.Development.Codebase
import Data.Func, Data.Either, Data.Error

from iTasks.Internal.IWorld import createIWorld, destroyIWorld, initJSCompilerState, ::IWorld{options} 
from iTasks.Internal.TaskStore import createTaskInstance, taskInstanceOutput, :: TaskOutput, :: TaskOutputMessage
from iTasks.Internal.TaskEval import evalTaskInstance
from iTasks.Internal.Store import emptyStore
from iTasks.Internal.Util import toCanonicalPath
import iTasks.Internal.Serialization
import iTasks.Internal.IWorld
import iTasks.UI.Definition
import qualified iTasks.Internal.SDS as SDS
from Data.Queue import :: Queue(..)
import System.OS
import iTasks.Util.Trace

derive class iTask TestSuite, Test, InteractiveTest, TestResult, SuiteResult, ExitCode

gText{|UnitTest|} _ _			            = []
gEditor{|UnitTest|} = emptyEditor 
JSONEncode{|UnitTest|} _ c	   = [dynamicJSONEncode c]
JSONDecode{|UnitTest|} _ [c:r] = (dynamicJSONDecode c,r)
JSONDecode{|UnitTest|} _ r	   = (Nothing,r)
gEq{|UnitTest|} _ _			   = True
gDefault{|UnitTest|}		   = {UnitTest|name="Default unit test",test=pass}
where
	pass :: *World -> *(TestResult,*World)
	pass w = (Passed,w)

//DEFINING TESTS
itest :: String String String (Task a) -> Test | iTask a
itest name instructions expectation tut
  = InteractiveTest {name=name,instructions = instructions, expectation = expectation, taskUnderTest = tut @! ()}

utest :: String (*World -> *(TestResult,*World)) -> Test
utest name test = UnitTest {UnitTest|name=name,test=test}

assert :: String (a -> Bool) a -> Test | gPrettyTrace{|*|} a
assert name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w = (if (exp sut) Passed (Failed (Just ("Actual:\n" +++ (prettyTrace sut)))),w)

assertEqual :: String a a -> Test | gEq{|*|} a & gPrettyTrace{|*|} a
assertEqual name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w = (checkEqual exp sut,w)

assertWorld :: String (a -> Bool) (*World -> *(a,*World)) -> Test | gPrettyTrace{|*|} a
assertWorld name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w 
		# (res,w) = sut w
		= (if (exp res) Passed (Failed (Just ("Actual:\n" +++ (prettyTrace res)))),w)

assertEqualWorld :: String a (*World -> *(a,*World)) -> Test | gEq{|*|} a & gPrettyTrace{|*|} a
assertEqualWorld name exp sut = UnitTest {UnitTest|name=name,test=test}
where
	test w
		# (res,w) = sut w
		= (if (exp === res) Passed (Failed (Just (sideBySideTrace ("Expected:",exp) ("Actual:",res)))),w)

checkEqual :: a a -> TestResult | gEq{|*|} a & gPrettyTrace{|*|} a
checkEqual exp sut = checkEqualWith (===) exp sut

checkEqualWith :: (a a -> Bool) a a -> TestResult | gPrettyTrace{|*|} a
checkEqualWith pred exp sut = if (pred exp sut) Passed (Failed (Just (sideBySideTrace ("Expected:",exp) ("Actual:", sut))))

pass :: String -> Test
pass name = UnitTest {UnitTest|name=name,test = \w -> (Passed,w)}

fail :: String -> Test
fail name = UnitTest {UnitTest|name=name,test = \w -> (Failed Nothing, w)}

skip :: Test -> Test
skip skipped = UnitTest {UnitTest|name=nameOf skipped,test= \w -> (Skipped,w)}
where
	nameOf (UnitTest {UnitTest|name}) = name
	nameOf (InteractiveTest {InteractiveTest|name}) = name

testsuite :: String String [Test] -> TestSuite
testsuite name description tests
  = {name=name,description=description,tests=tests}

filterSuitesByTestName ::String [TestSuite] -> [TestSuite]
filterSuitesByTestName pattern suites = [{TestSuite|s & tests =filterTestsByName pattern tests} \\ s=:{TestSuite|tests} <- suites]

filterTestsByName :: String [Test] -> [Test]
filterTestsByName pattern tests = filter match tests
where
	match (UnitTest {UnitTest|name}) = indexOf pattern name >= 0
	match (InteractiveTest {InteractiveTest|name}) = indexOf pattern name >= 0

//UTILITY TASKS
testEditor :: (Editor a) a EditMode -> Task a | iTask a
testEditor editor model mode
	=   (interact "Editor test" mode unitShare {onInit = const ((),model), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \_ l v -> (l,v,Nothing)} editor @ snd
	>&> viewSharedInformation "Editor value" [ViewAs (toString o toJSON)] @? tvFromMaybe
	)  <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal) )

testEditorWithShare :: (Editor a) a EditMode -> Task a | iTask a
testEditorWithShare editor model mode = (withShared model
	\smodel ->
		updateSharedInformation "Edit the shared source" [] smodel 
		||-
	    interact "Editor under test" mode smodel {onInit = \r -> ((),r)
												 ,onEdit = \v l _ -> (l,v,Just (\_ -> v))
												 ,onRefresh = \r l v -> (l,r,Nothing)} editor @ snd
	) <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal)) 

testCommonInteractions :: String -> Task a | iTask a
testCommonInteractions typeName
	= 	 enterInformation ("Enter","Enter information of type " +++ typeName) []
	-||- updateInformation ("Update","Update default value of type " +++ typeName) [] defaultValue
	-||- (withShared defaultValue
			\s -> (updateSharedInformation ("Update shared","Update shared value of type " +++ typeName) [] s
				   -||
				   viewSharedInformation ("View shared","View shared value of type " +++ typeName) [] s
				  )
		 )

testTaskOutput :: String (Task a) [Either Event Int] [TaskOutputMessage] ([TaskOutputMessage] [TaskOutputMessage] -> TestResult) -> Test | iTask a
testTaskOutput name task events exp comparison = utest name test
where
	test world 
		# (options,world) = defaultEngineOptions world
		# iworld = createIWorld options world
		//Initialize JS compiler support
		# (res,iworld) = initJSCompilerState iworld
		| res =:(Error _)
			= (Failed (Just (fromError res)),destroyIWorld iworld)
		//Empty the store to make sure that we get a reliable task instance no 1
		# iworld = emptyStore iworld
		//Create an instance with autolayouting disabled at the top level
		# (res,iworld) = createTaskInstance task iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				//Apply all events
				# (res,iworld) = applyEvents instanceNo events iworld 
				= case res of
					(Ok ())
						//Collect output
						# (res,iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceOutput) iworld
						# world = destroyIWorld iworld
						//Compare result
						# verdict = case res of
							Ok queue = comparison exp (toList queue)
							(Error (_,e)) = Failed (Just e)
						= (verdict,world)
					(Error e)
						# world = destroyIWorld iworld
						= (Failed (Just e),world)
			(Error (_,e)) 	
				# world = destroyIWorld iworld
				= (Failed (Just e),world)

	applyEvents _ [] iworld = (Ok (),iworld)
	applyEvents instanceNo [Left e:es] iworld
		= case evalTaskInstance instanceNo e iworld of
			(Ok _,iworld) = applyEvents instanceNo es iworld
			(Error e,iworld) = (Error e,iworld)
	applyEvents instanceNo [Right e:es] iworld
		//Wait between events
		# iworld = (sleep e) iworld
		= applyEvents instanceNo es iworld

	//SHOULD BE IN Data.Queue
	toList (Queue front rear) = front ++ reverse rear

	//TODO: Do this with a platform independent standard function
	sleep secs iworld = IF_POSIX (sleep_posix secs iworld) iworld
	sleep_posix secs iworld
		# x = sleep` secs
		| x == 0 && x <> 0 = undef
		= iworld
	where
       sleep` :: !Int -> Int
       sleep` secs = code {
          ccall sleep "I:I"
       }

allPassed :: TestReport -> Bool
allPassed suiteResults = all (checkSuiteResult (\r -> r =: Passed)) suiteResults

noneFailed :: TestReport -> Bool
noneFailed suiteResults = all (checkSuiteResult (\r -> r =: Passed || r =: Skipped)) suiteResults

checkSuiteResult :: (TestResult -> Bool) SuiteResult -> Bool
checkSuiteResult f {SuiteResult|testResults} = all (\(_,r) -> f r) testResults

runUnitTestsCLI :: [TestSuite] *World -> *World
runUnitTestsCLI suites world
	# (console,world)	       = stdio world
	# (report,(console,world)) = foldl runSuite ([],(console,world)) suites
	# (_,world)			       = fclose console world
	# world 			       = setReturnCode (if (noneFailed report) 0 1) world
    = world
where	
	runSuite (report,(console,world)) {TestSuite|name,tests}
		# console = fwrites ("===[ "+++ name +++ " ]===\n") console
		# (testResults,(console,world)) = foldl runTest ([],(console,world)) [t \\ UnitTest t <- tests]
		= ([{SuiteResult|suiteName=name,testResults=reverse testResults}:report],(console,world))
		
	runTest (results,(console,world)) {UnitTest|name,test}  
		# console = fwrites (name +++ "... ") console
		# (result,world) = test world
		# (console,world) = case result of
			Passed
				# console = fwrites (green "PASSED\n") console
				= (console,world)
			Failed Nothing
				# console = fwrites (red "FAILED\n") console
				= (console,world)
			Failed (Just msg)
				# console = fwrites (red ("FAILED\n" +++msg+++"\n")) console
				= (console,world)
			Skipped
				# console = fwrites (yellow "SKIPPED\n") console
				= (console,world)
		= ([(name,result):results],(console,world))

	//ANSI COLOR CODES -> TODO: Create a library in clean-platform for ANSI colored output
	red s = toString [toChar 27,'[','3','1','m'] +++ s +++ toString [toChar 27,'[','0','m']
	green s = toString [toChar 27,'[','3','2','m'] +++ s +++ toString [toChar 27,'[','0','m']
	yellow s = toString [toChar 27,'[','3','3','m'] +++ s +++ toString [toChar 27,'[','0','m']

runUnitTestsJSON :: [TestSuite] *World -> *World
runUnitTestsJSON suites world
	# (report,world) 	= runUnitTestsWorld suites world
	# (console,world)	= stdio world
	# console 			= fwrites (toString (toJSON report)) console
	# (_,world)			= fclose console world
	# world 			= setReturnCode (if (noneFailed report) 0 1) world
	= world

runUnitTestsWorld :: [TestSuite] *World -> *(!TestReport,!*World)
runUnitTestsWorld suites world = foldr runSuite ([],world) suites
where
    runSuite {TestSuite|name,tests} (report,world)
        # (testResults,world) = foldr runTest ([],world) [t \\ UnitTest t <- tests]
        = ([{SuiteResult|suiteName=name,testResults=testResults}:report],world)

    runTest {UnitTest|name,test} (results,world)
        # (result,world) = test world
        = ([(name,result):results],world)

execTestSuite :: TestSuite *World -> World //TODO: Use a standard format for reporting test results
execTestSuite  {TestSuite|name,tests} world
	# (console,world) = stdio world
	# console = fwrites ("Suite: " +++ name +++ "\n") console
	# console = fwrites ("Num: " +++ toString (length tests) +++ "\n") console
	# (allOk,console,world) = execTests tests console world 
	# (_,world) = fclose console world
	= setReturnCode (if allOk 0 1) world
where
	execTests [] console world = (True,console,world)
	execTests [t:ts] console world
		# (r,console,world) = execTest t console world
		# (rs,console,world) = execTests ts console world
		= (r && rs,console,world)

	execTest (UnitTest {UnitTest|name,test}) console world
		# console = fwrites ("\nTest: " +++ name +++ "\n") console
		# (result,world) = test world
		# console = case result of	
			Passed = fwrites "Result: Passed\n" console
			Skipped = fwrites "Result: Skipped\n" console
			Failed Nothing = fwrites "Result: Failed\n" console
			Failed (Just msg) = (fwrites "Result: Failed\n") $ (fwrites msg) console
		= (result =: Passed || result =: Skipped, console, world)
	execTest _ console world
		= (True,console,world)

