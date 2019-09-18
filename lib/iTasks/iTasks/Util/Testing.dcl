definition module iTasks.Util.Testing
import iTasks
import iTasks.Util.Trace
from iTasks.Internal.TaskStore import :: TaskOutputMessage(..)
from Testing.TestEvents import :: EndEventType
from Text.GenPrint import generic gPrint, :: PrintState, class PrintOutput

:: UnitTest
	= { name :: String
	  , test :: *World -> *(EndEventType,*World)
	  }

:: TestReport :== [(String,EndEventType)]

:: InteractiveTest 
	= { name :: String
      , instructions  	:: String
      , expectation 	:: String
	  , taskUnderTest 	:: Task ()
	  }

derive class iTask InteractiveTest

derive JSONEncode UnitTest
derive JSONDecode UnitTest
derive gEq UnitTest
derive gEditor UnitTest
derive gText UnitTest
derive gDefault UnitTest

assert :: String (a -> Bool) a -> UnitTest

assertEqual :: String a a -> UnitTest | gEq{|*|} a & gPrint{|*|} a

assertWorld :: String (a -> Bool) (*World -> *(a,*World)) -> UnitTest

assertEqualWorld :: String a (*World -> *(a,*World)) -> UnitTest | gEq{|*|} a & gPrint{|*|} a

checkEqual :: a a -> EndEventType | gEq{|*|} a & gPrint{|*|} a
checkEqualWith :: (a a -> Bool) a a -> EndEventType | gPrint{|*|} a

pass :: String -> UnitTest

fail :: String -> UnitTest

skip :: UnitTest -> UnitTest

/**
* Create a task instance and evaluate it to verify its output
*/
testTaskOutput :: String (Task a) [Either Event Int] [TaskOutputMessage] ([TaskOutputMessage] [TaskOutputMessage] -> EndEventType) -> UnitTest | iTask a

/**
* Filter test suites based on the name of a test
*/
filterTestsByName :: String [UnitTest] -> [UnitTest]

/**
* Test a specific editor
*
* @param The editor to test
* @param Edit mode to test (View,Enter,Update)
*/
testEditor :: (Editor a) (EditMode a) -> Task a | iTask a

/**
* Test a specific editor using a shared state.
*
* @param The editor to test
* @param The model value that the editor edits
* @param Use view mode for editor (otherwise update mode)
*/
testEditorWithShare :: (Editor a) a Bool -> Task a | iTask a

/**
* A generic test rig for testing the different editor variants for a type
*
* @param The name of the type to test (e.g. "Int" or "MyADT")
*/
testCommonInteractions :: String -> Task a | iTask, gDefault{|*|} a


/**
* Test if all tests have passed
*/
allPassed :: TestReport -> Bool
/**
* Check if no tests have failed (skipped and passed)
*/
noneFailed :: TestReport -> Bool

/**
* Runs the unit tests from the test suites and shows test
* results on stdout
*/
runUnitTests :: [UnitTest] *World -> *World
