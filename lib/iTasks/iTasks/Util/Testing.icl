implementation module iTasks.Util.Testing

import iTasks, StdFile, StdMisc
import iTasks.Extensions.Image
import iTasks.UI.Editor, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Common, iTasks.UI.Definition
import iTasks.Extensions.Editors.Ace
import iTasks.Internal.Serialization
import Text, Text.HTML, Text.GenPrint, System.CommandLine
import qualified Data.Map as DM
import iTasks.Extensions.Development.Codebase
import Data.Func, Data.Either, Data.Error

from iTasks.Internal.IWorld import createIWorld, destroyIWorld, initJSCompilerState, ::IWorld{options} 
from iTasks.Internal.TaskStore import createSessionTaskInstance, taskInstanceOutput, :: TaskOutput, :: TaskOutputMessage
from iTasks.Internal.TaskEval import evalTaskInstance
from iTasks.Internal.Store import emptyStore
from iTasks.Internal.Util import toCanonicalPath
import iTasks.Internal.Serialization
import iTasks.Internal.IWorld
import iTasks.UI.Definition
import qualified iTasks.Internal.SDS as SDS
from Data.Queue import :: Queue(..)

import System.OS, System.CommandLine, System.Options

import Testing.TestEvents
import Testing.Options

derive class iTask InteractiveTest

gText{|UnitTest|} _ _			            = []
gEditor{|UnitTest|} = emptyEditorWithErrorInEnterMode "A unit test cannot be entered."
JSONEncode{|UnitTest|} _ c	   = [dynamicJSONEncode c]
JSONDecode{|UnitTest|} _ [c:r] = (dynamicJSONDecode c,r)
JSONDecode{|UnitTest|} _ r	   = (Nothing,r)
gEq{|UnitTest|} _ _			   = True
gDefault{|UnitTest|}		   = {UnitTest|name="Default unit test",test=pass}
where
	pass :: *World -> *(EndEventType,*World)
	pass w = (Passed,w)

assert :: String (a -> Bool) a -> UnitTest
assert name exp sut = {UnitTest|name=name,test=test}
where
	test w = (if (exp sut) Passed (Failed Nothing),w)

assertEqual :: String a a -> UnitTest | gEq{|*|} a & gPrint{|*|} a
assertEqual name exp sut = {UnitTest|name=name,test=test}
where
	test w = (checkEqual exp sut,w)

assertWorld :: String (a -> Bool) (*World -> *(a,*World)) -> UnitTest
assertWorld name exp sut = {UnitTest|name=name,test=test}
where
	test w
		# (res,w) = sut w
		= (if (exp res) Passed (Failed Nothing),w)

assertEqualWorld :: String a (*World -> *(a,*World)) -> UnitTest | gEq{|*|} a & gPrint{|*|} a
assertEqualWorld name exp sut = {UnitTest|name=name,test=test}
where
	test w
		# (res,w) = sut w
		= (if (exp === res) Passed (Failed (Just (FailedAssertions [ExpectedRelation (GPrint (printToString exp)) Eq (GPrint (printToString res))]))),w)

checkEqual :: a a -> EndEventType | gEq{|*|} a & gPrint{|*|} a
checkEqual exp sut = checkEqualWith (===) exp sut

checkEqualWith :: (a a -> Bool) a a -> EndEventType | gPrint{|*|} a
checkEqualWith pred exp sut = if (pred exp sut) Passed (Failed (Just (FailedAssertions [ExpectedRelation (GPrint (printToString exp)) Eq (GPrint (printToString sut))])))

pass :: String -> UnitTest
pass name = {UnitTest|name=name,test = \w -> (Passed,w)}

fail :: String -> UnitTest
fail name = {UnitTest|name=name,test = \w -> (Failed Nothing, w)}

skip :: UnitTest -> UnitTest
skip skipped=:{UnitTest|name} = {UnitTest|name=name,test= \w -> (Skipped,w)}

filterTestsByName :: String [UnitTest] -> [UnitTest]
filterTestsByName pattern tests = filter (\{UnitTest|name} -> indexOf pattern name >= 0) tests

//UTILITY TASKS
testEditor :: (Editor a) (EditMode a) -> Task a | iTask a
testEditor editor mode
	=   (interact "Editor test" unitShare {onInit = const ((),mode), onEdit = \v l _ -> (l,v,Nothing), onRefresh = \_ l (Just v) -> (l,v,Nothing)} editor @ snd
	>&> viewSharedInformation "Editor value" [ViewAs (toString o toJSON)] @? tvFromMaybe
	)  <<@ ApplyLayout (setUIAttributes (directionAttr Horizontal) )

testEditorWithShare :: (Editor a) a Bool -> Task a | iTask a
testEditorWithShare editor model viewMode = (withShared model
	\smodel ->
		updateSharedInformation "Edit the shared source" [] smodel
		||-
	    interact "Editor under test" smodel {onInit = \r -> ((),if viewMode View Update $ r)
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

testTaskOutput :: String (Task a) [Either Event Int] [TaskOutputMessage] ([TaskOutputMessage] [TaskOutputMessage] -> EndEventType) -> UnitTest | iTask a
testTaskOutput name task events exp comparison = {UnitTest|name=name,test=test}
where
	test world
		# (options,world) = defaultEngineOptions world
		# iworld = createIWorld {options & autoLayout = False} world
		//Initialize JS compiler support
		# (res,iworld) = initJSCompilerState iworld
		| res =:(Error _)
			= (Failed (Just Crashed),destroyIWorld iworld)
		//Empty the store to make sure that we get a reliable task instance no 1
		# iworld = emptyStore iworld
		//Create an instance with autolayouting disabled at the top level
		# (res,iworld) = createSessionTaskInstance task 'DM'.newMap iworld
		= case res of
			(Ok (instanceNo,instanceKey))
				//Apply all events
				# (res,iworld) = applyEvents instanceNo events iworld
				= case res of
					(Ok ())
						//Collect output
						# (res,iworld) = 'SDS'.read (sdsFocus instanceNo taskInstanceOutput) 'SDS'.EmptyContext iworld
						# world = destroyIWorld iworld
						//Compare result
						# verdict = case res of
							Ok ('SDS'.ReadingDone queue) = comparison exp (toList queue)
							(Error (_,e)) = Failed (Just Crashed)
						= (verdict,world)
					(Error e)
						# world = destroyIWorld iworld
						= (Failed (Just Crashed),world)
			(Error (_,e))
				# world = destroyIWorld iworld
				= (Failed (Just Crashed),world)

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
allPassed report = checkSuiteResult (\r -> r =: Passed) report

noneFailed :: TestReport -> Bool
noneFailed report = checkSuiteResult (\r -> r =: Passed || r =: Skipped) report

checkSuiteResult :: (EndEventType -> Bool) [(String,EndEventType)] -> Bool
checkSuiteResult f testResults = all (\(_,r) -> f r) testResults

runUnitTests :: [UnitTest] *World -> *World
runUnitTests suites world
	# (args,world)             = getCommandLine world
	= case parseOptions testOptionDescription (tl args) gDefault{|*|} of
		(Ok options)
			# (console,world)	       = stdio world
			# (report,(console,world)) = foldl (runTest options) ([],(console,world)) suites
			# (_,world)			       = fclose console world
			# world 			       = setReturnCode (if (noneFailed report) 0 1) world
			= world
		(Error msgs)
			# (console,world)	       = stdio world
			# console                  = foldl (\c m -> fwrites (m +++ "\n") c) console args
			# console                  = foldl (\c m -> fwrites (m +++ "\n") c) console msgs
			# (_,world)			       = fclose console world
			= setReturnCode 1 world
where
	runTest options (results,(console,world)) {UnitTest|name,test}
		//Just print names
		| options.list
			# console = fwrites (name +++ "\n") console
			= (results,(console,world))
		//Skip
		| skipTest name options
			= (results,(console,world))
		//Check if the test should run
		| otherwise
			# console = fwrites (toString (toJSON (StartEvent {StartEvent|name=name})) +++ "\n") console
			# (result,world) = test world
			# message = case result of
				Passed = "PASSED"
				Failed _ = "FAILED"
				Skipped = "SKIPPED"
			# console = fwrites (toString (toJSON (EndEvent {EndEvent|name=name,event=result,message=message})) +++ "\n") console
			= ([(name,result):results],(console,world))

	skipTest name {runs,skip}
		| isMember name skip = True //Explicitly skipped
		| runs =: [] = False //Run all
		| otherwise = isMember name [name \\ {TestRun|name} <- runs] //Check if it was listed


