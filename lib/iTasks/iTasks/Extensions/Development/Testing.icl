implementation module iTasks.Extensions.Development.Testing
import iTasks
import System.Time

import Testing.TestEvents 
import iTasks.Util.Testing 
import iTasks.Extensions.Files
import iTasks.Extensions.Development.Tools
import iTasks.Extensions.Development.Codebase
import Text, Data.Tuple, Data.Error, Data.Func, System.FilePath, System.OS

derive class iTask EndEventType, Expression

derive gEditor EndEvent, TestLocation, FailReason, FailedAssertion, CounterExample, Relation
derive gText EndEvent, TestLocation, FailReason, FailedAssertion, CounterExample, Relation
derive gEq EndEvent, TestLocation, FailReason, FailedAssertion, CounterExample, Relation

compileTestModule :: CleanModuleName -> Task EndEvent
compileTestModule (path,name)
	=           copyFile prjDefaultPath prjPath
	>-|         get cpmExecutable
	>>- \cpm -> runWithOutput cpm [prjPath] Nothing //Build the test
	@   \(c,o) -> if (passed c o) 
			{name = testName, location=Just {moduleName=Just name}, event = Passed, message = join "" o}
			{name = testName, location=Just {moduleName=Just name}, event = (Failed Nothing), message = join "" o}
where
	testName = "Compile: " +++ name
	iclPath = cleanFilePath (path,name,Icl)
	prjDefaultPath = path </> name +++ ".prj.default"
	prjPath = path </> name +++ ".prj"

    //Cpm still returns exitcode 0 on failure, so we have to check the output
	passed 0 o = let lines = split OS_NEWLINE (join "" o) in not (any isErrorLine lines) 
	passed _ _ = False

 	isErrorLine l = startsWith "Error" l || startsWith "Type error" l || startsWith "Parse error" l

//Copy-paste.. should be in library
runTestModule :: CleanModuleName -> Task [EndEvent]
runTestModule (path,name)
	=   compileTestModule (path,name)
	>>- \res=:{EndEvent|event} -> case event of
		Passed = runWithOutput exe [] Nothing @ (parseTestResults o appSnd (join "")) //Run the test
	    _      = return [res]
where
	exe = IF_WINDOWS (base </> addExtension name "exe") (path </> name)
	baseDir = takeDirectory path
	base = dropExtension path

	parseTestResults (ecode,output)
		# lines = split OS_NEWLINE output
		| length lines < 2 = fallback ecode output
		= [res \\ Just res <- map (fromJSON o fromString) lines]		
	where
		//If we can't parse the output, We'll treat it as a single simple test executable
		fallback 0 _ = [{name=name,location=Just {moduleName=Just name},event=Passed,message="Execution returned 0"}]
		fallback _ output = [{name=name,location=Just {moduleName=Just name},event=Failed Nothing,message=output}]

runWithOutput :: FilePath [String] (Maybe FilePath) -> Task (Int,[String])
runWithOutput prog args dir = withShared ([], []) \out->withShared [] \stdin->
	externalProcess {tv_sec=0,tv_nsec=100000000} prog args dir Nothing stdin out
	>>- \c->get out @ tuple c o fst
