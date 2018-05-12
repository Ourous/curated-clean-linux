implementation module iTasks.Extensions.Development.Testing
import iTasks
import iTasks.Extensions.Development.Tools
import iTasks.Internal.Test.Definition
import Text, Data.Tuple, Data.Error, System.FilePath, System.OS

derive class iTask ExitCode

TESTS_PATH :== "../Tests/TestPrograms"

//:: CompileError = CompileError !Int

compileTestModule :: FilePath -> Task TestResult
compileTestModule path 
	= 	get cpmExecutable 
	>>- \cpm ->
	    withShared [] ( \io -> (
			 runWithOutput cpm [prj] Nothing io //Build the test
		@ \(ExitCode c,o) -> if (passed c o) Passed (Failed (Just ("Failed to build " +++ prj +++ "\n" +++ join "" o)))
       ))
where
    //Cpm still returns exitcode 0 on failure, so we have to check the output
	passed 0 o = let lines = split OS_NEWLINE (join "" o) in not (any isErrorLine lines) 
	passed _ _ = False

 	isErrorLine l = startsWith "Error" l || startsWith "Type error" l || startsWith "Parse error" l

	baseDir = takeDirectory path
	base = dropExtension path
	prj = addExtension base "prj"

//Copy-paste.. should be in library
runTestModule :: FilePath -> Task SuiteResult
runTestModule path
	= compileTestModule path
	>>- \res -> case res of
		Passed = withShared [] (\io -> ( runWithOutput exe [] Nothing io @ (parseSuiteResult o appSnd (join "")))) //Run the test
	    _      = return {SuiteResult|suiteName=path,testResults=[("build",res)]}
where
	baseDir = takeDirectory path
	base = dropExtension path
	exe = addExtension base "exe"
	prj = addExtension base "prj"

	parseSuiteResult :: (ExitCode,String) -> SuiteResult //QUICK AND DIRTY PARSER
	parseSuiteResult (ExitCode ecode,output)
		# lines = split "\n" output
		| length lines < 2 = fallback ecode output
		# suiteName = trim ((split ":" (lines !! 0)) !! 1)
		# results = [parseRes resLines \\ resLines <- splitLines (drop 3 lines) | length resLines >= 2]
		= {SuiteResult|suiteName=suiteName,testResults=results}
	where
		splitLines lines = split` lines [[]]
		where
			split` ["":lines] acc = split` lines [[]:acc]
			split` [l:lines] [h:acc] = split` lines [[l:h]:acc]
			split` [] acc = reverse (map reverse acc)

		parseRes [nameLine,resultLine:descLines]
			# name = trim ((split ":" nameLine) !! 1)
			# result = case resultLine of
				"Result: Passed" = Passed
				"Result: Skipped" = Skipped
				_ = Failed (if (descLines =: []) Nothing (Just (join "\n" descLines)))
			= (name,result)
		parseRes _ = ("oops",Failed Nothing)

		//If we can't parse the output, We'll treat it as a single simple test executable
		fallback 0 _ = {SuiteResult|suiteName="Unknown",testResults=[("executable",Passed)]}
		fallback _ output = {SuiteResult|suiteName="Unknown",testResults=[("executable",Failed (Just output))]}

runWithOutput :: FilePath [String] (Maybe FilePath) (Shared [String]) -> Task (ExitCode,[String])
runWithOutput prog args dir out
   = externalProcess () prog args dir out {onStartup=onStartup,onOutData=onOutData,onErrData=onErrData,onShareChange=onShareChange,onExit=onExit} Nothing gEditor{|*|}
	where
		onStartup r = (Ok (ExitCode 0,[]), Nothing, [], False) 
		onOutData data (e,o) r = (Ok (e,o ++ [data]), Just (r ++ [data]), [], False)
		onErrData data l r = (Ok l, Nothing, [], False)
		onShareChange  l r = (Ok l, Nothing, [], False)
		onExit ecode (_,o) r =  (Ok (ecode,o), Nothing)
