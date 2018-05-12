definition module iTasks.Extensions.Development.Testing
/**
* This module provides utilities for testing iTasks programs
*/
import iTasks
from iTasks.Internal.Test.Definition import :: TestResult, :: SuiteResult

compileTestModule :: FilePath -> Task TestResult
runTestModule :: FilePath -> Task SuiteResult
