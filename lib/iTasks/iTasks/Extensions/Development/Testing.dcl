definition module iTasks.Extensions.Development.Testing
/**
* This module provides utilities for testing iTasks programs
*/
import iTasks

from Testing.TestEvents import :: EndEvent, :: EndEventType
from iTasks.Util.Testing import :: TestReport 
from iTasks.Extensions.Development.Codebase import :: CleanModuleName, :: ModuleName

compileTestModule :: CleanModuleName -> Task EndEvent
runTestModule :: CleanModuleName -> Task [EndEvent]
