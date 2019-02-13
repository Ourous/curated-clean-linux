
#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"

Bool
	DoCode					= True,
	DoDebug 				= False,
	DoListAllTypes			= False,
	DoListTypes				= False,
	DoShowAttributes		= True,
	DoParallel				= False,
	DoStackLayout			= True,
	DoStrictnessAnalysis	= True,
	DoVerbose 				= False,
	DoWarning 				= True,
	DoStrictWarning			= True,
	DoStrictAllWarning		= False,
	DoStrictCheck			= False,
	DoListStrictTypes		= False;
Bool ListOptimizations		= False;

Bool DoDescriptors			= False;
Bool ExportLocalLabels		= False;
Bool AddStrictnessToExportedFunctionTypes = False;
Bool Dynamics = False;

Bool DoProfiling=False; /* no longer used by memory profiler */
Bool DoTimeProfiling=False;

Bool DoReuseUniqueNodes		= False;
Bool DoFusion				= False;
Bool DoGenericFusion		= False;

Bool OptimizeLazyTupleRecursion=False;
Bool OptimizeTailCallModuloCons=True;
Bool OptimizeInstanceCalls = False;
Bool WriteModificationTimes	= False;

unsigned StrictDepth		= 10; /* 8; */

Bool StrictDoLists			= False;
Bool StrictDoPaths			= True;
Bool StrictDoAllPaths		= True;
Bool StrictDoExtEq			= True;
Bool StrictDoLessEqual		= True;
Bool StrictDoEager			= True;
Bool StrictDoVerbose		= False;
Bool StrictDoAnnots			= True;

int FunctionMayFailWarningOrError = 0; /* 0: ignore, 1: warning, 2: error */

