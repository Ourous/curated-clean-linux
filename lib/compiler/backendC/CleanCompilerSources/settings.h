
/*
	Compiler setttings
*/

extern Bool DoCode;					/* not generated in abc file */
extern Bool DoDebug;
extern Bool DoParallel;
extern Bool DoStackLayout;
extern Bool DoStrictnessAnalysis;
extern Bool DoVerbose;
extern Bool DoWarning;	
extern Bool DoListTypes;			/* not generated in abc file */
extern Bool DoListAllTypes;			/* not generated in abc file */
extern Bool DoShowAttributes;		/* not generated in abc file */
extern Bool DoListStrictTypes;		/* not generated in abc file */
extern Bool DoStrictWarning;		/* not generated in abc file */
extern Bool DoStrictAllWarning;		/* not generated in abc file */
extern Bool DoStrictCheck;			/* not generated in abc file */
extern Bool DoDescriptors;			/* not generated in abc file */
extern Bool ListOptimizations;

extern Bool ExportLocalLabels;
extern Bool AddStrictnessToExportedFunctionTypes;
extern Bool Dynamics;

extern Bool DoProfiling;
extern Bool DoTimeProfiling;

extern Bool DoReuseUniqueNodes;
extern Bool DoFusion;
extern Bool DoGenericFusion;
extern Bool OptimizeLazyTupleRecursion;
extern Bool OptimizeTailCallModuloCons;
extern Bool OptimizeInstanceCalls;
extern Bool WriteModificationTimes;

#define	StrictDoRelated	False

extern unsigned StrictDepth;
extern Bool StrictDoLists;
extern Bool StrictDoPaths;
extern Bool StrictDoAllPaths;
extern Bool StrictDoExtEq;
extern Bool StrictDoLessEqual;
extern Bool StrictDoEager;
extern Bool StrictDoVerbose;
extern Bool StrictDoAnnots;

extern int FunctionMayFailWarningOrError;
