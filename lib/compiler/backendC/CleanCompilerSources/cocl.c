
#include "compiledefines.h"
#include "comsupport.h"
#include "settings.h"
#include <ctype.h>
#include "compiler.h"
#include "version.h"

#include "MAIN_CLM.d"

#if defined (_MSC_VER) || defined (_SUN_)
FILE *std_out_file_p,*std_error_file_p;
#endif

static char usage[]=
	"Usage: \'cocl [options] [-o file] file\'\n"
	"Options: [-v] [-w] [-tc] [-d] [-sl] [-p] [-sa] [-lt] [-lset] [-lat] [-lattr]";

static void Usage (void)
{
	FPutS (usage, StdError);
	FPutC ('\n', StdError);
}

static Bool GetInt (char *s, int *i)
{
	int j;
	char *cp;
	
	for (j = 0, cp = s; *cp; cp++)
	{	if (!isdigit (*cp))
			return False;
		
		j = (10 * j) + (*cp - '0');
	}
	*i = j;
	return True;
}

static Bool SetStrictOption (char *opt)
{	int i;

	if (strcmp (opt, "w") == 0)
		DoStrictWarning = False;
	else if (strcmp (opt, "wa") == 0)
		DoStrictAllWarning = True;
	else if (strcmp (opt, "c") == 0)
		DoStrictCheck = True;
	else if (strcmp (opt, "sa") == 0)
		StrictDoAnnots = True;
	else if (opt[0] == 'd')
	{	if (GetInt (opt+1, &i))
			StrictDepth = i;
		else
			return False;
	}
	else
		return False;

	return True;
}

char *path_parameter;
#ifdef _SUN_
int use_clean_system_files;
#endif

#ifdef CLEAN2
int StdOutReopened,StdErrorReopened;
#endif

#if defined (_MAC_) && defined (GNU_C)
extern char *convert_file_name (char *file_name,char *buffer);

static FILE *freopen_with_file_name_conversion (char *file_name,char *mode,FILE *file_p)
{
	char buffer[512+1];

	file_name=convert_file_name (file_name,buffer);
	if (file_name==NULL)
		return NULL;

	return freopen (file_name,mode,file_p);
}

# define freopen freopen_with_file_name_conversion
#endif

#ifdef CLEAN2
Bool ParseCommandArgs (int argc, char **argv, char **file_name_p, char **output_file_name_p)
#else
Bool CallCompiler (int argc, char **argv)
#endif
{
	char *fname,*output_file_name;
	int i;

# if defined (_MSC_VER) || defined (_SUN_)
	std_out_file_p = stdout;
	std_error_file_p = stderr;
# endif

	fname = NULL;
	output_file_name=NULL;

	path_parameter=NULL;
#ifdef _SUN_
	use_clean_system_files=0;
#endif
	
	DoWarning 				= True;
	DoVerbose 				= False;
	DoCode					= True;
	DoDebug 				= False;
	DoStrictnessAnalysis	= True;
	DoStackLayout			= True /* False */;
	DoParallel				= False;
	DoShowAttributes		= True;
	DoListTypes				= False;
	DoListAllTypes			= False;
	DoListStrictTypes		= False;

	DoStrictCheck			= False;
	DoStrictWarning			= True;
	DoStrictAllWarning		= False;

	DoProfiling=False;
	DoTimeProfiling=False;
	DoReuseUniqueNodes=False;
	DoFusion=False;
	DoGenericFusion=False;
	DoDescriptors=False;
	ExportLocalLabels=False;
	AddStrictnessToExportedFunctionTypes=False;
	OptimizeInstanceCalls=False;
	Dynamics=False;

	StrictDoAnnots			= False;
	StrictDepth				= 10;/* 8; */

	FunctionMayFailWarningOrError = 0;

#ifdef CLEAN2
	StdErrorReopened	= False;
	StdOutReopened		= False;
#endif

	for (i = 0; i < argc; i++){
		if (argv[i][0] == '-' || argv[i][0] == '+'){
			char *argv_i;
			
			argv_i=argv[i];
			
			if (strcmp (argv_i, "-v") == 0)
				DoVerbose = True;
			else if (strcmp (argv_i, "-w") == 0){
				DoWarning = False;
				DoStrictWarning	= False;
			} else if (strcmp (argv_i, "-d") == 0)
				DoDebug = True;
			else if (strcmp (argv_i, "-c") == 0)
				DoCode = False;
			else if (strcmp (argv_i, "-p") == 0)
				DoParallel = True;
#ifdef _SUN_
			else if (strcmp (argv_i, "-csf")==0)
				use_clean_system_files=1;
#endif
			else if (strcmp (argv_i, "-sl") == 0)
				DoStackLayout = True;
			else if (strcmp (argv_i, "-sa") == 0)
				DoStrictnessAnalysis = False;
			else if (strcmp (argv_i,"-ou") == 0)
				DoReuseUniqueNodes=True;
			else if (strcmp (argv_i,"-pm") == 0)
				DoProfiling=True;
			else if (strcmp (argv_i,"-pt") == 0)
				DoTimeProfiling=True;
			else if (strcmp (argv_i,"-wmt") == 0)
				WriteModificationTimes=True;
			else if (strcmp (argv_i,"-wmf") == 0)
				FunctionMayFailWarningOrError=1;
			else if (strcmp (argv_i,"-emf") == 0)
				FunctionMayFailWarningOrError=2;
			else if (strcmp (argv_i,"-desc") ==0)
				DoDescriptors=True;
			else if (strcmp (argv_i,"-exl") ==0)
				ExportLocalLabels=True;
			else if (strcmp (argv_i,"-fusion") == 0)
				DoFusion=True;
			else if (strcmp (argv_i,"-generic_fusion") == 0)
				DoGenericFusion=True;
			else if (strcmp (argv_i,"-seft") == 0)
				AddStrictnessToExportedFunctionTypes=True;
			else if (strcmp (argv_i,"-dynamics") == 0)
				Dynamics=True;
			else if (strcmp (argv_i,"-oic") == 0)
				OptimizeInstanceCalls=True;
			else if (strncmp (argv_i, "-sa", 3) == 0){
				if (!SetStrictOption (argv[i]+3)){
					CmdError ("unknown flag %s", argv[i]);
					Usage ();
					return False;
				}
			} else if (strcmp (argv_i, "-o") == 0){
				if (++i < argc)
					output_file_name = argv[i];
				else {
					CmdError ("no output file given to option -o");
					return False;
				}
			} else if (strcmp (argv_i, "-P") == 0){
				if (++i < argc)
					path_parameter = argv[i];
				else {
					CmdError ("no path list given to option -P");
					return False;
				}
			} else if (strcmp (argv_i, "-RE") == 0){
				if (++i < argc){
# if defined (_MSC_VER) || defined (_SUN_)
					std_error_file_p = fopen (argv[i],"w");
					if (std_error_file_p!=NULL)
						StdErrorReopened = True;
					else
						std_error_file_p = stderr;
#else
					freopen (argv[i],"w",StdError);
# ifdef CLEAN2
					StdErrorReopened	= True;
# endif
#endif
				} else {
					CmdError ("file name expected after -RE");
					return False;
				}
			} else if (strcmp (argv_i, "-RAE") == 0){
				if (++i < argc){
#if defined (_MSC_VER) || defined (_SUN_)
					std_error_file_p = fopen (argv[i],"a");
					if (std_error_file_p!=NULL)
						StdErrorReopened = True;
					else
						std_error_file_p = stderr;
#else
					freopen (argv[i],"a",StdError);
# ifdef CLEAN2
					StdErrorReopened	= True;
# endif
#endif
				} else {
					CmdError ("file name expected after -RAE");
					return False;
				}
			} else if (strcmp (argv_i, "-RO") == 0){
				if (++i < argc){
#if defined (_MSC_VER) || defined (_SUN_)
					std_out_file_p = fopen (argv[i],"w");
					if (std_out_file_p!=NULL)
						StdOutReopened = True;
					else
						std_out_file_p = stdout;
#else
					freopen (argv[i],"w",StdOut);
# ifdef CLEAN2
					StdOutReopened	= True;
# endif
#endif
				} else {
					CmdError ("file name expected after -RO");
					return False;
				}
			} else if (strcmp (argv_i, "-RAO") == 0){
				if (++i < argc){
#if defined (_MSC_VER) || defined (_SUN_)
					std_out_file_p = fopen (argv[i],"a");
					if (std_out_file_p!=NULL)
						StdOutReopened = True;
					else
						std_out_file_p = stdout;
#else
					freopen (argv[i],"a",StdOut);
# ifdef CLEAN2
					StdOutReopened	= True;
# endif
#endif
				} else {
					CmdError ("file name expected after -RAO");
					return False;
				}
			} else {
				CmdError ("unknown flag %s", argv_i);
				Usage ();
				return False;
			}
		} else {
			/* process (non-flag) argument */
			if (fname){
				CmdError ("only one input file allowed");
				return False;
			}
			fname = argv[i];
		}
	}

#ifdef CLEAN2
		*file_name_p=fname;
		*output_file_name_p=output_file_name;
	
	#ifdef _MAC_
		GetInitialPathList();
	#endif
	
		InitCompiler();
	
		return True;
	}
	/*
	Bool CallCompiler (int argc, char **argv)
	{
		char *fname, *output_file_name;
	
		if (!ParseCommandArgs (argc,argv,&fname,&output_file_name))
			return False;
	*/
#else

	if (fname)
		return Compile (fname,output_file_name);
	else if (DoVerbose){
		FPrintF (StdOut, "\nConcurrent Clean Compiler (Version %d.%d)\n\n", VERSION / 1000, VERSION % 1000);
		return True;
	} else {
		CmdError ("no input file given");
		Usage ();
		return False;
	}
}

#if ! defined (MAIN_CLM)
int main (int argc, char *argv[])
{
# if defined (_MSC_VER) || defined (_SUN_)
	std_out_file_p = stdout;
	std_error_file_p = stderr;
# endif

	if (CallCompiler (argc-1, & argv[1]))
		return 0;
	else
		return 1;
}
#endif

#endif
