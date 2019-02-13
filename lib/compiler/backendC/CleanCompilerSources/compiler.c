
#undef PROFILE

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"
#include "syntaxtr.t"
#include "comsupport.h"
#include "checker.h"
#include "compiler.h"
#include "codegen_types.h"
#include "codegen.h"
#include "statesgen.h"

#ifdef _PROFILE_
/* FROM profile IMPORT */
	extern DumpProfile ();
#endif

char *CurrentFilePath;

static Bool RemoveExtension (char *name)
{
	int len;

	len = strlen (name);

	if (len>=4 && name[len-4]=='.'){	
		name [len-4] = '\0';
		return True;
	} else
		return False;
}

static void AddExtension (char *name)
{
	name [strlen (name)] = '.';
}

static void ExecCompiler (char *fname,char *output_file_name)
{
	ImpMod imod;
	
/*	PrintVersion (); */

 	if (fname){	
		Bool hadext;
		char *p;
		
		CurrentFilePath = fname;
		
		hadext = RemoveExtension (CurrentFilePath);

		for (p=CurrentFilePath; *p!='\0'; ++p)
#if defined (_MAC_)
			if (*p == ':')
#elif defined (_WINDOWS_) || defined (OS2)
			if (*p == '\\')
#else
			if (*p == '/')
#endif
				fname = p+1;
		
		/* Parse and check */
		if (! (imod = ParseAndCheckImplementationModule (fname)))
			return;
			
		/* Code generation */
		if (output_file_name!=NULL){
			Bool hadext;
		
			hadext = RemoveExtension (output_file_name);
		
#ifdef DUMP_AND_RESTORE
			if (gDumpAndRestore){
				if (!CompilerError)
					CoclBackEnd (imod, output_file_name);
			} else
#endif
			CodeGeneration (imod,output_file_name);
		
			if (hadext)
				AddExtension (output_file_name);
		} else
#ifdef DUMP_AND_RESTORE
			if (gDumpAndRestore){
				if (!CompilerError)
					CoclBackEnd (imod, fname);
			} else
#endif
			CodeGeneration (imod, fname);

		if (hadext)
			AddExtension (CurrentFilePath);
	} else
		CmdError ("No source file specified");
}

#ifdef PROFILE
#include "profile.h"
#endif

#ifdef _MAC_
	extern void GetInitialPathList (void);
#endif

Bool Compile (char *fname,char *output_file_name)
{
#ifdef PROFILE
	InitProfile (900,300);
	freopen ("Profile","w",stdout);
#endif

#ifdef _MAC_
	GetInitialPathList();
#endif

	if (setjmp (ExitEnv)==0){
		InitCompiler ();
		ExecCompiler (fname,output_file_name);
	} else
		CompilerError = True;

	ExitCompiler ();

#ifdef PROFILE
	DumpProfile();
#endif

	return ! CompilerError;
}
