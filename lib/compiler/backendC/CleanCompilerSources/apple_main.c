
#include "compiledefines.h"

#define CG_PPC_XO

#ifdef KARBON
# define TARGET_API_MAC_CARBON 1
#endif

#include <stdio.h>
#if !defined (GNU_C)
# include <unix.h>
# include <SIOUX.h>
#endif

#include <quickdraw.h>
#include <fonts.h>
#include <events.h>
#include <windows.h>
#include <memory.h>
#include <resources.h>
#include <menus.h>
#include <OSUtils.h>
#include "AppleEvents.h"
#include "Gestalt.h"
#include "AERegistry.h"

#include "types.t"
#include "system.h"
#include "path_cache.h"
#include "compiler.h"

extern void clear_inline_cache (void);

#undef BACKGROUND
#define MW_DEBUG 0
#define NO68K

#ifndef BACKGROUND
#	undef NO_REDIRECT_STDFILES
#	undef STDIO_WINDOW
#endif

#define LINKER
#define CODE_GENERATOR
#undef PROFILE

#if 1

#define kSleepMax 50000
 
static Boolean gAppleEventsFlag, gQuitFlag;
static long gSleepVal;

static pascal OSErr DoAEOpenApplication (const AppleEvent *theAppleEvent,AppleEvent *replyAppleEvent,unsigned long refCon)
{
	return noErr;
}
 
static pascal OSErr DoAEOpenDocuments (const AppleEvent *theAppleEvent,AppleEvent *replyAppleEvent,unsigned long refCon)
{
	return errAEEventNotHandled;
}
 
static pascal OSErr DoAEPrintDocuments (const AppleEvent *theAppleEvent,AppleEvent *replyAppleEvent,unsigned long refCon)
{
	return errAEEventNotHandled;
}
 
static pascal OSErr DoAEQuitApplication (const AppleEvent *theAppleEvent,AppleEvent *replyAppleEvent,unsigned long refCon)
{
	gQuitFlag = true;
	return noErr;
}

#include <string.h>

extern int CallCompiler (int argc,char **argv);

#ifdef CODE_GENERATOR
# ifdef __cplusplus
 extern "C" { int generate_code (int,char **); }
#  ifndef NO68K
 extern int generate_code68 (int,char **);
#  endif
# else
 extern int generate_code (int,char **);
#  ifndef NO68K
 extern int generate_code68__FiPPc (int,char **);
#define generate_code68 generate_code68__FiPPc
#  endif
# endif
#endif

#if defined (LINKER) && !defined (NO68K)
# ifdef __cplusplus
 extern "C" { int link_application_argc_argv (int,char **); }
# else
 extern int link_application_argc_argv (int,char **);
# endif
#endif

char return_error_string[200];

#ifdef CG_PPC_XO
extern int generate_code_xo (int argc,char **argv,char *return_error_string_p,int *compiler_id_p);
extern int generate_code_o (int argc,char **argv,char *return_error_string_p,int *compiler_id_p);
#endif

#ifdef CLEAN2
int compiler_id=-1;
#else
extern int compiler_id;
#endif

int do_command (char *command)
{
	char *p,*(argv[257]);
	int argc,result;
	int redirect_stdout,redirect_stderr;

	result=0;
	
	redirect_stdout=0;
	redirect_stderr=0;
	
	argc=0;
	p=command;

	while (*p==' ' || *p=='\t')
		++p;

	while (*p!='\0' && argc<256){
		if (*p=='>' || *p=='³'){
			int redirection_char;
			char *file_name;
			
			redirection_char=*p;
			
			++p;
			while (*p==' ' || *p=='\t')
				++p;
			if (*p=='\0')
				break;

			if (*p=='\''){
				char c,*d_p;
				
				++p;
				file_name=p;
				
				d_p=p;
				
				c=*p;
				while (!(c=='\'' && p[1]!='\'') && c!='\0'){
					*d_p++=c;
					if (c=='\'')
						++p;
					c=*++p;
				}
				
				if (*p=='\0'){
					*d_p='\0';
					break;
				}
				
				*d_p='\0';
				++p;
			} else {
				file_name=p;
				
				while (*p!=' ' && *p!='\t' && *p!='\0')
					++p;
				if (*p!='\0')
					*p++='\0';
			}
			
			if (redirection_char=='>' && redirect_stdout==0){
#ifndef NO_REDIRECT_STDFILES
				freopen (file_name,"w",stdout);
				redirect_stdout=1;
#endif
			} else if (redirection_char=='³' && redirect_stderr==0){
#ifndef NO_REDIRECT_STDFILES
				freopen (file_name,"w",stderr);
				redirect_stderr=1;
#endif
			}

			if (*p=='\0')
				break;
			
			while (*p==' ' || *p=='\t')
				++p;
			continue;
		}

		if (*p=='\''){
			char c,*d_p;
			
			++p;
			argv[argc]=p;

			d_p=p;
			
			c=*p;
			while (!(c=='\'' && p[1]!='\'') && c!='\0'){
				*d_p++=c;
				if (c=='\'')
					++p;
				c=*++p;
			}
			
			if (*p=='\0'){
				*d_p='\0';
				break;
			}
			
			++argc;
			*d_p='\0';
			++p;
		} else {
			argv[argc++]=p;
			while (*p!=' ' && *p!='\t' && *p!='\0')
				++p;
	
			if (*p!='\0')
				*p++='\0';
		}
				
		while (*p==' ' || *p=='\t')
			++p;			
	}
	argv[argc]=NULL;

/*	{
		int n;
		
		for (n=0; n<argc; ++n)
			printf ("%d %s\n",n,argv[n]);
	}
*/

	if (argc>0){
#ifdef CLEAN2
		if (0)
			;
#else
		if (!strcmp (argv[0],"cocl")){
			if (argc>=2 && !strcmp ("-clear_cache",argv[1])){
				result=CallCompiler (argc-2,&argv[2]);
				clear_path_cache();
				clear_inline_cache();
				FreePathList();	
			} else
				result=CallCompiler (argc-1,&argv[1]);
		}
#endif
#ifdef CODE_GENERATOR
# ifndef GNU_C
		else if (!strcmp (argv[0],"cg"))
			result=generate_code (argc,&argv[0]);
#  ifdef CG_PPC_XO
		else if (!strcmp (argv[0],"cg_xo"))
			result=generate_code_xo (argc,&argv[0],return_error_string,&compiler_id);
		else if (!strcmp (argv[0],"cg_o"))
			result=generate_code_o (argc,&argv[0],return_error_string,&compiler_id);
#  endif
# else
		else if (!strcmp (argv[0],"cg_o"))
			result=generate_code_o (argc,&argv[0],return_error_string,&compiler_id);
# endif
# ifndef NO68K
		else if (!strcmp (argv[0],"cg68"))
			result=generate_code68 (argc,&argv[0]);
# endif
#endif
#if defined (LINKER) && !defined (NO68K)
		else if (!strcmp (argv[0],"linker"))
			result=link_application_argc_argv (argc,&argv[0]);
#endif
		else if (!strcmp (argv[0],"clear_cache")){		
			clear_path_cache();
			clear_inline_cache();
			FreePathList();
		} else {
			result=-1;
			strcpy (return_error_string,"unknown command");
		}
	}

	if (redirect_stdout)
		fclose (stdout);
	
	if (redirect_stderr)
		fclose (stderr);
			
	return result;
}

static char script_string[16001];

#ifdef CLEAN2
int compiler_id=-1;
#else
extern int compiler_id;
#endif

pascal OSErr do_script_apple_event (const AppleEvent *apple_event,AppleEvent *replyAppleEvent,unsigned long refCon)
{
	DescType returned_type;
	long actual_size;
	int error;
	
	error=AEGetParamPtr (apple_event,keyDirectObject,'TEXT',&returned_type,&script_string,sizeof (script_string),&actual_size);

	if (error==noErr && actual_size<=16000){
		int return_error_string_length;
		
		script_string[actual_size]='\0';
		return_error_string[0]='\0';

#if !MW_DEBUG
		error=do_command (script_string);
#endif

		if (compiler_id>=0){
			error += (compiler_id+1)<<1;

			compiler_id = -1;
		}

		return_error_string_length=strlen (return_error_string);
		if (return_error_string_length!=0){
			AEPutParamPtr (replyAppleEvent,keyErrorString,typeChar,return_error_string,return_error_string_length);
		}
	}
	
	return error;
}

#if defined (KARBON)
# define NewAEEventHandlerProc(userRoutine) NewAEEventHandlerUPP(userRoutine)
#endif

static void InitAppleEventsStuff (void)
{
	OSErr retCode;

	if (!gAppleEventsFlag)
		return;
	
	retCode = AEInstallEventHandler (kCoreEventClass,kAEOpenApplication,NewAEEventHandlerProc (DoAEOpenApplication),0,false);

	if (retCode==noErr)
		retCode = AEInstallEventHandler (kCoreEventClass,kAEOpenDocuments,NewAEEventHandlerProc (DoAEOpenDocuments),0,false);

	if (retCode==noErr)
		retCode = AEInstallEventHandler (kCoreEventClass,kAEPrintDocuments,NewAEEventHandlerProc (DoAEPrintDocuments),0,false);

	if (retCode==noErr)
		retCode = AEInstallEventHandler (kCoreEventClass,kAEQuitApplication,NewAEEventHandlerProc (DoAEQuitApplication),0,false);

	if (retCode==noErr)
		retCode = AEInstallEventHandler (kAEMiscStandards,kAEDoScript,NewAEEventHandlerProc (do_script_apple_event),0,false);

	if (retCode!=noErr)
		DebugStr("\pInstall event handler failed");
}

static void do_high_level_event (EventRecord *theEventRecPtr)
{
#if MW_DEBUG
	script_string[0]=0;
#endif

	AEProcessAppleEvent (theEventRecPtr);
	
#if MW_DEBUG
	if (script_string[0]){
		do_command (script_string);
		script_string[0]=0;
	}
#endif

}
 
extern short InstallConsole (short fd);

#ifdef PROFILE
# include <Profiler.h>
#endif

#if !defined (GNU_C)

int /*clean_compiler_*/ main (void)
{
	OSErr retCode;
	long gestResponse;
	EventRecord mainEventRec;
	Boolean eventFlag;

#ifndef KARBON
	SetApplLimit (GetApplLimit() - 200*1024);

	InitGraf (&qd.thePort);
	InitFonts();
#endif
	FlushEvents (everyEvent,0);

#ifndef BACKGROUND
# ifndef KARBON
	InitWindows();
# endif
	InitCursor();
# ifndef KARBON
	InitMenus();
# endif
#endif

#if !defined (GNU_C)
	_fcreator='3PRM';
#endif
	gQuitFlag = false;
	gSleepVal = kSleepMax;
	
	retCode = Gestalt(gestaltAppleEventsAttr,&gestResponse);
	if (retCode==noErr && (gestResponse & (1<<gestaltAppleEventsPresent))!=0)
		gAppleEventsFlag = true;
	else
		gAppleEventsFlag = false;

#if defined (STDIO_WINDOW)
	SIOUXSettings.autocloseonquit=1;
	SIOUXSettings.showstatusline=0;
	SIOUXSettings.asktosaveonclose=0;

	printf ("\n");
#endif

#if !defined (BACKGROUND) && !defined (STDIO_WINDOW)
 	fclose (stdout);
 	fclose (stderr);
#endif
 
	InitAppleEventsStuff();

#ifdef PROFILE
	if (ProfilerInit(/*collectSummary*/collectDetailed,bestTimeBase,10000,10)!=0)
		return 0;
#endif
    
	while (!gQuitFlag) {
		eventFlag = WaitNextEvent (everyEvent,&mainEventRec,gSleepVal,nil);

#ifdef STDIO_WINDOW
		if (SIOUXHandleOneEvent (&mainEventRec))
			continue;
#endif
		if (mainEventRec.what==keyDown)
			break;
		
		if (mainEventRec.what==kHighLevelEvent)
			do_high_level_event (&mainEventRec);
	}

#ifdef PROFILE
	ProfilerDump ("\pProfile");
	ProfilerTerm();
#endif
	
	return 1;
}

#endif
#endif