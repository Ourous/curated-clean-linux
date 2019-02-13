/*
       system.h
       Author: Eric Nocker
       At: Department of Computer Science
           University of Nijmegen
*/

#define _SYSTEM_

#if defined (__MWERKS__) && defined (_X86_)
#	define _WINDOWS_
#endif

#if defined (applec) || (defined (__MWERKS__) && !defined (_X86_)) || defined (__MRC__)
#	define _MAC_
#	define __ppc__
#endif

#define _DEBUG_

#if ! (defined (_MAC_) || defined (_SUN_))
# define NEW_APPLY
#endif

#if defined (_MAC_)
# include "mac.h"
#elif defined (_SUN_)
# include "sun.h"
#elif defined (OS2)
#  include "os2.h"
#elif defined (_WINDOWS_)
#  include "windows_io.h"
#else
#  include "standard.h"
#endif

#include "types.t"

#ifdef GEN_SUPPORT_H
# include "gensupport.h"
#else

#define MAXPATHLEN 1024

extern char *PATHLIST;

extern char *GetFileExtension (FileKind kind);
extern File FOpen (char *wname, FileKind kind, char *mode);
#if defined(WRITE_DCL_MODIFICATION_TIME) && WRITE_DCL_MODIFICATION_TIME
extern File FOpenWithFileTime (char *file_name,FileKind kind, char *mode,FileTime *file_time_p);
#endif
extern int FDelete (char *fname, FileKind kind);
extern int FClose (File f);

extern int FPutS (char *s, File f);
extern size_t FWrite (void *ptr, size_t size, size_t count, File f);
#ifdef _VARARGS_
extern int FPrintF (File f, char *fmt,...);
#else
extern int FPrintF (); /* (File w, char *fmt,...) */
#endif

#ifndef __ppc__
extern char *FGetS (char *s, int n, File f);
#endif
extern size_t FRead (void *ptr, size_t size, size_t count, File f);
extern int FSeek (File f, long offset, int origin);
extern long FTell (File f);
extern FileTime FGetFileTime (char *fname, FileKind kind);
#if defined(WRITE_DCL_MODIFICATION_TIME) && WRITE_DCL_MODIFICATION_TIME
extern void FWriteFileTime (FileTime file_time,File f);
#endif

#ifdef _VARARGS_
	extern void DoError (char *fmt,...);
	extern void DoFatalError (char *fmt,...);
	extern void CmdError (char *errormsg,...);
#else
	extern void DoError ();
	extern void DoFatalError ();
	extern void CmdError ();
#endif

extern void (*SetSignal (void (*f) (void))) (void);
	
extern int CheckInterrupt (void);

extern void *Alloc (unsigned long count, SizeT size);

extern void Free (void *p);

#ifdef THINK_C
#define ReSize(A) (((A)+1) & ~1)
#else
#define ReSize(A) (((A)+3) & ~3)
#endif

extern int System (char *s);

#endif
