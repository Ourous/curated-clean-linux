/*******************************************************************************
 *   MAC Dependencies                                                          *
 ******************************************************************************/

#ifdef THINK_C
	typedef int           TwoBytesInt;
	typedef long int      FourBytesInt;
	typedef unsigned      TwoBytesUnsigned;
	typedef unsigned long FourBytesUnsigned;
	typedef short double  EightBytesReal;
#else
	typedef short          TwoBytesInt;
	typedef int            FourBytesInt;
	typedef unsigned short TwoBytesUnsigned;
	typedef unsigned int   FourBytesUnsigned;
	typedef double  EightBytesReal;
#endif
typedef float         FourBytesReal;

#define SizeT		unsigned long
#define SizeOf(A)	((SizeT) sizeof (A))

#include <limits.h>
#define MAXUNSIGNED	ULONG_MAX

#define _VARARGS_

#include <string.h>
#include <stdlib.h>

#ifdef THINK_C
#	include <unix.h>
#else
#	include <stdio.h>
#endif

#include <setjmp.h>
#include <stdarg.h>

typedef FILE *File;

#ifdef THINK_C
	/* special for MacIntosh command line support */
	extern void InitIO (void);
	extern void GetPreferences (char *fname);
#else
	void GetInitialPathList (void);
	void FreePathList (void);
#endif

#define StdOut stdout
#if defined (__MWERKS__) || defined (__MRC__)
#define StdError stderr
#else
#define StdError stdout
#endif
#define StdVerboseH stdout
#define StdVerboseL stdout
#define StdListTypes stdout

#define FGetC(f) fgetc(f)
#define FGetS(s,n,f) fgets(s,n,f)
#define FPutC(c,f) fputc(c,f)

extern int open_dcl_file_for_block_reading (char *fname,File *file_p);
extern int read_next_block_from_dcl_file (char *buffer);

#if WRITE_DCL_MODIFICATION_TIME
extern int open_dcl_file_for_block_reading_with_file_time (char *file_name,File *file_p,FileTime *file_time_p);
#endif