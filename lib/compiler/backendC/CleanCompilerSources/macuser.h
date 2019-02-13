
/*******************************************************************************
 *                                                                             *
 *   Mac User Interface Dependencies                                           *
 *                                                                             *
 ******************************************************************************/

extern int MACUSERVAR;
#define CheckVersion if (MACUSERVAR != VERSION) DoFatalError ("Wrong version number")


typedef int           TwoBytesInt;
typedef long int      FourBytesInt;
typedef unsigned      TwoBytesUnsigned;
typedef unsigned long FourBytesUnsigned;
#ifdef applec
typedef double		  EightBytesReal;
#else
typedef short double  EightBytesReal;
#endif
typedef float         FourBytesReal;


#define SizeT		unsigned long
#define SizeOf(A)	((SizeT) sizeof (A))

#include <limits.h>
#define MAXUNSIGNED	ULONG_MAX

/*
#define _SCREENFileS_
*/

#define	_CURMOV_
#define _VARARGS_


#include <string.h>
#include <stdlib.h>
#ifdef applec
#	include <stdio.h>
#else
#	include <unix.h>
#endif
#include <setjmp.h>
#include <stdarg.h>


/*
this type is not provided by LightSpeed C

typedef unsigned long time_t;
*/
typedef	FILE	*File;


# define	FClose		fclose
# define	FPrintF		fprintf
# define	FPutC		fputc
# define	FPutS		fputs
# define	FWrite		fwrite
# define	FSeek		fseek
# define	FTell		ftell
# define	FGetC		fgetc
# define	FGetS		fgets
# define	FRead		fread
