
#define CheckVersion

typedef short int		TwoBytesInt;
typedef int				FourBytesInt;
typedef short unsigned	TwoBytesUnsigned;
typedef unsigned		FourBytesUnsigned;
typedef double			EightBytesReal;
typedef float			FourBytesReal;

#define SizeT	unsigned long
#define SizeOf(A) ((SizeT) sizeof (A))
#define MAXUNSIGNED 20000000L

#include <string.h>
#include <sys/types.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

#define _VARARGS_

typedef FILE *File;

extern FILE *std_out_file_p,*std_error_file_p;
#define StdOut std_out_file_p 
#define StdError std_error_file_p
#define StdVerboseH std_out_file_p
#define StdVerboseL std_out_file_p
#define StdTrace std_out_file_p
#define StdDebug std_out_file_p
#define StdListTypes std_out_file_p

#define FGetC(f) fgetc(f)
#define FGetS(s,n,f) fgets(s,n,f)
#define FPutC(c,f) fputc(c,f)

int System (char *s);
int abs (int n);

