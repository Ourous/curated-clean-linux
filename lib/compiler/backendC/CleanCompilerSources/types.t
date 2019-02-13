
#if !defined (_THE__TYPES_)
#define _THE__TYPES_

#if defined (__MWERKS__) && defined (_X86_)
# define _WINDOWS_
#endif

#if (defined (__MWERKS__) && !defined (_X86_)) || defined (__MRC__)
# define POWER 1
#endif

#define NIL			0L
#define Null		0L

#define REALSIZE	2 /*1*/
#define FILESIZE	2

#define KBYTE		1024L

#ifdef THINK_C
	typedef enum {
			False = 0, True, MightBeTrue
		} Bool;
#else
	typedef unsigned Bool;
		enum {
			False = 0, True, MightBeTrue
		};
#endif

typedef enum
		{abcFile = 1, iclFile, dclFile, applFile, assFile, sunAssFile,
		 obj00File, obj20File, obj81File,
		 otherFile, hsFile
		} FileKind;

#define FileNameMax		256
#define SeekSet
#define SeekCur
#define SeekEnd

typedef unsigned long SysTime;

#ifdef _WINDOWS_
# include <stdarg.h>
# ifdef _WIN64
#  undef _WINDOWS_
#  include <windows.h>
#  define FileTime struct _FILETIME
# else
#  ifdef __MWERKS__
# 	include <x86_prefix.h>
#  else
#   ifndef _X86_
#    define _X86_
#   endif
#  endif
#  include <windef.h>
#  include <winbase.h>
#  define FileTime FILETIME
# endif
#else
# if defined (POWER) && defined (KARBON)
#  include <UTCUtils.h>
typedef UTCDateTime FileTime;
# else
typedef unsigned long FileTime;
# endif
#endif

#define NoFile			((FileTime) 0)

#endif
