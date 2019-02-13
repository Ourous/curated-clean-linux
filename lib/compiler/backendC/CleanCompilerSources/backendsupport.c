
# include "compiledefines.h"
# include "types.t"
# include "system.h"
# include "comsupport.h"
# include "backendsupport.h"


/*
	Utilities
	=========
*/
# ifdef _WINDOWS_
# undef _WINDOWS_
# include <windows.h>
# define	Debugger()	DebugBreak();
# else
# define	Debugger()	{ * (int *) NULL = 0; }
# endif

void
AssertionFailed (char *conditionString, char *file, int line)
{	
	FPrintF (StdError, "Error in backend: File %s, Line %d (%s)\n", file, line, conditionString);

# ifdef _WINDOWS_
	{
		static char error[200];

		sprintf (error, "Error in backend: File %s, Line %d (%s)\nDebug ?", file, line, conditionString);
	
		if (MessageBox (NULL,error,"AssertionFailed",MB_YESNO)==IDYES)
			Debugger ();
	}
#else
# ifdef _MAC_
	{
		FILE *f;
	
		f=fopen ("AssertionFailedError","w");
		if (f!=NULL){
			FPrintF (f, "Error in backend: File %s, Line %d (%s)\n", file, line, conditionString);
			fclose (f);
		}
	}
# endif
	Debugger ();
#endif
} /* AssertionFailed */

void
fatal_backend_error (char *s)
{
	FPrintF (StdError, "Error in backend: %s\n", s);

#ifdef _MAC_
	{
		FILE *f;
	
		f=fopen ("AssertionFailedError","w");
		if (f!=NULL){
			FPrintF (f, "Error in backend: %s\n", s);
			fclose (f);
		}
	}
#endif	
	Debugger ();
}

void debug_message (const char *format,...)
{
	va_list ap;
	
	va_start (ap,format);
	vfprintf (StdError,format,ap);
	va_end (ap);

#ifdef _MAC_
	{
		FILE *f;
	
		f=fopen ("DebugMessages","a");
		if (f!=NULL){
			va_start (ap,format);
			vfprintf (f,format,ap);
			va_end (ap);
			fclose (f);
		}
	}
#endif	
}

#if 1
/*
	Memory management
	=================
*/

static enum {kMemoryInitClear, kMemoryInitSet} gMemoryInit = kMemoryInitSet;

# define	kDefaultConvertBufferSize	(32 * 1024)

typedef struct convert_buffer ConvertBufferS, *ConvertBufferP;

struct convert_buffer
{
	ConvertBufferP	cb_next;
	int				cb_size;
	char			cb_memory [kDefaultConvertBufferSize]; /* or more bytes */
};

static void
InvalidateMemory (void *memory, size_t size)
{
	char	value, *p;
	int		i;

	switch (gMemoryInit)
	{
		case kMemoryInitClear:
			value	= 0;
			break;
		case kMemoryInitSet:
			value	= ~0;
			break;
		default:
			Assert (False);
			break;
	}

	p	= memory;
	for (i = 0; i < size; i++)
		*p++	= value;
} /* InvalidateMemory */

static ConvertBufferP	gFirstBuffer = NULL, gCurrentBuffer = NULL;
static char 			*gMemory;
static long gBytesLeft = 0;

static void
AllocConvertBuffer (int min_size)
{
	ConvertBufferP	newBuffer;
	int new_convert_buffer_size;

	new_convert_buffer_size=kDefaultConvertBufferSize;
	while (new_convert_buffer_size<min_size)
		new_convert_buffer_size+=kDefaultConvertBufferSize;

	newBuffer	= (ConvertBufferP) malloc (sizeof (ConvertBufferS)+(new_convert_buffer_size-kDefaultConvertBufferSize));
	if (newBuffer == NULL)
		FatalCompError ("backendsupport.c", "AllocConvertBuffer", "out of memory");

	newBuffer->cb_size=new_convert_buffer_size;
	
	if (gFirstBuffer == NULL)
		gCurrentBuffer	= gFirstBuffer				= newBuffer;
	else
		gCurrentBuffer	= gCurrentBuffer->cb_next	= newBuffer;

	gCurrentBuffer->cb_next	=	NULL;

	gBytesLeft	= new_convert_buffer_size;
	gMemory		= gCurrentBuffer->cb_memory;

	InvalidateMemory (gMemory, new_convert_buffer_size);

	if (gFirstBuffer == NULL)
		gFirstBuffer	= gCurrentBuffer;
} /* AllocConvertBuffer */

void
FreeConvertBuffers (void)
{
	ConvertBufferP	buffer;

	buffer	= gFirstBuffer;

	while (buffer != NULL)
	{
		ConvertBufferP	nextBuffer;

		nextBuffer	= buffer->cb_next;

		InvalidateMemory (buffer,buffer->cb_size);
		free (buffer);

		buffer	= nextBuffer;
	}

	gFirstBuffer	= NULL;
	gCurrentBuffer	= NULL;
	gBytesLeft	= 0;
} /* FreeConvertBuffers */

void *
ConvertAlloc (SizeT size)
{
	void	*memory;

	size	= (size+3) & ~3;

	if (size > gBytesLeft){
		AllocConvertBuffer (size);

		if (size>gBytesLeft){
			static char s[100];
			
			sprintf (s,"ConvertAlloc: size = %ld, gBytesLeft = %ld",(long)size,gBytesLeft);
			fatal_backend_error (s);
		}
	}

	Assert (size <= gBytesLeft);

	memory	= gMemory;	
	gBytesLeft	-= size;
	gMemory	+=	size;

	return ((void *) memory);
} /* ConvertAlloc */
#endif