/*
	Clean string
	============
*/
typedef struct clean_string {size_t length; char chars [1]; } *CleanString;

/*
	Debugging
	=========
*/

extern void AssertionFailed (char *conditionString, char *file, int line);
# define	Assert(condition)	{if (!(condition)) AssertionFailed ("!(" #condition ")", __FILE__, __LINE__);}

extern void fatal_backend_error (char *s);
extern void debug_message (const char *format,...);

/*
	Memory management
	=================
*/
#if 1
extern void FreeConvertBuffers (void);
extern void	*ConvertAlloc (SizeT size);
#else
# define FreeConvertBuffers()
# define ConvertAlloc(size) CompAlloc (size)
#endif
# define ConvertAllocType(t) ((t*) ConvertAlloc (SizeOf (t)))
# define ArraySize(array)	((unsigned) (sizeof (array) / sizeof (array[0])))
