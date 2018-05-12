/*
	Version 1.0.3
	Ronny Wichers Schreur
	ronny@cs.kun.nl
*/

# ifdef _WINDOWS_
# include <windows.h>
# else
# include <stdlib.h>
# endif


# define kVariableSize	1

# ifndef NULL
# define NULL	((void *) 0)
# endif

/* defined by the Clean run-time system */
extern int global_argc;
extern char **global_argv;

typedef struct
{
	size_t		length;
	char	characters [kVariableSize];
} *CleanString;

static int
CStringLength (char *s)
{
	char	*begin;

	begin	= s;
	while (*s != '\0')
		s++;

	return (s - begin);
} /* CStringLength */


# ifdef _WINDOWS_

/* return size of environment variable's value (including null-terminator)
	zero if variable doesn't exist */
int
ArgEnvGetEnvironmentVariableSizeC (CleanString name)
{
	char	smallBuffer;

	/* Assume that name has been null-terminated in Clean */
	return (GetEnvironmentVariableA (name->characters, &smallBuffer, 0));
} /* ArgEnvGetEnvironmentVariableSizeC */

/* copy value of environment variable to Clean string
	return False if value had to be truncated */
int
ArgEnvGetEnvironmentVariableCharsC (CleanString value, CleanString name)
{
	int	size, length;

	length	= value->length;

	/* Assume that name has been null-terminated in Clean */
	size	= GetEnvironmentVariableA (name->characters, value->characters, length);

	if (size <= length)
	{
		size	=	CStringLength (value->characters);
		if (size <= length)
			value->length	= size;
	}

	return (size >= value->length);
} /* ArgEnvGetEnvironmentVariableCharsC */

# else /* ifndef _WINDOWS_ */

void
ArgEnvGetEnvironmentVariableC (CleanString name, size_t *sizeP, char **stringP)
{
	char	*value;

	/* Assume that name has been null-terminated in Clean */
	value	= getenv (name->characters);
	if (value == NULL)
		*sizeP	= 0;
	else
		*sizeP	= CStringLength (value);

	*stringP	= value;
} /* ArgEnvGetEnvironmentVariableCharsC */

# endif /* _WINDOWS_ */

int
ArgEnvGetCommandLineCountC (void)
{
	return (global_argc);
} /* ArgEnvGetCommandLineCountC */

void
ArgEnvGetCommandLineArgumentC (int i, size_t *sizeP, char **stringP)
{
	char	*arg;

	/* Assume that i is within bounds */
	arg	= global_argv [i];

	*sizeP		= CStringLength (arg);
	*stringP	= arg;
} /* ArgEnvGetCommandLineArgumentC */

/* copy C string to Clean string
	return False if string had to be truncated */
int
ArgEnvCopyCStringToCleanStringC (char *cString, CleanString cleanString)
{
	int		i, length;
	char	*to;

	length	= cleanString->length;

	to	= cleanString->characters;
	i	= 0;
	while (*cString != '\0' && i < length)
	{
		i++;
		*to++	= *cString++;
	}
	cleanString->length	= i;

	return (i <= length);
} /* ArgEnvCopyCStringToCleanStringC */
