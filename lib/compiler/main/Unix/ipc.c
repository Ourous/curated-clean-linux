/*
	Unix clm/cocl interface

	Ronny Wichers Schreur

*/
# include <stdio.h>
# include <stdlib.h>
# include <stdarg.h>
# include <strings.h>

/*
	Clean string
	============
*/

#ifdef _WIN64
typedef struct clean_string {__int64 length; char chars [1]; } *CleanString;
#else
typedef struct clean_string {long length; char chars [1]; } *CleanString;
#endif

# define Clean(ignore)
# include "ipc.h"

static void
log (char *format, ...)
{
#ifdef DEBUG
	va_list ap;

	va_start (ap, format);
	(void) fputs("                        cocl: ", stderr);
	(void) vfprintf(stderr, format, ap);
	va_end(ap);
#else /* ifndef DEBUG */
#endif
}

static char *
ConvertCleanString (CleanString string)
{
	int		length;
	char	*copy;

	length	= string->length;
	copy	= malloc (length+1);
	strncpy (copy, string->chars, length);
	copy [length]	= '\0';

	return (copy);
} /* ConvertCleanString */

static FILE *commands, *results;
static char *command_buffer_p=NULL;
static int command_buffer_size=0;

static void
crash (void)
{
	int	*p;

	p	= NULL;
	log ("crashing\n");
	*p	= 0;
} /* crash */

static void
hang (void)
{
	log ("hanging\n");
	for (;;)
		;
} /* hang */

int open_pipes (CleanString commands_clean, CleanString results_clean)
{
	char *commands_name, *results_name;

	commands_name	= ConvertCleanString (commands_clean);
	results_name	= ConvertCleanString (results_clean);

    if ((commands = fopen(commands_name, "r")) == NULL)
    {
		log("commands = %s\n",commands_name);
		perror("fopen commands");
		return -1;
    }
    if ((results = fopen(results_name, "w")) == NULL)
    {
		log("results = %s\n",results_name);
		perror("fopen results");
		return -1;
    }
	return 0;
}

int get_command_length (void)
{
	log ("reading command\n");

	if (command_buffer_p==NULL){
		command_buffer_p = malloc (1024);
		if (command_buffer_p==NULL)
			return -1;
		command_buffer_size=1024;
	}

	{
		int n_chars,max_n_chars,c;

		n_chars=0;
		max_n_chars=command_buffer_size-1;

		do {
			c=fgetc (commands);
			if (c==EOF)
				break;
			command_buffer_p[n_chars++]=c;
			if (n_chars==max_n_chars){
				char *new_command_buffer_p;

				new_command_buffer_p = realloc (command_buffer_p,command_buffer_size<<1);
				if (new_command_buffer_p==NULL){
					command_buffer_p[n_chars-1]='\0';
					return -1;
				}
				command_buffer_p=new_command_buffer_p;
				command_buffer_size=command_buffer_size<<1;
				max_n_chars=command_buffer_size-1;
			}
		} while (c!='\n');

		command_buffer_p[n_chars]='\0';

		log ("command = %s", command_buffer_p);

		return n_chars;
	}
}

int get_command (CleanString cleanString)
{
	log ("%s\n", command_buffer_p);
	strncpy (cleanString->chars, command_buffer_p, cleanString->length);
	return (0);
}

int send_result (int result)
{
	int	r;

	if (fprintf (results, "%d\n", result) > 0)
		r=0;
	else
		r=-1;
	fflush (results);

	return r;
}
