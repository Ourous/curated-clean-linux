typedef struct clean_string {int length; char chars [1]; } *CleanString;

# define Clean(ignore)
# include "docommand.h"
/*
	Clean string
	============
*/

extern int do_command (char *command);

int
DoCommandNullTerminated (CleanString command)
{
	return (do_command (command->chars));
} /* DoCommandNullTerminated */
