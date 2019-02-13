
#define Clean(a)

typedef struct clean_string *CleanString;

/*	a string in Clean is:
	struct clean_string {
		unsigned long clean_string_length;
		char clean_string_characters[clean_string_length];
	};
	The string does not end with a '\0' !
*/

/* CleanStringLength(clean_string) returns the length of the clean_string in characters */
#define CleanStringLength(clean_string) (*(unsigned long *)(clean_string))

/* CleanStringCharacters(clean_string) returns a pointer to the characters of the clean_string */
#define CleanStringCharacters(clean_string) ((char*)(1+(unsigned long *)(clean_string)))

/* CleanStringSizeInts(string_length) return size of CleanString in integers */
#define CleanStringSizeInts(string_length) (1+(((unsigned long)(string_length)+(sizeof(unsigned long)-1))>>(1+(sizeof(unsigned long)>>2))))

/* CleanStringVariable(clean_string,string_length) defines variable clean_string with length string_length,
   before using the clean_string variable, cast to CleanString, except for the macros above  */
#define CleanStringVariable(clean_string,string_length) unsigned long clean_string[CleanStringSizeInts(string_length)]

