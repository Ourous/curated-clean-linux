
#define Clean(a)

typedef struct clean_string *CleanString;

/*	a string in Clean is:
	struct clean_string {
		size_t clean_string_length;
		char clean_string_characters[clean_string_length];
	};
	The string does not end with a '\0' !
*/

#ifndef _WIN64

/* CleanStringLength(clean_string) returns the length of the clean_string in characters */
#define CleanStringLength(clean_string) (*(unsigned long *)(clean_string))

/* CleanStringCharacters(clean_string) returns a pointer to the characters of the clean_string */
#define CleanStringCharacters(clean_string) ((char*)(1+(unsigned long *)(clean_string)))

/* CleanStringSizeInts(string_length) return size of *CleanString in integers */
#define CleanStringSizeInts(string_length) (1+(((unsigned long)(string_length)+(sizeof(unsigned long)-1))>>(1+(sizeof(unsigned long)>>2))))

/* CleanStringVariable(clean_string,string_length) defines variable clean_string with length string_length,
   before using the clean_string variable, cast to CleanString, except for the macros above  */
#define CleanStringVariable(clean_string,string_length) unsigned long clean_string[CleanStringSizeInts(string_length)]

/* CleanStringSizeBytes(string_length) return size of *CleanString in bytes */
#define CleanStringSizeBytes(string_length) ((sizeof(unsigned long)<<1)+(((unsigned long)(string_length)+(sizeof(unsigned long)-1)) & -(sizeof(unsigned long))))

typedef long *CleanIntArray;

/* CleanIntArraySize(clean_array) returns the size (number of elements) of the clean_int_array */
#define CleanIntArraySize(clean_int_array) (((unsigned long *)(clean_int_array))[-2])

/* CleanRealArraySize(clean_real_array) returns the size (number of elements) of the clean_real_array */
#define CleanRealArraySize(clean_real_array) (((unsigned long *)(clean_real_array))[-2])

/* CleanCharArraySize(clean_char_array) returns the size (number of elements) of the clean_char_array */
#define CleanCharArraySize(clean_char_array) (((unsigned long *)(clean_char_array))[-1])

#else

/* CleanStringLength(clean_string) returns length of the clean_string in characters */
#define CleanStringLength(clean_string) (*(unsigned __int64 *)(clean_string))

/* CleanStringCharacters(clean_string) returns a pointer to the characters of the clean_string */
#define CleanStringCharacters(clean_string) ((char*)(1+(unsigned __int64 *)(clean_string)))

/* CleanStringSizeInts(string_length) return size of *CleanString in integers */
#define CleanStringSizeInts(string_length) (1+(((unsigned __int64)(string_length)+7)>>3))

/* CleanStringVariable(clean_string,string_length) defines variable clean_string with length string_length,
   before using the clean_string variable, cast to CleanString, except for the macros above  */
#define CleanStringVariable(clean_string,string_length) unsigned __int64 clean_string[CleanStringSizeInts(string_length)]

/* CleanStringSizeBytes(string_length) return size of *CleanString in bytes */
#define CleanStringSizeBytes(string_length) (8+(((unsigned __int64)(string_length)+7) & -8))

typedef __int64 *CleanIntArray;

/* CleanIntArraySize(clean_array) returns the size (number of elements) of the clean_int_array */
#define CleanIntArraySize(clean_int_array) (((unsigned __int64 *)(clean_int_array))[-2])

/* CleanRealArraySize(clean_real_array) returns the size (number of elements) of the clean_real_array */
#define CleanRealArraySize(clean_real_array) (((unsigned __int64 *)(clean_real_array))[-2])

/* CleanCharArraySize(clean_char_array) returns the size (number of elements) of the clean_char_array */
#define CleanCharArraySize(clean_char_array) (((unsigned __int64 *)(clean_char_array))[-1])

#endif

typedef double *CleanRealArray;

typedef unsigned char *CleanCharArray;
