
#define kCopyStringLength 512
#define MaxUnsigned	65535

/*	The maximum arity of tuples is defined by MaxTupleArity */

#define MaxNodeArity	32
#define MaxGeneratedIdentSize 512

/*	The scanner maintains a buffer for holding identifiers and literals whereof the
	size is indicated by ScanBuffSize
*/
	
#define ScanBuffSize	(KBYTE*32)

/*	Identifiers may cantain upto IdLength characters */

#define IdLength		256

/* 	The actual size of the id-buffer is greater than the IdLength.
	This allows us to add extensions of length 4 to identifiers 
	(used in module names) */

#define MaxIdLength      ((SizeT) (IdLength + 4)) 
#define MaxStrLength	256 /* maximum number of characters in a string */
#define MaxCharLength	6   /* maximum number of chararcters in a character denotation */
#define MaxNrOfDigits	80  /* maximum number of digits in a real */
#define MaxNumLength	(MaxNrOfDigits + 4) /* maximum number of characters in a real
									   or integer denotation */

#define MaxInstrLength   256 /* maximum number of characters in an instruction */

/*	Identifiers and literals are stored in different tables. The size of these tables
	are given below */

#define SymbTabSize ((SizeT) KBYTE)
#define NodeTabSize ((SizeT) KBYTE)
#define ModTabSize	((SizeT) 32)
#define LitTabSize	((SizeT) KBYTE)

/*
	Compsupport
*/

/*	The compiler uses its own storage administration. When some storage is required
	it is checked whether or not this storage is available. If not, a new memory
	block of size MemBlockSize is allocated. Keeping the size large will slightly
	increase the performance of the memory allocator.
*/

#ifdef __MWERKS__
# define MemBlockSize ((SizeT) (16*KBYTE))
#else
# define MemBlockSize ((SizeT) (32*KBYTE))
#endif
#define TH_BlockSize ((SizeT) (16*KBYTE))

/*	TypeChecker */

/*	For efficient internal garbage collection the type checker uses its own storage
	administration. The constant TCWorkSpaceSize has the same function as MemBlockSize
	in comsupport.
*/

#define TCWorkSpaceSize ((SizeT) (16*KBYTE))

/*	Code Generator */

/* The size of objects expressed in amounts of stack entries are given below */

#define SizeOfInt			1
#define SizeOfBool			1
#define SizeOfChar			1
#define SizeOfReal			REALSIZE
#define SizeOfFile			FILESIZE
#define SizeOfVoid			1
#define SizeOfProcId		1
#define SizeOfAStackElem 	1

#define NrOfGlobalSelectors	6

/* Inline instruction administration (part of the code generator) */

#define LineLength		300 /* maximum number of charcters on one line */
#define MaxInlineInstr	60  /* maximum number of instructions that may be 
						substituted for one system call */

#define InlineBuffSize	((SizeT) KBYTE * 32) /* the size of the buffer
						containing all the inline instructions */
