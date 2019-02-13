
#ifndef _COMSUPPORT_
#define _COMSUPPORT_

#ifndef _THE__TYPES_
#include "types.t"
#endif

#ifndef _SYSTEM_
#include "system.h"
#endif

#define   NoError 	0
#define   ErrKind1 	1
#define   ErrKind2	2

#define MINIMUM(a,b)	(((a)<(b)) ? (a) : (b))
#define MAXIMUM(a,b)	(((a)>(b)) ? (a) : (b))

extern void StaticMessage (Bool error, char *symbol_format, char *message_format, ...);

struct symbol;
extern void PrintSymbol (struct symbol *symbol,File file);

extern Bool  CompilerError;
extern char *CurrentModule, *CurrentExt, *CurrentPhase, *CompilerVersion;

extern struct symbol *CurrentSymbol;

extern char *OutName, *InName;

extern unsigned CurrentLine;

extern int ExitEnv_valid;
extern File OpenedFile;

extern jmp_buf ExitEnv;

struct heap_descr {
	char *				hd_start;
	char *				hd_end;
	char *				hd_free;
	struct heap_descr * hd_next;
};

typedef struct heap_descr * HeapDescr;

#endif

#define CompAllocType(t) ((t*)CompAlloc (SizeOf (t)))
#define CompAllocArray(s,t) ((t*)CompAlloc ((s)*SizeOf (t)))
extern void *CompAlloc (SizeT size);
extern void InitStorage (void);
extern void CompFree (void);

#define TH_AllocType(hd,t) ((t*)TH_Alloc (hd,SizeOf (t)))
#define TH_AllocArray(hd,s,t) ((t*)TH_Alloc (hd,(s)*SizeOf (t)))

extern void * TH_Alloc (HeapDescr hd, SizeT size);
extern void TH_Reset (HeapDescr hd);
extern void TH_Free (HeapDescr hd);
extern HeapDescr TH_New (void);

extern Bool ArgParser (int argc, char *argv[]);
extern void FatalCompError (char *mod, char *proc, char *mess);

extern void Verbose (char *msg);
extern void PrintVersion (void);
extern void InitSettings (void);
extern void ExitOnInterrupt (void);
extern void InitCompiler (void);
extern void ExitCompiler (void);

#ifdef _DEBUG_
extern void ErrorInCompiler (char *mod, char *proc, char *msg);
extern void Assume (Bool cond, char *mod, char *proc);
extern void AssumeError (char *mod, char *proc);
#define ifnot(cond) if(!(cond))
#endif
