/*
       Concurrent Clean Compiler: sa.c
       ===============================

       This file contains the strictness analyser. It can handle tuples
       as well as list strictness. The file is divided in the following
       parts:
       
       General support, including a local storage allocator
       Support for storage of strictness information
       Operations on expressions
       Converions for the internal representation of the syntax tree
       The abstract reducer
       Main (external) functions
       Debugging support
       
       Author: Eric Nocker
       At: Department of Computer Science
       University of Nijmegen
       Version: 0.9
       Date:    Januari, 1995
*/

#undef _DB_
/*
#define CHECK_STACK_OVERFLOW
#define _DB_STACK_
*/

#define DIVIDE_FUEL
#define SHOW_STRICT_EXPORTED_TUPLE_ELEMENTS
#define MORE_ANNOTS 1

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"
#include "sizes.h"
#include "syntaxtr.t"
#include "comsupport.h"
#include "checker.h"
#include "sa.t"
#include "sa.h"
#ifdef _DB_TEST_
# include "saprint.h"
#endif
#include "typeconv.h"
#include "statesgen.h"
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
#include "codegen_types.h"
#endif

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)

#define NR_BLOCKS_FOR_ANALYSIS 100

#define BLOCK_SIZE (unsigned long) (16 * KBYTE)

#ifdef CHECK_STACK_OVERFLOW
char *min_stack;
int stack_source = 0;
#endif

#if MORE_ANNOTS
# define MaxNrAnnots 100
#else
# define MaxNrAnnots 10					/* the maximum nr of strict rhs annots	*/
#endif

typedef int MyBool;
#define MyFalse 0
#define MyTrue 1
#define MyMightBeTrue 2
#define AreRelated 3
#define False MyFalse
#define True MyTrue
#define MightBeTrue MyMightBeTrue

Bool DoStrictExportChecks = False;
Bool DoStrictRelated      = False;

#define Bool MyBool

static Bool StrictWarning		= False;	/* general warnings					*/
static Bool StrictAllWarning	= True;		/* warnings per function			*/
static Bool StrictChecks      	= False;	/* warns for strange strictness		*/
static Bool StrictExportChecks	= False;	/* do export checks					*/

static unsigned StrictFuel		= 60; /* 20; */      /* amount of reductions to do 		*/

static unsigned min_d;
static unsigned max_memuse;				/* the maximum memory use				*/

static char		*CurrentName;			/* current function name				*/

#ifdef _DB_
static Bool DBPrinting = False;
Exp infp, botmemp, topmemp;
#endif

#ifdef _DB_
Fun
#else
static Fun
#endif
	* conssym,					/* the cons id							*/
	* nilsym,					/* the nil id							*/
	* apsym,					/* the apply id							*/
	* if_sym,					/* the if id							*/
	* true_sym,					/* the true id							*/
	* false_sym,				/* the false id							*/
	* selectsym[MaxNodeArity],	/* the select ids						*/
	* tuplesym[MaxNodeArity],	/* the tuple ids						*/
	* strict_sym[MaxNrAnnots],	/* the strict ids						*/
	* fail_sym,					/* the fail id							*/
	* inffunct_sym,				/* the E2 id							*/
	* botmemfunct_sym,			/* the E3 id							*/
	* strictapsym;				/* the strict apply id					*/

#if STRICT_LISTS
# ifndef _DB_
static
# endif
Fun *lazy_cons_sym0,*strict_cons_sym0,*tail_strict_cons_sym0,*strict_tail_strict_cons_sym0;
#endif

static ExpRepr top;
static ExpRepr bottom;
static ExpRepr inf;
static ExpRepr botmem;
static ExpRepr topmem;

/*
General Support
Containing:

	- Debugging options
	  The following debugging options are available:
	      _DB_		general option, should always be set for the others
	      _DB_RED_	set on if reductions should be traced
	      _DB_EQ_	set on if comparison on expressions should be traced
	  output of the tracing is sent to the file "uit".

	- Warnings Generation

	- Storage allocator
	  In principle the storage allocator is quite simple: it supports a
	  fast allocation by allocating some large blocks if necessary. With
	  the functions 'Freeze..' and 'UnFreeze..' a basic part of the
	  storage (used for the function table and syntax tree) can be frozen.
	  Unfreezing releases all the other memory. The function 'MemUse' can
	  be used for obtaining the current memory usage.

*/

#ifdef _DB_
 File outfile;
# define Assume ProcAssume
# define Assume2 ProcAssume
	static void ProcAssume (Bool cond, char *err, char *proc)
	{
		Bool stop = False;
		
		if (! cond)
		{	if (! stop)
				FPrintF (StdError, "FATAL ERROR: %s in %s\n", err, proc);
			else
				DoFatalError ("%s in %s\n", err, proc);
		}
	}
#else
# ifdef _DB_TEST_
#  define Assume(A,B,C)
#  define Assume2 ProcAssume
	static void ProcAssume (Bool cond, char *err, char *proc)
	{
		Bool stop = False;
		
		if (! cond)
		{	if (! stop)
				FPrintF (StdError, "FATAL ERROR: %s in %s\n", err, proc);
			else
				DoFatalError ("%s in %s\n", err, proc);
		}
	}
# else
#  define Assume2(A,B,C)
#  define Assume(A,B,C)
# endif
#endif

/*
Warnings
	Warnings can be given during or after the analysis. If after, some
	warnings that would be given during the analysis should be collected
	into one warning. This is indicated by 'depth_warning' and 
	'mem_warning'.
*/

static void error_in_function (char *m)
{
	ErrorInCompiler ("sa.c",m,"");
}

static Bool depth_warning;			/* set True if a depth warning is given		*/
static Bool mem_warning;			/* set True if a memory warning is given	*/
static Bool time_warning;			/* set True if a time warning is given		*/
static Bool export_warning;			/* set True if an export warning is given	*/
static Bool max_depth_reached;		/* set True if max depth reached, no more
									   such warnings should be given			*/
static Bool max_time_reached;		/* set True if max time reached, no more
									   such warnings should be given			*/
static Bool initialising = True;	/* set True when building function table
									   this results in other warnings			*/
static Bool instantiating = False;	/* set True when copying an expression		*/

static void GiveStrictWarning (char *f, char *msg)
{
#if 1
	CurrentLine=0;
	if (f)
		StaticMessage (False,"%s","%s",f,msg);
	else
		StaticMessage (False,"","%s",msg);
#else
	if (f)
		FPrintF (StdError, "Warning [%s%s,%s]: %s\n", CurrentModule, CurrentExt, f, msg);
	else
		FPrintF (StdError, "Warning [%s%s]: %s\n", CurrentModule, CurrentExt, msg);
#endif
}

/*******************************************************************************
 * The Storage Allocator for the strictness analysis                           *
 ******************************************************************************/

static char		**SA_store;					/* the memory blocks				*/
static int		SA_store_size = 0;
static unsigned n_allocated_blocks = 0;		/* the nr of allocated blocks		*/
static unsigned usedblocks   = 0;			/* the nr of blocks in use			*/
static char		*high        = Null;		/* current end position in block	*/
static char		*free_pos    = Null;		/* current free position in block	*/
static unsigned fblocks      = 0;			/* the freezed nr of blocks			*/
static char		*ffree       = Null;		/* the freezed free position		*/

static void NewBlock (void)
{
	if (usedblocks < n_allocated_blocks)
		++usedblocks;
	else if (fblocks==0 || n_allocated_blocks<fblocks+NR_BLOCKS_FOR_ANALYSIS){
		if (n_allocated_blocks>=SA_store_size){
			if (SA_store_size==0){
				SA_store = malloc (100 * (sizeof (char*)));
				if (SA_store==NULL){
					free_pos=NULL;
					return;
				}
				SA_store_size = 100;
			}
			while (SA_store_size<=n_allocated_blocks){
				char **new_SA_store;
				
				new_SA_store = realloc (SA_store,(SA_store_size<<1) * sizeof(char*));
				if (new_SA_store==NULL){
					free_pos=NULL;
					return;
				}
				SA_store=new_SA_store;
				SA_store_size=SA_store_size<<1;
			}
		}

		if (! (free_pos = (char *) Alloc (BLOCK_SIZE, SizeOf (char))))
			return;
		
		SA_store[n_allocated_blocks] = free_pos;
	
		n_allocated_blocks++;
		usedblocks++;
	} else {
		free_pos = NULL;
		return;
	}

	free_pos = SA_store[usedblocks - 1];
	high     = free_pos + BLOCK_SIZE;
}

static jmp_buf SAEnv, SAEnv2, SAEnv3;

#define SAllocType(t) ((t*)SAlloc(sizeof(t)))
#define SAllocArrayType(n,t) ((t*)SAlloc((n)*sizeof(t)))

static char *SAlloc (unsigned n)
{
	/* be sure to return an even address */
	n = ReSize (n);
		
	if (free_pos!=NULL && free_pos + n < high){
		char *m;
		
		m=free_pos;
		free_pos = m+n;

		return m;
	} else
		NewBlock ();
	
	if (free_pos!=NULL && free_pos + n < high){
		free_pos += n;

		return (free_pos - n);
	} else {
		if (initialising)
			longjmp (SAEnv, 1);
		
		if (StrictAllWarning)
			GiveStrictWarning (CurrentName, "out of memory (result approximated)");
		else
			mem_warning = True;
		
		if (instantiating)
			longjmp (SAEnv3, 1);
		else
			longjmp (SAEnv2, 1);
		
		return NULL;
	}
}

static void FreezeAlloc (void)
{
	ffree   = free_pos;
	fblocks = usedblocks;
}

static void FreeUnFreezedBlocks (void)
{
	usedblocks = fblocks;
	free_pos   = ffree;
	high       = SA_store[fblocks-1] + BLOCK_SIZE;
}

void free_unused_sa_blocks (void)
{
	int i;
	
	for (i=usedblocks; i<n_allocated_blocks; ++i){
		if (SA_store[i]!=NULL){
			Free ((void *) SA_store[i]);
			SA_store[i]=NULL;
		}
	}
	
	n_allocated_blocks = usedblocks;
}

static unsigned MemUse (void)
{
	long l;
	
	if (n_allocated_blocks==0)
		return 0;
	
	if (! free_pos)
		l = (long) (usedblocks-1) * BLOCK_SIZE;
	else
		l = (long) (usedblocks-1) * BLOCK_SIZE + (long) ((size_t) free_pos - (size_t) SA_store[usedblocks-1]);

	return (unsigned) ((l-1) / KBYTE) + 1;
}

static void FreeBlocks (void)
{
	unsigned i;

	for (i = 0; i < n_allocated_blocks; i++){
		if (SA_store[i]!=NULL){
			Free ((void *) SA_store[i]);
			SA_store[i]=NULL;
		}
	}
	
	n_allocated_blocks = usedblocks = fblocks = 0;
	free_pos   = ffree   = Null;
}

#define NewExpArgs(n) SAllocArrayType(n,Exp)

static Exp NewExp (ExpKind kind, unsigned sym, Bool hnf, unsigned arity)
{
	Exp e;

	e = SAllocType (ExpRepr);
	
	e->e_kind   = kind;
	e->e_sym    = sym;
	e->e_hnf    = hnf;
	e->e_spechnf= hnf;
	e->e_hasind = False;
	e->e_red    = False;
	e->e_mark   = False;
	e->e_mark2  = False;
	e->e_imark  = False;
	e->e_fwd    = Null;
	e->e_deps   = Null;
	
	if (arity == 0)
		e->e_args = NULL;
	else
		e->e_args = NewExpArgs (arity);
	
#ifdef _DB_
	e->e_mmark  = False;
	e->e_dmark  = False;
	e->e_shared = False;
	e->e_add    = 0;
#endif

	return e;
}

static Exp NewValueExp (Fun *fun, Bool hnf, unsigned arity)
{
	Exp e;

	e = SAllocType (ExpRepr);
	
	e->e_kind   = Value;
	e->e_fun    = fun;
	e->e_hnf    = hnf;
	e->e_spechnf= hnf;
	e->e_hasind = False;
	e->e_red    = False;
	e->e_mark   = False;
	e->e_mark2  = False;
	e->e_imark  = False;
	e->e_fwd    = Null;
	e->e_deps   = Null;
	
	if (arity == 0)
		e->e_args = NULL;
	else
		e->e_args = NewExpArgs (arity);
	
#ifdef _DB_
	e->e_mmark  = False;
	e->e_dmark  = False;
	e->e_shared = False;
	e->e_add    = 0;
#endif

	return e;
}

#define NewTop()  (NewExp (Top, 0, True, 0))

static void InitExp (Exp e, ExpKind kind, unsigned sym, Bool hnf)
{
	e->e_kind   = kind;
	e->e_sym    = sym;
	e->e_hnf    = hnf;
	e->e_spechnf= hnf;
	e->e_hasind = False;
	e->e_red    = False;
	e->e_mark   = False;
	e->e_mark2  = False;
	e->e_imark  = False;
	e->e_fwd    = Null;
	e->e_deps   = Null;
	
#ifdef _DB_
	e->e_mmark  = False;
	e->e_dmark  = False;
	e->e_shared = False;
	e->e_add    = 0;
#endif
}

static void InitValueExp (Exp e,Fun *fun,Bool hnf)
{
	e->e_kind   = Value;
	e->e_fun    = fun;
	e->e_hnf    = hnf;
	e->e_spechnf= hnf;
	e->e_hasind = False;
	e->e_red    = False;
	e->e_mark   = False;
	e->e_mark2  = False;
	e->e_imark  = False;
	e->e_fwd    = Null;
	e->e_deps   = Null;
	
#ifdef _DB_
	e->e_mmark  = False;
	e->e_dmark  = False;
	e->e_shared = False;
	e->e_add    = 0;
#endif
}

static unsigned start_fuel;

static void SetStartFuel (void)
{
	start_fuel = StrictFuel;
}

static Bool OutOfFuel (void)
{
	if (start_fuel == 0)
		return True;

	--start_fuel;
	return False;
}

/* Operations on StrictInfos and contexts */

static StrictKind MaxStrict (StrictKind s1, StrictKind s2)
{
	if (s1 < s2)
		return s2;
	else
		return s1;
}

static Context SimpleContext (Context context, StrictKind kind, Bool spec)
{
	if (! context)
		context = SAllocType (ContextRepr);

	context->context_arity       = 1;
	context->context_speculative = spec;
	context->context_kind        = kind;
	context->context_args        = NULL;

	return context;
}

static Context NewSimpleContext (StrictKind kind, Bool spec)
{
	Context context;
	
	context = SAllocType (ContextRepr);

	context->context_arity       = 1;
	context->context_speculative = spec;
	context->context_kind        = kind;
	context->context_args        = NULL;

	return context;
}

static Context StrictInfoToContext (StrictInfo *s, Context curcontext, Bool resultinfo)
{
	Context context;

	if (! resultinfo && curcontext->context_kind == NotStrict)
		return curcontext;
	
	if (IsTupleInfo (s)){
		StrictKind info_kind = GetTupleStrictKind (s);
		
		if (info_kind == NotStrict){
			if (resultinfo)
				return curcontext;

			context = SAllocType (ContextRepr);
			context->context_arity       = 1;
			context->context_speculative = curcontext->context_speculative;
			context->context_kind        = NotStrict;
			context->context_args        = (Context *) Null;
		}
		else {
			unsigned i, n;
			Bool has_strict_arg = False;
			Context subcontext;
			
			n = s->strict_arity;
			
			context = SAllocType (ContextRepr);
			context->context_arity       = s->strict_arity;
			context->context_speculative = curcontext->context_speculative;
			context->context_kind        = HnfStrict;
			context->context_args        = SAllocArrayType (n,Context);
		
			for (i = 0; i < n; i++)
			{	if (! resultinfo)
					subcontext = curcontext;
				else if (curcontext->context_arity > 1)
					subcontext = curcontext->context_args[i];
				else
					subcontext = NewSimpleContext (NotStrict, curcontext->context_speculative);
					
				context->context_args[i] = StrictInfoToContext (& GetTupleInfo (s, i), subcontext, resultinfo);
				if (context->context_args[i]->context_kind != NotStrict)
					has_strict_arg = True;
			}
			
			if (! has_strict_arg)
				context->context_arity = 1;
		}
	}
	else {
		StrictKind info_kind = GetStrictKind (s, ContextToIndex (curcontext->context_kind));
	
		if (resultinfo){
			if (info_kind <= curcontext->context_kind)
				return curcontext;
		
			context = SAllocType (ContextRepr);
			context->context_arity       = 1;
			context->context_speculative = curcontext->context_speculative;
			context->context_kind        = info_kind;
			context->context_args        = NULL;
		} else {
			if (info_kind == curcontext->context_kind && curcontext->context_arity == 1)
				return curcontext;
				
			context = SAllocType (ContextRepr);
			context->context_arity       = 1;
			context->context_speculative = curcontext->context_speculative;
			context->context_kind        = info_kind;
			context->context_args        = NULL;
		}
	}
	
	return context;
}

static Context copy_context (Context curcontext)
{
	Context context;

	if (! curcontext || curcontext->context_kind == NotStrict)
		return NULL;
	
	context = SAllocType (ContextRepr);
	context->context_arity       = curcontext->context_arity;
	context->context_speculative = False;
	context->context_kind        = curcontext->context_kind;
	
	if (context->context_arity > 1){
		unsigned i, n;
		
		n = context->context_arity;
		context->context_args = SAllocArrayType (n,Context);
	
		for (i = 0; i < n; i++)
			context->context_args[i] = copy_context (curcontext->context_args[i]);
	} else
		context->context_args  = NULL;
	
	return context;
}

/* Operations on expressions */

static void InitValues (void)
{
	static ExpRepr	botmem1;
	static ExpRepr	botmem2;
	static Exp		infargs[2];
	static Exp		botmem1args[2];
	static Exp		botmem2args[2];
	static Exp		botmemargs[2];
	static ExpRepr	topmem1;
	static ExpRepr	topmem2;
	static Exp		topmem1args[2];
	static Exp		topmemargs[2];

#ifdef _DB_
	infp    = & inf;
	botmemp = & botmem;
	topmemp = & topmem;
#endif

	InitValueExp (&inf, conssym, True);
	inf.e_args    = infargs;
	inf.e_args[0] = & top;
	inf.e_args[1] = & inf;

	InitValueExp (&topmem1,  nilsym,  True);
	InitValueExp (&topmem2,  conssym, True);

	InitExp (&topmem, Lub, 2, True);

	topmem.e_kind = Top;			
	topmem.e_args    = topmemargs;
	topmem.e_args[0] = & topmem1;
	topmem.e_args[1] = & topmem2;

	topmem2.e_args    = topmem1args;
	topmem2.e_args[0] = & top;
	topmem2.e_args[1] = & topmem;
	
	InitValueExp (&botmem1,  conssym, True);
	InitValueExp (&botmem2,  conssym, True);

	InitExp (&botmem, Lub, 2, True);

	botmem.e_args    = botmemargs;
	botmem.e_args[0] = & botmem1;
	botmem.e_args[1] = & botmem2;
	
	botmem1.e_args    = botmem1args;
	botmem1.e_args[0] = & top;
	botmem1.e_args[1] = & botmem;
	
	botmem2.e_args    = botmem2args;
	botmem2.e_args[0] = & bottom;
	botmem2.e_args[1] = & topmem;
}

static void RemoveMark (Exp e)
{
	unsigned n,i;
	
	if (! e->e_mark)
		return;
	
	e->e_mark = False;
	switch (e->e_kind){
		case Top:
		case Bottom:
		case FunValue:
			return;
		case Ind:
			RemoveMark (e->e_args[0]);
			return;
		case Argument:
			return;
		case Value:
			n = e->e_fun->fun_arity;
			break;
		case Dep:
		case Lub:
			n = e->e_sym;
			break;
		default:
			Assume (False, "unknown case", "RemoveMark");
			return;
	}
	
	for (i = 0; i < n; i++)
		RemoveMark (e->e_args[i]);
}

static Exp InstantiateExp2 (Exp e)
{
	unsigned arity, i;
	Exp new_e;

	if (e->e_mark)
		return e->e_fwd;

	e->e_mark = True;
	switch (e->e_kind){
		case Top:
			new_e    = NewTop();
			e->e_fwd = new_e;
			break;
		case Dep:
		{
			unsigned j;
			Exp arg_e;

			arity    = e->e_sym;
			new_e    = NewExp (Dep, e->e_sym, e->e_hnf, arity);
			e->e_fwd = new_e;
			for (i = 0, j = 0; i < arity; i++){
				arg_e = InstantiateExp2 (e->e_args[i]);
				if (arg_e->e_kind == Bottom){
					new_e->e_kind = Bottom;
					new_e->e_hnf = True;
					new_e = &bottom;
					e->e_fwd = new_e;
					return new_e;
				} else if (arg_e->e_kind == Top) /* || arg_e->e_hnf) */
					/* simply skip it */
					;
				else {
					new_e->e_args[j] = arg_e;
					j++;
				}
			}
			if (j == 0){
				new_e    = NewTop();
				e->e_fwd = new_e;
			} else
				new_e->e_sym = j;
			break;
		}
		case Bottom:
			e->e_fwd = & bottom;
			new_e    = & bottom;
			break;
		case FunValue:
			e->e_mark = False;
			e->e_fwd  = e;
			new_e     = e;
			break;
		case Ind:
			new_e            = NewExp (Ind, 0, False, 1);
			e->e_fwd         = new_e;
			new_e->e_args[0] = e->e_args[0];
			break;
		case Argument:
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			Assume2 (e->e_args[0] != Null, "argument not bound", "InstantiateExp");
#endif
			e->e_fwd     = new_e = e->e_args[0];
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			e->e_args[0] = NULL;
#endif
			break;
		case Value:
			arity = e->e_fun->fun_arity;
			new_e = NewValueExp (e->e_fun, e->e_hnf, arity);
			e->e_fwd = new_e;
			for (i = 0; i < arity; i++)
				new_e->e_args[i] = InstantiateExp2 (e->e_args[i]);
			break;
		case Lub:
			arity    = e->e_sym;
			new_e    = NewExp (Lub, e->e_sym, True, arity);
			e->e_fwd = new_e;
			for (i = 0; i < arity; i++)
				new_e->e_args[i] = InstantiateExp2 (e->e_args[i]);
			break;
		default:
			Assume (False, "unknown case", "InstantiateExp");
			return &top;
	}

	return new_e;
}

static Exp InstantiateExp (Exp e)
{
	Exp new_e;

	instantiating = True;
	
	if (setjmp (SAEnv3) != 0){
		RemoveMark (e);
		instantiating = False;
		longjmp (SAEnv2, 1);
	}

	new_e = InstantiateExp2 (e);
	RemoveMark (e);
	instantiating = False;

	return new_e;
}

/*
	 During (Ext)LtExp2 a the addresses in Val/Dep expressions are determined for
     which an AreRelated relation exists
     
     the Bool CheckAreRelated should be (un)set before(after) LtExp is called
*/
 
static Bool CheckAreRelated = False;
static Exp *s_exp1, *s_exp2, *q_exp;

/* JVG */
#define MAX_LT_EXP2_CALLS 100000
static long lt_exp2_max_n_calls;
/* */

static Bool LtExp2 (Exp e1, Exp e2)
{
	unsigned n, i;

#ifdef CHECK_STACK_OVERFLOW
	char x;

	if (&x < min_stack)
	{	printf ("Stack overflow in LtExp\n");
#ifdef _DB_
		FPrintF (outfile, "Stack overflow in LtExp\n");
#endif
		return False;
	}
#endif
	if (e1 == e2)
		return True;

	if (e1->e_mark || e2->e_mark)
		return MightBeTrue;

	if (e2->e_kind == Top)
		return True;

	/* JVG */
	if (++lt_exp2_max_n_calls >= MAX_LT_EXP2_CALLS)
		return MightBeTrue;
	/* */

	switch (e1->e_kind){
		case Bottom:
			return True;
		case Top:
			return False;
		case FunValue:
			if (e2->e_kind == FunValue && e1->e_fun==e2->e_fun)
				return True;
			else
				break;
		case Ind:
			e1->e_mark = True;
			if (LtExp2 (e1->e_args[0], e2)){
				e1->e_mark = False;
				return True;
			}
			e1->e_mark = False;
			break;
		case Value:
		case Dep:
		{
			int s_index;
			
			s_index = -1;
					
			if (e1->e_kind==Value){
				if (e1->e_kind!=e2->e_kind || e1->e_fun!=e2->e_fun)
					break;
				
				n = e1->e_fun->fun_arity;
			} else {
				if (e1->e_kind!=e2->e_kind || e1->e_sym!=e2->e_sym)
					break;

				n = e1->e_sym;
			}

			e1->e_mark = True;
			e2->e_mark = True;
			
			for (i = 0; i < n; i++){
				Bool b;
				
				b = LtExp2 (e1->e_args[i], e2->e_args[i]);
				switch (b){
					case True:
						continue;
					case MightBeTrue:
						e1->e_mark = False;
						e2->e_mark = False;
						return MightBeTrue;
					case False:
					case AreRelated:
						if (CheckAreRelated && s_index < 0){
							s_index = i;
							continue;
						} else {
							e1->e_mark = False;
							e2->e_mark = False;
							return False;
						}
				}
			}
			e1->e_mark = False;
			e2->e_mark = False;
			if (s_index >= 0){
				s_exp1 = & e1->e_args[s_index];
				s_exp2 = & e2->e_args[s_index];
				return AreRelated;
			} else
				return True;
		}
		case Lub:
			e1->e_mark = True;
			n = e1->e_sym;
			for (i = 0; i < n; i++){
				Bool b;
				
				b = LtExp2 (e1->e_args[i], e2);
				if (b != True){
					e1->e_mark = False;
					return b;
				}
			}
			e1->e_mark = False;
			return True;
		default:
			Assume (False, "illegal case", "LtExp");
			return False;
	}
	
	/* check if e2 is a lub or Ind */
	if (e2->e_kind == Lub){
		Bool result;
		
		result = False;
		e2->e_mark = True;
		n = e2->e_sym;

		for (i = 0; i < n; i++){
			Bool b;
			
			b = LtExp2 (e1, e2->e_args[i]);
			if (b == True){
				e2->e_mark = False;
				return b;
			} else if (b == MightBeTrue)
				result = MightBeTrue;
		}
		e2->e_mark = False;
		return result;
	} else if (e2->e_kind == Ind){
		e2->e_mark = True;

		if (LtExp2 (e1, e2->e_args[0])){
			e2->e_mark = False;
			return True;
		}
		e2->e_mark = False;
	}

	return False;
}

#ifdef _DB_
#undef Bool
Bool IsInAPath (Exp e1, Exp e2, APath p)
#define Bool MyBool
#else
static Bool IsInAPath (Exp e1, Exp e2, APath p)
#endif
{
	for ( ; p; p = p->ap_next){
		if (e1 == p->ap_e1 && e2 == p->ap_e2)
			return True;
	}
	return False;
}

#ifdef _DB_
APath AddToAPath (Exp e1, Exp e2, APath p)
#else
static APath AddToAPath (Exp e1, Exp e2, APath p)
#endif
{
	APath new;

	new = SAllocType (APathRepr);

	new->ap_e1   = e1;
	new->ap_e2   = e2;
	new->ap_next = p;
	return new;
}

static Bool EqExp2 (Exp e1, Exp e2)
{
	unsigned n, i;

	if (e1 == e2)
		return True;

	if (e1->e_mark)
		return MightBeTrue;
	
	switch (e1->e_kind)
	{
	case Bottom:
		if (e2->e_kind == Bottom)
			return True;
		else
			return False;
	case Top:
		if (e2->e_kind == Top)
			return True;
		else
			return False;
	case FunValue:
		if (e2->e_kind == FunValue && e1->e_fun==e2->e_fun)
			return True;
		else
			return False;
	case Argument:
		return False;
	case Ind:
		return (e2->e_kind == Ind && e1->e_args[0] == e2->e_args[0]);
	case Value:
	case Dep:
		if (e1->e_kind!=e2->e_kind)
			return False;
		
		if (e1->e_kind == Value){
			if (e1->e_fun != e2->e_fun)
				return False;

			n = e1->e_fun->fun_arity;
		} else {
			if (e1->e_sym != e2->e_sym)
				return False;
		
			n = e1->e_sym;
		}
		
		e1->e_mark = True;
		
		for (i = 0; i < n; i++)
		{	Bool b = EqExp2 (e1->e_args[i], e2->e_args[i]);
			if (b != True)
			{	e1->e_mark = False;
				return b;
			}
		}
		e1->e_mark = False;
		return True;
	case Lub:
		if (e2->e_kind != Lub || e1->e_sym != e2->e_sym)
			return False;
		e1->e_mark = True;
		n = e1->e_sym;
		for (i = 0; i < n; i++)
		{	Bool b = EqExp2 (e1->e_args[i], e2->e_args[i]);
/* JVG added: */
			if (b!=True)
/**/
			{	e1->e_mark = False;
				return b;
			}
		}
		e1->e_mark = False;
		return True;
	default:
		Assume (False, "illegal case", "EqExp");
		return False;
	}
} /* EqExp2 */

static Bool ExtEqExp2 (Exp e1, Exp e2, APath p)
{
	unsigned n, i;
	APath newp;
	
	if (e1 == e2)
		return True;
	
	if (IsInAPath (e1, e2, p))
		return True;
		
	if (e1->e_mark && e2->e_mark)
		return False;
	
	newp = AddToAPath (e1, e2, p);
		
	switch (e1->e_kind){
	case Bottom:
		if (e2->e_kind == Bottom)
			return True;
		else
			return False;
	case Top:
		if (e2->e_kind == Top)
			return True;
		else
			return False;
	case FunValue:
		if (e2->e_kind == FunValue && e1->e_fun==e2->e_fun)
			return True;
		else
			return False;
	case Argument:
		return False;
	case Ind:
		return (e2->e_kind == Ind && e1->e_args[0] == e2->e_args[0]);
	case Value:
	case Dep:
		if (e1->e_kind != e2->e_kind)
			return False;
		
		if (e1->e_kind == Value){
			if (e1->e_fun != e2->e_fun)
				return False;
				
			n = e1->e_fun->fun_arity;
		} else {
			if (e1->e_sym != e2->e_sym)
				return False;
			
			n = e1->e_sym;
		}

		e1->e_mark = True;
		e2->e_mark = True;

		for (i = 0; i < n; i++)
		{	if (! ExtEqExp2 (e1->e_args[i], e2->e_args[i], newp))
			{	e1->e_mark = False;
				e2->e_mark = False;
				return False;
			}
		}
		e1->e_mark = False;
		e2->e_mark = False;
		return True;
	case Lub:
		if (e2->e_kind != Lub || e1->e_sym != e2->e_sym)
			return False;
		e1->e_mark = True;
		e2->e_mark = True;
		n = e1->e_sym;
		for (i = 0; i < n; i++)
		{	if (! ExtEqExp2 (e1->e_args[i], e2->e_args[i], newp))
			{	e1->e_mark = False;
				e2->e_mark = False;
				return False;
			}
		}
		e1->e_mark = False;
		e2->e_mark = False;
		return True;
	default:
		Assume (False, "unknown case", "ExtEqExp2");
		return False;
	}
} /* ExtEqExp2 */

#ifdef _DB_
#undef Bool
static Bool EqExp (Exp e1, Exp e2)
#define Bool MyBool
#else
static Bool EqExp (Exp e1, Exp e2)
#endif /* _DB_ */
{
	Bool b;
	
	b = EqExp2 (e1, e2);
	
	if (b == MightBeTrue && StrictDoExtEq){
		b = ExtEqExp2 (e1, e2, (APath) Null);
		Assume (! ContainsMark (e1), "e1 is marked", "EqExp (Ext)");
		Assume (! ContainsMark (e2), "e2 is marked", "EqExp (Ext)");
	}
	else
	{	Assume (! ContainsMark (e1), "e1 is marked", "EqExp");
	}

	if (b == True)
		return True;
	else
		return False;
}

static Bool ExtLtExp2 (Exp e1, Exp e2, APath p)
{
	if (e1 == e2)
		return True;
	if (e1->e_kind == Bottom || e2->e_kind == Top)
		return True;
	if (e1->e_kind == Top || e2->e_kind == Bottom)
		return False;

	if (IsInAPath (e1, e2, p))
		return True;
	
	switch (e1->e_kind){
		case FunValue:
			if (e2->e_kind == FunValue && e1->e_fun == e2->e_fun)
				return True;
			else
				break;
		case Ind:
		{
			APath newp;
			
			newp = AddToAPath (e1, e2, p);
			if (ExtLtExp2 (e1->e_args[0], e2, newp))
				return True;
			else
				break;
		}
		case Value:
		case Dep:
		{
			unsigned n, i;
			int s_index;
			APath newp;

			if (e1->e_kind != e2->e_kind)
				break;
			
			if (e1->e_kind==Value){
				if (e1->e_fun != e2->e_fun)
					break;
				n=e1->e_fun->fun_arity;
			} else {
				if (e1->e_sym != e2->e_sym)
					break;
				n=e1->e_sym;
			}

			s_index = -1;
						
			newp = AddToAPath (e1, e2, p);
			for (i = 0; i < n; i++){
				Bool b = ExtLtExp2 (e1->e_args[i], e2->e_args[i], newp);
				switch (b){
					case True:
						continue;
					case False:
					case AreRelated:
						if (CheckAreRelated && s_index < 0){
							s_index = i;
							continue;
						}
						return False;
				}
			}
			if (s_index >= 0){
				s_exp1 = & e1->e_args[s_index];
				s_exp2 = & e2->e_args[s_index];
				return AreRelated;
			} else
				return True;
			
			return True;
		}
		case Lub:
		{
			unsigned n, i;
			APath newp;
				
			n    = e1->e_sym;
			newp = AddToAPath (e1, e2, p);
		
			for (i = 0; i < n; i++){
				Bool b = ExtLtExp2 (e1->e_args[i], e2, newp);
				if (b != True)
					return False;
			}
			return True;
		}
		default:
			Assume (False, "illegal case", "LtExp");
			return False;
	}

	/* check if e2 is a lub */
	if (e2->e_kind == Lub){
		unsigned n, i;
		APath newp;

		n    = e2->e_sym;
		newp = AddToAPath (e1, e2, p);

		for (i = 0; i < n; i++){
			if (ExtLtExp2 (e1, e2->e_args[i], newp) == True)
				return True;
		}
	} else if (e2->e_kind == Ind){
		if (ExtLtExp2 (e1, e2->e_args[0], p))
			return True;
	}
	
	return False;
}

static Bool LtExp (Exp e1, Exp e2)
{
	Bool b;

#ifdef _DB_EQ_
	if (DBPrinting)
	{	FPrintF (outfile, "Less then e1: ");
		DumpExp (outfile, e1);
		FPrintF (outfile, "\n        e2: ");
		DumpExp (outfile, e2);
		FPutC ('\n', outfile);
	}
#endif

	/* JVG */
	lt_exp2_max_n_calls=0;
	/* */
	b = LtExp2 (e1, e2);

#ifdef _DB_EQ_
	if (DBPrinting){
		if (b == True)
			FPrintF (outfile, "Result: True\n\n");
		else if (b == MightBeTrue)
			FPrintF (outfile, "Result: MightBeTrue\n\n");
		else
			FPrintF (outfile, "Result: False\n\n");
	}
#endif

	if (b == MightBeTrue && StrictDoExtEq){
		b = ExtLtExp2 (e1, e2, (APath) Null);

#ifdef _DB_EQ_
		if (DBPrinting){
			if (b == True)
				FPrintF (outfile, "Result2: True\n\n");
			else if (b == MightBeTrue)
				FPrintF (outfile, "Result2: MightBeTrue\n\n");
			else
				FPrintF (outfile, "Result2: False\n\n");
		}
#endif
	}

	return b;
}

static Bool IsContainedIn (Exp e1, ExpP ep2)
{
	Exp e2;
	
	e2 = *ep2;

	if (e2->e_mark2)
		return False;
		
	if (EqExp (e1, e2)){
		q_exp = ep2;
		return True;
	}
	
	switch (e2->e_kind){
		case Value:
		{	unsigned n, i;
			
			e2->e_mark2 = True;
			n = e2->e_fun->fun_arity;
			for (i = 0; i < n; i++){
				if (IsContainedIn (e1, & e2->e_args[i])){
					e2->e_mark2 = False;
					return True;
				}
			}
			e2->e_mark2 = False;
			return False;
		}
		case Lub:
		{	unsigned n, i;
			e2->e_mark2 = True;
			n    = e2->e_sym;
			for (i = 0; i < n; i++){
				if (! IsContainedIn (e1, & e2->e_args[i])){
					e2->e_mark2 = False;
					return False;
				}
			}
			e2->e_mark2 = False;
			return True;
		}
		default:
			return False;
	}
}

static int SortLtExp (Exp e1,Exp e2)
{
	ExpKind kind1, kind2;
	
	kind1 = e1->e_kind;
	kind2 = e2->e_kind;
	if (kind1 == kind2){
		if (kind1 == Value){
			if (e1->e_hnf)
				return -1;
			else if (e2->e_hnf)
				return -1;
			else
				return (e1->e_fun < e2->e_fun);
		} else
			return False; 
	} else
		return (kind1 < kind2);
}

#define LESS(a,b)	(SortLtExp ((a),(b)))

static void  Sort (Exp *defs, unsigned high)
{
	unsigned low,father, son;
	Exp val;

	low = high / 2;
	while (high > 1){
		val = defs[father = low];
		for (;;){
			son = 2 * father + 1;
			if (son >= high)
			{	defs[father] = val;
				break;
			};
			if (son == high - 1){
				if (LESS (val, defs[son])){
					defs[father] = defs[son];
					defs[son] = val;
				} else {
					defs[father] = val;
				};
				break;
			};
			if (LESS (defs[son], defs[son + 1]))
				son++;
			if (!LESS (val, defs[son])){
				defs[father] = val;
				break;
			};
			defs[father] = defs[son];
			father = son;
		};
		if (low > 0){
			low--;
		} else {
			val = defs[0];
			defs[0] = defs[--high];
			defs[high] = val;
		}
	}
}

static Bool ContainsExpOfKind (Exp e, ExpKind kind)
{
	unsigned i;
	Bool result = False;
	
	for (i = 0; i < e->e_sym; i++)
	{	if (e->e_args[i]->e_kind == kind)
			result = True;
		else if (kind == Dep && e->e_args[i]->e_kind == Bottom)
		{	e->e_kind = Bottom;
			e->e_hnf  = True;
			e->e_deps = Null;
			return False;
		}
		else if (kind == Lub && e->e_args[i]->e_kind == Top)
		{	e->e_kind = Top;
			e->e_hnf  = True;
			return False;
		}
	}
	
	return result;
}

static Bool IsInArgs (Exp *args, unsigned n, Exp e)
{
	unsigned i;
	
	for (i = 0; i < n; i++)
		if (args[i] == e)
			return True;
	
	return False;
}

static void RemoveExpOfKind (Exp e, ExpKind kind)
{	unsigned	i, j, k, n, new_n;
	Exp			*new_args;
	
	/* count the new number of 'kind' args (the current args + the new ones) */
	n     = e->e_sym;
	new_n = 0;
	for (i = 0; i < n; i++)
	{	if (e->e_args[i]->e_kind == kind)
			new_n += e->e_args[i]->e_sym;
		else
			new_n += 1;
	}
	
	new_args = NewExpArgs (new_n);
	
	for (i = 0, j = 0; i < n; i++){
		if (e->e_args[i]->e_kind == kind){
			int kind_n = e->e_args[i]->e_sym;
			for (k = 0; k < kind_n; k++){
				if (! IsInArgs (new_args, j, e->e_args[i]->e_args[k])){
					new_args[j] = e->e_args[i]->e_args[k];
					j++;
				}
			}
		} else
			if (! IsInArgs (new_args, j, e->e_args[i])){
				new_args[j] = e->e_args[i];
				j++;
			}
	}
	
	/* put new arguments in original expression */
	e->e_args = new_args;
	e->e_sym  = j;

	/* remove remaining subkind expressions */
	if (ContainsExpOfKind (e, kind))
		RemoveExpOfKind (e, kind);
}

#define IsTupleExp(A) ((A)->e_kind==Value && ((A)->e_fun>=tuplesym[0] && (A)->e_fun<=tuplesym[MaxNodeArity-1]))

/* JVG: added 16-8-2000 */
static void remove_deps_from_tuple_arguments (Exp e)
{
	if (e->e_deps==NULL)
		return;
	
	if (IsTupleExp(e)){
		int n,arity;

		arity=e->e_fun->fun_arity;
		for (n=0; n<arity; ++n){
			remove_deps_from_tuple_arguments (e->e_args[n]);
			e->e_args[n]->e_deps=NULL;
		}
	}
}
/**/

static void UpdateExp (Exp src, Exp dst);

static void RemoveCycles (ExpP ep, ExpKind kind)
{	unsigned i, n;
	Exp e = *ep;
	
	if (e->e_mark)
	{	*ep       = & bottom;
		e->e_mark = False;
		return;
	}
	
	e->e_mark = True;
	
	n = e->e_sym;
	for (i = 0; i < n; i++)
	{	if (e->e_args[i]->e_kind == kind)
			RemoveCycles (& e->e_args[i], kind);
	}
	
	e->e_mark = False;
} /* RemoveCycles */

static void SortExpOfKind (Exp e, ExpKind kind)
{	unsigned n, j, i;
	Bool remove;
	Exp e2 = e;		/* temp pointer: the pointer can be changed by RemoveCycles */
	
	Assume2 (e->e_kind == kind, "No exp of right kind", "SortExpOfKind");

	RemoveCycles (& e2, kind);

	if (ContainsExpOfKind (e, kind))
		RemoveExpOfKind (e, kind);
	
	if (e->e_kind != kind)
		return;

	n = e->e_sym;

	Sort (e->e_args, e->e_sym);
	
	if (kind == Dep)
	{	for (i = n; i > 0; i--)
			if (e->e_args[i-1]->e_kind != Top)
				break;
		
		n = i;
	}
	
	for (i = 0; i+1 < n; ){
		if (LtExp (e->e_args[i], e->e_args[i+1]) == True){
			remove = True;
#if 1
			/* JVG: added 16-8-2000 */
			if (kind==Lub)
				remove_deps_from_tuple_arguments (e->e_args[i]);
#endif
			e->e_args[i] = e->e_args[i+1];
		} else if (LtExp (e->e_args[i+1], e->e_args[i]) == True){
#if 1
			/* JVG: added 16-8-2000 */
			if (kind==Lub)
				remove_deps_from_tuple_arguments (e->e_args[i+1]);
#endif
			remove = True;
		} else
			remove = False;
		
		if (remove){
			for (j = i+1; j+1 < n; j++)
				e->e_args[j] = e->e_args[j+1];
			n--;
		} else
			i++;
	}
	e->e_sym = n;

	if (n > 20)
	{
#ifdef _DB_
		FPrintF (StdOut, "SortLub %d:", n);
		DumpExp (StdOut, e);
		FPutC ('\n', StdOut);
#endif /* _DB_ */
		e->e_kind = Top;
		return;
	}
	
	if (n == 1 && kind == Lub)
		UpdateExp (e->e_args[0], e);
	else if (n == 0 && kind == Dep)
		e->e_kind = Top;
}

static void CopyDeps (Dependency fromdep,Dependency *newdeps)
{
	Dependency new;

	for (;fromdep; fromdep = fromdep->dep_next){
		new = SAllocType (DependencyRepr);
		new->dep_exp     = fromdep->dep_exp;
		new->dep_next    = *newdeps;
		*newdeps         = new;
	}
}

static Dependency AddDeps (Dependency fromdep, Dependency taildeps)
{	Dependency new;

	for (;fromdep; fromdep = fromdep->dep_next)
	{
		new = SAllocType (DependencyRepr);
		new->dep_exp     = fromdep->dep_exp;
		new->dep_next    = taildeps;
		taildeps         = new;
	}
	
	return taildeps;
} /* AddDeps */

static Dependency CombineDependencies (Dependency deps1, Dependency deps2)
{
	Dependency new;

	new = NULL;

	if (! deps1 || ! deps2)
		return NULL;

	CopyDeps (deps1,&new);
	CopyDeps (deps2,&new);
/*
	for (; deps1; deps1 = deps1->dep_next)
	{	Dependency dep;
		Exp e;
		
		e = deps1->dep_exp;
		
		for (dep = deps2; dep; dep = dep->dep_next)
		{	if (e == dep->dep_exp)
			{	Dependency new2;
			
				new2 = SAllocType (DependencyRepr);
				new2->dep_exp     = e;
				new2->dep_next    = new;
				new = new2;
			}
		}
	}
*/
	return new;
}

static Exp TakeLub (Exp e1, Exp e2)
{
	Exp new_e;
	unsigned n, i, j;
	Dependency newdeps;
	
	if (! e1 && ! e2)
		return & bottom;
	if (! e1 || e1->e_kind == Bottom)
		return e2;
	if (! e2 || e2->e_kind == Bottom)
		return e1;
	
	newdeps = CombineDependencies (e1->e_deps, e2->e_deps);
	
	/* create a new Lub expression and copy all the elements */
	if (e1->e_kind == Lub && e2->e_kind == Lub)
	{	new_e = NewExp (Lub, 0, True, e1->e_sym + e2->e_sym);
		j = 0;
		for (i = 0; i < e1->e_sym; i++)
		{	if (e1->e_args[i]->e_kind == Bottom)
				continue;
			else if (e1->e_args[i]->e_kind == Top)
				return NewTop();
			else
			{	new_e->e_args[j] = e1->e_args[i];
				j++;
			}
		}
		for (i = 0; i < e2->e_sym; i++)
		{	if (e2->e_args[i]->e_kind == Bottom)
				continue;
			else if (e2->e_args[i]->e_kind == Top)
				return NewTop();
			else
			{	new_e->e_args[j] = e2->e_args[i];
				j++;
			}
		}
		new_e->e_sym = j;
	}
	else if (e1->e_kind == Lub)
	{	n = e1->e_sym;
		new_e = NewExp (Lub, 0, True, n + 1);
		j = 0;
		for (i = 0; i < n; i++)
		{	if (e1->e_args[i]->e_kind == Bottom)
				continue;
			else if (e1->e_args[i]->e_kind == Top)
				return NewTop();
			else
			{	new_e->e_args[j] = e1->e_args[i];
				j++;
			}
		}
		new_e->e_args[j] = e2;
		new_e->e_sym     = j + 1;
	}
	else if (e2->e_kind == Lub)
	{	n = e2->e_sym;
		new_e = NewExp (Lub, 0, True, 1 + n);
		j = 0;
		for (i = 0; i < n; i++)
		{	if (e2->e_args[i]->e_kind == Bottom)
				continue;
			else if (e2->e_args[i]->e_kind == Top)
				return NewTop();
			else
			{	new_e->e_args[j] = e2->e_args[i];
				j++;
			}
		}
		new_e->e_args[j] = e1;
		new_e->e_sym     = j + 1;
	}
	else
	{	new_e = NewExp (Lub, 2, True, 2);
		new_e->e_args[0] = e1;
		new_e->e_args[1] = e2;
	}

	SortExpOfKind (new_e, Lub);
	new_e->e_deps = newdeps;
	return new_e;
}

static void UpdateExp (Exp src, Exp dst)
{	unsigned arity, i;

	if (src == dst)
		return;
	
	dst->e_kind    = src->e_kind;
	dst->e_hnf     = src->e_hnf;
	dst->e_spechnf = src->e_spechnf;
	dst->e_red     = False;
	
	switch (src->e_kind)
	{
	case Top:
		dst->e_sym = src->e_sym;
		arity = 0;
		break;
	case FunValue:
		dst->e_fun = src->e_fun;
		arity = 0;
		break;
	case Bottom:
		dst->e_sym = src->e_sym;
		dst->e_args = Null;
		dst->e_deps = Null;
		return;
	case Ind:
#ifdef _DB_
		FPrintF (outfile, "Update with indirection %u %u\n", src->e_add,dst->e_add);
#endif
		dst->e_sym = src->e_sym;
		arity = 1;
		break;
	case Value:
		dst->e_fun = src->e_fun;
		arity = src->e_fun->fun_arity;
		break;
	case Lub:
	case Dep:
		dst->e_sym = src->e_sym;
		arity = src->e_sym;
		break;
	default:
		Assume (False, "unknown case", "UpdateExp");
		dst->e_sym = src->e_sym;
		arity = 0;
		break;
	}

	dst->e_args = NewExpArgs (arity);
	for (i = 0; i < arity; i++)
		dst->e_args[i] = src->e_args[i];

	/* add dependencies of source to destination */
	dst->e_deps = AddDeps (dst->e_deps, src->e_deps);
	
	if (dst->e_kind == Lub)
		SortExpOfKind (dst, Lub);
}

/*******************************************************************************
 * The function table, initialisation                                          *
 ******************************************************************************/

static Bool has_fail;						/* the current alternative contains a Fail			*/

#define IsTupleExp(A)				((A)->e_kind==Value && ((A)->e_fun>=tuplesym[0] && (A)->e_fun<=tuplesym[MaxNodeArity-1]))
#define TypeArgsOfRecord(R)			((R)->sdef_type->type_constructors->cl_constructor->type_node_arguments)

static Bool HasStrictAnnot (Annotation annot)
{
	if (! StrictDoAnnots)
		return False;

	return annot==StrictAnnot;
}

static Bool HasProcessAnnot (Annotation annot)
{
	return False;

	/* parallel annotations are only used in parallel compilation */
	if (! DoParallel || ! annot)
		return False;
	
	switch (annot){
		case ContinueAnnot:
		case ParallelAnnot:
		case ParallelAtAnnot:
		case LazyParallelAnnot:
		case InterleavedAnnot:
		case LazyInterleavedAnnot:
		case DeferAnnot:
		case WaitAnnot:
	    case ContInterleavedAnnot:
		case ParallelNFAnnot:
		case InterleavedNFAnnot:
			return True;
		default:
			return False;
	}
}

static Exp ConvertNode (Node node, NodeId node_id);

static void ConvertToApplyNode (Exp e, Node node, unsigned arity)
{
	if (arity==0){
		e->e_fun  = node->node_symbol->symb_def->sdef_sa_fun;
		e->e_kind = FunValue;
		e->e_hnf  = True;
	} else {
		Exp			left, right;
		Args		args;
		unsigned	i;

		args = node->node_arguments;
		
		left = NewValueExp (NULL,False,0);
		
		ConvertToApplyNode (left, node, arity-1);
		
		for (i = 1; i < arity; i++, args = args->arg_next)
			;

		right = ConvertNode	(args->arg_node, NULL);

		e->e_fun     = apsym;
		e->e_kind    = Value;
		e->e_hnf     = True;
		e->e_args    = NewExpArgs (2);
		e->e_args[0] = left;
		e->e_args[1] = right;
	}
}

static Exp ConvertNodeId (NodeId nid)
{
	Exp e;
	
	if (nid->nid_exp)
		return nid->nid_exp;
	
	if (nid->nid_refcount>=0){
		if (nid->nid_node_def)
			return ConvertNode (nid->nid_node_def->def_node, nid);
		else {
			DoFatalError ("ConvertNode (SA): no node or nid");
			return & top;
		}
	} else {
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		if (nid->nid_node)
			return ConvertNode (nid->nid_node, nid);
#endif
		e = NewExp (Argument, 0, False, 1);
		e->e_args[0] = NULL;

		nid->nid_exp_ = e;

		return e;
	}
}

static Exp ConvertNodeDefs (Node root, NodeDefs defs, StrictNodeIdP strictids)
{
	Exp e, rootexp;
	int i, nr_strict;
	NodeDefs node_def;
	StrictNodeIdP ids;
	
	/* convert node defs */
	for_l (node_def,defs,def_next)
		if (node_def->def_node!=NULL)
			ConvertNode (node_def->def_node,node_def->def_id);
	
	/* convert root node */
	rootexp = ConvertNode (root,NULL);
	
	/* convert strict node defs */
	nr_strict = 0;
	for_l (ids,strictids,snid_next)
		nr_strict++;
	
	if (nr_strict==0)
		return rootexp;

	e = NewValueExp (strict_sym [nr_strict - 1], False, nr_strict + 1);
			
	for (i=0,ids=strictids; i< nr_strict; i++,ids=ids->snid_next)
		e->e_args[i] = ConvertNode (ids->snid_node_id->nid_node,ids->snid_node_id);

	e->e_args[i] = rootexp;
	
	return e;
}

static unsigned CountStrictArgs (TypeArgs args)
{
	TypeNode node;
	unsigned n = 0;
	
	if (! args)
		return 0;
		
	for (; args; args = args->type_arg_next){
		node = args->type_arg_node;
		
		if (node->type_node_annotation!=StrictAnnot)
			continue;
			
		n += 1;
		
		if (!node->type_node_is_var && node->type_node_symbol->symb_kind==tuple_type)
			n += CountStrictArgs (node->type_node_arguments);
	}
	
	return n;
}

static void ConvertStrictSelections (Exp exp, TypeNode node, Exp *e_args, unsigned *i)
{
	if (!node->type_node_is_var && node->type_node_symbol->symb_kind==tuple_type){
		TypeArgs typeargs;
		unsigned j;
		Exp selexp;
		
		e_args[*i] = exp;
		(*i) ++;		
		
		for (j = 0, typeargs = node->type_node_arguments; typeargs; typeargs = typeargs->type_arg_next, j++){
			node = typeargs->type_arg_node;
			
			if (node->type_node_annotation!=StrictAnnot)
				continue;
			
			selexp            = NewValueExp (selectsym[j], False, 1);
			selexp->e_args[0] = exp;
			
			ConvertStrictSelections (selexp, node, e_args, i);
		}
	} else {
		if (exp->e_kind == Top || exp->e_hnf)
			return;
		
		e_args[*i] = exp;
		(*i) ++;		
	}
}

static void InitNode (Node node);

static void InitNodeDefs (NodeDefs defs)
{
	for ( ; defs; defs=defs->def_next){
		if (defs->def_id)
			defs->def_id->nid_exp_ = NULL;
	
		InitNode (defs->def_node);
	}
}

static void InitNode (Node node)
{
	if (! node)
		return;
	
	if (node->node_kind==NodeIdNode)
		node->node_node_id->nid_exp_ = NULL;
	else {
		Args args;
		
		if (node->node_kind==IfNode){
			InitNodeDefs (node->node_then_node_defs);
			InitNodeDefs (node->node_else_node_defs);
		}
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		else if (node->node_kind==SwitchNode){
			for_l (args,node->node_arguments,arg_next){
				NodeP node_p;

				node_p=args->arg_node;
				if (node_p->node_kind==CaseNode){
					NodeP case_alt_node_p;
					
					case_alt_node_p=node_p->node_arguments->arg_node;
					if (case_alt_node_p->node_kind==PushNode){
						NodeIdListElementP node_id_list;
						
						for_l (node_id_list,case_alt_node_p->node_node_ids,nidl_next)
							node_id_list->nidl_node_id->nid_exp=NULL;
						
						case_alt_node_p=case_alt_node_p->node_arguments->arg_next->arg_node;
					}
					
					InitNode (case_alt_node_p);
					InitNodeDefs (node_p->node_node_defs);
				} else if (node_p->node_kind==OverloadedCaseNode){
					node_p=node_p->node_node;
					InitNode (node_p->node_arguments->arg_node);
					InitNodeDefs (node_p->node_node_defs);

				} else if (node_p->node_kind==DefaultNode){
					InitNode (node_p->node_arguments->arg_node);
					InitNodeDefs (node_p->node_node_defs);
				} else
					error_in_function ("InitNode");
			}
			
			return;
		} else if (node->node_kind==GuardNode){
			InitNode (node->node_arguments->arg_node);
			InitNode (node->node_arguments->arg_next->arg_node);
			InitNodeDefs (node->node_node_defs);
			return;
		}
#endif

		for_l (args,node->node_arguments,arg_next)
			InitNode (args->arg_node);
	}
}

static void InitAlternative (RuleAltS *alt)
{
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	NodeDefs nds;
#endif

	InitNode (alt->alt_lhs_root);

#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	for_l (nds,alt->alt_lhs_defs,def_next){
		if (nds->def_id)
			nds->def_id->nid_exp_ = NULL;
	
		InitNode (nds->def_node);
	}
#endif

	if (alt->alt_kind==Contractum){
		InitNode (alt->alt_rhs_root);
		InitNodeDefs (alt->alt_rhs_defs);
	}
}

/* 'StrictUpdates' defines whether a record update is strict */

#define StrictUpdates

static Exp ConvertNode (Node node, NodeId nid)
{
	Exp e;
	unsigned arity, i;
	Args arg;
	
	if (nid==NULL){
		if (node->node_kind==NodeIdNode)
			return ConvertNodeId (node->node_node_id);
	} else {
		if (nid->nid_exp)
			return nid->nid_exp;
		
		if (node->node_kind==NodeIdNode){
			if (node->node_node_id==nid)
				return ConvertNodeId (nid);
			else
				return ConvertNodeId (nid->nid_node->node_node_id);
		}
	}
	
	if (HasProcessAnnot (node->node_annotation))
		return & top;
	
	e = NewValueExp (NULL,False,0);

	if (nid)
		nid->nid_exp_ = e;
	
	switch (node->node_kind){
		case NormalNode:
		{	arity = node->node_arity;

			switch (node->node_symbol->symb_kind){
				case tuple_symb:
					e->e_fun = tuplesym[arity];
					break;
				case bool_denot:
					if (node->node_symbol -> symb_bool)
						e->e_fun = true_sym;
					else
						e->e_fun = false_sym;
					e->e_hnf = True;
					break;
				case cons_symb:
#if STRICT_LISTS
					if (node->node_symbol->symb_head_strictness>1){
						e->e_fun = (node->node_symbol->symb_tail_strictness ? strict_tail_strict_cons_sym0 : strict_cons_sym0)+arity;
						break;
					} else if (node->node_symbol->symb_tail_strictness){
						e->e_fun = tail_strict_cons_sym0+arity;
						break;
					}
					e->e_hnf = True;
					e->e_fun = lazy_cons_sym0+arity;
#else
					e->e_hnf = True;
					e->e_fun = conssym;
#endif
					break;
				case nil_symb:
					e->e_hnf = True;
					e->e_fun = nilsym;
					break;
				case apply_symb:
					e->e_fun = apsym;

					/* for member calls: use strictapsym for strict arguments */
					if (arity==2){
						int n_apply_args;
						struct arg *arg_p;

						n_apply_args=1;
						arg_p = node->node_arguments;
						while (arg_p!=NULL && arg_p->arg_node->node_arity==2 && arg_p->arg_node->node_kind==NormalNode &&
							   arg_p->arg_node->node_symbol->symb_kind==apply_symb)
						{
							++n_apply_args;
							arg_p=arg_p->arg_node->node_arguments;
						}

						if (arg_p!=NULL && arg_p->arg_node->node_kind==SelectorNode && arg_p->arg_node->node_arity==1 &&
							(arg_p->arg_node->node_symbol->symb_def->sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE)!=0)
						{
							struct type_alt *member_type_alt;
							struct type_arg *type_arg_p;
							
							member_type_alt=arg_p->arg_node->node_symbol->symb_def->sdef_member_type_of_field;
							if (member_type_alt->type_alt_lhs->type_node_arity==n_apply_args+1){
								int arg_n;
								unsigned int arg_strictness;
								NodeP first_arg_of_apply_node_p;
								Exp e2;
								
								arg_strictness=0;
								arg_n=0;
								for_l (type_arg_p,member_type_alt->type_alt_lhs->type_node_arguments->type_arg_next,type_arg_next){
									if (type_arg_p->type_arg_node->type_node_annotation==StrictAnnot)
										arg_strictness |= 1<<arg_n;
									++arg_n;
								}
								
								--arg_n;
								if (arg_strictness & (1<<arg_n))
									e->e_fun = strictapsym;

								e->e_args = NewExpArgs (2);
								arg_p = node->node_arguments;
								e->e_args[1] = ConvertNode (arg_p->arg_next->arg_node, NULL);
								e2=e;
								first_arg_of_apply_node_p=arg_p->arg_node;
								while (first_arg_of_apply_node_p->node_arity==2){ /* node_arity of SelectorNode == 1 */
									Exp e3;

									--arg_n;
									e3 = NewValueExp (arg_strictness & (1<<arg_n) ? strictapsym : apsym,False,0);
									e2->e_args[0]=e3;
									e2=e3;
									e2->e_args = NewExpArgs (2);
									arg_p=first_arg_of_apply_node_p->node_arguments;
									e2->e_args[1] = ConvertNode (arg_p->arg_next->arg_node, NULL);
									
									first_arg_of_apply_node_p=arg_p->arg_node;
								}
								e2->e_args[0] = ConvertNode (first_arg_of_apply_node_p, NULL);
								
								return e;								
							}
						}
					}

					break;
				case select_symb:
					e->e_fun = selectsym[arity - 1];
					arity    = 1;
					break;
				case fail_symb:
					has_fail = True;
					e->e_fun = fail_sym;
					return e;
				case if_symb:
					e->e_fun  = if_sym;
					if (arity != 3)
					{	e->e_kind = FunValue;
						e->e_hnf  = True;;
					}
					break;
				case definition:
				{
					SymbDef sdef;
					
					sdef = node->node_symbol->symb_def;

					if (sdef->sdef_kind==DEFRULE || sdef->sdef_kind==SYSRULE){
						TypeAlts rule;
						TypeArgs typeargs;
						unsigned int i;
						Exp exp;

#if SA_RECOGNIZES_ABORT_AND_UNDEF
						if (sdef->sdef_module==StdMiscId->ident_name){
							if ((sdef->sdef_ident==abort_id && node->node_arity==1) || sdef->sdef_ident==undef_id){
								e->e_kind = Bottom;
								e->e_sym = 0;
								e->e_hnf = True;
								e->e_spechnf= True;

								return e;
							}
						}
#endif

						rule = sdef->sdef_rule_type->rule_type_rule;
						typeargs = rule->type_alt_lhs->type_node_arguments;

						/* count the number of strict args in the type */
						arity    = CountStrictArgs (typeargs);
						
						if (arity == 0){
							e->e_kind = Top;
							return e;
						}
						
						e->e_kind = Dep;
						e->e_args = NewExpArgs (arity);
						
						i = 0;
						for (arg = node->node_arguments; arg; arg = arg->arg_next, typeargs = typeargs->type_arg_next){
							if (typeargs->type_arg_node->type_node_annotation!=StrictAnnot)
								continue;
							
							exp = ConvertNode (arg->arg_node, NULL);
								
							ConvertStrictSelections (exp, typeargs->type_arg_node, e->e_args, &i);
						}
						if (i == 0)
							e->e_kind = Top;
						else
							e->e_sym = i;
						return e;
					} else {
						e->e_fun = sdef->sdef_sa_fun;
										
						if (arity==sdef->sdef_arity)
							e->e_kind = Value;
						else {
							ConvertToApplyNode (e, node, arity);
							return e;
						}
					}
					break;
				}
				case seq_symb:
					if (node->node_arity==2){
						e->e_kind = Dep;
						e->e_args = NewExpArgs (2);
						e->e_sym = 2;

						e->e_args[0] = ConvertNode (node->node_arguments->arg_node,NULL);
						e->e_args[1] = ConvertNode (node->node_arguments->arg_next->arg_node,NULL);

						if (nid)
							nid->nid_exp_ = e;

						return e;
					}
				default:
					e = & top;
					if (nid)
						nid->nid_exp_ = e;
					return e;
			}
			
			e->e_args = NewExpArgs (arity);
			
			for (i = 0,arg=node->node_arguments; arg!=NULL; arg=arg->arg_next,++i)
				e->e_args[i] = ConvertNode (arg->arg_node, NULL);
			
			break;
		}
		case IfNode:
		{	arity     = 3;
			e->e_fun  = if_sym;
			e->e_args = NewExpArgs (arity);
			
			/* conditional part */
			arg          = node->node_arguments;
			e->e_args[0] = ConvertNode (arg->arg_node, Null);

			/* then and else part */
			arg = arg->arg_next;
			e->e_args[1] = ConvertNodeDefs (arg->arg_node, node->node_then_node_defs,node->node_then_strict_node_ids);

			arg = arg->arg_next;
			e->e_args[2] = ConvertNodeDefs (arg->arg_node, node->node_else_node_defs,node->node_else_strict_node_ids);
			break;
		}
		case SelectorNode:
		{
			int field_nr;

			field_nr = node->node_symbol->symb_def->sdef_sel_field_number;
			arg = node->node_arguments;

			if (node->node_arity>=SELECTOR_U){
				if (node->node_arity>=SELECTOR_L){
					Exp tuple,record,result,tuple_result,selection;
					
					tuple=ConvertNode (arg->arg_node,NULL);
					
					record=NewValueExp (selectsym[0],False,1);
					record->e_args[0]=tuple;
					
					result=NewValueExp (selectsym[1],False,1);
					result->e_args[0]=tuple;

					selection=NewValueExp (selectsym [field_nr],False,1);
					selection->e_args[0]=record;
					
					tuple_result=NewValueExp (tuplesym[2],True,2);
					tuple_result->e_args[0]=selection;
					tuple_result->e_args[1]=result;
					
					e->e_fun     = strict_sym[1];
					e->e_args    = NewExpArgs (3);
					e->e_args[0] = record;
					e->e_args[1] = result;
					e->e_args[2] = tuple_result;
				} else {
					Exp record,tuple_result,selection;

					record=ConvertNode (arg->arg_node,NULL);

					selection=NewValueExp (selectsym [field_nr],False,1);
					selection->e_args[0]=record;

					tuple_result=NewValueExp (tuplesym[2],True,2);
					tuple_result->e_args[0]=selection;
					tuple_result->e_args[1]=record;

					e->e_fun     = strict_sym[0];
					e->e_args    = NewExpArgs (2);
					e->e_args[0] = record;
					e->e_args[1] = tuple_result;
				}
				break;
			}

			e->e_fun  = selectsym [field_nr];
			e->e_args = NewExpArgs (1);
					
			e->e_args[0] = ConvertNode (arg->arg_node, Null);
			break;
		}
		case UpdateNode:
		{	int field_nr, arity;
			Exp oldrecordexp, selexp, newrecordexp;
			
			/* make a new exp node if a strict update is required */
#ifndef StrictUpdates
			newrecordexp = e;
#else
			newrecordexp = NewValueExp (NULL,False,0);
#endif
		
			/* convert the old record */
			arg          = node->node_arguments;
			oldrecordexp = ConvertNode (arg->arg_node, Null);

			/* build a record expression for the new record node */
			newrecordexp->e_fun  = node->node_symbol->symb_def->sdef_sa_fun;
			newrecordexp->e_kind = Value;
			arity                = node->node_symbol->symb_def->sdef_arity;
			
			/* initialise the arguments of the new record exp */
			newrecordexp->e_args = NewExpArgs (arity);
			for (i = 0; i < arity; i++)
				newrecordexp->e_args[i] = NULL;

			/* now fill in the updates of the new record */
			for_l (arg,node->node_arguments->arg_next,arg_next){
				field_nr = arg->arg_node->node_symbol->symb_def->sdef_sel_field_number;
				newrecordexp->e_args[field_nr] = ConvertNode (arg->arg_node->node_arguments->arg_node, Null);
			}
			
			/* finally, create selections for the parts which are not updated */
			for (i = 0; i < arity; i++)
			{	if (newrecordexp->e_args[i])
					continue;
				
				selexp                  = NewValueExp (selectsym [i], False, 1);
				selexp->e_args[0]       = oldrecordexp;
				newrecordexp->e_args[i] = selexp;
			}

			/* fill the strictness cell if necessary */
#ifdef	StrictUpdates
			e->e_args    = NewExpArgs (2);
			e->e_fun     = strict_sym[0];
			e->e_args[0] = oldrecordexp;
			e->e_args[1] = newrecordexp;
#endif
			break;
		}
		case MatchNode:
		{
			Symbol symbol;

			symbol=node->node_symbol;
			if	(symbol->symb_kind==definition && symbol->symb_def->sdef_kind==CONSTRUCTOR &&
				 symbol->symb_def->sdef_arity==1)
			{
				Exp selexp;
				
				selexp = NewValueExp (selectsym[0], False, 1);
				if (nid)
					nid->nid_exp_ = selexp;

				selexp->e_args[0] = ConvertNode (node->node_arguments->arg_node,NULL);

				return selexp;
			} else {
				if (nid)
					nid->nid_exp_ = NULL;
				
				node=node->node_arguments->arg_node;
				if (node->node_kind==NodeIdNode)
					return ConvertNodeId (node->node_node_id);
				else
					return ConvertNode (node,nid);
			}
		}
		default:
			DoFatalError ("ConvertNode (SA): unknown node kind");
			return & top;
	}
	return e;
}

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS 
static void convert_pattern_to_apply_node (Exp e,SymbolP symbol,NodeIdListElementP node_id_list,unsigned arity)
{
	if (arity==0){
		e->e_fun  = symbol->symb_def->sdef_sa_fun;
		e->e_kind = FunValue;
		e->e_hnf  = True;
	} else {
		Exp left,right;
		unsigned i;
		NodeIdListElementP node_id_list_elem;
		
		left = NewValueExp (NULL,False,0);
		
		convert_pattern_to_apply_node (left,symbol,node_id_list,arity-1);
		
		node_id_list_elem=node_id_list;
		i=1;
		while  (i<arity){
			node_id_list_elem=node_id_list_elem->nidl_next;
			++i;
		}

		right = ConvertNodeId (node_id_list_elem->nidl_node_id);

		e->e_fun     = apsym;
		e->e_kind    = Value;
		e->e_hnf     = True;
		e->e_args    = NewExpArgs (2);
		e->e_args[0] = left;
		e->e_args[1] = right;
	}
}

static Exp convert_pattern (SymbolP symbol_p,int arity,NodeIdListElementP node_id_list)
{
	NodeIdListElementP node_id_list_elem;
	Exp e;
	
	e = NewValueExp (NULL,False,0);
	
	switch (symbol_p->symb_kind){
		case tuple_symb:
			e->e_fun = tuplesym[arity];
			break;
		case bool_denot:
			if (symbol_p -> symb_bool)
				e->e_fun = true_sym;
			else
				e->e_fun = false_sym;
			e->e_hnf = True;
			break;
		case cons_symb:
#if STRICT_LISTS
			if (symbol_p->symb_head_strictness>1){
				e->e_fun = (symbol_p->symb_tail_strictness ? strict_tail_strict_cons_sym0 : strict_cons_sym0)+arity;
				break;
			} else if (symbol_p->symb_tail_strictness){
				e->e_fun = tail_strict_cons_sym0+arity;
				break;
			}
			e->e_hnf = True;
			e->e_fun = lazy_cons_sym0+arity;
#else
			e->e_hnf = True;
			e->e_fun = conssym;
#endif
			break;
		case nil_symb:
			e->e_hnf = True;
			e->e_fun = nilsym;
			break;
		case definition:
		{
			SymbDef sdef;
			
			sdef = symbol_p->symb_def;

			if (sdef->sdef_kind==DEFRULE || sdef->sdef_kind==SYSRULE){
				TypeAlts rule;
				TypeArgs typeargs;
				unsigned int i;
				Exp exp;
				
				rule = sdef->sdef_rule_type->rule_type_rule;
				typeargs = rule->type_alt_lhs->type_node_arguments;

				/* count the number of strict args in the type */
				arity = CountStrictArgs (typeargs);
				
				if (arity == 0){
					e->e_kind = Top;
					return e;
				}
				
				e->e_kind = Dep;
				e->e_args = NewExpArgs (arity);
				
				i = 0;
				for (node_id_list_elem=node_id_list; node_id_list_elem!=NULL; node_id_list_elem=node_id_list_elem->nidl_next,typeargs=typeargs->type_arg_next){
					if (typeargs->type_arg_node->type_node_annotation==StrictAnnot){
						exp = ConvertNodeId (node_id_list_elem->nidl_node_id);
							
						ConvertStrictSelections (exp,typeargs->type_arg_node,e->e_args,&i);
					}
				}
				
				if (i == 0)
					e->e_kind = Top;
				else
					e->e_sym = i;

				return e;
			} else {
				e->e_fun = sdef->sdef_sa_fun;
								
				if (arity==sdef->sdef_arity)
					e->e_kind = Value;
				else {
					convert_pattern_to_apply_node (e,symbol_p,node_id_list,arity);
					return e;
				}
			}
			break;
		}
		default:
			e = & top;
			return e;
	}
	
	e->e_args = NewExpArgs (arity);

	{
		unsigned int i;

		for (i=0,node_id_list_elem=node_id_list; node_id_list_elem!=NULL; node_id_list_elem=node_id_list_elem->nidl_next,++i)
			e->e_args[i] = ConvertNodeId (node_id_list_elem->nidl_node_id);
	}
	
	return e;
}

static void convert_root_node (NodeP rhs_root_p,NodeDefs node_defs,StrictNodeIdP strict_node_ids,Alts fun_alt_p);

static void convert_switch_node (NodeP switch_node_p,Alts fun_alt_p)
{
	ArgP arg_p;
	Alts *last_next_switch_alt_p;
	
	fun_alt_p->fun_rhs = ConvertNodeId (switch_node_p->node_node_id);

	fun_alt_p->fun_is_guard=0;
	last_next_switch_alt_p=&fun_alt_p->fun_switch_alts;

	for_l (arg_p,switch_node_p->node_arguments,arg_next){
		Alts case_alt_p;
		Bool old_has_fail;
		NodeP case_alt_node_p,node_p;
		
		case_alt_p=SAllocType (AltsRepr);

		*last_next_switch_alt_p=case_alt_p;
		last_next_switch_alt_p=&case_alt_p->fun_next;
		
		node_p=arg_p->arg_node;
		if (node_p->node_kind==CaseNode){
			case_alt_node_p=node_p->node_arguments->arg_node;
			if (case_alt_node_p->node_kind==PushNode){
				case_alt_p->fun_lhs=convert_pattern (node_p->node_symbol,node_p->node_arity,case_alt_node_p->node_node_ids);
				case_alt_node_p=case_alt_node_p->node_arguments->arg_next->arg_node;
			} else {
				case_alt_p->fun_lhs=convert_pattern (node_p->node_symbol,0,NULL);
			}
		} else if (node_p->node_kind==DefaultNode){
			case_alt_node_p=node_p->node_arguments->arg_node;
			case_alt_p->fun_lhs=NULL;
		} else if (node_p->node_kind==OverloadedCaseNode){
			node_p=node_p->node_node;
			case_alt_node_p=node_p->node_arguments->arg_node;
			case_alt_p->fun_lhs=&top;

		} else
			error_in_function ("convert_switch_node");

		old_has_fail=has_fail;
		has_fail=False;

		convert_root_node (case_alt_node_p,node_p->node_node_defs,node_p->node_strict_node_ids,case_alt_p);

		case_alt_p->fun_has_fail=has_fail;
		if (old_has_fail)
			has_fail=True;
	}

	*last_next_switch_alt_p=NULL;
}

static void convert_guard_node (NodeP guard_node_p,NodeDefs node_defs,StrictNodeIdP strict_node_ids,Alts fun_alt_p)
{
	Alts fail_alt_p;
	
	fail_alt_p=SAllocType (AltsRepr);

	fun_alt_p->fun_is_guard=1;
	fun_alt_p->fun_switch_alts=fail_alt_p;
	
	fun_alt_p->fun_rhs=ConvertNodeDefs (guard_node_p->node_arguments->arg_node,node_defs,strict_node_ids);
	
	convert_root_node (guard_node_p->node_arguments->arg_next->arg_node,guard_node_p->node_node_defs,guard_node_p->node_guard_strict_node_ids,fail_alt_p);
}

static void convert_root_node (NodeP rhs_root_p,NodeDefs node_defs,StrictNodeIdP strict_node_ids,Alts fun_alt_p)
{
	if (rhs_root_p->node_kind==SwitchNode){
		NodeDefP node_def;
		
		for_l (node_def,node_defs,def_next)
			if (node_def->def_node!=NULL)
				ConvertNode (node_def->def_node,node_def->def_id);

		if (strict_node_ids!=NULL)
			error_in_function ("convert_root_node");

		convert_switch_node (rhs_root_p,fun_alt_p);
	} else if (rhs_root_p->node_kind==GuardNode){
		convert_guard_node (rhs_root_p,node_defs,strict_node_ids,fun_alt_p);
	} else {
		fun_alt_p->fun_rhs = ConvertNodeDefs (rhs_root_p,node_defs,strict_node_ids);
		fun_alt_p->fun_switch_alts=NULL;
	}
}
#endif

static void ConvertAlternatives (Alts *funalts,RuleAlts rulealts)
{
	Alts fun_alt_p;

	if (! rulealts){
		*funalts = NULL;
		return;
	}

	fun_alt_p=SAllocType (AltsRepr);
	*funalts = fun_alt_p;
	
	InitAlternative (rulealts);
	
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	fun_alt_p->fun_lhs = ConvertNodeDefs (rulealts->alt_lhs_root,rulealts->alt_lhs_defs,NULL);
#else
	fun_alt_p->fun_lhs = ConvertNodeDefs (rulealts->alt_lhs_root,NULL,NULL);
#endif
	
	has_fail = False;
	
	if (rulealts->alt_kind==Contractum){
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		convert_root_node (rulealts->alt_rhs_root,rulealts->alt_rhs_defs,rulealts->alt_strict_node_ids,fun_alt_p);
#else
		fun_alt_p->fun_rhs = ConvertNodeDefs (rulealts->alt_rhs_root, rulealts->alt_rhs_defs, rulealts->alt_strict_node_ids);
#endif
	} else {
		/* code block */
		fun_alt_p->fun_rhs = &top;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		fun_alt_p->fun_switch_alts=NULL;
#endif
	}
	
	fun_alt_p->fun_has_fail = has_fail;
	
	/* convert the following alternatives */
	ConvertAlternatives (&fun_alt_p->fun_next, rulealts->alt_next);
}

static StrictInfo *InitNewStrictInfos (unsigned arity, StrictKind s)
{
	unsigned	i;
	StrictInfo	*strict_infos;
	
	strict_infos = SAllocArrayType (arity,StrictInfo);

	for (i = 0; i < arity; i++){
		strict_infos[i].strict_arity = 1;
		InitStrictInfo (&strict_infos[i],s);
	}
	return strict_infos;
}

static void InitStrictResult (StrictInfo *s)
{
	s->strict_arity = 1;
	InitStrictInfo (s, HnfStrict);
}

static void ConvertStateToStrictInfo (TypeNode node, StrictInfo *s, Bool adopt_annots)
{
	if (node->type_node_is_var || node->type_node_symbol->symb_kind!=tuple_type){
/*
	#ifdef _DB_
			if (node->type_node_is_var)
				printf ("ConvertStateToStrictInfo Var\n");
			else {
				if (node->type_node_symbol->symb_kind==definition)
					printf ("ConvertStateToStrictInfo Definition %s\n",node->type_node_symbol->symb_def->sdef_ident->ident_name);
				else
					printf ("ConvertStateToStrictInfo NoTuple %d\n",node->type_node_symbol->symb_kind);
			}
	#endif
*/
		s->strict_arity = 1;
		if (adopt_annots && node->type_node_annotation==StrictAnnot)
			InitStrictInfo (s, HnfStrict);
		else
			InitStrictInfo (s, NotStrict);
	} else {
		unsigned	arity = node->type_node_arity;
		unsigned	i;
		TypeArgs	args  = node->type_node_arguments;
		
		s->strict_arity = arity;
/*
	#ifdef _DB_
			printf ("ConvertStateToStrictInfo Tuple %d\n",arity);
	#endif
*/
		if (adopt_annots && node->type_node_annotation==StrictAnnot)
			GetTupleStrictKind (s) = HnfStrict;
		else
			GetTupleStrictKind (s) = NotStrict;
		
		GetTupleInfos (s) = SAllocArrayType (arity,StrictInfo);
		
		for (i = 0; i < arity; i++, args = args->type_arg_next)
			ConvertStateToStrictInfo (args->type_arg_node, & GetTupleInfo (s, i),
									  adopt_annots);
	}
}

static void ConvertTypeArgsToStrictInfos (TypeArgs args, unsigned arity, StrictInfo **strict_args, Bool adopt_annots)
{
	unsigned i;

	*strict_args = SAllocArrayType (arity,StrictInfo);

	for (i = 0; i < arity; i++, args = args->type_arg_next){
/*
	#ifdef _DB_
			printf ("ConvertTypeArgsToStrictInfos %d\n",i);
	#endif
*/
		ConvertStateToStrictInfo (args->type_arg_node, & (*strict_args)[i], adopt_annots);
	}
}

static void ConvertStateInfoToStrictInfos (TypeAlts rule_type_alts, unsigned arity, StrictInfo **strict_args,
										   StrictInfo *result, Bool adopt_annots)
{
	TypeArgs	args;
	TypeNode	node;
	
	if (! rule_type_alts){
		*strict_args = InitNewStrictInfos (arity, NotStrict);
		InitStrictResult (result);
		return;
	}
	
	/* do the arguments */
	args = rule_type_alts->type_alt_lhs->type_node_arguments;
	ConvertTypeArgsToStrictInfos (args, arity, strict_args, adopt_annots);
	
	/* do the result */
	node = rule_type_alts->type_alt_rhs;

	if (node->type_node_is_var)
		InitStrictResult (result);
	else
		ConvertStateToStrictInfo (node, result, True);
	
	/* the result is of course always strict */
	if (IsTupleInfo (result))
		GetTupleStrictKind (result) = HnfStrict;
	else
		InitStrictInfo (result, HnfStrict);
}

#if CLEAN2

/*
	Encoding for strictness information:

	The strictness information that is found by the strictness
	analyser is encoded in a bit string. There are two encodings

	compact (but fragile):
		0 a (s)*     trailing zeros are removed

	robust (but long):
		1 a (w s t)*

		a	any strictness added
		()* repeated for each argument position, recursively
			for strict (after sa) tuples
		w	argument was strict
		s	argument strictness added
		t	argument is tuple

	Example:
				f ::  ! a   (   a,   [a]) -> a // before sa
				f ::  ! a ! ( ! a,   [a]) -> a // after sa

	compact		0 1   0   1   1    0	=> 01011 (trailing zeros removed)

	robust		1 1  100 011 010  000   => 11100011010000

	The bit string is represented by a bit count and an array of
	ints (each 32 bits), where the least significant bit of an int
	is the first bit in the bit string.
*/

#define StrictPositionsRobustEncoding 1

#define kMaxStrictPositions 1024

#if StrictPositionsRobustEncoding
# define	kBitsPerStrictPosition 3
#else
# define	kBitsPerStrictPosition 1
# endif

#define kMaxStrictBits (2+kMaxStrictPositions*kBitsPerStrictPosition)
#define kBitsPerInt (sizeof (int)*8)
#define ceilingdiv(a, b) (((a)+(b)-1)/(b)) /* ceiling (a/b) */
#define bits2ints(n) ceilingdiv(n, kBitsPerInt)

static int strict_positions_last_one;
static StrictPositionsP strict_positions;


static void StrictPositionsClear (void)
{
	int	i, sizeInts;

	if (strict_positions == NULL)
	{
		int	sizeBytes;

		sizeInts = bits2ints(kMaxStrictBits);
		sizeBytes = sizeof (StrictPositionsS) + (sizeInts-1) * sizeof (int);
		strict_positions = CompAlloc (sizeBytes);
		for (i = 0; i < sizeInts; i++)
			strict_positions->sp_bits[i]	= 0;
		strict_positions->sp_size = 0;
	}

	sizeInts = bits2ints (strict_positions->sp_size);
	for (i = 0; i < sizeInts; i++)
		strict_positions->sp_bits[i]	= 0;

	strict_positions->sp_size = 0;
	strict_positions_last_one = 0;
}

static void StrictPositionsAddBit (Bool bit)
{
	int size;
	StrictPositionsP positions;

	positions = strict_positions;
	size = positions->sp_size;

	if (bit)
	{
		Assume (size < kMaxStrictPositions, "too many strict positions", "AddStrictPositions");

		positions->sp_bits [size/kBitsPerInt] |= 1 << (size % kBitsPerInt);
		strict_positions_last_one = size+1;
	}

	positions->sp_size = size+1;
}


static StrictPositionsP StrictPositionsCopy (void)
{
	StrictPositionsP positions;
	int sizeBits;

#if StrictPositionsRobustEncoding
	sizeBits = strict_positions->sp_size;
#else
	sizeBits = strict_positions_last_one;
#endif

	Assume (sizeBits < kMaxStrictPositions, "too many strict positions", "StrictPositionsToInts");

	if (sizeBits == 0)
	{
		static StrictPositionsS no_strict_postions = {0, {0}};

		positions = &no_strict_postions;
	}
	else
	{
		int	sizeInts, sizeBytes;

		sizeInts = bits2ints(sizeBits);
		sizeBytes = sizeof (StrictPositionsS) + (sizeInts-1) * sizeof (int);
		positions = CompAlloc (sizeBytes);
		memcpy (positions, strict_positions, sizeBytes);
	}

	return positions;
}

#define StrictPositionsStrictAdded(is_strict) StrictPositionsAddBit (is_strict)

#if StrictPositionsRobustEncoding
# define StrictPositionsWasStrict(is_strict_annotated) StrictPositionsAddBit (is_strict_annotated)
# define StrictPositionsType(is_tuple) StrictPositionsAddBit (is_tuple)
#else
# define StrictPositionsWasStrict(is_strict_annotated)
# define StrictPositionsType(is_tuple)
#endif

#endif /* CLEAN2 */

static void UpdateStateInfoWithStrictInfo (TypeNode node, StrictInfo *s,Bool *strict_added_p,Bool *warning)
{
	Bool is_strict_annotated, is_strict, is_tuple, strict_added;

	is_strict_annotated = node->type_node_annotation==StrictAnnot;
	is_tuple = IsTupleInfo (s);
	is_strict = (is_tuple ? GetTupleStrictKind (s) : GetStrictKind (s, 0)) != NotStrict;
	strict_added = !is_strict_annotated && is_strict;

#if CLEAN2
	StrictPositionsWasStrict (is_strict_annotated);
	StrictPositionsStrictAdded (strict_added);
	StrictPositionsType (is_tuple);
#endif

	if (strict_added) {
		node->type_node_annotation=StrictAnnot;
		*strict_added_p = True;
	}

	if (is_strict_annotated && !is_strict && StrictChecks)
		*warning = True;

	if (is_tuple && (is_strict || is_strict_annotated)){
		unsigned	arity = s->strict_arity;
		unsigned	i;
		TypeArgs	args  = node->type_node_arguments;

		for (i = 0; i < arity; i++, args = args->type_arg_next) {
#ifndef SHOW_STRICT_EXPORTED_TUPLE_ELEMENTS
			Bool local_strict_added;

			local_strict_added = False;
			strict_added_p = &local_strict_added;
#endif
			UpdateStateInfoWithStrictInfo (args->type_arg_node,&GetTupleInfo (s,i),strict_added_p,warning);
		}
	}
}

static void UpdateStateInfosWithStrictInfos (TypeAlts rule, unsigned arity, StrictInfo *strict_args,
											 StrictInfo *result, Bool *strict_added, Bool *warning)
{	unsigned	i;
	TypeArgs	args;

	if (! rule)
		return;

	/* do the arguments */
	args = rule->type_alt_lhs->type_node_arguments;

#if CLEAN2
	StrictPositionsClear ();
	StrictPositionsAddBit (StrictPositionsRobustEncoding);
	StrictPositionsAddBit (False);
#endif

	for (i = 0; i < arity; i++, args = args->type_arg_next) {
		UpdateStateInfoWithStrictInfo (args->type_arg_node,&strict_args[i], strict_added, warning);
	}

#if CLEAN2
	if (*strict_added)
	{
		Assume (strict_positions->sp_size > 2, "not enough bits", "UpdateStateInfosWithStrictInfos");
		Assume (strict_positions_last_one > 2, "not enough bits", "UpdateStateInfosWithStrictInfos");
		strict_positions->sp_bits [0] |= 1 << 1;
	}

	rule->type_alt_strict_positions = StrictPositionsCopy ();
#endif

	/* the result has no sense at the moment */	
}

static void update_exported_function_state_info_with_strict_info (TypeNode node, StrictInfo *s)
{
	Bool is_strict_annotated, is_strict, is_tuple;

	is_strict_annotated = node->type_node_annotation==StrictAnnot;
	is_tuple = IsTupleInfo (s);
	is_strict = (is_tuple ? GetTupleStrictKind (s) : GetStrictKind (s, 0)) != NotStrict;

	if (!is_strict_annotated && is_strict)
		node->type_node_annotation=StrictAnnot;

	if (is_tuple && (is_strict || is_strict_annotated)){
		unsigned	arity,i;
		TypeArgs	args;

		arity = s->strict_arity;
		args  = node->type_node_arguments;

		for (i = 0; i < arity; i++, args = args->type_arg_next)
			update_exported_function_state_info_with_strict_info (args->type_arg_node,&GetTupleInfo (s,i));
	}
}

static void update_exported_function_type_state_infos_with_strict_infos (TypeAlts rule, unsigned arity, StrictInfo *strict_args)
{
	unsigned	i;
	TypeArgs	args;

	if (! rule)
		return;

	args = rule->type_alt_lhs->type_node_arguments;

	for (i = 0; i < arity; i++, args = args->type_arg_next)
		update_exported_function_state_info_with_strict_info (args->type_arg_node,&strict_args[i]);
}

Bool IsListArg (Fun *f, unsigned n)
{
	TypeArgs	args;
	TypeAlts	typerule;
	unsigned	i;
	
	if (f->fun_kind == Function)
		typerule = f->fun_symbol->sdef_rule->rule_type;
	else
		/* ?? */
		return False;
		
	args = typerule->type_alt_lhs->type_node_arguments;
	
	for (i = 0; i < n; i++)
		args = args->type_arg_next;
	
	return (! args->type_arg_node->type_node_is_var && args->type_arg_node->type_node_symbol->symb_kind==list_type);
}

static Bool HasListResult (Fun *f)
{
	TypeAlts	typerule;

	if (f->fun_kind == Function)
		typerule = f->fun_symbol->sdef_rule->rule_type;
	else
		return False;
		
	return (!typerule->type_alt_rhs->type_node_is_var && typerule->type_alt_rhs->type_node_symbol->symb_kind==list_type);
}

static void BuildInfFunction (Fun *f)
{
	Alts alt, alt2;
	Exp lhs, rhs, arg_cons, arg_x, arg_y, nil_exp; 
	
	/* the following function is built:
	   
	   E2 (Cons x y) = E2 y
	   E2 Nil        = Nil
	
	*/

	f->fun_symbol      = Null;
	f->fun_arity       = 1;
	f->fun_kind        = Function;
	f->fun_strictargs  = InitNewStrictInfos (1, HnfStrict);
	f->fun_single      = False;
	InitStrictResult (& f->fun_strictresult);

	f->fun_alts = alt  = SAllocType (AltsRepr);
	alt2               = SAllocType (AltsRepr);
	alt->fun_has_fail   = False;
	alt->fun_next      = alt2;
	alt2->fun_has_fail  = False;
	alt2->fun_next     = Null;
	
	nil_exp             = NewValueExp (nilsym, True, 0);
	arg_x               = NewExp (Argument, 0, False, 1);
	arg_y               = NewExp (Argument, 0, False, 1);
	arg_cons            = NewValueExp (conssym, True, 2);
	arg_cons->e_args[0] = arg_x;
	arg_cons->e_args[1] = arg_y;
	lhs                 = NewValueExp (inffunct_sym, False, 1);
	lhs->e_args[0]      = arg_cons;
	rhs                 = NewValueExp (inffunct_sym, False, 1);
	rhs->e_args[0]      = arg_y;
	
	alt->fun_lhs        = lhs;
	alt->fun_rhs        = rhs;
	
	lhs                 = NewValueExp (inffunct_sym, False, 1);
	lhs->e_args[0]      = nil_exp;
	rhs                 = nil_exp;
	alt2->fun_lhs       = lhs;
	alt2->fun_rhs       = rhs;
}

static void BuildBotmemFunction (Fun *f)
{
	Alts alt, alt2;
	Exp lhs, rhs, arg_cons, arg_x, arg_y, strict_rhs, nil_exp; 
	
	/* the following function is built:
	   
	   E3 (Cons x y) = Strict x (E3 y)
	   E3 Nil        = Nil
	   	   
	*/

	f->fun_symbol      = Null;
	f->fun_arity       = 1;
	f->fun_kind        = Function;
	f->fun_strictargs  = InitNewStrictInfos (1, HnfStrict);
	f->fun_single      = False;
	InitStrictResult (& f->fun_strictresult);

	f->fun_alts = alt  = SAllocType (AltsRepr);
	alt2               = SAllocType (AltsRepr);
	alt->fun_has_fail   = False;
	alt->fun_next      = alt2;
	alt2->fun_has_fail  = False;
	alt2->fun_next     = Null;
	
	nil_exp             = NewValueExp (nilsym, True, 0);
	arg_x               = NewExp (Argument, 0, False, 1);
	arg_y               = NewExp (Argument, 0, False, 1);
	arg_cons            = NewValueExp (conssym, True, 2);
	arg_cons->e_args[0] = arg_x;
	arg_cons->e_args[1] = arg_y;
	lhs                 = NewValueExp (botmemfunct_sym, False, 1);
	lhs->e_args[0]      = arg_cons;
	rhs                 = NewValueExp (botmemfunct_sym, False, 1);
	rhs->e_args[0]      = arg_y;

	strict_rhs          = NewValueExp (strict_sym[0], False, 2);
	strict_rhs->e_args[0]= arg_x;
	strict_rhs->e_args[1]= rhs;
	
	alt->fun_lhs        = lhs;
	alt->fun_rhs        = strict_rhs;
	
	lhs                 = NewValueExp (botmemfunct_sym, False, 1);
	lhs->e_args[0]      = nil_exp;
	rhs                 = nil_exp;
	alt2->fun_lhs       = lhs;
	alt2->fun_rhs       = rhs;	
}

static void init_predefined_symbols (void)
{
	unsigned i;
	Fun	*f,*funs;
	unsigned nr_funs;

	/* add entries for tuples (MaxTupleArity), selectors (MaxTupleArity),
	   strict functions (for strict annots), lists (2), conditional (4)
	   and the apply. Also for the two list functions if necessary.
	 */
	nr_funs = MaxNodeArity + MaxNodeArity + MaxNrAnnots + 2 + 4 + 2
#if STRICT_LISTS
				/* +3 */
				+11
#endif
	;
	if (StrictDoLists)
		nr_funs += 2;
		
	/* allocate enough space for the function table */
	funs = (Fun *) SAlloc ((unsigned long) nr_funs * sizeof (Fun));
	
	/* initialise the function table with tuples */
	for (i = 0, f = funs; i < MaxNodeArity; i++, f++){
		tuplesym[i] = f;
		f->fun_symbol     = Null; /* TupleDefs[i]; */
		f->fun_arity      = i;
		f->fun_kind       = Constructor;
		f->fun_strictargs = Null;
		f->fun_single     = True;
		InitStrictResult (& f->fun_strictresult);
	}
	
	/* initialise the function table with selectors and update functions */
	for (i = 0; i < MaxNodeArity; i++,f++){
		selectsym[i] = f;
		f->fun_symbol     = Null;
		f->fun_arity      = 1;
		f->fun_kind       = SelFunction;
		f->fun_strictargs = InitNewStrictInfos (1, HnfStrict);
		f->fun_single     = False;
		InitStrictResult (& f->fun_strictresult);
	}

#if MORE_ANNOTS
	{
		StrictInfo *shared_strict_infos;
		
		shared_strict_infos=InitNewStrictInfos (MaxNrAnnots+1,HnfStrict);
		
#endif
	/* initialise the function table with strict functions */
	for (i = 0; i < MaxNrAnnots; i++,f++){
		strict_sym[i] = f;
		f->fun_symbol     = Null;
		f->fun_arity      = i+2;
		f->fun_kind       = StrictFunction;
#if MORE_ANNOTS
		f->fun_strictargs = shared_strict_infos;
#else
		f->fun_strictargs = InitNewStrictInfos (i+2, HnfStrict);
#endif
		f->fun_single     = False;
		InitStrictResult (& f->fun_strictresult);
	}

#if MORE_ANNOTS
	}
#endif
	
	/* initialise the function table with lists, conditional and apply */
	nilsym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 0;
	f->fun_kind       = Constructor;
	f->fun_strictargs = Null;
	f->fun_single     = False;
	InitStrictResult (& f->fun_strictresult);
	f++;

#if !STRICT_LISTS
	conssym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 2;
	f->fun_kind       = Constructor;
	f->fun_strictargs = Null;
	f->fun_single     = False;
	InitStrictResult (& f->fun_strictresult);
	f++;
#else
	lazy_cons_sym0 = f;

	for (i=0; i<=2; ++i){
		f->fun_symbol     = Null;
		f->fun_arity      = i;
		f->fun_kind       = Constructor;
		f->fun_strictargs = Null;
		f->fun_single     = False;
		InitStrictResult (& f->fun_strictresult);
		f++;
	}
	
	strict_cons_sym0 = f;

	for (i=0; i<=2; ++i){
		f->fun_symbol = NULL;
		f->fun_arity = i;
		f->fun_kind = Constructor;
		f->fun_strictargs = InitNewStrictInfos (2,NotStrict);;
		f->fun_single = False;
		InitStrictInfo (f->fun_strictargs,HnfStrict);
		InitStrictResult (&f->fun_strictresult);
		++f;
	}
	
	tail_strict_cons_sym0 = f;

	for (i=0; i<=2; ++i){
		f->fun_symbol = NULL;
		f->fun_arity = i;
		f->fun_kind = Constructor;
		f->fun_strictargs = InitNewStrictInfos (2,NotStrict);;
		f->fun_single = False;
		InitStrictInfo (&f->fun_strictargs[1],HnfStrict);
		InitStrictResult (&f->fun_strictresult);
		++f;
	}
	
	strict_tail_strict_cons_sym0 = f;
	for (i=0; i<=2; ++i){
		f->fun_symbol = NULL;
		f->fun_arity = i;
		f->fun_kind = Constructor;
		f->fun_strictargs = InitNewStrictInfos (2,NotStrict);;
		f->fun_single = False;
		InitStrictInfo (f->fun_strictargs,HnfStrict);
		InitStrictInfo (&f->fun_strictargs[1],HnfStrict);
		InitStrictResult (&f->fun_strictresult);
		++f;
	}

	conssym = lazy_cons_sym0+2;
#endif

	if_sym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 3;
	f->fun_kind       = IfFunction;
	f->fun_strictargs = InitNewStrictInfos (3, NotStrict);
	f->fun_single     = False;
	InitStrictInfo (f->fun_strictargs, HnfStrict);
	InitStrictResult (& f->fun_strictresult);
	f++;

	true_sym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 0;
	f->fun_kind       = Constructor;
	f->fun_strictargs = Null;
	f->fun_single     = False;
	InitStrictResult (& f->fun_strictresult);
	f++;

	false_sym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 0;
	f->fun_kind       = Constructor;
	f->fun_strictargs = Null;
	f->fun_single     = False;
	InitStrictResult (& f->fun_strictresult);
	f++;

	fail_sym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 0;
	f->fun_kind       = FailFunction;
	f->fun_strictargs = Null;
	f->fun_single     = False;
	InitStrictResult (& f->fun_strictresult);
	f++;

	apsym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 2;
	f->fun_kind       = ApFunction;
	f->fun_strictargs = InitNewStrictInfos (2, NotStrict);
	f->fun_single     = False;
	InitStrictInfo (f->fun_strictargs, HnfStrict);
	InitStrictResult (& f->fun_strictresult);
	f++;

	strictapsym = f;
	f->fun_symbol     = Null;
	f->fun_arity      = 2;
	f->fun_kind       = ApFunction;
	f->fun_strictargs = InitNewStrictInfos (2, NotStrict);
	f->fun_single     = False;
	InitStrictInfo (f->fun_strictargs, HnfStrict);
	InitStrictInfo (&f->fun_strictargs[1], HnfStrict);
	InitStrictResult (& f->fun_strictresult);
	f++;
	
	/* initialise the function table with the inf and botmem function function */
	if (StrictDoLists){
		inffunct_sym = f;
		BuildInfFunction (f);
		f++;

		botmemfunct_sym = f;
		BuildBotmemFunction (f);
		f++;
	}
}

static void convert_imp_rule_type (SymbDef sdef)
{
	Fun *f;
	unsigned arity;
	TypeAlts rule_type;

	f=SAllocType (Fun);

	sdef->sdef_sa_fun = f;
	arity = sdef->sdef_arity;

	f->fun_kind = Function;
	f->fun_symbol = sdef;
	f->fun_arity  = arity;

	rule_type = sdef->sdef_rule->rule_type;
/*
	#ifdef _DB_
		printf ("ConvertStateInfoToStrictInfos %s\n",sdef->sdef_ident->ident_name);
	#endif
*/
	ConvertStateInfoToStrictInfos (rule_type,arity, &f->fun_strictargs, &f->fun_strictresult, !StrictChecks);
}

static void convert_imp_rule_alts (SymbDef sdef)
{
	Fun *f;

	f=sdef->sdef_sa_fun;
	if (f->fun_kind==Function){
		ImpRules rule;
		
		rule = f->fun_symbol->sdef_rule;
		ConvertAlternatives (&f->fun_alts,rule->rule_alts);
	} else
		f->fun_alts = NULL;
}

static void ConvertSyntaxTree (Symbol symbols)
{
	unsigned arity;
	Symbol		sym;
	Bool		annot_warning;
	SymbDef 	sdef;
	Fun	*f;

	annot_warning = False;

	init_predefined_symbols();
	
	/* initialise the function table with constructors */
	for_l (sym,symbols,symb_next)
		if (sym->symb_kind==definition){
			sdef = sym->symb_def;

			if (sdef->sdef_kind==TYPE){
				ConstructorList talts;
	
				for_l (talts,sdef->sdef_type->type_constructors,cl_next){
					SymbDef cdef;
					
					f=SAllocType (Fun);
					
					cdef = talts->cl_constructor->type_node_symbol->symb_def;
	
					cdef->sdef_sa_fun = f;
					f->fun_symbol        = cdef;
					arity = f->fun_arity = cdef->sdef_arity;			
					f->fun_single        = False;
					f->fun_kind          = Constructor;
					f->fun_single        = cdef->sdef_type->type_nr_of_constructors == 1;

					cdef->sdef_constructor=talts;

					if (cdef->sdef_strict_constructor)
						ConvertTypeArgsToStrictInfos (talts->cl_constructor->type_node_arguments,arity,&f->fun_strictargs, True);
					else
						f->fun_strictargs = NULL;
	
					InitStrictResult (& f->fun_strictresult);
				}
			} else if (sdef->sdef_kind==RECORDTYPE){
				f=SAllocType (Fun);

				sdef->sdef_sa_fun = f;
				f->fun_symbol        = sdef;
				arity = f->fun_arity = sdef->sdef_arity;			
				f->fun_kind          = Constructor;
				f->fun_single        = True;
	
				if (sdef->sdef_strict_constructor)
					ConvertTypeArgsToStrictInfos (TypeArgsOfRecord (sdef), arity,&f->fun_strictargs, True);
				else
					f->fun_strictargs = Null;
				
				InitStrictResult (& f->fun_strictresult);
			}
		}
	
	/* initialise the function table with symbols with a definition */
	for_l (sdef,scc_dependency_list,sdef_next_scc)
		if (sdef->sdef_kind==IMPRULE)
			convert_imp_rule_type (sdef);

	/* convert the rules */
	for_l (sdef,scc_dependency_list,sdef_next_scc)
		if (sdef->sdef_kind==IMPRULE)
			convert_imp_rule_alts (sdef);

	/* give a warning for annotated functions */
	if (annot_warning && StrictAllWarning)
		GiveStrictWarning ((char *) Null, "no strictness analysis for functions with code blocks");
}

static void update_function_strictness (SymbDef sdef)
{
	Fun *f;
	unsigned arity;

	f=sdef->sdef_sa_fun;

	arity = f->fun_arity;
	
	if (f->fun_kind == Function){
		TypeAlts rule;
		Bool strict_added,warning;
		
		rule = sdef->sdef_rule->rule_type;

#if 0
		printf ("%s\n",sdef->sdef_ident->ident_name);
#endif
		
		strict_added = False;
		warning      = False;
		UpdateStateInfosWithStrictInfos (rule, arity, f->fun_strictargs, &f->fun_strictresult,&strict_added, &warning);

		if (sdef->sdef_exported){
			if (strict_added){
				if (!AddStrictnessToExportedFunctionTypes)
					export_warning = True;
			}
			
			if (AddStrictnessToExportedFunctionTypes && sdef->sdef_dcl_icl!=NULL)
				update_exported_function_type_state_infos_with_strict_infos (sdef->sdef_dcl_icl->sdef_rule_type->rule_type_rule, arity, f->fun_strictargs);
		}
	
		if (warning && (StrictAllWarning || StrictChecks))
			GiveStrictWarning (sdef->sdef_ident->ident_name, "not all user annotations could be derived");

		if (export_warning && (StrictAllWarning || StrictExportChecks))
			GiveStrictWarning (sdef->sdef_ident->ident_name, "function not annotated as being strict in definition module");
	}
}

static void UpdateSyntaxTree (void)
{
	SymbDef sdef;
	
	for_l (sdef,scc_dependency_list,sdef_next_scc)
		if (sdef->sdef_kind==IMPRULE)
			update_function_strictness (sdef);
}

/*******************************************************************************
 * The Abstract Reducer                                                        *
 ******************************************************************************/

static Bool ReduceInContext (ExpP ep, Path p, Context context);

static int rel_depth = 0;

static Bool CheckRelation (Exp e, Path p, Context context)
{
	Exp exp_new, exp_cq, exp_dum;
	Bool result;
	unsigned old_fuel;
	
	/*
		FPrintF (outfile, "\n\nAreRelated?");
		FPrintF (outfile, "\ne:    ");
		DumpExp (outfile, e);
		FPrintF (outfile, "\np->e: ");
		DumpExp (outfile, p->p_exp);
		FPrintF (outfile, "\nexp1: ");
		DumpExp (outfile, *s_exp1);
		FPrintF (outfile, "\nexp2: ");
		DumpExp (outfile, *s_exp2);
		FPrintF (outfile, "\n");
	*/

	/* check if there is a common subexpression */
	if (! IsContainedIn (*s_exp2, s_exp1))
		return False;

	/*
		FPrintF (outfile, "Yes\nqexp: ");
		DumpExp (outfile, *q_exp);
		FPrintF (outfile, "\n\n");
	*/
	
	rel_depth++;

#ifdef _DB_EQ_
	if (DBPrinting){
		FPrintF (outfile, "Result: AreRelated (");
		DumpExp (outfile, *s_exp1);
		FPrintF (outfile, ", ");
		DumpExp (outfile, *s_exp2);
		FPrintF (outfile, ", ");
		DumpExp (outfile, *q_exp);
		FPrintF (outfile, ")\n\n");
	}
#endif /* _DB_EQ_ */	

	/* we have the following situation (e is a growing expression)
	       e    = C[C"[q]]
	       p->e = C[q]
	   
	   with
	       s_exp1 = C"[q] 
	       s_exp2 = q     (inside p->e)
	       q_exp  = q     (inside C"[q])
	   
	   we will reduce
	       C[x : <q, C"[x]>]
	 */

	/* fetch C"[q] from e (i.e. replace it by a copy) */
	exp_cq  = InstantiateExp (*s_exp1);
	exp_dum = *s_exp1;
	*s_exp1 = exp_cq;
	exp_cq  = exp_dum;
	
	/* replace q by <expcq,q>, but only if q is not Bot */
	if ((*q_exp)->e_kind == Bottom)
		*q_exp  = exp_cq;
	else
	{	exp_dum = NewExp (Lub, 2, True, 2);
		exp_dum->e_args[0] = exp_cq;
		exp_dum->e_args[1] = *q_exp;
		*q_exp            = exp_dum;
		SortExpOfKind (exp_dum, Lub);
	}
	
	/* create an expression to be reduced: C[q] becomes C[exp_cq] */
	exp_dum  = InstantiateExp (p->p_exp);
	exp_new  = p->p_exp;
	p->p_exp = exp_dum;
	*s_exp2  = exp_cq;
	
	/* instantiate ?? */
	exp_new  = InstantiateExp (exp_new);
	
#ifdef _DB_RED_
	if (DBPrinting)
		FPrintF (outfile, "Relation (%d) --> ", rel_depth);
#endif
	
	old_fuel   = start_fuel;
	result     = ReduceInContext (&exp_new, (Path) Null, copy_context (context));
	start_fuel = old_fuel;
	
#ifdef _DB_RED_
	if (DBPrinting)
		FPrintF (outfile, "\n<-- End relation (%d)\n", rel_depth);
#endif

	rel_depth--;
	return result;
}

static Bool IsInPath (Exp e, Path p, Exp *r, Context context)
{
	for ( ; p; p = p->p_next){
		Bool b;
		
		CheckAreRelated = DoStrictRelated;
		
		b = LtExp (e, p->p_exp);
		
		CheckAreRelated = False;
		
		if (b == True){
			*r = p->p_root;
			return True;
		} else if (b == AreRelated){
			if (CheckRelation (e, p, context)){
				*r = p->p_root;
				return True;
			}
		}
	}
	return False;
}

static Path AddToPath (Exp e, Path p)
{
	Path new;
	
	if (! StrictDoPaths)
		return p;
	
	if (e->e_kind != Value || e->e_fun->fun_kind != Function)
		return p;

	if (! StrictDoAllPaths && p && p->p_exp->e_kind == Value && p->p_exp->e_fun->fun_symbol &&
		p->p_exp->e_fun->fun_symbol->sdef_ancestor != e->e_fun->fun_symbol->sdef_ancestor)
		return p;
		
	new = SAllocType (PathRepr);
		
	new->p_exp  = InstantiateExp (e);
	new->p_root = e;
	new->p_next = p;
	return new;
}

/*
static Path AddToPath (Exp e, Path p)
{	Path new, p2;
	
	if (! StrictDoPaths)
		return p;
	
	if (e->e_kind != Value || e->e_fun->fun_kind != Function)
		return p;

	if (! StrictDoAllPaths && p && p->p_exp->e_kind == Value && p->p_exp->e_fun->fun_symbol &&
	    p->p_exp->e_fun->fun_symbol->sdef_ancestor != e->e_fun->fun_symbol->sdef_ancestor)
		return p;
		
	new = SAllocType (PathRepr);
		
	new->p_exp  = InstantiateExp (e);
	new->p_root = e;
	new->p_next = Null;
	
	if (! p)
		return new;
		
	for (p2 = p; p2->p_next; p2 = p2->p_next)
		;
	
	p2->p_next = new;
	
	return p;
} AddToPath
*/
		
static MatchKind CombineWithPartialMatch (MatchKind m)
{
	switch (m){
		case InfiniteMatch:
		case PartialInfiniteMatch:
			return PartialInfiniteMatch;
		case NoMatch:
			return NoMatch;
		case LubMatch:
			return LubMatch;
		case ReduceMatch:
			return ReduceMatch;
		default:	
			return PartialMatch;
	}
}

static void BindArgsToTop (Exp *args, unsigned arity, Bool *no_patterns)
{
	unsigned i;

	for (i = 0; i < arity; i++){
		switch (args[i]->e_kind){
			case Argument:
				args[i]->e_args[0] = NewTop();
				continue;
			case Value:
				if (! args[i]->e_fun->fun_single)
					*no_patterns = False;
				BindArgsToTop (args[i]->e_args, args[i]->e_fun->fun_arity, no_patterns);
				break;
			case Lub:
				Assume2 (False, "Lub in pattern", "BindArgsToExp");
			default:
				*no_patterns = False;
		}
	}
}

static Bool ReduceDepExpression (Exp e, Path p, Context context)
{
	unsigned arity, i;
	
	arity = e->e_sym;
	
	for (i = 0; i < arity; i++){
		if (ReduceInContext (& e->e_args[i], p, NewSimpleContext (HnfStrict, context->context_speculative)))
			return True;
		/*	JvG: if an argument is be equal to e (for example: let x=x*x in x),
			and ReduceInContext runs out of fuel, e will be updated with Top,
			and the rest of the arguments will no longer be valid */
		if (e->e_kind!=Dep)
			return e->e_kind==Bottom;
		/**/
	}
	
	SortExpOfKind (e, Dep);
	
	if (e->e_kind == Bottom)
		return True;

	/* collect all dependencies, and replace by Top */
	arity = e->e_sym;
	for (i = 0; i < arity; i++)
	{	if (e->e_args[i]->e_kind != Bottom)
			e->e_deps = AddDeps  (e->e_args[i]->e_deps, e->e_deps);
	}
	e->e_kind = Top;
	e->e_hnf  = True;
	
	return False;
}

static Exp ConvertExpWithContext (Exp e, Context context)
{
	if (context->context_arity != 1)
		return e;
	
	switch (context->context_kind){
		case SpineStrict:
		{
			Exp new;
		
			new = NewValueExp (inffunct_sym, False, 1);
			new->e_args[0] = e;
			return new;
		}
		case TailStrict:
		{
			Exp new;
		
			new = NewValueExp (botmemfunct_sym, False, 1);
			new->e_args[0] = e;
			return new;
		}
		default:
			return e;
	}
}

static Bool CheckStrictArgsOfFunction (Exp e, Path p, Context context)
{
	unsigned	arity, i;
	Fun *f;
	StrictInfo	*strictargs;
	Context		newcontext;
	Exp new, *args;
	Dependency  newdeps;
	
	f       = e->e_fun;
	args    = e->e_args;
	newdeps = e->e_deps;
	
	if (! (strictargs = f->fun_strictargs))
		return False;

	arity = f->fun_arity;
	for (i = 0; i < arity; i++){
		newcontext = StrictInfoToContext (& strictargs[i], context, False);
		if (! IsStrictContext (newcontext))
			continue;
			
		new = ConvertExpWithContext (args[i], newcontext);

		if (ReduceInContext (& new, p, newcontext))
			return True;
	
		CopyDeps (new->e_deps, & newdeps);
	}
	
	e->e_deps = newdeps;
	return False;
}

static Exp TakeContextLub (ExpP ep1, ExpP ep2, Path p, Context context)
{
	if (*ep1){
		if (ReduceInContext (ep1, p, context))
			*ep1 = & bottom;
	} else
		*ep1 = & bottom;

	if (*ep2){
		if (ReduceInContext (ep2, p, context))
			*ep2 = & bottom;
	} else
		*ep2 = & bottom;

	return TakeLub (*ep1, *ep2);
}

static MatchKind MatchArgs (Exp args_act[], Exp args_for[], unsigned n, Dependency *dep, ExpP *e_stopp);

static MatchKind MatchExp (ExpP ep_act,Exp e_for,Dependency *dep,Exp **e_stopp)
{
	MatchKind m;

	if (e_for->e_kind==Argument){
		e_for->e_args[0] = *ep_act;
		return TotalMatch;
	} else if (!(*ep_act)->e_hnf){
		*e_stopp = ep_act;
		return ReduceMatch;
	} else if ((*ep_act)->e_kind == Bottom)
		return InfiniteMatch;
	else if ((*ep_act)->e_kind == Lub){
		*e_stopp = ep_act;
		return LubMatch;
	}

	/* the formal argument is a pattern, the actual argument a reduce, non-Bottom, non-Lub
	   value, so start the pattern matching
	*/

	switch (e_for->e_kind){
		case Top:
			m = PartialMatch;
			break;
		case FunValue:
			if ((*ep_act)->e_kind == FunValue){
				if (e_for->e_fun == (*ep_act)->e_fun){
					m = TotalMatch;
					break;
				} else
					return NoMatch;
			}
			m = PartialMatch;
			break;
		case Value:
			switch ((*ep_act)->e_kind){
				case Top:
				case Dep:
				case Ind:
				{
					Bool no_patterns;

					/* In case of a constructor with only one alternative we have a TotalMatch */
					no_patterns = True;
					BindArgsToTop (e_for->e_args, e_for->e_fun->fun_arity, &no_patterns);

					if (no_patterns && e_for->e_fun->fun_single)
						m = TotalMatch;
					else
						m = PartialMatch;
					break;
				}
				case Value:
					if ((*ep_act)->e_fun != e_for->e_fun)
						return NoMatch;
					
					m = MatchArgs ((*ep_act)->e_args, e_for->e_args, (*ep_act)->e_fun->fun_arity, dep, e_stopp);
					if (m != PartialMatch && m != TotalMatch)
						return m;
					break;
				default:
					Assume (False, "illegal case", "MatchExp");
					return NoMatch;
			}
			break;
		default:
			Assume (False, "illegal case", "MatchExp");
			return NoMatch;
	}
	
	/* we have a partial or total match, test now for dependencies */
	if ((*ep_act)->e_deps && (*ep_act)->e_kind != Bottom)
		CopyDeps ((*ep_act)->e_deps, dep);
	
	return m;
}

static MatchKind MatchArgs (Exp args_act[],Exp args_for[],unsigned n,Dependency *dep,ExpP *e_stopp)
{
	MatchKind m;

	if (n == 0)
		return TotalMatch;
	
	m = MatchExp (&args_act[0],args_for[0],dep,e_stopp);

	switch (m){
		case LubMatch:
		case ReduceMatch:
		case NoMatch:
		case InfiniteMatch:
		case PartialInfiniteMatch:
			return m;
		case PartialMatch:
			return CombineWithPartialMatch (MatchArgs (&args_act[1], &args_for[1], n-1, dep, e_stopp));
		case TotalMatch:
			return MatchArgs (&args_act[1], &args_for[1], n-1, dep, e_stopp);
		default:
			Assume (False, "unknown case", "MatchArgs");
			return NoMatch;
	}
}

static MatchKind MatchAlternative (Exp *ep,Exp *args_act,Exp *args_for,Alts alt,unsigned n,Dependency rootdeps,Path p,Context context);

static MatchKind MatchAlternative (Exp *ep,Exp *args_act,Exp *args_for,Alts alt,unsigned n,Dependency rootdeps,Path p,Context context)
{
	MatchKind m;
	ExpP e_stopp;
	Dependency newdeps;

	newdeps = NULL;
	*ep     = NULL;
	
	m = MatchArgs (args_act,args_for,n,&newdeps,&e_stopp);
	
	switch (m){
		case  LubMatch:
		{
			Exp next_e,*lub_args,e_stop;
			unsigned k, i;
			MatchKind next_m;

			next_e   = NULL;
			
			/* store the Lub expression (it can be changed by future reductions, but the argument vector cannot) */
			 
			e_stop = *e_stopp;
			lub_args = e_stop->e_args;
			k        = e_stop->e_sym;
			m        = NoMatch;

			/* replace the Lub expression with all its elements */
			for (i = 0; i < k; i++){
				*e_stopp = lub_args[i];

				next_m = MatchAlternative (&next_e, args_act, args_for, alt, n, rootdeps, p, context);

				switch (next_m){
					case NoMatch:
						if (m == TotalMatch)
							m = PartialMatch;
						continue;
					case InfiniteMatch:
					case PartialInfiniteMatch:
						if (m == NoMatch)
							m = PartialInfiniteMatch;
						continue;
					case PartialMatch:
						m = PartialMatch;
						*ep = TakeContextLub (ep, &next_e, p, context);
						break;
					case TotalMatch:
						if (m == NoMatch && i == 0)
							m = TotalMatch;
						else if (m != TotalMatch)
							m = PartialMatch;
						*ep = TakeContextLub (ep, &next_e, p, context);
						break;
				}
			}

			/* restore the original expression */
			*e_stopp = e_stop;
			
			/* return the match result */
			if (m == LubMatch)
				m = NoMatch;
			break;
		}
		case ReduceMatch:
			ReduceInContext (e_stopp, p, NewSimpleContext (HnfStrict, False));
			return MatchAlternative (ep, args_act, args_for, alt, n, rootdeps, p, context);
		case InfiniteMatch:
		case PartialInfiniteMatch:
		case NoMatch:
			break;
		case PartialMatch:
		case TotalMatch:	
#ifdef _DB_
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			if (alt->fun_switch_alts==NULL)
# endif
			if (!ArgsBound (alt->fun_rhs)){
				FPrintF (StdError, "WARNING!!!\n");
				if (alt->fun_lhs==NULL)
					FPutS ("NULL",StdError);
				else
					DumpExp (StdError, alt->fun_lhs);
				FPutC ('\n', StdError);
				DumpExp (StdError, alt->fun_rhs);
				FPutC ('\n', StdError);
				DumpExp (StdError, *args_act);
				FPutC ('\n', StdError);
				/* Assume (False, "Not all args bound", "MatchAlternative"); */
			}
#endif

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			if (alt->fun_switch_alts==NULL){
#endif

#ifdef _DB_
				fprintf (outfile,"InstantiateExp: ");
				DumpExp (outfile,alt->fun_rhs);
				fprintf (outfile,"\n");
#endif

				*ep = InstantiateExp (alt->fun_rhs);

				if ((*ep)->e_kind!=Bottom){
					CopyDeps (rootdeps,&newdeps);
					(*ep)->e_deps = newdeps;
				}
				
#if 1 /* JVG */
				if (m==TotalMatch && alt->fun_has_fail && (*ep)->e_kind==Value && (*ep)->e_fun->fun_kind==IfFunction){				
					(*ep)->e_red = True;
		
					if (CheckStrictArgsOfFunction (*ep,p,context)){
						UpdateExp (&bottom,*ep);
						(*ep)->e_red = False;
						return InfiniteMatch;
					}
					
					(*ep)->e_red = False;
				}
#endif

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			} else {
				if (!alt->fun_is_guard){
					Exp switch_arg_exp,new_e,next_e;
					Alts switch_alt;
					MatchKind next_m,m2;
					
					switch_arg_exp=alt->fun_rhs;
					if (switch_arg_exp->e_kind!=Argument)
						error_in_function ("MatchAlternative");
					
					switch_arg_exp=switch_arg_exp->e_args[0];
					next_e=NULL;
					new_e=NULL;
					
					m2=NoMatch;
					
					for_l (switch_alt,alt->fun_switch_alts,fun_next){
						if (switch_alt->fun_lhs!=NULL){
							next_m=MatchAlternative (&next_e,&switch_arg_exp,&switch_alt->fun_lhs,switch_alt,1,rootdeps,p,context);
# if 0 && defined(_DB_)
							fprintf (outfile,"MatchAlternative\nactual arg = ");
							DumpExp (outfile,switch_arg_exp);
							fprintf (outfile,"\nformal arg = ");
							DumpExp (outfile,switch_alt->fun_lhs);
							fprintf (outfile,"\n");
# endif
						} else
							next_m=MatchAlternative (&next_e,&switch_arg_exp,&switch_alt->fun_lhs,switch_alt,0,rootdeps,p,context);

						switch (next_m){
							case NoMatch:
								continue;
							case PartialInfiniteMatch:
								if (m2==NoMatch)
									m2=PartialInfiniteMatch;
								continue;
							case InfiniteMatch:
								if (m2==NoMatch)
									m2=InfiniteMatch;
								if (new_e==NULL)
									new_e=&bottom;
								break;
							case PartialMatch:
								m2=PartialMatch;
								new_e=TakeContextLub (&new_e,&next_e,p,context);
								if (new_e->e_kind==Top && new_e->e_deps==NULL)
									break;
								continue;
							case TotalMatch:
								new_e=TakeContextLub (&new_e,&next_e,p,context);
								if (switch_alt->fun_has_fail){
									m2=PartialMatch;
									continue;
								}
								m2=TotalMatch;
								break;
							default:
								error_in_function ("MatchAlternative");
						}
						break;
					}
					
					if (m==PartialMatch)
						m=CombineWithPartialMatch (m2);
					else
						m=m2;
					*ep=new_e;
				} else {
					Exp new_e,next_e,dummy_exp;
					MatchKind next_m;
					
					new_e = InstantiateExp (alt->fun_rhs);

					if (new_e->e_kind!=Bottom){
						CopyDeps (rootdeps,&newdeps);
						new_e->e_deps = newdeps;
					}
					
					if (m==TotalMatch && new_e->e_kind==Value && new_e->e_fun->fun_kind==IfFunction){				
						new_e->e_red = True;
			
						if (CheckStrictArgsOfFunction (new_e,p,context)){
							UpdateExp (&bottom,new_e);
							new_e->e_red = False;
							*ep=new_e;
							return InfiniteMatch;
						}
						
						new_e->e_red = False;
					}
					
					dummy_exp=NULL;
					next_m=MatchAlternative (&next_e,&dummy_exp,&dummy_exp,alt->fun_switch_alts,0,rootdeps,p,context);

					switch (next_m){
						case NoMatch:
							m=NoMatch;
							break;
						case PartialInfiniteMatch:
							m=PartialInfiniteMatch;
							break;
						case InfiniteMatch:
							if (new_e==NULL)
								new_e=&bottom;
							m=PartialMatch;
							break;
						case PartialMatch:
							new_e=TakeContextLub (&new_e,&next_e,p,context);
							m=PartialMatch;
							break;
						case TotalMatch:
							new_e=TakeContextLub (&new_e,&next_e,p,context);
							break;
						default:
							error_in_function ("MatchAlternative");
					}
										
					*ep=new_e;
				}
			}
#endif
			break;
		default:
			Assume (False, "illegal case", "MatchAlternative");
	}
		
	return m;
}

/*******************************************************************************
 * Support for indirections                                                    *
 ******************************************************************************/

static Bool ContainsIndirection2 (Exp e)
{
	unsigned i,arity;
	
	if (e->e_mark)
		return False;
	
	if (e->e_deps)
		return True;
	
	switch (e->e_kind){
		case Bottom:
		case Top:
		case FunValue:
			return False;
		case Ind:
			return True;
		case Value:
			e->e_mark = True;
			arity = e->e_fun->fun_arity;
			break;
		case Dep:
		case Lub:
			e->e_mark = True;
			arity = e->e_sym;
			break;
		default:
			Assume (False, "illegal case", "ContainsIndirection");
			return False;
	}
	
	/* Only reached if kind is Value, Dep or Lub */
	for (i = 0; i < arity; i++)
		if (ContainsIndirection2 (e->e_args[i]))
			return True;

	return False;
}

static Bool ContainsIndirection (Exp e)
{
	Bool res;
	
	res = ContainsIndirection2 (e);

	RemoveMark (e);

	return res;
}

static Bool IsInEachAlt2 (Exp e, Exp root)
{
	unsigned i;
	
	if (e->e_mark)
		return False;

	if (e->e_deps){
		Dependency deps;
		
		for (deps = e->e_deps; deps; deps = deps->dep_next)
			if (deps->dep_exp == root)
				return True;
	}

	switch (e->e_kind){
		case Bottom:
			return True;
		case Top:
		case FunValue:
			return False;
		case Ind:
			/* it is sufficient that there is an indirection. 
			   let: t -> t' -> C[t,t']
			   and suppose we are solving indirections to t'.
			   Other indirections are to t, but since: 
			   
			   C[t,t'] -> C[t',t']
			
			   they can also be considered indirections to t'.
			*/
			return True;
		case Dep:
		case Value:
		{
			int arity;
			
			if (e->e_hnf && e->e_kind != Dep)
				return False;
				
			if (e->e_kind==Value)
				arity=e->e_fun->fun_arity;
			else
				arity=e->e_sym;
			
			e->e_mark = True;
			for (i = 0; i < arity; i++){
				if (IsInEachAlt2 (e->e_args[i], root)){
					e->e_mark = False;
					return True;
				}
			}
			e->e_mark = False;
			return False;
		}
		case Lub:
			e->e_mark = True;
			for (i = 0; i < e->e_sym; i++)
			{	if (! IsInEachAlt2 (e->e_args[i], root))
				{	e->e_mark = False;
					return False;
				}
			}
			e->e_mark = False;
			return True;
		default:
			Assume (False, "illegal case", "IsInEachAlt2");
			return False;
	}
}

static Bool IsInEachAlt (Exp e, Exp root)
{
	Bool res;
	
	res = IsInEachAlt2 (e, root);

	return res;
}

static void ReplaceIndByBottom (Exp e, Exp root)
{
	unsigned i, arity;
	
	if (e->e_imark)
		return;
		
	if (e->e_deps){
		Dependency deps;
		
		for (deps = e->e_deps; deps; deps = deps->dep_next){
			if (deps->dep_exp == root){
				e->e_kind = Bottom;
				e->e_hnf  = True;
				e->e_deps = Null;
				return;
			}
		}
	}

	switch (e->e_kind){
		case Bottom:
		case Top:
		case FunValue:
			return;
		case Ind:
			e->e_kind = Bottom;
			e->e_hnf  = True;
			e->e_deps = Null;
			return;
		case Value:
			e->e_imark = True;
			arity = e->e_fun->fun_arity;
			break;
		case Lub:
		case Dep:
			e->e_imark = True;
			arity = e->e_sym;
			break;
		default:
			Assume (False, "illegal case", "ReplaceIndByBottom");
			return;
	}

	/* Only reached if kind is Value or Lub */
	for (i = 0; i < arity; i++)
		ReplaceIndByBottom (e->e_args[i], root);
} /* ReplaceIndByBottom */

static void ReplaceIndByPtr (Exp *e, Exp root)
{
	unsigned	i, arity;
	
	if ((*e)->e_imark)
		return;
/*
	if ((*e)->e_deps)
	{	Dependency deps;
		
		for (deps = (*e)->e_deps; deps; deps = deps->dep_next)
		{	if (deps->dep_exp == root)
			{	*e = root;
				return;
			}
		}
	}
*/
	switch ((*e)->e_kind){
		case Bottom:
		case Top:
		case FunValue:
			return;
		case Ind:
			if (root == (*e)->e_args[0])
				*e = root;
			return;
		case Value:
			(*e)->e_imark = True;
			arity = (*e)->e_fun->fun_arity;
			break;
		case Lub:
		case Dep:
			(*e)->e_imark = True;
			arity = (*e)->e_sym;
			break;
		default:
			Assume (False, "illegal case", "ReplaceIndByPtr");
			return;
	}
	
	/* Only reached if kind is Value or Lub */
	for (i = 0; i < arity; i++)
		ReplaceIndByPtr (& (*e)->e_args[i], root);
} /* ReplaceIndByPtr */

static void RemoveMarksAndLubs (Exp e)
{
	unsigned arity, i;

	if (! e->e_imark)
		return;
	
	e->e_imark = False;
	switch (e->e_kind){
		case Bottom:
		case Top:
		case Ind:
		case FunValue:
			Assume2 (False, "e is marked illegal", "RemoveMarksAndLubs");
			return;
		case Value:
			arity = e->e_fun->fun_arity;
			break;
		case Lub:
		case Dep:
			arity = e->e_sym;
			break;
		default:
			Assume (False, "illegal case", "RemoveMarksAndLubs");
			return;
	}
	
	/* Only reached if kind is Value or Lub */
	for (i = 0; i < arity; i++)
		RemoveMarksAndLubs (e->e_args[i]);

	if (e->e_kind == Lub)
		SortExpOfKind (e, Lub);
} /* RemoveMarksAndLubs */

static void ResolveIndirections (ExpP rootp, Path p, Context context)
{
	if (! (*rootp)->e_hasind)
		return;
		
	if (IsInEachAlt (*rootp, *rootp))
		ReplaceIndByBottom (*rootp, *rootp);
	else
		ReplaceIndByPtr (rootp, *rootp);
	
	RemoveMarksAndLubs (*rootp);
	if ((*rootp)->e_kind == Value)
		(*rootp)->e_hnf = False;
	ReduceInContext (rootp, p, context);
}

/*******************************************************************************
 *                                                                             *
 *  Reduction functions for the various kinds of functions                     *

	They are called by 'Reduce'. Note that right type of the expression
	and reductions of strict arguments are checked by 'Reduce'

	ReduceStrict
		Strict !s1...!sn x -> x;
	ReduceIf
		If !Bool x x -> x;
	ReduceAp
		Ap !(x -> y) x -> y;
	ReduceSelection
		SelectN !(x1....xM) -> xN;
	ReduceFunction
		general function call

 *                                                                             *
 ******************************************************************************/

static Exp GetResultOfFunctionApplication (Exp e, Path p, Context context)
{
	MatchKind	m;
	Exp			new_e,next_e;
	Alts		alt;
	Fun * f;
#ifdef _DB_RED_
	unsigned	i;
#endif

	new_e = NULL;
	next_e = NULL;
	
	f = e->e_fun;
	
#ifdef _DB_RED_
	for (alt = f->fun_alts, i = 1; alt!=NULL; alt = alt->fun_next, i++){
#else
	for_l (alt,f->fun_alts,fun_next){
#endif
		m = MatchAlternative (&next_e,e->e_args,alt->fun_lhs->e_args,alt,f->fun_arity,e->e_deps,p,context);

#ifdef _DB_RED_
		if (DBPrinting){
			DumpMatch (outfile, m);
			FPrintF (outfile, " (%s, %d)\n", f->fun_symbol ? f->fun_symbol->sdef_ident->ident_name : "??", i);
		}
#endif

		switch (m){
			case NoMatch:
			case PartialInfiniteMatch:
				continue;
			case InfiniteMatch:
				if (new_e)
					return new_e;
				else
					return & bottom;
			case PartialMatch:
				new_e = TakeContextLub (&new_e, &next_e, p, context);
				if (new_e->e_kind == Top && ! new_e->e_deps)
					return new_e;
				continue;
			case TotalMatch:
				new_e = TakeContextLub (&new_e, &next_e, p, context);
				/* consider it as a partial match if the right hand side leads to a fail reduction */
				if (alt->fun_has_fail)
					continue;
				return new_e;
			default:
				Assume (False, "unknown case", "GetResultOfFunctionApplication");
		}
	}

	if (new_e)
		return new_e;
	else
		return &bottom;
}

static Exp ReduceFunction (Exp e, Path p, Context context)
{
	Exp			result;
	StrictInfo  *r;
	Context     newcontext;
	
	r          = &e->e_fun->fun_strictresult;
	newcontext = StrictInfoToContext (r, context, True);

	result = GetResultOfFunctionApplication (e, p, newcontext);
/* JVG */
	if (ReduceInContext (&result, p, newcontext))
/*
	if (ReduceInContext (&result, p, context))
*/
		return & bottom;
	
	return result;
}

static Exp ReduceStrictFunction (Exp e, Path p, Context context)
{
	int arity;
	
	arity = e->e_fun->fun_arity;
	
	if (ReduceInContext (& e->e_args[arity-1], p, context))
		return & bottom;
	else
		return e->e_args[arity-1];
}

static Exp ReduceIfFunction (Exp e, Path p, Context context)
{
	Exp cond;

	cond = e->e_args[0];
	if (cond->e_kind == Value){
		Exp e2;

		if (cond->e_fun==true_sym)
			e2 = e->e_args[1];
		else if (cond->e_fun==false_sym)
			e2 = e->e_args[2];
		else
			return TakeContextLub (& e->e_args[1], & e->e_args[2], p, context);
		
		if (ReduceInContext (& e2, p, context))
			return & bottom;
		else
			return e2;
	} else
		return TakeContextLub (& e->e_args[1], & e->e_args[2], p, context);

}

static Exp ReduceAp (Exp e, Path p, Context context)
{
	Exp e2;
	unsigned n;
	
	/* walk through the left AP spine, note that this spine is in hnf !! */
	for (e2 = e, n = 0; ; e2 = e2->e_args[0], n++){
		if (e2->e_kind == Value && e2->e_fun->fun_kind == ApFunction)
			continue;
		else
			break;
	}
	
	switch (e2->e_kind){
		case Top:
		case Dep:
		case Ind:
			if (IsSpeculativeContext (context))
				return e;
			else
				return NewTop();
		case Lub:
			{	Exp new_e = Null, next_e;
				unsigned arity;
				
				/* if there is a lub, it should be the first element of the spine */
				Assume2 (n == 1, "strange Lub in AP spine", "ReduceAp");
				
				/* replace the top most AP by a Lub of reduced AP nodes */
				arity = e2->e_sym;

				for (n = 0; n < arity; n++){
					next_e = NewValueExp (apsym, False, 2);
					
					next_e->e_args[0] = e2->e_args[n];
					next_e->e_args[1] = e->e_args[1];
					
					new_e = TakeContextLub (& new_e, & next_e, p, context);
				}
				return new_e;
			}
		case Value:
		case Bottom:
			Assume2 (False, "first arg of AP not reduced, or type error", "ReduceAp");
			return NewTop();
		case FunValue:
			{
				unsigned arity;
				Exp new;
				
				arity = e2->e_fun->fun_arity;

				if (arity != n){
					e->e_hnf = True;
					return e;
				}
				new = NewValueExp (e2->e_fun, False, arity);
				
				for (e2 = e, n = arity; n > 0; e2 = e2->e_args[0], n--)
					new->e_args[n-1] = e2->e_args[1];

				if (ReduceInContext (& new, p, context))
					return & bottom;
				else
					return new;
			}
		default:
			Assume (False, "unknown case", "ReduceAp");
			return NewTop();
	}
}

static Exp GetSelection (Exp tuple_exp, unsigned n, Path p, Context context)
{
	switch (tuple_exp->e_kind){
		case Top:
		case Dep:
		case Ind:
			return NewTop();
		case Bottom:
			return & bottom;
		case Value:
		{	ExpP	argp;

			/* JVG: added 14-8-2000 */
			if (!tuple_exp->e_hnf)
				return NewTop();
			/* */

			if (n >= tuple_exp->e_fun->fun_arity)
				return & bottom;

			argp = & tuple_exp->e_args [n];
			if (ReduceInContext (argp, p, context))
				return & bottom;
			else
				return *argp;
		}
		default:
			Assume (False, "illegal case", "GetSelection");
			return &top;
	}
}

static Exp ReduceSelector (Exp e, Path p, Context context)
{
	Exp			tuple_exp, new_e;
	unsigned	i, arity;
	
	tuple_exp = e->e_args[0];
	if (tuple_exp->e_kind == Lub){
		/* try to take the selections of the elements of the lub */
		arity = tuple_exp->e_sym;
		new_e = NewExp (Lub, arity, True, arity);
				
		for (i = 0; i < arity; i++)
			new_e->e_args[i] = GetSelection (tuple_exp->e_args[i], e->e_fun - selectsym[0], p, context);
		
		if (new_e->e_kind == Lub)
			SortExpOfKind (new_e, Lub);
		return new_e;
	}
	else
		return GetSelection (tuple_exp, e->e_fun - selectsym[0], p, context);
}

/* The reduction engine */

static void ReduceArguments (Exp e)
{
	unsigned i, arity;

	arity = e->e_fun->fun_arity;
	
	for (i = 0; i < arity; i++){
#if 0 && defined (_DB_)
		printf ("Reduce argument %d\n",i);
#endif

		(void) ReduceInContext (& e->e_args[i], (Path) Null, NewSimpleContext (HnfStrict, True));

#if 0 && defined (_DB_)
		printf ("End reduce argument %d\n",i);
#endif
	}
}

static Exp MakeIndirection (Exp e)
{
	Exp new;
	
	new                      = NewExp (Ind, 0, True, 1);
	new->e_deps              = SAllocType (DependencyRepr);
	new->e_deps->dep_exp     = e;
	new->e_deps->dep_next    = Null;
	new->e_args[0] = e;
	return new;
}

static void Reduce (ExpP ep, Path p, Context context)
{
	Exp e2,e;

	e = *ep;
	if (e->e_kind==Dep){
		if (ReduceDepExpression (e, p, context)){
			UpdateExp (& bottom, e);
			e->e_red = False;
		}
		return;
	}

	Assume (e->e_kind == Value, "illegal expression kind", "Reduce");
	
	/* mark the node is being under reduction. The marking should be removed
	   before returning
	 */
	e->e_red = True;

#ifdef DIVIDE_FUEL
	{
	unsigned int saved_fuel1,saved_fuel2;
		
	saved_fuel1=(start_fuel>>2);
	saved_fuel2=(start_fuel>>1);

	start_fuel-=saved_fuel1;
#endif
	
	if (CheckStrictArgsOfFunction (e, p, context)){
#ifdef DIVIDE_FUEL
		start_fuel+=saved_fuel1;
#endif
		e = *ep;
		UpdateExp (& bottom, e);
		e->e_red = False;
		return;
	}

#ifdef DIVIDE_FUEL
	start_fuel+=saved_fuel1;
#endif

	if (e->e_fun->fun_kind==Function && StrictDoEager){
#ifdef DIVIDE_FUEL
		if (start_fuel>saved_fuel2){
			start_fuel-=saved_fuel2;
#endif
		e = *ep;
		/* JVG added 23-1-2003: */
		if (e->e_kind==Value && e->e_fun->fun_kind==Function)
		/* */
			ReduceArguments (e);
#ifdef DIVIDE_FUEL
			start_fuel+=saved_fuel2;
		}
#endif
	}
#ifdef DIVIDE_FUEL
	}
#endif

	/* NOTE: the arguments have to be reduced before the next switches
		   statement, because 'ep' itself might be reduced by the above call
	*/

	e = *ep;
	if (e->e_kind != Value)
		return;

	switch (e->e_fun->fun_kind){
		case Constructor:
			e->e_red = False;
			e->e_hnf = True;
			return;
		case Function:
			e2 = ReduceFunction (e, p, context);
			break;
		case IfFunction:
			e2 = ReduceIfFunction (e, p, context);
			break;
		case ApFunction:
			e2 = ReduceAp (e, p, context);
			break;
		case SelFunction:
			e2 = ReduceSelector (e, p, context);
			break;
		case StrictFunction:
			e2 = ReduceStrictFunction (e, p, context);
			break;
		case FailFunction:
			e2 = & bottom;
			break;
		default:
			Assume (False, "illegal function kind", "Reduce");
			return;
	}
	
	e->e_red = False;
	UpdateExp (e2, e);
}

static Bool CheckEndOfReductions (ExpP ep, Path p, Context context, Bool *result)
{
	Exp root, e;

	e = *ep;

	/* check the reduction context */
	if (! IsStrictContext (context))
		return True;

	/* check for hnf and simple context */
	if (e->e_hnf){
/* JVG ??? 2-10-1998 */
		if (e->e_kind==Bottom){
			*result = True;
			return True;
		}
/* */
		if (context->context_arity == 1){
			*result = e->e_kind == Bottom;
			return True;
		}
	}
	
	/* check if expression is already evaluated in speculative context */
	if (IsSpeculativeContext (context) && e->e_spechnf)
		return True;

	/* check if current exp is already under reduction */
	if (e->e_red){
		*ep = MakeIndirection (e);

		/* JVG changed 23-1-2003: */
		e->e_hasind = True; 
		/*
		(*ep)->e_hasind = True;
		*/
#ifdef _DB_RED_
		if (DBPrinting){
			FPrintF (outfile, "Result is indirection: ");
			DumpExp (outfile, *ep);
			FPutC ('\n', outfile);
		}
#endif
		return True;
	}

#ifdef _DB_RED_
	if (DBPrinting){
		FPrintF (outfile, "Reduce (%u ", start_fuel);
		DumpContext (outfile, context);
		FPutS ("): ", outfile);
		DumpExp (outfile, *ep);
		FPrintF (outfile, "\n    Path: ");
		DumpPath (outfile, p);
		FPutC ('\n', outfile);
	}
#endif

	/* check current reduction fuel */
	if (OutOfFuel()){
/* JVG added */
		if (e->e_kind!=Bottom)
/* */
		UpdateExp (& top, e);
	
		if (! max_time_reached){
			if (StrictAllWarning)
				GiveStrictWarning (CurrentName,"out of fuel (result approximated)");
			else
				time_warning = True;
			max_time_reached = True;
		}
#ifdef _DB_RED_
		if (DBPrinting)
			FPrintF (outfile, "Result is approximated\n");
#endif
		return True;
	}

	/* check if exp is in current path */
	if (IsInPath (e, p, & root, context)){
		*ep = MakeIndirection (root);
		root->e_hasind = True;
#ifdef _DB_RED_
		if (DBPrinting){
			FPrintF (outfile, "Result is Indirection: ");
			DumpExp (outfile, *ep);
			FPutC ('\n', outfile);
		}
#endif
		return True;
	}
	
	return False;
}

static Bool ReduceInContext (ExpP ep, Path p, Context context)
{
	Exp e;
	Path newp;
	Bool result = False;
	
#ifdef _DB_RED_
	unsigned e_fuel = start_fuel;
#endif

#ifdef CHECK_STACK_OVERFLOW
	char x;

	if (&x < min_stack){
		printf ("Stack overflow in ReduceInContext\n");
#ifdef _DB_
		FPrintF (outfile, "Stack overflow in ReduceInContext\n");
#endif
/* JVG added */
		if ((*ep)->e_kind!=Bottom)
/* */
		UpdateExp (& top, *ep);
		return False;
	}
#endif

	/* start with some checks which result in easy returns */
	if (CheckEndOfReductions (ep, p, context, & result))
		return result;
	
	e = *ep;
	newp = AddToPath (e, p);

	if (! e->e_hnf){
		e->e_hasind = False;
		Reduce (ep, newp, context);
	}
	
	ResolveIndirections (ep, p, context);

	e = *ep;
	result = False;
	
	if (e->e_kind == Bottom)
		result = True;
	else if (IsSpeculativeContext (context)){
		e->e_spechnf = True;
		result = (e->e_kind == Bottom);
	} else {
/* JVG */
		if (e->e_kind==Value && e->e_fun->fun_kind!=Constructor && ! e->e_hnf && ! ContainsIndirection (e))
/*
		Bool cont_ind;
		
		cont_ind = ContainsIndirection (e);
		if (e->e_kind == Value && e->e_fun->fun_kind != Constructor && ! e->e_hnf && ! cont_ind)
*/
			UpdateExp (& top, e);
	}
	
#ifdef _DB_RED_
	if (DBPrinting){
		FPrintF (outfile, "Result (%d): ", e_fuel);
		DumpExp (outfile, e);
		FPutC ('\n', outfile);
		FPutC ('\n', outfile);
	}
#endif

	if (result == True)
		return True;
	else if (context->context_arity == 1){
		switch (context->context_kind){
			case SpineStrict:
				if (e->e_kind == Value && e->e_fun==conssym){
					if (ReduceInContext (& e->e_args[1], p, context))
						return True;
				}

				if (LtExp ((*ep), & inf) == True)
					return True;
				break;
			case TailStrict:
				if (e->e_kind == Value && e->e_fun==conssym){
					if (ReduceInContext (& e->e_args[1], p, context))
						return True;
					if (ReduceInContext (& (*ep)->e_args[0], p, NewSimpleContext (HnfStrict, False)))
						return True;
				}
				
				if (LtExp ((*ep), & botmem) == True)
					return True;
				break;
			default:
				return result;
		}
	} else {
		unsigned i, arity = context->context_arity;
		
		if (IsTupleExp (e)){
#if 1
			/* JVG: added 15-8-2000 */
			Dependency new_e_deps;
			
			new_e_deps=e->e_deps;
			
			for (i=0; i<arity; i++){
				Context arg_context;
				
				arg_context=context->context_args[i];
				
				if (ReduceInContext (&e->e_args[i],p,arg_context)){
					(*ep) = (*ep)->e_args[i] = &bottom;
					return True;
				}
				
				if (IsStrictContext (arg_context) && e->e_args[i]->e_kind!=Bottom){
					Dependency from_dep;

					for_l (from_dep,e->e_args[i]->e_deps,dep_next){
						Dependency old_dep;
						Exp from_dep_exp;
						
						from_dep_exp=from_dep->dep_exp;
						
						for_l (old_dep,new_e_deps,dep_next)
							if (old_dep->dep_exp==from_dep_exp)
								break;
						
						if (old_dep==NULL){
							Dependency new_dep;

							new_dep = SAllocType (DependencyRepr);
							new_dep->dep_exp = from_dep_exp;

							new_dep->dep_next = new_e_deps;
							new_e_deps = new_dep;
						}
					}
				}
			}
			
			e->e_deps=new_e_deps;
#else
			for (i=0; i<arity; i++){
				if (ReduceInContext (& e->e_args[i], p, context->context_args[i])){
					(*ep) = (*ep)->e_args[i] = &bottom;
					return True;
				}
			}
#endif
		} else {
			if (e->e_kind==Lub){
				for (i=0; i<(*ep)->e_sym; i++){
					if (!ReduceInContext (& (*ep)->e_args[i], p, context))
						return False;
				 
					(*ep)->e_args[i] = & bottom;
				}
				return True;
			} else 
				return False;
		}
	}
	
#ifdef _DB_RED_
	if (DBPrinting){
		FPrintF (outfile, "Result (%d): ", e_fuel);
		DumpExp (outfile, *ep);
		FPutC ('\n', outfile);
		FPutC ('\n', outfile);
	}
#endif

	return result;
}

/* The initialisation functions */

static unsigned found_strict;		/* the number of found strict args	*/

static Fun * cur_funct;				/* the current function id			*/
static unsigned cur_argnr;			/* the current argument number		*/

static Exp BuildTupleExp (StrictInfo *s, Exp bottomelem)
{	Exp e;

	if (s->strict_arity < 0)
	{	s->strict_arity = -s->strict_arity;
		e = bottomelem;
	}
	else if (! IsTupleInfo (s))
		e = NewTop();
	else
	{	unsigned arity, i;
		
		arity = s->strict_arity;
		e     = NewValueExp (tuplesym[arity], True, arity);
		
		for (i = 0; i < arity; i++)
			e->e_args[i] = BuildTupleExp (& GetTupleInfo (s, i), bottomelem);
	}
	
	return e;
}

static Exp BuildApplicationWithBottom (StrictKind argkind, StrictKind context)
{
	Exp			e, bottom_elem;
	unsigned	i;
	unsigned	arity;
	
	arity = cur_funct->fun_arity;
	
	/* set the general values of the expression */
	bottom_elem = & bottom;
	e           = NewValueExp (cur_funct, False, arity);
	
	/* set all arguments to top */
	for (i = 0; i < arity; i++)
		e->e_args[i] = NewTop();

	/* set the right argument to bottom, inf ... */
	switch (argkind){
		case NotStrict:
			return e;
		case HnfStrict:
			bottom_elem = & bottom;
			break;
		case SpineStrict:
			Assume2 (IsListArg (cur_funct,cur_argnr), "BuildAppWithBot" , "??");
			if (IsListArg (cur_funct,cur_argnr))
				bottom_elem = & inf;
			else
				bottom_elem = & bottom;
			break;
		case TailStrict:
			Assume2 (IsListArg (cur_funct,cur_argnr), "BuildAppWithBot" , "??");
			if (IsListArg (cur_funct,cur_argnr))
				bottom_elem = & botmem;
			else
				bottom_elem = & bottom;
			break;
	}
	
	e->e_args[cur_argnr] = BuildTupleExp (&cur_funct->fun_strictargs[cur_argnr], bottom_elem);
	
	/* set the outermost function */
	switch (context){
		case NotStrict:
		case HnfStrict:
			return e;
		case SpineStrict:
			{	Exp e2;
			
			    e2 = NewValueExp (inffunct_sym, False, 1);
			    e2->e_args[0] = e;
				return e2;
			}
		case TailStrict:
			{	Exp e2;
	
			    e2 = NewValueExp (botmemfunct_sym, False, 1);
			    e2->e_args[0] = e;
				return e2;
			}
	}
	
	return e;
}

static void SetStrict (StrictInfo *s, StrictKind kind, unsigned k)
{
	unsigned i;

	if (s == &cur_funct->fun_strictargs[cur_argnr])
		found_strict++;
	
	if (IsTupleInfo (s))
		GetTupleStrictKind (s) = kind;
	else {
		if (! IsListArg (cur_funct, cur_argnr) && kind != NotStrict)
			kind = HnfStrict;

		for (i = k; i < 3; i++)
			GetStrictKind (s, i) = MaxStrict (GetStrictKind (s, i), kind);
	}
}

static Bool CheckIfStrict (StrictKind arg_kind, StrictKind context)
{
	Bool		result;
	Exp			e;
	unsigned	m;
	
	SetStartFuel();
		
	if (setjmp (SAEnv2) == 0){
		e      = BuildApplicationWithBottom (arg_kind, context);
		result = ReduceInContext (& e, (Path) Null, NewSimpleContext (context, False));
	} else
		result = False;
		
	m = MemUse ();
	if (m > max_memuse)
		max_memuse = m;
	
	FreeUnFreezedBlocks();
	
	return (result || e->e_kind == Bottom);
}

static void FindStrictPropsOfStrictInfo (StrictInfo *s, StrictKind arg_kind, StrictKind context)
{
	unsigned i,index;
	
	if (! context)
		return;
		
	index = ContextToIndex (context);
	
	if (IsTupleInfo (s)){
		/* We allow no contexts for lists within a tuple at the moment */
		if (context == SpineStrict || context == TailStrict)
			return;

		if (GetTupleStrictKind (s) == NotStrict){
			s->strict_arity = - s->strict_arity;

			if (CheckIfStrict (arg_kind, context))
				SetStrict (s, HnfStrict, index);
		}
		
		/* Find strictness properties of arguments of tuple */
		if (context != HnfStrict)
			;
		else if (GetTupleStrictKind (s) == HnfStrict){
			for (i = 0; i < s->strict_arity; i++)
				FindStrictPropsOfStrictInfo (& GetTupleInfo(s, i), arg_kind, context);
		}
	} else {
		if (GetStrictKind (s, index) < arg_kind){
			s->strict_arity = - s->strict_arity;

			if (CheckIfStrict (arg_kind, context))
				SetStrict (s, arg_kind, index);
		}
	}
	if (s->strict_arity < 0)
		s->strict_arity = - s->strict_arity;
}

static void DeriveStrictness (Fun *f, unsigned arg, StrictKind arg_kind, StrictKind context)
{
	cur_funct   = f;
	cur_argnr   = arg;
	
	FindStrictPropsOfStrictInfo (&f->fun_strictargs[arg], arg_kind, context);
}

#define IsAnalysableFun(A) ((A)->fun_arity != 0 && (A)->fun_kind == Function)

static void FindStrictPropertiesOfFunction (Fun *f)
{
	unsigned arity,n;
	/* ContextRepr context; */

	n = 0;

	arity = f->fun_arity;

	if (! IsAnalysableFun (f))
		return;
		
	max_depth_reached = False;
	max_time_reached  = False;
	CurrentName       = f->fun_symbol->sdef_ident->ident_name;

#if 0
	printf ("%s\n",CurrentName);
#endif

#ifdef _DB_
	DBPrinting = 1; /* strcmp ("catenate", CurrentName) == 0; */
#endif

#ifdef _DB_STACK_
	if (DBPrinting)
		FPrintF (outfile, "--> %s\n", CurrentName);
#endif

	/* Check if function might terminate, currently disabled since all args
	   have to be changed!! */
	/* DeriveStrictness (f, 0, NotStrict, SimpleContext (&context, HnfStrict, False)); */
				
	/* Check for normal strictness in argument */
	for (n = 0; n < arity; n++)
		DeriveStrictness (f, n, HnfStrict, HnfStrict);
	
	/* Check for special kinds of strictness in the case of lists */
	if (StrictDoLists){
		Bool list_result;
		
		list_result = HasListResult (f);

		for (n = 0; n < arity; n++){
			if (! IsListArg (f, n))
				continue;
				
			/* Hnf context */
			DeriveStrictness (f, n, SpineStrict, HnfStrict);
			DeriveStrictness (f, n, TailStrict, HnfStrict);
	
			if (! list_result)
				continue;

			/* Spine context */
			DeriveStrictness (f, n, SpineStrict, SpineStrict);
			DeriveStrictness (f, n, TailStrict, SpineStrict);
			
			/* Tail context */
			DeriveStrictness (f, n, SpineStrict, TailStrict);
			DeriveStrictness (f, n, TailStrict, TailStrict);
		}
	}

#ifdef _DB_TEST_
	if (StrictDoVerbose)
	{	FPrintF (StdOut, "(%4d)%15s   ", (int) start_fuel, f->fun_symbol->sdef_ident->ident_name);
		DumpStrictInfoOfFunction (StdOut, f);
		FPutC ('\n', StdOut);
	}
#endif
}

#ifdef _DB_TEST_
static void PrintFoundStrictArgs (File w)
{
	unsigned perc,nr_args;
	SymbDef sdef;
	
	nr_args = 0;

	for_l (sdef,scc_dependency_list,sdef_next_scc)
		if (sdef->sdef_kind==IMPRULE && sdef->sdef_over_arity==0){
			Fun *f;
			
			f=sdef->sdef_sa_fun;
					
			if (! StrictDoVerbose)
			{	FPrintF (StdOut, "%15s   ", f->fun_symbol->sdef_ident->ident_name);
				DumpStrictInfoOfFunction (StdOut, f);
				FPutC ('\n', StdOut);
			}
			nr_args += f->fun_arity;
		}
	
	if (nr_args == 0)
		perc = 100;
	else
		perc = (100 * found_strict) / nr_args;
	FPrintF (w, "\n%d strict arguments found (%d%%), %d Kbyte used\n", found_strict, perc, max_memuse);
}
#endif

int init_strictness_analysis (ImpMod imod)
{
	StrictWarning      = DoStrictWarning;
	StrictAllWarning   = DoStrictAllWarning;
	StrictChecks       = DoStrictCheck;
	StrictExportChecks = DoStrictExportChecks;
	
	Verbose ("Strictness analysis");

	/* Initialise all */
#ifdef _DB_
	cur_add         = 1;
/*	outfile         = StdOut; */
	outfile = fopen ("SADump","w");
/*	StrictDoLists   = True; */
	DBPrinting      = False;
#endif

#if CLEAN2
	strict_positions	= NULL;
#endif
	max_memuse   = 0;
	found_strict = 0;
	initialising = True;
	FreeBlocks ();

	/* to be inited before converting the syntaxtree */
	InitExp (&top,    Top,    0,       True);
	InitExp (&bottom, Bottom, 0,       True);

	if (setjmp (SAEnv) == 0){
		
		ConvertSyntaxTree (imod->im_symbols);

		/* other values are converted after syntaxconversion (because of cons symbol) */
		InitValues ();

		/*
		dump the table (DB mode only) 
		DumpTable (StdOut);
		return;
		*/

		FreezeAlloc ();
	
		initialising   = False;

		return True;
	} else {
		FreeBlocks ();
		if (StrictWarning)
			GiveStrictWarning (NULL,"not enough memory for strictness analysis");

#ifdef _DB_
		FClose (outfile);
#endif
		return False;
	}
}

void do_strictness_analysis (void)
{
#ifdef CHECK_STACK_OVERFLOW
	char x;

	min_stack = &x - 20*1024;
#endif

	depth_warning  = False;
	time_warning   = False;
	export_warning = False;
	mem_warning    = False;

	/* Do the analysis */
	{
		SymbDef sdef;

		for_l (sdef,scc_dependency_list,sdef_next_scc)
			if (sdef->sdef_kind==IMPRULE)
				FindStrictPropertiesOfFunction (sdef->sdef_sa_fun);
	}
	
	UpdateSyntaxTree();

#ifdef _DB_TEST_
	PrintFoundStrictArgs (StdOut);
#endif

#ifdef _DB_
	FClose (outfile);
#endif

#if 0
	if (StrictWarning){
		if (mem_warning || depth_warning || time_warning)
			GiveStrictWarning (NULL, "derived strictness properties approximated");
	} else
#endif
	if (StrictAllWarning){
		if (mem_warning)
			GiveStrictWarning (NULL,"strictness analysis out of memory (result approximated)");
		if (depth_warning)
			GiveStrictWarning (NULL,"max depth reached in strictness analysis (result approximated)");
		if (time_warning)
			GiveStrictWarning (NULL,"max time needed in strictness analysis (result approximated)");
	}
	
	if (StrictWarning && export_warning)
		GiveStrictWarning ((char *) Null, "not all derived strictness information is exported");
	
	free_unused_sa_blocks();
}

void finish_strictness_analysis (void)
{
	if (n_allocated_blocks!=0){
		if (bottom.e_kind!=Bottom || bottom.e_hnf!=True || top.e_kind!=Top || top.e_hnf!=True)
			ErrorInCompiler ("sa","","Bottom or top changed");

		FreeBlocks();
	}
}

void StrictnessAnalysis (ImpMod imod)
{
	if (init_strictness_analysis (imod)){
		do_strictness_analysis();
		finish_strictness_analysis();
	}
}

int StrictnessAnalysisConvertRules (ImpRuleS *rules)
{
	if (initialising)
		return 0;

	initialising=True;
	
	if (setjmp (SAEnv)==0){
		ImpRuleS *rule;

		for_l (rule,rules,rule_next)
			convert_imp_rule_type (rule->rule_root->node_symbol->symb_def);

		for_l (rule,rules,rule_next)
			convert_imp_rule_alts (rule->rule_root->node_symbol->symb_def);

		FreezeAlloc();
	
		initialising = False;
		return 1;
	} else {
		FreeUnFreezedBlocks();
		if (StrictWarning)
			GiveStrictWarning (NULL,"not enough memory for strictness analysis of all functions");
		return 0;
	}
}

void StrictnessAnalysisForRule (SymbDef sdef)
{
	FindStrictPropertiesOfFunction (sdef->sdef_sa_fun);
	
	update_function_strictness (sdef);
}
