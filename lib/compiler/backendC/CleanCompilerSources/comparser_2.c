/*
		Ronny Wichers Schreur
		University of Nijmegen
*/

# pragma segment comparser
# ifdef THINK_C
# pragma options (!macsbug_names)
# endif

# undef	PRINT_RULES_AFTER_PARSING
# undef	STOP_AFTER_PARSING

# undef H

# include	"compiledefines.h"
# include	"types.t"
# include	"syntaxtr.t"

# include	"comsupport.h"
# include	"scanner.h"
# include	"sizes.h"
# include	"checker.h"
# include	"statesgen.h"
# include	"comparser.h"
# include	"buildtree.h"
# include	"settings.h"
# include	"checksupport.h"

# ifdef PRINT_RULES_AFTER_PARSING
# include "dbprint.h"
# endif

static void	*gSymbIdEnv;

static IdentP	gBasicTypeIdents [Nr_Of_Basic_Types], gIfIdent;

static SymbolP
NewPredefinedTypeSymbol (SymbKind symbolKind, KeywordKind keyWordKind, IdentP *identPtr)
{
	char		*symbolName;
	SymbolP		symbol;
	IdentP		ident;

	symbolName	= ReservedWords [keyWordKind];
	symbol		= NewSymbol (symbolKind);

	ident				= PutStringInHashTable (symbolName, TypeSymbolIdTable);
	ident->ident_symbol	= symbol;
	ident->ident_environ= (char*)gSymbIdEnv;
	*identPtr			= ident;

	return (symbol);
} /* NewPredefinedSymbol */

static SymbolP
NewPredefinedSymbol (SymbKind symbolKind, KeywordKind keyWordKind, IdentP *identPtr)
{
	char		*symbolName;
	SymbolP		symbol;
	IdentP		ident;

	symbolName	= ReservedWords [keyWordKind];
	symbol		= NewSymbol (symbolKind);

	ident				= PutStringInHashTable (symbolName, SymbolIdTable);
	ident->ident_symbol	= symbol;
	ident->ident_environ= (char*)gSymbIdEnv;
	*identPtr			= ident;

	return (symbol);
} /* NewPredefinedSymbol */

void
InitParser (void)
{
	int		i;

	ScanInitialise ();
#ifndef CLEAN2
	MakeErrorStructures ();

	gCurrentContext			= NULL;
	gNodeIdEnv				= (char *) 1;
	/* RWS, hack to avoid name space confusion bug */
	gAttributeEnv			= (char *) (1 << 16);

	gAttrVarAdmin = NULL;
#endif
	for (i = 0; i < MaxNodeArity; i++)
	{	SelectSymbols	 [i] = NULL;
		TupleTypeSymbols [i] = NULL;
	}

	BasicTypeSymbols [int_type]		= NewPredefinedTypeSymbol (int_type, intsym, & gBasicTypeIdents [int_type]);
	BasicTypeSymbols [bool_type]	= NewPredefinedTypeSymbol (bool_type, boolsym, & gBasicTypeIdents [bool_type]);
	BasicTypeSymbols [char_type]	= NewPredefinedTypeSymbol (char_type, charsym, & gBasicTypeIdents [char_type]);
	BasicTypeSymbols [string_type]	= NewPredefinedTypeSymbol (string_type, stringsym, & gBasicTypeIdents [string_type]);
	BasicTypeSymbols [real_type]	= NewPredefinedTypeSymbol (real_type, realsym, & gBasicTypeIdents [real_type]);
	BasicTypeSymbols [file_type]	= NewPredefinedTypeSymbol (file_type, filesym, & gBasicTypeIdents [file_type]);
	BasicTypeSymbols [world_type]	= NewPredefinedTypeSymbol (world_type, worldsym, & gBasicTypeIdents [world_type]);

	ArraySymbols [LazyArrayInstance]	= NewPredefinedTypeSymbol (array_type, arraysym, &gArrayIdents [LazyArrayInstance]);
	ArraySymbols [StrictArrayInstance]	= NewPredefinedTypeSymbol (strict_array_type, strictarraysym, &gArrayIdents [StrictArrayInstance]);
	ArraySymbols [UnboxedArrayInstance] = NewPredefinedTypeSymbol (unboxed_array_type, unboxedarraysym, &gArrayIdents [UnboxedArrayInstance]);
	
	BasicTypeSymbols [procid_type]	= NewPredefinedTypeSymbol (procid_type, procidsym, & gBasicTypeIdents [procid_type]);

	IfSymbol		= NewPredefinedSymbol (if_symb, ifsym, &gIfIdent);
	BasicTypeSymbols [redid_type]	= NewPredefinedTypeSymbol (procid_type, procidsym, & gBasicTypeIdents [redid_type]);
	ApplyTypeSymbol	= NewSymbol (fun_type);

	TrueSymbol		= NewSymbol (bool_denot);
	TrueSymbol->symb_bool = True;
	FalseSymbol		= NewSymbol (bool_denot);
	FalseSymbol->symb_bool = False;

	TupleSymbol		= NewSymbol (tuple_symb);
	ListSymbol		= NewSymbol (list_type);
	ConsSymbol		= NewSymbol (cons_symb);
	NilSymbol		= NewSymbol (nil_symb);

#if STRICT_LISTS
	ListSymbol->symb_head_strictness=0;
	ListSymbol->symb_tail_strictness=0;

	ConsSymbol->symb_head_strictness=0;
	ConsSymbol->symb_tail_strictness=0;

	NilSymbol->symb_head_strictness=0;
	NilSymbol->symb_tail_strictness=0;

	StrictListSymbol= NewSymbol (list_type);
	StrictListSymbol->symb_head_strictness=2;
	StrictListSymbol->symb_tail_strictness=0;

	UnboxedListSymbol= NewSymbol (list_type);
	UnboxedListSymbol->symb_head_strictness=3;
	UnboxedListSymbol->symb_tail_strictness=0;

	TailStrictListSymbol= NewSymbol (list_type);
	TailStrictListSymbol->symb_head_strictness=0;
	TailStrictListSymbol->symb_tail_strictness=1;

	StrictTailStrictListSymbol= NewSymbol (list_type);
	StrictTailStrictListSymbol->symb_head_strictness=2;
	StrictTailStrictListSymbol->symb_tail_strictness=1;

	UnboxedTailStrictListSymbol= NewSymbol (list_type);
	UnboxedTailStrictListSymbol->symb_head_strictness=3;
	UnboxedTailStrictListSymbol->symb_tail_strictness=1;

	StrictConsSymbol= NewSymbol (cons_symb);
	StrictConsSymbol->symb_head_strictness=2;
	StrictConsSymbol->symb_tail_strictness=0;

	UnboxedConsSymbol= NewSymbol (cons_symb);
	UnboxedConsSymbol->symb_head_strictness=3;
	UnboxedConsSymbol->symb_tail_strictness=0;

	TailStrictConsSymbol= NewSymbol (cons_symb);
	TailStrictConsSymbol->symb_head_strictness=0;
	TailStrictConsSymbol->symb_tail_strictness=1;

	StrictTailStrictConsSymbol= NewSymbol (cons_symb);
	StrictTailStrictConsSymbol->symb_head_strictness=2;
	StrictTailStrictConsSymbol->symb_tail_strictness=1;

	UnboxedTailStrictConsSymbol= NewSymbol (cons_symb);
	UnboxedTailStrictConsSymbol->symb_head_strictness=3;
	UnboxedTailStrictConsSymbol->symb_tail_strictness=1;

	StrictNilSymbol	= NewSymbol (nil_symb);	
	StrictNilSymbol->symb_head_strictness=2;
	StrictNilSymbol->symb_tail_strictness=0;

	UnboxedNilSymbol = NewSymbol (nil_symb);	
	UnboxedNilSymbol->symb_head_strictness=3;
	UnboxedNilSymbol->symb_tail_strictness=0;

	TailStrictNilSymbol	= NewSymbol (nil_symb);	
	TailStrictNilSymbol->symb_head_strictness=0;
	TailStrictNilSymbol->symb_tail_strictness=1;

	StrictTailStrictNilSymbol = NewSymbol (nil_symb);
	StrictTailStrictNilSymbol->symb_head_strictness=2;
	StrictTailStrictNilSymbol->symb_tail_strictness=1;

	UnboxedTailStrictNilSymbol = NewSymbol (nil_symb);
	UnboxedTailStrictNilSymbol->symb_head_strictness=3;
	UnboxedTailStrictNilSymbol->symb_tail_strictness=1;
#endif

	ApplySymbol		= NewSymbol (apply_symb);
	ApplySymbol->symb_instance_apply = 0;

	FailSymbol		= NewSymbol (fail_symb);

	clear_p_at_node_tree();
} /* InitParser */
