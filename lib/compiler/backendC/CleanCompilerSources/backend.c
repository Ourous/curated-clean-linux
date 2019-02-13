#define CODE_INLINE_FLAG
#define DYNAMIC_TYPE 1

# include "compiledefines.h"
# include "types.t"
# include "system.h"
# include "syntaxtr.t"
# include "codegen_types.h"
# include "statesgen.h"
# include "codegen.h"
# include "codegen2.h"
# include "instructions.h"
# include "sizes.h"
# include "set_scope_numbers.h"

# include "scanner.h"		/* InitScanner, ScanInitIdentStringTable */
# include "checker.h"		/* scc_dependency_list, ClearOpenDefinitionModules, AddOpenDefinitionModule */
# include "comsupport.h" 	/* CurrentModule */
# include "buildtree.h"		/* TupleSymbol, ApplySymbol */

extern void InitARC_Info (void); /* from typeconv.h */

# include "backendsupport.h"
# define Clean(ignore)

# include "dynamics.h"

# include "backend.h"

# include <limits.h>

# if 0
# include "dbprint.h"
# endif

void
BEGetVersion (int *current, int *oldestDefinition, int *oldestImplementation)
{
	*current				= kBEVersionCurrent;
	*oldestDefinition		= kBEVersionOldestDefinition;
	*oldestImplementation	= kBEVersionOldestImplementation;
}

#if STRICT_LISTS
 PolyList unboxed_record_cons_list,unboxed_record_decons_list;
#endif

extern PolyList UserDefinedArrayFunctions;	/* typechecker.c */
extern int StdOutReopened, StdErrorReopened; /* cocl.c */

/*
	Strings
	=======
*/
static char *
ConvertCleanString (CleanString string)
{
	int		length;
	char	*copy;

	length	= string->length;
	copy	= ConvertAlloc (length+1);
	strncpy (copy, string->chars, length);
	copy [length]	= '\0';

	return (copy);
} /* ConvertCleanString */

/*
	Counting routines
*/

static short
CountTypeArgs (BETypeArgP args)
{
	short	n;

	n	= 0;
	for (; args != NULL; args = args->type_arg_next)
		n++;

	return (n);
} /* CountTypeArgs */

static short
CountArgs (BEArgP args)
{
	short	n;

	n	= 0;
	for (; args != NULL; args = args->arg_next)
		n++;

	return (n);
} /* CountArgs */

/*
	BE routines
*/
STRUCT (be_module, BEModule)
{
	char			*bem_name;
	Bool			bem_isSystemModule;

	unsigned int	bem_nFunctions;
	SymbolP			bem_functions;
	unsigned int	bem_nConstructors;

	unsigned int	bem_nTypes;
	SymbolP			*bem_types;

	SymbolP			*bem_constructors;

	unsigned int	bem_nFields;
	SymbolP			bem_fields;
};

STRUCT (be_icl_module, BEIcl)
{
	ImpMod			beicl_module;
	BEModuleS		beicl_dcl_module;

	// +++ remove this (build deps list separately)
	SymbDefP		*beicl_depsP;
	unsigned int	beicl_previousAncestor;
};

STRUCT (be_state, BEState)
{
	Bool			be_initialised;

	char			**be_argv;
	int				be_argc;
	int				be_argi;

	BEModuleP		be_modules;

	BEIclS			be_icl;
	unsigned int	be_nModules;

	SymbolP			be_allSymbols;
	SymbolP			be_dontCareSymbol;
	SymbolP			be_dictionarySelectFunSymbol;
	SymbolP			be_dictionaryUpdateFunSymbol;

	// temporary hack
	int				be_dynamicTypeIndex;
	int				be_dynamicModuleIndex;
};

static BEStateS	gBEState = {False /* ... */};

STRUCT (be_locally_generated_function_info, BELocallyGeneratedFunction)
{
	char	*lgf_name;
	int		lgf_arity;
};

static BELocallyGeneratedFunctionS gLocallyGeneratedFunctions[] = {{"_dictionary_select", 3}, {"_dictionary_update", 4}};
# define	kDictionarySelect	0
# define	kDictionaryUpdate	1

// +++ put in gBEState
static NodeIdP (*gCurrentNodeIds)=NULL;
static int n_gCurrentNodeIds = 0;
static SymbolP	gBasicSymbols [Nr_Of_Predef_FunsOrConses];
static SymbolP	gTupleSelectSymbols [MaxNodeArity];

static int number_of_node_ids=0;

typedef IdentP *IdentH;
static IdentH gSpecialIdents[BESpecialIdentCount];

static IdentP
Identifier (char *name)
{
	IdentP	ident;

	ident	= ConvertAllocType (IdentS);
	ident->ident_name	= name;

	return (ident);
} /* Identifier */

static SymbolP
PredefinedSymbol (SymbKind symbolKind, int arity)
{
	SymbolP	symbol;

	symbol	= ConvertAllocType (SymbolS);

	symbol->symb_kind	= symbolKind;
	symbol->symb_arity	= arity;

	return (symbol);
} /* PredefinedSymbol */

static SymbolP
AllocateSymbols (int nTypeRecordAndConstructorSymbols,int nFieldSymbols, SymbolP allSymbols)
{
	int	nSymbols;

	nSymbols=nTypeRecordAndConstructorSymbols+nFieldSymbols;
	
	if (nSymbols > 0){
		int i;
		SymbolP	symbols;

		symbols	= (SymbolP) ConvertAlloc (nSymbols * sizeof (SymbolS));

		for (i = 0; i < nTypeRecordAndConstructorSymbols; i++){
			symbols [i].symb_kind	= erroneous_symb;
			symbols [i].symb_next	= &symbols [i+1];
		}
		
		if (nTypeRecordAndConstructorSymbols>0){
			symbols [nTypeRecordAndConstructorSymbols-1].symb_next	= allSymbols;
			allSymbols=symbols;
		}
		
		for (i = nTypeRecordAndConstructorSymbols; i < nSymbols; i++){
			symbols [i].symb_kind	= erroneous_symb;
			symbols [i].symb_next	= NULL;
		}
	}

	return (allSymbols);
} /* AllocateSymbols */

static void
InitPredefinedSymbols (void)
{
	int	i;

	gBasicSymbols [int_type]		= PredefinedSymbol (int_type, 0);
	gBasicSymbols [bool_type]		= PredefinedSymbol (bool_type, 0);
	gBasicSymbols [char_type]		= PredefinedSymbol (char_type, 0);
	gBasicSymbols [real_type]		= PredefinedSymbol (real_type, 0);
	gBasicSymbols [file_type]		= PredefinedSymbol (file_type, 0);
	gBasicSymbols [world_type]		= PredefinedSymbol (world_type, 0);
#if DYNAMIC_TYPE
	gBasicSymbols [dynamic_type]= PredefinedSymbol (dynamic_type, 0);
#endif
	gBasicSymbols [array_type]			= PredefinedSymbol (array_type, 1);
	gBasicSymbols [strict_array_type]	= PredefinedSymbol (strict_array_type, 1);
	gBasicSymbols [unboxed_array_type]	= PredefinedSymbol (unboxed_array_type, 1);

	gBasicSymbols [fun_type]	= PredefinedSymbol (fun_type, 2);

	ApplySymbol	= PredefinedSymbol (apply_symb, 2);
	ApplySymbol->symb_instance_apply = 0;
	gBasicSymbols [apply_symb]	= ApplySymbol;

	TupleSymbol	= PredefinedSymbol (tuple_symb, 2); /* arity doesn't matter */
	gBasicSymbols [tuple_symb]	= TupleSymbol;
	gBasicSymbols [tuple_type]	= PredefinedSymbol (tuple_type, 2);

	gBasicSymbols [if_symb]		= PredefinedSymbol (if_symb, 3);
	gBasicSymbols [fail_symb]	= PredefinedSymbol (fail_symb, 0);

	gBasicSymbols [nil_symb]	= PredefinedSymbol (nil_symb, 0);
	gBasicSymbols [cons_symb]	= PredefinedSymbol (cons_symb, 2);

	for (i = 0; i < MaxNodeArity; i++)
		gTupleSelectSymbols [i]	= NULL;

} /* InitPredefinedSymbols */

static void
AddUserDefinedArrayFunction (SymbolP functionSymbol)
{
	PolyList	elem;

	elem	= ConvertAllocType (struct poly_list);

	elem->pl_elem	= functionSymbol;
	elem->pl_next	= UserDefinedArrayFunctions;
	UserDefinedArrayFunctions	= elem;
} /* AddUserDefinedArrayFunction */

static Node
NewGuardNode (NodeP ifNode, NodeP node, NodeDefP nodeDefs, StrictNodeIdP stricts)
{
	NodeP	guardNode;
	
	guardNode	= ConvertAllocType (NodeS);
	
	guardNode->node_kind		= GuardNode;
	guardNode->node_node_defs	= nodeDefs;
	guardNode->node_arity		= 2;
	guardNode->node_guard_strict_node_ids	= stricts;

	guardNode->node_arguments	= BEArgs (ifNode, BEArgs (node, NULL));
	
	return (guardNode);
} /* NewGuardNode */

static void
DeclareModule (int moduleIndex, char *name, Bool isSystemModule, int nFunctions,
											int nTypes, int nConstructors, int nFields)
{
	BEModuleP	module;
	SymbolP		symbols, allSymbols;

	allSymbols	= gBEState.be_allSymbols;

	allSymbols	= AllocateSymbols (nFunctions + nTypes + nConstructors, nFields, allSymbols);

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	module->bem_name			= name;
	module->bem_isSystemModule	= isSystemModule;

	symbols	= allSymbols;

	module->bem_nFunctions	= (unsigned int) nFunctions;
	module->bem_functions	= symbols;
	symbols	+=	nFunctions;

	module->bem_nTypes	= (unsigned int) nTypes;
//	module->bem_types	= symbols;
	{
		/* +++ do this lazily */
		int	i;
		
		module->bem_types	= (SymbolP *) ConvertAlloc (nTypes * sizeof (SymbolP));

		for (i = 0; i < nTypes; i++)
		{
			module->bem_types [i]	= &symbols [i];
		}
	}
	{
		/* +++ change this */
		int		i;
		for (i = 0; i < nTypes; i++)
		{
			SymbDef	newSymbDef;

			newSymbDef	= ConvertAllocType (SymbDefS);
			newSymbDef->sdef_mark		= 0;
			newSymbDef->sdef_isused	= False;
			symbols [i].symb_def	= newSymbDef;
		}
	}
	symbols	+=	nTypes;

	module->bem_nConstructors	= (unsigned int) nConstructors;
//	module->bem_constructors	= symbols;
	{
		/* +++ do this lazily */
		int	i;
		
		module->bem_constructors	= (SymbolP *) ConvertAlloc (nConstructors * sizeof (SymbolP));

		for (i = 0; i < nConstructors; i++)
		{
			module->bem_constructors [i]	= &symbols [i];
		}
	}
	symbols	+=	nConstructors;

	module->bem_nFields	= (unsigned int) nFields;
	module->bem_fields	= symbols;
	symbols	+=	nFields;

	gBEState.be_allSymbols	= allSymbols;
} /* DeclareModule */

static int main_dcl_module_n=0;

void
BESetMainDclModuleN (int main_dcl_module_n_parameter)
{
	main_dcl_module_n=main_dcl_module_n_parameter;
}

static DefMod im_def_module;

static void	DeclareFunctionC (char *name, int arity, int functionIndex, unsigned int ancestor);
static BESymbolP CreateDictionarySelectFunSymbol (void);
static BESymbolP CreateDictionaryUpdateFunSymbol (void);

void
BEDeclareIclModule (CleanString name, CleanString modificationTime, int nFunctions, int nTypes, int nConstructors, int nFields)
{
	int		i;
	char	*cName;
	SymbolP	moduleNameSymbol;
	ImpMod	iclModule;
	BEIclP	icl;

	cName	= gBEState.be_modules [main_dcl_module_n].bem_name;

	if (cName == NULL)
		cName	= ConvertCleanString (name);

	moduleNameSymbol	= ConvertAllocType (SymbolS);
	moduleNameSymbol->symb_ident	= Identifier (cName);

/*	Assert (strcmp (gBEState.be_modules [main_dcl_module_n].bem_name, cName) == 0); */
	Assert (strncmp (cName, name->chars, name->length) == 0);

	icl	= &gBEState.be_icl;

	icl->beicl_module		= ConvertAllocType (ImpRepr);
	icl->beicl_dcl_module	= gBEState.be_modules [main_dcl_module_n];
	icl->beicl_previousAncestor	= 0;
	scc_dependency_list	= NULL;
	icl->beicl_depsP	= &scc_dependency_list;

	nFunctions	+= ArraySize (gLocallyGeneratedFunctions);
	DeclareModule (main_dcl_module_n, cName, False, nFunctions, nTypes, nConstructors, nFields);

	iclModule	= icl->beicl_module;
	iclModule->im_name			= moduleNameSymbol;
	iclModule->im_modification_time	= ConvertCleanString (modificationTime);
	iclModule->im_def_module	= im_def_module;
	iclModule->im_rules			= NULL;
	iclModule->im_start			= NULL;
	iclModule->im_symbols		= gBEState.be_allSymbols;
# if IMPORT_OBJ_AND_LIB
	iclModule->im_imported_objs	= NULL;
	iclModule->im_imported_libs	= NULL;
# endif
	iclModule->im_foreign_exports=NULL;

	CurrentModule	= cName;

	for (i = 0; i < ArraySize (gLocallyGeneratedFunctions); i++)
	{
		BELocallyGeneratedFunctionP	locallyGeneratedFunction;

		locallyGeneratedFunction	= &gLocallyGeneratedFunctions [i];

		DeclareFunctionC (locallyGeneratedFunction->lgf_name, locallyGeneratedFunction->lgf_arity, nFunctions-ArraySize(gLocallyGeneratedFunctions)+i,0);
	}

	/* +++ hack */
	{
		gBEState.be_dictionarySelectFunSymbol	= CreateDictionarySelectFunSymbol ();
		gBEState.be_dictionaryUpdateFunSymbol	= CreateDictionaryUpdateFunSymbol ();
	}
} /* BEDeclareIclModule */

void
BEDeclareDclModule (int moduleIndex, CleanString name, CleanString modificationTime, int isSystemModule, int nFunctions, int nTypes, int nConstructors, int nFields)
{
	char	*cName;
	SymbolP	moduleNameSymbol;
	DefMod	dclModule;
	SymbolP	saveSymbols;

	cName	= ConvertCleanString (name);

	moduleNameSymbol	= ConvertAllocType (SymbolS);
	moduleNameSymbol->symb_ident	= Identifier (cName);

	if (moduleIndex == main_dcl_module_n)
	{
		saveSymbols	= gBEState.be_allSymbols;
		gBEState.be_allSymbols	= NULL;
	}

	DeclareModule (moduleIndex, cName, isSystemModule, nFunctions, nTypes, nConstructors, nFields);

	dclModule	= ConvertAllocType (DefRepr);
	dclModule->dm_name			= moduleNameSymbol;
	dclModule->dm_modification_time	= ConvertCleanString (modificationTime);
	dclModule->dm_system_module	= isSystemModule;
	dclModule->dm_symbols		= gBEState.be_allSymbols; /* ??? too many symbols? */
	dclModule->dm_system_module_table_kind = FirstSystemModuleTable + moduleIndex;

	AddOpenDefinitionModule (moduleNameSymbol, dclModule);

	if (moduleIndex == main_dcl_module_n){
		gBEState.be_allSymbols	= saveSymbols;
		im_def_module=dclModule;
	}
} /* BEDeclareDclModule */

void
BEDeclarePredefinedModule (int nTypes, int nConstructors)
{
	char	*cName;

	cName	= "_predef";

	DeclareModule (kPredefinedModuleIndex, cName, False, 0, nTypes, nConstructors, 0);
} /* BEDeclarePredefinedModule */

void
BEDeclareModules (int nModules)
{
	int	i;

	Assert (gBEState.be_modules == NULL);

	gBEState.be_nModules	= (unsigned int) nModules;
	gBEState.be_modules		= (BEModuleP) ConvertAlloc (nModules * sizeof (BEModuleS));

	for (i = 0; i < nModules; i++)
	{
		gBEState.be_modules [i].bem_name	= NULL;
		gBEState.be_modules [i].bem_nFunctions	= 0;
	}
} /* BEDeclareModules */

BESymbolP
BEFunctionSymbol (int functionIndex, int moduleIndex)
{
	BEModuleP	module;
	SymbolP		functionSymbol;

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) functionIndex < module->bem_nFunctions);
	functionSymbol	= &module->bem_functions [functionIndex];
	Assert (functionSymbol->symb_kind == definition || functionSymbol->symb_kind == cons_symb || functionSymbol->symb_kind == nil_symb
				|| (moduleIndex == kPredefinedModuleIndex && functionSymbol->symb_kind != erroneous_symb));

	if (functionSymbol->symb_kind!=cons_symb && functionSymbol->symb_kind!=nil_symb)
		functionSymbol->symb_def->sdef_isused	= True;

	return (functionSymbol);
} /* BEFunctionSymbol */

void
BEBindSpecialModule (BESpecialIdentIndex index, int moduleIndex)
{
	BEModuleP	module;

	Assert (index >= 0 && index < BESpecialIdentCount);

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	(*gSpecialIdents [index])->ident_name	= module->bem_name;
} /* BEBindSpecialModule */

void
BEBindSpecialFunction (BESpecialIdentIndex index, int functionIndex, int moduleIndex)
{
	SymbolP		functionSymbol;
	BEModuleP	module;

	Assert (index >= 0 && index < BESpecialIdentCount);

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) functionIndex < module->bem_nFunctions);
	functionSymbol	= &module->bem_functions [functionIndex];

	if (functionSymbol->symb_kind == definition){
		*gSpecialIdents [index]	= functionSymbol->symb_def->sdef_ident;
		
		if (index==BESpecialIdentSeq && moduleIndex!=main_dcl_module_n){
			functionSymbol->symb_kind=seq_symb;
		}
	}
} /* BEBindSpecialFunction */

extern SymbDefP special_types[]; /* defined in statesgen */

void BEBindSpecialType (int special_type_n,int type_index,int module_index)
{
	SymbolP		type_symbol_p;
	BEModuleP	module;

	module	= &gBEState.be_modules [module_index];
	type_symbol_p = module->bem_types [type_index];

	if (type_symbol_p->symb_kind==definition)
		special_types[special_type_n] = type_symbol_p->symb_def;
	else
		special_types[special_type_n] = NULL;
}

BESymbolP
BESpecialArrayFunctionSymbol (BEArrayFunKind arrayFunKind, int functionIndex, int moduleIndex)
{
	Bool		isSpecialArrayFunction;
	BEModuleP	module;
	SymbolP		functionSymbol;
	SymbDefP	sdef;
	SymbDefP	originalsdef;
	TypeAlt		*typeAlt;
	TypeNode	elementType, arrayType;

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) functionIndex < module->bem_nFunctions);
	functionSymbol	= &module->bem_functions [functionIndex];
	Assert (functionSymbol->symb_kind == definition
				|| (moduleIndex == kPredefinedModuleIndex && functionSymbol->symb_kind != erroneous_symb));

	originalsdef	= functionSymbol->symb_def;

	typeAlt	= originalsdef->sdef_rule_type->rule_type_rule;
	isSpecialArrayFunction	= False;
	switch (arrayFunKind)
	{
		case BEArraySelectFun:
			Assert (originalsdef->sdef_arfun == BEArraySelectFun || originalsdef->sdef_arfun == BEUnqArraySelectFun);
			break;
		case BE_UnqArraySelectFun:
		case BE_UnqArraySelectLastFun:
			Assert (typeAlt->type_alt_lhs->type_node_arity == 2);
			elementType	= typeAlt->type_alt_rhs;
			arrayType	= typeAlt->type_alt_lhs->type_node_arguments->type_arg_node;

			isSpecialArrayFunction	= True;
			Assert (originalsdef->sdef_arfun == BEArraySelectFun);
			break;
		case BE_ArrayUpdateFun:
			isSpecialArrayFunction	= True;
			elementType	= typeAlt->type_alt_lhs->type_node_arguments->type_arg_next->type_arg_next->type_arg_node;
			arrayType	= typeAlt->type_alt_lhs->type_node_arguments->type_arg_node;
			/* fall through! */
		case BEArrayUpdateFun:
			Assert (originalsdef->sdef_arfun == BEArrayUpdateFun);
			break;
		default:
			Assert (False);
			break;
	}

	if (isSpecialArrayFunction)
	{
		SymbolP	previousFunctionSymbol;
		Bool	allreadyCreated;

		previousFunctionSymbol	= functionSymbol;
		functionSymbol	= functionSymbol->symb_next;

		allreadyCreated	= False;
		if (functionSymbol != NULL && functionSymbol->symb_kind == definition)
		{
			sdef			= functionSymbol->symb_def;
			allreadyCreated	= sdef->sdef_arfun == (ArrayFunKind) arrayFunKind;
			if (!allreadyCreated && arrayFunKind == BE_UnqArraySelectLastFun && sdef->sdef_arfun == BE_UnqArraySelectFun)
			{
				previousFunctionSymbol	= functionSymbol;
				functionSymbol	= functionSymbol->symb_next;
	
				if (functionSymbol != NULL && functionSymbol->symb_kind == definition)
				{
					sdef			= functionSymbol->symb_def;
					allreadyCreated	= sdef->sdef_arfun == (ArrayFunKind) arrayFunKind;
				}
			}
		}

		if (!allreadyCreated)
		{
			char		*functionName, *functionPrefix;
			TypeAlt		*newTypeAlt;
			IdentP		newIdent;
			SymbDefP	newsdef;
			SymbolP		newFunctionSymbol;
			RuleTypes	newRuleType;
			TypeArgs	lhsArgs;
			TypeNode	rhs;

			newFunctionSymbol	= ConvertAllocType (SymbolS);
			newsdef				= ConvertAllocType (SymbDefS);
			newIdent			= ConvertAllocType (IdentS);

			newTypeAlt	= ConvertAllocType (TypeAlt);

			newTypeAlt->type_alt_attr_equations	= NULL; /* used in PrintType */

			Assert (!arrayType->type_node_is_var);
			switch (arrayType->type_node_symbol->symb_kind)
			{
				case strict_array_type:
				case unboxed_array_type:
					elementType->type_node_annotation	= StrictAnnot;
					break;
				case array_type:
					break;
				default:
					Assert (False);
					break;
			}

			switch (arrayFunKind)
			{
				case BE_UnqArraySelectFun:
					rhs	= BENormalTypeNode (gBasicSymbols [tuple_type],
												BETypeArgs (elementType, BETypeArgs (arrayType, NULL)));
					lhsArgs	= BETypeArgs (arrayType, BETypeArgs (BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [int_type], NULL)), NULL));
					functionPrefix	= "_uselectf";
					break;
				case BE_UnqArraySelectLastFun:
				{
					struct clean_string	rName = {1, 'r'};
					TypeNode			rType;

					rType	= BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&rName));
					rhs	= BENormalTypeNode (gBasicSymbols [tuple_type],
												BETypeArgs (elementType, BETypeArgs (rType, NULL)));
					lhsArgs	= BETypeArgs (
								BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [tuple_type],
										BETypeArgs (arrayType, BETypeArgs (rType, NULL)))),
								BETypeArgs (BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [int_type], NULL)), NULL));
					functionPrefix	= "_uselectl";
					break;
				}
				case BE_ArrayUpdateFun:
				{
					struct clean_string	rName = {1, 'r'};
					TypeNode			rType;

					rType	= BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&rName));
					rhs	= rType;
					lhsArgs	= BETypeArgs (
								BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [tuple_type],
										BETypeArgs (arrayType, BETypeArgs (rType, NULL)))),
								BETypeArgs (BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [int_type], NULL)),
								BETypeArgs (elementType,
								NULL)));
					functionPrefix	= "_updatei";
					break;
				}
				default:
					Assert (False);
					break;
			}

			functionName	= ConvertAlloc (strlen (functionPrefix) + 1 + strlen (originalsdef->sdef_ident->ident_name) + 1);
			strcpy (functionName, functionPrefix);
			strcat (functionName, ";");
			strcat (functionName, originalsdef->sdef_ident->ident_name);

			newTypeAlt->type_alt_lhs	= BENormalTypeNode (newFunctionSymbol, lhsArgs);
			newTypeAlt->type_alt_rhs	= rhs;
			newTypeAlt->type_alt_strict_positions	= NULL;

			newIdent->ident_symbol	= newFunctionSymbol;
			newIdent->ident_name	= functionName;

			newRuleType	= ConvertAllocType (struct rule_type);
			newRuleType->rule_type_rule	= newTypeAlt;

			newsdef->sdef_ident			= newIdent;
			newsdef->sdef_module		= gBEState.be_icl.beicl_module->im_name->symb_def->sdef_module; /* phew! */
			newsdef->sdef_mark		= 0;
			newsdef->sdef_isused		= True;
			newsdef->sdef_exported		= False;
			newsdef->sdef_arity			= newTypeAlt->type_alt_lhs->type_node_arity;
			newsdef->sdef_arfun			= arrayFunKind;
			newsdef->sdef_kind 			= SYSRULE;
			newsdef->sdef_rule_type		= newRuleType;
			newsdef->sdef_ident			= newIdent;
			newsdef->sdef_mark			= 0;

			newFunctionSymbol->symb_kind	= definition;
			newFunctionSymbol->symb_def		= newsdef;

			functionSymbol						= previousFunctionSymbol->symb_next;
			previousFunctionSymbol->symb_next	= newFunctionSymbol;
			newFunctionSymbol->symb_next		= functionSymbol;

			AddUserDefinedArrayFunction (newFunctionSymbol);

			functionSymbol	= newFunctionSymbol;
		}

	}

	return (functionSymbol);
} /* BESpecialArrayFunctionSymbol */

static SymbolP
CreateLocallyDefinedFunction (int index, char ** abcCode, TypeArgs lhsArgs, TypeNode rhsType)
{
	int				i, arity, functionIndex;
	NodeP			lhs;
	BEStringListP	instructions, *instructionsP;
	BECodeBlockP	codeBlock;
	RuleAltP		ruleAlt;
	SymbolP			functionSymbol;
	TypeAlt			*typeAlt;
	ArgP			args;

	functionIndex	= gBEState.be_modules[main_dcl_module_n].bem_nFunctions - ArraySize (gLocallyGeneratedFunctions) + index;
	functionSymbol	= BEFunctionSymbol (functionIndex, main_dcl_module_n);
	functionSymbol->symb_def->sdef_isused	= False;

	instructionsP	= &instructions;
	for (i = 0; abcCode [i] != NULL; i++)
	{
		BEStringListP	string;

		string	= ConvertAllocType (struct string_list);

		string->sl_string	= abcCode [i];
		string->sl_next		= instructions;

		*instructionsP	= string;
		instructionsP	= &string->sl_next;
	}
	*instructionsP	=	NULL;

	codeBlock	= BEAbcCodeBlock (False, instructions);
		
	lhs		= BENormalNode (functionSymbol, NULL);
	arity	= CountTypeArgs (lhsArgs);

	args	= NULL;
	for (i = 0; i < arity; i++)
		args	= BEArgs (BENodeIdNode (BEWildCardNodeId (), NULL), args);

	lhs->node_arguments	= args;
	lhs->node_arity		= arity;

	Assert (arity == functionSymbol->symb_def->sdef_arity);

	ruleAlt		= BECodeAlt (0, NULL, lhs, codeBlock);

	typeAlt	= ConvertAllocType (TypeAlt);

	typeAlt->type_alt_attr_equations	= NULL; /* used in PrintType */
	typeAlt->type_alt_lhs	= BENormalTypeNode (functionSymbol, lhsArgs);
	typeAlt->type_alt_rhs	= rhsType;
	typeAlt->type_alt_strict_positions	= NULL;

	BERule (functionIndex, BEIsNotACaf, typeAlt, ruleAlt);
	
	return (functionSymbol);
} /* CreateLocallyDefinedFunction */

static BESymbolP
CreateDictionarySelectFunSymbol (void)
{
	TypeNode		rhsType;
	TypeArgs		lhsArgs;
	struct clean_string	aName = {1, 'a'};

	/* selectl :: !((a e) Int -> e) !(!a e, !r) !Int -> (e, !r) */
	/* select _ _ _ = code */
	static char *abcCode[] = {
		"push_a 1",
		"push_a 1",
		"build e_system_dAP 2 e_system_nAP",
		"buildI_b 0",
		"push_a 1",
		"update_a 1 2",
		"update_a 0 1",
		"pop_a 1",
		"build e_system_dAP 2 e_system_nAP",
		"push_a 3",
		"push_a 1",
		"update_a 1 2",
		"update_a 0 1",
		"pop_a 1",
		"update_a 1 4",
		"update_a 0 3",
		"pop_a 3",
		"pop_b 1",
		NULL
	};

	/*	actual type simplified to !a !(!a,!a) !Int -> (a,!a) */
	lhsArgs	=	BETypeArgs (
					BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&aName)),
				BETypeArgs (
					BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [tuple_type],
								BETypeArgs (
									BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&aName)),
								BETypeArgs (
									BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&aName)),
								NULL)))),
				BETypeArgs (
					BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [int_type], NULL)),
				NULL)));
	rhsType	= BENormalTypeNode (gBasicSymbols [tuple_type],
								BETypeArgs (BEVarTypeNode (&aName), BETypeArgs (BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&aName)), NULL)));

	return (CreateLocallyDefinedFunction (kDictionarySelect, abcCode, lhsArgs, rhsType));
} /* CreateDictionarySelectFunSymbol */

static BESymbolP
CreateDictionaryUpdateFunSymbol (void)
{
	TypeNode		rhsType;
	TypeArgs		lhsArgs;
	struct clean_string	aName = {1, 'a'};

	/* updatei :: !(*(a .e) -> *(!Int -> *(.e -> .(a .e)))) !(!*(a .e), !*r) !Int .e -> *r // !(!.(a .e), !*r) */
	/* updatei _ _ _ _ = code */
	static char *abcCode[] = {
		"push_a 3",
		"buildI_b 0",
		"push_a 3",
		"push_a 3",
		"pop_b 1",
		"update_a 6 7",
		"update_a 3 6",
		"update_a 2 5",
		"update_a 1 4",
		"updatepop_a 0 3",
		"jsr_ap 3",
		"pop_a 1",
		NULL
	};

	/*	actual type simplified to !a !(!a,!a) !Int a -> a */
	lhsArgs	=	BETypeArgs (
					BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&aName)),
				BETypeArgs (
					BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [tuple_type],
								BETypeArgs (
									BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&aName)),
								BETypeArgs (
									BEAnnotateTypeNode (StrictAnnot, BEVarTypeNode (&aName)),
								NULL)))),
				BETypeArgs (
					BEAnnotateTypeNode (StrictAnnot, BENormalTypeNode (gBasicSymbols [int_type], NULL)),
				BETypeArgs (
					BEVarTypeNode (&aName),
				NULL))));

	rhsType	= BEVarTypeNode (&aName);

	return (CreateLocallyDefinedFunction (kDictionaryUpdate, abcCode, lhsArgs, rhsType));
} /* CreateDictionaryUpdateFunSymbol */

BESymbolP
BEDictionarySelectFunSymbol (void)
{
	gBEState.be_dictionarySelectFunSymbol->symb_def->sdef_isused	= True;

	return (gBEState.be_dictionarySelectFunSymbol);
} /* BEDictionarySelectFunSymbol */

BESymbolP
BEDictionaryUpdateFunSymbol (void)
{
	gBEState.be_dictionaryUpdateFunSymbol->symb_def->sdef_isused	= True;

	return (gBEState.be_dictionaryUpdateFunSymbol);
} /* BEDictionaryUpdateFunSymbol */

BESymbolP
BETypeSymbol (int typeIndex, int moduleIndex)
{
	BEModuleP	module;
	SymbolP		typeSymbol;

	if ((unsigned int) moduleIndex >= gBEState.be_nModules)
		Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) typeIndex < module->bem_nTypes);
	typeSymbol	= module->bem_types [typeIndex];
/*	Assert (typeSymbol->symb_kind == definition
				|| (moduleIndex == kPredefinedModuleIndex && typeSymbol->symb_kind != erroneous_symb));
*/
	if (moduleIndex == main_dcl_module_n)
		typeSymbol->symb_def->sdef_isused	= True;

	return (typeSymbol);
} /* BETypeSymbol */

BESymbolP BETypeSymbolNoMark (int typeIndex, int moduleIndex)
{
	return gBEState.be_modules [moduleIndex].bem_types [typeIndex];
}

BESymbolP
BEDontCareDefinitionSymbol (void)
{
	SymbolP	symbol;

	symbol	= gBEState.be_dontCareSymbol;
	if (symbol == NULL)
	{
		SymbDefP	symbDef;

		symbDef	= ConvertAllocType (SymbDefS);
		symbDef->sdef_kind	= ABSTYPE;

		symbDef->sdef_ident	= Identifier ("_Don'tCare"); /* +++ name */

		symbol	= ConvertAllocType (SymbolS);
		symbol->symb_kind	= definition;
		symbol->symb_def	= symbDef;

		gBEState.be_dontCareSymbol	= symbol;
	}

	return (symbol);
} /* BEDontCareDefinitionSymbol */

BESymbolP
BEConstructorSymbol (int constructorIndex, int moduleIndex)
{
	BEModuleP	module;
	SymbolP		constructorSymbol;

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) constructorIndex < module->bem_nConstructors);
	constructorSymbol	= module->bem_constructors [constructorIndex];

	/* RWS +++ hack for record constructors, remove this */
	if (constructorSymbol->symb_kind == erroneous_symb){
		/* store index in symb_arity until BERecordType is called, should be passed directly to BERecordType */
		constructorSymbol->symb_arity = constructorIndex;
		return constructorSymbol;
	}

	Assert (constructorSymbol->symb_kind == definition || constructorSymbol->symb_kind == cons_symb
				|| (moduleIndex == kPredefinedModuleIndex && constructorSymbol->symb_kind != erroneous_symb));

	if (moduleIndex != kPredefinedModuleIndex && constructorSymbol->symb_kind!=cons_symb)
		constructorSymbol->symb_def->sdef_isused	= True;

	return (constructorSymbol);
} /* BEConstructorSymbol */

BESymbolP
BEFieldSymbol (int fieldIndex, int moduleIndex)
{
	BEModuleP	module;
	SymbolP		fieldSymbol;

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) fieldIndex < module->bem_nFields);
	fieldSymbol	= &module->bem_fields [fieldIndex];
	Assert (fieldSymbol->symb_kind == definition);

	fieldSymbol->symb_def->sdef_isused	= True;

	return (fieldSymbol);
} /* BEFieldSymbol */

BESymbolP
BEBoolSymbol (int value)
{
/* JVG: */
	if (value)
		return TrueSymbol;
	else
		return FalseSymbol;
/*
	SymbolP	symbol;
	
	symbol	= ConvertAllocType (SymbolS);
	symbol->symb_kind	= bool_denot;
	symbol->symb_bool	= value;

	return (symbol);
*/
} /* BEBoolSymbol */

BESymbolP
BELiteralSymbol (BESymbKind kind, CleanString value)
{
	SymbolP	symbol;

	symbol	= ConvertAllocType (SymbolS);
	symbol->symb_kind	= kind;
	symbol->symb_int	= ConvertCleanString (value);

	return (symbol);
} /* BELiteralSymbol */

# define	nid_ref_count_sign	nid_scope

#if STRICT_LISTS
static SymbolS unboxed_list_symbols[Nr_Of_Predef_Types][2];

static SymbolP strict_list_cons_symbols[8];

void BEPredefineListConstructorSymbol (int constructorIndex,int moduleIndex,BESymbKind symbolKind,int head_strictness,int tail_strictness)
{
	BEModuleP	module;
	SymbolP symbol_p;

	Assert (moduleIndex == kPredefinedModuleIndex);

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module = &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) constructorIndex < module->bem_nConstructors);
	
	symbol_p=module->bem_constructors [constructorIndex];

	symbol_p->symb_kind	= symbolKind;
	symbol_p->symb_head_strictness=head_strictness;
	symbol_p->symb_tail_strictness=tail_strictness;

	if (symbolKind==BEConsSymb && head_strictness<4)
		strict_list_cons_symbols[(head_strictness<<1)+tail_strictness]=symbol_p;
}

void BEPredefineListTypeSymbol (int typeIndex,int moduleIndex,BESymbKind symbolKind,int head_strictness,int tail_strictness)
{
	BEModuleP	module;
	SymbolP symbol_p;

	Assert (moduleIndex == kPredefinedModuleIndex);

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module = &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) typeIndex < module->bem_nTypes);

	symbol_p=module->bem_types [typeIndex];

	symbol_p->symb_kind = symbolKind;
	symbol_p->symb_arity = 1;
	symbol_p->symb_head_strictness=head_strictness;
	symbol_p->symb_tail_strictness=tail_strictness;
}

void BEAdjustStrictListConsInstance (int functionIndex,int moduleIndex)
{
	SymbolP symbol_p;

	symbol_p=&gBEState.be_modules[moduleIndex].bem_functions[functionIndex];

	if (symbol_p->symb_kind==definition){
		TypeNode element_type_p,list_type_p;
		SymbDef sdef;
		TypeArgs type_args_p;
		
		sdef=symbol_p->symb_def;
		type_args_p=sdef->sdef_rule_type->rule_type_rule->type_alt_lhs->type_node_arguments;
		element_type_p=type_args_p->type_arg_node;
		list_type_p=type_args_p->type_arg_next->type_arg_node;
		
		Assert (list_type_p->type_node_is_var==0);
		Assert (list_type_p->type_node_symbol->symb_kind==list_type);

		symbol_p->symb_head_strictness=list_type_p->type_node_symbol->symb_head_strictness;
		symbol_p->symb_tail_strictness=list_type_p->type_node_symbol->symb_tail_strictness;

		if (list_type_p->type_node_symbol->symb_head_strictness==3){
			int element_symbol_kind;
			struct unboxed_cons *unboxed_cons_p;

			Assert (element_type_p->type_node_is_var==0);

			element_symbol_kind=element_type_p->type_node_symbol->symb_kind;

			symbol_p->symb_head_strictness=4;

			unboxed_cons_p=ConvertAllocType (struct unboxed_cons);

			unboxed_cons_p->unboxed_cons_sdef_p=sdef;

			if (element_symbol_kind < Nr_Of_Predef_Types)
				unboxed_cons_p->unboxed_cons_state_p = unboxed_list_symbols[element_symbol_kind][symbol_p->symb_tail_strictness].symb_state_p;
			else if (element_symbol_kind==definition && element_type_p->type_node_symbol->symb_def->sdef_kind==RECORDTYPE){
				PolyList new_unboxed_record_cons_element;
				SymbDef record_sdef;
				
				record_sdef=element_type_p->type_node_symbol->symb_def;
				record_sdef->sdef_isused=True;
				sdef->sdef_isused=True;
				unboxed_cons_p->unboxed_cons_state_p = &record_sdef->sdef_record_state;
				
				new_unboxed_record_cons_element=ConvertAllocType (struct poly_list);
				new_unboxed_record_cons_element->pl_elem = sdef;
				new_unboxed_record_cons_element->pl_next = unboxed_record_cons_list;
				unboxed_record_cons_list = new_unboxed_record_cons_element;
				
				sdef->sdef_module=NULL;
			} else
				unboxed_cons_p->unboxed_cons_state_p = &StrictState;
			
			symbol_p->symb_unboxed_cons_p=unboxed_cons_p;
		}
	} else {
		Assert (symbol_p->symb_kind==definition);
		
		debug_message ("BEAdjustStrictListInstance: !(symbol_p->symb_kind==definition) %d %d %d\n",functionIndex,moduleIndex,symbol_p->symb_kind);

		symbol_p->symb_head_strictness=0;
		symbol_p->symb_tail_strictness=0;
	}
	
	symbol_p->symb_kind = cons_symb;
	/* symbol_p->symb_arity = 2; no symb_arity for cons_symb, because symb_state_p is used of this union */
}

void BEAdjustUnboxedListDeconsInstance (int functionIndex,int moduleIndex)
{
	SymbolP symbol_p,cons_symbol_p;
	SymbDefP sdef_p;
	TypeNode element_type_p,list_type_p;
	PolyList new_unboxed_record_decons_element;

	symbol_p=&gBEState.be_modules[moduleIndex].bem_functions[functionIndex];

	Assert (symbol_p->symb_kind==definition);
	sdef_p=symbol_p->symb_def;
	
	list_type_p=sdef_p->sdef_rule_type->rule_type_rule->type_alt_lhs->type_node_arguments->type_arg_node;
	element_type_p=list_type_p->type_node_arguments->type_arg_node;
	
	Assert (list_type_p->type_node_is_var==0);
	Assert (list_type_p->type_node_symbol->symb_kind==list_type);
	Assert (list_type_p->type_node_symbol->symb_head_strictness==3);
	Assert (element_type_p->type_node_symbol->symb_def->sdef_kind==RECORDTYPE);
	
	cons_symbol_p=ConvertAllocType (SymbolS);

	cons_symbol_p->symb_kind = cons_symb;
	cons_symbol_p->symb_head_strictness=4;
	cons_symbol_p->symb_tail_strictness=list_type_p->type_node_symbol->symb_tail_strictness;
	cons_symbol_p->symb_state_p=&element_type_p->type_node_symbol->symb_def->sdef_record_state;

	sdef_p->sdef_unboxed_cons_symbol=cons_symbol_p;
	
	new_unboxed_record_decons_element=ConvertAllocType (struct poly_list);
	new_unboxed_record_decons_element->pl_elem = sdef_p;
	new_unboxed_record_decons_element->pl_next = unboxed_record_decons_list;
	unboxed_record_decons_list = new_unboxed_record_decons_element;
}

void BEAdjustOverloadedNilFunction (int functionIndex,int moduleIndex)
{
	SymbolP symbol_p;

	symbol_p=&gBEState.be_modules[moduleIndex].bem_functions[functionIndex];

	symbol_p->symb_head_strictness=1;
	symbol_p->symb_tail_strictness=0;
	
	symbol_p->symb_kind = nil_symb;
}

BESymbolP BEOverloadedConsSymbol (int constructorIndex,int moduleIndex,int deconsIndex,int deconsModuleIndex)
{
	BEModuleP module,decons_module;
	SymbolP constructor_symbol,decons_symbol,list_type_symbol;
	TypeNode list_type,element_type;

	Assert ((unsigned int) deconsModuleIndex < gBEState.be_nModules);
	decons_module = &gBEState.be_modules [deconsModuleIndex];

	Assert ((unsigned int) deconsIndex < decons_module->bem_nFunctions);
	decons_symbol = &decons_module->bem_functions [deconsIndex];

	Assert (decons_symbol->symb_kind==definition);
	
	list_type=decons_symbol->symb_def->sdef_rule_type->rule_type_rule->type_alt_lhs->type_node_arguments->type_arg_node;
	element_type=list_type->type_node_arguments->type_arg_node;
	
	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module = &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) constructorIndex < module->bem_nConstructors);
	constructor_symbol = module->bem_constructors [constructorIndex];

	Assert (constructor_symbol->symb_kind==definition
				|| (moduleIndex==kPredefinedModuleIndex && constructor_symbol->symb_kind!=erroneous_symb));

	if (moduleIndex != kPredefinedModuleIndex)
		constructor_symbol->symb_def->sdef_isused = True;

	list_type_symbol=list_type->type_node_symbol;

	if (constructor_symbol->symb_head_strictness==1 && list_type_symbol->symb_head_strictness<4)
		constructor_symbol=strict_list_cons_symbols[(list_type_symbol->symb_head_strictness<<1)+list_type_symbol->symb_tail_strictness];

	if (list_type_symbol->symb_head_strictness==3){
		int element_symbol_kind;
		
		Assert (element_type->type_node_is_var==0);

		element_symbol_kind=element_type->type_node_symbol->symb_kind;

		if (element_symbol_kind<Nr_Of_Predef_Types)
			constructor_symbol=&unboxed_list_symbols[element_symbol_kind][list_type_symbol->symb_tail_strictness];
		else if (element_symbol_kind==definition && element_type->type_node_symbol->symb_def->sdef_kind==RECORDTYPE)
			constructor_symbol=decons_symbol->symb_def->sdef_unboxed_cons_symbol;
	}
	
	return constructor_symbol;
}

BENodeP BEOverloadedPushNode (int arity,BESymbolP symbol,BEArgP arguments,BENodeIdListP nodeIds,BENodeP decons_node)
{
	NodeP	push_node;

	push_node	= ConvertAllocType (NodeS);

	push_node->node_kind		= PushNode;
	push_node->node_arity		= arity;
	push_node->node_arguments	= arguments;
	push_node->node_push_symbol = symbol;
	push_node->node_decons_node = decons_node;
	push_node->node_node_ids	= nodeIds;
	push_node->node_number		= 0;

	Assert (arguments->arg_node->node_kind == NodeIdNode);
	Assert (arguments->arg_node->node_node_id->nid_ref_count_sign == -1);
		
	arguments->arg_node->node_node_id->nid_refcount++;
	
	return push_node;
}
#endif

void
BEPredefineConstructorSymbol (int arity, int constructorIndex, int moduleIndex, BESymbKind symbolKind)
{
	BEModuleP	module;

	Assert (moduleIndex == kPredefinedModuleIndex);

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) constructorIndex < module->bem_nConstructors);
	Assert (module->bem_constructors [constructorIndex]->symb_kind == erroneous_symb);

	module->bem_constructors [constructorIndex]->symb_kind	= symbolKind;
	module->bem_constructors [constructorIndex]->symb_arity	= arity;
} /* BEPredefineConstructorSymbol */

void
BEPredefineTypeSymbol (int arity, int typeIndex, int moduleIndex, BESymbKind symbolKind)
{
	BEModuleP	module;

	Assert (moduleIndex == kPredefinedModuleIndex);

	Assert ((unsigned int) moduleIndex < gBEState.be_nModules);
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) typeIndex < module->bem_nTypes);
	Assert (module->bem_types [typeIndex]->symb_kind == erroneous_symb);

	module->bem_types [typeIndex]->symb_kind		= symbolKind;
	module->bem_types [typeIndex]->symb_arity	= arity;
} /* BEPredefineTypeSymbol */

BESymbolP
BEBasicSymbol (BESymbKind kind)
{
	Assert (gBasicSymbols [kind] != NULL);

	return (gBasicSymbols [kind]);
} /* BEBasicSymbol */

BETypeNodeP
BEVarTypeNode (CleanString name)
{
	TypeNode	node;

	node	= ConvertAllocType (struct type_node);

	node->type_node_is_var		= True;
	node->type_node_tv			= BETypeVar (name);
	node->type_node_arity		= 0;
	node->type_node_annotation	= NoAnnot;
	node->type_node_attribute	= NoUniAttr;
	node->type_for_all_vars		= NULL;

	return (node);
} /* BEVarTypeNode */

BETypeNodeP
BENumberedVarTypeNode (CleanString name,int argument_n)
{
	TypeNode	node;

	node	= ConvertAllocType (struct type_node);

	node->type_node_is_var		= True;
	node->type_node_tv			= BENumberedTypeVar (name,argument_n);
	node->type_node_arity		= 0;
	node->type_node_annotation	= NoAnnot;
	node->type_node_attribute	= NoUniAttr;
	node->type_for_all_vars		= NULL;

	return node;
}

BETypeNodeP
BENormalTypeNode (BESymbolP symbol, BETypeArgP args)
{
	TypeNode	node;

	node	= ConvertAllocType (struct type_node);

	node->type_node_is_var		= False;
	node->type_node_arity		= CountTypeArgs (args);
	node->type_node_annotation	= NoAnnot;
	node->type_node_attribute	= NoUniAttr;
	node->type_node_symbol		= symbol;
	node->type_node_arguments	= args;
	node->type_for_all_vars		= NULL;

	return (node);
} /* BENormalTypeNode */


BETypeNodeP
BEAttributeTypeNode (BEAttribution attribution, BETypeNodeP typeNode)
{
	Assert (typeNode->type_node_attribute == NoUniAttr);
	typeNode->type_node_attribute	= (AttributeKind) attribution;

	return (typeNode);
} /* BEAttributeTypeNode */

BEAttributeKindList
BEAttributeKind (BEAttribution attributeKind)
{
	AttributeKindList	new;

	new	= ConvertAllocType (struct attr_kind_list);

	new->akl_elem	= attributeKind;
	new->akl_next	= NULL;

	return (new);
} /* BEAttributeKind */

BEAttributeKindList
BENoAttributeKinds (void)
{
	return (NULL);
} /* BENoAttributeKinds */

BEAttributeKindList
BEAttributeKinds (BEAttributeKindList elem, BEAttributeKindList list)
{
	Assert (elem->akl_next == NULL);
	elem->akl_next	= list;

	return (elem);
} /* BEAttributeKindList */

BEUniVarEquations
BEUniVarEquation (BEAttribution demanded, BEAttributeKindList offered)
{
	UniVarEquations	new;

	new	= ConvertAllocType (struct uni_var_equats);

	new->uve_demanded	= demanded;
	new->uve_offered	= offered;
	new->uve_next		= NULL;

	return (new);
} /* BEUniVarEquation */

BEUniVarEquations
BENoUniVarEquations (void)
{
	return (NULL);
} /* BENoUniVarEquations */

BEUniVarEquations
BEUniVarEquationsList (BEUniVarEquations elem, BEUniVarEquations list)
{
	Assert (elem->uve_next == NULL);
	elem->uve_next	= list;

	return (elem);
} /* BEUniVarEquations */

BETypeNodeP
BEAnnotateTypeNode (BEAnnotation annotation, BETypeNodeP typeNode)
{
	Assert (typeNode->type_node_annotation == NoAnnot);
	typeNode->type_node_annotation	= (Annotation) annotation;

	return (typeNode);
} /* BEAnnotateTypeNode */

BETypeNodeP
BEAddForAllTypeVariables (BETypeVarListP vars, BETypeNodeP type)
{
	Assert (type->type_for_all_vars == NULL);
	type->type_for_all_vars		= vars;

	return (type);
} /* BEAddForAllTypeVariables */

BETypeArgP
BENoTypeArgs (void)
{
	return (NULL);
} /* BENoTypeArgs */

BETypeArgP
BETypeArgs (BETypeNodeP node, BETypeArgP nextArgs)
{
	TypeArgs	arg;

	arg	= ConvertAllocType (TypeArg);

	arg->type_arg_node	= node;
	arg->type_arg_next	= nextArgs;

	return (arg);
} /* BETypeArgs */

BETypeAltP
BETypeAlt (BETypeNodeP lhs, BETypeNodeP rhs, BEUniVarEquations attributeEquations)
{
	TypeAlt	*alt;

	alt	= ConvertAllocType (struct type_alt);

	alt->type_alt_lhs	= lhs;
	alt->type_alt_rhs	= rhs;

	alt->type_alt_attr_equations	= attributeEquations; /* used in PrintType */
	alt->type_alt_strict_positions	= NULL;

	return (alt);
} /* BETypeAlt */

static Node
GenerateApplyNodes (Node root, int offarity, int demarity)
{
	if (offarity > demarity)
	{
		int		i;
		Args	lastarg, nextarg;
		
		if (demarity != 0)
		{
			for (i=1, lastarg = root->node_arguments; i < demarity; i++)
				lastarg = lastarg->arg_next;
		
			nextarg = lastarg->arg_next;
			lastarg->arg_next = NULL;
		}
		else
		{
			nextarg = root->node_arguments;
			root->node_arguments = NULL;
		}
		root->node_arity = (short) demarity;
		
		for (i=demarity+1; i<=offarity; i++)
		{
			Args	arg;

			arg	= BEArgs (root, nextarg);

   			nextarg	= nextarg->arg_next;
			arg->arg_next->arg_next = NULL;

			root	= BENormalNode (gBasicSymbols [apply_symb], arg);
		}
	}

	return (root);
} /* GenerateApplyNodes */

BENodeP
BENormalNode (BESymbolP symbol, BEArgP args)
{
	NodeP	node;

	node	= ConvertAllocType (NodeS);

	node->node_annotation	= NoAnnot;
	node->node_kind			= NormalNode;
	node->node_symbol		= symbol;
	node->node_arity		= CountArgs (args);
	node->node_arguments	= args;
	node->node_number=0;

	/* +++ hackerdiehack */
	if (symbol->symb_kind == definition)
		node	= GenerateApplyNodes (node, node->node_arity, symbol->symb_def->sdef_arity);

	return (node);
} /* BENormalNode */

BENodeP
BEMatchNode (int arity, BESymbolP symbol, BENodeP node)
{
	NodeP	matchNode;

	if (symbol->symb_kind == tuple_symb)
		return (node);

	matchNode	= ConvertAllocType (NodeS);

	matchNode->node_annotation	= NoAnnot;
	matchNode->node_kind		= MatchNode;
	matchNode->node_symbol		= symbol;
	matchNode->node_arity		= arity;
	matchNode->node_arguments	= BEArgs (node, NULL);
	matchNode->node_number=0;

	return (matchNode);
} /* BEMatchNode */

BENodeP
BETupleSelectNode (int arity, int index, BENodeP node)
{
	SymbolP symbol;
	NodeP	select;

	if ((symbol = gTupleSelectSymbols [arity-1]) == NULL)
	{
		symbol	= ConvertAllocType (SymbolS);
	
		symbol->symb_kind	= select_symb;
		symbol->symb_arity	= arity;

		gTupleSelectSymbols [arity-1]	= symbol;
	}

	select	= ConvertAllocType (NodeS);

	select->node_annotation	= NoAnnot;
	select->node_kind		= NormalNode;
	select->node_symbol		= symbol;
	select->node_arity		= index+1;
	select->node_arguments	= BEArgs (node, NULL);
	select->node_number		= 0;

	return (select);
} /* BETupleSelectNode */

BENodeP
BEIfNode (BENodeP cond, BENodeP then, BENodeP elsje)
{
	NodeP	node;

	node	= ConvertAllocType (NodeS);

	node->node_annotation	= NoAnnot;
	node->node_kind			= NormalNode;
	node->node_symbol		= gBasicSymbols [if_symb];
	node->node_arguments	= BEArgs (cond, BEArgs (then, BEArgs (elsje, NULL)));
	node->node_arity		= 3;
	node->node_number		= 0;

	return (node);
} /* BEIfNode */

BENodeP
BEGuardNode (BENodeP cond, BENodeDefP thenNodeDefs, BEStrictNodeIdP thenStricts, BENodeP then, BENodeDefP elseNodeDefs, BEStrictNodeIdP elseStricts, BENodeP elsje)
{
	NodeP	node;
	struct if_node_contents *thenElseInfo;

	thenElseInfo = ConvertAllocType (struct if_node_contents);

	thenElseInfo->if_then_node_defs			= thenNodeDefs;
	thenElseInfo->if_then_strict_node_ids	= thenStricts;
	thenElseInfo->if_else_node_defs			= elseNodeDefs;
	thenElseInfo->if_else_strict_node_ids	= elseStricts;

	node	= ConvertAllocType (NodeS);

	node->node_annotation			= NoAnnot;
	node->node_kind					= IfNode;
	node->node_contents.contents_if	= thenElseInfo;
	node->node_arguments			= BEArgs (cond, BEArgs (then, BEArgs (elsje, NULL)));
	node->node_number				= 0;

	switch (elsje->node_kind)
	{
		case SwitchNode:
			thenElseInfo->if_else_node_defs			= NULL;
			thenElseInfo->if_else_strict_node_ids	= NULL;
			node->node_arguments->arg_next->arg_next->arg_node
								= BENormalNode (BEBasicSymbol (BEFailSymb), BENoArgs ());

			node	= NewGuardNode (node, elsje, elseNodeDefs, elseStricts);
			break;
		case GuardNode:
			/* move the GuardNode to the top */
			node->node_arguments->arg_next->arg_next->arg_node
								= elsje->node_arguments->arg_node;
			elsje->node_arguments->arg_node	=	node;
			node	= elsje;
			break;
		default:
			break;
	}

	return (node);
} /* BEGuardNode */

static NodeIdRefCountListP
NewRefCount (NodeIdRefCountListP next, NodeIdP nodeId, int ref_count)
{
	NodeIdRefCountListP	newRefCount;

	newRefCount	= ConvertAllocType (NodeIdRefCountListS);

	newRefCount->nrcl_next		= next;
	newRefCount->nrcl_node_id	= nodeId;
	newRefCount->nrcl_ref_count	= ref_count;

	return (newRefCount);
} /* NewRefCount */

/* +++ dynamic allocation */
# define kMaxScope 1000
static int gCurrentScope = 0;
static	NodeIdRefCountListP	gRefCountLists [kMaxScope];
static	NodeIdRefCountListP	gRefCountList;

static void
AddRefCount (NodeIdP nodeId)
{
	gRefCountList	= NewRefCount (gRefCountList, nodeId, 0);
} /* AddRefCount */

void
BESetNodeDefRefCounts (BENodeP lhs)
{
	ArgP	arg;
	
	gRefCountList	= NULL;

	Assert (lhs->node_kind == NormalNode);

	for (arg = lhs->node_arguments; arg != NULL; arg = arg->arg_next)
	{
		NodeP	node;
		NodeIdP	nodeId;

		node	= arg->arg_node;

		Assert (node->node_kind == NodeIdNode);
		Assert (node->node_arguments == NULL);

		nodeId	= node->node_node_id;

		nodeId->nid_mark |= NID_ALIAS_MARK_MASK;
		AddRefCount (nodeId);
	}
} /* BESetNodeDefRefCounts */

void
BEAddNodeIdsRefCounts (int sequenceNumber, BESymbolP symbol, BENodeIdListP nodeIds)
{
	NodeIdP	nodeId;

	nodeId	= gCurrentNodeIds [sequenceNumber];

	Assert (nodeId != NULL);
	if (nodeId->nid_mark & NID_ALIAS_MASK)
	{
		nodeId	= nodeId->nid_forward_node_id;
		Assert (nodeId != NULL);
	}

	Assert (nodeId != NULL);

	if ((nodeId->nid_mark & NID_ALIAS_MARK_MASK)
			&&
				(symbol->symb_kind == tuple_symb
			 	|| (symbol->symb_kind == definition && symbol->symb_def->sdef_kind == RECORDTYPE)))
	{
		NodeP				node;
		ArgP				arg, args;
		NodeIdListElement	nodeIdList;

		node	= nodeId->nid_node;

		if (node == NULL)
		{
			NodeIdP	nid;

			nodeIdList	= nodeIds;

			nid	= nodeIdList->nidl_node_id;

			nid->nid_mark |= NID_ALIAS_MARK_MASK;

			args	= BEArgs (BENodeIdNode (nid, NULL), NULL);;

			for (nodeIdList = nodeIdList->nidl_next, arg = args;
					nodeIdList != NULL;
					nodeIdList = nodeIdList->nidl_next, arg	= arg->arg_next)
			{
				nid	= nodeIdList->nidl_node_id;

				nid->nid_mark |= NID_ALIAS_MARK_MASK;
				arg->arg_next	= BEArgs (BENodeIdNode (nid, NULL), NULL);
			}

			nodeId->nid_node	= BENormalNode (symbol, args);
		}
		else
		{	
			Assert (node->node_symbol == symbol);

			arg	= node->node_arguments;
			for (nodeIdList = nodeIds; nodeIdList != NULL && arg != NULL;
					nodeIdList = nodeIdList->nidl_next, arg = arg->arg_next)
			{
				Assert (arg->arg_node->node_kind == NodeIdNode);

				nodeId	= nodeIdList->nidl_node_id;

				nodeId->nid_mark |= NID_ALIAS_MASK;
				nodeId->nid_forward_node_id	= arg->arg_node->node_node_id;
				nodeIdList->nidl_node_id	= nodeId->nid_forward_node_id;
			}

			Assert (nodeIdList == NULL && arg == NULL);
		}
	}

	for (; nodeIds; nodeIds=nodeIds->nidl_next)
	{
		NodeIdP	nodeId;
		
		nodeId	= nodeIds->nidl_node_id;

		Assert (nodeId != NULL);
		if (nodeId->nid_mark & NID_ALIAS_MASK)
		{
			nodeId	= nodeId->nid_forward_node_id;
			Assert (nodeId != NULL);
		}

		Assert (nodeId != NULL);
		AddRefCount (nodeId);
		Assert (nodeId->nid_ref_count_sign == -1);
	}
} /* BEAddNodeIdsRefCounts */

static NodeIdRefCountListP
CopyRefCountList (NodeIdRefCountListP refCount)
{
	NodeIdRefCountListP	first, copy;

	first	= NULL;
	copy	= NULL;
	for (; refCount != NULL; refCount = refCount->nrcl_next)
	{
		NodeIdP				nodeId;
		NodeIdRefCountListP	new;

		nodeId	= refCount->nrcl_node_id;

		Assert (nodeId->nid_ref_count_sign == -1);

		new	= NewRefCount (NULL, nodeId, refCount->nrcl_ref_count);

		if (copy == NULL)
			first	= new;
		else
			copy->nrcl_next	=	new;

		copy	=	new;
	}

	return (first);
} /* CopyRefCountList */

BENodeP
BESwitchNode (BENodeIdP nodeId, BEArgP cases)
{
	NodeP	switchNode;

	switchNode	= ConvertAllocType (NodeS);
	
	switchNode->node_kind		= SwitchNode;
	switchNode->node_node_id	= nodeId;
	switchNode->node_arity		= 1;
	switchNode->node_arguments	= cases;
	switchNode->node_annotation	= NoAnnot;

	Assert (nodeId->nid_ref_count_sign == -1);
	
	return (switchNode);
} /* BESwitchNode */

void
BEEnterLocalScope (void)
{
	NodeIdRefCountListP	refCount;

	gRefCountList	= CopyRefCountList (gRefCountList);

	for (refCount = gRefCountList; refCount != NULL; refCount = refCount->nrcl_next)
	{
		NodeIdP	nodeId;

		nodeId	= refCount->nrcl_node_id;

		Assert (nodeId->nid_ref_count_sign == -1);

		refCount->nrcl_ref_count	= nodeId->nid_refcount;
		nodeId->nid_refcount		= -1;
	}

	Assert (gCurrentScope < kMaxScope);
	gRefCountLists [gCurrentScope++]	= gRefCountList;
} /* BEEnterLocalScope */

void
BELeaveLocalScope (BENodeP node)
{
	NodeIdRefCountListP	refCount;

	Assert (gCurrentScope > 0);
	gRefCountList	= gRefCountLists [--gCurrentScope];

	for (refCount = gRefCountList; refCount != NULL; refCount = refCount->nrcl_next)
	{
		NodeIdP	nodeId;
		int		count;

		nodeId	= refCount->nrcl_node_id;

		Assert (nodeId->nid_ref_count_sign == -1);

		count	= refCount->nrcl_ref_count;
		refCount->nrcl_ref_count	= nodeId->nid_refcount;
		nodeId->nid_refcount	+= count + 1;
	}

	Assert (node->node_kind == CaseNode || node->node_kind == DefaultNode);
	node->node_node_id_ref_counts	= gRefCountList;
} /* BELeaveLocalScope */

BENodeP
BECaseNode (int symbolArity, BESymbolP symbol, BENodeDefP nodeDefs, BEStrictNodeIdP strictNodeIds, BENodeP node)
{
	NodeP	caseNode;
	
	caseNode	= ConvertAllocType (NodeS);

	caseNode->node_kind			= CaseNode;
	caseNode->node_symbol		= symbol;
	caseNode->node_arity		= symbolArity;
	caseNode->node_node_defs	= nodeDefs;
	caseNode->node_arguments	= NewArgument (node);

	caseNode->node_su.su_u.u_case		= ConvertAllocType (CaseNodeContentsS);
	caseNode->node_node_id_ref_counts	= NULL;
	caseNode->node_strict_node_ids	= strictNodeIds;

	return (caseNode);
} /* BECaseNode */

BENodeP BEOverloadedCaseNode (BENodeP case_node,BENodeP equal_node,BENodeP from_integer_node)
{
	NodeP overloaded_case_node;
	ArgP equal_arg,from_integer_arg;
	
	overloaded_case_node = ConvertAllocType (NodeS);
	overloaded_case_node->node_kind = OverloadedCaseNode;

	overloaded_case_node->node_node = case_node;

	equal_arg = ConvertAllocType (ArgS);
	equal_arg->arg_node = equal_node;

	from_integer_arg = ConvertAllocType (ArgS);
	from_integer_arg->arg_node = from_integer_node;

	from_integer_arg->arg_next = NULL;
	equal_arg->arg_next = from_integer_arg;
	overloaded_case_node->node_arguments = equal_arg;

	return overloaded_case_node;
}

BENodeP
BEDefaultNode (BENodeDefP nodeDefs, BEStrictNodeIdP strictNodeIds, BENodeP node)
{
	NodeP	defaultNode;
	
	defaultNode	= ConvertAllocType (NodeS);

	defaultNode->node_kind		= DefaultNode;
	defaultNode->node_node_defs	= nodeDefs;
	defaultNode->node_arity		= 1;
	defaultNode->node_arguments	= NewArgument (node);

	defaultNode->node_su.su_u.u_case		= ConvertAllocType (CaseNodeContentsS);
	defaultNode->node_strict_node_ids	= strictNodeIds;
	defaultNode->node_node_id_ref_counts	= NULL;
	
	return (defaultNode);
} /* BEDefaultNode */

BENodeP
BEPushNode (int arity, BESymbolP symbol, BEArgP arguments, BENodeIdListP nodeIds)
{
	NodeP	pushNode;

	pushNode	= ConvertAllocType (NodeS);

	pushNode->node_kind			= PushNode;
	pushNode->node_arity		= arity;
	pushNode->node_arguments	= arguments;
#if STRICT_LISTS
	pushNode->node_push_symbol = symbol;
#else
	pushNode->node_record_symbol= symbol;
#endif
	pushNode->node_node_ids		= nodeIds;
	pushNode->node_number		= 0;
/*
	for (; nodeIds; nodeIds=nodeIds->nidl_next)
	{
		NodeIdP	nodeId;
		
		nodeId	= nodeIds->nidl_node_id;

		Assert (nodeId->nid_ref_count_sign == -1);

		nodeId->nid_refcount++;
	}
*/
	Assert (arguments->arg_node->node_kind == NodeIdNode);
	Assert (arguments->arg_node->node_node_id->nid_ref_count_sign == -1);

	arguments->arg_node->node_node_id->nid_refcount++;
	
	return (pushNode);
} /* BEPushNode */

BENodeP
BESelectorNode (BESelectorKind selectorKind, BESymbolP fieldSymbol, BEArgP args)
{
	NodeP	node;

	Assert (CountArgs (args) == 1);

	node	= ConvertAllocType (NodeS);

	node->node_annotation	= NoAnnot;
	node->node_kind			= SelectorNode;
	node->node_symbol		= fieldSymbol;
	node->node_arity		= selectorKind;
	node->node_arguments	= args;
	node->node_number		= 0;

	return (node);
} /* BESelectorNode */

BENodeP
BEUpdateNode (BEArgP args)
{
	NodeP	node;
	SymbolP	recordSymbol;
	int n_args;
	
	n_args = CountArgs (args);

	Assert (n_args >= 2);
	Assert (args->arg_next->arg_node->node_kind == SelectorNode);
	Assert (args->arg_next->arg_node->node_arity == BESelector);

	recordSymbol	= args->arg_next->arg_node->node_symbol->symb_def->sdef_type->type_lhs->ft_symbol;

	node	= ConvertAllocType (NodeS);

	node->node_annotation	= NoAnnot;
	node->node_kind			= UpdateNode;
	node->node_symbol		= recordSymbol;
	node->node_arity		= n_args;
	node->node_arguments	= args;
	node->node_number=0;

	return (node);
} /* BEUpdateNode */

BENodeP
BENodeIdNode (BENodeIdP nodeId, BEArgP args)
{
	NodeP	node;

	node	= ConvertAllocType (NodeS);

	node->node_annotation	= NoAnnot;
	node->node_kind			= NodeIdNode;
	node->node_node_id		= nodeId;
	node->node_arity		= CountArgs (args);
	node->node_arguments	= args;
	node->node_number		= 0;

	return (node);
} /* BENodeIdNode */

BEArgP
BENoArgs (void)
{
	return (NULL);
} /* BENoArgs */

BEArgP
BEArgs (BENodeP node, BEArgP nextArgs)
{
	ArgP	arg;

	arg	= ConvertAllocType (ArgS);

	arg->arg_node	= node;
	arg->arg_next	= nextArgs;

	return (arg);
} /* BEArgs */

static void increase_number_of_node_ids (int sequenceNumber)
{
	if (n_gCurrentNodeIds==0){
		gCurrentNodeIds = malloc (1000*sizeof (NodeId *));
		if (gCurrentNodeIds!=NULL)
			n_gCurrentNodeIds = 1000;
	}
	
	while (sequenceNumber>=n_gCurrentNodeIds){
		static NodeIdP (*new_gCurrentNodeIds);

		new_gCurrentNodeIds = realloc (gCurrentNodeIds,(n_gCurrentNodeIds+(n_gCurrentNodeIds>>1)) * sizeof (NodeId *));
		if (new_gCurrentNodeIds==NULL)
			return;
		
		gCurrentNodeIds = new_gCurrentNodeIds;
		n_gCurrentNodeIds += (n_gCurrentNodeIds>>1);
	}
}

void
BEDeclareNodeId (int sequenceNumber, int lhsOrRhs, CleanString name)
{
	IdentP	newIdent;
	NodeIdP	newNodeId;

	if (sequenceNumber>=n_gCurrentNodeIds)
		increase_number_of_node_ids (sequenceNumber);

	/* +++ ifdef DEBUG */
	if (sequenceNumber>=number_of_node_ids){
		int i;
		
		for (i=number_of_node_ids; i<=sequenceNumber; ++i)
			gCurrentNodeIds[i] = NULL;
		
		number_of_node_ids=sequenceNumber+1;
	}
	/* endif DEBUG */

	Assert (gCurrentNodeIds [sequenceNumber] == NULL);

	/* +++ share idents ??? */
	newIdent	= ConvertAllocType (IdentS);
	newIdent->ident_name	= ConvertCleanString (name);

	newNodeId	= ConvertAllocType (NodeIdS);
	newNodeId->nid_ident	= newIdent;

	newNodeId->nid_node_def			= NULL;
	newNodeId->nid_forward_node_id	= NULL;
	newNodeId->nid_node				= NULL;
	newNodeId->nid_state.state_kind	= 0;
	newNodeId->nid_mark				= 0;
	newNodeId->nid_mark2			= 0;
	newNodeId->nid_ref_count_sign	= lhsOrRhs==BELhsNodeId ? -1 : 1;
	newNodeId->nid_refcount			= 0;
	newNodeId->nid_ref_count_copy	= 0;

	gCurrentNodeIds [sequenceNumber]	= newNodeId;
} /* BEDeclareNodeId */

BENodeIdP
BENodeId (int sequenceNumber)
{
	NodeIdP		nodeId;

	/* +++ ifdef DEBUG */
	if (sequenceNumber>=number_of_node_ids){
		int i;
		
		for (i=number_of_node_ids; i<=sequenceNumber; ++i)
			gCurrentNodeIds[i] = NULL;
		
		number_of_node_ids=sequenceNumber+1;
	}
	/* endif DEBUG */

	nodeId	= gCurrentNodeIds [sequenceNumber];

	Assert (nodeId != NULL);

	if (nodeId->nid_mark & NID_ALIAS_MASK)
	{
		nodeId	= nodeId->nid_forward_node_id;
		Assert (nodeId != NULL);
	}

	Assert (nodeId != NULL);

	nodeId->nid_refcount	+= nodeId->nid_ref_count_sign;

	return (nodeId);
} /* BENodeId */

BENodeIdP
BEWildCardNodeId (void)
{
	NodeIdP	newNodeId;

	/* +++ share wild card nodeids ??? */

	newNodeId	= ConvertAllocType (NodeIdS);

	newNodeId->nid_ident			= NULL;
	newNodeId->nid_node_def			= NULL;
	newNodeId->nid_forward_node_id	= NULL;
	newNodeId->nid_node				= NULL;
	newNodeId->nid_state.state_kind	= 0;
	newNodeId->nid_mark				= 0;
	newNodeId->nid_mark2			= 0;
	newNodeId->nid_ref_count_sign	= -1;
	newNodeId->nid_refcount			= -1;

	return (newNodeId);
} /* BEWildCardNodeId */

BENodeDefP
BENodeDef (int sequenceNumber, BENodeP node)
{
	NodeIdP		nodeId;
	NodeDefP	nodeDef;

	/* +++ ifdef DEBUG */
	if (sequenceNumber>=number_of_node_ids){
		int i;
		
		for (i=number_of_node_ids; i<=sequenceNumber; ++i)
			gCurrentNodeIds[i] = NULL;
		
		number_of_node_ids=sequenceNumber+1;
	}
	/* endif DEBUG */

	nodeDef	=	ConvertAllocType (NodeDefS);

	nodeId	= gCurrentNodeIds [sequenceNumber];

	Assert (nodeId != NULL);
	Assert (nodeId->nid_node == NULL);
	nodeId->nid_node_def	= nodeDef;
	nodeId->nid_node		= node;

	nodeDef->def_id		= nodeId;
	nodeDef->def_node	= node;
	/* ifdef DEBUG */
	nodeDef->def_next	= NULL;
	/* endif DEBUG */

	return (nodeDef);
} /* BENodeDef */

BENodeDefP
BENodeDefs (BENodeDefP nodeDef, BENodeDefP nodeDefs)
{
	Assert (nodeDef->def_next == NULL);

	nodeDef->def_next	= nodeDefs;

	return (nodeDef);
} /* BENodeDefs */

BENodeDefP
BENoNodeDefs (void)
{
	return (NULL);
} /* BENoNodeDefs */

BEStrictNodeIdP
BEStrictNodeId (BENodeIdP nodeId)
{
	StrictNodeId	strictNodeId;

	strictNodeId	=	ConvertAllocType (struct strict_node_id);
	strictNodeId->snid_node_id	= nodeId;

	/* +++ remove this hack */
	nodeId->nid_refcount--;

	/* ifdef DEBUG */
	strictNodeId->snid_next	= NULL;
	/* endif */

	return (strictNodeId);
} /* BEStrictNodeId */

BEStrictNodeIdP
BEStrictNodeIds (BEStrictNodeIdP strictNodeId, BEStrictNodeIdP strictNodeIds)
{
	Assert (strictNodeId->snid_next == NULL);

	strictNodeId->snid_next	= strictNodeIds;

	return (strictNodeId);
} /* BEStrictNodeIds */

BEStrictNodeIdP
BENoStrictNodeIds (void)
{
	return (NULL);
} /* BENoStrictNodeIds */

static NodeDefP
CollectNodeNodeDefs (NodeP node, NodeDefP defs)
{
	switch (node->node_kind)
	{
		case NormalNode:
			{
				ArgP	arg;

				for (arg = node->node_arguments; arg != NULL; arg = arg->arg_next)
					defs	= CollectNodeNodeDefs (arg->arg_node, defs);
			}
			break;
		case NodeIdNode:
			{
				NodeIdP	nodeId;
			
				nodeId	= node->node_node_id;
				if (nodeId->nid_node != NULL && !(nodeId->nid_mark & NID_VERIFY_MASK))
				{
					NodeDefP	nodeDef;

					nodeId->nid_mark |= NID_VERIFY_MASK;

					nodeDef	=	ConvertAllocType (NodeDefS);

					nodeDef->def_id		= nodeId;
					nodeDef->def_node	= nodeId->nid_node;
					nodeDef->def_next	= defs;

					nodeId->nid_node_def			= nodeDef;

					defs	= CollectNodeNodeDefs (nodeId->nid_node, nodeDef);
				}
			}
			break;
		default:
			break;
	}

	return (defs);
} /* CollectNodeNodeDefs */

static NodeDefP
CollectNodeDefs (NodeP node, NodeDefP defs)
{
	NodeDefP	def;

	for (def = defs; def != NULL; def = def->def_next)
		def->def_id->nid_mark |= NID_VERIFY_MASK;

	defs	= CollectNodeNodeDefs (node, defs);

	for (def = defs; def != NULL; def = def->def_next)
		def->def_id->nid_mark &= ~NID_VERIFY_MASK;

	return (defs);
} /* CollectNodeDefs */

BERuleAltP
BERuleAlt (int line, BENodeDefP lhsDefs, BENodeP lhs, BENodeDefP rhsDefs, BEStrictNodeIdP rhsStrictNodeIds, BENodeP rhs)
{
	RuleAltP	alt;

	alt	= ConvertAllocType (RuleAltS);

	alt->alt_lhs_root			= lhs;
	alt->alt_lhs_defs 			= CollectNodeDefs (lhs, lhsDefs);
	alt->alt_rhs_root			= rhs;
	alt->alt_rhs_defs 			= rhsDefs;
	alt->alt_line				= line<0 ? 0 : line;
	alt->alt_kind				= Contractum;
	alt->alt_strict_node_ids	= rhsStrictNodeIds;

	/* +++ ifdef DEBUG */
	alt->alt_next				= NULL;
	number_of_node_ids=0;
	/* endif DEBUG */

	set_scope_numbers (alt);

	return (alt);
} /* BERuleAlt */

BERuleAltP
BECodeAlt (int line, BENodeDefP lhsDefs, BENodeP lhs, BECodeBlockP codeBlock)
{
	RuleAltP	alt;

	alt	= ConvertAllocType (RuleAltS);

	alt->alt_lhs_root			= lhs;
	alt->alt_lhs_defs 			= CollectNodeDefs (lhs, lhsDefs);
	alt->alt_rhs_code			= codeBlock;
	alt->alt_rhs_defs 			= NULL;
	alt->alt_line				= line<0 ? 0 : line;
	alt->alt_kind				= ExternalCall;
	alt->alt_strict_node_ids	= NULL;

	/* +++ ifdef DEBUG */
	alt->alt_next				= NULL;
	number_of_node_ids=0;
	/* endif DEBUG */

# ifdef CODE_INLINE_FLAG
	/* RWS +++ move to code generator ??? */
	if (codeBlock->co_is_abc_code && codeBlock->co_is_inline)
	{
		char			*functionName, *instructionLine;
		Instructions	instruction;

		Assert (lhs->node_kind == NormalNode);
		Assert (lhs->node_symbol->symb_kind == definition);
		functionName	= lhs->node_symbol->symb_def->sdef_ident->ident_name;

		/* .inline <name> */
		instructionLine	= ConvertAlloc (sizeof (".inline ") + strlen (functionName));
		strcpy (instructionLine, ".inline ");
		strcat (instructionLine, functionName);

		instruction	= ConvertAllocType (Instruction);
		instruction->instr_this	= instructionLine;
		instruction->instr_next	= codeBlock->co_instr;
		codeBlock->co_instr	= instruction;

		for (; instruction->instr_next != NULL; instruction = instruction->instr_next)
			/* find last element */;
		instruction	= instruction->instr_next	= ConvertAllocType (Instruction);

		instruction->instr_this	= ".end";
		instruction->instr_next	= NULL;
	}
# endif

	return (alt);
} /* BECodeAlt */


BERuleAltP
BERuleAlts (BERuleAltP alt, BERuleAltP alts)
{
	Assert (alt->alt_next == NULL);

	alt->alt_next	= alts;

	return (alt);
} /* BERuleAlts*/

BERuleAltP
BENoRuleAlts (void)
{
	return (NULL);
} /* BENoRuleAlts */

static void
DeclareFunctionC (char *name, int arity, int functionIndex, unsigned int ancestor)
{
	SymbDefP	newSymbDef;
	Ident		newIdent;
	SymbolP	 	functions;
	BEIcl		icl;
	BEModule	module;

	icl	= &gBEState.be_icl;

	module	= &gBEState.be_modules [main_dcl_module_n];
	functions	=	module->bem_functions;
	Assert (functions != NULL);

	Assert (icl->beicl_previousAncestor <= ancestor);
	icl->beicl_previousAncestor	= ancestor;

	Assert (functionIndex < module->bem_nFunctions);
	newSymbDef	= ConvertAllocType (SymbDefS);

	newSymbDef->sdef_kind		= IMPRULE;
	newSymbDef->sdef_mark		= 0;
	newSymbDef->sdef_arity		= arity;
	newSymbDef->sdef_module		= module->bem_name;
	newSymbDef->sdef_ancestor	= ancestor;
	newSymbDef->sdef_arfun		= NoArrayFun;
	newSymbDef->sdef_next_scc	= NULL;
	newSymbDef->sdef_exported	= False;
	newSymbDef->sdef_dcl_icl	= NULL;
	newSymbDef->sdef_isused		= 0;

	*icl->beicl_depsP	= newSymbDef;
	icl->beicl_depsP	= &newSymbDef->sdef_next_scc;
	newSymbDef->sdef_arfun		= NoArrayFun;

	newIdent	= ConvertAllocType (IdentS);

	newIdent->ident_name	= name;
	newIdent->ident_symbol	= &functions [functionIndex];

	newSymbDef->sdef_ident	= newIdent;

	Assert (functions [functionIndex].symb_kind == erroneous_symb);
	functions [functionIndex].symb_kind	= definition;
	functions [functionIndex].symb_def	= newSymbDef;


	/* +++ ugly */
	if (strcmp (newIdent->ident_name, "Start") == 0)
	{
		Assert (icl->beicl_module->im_start == NULL);
		icl->beicl_module->im_start	= newSymbDef;
	}
} /* DeclareFunctionC */

void BEStartFunction (int functionIndex)
{
	gBEState.be_icl.beicl_module->im_start
		= gBEState.be_modules[main_dcl_module_n].bem_functions[functionIndex].symb_def;
}

void
BEDeclareFunction (CleanString name, int arity, int functionIndex, int ancestor)
{
	/* +++ ugly */
	if (strncmp (name->chars, "Start;", 6) == 0 && isdigit (name->chars[6]))
		name->length	= 5;

	DeclareFunctionC (ConvertCleanString (name), arity, functionIndex, ancestor);
} /* BEDeclareFunction */

BEImpRuleP
BERules (BEImpRuleP rule, BEImpRuleP rules)
{
	Assert (rule->rule_next == NULL);

	rule->rule_next	= rules;

	return (rule);
} /* BERules */

BEImpRuleP
BENoRules (void)
{
	return (NULL);
} /* BENoRules */

BEImpRuleP
BERule (int functionIndex, int isCaf, BETypeAltP type, BERuleAltP alts)
{
	SymbDefP	functionDef;
	SymbolP		functionSymbol;
	ImpRuleP	rule;
	BEModule	module;

	rule	= ConvertAllocType (ImpRuleS);

	module	= &gBEState.be_modules [main_dcl_module_n];
	functionSymbol	= &module->bem_functions [functionIndex];
	functionDef	= functionSymbol->symb_def;
	functionDef->sdef_rule	= rule;

	rule->rule_type	= type;
	rule->rule_alts	= alts;
	rule->rule_mark	= isCaf ? RULE_CAF_MASK : 0;

	rule->rule_root	= alts->alt_lhs_root;

	/* ifdef DEBUG */
	rule->rule_next	= NULL;
	/* endif DEBUG */

	return (rule);
} /* BERule */

void
BEDeclareRuleType (int functionIndex, int moduleIndex, CleanString name)
{
	IdentP		newIdent;
	SymbDefP	newSymbDef;
	SymbolP		functions;
	BEModuleP	module;

	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) functionIndex < module->bem_nFunctions);

	functions	= module->bem_functions;

	Assert (functions != NULL);
	Assert (functions [functionIndex].symb_kind == erroneous_symb);

	if (module->bem_isSystemModule)
		/* for inline code */
		newIdent	= PutStringInHashTable (ConvertCleanString (name), FirstSystemModuleTable + moduleIndex);
	else
	{
		newIdent	= ConvertAllocType (IdentS);
		newIdent->ident_name	= ConvertCleanString (name);
	}

	newIdent->ident_symbol	= &functions [functionIndex];

	newSymbDef	= ConvertAllocType (SymbDefS);
	newSymbDef->sdef_kind		= NEWDEFINITION;
	newSymbDef->sdef_exported	= False;
	newSymbDef->sdef_module		= module->bem_name;
	newSymbDef->sdef_ident		= newIdent;
	newSymbDef->sdef_mark		= 0;
	newSymbDef->sdef_isused		= 0;

	functions [functionIndex].symb_kind	= definition;
	functions [functionIndex].symb_def	= newSymbDef;

} /* BEDeclareRuleType */

void
BEDefineRuleType (int functionIndex, int moduleIndex, BETypeAltP typeAlt)
{
	SymbolP		functionSymbol;
	SymbDef		sdef;
	RuleTypes	ruleType;
	BEModule	module;

	ruleType	= ConvertAllocType (struct rule_type);
	ruleType->rule_type_rule	= typeAlt;

	module	= &gBEState.be_modules [moduleIndex];
	functionSymbol	= &module->bem_functions [functionIndex];

	sdef	= functionSymbol->symb_def;
	Assert (sdef->sdef_kind == NEWDEFINITION);
	sdef->sdef_arity		= typeAlt->type_alt_lhs->type_node_arity;
	sdef->sdef_arfun		= NoArrayFun;
	sdef->sdef_kind 		= module->bem_isSystemModule ? SYSRULE : DEFRULE;
	sdef->sdef_rule_type	= ruleType;
} /* BEDefineRuleType */

void
BEAdjustArrayFunction (BEArrayFunKind arrayFunKind, int functionIndex, int moduleIndex)
{
	SymbolP		functionSymbol;
	SymbDef		sdef;
	BEModule	module;

	module	= &gBEState.be_modules [moduleIndex];

	functionSymbol	= &module->bem_functions [functionIndex];

	sdef	= functionSymbol->symb_def;

	Assert (sdef->sdef_kind == DEFRULE || (moduleIndex == main_dcl_module_n && sdef->sdef_kind == IMPRULE));
	sdef->sdef_arfun	= arrayFunKind;
	sdef->sdef_mark		= 0;

	if (sdef->sdef_kind == DEFRULE  && moduleIndex == main_dcl_module_n)
	{
		AddUserDefinedArrayFunction (functionSymbol);
		sdef->sdef_kind	= SYSRULE;
	}
} /* BEAdjustArrayFunction */

BETypeP
BETypes (BETypeP type, BETypeP types)
{
	Assert (type->type_next == NULL);

	type->type_next	= types;

	return (type);
} /* BETypes */

BETypeP
BENoTypes (void)
{
	return (NULL);
} /* BENoTypes */

void
BEDeclareType (int typeIndex, int moduleIndex, CleanString name)
{
	SymbDefP	newSymbDef;
	Ident		newIdent;
	SymbolP	 	*types;
	BEModuleP	module;
	module	= &gBEState.be_modules [moduleIndex];

	Assert ((unsigned int) typeIndex < module->bem_nTypes);
	Assert (module->bem_types [typeIndex]->symb_kind == erroneous_symb);

	types	=	module->bem_types;
	Assert (types != NULL);

	newIdent	= ConvertAllocType (IdentS);
	newIdent->ident_name	= ConvertCleanString (name);
	newIdent->ident_symbol	= types [typeIndex];
/* RWS change this
	newSymbDef	= ConvertAllocType (SymbDefS);
*/
	newSymbDef	= types [typeIndex]->symb_def;
	Assert (newSymbDef != NULL);

	newSymbDef->sdef_kind		= NEWDEFINITION;
	newSymbDef->sdef_exported	= False;
	newSymbDef->sdef_dcl_icl	= NULL;
	newSymbDef->sdef_mark		= 0;
	newSymbDef->sdef_isused		= 0;

	newSymbDef->sdef_module		= module->bem_name;
	newSymbDef->sdef_ident		= newIdent;

	types [typeIndex]->symb_kind	= definition;
	types [typeIndex]->symb_def		= newSymbDef;
} /* BEDeclareType */
 
BETypeVarP
BETypeVar (CleanString name)
{
	IdentP	ident;
	TypeVar	typeVar;

	ident	= ConvertAllocType (IdentS);
	typeVar	= ConvertAllocType (struct type_var);

	ident->ident_name	= ConvertCleanString (name);

	typeVar->tv_ident		= ident;
	typeVar->tv_argument_nr	= 0; /* ??? */

	return (typeVar);
} /* BETypeVar */

BETypeVarP
BENumberedTypeVar (CleanString name,int argument_n)
{
	IdentP	ident;
	TypeVar	typeVar;

	ident	= ConvertAllocType (IdentS);
	typeVar	= ConvertAllocType (struct type_var);

	ident->ident_name	= ConvertCleanString (name);

	typeVar->tv_ident		= ident;
	typeVar->tv_argument_nr	= argument_n;

	return typeVar;
}

BETypeVarListP
BETypeVarListElem (BETypeVarP typeVar, BEAttribution attribute)
{
	TypeVarList	typeVarListElement;

	typeVarListElement	= ConvertAllocType (struct type_var_list);
	typeVarListElement->tvl_elem		= typeVar;
	typeVarListElement->tvl_attribute	= attribute;
	typeVarListElement->tvl_next		= NULL;

	return (typeVarListElement);
} /* BETypeVarListElem */

BETypeVarListP
BETypeVars (BETypeVarListP typeVarListElement, BETypeVarListP typeVarList)
{
	Assert (typeVarListElement->tvl_next == NULL);

	typeVarListElement->tvl_next	= typeVarList;

	return (typeVarListElement);
} /* BETypeVars */

BETypeVarListP
BENoTypeVars (void)
{
	return (NULL);
} /* BENoTypeVars */

BEFlatTypeP
BEFlatType (BESymbolP symbol, BEAttribution attribution, BETypeVarListP arguments)
{
	FlatType	flatType;

	flatType	= ConvertAllocType (struct flat_type);

	flatType->ft_symbol		= symbol;
	flatType->ft_arguments	= arguments;
	flatType->ft_attribute = (AttributeKind) attribution;;

	return (flatType);
} /* BEFlatType */

void
BEAlgebraicType (BEFlatTypeP lhs, BEConstructorListP constructors)
{
	Types		type;
	SymbDefP	sdef;
	int			nConstructors;

	type	= ConvertAllocType (struct type);
	/* ifdef DEBUG */
	type->type_next	= NULL;
	/* endif */

	type->type_lhs	=	lhs;
	type->type_constructors	= constructors;

	nConstructors	=	0;
	for (; constructors != NULL; constructors = constructors->cl_next)
	{
		SymbDef	cdef;

		Assert (!constructors->cl_constructor->type_node_is_var);
		Assert (constructors->cl_constructor->type_node_symbol->symb_kind == definition);

		cdef	= constructors->cl_constructor->type_node_symbol->symb_def;
		Assert (cdef->sdef_type == NULL);
		cdef->sdef_type	= type;

		nConstructors++;
	}

	type->type_nr_of_constructors	= nConstructors;

	Assert (type->type_lhs->ft_symbol->symb_kind == definition);
	sdef	= type->type_lhs->ft_symbol->symb_def;
	Assert (sdef->sdef_kind == NEWDEFINITION);
	sdef->sdef_kind 		= TYPE;
	sdef->sdef_type			= type;
} /* BEAlgebraicType */

void BEExtendableAlgebraicType (BEFlatTypeP lhs, BEConstructorListP constructors)
{
	Types type;
	SymbDefP sdef;

	type = ConvertAllocType (struct type);
	type->type_next = NULL;
	type->type_lhs = lhs;
	type->type_constructors	= constructors;
	type->type_nr_of_constructors = 0;

	for (; constructors!=NULL; constructors=constructors->cl_next)
		constructors->cl_constructor->type_node_symbol->symb_def->sdef_type = type;

	sdef = type->type_lhs->ft_symbol->symb_def;
	sdef->sdef_kind = TYPE;
	sdef->sdef_type = type;
}

void
BERecordType (int moduleIndex, BEFlatTypeP lhs, BETypeNodeP constructorType, int is_boxed_record, BEFieldListP fields)
{
	int					nFields;
	Types				type;
	SymbDefP			sdef;
	BEConstructorListP	constructor;

	type	= ConvertAllocType (struct type);
	type->type_next	= NULL;

	constructor	= ConvertAllocType (struct constructor_list);

	constructor->cl_next		= NULL;
	constructor->cl_constructor	= constructorType;

	type->type_lhs	=	lhs;
	type->type_constructors	= constructor;
	type->type_fields		= fields;

	nFields	=	0;
	for (; fields != NULL; fields = fields->fl_next)
	{
		SymbDef	fdef;

		Assert (fields->fl_symbol->symb_kind == definition);

		fdef	= fields->fl_symbol->symb_def;
		Assert (fdef->sdef_type == NULL);
		fdef->sdef_type	= type;
		fdef->sdef_sel_field_number	= nFields++;
	}

	type->type_nr_of_constructors	= 0;

	Assert (type->type_lhs->ft_symbol->symb_kind == definition);
	sdef	= type->type_lhs->ft_symbol->symb_def;
	Assert (sdef->sdef_kind == NEWDEFINITION);
	sdef->sdef_checkstatus	= TypeChecked;
	sdef->sdef_kind 		= RECORDTYPE;
	sdef->sdef_type			= type;
	sdef->sdef_arity		= constructorType->type_node_arity;

	sdef->sdef_boxed_record	= is_boxed_record;

	{
		int constructor_index;
		struct symbol *constructor_symbol_p;
		BEModuleP module;
		
		constructor_symbol_p = constructorType->type_node_symbol;
		/* BEConstructorSymbol has stored the index in symb_arity, should be passed directly to BERecordType */
		constructor_index = constructor_symbol_p->symb_arity;
		constructor_symbol_p->symb_arity = 0;

		module = &gBEState.be_modules [moduleIndex];
		
		Assert (module->bem_constructors[constructor_index]==constructor_symbol_p);
		module->bem_constructors[constructor_index] = type->type_lhs->ft_symbol;
	}
} /* BERecordType */

void
BEAbsType (BEFlatTypeP lhs)
{
	AbsTypes	absType;
	SymbDefP	sdef;

	absType	= ConvertAllocType (struct abs_type);

	absType->abs_graph	=	lhs;

	Assert (lhs->ft_symbol->symb_kind == definition);
	sdef	= lhs->ft_symbol->symb_def;
	Assert (sdef->sdef_kind == NEWDEFINITION);
	sdef->sdef_checkstatus	= TypeChecked;
	sdef->sdef_kind 		= ABSTYPE;
	sdef->sdef_abs_type		= absType;

} /* BEAbsType */

BEConstructorListP
BEConstructors (BEConstructorListP constructor, BEConstructorListP constructors)
{
	Assert (constructor->cl_next == NULL);

	constructor->cl_next	= constructors;

	return (constructor);
} /* BEConstructors */

BEConstructorListP
BENoConstructors (void)
{
	return (NULL);
} /* BENoConstructors */

BEConstructorListP
BEConstructor (BETypeNodeP type)
{
	ConstructorList	constructor;
	SymbDef			sdef;

	Assert (!type->type_node_is_var);
	Assert (type->type_node_symbol->symb_kind == definition);

	sdef	= type->type_node_symbol->symb_def;

	constructor	= ConvertAllocType (struct constructor_list);

	/* ifdef DEBUG */
	constructor->cl_next	= NULL;
	/* endif */
	constructor->cl_constructor	= type;

	sdef->sdef_kind = CONSTRUCTOR;
	sdef->sdef_constructor	= constructor;
	sdef->sdef_arity		= type->type_node_arity;
	/* ifdef DEBUG */
	sdef->sdef_type			= NULL;
	/* endif */

	return (constructor);
} /* BEConstructor */

void
BEDeclareField (int fieldIndex, int moduleIndex, CleanString name)
{
	SymbDefP	newSymbDef;
	Ident		newIdent;
	SymbolP	 	fields;
	BEModuleP	module;

	module	= &gBEState.be_modules [moduleIndex];
	Assert ((unsigned) fieldIndex < module->bem_nFields);
	Assert (module->bem_fields [fieldIndex].symb_kind == erroneous_symb);

	fields	=	module->bem_fields;
	Assert (fields != NULL);

	newIdent	= ConvertAllocType (IdentS);
	newIdent->ident_name	= ConvertCleanString (name);
	newIdent->ident_symbol	= &fields [fieldIndex];

	newSymbDef	= ConvertAllocType (SymbDefS);
	newSymbDef->sdef_kind		= NEWDEFINITION;
	newSymbDef->sdef_exported	= False;
	newSymbDef->sdef_module		= module->bem_name;
	newSymbDef->sdef_ident		= newIdent;
	newSymbDef->sdef_mark		= 0;
	newSymbDef->sdef_isused		= 0;

	fields [fieldIndex].symb_kind	= definition;
	fields [fieldIndex].symb_def	= newSymbDef;
} /* BEDeclareField */

BEFieldListP
BEField (int fieldIndex, int moduleIndex, BETypeNodeP type)
{
	SymbDef		sdef;
	SymbolP		fields;
	BEModuleP	module;
	FieldList	field;

	module	= &gBEState.be_modules [moduleIndex];
	Assert ((unsigned) fieldIndex < module->bem_nFields);
	Assert (module->bem_fields [fieldIndex].symb_kind == definition);

	fields	=	module->bem_fields;
	Assert (fields != NULL);

	field	= ConvertAllocType (struct field_list);

	/* ifdef DEBUG */
	field->fl_next	= NULL;
	/* endif */
	field->fl_symbol	= &fields [fieldIndex];
	field->fl_type		= type;

	sdef	=	fields [fieldIndex].symb_def;

	sdef->sdef_kind = FIELDSELECTOR;
	sdef->sdef_sel_field	= field;
	sdef->sdef_arity		= 1;
	sdef->sdef_mark			= 0;
	/* ifdef DEBUG */
	sdef->sdef_type			= NULL;
	/* endif */

	return (field);
} /* BEField */

void
BESetMemberTypeOfField (int fieldIndex, int moduleIndex, BETypeAltP typeAlt)
{
	SymbDef sdef;

	sdef = gBEState.be_modules [moduleIndex].bem_fields [fieldIndex].symb_def;
	sdef->sdef_mark |= SDEF_FIELD_HAS_MEMBER_TYPE;
	sdef->sdef_member_type_of_field = typeAlt;
	sdef->sdef_member_states_of_field = NULL;
}

int
BESetDictionaryFieldOfMember (int function_index,int field_index, int field_module_index)
{
	SymbolP function_symbol_p;
	SymbDefP field_sdef,function_sdef;
	
	function_symbol_p = &gBEState.be_modules[main_dcl_module_n].bem_functions[function_index];
	if (function_symbol_p->symb_kind==erroneous_symb)
		return 1;

	field_sdef = gBEState.be_modules [field_module_index].bem_fields[field_index].symb_def;

	/* in BEAdjustStrictListConsInstance symb_kind=cons_symb */	
	if (function_symbol_p->symb_kind==cons_symb)
		function_sdef = function_symbol_p->symb_unboxed_cons_p->unboxed_cons_sdef_p;
	else
		function_sdef = function_symbol_p->symb_def;

	if (! (function_sdef->sdef_kind==IMPRULE || function_sdef->sdef_kind==DEFRULE || function_sdef->sdef_kind==SYSRULE))
		return 2;

	function_sdef->sdef_mark |= SDEF_INSTANCE_RULE_WITH_FIELD_P;
	function_sdef->sdef_dictionary_field = field_sdef;
	
	return 0;
}

void
BESetInstanceFunctionOfFunction (int function_index,int instance_function_index)
{
	SymbolP instance_function_symbol_p;
	SymbDefP function_sdef,instance_function_sdef;

	function_sdef = gBEState.be_modules[main_dcl_module_n].bem_functions[function_index].symb_def;
	instance_function_symbol_p = &gBEState.be_modules[main_dcl_module_n].bem_functions[instance_function_index];
	
	if (instance_function_symbol_p->symb_kind==erroneous_symb){
		/*	the instance function is not used any more, only specialized versions of it,
			allocate empty SymbDef for BESetDictionaryFieldOfMember */

		instance_function_sdef = ConvertAllocType (SymbDefS);
		instance_function_sdef->sdef_kind = IMPRULE;
		instance_function_sdef->sdef_mark = 0;
		instance_function_sdef->sdef_isused = 0;

		instance_function_symbol_p->symb_def=instance_function_sdef;
		instance_function_symbol_p->symb_kind=instance_symb;
	}

	instance_function_sdef = instance_function_symbol_p->symb_def;

	function_sdef->sdef_mark |= SDEF_RULE_INSTANCE_RULE_P;
	function_sdef->sdef_instance_rule = instance_function_sdef;
}

BEFieldListP
BEFields (BEFieldListP field, BEFieldListP fields)
{
	Assert (field->fl_next == NULL);

	field->fl_next	= fields;

	return (field);
} /* BEFields */

BEFieldListP
BENoFields (void)
{
	return (NULL);
} /* BENoFields */

void
BEDeclareConstructor (int constructorIndex, int moduleIndex, CleanString name)
{
	SymbDefP	newSymbDef;
	Ident		newIdent;
	SymbolP	 	*constructors;
	BEModuleP	module;

	module	= &gBEState.be_modules [moduleIndex];
	Assert ((unsigned) constructorIndex < module->bem_nConstructors);
	Assert (module->bem_constructors [constructorIndex]->symb_kind == erroneous_symb);

	constructors	=	module->bem_constructors;
	Assert (constructors != NULL);

	newIdent	= ConvertAllocType (IdentS);
	newIdent->ident_name	= ConvertCleanString (name);
	newIdent->ident_symbol	= constructors [constructorIndex];

	newSymbDef	= ConvertAllocType (SymbDefS);
	newSymbDef->sdef_kind		= NEWDEFINITION;
	newSymbDef->sdef_exported	= False;
	newSymbDef->sdef_module		= module->bem_name;
	newSymbDef->sdef_ident		= newIdent;
	newSymbDef->sdef_mark		= 0;
	newSymbDef->sdef_isused		= 0;

	constructors [constructorIndex]->symb_kind	= definition;
	constructors [constructorIndex]->symb_def	= newSymbDef;
} /* BEDeclareConstructor */

void
BEDefineRules (BEImpRuleP rules)
{
	gBEState.be_icl.beicl_module->im_rules	= rules;
} /* BEDefineRules */

void
BEDefineImportedObjsAndLibs (BEStringListP objs, BEStringListP libs)
{
	gBEState.be_icl.beicl_module->im_imported_objs	= objs;
	gBEState.be_icl.beicl_module->im_imported_libs	= libs;
} /* BEDefineRules */

void BEInsertForeignExport (BESymbolP symbol_p,int stdcall)
{
	ImpMod icl_mod_p;
	struct foreign_export_list *foreign_export_list_p;
		
	foreign_export_list_p=ConvertAllocType (struct foreign_export_list);
	
	icl_mod_p=gBEState.be_icl.beicl_module;

	foreign_export_list_p->fe_symbol_p=symbol_p;
	foreign_export_list_p->fe_stdcall=stdcall;
	foreign_export_list_p->fe_next=icl_mod_p->im_foreign_exports;
	icl_mod_p->im_foreign_exports=foreign_export_list_p;
}

BEStringListP
BEString (CleanString cleanString)
{
	struct string_list	*string;

	string	= ConvertAllocType (struct string_list);

	string->sl_string	= ConvertCleanString (cleanString);
	/* ifdef DEBUG */
	string->sl_next	= NULL;
	/* endif */

	return (string);
} /* BEString */

BEStringListP
BEStrings (BEStringListP string, BEStringListP strings)
{
	Assert (string->sl_next == NULL);

	string->sl_next	= strings;

	return (string);
} /* BEStringList*/

BEStringListP
BENoStrings (void)
{
	return (NULL);
} /* BENoStrings */


BENodeIdListP
BENodeIdListElem (BENodeIdP nodeId)
{
	struct node_id_list_element	*elem;

	elem	= ConvertAllocType (struct node_id_list_element);

	elem->nidl_node_id	= nodeId;
	/* ifdef DEBUG */
	elem->nidl_next	= NULL;
	/* endif */

	return (elem);
} /* BENodeIdListElem */

BENodeIdListP
BENodeIds (BENodeIdListP nid, BENodeIdListP nids)
{
	Assert (nid->nidl_next == NULL);

	nid->nidl_next	= nids;

	return (nid);
} /* BENodeIds*/

BENodeIdListP
BENoNodeIds (void)
{
	return (NULL);
} /* BENoNodeIds */

BECodeBlockP
BEAbcCodeBlock (int inlineFlag, BEStringListP instructions)
{
	CodeBlock	codeBlock;

	codeBlock	=	ConvertAllocType (CodeBlockS);

	codeBlock->co_instr			= (Instructions) instructions;
	codeBlock->co_is_abc_code	= True;
	codeBlock->co_is_inline		= inlineFlag;

	return (codeBlock);
} /* BEAbcCodeBlock */

BECodeBlockP
BEAnyCodeBlock (BECodeParameterP inParams, BECodeParameterP outParams, BEStringListP instructions)
{
	CodeBlock	codeBlock;

	codeBlock	=	ConvertAllocType (CodeBlockS);

	codeBlock->co_instr			= (Instructions) instructions;
	codeBlock->co_is_abc_code	= False;
	codeBlock->co_parin			= inParams;
	codeBlock->co_parout		= outParams;

	return (codeBlock);
} /* BEAnyCodeBlock */

BECodeParameterP
BECodeParameter (CleanString location, BENodeIdP nodeId)
{
	Parameters	parameter;

	parameter	= ConvertAllocType (struct parameter);

	parameter->par_kind		= 0;
	parameter->par_node_id	= nodeId;
	parameter->par_loc		= Identifier (ConvertCleanString (location));

	/* ifdef DEBUG */
	parameter->par_next	= NULL;
	/* endif */

	return (parameter);
} /* BECodeParameter */

BECodeParameterP
BECodeParameters (BECodeParameterP parameter, BECodeParameterP parameters)
{
	Assert (parameter->par_next == NULL);

	parameter->par_next	= parameters;

	return (parameter);
} /* BECodeParameters */

BECodeParameterP
BENoCodeParameters (void)
{
	return (NULL);
} /* BENoCodeParameters */

static void
RemoveSpecialArrayFunctionsFromSymbolList (SymbolP *symbolH)
{
	SymbolP	symbolP;

	while ((symbolP = *symbolH) != NULL)
	{
		SymbDefP	sdef;

		sdef	= symbolP->symb_def;

		if (symbolP->symb_kind == definition && sdef->sdef_kind == IMPRULE && sdef->sdef_arfun != NoArrayFun)
			*symbolH	= symbolP->symb_next;
		else
			symbolH	= &symbolP->symb_next;
	}
} /* RemoveSpecialArrayFunctionsFromSymbolList */

#if 0
File rules_file;
#endif

struct clean_string_128 { size_t length; char chars[128]; } clean_error_string;

int
BEGenerateCode (CleanString outputFile)
{
	char	*outputFileName;
	ImpRule	rule;

	clean_error_string.length=0;

	if (CompilerError)
		return False;

	if (setjmp (ExitEnv)!=0){
		ExitEnv_valid=0;
		return False;
	}

	ExitEnv_valid=1;	

	// RemoveSpecialArrayFunctionsFromSymbolList (&gBEState.be_icl.beicl_module->im_symbols);

	/* +++ hack */
	rule	= gBEState.be_dictionarySelectFunSymbol->symb_def->sdef_rule;
	rule->rule_next	= gBEState.be_icl.beicl_module->im_rules;
	gBEState.be_icl.beicl_module->im_rules	=	rule;

	rule	= gBEState.be_dictionaryUpdateFunSymbol->symb_def->sdef_rule;
	rule->rule_next	= gBEState.be_icl.beicl_module->im_rules;
	gBEState.be_icl.beicl_module->im_rules	=	rule;

	outputFileName	= ConvertCleanString (outputFile);

#if 0
	{
		File f;
		
		f=fopen ("Rules","w");
		if (f){
			ImpRuleS *rule;

			for (rule=gBEState.be_icl.beicl_module->im_rules; rule!=NULL; rule=rule->rule_next){
				PrintImpRule (rule,4,f);
				
				if (rule->rule_next!=NULL)
					FPutC ('\n',f);
			}
			fclose (f);
		}
	}
#endif

#if 0
	rules_file=fopen ("Rules","w");
#endif

	CodeGeneration (gBEState.be_icl.beicl_module, outputFileName);

#if 0
	fclose (rules_file);
#endif

	ExitEnv_valid=0;

	return (!CompilerError);
} /* BEGenerateCode */

CleanString BEGetError (void)
{
	return (CleanString)&clean_error_string;
}

void
BEExportType (int isDictionary, int typeIndex)
{
	BEModuleP	dclModule, iclModule;
	SymbolP		typeSymbol;
	SymbDefP	iclDef, dclDef;

	iclModule	= &gBEState.be_modules [main_dcl_module_n];

	Assert ((unsigned int) typeIndex < iclModule->bem_nTypes);
	typeSymbol	= iclModule->bem_types [typeIndex];
	Assert (typeSymbol->symb_kind == definition);

	iclDef	= typeSymbol->symb_def;
	iclDef->sdef_exported	= True;

	dclModule	= &gBEState.be_icl.beicl_dcl_module;

	if (isDictionary)
		dclDef	= iclDef;
	else
	{
		Assert ((unsigned int) typeIndex < dclModule->bem_nTypes);
		typeSymbol	= dclModule->bem_types [typeIndex];
		Assert (typeSymbol->symb_kind == definition);
		dclDef	= typeSymbol->symb_def;
	}
	Assert (strcmp (iclDef->sdef_ident->ident_name, dclDef->sdef_ident->ident_name) == 0);

	iclDef->sdef_dcl_icl	= dclDef;
	dclDef->sdef_dcl_icl	= iclDef;
	iclDef->sdef_exported = True;
	dclDef->sdef_exported = True;
} /* BEExportType */

void
BEExportConstructor (int constructorIndex)
{
	BEModuleP	iclModule;
	SymbolP		constructorSymbol;
	SymbDefP	iclDef, dclDef;

	iclModule	= &gBEState.be_modules [main_dcl_module_n];

	Assert ((unsigned int) constructorIndex < iclModule->bem_nConstructors);
	constructorSymbol	= iclModule->bem_constructors [constructorIndex];
	Assert (constructorSymbol->symb_kind == definition);

	iclDef	= constructorSymbol->symb_def;
	iclDef->sdef_exported	= True;

	dclDef	= iclDef;

	iclDef->sdef_dcl_icl	= dclDef;
	dclDef->sdef_dcl_icl	= iclDef;

	iclDef->sdef_exported = True;
} /* BEExportConstructor */

void
BEExportField (int isDictionaryField, int fieldIndex)
{
	BEModuleP	dclModule, iclModule;
	SymbolP		fieldSymbol;
	SymbDefP	iclDef, dclDef;

	iclModule	= &gBEState.be_modules [main_dcl_module_n];

	Assert ((unsigned int) fieldIndex < iclModule->bem_nFields);
	fieldSymbol	= &iclModule->bem_fields [fieldIndex];
	Assert (fieldSymbol->symb_kind == definition);

	iclDef	= fieldSymbol->symb_def;
	iclDef->sdef_exported	= True;

	if (isDictionaryField)
		dclDef	= iclDef;
	else
	{
		dclModule	= &gBEState.be_icl.beicl_dcl_module;
	
		Assert ((unsigned int) fieldIndex < dclModule->bem_nFields);
		fieldSymbol	= &dclModule->bem_fields [fieldIndex];
		Assert (fieldSymbol->symb_kind == definition);
		dclDef	= fieldSymbol->symb_def;
	}

	Assert (strcmp (iclDef->sdef_ident->ident_name, dclDef->sdef_ident->ident_name) == 0);

	iclDef->sdef_dcl_icl	= dclDef;
	dclDef->sdef_dcl_icl	= iclDef;

	iclDef->sdef_exported = True;
} /* BEExportField */

void
BEExportFunction (int functionIndex)
{
	BEModuleP	dclModule, iclModule;
	SymbolP		functionSymbol;
	SymbDefP	iclDef, dclDef;

	iclModule	= &gBEState.be_modules [main_dcl_module_n];

	Assert ((unsigned int) functionIndex < iclModule->bem_nFunctions);
	functionSymbol	= &iclModule->bem_functions [functionIndex];
	Assert (functionSymbol->symb_kind == definition);

	iclDef	= functionSymbol->symb_def;
	iclDef->sdef_exported	= True;

	dclModule	= &gBEState.be_icl.beicl_dcl_module;

	if (((unsigned int) functionIndex < dclModule->bem_nFunctions))
	{
		functionSymbol	= &dclModule->bem_functions [functionIndex];
		Assert (functionSymbol->symb_kind == definition);
		dclDef	= functionSymbol->symb_def;
	
		dclDef->sdef_dcl_icl	= iclDef;

		Assert (strcmp (iclDef->sdef_ident->ident_name, dclDef->sdef_ident->ident_name) == 0);
	}
	else
		dclDef	= NULL;


	iclDef->sdef_dcl_icl	= dclDef;

	iclDef->sdef_exported = True;
} /* BEExportFunction */

void
BEStrictPositions (int functionIndex, int *bits, int **positions)
{
	BEModuleP			module;
	SymbolP				functionSymbol;
	SymbDef				functionDefinition;
	ImpRules			rule;
	TypeAlts			ruleType;
	StrictPositionsP	strict_positions;

	Assert ((unsigned int) main_dcl_module_n < gBEState.be_nModules);
	module	= &gBEState.be_modules [main_dcl_module_n];

	Assert ((unsigned int) functionIndex < module->bem_nFunctions);
	functionSymbol	= &module->bem_functions [functionIndex];

	Assert (functionSymbol->symb_kind == definition);
	functionDefinition	= functionSymbol->symb_def;

	Assert (functionDefinition->sdef_kind == IMPRULE);
	rule	= functionDefinition->sdef_rule;

	ruleType	= rule->rule_type;
	Assert (ruleType != NULL);

	strict_positions	= ruleType->type_alt_strict_positions;

	if (strict_positions == NULL)
	{
		/*	this can happen if sa is turned of, or if the sa has failed
			(for example when it's out of memory) */
		*bits		= 0;
		*positions	= NULL;
	}
	else
	{
		*bits		= strict_positions->sp_size;
		*positions	= strict_positions->sp_bits;
	}
} /* BEStrictPositions */

int
BECopyInts  (int cLength, int *ints, int *cleanArray)
{
    int	cleanLength, truncate;

	cleanLength	= cleanArray [-2];

	truncate	= cleanLength < cLength;
	if (truncate)
		cLength	= cleanLength;

	memcpy (cleanArray, ints, cLength * sizeof (int));

	Assert (!truncate);

	return (!truncate);
} /* BECopyInts */

int
BEGetIntFromArray  (int index, int *ints)
{
	return ints[index];
}

static void
CheckBEEnumTypes (void)
{
	/* Annotation */
 	Assert (NoAnnot		== BENoAnnot);
	Assert (StrictAnnot	== BEStrictAnnot);

	/* Annotation */
 	Assert (NoUniAttr			== BENoUniAttr);
 	Assert (NotUniqueAttr		== BENotUniqueAttr);
 	Assert (UniqueAttr			== BEUniqueAttr);
 	Assert (ExistsAttr			== BEExistsAttr);
 	Assert (UniqueVariable		== BEUniqueVariable);
 	Assert (FirstUniVarNumber	== BEFirstUniVarNumber);

	/* SymbKind */
	Assert (int_type					== BEIntType);
	Assert (bool_type					== BEBoolType);
	Assert (char_type					== BECharType);
	Assert (real_type					== BERealType);
	Assert (file_type					== BEFileType);
	Assert (string_type					== BEStringType);
	Assert (world_type					== BEWorldType);
	Assert (procid_type					== BEProcIdType);
	Assert (redid_type					== BERedIdType);
	Assert (rational_denot				== BERationalDenot);
	Assert (int_denot					== BEIntDenot);
	Assert (bool_denot					== BEBoolDenot);
	Assert (char_denot					== BECharDenot);
	Assert (real_denot					== BERealDenot);
	Assert (integer_denot				== BEIntegerDenot);
	Assert (string_denot				== BEStringDenot);
	Assert (fun_type					== BEFunType);
	Assert (array_type					== BEArrayType);
	Assert (strict_array_type			== BEStrictArrayType);
	Assert (unboxed_array_type			== BEUnboxedArrayType);
	Assert (list_type					== BEListType);
	Assert (tuple_type					== BETupleType);
	Assert (empty_type					== BEEmptyType);
#if DYNAMIC_TYPE
	Assert (dynamic_type				== BEDynamicType);
#endif
	Assert (Nr_Of_Predef_Types			== BENrOfPredefTypes);
	Assert (tuple_symb					== BETupleSymb);
	Assert (cons_symb					== BEConsSymb);
	Assert (nil_symb					== BENilSymb);
	Assert (apply_symb					== BEApplySymb);
	Assert (if_symb						== BEIfSymb);
	Assert (fail_symb					== BEFailSymb);
	Assert (select_symb					== BESelectSymb);
	Assert (Nr_Of_Predef_FunsOrConses	== BENrOfPredefFunsOrConses);
	Assert (definition					== BEDefinition);
	Assert (newsymbol					== BENewSymbol);
	Assert (instance_symb				== BEInstanceSymb);
	Assert (empty_symbol				== BEEmptySymbol);
	Assert (field_symbol_list			== BEFieldSymbolList);
	Assert (erroneous_symb				== BEErroneousSymb);

	/* ArrayFunKind */
	Assert (CreateArrayFun			== BECreateArrayFun);
	Assert (ArraySelectFun			== BEArraySelectFun);
	Assert (UnqArraySelectFun		== BEUnqArraySelectFun);
	Assert (ArrayUpdateFun			== BEArrayUpdateFun);
	Assert (ArrayReplaceFun			== BEArrayReplaceFun);
	Assert (ArraySizeFun			== BEArraySizeFun);
	Assert (UnqArraySizeFun			== BEUnqArraySizeFun);
	Assert (_CreateArrayFun			== BE_CreateArrayFun);
	Assert (_UnqArraySelectFun		== BE_UnqArraySelectFun);
	Assert (_UnqArraySelectNextFun	== BE_UnqArraySelectNextFun);
	Assert (_UnqArraySelectLastFun	== BE_UnqArraySelectLastFun);
	Assert (_ArrayUpdateFun			== BE_ArrayUpdateFun);
	Assert (NoArrayFun				== BENoArrayFun);

	/* SelectorKind */
	Assert (1			== BESelector);
	Assert (SELECTOR_U	== BESelector_U);
	Assert (SELECTOR_F	== BESelector_F);
	Assert (SELECTOR_L	== BESelector_L);
	Assert (SELECTOR_N	== BESelector_N);
} /* CheckBEEnumTypes */

void
BEArg (CleanString arg)
{
	Assert (gBEState.be_argi < gBEState.be_argc);

	gBEState.be_argv [gBEState.be_argi++]	= ConvertCleanString (arg);

	// +++ ugly
	if (gBEState.be_argi == gBEState.be_argc)
	{
		char	*dummy;
		extern	Bool ParseCommandArgs (int argc, char **argv, char **file_name_p, char **output_file_name_p);

		if (!ParseCommandArgs (gBEState.be_argc, gBEState.be_argv, &dummy, &dummy))
			FatalCompError ("backend", "BEInit", "compilation aborted");

	/*	FatalCompError ("backend", "BEInit", "FatalCompError in backend"); */
	/*	ErrorInCompiler ("backend", "BEInit", "ErrorInCompiler in backend"); */
	/*	StaticMessage (True, "<backend>", "StaticMessage (True) in backend"); */
	/*	StaticMessage (False, "<backend>", "StaticMessage (False) in backend"); */
	/*	*(int*)0L= 17; */
	}
} /* BEArg */

#if STRICT_LISTS
static void init_unboxed_list_symbols (void)
{
	StateP array_state_p,strict_array_state_p,unboxed_array_state_p;
	int i;
	
	for (i=0; i<Nr_Of_Predef_Types; ++i){
		SymbolP symbol_p;
		
		symbol_p=&unboxed_list_symbols[i][0];
		symbol_p->symb_kind=cons_symb;
		symbol_p->symb_head_strictness=4;
		symbol_p->symb_tail_strictness=0;
		symbol_p->symb_state_p=&BasicSymbolStates[i];
		symbol_p->symb_next=NULL;
		
		symbol_p=&unboxed_list_symbols[i][1];
		symbol_p->symb_kind=cons_symb;
		symbol_p->symb_head_strictness=4;
		symbol_p->symb_tail_strictness=1;
		symbol_p->symb_state_p=&BasicSymbolStates[i];
		symbol_p->symb_next=NULL;
	}

	array_state_p=ConvertAllocType (StateS);
	array_state_p->state_type = ArrayState;
	array_state_p->state_arity = 1;
	array_state_p->state_array_arguments = ConvertAllocType (StateS);
	array_state_p->state_mark = 0;
	SetUnaryState (&array_state_p->state_array_arguments[0],OnA,UnknownObj);

	unboxed_list_symbols[array_type][0].symb_state_p=array_state_p;
	unboxed_list_symbols[array_type][1].symb_state_p=array_state_p;

	strict_array_state_p=ConvertAllocType (StateS);
	strict_array_state_p->state_type = ArrayState;
	strict_array_state_p->state_arity = 1;
	strict_array_state_p->state_array_arguments = ConvertAllocType (StateS);
	strict_array_state_p->state_mark = 0;
	strict_array_state_p->state_array_arguments[0] = StrictState;

	unboxed_list_symbols[strict_array_type][0].symb_state_p=strict_array_state_p;
	unboxed_list_symbols[strict_array_type][1].symb_state_p=strict_array_state_p;

	unboxed_array_state_p=ConvertAllocType (StateS);
	unboxed_array_state_p->state_type = ArrayState;
	unboxed_array_state_p->state_arity = 1;
	unboxed_array_state_p->state_array_arguments = ConvertAllocType (StateS);
	unboxed_array_state_p->state_mark = STATE_UNBOXED_ARRAY_MASK;
	unboxed_array_state_p->state_array_arguments [0] = StrictState;

	unboxed_list_symbols[unboxed_array_type][0].symb_state_p=unboxed_array_state_p;
	unboxed_list_symbols[unboxed_array_type][1].symb_state_p=unboxed_array_state_p;
}
#endif

BackEnd
BEInit (int argc)
{
	Assert (!gBEState.be_initialised);

	ExitEnv_valid=0;

	CurrentPhase	= "Back End";
	CurrentModule	= "<unknown module>";
	CurrentExt		= "";

	InitStorage ();
	/* +++ remove symbol table from backend */
	ScanInitIdentStringTable ();
	InitScanner (); /* for inlining */
	ApplyId		= Identifier ("AP");
	ListId		= Identifier ("List");
	TupleId		= Identifier ("Tuple");
	ConsId		= Identifier ("[:]");
	NilId		= Identifier ("[]");
	SelectId	= Identifier ("_Select");
	IfId		= Identifier ("if");
	FailId		= Identifier ("_Fail");
#if DYNAMIC_TYPE
	DynamicId	= Identifier ("Dynamic");
#endif

#if SA_RECOGNIZES_ABORT_AND_UNDEF
	StdMiscId	= Identifier ("StdMisc");

	abort_id	= NULL;
	undef_id	= NULL;
	gSpecialIdents [BESpecialIdentStdMisc]	= &StdMiscId;
	gSpecialIdents [BESpecialIdentAbort]	= &abort_id;
	gSpecialIdents [BESpecialIdentUndef]	= &undef_id;
#endif

	StdBoolId	= Identifier ("StdBool");
	AndId	= NULL;
	OrId	= NULL;
	gSpecialIdents [BESpecialIdentStdBool]	= &StdBoolId;
	gSpecialIdents [BESpecialIdentAnd]		= &AndId;
	gSpecialIdents [BESpecialIdentOr]		= &OrId;

	PreludeId = Identifier ("Prelude");
	seq_id = NULL;
	gSpecialIdents[BESpecialIdentPrelude] = &PreludeId;
	gSpecialIdents[BESpecialIdentSeq] = &seq_id;

	UserDefinedArrayFunctions	= NULL;
#if STRICT_LISTS
	unboxed_record_cons_list=NULL;
	unboxed_record_decons_list=NULL;
#endif

	InitPredefinedSymbols ();

	ClearOpenDefinitionModules ();

	InitARC_Info ();
	InitStatesGen ();
	InitCoding ();
	InitInstructions ();

#if STRICT_LISTS
	init_unboxed_list_symbols();
#endif

	CheckBEEnumTypes ();

	gBEState.be_argv		= ConvertAlloc ((argc+1) * sizeof (char *));
	gBEState.be_argv [argc]	= NULL;
	gBEState.be_argc		= argc;
	gBEState.be_argi		= 0;

	gBEState.be_modules						= NULL;
	gBEState.be_allSymbols					= NULL;
	gBEState.be_dontCareSymbol				= NULL;
	gBEState.be_dictionarySelectFunSymbol	= NULL;
	gBEState.be_dictionaryUpdateFunSymbol	= NULL;

	gBEState.be_initialised	= True;

	im_def_module = NULL;

	special_types[0]=NULL;
	special_types[1]=NULL;

	return ((BackEnd) &gBEState);
} /* BEInit */

void
BECloseFiles (void)
{
	if (StdErrorReopened){
#ifdef _SUN_
		fclose (std_error_file_p);
		std_error_file_p = stderr;
#else
		fclose (StdError);
#endif
		StdErrorReopened = False;
	}
	if (StdOutReopened){
#ifdef _SUN_
		fclose (std_out_file_p);
		std_out_file_p = stdout;
#else
		fclose (StdOut);
#endif
		StdOutReopened = False;
	}
} /* BECloseFiles */

void
BEFree (BackEnd backEnd)
{
	Assert (backEnd == (BackEnd) &gBEState);

	FreeConvertBuffers ();
	CompFree ();

	Assert (gBEState.be_initialised);
	gBEState.be_initialised	= False;

	BECloseFiles ();
} /* BEFree */

// temporary hack

void
BEDeclareDynamicTypeSymbol (int typeIndex, int moduleIndex)
{
	gBEState.be_dynamicTypeIndex	= moduleIndex;
	gBEState.be_dynamicModuleIndex	= typeIndex;
} /* BEDeclareDynamicTypeSymbol */


BESymbolP
BEDynamicTempTypeSymbol (void)
{
	return (BETypeSymbol (gBEState.be_dynamicTypeIndex, gBEState.be_dynamicModuleIndex));
} /* BEDynamicTemp */
