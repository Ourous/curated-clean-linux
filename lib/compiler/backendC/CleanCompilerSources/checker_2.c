/*
	Authors:	Sjaak Smetsers & John van Groningen
	Version:	1.2
*/

#define COMPLEX_ABSTYPES
#define MOVE_LIFTED_CONSTANTS
#define OPTIMIZE_APPLIES
#define MOVE_MORE_LIFTED_CONSTANTS
#define MOVE_CURRIED_APPLICATIONS
#define MOVE_FUNCTIONS_IN_LAMBDAS

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "syntaxtr.t"
#include "comsupport.h"
#include "sizes.h"
#include "scanner.h"
#include "buildtree.h"
#include "comparser.h"
#include "statesgen.h"
#include "codegen_types.h"
#include "codegen1.h"
#include "codegen2.h"
#include "instructions.h"
#include "transform.h"
#include "checksupport.h"
#include "settings.h"
#include "checker.h"
#ifdef MOVE_FUNCTIONS_IN_LAMBDAS
# include "optimise_lambda.h"
#endif
#ifdef applec
#	include <types.h>
#endif

#undef DEBUG_REF_COUNT

#ifdef DEBUG_REF_COUNT
# define IF_DEBUG_REF_COUNT(a) a
# include "dbprint.h"
#else
# define IF_DEBUG_REF_COUNT(a)
#endif

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)

struct def_list {
	Symbol				mod_name;
	Bool				mod_undereval;
	DefMod				mod_body;
	struct def_list *	mod_next;
};

typedef struct def_list *DefModList,DefModElem;

static DefModList OpenDefinitionModules;

void GenDependencyList (void)
{
	DefModList def_mod;

	for_l (def_mod,OpenDefinitionModules,mod_next)
		GenDepend (def_mod->mod_body->dm_name->symb_ident->ident_name
#if WRITE_DCL_MODIFICATION_TIME
					,def_mod->mod_body->dm_modification_time
#endif
					);
}

void ReadInlineCode (void)
{
	DefModList d_mod;

	for_l (d_mod,OpenDefinitionModules,mod_next){
		DefMod def_mod;
		
		def_mod=d_mod->mod_body;
		if (def_mod->dm_system_module){
			Symbol symbol;
			
			for_l (symbol,def_mod->dm_symbols,symb_next)
				if (symbol->symb_kind==definition){
					SymbDef sdef;

					sdef=symbol->symb_def;
					if (sdef->sdef_kind==SYSRULE && sdef->sdef_mark & SDEF_USED_STRICTLY_MASK)
						break;
				}
			
			if (symbol!=NULL && d_mod->mod_name->symb_ident->ident_name!=CurrentModule)
				/* Get the inline instructions of all the rules that are defined in this module */
				ScanInlineFile (d_mod->mod_name->symb_ident->ident_name,def_mod->dm_system_module_table_kind);
		}
	}
}

Ident AnnotatedId, ListId, TupleId, ConsId, NilId, ApplyId, SelectId,
#ifdef CLEAN2
	DynamicId,
#endif
	StdBoolId, IfId, FailId, AndId, OrId,
	StdArrayId, ArrayFunctionIds [NoArrayFun];

#if SA_RECOGNIZES_ABORT_AND_UNDEF
Ident StdMiscId,abort_id,undef_id;	
#endif

Ident PreludeId,seq_id,system_seq_id;

Symbol StartSymbol;

SymbDef ArrayFunctionDefs [NoArrayFun],StdArrayAbortDef;

SymbDef scc_dependency_list;

SymbDef MakeNewSymbolDefinition (char * module, Ident name, int arity, SDefKind kind)
{
	SymbDef def;
	
	def = CompAllocType (SymbDefS);
	
	def->sdef_module = module;
	def->sdef_ident = name;
	def->sdef_arity = arity;
	def->sdef_kind = kind;

	def->sdef_mark=0;

	def->sdef_exported=False;

	def->sdef_arfun				= NoArrayFun;
	
	return def;
}

static NodeDefs FreeDefs;

NodeDefs NewNodeDef (NodeId nid,Node node)
{
	NodeDefs new;

	if (FreeDefs){
		new = FreeDefs;
		FreeDefs = FreeDefs->def_next;
	} else
		new = CompAllocType (NodeDefS);

	new->def_id		= nid;
	new->def_node	= node;
	new->def_mark	= 0;

	return new;
}

void InitChecker (void)
{
	FreeDefs=NIL;
#ifndef CLEAN2
	free_ldefs=NULL;
	free_depend_macros=NULL;
#endif
	AnnotatedId	= PutStringInHashTable ("_annotated", SymbolIdTable);
	ListId		= PutStringInHashTable ("[...]", SymbolIdTable);
	TupleId		= PutStringInHashTable ("(...)", SymbolIdTable);
	ConsId		= PutStringInHashTable ("[...|...]", SymbolIdTable);
	NilId		= PutStringInHashTable ("[]", SymbolIdTable);
	ApplyId		= PutStringInHashTable ("AP", SymbolIdTable);
	SelectId	= PutStringInHashTable ("_Select", SymbolIdTable);

#ifdef CLEAN2
	DynamicId	= PutStringInHashTable ("Dynamic", SymbolIdTable);
#endif
	
	/* hack RWS */
	IfId		= PutStringInHashTable ("if ", SymbolIdTable);
	IfId->ident_name	= "if";

	FailId		= PutStringInHashTable ("_Fail", SymbolIdTable);

	StartSymbol = NewSymbol (newsymbol);
	StartSymbol -> symb_ident = PutStringInHashTable ("Start", SymbolIdTable);

	AndId		= PutStringInHashTable ("&&", SymbolIdTable);
	OrId		= PutStringInHashTable ("||", SymbolIdTable);

#if SA_RECOGNIZES_ABORT_AND_UNDEF
	abort_id = PutStringInHashTable ("abort",SymbolIdTable);
	undef_id = PutStringInHashTable ("undef",SymbolIdTable);
#endif

	StdBoolId 			= PutStringInHashTable ("StdBool", ModuleIdTable);
	StdArrayId			= PutStringInHashTable ("_SystemArray", ModuleIdTable);

#if SA_RECOGNIZES_ABORT_AND_UNDEF
	StdMiscId = PutStringInHashTable ("StdMisc",ModuleIdTable);
#endif

	system_seq_id = PutStringInHashTable ("seq", SymbolIdTable);

 	/* Predefined Array functions */

 	ArrayFunctionIds[CreateArrayFun]	= PutStringInHashTable ("createArray", SymbolIdTable);
	ArrayFunctionIds[UnqArraySelectFun]	= PutStringInHashTable ("uselect", SymbolIdTable);
	ArrayFunctionIds[ArrayReplaceFun]	= PutStringInHashTable ("replace", SymbolIdTable);
	ArrayFunctionIds[UnqArraySizeFun]	= PutStringInHashTable ("usize", SymbolIdTable);
	ArrayFunctionIds[ArrayUpdateFun]	= PutStringInHashTable ("update", SymbolIdTable);
	ArrayFunctionIds[ArraySelectFun]	= PutStringInHashTable ("select", SymbolIdTable);
	ArrayFunctionIds[ArraySizeFun]		= PutStringInHashTable ("size", SymbolIdTable);
	ArrayFunctionIds[_CreateArrayFun]	= PutStringInHashTable ("_createArrayc", SymbolIdTable);
	ArrayFunctionIds[_UnqArraySelectFun]= PutStringInHashTable ("_uselectf", SymbolIdTable);
	ArrayFunctionIds[_UnqArraySelectNextFun]= PutStringInHashTable ("_uselectn", SymbolIdTable);
	ArrayFunctionIds[_UnqArraySelectLastFun]= PutStringInHashTable ("_uselectl", SymbolIdTable);
	ArrayFunctionIds[_ArrayUpdateFun]= PutStringInHashTable ("_updatei", SymbolIdTable);

	OpenDefinitionModules	= NIL;
}

void ClearOpenDefinitionModules (void)
{
	OpenDefinitionModules	= NULL;
}

void AddOpenDefinitionModule (SymbolP moduleNameSymbol, DefMod definitionModule)
{
	DefModList	openModule;

	openModule = CompAllocType (DefModElem);
	openModule->mod_name	= moduleNameSymbol;
	openModule->mod_body	= definitionModule;
	openModule->mod_next	= OpenDefinitionModules;

	OpenDefinitionModules  = openModule;
}
