/*
	Version 1.2.3 26/03/1997

	Author:  Sjaak Smetsers
*/

#pragma options (!macsbug_names)

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"
#include "syntaxtr.t"
#include "comsupport.h"

#include "scanner.h"
#include "comparser.h"
#include "sizes.h"
#include "checker.h"
#include "checksupport.h"
#include "transform.h"
#include "sa.h"
#include "statesgen.h"
#include "tctypes.t"
#include "typechecker.h"
#include "typechecker2.h"
#include "typeconv.h"
#include "refcountanal.h"
#include "overloading.h"
#include "tcsupport.h"
#include "buildtree.h"
#include "version.h"

#ifdef _DEBUG_
	static char *TC = "typechecker";
#endif

#undef _TYPESBUG_

#ifdef _PRINTRULES_
#include "dbprint.h"
#endif

SymbDef ArrayDefs [NrOfArrayInstances];

void ListTypes (ImpMod imod)
{
	if (DoListAllTypes)
	{	ImpRules irule;
		for (irule = imod -> im_rules; irule; irule = irule -> rule_next)
		{	SymbDef imp_sdef = irule -> rule_root -> node_symbol -> symb_def;
		
#ifdef CLEAN2
			if (strncmp (imp_sdef->sdef_ident->ident_name, "_dictionary", 11) != 0 || imp_sdef->sdef_isused)
#endif
			PrintType (imp_sdef, irule -> rule_type);
		}
	}

} /* ListTypes */

PolyList UserDefinedArrayFunctions;

Bool TypeError;

FlatType RetrieveLhsOfTypeDefinition (SymbDef tdef)
{
	switch (tdef -> sdef_kind)
	{
	case TYPE:
	case RECORDTYPE:
		return tdef -> sdef_type != NULL ? tdef -> sdef_type -> type_lhs : NULL;
	case TYPESYN:
		return tdef -> sdef_syn_type -> syn_lhs;
		break;
	case ABSTYPE:
		return tdef -> sdef_abs_type -> abs_graph;
		break;
	default:
		return NULL;
	}

} /* RetrieveLhsOfTypeDefinition */

HeapDescr TCTempSpace;

void InitTypeChecker (void)
{
#ifndef CLEAN2
	EmptySymbol = CompAllocType (SymbolS);
	EmptySymbol -> symb_kind = empty_symbol;

	InitialCellInfo.ci_removed		= False;
	InitialCellInfo.ci_free			= False;
	InitialCellInfo.ci_mark			= False;
	InitialCellInfo.ci_expanded		= False;
	InitialCellInfo.ci_printed		= False;
	InitialCellInfo.ci_hidden		= False;
	InitialCellInfo.ci_overloaded		= False;
	InitialCellInfo.ci_no_match		= False;
	InitialCellInfo.ci_class_var		= False;
	InitialCellInfo.ci_tmp_cell		= False;
	InitialCellInfo.ci_copy_cell		= False;
	InitialCellInfo.ci_strict		= False;
	InitialCellInfo.ci_with_insres	= False;
	InitialCellInfo.ci_non_coercible	= False;
	InitialCellInfo.ci_default		= False;

	InitialCellInfo.ci_kind			= BasicType;
	InitialCellInfo.ci_attrkind		= AC_NotUnique;
	InitialCellInfo.ci_attrvarkind	= AVK_Plain;
	InitialCellInfo.ci_instdepth		= 0;

	InitialTempAttrVar.tav_mark 			= False;
	InitialTempAttrVar.tav_present		= False;
	InitialTempAttrVar.tav_free			= False;
	InitialTempAttrVar.tav_onstack		= False;
	InitialTempAttrVar.tav_exi_quanti		= False;
	InitialTempAttrVar.tav_non_coercible	= False;
	
	InitialTempAttrVar.tav_varkind		= AC_Variable;
	InitialTempAttrVar.tav_number			= 0;
	InitialTempAttrVar.tav_offered		= NULL;
	InitialTempAttrVar.tav_demanded		= NULL;
#endif
}

