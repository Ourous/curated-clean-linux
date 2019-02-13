/*
	Version 1.0 21/09/1994

	Author:  Sjaak Smetsers
*/

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
#include "transform.h"
#include "sa.h"
#include "statesgen.h"
#include "tctypes.t"
#include "typechecker.h"
#include "typechecker2.h"
#include "tcsupport.h"
#include "typeconv.h"
#include "refcountanal.h"
#include "overloading.h"
#include "buildtree.h"

static char *TC2 = "typechecker2";

static TypeCell gSubType = NULL;
static unsigned NextTypeVariable;
static PolyList OverloadedTypeVariables;

static void PrintAttributeEquationsOfTypes (TypeCell type);

static Bool DPST = True, DPT = True;

static void PrintTypeCell (TypeCell type, Bool brackets, Bool with_uni_attrs);

static void PrintTypeCells (int arity, TypeCell typecells[], char separator, Bool brackets, Bool with_uni_attrs)
{
	if (arity > 0)
	{	int i;
		PrintTypeCell (typecells [0], brackets, with_uni_attrs);
		for (i=1; i<arity; i++)
		{	FPutC (separator, StdError);
			PrintTypeCell (typecells [i], brackets, with_uni_attrs);
		}
	}

} /* PrintTypeCells */

#define cDoPrintBrackets		True
#define cDoNotPrintBrackets	False

#define cDoPrintAttributes	True
#define cDoNotPrintAttributes	False

static void PrintAttributeVarWithEquations (AttrVarWithEquations avar)
{
/**	if (avar -> ave_coercible)
	{	switch (avar -> ave_refcount)
		{
		case RC_UsedOnceInLhs:
			switch (avar -> ave_offrefcount)
			{
			case RC_NotUsed:
				FPrintF (StdError, "<<%u>> ", avar -> ave_number);
				break;
			case RC_UsedOnceInLhs:
			case RC_UsedInLhs:
				FPrintF (StdError, "<%u> ", avar -> ave_number);
				break;
			default:
				FPrintF (StdError, "%u ", avar -> ave_number);
				break;
			}
			break;
		case RC_UsedInRhs:
		case RC_UsedOnceInRhs:
			if (AppearOnlyAsOfferedAttrsInTheRhs (avar -> ave_equations))
				FPrintF (StdError, "|%u| ", avar -> ave_number);
			else
				FPrintF (StdError, "%u ", avar -> ave_number);
			break;
		default:
			FPrintF (StdError, "%u ", avar -> ave_number);
			break;
		}
	}
	else **/
		FPrintF (StdError, "<%u> ", avar -> ave_number);

} /* PrintAttributeVarWithEquations */

static char *PrintVars = "abcdefghi";
#define NrOfPrintVars 9

static void PrintTypeVariable (TypeCell type_var)
{
	if (type_var -> contents_vc_number < NrOfPrintVars)
		FPutC (PrintVars [type_var -> contents_vc_number - 1], StdError);
	else
		FPrintF (StdError, "a%d", type_var -> contents_vc_number);

} /* PrintTypeVariable */

static void PrintTypeCell (TypeCell type, Bool brackets, Bool with_uni_attrs)
{
	if (type == NULL)
		FPutC ('_', StdError);
	else
	{	TypeCell ind_type = type;
		SkipIndirections (type);

		if (gSubType != NULL)
			type = SkipTypeSynIndirection (type);
		if (type == gSubType)
			FPutS ("^ ", StdError);

		if (with_uni_attrs)
		{	if (ind_type -> tc_attrkind != AC_NotUnique)
			{	if (ind_type -> tc_attrkind == AC_Unique)
					FPutS ("* ", StdError);
				else
				{	switch (ind_type -> tc_attrvarkind)
					{
					case AVK_Plain:
						FPrintF (StdError, "Plain %lu ", (unsigned long) ind_type -> tc_plainattrvar);
						break;
					case AVK_Equation:
						PrintAttributeVarWithEquations (ind_type -> tc_equattrvar);
						ind_type -> tc_equattrvar -> ave_present_mark = True;
						break;
					case AVK_Temporarily:
						if (ind_type -> tc_tempattrvar -> tav_varkind == AC_Unique)
							FPutS ("* ", StdError);
						else if (ind_type -> tc_tempattrvar -> tav_varkind == AC_Variable)
							FPrintF (StdError, "Temp %lu ", (unsigned long) ind_type -> tc_tempattrvar);
						break;
					}
				}
			}
		}
		switch (type -> tc_kind)
		{
		case BasicType:
			PrintSymbol (type -> contents_basic, StdError);
			return;
		case ConstructorType:
		{	SymbDef def = type -> contents_tc_symbdef;
			if (def -> sdef_ident == ListId)
			{	FPutC ('[', StdError);
				if (type -> contents_tc_arity == 1)
					PrintTypeCell (type -> contents_tc_args [0], cDoNotPrintBrackets, with_uni_attrs);
				FPutC (']', StdError);
			}
			else if (def -> sdef_ident == TupleId)
			{	FPutC ('(', StdError);
				PrintTypeCells (type -> contents_tc_arity, type -> contents_tc_args, ',', False, with_uni_attrs);
				FPutC (')', StdError);
			}
			else
			{	ArrayInstance arr_inst;
				
				for (arr_inst = 0; arr_inst < NrOfArrayInstances; arr_inst++)
				{	if (def == ArrayDefs [arr_inst])
					{	switch (arr_inst)
						{
						case LazyArrayInstance:
							FPutC ('{', StdError);
							break;
						case StrictArrayInstance:
							FPutS ("{!", StdError);
							break;
						case UnboxedArrayInstance:
							FPutS ("{#", StdError);
							break;
						}
						if (type -> contents_tc_arity == 1)
							PrintTypeCell (type -> contents_tc_args [0], cDoNotPrintBrackets, with_uni_attrs);

						FPutC ('}', StdError);
						return;
					}	
				}	
				if (brackets && type -> contents_tc_arity > 0)
					FPutC ('(', StdError);
				FPutS (def -> sdef_ident -> ident_name, StdError);
				if (type -> contents_tc_arity > 0)
				{	FPutC (' ', StdError);
					PrintTypeCells (type -> contents_tc_arity, type -> contents_tc_args, ' ', True, with_uni_attrs);
					if (brackets)
						FPutC (')', StdError);
				}
			}
			return;
		}
		case FunctionType:
		{	TypeCell ft_arg = type -> contents_ft_arg;
		
			if (brackets)
				FPutC ('(', StdError);
			PrintTypeCell (ft_arg, ft_arg -> tc_kind == FunctionType, with_uni_attrs);
				FPutS (" -> ", StdError);

			PrintTypeCell (type -> contents_ft_result, cDoNotPrintBrackets, with_uni_attrs);
			if (brackets)
				FPutC (')', StdError);
			return;
		}		
		case TypeVariable:
		case ExistentialVariable:
			if (! type -> tc_printed)
			{	type -> contents_vc_number = NextTypeVariable++;
				type -> tc_printed = True;
				if (type -> tc_overloaded)
					OverloadedTypeVariables = NewPolyListElem (type, OverloadedTypeVariables, TCTempSpace);
			}
			PrintTypeVariable (type);
			return;
		case VoidType:
			FPutS ("Void", StdError);
			return;
		case ConstructorVariable:
			FPutC ('(', StdError);
			PrintTypeCells (type -> contents_cova_arity + 1, type -> contents_cova_types, ' ', False, with_uni_attrs);
			FPutC (')', StdError);
			return;	
		default:
			Assume (False, TC2, "PrintTypeCell");
			return;
		}
	}
	
} /* PrintTypeCell */

static void *AllocInTCTempSpace (SizeT size)
{
	return TH_Alloc (TCTempSpace, size);

} /* AllocInTCTempSpace */

static void PrintSymbolList (SymbolList class_symbs)
{
	SymbolList new_list = NULL;
	ConvertClassSymbolTreeToList (class_symbs, & new_list, AllocInTCTempSpace);
		
	FPutC (' ', StdError);
	PrintTypeClass (new_list -> sl_symbol, StdError);
	
	for (new_list = new_list -> sl_next; new_list; new_list = new_list -> sl_next)
	{	FPutS (" , ", StdError);
		PrintTypeClass (new_list -> sl_symbol, StdError);
	}

} /* PrintTypeContext */

static void PrintTypeContexts (PolyList over_vars)
{
	for (;;)
	{	TypeCell next_var = (TypeCell) over_vars -> pl_elem;
	
		PrintSymbolList (next_var -> contents_overloaded);
		FPutC (' ', StdError);
		PrintTypeVariable (next_var);
		if ((over_vars = over_vars -> pl_next))
			FPutS (" &", StdError);
		else
			break;
	}

} /* PrintTypeContexts */

void PrintTCType (TypeCell type, TypeCell sub_type)
{
	TypeCell prev_sub_type = gSubType;
	gSubType = sub_type;
	NextTypeVariable = 1;
	OverloadedTypeVariables = NULL;
	PrintTypeCell (type, cDoNotPrintBrackets, cDoNotPrintAttributes); 
	if (OverloadedTypeVariables)
	{	FPutS (" |", StdError);
		PrintTypeContexts (OverloadedTypeVariables);
	}
	gSubType = prev_sub_type;

} /* PrintTCType */
