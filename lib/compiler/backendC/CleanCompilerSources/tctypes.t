/*

Version 1.0 26/08/1994

Author: Sjaak Smetsers 

*/

/****
	internal representation for types used by the type checker
****/

STRUCT (symbol_type, SymbolType)
{	
	struct type_cell **	syt_args;
	struct type_cell *	syt_result;
/*
	unsigned long	syt_unspec_args;
*/	
	unsigned short	syt_arity;
	unsigned short syt_mark:1;
	unsigned short syt_part_spec:1;
	
};



STRUCT (type_alt_info, TypeAltInfo)
{
	struct recursive_call *		tai_recursive_calls;
	struct overloaded_function *	tai_overloaded_funs;
	struct internal_call *		tai_internal_calls;
	struct type_alt_info *		tai_next;
	unsigned					tai_line;

};

struct lifted_arguments_info
{
	struct overloaded_variable_list_elem *	lai_overloaded_vars;
	PolyList  						lai_type_vars;
	PolyList  						lai_attribute_vars;
} ;

STRUCT (symbol_type_info,SymbolTypeInfo)
{
	SymbolTypeP						sti_type1;
	SymbolTypeP						sti_type2;
	struct type_cell **					sti_overloaded_vars;
	TypeAltInfo						sti_type_alt_info;
	union
	{	struct lifted_arguments_info * 	sti_u_lifted_args_info;
		PolyList						sti_u_lifted_over_vars;
	} sti_union;
} ;

#define sti_lifted_args_info	sti_union.sti_u_lifted_args_info
#define sti_lifted_over_vars	sti_union.sti_u_lifted_over_vars

STRUCT (type_cons_repr, TypeConsRepr)
{
	SymbDef		tcr_symbol;
	int			tcr_arity;
};

#define sdef_inf_type			sdef_rule_type_info -> sti_type1
#define sdef_predef_type			sdef_rule_type_info -> sti_type2
#define sdef_overloaded_vars		sdef_rule_type_info -> sti_overloaded_vars
#define sdef_type_alt_info		sdef_rule_type_info -> sti_type_alt_info
#define sdef_lifted_args_info		sdef_rule_type_info -> sti_lifted_args_info

#define sdef_lifted_temp_over_vars		sdef_rule_type_info -> sti_lifted_args_info -> lai_overloaded_vars
#define sdef_lifted_type_vars			sdef_rule_type_info -> sti_lifted_args_info -> lai_type_vars
#define sdef_lifted_attr_vars			sdef_rule_type_info -> sti_lifted_args_info -> lai_attribute_vars
#define sdef_lifted_over_vars			sdef_rule_type_info -> sti_lifted_over_vars

#define sdef_rc_inf_type			sdef_rule_cons_type_info -> sti_type1
#define sdef_rc_predef_type		sdef_rule_cons_type_info -> sti_type2

typedef unsigned int  AttributeCellKind;
	enum
	{	AC_Indirection = 0, AC_UnChanging = 0, AC_Unique, AC_Variable, AC_NotUnique
	};

#define AttributeCellKindSize 2

#ifdef THINK_C
#define DAttributeCellKind(v) (\
	v == AC_Indirection ? "AC_Indirection" 	:\
	v == AC_Unique 	? "AC_Unique"		:\
	v == AC_Variable  	? "AC_Variable"	:\
	v == AC_NotUnique 	? "AC_NotUnique"	:\
	"Unknown")
#endif
	
typedef struct plain_attr_var
{	Bool					pav_mark:1;
	Bool					pav_exi_quanti:1;
	AttributeCellKind		pav_varkind:AttributeCellKindSize;
	struct temp_attr_var *	pav_forward;
} *PlainAttrVar;
	
STRUCT (attr_var_with_equatuations, AttrVarWithEquations)
{	struct
	{	Bool				ave_bi_mark:1;
		Bool				ave_bi_coercible:1;
		Bool				ave_bi_present_mark:1;
		Bool				ave_bi_printed:1;
		unsigned			ave_bi_number;
	} ave_bitinfo;
	union
	{	struct temp_attr_var *		ave_inf_forward;
		struct simple_attr_equation *	ave_inf_impl_equa;
	} ave_info;		
	struct simple_attr_equation * ave_equations;
} ;

#define ave_mark		ave_bitinfo.ave_bi_mark
#define ave_coercible	ave_bitinfo.ave_bi_coercible

#define ave_present_mark	ave_bitinfo.ave_bi_present_mark
#define ave_printed		ave_bitinfo.ave_bi_printed

#define ave_number		ave_bitinfo.ave_bi_number
#define ave_refcount	ave_bitinfo.ave_bi_refcount
#define ave_offrefcount	ave_bitinfo.ave_bi_offrefcount
#define ave_forward		ave_info.ave_inf_forward
#define ave_impl_equa	ave_info.ave_inf_impl_equa

STRUCT (attr_equation_list, AttrEquationList)
{	struct
	{	Bool				ae_bi_mark:1;
		Bool				ae_bi_implicit:1;
	} ae_bitinfo;
	
	AttrEquationList		ae_nextoffered;
	struct temp_attr_var *	ae_offered;
	
	union
	{	struct attr_equation_list *	ae_inf_nextdemanded;
		AttrVarWithEquationsP		ae_inf_forward;
	} ae_info;
	
	struct temp_attr_var *	ae_demanded;
};

#define ae_mark		ae_bitinfo.ae_bi_mark
#define ae_implicit		ae_bitinfo.ae_bi_implicit
#define ae_nextdemanded	ae_info.ae_inf_nextdemanded
#define ae_forward		ae_info.ae_inf_forward

STRUCT (simple_attr_equation, SimpleAttrEquation)
{	Bool				 		sae_mark:1;
	AttrVarWithEquationsP		sae_offattr;
	struct simple_attr_equation *	sae_next;
};

STRUCT (temp_attr_var, TempAttrVar)
{	Bool				tav_mark:			1;
	Bool				tav_present:		1;
	Bool				tav_free:			1;
	Bool				tav_onstack:		1;
	Bool				tav_exi_quanti:	1;
	Bool				tav_non_coercible:	1;

	AttributeCellKind	tav_varkind:	AttributeCellKindSize;
	unsigned			tav_number;
	AttrEquationList	tav_offered;
	union
	{	AttrVarWithEquationsP	tav_inf_forward;
		AttrEquationList		tav_inf_demanded;
		TempAttrVar			tav_inf_indirection;
		SimpleAttrEquation		tav_inf_impl_equa;
	} tav_info;
}; 

#define tav_forward		tav_info.tav_inf_forward
#define tav_demanded	tav_info.tav_inf_demanded
#define tav_indirection	tav_info.tav_inf_indirection
#define tav_impl_equa	tav_info.tav_inf_impl_equa

STRUCT (class_variable_info, ClassVariableInfo)
{
	SymbolList	cv_overloaded;
	NodeId		cv_nodeid;
};

typedef union
{	struct
	{	SymbolList 		tv_overloaded;
		union
		{	struct type_cell *	tv_u_forward;
			TypeVar			tv_u_type_var;
			unsigned			tv_u_number;
		} tv_u;
	} cc_variable;
	Symbol cc_basic;
	struct
	{	TypeConsRepr		tcc_symbol;
		struct type_cell **	tcc_args;
	} cc_typeconstructor;
	struct
	{	struct type_cell *	fc_arg;
		struct type_cell *  fc_result;
	} cc_funtype;
	struct
	{	struct type_cell *	sc_indirect;
		struct type_cell *	sc_arg;
	} cc_strictnessinfo;
	struct
	{	union
		{	struct type_cell *	cv_u_forward;
			TypeVar			cv_u_type_var;
		} cv_u;
		ClassVariableInfo	cv_info;
	} cc_classvariable;
	struct
	{	int				cova_arity;
		struct type_cell **	cova_types;
	} cc_constructorvariable;
	struct
	{	struct type_cell *	void_forward;
	} cc_voidtype;
	struct
	{	struct type_cell *	empty_forward;
	} cc_emptytype;
	
} CellContents;

/* Don not change the order of constants in the next enumaration type */

typedef  unsigned int CellKind;
	enum
	{	BasicType, VoidType, FunctionType, ConstructorType,
		TypeVariable, ExistentialVariable, StrictnessCell,
		ClassVariable, ConstructorVariable,
		Indirection, EmptyType
	};

#define CellKindSize 4

#ifdef THINK_C
#define DCellKind(v) (\
	v == BasicType 			? "BasicType" 				:\
	v == ConstructorType 		? "ConstructorType"			:\
	v == FunctionType  			? "FunctionType"			:\
	v == TypeVariable 			? "TypeVariable"			:\
	v == VoidType 				? "VoidType"				:\
	v == StrictnessCell 		? "StrictnessCell"			:\
	v == Indirection 			? "Indirection"			:\
	v == ExistentialVariable 	? "ExistentialVariable"		:\
	v == ClassVariable 			? "ClassVariable"			:\
	v == ConstructorVariable  	? "ConstructorVariable"		:\
	"Unknown")
#endif

#ifndef _NEW_ARRAY_

typedef unsigned int StrictCellKind;
	enum
	{	SC_Lazy, SC_Strict, SC_Var, SC_Indirection
	};

#define StrictCellKindSize 2

#endif

#ifdef THINK_C
#define DStrictCellKind(v) (\
	v == SC_Lazy 		? "SC_Lazy" 		:\
	v == SC_Strict 	? "SC_Strict"		:\
	v == SC_Var  		? "SC_Var"		:\
	v == SC_Indirection ? "SC_Indirection"	:\
	"Unknown")
#endif


typedef unsigned int AttrVarKind;
	enum
	{	AVK_None, AVK_Plain, AVK_Equation, AVK_Temporarily
	};

#define AttrVarKindSize 2

#ifdef THINK_C
#define DAttrVarKind(v) (\
	v == AVK_None 			? "AVK_None" 		:\
	v == AVK_Plain 		? "AVK_Plain"		:\
	v == AVK_Equation  		? "AVK_Equation"	:\
	v == AVK_Temporarily 	? "AVK_Temporarily":\
	"Unknown")
#endif

#define MaxInstanciationDepth 8	

typedef struct
{	Bool				ci_removed:		1;
	Bool				ci_printed:		1;
	Bool				ci_free:			1;
	Bool				ci_mark:			1;
	Bool				ci_expanded:		1;
	Bool				ci_hidden:		1;
	Bool				ci_overloaded:		1;
	Bool				ci_class_var:		1;
	Bool				ci_strict:		1;
	Bool				ci_tmp_cell:		1;
	Bool				ci_copy_cell:		1;
	Bool				ci_with_insres:	1;
	Bool				ci_no_match:		1;
	Bool				ci_non_coercible:	1;
	Bool				ci_default:		1;

	CellKind			ci_kind:		CellKindSize;
	AttributeCellKind	ci_attrkind:	AttributeCellKindSize;
	AttrVarKind		ci_attrvarkind:AttrVarKindSize;
	unsigned			ci_instdepth:	MaxInstanciationDepth;

} CellInfo;

STRUCT (overloaded_type, OverloadedType)
{	TempAttrVar		olt_tempvar;
	struct type_cell *	olt_forward;
};

typedef union
{	TempAttrVar			cai_tempvar;
	PlainAttrVar			cai_plainvar;
	AttrVarWithEquationsP	cai_equvar;
	struct type_cell *		cai_forward;
	OverloadedTypeP		cai_overloadedtypes;
} CellAttrInfo;
			
typedef struct type_cell
{	CellInfo			tc_cellinfo;
	CellContents		tc_contents;
	CellAttrInfo		tc_attrinfo;
} *TypeCell;
	
#define tc_kind			tc_cellinfo.ci_kind
#define tc_removed			tc_cellinfo.ci_removed
#define tc_free			tc_cellinfo.ci_free
#define tc_mark			tc_cellinfo.ci_mark
#define tc_expanded			tc_cellinfo.ci_expanded
#define tc_hidden			tc_cellinfo.ci_hidden
#define tc_overloaded		tc_cellinfo.ci_overloaded
#define tc_class_var		tc_cellinfo.ci_class_var
#define tc_printed			tc_cellinfo.ci_printed
#define tc_tmp_cell			tc_cellinfo.ci_tmp_cell
#define tc_copy_cell		tc_cellinfo.ci_copy_cell
#define tc_strict			tc_cellinfo.ci_strict
#define tc_with_insres		tc_cellinfo.ci_with_insres
#define tc_no_match			tc_cellinfo.ci_no_match
#define tc_non_coercible		tc_cellinfo.ci_non_coercible
#define tc_default			tc_cellinfo.ci_default

#ifndef _NEW_ARRAY_

#define tc_strictkind		tc_cellinfo.ci_strictkind

#endif

#define tc_attrkind			tc_cellinfo.ci_attrkind
#define tc_attrvarkind		tc_cellinfo.ci_attrvarkind
#define tc_instdepth		tc_cellinfo.ci_instdepth

#define tc_tempattrvar			tc_attrinfo.cai_tempvar
#define tc_plainattrvar			tc_attrinfo.cai_plainvar
#define tc_equattrvar			tc_attrinfo.cai_equvar
#define tc_forward				tc_attrinfo.cai_forward
#define tc_overloadedtypes		tc_attrinfo.cai_overloadedtypes


#define tv_forward				tv_u.tv_u_forward
#define tv_type_var				tv_u.tv_u_type_var
#define tv_number				tv_u.tv_u_number

#define	contents_vc_number		tc_contents.cc_variable.tv_number
#define	contents_vc_forward		tc_contents.cc_variable.tv_forward
#define	contents_vc_type_var	tc_contents.cc_variable.tv_type_var
#define	contents_overloaded		tc_contents.cc_variable.tv_overloaded
#define	contents_indirect		tc_contents.cc_variable.tv_forward

#define	contents_basic			tc_contents.cc_basic
#define	contents_tc_symbol		tc_contents.cc_typeconstructor.tcc_symbol

#define	contents_tc_symbdef		contents_tc_symbol -> tcr_symbol
#define	contents_tc_arity		contents_tc_symbol -> tcr_arity

#define	contents_tc_args		tc_contents.cc_typeconstructor.tcc_args
#define	contents_ft_arg		tc_contents.cc_funtype.fc_arg
#define	contents_ft_result		tc_contents.cc_funtype.fc_result
#define	contents_si_indirect	tc_contents.cc_strictnessinfo.sc_indirect
#define	contents_si_arg		tc_contents.cc_strictnessinfo.sc_arg

#define	contents_cv_forward		tc_contents.cc_classvariable.cv_u.cv_u_forward
#define	contents_cv_type_var	tc_contents.cc_classvariable.cv_u.cv_u_type_var
#define	contents_cv_info		tc_contents.cc_classvariable.cv_info

#define	contents_cv_overloaded	contents_cv_info -> cv_overloaded
#define	contents_cv_nodeid		contents_cv_info -> cv_nodeid

#define	contents_cova_arity		tc_contents.cc_constructorvariable.cova_arity
#define	contents_cova_types		tc_contents.cc_constructorvariable.cova_types

#define	contents_void_forward	tc_contents.cc_voidtype.void_forward
#define	contents_empty_forward	tc_contents.cc_emptytype.empty_forward
