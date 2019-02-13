/*
	 (Concurrent) Clean Compiler:  Generation of states
	 ==================================================

	This module generates the (internal) states that are used during the code generation.

	Authors: Sjaak Smetsers & John van Groningen
	At:		 University of Nijmegen, department of computing science
*/

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"
#include "syntaxtr.t"
#include "comsupport.h"
#include "buildtree.h"
#include "codegen_types.h"
#include "codegen1.h"
#include "codegen2.h"
#include "statesgen.h"
#include "checker.h"
#include "instructions.h"
#include "optimisations.h"
#include "sa.h"

/* #include "dbprint.h" */

#define UNBOX_STRICT_CONSTRUCTOR_RECORD
#define UNBOX_STRICT_RECORD_WITH_ONE_FIELD
#define FASTER_STRICT_IF /* also in codegen2.c */
#define FASTER_STRICT_AND_OR
#define UNTUPLE_STRICT_TUPLES /* also in optimisations.c */

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)
#define for_la(v1,v2,l1,l2,n1) for(v1=(l1),v2=(l2);v1!=NULL;v1=v1->n1,++v2)
#define for_li(v,i,l,n) for(v=(l),i=0;v!=NULL;v=v->n,++i)
#define for_ll(v1,v2,l1,l2,n1,n2) for(v1=(l1),v2=(l2);v1!=NULL;v1=v1->n1,v2=v2->n2)
#define for_lla(v1,v2,v3,l1,l2,a3,n1,n2) for(v1=(l1),v2=(l2),v3=(a3);v1!=NULL;v1=v1->n1,v2=v2->n2,++v3)

static void error_in_function (char *m)
{
	ErrorInCompiler ("statesgen.c",m,"");
}

static void error_in_function_s (char *m,char *s)
{
	ErrorInCompiler ("statesgen.c",m,s);
}

static char *Elhsannots	  = "annotations are not allowed at this position";
static char *ECodeBlock	  = "missing type specification";
static char *Wtypeannot	  = "only strict annotations in a type rule will be regarded";
static char *Wparannot	  = "parallel annotations ignored in sequential mode";
static char *Wrootannot	  = "root annotations are ignored";
/*
static char *Econflict     = "conflicting parallel annotations";
static char *WEorIoverrule   = "strict annotation overruled by parallel annotation";
static char *WStrictoverrule = "parallel annotation overruled by strict annotation";
static char *Wapplannot = "parallel annotations on implicitly defined nodeids are ignored";
static char *Wnonterm	  = "non-terminating rule specified";
static char *Wunkannot       = "unknown annotation";
*/

/* some routines for setting the state fields of an object */

StateS BasicSymbolStates [Nr_Of_Predef_Types];

int FirstStateIsStricter (StateS offered_state,StateS demanded_state)
{
	if (offered_state.state_type==SimpleState){
		if (IsSimpleState (demanded_state)){
			switch (offered_state.state_kind){
				case OnB:
				case StrictOnA:
				case StrictRedirection:
					return 1;
				case OnA:
				case SemiStrict:
				case LazyRedirection:
				case Parallel:
				case UnderEval:
					return demanded_state.state_kind==OnA;
				default:
					error_in_function ("FirstStateIsStricter");
					return 0;
			}
		} else {
			return 0;
		}
	} else {
		if (IsSimpleState (demanded_state))
			return 1;
		else if (offered_state.state_type==ArrayState && demanded_state.state_type==ArrayState)
			return 1;
		else if (offered_state.state_type==RecordState && demanded_state.state_type==RecordState)
			return 1;
		else
			return 0;
	}
}

#define BETWEEN(l,h,v) ((unsigned)((v)-(l)) <= (unsigned)((h)-(l)))

int FieldArgumentNodeStatesAreStricter (ArgS *offered_args,ArgS *field_args,States record_states)
{
	ArgS *offered_arg,*field_arg;
	
	for_ll (offered_arg,field_arg,offered_args,field_args,arg_next,arg_next){
		int node_kind;
		Node arg_node;
		int field_number;
		
		field_number=field_arg->arg_node->node_symbol->symb_def->sdef_sel_field_number;

		arg_node=offered_arg->arg_node;
		
		node_kind=arg_node->node_kind;
		if (node_kind!=NodeIdNode){
			if (node_kind==NormalNode && (BETWEEN (int_denot,real_denot,arg_node->node_symbol->symb_kind) || arg_node->node_symbol->symb_kind==string_denot))
				;
			else
				if (!FirstStateIsStricter (arg_node->node_state,record_states[field_number]))
					return 0;		
		} else
			if (!FirstStateIsStricter (arg_node->node_node_id->nid_state,record_states[field_number]))
				return 0;
	}

	return 1;
}

void SetUnaryState (StateS *state_p,StateKind kind,ObjectKind object)
{
	state_p->state_arity = 1;
	state_p->state_kind = kind;
	state_p->state_object = object;
	state_p->state_type = SimpleState;
	state_p->state_mark = 0;
}

static void SetTupleState (StateS *state_p,States args,int arity)
{
	state_p->state_arity = arity;
	state_p->state_tuple_arguments = args;
	state_p->state_type = TupleState;
	state_p->state_mark = 0;
}

static void SetUpdateableTupleState (StateS *state_p,States args,int arity)
{
	state_p->state_arity = arity;
	state_p->state_tuple_arguments = args;
	state_p->state_type = TupleState;
	state_p->state_mark = STATE_ELEMENTS_UPDATEABLE_MASK;
}

static void SetRecordState (StateS *state_p,SymbDef record_sdef,int arity)
{
	RecordStateDescr recdesc;
	
	recdesc = (RecordStateDescr) CompAlloc (sizeof (struct record_state_descr) + (arity-1)*sizeof (struct state));
	recdesc->rs_symb = record_sdef;

	state_p->state_arity = arity;
	state_p->state_record_desc = recdesc;
	state_p->state_type = RecordState;
	state_p->state_mark = 0;
}

static void SetUnboxedArrayState (StateS *state_p,States arg)
{
	state_p->state_arity = 0;
	state_p->state_array_arguments = arg;
	state_p->state_type = ArrayState ;
	state_p->state_mark = 0;
}

StateS LazyState,StrictState; 

#define NewArrayOfStates(n)	(States) CompAlloc (sizeof (StateS)*(n))

static States NewArrayOfUnaryStates (int arity, StateKind init)
{
	int	i;
	States argstates;

	argstates = NewArrayOfStates (arity);

	for (i=0; i<arity; i++)
		SetUnaryState (&argstates [i], init, UnknownObj);

	return argstates;
}

void ConvertAnnotationToState (Annotation annot,StateS *state_p)
{
	if (annot==NoAnnot)
		return;
	else if (annot==StrictAnnot){
		*state_p=StrictState;
		return;
	} else {
		SetUnaryState (state_p, DoParallel ? Parallel : OnA, UnknownObj);
		if (DoParallel)
			state_p->state_mark |= STATE_PARALLEL_MASK;
		return;
	}
}

#if 0
# include "dbprint.h"
extern File rules_file;
#endif

static void GenRecordState (SymbDef sdef);

void ConvertTypeToState (TypeNode type,StateS *state_p,StateKind kind)
{
	Symbol symbol;

	symbol=type->type_node_symbol;

	if (symbol->symb_kind < Nr_Of_Predef_Types){
		*state_p = BasicSymbolStates [symbol->symb_kind];
		if (kind!=StrictOnA)
			state_p->state_kind=kind;
	} else if (symbol->symb_kind==definition && symbol->symb_def->sdef_kind==RECORDTYPE){
		if (kind==StrictOnA){
			GenRecordState (symbol->symb_def);
#if BOXED_RECORDS
			if (symbol->symb_def->sdef_boxed_record)
				SetUnaryState (state_p,StrictOnA,RecordObj);
			else
#endif
				*state_p=symbol->symb_def->sdef_record_state;
		} else
			SetUnaryState (state_p,kind,RecordObj);
	} else
#if ABSTRACT_OBJECT
	if (symbol->symb_kind==definition && symbol->symb_def->sdef_kind==ABSTYPE)
		SetUnaryState (state_p,kind,AbstractObj);
	else
#endif
		SetUnaryState (state_p,kind,UnknownObj);

#ifdef REUSE_UNIQUE_NODES
	if (type->type_node_attribute==UniqueAttr || (symbol->symb_kind==definition && 
			(symbol->symb_def->sdef_kind==TYPE || symbol->symb_def->sdef_kind==RECORDTYPE) &&
			symbol->symb_def->sdef_type->type_lhs->ft_attribute==UniqueAttr))
	{
		state_p->state_mark |= STATE_UNIQUE_MASK;
	}

	if ((state_p->state_mark & STATE_UNIQUE_MASK) && state_p->state_type==SimpleState){
		if (symbol->symb_kind==list_type || symbol->symb_kind==tuple_type || 
			(symbol->symb_kind==definition && (symbol->symb_def->sdef_kind==TYPE || symbol->symb_def->sdef_kind==RECORDTYPE)))
		{
			unsigned long unq_type_args;
			TypeArgs type_arg;
			int i;
			
			unq_type_args=0;
			for_li (type_arg,i,type->type_node_arguments,type_arg_next)
				if (type_arg->type_arg_node->type_node_attribute==UniqueAttr)
					unq_type_args |= 1<<i;
			
			if (unq_type_args!=0){
				state_p->state_mark |= STATE_UNIQUE_TYPE_ARGUMENTS_MASK;
				state_p->state_unq_type_args = unq_type_args;
			}
		}
	}
#endif

	if (kind==StrictOnA && IsSimpleState (*state_p)){
		ObjectKind obj_kind;
		
		obj_kind=state_p->state_object;
		if (obj_kind==TupleObj){
			int i;
			TypeArgs arg;
			
			SetTupleState (state_p, NewArrayOfStates (type->type_node_arity), type->type_node_arity);

			for_li (arg,i,type->type_node_arguments,type_arg_next){
				TypeNode arg_type_node;
				
				arg_type_node=arg->type_arg_node;

				if (!arg_type_node->type_node_is_var){
					ConvertTypeToState (arg_type_node,&state_p->state_tuple_arguments[i],arg_type_node->type_node_annotation==NoAnnot ? OnA : StrictOnA);
#ifdef UNTUPLE_STRICT_TUPLES_
					arg_type_node->type_node_state=state_p->state_tuple_arguments[i];
#endif
				} else
					state_p->state_tuple_arguments[i] = arg_type_node->type_node_annotation==NoAnnot ? LazyState : StrictState;
			}
		} else if (obj_kind==UnboxedArrayObj || obj_kind==StrictArrayObj || obj_kind==ArrayObj){
			TypeNode element_node;
				
			element_node=type->type_node_arguments->type_arg_node;
			
			state_p->state_arity			= 1;
			state_p->state_array_arguments	= NewArrayOfStates (1);
			state_p->state_type				= ArrayState;
			state_p->state_mark=0;

			switch (obj_kind){
				case ArrayObj:
					SetUnaryState (& state_p->state_array_arguments [0], OnA, UnknownObj);
					break;
				case StrictArrayObj:
					state_p->state_array_arguments [0] = StrictState;
					break;
				case UnboxedArrayObj:
					if (element_node->type_node_is_var)
						state_p->state_array_arguments [0] = StrictState;
					else
						ConvertTypeToState (element_node,&state_p->state_array_arguments [0],StrictOnA);
					state_p->state_mark |= STATE_UNBOXED_ARRAY_MASK;
					break;
			}			
		}
	}
}

static void GenRecordState (SymbDef sdef)
{
	if (sdef->sdef_checkstatus < ConvertingToState){
		Types rectype;
		FieldList fields;
		States fieldstates;
		int i,oldline;
		Symbol oldsymbol;
		FlatType lhs;
		int strict_record;

		rectype = sdef->sdef_type;
		lhs = rectype->type_lhs;
		
		oldline		= CurrentLine;
		oldsymbol	= CurrentSymbol;
		
		CurrentSymbol	= lhs->ft_symbol;
		CurrentLine		= 0 /*rectype->type_line*/;

		sdef->sdef_checkstatus = ConvertingToState; /* to detect cyclic strict field dependencies */
		SetRecordState (&sdef->sdef_record_state, sdef, sdef->sdef_arity);
		fieldstates=sdef->sdef_record_state.state_record_arguments;

		strict_record=0;
		
		for_li (fields,i,rectype->type_fields,fl_next){
			TypeNode field_type_node;

			field_type_node = fields->fl_type;

			if (field_type_node->type_node_annotation==StrictAnnot){
				strict_record=1;
				if (!field_type_node->type_node_is_var){
					ConvertTypeToState (field_type_node,&fields->fl_state,StrictOnA);
					
					if (fields->fl_state.state_type==RecordState
#ifdef UNBOX_STRICT_RECORD_WITH_ONE_FIELD
						&& !(fields->fl_state.state_arity==1)
#endif
						)
					{
						SetUnaryState (&fieldstates[i], StrictOnA, RecordObj);
#ifdef REUSE_UNIQUE_NODES
						if (field_type_node->type_node_attribute==UniqueAttr)
							fieldstates[i].state_mark |= STATE_UNIQUE_MASK;
#endif	
					} else
						fieldstates[i]=fields->fl_state;
				} else
					fieldstates[i]=fields->fl_state=field_type_node->type_node_annotation==NoAnnot ? LazyState : StrictState;
#ifdef REUSE_UNIQUE_NODES
				if (field_type_node->type_node_attribute==UniqueAttr){
					fieldstates[i].state_mark |= STATE_UNIQUE_MASK;
					fields->fl_state.state_mark |= STATE_UNIQUE_MASK;
				}
#endif	
			} else {
				fieldstates[i] = LazyState;
#ifdef REUSE_UNIQUE_NODES
				if (field_type_node->type_node_attribute==UniqueAttr)
					fieldstates[i].state_mark |= STATE_UNIQUE_MASK;
#endif	
			}

			fields->fl_symbol->symb_def->sdef_sel_field = fields;
		}

		sdef->sdef_strict_constructor=strict_record;
		sdef->sdef_checkstatus = ConvertedToState; /* to detect cyclic strict field dependencies */
				
		CurrentSymbol = oldsymbol;
		CurrentLine = oldline;
	} else if (sdef->sdef_checkstatus == ConvertedToState)
		return;
	else
		StaticMessage (True, "%S", "%s cyclic strict field dependencies are not allowed", CurrentSymbol, sdef->sdef_ident->ident_name);
}

static void GenResultStatesOfLazyFields (SymbDef sdef)
{
	FieldList fields;
	Types rectype;
	int i;

	rectype = sdef->sdef_type;
	
	CurrentSymbol = rectype->type_lhs->ft_symbol;
	CurrentLine = 0 /*rectype->type_line*/;
	
	for (i=0, fields = rectype->type_fields; fields; i++, fields = fields->fl_next){
		TypeNode field_type_node = fields->fl_type;

		if (field_type_node->type_node_annotation!=StrictAnnot){
			if (field_type_node->type_node_is_var || field_type_node->type_node_symbol->symb_kind==apply_symb)
				SetUnaryState (&fields->fl_state, LazyRedirection, UnknownObj);
			else
				ConvertTypeToState (field_type_node,&fields->fl_state,StrictOnA);
		}
	}
}

static void ChangeFieldRecordStateForStrictAbsTypeFields (SymbDef icl_sdef,SymbDef dcl_sdef)
{
	Types icl_type;
	FieldList icl_field;
	StateP icl_fieldstate_p,dcl_fieldstate_p;

	icl_type = icl_sdef->sdef_type;

	CurrentSymbol = icl_type->type_lhs->ft_symbol;
	CurrentLine = 0 /*icl_type->type_line*/;

	icl_fieldstate_p=icl_sdef->sdef_record_state.state_record_arguments;
	dcl_fieldstate_p=dcl_sdef->sdef_record_state.state_record_arguments;

	for_l (icl_field,icl_type->type_fields,fl_next){
		if (dcl_fieldstate_p->state_type==SimpleState &&
			(icl_fieldstate_p->state_type!=SimpleState ||
			icl_fieldstate_p->state_kind!=dcl_fieldstate_p->state_kind))
		{
			StaticMessage (False, "%S", "%S strict field is boxed because the field type is an abstract type",
				CurrentSymbol, icl_field->fl_symbol);
			
			*icl_fieldstate_p=*dcl_fieldstate_p;
		}
		
		++icl_fieldstate_p;
		++dcl_fieldstate_p;
	}
}

static void GenerateStatesForConstructors (SymbDef sdef)
{
	ConstructorList constructor;

	CurrentLine	 = 0 /*sdef->sdef_type->type_line*/;

	for_l (constructor,sdef->sdef_type->type_constructors,cl_next){
		int strict_constructor;
		SymbDef constructor_sdef;
		TypeNode type_node;
		TypeArgs arg;
		StateP state_p;

		type_node=constructor->cl_constructor;
		CurrentSymbol=type_node->type_node_symbol;

		constructor_sdef=CurrentSymbol->symb_def;

		state_p = NewArrayOfStates (constructor_sdef->sdef_arity);
		constructor->cl_state_p = state_p;		

		constructor_sdef->sdef_constructor=constructor;

		strict_constructor=0;
	
		for_l (arg,type_node->type_node_arguments,type_arg_next){
			TypeNode type_arg_node;
			
			type_arg_node=arg->type_arg_node;
			
			if (type_arg_node->type_node_annotation==StrictAnnot){	
				strict_constructor=1;
				
				if (!type_arg_node->type_node_is_var){
					ConvertTypeToState (type_arg_node,state_p,StrictOnA);
					
					if (state_p->state_type==RecordState)
#ifdef UNBOX_STRICT_CONSTRUCTOR_RECORD
					if (type_node->type_node_arguments->type_arg_next!=NULL)
#endif
#ifdef UNBOX_STRICT_RECORD_WITH_ONE_FIELD
					if (!(state_p->state_arity==1))
#endif
						SetUnaryState (state_p, StrictOnA, RecordObj);
				} else {
					*state_p=StrictState;
				}
			} else
				*state_p=LazyState;

			++state_p;
		}
		
		constructor_sdef->sdef_strict_constructor=strict_constructor;
	}
}

static void ChangeElementStateForStrictAbsTypeFields (SymbDef icl_sdef,SymbDef dcl_sdef)
{
	Types icl_type = icl_sdef->sdef_type, dcl_type = dcl_sdef->sdef_type;
	ConstructorList icl_cons, dcl_cons;

	CurrentLine = 0 /*icl_type->type_line*/;

	for (icl_cons = icl_type->type_constructors, dcl_cons = dcl_type->type_constructors; dcl_cons;
		 icl_cons = icl_cons->cl_next, dcl_cons = dcl_cons->cl_next)
	{
		TypeNode icl_node,dcl_node;
		SymbDef icl_constructor,dcl_constructor;
		
		icl_node=icl_cons->cl_constructor;
		icl_constructor=icl_node->type_node_symbol->symb_def;
		
		if (icl_constructor->sdef_strict_constructor){
			TypeArgs icl_arg,dcl_arg;
			StateP icl_arg_state_p,dcl_arg_state_p;
			
			dcl_node=dcl_cons->cl_constructor;
			CurrentSymbol=dcl_node->type_node_symbol;
			dcl_constructor=CurrentSymbol->symb_def;
					
			icl_arg=icl_node->type_node_arguments;
			dcl_arg=dcl_node->type_node_arguments;
			icl_arg_state_p=icl_cons->cl_state_p;
			dcl_arg_state_p=dcl_cons->cl_state_p;
			
			while (icl_arg!=NULL){
				TypeNode icl_element_node,dcl_element_node;
				
				icl_element_node=icl_arg->type_arg_node;
				dcl_element_node=dcl_arg->type_arg_node;

				if (dcl_arg_state_p->state_type==SimpleState &&
					(icl_arg_state_p->state_type!=SimpleState || icl_arg_state_p->state_kind!=dcl_arg_state_p->state_kind))
				{
					StaticMessage (False, "%S", "%S element is boxed because its type is an abstract type",
						CurrentSymbol, icl_element_node->type_node_symbol);
					
					*icl_arg_state_p=*dcl_arg_state_p;
				}
				
				icl_arg=icl_arg->type_arg_next;
				dcl_arg=dcl_arg->type_arg_next;
				++icl_arg_state_p;
				++dcl_arg_state_p;
			}
		}				
	}
}

#ifdef CLEAN2
SymbDefP special_types[2];
#endif

void GenerateStatesForRecords (Symbol symbols)
{
	Symbol symb;
	
	for_l (symb,symbols,symb_next)
		if (symb->symb_kind==definition){
			SymbDef def;

			def=symb->symb_def;
			if (def->sdef_kind==RECORDTYPE){
				GenRecordState (def);
				GenResultStatesOfLazyFields (def);

				if (def->sdef_exported){
					SymbDef dcl_sdef;
					
					dcl_sdef=def->sdef_dcl_icl;

					if (dcl_sdef!=NULL && dcl_sdef->sdef_kind==RECORDTYPE){
						GenRecordState (dcl_sdef);
						GenResultStatesOfLazyFields (dcl_sdef);
						ChangeFieldRecordStateForStrictAbsTypeFields (def,dcl_sdef);
					}
				}
			} else if (def->sdef_kind==TYPE){
				GenerateStatesForConstructors (def);
				
				if (def->sdef_exported){
					SymbDef dcl_sdef;
					
					dcl_sdef=def->sdef_dcl_icl;
					
					if (dcl_sdef->sdef_kind==TYPE){
						GenerateStatesForConstructors (dcl_sdef);
						ChangeElementStateForStrictAbsTypeFields (def,dcl_sdef);
					}
				}
			}
		}

#ifdef CLEAN2
	if (special_types[0]!=NULL)
		BasicSymbolStates[integer_denot] = special_types[0]->sdef_record_state;
#endif
}

/*
static Bool AnnotHasDeferAttr (Annotation annotkind)
{
	switch (annotkind){
		case InterleavedAnnot:
		case LazyInterleavedAnnot:
		case ContinueAnnot:
		case DeferAnnot:
		case WaitAnnot:
		case ContInterleavedAnnot:
		case InterleavedNFAnnot:
			return True;
		default:
			return False;
	}
}
*/	

static StateS DetermineStatesOfRuleType (TypeAlts ruletype,StateS *const function_state_p)
{
	TypeNode lhsroot;
	TypeArgs type_arg;
	StateP arg_state_p;

	lhsroot = ruletype->type_alt_lhs;
	
	CurrentSymbol	= lhsroot ->type_node_symbol;
	CurrentLine = 0 /*ruletype->type_alt_line*/;
	
	if (lhsroot->type_node_annotation!=NoAnnot)
		StaticMessage (False, "%S", Wrootannot, CurrentSymbol);

	arg_state_p=function_state_p;
	for_l (type_arg,lhsroot->type_node_arguments,type_arg_next){
		if (!(type_arg->type_arg_node->type_node_annotation==NoAnnot || type_arg->type_arg_node->type_node_annotation==StrictAnnot))
			StaticMessage (False, "%S", Wtypeannot, CurrentSymbol);
	
		if (!type_arg->type_arg_node->type_node_is_var)
			ConvertTypeToState (type_arg->type_arg_node,arg_state_p,type_arg->type_arg_node->type_node_annotation==NoAnnot ? OnA : StrictOnA);
		else
			*arg_state_p = type_arg->type_arg_node->type_node_annotation==NoAnnot ? LazyState : StrictState;
		
		++arg_state_p;
	}

	if (ruletype->type_alt_rhs->type_node_is_var || ruletype->type_alt_rhs->type_node_symbol->symb_kind==apply_symb){
		function_state_p[-1] = StrictState;
		function_state_p[-1].state_kind = StrictRedirection;
	} else
		ConvertTypeToState (ruletype->type_alt_rhs,&function_state_p[-1],StrictOnA);

	return function_state_p[-1];
}

typedef struct type_node *TypeNodeP;

#ifdef REUSE_UNIQUE_NODES
# ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
static void determine_unique_state_of_constructor_argument (StateP result_state_p,StateP type_state_p,TypeNodeP type_arg_node,int lhs_type_attribute)
{
	if (type_arg_node->type_node_is_var){
		if ((type_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) &&
			type_arg_node->type_node_tv->tv_argument_nr>=0 &&
			(type_state_p->state_unq_type_args & (1<<(type_arg_node->type_node_tv->tv_argument_nr))))
		{
			result_state_p->state_mark |= STATE_UNIQUE_MASK;
		}
	} else {
		AttributeKind arg_type_attribute;
		
		arg_type_attribute=type_arg_node->type_node_attribute;
		
		if (arg_type_attribute==UniqueAttr || (arg_type_attribute>=FirstUniVarNumber && arg_type_attribute==lhs_type_attribute))
			result_state_p->state_mark |= STATE_UNIQUE_MASK;
		
		if ((result_state_p->state_mark & STATE_UNIQUE_MASK) && result_state_p->state_type==SimpleState){
			Symbol symbol;
			
			symbol=type_arg_node->type_node_symbol;

			if (symbol->symb_kind==list_type || symbol->symb_kind==tuple_type || (symbol->symb_kind==definition && 
				(symbol->symb_def->sdef_kind==TYPE || symbol->symb_def->sdef_kind==RECORDTYPE)))
			{
				unsigned long unq_type_args;
				TypeArgs type_arg;
				int i;
				
				unq_type_args=0;
				for_li (type_arg,i,type_arg_node->type_node_arguments,type_arg_next){
					TypeNodeP type_arg_node_p;
					
					type_arg_node_p=type_arg->type_arg_node;
					if (type_arg_node_p->type_node_is_var){
						if ((type_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) &&
							type_arg_node_p->type_node_tv->tv_argument_nr>=0 &&
							(type_state_p->state_unq_type_args & (1<<(type_arg_node_p->type_node_tv->tv_argument_nr))))
						{
							unq_type_args |= 1<<i;
						}
					} else {
						AttributeKind arg_type_attribute;

						arg_type_attribute=type_arg_node_p->type_node_attribute;
						if (arg_type_attribute==UniqueAttr || (arg_type_attribute>=FirstUniVarNumber && arg_type_attribute==lhs_type_attribute))
							unq_type_args |= 1<<i;
					}
				}
				
				if (unq_type_args!=0){
					result_state_p->state_mark |= STATE_UNIQUE_TYPE_ARGUMENTS_MASK;
					result_state_p->state_unq_type_args = unq_type_args;
				}
			}
		}
	}
}
# else
static StateP determine_unique_state_of_constructor_argument
				(StateP type_state_p,TypeNodeP type_arg_node,int lhs_type_attribute,StateP constructor_state_p)
{
	if (type_arg_node->type_node_is_var){
		if ((constructor_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) &&
			type_arg_node->type_node_tv->tv_argument_nr>=0 &&
			(constructor_state_p->state_unq_type_args & (1<<(type_arg_node->type_node_tv->tv_argument_nr))) &&
			(type_state_p->state_mark & STATE_UNIQUE_MASK)==0)
		{
			StateP result_state_p;
			
			result_state_p=CompAllocType (StateS);
			*result_state_p=*type_state_p;
			result_state_p->state_mark |= STATE_UNIQUE_MASK;
			
			return result_state_p;
		} else
			return type_state_p;
	} else {
		AttributeKind arg_type_attribute;
		StateP result_state_p;
		
		arg_type_attribute=type_arg_node->type_node_attribute;
		
		if (arg_type_attribute==UniqueAttr || (arg_type_attribute>=FirstUniVarNumber && arg_type_attribute==lhs_type_attribute)){
			result_state_p=CompAllocType (StateS);
			*result_state_p=*type_state_p;
			result_state_p->state_mark |= STATE_UNIQUE_MASK;
			
			type_state_p=result_state_p;
		} else
			result_state_p=NULL;
		
		if ((type_state_p->state_mark & STATE_UNIQUE_MASK) && type_state_p->state_type==SimpleState){
			Symbol symbol;
			
			symbol=type_arg_node->type_node_symbol;

			if (symbol->symb_kind==list_type || symbol->symb_kind==tuple_type || (symbol->symb_kind==definition && 
				(symbol->symb_def->sdef_kind==TYPE || symbol->symb_def->sdef_kind==RECORDTYPE)))
			{
				unsigned long unq_type_args;
				TypeArgs type_arg;
				int i;
				
				unq_type_args=0;
				for_li (type_arg,i,type_arg_node->type_node_arguments,type_arg_next){
					TypeNodeP type_arg_node_p;
					
					type_arg_node_p=type_arg->type_arg_node;
					if (type_arg_node_p->type_node_is_var){
						if ((constructor_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) &&
							type_arg_node_p->type_node_tv->tv_argument_nr>=0 &&
							(constructor_state_p->state_unq_type_args & (1<<(type_arg_node_p->type_node_tv->tv_argument_nr))))
						{
							unq_type_args |= 1<<i;
						}
					} else {
						AttributeKind arg_type_attribute;

						arg_type_attribute=type_arg_node_p->type_node_attribute;
						if (arg_type_attribute==UniqueAttr || (arg_type_attribute>=FirstUniVarNumber && arg_type_attribute==lhs_type_attribute))
							unq_type_args |= 1<<i;
					}
				}
				
				if (unq_type_args!=0){
					if (result_state_p==NULL){
						result_state_p=CompAllocType (StateS);
						*result_state_p=*type_state_p;
					}
					result_state_p->state_mark |= STATE_UNIQUE_TYPE_ARGUMENTS_MASK;
					result_state_p->state_unq_type_args = unq_type_args;
					
					return result_state_p;
				}
			}
		}
		
		return type_state_p;
	}
}
# endif
#endif

#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
static void GenStatesInLhsSubArguments (Node argnode,StateS states[]);
static void GenStatesInLhsSubArg (Node argnode,StateP arg_state_p);

static void GenStatesInLhsNode (Node node,StateP arg_state_p)
{
	Args arg;

	if (node->node_annotation!=NoAnnot)
		StaticMessage (True, "%S", "%S %s",CurrentSymbol,node->node_symbol, Elhsannots);

	if (node->node_kind==NormalNode){
		Symbol symbol;
		
		symbol=node->node_symbol;
		if (symbol->symb_kind==definition){
			SymbDef sdef;

			sdef=symbol->symb_def;

			if (sdef->sdef_kind==CONSTRUCTOR){
# ifdef REUSE_UNIQUE_NODES
				AttributeKind lhs_type_attribute;
				
				lhs_type_attribute=sdef->sdef_type->type_lhs->ft_attribute;

				if (lhs_type_attribute==UniqueAttr)
					arg_state_p->state_mark |= STATE_UNIQUE_MASK;
				
				if (sdef->sdef_arity==node->node_arity && (arg_state_p->state_mark & STATE_UNIQUE_MASK)){
					if (sdef->sdef_strict_constructor){
						struct type_arg *type_arg_p;
						StateP constructor_arg_state_p;
						ArgS *arg;
						
						for_lla (arg,type_arg_p,constructor_arg_state_p,
								node->node_arguments,sdef->sdef_constructor->cl_constructor->type_node_arguments,symbol->symb_def->sdef_constructor->cl_state_p,
								arg_next,type_arg_next)
						{
							Node arg_node;
							
							arg->arg_state = *constructor_arg_state_p;
							
							determine_unique_state_of_constructor_argument (&arg->arg_state,arg_state_p,type_arg_p->type_arg_node,lhs_type_attribute);
														
							arg_node=arg->arg_node;
							
							if (arg_node->node_kind==NodeIdNode){
								arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
								arg_node=arg_node->node_node_id->nid_node;
								if (!arg_node)
									continue;
							}

							GenStatesInLhsSubArg (arg_node,&arg->arg_state);
						}							
					} else {
						struct type_arg *type_arg_p;
						ArgS *arg;
						
						for_ll (arg,type_arg_p,node->node_arguments,sdef->sdef_constructor->cl_constructor->type_node_arguments,arg_next,type_arg_next){
							Node arg_node;
							
							arg->arg_state=LazyState;							
							
							determine_unique_state_of_constructor_argument (&arg->arg_state,arg_state_p,type_arg_p->type_arg_node,lhs_type_attribute);

							arg_node=arg->arg_node;

							if (arg_node->node_kind==NodeIdNode){
								arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
								arg_node=arg_node->node_node_id->nid_node;
								if (!arg_node)
									continue;
							}
							
							GenStatesInLhsNode (arg_node,&arg->arg_state);
						}						
					}
					return;
				}
# endif				
				if (sdef->sdef_strict_constructor && sdef->sdef_arity==node->node_arity){
					GenStatesInLhsSubArguments (node,symbol->symb_def->sdef_constructor->cl_state_p);
					return;
				}
			} else if (sdef->sdef_kind==RECORDTYPE){
# ifdef REUSE_UNIQUE_NODES
				AttributeKind lhs_type_attribute;
				
				lhs_type_attribute=sdef->sdef_type->type_lhs->ft_attribute;

				if (lhs_type_attribute==UniqueAttr)
					arg_state_p->state_mark |= STATE_UNIQUE_MASK;
				
				if (arg_state_p->state_mark & STATE_UNIQUE_MASK){
					StateP arg_state_p;
					ArgS *arg;
					FieldList field;

					for_lla (arg,field,arg_state_p,node->node_arguments,
							 sdef->sdef_type->type_fields,sdef->sdef_record_state.state_record_arguments,arg_next,fl_next)
					{
						Node arg_node;
						
						arg->arg_state = *arg_state_p;

						determine_unique_state_of_constructor_argument
							(&arg->arg_state,arg_state_p,field->fl_type,lhs_type_attribute,node_id_state_p);
						
						arg_node=arg->arg_node;

						if (arg_node->node_kind==NodeIdNode){
							arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
							arg_node=arg_node->node_node_id->nid_node;
							if (!arg_node)
								continue;
						}

						GenStatesInLhsSubArg (arg_node,&arg->arg_state);
					}							
				} else				
# endif
				GenStatesInLhsSubArguments (node,sdef->sdef_record_state.state_record_arguments);
				return;
			}
		}
# ifdef REUSE_UNIQUE_NODES
		else if (symbol->symb_kind==cons_symb && (arg_state_p->state_mark & STATE_UNIQUE_MASK) && node->node_arity==2){
			Node arg_node;

			arg=node->node_arguments;
			
			arg->arg_state=LazyState;
			if ((arg_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) && (arg_state_p->state_unq_type_args & 1)){
				arg->arg_state.state_mark |= STATE_UNIQUE_MASK;
			}

			arg_node=arg->arg_node;
			if (arg_node->node_kind==NodeIdNode){
				arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
				arg_node=arg_node->node_node_id->nid_node;
			}
			if (arg_node!=NULL)
				GenStatesInLhsNode (arg_node,&arg->arg_state);

			arg=arg->arg_next;
			
			arg->arg_state=LazyState;
			arg->arg_state.state_mark |= STATE_UNIQUE_MASK;
			if ((arg_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) && (arg_state_p->state_unq_type_args & 1)){
				arg->arg_state.state_mark |= STATE_UNIQUE_TYPE_ARGUMENTS_MASK;
				arg->arg_state.state_unq_type_args = 1;
			}
			
			arg_node=arg->arg_node;
			if (arg_node->node_kind==NodeIdNode){
				arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
				arg_node=arg_node->node_node_id->nid_node;
			}
			if (arg_node!=NULL)
				GenStatesInLhsNode (arg_node,&arg->arg_state);
		
			return;
		} else if (symbol->symb_kind==tuple_symb && (arg_state_p->state_mark & STATE_UNIQUE_MASK)){
			int i;

			for_li (arg,i,node->node_arguments,arg_next){
				Node arg_node;
				
				arg->arg_state=LazyState;
				if ((arg_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) && (arg_state_p->state_unq_type_args & (1<<i))){
					arg->arg_state.state_mark |= STATE_UNIQUE_MASK;
				}

				arg_node=arg->arg_node;
				if (arg_node->node_kind==NodeIdNode){
					arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
					arg_node=arg_node->node_node_id->nid_node;
				}
				if (arg_node!=NULL)
					GenStatesInLhsNode (arg_node,&arg->arg_state);
			}
			
			return;
		}
# endif
	}

	for_l (arg,node->node_arguments,arg_next){
		Node arg_node;
		
		arg->arg_state=LazyState;
		
		arg_node=arg->arg_node;

		if (arg_node->node_kind==NodeIdNode){
			arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
			arg_node=arg_node->node_node_id->nid_node;
			if (!arg_node)
				continue;
		}
		
		GenStatesInLhsNode (arg_node,&arg->arg_state);
	}
}

static void GenStatesInLhsSubArg (Node arg_node,StateP arg_state_p)
{
	if (arg_node->node_annotation!=NoAnnot)
		StaticMessage (True, "%S", Elhsannots, CurrentSymbol);

	switch (arg_state_p->state_type){
		case RecordState:
			GenStatesInLhsSubArguments (arg_node,arg_node->node_symbol->symb_def->sdef_record_state.state_record_arguments);
			return;
		case TupleState:
			GenStatesInLhsSubArguments (arg_node,arg_state_p->state_tuple_arguments);
			return;
		default:
			GenStatesInLhsNode (arg_node,arg_state_p);
			return;
	}
}

static void GenStatesInLhsSubArguments (Node node,StateS states[])
{
	StateP arg_state_p;
	ArgS *arg;

	for (arg=node->node_arguments,arg_state_p=states; arg!=NULL; arg=arg->arg_next,++arg_state_p){
		Node arg_node;
		
		arg->arg_state = *arg_state_p;
			
		arg_node=arg->arg_node;

		if (arg_node->node_kind==NodeIdNode){
			arg_node->node_node_id->nid_lhs_state_p_=&arg->arg_state;
			arg_node=arg_node->node_node_id->nid_node;
			if (!arg_node)
				continue;
		}

		GenStatesInLhsSubArg (arg_node,&arg->arg_state);
	}	
}

static void DetermineLhsStatesOfRule (ImpRules rule)
{
	RuleAlts alt;
	StateP function_state_p;
	
	function_state_p=rule->rule_state_p;
	
	CurrentSymbol = rule->rule_root->node_symbol;
	
	for_l (alt,rule->rule_alts,alt_next){
		CurrentLine = alt->alt_line;
		
		GenStatesInLhsSubArguments (alt->alt_lhs_root,function_state_p);

		alt->alt_lhs_root->node_state = function_state_p[-1]; /* i.e. the result kind */
	}
}
#endif

unsigned next_def_number;

void ExamineTypesAndLhsOfImpRuleSymbolDefinitionAgain (SymbDef def)
{
	StateS rootstate;

	rootstate = DetermineStatesOfRuleType (def->sdef_rule->rule_type,def->sdef_rule->rule_state_p);

#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	DetermineLhsStatesOfRule (def->sdef_rule);
#endif

	if (def->sdef_exported && def->sdef_dcl_icl!=NULL)
		rootstate=def->sdef_dcl_icl->sdef_rule_type->rule_type_state_p[-1];

	if (IsSimpleState (rootstate)){
		if (rootstate.state_kind == OnA || rootstate.state_kind == StrictOnA){
			def->sdef_calledwithrootnode = True;
			def->sdef_returnsnode = True;
		} else if (rootstate.state_kind == StrictRedirection){
			def->sdef_calledwithrootnode = False;
			def->sdef_returnsnode = True;
		} else {
			def->sdef_calledwithrootnode = False;
			def->sdef_returnsnode = False;
		}
	} else {
		def->sdef_calledwithrootnode = False;
		def->sdef_returnsnode = False;
	}
}

#define allocate_function_state(arity) (((StateP)(CompAlloc (sizeof(StateS)*((arity)+1))))+1)

void ExamineTypesAndLhsOfSymbolDefinition (SymbDef def)
{
	StateS rootstate;

	def->sdef_number = 0;

	if (def->sdef_exported && def->sdef_dcl_icl!=def)
		def->sdef_mark |= SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK;

	switch (def->sdef_kind){
		case SYSRULE:
			def->sdef_ident->ident_symbol = NULL;
			def->sdef_ident->ident_environ = CurrentModule;
		case DEFRULE:
			if (def->sdef_isused){
				def->sdef_rule_type->rule_type_state_p = allocate_function_state (def->sdef_arity);
				rootstate = DetermineStatesOfRuleType (def->sdef_rule_type->rule_type_rule,def->sdef_rule_type->rule_type_state_p);
			} else
				return;
			break;
		case IMPRULE:
			if (def->sdef_module==CurrentModule)
				def->sdef_number = next_def_number++;

			def->sdef_rule->rule_state_p = allocate_function_state (def->sdef_arity);
			rootstate = DetermineStatesOfRuleType (def->sdef_rule->rule_type,def->sdef_rule->rule_state_p);
			
			if (def->sdef_exported && def->sdef_dcl_icl!=NULL){
				def->sdef_dcl_icl->sdef_rule_type->rule_type_state_p = allocate_function_state (def->sdef_arity);
				rootstate = DetermineStatesOfRuleType (def->sdef_dcl_icl->sdef_rule_type->rule_type_rule,def->sdef_dcl_icl->sdef_rule_type->rule_type_state_p);
			}
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			DetermineLhsStatesOfRule (def->sdef_rule);
#endif
			break;
		case RECORDTYPE:
		{
			FieldList fields;

			if (def->sdef_module==CurrentModule)
				def->sdef_number = next_def_number++;
			for_l (fields,def->sdef_type->type_fields,fl_next)
				ExamineTypesAndLhsOfSymbolDefinition (fields->fl_symbol->symb_def);

			if (def->sdef_boxed_record){
				def->sdef_calledwithrootnode = True;
				def->sdef_returnsnode = True;
				return;
			}

			rootstate = def->sdef_record_state;
			break;
		}
		case FIELDSELECTOR:
			if (def->sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE){
				def->sdef_member_states_of_field = allocate_function_state (def->sdef_member_type_of_field->type_alt_lhs->type_node_arity);
				(void) DetermineStatesOfRuleType (def->sdef_member_type_of_field,def->sdef_member_states_of_field);
			}

			rootstate = def->sdef_sel_field->fl_state;

			if (def->sdef_module==CurrentModule)
				def->sdef_number = next_def_number++;
			if (def->sdef_exported && def->sdef_dcl_icl!=NULL && def->sdef_dcl_icl->sdef_sel_field)
				rootstate = def->sdef_dcl_icl->sdef_sel_field->fl_state;
			break;
		case TYPE:
			if (def->sdef_module==CurrentModule)
				def->sdef_number = next_def_number++;
			rootstate = LazyState;
			break;
		case CONSTRUCTOR:
			if (def->sdef_module==CurrentModule)
				def->sdef_number = next_def_number++;
			rootstate = OnAState;
			break;
		default:
			rootstate = OnAState;
			break;
	}

	if (IsSimpleState (rootstate)){
		if (rootstate.state_kind == OnA || rootstate.state_kind == StrictOnA){
			def->sdef_calledwithrootnode = True;
			def->sdef_returnsnode = True;
		} else if (rootstate.state_kind == StrictRedirection){
			def->sdef_calledwithrootnode = False;
			def->sdef_returnsnode = True;
		} else {
			def->sdef_calledwithrootnode = False;
			def->sdef_returnsnode = False;
		}
	} else {
		def->sdef_calledwithrootnode = False;
		def->sdef_returnsnode = False;
	}
}

#if STRICT_LISTS
extern PolyList unboxed_record_cons_list,unboxed_record_decons_list;
#endif

void ExamineTypesAndLhsOfSymbols (Symbol symbs)
{
	next_def_number = 1;

	while (symbs!=NULL){
		if (symbs->symb_kind==definition)
			ExamineTypesAndLhsOfSymbolDefinition (symbs->symb_def);

		symbs=symbs->symb_next;
	}
#if STRICT_LISTS
	{
		PolyList unboxed_record_cons_elem,unboxed_record_decons_elem;
		
		for_l (unboxed_record_cons_elem,unboxed_record_cons_list,pl_next)
			ExamineTypesAndLhsOfSymbolDefinition (unboxed_record_cons_elem->pl_elem);
		for_l (unboxed_record_decons_elem,unboxed_record_decons_list,pl_next)
			ExamineTypesAndLhsOfSymbolDefinition (unboxed_record_decons_elem->pl_elem);
	}
#endif
}

PolyList UserDefinedArrayFunctions;

char *current_imported_module; /* also used by instructions.c */

void ImportSymbols (Symbol symbols)
{
	Symbol symbol;	
	PolyList array_fun;

	current_imported_module = NULL;
	
	for_l (array_fun,UserDefinedArrayFunctions,pl_next){
		SymbDef fun_def;
		
		fun_def = ((Symbol) array_fun->pl_elem)->symb_def;
		
		if (fun_def ->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK))
			fun_def -> sdef_module = CurrentModule;
	}

	for_l (symbol,symbols,symb_next){
		SymbDef sdef;

		if (symbol->symb_kind!=definition)
			continue;

		sdef=symbol->symb_def;
		if (sdef->sdef_module!=CurrentModule){
			if (sdef->sdef_isused
				&& sdef->sdef_mark & (SDEF_USED_STRICTLY_MASK | SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK)
			){
				if (sdef->sdef_module!=current_imported_module){
					current_imported_module=sdef->sdef_module;
					GenImpMod (current_imported_module);
				}
				GenImport (sdef);
			}

			if (sdef->sdef_kind==RECORDTYPE){
				FieldList fields;
	
				for_l (fields,sdef->sdef_type->type_fields,fl_next){
					SymbDef field_sdef;

					field_sdef=fields->fl_symbol->symb_def;

					if (field_sdef->sdef_isused && field_sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
						if (sdef->sdef_module!=current_imported_module){
							current_imported_module=sdef->sdef_module;
							GenImpMod (current_imported_module);
						}
						GenImport (field_sdef);
					}
				}
			}
		}
	}
}

void import_not_yet_imported_record_r_labels (Symbol symbols)
{
	Symbol symbol;	
	
	for_l (symbol,symbols,symb_next){
		if (symbol->symb_kind==definition){
			SymbDef sdef;

			sdef=symbol->symb_def;
			if ((sdef->sdef_mark & (SDEF_USED_STRICTLY_MASK | SDEF_RECORD_R_LABEL_IMPORTED_MASK))==SDEF_USED_STRICTLY_MASK
				&& sdef->sdef_kind==RECORDTYPE && sdef->sdef_module!=CurrentModule)
			{
				if (sdef->sdef_module!=current_imported_module){
					current_imported_module=sdef->sdef_module;
					GenImpMod (current_imported_module);
				}
				GenImport (sdef);
			}
		}
	}
}

static Bool ShouldDecrRefCount;

#if OPTIMIZE_LAZY_TUPLE_RECURSION
# define IF_OPTIMIZE_LAZY_TUPLE_RECURSION(a) ,a
#else
# define IF_OPTIMIZE_LAZY_TUPLE_RECURSION(a)
#endif

#if OPTIMIZE_LAZY_TUPLE_RECURSION
static int roots_are_tuples_or_calls_to_this_function (NodeP node_p,NodeDefP node_defs,SymbDef function_sdef_p)
{
	switch (node_p->node_kind){
		case SwitchNode:
		{
			ArgP arg_p;
			
			for_l (arg_p,node_p->node_arguments,arg_next)
				if (!roots_are_tuples_or_calls_to_this_function (arg_p->arg_node->node_arguments->arg_node,arg_p->arg_node->node_node_defs,function_sdef_p))
					return 0;
			
			return 1;
		}
		case PushNode:
			return roots_are_tuples_or_calls_to_this_function (node_p->node_arguments->arg_next->arg_node,node_defs,function_sdef_p);
		case GuardNode:
		{			
			while (node_p->node_kind==GuardNode){
				if (!roots_are_tuples_or_calls_to_this_function (node_p->node_arguments->arg_node,node_defs,function_sdef_p))
					return 0;
		
				node_defs=node_p->node_node_defs;
				node_p=node_p->node_arguments->arg_next->arg_node;
			}

			return roots_are_tuples_or_calls_to_this_function (node_p,node_defs,function_sdef_p);
		}
		case IfNode:
		{
			ArgP then_arg_p;
			NodeP else_node_p;
			
			then_arg_p=node_p->node_arguments->arg_next;
			
			if (!roots_are_tuples_or_calls_to_this_function (then_arg_p->arg_node,node_p->node_then_node_defs,function_sdef_p))
				return 0;
			
			else_node_p=then_arg_p->arg_next->arg_node;

			if (else_node_p->node_kind==NormalNode && else_node_p->node_symbol->symb_kind==fail_symb)
				return 1;

			return roots_are_tuples_or_calls_to_this_function (else_node_p,node_p->node_else_node_defs,function_sdef_p);
		}
		default:
		{
			if (node_p->node_kind==NormalNode){
				SymbolP node_symbol_p;
				
				node_symbol_p=node_p->node_symbol;
				if	(node_symbol_p->symb_kind==tuple_symb)
					return 1;
				else if (node_symbol_p->symb_kind==definition && node_symbol_p->symb_def==function_sdef_p
						&& node_p->node_arity==node_symbol_p->symb_def->sdef_arity)
					return 1;
			}
		}
	}
		
	return 0;
}
#endif

static void DecrRefCountCopiesOfArgs (Args args IF_OPTIMIZE_LAZY_TUPLE_RECURSION(int local_scope));

static void DecrRefCountCopiesOfArg (Args arg IF_OPTIMIZE_LAZY_TUPLE_RECURSION(int local_scope))
{
	Node node;
	
	node=arg->arg_node;
	
	if (node->node_kind!=NodeIdNode){
#if OPTIMIZE_LAZY_TUPLE_RECURSION
		if (OptimizeLazyTupleRecursion && node->node_kind==NormalNode && node->node_symbol->symb_kind==select_symb && node->node_arguments->arg_node->node_kind==NodeIdNode
			&& node->node_arguments->arg_node->node_node_id->nid_scope==local_scope
		){
			NodeId node_id;
		
			node_id=node->node_arguments->arg_node->node_node_id;
			
			if (node_id->nid_mark2 & NID_HAS_LAZY_SELECTOR_COUNTER){				
				++node_id->nid_lazy_selector_ref_count;

				if (node_id->nid_lazy_selector_ref_count==node_id->nid_refcount){
					NodeP node_id_def_node;
					
					node_id_def_node=node_id->nid_node_def->def_node;
					if (node_id_def_node->node_kind==NormalNode && node_id_def_node->node_symbol->symb_kind==definition
						&& node_id_def_node->node_symbol->symb_def->sdef_kind==IMPRULE && IsLazyState (node_id_def_node->node_state)
						&& node_id_def_node->node_symbol==CurrentSymbol
					){
						SymbDef function_sdef_p;
						RuleAltP rule_alt_p;
						
						function_sdef_p=node_id_def_node->node_symbol->symb_def;
						rule_alt_p=function_sdef_p->sdef_rule->rule_alts;
						
						if (roots_are_tuples_or_calls_to_this_function (rule_alt_p->alt_rhs_root,rule_alt_p->alt_rhs_defs,function_sdef_p)){
							node_id->nid_node_def->def_id->nid_mark2 |= NID_CALL_VIA_LAZY_SELECTIONS_ONLY;
							node_id_def_node->node_symbol->symb_def->sdef_rule->rule_mark |= RULE_CALL_VIA_LAZY_SELECTIONS_ONLY;
							if (ListOptimizations)
								printf ("Optimize lazy tuple recursion of %s\n",node_id_def_node->node_symbol->symb_def->sdef_ident->ident_name);
						}
					}
				}
			} else {
				node_id->nid_mark2 |= NID_HAS_LAZY_SELECTOR_COUNTER;
				node_id->nid_lazy_selector_ref_count=1;
			}
			
			if (node_id->nid_ref_count_copy>0 && node_id->nid_node_def)
				--node_id->nid_ref_count_copy__;
		} else
#endif
		DecrRefCountCopiesOfArgs (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
	} else {
		NodeId node_id;
		
		node_id=node->node_node_id;
		if (node_id->nid_ref_count_copy>0 && node_id->nid_node_def)
			--node_id->nid_ref_count_copy__;
	}
}

static void DecrRefCountCopiesOfArgs (Args args IF_OPTIMIZE_LAZY_TUPLE_RECURSION(int local_scope))
{
	for (; args; args = args->arg_next)
		DecrRefCountCopiesOfArg (args IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
}

static StateS *RemoveUndefinedsFromTupleState (StateS *state_p,int arity)
{
	int n;
	StateS *element_state_p,*new_element_states;

	element_state_p=state_p;
		
	new_element_states=NULL;
		
	for (n=0; n<arity; ++n, ++element_state_p){
		if (IsSimpleState (*element_state_p)){
			if (element_state_p->state_kind==Undefined){
				if (new_element_states==NULL){
					StateS *new_element_state_p,*old_element_state_p;
					int i;
					
					new_element_states=NewArrayOfStates (arity);
					element_state_p=&new_element_states[n];
					
					new_element_state_p=new_element_states;
					old_element_state_p=state_p;
					for (i=0; i<arity; ++i)
						*new_element_state_p++ = *old_element_state_p++;
				}
				element_state_p->state_kind=OnA;
			}
		} else if (element_state_p->state_type==TupleState){
			StateS *new_element_states_2;
			
			new_element_states_2=RemoveUndefinedsFromTupleState
				(element_state_p->state_tuple_arguments,element_state_p->state_arity);
			if (new_element_states_2){
				if (new_element_states==NULL){
					StateS *new_element_state_p,*old_element_state_p;
					int i;
					
					new_element_states=NewArrayOfStates (arity);
					element_state_p=&new_element_states[n];
					
					new_element_state_p=new_element_states;
					old_element_state_p=state_p;
					for (i=0; i<arity; ++i)
						*new_element_state_p++ = *old_element_state_p++;
				}
				element_state_p->state_tuple_arguments=new_element_states_2;				
			}
		}
	}

	return new_element_states;
}

static Bool ChangeState (StateS *old_state_p,StateS newstate)
{
	switch (old_state_p->state_kind){
		case OnA:
		case StrictOnA:		
			*old_state_p = newstate;
			return True;
		case Undefined:
			*old_state_p = newstate;
			return False;
		default:
			return False;
	}
}

static Bool AdjustState (StateS *old_state_p, StateS newstate)
{
	if (IsSimpleState (newstate)){
		if (IsSimpleState (*old_state_p)){
			switch (newstate.state_kind){
				case StrictOnA:
				case OnB:
					return ChangeState (old_state_p, newstate);
				default:
					return False;
			}
		} else
			return False;
	} else if (IsSimpleState (*old_state_p)){
		if (newstate.state_type==TupleState && 
			(old_state_p->state_kind==OnA || old_state_p->state_kind==StrictOnA || old_state_p->state_kind==Undefined))
		{
			StateS *element_states;
		
			element_states=RemoveUndefinedsFromTupleState (newstate.state_tuple_arguments,newstate.state_arity);
			
			*old_state_p=newstate;
			
			if (element_states){
/*				CheckWarning ("undefined state in tuple state removed",NULL); */
				old_state_p->state_tuple_arguments=element_states;
			}
			
			return old_state_p->state_kind!=Undefined;
		}

		return ChangeState (old_state_p,newstate);
	} else if (newstate.state_type==TupleState){
		int i, arity;
		StateS argstate;
		Bool new_arg_states;

		arity = old_state_p->state_arity;
		new_arg_states = False;

		Assume (newstate.state_arity == arity, "statesgen", "AdjustState");

		for (i=0; i<arity; i++){
			argstate = old_state_p->state_tuple_arguments[i];
			if (AdjustState (& argstate, newstate.state_tuple_arguments[i]) && ! new_arg_states){
				int j;
				States argstates;
			
				new_arg_states = True;
				argstates = NewArrayOfStates (arity);
				for (j=0; j<arity; j++)
					argstates[j] = old_state_p->state_tuple_arguments[j];
				old_state_p->state_tuple_arguments = argstates;
			}
			old_state_p->state_tuple_arguments[i] = argstate;
		}
		return new_arg_states;
	} else
		return False;
}

static void DetermineStateOfThenOrElse (Args t_or_e_args, NodeDefs *t_or_e_defs, StateS demstate,int local_scope)
{
	Node node;
	
	node=t_or_e_args->arg_node;
	
	if (node->node_kind==NodeIdNode && *t_or_e_defs==NULL){
		NodeId node_id;
		
		node_id=node->node_node_id;
		if (node_id->nid_ref_count_copy>=0)
			--node_id->nid_ref_count_copy__;	
	} else {
#if 0
		FPrintF (rules_file,"DetermineStateOfThenOrElse %d\n",local_scope);
#endif
		DetermineStatesOfRootNodeAndDefs (node,t_or_e_defs,demstate, local_scope);
	}

	AdjustState (&t_or_e_args->arg_state,demstate);
}

static void DecrementRefCountCopy (NodeId nid)
{ 
	if (nid->nid_ref_count_copy>0)
		--nid->nid_ref_count_copy__;
}

static Bool AdjustStateOfSharedNode (NodeId nid, StateS demstate, int local_scope)
{
	/*
		Note that if the reference count of a node identifier smaller than one means that the corresponding node 
		has already been treated. In that case it would be dangerous to adjust the node state
	*/
   	
	if (nid->nid_ref_count_copy>=0){
		NodeDefs nodedef;

		nodedef = nid->nid_node_def;

		if (nodedef!=NULL){
			int node_id_scope;
			
			if (ShouldDecrRefCount)
				DecrementRefCountCopy (nid);

			node_id_scope=nid->nid_scope;
			if (node_id_scope<0)
				node_id_scope=-node_id_scope;

#if 0
			FPrintF (rules_file,"AdjustStateOfSharedNode ");
			DPrintNodeId (nid,rules_file);
			FPrintF (rules_file," %d %d\n",node_id_scope,local_scope);
#endif

			if (node_id_scope>=local_scope){
				Node argnode;
				
				argnode = nodedef->def_node;
				if (nid->nid_mark & ON_A_CYCLE_MASK)
					AdjustState (&argnode->node_state, StrictState);
				else
					AdjustState (&argnode->node_state, demstate);
			}

			if (nodedef->def_node)
				return (nodedef->def_node->node_state.state_mark & STATE_PARALLEL_MASK)!=0;
		}
#if 0
		else {
			printf ("AdjustStateOfSharedNode nid_node_def=NULL ");
			DPrintNodeId (nid,StdOut);			
			printf (" %d %d\n",nid->nid_scope,local_scope);
		}
#endif
	}

	return False;
}

static Bool ArgInAStrictContext (Args arg, StateS demstate, Bool semistrict, int local_scope);

static Bool DetermineStrictArgContext (Args arg, StateS demstate, int local_scope)
{
	AdjustState (&arg->arg_state, demstate);
	
	return ArgInAStrictContext (arg, arg->arg_state, False, local_scope);
}

static StateP GetStateOfArguments (SymbDef sdef,int actual_arity)
{
	int symbol_arity;
	StateP state_p;
	
	switch (sdef->sdef_kind){
		case DEFRULE:
		case SYSRULE:
			state_p=sdef->sdef_rule_type->rule_type_state_p;
			symbol_arity = sdef->sdef_arity;
			break;
		case IMPRULE:
			state_p=sdef->sdef_rule->rule_state_p;
			symbol_arity = sdef->sdef_arity;
			break;
		case RECORDTYPE:
			state_p=sdef->sdef_record_state.state_record_arguments;
			symbol_arity = sdef->sdef_arity;
			break;
		default:
			return NULL;
	}
			
	if (symbol_arity==actual_arity)
		return state_p;
	else
		return NULL;
}

static Bool ArgsInAStrictContext (StateP arg_state_p,Args argn, int local_scope)
{
	Bool parallel;
	
	parallel = False;

	for (; argn!=NULL; argn=argn->arg_next){	
		if (! IsLazyState (*arg_state_p)){
			if (DetermineStrictArgContext (argn,*arg_state_p,local_scope))
				parallel = True;
		} else if (ShouldDecrRefCount)
			DecrRefCountCopiesOfArg (argn IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
		
		++arg_state_p;
	}

	return parallel;
}

#if DESTRUCTIVE_RECORD_UPDATES
	static Bool UpdateNodeInAStrictOrSemiStrictContext (Node node,StateP demanded_state_p,int local_scope,Bool semi_strict)
#else
	static Bool UpdateNodeInAStrictOrSemiStrictContext (Node node,int local_scope,Bool semi_strict)
#endif
{
	Bool parallel;
	SymbDef record_sdef;
	int type_arg_number;
	ArgS *arg;
	StateP record_arg_states;

	parallel=False;
	
	record_sdef=node->node_symbol->symb_def;
	
	if (!semi_strict){
#if DESTRUCTIVE_RECORD_UPDATES
		if (demanded_state_p->state_type==SimpleState && 
			demanded_state_p->state_kind==StrictOnA
/*			 && demanded_state_p->state_object==RecordObj */
			)
		{
			node->node_state = *demanded_state_p;
			node->node_state.state_object = RecordObj;
		} else
#endif	
		node->node_state = record_sdef->sdef_record_state;
	}

	arg=node->node_arguments;
	
	if (semi_strict){
		if (ArgInAStrictContext (arg,StrictState,True,local_scope))
			parallel = True;
	} else {

#if BOXED_RECORDS
		if (record_sdef->sdef_boxed_record){
			StateS boxed_record_state;
			
			SetUnaryState (&boxed_record_state,StrictOnA,RecordObj);
			if (DetermineStrictArgContext (arg, boxed_record_state,local_scope))
				parallel = True;			
		} else
#endif

		if (DetermineStrictArgContext (arg, record_sdef->sdef_record_state,local_scope))
			parallel = True;
	}
	
	type_arg_number=0;

	record_arg_states=record_sdef->sdef_record_state.state_record_arguments;
	
	while ((arg=arg->arg_next)!=NULL){
		int selector_number;
		Node selector_node;
			
		selector_node=arg->arg_node;
		selector_number=selector_node->node_symbol->symb_def->sdef_sel_field_number;

#if 1
		type_arg_number=selector_number;
#else
		/* Codewarrior 6 optimizer bug */
		while (type_arg_number!=selector_number){
			++type_arg_number;
		}
#endif

		if (!IsLazyState (record_arg_states[type_arg_number])){
			if (semi_strict
				? ArgInAStrictContext (selector_node->node_arguments,StrictState,True,local_scope)
				: DetermineStrictArgContext (selector_node->node_arguments,record_arg_states[type_arg_number],local_scope))
				parallel = True;
		} else if (ShouldDecrRefCount)
			DecrRefCountCopiesOfArg (selector_node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
	}
	
	if (parallel)
		node->node_state.state_mark |= STATE_PARALLEL_MASK;
	
	return parallel;
}

int optimise_strict_tuple_result_functions;

static Bool MemberCallInAStrictContext (Node node,int arg_n,unsigned int arg_strictness,int local_scope)
{
	struct arg *arg1_p,*arg2_p;
	struct node *arg1_of_apply_node_p;
	Bool parallel;
	
	parallel=False;
	
	node->node_state = StrictState;
	node->node_state.state_kind = StrictRedirection;

	arg1_p = node->node_arguments;
	arg1_of_apply_node_p=arg1_p->arg_node;
	--arg_n;
	arg2_p = arg1_p->arg_next;

	if (arg1_of_apply_node_p->node_arity==2){ /* node_arity of SelectorNode == 1 */
		/* arg 1 is apply node */

		AdjustState (&arg1_p->arg_state, StrictState);
		parallel = MemberCallInAStrictContext (arg1_of_apply_node_p,arg_n,arg_strictness,local_scope);
	} else {
		/* arg 1 is SelectorNode */
		parallel = DetermineStrictArgContext (arg1_p, StrictState, local_scope);
	}

	if ((arg_strictness & (1<<arg_n)) &&
		(arg2_p->arg_node->node_kind!=NodeIdNode ||
		 (arg2_p->arg_node->node_node_id->nid_ref_count_copy>=0 && arg2_p->arg_node->node_node_id->nid_node_def)))
	{
		if (DetermineStrictArgContext (arg2_p, StrictState, local_scope))
			parallel = True;
	} else
		if (ShouldDecrRefCount)
			DecrRefCountCopiesOfArg (arg2_p IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
	
	if (parallel)
		node->node_state.state_mark |= STATE_PARALLEL_MASK;

	return parallel;	
}

static Bool OptimizedMemberCallInAStrictContext (Node node,int arg_n,struct state *member_states_of_field,int local_scope)
{
	struct arg *arg1_p,*arg2_p;
	struct node *arg1_of_apply_node_p;
	Bool parallel;
	
	parallel=False;
	
	arg1_p = node->node_arguments;
	arg1_of_apply_node_p=arg1_p->arg_node;
	--arg_n;
	arg2_p = arg1_p->arg_next;

	if (arg1_of_apply_node_p->node_arity==2){ /* node_arity of SelectorNode == 1 */
		/* arg 1 is apply node */

		AdjustState (&arg1_p->arg_state, StrictState);

		arg1_of_apply_node_p->node_state = StrictState;
		arg1_of_apply_node_p->node_state.state_kind = StrictRedirection;

		parallel = OptimizedMemberCallInAStrictContext (arg1_of_apply_node_p,arg_n,member_states_of_field,local_scope);
	} else {
		/* arg 1 is SelectorNode */
		parallel = DetermineStrictArgContext (arg1_p, StrictState, local_scope);
	}

	if (! IsLazyState (member_states_of_field[arg_n+1])){
		if (DetermineStrictArgContext (arg2_p, member_states_of_field[arg_n+1], local_scope))
			parallel = True;
	} else
		if (ShouldDecrRefCount)
			DecrRefCountCopiesOfArg (arg2_p IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
	
	if (parallel)
		node->node_state.state_mark |= STATE_PARALLEL_MASK;

	return parallel;
}

static Bool NodeInAStrictContext (Node node,StateS demanded_state,int local_scope)
{
	Bool parallel;
	
	parallel=False;

	if (node->node_kind==NormalNode){
		Symbol rootsymb;
		
		rootsymb = node->node_symbol;
		switch (rootsymb->symb_kind){
			case cons_symb:
#if STRICT_LISTS
				if (node->node_arity==2){
					if (rootsymb->symb_head_strictness>1)
						if (rootsymb->symb_head_strictness==4)
							parallel = DetermineStrictArgContext (node->node_arguments,*rootsymb->symb_unboxed_cons_state_p,local_scope);
						else
							parallel = DetermineStrictArgContext (node->node_arguments,StrictState,local_scope);
					if (rootsymb->symb_tail_strictness)
						if (DetermineStrictArgContext (node->node_arguments->arg_next,StrictState,local_scope))
							parallel = True;

					if (ShouldDecrRefCount){
						if (!(rootsymb->symb_head_strictness>1))
							DecrRefCountCopiesOfArg (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
						if (!rootsymb->symb_tail_strictness)
							DecrRefCountCopiesOfArg (node->node_arguments->arg_next IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
					}
				} else
#endif
				if (ShouldDecrRefCount)
					DecrRefCountCopiesOfArgs (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));

				SetUnaryState (&node->node_state, StrictOnA, ListObj);
				break;
			case nil_symb:
#if STRICT_LISTS
				if (rootsymb->symb_head_strictness & 1)
					parallel = DetermineStrictArgContext (node->node_arguments,StrictState,local_scope);
#endif

				SetUnaryState (&node->node_state, StrictOnA, ListObj);
				break;
			case apply_symb:
				if (node->node_symbol->symb_instance_apply){ /* if set by optimisations.c */
					struct symbol_def *field_sdef;
					struct type_alt *member_type_alt;
					int arg_n;
					unsigned int arg_strictness;
					struct type_arg *type_arg_p;
					
					field_sdef = (struct symbol_def *)node->node_symbol->symb_next;
					member_type_alt=field_sdef->sdef_member_type_of_field;

					if (OptimizeInstanceCalls){
						arg_n=member_type_alt->type_alt_lhs->type_node_arity-1;
						node->node_state = field_sdef->sdef_member_states_of_field[-1];
						return OptimizedMemberCallInAStrictContext (node,arg_n,field_sdef->sdef_member_states_of_field,local_scope);
					}

					arg_strictness=0;
					arg_n=0;
					for_l (type_arg_p,member_type_alt->type_alt_lhs->type_node_arguments->type_arg_next,type_arg_next){
						if (type_arg_p->type_arg_node->type_node_annotation==StrictAnnot){
							arg_strictness |= 1<<arg_n;
						}
						++arg_n;
					}

					return MemberCallInAStrictContext (node,arg_n,arg_strictness,local_scope);
				}

				if (node->node_arity==2){
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
					
					if (arg_p!=NULL && arg_p->arg_node->node_kind==SelectorNode && arg_p->arg_node->node_arity==1){
						struct node *selector_node_p;
						
						selector_node_p=arg_p->arg_node;
						if ((selector_node_p->node_symbol->symb_def->sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE)!=0){
							struct symbol_def *field_sdef;
							struct type_alt *member_type_alt;
							
							field_sdef=selector_node_p->node_symbol->symb_def;
							member_type_alt=field_sdef->sdef_member_type_of_field;
							if (member_type_alt->type_alt_lhs->type_node_arity==n_apply_args+1){
								int arg_n;
								unsigned int arg_strictness;
								struct type_arg *type_arg_p;

								{
									struct symbol *new_symbol_p;

									new_symbol_p = CompAlloc (sizeof (struct symbol));
									*new_symbol_p = *node->node_symbol;
									new_symbol_p->symb_instance_apply = 1;
									new_symbol_p->symb_next = (struct symbol*)field_sdef;
									node->node_symbol = new_symbol_p;
								}

								if (OptimizeInstanceCalls){
									arg_n=member_type_alt->type_alt_lhs->type_node_arity-1;
									node->node_state = field_sdef->sdef_member_states_of_field[-1];
									return OptimizedMemberCallInAStrictContext (node,arg_n,field_sdef->sdef_member_states_of_field,local_scope);
								}

								arg_strictness=0;
								arg_n=0;
								for_l (type_arg_p,member_type_alt->type_alt_lhs->type_node_arguments->type_arg_next,type_arg_next){
									if (type_arg_p->type_arg_node->type_node_annotation==StrictAnnot){
										arg_strictness |= 1<<arg_n;
									}
									++arg_n;
								}
		
								return MemberCallInAStrictContext (node,arg_n,arg_strictness,local_scope);
							}
						}
					}
				}

				node->node_state = StrictState;
				node->node_state.state_kind = StrictRedirection;
				parallel = DetermineStrictArgContext (node->node_arguments, StrictState, local_scope);
				if (ShouldDecrRefCount)
					DecrRefCountCopiesOfArg (node->node_arguments->arg_next IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
				break;
			case if_symb:
			{
				Args args;
				
				args = node->node_arguments;
				node->node_state = StrictState;

				if (node->node_arity==3){
					if (DetermineStrictArgContext (args, BasicSymbolStates[bool_type], local_scope))
						parallel = True;

					args = args->arg_next;
#ifdef FASTER_STRICT_IF
					node->node_state=demanded_state;

					if (DetermineStrictArgContext (args,demanded_state,20000/*local_scope+1*/))
						parallel = True;

					args=args->arg_next;
					
					if (DetermineStrictArgContext (args,demanded_state,20000/*local_scope+1*/))
						parallel = True;

					break;
#else
					node->node_state.state_kind = StrictRedirection;
#endif
				}
				if (ShouldDecrRefCount)
					for (; args; args = args->arg_next)
						DecrRefCountCopiesOfArg (args IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
				break;
			}
			case select_symb:
			{
				Args arg;
				Node argnode;
				
				arg=node->node_arguments;
				
				SetUpdateableTupleState (&arg->arg_state, NewArrayOfUnaryStates (rootsymb->symb_arity, Undefined), rootsymb->symb_arity);
	
				arg->arg_state.state_tuple_arguments [node->node_arity - 1] = demanded_state;
				
				argnode=arg->arg_node;
				if (argnode->node_kind!=NodeIdNode)
					parallel = NodeInAStrictContext (argnode, arg->arg_state, local_scope);
				else {
					NodeId node_id;
					
					node_id=argnode->node_node_id;

					if (node_id->nid_ref_count_copy>=0 && node_id->nid_node_def){
						int node_id_scope;
					
						argnode = node_id->nid_node_def->def_node;
						
						if (ShouldDecrRefCount)
							DecrementRefCountCopy (node_id);
	
						node_id_scope=node_id->nid_scope;
						if (node_id_scope<0)
							node_id_scope=-node_id_scope;

#if 0
						FPrintF (rules_file,"NodeInAStrictContext select_symb %d ",node->node_arity);
						DPrintNodeId (node_id,rules_file);
						FPrintF (rules_file," %d %d\n",node_id_scope,local_scope);
#endif
						if (node_id_scope>=local_scope){
							if (IsSimpleState (argnode->node_state)){
								if (argnode->node_state.state_kind!=Parallel){
									SetUpdateableTupleState (&argnode->node_state, NewArrayOfUnaryStates (rootsymb->symb_arity, OnA), rootsymb->symb_arity);
									AdjustState (&argnode->node_state.state_tuple_arguments[node->node_arity-1],demanded_state);
								}
							} else {
								if ((argnode->node_state.state_mark & STATE_ELEMENTS_UPDATEABLE_MASK)==0){
									int i,arity;
									States arg_states;

									arity = argnode->node_state.state_arity;
									arg_states = NewArrayOfStates (arity);

									for (i=0; i<arity; ++i)
										arg_states[i] = argnode->node_state.state_tuple_arguments[i];
									
									argnode->node_state.state_tuple_arguments = arg_states;
									argnode->node_state.state_mark |= STATE_ELEMENTS_UPDATEABLE_MASK;
								}
								
								AdjustState (&argnode->node_state.state_tuple_arguments[node->node_arity-1],demanded_state);
							}
						}
						
#if 0
						PrintState (argnode->node_state,rules_file);
						FPrintF (rules_file,"\n");
#endif
					}
				}
				node->node_state = demanded_state;
				break;
			}
			case tuple_symb:
				if (IsSimpleState (demanded_state)){
					SetUnaryState (&node->node_state, StrictOnA, TupleObj);
					if (ShouldDecrRefCount)
						DecrRefCountCopiesOfArgs (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
				} else {
					Args arg; int i;
					
					for (i=0, arg=node->node_arguments; arg!=NULL; arg=arg->arg_next, i++){
						Bool par;
					
						par = False;
						if (!IsLazyState (demanded_state.state_tuple_arguments[i])){
							if (DetermineStrictArgContext (arg,demanded_state.state_tuple_arguments[i],local_scope))
								par = True;
						} else if (ShouldDecrRefCount)
							DecrRefCountCopiesOfArg (arg IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
						
						arg->arg_state = demanded_state.state_tuple_arguments[i];
						
						if (par){
							arg->arg_state.state_mark |= STATE_PARALLEL_MASK;
							parallel = True;
						}
					}
					node->node_state = demanded_state;
				}
				break;
			case definition:
			{
				StateP definition_state_p;
				SymbDef sdef;
				
				sdef = rootsymb->symb_def;
	
				if (sdef->sdef_arfun!=NoArrayFun
					&& (   (sdef->sdef_arfun==_UnqArraySelectLastFun || sdef->sdef_arfun==_UnqArraySelectNextFun) && node->node_arity==2)
						|| (sdef->sdef_arfun==_ArrayUpdateFun && node->node_arity==3))
				{
					StateP function_state_p;

					function_state_p=sdef->sdef_rule_type->rule_type_state_p;
				
					if (!(function_state_p[0].state_type==SimpleState && function_state_p[0].state_object==UnknownObj)){
						StateP arg_node_state_p;
						NodeP arg_node_p;
						ArgP arg_p;
						
						arg_p=node->node_arguments;

						if (sdef->sdef_arfun!=_ArrayUpdateFun){
							parallel = ArgsInAStrictContext (function_state_p,arg_p,local_scope);
							node->node_state = function_state_p[-1];

							arg_node_p=arg_p->arg_node;
							if (arg_node_p->node_kind!=NodeIdNode){
								arg_node_state_p=&arg_node_p->node_state;
													
								if (arg_node_state_p->state_type==TupleState && arg_node_state_p->state_tuple_arguments[1].state_type!=SimpleState){
									StateP tuple_state_args_p,tuple_result_state_args_p,result_state_p;
									
									result_state_p=&arg_node_state_p->state_tuple_arguments[1];
									
									tuple_state_args_p=CompAllocArray (2,StateS);
									tuple_state_args_p[0]=arg_p->arg_state.state_tuple_arguments[0];							
									tuple_state_args_p[1]=*result_state_p;	
									arg_p->arg_state.state_tuple_arguments=tuple_state_args_p;
									
									tuple_result_state_args_p=CompAllocArray (2,StateS);
									tuple_result_state_args_p[0]=node->node_state.state_tuple_arguments[0];
									tuple_result_state_args_p[1]=*result_state_p;
									node->node_state.state_tuple_arguments=tuple_result_state_args_p;
								}
							}
						} else {
							NodeP arg_node_p;

							if (demanded_state.state_type!=SimpleState){
								StateS arg_states[3],*update_arg_tuple_arg_states;
							
								arg_states[0]=function_state_p[0];
								arg_states[1]=function_state_p[1];
								arg_states[2]=function_state_p[2];
							
								update_arg_tuple_arg_states=NewArrayOfStates (2);
								update_arg_tuple_arg_states[0]=arg_states[0].state_tuple_arguments[0];
								update_arg_tuple_arg_states[1]=demanded_state;
								
								arg_states[0].state_tuple_arguments=update_arg_tuple_arg_states;
						
								parallel = ArgsInAStrictContext (arg_states,arg_p,local_scope);
								node->node_state = demanded_state;
							} else {
								parallel = ArgsInAStrictContext (function_state_p,arg_p,local_scope);
								node->node_state = function_state_p[-1];

								arg_node_p=arg_p->arg_node;
								if (arg_node_p->node_kind!=NodeIdNode){
									arg_node_state_p=&arg_node_p->node_state;
														
									if (arg_node_state_p->state_type==TupleState && arg_node_state_p->state_tuple_arguments[1].state_type!=SimpleState){
										StateP tuple_state_args_p,result_state_p;
										
										result_state_p=&arg_node_state_p->state_tuple_arguments[1];
										
										tuple_state_args_p=CompAllocArray (2,StateS);
										tuple_state_args_p[0]=arg_p->arg_state.state_tuple_arguments[0];							
										tuple_state_args_p[1]=*result_state_p;	
										arg_p->arg_state.state_tuple_arguments=tuple_state_args_p;
										
										node->node_state=*result_state_p;
									}
								}
							}
						}
						break;
					}
				}

				definition_state_p = GetStateOfArguments (sdef,node->node_arity);

				if (definition_state_p!=NULL){
#ifdef FASTER_STRICT_AND_OR
					if (sdef->sdef_module==StdBoolId->ident_name && node->node_arity==2){
						if (sdef->sdef_ident==AndId){
							ArgP arg1,arg2,false_arg;
							NodeP false_node;
							
							arg1=node->node_arguments;
							arg2=arg1->arg_next;
							
							/* if arg2 is a node_id, incorrect code if and on root (redirection with jmpevalupd) */
							
							if (arg2->arg_node->node_kind!=NodeIdNode){	
								node->node_symbol=IfSymbol;
								node->node_arity=3;
								
								false_node=NewNode (FalseSymbol,NULL,0);
								false_node->node_state=LazyState;
								
								false_arg=NewArgument (false_node);
								false_arg->arg_state=LazyState;
								
								arg2->arg_next=false_arg;		

								return NodeInAStrictContext (node,demanded_state,local_scope);
							}
						} else if (sdef->sdef_ident==OrId){
							ArgP arg1,arg2,true_arg;
							NodeP true_node;
							
							arg1=node->node_arguments;
							arg2=arg1->arg_next;

							/* if arg2 is a node_id, incorrect code if or on root (redirection with jmpevalupd) */
							
							if (arg2->arg_node->node_kind!=NodeIdNode){									
								node->node_symbol=IfSymbol;
								node->node_arity=3;

								true_node=NewNode (TrueSymbol,NULL,0);
								true_node->node_state=LazyState;
								
								true_arg=NewArgument (true_node);
								true_arg->arg_state=LazyState;
			
								arg1->arg_next=true_arg;
								true_arg->arg_next=arg2;
								
								return NodeInAStrictContext (node,demanded_state,local_scope);
							}
						}
					}
#endif

					if (sdef->sdef_kind==IMPRULE && demanded_state.state_type==TupleState && definition_state_p[-1].state_type==TupleState
						&& !(sdef->sdef_rule->rule_mark & RULE_CAF_MASK) && sdef->sdef_rule->rule_alts->alt_kind==Contractum && optimise_strict_tuple_result_functions)
							optimise_tuple_result_function (node,demanded_state);

					if (sdef->sdef_kind==RECORDTYPE)
#if BOXED_RECORDS
						if (sdef->sdef_boxed_record)
							SetUnaryState (&node->node_state,StrictOnA,RecordObj);
						else
#endif
						node->node_state = sdef->sdef_record_state;
					else
						node->node_state = definition_state_p[-1];
					parallel = ArgsInAStrictContext (definition_state_p,node->node_arguments,local_scope);
				} else {
					if (sdef->sdef_kind==CONSTRUCTOR && sdef->sdef_strict_constructor && sdef->sdef_arity==node->node_arity){
						SetUnaryState (&node->node_state,StrictOnA,UnknownObj);
						parallel = ArgsInAStrictContext (sdef->sdef_constructor->cl_state_p,node->node_arguments,local_scope);
					} else {
						if (ShouldDecrRefCount)
							DecrRefCountCopiesOfArgs (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
						node->node_state = StrictState;
					}
				}
				break;
			}
			case seq_symb:
				node->node_state=demanded_state;
				if (node->node_arity==2){
					parallel = DetermineStrictArgContext (node->node_arguments,StrictState,local_scope);
					parallel = DetermineStrictArgContext (node->node_arguments->arg_next,demanded_state,local_scope);
				} else {
					if (ShouldDecrRefCount)
						DecrRefCountCopiesOfArgs (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
					node->node_state = StrictState;
				}
				break;
			default:
				if (rootsymb->symb_kind < Nr_Of_Predef_Types){
					node->node_state = BasicSymbolStates [rootsymb->symb_kind];
					node->node_state.state_kind = demanded_state.state_kind;
				}
				break;
		}
	} else if (node->node_kind==SelectorNode){
		SymbDef ssymb;
		StateP record_state_p,unboxed_record_state_p;
#if BOXED_RECORDS
		static StateS boxed_record_state;
#endif		
		ssymb = node->node_symbol->symb_def;

		unboxed_record_state_p=&ssymb->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
#if BOXED_RECORDS		
		if (ssymb->sdef_type->type_lhs->ft_symbol->symb_def->sdef_boxed_record){
			SetUnaryState (&boxed_record_state,StrictOnA,RecordObj);
			record_state_p = &boxed_record_state;
		} else
#endif
		record_state_p=unboxed_record_state_p;
		
		if (node->node_arity>=SELECTOR_U){
			StateP tuple_arg_states;
			
			tuple_arg_states=NewArrayOfStates (2);

			if (node->node_arity>=SELECTOR_L){
				StateS sel_arg_state,*sel_arg_tuple_arg_states;
				NodeP arg_node_p;

				sel_arg_tuple_arg_states=NewArrayOfStates (2);
				sel_arg_tuple_arg_states[0]=*record_state_p;
				
				if (demanded_state.state_type==TupleState && !IsLazyState (demanded_state.state_tuple_arguments[1])){
					sel_arg_tuple_arg_states[1]=demanded_state.state_tuple_arguments[1];
					tuple_arg_states[1]=demanded_state.state_tuple_arguments[1];
				} else {
					sel_arg_tuple_arg_states[1]=StrictState;
					tuple_arg_states[1]=StrictState;
				}
				SetTupleState (&sel_arg_state,sel_arg_tuple_arg_states,2);
				
				parallel = DetermineStrictArgContext (node->node_arguments,sel_arg_state,local_scope);

				arg_node_p=node->node_arguments->arg_node;
				if (arg_node_p->node_kind!=NodeIdNode && arg_node_p->node_state.state_type==TupleState
					&& arg_node_p->node_state.state_tuple_arguments[1].state_type!=SimpleState)
				{
					StateP result_state_p;
				
					result_state_p=&arg_node_p->node_state.state_tuple_arguments[1];
					
					tuple_arg_states[1]=*result_state_p;
					sel_arg_tuple_arg_states[1]=*result_state_p;	
				}
			} else {
				parallel = DetermineStrictArgContext (node->node_arguments,*record_state_p,local_scope);
				tuple_arg_states[1]=*record_state_p;
			}			

			tuple_arg_states[0]=unboxed_record_state_p->state_record_arguments[ssymb->sdef_sel_field_number];
			SetTupleState (&node->node_state,tuple_arg_states,2);
		} else {
			parallel = DetermineStrictArgContext (node->node_arguments,*record_state_p,local_scope);
			node->node_state=demanded_state;
		}
	} else if (node->node_kind==UpdateNode)
#if DESTRUCTIVE_RECORD_UPDATES
		return UpdateNodeInAStrictOrSemiStrictContext (node,&demanded_state,local_scope,False);
#else
		return UpdateNodeInAStrictOrSemiStrictContext (node,local_scope,False);
#endif
	else if (node->node_kind==MatchNode){
		parallel = DetermineStrictArgContext (node->node_arguments,StrictState,local_scope);

		node->node_state=demanded_state;		
	} else
		error_in_function ("NodeInAStrictContext");

	if (parallel)
		node->node_state.state_mark |= STATE_PARALLEL_MASK;

	return parallel;	
}

static Bool NodeInASemiStrictContext (Node node, int local_scope);

static Bool ArgInAStrictContext (Args arg, StateS demstate, Bool semistrict, int local_scope)
{
	Bool parallel;
	Node argnode;

	parallel = False;

	argnode=arg->arg_node;
	
	if (argnode->node_kind!=NodeIdNode){
		if (semistrict && argnode->node_number<0){
			parallel = NodeInASemiStrictContext (argnode,local_scope);
			argnode->node_state.state_kind = SemiStrict;
		} else
			parallel = NodeInAStrictContext (argnode, demstate, local_scope);
	} else
		parallel =  AdjustStateOfSharedNode (argnode->node_node_id, demstate, local_scope);

	if (parallel)
		arg->arg_state.state_mark |= STATE_PARALLEL_MASK;

	return parallel;
}

static Bool NodeInASemiStrictContext (Node node,int local_scope)
{
	switch (node->node_kind){
		case NormalNode:
		{
			Bool parallel;
			Symbol symb;

			symb = node->node_symbol;

			parallel = False;
			
			if (symb->symb_kind==definition){
				SymbDef sdef;
				ArgP arg_p;
				StateP arg_state_p;
				
				sdef=symb->symb_def;
				arg_state_p = GetStateOfArguments (sdef,node->node_arity);

				if (arg_state_p==NULL && sdef->sdef_kind==CONSTRUCTOR &&
					sdef->sdef_strict_constructor && sdef->sdef_arity==node->node_arity)
				{
					arg_state_p=sdef->sdef_constructor->cl_state_p;
				}
					
				if (arg_state_p!=NULL){
					for_l (arg_p,node->node_arguments,arg_next){
						if (IsLazyState (*arg_state_p)){
							if (ShouldDecrRefCount)
								DecrRefCountCopiesOfArg (arg_p IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
						} else 
							if (ArgInAStrictContext (arg_p,StrictState,True,local_scope))
								parallel = True;
						
						++arg_state_p;
					}
				} else
					DecrRefCountCopiesOfArgs (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
			}
#if STRICT_LISTS
			else if (symb->symb_kind==cons_symb && node->node_arity==2){
				ArgP arg_p;
				
				arg_p=node->node_arguments;				
				if (symb->symb_head_strictness>1){
					if (ArgInAStrictContext (arg_p,StrictState,True,local_scope))
						parallel = True;
				} else
					if (ShouldDecrRefCount)
						DecrRefCountCopiesOfArg (arg_p IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));

				arg_p=arg_p->arg_next;
				if (symb->symb_tail_strictness){
					if (ArgInAStrictContext (arg_p,StrictState,True,local_scope))
						parallel = True;
				} else
					if (ShouldDecrRefCount)
						DecrRefCountCopiesOfArg (arg_p IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
			}
#endif
			else
				DecrRefCountCopiesOfArgs (node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));

			if (parallel)
				node->node_state.state_mark |= STATE_PARALLEL_MASK;
		
			return parallel;
		}
		case SelectorNode:
		case MatchNode:
			if (ArgInAStrictContext (node->node_arguments,StrictState,True,local_scope)){
				node->node_state.state_mark |= STATE_PARALLEL_MASK;
				return True;
			} else
				return False;
		case UpdateNode:
#if DESTRUCTIVE_RECORD_UPDATES
			return UpdateNodeInAStrictOrSemiStrictContext (node,&StrictState,local_scope,True);
#else		
			return UpdateNodeInAStrictOrSemiStrictContext (node,local_scope,True);
#endif
		default:
			error_in_function ("NodeInASemiStrictContext");
			return False;
	}
}

static void DetermineStatesOfNonIfRootNode (Node root,NodeId root_id,StateS demstate,int local_scope)
{	
	if (root->node_state.state_kind != OnA){
		StaticMessage (False, "%S", Wrootannot, CurrentSymbol);
		root->node_state.state_kind = OnA;
	}
	
	if (root_id)
		root_id->nid_ref_count_copy_=-1; /* to indicate that this node has been examined */
	
	if (root_id && (root_id->nid_mark & ON_A_CYCLE_MASK)){
		(void) NodeInASemiStrictContext (root, local_scope);
		root->node_state.state_kind = SemiStrict;
	} else
		NodeInAStrictContext (root, demstate, local_scope);
}

static int scope;
static void DetermineStatesRootNode (Node node, NodeId nid, StateS demstate,int local_scope);

static void DetermineStatesIfRootNode (Node node, StateS demstate,int local_scope)
{
	Args condpart;
	int new_local_scope;

	new_local_scope=scope+2;
	scope+=3;

	condpart = node->node_arguments;
			
	AdjustState (&condpart->arg_state, BasicSymbolStates [bool_type]);

	if (condpart->arg_node->node_kind!=NodeIdNode)
		DetermineStatesRootNode (condpart->arg_node, NULL,condpart->arg_state, local_scope);
	else 
		/* the parallel state of the condition is not needed */
		AdjustStateOfSharedNode (condpart->arg_node->node_node_id,condpart->arg_state,local_scope);
	
	AdjustState (&node->node_state, demstate);

	++scope;
	DetermineStateOfThenOrElse (condpart->arg_next,&node->node_then_node_defs,demstate,new_local_scope);

	++scope;
	DetermineStateOfThenOrElse (condpart->arg_next->arg_next,&node->node_else_node_defs,demstate,new_local_scope);
}

static void DetermineStatesSwitchRootNode (Node root_node, StateS demstate, int local_scope)
{
	ArgP arg_p;

	AdjustState (&root_node->node_state, demstate);

	for_l (arg_p,root_node->node_arguments,arg_next){
		NodeP node;

		node=arg_p->arg_node;
		if (node->node_kind==CaseNode){
			NodeP case_alt_node_p;

			case_alt_node_p=node->node_arguments->arg_node;
/*	Codewarrior bug			if (case_alt_node_p->node_kind==PushNode){  */
			if (node->node_arguments->arg_node->node_kind==PushNode)
				DetermineStatesOfRootNodeAndDefs (case_alt_node_p->node_arguments->arg_next->arg_node,&node->node_node_defs,demstate,local_scope);
			else
				DetermineStatesOfRootNodeAndDefs (node->node_arguments->arg_node,&node->node_node_defs,demstate,local_scope);
		} else if (node->node_kind==DefaultNode){
			DetermineStatesOfRootNodeAndDefs (node->node_arguments->arg_node,&node->node_node_defs,demstate,local_scope);
		} else if (node->node_kind==OverloadedCaseNode){
			NodeP case_node_p;

			DetermineStrictArgContext (node->node_arguments,StrictState,local_scope);
			if (ShouldDecrRefCount)
				DecrRefCountCopiesOfArg (node->node_arguments->arg_next IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));

			case_node_p=node->node_node;
			DetermineStatesOfRootNodeAndDefs (case_node_p->node_arguments->arg_node,&case_node_p->node_node_defs,demstate,local_scope);
		} else
			error_in_function ("DetermineStatesSwitchRootNode");
	}
}

static void DetermineStatesRootNode (Node node, NodeId nid, StateS demstate,int local_scope)
{
	switch (node->node_kind)
	{
		case IfNode:
			DetermineStatesIfRootNode (node, demstate, local_scope);
			break;
		case SwitchNode:
			DetermineStatesSwitchRootNode (node, demstate, local_scope);
			break;
		default:
			DetermineStatesOfNonIfRootNode (node, nid, demstate, local_scope);
			break;
	}
}

static void ParAnnotInAStrictContext (Node node,Annotation annot, int local_scope)
{
	if (annot==ParallelAtAnnot){
		Node at_node;
		
		at_node=get_p_at_node (node);
		
		if (at_node->node_kind!=NodeIdNode)
			NodeInAStrictContext (at_node,BasicSymbolStates[procid_type],local_scope);
	}
}

static void DetermineStatesOfNodeDefs (NodeDefs firstdef, int local_scope)
{
	NodeDefs next;
	Bool ready;

	for_l (next,firstdef,def_next)
		if ((next->def_id->nid_mark & ON_A_CYCLE_MASK) && next->def_node!=NULL)
			DecrRefCountCopiesOfArgs (next->def_node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));

	/* examine all parallel annotated nodes */
	
	for_l (next,firstdef,def_next){
		Node node;
	
		node=next->def_node;
		if (node && node->node_annotation && node->node_state.state_kind==Parallel)
			ParAnnotInAStrictContext (node,node->node_annotation, local_scope);
	}
	
	/* determine states */
	
	do {
		ready = True;

		/* First we examine all the nodes that are not lazy anymore */

		for_l (next,firstdef,def_next){
			Node node;
			
			node = next->def_node;
			if (node && ! IsLazyState (node->node_state) &&
				(next->def_id->nid_ref_count_copy==0 ||
				(next->def_id->nid_ref_count_copy>=0 && (next->def_id->nid_mark & ON_A_CYCLE_MASK))))
			{
				
				/* to indicate that this node has already been examined: */
				next->def_id->nid_ref_count_copy_ = -1;
				
				ready =  False;

				/*
				JVG: hack to remove undefined's in tuple state of tuples which are selected
				and for which there are selectors and may be other references in a lazy context
				(can lead to less efficient code)
				*/
				
				if (next->def_id->nid_refcount>1 && node->node_kind==NormalNode
					&& node->node_symbol->symb_kind==select_symb
					&& node->node_state.state_type==TupleState)
				{
					StateS *element_states;
		
					element_states=RemoveUndefinedsFromTupleState
						(node->node_state.state_tuple_arguments,node->node_state.state_arity);
					if (element_states)
						node->node_state.state_tuple_arguments=element_states;
				}

				if (next->def_id->nid_mark & ON_A_CYCLE_MASK){
					ShouldDecrRefCount = False;
					NodeInASemiStrictContext (node,local_scope);
					SetUnaryState (&node->node_state, SemiStrict, node->node_state.state_object);
				} else {
					ShouldDecrRefCount = True;
					NodeInAStrictContext (node, node->node_state, local_scope);
				}
			}
		}
		
		if (ready){
			for_l (next,firstdef,def_next){
				if (next->def_node && IsLazyState (next->def_node->node_state) && 
					! (next->def_id->nid_mark & ON_A_CYCLE_MASK) && next->def_id->nid_ref_count_copy==0)
				{
					next->def_id->nid_ref_count_copy_ = -1;
					ready =  False;
					DecrRefCountCopiesOfArgs (next->def_node->node_arguments IF_OPTIMIZE_LAZY_TUPLE_RECURSION(local_scope));
					break;
				}
			}
		}
	} while (! ready);

	for_l (next,firstdef,def_next)
		if (next->def_node)
			if (! (next->def_id->nid_ref_count_copy<0 ||
					(next->def_id->nid_ref_count_copy==0 && (next->def_id->nid_mark & ON_A_CYCLE_MASK))))
#if 1
				error_in_function_s ("DetermineStatesOfNodeDefs",CurrentSymbol->symb_def->sdef_ident->ident_name);
#else
			{
				char s[20];
				
				sprintf (s,"%x %d %d",(int)next->def_id,next->def_id->nid_ref_count_copy,next->def_id->nid_refcount);
				error_in_function_s ("DetermineStatesOfNodeDefs",/*CurrentSymbol->symb_def->sdef_ident->ident_name*/s/*next->def_id->nid_ident->ident_name*/);
			}
#endif
}

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS

static void set_push_node_id_states (NodeIdListElementP node_ids,StateS states[])
{
	StateP arg_state_p;

	for (arg_state_p=states; node_ids!=NULL; node_ids=node_ids->nidl_next,++arg_state_p){
		NodeIdP node_id_p;
		
		node_id_p=node_ids->nidl_node_id;
		node_id_p->nid_lhs_state_p_=arg_state_p;
		node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
	}
}

static void set_lazy_push_node_id_states (NodeIdListElementP node_ids)
{
	for (; node_ids!=NULL; node_ids=node_ids->nidl_next){
		NodeIdP node_id_p;
		
		node_id_p=node_ids->nidl_node_id;
		node_id_p->nid_lhs_state_p_=&LazyState;
		node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
	}
}

static void DetermineStatesOfNodeAndDefs (Node root_node,NodeDefs node_defs,StateS demstate,int local_scope)
{
	ShouldDecrRefCount = True;

	if (root_node->node_kind==SwitchNode){
		ArgP arg_p;
		int old_scope;
		
		old_scope=scope;
		
		if (node_defs!=NULL)
			error_in_function ("DetermineStatesOfNodeAndDefs");
		
		root_node->node_state = *root_node->node_node_id->nid_lhs_state_p;
		root_node->node_node_id->nid_ref_count_copy=root_node->node_node_id->nid_refcount;
		
		for_l (arg_p,root_node->node_arguments,arg_next){
			NodeP arg_node_p;

			arg_node_p=arg_p->arg_node;
			scope=old_scope;
			
			if (arg_node_p->node_kind==CaseNode){
				NodeP case_alt_node_p;
				
				case_alt_node_p=arg_node_p->node_arguments->arg_node;
				if (case_alt_node_p->node_kind==PushNode){
					NodeIdP node_id_p;
					StateP node_id_state_p;
					NodeIdListElementP node_ids;
					
					node_id_p=case_alt_node_p->node_arguments->arg_node->node_node_id;
					node_id_state_p=node_id_p->nid_lhs_state_p;
					node_ids=case_alt_node_p->node_node_ids;
					
					switch (node_id_state_p->state_type){
						case RecordState:
							set_push_node_id_states (node_ids,case_alt_node_p->node_push_symbol->symb_def->sdef_record_state.state_record_arguments);
							break;
						case TupleState:
							set_push_node_id_states (node_ids,node_id_state_p->state_tuple_arguments);
							break;
						default:
						{
							Symbol symbol;
							
							symbol=case_alt_node_p->node_push_symbol;
							
							if (symbol->symb_kind==definition){
								SymbDef sdef;

								sdef=symbol->symb_def;

								if (sdef->sdef_kind==CONSTRUCTOR){																			
# ifdef REUSE_UNIQUE_NODES
									AttributeKind lhs_type_attribute;
									
									lhs_type_attribute=sdef->sdef_type->type_lhs->ft_attribute;

									if (lhs_type_attribute==UniqueAttr && (node_id_state_p->state_mark & STATE_UNIQUE_MASK)==0){
										StateP unique_state_p;
										
										unique_state_p=CompAllocType (StateS);
										*unique_state_p=*node_id_state_p;
										unique_state_p->state_mark |= STATE_UNIQUE_MASK;

										node_id_state_p=unique_state_p;
										node_id_p->nid_lhs_state_p=unique_state_p;
									}
									
									if (sdef->sdef_arity==case_alt_node_p->node_arity && (node_id_state_p->state_mark & STATE_UNIQUE_MASK)){
										NodeIdListElementP node_ids_elem;

										if (sdef->sdef_strict_constructor){
											struct type_arg *type_arg_p;
											StateP constructor_arg_state_p;
											
											for_lla (node_ids_elem,type_arg_p,constructor_arg_state_p,
													node_ids,sdef->sdef_constructor->cl_constructor->type_node_arguments,symbol->symb_def->sdef_constructor->cl_state_p,
													nidl_next,type_arg_next)
											{
												NodeIdP node_id_p;
												
												node_id_p=node_ids_elem->nidl_node_id;
												node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
												
												node_id_p->nid_lhs_state_p_ = determine_unique_state_of_constructor_argument
																				(constructor_arg_state_p,type_arg_p->type_arg_node,lhs_type_attribute,node_id_state_p);
											}
										} else {
											struct type_arg *type_arg_p;
											
											for_ll (node_ids_elem,type_arg_p,node_ids,sdef->sdef_constructor->cl_constructor->type_node_arguments,nidl_next,type_arg_next){
												NodeIdP node_id_p;
												
												node_id_p=node_ids_elem->nidl_node_id;
												node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
												
												node_id_p->nid_lhs_state_p_ = determine_unique_state_of_constructor_argument
																				(&LazyState,type_arg_p->type_arg_node,lhs_type_attribute,node_id_state_p);
											}
										}
									} else
# endif				
									if (sdef->sdef_strict_constructor && sdef->sdef_arity==case_alt_node_p->node_arity)
										set_push_node_id_states (node_ids,sdef->sdef_constructor->cl_state_p);
									else
										set_lazy_push_node_id_states (node_ids);
								} else if (sdef->sdef_kind==RECORDTYPE){
# ifdef REUSE_UNIQUE_NODES
									AttributeKind lhs_type_attribute;
									
									lhs_type_attribute=sdef->sdef_type->type_lhs->ft_attribute;

									if (lhs_type_attribute==UniqueAttr && (node_id_state_p->state_mark & STATE_UNIQUE_MASK)==0){
										StateP unique_state_p;
										
										unique_state_p=CompAllocType (StateS);
										*unique_state_p=*node_id_state_p;
										unique_state_p->state_mark |= STATE_UNIQUE_MASK;

										node_id_state_p=unique_state_p;
										node_id_p->nid_lhs_state_p=unique_state_p;
									}
									
									if (node_id_state_p->state_mark & STATE_UNIQUE_MASK){
										NodeIdListElementP node_ids_elem;
										StateP arg_state_p;
										FieldList field;

										for_lla (node_ids_elem,field,arg_state_p,node_ids,
												 sdef->sdef_type->type_fields,sdef->sdef_record_state.state_record_arguments,nidl_next,fl_next)
										{
											NodeIdP node_id_p;
											
											node_id_p=node_ids_elem->nidl_node_id;
											node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
											
											node_id_p->nid_lhs_state_p_ = determine_unique_state_of_constructor_argument
																			(arg_state_p,field->fl_type,lhs_type_attribute,node_id_state_p);
										}
									} else				
# endif

									set_push_node_id_states (node_ids,sdef->sdef_record_state.state_record_arguments);
								} else
									set_lazy_push_node_id_states (node_ids);
							} else
# ifdef REUSE_UNIQUE_NODES
							if (symbol->symb_kind==cons_symb && (node_id_state_p->state_mark & STATE_UNIQUE_MASK) && case_alt_node_p->node_arity==2){
								NodeIdP node_id_p;
								StateP element_state_p;
								
								node_id_p=node_ids->nidl_node_id;
								node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
#  if STRICT_LISTS
								if (symbol->symb_head_strictness>1)
									if (symbol->symb_head_strictness==4)
										element_state_p=symbol->symb_state_p;
									else
										element_state_p=&StrictState;
								else
#  endif
								element_state_p=&LazyState;

								if ((node_id_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) && (node_id_state_p->state_unq_type_args & 1)){
									StateP unique_state_p;
									
									unique_state_p=CompAllocType (StateS);
									*unique_state_p=*element_state_p;
									unique_state_p->state_mark |= STATE_UNIQUE_MASK;

									node_id_p->nid_lhs_state_p_=unique_state_p;
								} else
									node_id_p->nid_lhs_state_p_=element_state_p;
																
								node_ids=node_ids->nidl_next;
								
								node_id_p=node_ids->nidl_node_id;
								node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
								
								{
									StateP unique_state_p;
									
									unique_state_p=CompAllocType (StateS);
#  if STRICT_LISTS
									if (symbol->symb_tail_strictness)
										*unique_state_p=StrictState;
									else
#  endif
									*unique_state_p=LazyState;
									
									unique_state_p->state_mark |= STATE_UNIQUE_MASK;
									if ((node_id_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) && (node_id_state_p->state_unq_type_args & 1)){
										unique_state_p->state_mark |= STATE_UNIQUE_TYPE_ARGUMENTS_MASK;
										unique_state_p->state_unq_type_args = 1;
									}

									node_id_p->nid_lhs_state_p_=unique_state_p;
								}
							} else if (symbol->symb_kind==tuple_symb && (node_id_state_p->state_mark & STATE_UNIQUE_MASK)){
								NodeIdListElementP node_ids_elem;
								int i;

								for_li (node_ids_elem,i,node_ids,nidl_next){
									NodeIdP node_id_p;
									
									node_id_p=node_ids_elem->nidl_node_id;
									node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
									
									if ((node_id_state_p->state_mark & STATE_UNIQUE_TYPE_ARGUMENTS_MASK) && (node_id_state_p->state_unq_type_args & (1<<i))){
										StateP unique_state_p;
										
										unique_state_p=CompAllocType (StateS);
										*unique_state_p=LazyState;
										unique_state_p->state_mark |= STATE_UNIQUE_MASK;

										node_id_p->nid_lhs_state_p_=unique_state_p;
									} else
										node_id_p->nid_lhs_state_p_=&LazyState;
								}
							} else
# endif
# if STRICT_LISTS
							if (symbol->symb_kind==cons_symb && (symbol->symb_head_strictness>1 || symbol->symb_tail_strictness) && case_alt_node_p->node_arity==2){
								NodeIdP node_id_p;
							
								node_id_p=node_ids->nidl_node_id;
								node_id_p->nid_lhs_state_p_= symbol->symb_head_strictness>1 ? (symbol->symb_head_strictness==4 ? symbol->symb_state_p : &StrictState) : &LazyState;
								node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;

								node_id_p=node_ids->nidl_next->nidl_node_id;
								node_id_p->nid_lhs_state_p_= symbol->symb_tail_strictness ? &StrictState : &LazyState;
								node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
							} else
# endif
							set_lazy_push_node_id_states (node_ids);
						}
					}
					
					DetermineStatesOfNodeAndDefs (case_alt_node_p->node_arguments->arg_next->arg_node,arg_node_p->node_node_defs,demstate,local_scope);
				} else
					DetermineStatesOfNodeAndDefs (case_alt_node_p,arg_node_p->node_node_defs,demstate,local_scope);
			} else if (arg_node_p->node_kind==DefaultNode){
				DetermineStatesOfNodeAndDefs (arg_node_p->node_arguments->arg_node,arg_node_p->node_node_defs,demstate,local_scope);
			} else if (arg_node_p->node_kind==OverloadedCaseNode){
				DetermineStatesOfNodeAndDefs (arg_node_p->node_node->node_arguments->arg_node,arg_node_p->node_node->node_node_defs,demstate,local_scope);
			} else
				error_in_function ("DetermineStatesOfNodeAndDefs");
		}
	} else if (root_node->node_kind==GuardNode){
		int old_scope;
		
 		old_scope=scope;
		DetermineStatesOfNodeAndDefs (root_node->node_arguments->arg_node,node_defs,demstate,local_scope);
		scope=old_scope;
		DetermineStatesOfNodeAndDefs (root_node->node_arguments->arg_next->arg_node,root_node->node_node_defs,demstate,local_scope);
	} else {
		if (root_node->node_kind==NodeIdNode){
			NodeId node_id;
			
			node_id=root_node->node_node_id;
			if (node_id->nid_node==NULL || node_id->nid_ref_count_copy<0)
				return;
			
			DetermineStatesRootNode (node_id->nid_node,node_id,demstate,local_scope);
		} else
			DetermineStatesRootNode (root_node,NULL,demstate,local_scope);

		if (node_defs)
			DetermineStatesOfNodeDefs (node_defs,local_scope);
	}
}
#endif

void DetermineStatesOfRootNodeAndDefs (Node root_node,NodeDefs *rootdef,StateS demstate,int local_scope)
{
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	DetermineStatesOfNodeAndDefs (root_node,*rootdef,demstate,local_scope);
#else
	ShouldDecrRefCount = True;

	if (root_node->node_kind==NodeIdNode){
		NodeId node_id;
		
		node_id=root_node->node_node_id;
		if (node_id->nid_node==NULL || node_id->nid_ref_count_copy<0)
			return;
		
		DetermineStatesRootNode (node_id->nid_node,node_id,demstate,local_scope);
	} else
		DetermineStatesRootNode (root_node,NULL,demstate,local_scope);

	if (*rootdef)
		DetermineStatesOfNodeDefs (*rootdef,local_scope);
#endif
}

#ifdef OBSERVE_ARRAY_SELECTS_IN_PATTERN
	static void set_states_of_array_selects_in_pattern (RuleAlts alt)
	{
		StrictNodeIdP strict_node_id;

		for_l (strict_node_id,alt->alt_strict_node_ids,snid_next){
			if (strict_node_id->snid_array_select_in_pattern){
				NodeP select_node,array_uselect_node;
				NodeId node_id;

				node_id=strict_node_id->snid_node_id;
				if (node_id->nid_node->node_symbol->symb_kind==select_symb){							
					select_node=node_id->nid_node;
					array_uselect_node=select_node->node_arguments->arg_node;
				} else {
					select_node=NULL;
					array_uselect_node=node_id->nid_node;
				}
				
				if (array_uselect_node->node_state.state_type==TupleState){
					StateP tuple_arg_states;

					tuple_arg_states=NewArrayOfStates (2);
					SetUnaryState (&tuple_arg_states[1],Undefined,UnknownObj);
					tuple_arg_states[0]=array_uselect_node->node_state.state_tuple_arguments[0];
					SetTupleState (&array_uselect_node->node_state,tuple_arg_states,2);

					if (select_node!=NULL && select_node->node_state.state_type==SimpleState
						&& select_node->node_state.state_kind==StrictOnA
						&& select_node->node_arguments->arg_state.state_type==TupleState
						&& select_node->node_arguments->arg_state.state_tuple_arguments[0].state_type==SimpleState
						&& select_node->node_arguments->arg_state.state_tuple_arguments[0].state_kind==StrictOnA
					){
						select_node->node_state=array_uselect_node->node_state.state_tuple_arguments[0];
						SetTupleState (&select_node->node_arguments->arg_state,tuple_arg_states,2);
					}
				}
			}
		}
	}
#endif				

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
static void set_states_in_lhs (ArgP arguments,StateP states)
{
	StateP arg_state_p;
	ArgP arg_p;

	for_la (arg_p,arg_state_p,arguments,states,arg_next){
		Node arg_node;
		
		arg_p->arg_state=*arg_state_p;
		
		arg_node=arg_p->arg_node;

		if (arg_node->node_kind==NodeIdNode){
			NodeId node_id_p;
			
			node_id_p=arg_node->node_node_id;
			node_id_p->nid_lhs_state_p_=&arg_p->arg_state;
			node_id_p->nid_ref_count_copy=node_id_p->nid_refcount;
			
			arg_node=node_id_p->nid_node;
			if (arg_node!=NULL){
				if (arg_state_p->state_type==TupleState)
					set_states_in_lhs (arg_node->node_arguments,arg_state_p->state_tuple_arguments);
				else if (arg_state_p->state_type==RecordState)
					set_states_in_lhs (arg_node->node_arguments,arg_state_p->state_record_arguments);
			}
		} else {
			if (arg_state_p->state_type==TupleState)
				set_states_in_lhs (arg_node->node_arguments,arg_state_p->state_tuple_arguments);
			else if (arg_state_p->state_type==RecordState)
				set_states_in_lhs (arg_node->node_arguments,arg_state_p->state_record_arguments);
			else
				error_in_function ("set_states_in_lhs");
		}
	}
}
#endif

static void mark_is_constructor_function (ImpRuleP rule)
{
	RuleAltP alt;

	alt=rule->rule_alts;
	if (alt->alt_kind==Contractum && alt->alt_next==NULL && alt->alt_rhs_defs==NULL && alt->alt_lhs_defs==NULL){
		NodeP node_p;
		ArgP arg_p;
		
		arg_p=alt->alt_lhs_root->node_arguments;
		if (arg_p!=NULL && arg_p->arg_next==NULL && arg_p->arg_node->node_kind==NodeIdNode){
			node_p=alt->alt_rhs_root;
			if (node_p->node_kind==SwitchNode){
				NodeP case_node;

				arg_p=node_p->node_arguments;
				case_node=arg_p->arg_node;
				if (case_node->node_kind==CaseNode){
					struct symbol *symbol;
					NodeP case_rhs_node;

					case_rhs_node = case_node->node_arguments->arg_node;
					if (case_rhs_node->node_kind==PushNode)
						case_rhs_node = case_rhs_node->node_arguments->arg_next->arg_node;

					symbol=case_node->node_symbol;
					if (((symbol->symb_kind==definition && symbol->symb_def->sdef_kind==CONSTRUCTOR) ||
						 symbol->symb_kind==nil_symb || symbol->symb_kind==cons_symb) &&
						case_node->node_node_defs==NULL &&
						case_rhs_node->node_kind==NormalNode && case_rhs_node->node_symbol->symb_kind==bool_denot &&
						case_rhs_node->node_symbol->symb_bool==True)
					{
						arg_p=arg_p->arg_next;
						if (arg_p!=NULL){
							NodeP default_node;

							default_node=arg_p->arg_node;
							if (arg_p->arg_node->node_kind==DefaultNode){
								NodeP default_rhs_node;
								
								default_rhs_node=default_node->node_arguments->arg_node;
								if (default_node->node_node_defs==NULL &&
									default_rhs_node->node_kind==NormalNode && default_rhs_node->node_symbol->symb_kind==bool_denot &&
									default_rhs_node->node_symbol->symb_bool==False)
								{
									rule->rule_root->node_symbol->symb_def->sdef_mark |= SDEF_INLINE_IS_CONSTRUCTOR;
								}
							}
						}
					}
				}
			}
		}
	}
}

void GenerateStatesForRule (ImpRuleS *rule)
{
	SymbDef rule_sdef;
	RuleAlts alt;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	StateP function_state_p;
#endif

	CurrentSymbol=rule->rule_root->node_symbol;
	rule_sdef=CurrentSymbol->symb_def;

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	function_state_p=rule->rule_state_p;
#endif

	for_l (alt,rule->rule_alts,alt_next){
		CurrentLine = alt->alt_line;
	
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		set_states_in_lhs (alt->alt_lhs_root->node_arguments,function_state_p);
		alt->alt_lhs_root->node_state = function_state_p[-1]; /* i.e. the result state */
#endif

		scope=1;

		if (alt->alt_kind==Contractum){
			DetermineStatesOfRootNodeAndDefs (alt->alt_rhs_root,&alt->alt_rhs_defs,alt->alt_lhs_root->node_state,0);

#ifdef OBSERVE_ARRAY_SELECTS_IN_PATTERN
			set_states_of_array_selects_in_pattern (alt);
#endif
		} else if (rule->rule_type==NULL)
			StaticMessage (True, "%S", ECodeBlock, CurrentSymbol);
	}

	if (rule_sdef->sdef_arity==1 &&
			function_state_p[-1].state_type==SimpleState && function_state_p[-1].state_kind==OnB && function_state_p[-1].state_object==BoolObj &&
			function_state_p[0].state_type==SimpleState && function_state_p[0].state_kind==StrictOnA)
		mark_is_constructor_function (rule);
}

void GenerateStates (ImpRules rules)
{
	ImpRuleS *rule;
	
	for_l (rule,rules,rule_next)
		GenerateStatesForRule (rule);
}

static NodeDefS **RemoveLocallySharedNodeDefs (NodeDefS **start,NodeDefS **end,NodeDefS **loclist,int scope)
{
	NodeDefS **newend,**current;
	
	newend = start;
	current = start;

	while (current!=end){
		int node_id_scope;
		
		node_id_scope=(*current)->def_id->nid_scope;
		if (node_id_scope<0)
			node_id_scope=-node_id_scope;
		
		if (node_id_scope>scope){
			NodeDefP remove;
			
			remove = *current;
			*start = remove->def_next;
			current = &remove->def_next;
			*loclist = remove;
			loclist = current;
		} else {
			current = start = &(*start)->def_next;
			newend = start;
		} 
	}
	
	*loclist = NULL;
	
	return newend;
}

void DetermineNodeState (Node node)
{
	if (node->node_annotation==NoAnnot)
		node->node_state=LazyState;
	else if (node->node_annotation==StrictAnnot)
		node->node_state=StrictState;
	else {
		SetUnaryState (&node->node_state, DoParallel ? Parallel : OnA, UnknownObj);
		if (DoParallel)
			node->node_state.state_mark |= STATE_PARALLEL_MASK;

		if (node->node_state.state_kind==Parallel){
			if (DoParallel)
				/* node->node_attribute = AnnotHasDeferAttr (node->node_annotation->annot_kind) */;
			else {
				StaticMessage (False, "%S", Wparannot, CurrentSymbol);
				node->node_state.state_kind = OnA;
			}
		}
	}
}

static int NodeIdCount;
static NodeId NodeIdStackTop;

static Bool MarkComponentNodesOnACycle (Node node,int group_number)
{
	if (node->node_number!=0)
		return node->node_number<0;
	
	switch (node->node_kind){
		case NodeIdNode:
		{
			NodeId node_id;
			
			node_id=node->node_node_id;
			
			if (node_id->nid_mark & ON_A_CYCLE_MASK && node_id->nid_number==group_number){
				node->node_number=-1;
				MarkComponentNodesOnACycle (node_id->nid_node,group_number);
				return True;
			} else {
				node->node_number=1;
				return False;
			}
		}
		case NormalNode:
		case UpdateNode:
		case SelectorNode:
		case MatchNode:
		{
			ArgS *arg;
			Bool on_a_cycle;
		
			on_a_cycle=False;
			
			node->node_number=1;
			for_l (arg,node->node_arguments,arg_next)
				if (MarkComponentNodesOnACycle (arg->arg_node,group_number))
					on_a_cycle=True;
			
			if (on_a_cycle)
				node->node_number=-1;
			
			return on_a_cycle;
		}
		case IfNode:
		default:
			error_in_function ("MarkComponentNodesOnACycle");
			return False;
	}
}

static void AddStrictLhsNodeIdsToNodeDefs (StrictNodeIdP strict_node_id,NodeDefs *defs_p)
{
	while (strict_node_id){
		NodeId node_id;

		node_id=strict_node_id->snid_node_id;

		if (node_id->nid_refcount<0){
			NodeDefS *new_def;
			
			new_def = NewNodeDef (node_id,NULL);
/*				node_id->nid_node_def = new_def; */
			
			new_def->def_next=*defs_p;
			*defs_p=new_def;
			defs_p=&new_def->def_next;
		}

		strict_node_id=strict_node_id->snid_next;
	}
}

#ifdef ADD_ARGUMENTS_TO_HIGHER_ORDER_FUNCTIONS
static ImpRuleP new_rules_with_more_arguments,*last_new_rule_with_more_arguments_h;

static int get_symbol_arity_or_zero (SymbolP symbol_p)
{
	switch (symbol_p->symb_kind){
		case definition:
		{
			SymbDef sdef;
			
			sdef=symbol_p->symb_def;
			
			switch (sdef->sdef_kind){
				case DEFRULE:
				case SYSRULE:
				case IMPRULE:
				case CONSTRUCTOR:
				case RECORDTYPE:
					return sdef->sdef_arity;
			}
			break;
		}
		case cons_symb:
			return 2;
		case if_symb:
			return 3;
	}
	
	return 0;
}

static NodeP add_argument_to_node (NodeP rhs_root_p,NodeIdP new_node_id_p);

static NodeP add_argument_to_if_node (NodeP rhs_root_p,NodeIdP new_node_id_p)
{
	ArgP then_arg_p,else_arg_p;
	NodeP else_node_p;
	
	then_arg_p=rhs_root_p->node_arguments->arg_next;
	else_arg_p=then_arg_p->arg_next;
	else_node_p=else_arg_p->arg_node;

	then_arg_p->arg_node=add_argument_to_node (then_arg_p->arg_node,new_node_id_p);
	
	if (else_node_p->node_kind!=NormalNode || else_node_p->node_symbol->symb_kind!=fail_symb){
		--new_node_id_p->nid_refcount;
		else_arg_p->arg_node=add_argument_to_node (else_node_p,new_node_id_p);
	}
				
	return rhs_root_p;
}

static NodeP add_argument_to_switch_node (NodeP rhs_root_p,NodeIdP new_node_id_p)
{
	ArgP arg_p;

	for_l (arg_p,rhs_root_p->node_arguments,arg_next){
		NodeP node_p,*node_h;

		node_p=arg_p->arg_node;
		if (node_p->node_kind==CaseNode){
			node_h=&node_p->node_arguments->arg_node;
			if ((*node_h)->node_kind==PushNode)
				node_h=&(*node_h)->node_arguments->arg_next->arg_node;
		} else if (node_p->node_kind==DefaultNode){
			node_h=&node_p->node_arguments->arg_node;
		} else if (node_p->node_kind==OverloadedCaseNode){
			node_h=&node_p->node_node->node_arguments->arg_node;
		} else
			error_in_function ("add_argument_to_switch_node");

		*node_h=add_argument_to_node (*node_h, new_node_id_p);

		--new_node_id_p->nid_refcount;
	}

	++new_node_id_p->nid_refcount;

	return rhs_root_p;
}

static NodeP add_argument_to_guard_node (NodeP rhs_root_p,NodeIdP new_node_id_p)
{
	add_argument_to_node (rhs_root_p->node_arguments->arg_node, new_node_id_p);
	--new_node_id_p->nid_refcount;
	add_argument_to_node (rhs_root_p->node_arguments->arg_next->arg_node, new_node_id_p);

	return rhs_root_p;
}

static NodeP add_argument_to_node (NodeP rhs_root_p,NodeIdP new_node_id_p)
{
	ArgP new_arg1,new_arg2;

	if (rhs_root_p->node_kind==NormalNode){
		SymbolP root_symbol_p;
		
		root_symbol_p=rhs_root_p->node_symbol;
		if (root_symbol_p->symb_kind==if_symb && rhs_root_p->node_arity==3)
			return add_argument_to_if_node (rhs_root_p,new_node_id_p);
		else if (root_symbol_p->symb_kind == fail_symb)
			return rhs_root_p;
		else if (get_symbol_arity_or_zero (root_symbol_p) > rhs_root_p->node_arity){
			ArgP *last_rhs_arg_h;
			
			new_arg2=NewArgument (NewNodeIdNode (new_node_id_p));
		
			last_rhs_arg_h=&rhs_root_p->node_arguments;
			while (*last_rhs_arg_h)
				last_rhs_arg_h=&(*last_rhs_arg_h)->arg_next;
			
			*last_rhs_arg_h=new_arg2;
			++rhs_root_p->node_arity;
			
			return rhs_root_p;
		}
	} else if (rhs_root_p->node_kind==IfNode)
		return add_argument_to_if_node (rhs_root_p,new_node_id_p);
	else if (rhs_root_p->node_kind==SwitchNode)
		return add_argument_to_switch_node (rhs_root_p,new_node_id_p);
	else if (rhs_root_p->node_kind==GuardNode)
		return add_argument_to_guard_node (rhs_root_p,new_node_id_p);

	new_arg2=NewArgument (NewNodeIdNode (new_node_id_p));

	new_arg1=NewArgument (rhs_root_p);
	new_arg1->arg_next=new_arg2;
	rhs_root_p=NewNode (ApplySymbol,new_arg1,2);
	
	return rhs_root_p;
}

static SymbolP copy_imp_rule_and_add_arguments (SymbDef rule_sdef,int n_extra_arguments)
{
	SymbolP new_symbol_p;
	SymbDef new_sdef_p;
	ImpRuleP old_rule_p,new_rule_p,last_rule_version_p;
	int n_wanted_arguments;
	
	n_wanted_arguments=rule_sdef->sdef_arity + n_extra_arguments;
	
	old_rule_p=rule_sdef->sdef_rule;
	
	while (old_rule_p->rule_mark & RULE_HAS_VERSION_WITH_MORE_ARGUMENTS){
		old_rule_p=old_rule_p->rule_next_function_with_more_arguments;
		
		if (old_rule_p->rule_root->node_symbol->symb_def->sdef_arity==n_wanted_arguments)
			return old_rule_p->rule_root->node_symbol;
	}
	
	new_symbol_p=copy_imp_rule_and_type (rule_sdef);

	new_sdef_p=new_symbol_p->symb_def;
	new_rule_p=new_sdef_p->sdef_rule;
	old_rule_p=rule_sdef->sdef_rule;

	last_rule_version_p=old_rule_p;
	while (last_rule_version_p->rule_mark & RULE_HAS_VERSION_WITH_MORE_ARGUMENTS)
		last_rule_version_p=last_rule_version_p->rule_next_function_with_more_arguments;

	last_rule_version_p->rule_mark |= RULE_HAS_VERSION_WITH_MORE_ARGUMENTS;	
	last_rule_version_p->rule_next_function_with_more_arguments=new_rule_p;

	new_sdef_p->sdef_next_scc=rule_sdef->sdef_next_scc;
	rule_sdef->sdef_next_scc=new_sdef_p;

	copy_imp_rule_nodes (old_rule_p,new_rule_p);

	{
		struct type_alt *rule_type;
		struct type_node *rhs_type_node_p;
		struct type_arg **last_lhs_type_arg_p;
		int n;
		
		rule_type=new_rule_p->rule_type;
		rhs_type_node_p=rule_type->type_alt_rhs;
		last_lhs_type_arg_p=&rule_type->type_alt_lhs->type_node_arguments;
		while (*last_lhs_type_arg_p)
			last_lhs_type_arg_p=&(*last_lhs_type_arg_p)->type_arg_next;
		
		for (n=0; n<n_extra_arguments; ++n){
#if 0
			if (rhs_type_node_p->type_node_is_var){
				struct type_arg *new_type_arg_p;
				
				new_type_arg_p=NewTypeArgument (NewTypeVarNode (NewTypeVar (NULL),NoAnnot,NoAttr));

				*last_lhs_type_arg_p=new_type_arg_p;
				last_lhs_type_arg_p=&new_type_arg_p->type_arg_next;
			} else
#else				
			if (rhs_type_node_p->type_node_is_var || rhs_type_node_p->type_node_symbol->symb_kind!=fun_type)
				error_in_function ("copy_imp_rule_and_add_arguments");
#endif
			{
				struct type_arg *first_arg_p;
				
				if (rhs_type_node_p->type_node_symbol->symb_kind!=fun_type)
					error_in_function ("copy_imp_rule_and_add_arguments");
							
				first_arg_p=rhs_type_node_p->type_node_arguments;
				*last_lhs_type_arg_p=first_arg_p;
				
				first_arg_p->type_arg_node->type_node_annotation=NoAnnot;
				
				last_lhs_type_arg_p=&first_arg_p->type_arg_next;

				rhs_type_node_p=first_arg_p->type_arg_next->type_arg_node;
			}
		}
		
		*last_lhs_type_arg_p=NULL;
		rule_type->type_alt_rhs=rhs_type_node_p;
		
		rule_type->type_alt_lhs->type_node_arity += n_extra_arguments;
	}
	
	{
		RuleAltP alt_p;
		
		for_l (alt_p,new_rule_p->rule_alts,alt_next){
			int n;
			ArgP *last_lhs_arg_h;
			
			last_lhs_arg_h=&alt_p->alt_lhs_root->node_arguments;
			while (*last_lhs_arg_h)
				last_lhs_arg_h=&(*last_lhs_arg_h)->arg_next;
			
			for (n=0; n<n_extra_arguments; ++n){
				NodeIdP new_node_id_p;
				ArgP new_arg;
				
				new_node_id_p=NewNodeId (NULL);
				new_node_id_p->nid_refcount=-2;
				
				new_arg=NewArgument (NewNodeIdNode (new_node_id_p));
				
				*last_lhs_arg_h=new_arg;
				last_lhs_arg_h=&new_arg->arg_next;
								
				alt_p->alt_rhs_root=add_argument_to_node (alt_p->alt_rhs_root,new_node_id_p);
			}
			
			*last_lhs_arg_h=NULL;
			
			alt_p->alt_lhs_root->node_arity += n_extra_arguments;
		}
		
		new_sdef_p->sdef_arity += n_extra_arguments;
	}
	
	new_rule_p->rule_next=NULL;
	
	*last_new_rule_with_more_arguments_h=new_rule_p;
	last_new_rule_with_more_arguments_h=&new_rule_p->rule_next;
	
	return new_symbol_p;
}

static void create_new_function_with_more_arguments (NodeP node_p)
{
	NodeP function_node_p;
	int n_extra_arguments;
	
	n_extra_arguments=1;
	function_node_p=node_p->node_arguments->arg_node;

	if (function_node_p->node_kind==NodeIdNode && function_node_p->node_node_id->nid_refcount==1 && function_node_p->node_node_id->nid_node->node_annotation==NoAnnot){
		function_node_p=function_node_p->node_node_id->nid_node;
		node_p->node_arguments->arg_node=function_node_p;
	}
	
	while (function_node_p->node_kind==NormalNode && function_node_p->node_symbol->symb_kind==apply_symb){
		ArgP next_function_node_p_arg;
		
		next_function_node_p_arg=function_node_p->node_arguments;
		function_node_p=next_function_node_p_arg->arg_node;
		++n_extra_arguments;

		if (function_node_p->node_kind==NodeIdNode && function_node_p->node_node_id->nid_refcount==1 && function_node_p->node_node_id->nid_node->node_annotation==NoAnnot){
			function_node_p=function_node_p->node_node_id->nid_node;
			next_function_node_p_arg->arg_node=function_node_p;
		}
	}
	
	if (function_node_p->node_kind==NormalNode){
		SymbolP function_symbol_p;
		
		function_symbol_p=function_node_p->node_symbol;

		if (function_symbol_p->symb_kind==definition){
			if (function_symbol_p->symb_def->sdef_kind==IMPRULE){
				if (!(function_symbol_p->symb_def->sdef_rule->rule_mark & RULE_CAF_MASK) && function_symbol_p->symb_def->sdef_rule->rule_alts->alt_kind==Contractum
					&& function_node_p->node_arity <= function_symbol_p->symb_def->sdef_arity)
				{
					struct type_node *rhs_type_node_p;
					SymbolP new_function_symbol;
					SymbDef rule_sdef;
					int n_extra_function_arguments,n;

					rule_sdef=function_symbol_p->symb_def;
					rhs_type_node_p=rule_sdef->sdef_rule->rule_type->type_alt_rhs;
					
					n_extra_function_arguments=n_extra_arguments+function_node_p->node_arity-rule_sdef->sdef_arity;
					
					n=n_extra_function_arguments;
					while (n>0){
						if (rhs_type_node_p->type_node_is_var){
#if 0
							n=0;
#endif
							break;
						} else if (rhs_type_node_p->type_node_symbol->symb_kind==fun_type){
							rhs_type_node_p=rhs_type_node_p->type_node_arguments->type_arg_next->type_arg_node;
							--n;
						} else
							break;
					}
										
					if (n<=0){
						if (n_extra_function_arguments>0){
							new_function_symbol=copy_imp_rule_and_add_arguments (rule_sdef,n_extra_function_arguments);					

							node_p->node_symbol=new_function_symbol;
						} else
							node_p->node_symbol=function_node_p->node_symbol;
					} else
						return;
				} else
					return;
			} else if (function_symbol_p->symb_def->sdef_kind==DEFRULE || function_symbol_p->symb_def->sdef_kind==SYSRULE){
				if (function_node_p->node_arity + n_extra_arguments <= function_symbol_p->symb_def->sdef_arity){
					node_p->node_symbol=function_node_p->node_symbol;
				} else
					return;
			} else
				return;

			{
				NodeP function_node_p2;
				ArgP *arg_h;

				function_node_p2=node_p->node_arguments->arg_node;
				node_p->node_arguments=node_p->node_arguments->arg_next;
				
				while (function_node_p2!=function_node_p){
					ArgP second_arg_p;
					
					second_arg_p=function_node_p2->node_arguments->arg_next;
					
					second_arg_p->arg_next=node_p->node_arguments;
					node_p->node_arguments=second_arg_p;
					
					function_node_p2=function_node_p2->node_arguments->arg_node;							
				}
									
				arg_h=&function_node_p->node_arguments;
				while (*arg_h!=NULL)
					arg_h=&(*arg_h)->arg_next;
				
				*arg_h=node_p->node_arguments;
				node_p->node_arguments=function_node_p->node_arguments;
				
				node_p->node_arity=function_node_p->node_arity+n_extra_arguments;
				
				return;
			}
		} else if (function_symbol_p->symb_kind==if_symb && function_node_p->node_arity==3){
			NodeP apply_node_p;
			int n_lhs_node_id_applies;
			
			n_lhs_node_id_applies=0;
			apply_node_p=node_p;
			while (apply_node_p->node_kind==NormalNode && apply_node_p->node_symbol->symb_kind==apply_symb){
				ArgP apply_arg1;
				NodeP apply_arg2_node_p;
				
				apply_arg1=apply_node_p->node_arguments;
				apply_arg2_node_p=apply_arg1->arg_next->arg_node;
				
				if (apply_arg2_node_p->node_kind==NodeIdNode && apply_arg2_node_p->node_node_id->nid_refcount<0){
					apply_node_p=apply_arg1->arg_node;
					++n_lhs_node_id_applies;
				} else
					break;
			}
			
			if (n_lhs_node_id_applies==n_extra_arguments){
				int n;
				
				for (n=0; n<n_extra_arguments; ++n){
					int m;
					
					apply_node_p=node_p;

					for (m=0; m<n_extra_arguments-1-n; ++m)
						apply_node_p=apply_node_p->node_arguments->arg_node;

					function_node_p=add_argument_to_node (function_node_p,apply_node_p->node_arguments->arg_next->arg_node->node_node_id);
				}
				
				*node_p=*function_node_p;
				DetermineNodeState (node_p);
			}
		}
	}
}
#endif

static NodeDefs *CollectSharedNodeIdsInNode (Node* node_p,NodeId parent_node_id,NodeDefs *last)
{
	Node node;
	
	node=*node_p;
	if (node->node_kind==NodeIdNode){
		NodeId node_id;

		node_id=node->node_node_id;

		if (node_id->nid_refcount>0){
			if (!(node_id->nid_mark & SHARED_NODES_COLLECTED_MASK)){
				node_id->nid_mark |= SHARED_NODES_COLLECTED_MASK;

				node_id->nid_ref_count_copy_=node_id->nid_refcount;

				if (node_id->nid_refcount>1 || node_id->nid_node->node_annotation){
					int my_number;
				
					my_number=NodeIdCount++;

					node_id->nid_number=my_number;
					node_id->nid_forward_node_id_=NodeIdStackTop;
					NodeIdStackTop=node_id;
					
					last = CollectSharedNodeIdsInNode (&node_id->nid_node,node_id,last);
				
					if (/* node_id->nid_forward_node_id && */ parent_node_id)
						parent_node_id->nid_number=MINIMUM (parent_node_id->nid_number,node_id->nid_number);

					if (node_id->nid_number==my_number){
						NodeId next_node_id;
						NodeDefs newdef;
							
						next_node_id = NodeIdStackTop;
						NodeIdStackTop = next_node_id->nid_forward_node_id;
						next_node_id->nid_forward_node_id_ = NULL;

						newdef = NewNodeDef (next_node_id,next_node_id->nid_node);
						next_node_id->nid_node_def_ = newdef;
						*last=newdef;
						last=&newdef->def_next;

						if (next_node_id!=node_id){
							next_node_id->nid_number=my_number;
							next_node_id->nid_mark|=ON_A_CYCLE_MASK;

							do {
								next_node_id = NodeIdStackTop;
								NodeIdStackTop = next_node_id->nid_forward_node_id;
								next_node_id->nid_forward_node_id_ = NULL;
						
								next_node_id->nid_number=my_number;
								next_node_id->nid_mark|=ON_A_CYCLE_MASK;

								newdef = NewNodeDef (next_node_id,next_node_id->nid_node);
								next_node_id->nid_node_def_ = newdef;
								*last=newdef;
								last=&newdef->def_next;
							} while (next_node_id!=node_id);
						}
						
						if (node_id->nid_mark & ON_A_CYCLE_MASK)
							MarkComponentNodesOnACycle (node_id->nid_node,node_id->nid_number);
					}
				} else {
					*node_p=node_id->nid_node;
					last = CollectSharedNodeIdsInNode (node_p,parent_node_id,last);
				}
			} else
				if (node_id->nid_forward_node_id){
					node_id->nid_mark|=ON_A_CYCLE_MASK;
					parent_node_id->nid_number=MINIMUM (parent_node_id->nid_number,node_id->nid_number);
				}
		} else
			node_id->nid_ref_count_copy_=node_id->nid_refcount;
	} else {
		DetermineNodeState (node);

		if (node->node_annotation==ParallelAtAnnot){
			Node at_node;
		
			at_node=get_p_at_node (node);

			last = CollectSharedNodeIdsInNode (&at_node,parent_node_id,last);
		}

		if (node->node_kind==IfNode){
			NodeDefs *shared;
			Args cond_arg,then_arg,else_arg;
			int local_scope;
	
			cond_arg=node->node_arguments;
			then_arg=cond_arg->arg_next;
			else_arg=then_arg->arg_next;
						
			local_scope=scope+1;
			scope+=3;

			cond_arg->arg_state=LazyState;
			
			/*{
				Node root_node;
		
				root_node=cond_arg->arg_node;
				if (root_node->node_kind==NodeIdNode && root_node->node_node_id->nid_refcount==1)
					root_node->node_node_id->nid_node->node_annotation=NoAnnot;
			}*/
			
			last = CollectSharedNodeIdsInNode (&cond_arg->arg_node,parent_node_id,last);
			++scope;
	
			shared=last;
			then_arg->arg_state=LazyState;
			
			/*{
				Node root_node;
		
				root_node=then_arg->arg_node;
				if (root_node->node_kind==NodeIdNode && root_node->node_node_id->nid_refcount==1)
					root_node->node_node_id->nid_node->node_annotation=NoAnnot;
			}*/
			
			last=CollectSharedNodeIdsInNode (&then_arg->arg_node,parent_node_id,last);
			last=RemoveLocallySharedNodeDefs (shared,last,&node->node_then_node_defs,local_scope);
	
			++scope;
			
			shared=last;
			else_arg->arg_state=LazyState;
			
			/*{
				Node root_node;
		
				root_node=else_arg->arg_node;
				if (root_node->node_kind==NodeIdNode && root_node->node_node_id->nid_refcount==1)
					root_node->node_node_id->nid_node->node_annotation=NoAnnot;
			}*/
			
			last=CollectSharedNodeIdsInNode (&else_arg->arg_node,parent_node_id,last);
			last=RemoveLocallySharedNodeDefs (shared,last,&node->node_else_node_defs,local_scope);

			AddStrictLhsNodeIdsToNodeDefs (node->node_then_strict_node_ids,&node->node_then_node_defs);
			AddStrictLhsNodeIdsToNodeDefs (node->node_else_strict_node_ids,&node->node_else_node_defs);
		}
		else if (node->node_kind==SwitchNode)
		{
			error_in_function ("CollectSharedNodeIdsInNode");
		}
		else {
			ArgP arg;

#ifdef ADD_ARGUMENTS_TO_HIGHER_ORDER_FUNCTIONS
			if (node->node_kind==NormalNode && node->node_symbol->symb_kind==apply_symb)
				create_new_function_with_more_arguments (node);
#endif
			
			for_l (arg,node->node_arguments,arg_next){
				arg->arg_state=LazyState;
				last = CollectSharedNodeIdsInNode (&arg->arg_node,parent_node_id,last);
			}
		}
	}

	return last;
}

/* RWS ... */
/* parent_node_id always NULL? */
static NodeDefs *CollectSharedNodeIdsInRootNode (Node* node_p,NodeId parent_node_id,NodeDefs *last)
{
	NodeP	root_node;

	root_node=*node_p;

	switch (root_node->node_kind){
		case SwitchNode:
		{
			ArgP arg_p;
		
			for_l (arg_p,root_node->node_arguments,arg_next){
				NodeP node;
				
				node=arg_p->arg_node;
				if (node->node_kind==CaseNode){
					NodeP case_alt_node_p;
					NodeDefs *case_last;

					case_alt_node_p=node->node_arguments->arg_node;
					case_last=&node->node_node_defs;
/*	Codewarrior bug			if (case_alt_node_p->node_kind==PushNode){ */
					if (node->node_arguments->arg_node->node_kind==PushNode)
						case_last=CollectSharedNodeIdsInRootNode (&case_alt_node_p->node_arguments->arg_next->arg_node, parent_node_id, case_last);
					else
						case_last=CollectSharedNodeIdsInRootNode (&node->node_arguments->arg_node, parent_node_id, case_last);
					*case_last=NULL;

					AddStrictLhsNodeIdsToNodeDefs (node->node_strict_node_ids,&node->node_node_defs);
				} else if (node->node_kind==DefaultNode){
					NodeDefs *default_last;

					default_last=&node->node_node_defs;
					default_last=CollectSharedNodeIdsInRootNode (&node->node_arguments->arg_node, parent_node_id, default_last);
					*default_last=NULL;

					AddStrictLhsNodeIdsToNodeDefs (node->node_strict_node_ids,&node->node_node_defs);
				} else if (node->node_kind==OverloadedCaseNode){
					NodeP case_node_p;
					NodeDefs *case_last;
					ArgP arg;

					arg=node->node_arguments;
					arg->arg_state=LazyState;
					last = CollectSharedNodeIdsInNode (&arg->arg_node,parent_node_id,last);
					arg=arg->arg_next;
					arg->arg_state=LazyState;
					last = CollectSharedNodeIdsInNode (&arg->arg_node,parent_node_id,last);

					case_node_p=node->node_node;
					case_last=&case_node_p->node_node_defs;
					case_last=CollectSharedNodeIdsInRootNode (&case_node_p->node_arguments->arg_node, parent_node_id, case_last);
					*case_last=NULL;
				} else
					error_in_function ("CollectSharedNodeIdsInRootNode");
			}
			break;
		}
		case GuardNode:
		{
			NodeDefs *guard_last;

			last = CollectSharedNodeIdsInRootNode (&root_node->node_arguments->arg_node, parent_node_id, last);
			guard_last=&root_node->node_node_defs;
			guard_last=CollectSharedNodeIdsInRootNode (&root_node->node_arguments->arg_next->arg_node, parent_node_id, guard_last);
			*guard_last=NULL;
			break;
		}
		default:
			scope=1;
			last=CollectSharedNodeIdsInNode (node_p,parent_node_id,last);
			break;

	}

	return last;
}
/* ... RWS */

static void CollectSharedAndAnnotatedNodesInRhs (NodeS **root_p,NodeDefS **defs_p,StrictNodeIdP strict_node_ids)
{
	NodeDefS **last;
	NodeP root_node;
/*	
	scope=1;
*/
	NodeIdCount=1;
	NodeIdStackTop = (NodeId)-1;

	root_node=*root_p;
	
	/* removed, causes crash if let! in other scope
	if (root_node->node_kind==NodeIdNode && root_node->node_node_id->nid_refcount==1)
		root_node->node_node_id->nid_node->node_annotation=NoAnnot;
	*/

	last=defs_p;
	
/* RWS ...
	last = CollectSharedNodeIdsInNode (root_p,NULL,last);
*/
	last = CollectSharedNodeIdsInRootNode (root_p,NULL,last);
/* ... RWS */
	*last = NULL;

	AddStrictLhsNodeIdsToNodeDefs (strict_node_ids,defs_p);
}

static void AnnotateStrictNodeIds (Node node,StrictNodeIdP strict_node_ids,NodeDefS **node_def_h)
{
	StrictNodeIdP strict_node_id;

	for_l (strict_node_id,strict_node_ids,snid_next){
		NodeId node_id;

		node_id=strict_node_id->snid_node_id;

#ifdef OBSERVE_ARRAY_SELECTS_IN_PATTERN
		if (strict_node_id->snid_array_select_in_pattern && node_id->nid_node->node_symbol->symb_kind==select_symb){
			NodeP array_uselect_node;
			SymbDef uselect_sdef;
			TypeArg *type_arg;
			
			array_uselect_node=node_id->nid_node->node_arguments->arg_node;
			uselect_sdef=array_uselect_node->node_symbol->symb_def;
			
			if (uselect_sdef->sdef_kind==IMPRULE)
				type_arg=uselect_sdef->sdef_rule->rule_type->type_alt_lhs->type_node_arguments;
			else
				type_arg=uselect_sdef->sdef_rule_type->rule_type_rule->type_alt_lhs->type_node_arguments;

			if (!type_arg->type_arg_node->type_node_is_var &&
				(type_arg->type_arg_node->type_node_symbol->symb_kind==strict_array_type ||
				 type_arg->type_arg_node->type_node_symbol->symb_kind==unboxed_array_type)
			){
				node_id->nid_node->node_annotation=StrictAnnot;
			} else {
				NodeIdP uselect_node_id_p;
				NodeDefP new_def;

				uselect_node_id_p=NewNodeId (NULL);
				uselect_node_id_p->nid_refcount=1;

				node_id->nid_node->node_arguments->arg_node=NewNodeIdNode (uselect_node_id_p);
				array_uselect_node->node_annotation=StrictAnnot;
				
				strict_node_id->snid_node_id=uselect_node_id_p;
				
				new_def = NewNodeDef (uselect_node_id_p,array_uselect_node);
				uselect_node_id_p->nid_node=array_uselect_node;
				new_def->def_next=*node_def_h;
				*node_def_h=new_def;
				node_def_h=&new_def->def_next;
			}
		} else
#endif
			if (node_id->nid_refcount>0 && node_id->nid_node)
				node_id->nid_node->node_annotation=StrictAnnot;
	}
	
	switch (node->node_kind){
		case IfNode:
		{
			ArgS *arg;
		
			arg=node->node_arguments;
			AnnotateStrictNodeIds (arg->arg_node,NULL,NULL);
			arg = arg->arg_next;
			AnnotateStrictNodeIds (arg->arg_node,node->node_then_strict_node_ids,&node->node_then_node_defs);
			arg = arg->arg_next;
			AnnotateStrictNodeIds (arg->arg_node,node->node_else_strict_node_ids,&node->node_else_node_defs);
			break;
		}
		case SwitchNode:
		{
			ArgS *arg_p;

			for_l (arg_p,node->node_arguments,arg_next){
				NodeP node;

				node=arg_p->arg_node;
				if (node->node_kind==CaseNode){
					NodeP case_alt_node_p;

					case_alt_node_p=node->node_arguments->arg_node;
		/*	Codewarrior bug			if (case_alt_node_p->node_kind==PushNode){  */
					if (node->node_arguments->arg_node->node_kind==PushNode)
						AnnotateStrictNodeIds (case_alt_node_p->node_arguments->arg_next->arg_node,node->node_strict_node_ids,&node->node_node_defs);
					else
						AnnotateStrictNodeIds (node->node_arguments->arg_node,node->node_strict_node_ids,&node->node_node_defs);
				} else if (node->node_kind==DefaultNode){
					AnnotateStrictNodeIds (node->node_arguments->arg_node,node->node_strict_node_ids,&node->node_node_defs);
				} else if (node->node_kind==OverloadedCaseNode){
					NodeP case_node_p;
					
					case_node_p=node->node_node;
					AnnotateStrictNodeIds (case_node_p->node_arguments->arg_node,case_node_p->node_strict_node_ids,&case_node_p->node_node_defs);
				} else
					error_in_function ("AnnotateStrictNodeIds");
			}
			break;
		}
		case GuardNode:
		{
			AnnotateStrictNodeIds (node->node_arguments->arg_node,strict_node_ids,node_def_h);
			AnnotateStrictNodeIds (node->node_arguments->arg_next->arg_node,node->node_guard_strict_node_ids,&node->node_node_defs);
			break;
		}
		default:
			break;

	}
}

static void DetermineSharedAndAnnotatedNodesOfRule (ImpRuleP rule)
{
	SymbDef rule_sdef;
	RuleAlts alt;

	CurrentSymbol=rule->rule_root->node_symbol;
	
	rule_sdef=CurrentSymbol->symb_def;

	for_l (alt,rule->rule_alts,alt_next)
		if (alt->alt_kind==Contractum){
			CurrentLine = alt->alt_line;

			AnnotateStrictNodeIds (alt->alt_rhs_root,alt->alt_strict_node_ids,&alt->alt_rhs_defs);

			CollectSharedAndAnnotatedNodesInRhs (&alt->alt_rhs_root,&alt->alt_rhs_defs,alt->alt_strict_node_ids);
		}
}

static void reset_states_and_ref_count_copies_of_node_defs (NodeDefS *node_def);

static void reset_states_and_ref_count_copies_of_node (Node node)
{
	if (node->node_kind==NodeIdNode){
		NodeId node_id;

		node_id=node->node_node_id;

		node_id->nid_ref_count_copy_=node_id->nid_refcount;
#if OPTIMIZE_LAZY_TUPLE_RECURSION
		node_id->nid_mark2 &= ~NID_HAS_LAZY_SELECTOR_COUNTER;
#endif
	} else {
		DetermineNodeState (node);

		if (node->node_annotation==ParallelAtAnnot){
			Node at_node;
		
			at_node=get_p_at_node (node);

			reset_states_and_ref_count_copies_of_node (at_node);
		}

		if (node->node_kind!=IfNode){
			ArgP arg;
			
			for_l (arg,node->node_arguments,arg_next){
				arg->arg_state=LazyState;
				reset_states_and_ref_count_copies_of_node (arg->arg_node);
			}
		/* RWS SwitchNode */
		} else {
			Args cond_arg,then_arg,else_arg;
	
			cond_arg=node->node_arguments;
			then_arg=cond_arg->arg_next;
			else_arg=then_arg->arg_next;
			
			cond_arg->arg_state=LazyState;
			reset_states_and_ref_count_copies_of_node (cond_arg->arg_node);
	
			then_arg->arg_state=LazyState;
			reset_states_and_ref_count_copies_of_node (then_arg->arg_node);
				
			else_arg->arg_state=LazyState;
			reset_states_and_ref_count_copies_of_node (else_arg->arg_node);
			
			reset_states_and_ref_count_copies_of_node_defs (node->node_then_node_defs);
			reset_states_and_ref_count_copies_of_node_defs (node->node_else_node_defs);
		}
	}
}

static void reset_states_and_ref_count_copies_of_node_defs (NodeDefS *node_defs)
{
	NodeDefS *node_def;
	
	for_l (node_def,node_defs,def_next)
		if (node_def->def_node!=NULL){
			node_def->def_id->nid_ref_count_copy_=node_def->def_id->nid_refcount;
			node_def->def_id->nid_node_def_=node_def;
			reset_states_and_ref_count_copies_of_node (node_def->def_node);
		}
}

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
static void reset_states_and_ref_count_copies_of_root_node (NodeP node_p)
{
	if (node_p->node_kind==IfNode){
		Args cond_arg,then_arg,else_arg;

		DetermineNodeState (node_p);
		
		cond_arg=node_p->node_arguments;
		then_arg=cond_arg->arg_next;
		else_arg=then_arg->arg_next;
		
		cond_arg->arg_state=LazyState;
		reset_states_and_ref_count_copies_of_root_node (cond_arg->arg_node);

		then_arg->arg_state=LazyState;
		reset_states_and_ref_count_copies_of_root_node (then_arg->arg_node);
			
		else_arg->arg_state=LazyState;
		reset_states_and_ref_count_copies_of_root_node (else_arg->arg_node);
		
		reset_states_and_ref_count_copies_of_node_defs (node_p->node_then_node_defs);
		reset_states_and_ref_count_copies_of_node_defs (node_p->node_else_node_defs);
	} else if (node_p->node_kind==SwitchNode){
		ArgP arg_p;
		
		for_l (arg_p,node_p->node_arguments,arg_next){
			NodeP node_p;

			node_p=arg_p->arg_node;
			if (node_p->node_kind==CaseNode){
				NodeP case_alt_node_p;
				
				case_alt_node_p=node_p->node_arguments->arg_node;
				if (case_alt_node_p->node_kind==PushNode){
					NodeIdListElementP node_id_list;
					
					for_l (node_id_list,case_alt_node_p->node_node_ids,nidl_next)
						node_id_list->nidl_node_id->nid_ref_count_copy_=node_id_list->nidl_node_id->nid_refcount;
					
					case_alt_node_p=case_alt_node_p->node_arguments->arg_next->arg_node;
				}
				
				reset_states_and_ref_count_copies_of_root_node (case_alt_node_p);
				reset_states_and_ref_count_copies_of_node_defs (node_p->node_node_defs);
			} else if (node_p->node_kind==DefaultNode){
				reset_states_and_ref_count_copies_of_root_node (node_p->node_arguments->arg_node);
				reset_states_and_ref_count_copies_of_node_defs (node_p->node_node_defs);
			} else if (node_p->node_kind==OverloadedCaseNode){
				NodeP case_node_p;

				case_node_p=node_p->node_node;
												
				reset_states_and_ref_count_copies_of_root_node (case_node_p->node_arguments->arg_node);
				reset_states_and_ref_count_copies_of_node_defs (case_node_p->node_node_defs);

				reset_states_and_ref_count_copies_of_node (node_p->node_arguments->arg_node);
				reset_states_and_ref_count_copies_of_node (node_p->node_arguments->arg_next->arg_node);
			} else
				error_in_function ("reset_states_and_ref_count_copies_of_root_node");
		}
	} else if (node_p->node_kind==GuardNode){
		reset_states_and_ref_count_copies_of_root_node (node_p->node_arguments->arg_node);
		reset_states_and_ref_count_copies_of_root_node (node_p->node_arguments->arg_next->arg_node);
		reset_states_and_ref_count_copies_of_node_defs (node_p->node_node_defs);
	} else
		reset_states_and_ref_count_copies_of_node (node_p);
}
#endif

void reset_states_and_ref_count_copies (ImpRuleS *rule)
{
	SymbDef rule_sdef;
	RuleAlts alt;

	CurrentSymbol=rule->rule_root->node_symbol;
	
	rule_sdef=CurrentSymbol->symb_def;

	for_l (alt,rule->rule_alts,alt_next)
		if (alt->alt_kind==Contractum){
			CurrentLine = alt->alt_line;

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			reset_states_and_ref_count_copies_of_root_node (alt->alt_rhs_root);
#else
			reset_states_and_ref_count_copies_of_node (alt->alt_rhs_root);
#endif
			reset_states_and_ref_count_copies_of_node_defs (alt->alt_rhs_defs);
		}
}

void DetermineSharedAndAnnotatedNodes (ImpRules rules,SymbolP *im_symbols_h)
{
	ImpRuleP rule;

#ifdef ADD_ARGUMENTS_TO_HIGHER_ORDER_FUNCTIONS
	last_new_rule_with_more_arguments_h=&new_rules_with_more_arguments;
#endif
	
	for_l (rule,rules,rule_next)
		DetermineSharedAndAnnotatedNodesOfRule (rule);

#ifdef ADD_ARGUMENTS_TO_HIGHER_ORDER_FUNCTIONS
	if (new_rules_with_more_arguments!=NULL){
		ImpRuleP *rule_h,new_rule_p;
		
		rule_h=&rules;
		while (*rule_h!=NULL)
			rule_h=&(*rule_h)->rule_next;
			
		while (*im_symbols_h)
			im_symbols_h=&(*im_symbols_h)->symb_next;
		
		while (new_rules_with_more_arguments!=NULL){
			SymbolP new_symbol_p;
			
			new_rule_p=new_rules_with_more_arguments;
		
			DetermineSharedAndAnnotatedNodesOfRule (new_rule_p);

			new_rules_with_more_arguments=new_rule_p->rule_next;
		
			*rule_h=new_rule_p;
			rule_h=&new_rule_p->rule_next;
			
			new_symbol_p=new_rule_p->rule_root->node_symbol;
			*im_symbols_h=new_symbol_p;
			im_symbols_h=&new_symbol_p->symb_next;
		}
		
		*rule_h=NULL;
		*im_symbols_h=NULL;
	}
#endif
}

void InitStatesGen (void)
{
	SetUnaryState (& StrictState, StrictOnA, UnknownObj);
	SetUnaryState (& LazyState, OnA, UnknownObj);
	
	SetUnaryState (& BasicSymbolStates[int_type], OnB, IntObj);
	SetUnaryState (& BasicSymbolStates[bool_type], OnB, BoolObj);
	SetUnaryState (& BasicSymbolStates[char_type], OnB, CharObj);
	SetUnaryState (& BasicSymbolStates[string_type], StrictOnA, StringObj);
	SetUnaryState (& BasicSymbolStates[real_type], OnB, RealObj);
	SetUnaryState (& BasicSymbolStates[file_type], OnB, FileObj);
	SetUnaryState (& BasicSymbolStates[world_type], StrictOnA, WorldObj);
	SetUnaryState (& BasicSymbolStates[procid_type], OnB, ProcIdObj);
	SetUnaryState (& BasicSymbolStates[redid_type], OnB, RedIdObj);
	SetUnaryState (& BasicSymbolStates[int_denot], OnB, IntObj);
	SetUnaryState (& BasicSymbolStates[bool_denot], OnB, BoolObj);
	SetUnaryState (& BasicSymbolStates[char_denot], OnB, CharObj);
	SetUnboxedArrayState (& BasicSymbolStates[string_denot],&BasicSymbolStates[char_type]);
	SetUnaryState (& BasicSymbolStates[real_denot], OnB, RealObj);
	SetUnaryState (& BasicSymbolStates[array_type], StrictOnA, ArrayObj);
	SetUnaryState (& BasicSymbolStates[strict_array_type], StrictOnA, StrictArrayObj);
	SetUnaryState (& BasicSymbolStates[unboxed_array_type], StrictOnA, UnboxedArrayObj);
	SetUnaryState (& BasicSymbolStates[fun_type], StrictOnA, UnknownObj);
	SetUnaryState (& BasicSymbolStates[list_type], StrictOnA, ListObj);
	SetUnaryState (& BasicSymbolStates[tuple_type], StrictOnA, TupleObj);
#ifdef CLEAN2
	SetUnaryState (& BasicSymbolStates[dynamic_type], StrictOnA, DynamicObj);
	SetUnaryState (& BasicSymbolStates[rational_denot], StrictOnA, UnknownObj);
#endif
}
