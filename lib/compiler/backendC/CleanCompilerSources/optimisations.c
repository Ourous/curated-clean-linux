/*
	File:	optimisations.c
	Author:	John van Groningen
*/

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "syntaxtr.t"
#include "comsupport.h"
#include "statesgen.h"
#include "checker.h"
#include "scanner.h"
#include "buildtree.h"
#include "codegen_types.h"
#include "optimisations.h"
#include "codegen1.h"
#include "codegen2.h"
#include "sa.h"
#include "settings.h"
#include "pattern_match.h"

#define STRICT_STATE_FOR_LAZY_TUPLE_CONSTRUCTORS
#define UNTUPLE_STRICT_TUPLES /* also in statesgen.c */
#define MOVE_TUPLE_RECORD_AND_ARRAY_RESULT_FUNCTION_ARGUMENT_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
#define MOVE_APPLY_NODES_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
#define THUNK_LIFT_U_RECORD_SELECTORS
#define THUNK_LIFT_0_CONSTRUCTORS

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)
#define for_la(v1,v2,l1,l2,n) for(v1=(l1),v2=(l2);v1!=NULL;v1=v1->n,++v2)
#define for_li(v,i,l,n) for(v=(l),i=0;v!=NULL;v=v->n,++i)
#define for_l_l(v1,l1,n1,v2,l2,n2) for(v1=(l1),v2=(l2);v1!=NULL;v1=v1->n1,v2=v2->n2)
#define for_lla(v1,v2,v3,l1,l2,l3,n1,n2) for(v1=(l1),v2=(l2),v3=(l3);v1!=NULL;v1=v1->n1,v2=v2->n2,++v3)

#define BETWEEN(l,h,v) ((unsigned)((v)-(l)) <= (unsigned)((h)-(l)))

static void error_in_function (char *m)
{
	ErrorInCompiler ("optimisations.c",m,"");
}

#define MAX_N_VERSIONS 3

static int function_changed;

static int tuple_state_has_more_strictness (StateS *state_p,TypeNode type_node,StateS *function_state_p)
{
	StateS *arg_state_p,*function_arg_state_p;
	TypeArg *type_arg;

	if (type_node->type_node_is_var || type_node->type_node_symbol->symb_kind!=tuple_type)
		return 0;
	
	if (type_node->type_node_arity!=state_p->state_arity || type_node->type_node_symbol->symb_arity!=state_p->state_arity)
		return 0;
	
	type_arg=type_node->type_node_arguments;
	arg_state_p=state_p->state_tuple_arguments;
	function_arg_state_p=function_state_p->state_tuple_arguments;
	
	while (type_arg!=NULL){
		switch (arg_state_p->state_type){
			case SimpleState:
				if (! IsLazyStateKind (arg_state_p->state_kind))
					if (IsLazyState (*function_arg_state_p))
						return 1;
				break;
			case TupleState:
				if (IsLazyState (*function_arg_state_p))
					return 1;
				
				if (function_arg_state_p->state_type==TupleState)
					if (tuple_state_has_more_strictness (arg_state_p,type_arg->type_arg_node,function_arg_state_p))
						return 1;
				break;
			case ArrayState:
			case RecordState:
				if (IsLazyState (*function_arg_state_p))
					return 1;
				break;
		}
		
		type_arg=type_arg->type_arg_next;
		++arg_state_p;
		++function_arg_state_p;
	}
	
	return 0;
}

static int equal_strictness_in_types (TypeNode lazy_type_node,TypeNode strict_type_node)
{
	TypeArg *lazy_type_arg,*strict_type_arg;

	if (lazy_type_node->type_node_is_var || lazy_type_node->type_node_symbol->symb_kind!=tuple_type)
		return 0;
	
	for_l_l (lazy_type_arg,lazy_type_node->type_node_arguments,type_arg_next,
			strict_type_arg,strict_type_node->type_node_arguments,type_arg_next)
	{
		TypeNode lazy_type_arg_node,strict_type_arg_node;

		lazy_type_arg_node=lazy_type_arg->type_arg_node;
		strict_type_arg_node=strict_type_arg->type_arg_node;
				
		if (lazy_type_arg_node->type_node_annotation==StrictAnnot != strict_type_arg_node->type_node_annotation==StrictAnnot)
			return 0;

		if (!lazy_type_arg_node->type_node_is_var && lazy_type_arg_node->type_node_symbol->symb_kind==tuple_type)
			if (!equal_strictness_in_types (lazy_type_arg_node,strict_type_arg_node))
				return 0;				
	}
	
	return 1;
}

static int type_and_strictness_in_state_equals_type (TypeNode lazy_type_node,StateS *state_p,TypeNode strict_type_node)
{
	StateS *arg_state_p;
	TypeArg *lazy_type_arg,*strict_type_arg;

	if (lazy_type_node->type_node_is_var || lazy_type_node->type_node_symbol->symb_kind!=tuple_type)
		return 0;
	
	if (lazy_type_node->type_node_arity!=state_p->state_arity || lazy_type_node->type_node_symbol->symb_arity!=state_p->state_arity)
		return 0;
	
	arg_state_p=state_p->state_tuple_arguments;
	lazy_type_arg=lazy_type_node->type_node_arguments;
	strict_type_arg=strict_type_node->type_node_arguments;

	while (lazy_type_arg!=NULL){
		TypeNode lazy_type_arg_node,strict_type_arg_node;
		int strict;

		lazy_type_arg_node=lazy_type_arg->type_arg_node;
		strict_type_arg_node=strict_type_arg->type_arg_node;
		
		strict = lazy_type_arg_node->type_node_annotation==StrictAnnot || !IsLazyState (*arg_state_p);
		
		if (strict != strict_type_arg_node->type_node_annotation==StrictAnnot)
			return 0;

		if (!lazy_type_arg_node->type_node_is_var && lazy_type_arg_node->type_node_symbol->symb_kind==tuple_type)
			if (arg_state_p->state_type==TupleState){
				if (!type_and_strictness_in_state_equals_type (lazy_type_arg_node,arg_state_p,strict_type_arg_node))
					return 0;
			} else {
				if (!equal_strictness_in_types (lazy_type_arg_node,strict_type_arg_node))
					return 0;				
			}

		++arg_state_p;		
		lazy_type_arg=lazy_type_arg->type_arg_next;
		strict_type_arg=strict_type_arg->type_arg_next;
	}
	
	return 1;
}

static void add_strictness_in_state_to_type (StateS *state_p,TypeNode type_node)
{
	StateS *arg_state_p;
	TypeArg *type_arg;

	if (type_node->type_node_is_var || type_node->type_node_symbol->symb_kind!=tuple_type)
		return;
	
	if (type_node->type_node_arity!=state_p->state_arity || type_node->type_node_symbol->symb_arity!=state_p->state_arity)
		return;
	
	arg_state_p=state_p->state_tuple_arguments;
	type_arg=type_node->type_node_arguments;

	while (type_arg!=NULL){
		TypeNode type_arg_node;

		type_arg_node=type_arg->type_arg_node;

		switch (arg_state_p->state_type){
			case SimpleState:
				if (IsLazyStateKind (arg_state_p->state_kind))
					break;
			case ArrayState:
			case RecordState:
				if (type_arg_node->type_node_annotation==NoAnnot)
					type_arg_node->type_node_annotation=StrictAnnot;
				break;
			case TupleState:
				if (type_arg_node->type_node_annotation==NoAnnot)
					type_arg_node->type_node_annotation=StrictAnnot;
				
				if (!type_arg_node->type_node_is_var && type_arg_node->type_node_symbol->symb_kind==tuple_type)
					add_strictness_in_state_to_type (arg_state_p,type_arg_node);
				break;
		}
				
		++arg_state_p;		
		type_arg=type_arg->type_arg_next;
	}
}

static TypeNode copy_type (TypeNode old_type)
{
	TypeNode new_type;
	
	new_type=CompAllocType (struct type_node);
	*new_type=*old_type;

#if 0
	ConvertAnnotationToStateForTypeNode (new_type);
#endif
	
	if (!old_type->type_node_is_var){
		TypeArgs old_arg,*next_p;
		
		next_p=&new_type->type_node_arguments;
		for_l (old_arg,old_type->type_node_arguments,type_arg_next){
			TypeArgs new_arg;
			
			new_arg=CompAllocType (TypeArg);
			new_arg->type_arg_node=copy_type (old_arg->type_arg_node);
			*next_p=new_arg;
			next_p=&new_arg->type_arg_next;
		}
		
		*next_p=NULL;
	}
	
	return new_type;
}

static TypeAlts copy_rule_type (TypeAlts old_rule_alt)
{
	TypeAlts new_rule_alt;
	
	new_rule_alt=CompAllocType (TypeAlt);
	*new_rule_alt=*old_rule_alt;
	
	new_rule_alt->type_alt_lhs = copy_type (old_rule_alt->type_alt_lhs);
	new_rule_alt->type_alt_rhs = copy_type (old_rule_alt->type_alt_rhs);
	
	return new_rule_alt;
}

SymbolP copy_imp_rule_and_type (SymbDef old_sdef)
{
	SymbDef new_sdef;
	ImpRuleP new_rule,old_rule;
	Symbol new_symbol;

	new_sdef = CompAllocType (SymbDefS);

	new_symbol=NewSymbol (definition);
	new_symbol->symb_def=new_sdef;
		
	*new_sdef = *old_sdef;
	new_sdef->sdef_mark &= ~(SDEF_USED_STRICTLY_MASK | SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK | 
							 SDEF_NEXT_IMP_RULE_VERSION_MASK | SDEF_HAS_IMP_RULE_VERSIONS_MASK);
	new_sdef->sdef_exported=False;
	new_sdef->sdef_sa_fun=NULL;

	new_rule = CompAllocType (ImpRuleS);
	new_sdef->sdef_rule=new_rule;

	old_rule=old_sdef->sdef_rule;

	new_rule->rule_type=copy_rule_type (old_rule->rule_type);
	new_rule->rule_type->type_alt_lhs->type_node_symbol=new_symbol;
			
	return new_symbol;	
}

static Node copy_node (Node old, Bool lhs);

static NodeId copy_node_id (NodeId old_node_id)
{
	NodeId new_node_id;
	
	new_node_id = CompAllocType (NodeIdS);
	
	*new_node_id = *old_node_id;

	new_node_id->nid_mark &= ~SHARED_NODES_COLLECTED_MASK;

#if OPTIMIZE_LAZY_TUPLE_RECURSION
	new_node_id->nid_mark2 &= ~NID_CALL_VIA_LAZY_SELECTIONS_ONLY;
#endif
	new_node_id->nid_ref_count_copy_=new_node_id->nid_refcount;
	new_node_id->nid_exp_=NULL;

	old_node_id->nid_forward_node_id_ = new_node_id;
		
	return new_node_id;
}

static NodeDefP copy_lhs_node_ids_of_node_defs (NodeDefs old_node_defs)
{
	NodeDefP old_def_p,first_p,*next_h;
	
	next_h=&first_p;
	
	for_l (old_def_p,old_node_defs,def_next){
		NodeDefs new_node_def;
	
		new_node_def = CompAllocType (NodeDefS);

		new_node_def->def_id = copy_node_id (old_def_p->def_id);
		new_node_def->def_node = old_def_p->def_node;
		new_node_def->def_mark = 0;

		*next_h=new_node_def;
		next_h=&new_node_def->def_next;
	}

	*next_h=NULL;
	
	return first_p;
}

static NodeDefP copy_rhs_node_ids_of_node_defs (NodeDefs old_node_defs)
{
	NodeDefP old_def_p,first_p,*next_h;
	
	next_h=&first_p;
	
	for_l (old_def_p,old_node_defs,def_next){
		NodeDefs new_node_def;
		NodeId new_node_id;

		if (old_def_p->def_node!=NULL)
			new_node_id = copy_node_id (old_def_p->def_id);
		else
			new_node_id = old_def_p->def_id->nid_forward_node_id;

		new_node_def = CompAllocType (NodeDefS);
		
		new_node_def->def_id=new_node_id;
		new_node_id->nid_node_def_=new_node_def;
		new_node_def->def_mark=0;
		
		new_node_def->def_node = old_def_p->def_node;
		*next_h=new_node_def;
		next_h=&new_node_def->def_next;
	}

	*next_h=NULL;
	
	return first_p;
}

static void copy_nodes_of_node_defs (NodeDefs node_defs,Bool lhs)
{
	NodeDefS *node_def;
	
	for_l (node_def,node_defs,def_next){
		if (node_def->def_node!=NULL)
			node_def->def_node = copy_node (node_def->def_node,lhs);
		node_def->def_id->nid_node=node_def->def_node;
	}
}

static StrictNodeIdP copy_strict_node_ids (StrictNodeIdP old_strict_node_ids)
{
	StrictNodeIdP old_p,first_p,*next_h;
	
	next_h=&first_p;
	
	for_l (old_p,old_strict_node_ids,snid_next){
		StrictNodeIdP new;
		
		new = CompAllocType (StrictNodeIdS);

#ifdef OBSERVE_ARRAY_SELECTS_IN_PATTERN
		new->snid_array_select_in_pattern=old_p->snid_array_select_in_pattern;
#endif
		new->snid_node_id = old_p->snid_node_id->nid_forward_node_id;

		*next_h = new;
		next_h = &new->snid_next;
	}
	*next_h = NULL;
	
	return first_p;
}

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
static Node copy_root_node (Node old)
{
	if (old->node_kind==IfNode){
		struct if_node_contents *new_then_else_info,*old_then_else_info;
		ArgS *previous_arg,*new_arg,*old_arg;
		Node new;
		
		new = CompAllocType (NodeS);

		*new = *old;
		
		DetermineNodeState (new);

		new_then_else_info = CompAllocType (IfNodeContentsS);

		old_then_else_info = old->node_contents.contents_if;
		new->node_contents.contents_if = new_then_else_info;

		old_arg=old->node_arguments;

		new_arg = CompAllocType (ArgS);
		new_arg->arg_node = copy_root_node (old_arg->arg_node);
		new_arg->arg_state = LazyState;
		new->node_arguments = new_arg;
		previous_arg = new_arg;

		old_arg=old_arg->arg_next;

		new_then_else_info->if_then_node_defs=copy_rhs_node_ids_of_node_defs (old_then_else_info->if_then_node_defs);
			
		new_arg = CompAllocType (ArgS);
		new_arg->arg_state = LazyState;
		new_arg->arg_node = copy_root_node (old_arg->arg_node);
		previous_arg->arg_next = new_arg;
		previous_arg = new_arg;

		copy_nodes_of_node_defs (new_then_else_info->if_then_node_defs,False);
		new_then_else_info->if_then_strict_node_ids=copy_strict_node_ids (old_then_else_info->if_then_strict_node_ids);

		new_then_else_info->if_else_node_defs=copy_rhs_node_ids_of_node_defs (old_then_else_info->if_else_node_defs);

		old_arg=old_arg->arg_next;
			
		new_arg = CompAllocType (ArgS);
		new_arg->arg_state = LazyState;
		new_arg->arg_node = copy_root_node (old_arg->arg_node);
		previous_arg->arg_next = new_arg;
		new_arg->arg_next=NULL;

		copy_nodes_of_node_defs (new_then_else_info->if_else_node_defs,False);
		new_then_else_info->if_else_strict_node_ids=copy_strict_node_ids (old_then_else_info->if_else_strict_node_ids);
		
		new_then_else_info->if_local_scope=old_then_else_info->if_local_scope;

		return new;
	} else if (old->node_kind==SwitchNode){
		Args *next,old_arg;
		NodeP new;
		
		new = CompAllocType (NodeS);
		*new = *old;

		{
			NodeIdP old_node_id,new_node_id;
			
			old_node_id = old->node_node_id;
			
			new_node_id=old_node_id->nid_forward_node_id;
			
			if (new_node_id==NULL)
				error_in_function ("copy_root_node");
			
			new->node_node_id=new_node_id;
		}
		
		next = &new->node_arguments;
		for_l (old_arg,old->node_arguments,arg_next){
			NodeP case_node_p,new_case_node_p;
			ArgP new_arg,case_node_arg_p,new_case_node_arg_p;
			
			new_arg = CompAllocType (ArgS);
			new_arg->arg_state = LazyState;
			
			*next = new_arg;
			next = &new_arg->arg_next;

			case_node_p=old_arg->arg_node;

			new_case_node_p = CompAllocType (NodeS);
			*new_case_node_p = *case_node_p;

			new_arg->arg_node = new_case_node_p;
			
			new_case_node_arg_p=CompAllocType (ArgS);
			new_case_node_arg_p->arg_state=LazyState;
			
			new_case_node_p->node_arguments=new_case_node_arg_p;
			new_case_node_arg_p->arg_next=NULL;

			case_node_arg_p=case_node_p->node_arguments;

			if (case_node_p->node_kind==CaseNode){
				new_case_node_p->node_su.su_u.u_case=CompAllocType (CaseNodeContentsS);

				new_case_node_p->node_node_defs = copy_rhs_node_ids_of_node_defs (case_node_p->node_node_defs);
				
				if (case_node_arg_p->arg_node->node_kind==PushNode){
					ArgP push_node_arg_1,new_push_node_arg_1,new_push_node_arg_2;
					NodeP push_node_arg_2_node,push_node,new_push_node;
					NodeIdListElementP node_id_list,*new_node_id_list_p;
					
					push_node=case_node_arg_p->arg_node;

					new_push_node=CompAllocType (NodeS);
					*new_push_node=*push_node;
					
					new_case_node_arg_p->arg_node=new_push_node;
					push_node_arg_1=push_node->node_arguments;

					new_node_id_list_p=&new_push_node->node_node_ids;
					
					if (push_node_arg_1->arg_node->node_node_id->nid_node!=NULL){
						/* unboxable lhs tuple or record */
						for_l (node_id_list,push_node->node_node_ids,nidl_next){
							NodeIdListElementP new_node_id_list;
							
							new_node_id_list=CompAllocType (NodeIdListElementS);
							
							new_node_id_list->nidl_node_id=node_id_list->nidl_node_id->nid_forward_node_id;
									
							*new_node_id_list_p=new_node_id_list;
							new_node_id_list_p=&new_node_id_list->nidl_next;
						}

					} else {
						for_l (node_id_list,push_node->node_node_ids,nidl_next){
							NodeIdListElementP new_node_id_list;
							
							new_node_id_list=CompAllocType (NodeIdListElementS);
							
							new_node_id_list->nidl_node_id=copy_node_id (node_id_list->nidl_node_id);
																
							*new_node_id_list_p=new_node_id_list;
							new_node_id_list_p=&new_node_id_list->nidl_next;
						}
					}
					
					*new_node_id_list_p=NULL;

					push_node_arg_2_node=push_node_arg_1->arg_next->arg_node;
					
					new_push_node_arg_1=CompAllocType (ArgS);
					new_push_node_arg_1->arg_state=LazyState;
					
					new_push_node_arg_2=CompAllocType (ArgS);
					new_push_node_arg_2->arg_state=LazyState;
					
					new_push_node->node_arguments=new_push_node_arg_1;
					new_push_node_arg_1->arg_next=new_push_node_arg_2;
					new_push_node_arg_2->arg_next=NULL;
					
					copy_nodes_of_node_defs (new_case_node_p->node_node_defs,False);
					new_push_node_arg_1->arg_node = copy_node (push_node_arg_1->arg_node,False);
					new_push_node_arg_2->arg_node = copy_root_node (push_node_arg_2_node);
					
#if STRICT_LISTS
					if (new_push_node->node_push_symbol->symb_kind==cons_symb && (new_push_node->node_push_symbol->symb_head_strictness & 1))
						new_push_node->node_decons_node=copy_node (new_push_node->node_decons_node,False);
#endif
				} else {
					copy_nodes_of_node_defs (new_case_node_p->node_node_defs,False);
					new_case_node_arg_p->arg_node = copy_root_node (case_node_arg_p->arg_node);
				}
			} else if (case_node_p->node_kind==DefaultNode){
				new_case_node_p->node_su.su_u.u_case=CompAllocType (CaseNodeContentsS);

				new_case_node_p->node_node_defs = copy_rhs_node_ids_of_node_defs (case_node_p->node_node_defs);
				copy_nodes_of_node_defs (new_case_node_p->node_node_defs,False);
				new_case_node_arg_p->arg_node = copy_root_node (case_node_arg_p->arg_node);

			} else if (case_node_p->node_kind==OverloadedCaseNode){
				NodeP new_overloaded_case_node_p;
				ArgP new_overloaded_case_node_arg2_p;

				new_overloaded_case_node_arg2_p=CompAllocType (ArgS);
				new_overloaded_case_node_arg2_p->arg_state=LazyState;

				new_overloaded_case_node_arg2_p->arg_next=NULL;
				new_case_node_arg_p->arg_next=new_overloaded_case_node_arg2_p;

				new_case_node_arg_p->arg_node=copy_node (case_node_arg_p->arg_node,False);
				new_overloaded_case_node_arg2_p->arg_node=copy_node (case_node_arg_p->arg_next->arg_node,False);

				case_node_p=case_node_p->node_node;
				new_overloaded_case_node_p=new_case_node_p;

				new_case_node_p = CompAllocType (NodeS);
				*new_case_node_p = *case_node_p;

				new_overloaded_case_node_p->node_node = new_case_node_p;
				
				new_case_node_arg_p=CompAllocType (ArgS);
				new_case_node_arg_p->arg_state=LazyState;
				
				new_case_node_p->node_arguments=new_case_node_arg_p;
				new_case_node_arg_p->arg_next=NULL;

				case_node_arg_p=case_node_p->node_arguments;

				new_case_node_p->node_su.su_u.u_case=CompAllocType (CaseNodeContentsS);

				new_case_node_p->node_node_defs = copy_rhs_node_ids_of_node_defs (case_node_p->node_node_defs);

				copy_nodes_of_node_defs (new_case_node_p->node_node_defs,False);
				new_case_node_arg_p->arg_node = copy_root_node (case_node_arg_p->arg_node);


			} else
				error_in_function ("copy_root_node");

			{
				NodeIdRefCountListP node_id_ref_count_elem_p,new_node_id_ref_count_elem_p,*node_id_ref_count_elem_h;

				node_id_ref_count_elem_h=&new_case_node_p->node_node_id_ref_counts;
				
				for_l (node_id_ref_count_elem_p,case_node_p->node_node_id_ref_counts,nrcl_next){
					new_node_id_ref_count_elem_p=CompAllocType (NodeIdRefCountListS);
					
					*node_id_ref_count_elem_h=new_node_id_ref_count_elem_p;
					new_node_id_ref_count_elem_p->nrcl_ref_count = node_id_ref_count_elem_p->nrcl_ref_count;
					new_node_id_ref_count_elem_p->nrcl_node_id = node_id_ref_count_elem_p->nrcl_node_id->nid_forward_node_id;

					node_id_ref_count_elem_h=&new_node_id_ref_count_elem_p->nrcl_next;
				}
				*node_id_ref_count_elem_h=NULL;
			}
			{				
				StrictNodeIdP strict_node_id_p,new_strict_node_id,*strict_node_id_h;

				strict_node_id_h=&new_case_node_p->node_strict_node_ids;

				for_l (strict_node_id_p,case_node_p->node_strict_node_ids,snid_next){
					new_strict_node_id=CompAllocType (StrictNodeIdS);
					
					*strict_node_id_h=new_strict_node_id;
					new_strict_node_id->snid_node_id = strict_node_id_p->snid_node_id->nid_forward_node_id;
					
					strict_node_id_h=&new_strict_node_id->snid_next;
				}
				
				*strict_node_id_h=NULL;
			}
		}
		
		*next = NULL;

		return new;
	} else if (old->node_kind==GuardNode){
		NodeP new;
		ArgP arg_1,arg_2;
		
		new = CompAllocType (NodeS);
		*new = *old;

		arg_1 = CompAllocType (ArgS);
		arg_1->arg_state = LazyState;
			
		arg_2 = CompAllocType (ArgS);
		arg_2->arg_state = LazyState;
		
		new->node_arguments=arg_1;
		arg_1->arg_next=arg_2;
		arg_2->arg_next=NULL;
		
		arg_1->arg_node = copy_root_node (old->node_arguments->arg_node);

		new->node_node_defs = copy_rhs_node_ids_of_node_defs (old->node_node_defs);

		arg_2->arg_node = copy_root_node (old->node_arguments->arg_next->arg_node);

		copy_nodes_of_node_defs (new->node_node_defs,False);

		{	
			StrictNodeIdP strict_node_id_p,new_strict_node_id,*strict_node_id_h;

			strict_node_id_h=&new->node_guard_strict_node_ids;

			for_l (strict_node_id_p,old->node_guard_strict_node_ids,snid_next){
				new_strict_node_id=CompAllocType (StrictNodeIdS);
				
				*strict_node_id_h=new_strict_node_id;
				new_strict_node_id->snid_node_id = strict_node_id_p->snid_node_id->nid_forward_node_id;
				
				strict_node_id_h=&new_strict_node_id->snid_next;
			}
			
			*strict_node_id_h=NULL;
		}
				
		return new;
	} else
		return copy_node (old,False);
}
#endif

static Node copy_node (Node old,Bool lhs)
{
	Node new;
	
	new = CompAllocType (NodeS);

	*new = *old;
	
	if (old->node_kind==NodeIdNode){
		NodeId old_nid,new_node_id;
		
		old_nid = old->node_node_id;
		
		if (lhs && old_nid->nid_node==NULL)
			new_node_id=copy_node_id (old_nid);
		else
			new_node_id=old_nid->nid_forward_node_id;
		
		if (new_node_id==NULL)
			error_in_function ("copy_node");
		
		new->node_node_id=new_node_id;
		
		return new;
	}
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	else if (old->node_kind==IfNode){
		struct if_node_contents *new_then_else_info,*old_then_else_info;
		ArgS *previous_arg,*new_arg,*old_arg;

		DetermineNodeState (new);

		new_then_else_info = CompAllocType (IfNodeContentsS);

		old_then_else_info = old->node_contents.contents_if;
		new->node_contents.contents_if = new_then_else_info;

		new_then_else_info->if_then_rules = NULL;
		new_then_else_info->if_else_rules = NULL;

		old_arg=old->node_arguments;

		new_arg = CompAllocType (ArgS);
		new_arg->arg_node = copy_node (old_arg->arg_node,lhs);
		new_arg->arg_state = LazyState;
		new->node_arguments = new_arg;
		previous_arg = new_arg;

		old_arg=old_arg->arg_next;

		new_then_else_info->if_then_node_defs=copy_rhs_node_ids_of_node_defs (old_then_else_info->if_then_node_defs);
		
		new_arg = CompAllocType (ArgS);
		new_arg->arg_state = LazyState;
		new_arg->arg_node = copy_node (old_arg->arg_node,lhs);
		previous_arg->arg_next = new_arg;
		previous_arg = new_arg;

		copy_nodes_of_node_defs (new_then_else_info->if_then_node_defs,False);
		new_then_else_info->if_then_strict_node_ids=copy_strict_node_ids (old_then_else_info->if_then_strict_node_ids);

		new_then_else_info->if_else_node_defs=copy_rhs_node_ids_of_node_defs (old_then_else_info->if_else_node_defs);

		old_arg=old_arg->arg_next;
			
		new_arg = CompAllocType (ArgS);
		new_arg->arg_state = LazyState;
		new_arg->arg_node = copy_node (old_arg->arg_node,lhs);
		previous_arg->arg_next = new_arg;
		new_arg->arg_next=NULL;

		copy_nodes_of_node_defs (new_then_else_info->if_else_node_defs,False);
		new_then_else_info->if_else_strict_node_ids=copy_strict_node_ids (old_then_else_info->if_else_strict_node_ids);
		
		new_then_else_info->if_local_scope=old_then_else_info->if_local_scope;

		return new;
	}
#endif
	else if (!lhs)
		DetermineNodeState (new);

	if (old->node_arguments!=NULL){
		Args *next,old_arg;
	
		next = &new->node_arguments;
		for_l (old_arg,old->node_arguments,arg_next){
			Args new_arg;
			
			new_arg = CompAllocType (ArgS);
			new_arg->arg_node = copy_node (old_arg->arg_node,lhs);
			new_arg->arg_state = LazyState;
			
			*next = new_arg;
			next = &new_arg->arg_next;
		}
		*next = NULL;
	}
	
	return new;
}

static void copy_alts (RuleAltP old_alts,RuleAlts *next_p,Symbol new_symbol)
{
	RuleAltP old;
	
	for_l (old,old_alts,alt_next){
		RuleAltP new;
		
		new = CompAllocType (RuleAltS);
		
		new->alt_lhs_defs=copy_lhs_node_ids_of_node_defs (old->alt_lhs_defs);
		new->alt_lhs_root = copy_node (old->alt_lhs_root, True);
		new->alt_lhs_root->node_symbol=new_symbol;
		copy_nodes_of_node_defs (new->alt_lhs_defs,True);
		
	 	new->alt_rhs_defs=copy_rhs_node_ids_of_node_defs (old->alt_rhs_defs);
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		new->alt_rhs_root = copy_root_node (old->alt_rhs_root);
#else
		new->alt_rhs_root = copy_node (old->alt_rhs_root, False);
#endif
		copy_nodes_of_node_defs (new->alt_rhs_defs,False);
		new->alt_strict_node_ids=copy_strict_node_ids (old->alt_strict_node_ids);
		
		new->alt_line = old->alt_line;
		new->alt_kind = old->alt_kind;

		*next_p = new;
		next_p = &new->alt_next;			
	}
	*next_p = NULL;
}

void copy_imp_rule_nodes (ImpRuleP old_rule_p,ImpRuleP new_rule_p)
{
	copy_alts (old_rule_p->rule_alts,&new_rule_p->rule_alts,new_rule_p->rule_type->type_alt_lhs->type_node_symbol);
	new_rule_p->rule_root = new_rule_p->rule_alts->alt_lhs_root;
	new_rule_p->rule_mark = old_rule_p->rule_mark & RULE_CAF_MASK;
}

static ImpRules new_strict_result_rules;

int optimise_tuple_result_function (Node node,StateS demanded_state)
{
	Symbol symbol;
	TypeNode result_type;
	Symbol new_function_symbol;
	SymbDef sdef,new_sdef,last_version;
	ImpRuleP new_rule_p;

	symbol=node->node_symbol;
	sdef=symbol->symb_def;

	if (sdef->sdef_kind!=IMPRULE || node->node_arity!=sdef->sdef_arity)
		return 0;
	
	result_type=sdef->sdef_rule->rule_type->type_alt_rhs;
	if (! tuple_state_has_more_strictness (&demanded_state,result_type,&sdef->sdef_rule->rule_state_p[-1]))
		return 0;

	if (sdef->sdef_mark & SDEF_HAS_IMP_RULE_VERSIONS_MASK){
		while (sdef->sdef_mark & SDEF_NEXT_IMP_RULE_VERSION_MASK)
			sdef=sdef->sdef_next_version;
		last_version=sdef;
		sdef=sdef->sdef_next_version;
	} else
		last_version=sdef;
	
	if (sdef->sdef_mark & SDEF_HAS_IMP_RULE_VERSIONS_MASK){
		SymbDef version;
		int n_versions;

		version=sdef;
		n_versions=0;

		do {
			version=version->sdef_next_version;
			++n_versions;

			if (type_and_strictness_in_state_equals_type (result_type,&demanded_state,version->sdef_rule->rule_type->type_alt_rhs)){
				if (symbol!=version->sdef_rule->rule_type->type_alt_lhs->type_node_symbol){
					node->node_symbol=version->sdef_rule->rule_type->type_alt_lhs->type_node_symbol;
					function_changed=1;

					return 1;
				}
				return 0;
			}
		} while (version->sdef_mark & SDEF_NEXT_IMP_RULE_VERSION_MASK);

		if (n_versions>=MAX_N_VERSIONS)
			return 0;
	}
	
	new_function_symbol = copy_imp_rule_and_type (sdef);
	new_sdef=new_function_symbol->symb_def;
	new_rule_p=new_sdef->sdef_rule;
	
	new_rule_p->rule_next_changed_function=sdef->sdef_rule;

	new_rule_p->rule_next=new_strict_result_rules;
	new_strict_result_rules=new_rule_p;
	
	add_strictness_in_state_to_type (&demanded_state,new_rule_p->rule_type->type_alt_rhs);

	new_rule_p->rule_state_p=NULL;

	node->node_symbol=new_function_symbol;
	function_changed=1;
	
	last_version->sdef_mark |= SDEF_NEXT_IMP_RULE_VERSION_MASK | SDEF_HAS_IMP_RULE_VERSIONS_MASK;
	last_version->sdef_next_version=new_sdef;
	new_sdef->sdef_next_version=sdef;
	new_sdef->sdef_mark |= SDEF_HAS_IMP_RULE_VERSIONS_MASK;
	
	return 1;
}

#if 0
#include "dbprint.h"
#endif

void generate_states (ImpRuleS *rules,int do_strictness_analysis)
{
	ImpRuleS *rule,*changed_functions,**last_changed_function_l,**rule_p;
	
	new_strict_result_rules=NULL;
	changed_functions=NULL;
	last_changed_function_l=&changed_functions;
	
	for (rule_p=&rules; (rule=*rule_p)!=NULL; rule_p=&rule->rule_next){
		function_changed=0;

		GenerateStatesForRule (rule);
		
		if (function_changed){
			*last_changed_function_l=rule;
			last_changed_function_l=&rule->rule_next_changed_function;
			*last_changed_function_l=NULL;
		}
	}
	
	do {
		ImpRuleS *rule;
		
		while (new_strict_result_rules!=NULL){
			for_l (rule,new_strict_result_rules,rule_next){
				copy_imp_rule_nodes (rule->rule_next_changed_function,rule);
#if 0
				PrintRuleAlt (rule->rule_alts,4,StdOut);
#endif
			}
			
			if (do_strictness_analysis)
				if (StrictnessAnalysisConvertRules (new_strict_result_rules)){
					for_l (rule,new_strict_result_rules,rule_next)
						StrictnessAnalysisForRule (rule->rule_root->node_symbol->symb_def);

					free_unused_sa_blocks();
				} else
					do_strictness_analysis=0;

			for_l (rule,new_strict_result_rules,rule_next)
				ExamineTypesAndLhsOfSymbolDefinition (rule->rule_root->node_symbol->symb_def);
			
			rule=new_strict_result_rules;
			new_strict_result_rules=NULL;

			*rule_p=rule;

			while (rule!=NULL){
				SymbDef sdef;
								
				sdef=rule->rule_root->node_symbol->symb_def;
						
				function_changed=0;

				GenerateStatesForRule (rule);
				
				if (function_changed){
					*last_changed_function_l=rule;
					last_changed_function_l=&rule->rule_next_changed_function;
					*last_changed_function_l=NULL;
				}

				rule_p=&rule->rule_next;
				rule=*rule_p;
			}	
		}
		
		while (new_strict_result_rules==NULL && changed_functions!=NULL){
			SymbDef sdef;

			rule=changed_functions;
										
			sdef=rule->rule_root->node_symbol->symb_def;
						
			reset_states_and_ref_count_copies (rule);

			ExamineTypesAndLhsOfImpRuleSymbolDefinitionAgain (sdef);
			
			function_changed=0;
			
			GenerateStatesForRule (rule);
			
			if (!function_changed)
				changed_functions=changed_functions->rule_next_changed_function;
		}
	} while (changed_functions!=NULL || new_strict_result_rules!=NULL);
}

static ImpRules new_rules;
static int next_function_n;

static Symbol new_rule_symbol (char *function_name)
{
	SymbDef function_sdef;
	Symbol function_symbol;
	Ident function_ident;

	function_ident=PutStringInHashTable (function_name,SymbolIdTable);
	function_sdef=MakeNewSymbolDefinition (CurrentModule,function_ident,0,IMPRULE);

	function_sdef->sdef_ancestor = ~next_def_number;
	function_sdef->sdef_number=next_def_number++;
	function_sdef->sdef_isused=True;

	function_sdef->sdef_mark |= SDEF_OPTIMISED_FUNCTION_MASK;
	function_sdef->sdef_returnsnode=True;
	function_sdef->sdef_calledwithrootnode=True;
		
	function_symbol=NewSymbol (definition);
	function_symbol->symb_def=function_sdef;
	
	return function_symbol;
}

#ifdef MOVE_APPLY_NODES_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
static StateS apply_symb_function_states[3];
static StateP apply_symb_function_state_p=NULL;

static void init_apply_symb_function_state_p()
{
	SetUnaryState (&apply_symb_function_states[0],StrictRedirection,UnknownObj);
	SetUnaryState (&apply_symb_function_states[1],StrictOnA,UnknownObj);
	SetUnaryState (&apply_symb_function_states[2],OnA,UnknownObj);
	apply_symb_function_state_p=&apply_symb_function_states[1];
}
#endif

#define cTypeDelimiter	';'		/* also in checksupport.h */
#ifndef CLEAN2
#define _ANALYSE_IDENT_			/* also in checksupport.c */
#endif

static int compute_length_before_type_delimiter (char *fname)
{
	char *p;
	unsigned int c;	

	p=fname;
	
#ifdef _ANALYSE_IDENT_
	--p;
	do {
		c=*++p;
	} while (c!=cTypeDelimiter && c!='\0');

	if (c == cTypeDelimiter && *(p+1) != '\0')
	{
		p++;

		if (isdigit (*p))
		{	
			for (p = p+1; *p != cTypeDelimiter && *p != '\0'; p++)
				 ;
		}
	}
#else /* ifndef _ANALYSE_IDENT_ */
	--p;
	do {
		c=*++p;
	} while (c!='\0');

#endif /* _ANALYSE_IDENT_ */

	return 	p-fname;
}

static char *append_n_chars (char *dest,const char *src,int length)
{
	while (length>0){
		*dest++ = *src++;
		--length;
	}

	return dest;
}

#define allocate_function_state(arity) (((StateP)(CompAlloc (sizeof(StateS)*((arity)+1))))+1)

#ifdef THUNK_LIFT_U_RECORD_SELECTORS
static StateP selector_l_or_n_state_p (StateP tuple_state_p,StateP tuple_arg_states,StateP selector_arg_state_p)
{
	tuple_arg_states[0]=*selector_arg_state_p;
	tuple_arg_states[1]=StrictState;

	tuple_state_p->state_arity = 2;
	tuple_state_p->state_tuple_arguments = tuple_arg_states;
	tuple_state_p->state_type = TupleState;
	tuple_state_p->state_mark = 0;
	
	return tuple_state_p;
}
#endif

#define MAX_N_FUNCTION_ARGUMENTS 32

static int add_n_new_arguments_for_local_function (ArgP arg_p,int n_arguments)
{
	for ( ; arg_p!=NULL; arg_p=arg_p->arg_next){
		NodeP arg_node_p;

		arg_node_p=arg_p->arg_node;
		
		if (arg_node_p->node_kind==NodeIdNode){
			if (arg_node_p->node_node_id->nid_mark & NID_LIFTED_BY_OPTIMISE){
				continue;
			} else {
				arg_node_p->node_node_id->nid_mark |= NID_LIFTED_BY_OPTIMISE;
				arg_node_p->node_node_id->nid_forward_node_id=NULL;
							
				n_arguments=add_n_new_arguments_for_local_function (arg_p->arg_next,n_arguments+1);
				
				if (n_arguments>MAX_N_FUNCTION_ARGUMENTS)
					arg_node_p->node_node_id->nid_mark &= ~NID_LIFTED_BY_OPTIMISE;
				
				return n_arguments;
			}	
		} else if (arg_node_p->node_kind==NormalNode){
			switch (arg_node_p->node_symbol->symb_kind){
				case int_denot:
				case bool_denot:
				case char_denot:
				case string_denot:
				case real_denot:
					continue;
			}		
		}
		
		++n_arguments;
	}
	
	return n_arguments;
}

static struct arg *remove_apply_nodes (struct node *node,int n_apply_args)
{
	struct arg *old_arg_p,*new_arg_list,*arg_p;
	int arg_n;

	new_arg_list=NULL;
	arg_p=node->node_arguments;
	old_arg_p=arg_p;

	for (arg_n=n_apply_args; arg_n>1; --arg_n){
		struct arg *arg_next_p;

		arg_next_p=arg_p->arg_next;
		arg_next_p->arg_next=new_arg_list;
		new_arg_list=arg_next_p;
		arg_p=arg_p->arg_node->node_arguments;
	}

	arg_p->arg_next->arg_next=new_arg_list;
	node->node_arguments=arg_p;

	return old_arg_p;
}

static void restore_removed_apply_nodes (struct node *node,struct arg *old_arg_p,int n_apply_args)
{
	struct arg *arg_p;
	int arg_n;
	
	node->node_arguments=old_arg_p;
	arg_p=old_arg_p;

	for (arg_n=n_apply_args; arg_n>1; --arg_n){
		arg_p->arg_next->arg_next=NULL;
		arg_p=arg_p->arg_node->node_arguments;
	}

	arg_p->arg_next->arg_next=NULL;
}

static struct arg *store_args_in_apply_nodes (int n_apply_args,struct arg *arg_p,struct node *apply_node)
{
	struct arg *remaining_args;

	if (n_apply_args==1){
		apply_node->node_arguments=arg_p;
		remaining_args=arg_p->arg_next->arg_next;
		arg_p->arg_next->arg_next=NULL;
		return remaining_args;
	} else {
		struct arg *first_arg_p,*next_arg_p;
		first_arg_p=apply_node->node_arguments;
		next_arg_p=store_args_in_apply_nodes (n_apply_args-1,arg_p,first_arg_p->arg_node);
		remaining_args=next_arg_p->arg_next;
		next_arg_p->arg_next=NULL;
		first_arg_p->arg_next=next_arg_p;
		return remaining_args;
	}
}

static char *create_arguments_for_local_function (NodeP node_p,ArgS ***arg_h,ArgS ***lhs_arg_h,ArgS **rhs_arg_p,StateP arg_state_p,int *arity_p,char *function_name_p,char *end_function_name,int *n_arguments_p)
{
	NodeIdP arg_node_id;
	StateP call_state_p;
	ArgP arg;
	
	if (function_name_p!=NULL && node_p->node_symbol->symb_kind==definition){
		int length_before_type_delimiter;
		char *f_name;
		
		f_name=node_p->node_symbol->symb_def->sdef_ident->ident_name;
		length_before_type_delimiter=compute_length_before_type_delimiter (f_name);
		
		if (function_name_p+2+length_before_type_delimiter < end_function_name){
			*function_name_p++='.';
			function_name_p=append_n_chars (function_name_p,f_name,length_before_type_delimiter);
		} else
			end_function_name=function_name_p;
	}

	for_l (arg,node_p->node_arguments,arg_next){
		ArgP lhs_arg,rhs_arg;
		NodeP arg_node;

		arg_node=arg->arg_node;
		
		if (arg_node->node_kind==NormalNode)
			switch (arg_node->node_symbol->symb_kind){
				case int_denot:
				case bool_denot:
				case char_denot:
				case string_denot:
				case real_denot:
				{
					NodeP function_node;
					ArgP new_arg;
					
					function_node=NewNode (arg_node->node_symbol,NULL,arg_node->node_arity);
					function_node->node_state=LazyState;
					function_node->node_number=0;
					
					new_arg=NewArgument (function_node);
					new_arg->arg_state=LazyState;
					*rhs_arg_p=new_arg;
					rhs_arg_p=&new_arg->arg_next;

					++arg_state_p;
					continue;
				}
				case definition:
				{
					if ((arg_state_p->state_type==SimpleState && arg_state_p->state_kind==OnB)
#ifdef MOVE_TUPLE_RECORD_AND_ARRAY_RESULT_FUNCTION_ARGUMENT_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
						|| arg_state_p->state_type==TupleState || arg_state_p->state_type==RecordState || arg_state_p->state_type==ArrayState
#endif
					){
						SymbDef sdef;
						
						sdef=arg_node->node_symbol->symb_def;
						
						if (arg_node->node_arity==sdef->sdef_arity){
							Bool is_rule;
							StateP function_state_p;

							switch (sdef->sdef_kind){
								case IMPRULE:
									is_rule=True;
									function_state_p=sdef->sdef_rule->rule_state_p;
									break;
								case DEFRULE:
								case SYSRULE:
									is_rule=True;
									function_state_p=sdef->sdef_rule_type->rule_type_state_p;
									break;
								case RECORDTYPE:
									if (sdef->sdef_strict_constructor){
										is_rule=True;
										function_state_p=sdef->sdef_record_state.state_record_arguments;
									} else
										is_rule=False;
									break;
								default:
									is_rule=False;
							}
						
							if (is_rule){
								Node function_node;
								ArgP new_arg;
								int new_n_arguments;
								
								new_n_arguments=add_n_new_arguments_for_local_function (arg_node->node_arguments,*n_arguments_p-1);
								
								if (new_n_arguments>MAX_N_FUNCTION_ARGUMENTS)
									break;
									
								*n_arguments_p=new_n_arguments;

								function_node=NewNode (arg_node->node_symbol,NULL,arg_node->node_arity);
								function_node->node_state=LazyState;
								function_node->node_number=0;
								
								new_arg=NewArgument (function_node);
								new_arg->arg_state=LazyState;
								*rhs_arg_p=new_arg;
								rhs_arg_p=&new_arg->arg_next;

								function_name_p = create_arguments_for_local_function (arg_node,arg_h,lhs_arg_h,&function_node->node_arguments,
																						 function_state_p,arity_p,function_name_p,end_function_name,n_arguments_p);
								
								++arg_state_p;
								continue;
							}
						}
					}
#ifdef THUNK_LIFT_0_CONSTRUCTORS
					else if (arg_node->node_arity==0 &&
						(arg_node->node_symbol->symb_def->sdef_kind==CONSTRUCTOR ||
						 (arg_node->node_symbol->symb_def->sdef_kind!=RECORDTYPE && arg_node->node_symbol->symb_def->sdef_arity>0))
					){
						NodeP function_node;
						ArgP new_arg;
						
						function_node=NewNode (arg_node->node_symbol,NULL,0);
						function_node->node_state=LazyState;
						function_node->node_number=0;
						
						new_arg=NewArgument (function_node);
						new_arg->arg_state=LazyState;
						*rhs_arg_p=new_arg;
						rhs_arg_p=&new_arg->arg_next;
						
						++arg_state_p;
						continue;
					}
#endif
					break;
				}
#ifdef UNTUPLE_STRICT_TUPLES
				case tuple_symb:
				{
					if (arg_state_p->state_type==TupleState){
						NodeP tuple_node;
						ArgP new_arg;
						int new_n_arguments;
						
						new_n_arguments=add_n_new_arguments_for_local_function (arg_node->node_arguments,*n_arguments_p-1);
								
						if (new_n_arguments>MAX_N_FUNCTION_ARGUMENTS)
							break;

						*n_arguments_p=new_n_arguments;
						
						tuple_node=NewNode (arg_node->node_symbol,NULL,arg_node->node_arity);
						tuple_node->node_state=LazyState;
						tuple_node->node_number=0;
						
						new_arg=NewArgument (tuple_node);
						new_arg->arg_state=LazyState;
						*rhs_arg_p=new_arg;
						rhs_arg_p=&new_arg->arg_next;
						
						function_name_p = create_arguments_for_local_function (arg_node,arg_h,lhs_arg_h,&tuple_node->node_arguments,
																				arg_state_p->state_tuple_arguments,arity_p,function_name_p,end_function_name,n_arguments_p);

						++arg_state_p;

						continue;						
					}
					break;
				}
#endif
#ifdef MOVE_APPLY_NODES_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
				case apply_symb:
					if (arg_node->node_arity==2){
						struct arg *arg_p;
						int n_apply_args;
				
						n_apply_args=1;
						arg_p=arg_node->node_arguments;
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
									struct symbol *new_symbol_p;

									new_symbol_p = CompAlloc (sizeof (struct symbol));
									*new_symbol_p = *arg_node->node_symbol;
									new_symbol_p->symb_instance_apply = 1;
									new_symbol_p->symb_next = (struct symbol*)selector_node_p->node_symbol->symb_def;
									arg_node->node_symbol = new_symbol_p;

									if (arg_state_p->state_type==SimpleState && (arg_state_p->state_kind==StrictOnA || arg_state_p->state_kind==StrictRedirection)
										&& OptimizeInstanceCalls)
									{
										struct state *member_states_of_field;
										struct arg *old_arg_p;
										Node function_node;
										ArgP new_arg;
										int new_n_arguments;
										
										member_states_of_field=field_sdef->sdef_member_states_of_field;

										old_arg_p = remove_apply_nodes (arg_node,n_apply_args);

										new_n_arguments=add_n_new_arguments_for_local_function (arg_node->node_arguments,*n_arguments_p-1);
										
										if (new_n_arguments>MAX_N_FUNCTION_ARGUMENTS){
											restore_removed_apply_nodes (arg_node,old_arg_p,n_apply_args);
											break;
										}
										
										*n_arguments_p=new_n_arguments;

										function_node=NewNode (arg_node->node_symbol,NULL,arg_node->node_arity);
										function_node->node_state=LazyState;
										function_node->node_number=0;
										
										new_arg=NewArgument (function_node);
										new_arg->arg_state=LazyState;
										*rhs_arg_p=new_arg;
										rhs_arg_p=&new_arg->arg_next;
										
										function_name_p = create_arguments_for_local_function (arg_node,arg_h,lhs_arg_h,&function_node->node_arguments,
																								 member_states_of_field,arity_p,function_name_p,end_function_name,n_arguments_p);
										
										++arg_state_p;

										arg_p=function_node->node_arguments;
										function_node->node_arguments=old_arg_p;
										store_args_in_apply_nodes (n_apply_args,arg_p,function_node);

										continue;
									}
								}
							}
						}
					}

					if (arg_state_p->state_type==SimpleState && (arg_state_p->state_kind==StrictOnA || arg_state_p->state_kind==StrictRedirection)){
						Node function_node;
						ArgP new_arg;
						int new_n_arguments;
						
						new_n_arguments=add_n_new_arguments_for_local_function (arg_node->node_arguments,*n_arguments_p-1);
								
						if (new_n_arguments>MAX_N_FUNCTION_ARGUMENTS)
							break;
						
						*n_arguments_p=new_n_arguments;

						function_node=NewNode (arg_node->node_symbol,NULL,arg_node->node_arity);
						function_node->node_state=LazyState;
						function_node->node_number=0;
						
						new_arg=NewArgument (function_node);
						new_arg->arg_state=LazyState;
						*rhs_arg_p=new_arg;
						rhs_arg_p=&new_arg->arg_next;
						
						if (apply_symb_function_state_p==NULL)
							init_apply_symb_function_state_p();

						function_name_p = create_arguments_for_local_function (arg_node,arg_h,lhs_arg_h,&function_node->node_arguments,
																				 apply_symb_function_state_p,arity_p,function_name_p,end_function_name,n_arguments_p);
						
						++arg_state_p;
						continue;
					}
					break;
#endif
#ifdef THUNK_LIFT_SELECTORS
				case select_symb:
				{
					NodeP tuple_node_p;
					NodeDefP node_def_p;
					
					if (arg_node->node_arguments->arg_node->node_kind==NodeIdNode &&
						!IsLazyState (*arg_state_p) &&
						arg_node->node_arguments->arg_node->node_node_id->nid_refcount>0 &&
						IsLazyState ((tuple_node_p=(node_def_p=arg_node->node_arguments->arg_node->node_node_id->nid_node_def)->def_node)->node_state) &&
						tuple_node_p->node_kind==NormalNode &&
						tuple_node_p->node_symbol->symb_kind==definition &&
						(tuple_node_p->node_symbol->symb_def->sdef_kind==IMPRULE ||
						 tuple_node_p->node_symbol->symb_def->sdef_kind==DEFRULE ||
						 tuple_node_p->node_symbol->symb_def->sdef_kind==SYSRULE) &&
						tuple_node_p->node_arity==tuple_node_p->node_symbol->symb_def->sdef_arity)
					{
						Node function_node;
						ArgP new_arg;
						int new_n_arguments;
						
						new_n_arguments=add_n_new_arguments_for_local_function (arg_node->node_arguments,*n_arguments_p-1);
						
						if (new_n_arguments>MAX_N_FUNCTION_ARGUMENTS)
							break;
						
						*n_arguments_p=new_n_arguments;

						function_node=NewNode (arg_node->node_symbol,NULL,arg_node->node_arity);
						function_node->node_state=LazyState;
						function_node->node_number=1;
						
						node_def_p->def_mark |= NODE_DEF_SELECT_AND_REMOVE_MASK;
						
						new_arg=NewArgument (function_node);
						new_arg->arg_state=LazyState;
						*rhs_arg_p=new_arg;
						rhs_arg_p=&new_arg->arg_next;

						function_name_p = create_arguments_for_local_function (arg_node,arg_h,lhs_arg_h,&function_node->node_arguments,
																				 &StrictState,arity_p,function_name_p,end_function_name,n_arguments_p);
						
						++arg_state_p;	
						continue;
					}
					break;
				}
#endif
			}
#ifdef THUNK_LIFT_U_RECORD_SELECTORS
		else if (arg_node->node_kind==SelectorNode && arg_node->node_arity>=SELECTOR_U && arg_state_p->state_type==TupleState){
			int new_n_arguments;

			new_n_arguments=add_n_new_arguments_for_local_function (arg_node->node_arguments,*n_arguments_p-1);
			
			if (new_n_arguments<=MAX_N_FUNCTION_ARGUMENTS){
				Node selector_node;
				ArgP new_arg;
				StateP selector_arg_state_p;
				StateS tuple_state,tuple_arg_states[2];

				*n_arguments_p=new_n_arguments;

				selector_node=NewSelectorNode (arg_node->node_symbol,NULL,arg_node->node_arity);
				selector_node->node_state=LazyState;
				selector_node->node_number=0;

				new_arg=NewArgument (selector_node);
				new_arg->arg_state=LazyState;
				*rhs_arg_p=new_arg;
				rhs_arg_p=&new_arg->arg_next;

				selector_arg_state_p=&arg_node->node_symbol->symb_def->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;

				if (arg_node->node_arity>=SELECTOR_L)
					selector_arg_state_p=selector_l_or_n_state_p (&tuple_state,tuple_arg_states,selector_arg_state_p);

				function_name_p = create_arguments_for_local_function (arg_node,arg_h,lhs_arg_h,&selector_node->node_arguments,
																		 selector_arg_state_p,arity_p,function_name_p,end_function_name,n_arguments_p);

				++arg_state_p;	
				continue;
			}
		}
#endif

		if (arg_node->node_kind==NodeIdNode && (arg_node->node_node_id->nid_mark & NID_LIFTED_BY_OPTIMISE) && arg_node->node_node_id->nid_forward_node_id!=NULL){
			arg_node_id=arg_node->node_node_id->nid_forward_node_id;
			--arg_node_id->nid_refcount;
			--arg_node_id->nid_ref_count_copy__;
		} else {
			arg_node_id=NewNodeId (NULL);
			arg_node_id->nid_refcount=-2;
			arg_node_id->nid_ref_count_copy__=-2;
			
			if (arg_node->node_kind==NodeIdNode){
				NodeIdP node_id;
				
				node_id=arg_node->node_node_id;

				node_id->nid_forward_node_id_=arg_node_id;
				arg_node_id->nid_forward_node_id_=node_id;
				node_id->nid_mark |= NID_LIFTED_BY_OPTIMISE;
				arg_node_id->nid_mark |= NID_LIFTED_BY_OPTIMISE;

				if (node_id->nid_refcount<0){
					call_state_p=node_id->nid_lhs_state_p;
				} else
					call_state_p=&node_id->nid_node->node_state;
			} else if (arg_node->node_kind==NormalNode){
#ifdef STRICT_STATE_FOR_LAZY_TUPLE_CONSTRUCTORS
				if (BETWEEN (tuple_symb,nil_symb,arg_node->node_symbol->symb_kind)
					&& arg_node->node_state.state_type==SimpleState && arg_node->node_state.state_kind==OnA)
				{
					call_state_p=&StrictState;
				} else
#endif
				if (arg_node->node_symbol->symb_kind==definition
					&& arg_node->node_state.state_type==SimpleState && arg_node->node_state.state_kind==OnA)
				{
					SymbDef sdef;

					sdef=arg_node->node_symbol->symb_def;

					if (sdef->sdef_kind!=RECORDTYPE && arg_node->node_arity<sdef->sdef_arity)
						call_state_p=&StrictState;
					else
						call_state_p=&arg_node->node_state;
				} else
					call_state_p=&arg_node->node_state;
			} else
				call_state_p=&arg_node->node_state;

			lhs_arg=NewArgument (NewNodeIdNode (arg_node_id));
			lhs_arg->arg_state=LazyState;
			if (!IsLazyState (*call_state_p)){
				if (call_state_p->state_type==SimpleState && call_state_p->state_kind==OnB)
					lhs_arg->arg_state=*call_state_p;
				else if (call_state_p->state_type==ArrayState)
					lhs_arg->arg_state=*call_state_p;
				else
					lhs_arg->arg_state.state_kind=StrictOnA;
			}
			
			arg_node_id->nid_lhs_state_p_=&lhs_arg->arg_state;

			++*arity_p;

			**lhs_arg_h=lhs_arg;
			*lhs_arg_h=&lhs_arg->arg_next;
			
			**arg_h=arg;
			*arg_h=&arg->arg_next;
		}

		++arg_state_p;

		rhs_arg=NewArgument (NewNodeIdNode (arg_node_id));
		rhs_arg->arg_state=LazyState;
		*rhs_arg_p=rhs_arg;
		rhs_arg_p=&rhs_arg->arg_next;
	}
	
	*rhs_arg_p=NULL;
		
	return function_name_p;
}

static struct node *create_new_local_function (Node node,StateP function_state_p)
{
	static char function_name[64];
	Symbol function_symbol;
	int n_arguments,function_arity;
	ImpRuleS *imp_rule;
	ArgS **lhs_arg_p,**arg_p;
	Node lhs_root,rhs_root;
	char *function_name_p,*end_function_name;

	n_arguments = add_n_new_arguments_for_local_function (node->node_arguments,0);
	
	if (n_arguments>MAX_N_FUNCTION_ARGUMENTS)
		return NULL;

	sprintf (function_name,"_f%d",next_function_n);
	++next_function_n;

	if (DoTimeProfiling || DoProfiling){
		char *f_name;
		int length_before_type_delimiter;
	
		end_function_name=function_name+sizeof (function_name);
		function_name_p=&function_name[strlen (function_name)];
		
		f_name=CurrentSymbol->symb_def->sdef_ident->ident_name;
		length_before_type_delimiter=compute_length_before_type_delimiter (f_name);
		
		if (function_name_p+2+length_before_type_delimiter < end_function_name){
			*function_name_p++='.';
			function_name_p=append_n_chars (function_name_p,f_name,length_before_type_delimiter);
		} else
			end_function_name=function_name_p;
	} else {
		function_name_p=NULL;
		end_function_name=NULL;
	}

	lhs_root=NewNode (NULL,NULL,0);
	lhs_root->node_state=StrictState;

#ifdef THUNK_LIFT_U_RECORD_SELECTORS
	rhs_root=NewNodeByKind (node->node_kind,node->node_symbol,NULL,node->node_arity);
#else
	rhs_root=NewNode (node->node_symbol,NULL,node->node_arity);
#endif
	rhs_root->node_state=LazyState;
	rhs_root->node_number=0;
	
	function_arity=0;

	lhs_arg_p=&lhs_root->node_arguments;
	arg_p=&node->node_arguments;

	function_name_p = create_arguments_for_local_function (node,&arg_p,&lhs_arg_p,&rhs_root->node_arguments,function_state_p,
															&function_arity,function_name_p,end_function_name,&n_arguments);
	
	if (function_name_p!=NULL)
		*function_name_p='\0';

	function_symbol=new_rule_symbol (function_name);
	lhs_root->node_symbol=function_symbol;
	
	*lhs_arg_p=NULL;
	*arg_p=NULL;

	{
		ArgP arg;
		
		for_l (arg,lhs_root->node_arguments,arg_next){
			NodeIdP lhs_node_id,rhs_node_id;
			
			lhs_node_id=arg->arg_node->node_node_id;
			if (lhs_node_id->nid_mark & NID_LIFTED_BY_OPTIMISE){
				rhs_node_id=lhs_node_id->nid_forward_node_id;
				lhs_node_id->nid_mark &= ~NID_LIFTED_BY_OPTIMISE;
				rhs_node_id->nid_mark &= ~NID_LIFTED_BY_OPTIMISE;
			}
		}
	}

	lhs_root->node_arity=function_arity;
	function_symbol->symb_def->sdef_arity=function_arity;
	
#ifdef THUNK_LIFT_U_RECORD_SELECTORS
	node->node_kind=NormalNode;
#endif
	node->node_symbol=function_symbol;
	node->node_arity=function_arity;

	imp_rule=create_simple_imp_rule (lhs_root,rhs_root,function_symbol->symb_def);
	
	{
		StateP arg_state_p;
		ArgP arg_p;
		
		arg_state_p=allocate_function_state (function_arity);

		imp_rule->rule_state_p=arg_state_p;

		arg_state_p[-1]=StrictState;
		
		for_l (arg_p,lhs_root->node_arguments,arg_next)
			*arg_state_p++ = arg_p->arg_state;
	}

	imp_rule->rule_next=new_rules;
	new_rules=imp_rule;
	
	return rhs_root;
}

static int is_optimisable_argument (NodeP arg_node,StateP function_arg_state_p)
{
	if (arg_node->node_kind==NormalNode){
#ifdef THUNK_LIFT_SELECTORS
		NodeP tuple_node_p;
#endif
		if (arg_node->node_symbol->symb_kind==definition){
			if ((function_arg_state_p->state_type==SimpleState && function_arg_state_p->state_kind==OnB)
#ifdef MOVE_TUPLE_RECORD_AND_ARRAY_RESULT_FUNCTION_ARGUMENT_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
				|| function_arg_state_p->state_type==TupleState || function_arg_state_p->state_type==RecordState || function_arg_state_p->state_type==ArrayState
#endif
			){
				SymbDef sdef;
				
				unsigned kind;
					
				sdef=arg_node->node_symbol->symb_def;
				kind=sdef->sdef_kind;
				
				if (arg_node->node_arity==sdef->sdef_arity){
					if (kind==IMPRULE || kind==DEFRULE || kind==SYSRULE || (kind==RECORDTYPE && sdef->sdef_strict_constructor))
						return 1;
				}
			}
		}
#ifdef UNTUPLE_STRICT_TUPLES
		else if (arg_node->node_symbol->symb_kind==tuple_symb && function_arg_state_p->state_type==TupleState)
			return 1;
#endif
#ifdef MOVE_APPLY_NODES_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
		else if (arg_node->node_symbol->symb_kind==apply_symb && function_arg_state_p->state_type==SimpleState &&
				(function_arg_state_p->state_kind==StrictOnA || function_arg_state_p->state_kind==StrictRedirection))
			return 1;
#endif
#ifdef THUNK_LIFT_SELECTORS
		else if (arg_node->node_symbol->symb_kind==select_symb && 
				 !IsLazyState (*function_state_p) &&
			 	 arg_node->node_arguments->arg_node->node_kind==NodeIdNode &&
				 arg_node->node_arguments->arg_node->node_node_id->nid_refcount>0 &&
				 IsLazyState ((tuple_node_p=arg_node->node_arguments->arg_node->node_node_id->nid_node_def->def_node)->node_state) &&
				 tuple_node_p->node_kind==NormalNode && tuple_node_p->node_symbol->symb_kind==definition &&
				 (tuple_node_p->node_symbol->symb_def->sdef_kind==IMPRULE ||
				  tuple_node_p->node_symbol->symb_def->sdef_kind==DEFRULE ||
				  tuple_node_p->node_symbol->symb_def->sdef_kind==SYSRULE) &&
				 tuple_node_p->node_arity==tuple_node_p->node_symbol->symb_def->sdef_arity)
		{
			return 1;
		}
#endif
	}
#ifdef THUNK_LIFT_U_RECORD_SELECTORS
	else if (arg_node->node_kind==SelectorNode && arg_node->node_arity>=SELECTOR_U && function_arg_state_p->state_type==TupleState)
		return 1;
#endif
	
	return 0;
}

static int has_optimisable_argument (NodeP node,StateP function_state_p)
{
	ArgP arg;
	int arg_n;

	arg=node->node_arguments;

	for (arg_n=0; arg_n<node->node_arity; ++arg_n){
		if (is_optimisable_argument (arg->arg_node,&function_state_p[arg_n]))
			return 1;

		arg=arg->arg_next;
	}
	
	return 0;
}

static int can_build_strict_constructor_or_record_in_lazy_context (NodeP node_p,StateP demanded_states)
{
	ArgP offered_arg;
	StateP demanded_state_p;
	
	for_la (offered_arg,demanded_state_p,node_p->node_arguments,demanded_states,arg_next){
		Node arg_node;
		NodeKind node_kind;
		
		arg_node=offered_arg->arg_node;
		node_kind=(NodeKind)arg_node->node_kind;

		if (node_kind!=NodeIdNode){
			if (node_kind==NormalNode){
				Symbol symbol;

				symbol=arg_node->node_symbol;

				if (BETWEEN (int_denot,real_denot,symbol->symb_kind) || symbol->symb_kind==string_denot)
					continue;

				if (symbol->symb_kind==definition){
					SymbDef sdef;

					sdef=symbol->symb_def;
					if (sdef->sdef_kind!=RECORDTYPE){
						if (arg_node->node_arity<sdef->sdef_arity || (arg_node->node_arity==0 && sdef->sdef_kind==CONSTRUCTOR))
							continue;
					} else {
						if (demanded_state_p->state_type==RecordState && arg_node->node_state.state_type==SimpleState){
							if (arg_node->node_state.state_kind==StrictOnA)
								continue;

							if (arg_node->node_state.state_kind==OnA){
								if (!sdef->sdef_strict_constructor)
									continue;
								if (can_build_strict_constructor_or_record_in_lazy_context (arg_node,sdef->sdef_record_state.state_record_arguments))
									continue;
							}
						}
					}
				}
			}
			if (!FirstStateIsStricter (arg_node->node_state,*demanded_state_p))
				return 0;
		} else {
			struct node_id *node_id;

			node_id=arg_node->node_node_id;
			if (node_id->nid_refcount<0){
				if (!FirstStateIsStricter (*node_id->nid_lhs_state_p,*demanded_state_p))
					return 0;
			} else {
				if (node_id->nid_node==NULL)
					error_in_function ("can_build_strict_constructor_or_record_in_lazy_context");

				if (!FirstStateIsStricter (node_id->nid_node->node_state,*demanded_state_p))
					return 0;
			}
		}
	}
	
	return 1;
}

#ifdef MOVE_APPLY_NODES_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
static void optimise_instance_call (NodeP node,int n_apply_args,struct state *member_states_of_field)
{
	int arg_n;
	
	arg_n=n_apply_args;
	if (arg_n>1 || is_optimisable_argument (node->node_arguments->arg_next->arg_node,&member_states_of_field[1])){
		struct arg *old_arg_p;
		struct node *rhs_root_of_function;
		
		old_arg_p = remove_apply_nodes (node,n_apply_args);
		
		rhs_root_of_function = create_new_local_function (node,member_states_of_field);
		
		if (rhs_root_of_function!=NULL){
			struct arg *arg_p;
			
			arg_p=rhs_root_of_function->node_arguments;
			rhs_root_of_function->node_arguments=old_arg_p;
			store_args_in_apply_nodes (n_apply_args,arg_p,rhs_root_of_function);
		} else
			restore_removed_apply_nodes (node,old_arg_p,n_apply_args);
	}
}
#endif

static void optimise_normal_node (Node node)
{	
	Symbol symbol;
	StateP function_state_p;

	symbol=node->node_symbol;

	if (node->node_state.state_type!=SimpleState || node->node_state.state_kind!=OnA)
		return;

	if (symbol->symb_kind!=definition){
#ifndef STRICT_STATE_FOR_LAZY_TUPLE_CONSTRUCTORS
		if (BETWEEN (int_denot,real_denot,symbol->symb_kind) || symbol->symb_kind==string_denot){
#else
		if ((BETWEEN (int_denot,real_denot,symbol->symb_kind)
			 || symbol->symb_kind==string_denot
# if STRICT_LISTS
			 || (BETWEEN (tuple_symb,nil_symb,symbol->symb_kind) && !(symbol->symb_kind==cons_symb && (symbol->symb_head_strictness>1 || symbol->symb_tail_strictness)))
# else
			 || BETWEEN (tuple_symb,nil_symb,symbol->symb_kind)
# endif
			) && node->node_state.state_kind==OnA){
#endif
			node->node_state.state_kind=StrictOnA;
			return;
		}
#ifdef MOVE_APPLY_NODES_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
		else if (symbol->symb_kind==apply_symb){
			if (node->node_arity==2 && node->node_symbol->symb_instance_apply==0){
				struct arg *arg_p;
				int n_apply_args;
				
				n_apply_args=1;
				arg_p=node->node_arguments;
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
							struct symbol *new_symbol_p;

							new_symbol_p = CompAlloc (sizeof (struct symbol));
							*new_symbol_p = *node->node_symbol;
							new_symbol_p->symb_instance_apply = 1;
							new_symbol_p->symb_next = (struct symbol*)selector_node_p->node_symbol->symb_def;
							node->node_symbol = new_symbol_p;

							if (OptimizeInstanceCalls){
								optimise_instance_call (node,n_apply_args,field_sdef->sdef_member_states_of_field);
								return;
							}
						}
					}
				}
			}

			if (apply_symb_function_state_p==NULL)
				init_apply_symb_function_state_p();
			function_state_p=apply_symb_function_state_p;
		} else
#endif		
		return;
	}
#ifdef MOVE_APPLY_NODES_IN_LAZY_CONTEXT_TO_NEW_FUNCTION
	else
#endif
	{
		SymbDef sdef;

		sdef=symbol->symb_def;

		if (node->node_arity!=sdef->sdef_arity){
			if (sdef->sdef_kind!=RECORDTYPE && node->node_arity<sdef->sdef_arity)
				node->node_state.state_kind=StrictOnA;
			return;
		}

		switch (sdef->sdef_kind){
			case IMPRULE:
# if OPTIMIZE_LAZY_TUPLE_RECURSION
				if (sdef->sdef_rule->rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY)
					return;
# endif
				function_state_p=sdef->sdef_rule->rule_state_p;
				break;
			case DEFRULE:
			case SYSRULE:
				function_state_p=sdef->sdef_rule_type->rule_type_state_p;
				break;
			case CONSTRUCTOR:
				if (sdef->sdef_strict_constructor){
					function_state_p=sdef->sdef_constructor->cl_state_p;

					if (has_optimisable_argument (node,function_state_p)
						&& !can_build_strict_constructor_or_record_in_lazy_context (node,function_state_p))
						create_new_local_function (node,function_state_p);

					return;
				} else
					return;
			default:
				return;
		}
	}

	if (has_optimisable_argument (node,function_state_p))
		create_new_local_function (node,function_state_p);
}

static int ChangeArgumentNodeStatesIfStricter (NodeP node_p,StateP demanded_states)
{
	ArgP offered_arg;
	StateP demanded_state_p;
	
	for_la (offered_arg,demanded_state_p,node_p->node_arguments,demanded_states,arg_next){
		Node arg_node;
		NodeKind node_kind;
		
		arg_node=offered_arg->arg_node;
		node_kind=(NodeKind)arg_node->node_kind;
		
		if (node_kind!=NodeIdNode){
			if (node_kind==NormalNode &&
				(BETWEEN (int_denot,real_denot,arg_node->node_symbol->symb_kind) || arg_node->node_symbol->symb_kind==string_denot)
			)
				;
			else if (demanded_state_p->state_type==RecordState
					&& arg_node->node_state.state_type==SimpleState && arg_node->node_state.state_kind==StrictOnA
					&& node_kind==NormalNode && arg_node->node_symbol->symb_kind==definition && arg_node->node_symbol->symb_def->sdef_kind==RECORDTYPE
			)
				;
			else
				if (!FirstStateIsStricter (arg_node->node_state,*demanded_state_p))
					return 0;		
		} else {
			struct node_id *node_id;

			node_id=arg_node->node_node_id;
			if (node_id->nid_refcount<0){
				if (!FirstStateIsStricter (*node_id->nid_lhs_state_p,*demanded_state_p))
					return 0;
			} else {
				if (node_id->nid_node==NULL)
					error_in_function ("ChangeArgumentNodeStatesIfStricter");

				if (!FirstStateIsStricter (node_id->nid_node->node_state,*demanded_state_p))
					return 0;
			}
		}
	}

	for_la (offered_arg,demanded_state_p,node_p->node_arguments,demanded_states,arg_next){
		Node arg_node;
		
		arg_node=offered_arg->arg_node;
		if (arg_node->node_kind==NormalNode){
			if (BETWEEN (int_denot,real_denot,arg_node->node_symbol->symb_kind) || arg_node->node_symbol->symb_kind==string_denot)
				arg_node->node_state=*demanded_state_p;
			else if (demanded_state_p->state_type==RecordState
					&& arg_node->node_state.state_type==SimpleState && arg_node->node_state.state_kind==StrictOnA
					&& arg_node->node_symbol->symb_kind==definition && arg_node->node_symbol->symb_def->sdef_kind==RECORDTYPE)
			{
				arg_node->node_state=*demanded_state_p;
			}							
		}
		
		offered_arg->arg_state=*demanded_state_p;
	}

	return 1;
}

#ifdef REUSE_UNIQUE_NODES

static NodeP replace_node_by_unique_fill_node (NodeP node,NodeP push_node,int node_size)
{
	NodeP node_copy;
	ArgP arg_p;

	node_copy=CompAllocType (NodeS);
	*node_copy=*node;

	arg_p=CompAllocType (ArgS);
	arg_p->arg_node=node_copy;
	arg_p->arg_next=NULL;
	arg_p->arg_occurrence=-1;
	
	node->node_kind=FillUniqueNode;
	node->node_node=push_node;
	node->node_arguments=arg_p;
	node->node_arity=1;

	push_node->node_push_size=node_size;
	
	--push_node->node_arguments->arg_node->node_node_id->nid_refcount;
	push_node->node_number=1;

	return node_copy;
}

void add_sizes_of_states_of_node_ids (NodeIdListElementP node_id_list,int *total_a_size_p,int *total_b_size_p)
{
	NodeIdListElementP node_id_list_elem_p;

	for_l (node_id_list_elem_p,node_id_list,nidl_next){
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		AddSizeOfState (*node_id_list_elem_p->nidl_node_id->nid_lhs_state_p,total_a_size_p,total_b_size_p);
# else
		AddSizeOfState (node_id_list_elem_p->nidl_node_id->nid_state,total_a_size_p,total_b_size_p);	
# endif	
	}	
}

#if BOXED_RECORDS
static int compute_root_n_not_updated_words (NodeP push_node,NodeP node,int node_a_size)
{
	NodeIdListElementP node_id_list;
	unsigned long n_not_updated_words;
	ArgP node_arg_p;
	unsigned int n,arity;
	int a_size1,b_size1,a_size2,b_size2;
	int total_a_size2,total_b_size2;
	
	total_a_size2=0;
	total_b_size2=0;
	add_sizes_of_states_of_node_ids (push_node->node_node_ids,&total_a_size2,&total_b_size2);
	
	n_not_updated_words=0;
	node_arg_p=node->node_arguments;
	arity=node->node_arity;
	node_id_list=push_node->node_node_ids;

	a_size1=0;
	b_size1=0;
	a_size2=0;
	b_size2=0;
	
	for (n=0; n<arity; ++n){
		int e_a_size1,e_b_size1,e_a_size2,e_b_size2;

		DetermineSizeOfState (node_arg_p->arg_state,&e_a_size1,&e_b_size1);
		
		if (node_id_list!=NULL){
			NodeIdP node_id_p;
			
			node_id_p=node_id_list->nidl_node_id;
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			DetermineSizeOfState (*node_id_p->nid_lhs_state_p,&e_a_size2,&e_b_size2);
# else
			DetermineSizeOfState (node_id_p->nid_state,&e_a_size2,&e_b_size2);
# endif
			if (node_arg_p->arg_node->node_kind==NodeIdNode && node_arg_p->arg_node->node_node_id==node_id_list->nidl_node_id){
				if (! (e_a_size1!=e_a_size2 || e_b_size1!=e_b_size2 ||
					((e_a_size1 | e_a_size2)!=0 && a_size1!=a_size2 && a_size1!=0) ||
					((e_b_size1 | e_b_size2)!=0 && b_size1+node_a_size!=b_size2+total_a_size2 && b_size1+node_a_size!=0)))
				{
					n_not_updated_words += e_a_size1+e_b_size1;
				}
			}
			
			a_size2+=e_a_size2;
			b_size2+=e_b_size2;
			node_id_list=node_id_list->nidl_next;
		}

		a_size1+=e_a_size1;
		b_size1+=e_b_size1;
		node_arg_p=node_arg_p->arg_next;
	}
	
	return n_not_updated_words;
}
#endif

static int compute_n_not_updated_words (NodeP push_node,NodeP node,int node_a_size)
{
	NodeIdListElementP node_id_list;
	unsigned long n_not_updated_words;
	ArgP node_arg_p;
	unsigned int n,arity;
	int a_size1,b_size1,a_size2,b_size2;
	int total_a_size2,total_b_size2;
	
	total_a_size2=0;
	total_b_size2=0;
	add_sizes_of_states_of_node_ids (push_node->node_node_ids,&total_a_size2,&total_b_size2);
	
	n_not_updated_words=0;
	node_arg_p=node->node_arguments;
	arity=node->node_arity;
	node_id_list=push_node->node_node_ids;

	a_size1=0;
	b_size1=0;
	a_size2=0;
	b_size2=0;
	
	for (n=0; n<arity; ++n){
		int e_a_size1,e_b_size1,e_a_size2,e_b_size2;

		DetermineSizeOfState (node_arg_p->arg_state,&e_a_size1,&e_b_size1);
		
		if (node_id_list!=NULL){
			NodeIdP node_id_p;
			
			node_id_p=node_id_list->nidl_node_id;
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			DetermineSizeOfState (*node_id_p->nid_lhs_state_p,&e_a_size2,&e_b_size2);
# else
			DetermineSizeOfState (node_id_p->nid_state,&e_a_size2,&e_b_size2);
# endif	
			if (node_arg_p->arg_node->node_kind==NodeIdNode && node_arg_p->arg_node->node_node_id==node_id_list->nidl_node_id){
				if (! (e_a_size1!=e_a_size2 || e_b_size1!=e_b_size2 ||
					((e_a_size1 | e_a_size2)!=0 && a_size1!=a_size2) ||
					((e_b_size1 | e_b_size2)!=0 && b_size1+node_a_size!=b_size2+total_a_size2)))
				{
					n_not_updated_words += e_a_size1+e_b_size1;
				}
			}
			
			a_size2+=e_a_size2;
			b_size2+=e_b_size2;
			node_id_list=node_id_list->nidl_next;
		}

		a_size1+=e_a_size1;
		b_size1+=e_b_size1;		
		node_arg_p=node_arg_p->arg_next;
	}
	
	return n_not_updated_words;
}

#if BOXED_RECORDS
static Bool insert_root_unique_fill_node (NodeP node,FreeUniqueNodeIdsP *f_node_ids,int node_a_size,int node_b_size)
{
	FreeUniqueNodeIdsP f_node_id;
	NodeP push_node,node_copy;
	ArgP node_copy_arg_p;
	unsigned long argument_overwrite_bits;
	NodeIdListElementP node_id_list;
	unsigned int n,arity;
	int node_size;
	
	node_size=node_a_size+node_b_size;

	arity=node->node_arity;

	/* optimization: update node with fewest number of words to be updated */
	{
		FreeUniqueNodeIdsP *f_node_id_h,*found_f_node_id_h;
		int found_size,found_n_not_updated_words;
		
		found_f_node_id_h=NULL;
		f_node_id_h=f_node_ids;
		
		while ((f_node_id=*f_node_id_h)!=NULL){
			int new_found_size;
			
			new_found_size=f_node_id->fnid_node_size;

			if (new_found_size>=node_size){
				int new_found_n_not_updated_words;
				
				new_found_n_not_updated_words=compute_root_n_not_updated_words (f_node_id->fnid_push_node,node,node_a_size);
				
				if (found_f_node_id_h==NULL || new_found_size<found_size || new_found_n_not_updated_words>found_n_not_updated_words){
					found_f_node_id_h=f_node_id_h;
					found_size=new_found_size;
					found_n_not_updated_words=new_found_n_not_updated_words;
				}
			}

			f_node_id_h=&f_node_id->fnid_next;
		}
		
		if (found_f_node_id_h==NULL)
			return False;
		
		f_node_id=*found_f_node_id_h;
		*found_f_node_id_h=f_node_id->fnid_next;
	}

	push_node=f_node_id->fnid_push_node;
	
	node_copy=replace_node_by_unique_fill_node (node,push_node,f_node_id->fnid_node_size);

	{
	int a_size1,b_size1,a_size2,b_size2;
	int total_a_size2,total_b_size2;
	
	total_a_size2=0;
	total_b_size2=0;
	add_sizes_of_states_of_node_ids (push_node->node_node_ids,&total_a_size2,&total_b_size2);
	
	argument_overwrite_bits=0;
	node_copy_arg_p=node_copy->node_arguments;
	node_id_list=push_node->node_node_ids;

	a_size1=0;
	b_size1=0;
	a_size2=0;
	b_size2=0;
	
	for (n=0; n<arity; ++n){
		int e_a_size1,e_b_size1,e_a_size2,e_b_size2;
		
		DetermineSizeOfState (node_copy_arg_p->arg_state,&e_a_size1,&e_b_size1);

		if (node_id_list!=NULL){
			NodeIdP node_id_p;
			
			node_id_p=node_id_list->nidl_node_id;
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			DetermineSizeOfState (*node_id_p->nid_lhs_state_p,&e_a_size2,&e_b_size2);
# else
			DetermineSizeOfState (node_id_p->nid_state,&e_a_size2,&e_b_size2);
# endif
			if (node_copy_arg_p->arg_node->node_kind==NodeIdNode && node_copy_arg_p->arg_node->node_node_id==node_id_list->nidl_node_id){
				if (e_a_size1!=e_a_size2 ||
					e_b_size1!=e_b_size2 ||
					(e_a_size1!=0 && (a_size1!=a_size2 ||
						a_size1==0 ||
						((a_size1==1 || (a_size1==0 && e_a_size1>1)) &&
							((node_size==2) != (total_a_size2+total_b_size2==2)))
						)) ||
					(e_b_size1!=0 && (b_size1+node_a_size!=b_size2+total_a_size2 ||
						b_size1+node_a_size==0 ||
						((b_size1+node_a_size==1 || (b_size1+node_a_size==0 && e_b_size1>1)) &&
							((node_size==2) != (total_a_size2+total_b_size2==2)))
						)))
				{
					argument_overwrite_bits|=1<<n;
				} else {
					++node_id_p->nid_refcount;
					node_id_p->nid_mark |= NID_EXTRA_REFCOUNT_MASK;
				}
			} else
				argument_overwrite_bits|=1<<n;
			
			a_size2+=e_a_size2;
			b_size2+=e_b_size2;
			node_id_list=node_id_list->nidl_next;
		} else
			argument_overwrite_bits|=1<<n;

		a_size1+=e_a_size1;
		b_size1+=e_b_size1;
		node_copy_arg_p=node_copy_arg_p->arg_next;
	}
	}
	
	node->node_arguments->arg_occurrence=argument_overwrite_bits;

	return True;
}
#endif

static Bool insert_unique_fill_node (NodeP node,FreeUniqueNodeIdsP *f_node_ids,int node_a_size,int node_b_size)
{
	FreeUniqueNodeIdsP f_node_id;
	NodeP push_node,node_copy;
	ArgP node_copy_arg_p;
	unsigned long argument_overwrite_bits;
	NodeIdListElementP node_id_list;
	unsigned int n,arity;
	int node_size;
	
	node_size=node_a_size+node_b_size;

	arity=node->node_arity;

#if 1
	/* optimization: update node with fewest number of words to be updated */
	{
		FreeUniqueNodeIdsP *f_node_id_h,*found_f_node_id_h;
		int found_size,found_n_not_updated_words;
		
		found_f_node_id_h=NULL;
		f_node_id_h=f_node_ids;
		
		while ((f_node_id=*f_node_id_h)!=NULL){
			int new_found_size;
			
			new_found_size=f_node_id->fnid_node_size;
			if (new_found_size>=node_size){
				int new_found_n_not_updated_words;
				
				new_found_n_not_updated_words=compute_n_not_updated_words (f_node_id->fnid_push_node,node,node_a_size);
				
				if (found_f_node_id_h==NULL || new_found_size<found_size || new_found_n_not_updated_words>found_n_not_updated_words){
					found_f_node_id_h=f_node_id_h;
					found_size=new_found_size;
					found_n_not_updated_words=new_found_n_not_updated_words;
				}
			}

			f_node_id_h=&f_node_id->fnid_next;
		}
		
		if (found_f_node_id_h==NULL)
			return False;
		
		f_node_id=*found_f_node_id_h;
		*found_f_node_id_h=f_node_id->fnid_next;
	}
#else
	f_node_id=*f_node_ids;
	
	if (f_node_id->fnid_node_size>=node_size)
		*f_node_ids=f_node_id->fnid_next;
	else {
		FreeUniqueNodeIdsP prev_f_node_id;
		
		do {
			prev_f_node_id=f_node_id;
			f_node_id=f_node_id->fnid_next;

			if (f_node_id==NULL)
				return False;

		} while (f_node_id->fnid_node_size<node_size);
		
		prev_f_node_id->fnid_next=f_node_id->fnid_next;
	}
#endif

	push_node=f_node_id->fnid_push_node;
	
	node_copy=replace_node_by_unique_fill_node (node,push_node,f_node_id->fnid_node_size);

	{
	int a_size1,b_size1,a_size2,b_size2;
	int total_a_size2,total_b_size2;
	
	total_a_size2=0;
	total_b_size2=0;
	add_sizes_of_states_of_node_ids (push_node->node_node_ids,&total_a_size2,&total_b_size2);
	
	argument_overwrite_bits=0;
	node_copy_arg_p=node_copy->node_arguments;
	node_id_list=push_node->node_node_ids;

	a_size1=0;
	b_size1=0;
	a_size2=0;
	b_size2=0;
	
	for (n=0; n<arity; ++n){
		int e_a_size1,e_b_size1,e_a_size2,e_b_size2;
		
		DetermineSizeOfState (node_copy_arg_p->arg_state,&e_a_size1,&e_b_size1);

		if (node_id_list!=NULL){
			NodeIdP node_id_p;
			
			node_id_p=node_id_list->nidl_node_id;
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			DetermineSizeOfState (*node_id_p->nid_lhs_state_p,&e_a_size2,&e_b_size2);
# else
			DetermineSizeOfState (node_id_p->nid_state,&e_a_size2,&e_b_size2);
# endif	
			if (node_copy_arg_p->arg_node->node_kind==NodeIdNode && node_copy_arg_p->arg_node->node_node_id==node_id_list->nidl_node_id){
				if (e_a_size1!=e_a_size2 ||
					e_b_size1!=e_b_size2 ||
					(e_a_size1!=0 && (a_size1!=a_size2 ||
						((a_size1==1 || (a_size1==0 && e_a_size1>1)) &&
							((node_size==2) != (total_a_size2+total_b_size2==2)))
						)) ||
					(e_b_size1!=0 && (b_size1+node_a_size!=b_size2+total_a_size2 ||
						((b_size1+node_a_size==1 || (b_size1+node_a_size==0 && e_b_size1>1)) &&
							((node_size==2) != (total_a_size2+total_b_size2==2)))
						)))
				{
					argument_overwrite_bits|=1<<n;
				} else {
					++node_id_p->nid_refcount;
					node_id_p->nid_mark |= NID_EXTRA_REFCOUNT_MASK;
				}
			} else
				argument_overwrite_bits|=1<<n;
			
			a_size2+=e_a_size2;
			b_size2+=e_b_size2;
			node_id_list=node_id_list->nidl_next;
		} else
			argument_overwrite_bits|=1<<n;

		a_size1+=e_a_size1;
		b_size1+=e_b_size1;
		node_copy_arg_p=node_copy_arg_p->arg_next;
	}
	}
	
	node->node_arguments->arg_occurrence=argument_overwrite_bits;

	return True;
}

static Bool try_insert_constructor_update_node (NodeP node,FreeUniqueNodeIdsP *f_node_ids)
{
	if (node->node_state.state_type==SimpleState && node->node_state.state_kind!=SemiStrict){
		switch (node->node_symbol->symb_kind){
			case definition:
			{
				SymbDef sdef;
				
				sdef=node->node_symbol->symb_def;
				switch (sdef->sdef_kind){
					case CONSTRUCTOR:
						if (! (node->node_arity>0 && sdef->sdef_arity==node->node_arity))
							return False;
						/* else */
					case RECORDTYPE:
						if (!sdef->sdef_strict_constructor)
							return insert_unique_fill_node (node,f_node_ids,node->node_arity,0);
						else if (!IsLazyStateKind (node->node_state.state_kind)){
							int a_size,b_size;

							DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
															
							return insert_unique_fill_node (node,f_node_ids,a_size,b_size);
						} else
							return False;
				}
				
				break;
			}
			case cons_symb:
#if STRICT_LISTS
				if (node->node_symbol->symb_head_strictness>1 || node->node_symbol->symb_tail_strictness){
					if (!IsLazyStateKind (node->node_state.state_kind) && !(node->node_symbol->symb_head_strictness & 1) && node->node_arity==2){
						if (node->node_symbol->symb_head_strictness!=4)
							return insert_unique_fill_node (node,f_node_ids,2,0);			
						else {
							int a_size,b_size;

							DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
															
							return insert_unique_fill_node (node,f_node_ids,a_size,b_size);
						}
					} else
						return False;
				} else
#endif
				return insert_unique_fill_node (node,f_node_ids,2,0);
			case tuple_symb:
				return insert_unique_fill_node (node,f_node_ids,node->node_arity,0);
		}
	}

	return False;
}

#if BOXED_RECORDS
static Bool try_insert_root_constructor_update_node (NodeP node,FreeUniqueNodeIdsP *f_node_ids)
{
	if (node->node_state.state_type==SimpleState && node->node_state.state_kind!=SemiStrict){
		switch (node->node_symbol->symb_kind){
			case definition:
			{
				SymbDef sdef;
				
				sdef=node->node_symbol->symb_def;
				switch (sdef->sdef_kind){
					case CONSTRUCTOR:
						if (sdef->sdef_arity==node->node_arity){
							if (sdef->sdef_strict_constructor){
								int a_size,b_size;

								DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
								
								if (a_size+b_size>2)
									return insert_root_unique_fill_node (node,f_node_ids,a_size,b_size);
							} else {
								if (node->node_arity>2)
									return insert_root_unique_fill_node (node,f_node_ids,node->node_arity,0);
							}
						}
						return False;
					case RECORDTYPE:
						if (sdef->sdef_boxed_record){
							if (sdef->sdef_strict_constructor){
								int a_size,b_size;

								DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
								
								if (a_size+b_size>2)
									return insert_root_unique_fill_node (node,f_node_ids,a_size,b_size);							
							} else {
								if (node->node_arity>2)
									return insert_root_unique_fill_node (node,f_node_ids,node->node_arity,0);
							}
						}
						return False;
				}
				break;
			}
		}
	}

	return False;
}
#endif

static NodeP try_insert_function_update_node (NodeP node,FreeUniqueNodeIdsS **f_node_ids_l)
{
	if (!(node->node_state.state_type==SimpleState && node->node_state.state_kind==SemiStrict) &&
		(IsLazyState (node->node_state) ? node->node_arity<=2 : ExpectsResultNode (node->node_state)) && 
		node->node_symbol->symb_kind==definition)
	{
		SymbDef sdef;
		
		sdef=node->node_symbol->symb_def;
		
		if (node->node_arity==sdef->sdef_arity)
			switch (sdef->sdef_kind){
				case IMPRULE:
				case DEFRULE:
				case SYSRULE:
				{
					FreeUniqueNodeIdsP f_node_id;
					NodeP node_copy;
					
					f_node_id=*f_node_ids_l;
					
					if (f_node_id->fnid_node_size>=2)
						*f_node_ids_l=f_node_id->fnid_next;
					else {
						FreeUniqueNodeIdsP prev_f_node_id;
						
						do {
							prev_f_node_id=f_node_id;
							f_node_id=f_node_id->fnid_next;

							if (f_node_id==NULL)
								return node;

						} while (f_node_id->fnid_node_size<2);
						
						prev_f_node_id->fnid_next=f_node_id->fnid_next;
					}
					
					node_copy=replace_node_by_unique_fill_node (node,f_node_id->fnid_push_node,f_node_id->fnid_node_size);
					
					return node_copy;
				}
			}
	}
	return node;
}

#endif

static void optimise_strict_constructor_in_lazy_context (NodeP node,FreeUniqueNodeIdsS **f_node_ids_l)
{
	Symbol symbol;

	symbol = node->node_symbol;
	if (symbol->symb_kind==definition){
		SymbDef sdef;
		
		sdef=symbol->symb_def;
		if (sdef->sdef_kind==CONSTRUCTOR){
			if (node->node_state.state_type==SimpleState && node->node_state.state_kind==OnA && sdef->sdef_arity==node->node_arity){
				if (!sdef->sdef_strict_constructor){
					node->node_state.state_kind=StrictOnA;
				} else {
					if (ChangeArgumentNodeStatesIfStricter (node,sdef->sdef_constructor->cl_state_p)){
						node->node_state.state_kind=StrictOnA;
#ifdef REUSE_UNIQUE_NODES
						if (*f_node_ids_l!=NULL)
							try_insert_constructor_update_node (node,f_node_ids_l);
#endif
					}
				}
			}
		} else if (sdef->sdef_kind==RECORDTYPE){
			if (node->node_state.state_type==SimpleState && node->node_state.state_kind==OnA){
				if (!sdef->sdef_strict_constructor){
					node->node_state.state_kind=StrictOnA;
				} else {
					if (ChangeArgumentNodeStatesIfStricter (node,sdef->sdef_record_state.state_record_arguments)){
						node->node_state.state_kind=StrictOnA;
#ifdef REUSE_UNIQUE_NODES
						if (*f_node_ids_l!=NULL)
							try_insert_constructor_update_node (node,f_node_ids_l);
#endif
					}
				}
			}
		}
	}
	else if (symbol->symb_kind==select_symb && node->node_arguments->arg_node->node_kind==NodeIdNode){
		NodeIdP node_id;

		node_id=node->node_arguments->arg_node->node_node_id;
		if (node_id->nid_refcount>0){
			NodeP tuple_node;
			
			tuple_node=node_id->nid_node_def->def_node;
			if (tuple_node->node_kind==TupleSelectorsNode){
				ArgP new_arg;
				
				new_arg=NewArgument (node);
				new_arg->arg_next=tuple_node->node_arguments; 
				tuple_node->node_arguments=new_arg;
				++tuple_node->node_arity;
			} else {
				if (tuple_node->node_state.state_type==TupleState){
					if (! (tuple_node->node_kind==NodeIdNode && tuple_node->node_arguments->arg_state.state_type!=TupleState)){
						Node tuple_selectors_node;
						
						tuple_selectors_node=NewNodeByKind (TupleSelectorsNode,NULL,NewArgument (node),1);
						tuple_selectors_node->node_state=tuple_node->node_state;
						tuple_selectors_node->node_node=tuple_node;
						tuple_selectors_node->node_number=0;
						node_id->nid_node_def->def_node=tuple_selectors_node;
					}
				} else if (tuple_node->node_kind==NormalNode && tuple_node->node_symbol->symb_kind==select_symb){
					NodeP select2_node_p,tuple_node2_p;
					NodeIdP node_id_p;
					
					select2_node_p=tuple_node->node_arguments->arg_node;
					if (select2_node_p->node_kind==NodeIdNode){						
						node_id_p=select2_node_p->node_node_id;
						if (node_id_p->nid_refcount>0){
							tuple_node2_p=node_id_p->nid_node_def->def_node;
							if (tuple_node2_p->node_kind==TupleSelectorsNode && tuple_node2_p->node_state.state_type==TupleState){
								int element_n;

								element_n=tuple_node->node_arity-1;																
								if (tuple_node2_p->node_state.state_tuple_arguments[element_n].state_type==TupleState){
									NodeP tuple_selectors_node;

									tuple_selectors_node=NewNodeByKind (TupleSelectorsNode,NULL,NewArgument (node),1);
									tuple_selectors_node->node_state=tuple_node2_p->node_state.state_tuple_arguments[element_n];
									tuple_selectors_node->node_node=tuple_node;
									tuple_selectors_node->node_number=1;
									node_id->nid_node_def->def_node=tuple_selectors_node;
								}
							}
						}
					}
				}
			}
		}
	}
}

#if OPTIMIZE_LAZY_TUPLE_RECURSION
static unsigned int current_rule_mark;
#endif

static FreeUniqueNodeIdsP free_unique_node_id_list;

static FreeUniqueNodeIdsP copy_free_unique_node_ids (FreeUniqueNodeIdsP f_node_ids)
{
	FreeUniqueNodeIdsP f_node_id,new_f_node_ids,*new_f_node_ids_l;
	
	new_f_node_ids_l=&new_f_node_ids;
	
	for_l (f_node_id,f_node_ids,fnid_next){
		FreeUniqueNodeIdsP next_f_node_id;

		if (free_unique_node_id_list!=NULL){
			next_f_node_id=free_unique_node_id_list;
			free_unique_node_id_list=next_f_node_id->fnid_next;
		} else
			next_f_node_id=CompAllocType (FreeUniqueNodeIdsS);

		next_f_node_id->fnid_push_node=f_node_id->fnid_push_node;
		next_f_node_id->fnid_node_size=f_node_id->fnid_node_size;
		
		*new_f_node_ids_l=next_f_node_id;
		new_f_node_ids_l=&next_f_node_id->fnid_next;
	}
	*new_f_node_ids_l=NULL;
	
	return new_f_node_ids;
}

static FreeUniqueNodeIdsS *free_free_unique_node_ids (FreeUniqueNodeIdsS *f_node_id)
{
	while (f_node_id!=NULL){
		FreeUniqueNodeIdsP next_f_node_id;
		
		next_f_node_id=f_node_id->fnid_next;
		
		f_node_id->fnid_next=free_unique_node_id_list;
		free_unique_node_id_list=f_node_id;
		
		f_node_id=next_f_node_id;
	}
	
	return f_node_id;
}

static void optimise_then_or_else (NodeP node,NodeDefP node_defs,FreeUniqueNodeIdsP f_node_ids,int local_scope);

static void optimise_then_and_else (NodeP if_node,FreeUniqueNodeIdsP f_node_ids,int local_scope)
{
	FreeUniqueNodeIdsP then_f_node_ids,else_f_node_ids;
	ArgP then_arg;

	then_arg=if_node->node_arguments->arg_next;

#ifdef REUSE_UNIQUE_NODES
	then_f_node_ids=copy_free_unique_node_ids (f_node_ids);
#else
	then_f_node_ids=NULL;
#endif
	optimise_then_or_else (then_arg->arg_node,if_node->node_then_node_defs,then_f_node_ids,local_scope);

#ifdef REUSE_UNIQUE_NODES
	then_f_node_ids=free_free_unique_node_ids (then_f_node_ids);
	else_f_node_ids=copy_free_unique_node_ids (f_node_ids);
#else
	else_f_node_ids=NULL;
#endif
	optimise_then_or_else (then_arg->arg_next->arg_node,if_node->node_else_node_defs,else_f_node_ids,local_scope);

#ifdef REUSE_UNIQUE_NODES
	else_f_node_ids=free_free_unique_node_ids (else_f_node_ids);
#endif
}

static FreeUniqueNodeIdsP no_free_unique_node_ids=NULL;

static void optimise_node_in_then_or_else (NodeP node,FreeUniqueNodeIdsS **f_node_ids_l,int local_scope)
{
	switch (node->node_kind){
		case NodeIdNode:
			return;
		case NormalNode:
		{
			ArgP arg;

#ifdef REUSE_UNIQUE_NODES
			if (*f_node_ids_l!=NULL && try_insert_constructor_update_node (node,f_node_ids_l)){
				unsigned int n,argument_overwrite_bits;
				
				argument_overwrite_bits=node->node_arguments->arg_occurrence;
				node=node->node_arguments->arg_node;
				
				n=0;
				for_l (arg,node->node_arguments,arg_next){
					if (argument_overwrite_bits & (1<<n))
						optimise_node_in_then_or_else (arg->arg_node,f_node_ids_l,local_scope);

					++n;
				}
			} else {
#endif
			optimise_normal_node (node);
		
			for_l (arg,node->node_arguments,arg_next)
				optimise_node_in_then_or_else (arg->arg_node,f_node_ids_l,local_scope);

#ifdef REUSE_UNIQUE_NODES
			if (*f_node_ids_l!=NULL)
				node=try_insert_function_update_node (node,f_node_ids_l);
			}
#endif
			optimise_strict_constructor_in_lazy_context (node,f_node_ids_l);

			return;
		}
		case SelectorNode:
#ifdef THUNK_LIFT_U_RECORD_SELECTORS
			if (node->node_arity>=SELECTOR_U && node->node_state.state_type==SimpleState && node->node_state.state_kind==OnA){
				StateP selector_arg_state_p;
				StateS tuple_state,tuple_arg_states[2];

				selector_arg_state_p=&node->node_symbol->symb_def->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;

				if (node->node_arity>=SELECTOR_L)
					selector_arg_state_p=selector_l_or_n_state_p (&tuple_state,tuple_arg_states,selector_arg_state_p);

				if (is_optimisable_argument (node->node_arguments->arg_node,selector_arg_state_p)){
					ArgP arg;
					
					create_new_local_function (node,selector_arg_state_p);

					for_l (arg,node->node_arguments,arg_next)
						optimise_node_in_then_or_else (arg->arg_node,f_node_ids_l,local_scope);
					
					return;
				}
			}
#endif
#if DESTRUCTIVE_RECORD_UPDATES
			if (node->node_arguments->arg_node->node_kind==NodeIdNode && node->node_arity==1)
				return;
#endif
		case MatchNode:
			optimise_node_in_then_or_else (node->node_arguments->arg_node,f_node_ids_l,local_scope);
			return;
		case UpdateNode:
		{
			ArgP arg;
			
#if DESTRUCTIVE_RECORD_UPDATES
			arg=node->node_arguments;
			if (arg->arg_node->node_kind==NodeIdNode){
				NodeIdP node_id;
				
				node_id=arg->arg_node->node_node_id;
				if ((node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 && node_id->nid_refcount==-2)
					++node_id->nid_number;
# if BOXED_RECORDS
				node_id->nid_mark2 |= NID_RECORD_USED_BY_UPDATE;
# endif
				arg=arg->arg_next;
			}
# if BOXED_RECORDS
			else {
				optimise_node_in_then_or_else (arg->arg_node,f_node_ids_l,local_scope);
				if (arg->arg_node->node_kind==NodeIdNode)
					arg->arg_node->node_node_id->nid_mark2 |= NID_RECORD_USED_BY_UPDATE;
				
				arg=arg->arg_next;
			}
# endif
			for (; arg!=NULL; arg=arg->arg_next)
				optimise_node_in_then_or_else (arg->arg_node,f_node_ids_l,local_scope);
#else
			for_l (arg,node->node_arguments,arg_next)
				optimise_node_in_then_or_else (arg->arg_node,f_node_ids_l,local_scope);
#endif

			return;
		}
		case IfNode:
			optimise_then_and_else (node,*f_node_ids_l,node->node_if_scope+2);		

			optimise_node_in_then_or_else (node->node_arguments->arg_node,&no_free_unique_node_ids,local_scope);
			return;
		case TupleSelectorsNode:
			optimise_node_in_then_or_else (node->node_node,f_node_ids_l,local_scope);
			return;
		default:
			error_in_function ("optimise_node_in_then_or_else");
			return;
	}
}

#if OPTIMIZE_LAZY_TUPLE_RECURSION
unsigned long global_result_and_call_same_select_vector;

static void compute_same_select_vector (NodeP root_node)
{
	unsigned long same_select_vector;
	ArgP tuple_element_p;
	int n;
	
	same_select_vector=0;
	
	for_li (tuple_element_p,n,root_node->node_arguments,arg_next){
		NodeP node_p;
		
		node_p=tuple_element_p->arg_node;
		
		if (node_p->node_symbol->symb_kind==select_symb
			&& node_p->node_arguments->arg_node->node_kind==NodeIdNode
			&& n+1==node_p->node_arity
			&& (node_p->node_arguments->arg_node->node_node_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY)
		)
			same_select_vector |= (1<<n);
	}
	
	global_result_and_call_same_select_vector=same_select_vector;
}

static FreeUniqueNodeIdsP insert_unique_fill_nodes_for_lazy_tuple_recursive_call (NodeP node,FreeUniqueNodeIdsP f_node_ids)
{
	int n,tuple_arity;
	
	tuple_arity=node->node_symbol->symb_def->sdef_rule->rule_type->type_alt_rhs->type_node_arity;

	for (n=tuple_arity-1; n>=0 && f_node_ids!=NULL; --n){
		if (!(global_result_and_call_same_select_vector & (1<<n))){
			FreeUniqueNodeIdsP f_node_id;
					
			if (f_node_ids->fnid_node_size>=2){
				f_node_id=f_node_ids;
				f_node_ids=f_node_ids->fnid_next;
			} else {
				FreeUniqueNodeIdsP prev_f_node_id;
				
				f_node_id=f_node_ids;
				do {
					prev_f_node_id=f_node_id;
					f_node_id=f_node_id->fnid_next;

					if (f_node_id==NULL)
						break;

				} while (f_node_id->fnid_node_size<2);
				
				prev_f_node_id->fnid_next=f_node_id->fnid_next;
			}
			
			replace_node_by_unique_fill_node (node,f_node_id->fnid_push_node,f_node_id->fnid_node_size);
		}
	}
	
	return f_node_ids;
}
#endif

static void optimise_then_or_else (NodeP node,NodeDefP node_defs,FreeUniqueNodeIdsP f_node_ids,int local_scope)
{
	NodeDefP node_def;

#if OPTIMIZE_LAZY_TUPLE_RECURSION
	if ((current_rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY) && node->node_kind==NormalNode && node->node_symbol->symb_kind==tuple_symb)
		compute_same_select_vector (node);
#endif
	
	for_l (node_def,node_defs,def_next)
		if (node_def->def_node){
#if OPTIMIZE_LAZY_TUPLE_RECURSION
			if (node_def->def_id!=NULL && (node_def->def_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY)){
				ArgP arg;
				NodeP node;
				
				node=node_def->def_node;
   
				optimise_normal_node (node);
				
				for_l (arg,node->node_arguments,arg_next)
					optimise_node_in_then_or_else (arg->arg_node,&f_node_ids,local_scope);

				optimise_strict_constructor_in_lazy_context (node,&f_node_ids);

				if (f_node_ids!=NULL)
					f_node_ids=insert_unique_fill_nodes_for_lazy_tuple_recursive_call (node,f_node_ids);
			} else
#endif
			optimise_node_in_then_or_else (node_def->def_node,&f_node_ids,local_scope);
		}

#ifdef REUSE_UNIQUE_NODES
	if (node->node_kind==NormalNode){
		ArgP arg;

		
#if BOXED_RECORDS
		if (f_node_ids!=NULL && try_insert_root_constructor_update_node (node,&f_node_ids)){
			unsigned int n,argument_overwrite_bits;

			argument_overwrite_bits=node->node_arguments->arg_occurrence;
			node=node->node_arguments->arg_node;
			
			n=0;
			for_l (arg,node->node_arguments,arg_next){
				if (argument_overwrite_bits & (1<<n))
					optimise_node_in_then_or_else (arg->arg_node,&f_node_ids,local_scope);

				++n;
			}
		} else {
#endif


		optimise_normal_node (node);
	
#if OPTIMIZE_LAZY_TUPLE_RECURSION
		if (node->node_symbol->symb_kind==tuple_symb && (current_rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY)){
			for_l (arg,node->node_arguments,arg_next){
				NodeP node;

				node=arg->arg_node;
				
				if (node->node_kind==NormalNode){
					ArgS *arg;

					optimise_normal_node (node);
					
					for_l (arg,node->node_arguments,arg_next)
						optimise_node_in_then_or_else (arg->arg_node,&f_node_ids,local_scope);

					optimise_strict_constructor_in_lazy_context (node,&no_free_unique_node_ids);
				} else
					optimise_node_in_then_or_else (arg->arg_node,&f_node_ids,local_scope);
			}
		} else
#endif
		for_l (arg,node->node_arguments,arg_next)
			optimise_node_in_then_or_else (arg->arg_node,&f_node_ids,local_scope);

		optimise_strict_constructor_in_lazy_context (node,&no_free_unique_node_ids);

#if BOXED_RECORDS
			}
#endif


	} else
#endif
	optimise_node_in_then_or_else (node,&f_node_ids,local_scope);
}

static void optimise_node (NodeP node,FreeUniqueNodeIdsS **f_node_ids_l)
{
	switch (node->node_kind){
		case NodeIdNode:
			return;
		case NormalNode:
		{
			ArgP arg;
				
#ifdef REUSE_UNIQUE_NODES
			if (*f_node_ids_l!=NULL && try_insert_constructor_update_node (node,f_node_ids_l)){
				unsigned int n,argument_overwrite_bits;
				
				argument_overwrite_bits=node->node_arguments->arg_occurrence;
				node=node->node_arguments->arg_node;
				
				n=0;
				for_l (arg,node->node_arguments,arg_next){
					if (argument_overwrite_bits & (1<<n))
						optimise_node (arg->arg_node,f_node_ids_l);

					++n;
				}
			} else {
#endif
			optimise_normal_node (node);
			
			for_l (arg,node->node_arguments,arg_next)
				optimise_node (arg->arg_node,f_node_ids_l);

#ifdef REUSE_UNIQUE_NODES
			if (*f_node_ids_l!=NULL)
				node=try_insert_function_update_node (node,f_node_ids_l);
			}
#endif
			optimise_strict_constructor_in_lazy_context (node,f_node_ids_l);

			return;
		}
		case SelectorNode:
#ifdef THUNK_LIFT_U_RECORD_SELECTORS
			if (node->node_arity>=SELECTOR_U && node->node_state.state_type==SimpleState && node->node_state.state_kind==OnA){
				StateP selector_arg_state_p;
				StateS tuple_state,tuple_arg_states[2];

				selector_arg_state_p=&node->node_symbol->symb_def->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
				
				if (node->node_arity>=SELECTOR_L)
					selector_arg_state_p=selector_l_or_n_state_p (&tuple_state,tuple_arg_states,selector_arg_state_p);

				if (is_optimisable_argument (node->node_arguments->arg_node,selector_arg_state_p)){
					ArgP arg;
					
					create_new_local_function (node,selector_arg_state_p);

					for_l (arg,node->node_arguments,arg_next)
						optimise_node (arg->arg_node,f_node_ids_l);
					
					return;
				}
			}
#endif
#if DESTRUCTIVE_RECORD_UPDATES
			if (node->node_arguments->arg_node->node_kind==NodeIdNode && node->node_arity==1)
				return;
#endif
		case MatchNode:
			optimise_node (node->node_arguments->arg_node,f_node_ids_l);
			return;
		case UpdateNode:
		{
			ArgP arg;

#if DESTRUCTIVE_RECORD_UPDATES
			arg=node->node_arguments;

			if (arg->arg_node->node_kind==NodeIdNode){
				NodeIdP node_id;
				
				node_id=arg->arg_node->node_node_id;
				if ((node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 && node_id->nid_refcount==-2)
					++node_id->nid_number;
# if BOXED_RECORDS
				node_id->nid_mark2 |= NID_RECORD_USED_BY_UPDATE;
# endif
				arg=arg->arg_next;
			}
# if BOXED_RECORDS
			else {
				optimise_node (arg->arg_node,f_node_ids_l);
				if (arg->arg_node->node_kind==NodeIdNode)
					arg->arg_node->node_node_id->nid_mark2 |= NID_RECORD_USED_BY_UPDATE;
				
				arg=arg->arg_next;
			}
# endif

			for (; arg!=NULL; arg=arg->arg_next)
				optimise_node (arg->arg_node,f_node_ids_l);
#else
			for_l (arg,node->node_arguments,arg_next)
				optimise_node (arg->arg_node,f_node_ids_l);
#endif
			return;
		}
		case TupleSelectorsNode:
			optimise_node (node->node_node,f_node_ids_l);
			return;
		default:
			error_in_function ("optimise_node");
	}
}

#ifdef REUSE_UNIQUE_NODES
static FreeUniqueNodeIdsP check_unique_push_node (NodeP node,FreeUniqueNodeIdsP f_node_ids,int switch_node_id_refcount)
{
	NodeIdP node_id_p;

# if STRICT_LISTS
	if (node->node_symbol->symb_kind==cons_symb && (node->node_symbol->symb_head_strictness & 1))
		return f_node_ids;
#endif

	node_id_p=node->node_arguments->arg_node->node_node_id;

	if (switch_node_id_refcount==-1 && (node_id_p->nid_mark & NID_EXTRA_REFCOUNT_MASK)==0){
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		if (node_id_p->nid_lhs_state_p->state_type==SimpleState && (node_id_p->nid_lhs_state_p->state_mark & STATE_UNIQUE_MASK)){
# else
		if (node_id_p->nid_state.state_type==SimpleState && (node_id_p->nid_state.state_mark & STATE_UNIQUE_MASK)){
# endif
			int a_size,b_size;
			NodeIdListElementP arg_node_id_list;

			a_size=0;
			b_size=0;

			for_l (arg_node_id_list,node->node_node_ids,nidl_next){
				NodeIdP arg_node_id;
				StateP arg_node_id_state_p;
				
				arg_node_id=arg_node_id_list->nidl_node_id;
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				arg_node_id_state_p=arg_node_id->nid_lhs_state_p;
# else
				arg_node_id_state_p=&arg_node_id->nid_state;
# endif				

#if DESTRUCTIVE_RECORD_UPDATES	
				arg_node_id->nid_mark2|=NID_HAS_REFCOUNT_WITHOUT_UPDATES;
				arg_node_id->nid_number=arg_node_id->nid_refcount;
#endif			
				AddSizeOfState (*arg_node_id_state_p,&a_size,&b_size);
			}
			
			if (a_size+b_size>0){
				FreeUniqueNodeIdsP f_node_id;

				f_node_id=CompAllocType (FreeUniqueNodeIdsS);			
				f_node_id->fnid_push_node=node;
				f_node_id->fnid_node_size=a_size+b_size;

#if 0
				printf ("Push unique node of size %d\n",a_size+b_size);
#endif

				f_node_id->fnid_next=f_node_ids;
				return f_node_id;
			}
		}
#if DESTRUCTIVE_RECORD_UPDATES
		else {
			NodeIdListElementP arg_node_id_list;

			for_l (arg_node_id_list,node->node_node_ids,nidl_next){
				NodeIdP node_id;
			
				node_id=arg_node_id_list->nidl_node_id;
				node_id->nid_mark2|=NID_HAS_REFCOUNT_WITHOUT_UPDATES;
				node_id->nid_number=node_id->nid_refcount;
			}
		}
#endif			
	} else {
		NodeIdListElementP arg_node_id_list;

		for_l (arg_node_id_list,node->node_node_ids,nidl_next){
			NodeIdP node_id;
			
			node_id=arg_node_id_list->nidl_node_id;
			node_id->nid_mark |= NID_EXTRA_REFCOUNT_MASK;
		}
	}
	return f_node_ids;
}

static void optimise_root_node (NodeP node,NodeDefP node_defs,FreeUniqueNodeIdsP f_node_ids)
{
	switch (node->node_kind){
		case SwitchNode:
		{
			ArgP arg;
			int switch_node_id_refcount;
			NodeIdP switch_node_id_p;

			if (node_defs!=NULL)
				error_in_function ("optimise_root_node");
			
			switch_node_id_p=node->node_node_id;
			
			++switch_node_id_p->nid_refcount;
			switch_node_id_refcount=switch_node_id_p->nid_refcount;
			
			for_l (arg,node->node_arguments,arg_next){
				Node case_node;
				
				case_node=arg->arg_node;
				if (case_node->node_kind==CaseNode || case_node->node_kind==DefaultNode){
					NodeP case_alt_node_p;
					FreeUniqueNodeIdsP case_f_node_ids;
						
					case_f_node_ids=f_node_ids;
					
					case_alt_node_p=case_node->node_arguments->arg_node;

					set_local_reference_counts (case_node);

					if (case_alt_node_p->node_kind==PushNode){
#ifdef REUSE_UNIQUE_NODES
						if (DoReuseUniqueNodes){
							if (case_alt_node_p->node_arguments->arg_node->node_node_id!=switch_node_id_p)
								error_in_function ("optimise_root_node");

							case_f_node_ids=check_unique_push_node (case_alt_node_p,case_f_node_ids,switch_node_id_refcount);
						}
#endif
						case_alt_node_p=case_alt_node_p->node_arguments->arg_next->arg_node;
					}

					optimise_root_node (case_alt_node_p,case_node->node_node_defs,case_f_node_ids);

					set_global_reference_counts (case_node);

				} else if (case_node->node_kind==OverloadedCaseNode){
					NodeP case_alt_node_p;
					FreeUniqueNodeIdsP case_f_node_ids;
					
					case_node=case_node->node_node;
					
					case_f_node_ids=f_node_ids;
					
					case_alt_node_p=case_node->node_arguments->arg_node;

					set_local_reference_counts (case_node);

					optimise_root_node (case_alt_node_p,case_node->node_node_defs,case_f_node_ids);

					set_global_reference_counts (case_node);

				} else
					error_in_function ("optimise_root_node");
			}

			--switch_node_id_p->nid_refcount;
			
			return;
		}
		case GuardNode:
			optimise_root_node (node->node_arguments->arg_node,node_defs,f_node_ids);
			optimise_root_node (node->node_arguments->arg_next->arg_node,node->node_node_defs,f_node_ids);
			return;
		case IfNode:
			optimise_then_and_else (node,f_node_ids,node->node_if_scope+2);

			optimise_root_node (node->node_arguments->arg_node,node_defs,NULL);

			return;
		default:
		{
			NodeDefP def;

#if OPTIMIZE_LAZY_TUPLE_RECURSION
			if ((current_rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY) && node->node_kind==NormalNode && node->node_symbol->symb_kind==tuple_symb)
				compute_same_select_vector (node);
#endif
			
#ifdef REUSE_UNIQUE_NODES
			f_node_ids=copy_free_unique_node_ids (f_node_ids);
#else
			f_node_ids=NULL;
#endif

			for_l (def,node_defs,def_next)
				if (def->def_node){
#if OPTIMIZE_LAZY_TUPLE_RECURSION
					if (def->def_id!=NULL && (def->def_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY)){
						ArgP arg;
						NodeP node;
						
						node=def->def_node;

						optimise_normal_node (node);
						
						for_l (arg,node->node_arguments,arg_next)
							optimise_node (arg->arg_node,&f_node_ids);

						optimise_strict_constructor_in_lazy_context (node,&f_node_ids);

						if (f_node_ids!=NULL)
							f_node_ids=insert_unique_fill_nodes_for_lazy_tuple_recursive_call (node,f_node_ids);
					} else
#endif
					optimise_node (def->def_node,&f_node_ids);
				}
			
			if (node->node_kind==NormalNode){
				ArgS *arg;

#if BOXED_RECORDS
				if (f_node_ids!=NULL && try_insert_root_constructor_update_node (node,&f_node_ids)){
					unsigned int n,argument_overwrite_bits;

					argument_overwrite_bits=node->node_arguments->arg_occurrence;
					node=node->node_arguments->arg_node;
					
					n=0;
					for_l (arg,node->node_arguments,arg_next){
						if (argument_overwrite_bits & (1<<n))
							optimise_node (arg->arg_node,&f_node_ids);

						++n;
					}
				} else {
#endif

				optimise_normal_node (node);
				
#if OPTIMIZE_LAZY_TUPLE_RECURSION
				if (node->node_symbol->symb_kind==tuple_symb && (current_rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY)){
					for_l (arg,node->node_arguments,arg_next){
						NodeP node;

						node=arg->arg_node;
						
						if (node->node_kind==NormalNode){
							ArgS *arg;

							optimise_normal_node (node);
							
							for_l (arg,node->node_arguments,arg_next)
								optimise_node (arg->arg_node,&f_node_ids);

							optimise_strict_constructor_in_lazy_context (node,&no_free_unique_node_ids);
						} else
							optimise_node (node,&f_node_ids);
					}
				} else
#endif
				for_l (arg,node->node_arguments,arg_next)
					optimise_node (arg->arg_node,&f_node_ids);

				optimise_strict_constructor_in_lazy_context (node,&no_free_unique_node_ids);

#if BOXED_RECORDS
			}
#endif

			} else
				optimise_node (node,&f_node_ids);

#ifdef REUSE_UNIQUE_NODES
			f_node_ids=free_free_unique_node_ids (f_node_ids);
#endif
		}
	}
}
#endif

static ImpRuleS *used_local_functions;

static Bool IsObservedDef (NodeDefP def_p)
{
	NodeP node_p;

	node_p=def_p->def_node;
	if (node_p==NULL)
		return True;
	else
		return False;
}

static Bool IsStrictAnnotedAndNotParallelDef (NodeDefs def)
{
	Node node;
	
	node=def->def_node;
	if (node==NULL || (node->node_annotation==StrictAnnot && !(node->node_state.state_mark & STATE_PARALLEL_MASK)))
		return True;
	else
		return False;
}

Bool HasExternalAnnot (Node node)
{
	if (node->node_annotation==NoAnnot)
		return False;
	
	switch (node->node_annotation){
		case ParallelAnnot:
		case ParallelAtAnnot:
		case ParallelNFAnnot:
			return True;
		default:
			return False;
	}
}

static Bool IsExternalNodeDef (NodeDefs def)
{
	if (def->def_node)
	    return HasExternalAnnot (def->def_node);

	return False;
}

static Bool IsParallelNodeDef (NodeDefs def)
{
	if (def->def_node && def->def_node->node_annotation>StrictAnnot)
		return True;

	return False;
}

static Bool IsNotParStrictDef (NodeDefs def)
{
	if (def->def_node==NULL
			|| !(def->def_node->node_state.state_mark & STATE_PARALLEL_MASK)
			|| IsLazyState (def->def_node->node_state))
		return True;
	else
		return False;
}

static Bool IsAnyNodeDef (NodeDefs def)
{
#pragma unused(def)

	return True;
}

static void ExamineSymbolApplication (struct node *node)
{
	Symbol symbol;
	SymbDef sdef;

	symbol=node->node_symbol;

	if (symbol->symb_kind!=definition){
		if (symbol->symb_kind==cons_symb && symbol->symb_head_strictness==4){
			if (node->node_arity<2)
				symbol->symb_unboxed_cons_sdef_p->sdef_mark |= SDEF_USED_CURRIED_MASK;
			else {
				StateP unboxed_cons_state_p;
				int mark;
				
				if (IsLazyState (node->node_state)){
					symbol->symb_unboxed_cons_sdef_p->sdef_mark |= SDEF_USED_LAZILY_MASK;
					mark = SDEF_USED_LAZILY_MASK;
				} else {
					mark = SDEF_USED_STRICTLY_MASK;
				}
				unboxed_cons_state_p = symbol->symb_unboxed_cons_state_p;
				if (unboxed_cons_state_p->state_type==SimpleState){
					if (BETWEEN (IntObj,FileObj,unboxed_cons_state_p->state_object))
						unboxed_cons_mark[unboxed_cons_state_p->state_object-IntObj][symbol->symb_tail_strictness] |= mark;
				} else if (unboxed_cons_state_p->state_type==ArrayState){
					unboxed_cons_array_mark |= mark;				
				}
			}
		} else if (symbol->symb_kind==seq_symb){
			if (node->node_arity!=2)
				SeqDef->sdef_mark |= SDEF_USED_CURRIED_MASK;
			else {
				if (IsLazyState (node->node_state))
					SeqDef->sdef_mark |= SDEF_USED_LAZILY_MASK;
				else
					SeqDef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
			}
		}
		return;
	}
		
	sdef=symbol->symb_def;

	if (sdef->sdef_kind==IMPRULE){
		if (sdef->sdef_arity!=node->node_arity){
			if (!sdef->sdef_exported){
				ImpRuleP rule_p;
				
				rule_p=sdef->sdef_rule;
				if (sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK)){
					rule_p->rule_mark &= ~RULE_LAZY_CALL_NODE_MASK;
				} else {
					if (!(sdef->sdef_mark & (SDEF_USED_STRICTLY_MASK | SDEF_OPTIMISED_FUNCTION_MASK))){
						rule_p->rule_next_used_function=used_local_functions;
						used_local_functions=rule_p;
					}

					rule_p->rule_mark |= RULE_LAZY_CALL_NODE_MASK;
					rule_p->rule_lazy_call_node = node;
				}
#if STORE_STRICT_CALL_NODES
				rule_p->rule_mark &= ~(RULE_STRICT_CALL_NODE_MASK | RULE_STRICT_CALL_NODE2_MASK);
#endif
			}
			sdef->sdef_mark |= SDEF_USED_CURRIED_MASK;
		} else {
			if (IsLazyState (node->node_state)){
				if (!sdef->sdef_exported){
					ImpRuleP rule_p;
				
					rule_p=sdef->sdef_rule;
					if (sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK)){
						rule_p->rule_mark &= ~RULE_LAZY_CALL_NODE_MASK;
					} else {
						if (!(sdef->sdef_mark & (SDEF_USED_STRICTLY_MASK | SDEF_OPTIMISED_FUNCTION_MASK))){
							rule_p->rule_next_used_function=used_local_functions;
							used_local_functions=rule_p;
						}

						rule_p->rule_mark |= RULE_LAZY_CALL_NODE_MASK;
						rule_p->rule_lazy_call_node = node;
					}
#if STORE_STRICT_CALL_NODES
					rule_p->rule_mark &= ~(RULE_STRICT_CALL_NODE_MASK | RULE_STRICT_CALL_NODE2_MASK);
#endif
				}
				sdef->sdef_mark |= SDEF_USED_LAZILY_MASK;
			} else {
				if (!(sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK | SDEF_USED_STRICTLY_MASK | SDEF_OPTIMISED_FUNCTION_MASK))
					&& !sdef->sdef_exported)
				{
					sdef->sdef_rule->rule_next_used_function=used_local_functions;
					used_local_functions=sdef->sdef_rule;
				}

#if STORE_STRICT_CALL_NODES
				if (!sdef->sdef_exported){
					ImpRuleP rule_p;
				
					rule_p=sdef->sdef_rule;
					if (sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK) ||
						((sdef->sdef_mark & SDEF_USED_STRICTLY_MASK) && !(rule_p->rule_mark & RULE_STRICT_CALL_NODE_MASK)))
					{
						rule_p->rule_mark &= ~(RULE_STRICT_CALL_NODE_MASK | RULE_STRICT_CALL_NODE2_MASK);
					} else {
						if (!(rule_p->rule_mark & RULE_STRICT_CALL_NODE_MASK)){
							rule_p->rule_mark |= RULE_STRICT_CALL_NODE_MASK;
							rule_p->rule_strict_call_node = node;
						} else if (!(rule_p->rule_mark & RULE_STRICT_CALL_NODE2_MASK)){
							rule_p->rule_mark |= RULE_STRICT_CALL_NODE2_MASK;
							rule_p->rule_strict_call_node2 = node;
						} else {
							rule_p->rule_mark &= ~(RULE_STRICT_CALL_NODE_MASK | RULE_STRICT_CALL_NODE2_MASK);
						}
					}
				}
#endif

				sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
			}
		}
	} else {
		if (sdef->sdef_arity != node->node_arity)
			sdef->sdef_mark |= SDEF_USED_CURRIED_MASK;
		else
			if (IsLazyState (node->node_state))
				sdef->sdef_mark |= SDEF_USED_LAZILY_MASK;
			else
				sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
	}
}

static void ExamineSymbolApplicationOfSelectorOrUpdateNode (Symbol symb,StateS symbstate)
{
	SymbDef sdef;

	if (symb->symb_kind!=definition)
		return;
	
	sdef = symb->symb_def;

	if (IsLazyState (symbstate))
		sdef->sdef_mark |= SDEF_USED_LAZILY_MASK;
	else
		sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
}

static void MarkDependentNodeDefs (NodeP node);

static void MarkTupleSelectorsNode (NodeIdP node_id,NodeP tuple_node)
{
	if (tuple_node->node_arity==node_id->nid_refcount){
		struct arg *arg,**arg_pp;
		int i,arity;
		Node select_nodes[32];

		if (tuple_node->node_number==1){
			if (tuple_node->node_node->node_kind==NodeIdNode)
				tuple_node->node_node->node_arguments->arg_state=tuple_node->node_state;
			else {
				tuple_node->node_state=tuple_node->node_node->node_state;

				MarkDependentNodeDefs (tuple_node->node_node);
				
				return;	
			}
		}

		arity=tuple_node->node_state.state_arity;
		for (i=0; i<arity; ++i)
			select_nodes[i]=NULL;
		
		for_l (arg,tuple_node->node_arguments,arg_next){
			int element_n;
			Node select_node;
			NodeId element_node_id;
			struct arg *select_arg;
			
			select_node=arg->arg_node;
			element_n=select_node->node_arity-1;
			
			if (select_nodes[element_n]!=NULL){
				element_node_id=select_nodes[element_n]->node_node_id;
				element_node_id->nid_mark |= NID_SHARED_SELECTION_NODE_ID;
			} else {
				element_node_id=NewNodeId (NULL);
				element_node_id->nid_number=element_n;
				element_node_id->nid_node=select_node;
				element_node_id->nid_scope = node_id->nid_scope;
				element_node_id->nid_mark2 |= NID_SELECTION_NODE_ID;
				select_nodes[element_n]=select_node;
			}
			
			++element_node_id->nid_refcount;
						
			select_node->node_kind=NodeIdNode;
			select_node->node_node_id=element_node_id;
			
			select_node->node_state=tuple_node->node_state.state_tuple_arguments[element_n];
			select_arg=select_node->node_arguments;

			if (!IsSimpleState (select_arg->arg_state))
				select_arg->arg_state=select_arg->arg_state.state_tuple_arguments[element_n];
		}
		
		arg_pp=&tuple_node->node_arguments;
		arg=tuple_node->node_arguments;
		
		for (i=arity-1; i>=0; --i)
			if (select_nodes[i]!=NULL){
				arg->arg_node=select_nodes[i];
				*arg_pp=arg;
				arg_pp=&arg->arg_next;
				arg=arg->arg_next;
			}
		
		*arg_pp=NULL;
	} else if (tuple_node->node_number==1)
		tuple_node->node_state=tuple_node->node_node->node_state;

	MarkDependentNodeDefs (tuple_node->node_node);
}

static void MarkDependentNodeDefs (NodeP node)
{
	Args arg;

	switch (node->node_kind){
		case NodeIdNode:
		{
			NodeId node_id;
			NodeDefS *def;
			
			node_id=node->node_node_id;
			if (node_id->nid_refcount>0){
				def=node_id->nid_node_def;
			
				if (def && (def->def_mark & NODE_DEF_MARKED)==0 && def->def_node){					
					def->def_mark |= NODE_DEF_MARKED;
					
					if (def->def_node->node_kind==TupleSelectorsNode)
						MarkTupleSelectorsNode (node_id,def->def_node);
					else
						MarkDependentNodeDefs (def->def_node);
				}
			}

			return;
		}
		case NormalNode:
			if (node->node_symbol->symb_kind==select_symb && node->node_arguments->arg_node->node_kind==NodeIdNode){
				NodeId node_id;

				node_id=node->node_arguments->arg_node->node_node_id;
				if (node_id->nid_refcount>0){
					MarkDependentNodeDefs (node->node_arguments->arg_node);
					ExamineSymbolApplication (node);						
					return;
				}
			}

			ExamineSymbolApplication (node);
			break;
		case MatchNode:
			ExamineSymbolApplication (node);
			break;
		case SelectorNode:
			if (node->node_symbol->symb_kind==definition){
				if (node->node_arity==1 && IsLazyState (node->node_state))
					node->node_symbol->symb_def->sdef_mark |= SDEF_USED_LAZILY_MASK;
				else
					node->node_symbol->symb_def->sdef_mark |= SDEF_USED_STRICTLY_MASK;
			}
			break;
		case UpdateNode:
			ExamineSymbolApplicationOfSelectorOrUpdateNode (node->node_symbol,node->node_state);

			arg=node->node_arguments;
			MarkDependentNodeDefs (arg->arg_node);

			while ((arg=arg->arg_next)!=NULL)
				MarkDependentNodeDefs (arg->arg_node->node_arguments->arg_node);

			return;
		case IfNode:
			break;
/*
			MarkDependentNodeDefs (node->node_arguments->arg_node);
			return;
*/
		case PushNode:
			break;
#ifdef REUSE_UNIQUE_NODES
		case FillUniqueNode:
			break;
#endif
		default:
			error_in_function ("MarkDependentNodeDefs");
	}

	for_l (arg,node->node_arguments,arg_next)
		MarkDependentNodeDefs (arg->arg_node);
}

typedef Bool NodeDefFun (NodeDefs);

static void MarkNodeDefsWithProperty
#ifdef applec
	(NodeDefs defs, Bool (*node_def_function)())
#else
	(NodeDefs defs, NodeDefFun node_def_function)
#endif
{
	NodeDefS *def;
	
	for_l (def,defs,def_next)
		if ((def->def_mark & NODE_DEF_MARKED)==0 && node_def_function (def)){
			def->def_mark |= NODE_DEF_MARKED;
			if (def->def_node){
				if (def->def_node->node_kind==TupleSelectorsNode)
					MarkTupleSelectorsNode (def->def_id,def->def_node);
				else
					MarkDependentNodeDefs (def->def_node);
			}
		}
}

static NodeDefs *MoveMarkedNodeDefsToReorderedList (NodeDefs *def_p,NodeDefs *reordered_defs_p)
{
	NodeDefs def;
	
	while (def=*def_p,def!=NULL)
		if ((def->def_mark & NODE_DEF_MARKED)!=0){
			*def_p=def->def_next;
			*reordered_defs_p=def;
			reordered_defs_p=&def->def_next;
		} else
			def_p=&def->def_next;
	
	return reordered_defs_p;
}

static void ReorderNodeDefinitionsAndDetermineUsedEntries (NodeDefs *def_p,Node root)
{
	NodeDefs reordered_defs,*reordered_defs_p;

	while (root->node_kind==PushNode)
		root=root->node_arguments->arg_next->arg_node;
	
	if (root->node_kind==SwitchNode){
		struct arg *arg;
		
		if (*def_p!=NULL)
			error_in_function ("ReorderNodeDefinitionsAndDetermineUsedEntries");
		
		for_l (arg,root->node_arguments,arg_next){
			switch (arg->arg_node->node_kind){
				case CaseNode:
				{
					SymbolP symbol;

					symbol=arg->arg_node->node_symbol;
					if (symbol->symb_kind==definition){
						SymbDef sdef;
	
						sdef=symbol->symb_def;
						if (sdef->sdef_kind==CONSTRUCTOR){
							sdef->sdef_isused=True;
							sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
						}
					}		
					/* no break */
				}
				case DefaultNode:
					ReorderNodeDefinitionsAndDetermineUsedEntries (&arg->arg_node->node_node_defs,arg->arg_node->node_arguments->arg_node);
					break;
				case OverloadedCaseNode:
				{
					NodeP overloaded_case_node_p,case_node_p;
					
					overloaded_case_node_p=arg->arg_node;
					MarkDependentNodeDefs (overloaded_case_node_p->node_arguments->arg_node);
					MarkDependentNodeDefs (overloaded_case_node_p->node_arguments->arg_next->arg_node);

					case_node_p=overloaded_case_node_p->node_node;
					ReorderNodeDefinitionsAndDetermineUsedEntries (&case_node_p->node_node_defs,case_node_p->node_arguments->arg_node);
					break;
				}
				default:
					error_in_function ("ReorderNodeDefinitionsAndDetermineUsedEntries");
			}
		}
		
		return;
	} else if (root->node_kind==GuardNode){
		ReorderNodeDefinitionsAndDetermineUsedEntries (def_p,root->node_arguments->arg_node);
		ReorderNodeDefinitionsAndDetermineUsedEntries (&root->node_node_defs,root->node_arguments->arg_next->arg_node);		
		return;
	}
	
	reordered_defs_p=&reordered_defs;

	MarkNodeDefsWithProperty (*def_p,&IsObservedDef);
	reordered_defs_p=MoveMarkedNodeDefsToReorderedList (def_p,reordered_defs_p);
	
	MarkNodeDefsWithProperty (*def_p,&IsStrictAnnotedAndNotParallelDef);
	reordered_defs_p=MoveMarkedNodeDefsToReorderedList (def_p,reordered_defs_p);
	
	MarkNodeDefsWithProperty (*def_p,&IsExternalNodeDef);
	reordered_defs_p=MoveMarkedNodeDefsToReorderedList (def_p,reordered_defs_p);

	MarkNodeDefsWithProperty (*def_p,&IsParallelNodeDef);
	reordered_defs_p=MoveMarkedNodeDefsToReorderedList (def_p,reordered_defs_p);

	MarkNodeDefsWithProperty (*def_p,&IsNotParStrictDef);
	reordered_defs_p=MoveMarkedNodeDefsToReorderedList (def_p,reordered_defs_p);

	MarkNodeDefsWithProperty (*def_p,&IsAnyNodeDef);
	if (root->node_kind!=IfNode)
		MarkDependentNodeDefs (root);
	else
		MarkDependentNodeDefs (root->node_arguments->arg_node);
	reordered_defs_p=MoveMarkedNodeDefsToReorderedList (def_p,reordered_defs_p);

	*reordered_defs_p=NULL;
	*def_p=reordered_defs;

	if (root->node_kind==IfNode){
		ReorderNodeDefinitionsAndDetermineUsedEntries (&root->node_then_node_defs,root->node_arguments->arg_next->arg_node);
		ReorderNodeDefinitionsAndDetermineUsedEntries (&root->node_else_node_defs,root->node_arguments->arg_next->arg_next->arg_node);
	}
}

static NodeIdRefCountListP determine_then_or_else_ref_counts (NodeP node,NodeDefP node_defs,int local_scope);

static NodeIdRefCountListP determine_then_else_ref_counts_of_graph (NodeP node,NodeIdRefCountListP node_id_ref_counts,int local_scope)
{
	switch (node->node_kind){
		case NodeIdNode:
		{
			NodeIdP node_id;
			int node_id_scope;
			
			node_id=node->node_node_id;

			node_id_scope=node_id->nid_scope;
			if (node_id_scope<0)
				node_id_scope=-node_id_scope;
			
			if (node_id_scope<local_scope){
				if (!(node_id->nid_mark & NID_THEN_ELSE_NON_LOCAL_NODE_ID)){
					node_id->nid_mark |= NID_THEN_ELSE_NON_LOCAL_NODE_ID;
					node_id_ref_counts=new_node_id_ref_count (node_id_ref_counts,node_id,1);
					node_id->nid_node_id_ref_count_element_=node_id_ref_counts;
				} else
					++node_id->nid_node_id_ref_count_element->nrcl_ref_count;
			}

			return node_id_ref_counts;
		}
		case NormalNode:
		case UpdateNode:
		{
			ArgP arg;
			
			for_l (arg,node->node_arguments,arg_next)
				node_id_ref_counts=determine_then_else_ref_counts_of_graph (arg->arg_node,node_id_ref_counts,local_scope);

			return node_id_ref_counts;
		}
		case SelectorNode:
		case MatchNode:
			return determine_then_else_ref_counts_of_graph (node->node_arguments->arg_node,node_id_ref_counts,local_scope);
#ifdef REUSE_UNIQUE_NODES
		case FillUniqueNode:
		{
			NodeP node_p;
			ArgP arg_p;
			unsigned long occurences;
			int n;
			
			node_p=node->node_arguments->arg_node;
			if (node_p->node_kind!=NormalNode)
				error_in_function ("determine_then_else_ref_counts_of_graph");
			
			n=0;
			occurences=node->node_arguments->arg_occurrence;
			
			for_l (arg_p,node_p->node_arguments,arg_next){
				if (occurences & (1<<n))
					node_id_ref_counts=determine_then_else_ref_counts_of_graph (arg_p->arg_node,node_id_ref_counts,local_scope);
				++n;
			}

			return node_id_ref_counts;
		}
#endif
		case IfNode:
		{
			ArgP cond_arg,then_arg;
			NodeIdRefCountListP local_node_id_ref_count;
			int new_local_scope;
			
			new_local_scope=node->node_if_scope+2;
			
			cond_arg=node->node_arguments;

			then_arg=cond_arg->arg_next;
			
			node->node_then_node_id_ref_counts=
				determine_then_or_else_ref_counts (then_arg->arg_node,node->node_then_node_defs,new_local_scope);		
			node->node_else_node_id_ref_counts=
				determine_then_or_else_ref_counts (then_arg->arg_next->arg_node,node->node_else_node_defs,new_local_scope);

			for_l (local_node_id_ref_count,node->node_then_node_id_ref_counts,nrcl_next){
				NodeIdP node_id;
				int node_id_scope;
				
				node_id=local_node_id_ref_count->nrcl_node_id;
				
				node_id_scope=node_id->nid_scope;
				if (node_id_scope<0)
					node_id_scope=-node_id_scope;
			
				if (node_id_scope<local_scope){
					if (!(node_id->nid_mark & NID_THEN_ELSE_NON_LOCAL_NODE_ID)){
						node_id->nid_mark |= NID_THEN_ELSE_NON_LOCAL_NODE_ID;
						node_id_ref_counts=new_node_id_ref_count (node_id_ref_counts,node_id,local_node_id_ref_count->nrcl_ref_count);
						node_id->nid_node_id_ref_count_element_=node_id_ref_counts;
					} else
						node_id->nid_node_id_ref_count_element->nrcl_ref_count += local_node_id_ref_count->nrcl_ref_count;
				}
			}

			for_l (local_node_id_ref_count,node->node_else_node_id_ref_counts,nrcl_next){
				NodeIdP node_id;
				int node_id_scope;
				
				node_id=local_node_id_ref_count->nrcl_node_id;
				
				node_id_scope=node_id->nid_scope;
				if (node_id_scope<0)
					node_id_scope=-node_id_scope;
			
				if (node_id_scope<local_scope){
					if (!(node_id->nid_mark & NID_THEN_ELSE_NON_LOCAL_NODE_ID)){
						node_id->nid_mark |= NID_THEN_ELSE_NON_LOCAL_NODE_ID;
						node_id_ref_counts=new_node_id_ref_count (node_id_ref_counts,node_id,local_node_id_ref_count->nrcl_ref_count);
						node_id->nid_node_id_ref_count_element_=node_id_ref_counts;
					} else
						node_id->nid_node_id_ref_count_element->nrcl_ref_count += local_node_id_ref_count->nrcl_ref_count;
				}
			}
						
			return determine_then_else_ref_counts_of_graph (cond_arg->arg_node,node_id_ref_counts,local_scope);
		}
		case TupleSelectorsNode:
			return determine_then_else_ref_counts_of_graph (node->node_node,node_id_ref_counts,local_scope);
		default:
			error_in_function ("determine_then_else_ref_counts_of_graph");
			return node_id_ref_counts;
	}
}

static NodeIdRefCountListP determine_then_or_else_ref_counts (NodeP node,NodeDefP node_defs,int local_scope)
{
	NodeIdRefCountListP local_node_id_ref_counts,local_node_id_ref_count;
	NodeDefP node_def;

	local_node_id_ref_counts=determine_then_else_ref_counts_of_graph (node,NULL,local_scope);

	for_l (node_def,node_defs,def_next)
		if (node_def->def_node)
			local_node_id_ref_counts=determine_then_else_ref_counts_of_graph (node_def->def_node,local_node_id_ref_counts,local_scope);

	for_l (local_node_id_ref_count,local_node_id_ref_counts,nrcl_next)
		local_node_id_ref_count->nrcl_node_id->nid_mark &= ~NID_THEN_ELSE_NON_LOCAL_NODE_ID;

	return local_node_id_ref_counts;
}

static void determine_then_else_ref_counts (NodeP node)
{
 	switch (node->node_kind){
		case IfNode:
		{
			ArgP then_arg;
			int local_scope;
			
			local_scope=node->node_if_scope+2;
			
			then_arg=node->node_arguments->arg_next;
			
			node->node_then_node_id_ref_counts=determine_then_or_else_ref_counts (then_arg->arg_node,node->node_then_node_defs,local_scope);
			node->node_else_node_id_ref_counts=determine_then_or_else_ref_counts (then_arg->arg_next->arg_node,node->node_else_node_defs,local_scope);
			
			determine_then_else_ref_counts (node->node_arguments->arg_node);
			return;
		}
		case GuardNode:
			determine_then_else_ref_counts (node->node_arguments->arg_node);
			determine_then_else_ref_counts (node->node_arguments->arg_next->arg_node);
			return;
		case SwitchNode:
		{
			ArgP arg;
		
			for_l (arg,node->node_arguments,arg_next){
				Node case_node;
			
				case_node=arg->arg_node;
				if (case_node->node_kind==CaseNode || case_node->node_kind==DefaultNode){
					NodeP case_alt_node_p;
											
					case_alt_node_p=case_node->node_arguments->arg_node;
					if (case_alt_node_p->node_kind==PushNode)
						case_alt_node_p=case_alt_node_p->node_arguments->arg_next->arg_node;

					++node->node_node_id->nid_refcount;
					set_local_reference_counts (case_node);

					determine_then_else_ref_counts (case_alt_node_p);

					set_global_reference_counts (case_node);
					--node->node_node_id->nid_refcount;
				
				} else  if (case_node->node_kind==OverloadedCaseNode){
					case_node=case_node->node_node;
					
					++node->node_node_id->nid_refcount;
					set_local_reference_counts (case_node);

					determine_then_else_ref_counts (case_node->node_arguments->arg_node);

					set_global_reference_counts (case_node);
					--node->node_node_id->nid_refcount;
				
				} else
					error_in_function ("determine_then_else_ref_counts");
			}
			return;
		}
		default:
			return;
	}
}

#ifdef REUSE_UNIQUE_NODES
static void mark_shared_strict_tuple_or_record (ArgP arguments)
{
	ArgP arg_p;

	for_l (arg_p,arguments,arg_next){
		if (arg_p->arg_node->node_kind==NodeIdNode)
			arg_p->arg_node->node_node_id->nid_mark |= NID_EXTRA_REFCOUNT_MASK;
	}
}

static void mark_shared_strict_tuple_and_record_elements (Args args,int ref_count_one)
{
	ArgP arg_p;
	
	for_l (arg_p,args,arg_next){
		Node arg_node;
		int ref_count_one_for_arg;

		arg_node=arg_p->arg_node;
		ref_count_one_for_arg=ref_count_one;
		
		if (arg_node->node_kind==NodeIdNode){
			NodeId node_id;
			
			node_id=arg_node->node_node_id;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS /* added 3-3-2000 */
			if (node_id->nid_refcount<-2 || (node_id->nid_mark & NID_EXTRA_REFCOUNT_MASK))
#else
			if (node_id->nid_refcount!=-1 || (node_id->nid_mark & NID_EXTRA_REFCOUNT_MASK))
#endif
				ref_count_one_for_arg=0;

#if DESTRUCTIVE_RECORD_UPDATES	
			node_id->nid_mark2|=NID_HAS_REFCOUNT_WITHOUT_UPDATES;
			node_id->nid_number=node_id->nid_refcount;
#endif
			arg_node=arg_node->node_node_id->nid_node;
		}
		
		if (arg_node!=NULL){
			Symbol symbol;
			
			symbol = arg_node->node_symbol;
			
			switch (symbol->symb_kind){
				case tuple_symb:
					if (!IsSimpleState (arg_p->arg_state)){
						if (!ref_count_one_for_arg)
							mark_shared_strict_tuple_or_record (arg_node->node_arguments);
						
						mark_shared_strict_tuple_and_record_elements (arg_node->node_arguments,ref_count_one_for_arg);
					}
					break;
				case definition:
				{
					SymbDef def;
					
					def = symbol->symb_def;
					if (def->sdef_kind==RECORDTYPE){
						if (arg_p->arg_state.state_type==RecordState){
							if (!ref_count_one_for_arg)
								mark_shared_strict_tuple_or_record (arg_node->node_arguments);
							
							mark_shared_strict_tuple_and_record_elements (arg_node->node_arguments,ref_count_one_for_arg);
						}
					}
				}
			}
		}
	}
}
#endif

static ImpRuleS **OptimiseRule (ImpRuleS *rule)
{
	SymbDef rule_sdef;
	RuleAlts alt;

	CurrentSymbol = rule->rule_root->node_symbol;
	
	rule_sdef= CurrentSymbol->symb_def;

	alt=rule->rule_alts;
	CurrentLine = alt->alt_line;

	if (alt->alt_kind==Contractum){				
#ifdef REUSE_UNIQUE_NODES
		if (DoReuseUniqueNodes)
			mark_shared_strict_tuple_and_record_elements (alt->alt_lhs_root->node_arguments,1);
#endif
#if OPTIMIZE_LAZY_TUPLE_RECURSION
		current_rule_mark=rule->rule_mark;
#endif
		optimise_root_node (alt->alt_rhs_root,alt->alt_rhs_defs,NULL);

		ReorderNodeDefinitionsAndDetermineUsedEntries (&alt->alt_rhs_defs,alt->alt_rhs_root);

		determine_then_else_ref_counts (alt->alt_rhs_root);
	}
	
	while (new_rules){
		ImpRuleP new_rule;
		RuleAltP alt;
		
		new_rule=new_rules;
		new_rules=new_rule->rule_next;
		
		alt=new_rule->rule_alts;
		DetermineStatesOfRootNodeAndDefs (alt->alt_rhs_root,&alt->alt_rhs_defs,alt->alt_lhs_root->node_state,0);
		ReorderNodeDefinitionsAndDetermineUsedEntries (&alt->alt_rhs_defs,alt->alt_rhs_root);
		
		new_rule->rule_next=rule->rule_next;
		rule->rule_next=new_rule;
		rule=new_rule;
	}
	
	return &rule->rule_next;
}

StateP state_of_node_or_node_id (NodeP node_p)
{
	if (node_p->node_kind!=NodeIdNode){
		return &node_p->node_state;	
	} else {
		NodeIdP node_id;

		node_id=node_p->node_node_id;
		if (node_id->nid_refcount<0)
			return node_id->nid_lhs_state_p;
		else
			return &node_id->nid_node->node_state;
	}
}

void OptimiseRules (ImpRules rules,SymbDef start_sdef)
{
	ImpRuleS **rule_h;
	
	next_function_n=0;
	new_rules=NULL;
#ifdef REUSE_UNIQUE_NODES
	free_unique_node_id_list=NULL;
#endif

	used_local_functions=NULL;

	if (start_sdef!=NULL && !start_sdef->sdef_exported){
		used_local_functions=start_sdef->sdef_rule;
		used_local_functions->rule_next_used_function=NULL;
	}

	for (rule_h=&rules; *rule_h!=NULL; )
		if ((*rule_h)->rule_root->node_symbol->symb_def->sdef_exported)
			rule_h=OptimiseRule (*rule_h);
		else
			rule_h=&(*rule_h)->rule_next;
	
	while (used_local_functions!=NULL){
		ImpRuleS *rule;
		
		rule=used_local_functions;
		used_local_functions=used_local_functions->rule_next_used_function;
		
		OptimiseRule (rule);
	}

# ifdef THINK_C
	if (!DoParallel)
# endif
	{
		ImpRuleP rule_p;
		
		for_l (rule_p,rules,rule_next){
			if ((rule_p->rule_mark & RULE_LAZY_CALL_NODE_MASK) &&
				!(rule_p->rule_root->node_symbol->symb_def->sdef_mark & SDEF_USED_CURRIED_MASK) &&
				!(rule_p->rule_mark & RULE_CAF_MASK))
			{
				NodeP call_node_p;
				
				call_node_p=rule_p->rule_lazy_call_node;
				if (call_node_p->node_number==0 && !(call_node_p->node_state.state_type==SimpleState && call_node_p->node_state.state_kind==SemiStrict)){
					StateP function_arg_state_p;
					ArgP arg_p;

					rule_p->rule_mark |= RULE_UNBOXED_LAZY_CALL;

					for_la (arg_p,function_arg_state_p,call_node_p->node_arguments,rule_p->rule_state_p,arg_next){
						if (function_arg_state_p->state_type==SimpleState){
							if (function_arg_state_p->state_kind==OnB){		
								StateP arg_state_p;
						
								arg_state_p=state_of_node_or_node_id (arg_p->arg_node);
								if (arg_state_p->state_type==SimpleState && arg_state_p->state_kind==OnB){
									arg_p->arg_state=*arg_state_p;
									continue;
								}			
							}
						} else if (function_arg_state_p->state_type==ArrayState){
							StateP arg_state_p;
						
							arg_state_p=state_of_node_or_node_id (arg_p->arg_node);
							if (arg_state_p->state_type==ArrayState){
								arg_p->arg_state=*arg_state_p;
								continue;
							}
						}
					}
				}
			}
		}
	}	

#if STORE_STRICT_CALL_NODES
	{
		ImpRuleP rule_p;
		
		for_l (rule_p,rules,rule_next){
			if (rule_p->rule_mark & RULE_STRICT_CALL_NODE_MASK){
				ArgP arg_p1,arg_p2,lhs_arg_p;
				StateP function_arg_state_p;
				
				if (rule_p->rule_mark & RULE_STRICT_CALL_NODE2_MASK)
					arg_p2=rule_p->rule_strict_call_node2->node_arguments;
				else
					arg_p2=NULL;
				
				for_lla (arg_p1,lhs_arg_p,function_arg_state_p,
						rule_p->rule_strict_call_node->node_arguments,rule_p->rule_alts->alt_lhs_root->node_arguments,rule_p->rule_state_p,
						arg_next,arg_next)
				{
					if (function_arg_state_p->state_type==SimpleState && function_arg_state_p->state_kind==OnA){
						if (lhs_arg_p->arg_node->node_kind==NodeIdNode){
							StateP lhs_arg_state_p;
							NodeIdP lhs_node_id_p;
							
							lhs_node_id_p=lhs_arg_p->arg_node->node_node_id;
							lhs_arg_state_p=lhs_node_id_p->nid_lhs_state_p;
							
							if (lhs_arg_state_p->state_type==SimpleState && lhs_arg_state_p->state_kind==OnA){
								NodeP call_arg_node1,call_arg_node2;
								StateP call_arg_state_p1,call_arg_state_p2;
								
								call_arg_node1=arg_p1->arg_node;
								if (call_arg_node1->node_kind!=NodeIdNode){
									call_arg_state_p1=&call_arg_node1->node_state;	
								} else {
									struct node_id *node_id;
						
									node_id=call_arg_node1->node_node_id;
									if (node_id->nid_refcount<0){
										if (node_id==lhs_node_id_p)
											call_arg_state_p1=NULL;
										else
											call_arg_state_p1=node_id->nid_lhs_state_p;
									} else
										call_arg_state_p1=&node_id->nid_node->node_state;
								}
								
								if (call_arg_state_p1==NULL || !IsLazyState (*call_arg_state_p1)){
									if (arg_p2!=NULL){
										call_arg_node2=arg_p2->arg_node;
										if (call_arg_node2->node_kind!=NodeIdNode){
											call_arg_state_p2=&call_arg_node2->node_state;	
										} else {
											struct node_id *node_id;
								
											node_id=call_arg_node2->node_node_id;
											if (node_id->nid_refcount<0){
												if (node_id==lhs_node_id_p)
													call_arg_state_p2=NULL;
												else
													call_arg_state_p2=node_id->nid_lhs_state_p;
											} else
												call_arg_state_p2=&node_id->nid_node->node_state;
										}						
									} else
										call_arg_state_p2=NULL;
									
									if (call_arg_state_p1!=NULL || call_arg_state_p2!=NULL){
										if (call_arg_state_p2==NULL || !IsLazyState (*call_arg_state_p2)){
											if ((call_arg_state_p1==NULL || 
												 (call_arg_state_p1->state_type==ArrayState ||
												  (call_arg_state_p1->state_type==SimpleState && call_arg_state_p1->state_kind==OnB))) && 
												(call_arg_state_p2==NULL || 
												 (call_arg_state_p2->state_type==ArrayState ||
												  (call_arg_state_p2->state_type==SimpleState && call_arg_state_p2->state_kind==OnB))))
											{
												StateP new_call_state_p;
												
												if (call_arg_state_p1!=NULL)
													new_call_state_p = call_arg_state_p1;
												else
													new_call_state_p = call_arg_state_p2;
												
												*lhs_arg_state_p = *new_call_state_p;
												
												*function_arg_state_p = *new_call_state_p;
												
												arg_p1->arg_state = *new_call_state_p;
												
												if (call_arg_node1->node_kind==NodeIdNode && 
													call_arg_node1->node_node_id->nid_refcount==1 &&
													call_arg_node1->node_node_id->nid_node->node_kind==NodeIdNode)
												{
													call_arg_node1->node_node_id->nid_node->node_arguments->arg_state = *new_call_state_p;
												}
												
												if (arg_p2!=NULL){
													arg_p2->arg_state = *new_call_state_p;
													
													if (call_arg_node2->node_kind==NodeIdNode && 
														call_arg_node2->node_node_id->nid_refcount==1 &&
														call_arg_node2->node_node_id->nid_node->node_kind==NodeIdNode)
													{
														StateP state_p;
														
														state_p=&call_arg_node2->node_node_id->nid_node->node_arguments->arg_state;
														*state_p = *new_call_state_p;
													}
												}
											} else {
												lhs_arg_state_p->state_kind=StrictOnA;
												function_arg_state_p->state_kind=StrictOnA;
											}
										}
									}
								}
							}
						}
					}			
					if (arg_p2!=NULL)
						arg_p2=arg_p2->arg_next;
				}
			}
		}	
	}
#endif
}
