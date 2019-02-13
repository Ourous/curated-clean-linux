/*
	File:	pattern_match.c
	Author:	John van Groningen
*/

#define DEBUG_OUTPUT 0

#if defined (applec) || defined (__MWERKS__) || defined (__MRC__)
# define __ppc__
#endif

#include <stdio.h>

#include "compiledefines.h"
#include "types.t"
#include "syntaxtr.t"
#include "pattern_match.h"
#include "buildtree.h"
#include "comsupport.h"
#include "statesgen.h"
#include "settings.h"
#include "codegen_types.h"

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)

static void error_in_function (char *m)
{
	ErrorInCompiler ("",m,"");
}

#if DEBUG_OUTPUT
char *node_id_name (NodeId node_id)
{
	static char node_id_name_s[65];
		
	if (node_id->nid_ident!=NULL && node_id->nid_ident->ident_name!=NULL)
		return node_id->nid_ident->ident_name;
	else {
		sprintf (node_id_name_s,"i_%lx",(long)node_id);
		return node_id_name_s;
	}
}
#endif

static NodeP new_switch_node (NodeIdP node_id,NodeP case_node,StateP state_p,NodeS ***root_l)
{
	NodeP switch_node;
	
	switch_node=CompAllocType (NodeS);
	
	switch_node->node_kind=SwitchNode;
	switch_node->node_node_id=node_id;
	switch_node->node_arity=1;
	switch_node->node_arguments=NewArgument (case_node);
	switch_node->node_state=*state_p;
	
#if DEBUG_OUTPUT
	printf ("dec %s %d\n",node_id_name (node_id),node_id->nid_refcount);
#endif

	--node_id->nid_refcount;
	
	**root_l=switch_node;
	*root_l=&case_node->node_arguments->arg_node;

	return switch_node;
}

static NodeP new_case_node (SymbolP symbol,int symbol_arity,NodeP node,NodeDefP **def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
							,StrictNodeIdP **strict_node_ids_l
#endif
							)
{
	NodeP case_node;
	
	case_node=CompAllocType (NodeS);
	
	case_node->node_kind=CaseNode;
	case_node->node_symbol=symbol;
	case_node->node_arity=symbol_arity;
	case_node->node_arguments=NewArgument (node);

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	case_node->node_su.su_u.u_case=CompAllocType (CaseNodeContentsS);
	case_node->node_strict_node_ids=NULL;
#endif

	case_node->node_node_id_ref_counts=NULL;

	case_node->node_node_defs=**def_l;
	**def_l=NULL;
	*def_l=&case_node->node_node_defs;

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	case_node->node_strict_node_ids=**strict_node_ids_l;
	**strict_node_ids_l=NULL;
	*strict_node_ids_l=&case_node->node_strict_node_ids;
#endif

	return case_node;
}

struct root_and_defs_l {
	NodeP **		root_l;
	NodeDefP **		def_l;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	StrictNodeIdP **strict_node_ids_l;
	NodeDefP **		end_lhs_defs_l;
#endif
};

struct root_and_defs {
	NodeP			root;
	NodeDefP		defs;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	StrictNodeIdP	strict_node_ids;
#endif
};

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
static void add_new_lhs_node_id_to_lhs_node_defs (NodeIdP node_id_p,struct root_and_defs_l *root_and_defs_lp)
{
	NodeDefP new_node_def_p;
	
	new_node_def_p=CompAllocType (NodeDefS);
	
	new_node_def_p->def_id=node_id_p;
	new_node_def_p->def_mark=0;
	new_node_def_p->def_node=node_id_p->nid_node;
	
	**root_and_defs_lp->end_lhs_defs_l=new_node_def_p;
	*root_and_defs_lp->end_lhs_defs_l=&new_node_def_p->def_next;
}
#endif

static NodeP new_switch_and_case_node (NodeIdP node_id,StateP state_p,SymbolP symbol,int symbol_arity,struct root_and_defs_l *root_and_defs_lp)
{
	NodeP case_node_p;
	
	case_node_p=new_case_node (symbol,symbol_arity,**root_and_defs_lp->root_l,root_and_defs_lp->def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
								,root_and_defs_lp->strict_node_ids_l
#endif	
								);
	return new_switch_node (node_id,case_node_p,state_p,root_and_defs_lp->root_l);
}

static NodeP new_default_node (NodeP node,NodeDefP node_defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
								,StrictNodeIdP strict_node_ids
#endif
								)
{
	NodeP default_node;
	
	default_node=CompAllocType (NodeS);
	
	default_node->node_kind=DefaultNode;
	default_node->node_node_defs=node_defs;
	default_node->node_arity=1;
	default_node->node_arguments=NewArgument (node);

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	default_node->node_su.su_u.u_case=CompAllocType (CaseNodeContentsS);
	default_node->node_strict_node_ids=strict_node_ids;
#endif

	default_node->node_node_id_ref_counts=NULL;
	
	return default_node;
}

static NodeP new_push_node (Symbol symbol,int arity,ArgP arguments)
{
	NodeP push_node;
	
	push_node=CompAllocType (NodeS);
	
	push_node->node_kind=PushNode;
	push_node->node_arity=arity;
	push_node->node_arguments=arguments;
	push_node->node_record_symbol=symbol;
	push_node->node_number=0;	/* if !=0 then unique */
	
	return push_node;
}

static NodeP new_guard_node (NodeP if_node,NodeP node,NodeDefP node_defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
							,StrictNodeIdP strict_node_ids
#endif						
							)
{
	NodeP guard_node;
	ArgP arg1,arg2;
	
	guard_node=CompAllocType (NodeS);
	
	guard_node->node_kind=GuardNode;
	guard_node->node_node_defs=node_defs;
	guard_node->node_arity=2;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	guard_node->node_guard_strict_node_ids=strict_node_ids;
#endif	

	arg1=NewArgument (if_node);
	arg2=NewArgument (node);

	guard_node->node_arguments=arg1;
	arg1->arg_next=arg2;
	
	return guard_node;
}

static void transform_normal_pattern_node (NodeP node,StateP state_p,NodeIdP node_id,struct root_and_defs_l *root_and_defs_lp);

static void transform_pattern_arguments (SymbolP symbol,ArgP arguments,int arity,NodeIdP node_id,struct root_and_defs_l *root_and_defs_lp)
{
	NodeP push_node;
	NodeIdListElementP *last_node_id_p;
	ArgP arg,arg1,arg2;

	arg2=NewArgument (**root_and_defs_lp->root_l);
	arg1=NewArgument (NULL);
	arg1->arg_next=arg2;

	push_node=new_push_node (symbol,arity,arg1);

	**root_and_defs_lp->root_l=push_node;
	*root_and_defs_lp->root_l=&arg2->arg_node;
	
	last_node_id_p=&push_node->node_node_ids;
	
	for_l (arg,arguments,arg_next){
		NodeIdP argument_node_id;
		NodeP node;
		
		node=arg->arg_node;
		if (node->node_kind==NormalNode){
			argument_node_id=NewNodeId (NULL);
			argument_node_id->nid_refcount=-1;

			argument_node_id->nid_lhs_state_p_=&arg->arg_state;
			
			transform_normal_pattern_node (node,&arg->arg_state,argument_node_id,root_and_defs_lp);
		} else {
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			NodeP argument_node_id_node;
			
			argument_node_id=node->node_node_id;
			
			argument_node_id->nid_lhs_state_p_=&arg->arg_state;
			
			argument_node_id_node=argument_node_id->nid_node;
			if (argument_node_id_node){
				argument_node_id->nid_node=NULL;
				transform_normal_pattern_node (argument_node_id_node,&arg->arg_state,argument_node_id,root_and_defs_lp);
			}
#else			
			argument_node_id=node->node_node_id;
			if (argument_node_id->nid_node)
				transform_normal_pattern_node (argument_node_id->nid_node,&arg->arg_state,argument_node_id,root_and_defs_lp);				
#endif
		}

#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		argument_node_id->nid_state_=arg->arg_state;
#endif

		*last_node_id_p=CompAllocType (NodeIdListElementS);
		(*last_node_id_p)->nidl_node_id=argument_node_id;
		last_node_id_p=&(*last_node_id_p)->nidl_next;
	}

	*last_node_id_p=NULL;

	arg1->arg_node=NewNodeIdNode (node_id);
}

static void transform_normal_pattern_node (NodeP node,StateP state_p,NodeIdP node_id,struct root_and_defs_l *root_and_defs_lp)
{
	SymbolP symbol;
	NodeP switch_node,case_node;
	NodeP **root_l;
	NodeDefP **def_l;
	
	symbol=node->node_symbol;
	root_l=root_and_defs_lp->root_l;
	def_l=root_and_defs_lp->def_l;
	
	switch (symbol->symb_kind){
		case definition:
			case_node=new_case_node (symbol,node->node_arity,**root_l,def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
									,root_and_defs_lp->strict_node_ids_l
#endif	
									);
			switch_node=new_switch_node (node_id,case_node,state_p,root_l);

			if (node->node_arity>0)
				transform_pattern_arguments (symbol,node->node_arguments,node->node_arity,node_id,root_and_defs_lp);

			return;
		case cons_symb:
			case_node=new_case_node (symbol,2,**root_l,def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
									,root_and_defs_lp->strict_node_ids_l
#endif	
						);
			switch_node=new_switch_node (node_id,case_node,state_p,root_l);
			transform_pattern_arguments (symbol,node->node_arguments,2,node_id,root_and_defs_lp);
			return;
		case nil_symb:
			case_node=new_case_node (symbol,0,**root_l,def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
									,root_and_defs_lp->strict_node_ids_l
#endif	
									);
			switch_node=new_switch_node (node_id,case_node,state_p,root_l);
			return;
		case tuple_symb:
			case_node=new_case_node (symbol,node->node_arity,**root_l,def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
									,root_and_defs_lp->strict_node_ids_l
#endif	
									);
			switch_node=new_switch_node (node_id,case_node,state_p,root_l);
			transform_pattern_arguments (symbol,node->node_arguments,node->node_arity,node_id,root_and_defs_lp);
			return;			
		case apply_symb:
		case if_symb:
			error_in_function ("transform_normal_pattern_node");
			return;
		case string_denot:
			case_node=new_case_node (symbol,0,**root_l,def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
									,root_and_defs_lp->strict_node_ids_l
#endif	
									);
			switch_node=new_switch_node (node_id,case_node,state_p,root_l);
			return;
		default:
			if (symbol->symb_kind < Nr_Of_Basic_Types)
				error_in_function ("transform_normal_pattern_node");
			else {
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				if (state_p->state_object==BasicSymbolStates [symbol->symb_kind].state_object){
#endif
					case_node=new_case_node (symbol,0,**root_l,def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
											,root_and_defs_lp->strict_node_ids_l
#endif	
										);
					switch_node=new_switch_node (node_id,case_node,state_p,root_l);
					return;
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				} else if (state_p->state_object==UnknownObj
# if ABSTRACT_OBJECT
						|| state_p->state_object==AbstractObj
# endif
				){
					case_node=new_case_node (symbol,0,**root_l,def_l
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
										,root_and_defs_lp->strict_node_ids_l
#endif	
					);
					switch_node=new_switch_node (node_id,case_node,state_p,root_l);
					return;
				} else
					error_in_function ("transform_normal_pattern_node");
#endif
			}
	}
}

NodeIdRefCountListP new_node_id_ref_count (NodeIdRefCountListP node_id_ref_count_list,NodeIdP node_id,int ref_count)
{
	NodeIdRefCountListP new_node_id_ref_count_elem;

	new_node_id_ref_count_elem=CompAllocType (NodeIdRefCountListS);

	new_node_id_ref_count_elem->nrcl_next=node_id_ref_count_list;
	new_node_id_ref_count_elem->nrcl_node_id=node_id;
	new_node_id_ref_count_elem->nrcl_ref_count=ref_count;

	return new_node_id_ref_count_elem;
}

static NodeIdRefCountListP *insert_new_node_id_ref_count (NodeIdRefCountListP *node_id_ref_count_p,NodeIdP node_id,int ref_count)
{
	NodeIdRefCountListP node_id_ref_count_elem;
	
	node_id_ref_count_elem=new_node_id_ref_count (NULL,node_id,ref_count);
	*node_id_ref_count_p=node_id_ref_count_elem;
	
	return &node_id_ref_count_elem->nrcl_next;
}

static void remove_aliases_from_node_and_node_definitions (NodeP node_p,NodeDefP node_defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
															,StrictNodeIdP strict_node_ids
#endif
															);

static void remove_aliases_from_node (NodeP node)
{
	switch (node->node_kind){
		case NodeIdNode:
		{
			NodeIdP node_id;
			
			node_id=node->node_node_id;
			if (node_id->nid_mark & NID_ALIAS_MASK)
				node->node_node_id=node_id->nid_forward_node_id;

			return;
		}
		case NormalNode:
		case UpdateNode:
		{
			ArgP arg;
			
			for_l (arg,node->node_arguments,arg_next)
				remove_aliases_from_node (arg->arg_node);

			return;
		}
		case SelectorNode:
		case MatchNode:
			remove_aliases_from_node (node->node_arguments->arg_node);
			return;
		case IfNode:
		{
			ArgP cond_arg,then_arg;

			cond_arg=node->node_arguments;
			then_arg=cond_arg->arg_next;

			remove_aliases_from_node_and_node_definitions (then_arg->arg_node,node->node_then_node_defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
															,node->node_then_strict_node_ids
#endif
															);
			remove_aliases_from_node_and_node_definitions (then_arg->arg_next->arg_node,node->node_else_node_defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
															,node->node_else_strict_node_ids
#endif
															);

			remove_aliases_from_node (cond_arg->arg_node);

			return;
		}
		case PushNode:
		{
			NodeIdP node_id;
			ArgP arguments;

			arguments=node->node_arguments;
			
			node_id=arguments->arg_node->node_node_id;
			if (node_id->nid_mark & NID_ALIAS_MASK)
				arguments->arg_node->node_node_id=node_id->nid_forward_node_id;

			remove_aliases_from_node (arguments->arg_next->arg_node);
			return;
		}
		case SwitchNode:
		{
			NodeIdP node_id;

			node_id=node->node_node_id;
			if (node_id->nid_mark & NID_ALIAS_MASK)
				node->node_node_id=node_id->nid_forward_node_id;

			remove_aliases_from_node (node->node_arguments->arg_node);
			return;
		}
		case CaseNode:
		case DefaultNode:
			remove_aliases_from_node_and_node_definitions (node->node_arguments->arg_node,node->node_node_defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
															,node->node_strict_node_ids
#endif
															);
			return;
		case TupleSelectorsNode:
			remove_aliases_from_node (node->node_node);
			return;
		default:
			error_in_function ("remove_aliases_from_node");
	}
}

static void remove_aliases_from_node_and_node_definitions (NodeP node_p,NodeDefP node_defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
															,StrictNodeIdP strict_node_ids
#endif
														)
{
	NodeDefP node_def;

	remove_aliases_from_node (node_p);
	
	for_l (node_def,node_defs,def_next)
		if (node_def->def_node)
			remove_aliases_from_node (node_def->def_node);
	
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	{
	StrictNodeIdP strict_node_id_p;
	
	for_l (strict_node_id_p,strict_node_ids,snid_next){
		NodeIdP node_id;
			
		node_id=strict_node_id_p->snid_node_id;
		if (node_id->nid_mark & NID_ALIAS_MASK)
			strict_node_id_p->snid_node_id=node_id->nid_forward_node_id;
	}
	}
#endif
}

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
static void transform_tuple_or_record_pattern_node (NodeP node,StateP state_p,NodeIdP node_id,struct root_and_defs_l *root_and_defs_lp);

static void transform_tuple_or_record_pattern_arguments (SymbolP symbol,ArgP arguments,int arity,NodeIdP node_id,struct root_and_defs_l *root_and_defs_lp)
{
	NodeP push_node;
	NodeIdListElementP *last_node_id_p;
	ArgP arg,arg1,arg2;

	arg2=NewArgument (**root_and_defs_lp->root_l);
	arg1=NewArgument (NULL);
	arg1->arg_next=arg2;

	push_node=new_push_node (symbol,arity,arg1);

	**root_and_defs_lp->root_l=push_node;
	*root_and_defs_lp->root_l=&arg2->arg_node;
	
	last_node_id_p=&push_node->node_node_ids;
	
	for_l (arg,arguments,arg_next){
		NodeIdP argument_node_id;
		NodeP node;
		
		node=arg->arg_node;
		if (node->node_kind==NormalNode){
			argument_node_id=NewNodeId (NULL);
			argument_node_id->nid_refcount=-1;

			argument_node_id->nid_lhs_state_p_=&arg->arg_state;

			arg->arg_node=NewNodeIdNode (argument_node_id);
			
			if (node->node_symbol->symb_kind==tuple_symb || (node->node_symbol->symb_kind==definition && node->node_symbol->symb_def->sdef_kind==RECORDTYPE)){
				argument_node_id->nid_node=node;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				add_new_lhs_node_id_to_lhs_node_defs (argument_node_id,root_and_defs_lp);
#endif
				transform_tuple_or_record_pattern_node (node,&arg->arg_state,argument_node_id,root_and_defs_lp);
			} else
				transform_normal_pattern_node (node,&arg->arg_state,argument_node_id,root_and_defs_lp);
		} else {
			argument_node_id=node->node_node_id;
			
			if (argument_node_id->nid_node){
				SymbolP argument_node_id_node_symbol;
				
				argument_node_id_node_symbol=argument_node_id->nid_node->node_symbol;
				if (argument_node_id_node_symbol->symb_kind==tuple_symb || (argument_node_id_node_symbol->symb_kind==definition && argument_node_id_node_symbol->symb_def->sdef_kind==RECORDTYPE)){
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					add_new_lhs_node_id_to_lhs_node_defs (argument_node_id,root_and_defs_lp);
#endif
					transform_tuple_or_record_pattern_node (argument_node_id->nid_node,&arg->arg_state,argument_node_id,root_and_defs_lp);
				} else {
					NodeP argument_node_id_node;

					argument_node_id_node=argument_node_id->nid_node;
					argument_node_id->nid_node=NULL;
					transform_normal_pattern_node (argument_node_id_node,&arg->arg_state,argument_node_id,root_and_defs_lp);
				}
			}
		}

		argument_node_id->nid_state_=arg->arg_state;

		*last_node_id_p=CompAllocType (NodeIdListElementS);
		(*last_node_id_p)->nidl_node_id=argument_node_id;
		last_node_id_p=&(*last_node_id_p)->nidl_next;
	}

	*last_node_id_p=NULL;

	arg1->arg_node=NewNodeIdNode (node_id);
}

static void transform_tuple_or_record_pattern_node (NodeP node,StateP state_p,NodeIdP node_id,struct root_and_defs_l *root_and_defs_lp)
{
	NodeP switch_node;

	switch_node=new_switch_and_case_node (node_id,state_p,node->node_symbol,node->node_arity,root_and_defs_lp);
	transform_tuple_or_record_pattern_arguments (node->node_symbol,node->node_arguments,node->node_arity,node_id,root_and_defs_lp);
}

static void insert_push_node (SymbolP symbol,ArgP arguments,int arity,NodeIdP node_id,NodeP **root_l)
{
	NodeP push_node;
	NodeIdListElementP *last_node_id_p;
	ArgP arg,arg1,arg2;

	arg2=NewArgument (**root_l);
	arg1=NewArgument (NULL);
	arg1->arg_next=arg2;

	push_node=new_push_node (symbol,arity,arg1);

	**root_l=push_node;
	*root_l=&arg2->arg_node;
	
	last_node_id_p=&push_node->node_node_ids;
	
	for_l (arg,arguments,arg_next){
		NodeIdP argument_node_id;
		NodeP node;
		
		node=arg->arg_node;
		argument_node_id=node->node_node_id;
		
		argument_node_id->nid_state_=arg->arg_state;

		*last_node_id_p=CompAllocType (NodeIdListElementS);
		(*last_node_id_p)->nidl_node_id=argument_node_id;
		last_node_id_p=&(*last_node_id_p)->nidl_next;
	}

	*last_node_id_p=NULL;

	arg1->arg_node=NewNodeIdNode (node_id);
}
#endif

static void transform_argument (ArgP arg_p,struct root_and_defs_l *root_and_defs_lp)
{
	NodeP node;
	
	node=arg_p->arg_node;
	
	switch (node->node_kind){
		case NormalNode:
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			if (arg_p->arg_state.state_type==TupleState || arg_p->arg_state.state_type==RecordState){
				ArgP arg;
			
				for_l (arg,node->node_arguments,arg_next)
					transform_argument (arg,root_and_defs_lp);
			} else
#endif
			{
				NodeIdP node_id;
				
				node_id=NewNodeId (NULL);
				node_id->nid_refcount=-1;

				node_id->nid_lhs_state_p_=&arg_p->arg_state;

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				if (node->node_symbol->symb_kind==tuple_symb || 
					(node->node_symbol->symb_kind==definition && node->node_symbol->symb_def->sdef_kind==RECORDTYPE))
				{
					node_id->nid_node=node;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					add_new_lhs_node_id_to_lhs_node_defs (node_id,root_and_defs_lp);
#endif
					transform_tuple_or_record_pattern_node (node,&arg_p->arg_state,node_id,root_and_defs_lp);					
				} else
#endif
				transform_normal_pattern_node (node,&arg_p->arg_state,node_id,root_and_defs_lp);
				
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				node_id->nid_state_=arg_p->arg_state;
#endif
				arg_p->arg_node=NewNodeIdNode (node_id);
			}
			break;
		case NodeIdNode:
		{
			NodeIdP node_id;
			
			node_id=node->node_node_id;
			
			if (node_id->nid_node!=NULL){
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				SymbolP node_id_nid_node_symbol;
				
				node_id_nid_node_symbol=node_id->nid_node->node_symbol;
				if (node_id_nid_node_symbol->symb_kind==tuple_symb ||
					(node_id_nid_node_symbol->symb_kind==definition && node_id_nid_node_symbol->symb_def->sdef_kind==RECORDTYPE))
				{
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					add_new_lhs_node_id_to_lhs_node_defs (node_id,root_and_defs_lp);
# endif
					transform_tuple_or_record_pattern_node (node_id->nid_node,&arg_p->arg_state,node_id,root_and_defs_lp);
					return;
				}
#else
				if (arg_p->arg_state.state_type==TupleState || arg_p->arg_state.state_type==RecordState){
					ArgP arg;
				
					for_l (arg,node_id->nid_node->node_arguments,arg_next)
						transform_argument (arg,root_and_defs_lp);
				} else
#endif
				{
					transform_normal_pattern_node (node_id->nid_node,&arg_p->arg_state,node_id,root_and_defs_lp);

					node_id->nid_node=NULL;
				}
			}
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			node_id->nid_lhs_state_p_=&arg_p->arg_state;
#else
			node_id->nid_state_=arg_p->arg_state;
#endif
			break;
		}
		default:
			error_in_function ("transform_argument");
	}
}

#if 0
# include "dbprint.h"
#endif

static void transform_and_merge_argument (ArgP arg_p,ArgP first_alt_arg_p,struct root_and_defs_l *root_and_defs_lp,
											NodeIdRefCountListP **node_id_ref_count_list_h)
{
	NodeP node;
	
	node=arg_p->arg_node;
	
	switch (node->node_kind){
		case NormalNode:
#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			if (arg_p->arg_state.state_type==TupleState || arg_p->arg_state.state_type==RecordState){
				ArgP tuple_arg_p,first_alt_tuple_arg_p;

				tuple_arg_p=node->node_arguments;

				if (first_alt_arg_p->arg_node->node_kind==NodeIdNode){
					NodeIdP first_alt_node_id;
				
					first_alt_node_id=first_alt_arg_p->arg_node->node_node_id;
#if 1 /* added 9-4-1999 */
					*node_id_ref_count_list_h = insert_new_node_id_ref_count (*node_id_ref_count_list_h,first_alt_node_id,-1);
#endif
					if (first_alt_node_id->nid_node==NULL){
						first_alt_node_id->nid_node=node;
						
						for (; tuple_arg_p!=NULL; tuple_arg_p=tuple_arg_p->arg_next)
							transform_argument (tuple_arg_p,root_and_defs_lp);
						
						return;
					} else
						first_alt_tuple_arg_p=first_alt_node_id->nid_node->node_arguments;
				} else
					first_alt_tuple_arg_p=first_alt_arg_p->arg_node->node_arguments;
				
				for (; tuple_arg_p!=NULL; tuple_arg_p=tuple_arg_p->arg_next,first_alt_tuple_arg_p=first_alt_tuple_arg_p->arg_next)
					transform_and_merge_argument (tuple_arg_p,first_alt_tuple_arg_p,root_and_defs_lp,node_id_ref_count_list_h);
			} else
#endif
			{
				NodeIdP first_alt_node_id;

				first_alt_node_id=first_alt_arg_p->arg_node->node_node_id;
				
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				if (node->node_symbol->symb_kind==tuple_symb || 
					(node->node_symbol->symb_kind==definition && node->node_symbol->symb_def->sdef_kind==RECORDTYPE))
				{
					if (first_alt_node_id->nid_node!=NULL){
						ArgP tuple_arg_p,first_alt_tuple_arg_p;
						NodeP switch_node;

						switch_node=new_switch_and_case_node (first_alt_node_id,&arg_p->arg_state,node->node_symbol,node->node_arity,root_and_defs_lp);

						tuple_arg_p=node->node_arguments;
						first_alt_tuple_arg_p=first_alt_node_id->nid_node->node_arguments;

						insert_push_node (node->node_symbol,first_alt_tuple_arg_p,node->node_arity,first_alt_node_id,root_and_defs_lp->root_l);
										
						for (; tuple_arg_p!=NULL; tuple_arg_p=tuple_arg_p->arg_next,first_alt_tuple_arg_p=first_alt_tuple_arg_p->arg_next)
							transform_and_merge_argument (tuple_arg_p,first_alt_tuple_arg_p,root_and_defs_lp,node_id_ref_count_list_h);
					} else {
						first_alt_node_id->nid_node=node;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
						add_new_lhs_node_id_to_lhs_node_defs (first_alt_node_id,root_and_defs_lp);
#endif
						transform_tuple_or_record_pattern_node (node,&arg_p->arg_state,first_alt_node_id,root_and_defs_lp);					
					}
				} else	
#endif
				{
				transform_normal_pattern_node (node,&arg_p->arg_state,first_alt_node_id,root_and_defs_lp);
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
				first_alt_node_id->nid_node=NULL;
#endif
				}
				/* JVG: added 4-10-95 */
				++first_alt_node_id->nid_refcount;
				/* */
				/* JVG: added 4 april 95 */
				*node_id_ref_count_list_h = insert_new_node_id_ref_count (*node_id_ref_count_list_h,first_alt_node_id,-2);
				/* */
			}
			return;
		case NodeIdNode:
		{
			NodeId node_id;
			
			node_id=node->node_node_id;

#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			if (arg_p->arg_state.state_type==TupleState || arg_p->arg_state.state_type==RecordState){
				if (node_id->nid_node==NULL){
					if (first_alt_arg_p->arg_node->node_kind==NodeIdNode){
						NodeId first_alt_node_id;
						
						first_alt_node_id=first_alt_arg_p->arg_node->node_node_id;
# if 1 /* added 8-4-1999 */
						*node_id_ref_count_list_h = insert_new_node_id_ref_count (*node_id_ref_count_list_h,first_alt_node_id,node_id->nid_refcount);
# else
						first_alt_node_id->nid_refcount += node_id->nid_refcount+1;
# endif					
						node_id->nid_mark |= NID_ALIAS_MASK;
						node_id->nid_forward_node_id_=first_alt_node_id;
					} else {
						NodeP node;
						
						node=first_alt_arg_p->arg_node;
						node_id->nid_node=node;
						first_alt_arg_p->arg_node=arg_p->arg_node;

# ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
						node_id->nid_state_=arg_p->arg_state;
# endif
					}
				} else {
					ArgP tuple_arg_p,first_alt_tuple_arg_p;
					
					if (first_alt_arg_p->arg_node->node_kind==NodeIdNode){
						NodeIdP first_alt_node_id;

						first_alt_node_id=first_alt_arg_p->arg_node->node_node_id;
# if 1 /* added 20-4-1999 */
						*node_id_ref_count_list_h = insert_new_node_id_ref_count (*node_id_ref_count_list_h,first_alt_node_id,node_id->nid_refcount);
# else
						first_alt_node_id->nid_refcount += node_id->nid_refcount+1;
# endif					
						node_id->nid_mark |= NID_ALIAS_MASK;
						node_id->nid_forward_node_id_=first_alt_node_id;
									
						if (first_alt_node_id->nid_node==NULL){
							ArgP tuple_arg_p;
							
							first_alt_node_id->nid_node=node_id->nid_node;

							tuple_arg_p=node_id->nid_node->node_arguments;
						 	
							for (; tuple_arg_p!=NULL; tuple_arg_p=tuple_arg_p->arg_next)
								transform_argument (tuple_arg_p,root_and_defs_lp);
														
							return;						
						} else {
							tuple_arg_p=node_id->nid_node->node_arguments;
							first_alt_tuple_arg_p=first_alt_node_id->nid_node->node_arguments;
						}
					} else {
						tuple_arg_p=node_id->nid_node->node_arguments;
						first_alt_tuple_arg_p=first_alt_arg_p->arg_node->node_arguments;

						node_id->nid_node=first_alt_arg_p->arg_node;
						first_alt_arg_p->arg_node=arg_p->arg_node;
# ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
						node_id->nid_state_=arg_p->arg_state;
# endif
					}

					for (; tuple_arg_p!=NULL; tuple_arg_p=tuple_arg_p->arg_next,first_alt_tuple_arg_p=first_alt_tuple_arg_p->arg_next)
						transform_and_merge_argument (tuple_arg_p,first_alt_tuple_arg_p,root_and_defs_lp,node_id_ref_count_list_h);

					return;
				}
			} else
#endif
			{
				NodeIdP first_alt_node_id;

				first_alt_node_id=first_alt_arg_p->arg_node->node_node_id;

				node_id->nid_mark |= NID_ALIAS_MASK;
				node_id->nid_forward_node_id_=first_alt_node_id;

				if (node_id->nid_node!=NULL){
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					SymbolP node_id_nid_node_symbol;
				
					node_id_nid_node_symbol=node_id->nid_node->node_symbol;

					if (node_id_nid_node_symbol->symb_kind==tuple_symb ||
						(node_id_nid_node_symbol->symb_kind==definition && node_id_nid_node_symbol->symb_def->sdef_kind==RECORDTYPE))
					{
						if (first_alt_node_id->nid_node!=NULL){
							ArgP tuple_arg_p,first_alt_tuple_arg_p;
							NodeP switch_node;

							switch_node=new_switch_and_case_node (first_alt_node_id,&arg_p->arg_state,first_alt_node_id->nid_node->node_symbol,first_alt_node_id->nid_node->node_arity,root_and_defs_lp);

							tuple_arg_p=node_id->nid_node->node_arguments;
							first_alt_tuple_arg_p=first_alt_node_id->nid_node->node_arguments;

							insert_push_node (first_alt_node_id->nid_node->node_symbol,first_alt_tuple_arg_p,first_alt_node_id->nid_node->node_arity,first_alt_node_id,root_and_defs_lp->root_l);
					
							for (; tuple_arg_p!=NULL; tuple_arg_p=tuple_arg_p->arg_next,first_alt_tuple_arg_p=first_alt_tuple_arg_p->arg_next)
								transform_and_merge_argument (tuple_arg_p,first_alt_tuple_arg_p,root_and_defs_lp,node_id_ref_count_list_h);
						} else {
							first_alt_node_id->nid_node=node_id->nid_node;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
							add_new_lhs_node_id_to_lhs_node_defs (first_alt_node_id,root_and_defs_lp);
#endif						
							transform_tuple_or_record_pattern_node (node_id->nid_node,&arg_p->arg_state,first_alt_node_id,root_and_defs_lp);					
						}
						++first_alt_node_id->nid_refcount;
						*node_id_ref_count_list_h = insert_new_node_id_ref_count (*node_id_ref_count_list_h,first_alt_node_id,node_id->nid_refcount-1);
						return;
					} else
#endif
					{
					transform_normal_pattern_node (node_id->nid_node,&arg_p->arg_state,node_id,root_and_defs_lp);
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					first_alt_node_id->nid_node=NULL;
#endif
					}
				}

				*node_id_ref_count_list_h = insert_new_node_id_ref_count (*node_id_ref_count_list_h,first_alt_node_id,node_id->nid_refcount);
			}
			return;
		}
		default:
			error_in_function ("transform_and_merge_argument");
	}
}

static NodeIdRefCountListP copy_node_id_ref_count_list (NodeIdRefCountListP node_id_ref_count_list)
{
	NodeIdRefCountListP node_id_ref_count_elem,new_node_id_ref_count_list,*new_node_id_ref_count_list_p;

	new_node_id_ref_count_list_p=&new_node_id_ref_count_list;

	for_l (node_id_ref_count_elem,node_id_ref_count_list,nrcl_next){
		NodeIdRefCountListP new_node_id_ref_count_elem;
		NodeIdP node_id;

		node_id=node_id_ref_count_elem->nrcl_node_id;

		new_node_id_ref_count_elem=new_node_id_ref_count (NULL,node_id,node_id->nid_refcount);

		*new_node_id_ref_count_list_p=new_node_id_ref_count_elem;
		new_node_id_ref_count_list_p=&new_node_id_ref_count_elem->nrcl_next;

#if DEBUG_OUTPUT
		printf ("copy %s %d %d ",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count);
#endif
		node_id->nid_refcount += node_id_ref_count_elem->nrcl_ref_count+1;
	}

#if DEBUG_OUTPUT
	printf ("\n");
#endif

	*new_node_id_ref_count_list_p=NULL;

	return new_node_id_ref_count_list;
}

static void increment_ref_counts_of_node_id_ref_count_list (NodeIdRefCountListP node_id_ref_count_list)
{
	NodeIdRefCountListP node_id_ref_count_elem;

	for_l (node_id_ref_count_elem,node_id_ref_count_list,nrcl_next){
#if DEBUG_OUTPUT
		{
			NodeIdP node_id;

			node_id=node_id_ref_count_elem->nrcl_node_id;
	
			printf ("increment %s %d %d ",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count);
		}
#endif
		node_id_ref_count_elem->nrcl_node_id->nid_refcount += node_id_ref_count_elem->nrcl_ref_count+1;
	}

#if DEBUG_OUTPUT
	printf ("\n");
#endif
}

static void replace_global_ref_count_by_local_ref_count (NodeIdRefCountListP node_id_ref_count_list)
{
	NodeIdRefCountListP node_id_ref_count_elem;

	for_l (node_id_ref_count_elem,node_id_ref_count_list,nrcl_next){
		int local_ref_count;
		NodeIdP node_id;

		node_id=node_id_ref_count_elem->nrcl_node_id;
		local_ref_count=node_id_ref_count_elem->nrcl_ref_count;

#if DEBUG_OUTPUT
		printf ("global_to_local %s %d %d ",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count);
#endif

		node_id_ref_count_elem->nrcl_ref_count=node_id->nid_refcount - local_ref_count;
		node_id->nid_refcount = local_ref_count;
	}

#if DEBUG_OUTPUT
	printf ("\n");
#endif
}

void set_local_reference_counts (NodeP case_node)
{
	replace_global_ref_count_by_local_ref_count (case_node->node_node_id_ref_counts);
}

static void replace_local_ref_count_by_global_ref_count (NodeIdRefCountListP node_id_ref_count_list)
{
	NodeIdRefCountListP node_id_ref_count_elem;

	for_l (node_id_ref_count_elem,node_id_ref_count_list,nrcl_next){
		int local_ref_count;
		NodeIdP node_id;

		node_id=node_id_ref_count_elem->nrcl_node_id;
		local_ref_count=node_id->nid_refcount;

#if DEBUG_OUTPUT
		printf ("local_to_global %s %d %d ",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count);
#endif

		node_id->nid_refcount = local_ref_count + node_id_ref_count_elem->nrcl_ref_count;
		node_id_ref_count_elem->nrcl_ref_count=local_ref_count;
	}

#if DEBUG_OUTPUT
	printf ("\n");
#endif
}

void set_global_reference_counts (NodeP case_node)
{
	replace_local_ref_count_by_global_ref_count (case_node->node_node_id_ref_counts);
}

static NodeP merge_alternative_with_node (NodeP root,struct root_and_defs *root_and_defs_p,NodeIdRefCountListP node_id_ref_count_list);

		static void decrement_reference_count_of_switch_node_id (NodeIdP root_node_id,NodeIdRefCountListP node_id_ref_count_list)
		{
			NodeIdRefCountListP node_id_ref_count_elem;
			
			for_l (node_id_ref_count_elem,node_id_ref_count_list,nrcl_next)
				if (node_id_ref_count_elem->nrcl_node_id==root_node_id){
#if DEBUG_OUTPUT
					printf ("inc %s %d %d\n",node_id_name (root_node_id),root_node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count);
#endif
					++node_id_ref_count_elem->nrcl_ref_count;
					break;
				}
			if (node_id_ref_count_elem==NULL){
				/* possibly less efficient code if this happens */
			}
		}

	static void merge_alternative_with_switch_node (NodeP root,struct root_and_defs *root_and_defs_p,NodeIdRefCountListP node_id_ref_count_list)
	{
		NodeP default_node,node;
		ArgP *arg_p,arg;
		
		node=root_and_defs_p->root;
		
		for (arg_p=&root->node_arguments; arg=*arg_p,arg!=NULL; arg_p=&arg->arg_next){
			NodeP case_node;
			
			case_node=arg->arg_node;
	
			switch (case_node->node_kind){
				case CaseNode:
					break;
				case DefaultNode:
					++root->node_node_id->nid_refcount;
					replace_global_ref_count_by_local_ref_count (case_node->node_node_id_ref_counts);

					case_node->node_arguments->arg_node
						= merge_alternative_with_node (case_node->node_arguments->arg_node,root_and_defs_p,node_id_ref_count_list);

					replace_local_ref_count_by_global_ref_count (case_node->node_node_id_ref_counts);
					--root->node_node_id->nid_refcount;
					return;
				default:
					error_in_function ("merge_alternative_with_switch_node");
			}
		}
		
		++root->node_node_id->nid_refcount;

		default_node=new_default_node (node,root_and_defs_p->defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
										,root_and_defs_p->strict_node_ids
#endif		
										);
		default_node->node_node_id_ref_counts= node_id_ref_count_list;

		if (root->node_arguments->arg_next==NULL) /* only one case or default ? */
			root->node_arguments->arg_node->node_node_id_ref_counts=copy_node_id_ref_count_list (node_id_ref_count_list);
		else
			increment_ref_counts_of_node_id_ref_count_list (node_id_ref_count_list);

		*arg_p=NewArgument (default_node);
	
		remove_aliases_from_node_and_node_definitions (root_and_defs_p->root,root_and_defs_p->defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
														,root_and_defs_p->strict_node_ids
#endif
														);

		--root->node_node_id->nid_refcount;
	}

	static void merge_switch_alternative_with_switch_node (NodeP root,struct root_and_defs *root_and_defs_p,NodeIdRefCountListP node_id_ref_count_list)
	{
		ArgP *arg_p,arg;
		NodeP case_node;

		for (arg_p=&root->node_arguments; arg=*arg_p,arg!=NULL; arg_p=&arg->arg_next){
			NodeP case_arg_node;
			
			case_arg_node=arg->arg_node;
	
			switch (case_arg_node->node_kind){
				case CaseNode:
					break;
				case DefaultNode:
					++root->node_node_id->nid_refcount;
					replace_global_ref_count_by_local_ref_count (case_arg_node->node_node_id_ref_counts);

					case_arg_node->node_arguments->arg_node= merge_alternative_with_node
						(case_arg_node->node_arguments->arg_node,root_and_defs_p,node_id_ref_count_list);

					replace_local_ref_count_by_global_ref_count (case_arg_node->node_node_id_ref_counts);
					--root->node_node_id->nid_refcount;
					return;
				default:
					error_in_function ("merge_switch_alternative_with_switch_node");
			}
		}

		case_node=root_and_defs_p->root->node_arguments->arg_node;

		for (arg_p=&root->node_arguments; arg=*arg_p,arg!=NULL; arg_p=&arg->arg_next){
			NodeP case_arg_node;
			
			case_arg_node=arg->arg_node;
						
			switch (case_arg_node->node_kind){
				case CaseNode:
				{
					struct root_and_defs case_root_and_defs;
					
					if (case_arg_node->node_arity!=case_node->node_arity)
						break;
					else {	
						if (case_arg_node->node_symbol==case_node->node_symbol){
							if (case_node->node_symbol->symb_kind==real_denot){
								merge_alternative_with_switch_node (root,root_and_defs_p,node_id_ref_count_list);
								return;
							}
						} else {
							int symbol_kind;
							
							symbol_kind=case_node->node_symbol->symb_kind;
							if (symbol_kind==int_denot || symbol_kind==char_denot || symbol_kind==string_denot){
								if (strcmp (case_arg_node->node_symbol->symb_int,case_node->node_symbol->symb_int)!=0)
									break;							
							} else
								break;
						}
					}
					
					decrement_reference_count_of_switch_node_id (root->node_node_id,node_id_ref_count_list);
					++root->node_node_id->nid_refcount;

					replace_global_ref_count_by_local_ref_count (case_arg_node->node_node_id_ref_counts);

					case_root_and_defs.root=case_node->node_arguments->arg_node;
					case_root_and_defs.defs=case_node->node_node_defs;
					
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					case_root_and_defs.strict_node_ids=case_node->node_strict_node_ids;
#endif
					case_arg_node->node_arguments->arg_node = merge_alternative_with_node
																(case_arg_node->node_arguments->arg_node,&case_root_and_defs,node_id_ref_count_list);
					case_node->node_node_defs=NULL;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					case_node->node_strict_node_ids=NULL;
#endif

					replace_local_ref_count_by_global_ref_count (case_arg_node->node_node_id_ref_counts);
					--root->node_node_id->nid_refcount;

					return;
				}
			}
		}

		decrement_reference_count_of_switch_node_id (root->node_node_id,node_id_ref_count_list);
		++root->node_node_id->nid_refcount;

		case_node->node_node_id_ref_counts= node_id_ref_count_list;

		if (root->node_arguments->arg_next==NULL) /* only one case or default ? */
			root->node_arguments->arg_node->node_node_id_ref_counts=copy_node_id_ref_count_list (node_id_ref_count_list);
		else
			increment_ref_counts_of_node_id_ref_count_list (node_id_ref_count_list);
	
		*arg_p=NewArgument (case_node);
		
		remove_aliases_from_node_and_node_definitions (case_node,root_and_defs_p->defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
														,root_and_defs_p->strict_node_ids
#endif
														);

		--root->node_node_id->nid_refcount;
	}

#define SEARCH_SWITCH 1

static NodeP merge_alternative_with_node (NodeP root,struct root_and_defs *root_and_defs_p,NodeIdRefCountListP node_id_ref_count_list)
{
	NodeP node;
	
	node=root_and_defs_p->root;
	
	switch (root->node_kind){
		case SwitchNode:
		{
			NodeIdP node_id;
			struct root_and_defs root_and_defs;
						
			if (node->node_kind!=SwitchNode){
				merge_alternative_with_switch_node (root,root_and_defs_p,node_id_ref_count_list);
				return root;
			}

			root_and_defs=*root_and_defs_p;

			node_id=node->node_node_id;
			
			if (node_id->nid_mark & NID_ALIAS_MASK){
				node_id=node_id->nid_forward_node_id;
				node->node_node_id=node_id;
			}

#if SEARCH_SWITCH
			if (node_id!=root->node_node_id){
				NodeP next_switch_node,*next_switch_node_p,case_node;
				NodeIdP next_node_id;

				next_switch_node=node;
				do {
					case_node=next_switch_node->node_arguments->arg_node;
					next_switch_node_p=&case_node->node_arguments->arg_node;
					next_switch_node=*next_switch_node_p;
	
					if (next_switch_node->node_kind==PushNode){
						next_switch_node_p=&next_switch_node->node_arguments->arg_next->arg_node;
						next_switch_node=*next_switch_node_p;
					}

					if (next_switch_node->node_kind!=SwitchNode)
						break;

					next_node_id=next_switch_node->node_node_id;

					if (next_node_id->nid_mark & NID_ALIAS_MASK)
						next_node_id=next_node_id->nid_forward_node_id;
				} while (next_node_id!=root->node_node_id);
	
				if (next_switch_node->node_kind==SwitchNode && next_node_id==root->node_node_id){
					NodeP next_case_node,*node_p;
	
					next_case_node=next_switch_node->node_arguments->arg_node;
					node_p=&next_case_node->node_arguments->arg_node;
	
					if ((*node_p)->node_kind==PushNode)
						node_p=&(*node_p)->node_arguments->arg_next->arg_node;
					
					if (next_case_node->node_node_defs!=NULL){
						if (case_node->node_node_defs!=NULL)
							error_in_function ("merge_alternative_with_node");

						case_node->node_node_defs=next_case_node->node_node_defs;
						next_case_node->node_node_defs=NULL;
					}
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					if (next_case_node->node_strict_node_ids!=NULL){
						if (case_node->node_strict_node_ids!=NULL)
							error_in_function ("merge_alternative_with_node");

						case_node->node_strict_node_ids=next_case_node->node_strict_node_ids;
						next_case_node->node_strict_node_ids=NULL;
					}						
# endif

					*next_switch_node_p=*node_p;
					*node_p=node;
					node=next_switch_node;

					next_switch_node->node_node_id=next_node_id;
					node_id=next_node_id;
				}
			}
#endif
			
			root_and_defs.root=node;
			
			if (node_id==root->node_node_id)
				merge_switch_alternative_with_switch_node (root,&root_and_defs,node_id_ref_count_list);
			else
				merge_alternative_with_switch_node (root,&root_and_defs,node_id_ref_count_list);
			
			return root;
		}
		case PushNode:
		{
			ArgP node_arguments,root_arguments;
			NodeIdP node_id;
			NodeIdListElementP root_node_id_list,node_id_list;
			struct root_and_defs root_and_defs;
			
			root_and_defs=*root_and_defs_p;
			
			node_arguments=node->node_arguments;
			root_arguments=root->node_arguments;

			if (node->node_kind!=PushNode)
				error_in_function ("merge_alternative_with_node");
			
			node_id=node_arguments->arg_node->node_node_id;
			if (node_id->nid_mark & NID_ALIAS_MASK){
				node_id=node_id->nid_forward_node_id;
				node_arguments->arg_node->node_node_id=node_id;
			}
			
			if (root_arguments->arg_node->node_node_id!=node_id)
				error_in_function ("merge_alternative_with_node");

			root_node_id_list=root->node_node_ids;
			node_id_list=node->node_node_ids;
			
			while (root_node_id_list!=NULL){
				NodeIdP node_id,root_node_id;

				root_node_id=root_node_id_list->nidl_node_id;
				node_id=node_id_list->nidl_node_id;
				
				if (node_id!=root_node_id){
					node_id_ref_count_list=new_node_id_ref_count (node_id_ref_count_list,root_node_id,node_id->nid_refcount);

					node_id->nid_mark |= NID_ALIAS_MASK;
					node_id->nid_forward_node_id_=root_node_id;
				}

				root_node_id_list=root_node_id_list->nidl_next;
				node_id_list=node_id_list->nidl_next;
			}

			root_and_defs.root=node_arguments->arg_next->arg_node;

			root_arguments->arg_next->arg_node=merge_alternative_with_node
														(root_arguments->arg_next->arg_node,&root_and_defs,node_id_ref_count_list);

			return root;
		}
		case GuardNode:
			root->node_arguments->arg_next->arg_node=merge_alternative_with_node
													(root->node_arguments->arg_next->arg_node,root_and_defs_p,node_id_ref_count_list);

			return root;
		case IfNode:
		{
			NodeP else_node;
			
			else_node=root->node_arguments->arg_next->arg_next->arg_node;
			while (else_node->node_kind==IfNode)
				else_node=else_node->node_arguments->arg_next->arg_next->arg_node;
			
			if (else_node->node_kind==NormalNode && else_node->node_symbol->symb_kind==fail_symb){
				NodeP guard_node;

				increment_ref_counts_of_node_id_ref_count_list (node_id_ref_count_list);
				
				guard_node=new_guard_node (root,root_and_defs_p->root,root_and_defs_p->defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
											,root_and_defs_p->strict_node_ids
#endif
											);
				
				remove_aliases_from_node_and_node_definitions (node,root_and_defs_p->defs
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
																,root_and_defs_p->strict_node_ids
#endif
																);
				
				return guard_node;
			} else
				break;
		}
	}

	StaticMessage (False, "%S", "pattern will never match", CurrentSymbol);

	return root;
}

static void merge_node_id_ref_count_lists (NodeIdRefCountListP *list1_p,NodeIdRefCountListP list2)
{
	while (list2!=NULL){
		NodeIdP node_id;
		NodeIdRefCountListP next_list2,list1;

		node_id=list2->nrcl_node_id;

		while (list1=*list1_p,list1!=NULL && list1->nrcl_node_id<=node_id)
			list1_p=&list1->nrcl_next;

		if (list1==NULL){
			*list1_p=list2;
			return;
		}

		next_list2=list2->nrcl_next;

		*list1_p=list2;
		list2->nrcl_next=list1;
		list1_p=&list2->nrcl_next;

		list2=next_list2;
	}
}

static void sort_node_id_ref_count_lists (NodeIdRefCountListP *list_p)
{
	NodeIdRefCountListP element1,element2,element3;

	element1=*list_p;
	if (element1==NULL)
		return;

	element2=element1->nrcl_next;
	if (element2==NULL)
		return;

	element3=element2->nrcl_next;
	if (element3==NULL){
		if (element1->nrcl_node_id<=element2->nrcl_node_id)
			return;
		
		*list_p=element2;
		element2->nrcl_next=element1;
		element1->nrcl_next=NULL;
	} else {
		NodeIdRefCountListP list2,end_list1,end_list2;
		
		list2=element2;
		end_list1=element1;
		end_list2=element2;

		element1=element3;
		do {
			end_list1->nrcl_next=element1;
			end_list1=element1;

			element2=element1->nrcl_next;
			if (element2==NULL)
				break;

			end_list2->nrcl_next=element2;
			end_list2=element2;

			element1=element2->nrcl_next;
		} while (element1!=NULL);

		end_list1->nrcl_next=NULL;
		end_list2->nrcl_next=NULL;
		
		sort_node_id_ref_count_lists (list_p);
		sort_node_id_ref_count_lists (&list2);
		
		merge_node_id_ref_count_lists (list_p,list2);
	}
}

static void add_sorted_node_id_ref_count_list (NodeIdRefCountListP *node_id_ref_count_list1_p,NodeIdRefCountListP node_id_ref_count_list2)
{
	NodeIdRefCountListP node_id_ref_count_list1;

	while (node_id_ref_count_list2!=NULL){
		NodeIdP node_id;		

		node_id=node_id_ref_count_list2->nrcl_node_id;

		while (node_id_ref_count_list1=*node_id_ref_count_list1_p,node_id_ref_count_list1!=NULL && node_id_ref_count_list1->nrcl_node_id<node_id)
			node_id_ref_count_list1_p=&node_id_ref_count_list1->nrcl_next;

		if (node_id_ref_count_list1!=NULL && node_id_ref_count_list1->nrcl_node_id==node_id){

#if DEBUG_OUTPUT
			printf ("add %s %d %d %d\n",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_list1->nrcl_ref_count,node_id_ref_count_list2->nrcl_ref_count);
#endif

			node_id_ref_count_list1->nrcl_ref_count += node_id_ref_count_list2->nrcl_ref_count+1;
			node_id_ref_count_list1_p=&node_id_ref_count_list1->nrcl_next;
		} else {
			NodeIdRefCountListP new_node_id_ref_count_elem;

#if DEBUG_OUTPUT
			printf ("addnew %s %d %d\n",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_list2->nrcl_ref_count);
#endif

			new_node_id_ref_count_elem=new_node_id_ref_count (node_id_ref_count_list1,node_id_ref_count_list2->nrcl_node_id,node_id_ref_count_list2->nrcl_ref_count);

			*node_id_ref_count_list1_p=new_node_id_ref_count_elem;
			node_id_ref_count_list1_p=&new_node_id_ref_count_elem->nrcl_next;
		}

		node_id_ref_count_list2=node_id_ref_count_list2->nrcl_next;
	}
}

/* JVG added 16-2-2000 */
static void add_sorted_node_id_ref_count_list_for_case (NodeIdRefCountListP *node_id_ref_count_list1_p,NodeIdRefCountListP node_id_ref_count_list2)
{
	NodeIdRefCountListP node_id_ref_count_list1;

	while (node_id_ref_count_list2!=NULL){
		NodeIdP node_id;		

		node_id=node_id_ref_count_list2->nrcl_node_id;

		while (node_id_ref_count_list1=*node_id_ref_count_list1_p,node_id_ref_count_list1!=NULL && node_id_ref_count_list1->nrcl_node_id<node_id)
			node_id_ref_count_list1_p=&node_id_ref_count_list1->nrcl_next;

		if (node_id_ref_count_list1!=NULL && node_id_ref_count_list1->nrcl_node_id==node_id){

#if DEBUG_OUTPUT
			printf ("add %s %d %d %d\n",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_list1->nrcl_ref_count,node_id_ref_count_list2->nrcl_ref_count);
#endif

			node_id_ref_count_list1->nrcl_ref_count += node_id_ref_count_list2->nrcl_ref_count+1;
			node_id_ref_count_list1_p=&node_id_ref_count_list1->nrcl_next;
		} /* else do nothing*/

		node_id_ref_count_list2=node_id_ref_count_list2->nrcl_next;
	}
}
/**/

/*
	static NodeIdRefCountListP merge_sorted_node_id_ref_count_lists
		(NodeIdRefCountListP node_id_ref_count_list1,NodeIdRefCountListP node_id_ref_count_list2)
	{
		NodeIdRefCountListP node_id_ref_count_list,*node_id_ref_count_list_p;
	
		node_id_ref_count_list_p=&node_id_ref_count_list;
		
		while (node_id_ref_count_list2!=NULL){
			NodeIdP node_id;		
	
			node_id=node_id_ref_count_list2->nrcl_node_id;
	
			while (node_id_ref_count_list1!=NULL && node_id_ref_count_list1->nrcl_node_id<node_id){
				NodeIdRefCountListP new_node_id_ref_count_elem;
	
	#if DEBUG_OUTPUT
				{
					char *node_id_name;
					
					node_id_name="";
					if (node_id_ref_count_list1->nrcl_node_id->nid_ident!=NULL && node_id_ref_count_list1->nrcl_node_id->nid_ident->ident_name!=NULL)
						node_id_name=node_id_ref_count_list1->nrcl_node_id->nid_ident->ident_name;
				
					printf ("from1 %s %d %d\n",node_id_name,node_id_ref_count_list1->nrcl_node_id->nid_refcount,node_id_ref_count_list1->nrcl_ref_count);
				}
	#endif
		
				new_node_id_ref_count_elem=new_node_id_ref_count (NULL,node_id_ref_count_list1->nrcl_node_id,node_id_ref_count_list1->nrcl_ref_count);
		
				*node_id_ref_count_list_p=new_node_id_ref_count_elem;
				node_id_ref_count_list_p=&new_node_id_ref_count_elem->nrcl_next;
	
				node_id_ref_count_list1=node_id_ref_count_list1->nrcl_next;
			}
	
			if (node_id_ref_count_list1!=NULL && node_id_ref_count_list1->nrcl_node_id==node_id){
				NodeIdRefCountListP new_node_id_ref_count_elem;
	
	#if DEBUG_OUTPUT
				{
					char *node_id_name;
					
					node_id_name="";
					if (node_id->nid_ident!=NULL && node_id->nid_ident->ident_name!=NULL)
						node_id_name=node_id->nid_ident->ident_name;
				
					printf ("combine %s %d %d\n",node_id_name,node_id_ref_count_list1->nrcl_node_id->nid_refcount,node_id_ref_count_list2->nrcl_ref_count);
				}
	#endif
		
				new_node_id_ref_count_elem=new_node_id_ref_count (NULL,node_id,
					node_id_ref_count_list1->nrcl_ref_count+node_id_ref_count_list2->nrcl_ref_count+1);
		
				*node_id_ref_count_list_p=new_node_id_ref_count_elem;
				node_id_ref_count_list_p=&new_node_id_ref_count_elem->nrcl_next;
	
				node_id_ref_count_list1=node_id_ref_count_list1->nrcl_next;
			} else {
				NodeIdRefCountListP new_node_id_ref_count_elem;
	
	#if DEBUG_OUTPUT
				{
					char *node_id_name;
					
					node_id_name="";
					if (node_id_ref_count_list2->nrcl_node_id->nid_ident!=NULL && node_id_ref_count_list2->nrcl_node_id->nid_ident->ident_name!=NULL)
						node_id_name=node_id_ref_count_list2->nrcl_node_id->nid_ident->ident_name;
				
					printf ("from2 %s %d %d\n",node_id_name,node_id_ref_count_list2->nrcl_node_id->nid_refcount,node_id_ref_count_list2->nrcl_ref_count);
				}
	#endif
	
				new_node_id_ref_count_elem=new_node_id_ref_count (NULL,node_id_ref_count_list2->nrcl_node_id,node_id_ref_count_list2->nrcl_ref_count);
	
				*node_id_ref_count_list_p=new_node_id_ref_count_elem;
				node_id_ref_count_list_p=&new_node_id_ref_count_elem->nrcl_next;
			}
	
			node_id_ref_count_list2=node_id_ref_count_list2->nrcl_next;
		}
	
		while (node_id_ref_count_list1!=NULL){
			NodeIdRefCountListP new_node_id_ref_count_elem;
	
	#if DEBUG_OUTPUT
				{
					char *node_id_name;
					
					node_id_name="";
					if (node_id_ref_count_list1->nrcl_node_id->nid_ident!=NULL && node_id_ref_count_list1->nrcl_node_id->nid_ident->ident_name!=NULL)
						node_id_name=node_id_ref_count_list1->nrcl_node_id->nid_ident->ident_name;
				
					printf ("from1 %s %d %d\n",node_id_name,node_id_ref_count_list1->nrcl_node_id->nid_refcount,node_id_ref_count_list1->nrcl_ref_count);
				}
	#endif
		
			new_node_id_ref_count_elem=new_node_id_ref_count (NULL,node_id_ref_count_list1->nrcl_node_id,node_id_ref_count_list1->nrcl_ref_count);
	
			*node_id_ref_count_list_p=new_node_id_ref_count_elem;
			node_id_ref_count_list_p=&new_node_id_ref_count_elem->nrcl_next;
			
			node_id_ref_count_list1=node_id_ref_count_list1->nrcl_next;
		}
		
		*node_id_ref_count_list_p=NULL;
		
		return node_id_ref_count_list;
	}
*/

static NodeIdRefCountListP duplicate_node_id_ref_count_list (NodeIdRefCountListP node_id_ref_count_list)
{
	NodeIdRefCountListP node_id_ref_count_elem,new_node_id_ref_count_list,*new_node_id_ref_count_list_p;

	new_node_id_ref_count_list_p=&new_node_id_ref_count_list;

	for (node_id_ref_count_elem=node_id_ref_count_list; node_id_ref_count_elem!=NULL; node_id_ref_count_elem=node_id_ref_count_elem->nrcl_next){
		NodeIdRefCountListP new_node_id_ref_count_elem;

		new_node_id_ref_count_elem=new_node_id_ref_count (NULL,node_id_ref_count_elem->nrcl_node_id,node_id_ref_count_elem->nrcl_ref_count);

#if DEBUG_OUTPUT
		printf ("duplicate %s %d %d\n",node_id_name (node_id_ref_count_elem->nrcl_node_id),node_id_ref_count_elem->nrcl_node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count);
#endif

		*new_node_id_ref_count_list_p=new_node_id_ref_count_elem;
		new_node_id_ref_count_list_p=&new_node_id_ref_count_elem->nrcl_next;
	}

	*new_node_id_ref_count_list_p=NULL;

	return new_node_id_ref_count_list;
}

#ifdef CLEAN2
extern int contains_fail (NodeP node_p);
#endif

static int determine_failing_cases_and_adjust_ref_counts (NodeP node,NodeIdRefCountListP *node_id_ref_count_list_p)
{
	switch (node->node_kind){
		case SwitchNode:
		{
			ArgP arg;
			int switch_may_fail,default_may_fail;
			int node_id_ref_count_list_sorted;

			node_id_ref_count_list_sorted=0;

			for (arg=node->node_arguments; arg!=NULL; arg=arg->arg_next)
				if (arg->arg_node->node_kind!=CaseNode)
					break;
			
			default_may_fail=1;

			if (arg!=NULL){
				NodeP arg_node;

				arg_node=arg->arg_node;

				if (arg_node->node_kind!=DefaultNode)
					error_in_function ("determine_failing_cases_and_adjust_ref_counts");

				default_may_fail=determine_failing_cases_and_adjust_ref_counts (arg_node->node_arguments->arg_node,node_id_ref_count_list_p);
				arg_node->node_number=default_may_fail;

				if (default_may_fail){
					/* NodeP default_rhs_node; */

					sort_node_id_ref_count_lists (&arg_node->node_node_id_ref_counts);

					if (!node_id_ref_count_list_sorted){
						sort_node_id_ref_count_lists (node_id_ref_count_list_p);
						node_id_ref_count_list_sorted=1;
					}

					/* JVG: maybe incorrect, optimisation: find simple case which can not fail and set node_id_refcounts
						default_rhs_node=arg_node->node_arguments->arg_node;
						
						if (default_rhs_node->node_kind==PushNode)
							default_rhs_node=default_rhs_node->node_arguments->arg_next->arg_node;
						
						if (default_rhs_node->node_kind==SwitchNode && default_rhs_node->node_arguments->arg_next==NULL)
							default_rhs_node->node_arguments->arg_node->node_node_id_ref_counts
								= duplicate_node_id_ref_count_list (arg_node->node_node_id_ref_counts);
					*/

					add_sorted_node_id_ref_count_list (&arg_node->node_node_id_ref_counts,*node_id_ref_count_list_p);
					node_id_ref_count_list_p=&arg_node->node_node_id_ref_counts;
					
					/*					
						arg_node->node_node_id_ref_counts=merge_sorted_node_id_ref_count_lists (arg_node->node_node_id_ref_counts,*node_id_ref_count_list_p);
						node_id_ref_count_list_p=&arg_node->node_node_id_ref_counts;
					*/
				} else
					node_id_ref_count_list_p=&arg_node->node_node_id_ref_counts;
			}

			switch_may_fail=1;

			/* to do: if non failing case for every constructor, default not reachable */

#if 1 /* added 8-4-1999 */
			if (node->node_arguments->arg_next==NULL && node->node_arguments->arg_node->node_kind==CaseNode
				&& (node->node_arguments->arg_node->node_symbol->symb_kind==tuple_symb
					|| (node->node_arguments->arg_node->node_symbol->symb_kind==definition && 
						node->node_arguments->arg_node->node_symbol->symb_def->sdef_kind==RECORDTYPE)))
			{
				int case_may_fail;
				NodeP arg_node;

				arg_node=node->node_arguments->arg_node;

				case_may_fail=determine_failing_cases_and_adjust_ref_counts (arg_node->node_arguments->arg_node,node_id_ref_count_list_p);
				
				arg_node->node_number=case_may_fail;
				
				switch_may_fail=case_may_fail;
			} else
#endif

			for_l (arg,node->node_arguments,arg_next){
				NodeP arg_node;

				arg_node=arg->arg_node;

				switch (arg_node->node_kind){
					case CaseNode:
					{
						int case_may_fail;

						case_may_fail=determine_failing_cases_and_adjust_ref_counts (arg_node->node_arguments->arg_node,node_id_ref_count_list_p);

						if (case_may_fail && node->node_arguments->arg_next!=NULL){
							/* NodeP case_rhs_node; */

							sort_node_id_ref_count_lists (&arg_node->node_node_id_ref_counts);

							if (!node_id_ref_count_list_sorted){
								sort_node_id_ref_count_lists (node_id_ref_count_list_p);
								node_id_ref_count_list_sorted=1;
							}
							
							/* JVG: maybe incorrect, optimisation: find simple case which can not fail and set node_id_refcounts
								case_rhs_node=arg_node->node_arguments->arg_node;
																				
								if (case_rhs_node->node_kind==PushNode)
									case_rhs_node=case_rhs_node->node_arguments->arg_next->arg_node;
								
								if (case_rhs_node->node_kind==SwitchNode && case_rhs_node->node_arguments->arg_next==NULL)
									case_rhs_node->node_arguments->arg_node->node_node_id_ref_counts
										= duplicate_node_id_ref_count_list (arg_node->node_node_id_ref_counts);
							*/

							/* JVG changed 16-2-2000
							add_sorted_node_id_ref_count_list (&arg_node->node_node_id_ref_counts,*node_id_ref_count_list_p);
							*/
							add_sorted_node_id_ref_count_list_for_case (&arg_node->node_node_id_ref_counts,*node_id_ref_count_list_p);
							/**/
							
							/*
								arg_node->node_node_id_ref_counts=
									merge_sorted_node_id_ref_count_lists (arg_node->node_node_id_ref_counts,*node_id_ref_count_list_p);
							*/
						}

						arg_node->node_number=case_may_fail;
						break;
					}
					case DefaultNode:
						switch_may_fail=default_may_fail;
						break;
					default:
						error_in_function ("determine_failing_cases_and_adjust_ref_counts");
				}
			}
			return switch_may_fail;
		}
		case PushNode:
			return determine_failing_cases_and_adjust_ref_counts (node->node_arguments->arg_next->arg_node,node_id_ref_count_list_p);
		case GuardNode:
			return determine_failing_cases_and_adjust_ref_counts (node->node_arguments->arg_next->arg_node,node_id_ref_count_list_p);
		case IfNode:
#ifdef CLEAN2
			return contains_fail (node);
#else
		{
			NodeP else_node;
			
			else_node=node->node_arguments->arg_next->arg_next->arg_node;
			while (else_node->node_kind==IfNode)
				else_node=else_node->node_arguments->arg_next->arg_next->arg_node;
			
			return else_node->node_kind==NormalNode && else_node->node_symbol->symb_kind==fail_symb;
		}
#endif
		default:
			return False;
	}
}

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
void determine_failing_cases_and_adjust_ref_counts_of_rule (RuleAltP first_alt)
{
	NodeIdRefCountListP node_id_ref_count_list;

	if (first_alt->alt_kind!=Contractum)
		return;

	node_id_ref_count_list=NULL;
	determine_failing_cases_and_adjust_ref_counts (first_alt->alt_rhs_root,&node_id_ref_count_list);

# if 0
	PrintRuleAlt (first_alt,4,StdOut);
# endif
}
#endif

#if 0
#include "dbprint.h"
#endif

void transform_patterns_to_case_and_guard_nodes (RuleAltP rule_alts)
{
	RuleAltP rule_alt,first_alt;
	ArgP arg;
	struct root_and_defs_l root_and_defs_l;
	NodeP *node_p;
	NodeDefP *def_p;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	StrictNodeIdP *strict_node_ids_p;
	NodeDefP *end_lhs_defs_p;
#endif

	first_alt=rule_alts;
	
	if (first_alt->alt_kind!=Contractum)
		return;

	node_p=&first_alt->alt_rhs_root;
	def_p=&first_alt->alt_rhs_defs;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	strict_node_ids_p=&first_alt->alt_strict_node_ids;
	end_lhs_defs_p=&first_alt->alt_lhs_defs;
#endif

	root_and_defs_l.root_l=&node_p;
	root_and_defs_l.def_l=&def_p;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	root_and_defs_l.strict_node_ids_l=&strict_node_ids_p;
	root_and_defs_l.end_lhs_defs_l=&end_lhs_defs_p;
#endif
	
	for_l (arg,first_alt->alt_lhs_root->node_arguments,arg_next)
		transform_argument (arg,&root_and_defs_l);

	for_l (rule_alt,first_alt->alt_next,alt_next){
		ArgP arg,first_alt_arg;
		NodeIdRefCountListP node_id_ref_count_list,*node_id_ref_count_list_p;
		struct root_and_defs root_and_defs;
		NodeP *node_p;
		NodeDefP *def_p;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		StrictNodeIdP *strict_node_ids_p;
#endif

		node_p=&rule_alt->alt_rhs_root;
		def_p=&rule_alt->alt_rhs_defs;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		strict_node_ids_p=&rule_alt->alt_strict_node_ids;
#endif		

		arg=rule_alt->alt_lhs_root->node_arguments;
		first_alt_arg=first_alt->alt_lhs_root->node_arguments;
		
		node_id_ref_count_list=NULL;
		node_id_ref_count_list_p=&node_id_ref_count_list;

		root_and_defs_l.root_l=&node_p;
		root_and_defs_l.def_l=&def_p;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		root_and_defs_l.strict_node_ids_l=&strict_node_ids_p;
		root_and_defs_l.end_lhs_defs_l=&end_lhs_defs_p;
#endif

		for (; arg!=NULL; arg=arg->arg_next,first_alt_arg=first_alt_arg->arg_next)
			transform_and_merge_argument (arg,first_alt_arg,&root_and_defs_l,&node_id_ref_count_list_p);
		
		CurrentLine=rule_alt->alt_line;

#if DEBUG_OUTPUT
		printf ("line %d\n",CurrentLine);
#endif

		root_and_defs.root=rule_alt->alt_rhs_root;
		root_and_defs.defs=rule_alt->alt_rhs_defs;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		root_and_defs.strict_node_ids=rule_alt->alt_strict_node_ids;
#endif
		
		first_alt->alt_rhs_root=merge_alternative_with_node (first_alt->alt_rhs_root,&root_and_defs,node_id_ref_count_list);
	}

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	*end_lhs_defs_p=NULL;
#endif

	first_alt->alt_next=NULL;

#if 0
	PrintRuleAlt (first_alt,4,StdOut);
#endif

#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	{
		NodeIdRefCountListP node_id_ref_count_list;
		
		node_id_ref_count_list=NULL;
		determine_failing_cases_and_adjust_ref_counts (first_alt->alt_rhs_root,&node_id_ref_count_list);
	}

# if 0
	PrintRuleAlt (first_alt,4,StdOut);
# endif
#endif
}
