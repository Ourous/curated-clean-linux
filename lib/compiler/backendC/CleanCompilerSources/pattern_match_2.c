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

static NodeP new_push_node (Symbol symbol,int arity,ArgP arguments)
{
	NodeP push_node;
	
	push_node=CompAllocType (NodeS);
	
	push_node->node_kind=PushNode;
	push_node->node_arity=arity;
	push_node->node_arguments=arguments;
	push_node->node_push_symbol=symbol;
	push_node->node_number=0;	/* if !=0 then unique */
	
	return push_node;
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

#if BOXED_RECORDS
void set_global_reference_counts_and_exchange_record_update_marks (NodeP case_node)
{
	NodeIdRefCountListP node_id_ref_count_elem;

	for_l (node_id_ref_count_elem,case_node->node_node_id_ref_counts,nrcl_next){
		int local_ref_count;
		NodeIdP node_id;
		unsigned int node_id_mark2;
		
		node_id=node_id_ref_count_elem->nrcl_node_id;

		node_id_mark2=node_id->nid_mark2;
		node_id->nid_mark2=(node_id_mark2 & ~NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES) | node_id_ref_count_elem->nrcl_mark2;
		node_id_ref_count_elem->nrcl_mark2=node_id_mark2 & NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;

		local_ref_count=node_id->nid_refcount;

		node_id->nid_refcount = local_ref_count + node_id_ref_count_elem->nrcl_ref_count;
		node_id_ref_count_elem->nrcl_ref_count=local_ref_count;
	}
}
#endif

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
				if (arg->arg_node->node_kind!=CaseNode && arg->arg_node->node_kind!=OverloadedCaseNode)
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
					case OverloadedCaseNode:
						arg_node = arg_node->node_node;
						/* no break */
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
