/*
	File:	result_state_database.c
	Author: John van Groningen
	At:		University of Nijmegen
*/

#if defined (applec) || defined (__MWERKS__) || defined (__MRC__)
# define __ppc__
#endif

#include <stdio.h>

#include "compiledefines.h"
#include "types.t"
#include "syntaxtr.t"
#include "comsupport.h"
#include "result_state_database.h"

struct state_tree {
	struct state_tree *	stt_left;
	struct state_tree *	stt_right;
	struct state *		stt_state_p;
	int					stt_label_number;
	int					stt_label_defined;
};

static struct state_tree *state_tree;
static int next_update_label_number;

static int compare_states (struct state *state1_p,struct state *state2_p)
{
	int r;
	
	r=state1_p->state_type - state2_p->state_type;
	if (r!=0)
		return r;
	
	switch (state1_p->state_type){
		case SimpleState:
			r=state1_p->state_kind - state2_p->state_kind;
			if (r!=0)
				return r;
			
			return state1_p->state_object - state2_p->state_object;
		case ArrayState:
			return compare_states (state1_p->state_array_arguments,state2_p->state_array_arguments);
		case TupleState:
		{
			int n;
			
			r=state1_p->state_arity - state2_p->state_arity;
			if (r!=0)
				return r;

			n=state1_p->state_arity;
			
			state1_p=state1_p->state_tuple_arguments;
			state2_p=state2_p->state_tuple_arguments;
			
			while (n>0){
				r=compare_states (state1_p,state2_p);
				if (r!=0)
					return r;
				
				--n;
				++state1_p;
				++state2_p;
			}

			return 0;
		}
		case RecordState:
		{
			struct symbol_def *sdef1,*sdef2;
			
			sdef1=state1_p->state_record_symbol;
			sdef2=state2_p->state_record_symbol;
			if (sdef1==sdef2)
				return 0;
			else
				if (sdef1<sdef2)
					return -1;
				else
					return 1;
		}
		default:
			ErrorInCompiler ("compare_states","","");
			return -1;
	}
}	

static void store_state_in_database (struct state *state_p)
{
	struct state_tree **state_node_h;
	struct state_tree *state_node_p;
	
	state_node_h=&state_tree;
	
	while (state_node_p=*state_node_h,state_node_p!=NULL){
		int state_compare_result;
		
		state_compare_result=compare_states (state_p,state_node_p->stt_state_p);
		
		if (state_compare_result==0){
			if (state_node_p->stt_label_number==0){
				state_node_p->stt_label_number=next_update_label_number;
				++next_update_label_number;
			}

			return;
		} else
			if (state_compare_result<0)
				state_node_h=&state_node_p->stt_left;
			else
				state_node_h=&state_node_p->stt_right;
	}
	
	state_node_p=CompAllocType (struct state_tree);
	
	state_node_p->stt_left=NULL;
	state_node_p->stt_right=NULL;
	state_node_p->stt_state_p=state_p;
	state_node_p->stt_label_number=0;
	state_node_p->stt_label_defined=0;
	
	*state_node_h=state_node_p;
}

void create_result_state_database (struct imp_rule *imp_rules)
{
	struct imp_rule *rule;

	state_tree=NULL;
	next_update_label_number=1;

	for (rule=imp_rules; rule; rule=rule->rule_next){
		TypeAlts type_alt;
		struct state *state_p;
		
		type_alt=rule->rule_type;
		if (type_alt==NULL)
			continue;
		
#if 1
		state_p=&rule->rule_state_p[-1];
#else
		state_p=&type_alt->type_alt_lhs->type_node_state;
#endif		
		if (state_p->state_type==SimpleState){
			if (state_p->state_kind==OnB)
				store_state_in_database (state_p);
		} else
			store_state_in_database (state_p);
	}			
}

static int find_state_in_database (struct state *state_p,int mask,int *label_number_p)
{
	struct state_tree *state_node_p;
	
	state_node_p=state_tree;
	
	while (state_node_p!=NULL){
		int state_compare_result;
		
		state_compare_result=compare_states (state_p,state_node_p->stt_state_p);
		
		if (state_compare_result==0){
			if (state_node_p->stt_label_number==0)
				return 0;
			
			*label_number_p=state_node_p->stt_label_number;
			
			if ((state_node_p->stt_label_defined & mask)==0){
				state_node_p->stt_label_defined|=mask;
				return 1;
			} else
				return 2;
		} else
			if (state_compare_result<0)
				state_node_p=state_node_p->stt_left;
			else
				state_node_p=state_node_p->stt_right;
	}
	
	return 0;
}

/*
	get_label_number_from_result_state_database returns:
		0: no label (state occurs only once)
		1: label not yet defined
		2: label already defined
*/

#if 1
int get_label_number_from_result_state_database (StateP result_state_p,int mask,int *label_number_p)
#else
int get_label_number_from_result_state_database (TypeAlts type_alt,int mask,int *label_number_p)
#endif
{
	struct state *state_p;

	*label_number_p=0;
	
#if 1
	state_p=result_state_p;
#else
	if (type_alt==NULL)
		return 0;

	state_p=&type_alt->type_alt_lhs->type_node_state;
#endif

	if (state_p->state_type==SimpleState){
		if (state_p->state_kind==OnB)
			return find_state_in_database (state_p,mask,label_number_p);
		else
			return 0;
	} else
		return find_state_in_database (state_p,mask,label_number_p);
}
