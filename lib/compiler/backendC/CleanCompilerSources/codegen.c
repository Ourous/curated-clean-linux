
#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)

#define SHARE_UPDATE_CODE 0 /* also in codegen1.c */
#define SELECTORS_FIRST 1 /* also in codegen2.c */
#define UNBOXED_RECORDS_IN_UNBOXED_CLOSURES 1 /* 1 if UNBOX_UPDATE_FUNCTION_ARGUMENTS */
#define UNBOXED_TUPLES_IN_UNBOXED_CLOSURES 1 /* 1 if UNBOX_UPDATE_FUNCTION_ARGUMENTS */

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "syntaxtr.t"
#include "comsupport.h"
#include "checker.h"
#include "settings.h"
#include "sa.h"
#include "statesgen.h"
#include "typeconv.h"
#include "codegen_types.h"
#include "codegen1.h"
#include "codegen2.h"
#include "codegen3.h"
#include "instructions.h"
#include "codegen.h"
#include "optimisations.h"
#include "pattern_match.h"
#if SHARE_UPDATE_CODE
#	include "result_state_database.h"
#endif
# if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
#include "tuple_tail_recursion.h"
# endif

static char *ECodeBlock	= "incorrect number of output parameters";

static Parameters CalculateOffsetsOfParameters (Parameters params,States resultstates,int statearity,int *asp_p,int *bsp_p);

static Parameters CalculateOffsetsOfParameter (Parameters param,StateS resultstate,int *asp_p,int *bsp_p)
{
	if (IsSimpleState (resultstate)){
		if (param->par_node_id!=NULL)
			param->par_node_id->nid_state_=resultstate;
		
		if (resultstate.state_kind==OnB){
			if (param->par_node_id)
				param->par_node_id->nid_b_index_=*bsp_p;
			*bsp_p -= ObjectSizes [resultstate.state_object];
		} else {
			if (param->par_node_id)
				param->par_node_id->nid_a_index_=*asp_p;
			*asp_p -= 1;
		}
		
		return param->par_next;
	} else {
		if (resultstate.state_type==ArrayState){
			if (param->par_node_id)
				param->par_node_id->nid_a_index_=*asp_p;
			*asp_p -= 1;

			return param->par_next;
		} else
			return CalculateOffsetsOfParameters (param,resultstate.state_tuple_arguments,resultstate.state_arity,asp_p,bsp_p);
	}
}	

static Parameters CalculateOffsetsOfParameters (Parameters params,States resultstates,int statearity,int *asp_p,int *bsp_p)
{
	int arity;

	for (arity=0; arity<statearity; arity++){
		if (params)
			params = CalculateOffsetsOfParameter (params,resultstates[arity],asp_p,bsp_p);
		else {
			StaticMessage (True,CurrentAltLabel.lab_symbol->sdef_ident->ident_name,ECodeBlock);
			break;
		}
	}

	return params;
}

static void GenCodeBlock (CodeBlock code, int asp, int bsp, StateS resultstate)
{
	int newasp,newbsp,asize,bsize;
	
	DetermineSizeOfState (resultstate,&newasp,&newbsp);

	if (code->co_is_abc_code){
		GenInstructions (code->co_instr);
		GenRtn (newasp, newbsp, resultstate);
	} else {
		Parameters nextparam;
		
		asize = newasp;
		bsize = newbsp;
		
		if (IsSimpleState (resultstate))
			nextparam = CalculateOffsetsOfParameter (code->co_parout,resultstate,&asize,&bsize);
		else {
			switch (resultstate.state_type){
				case TupleState:
					nextparam = CalculateOffsetsOfParameters (code->co_parout,resultstate.state_tuple_arguments,
						resultstate.state_arity,&asize,&bsize);
					break;
				case RecordState:
					nextparam = CalculateOffsetsOfParameters (code->co_parout,resultstate.state_record_arguments,
						resultstate.state_arity,&asize,&bsize);
					break;
				case ArrayState:
					if (code->co_parout->par_node_id!=NULL)
						code->co_parout->par_node_id->nid_state_=resultstate;
					code->co_parout->par_node_id->nid_a_index_=asize;
					asize -= 1;
					nextparam=code->co_parout->par_next;
					break;
			}
		}
		
		if (nextparam)
			StaticMessage (True,CurrentAltLabel.lab_symbol->sdef_ident->ident_name, ECodeBlock);

		GenParameters (True, code->co_parin, asp, bsp);
		GenInstructions (code->co_instr);
		GenOStackLayoutOfState (newasp, newbsp, resultstate);
		GenParameters (False, code->co_parout, newasp, newbsp);
		GenRtn (newasp, newbsp, resultstate);
	}	
}

static Bool CodeRuleAlt (RuleAlts alt,int asp,int bsp,unsigned int altnr,StateS resultstate)
{
	struct label esclab;
	struct esc esc;
	struct ab_node_ids ab_node_ids;

	ab_node_ids.a_node_ids=NULL;
	ab_node_ids.b_node_ids=NULL;
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
	ab_node_ids.free_node_ids=NULL;
#endif

	esc.esc_asp=asp;
	esc.esc_bsp=bsp;
	esc.esc_label=&esclab;

	MakeSymbolLabel (&esclab,CurrentAltLabel.lab_mod,s_pref,CurrentAltLabel.lab_symbol,altnr+1);

	LhsComment (altnr, asp, bsp);     

	bind_arguments (alt->alt_lhs_root->node_arguments,asp,bsp,&ab_node_ids);
	MatchArgs (alt->alt_lhs_root->node_arguments,asp,bsp,asp,bsp,&ab_node_ids);

	if (alt->alt_kind==Contractum)
		return generate_code_for_root_node (alt->alt_rhs_root,asp,bsp,&esc,alt->alt_rhs_defs,&resultstate,NULL,&ab_node_ids);
	else {
		GenCodeBlock (alt->alt_rhs_code,asp,bsp,resultstate);
		return False;
	}

#ifdef _FRAMECHECK_
	if (MaxAFrameSize !=0 || MaxBFrameSize != 0 || OfferedAFrame != InitOfferedAFrame)
		ErrorInCompiler ("codegen2.c", "CodeRuleAlt","inconsistent stack frames");
#endif
}

static void MoveArgumentFromAToB (StateS argstate,int index,int *current_asp_p,int *old_asp_p,int *dest_asp_p)
{
	if (IsSimpleState (argstate)){
		if (argstate.state_kind==OnB)
			PushBasicFromAOnB ((ObjectKind)(argstate.state_object),*current_asp_p-index);
		else if (argstate.state_kind!=Undefined)
			PutInAFrames (index,dest_asp_p);
	} else {
		int arity;
		
		arity = argstate.state_arity;

		switch (argstate.state_type){
			case TupleState:
			{
				int locindex,i;
				States argstates;
				
				argstates = argstate.state_tuple_arguments;

				if (*old_asp_p==index)
					--*old_asp_p;

				*old_asp_p += arity;
				locindex = *old_asp_p;

				for (i=arity-1; i>=0; --i)
					MoveArgumentFromAToB (argstates [i],locindex-i,current_asp_p,old_asp_p,dest_asp_p);
				break;
			}
			case RecordState:
			{
				int asize,bsize,a_index,element_n;
			
				DetermineSizeOfStates (arity,argstate.state_record_arguments,&asize,&bsize);

				if (*current_asp_p==index){
					GenReplRArgs (asize,bsize);
					*current_asp_p += asize-1;
				} else {
					GenPushRArgs (*current_asp_p-index,asize,bsize);
					*current_asp_p += asize;
				}

				a_index=*current_asp_p;
				for (element_n=asize-1; element_n>=0; --element_n)
					PutInAFrames (a_index-element_n,dest_asp_p);
				break;
			} 
			case ArrayState:
				GenPushArray (*current_asp_p-index);
				*current_asp_p += 1;
				PutInAFrames (*current_asp_p,dest_asp_p);
				break;
		}
	}
}

static void MoveStateArgumentsFromAToB (int n_states,StateP state_p,int index,int *current_asp_p,int *old_asp_p,int *dest_asp_p)
{
	int i;
	
	for (i=n_states-1; i>=0; --i)
		MoveArgumentFromAToB (state_p[i],index-i,current_asp_p,old_asp_p,dest_asp_p);
}

static void EvaluateArgument (StateS argstate,int *asp_p,int index)
{
	if (!IsLazyState (argstate)){
		switch (argstate.state_type){
			case SimpleState:
			case RecordState:
			case ArrayState:
				GenJsrEval (*asp_p-index);
				break;
			case TupleState:
			{
				int i,arity,locasp;
				
				arity = argstate.state_arity;

				if (*asp_p-index > 0){
					GenPushA (*asp_p-index);
					GenJsrEval (0);
					GenReplArgs (arity,arity);
				} else {
					GenJsrEval (0);
					GenReplArgs (arity,arity);
					--*asp_p;
				}
				*asp_p += arity;
				locasp = *asp_p;

				for (i=arity-1; i>=0; i--)
					EvaluateArgument (argstate.state_tuple_arguments[i],asp_p,locasp-i);
				break;
			}
		}
	}
}

static void EvaluateStateArguments (int n_states,StateP state_p,int *asp_p,int index)
{
	int i;
	
	for (i=n_states-1; i>=0; i--)
		EvaluateArgument (state_p[i],asp_p,index-i);
}

void EvaluateAndMoveArguments (int arity,StateP argstates,int *locasp_p,int *aselmts_p)
{
	int i,index;
	
	index=*locasp_p;
	
	for (i=arity-1; i>=0; i--)
		EvaluateArgument (argstates[i],locasp_p, arity-i);

	for (i=arity-1; i>=0; i--)
		MoveArgumentFromAToB (argstates[i],arity-i,locasp_p,&index,aselmts_p);
}

void EvaluateAndMoveStateArguments (int state_arity,StateP states,int oldasp,int maxassize)
{
	int oldaframesize,arity,newasp,i;
	
	arity = oldasp;
	newasp = 0;

	InitAStackConversions (arity + maxassize + 1,&oldaframesize);

	for (i=state_arity-1; i>=0; i--)
		EvaluateArgument (states[i],&oldasp,state_arity-i);

	for (i=state_arity-1; i>=0; i--)
		MoveArgumentFromAToB (states[i],state_arity-i,&oldasp,&arity,&newasp);

	GenAStackConversions (oldasp,newasp);

	FreeAFrameSpace (oldaframesize);
}

static void EvaluateArgumentIfNecesary (StateS argstate,int *asp_p,int a_index,struct state *state_p)
{
	if (!IsLazyState (argstate)){
		switch (argstate.state_type){
			case SimpleState:
			case RecordState:
			case ArrayState:
				if (IsLazyState (*state_p))
					GenJsrEval (*asp_p-a_index);
				break;
			case TupleState:
			{
				int i,arity,locasp;
				
				arity = argstate.state_arity;

				if (*asp_p-a_index > 0){
					GenPushA (*asp_p-a_index);
					if (IsLazyState (*state_p))
						GenJsrEval (0);
					GenReplArgs (arity,arity);
				} else {
					if (IsLazyState (*state_p))
						GenJsrEval (0);
					GenReplArgs (arity,arity);
					--*asp_p;
				}
				*asp_p += arity;
				locasp = *asp_p;
				
				if (state_p->state_type==TupleState){
					for (i=arity-1; i>=0; i--)
						EvaluateArgumentIfNecesary (argstate.state_tuple_arguments[i],asp_p,locasp-i,&state_p->state_tuple_arguments[i]);
				} else
					for (i=arity-1; i>=0; i--)
						EvaluateArgument (argstate.state_tuple_arguments[i],asp_p,locasp-i);
				break;
			}
		}
	}
}

static void EvaluateArgumentsForFunctionWithOneCall (int n_states,StateP arg_state_p,int *asp_p,int a_index,ArgP call_arg)
{
	if (call_arg==NULL)
		EvaluateStateArguments (n_states,arg_state_p,asp_p,a_index);
	else
		if (n_states>0){
			EvaluateArgumentsForFunctionWithOneCall (n_states-1,arg_state_p+1,asp_p,a_index-1,call_arg->arg_next);
			EvaluateArgumentIfNecesary (*arg_state_p,asp_p,a_index,state_of_node_or_node_id (call_arg->arg_node));
		}
}

static void EvaluateAndMoveArgumentsForFunctionWithOneCall (StateS *const function_state_p,int oldasp,int maxassize,struct node *call_node)
{
	int oldaframesize,arity,newasp;
	
	arity = oldasp;
	newasp = 0;

	InitAStackConversions (arity + maxassize + 1, &oldaframesize);

	EvaluateArgumentsForFunctionWithOneCall (arity,function_state_p,&oldasp,arity,call_node->node_arguments);

	MoveStateArgumentsFromAToB (arity,function_state_p,arity,&oldasp,&arity,&newasp);

	GenAStackConversions (oldasp,newasp);

	FreeAFrameSpace (oldaframesize);
}

void EvalArgsEntry (StateS *const function_state_p,SymbDef rule_sdef,int maxasize,Label ea_lab,int n_result_nodes_on_a_stack)
{
	int asp;
	
	asp=rule_sdef->sdef_arity;
	
	GenOAStackLayout (asp+n_result_nodes_on_a_stack);

	if (DoTimeProfiling)
		GenPN();
	GenLabelDefinition (ea_lab);

	if (rule_sdef->sdef_kind==IMPRULE && (rule_sdef->sdef_rule->rule_mark & RULE_LAZY_CALL_NODE_MASK))
		EvaluateAndMoveArgumentsForFunctionWithOneCall (function_state_p,asp,maxasize,rule_sdef->sdef_rule->rule_lazy_call_node);
	else
		EvaluateAndMoveStateArguments (asp,function_state_p,asp,maxasize);
}

static void EvaluateArgumentsForFunctionWithUnboxedArguments (int n_states,StateP arg_state_p,int *asp_p,int a_index,ArgP call_arg)
{
	if (n_states==0)
		return;
	else {
#if UNBOXED_RECORDS_IN_UNBOXED_CLOSURES
		if (call_arg->arg_state.state_type==SimpleState){
			if (call_arg->arg_state.state_kind==OnB){
				EvaluateArgumentsForFunctionWithUnboxedArguments (n_states-1,arg_state_p+1,asp_p,a_index,call_arg->arg_next);
				return;
			}
		} else if (call_arg->arg_state.state_type==RecordState){
			int a_size,b_size;
			
			DetermineSizeOfStates (call_arg->arg_state.state_arity,call_arg->arg_state.state_record_arguments,&a_size,&b_size);
			a_index -= a_size;

			EvaluateArgumentsForFunctionWithUnboxedArguments (n_states-1,arg_state_p+1,asp_p,a_index,call_arg->arg_next);
			return;
		}
# if UNBOXED_TUPLES_IN_UNBOXED_CLOSURES
		else if (call_arg->arg_state.state_type==TupleState){
			int a_size,b_size;
			
			DetermineSizeOfStates (call_arg->arg_state.state_arity,call_arg->arg_state.state_tuple_arguments,&a_size,&b_size);
			a_index -= a_size;

			EvaluateArgumentsForFunctionWithUnboxedArguments (n_states-1,arg_state_p+1,asp_p,a_index,call_arg->arg_next);
			return;
		}
# endif

		EvaluateArgumentsForFunctionWithUnboxedArguments (n_states-1,arg_state_p+1,asp_p,a_index-1,call_arg->arg_next);
		EvaluateArgumentIfNecesary (*arg_state_p,asp_p,a_index,!IsLazyState (call_arg->arg_state) ? &call_arg->arg_state : state_of_node_or_node_id (call_arg->arg_node));
#else
		if (call_arg->arg_state.state_type==SimpleState && call_arg->arg_state.state_kind==OnB)
			EvaluateArgumentsForFunctionWithUnboxedArguments (n_states-1,arg_state_p+1,asp_p,a_index,call_arg->arg_next);
		else {
			EvaluateArgumentsForFunctionWithUnboxedArguments (n_states-1,arg_state_p+1,asp_p,a_index-1,call_arg->arg_next);
			EvaluateArgumentIfNecesary (*arg_state_p,asp_p,a_index,!IsLazyState (call_arg->arg_state) ? &call_arg->arg_state : state_of_node_or_node_id (call_arg->arg_node));
		}
#endif
	}
}

static void MoveArgumentsToBStack (StateS src_state,StateS dest_state,
									int a_index,int *current_asp_p,int *old_asp_p,int *dest_asp_p,
									int b_index,int *current_bsp_p,int *dest_bsp_p)
{
	if (IsSimpleState (dest_state)){
		if (dest_state.state_kind==OnB){
			if (src_state.state_type==SimpleState && src_state.state_kind==OnB)
				PutInBFrames (b_index,dest_bsp_p,ObjectSizes[dest_state.state_object]);
			else {
				PushBasicFromAOnB ((ObjectKind)(dest_state.state_object),*current_asp_p-a_index);
				*current_bsp_p+=ObjectSizes[dest_state.state_object];
				PutInBFrames (*current_bsp_p,dest_bsp_p,ObjectSizes[dest_state.state_object]);
			}				
		} else if (dest_state.state_kind!=Undefined)
			PutInAFrames (a_index,dest_asp_p);
	} else {
		switch (dest_state.state_type){
			case TupleState:
			{
				int tuple_a_index,i,arity;
				States dest_states;
		
				arity = dest_state.state_arity;
				dest_states = dest_state.state_tuple_arguments;

#if UNBOXED_TUPLES_IN_UNBOXED_CLOSURES
				if (src_state.state_type==TupleState){
					int asize,bsize,element_n;
					
					DetermineSizeOfStates (arity,dest_state.state_tuple_arguments,&asize,&bsize);

					for (element_n=asize-1; element_n>=0; --element_n)
						PutInAFrames (a_index-element_n,dest_asp_p);
					PutInBFrames (b_index,dest_bsp_p,bsize);									
					return;
				}
#endif

				if (*old_asp_p==a_index)
					--*old_asp_p;

				*old_asp_p += arity;

				tuple_a_index = *old_asp_p;
				for (i=arity-1; i>=0; --i)
					MoveArgumentsToBStack (LazyState,dest_states[i],tuple_a_index-i,current_asp_p,old_asp_p,dest_asp_p,-1000,current_bsp_p,dest_bsp_p);
				break;
			}
			case RecordState:
			{
				int asize,bsize,record_a_index,element_n,arity;
		
				arity = dest_state.state_arity;
			
				DetermineSizeOfStates (arity,dest_state.state_record_arguments,&asize,&bsize);

#if UNBOXED_RECORDS_IN_UNBOXED_CLOSURES
				if (src_state.state_type==RecordState){
					for (element_n=asize-1; element_n>=0; --element_n)
						PutInAFrames (a_index-element_n,dest_asp_p);
					PutInBFrames (b_index,dest_bsp_p,bsize);									
					return;
				}
#endif

				if (*current_asp_p==a_index){
					GenReplRArgs (asize,bsize);
					*current_asp_p += asize-1;
				} else {
					GenPushRArgs (*current_asp_p-a_index,asize,bsize);
					*current_asp_p += asize;
				}
				*current_bsp_p += bsize;

				record_a_index=*current_asp_p;
				for (element_n=asize-1; element_n>=0; --element_n)
					PutInAFrames (record_a_index-element_n,dest_asp_p);

				PutInBFrames (*current_bsp_p,dest_bsp_p,bsize);
				break;
			}
			case ArrayState:
				if (src_state.state_type==ArrayState)
					PutInAFrames (a_index,dest_asp_p);
				else {
					GenPushArray (*current_asp_p-a_index);
					++*current_asp_p;
					PutInAFrames (*current_asp_p,dest_asp_p);
				}
				break;
		}
	}
}

static void MoveArgumentsForFunctionWithUnboxedArguments (int n_states,StateP state_p,ArgP call_arg,
														  int a_index,int *current_asp_p,int *old_asp_p,int *dest_asp_p,
														  int b_index,int *current_bsp_p,int *dest_bsp_p)
{
	if (n_states==0)
		return;
	else {
		int next_a_index,next_b_index;
		
		if (call_arg->arg_state.state_type==SimpleState){
			if (call_arg->arg_state.state_kind==OnB){
				next_a_index=a_index;
				next_b_index=b_index-ObjectSizes[call_arg->arg_state.state_object];
			} else {
				next_a_index=a_index-1;
				next_b_index=b_index;
			}
#if UNBOXED_RECORDS_IN_UNBOXED_CLOSURES
		} else if (call_arg->arg_state.state_type==RecordState){
			int a_size,b_size;
			
			DetermineSizeOfStates (call_arg->arg_state.state_arity,call_arg->arg_state.state_record_arguments,&a_size,&b_size);

			next_a_index=a_index-a_size;
			next_b_index=b_index-b_size;
#endif
#if UNBOXED_TUPLES_IN_UNBOXED_CLOSURES
		} else if (call_arg->arg_state.state_type==TupleState){
			int a_size,b_size;
			
			DetermineSizeOfStates (call_arg->arg_state.state_arity,call_arg->arg_state.state_tuple_arguments,&a_size,&b_size);

			next_a_index=a_index-a_size;
			next_b_index=b_index-b_size;
#endif
		} else {
			next_a_index=a_index-1;
			next_b_index=b_index;
		}
		MoveArgumentsForFunctionWithUnboxedArguments (n_states-1,state_p+1,call_arg->arg_next,
													  next_a_index,current_asp_p,old_asp_p,dest_asp_p,
													  next_b_index,current_bsp_p,dest_bsp_p);
		MoveArgumentsToBStack (call_arg->arg_state,*state_p,a_index,current_asp_p,old_asp_p,dest_asp_p,b_index,current_bsp_p,dest_bsp_p);
	}
}

static void EvalArgsEntryUnboxed (ImpRuleP rule_p,SymbDef rule_sdef,int strict_a_size,int strict_b_size,int maxasize,Label ea_lab,int n_result_nodes_on_a_stack)
{
	int args_a_size,args_b_size,old_a_frame_size,old_b_frame_size,init_a_stack_size;
	int old_asp,old_bsp,new_asp,new_bsp;
	StateP function_state_p;
	NodeP call_node_p;
	
	function_state_p=rule_p->rule_state_p;
	call_node_p=rule_p->rule_lazy_call_node;

	DetermineSizeOfArguments (call_node_p->node_arguments,&args_a_size,&args_b_size);

	init_a_stack_size=args_a_size + n_result_nodes_on_a_stack;
	GenOStackLayout (init_a_stack_size,args_b_size,call_node_p->node_arguments);

	if (DoTimeProfiling)
		GenPN();
	GenLabelDefinition (ea_lab);
	
	InitStackConversions (init_a_stack_size+maxasize+1,strict_b_size+1,&old_a_frame_size,&old_b_frame_size);

	old_asp=args_a_size;
	old_bsp=args_b_size;
	EvaluateArgumentsForFunctionWithUnboxedArguments (rule_sdef->sdef_arity,function_state_p,&old_asp,args_a_size,call_node_p->node_arguments);

	new_asp=0;
	new_bsp=0;
	MoveArgumentsForFunctionWithUnboxedArguments (rule_sdef->sdef_arity,function_state_p,call_node_p->node_arguments,
													args_a_size,&old_asp,&args_a_size,&new_asp,args_b_size,&old_bsp,&new_bsp);

	GenAStackConversions (old_asp,new_asp);
	GenBStackConversions (old_bsp,new_bsp);

	FreeAFrameSpace (old_a_frame_size);
	FreeBFrameSpace (old_b_frame_size);
}

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
int tail_call_modulo_cons;
#endif
#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
int tail_call_modulo_tuple_cons;
unsigned long global_same_select_vector;
#endif
#if OPTIMIZE_LAZY_TUPLE_RECURSION
int lazy_tuple_recursion=0;
#endif

#if GENERATE_CODE_AGAIN
int call_code_generator_again;
#endif

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
extern int does_tail_call_modulo_cons (NodeP node_p,NodeDefP node_defs);
#endif

int function_called_only_curried_or_lazy_with_one_return=0;

#if 0
# include "dbprint.h"
extern File rules_file;
#endif

static void CodeRule (ImpRuleP rule)
{
	Bool jmp_to_eval_args_entry,root_node_needed;
	int	asize,bsize,maxasize,a_stack_size_of_strict_entry;
	StateS	resultstate;
	SymbDef	rule_sdef;
	LabDef ea_lab;
	int init_a_stack_top,init_b_stack_top,rule_may_fail;

# if 0
	PrintImpRule (rule,4,StdOut);
# endif

	CurrentSymbol=rule->rule_root->node_symbol;
	CurrentLine=rule->rule_alts->alt_line;

	resultstate	= rule->rule_root->node_state;
	rule_sdef = CurrentSymbol->symb_def;
	
	ConvertSymbolToLabel (&CurrentAltLabel,rule_sdef);

	if (rule_sdef->sdef_exported){
		GenExportStrictAndEaEntry (rule_sdef);
	} else if (!(rule_sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK) ||
				 (rule_sdef->sdef_mark & SDEF_USED_STRICTLY_MASK && !(rule_sdef->sdef_mark & SDEF_INLINE_IS_CONSTRUCTOR))))
		return;

	GenFunctionDescriptorAndExportNodeAndDescriptor (rule_sdef);

	if (DoTimeProfiling)
		GenPB_ident (rule_sdef->sdef_ident,0 /*rule_sdef->sdef_line*/);

	if (rule_sdef->sdef_exported && rule_sdef->sdef_calledwithrootnode && ExpectsResultNode (resultstate))
		MakeSymbolLabel (&ea_lab,CurrentModule,ea_pref,rule_sdef,0);
	else
		MakeSymbolLabel (&ea_lab,NULL,ea_pref,rule_sdef,0);
	
	asize = 0;
	bsize = 0;
	maxasize = 0;
	AddStateSizesAndMaxFrameSizesOfArguments (rule->rule_root->node_arguments,&maxasize,&asize,&bsize);

	function_called_only_curried_or_lazy_with_one_return=0;

	if (!(rule_sdef->sdef_mark & SDEF_USED_STRICTLY_MASK) &&
		(  (rule_sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK))==SDEF_USED_CURRIED_MASK
		|| (rule_sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK))==SDEF_USED_LAZILY_MASK)
		&& !rule_sdef->sdef_returnsnode && rule->rule_alts->alt_kind==Contractum && !(rule->rule_mark & RULE_CAF_MASK))
	{
		NodeP node_p;
		
		node_p=rule->rule_alts->alt_rhs_root;
		
		while (node_p->node_kind==PushNode || node_p->node_kind==SwitchNode){
			if (node_p->node_kind==PushNode)
				node_p=node_p->node_arguments->arg_next->arg_node;
			else
				if (node_p->node_arguments->arg_next==NULL)
					node_p=node_p->node_arguments->arg_node->node_arguments->arg_node;
				else
					break;
		}

		if (node_p->node_kind==NormalNode){
			if (node_p->node_symbol->symb_kind==tuple_symb || 
				(node_p->node_symbol->symb_kind==definition && node_p->node_symbol->symb_def->sdef_kind==RECORDTYPE) ||
				(unsigned)(node_p->node_symbol->symb_kind-int_denot) <= (unsigned)(real_denot-int_denot))
					function_called_only_curried_or_lazy_with_one_return=1;
		} else {
			if (node_p->node_kind==SelectorNode || node_p->node_kind==UpdateNode)
				function_called_only_curried_or_lazy_with_one_return=1;
		}
	}

	if (rule_sdef->sdef_mark & SDEF_USED_CURRIED_MASK){
		struct label i_label;
		
		if (rule_sdef->sdef_mark & (SDEF_INSTANCE_RULE_WITH_FIELD_P | SDEF_RULE_INSTANCE_RULE_P) &&
			generate_instance_entry (rule_sdef,rule->rule_state_p,&i_label))
		{
			function_called_only_curried_or_lazy_with_one_return=0;
			ApplyInstanceEntry (rule->rule_state_p,rule_sdef->sdef_arity,&ea_lab,&i_label,!(rule_sdef->sdef_mark & SDEF_USED_LAZILY_MASK));
		} else
			ApplyEntry (rule->rule_state_p,rule_sdef->sdef_arity,&ea_lab,!(rule_sdef->sdef_mark & SDEF_USED_LAZILY_MASK));
	}

	if (rule_sdef->sdef_mark & SDEF_USED_LAZILY_MASK)
		if (rule->rule_mark & RULE_UNBOXED_LAZY_CALL){
			int args_a_size,args_b_size;
				
			DetermineSizeOfArguments (rule->rule_lazy_call_node->node_arguments,&args_a_size,&args_b_size);
			jmp_to_eval_args_entry = NodeEntryUnboxed (rule->rule_state_p,rule->rule_lazy_call_node,args_a_size,args_b_size,&ea_lab,rule_sdef);
		} else
			jmp_to_eval_args_entry = NodeEntry (rule->rule_state_p,rule_sdef->sdef_arity,&ea_lab,rule_sdef);
	else
		jmp_to_eval_args_entry = False;

	init_a_stack_top = asize;
	init_b_stack_top = bsize;

	root_node_needed = ! (IsOnBStack (resultstate) || 
						(IsSimpleState (resultstate) && resultstate.state_kind==StrictRedirection));

	a_stack_size_of_strict_entry=root_node_needed ? init_a_stack_top+1 : init_a_stack_top;
	
	CurrentAltLabel.lab_pref = s_pref;
	CurrentAltLabel.lab_post = 0;
	
	if (rule_sdef->sdef_exported){
		Bool ext_label_needed;
		LabDef extlab;

		extlab = CurrentAltLabel;
		extlab.lab_post = 0;
		CurrentAltLabel.lab_mod = NULL;
		
		if (rule_sdef->sdef_dcl_icl!=NULL){
			switch (rule_sdef->sdef_dcl_icl->sdef_kind){
				case DEFRULE:
				case SYSRULE:
					ext_label_needed = ConvertExternalToInternalCall (rule_sdef->sdef_arity,
								rule_sdef->sdef_dcl_icl->sdef_rule_type->rule_type_state_p,rule->rule_state_p,
								jmp_to_eval_args_entry,init_a_stack_top, init_b_stack_top, &ea_lab, &extlab, root_node_needed);
					break;
				default:
					ErrorInCompiler ("codegen.c","CodeRule","unknown kind of rewrite rule");
					break;
			}
		} else
			ext_label_needed=True;

		EvalArgsEntry (rule->rule_state_p,rule_sdef,maxasize,&ea_lab,root_node_needed ? 1 : 0);

		if (ext_label_needed){
			GenOStackLayoutOfStates (a_stack_size_of_strict_entry,init_b_stack_top,rule_sdef->sdef_arity,rule->rule_state_p);
			GenLabelDefinition (&extlab);
		}
	} else if (rule_sdef->sdef_mark & (SDEF_USED_CURRIED_MASK | SDEF_USED_LAZILY_MASK))
		if (rule->rule_mark & RULE_UNBOXED_LAZY_CALL)
			EvalArgsEntryUnboxed (rule,rule_sdef,asize,bsize,maxasize,&ea_lab,root_node_needed ? 1 : 0);
		else
			EvalArgsEntry (rule->rule_state_p,rule_sdef,maxasize,&ea_lab,root_node_needed ? 1 : 0);

	if ((rule->rule_mark & RULE_CAF_MASK) && ! (rule->rule_alts->alt_rhs_root->node_kind==NormalNode && 
		(unsigned)(rule->rule_alts->alt_rhs_root->node_symbol->symb_kind-int_denot) <= (unsigned)(real_denot-int_denot)))
	{
		LabDef caf_label,local_label;
		int a_size,b_size;
	
		GenOStackLayoutOfStates (a_stack_size_of_strict_entry,init_b_stack_top,rule_sdef->sdef_arity,rule->rule_state_p);
		GenLabelDefinition (&CurrentAltLabel);
	
		MakeLabel (&caf_label,rule_sdef->sdef_ident->ident_name,0,caf_pref);
		MakeLabel (&local_label,m_symb,NewLabelNr++,no_pref);

		DetermineSizeOfState (resultstate,&a_size,&b_size);

		GenTestCaf (&caf_label);
		GenJmpFalse (&local_label);

		GenPushCaf (&caf_label,a_size,b_size);
		
		if (root_node_needed){
			GenFillFromA (0,1,NormalFill);
			GenPopA (1);
		}
		GenRtn (a_size,b_size,resultstate);

		GenCaf (&caf_label,a_size,b_size);

		GenLabelDefinition	(&local_label);

		++CurrentAltLabel.lab_post;		

		GenDStackLayoutOfStates (a_stack_size_of_strict_entry,init_b_stack_top,rule_sdef->sdef_arity,rule->rule_state_p);
		GenJsr (&CurrentAltLabel);
		GenOStackLayoutOfState (a_size,b_size,resultstate);

		GenFillCaf (&caf_label,a_size,b_size);
		GenRtn (a_size,b_size,resultstate);
	}

#if 0
	if (rule_sdef->sdef_exported || rule_sdef->sdef_mark & SDEF_USED_STRICTLY_MASK || rule->rule_mark & RULE_CAF_MASK){
#endif

	if (!function_called_only_curried_or_lazy_with_one_return){
		GenOStackLayoutOfStates (a_stack_size_of_strict_entry,init_b_stack_top,rule_sdef->sdef_arity,rule->rule_state_p);
		GenLabelDefinition (&CurrentAltLabel);
	}

#if 0
	}
#endif

#if GENERATE_CODE_AGAIN
	call_code_generator_again=0;

	{
	struct saved_node_id_ref_counts *saved_node_id_ref_counts_p; 
	struct saved_case_node_id_ref_counts *saved_case_node_id_ref_counts_p; 

# if TAIL_CALL_MODULO_CONS_OPTIMIZATION
	if (OptimizeTailCallModuloCons && rule->rule_alts->alt_kind==Contractum && (rule->rule_mark & RULE_TAIL_MODULO_CONS_ENTRY_MASK)){
		tail_call_modulo_cons=1;

		if (ListOptimizations)
			printf ("Optimize tail call modulo cons of %s\n",rule_sdef->sdef_ident->ident_name);				
		call_code_generator_again=1;
	} else
		tail_call_modulo_cons=0;
# endif

# if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
	tail_call_modulo_tuple_cons=0;
	if (rule->rule_alts->alt_kind==Contractum){		
		int has_tuple_tail_call;
		
		global_same_select_vector=(unsigned long)-1l;
		has_tuple_tail_call=0;
		
		if (roots_are_tuples_or_calls_to_this_function_and_compute_same_select_vector (rule->rule_alts->alt_rhs_root,rule->rule_alts->alt_rhs_defs,rule_sdef,&global_same_select_vector,&has_tuple_tail_call) &&
			has_tuple_tail_call!=0)
		{
			/* printf ("%x\n",global_same_select_vector); */

			rule->rule_mark |= RULE_CALL_VIA_LAZY_SELECTIONS_ONLY;
			tail_call_modulo_tuple_cons=1;
		}
	}
# endif

# if OPTIMIZE_LAZY_TUPLE_RECURSION
	if (rule->rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY)
		call_code_generator_again=1;
# endif

	if (call_code_generator_again){
		struct saved_case_node_id_ref_counts **saved_case_node_id_ref_counts_h;
		
		saved_node_id_ref_counts_p=save_lhs_node_id_ref_counts (rule->rule_alts->alt_lhs_root,NULL);
		saved_case_node_id_ref_counts_h=&saved_case_node_id_ref_counts_p;
		saved_case_node_id_ref_counts_p=NULL;
		saved_node_id_ref_counts_p=save_rhs_node_id_ref_counts (rule->rule_alts->alt_rhs_root,rule->rule_alts->alt_rhs_defs,
																	saved_node_id_ref_counts_p,&saved_case_node_id_ref_counts_h);
	}
#endif

	if ((rule_sdef->sdef_mark & SDEF_INLINE_IS_CONSTRUCTOR)!=0){
		generate_is_constructor (rule);
		GenRtn (0, 1, BasicSymbolStates[bool_type]);
		rule_may_fail = False;
	} else
		rule_may_fail = CodeRuleAlt (rule->rule_alts,init_a_stack_top,init_b_stack_top,CurrentAltLabel.lab_post,resultstate);

	if (function_called_only_curried_or_lazy_with_one_return){
		StateS *function_state_p;
		
		function_state_p=rule->rule_state_p;
		
		if (IsSimpleState (function_state_p[-1])){
			if (function_state_p[-1].state_kind==OnB){
				if (rule_sdef->sdef_mark & SDEF_USED_LAZILY_MASK)
					FillBasicFromB (function_state_p[-1].state_object, 0, 0, ReleaseAndFill);
				else
					BuildBasicFromB (function_state_p[-1].state_object,0);

				GenPopB (ObjectSizes [function_state_p[-1].state_object]);
				GenRtn (1,0,OnAState);
			}
		} else {	
			int asize, bsize;

			DetermineSizeOfState (function_state_p[-1], &asize, &bsize);

			if (rule_sdef->sdef_mark & SDEF_USED_LAZILY_MASK){
				switch (function_state_p[-1].state_type){
					case TupleState:
						BuildTuple (asize,bsize,asize,bsize,function_state_p[-1].state_arity,
							function_state_p[-1].state_tuple_arguments,asize, bsize, 0, ReleaseAndFill,False);
						GenPopA (asize);
						break;
					case RecordState:
						BuildRecord (function_state_p[-1].state_record_symbol,asize, bsize, asize, bsize,
							asize, bsize, 0, ReleaseAndFill, False);
						GenPopA (asize);
						break;
					case ArrayState:
						GenFillArrayAndPop (1, ReleaseAndFill);
						break;
				}
				GenPopB (bsize);
			} else {
				switch (function_state_p[-1].state_type){
					case TupleState:
						BuildTuple (asize, bsize, asize, bsize, function_state_p[-1].state_arity,
							function_state_p[-1].state_tuple_arguments,asize,bsize, asize,NormalFill,True);
						GenUpdatePopA (0, asize);
						GenPopB (bsize);
						break;
					case RecordState:
						BuildNewRecordPop (function_state_p[-1].state_record_symbol,asize,bsize);
						break;
					case ArrayState:
						GenBuildArrayPop();
						break;
				}
			}
	
			GenRtn		(1,0,OnAState);
		}
		
		function_called_only_curried_or_lazy_with_one_return=0;
	}
	
	if (rule_may_fail){
		++CurrentAltLabel.lab_post;

		CurrentLine=rule->rule_alts->alt_line;
		if (FunctionMayFailWarningOrError!=0)
			StaticMessage (FunctionMayFailWarningOrError==2, "%S", "function may fail", CurrentSymbol);

		MatchError (asize,bsize,rule_sdef,root_node_needed,0);
	}

#if GENERATE_CODE_AGAIN
	if (call_code_generator_again)
		restore_node_id_ref_counts (saved_node_id_ref_counts_p,saved_case_node_id_ref_counts_p);
	}
#endif

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
	if (tail_call_modulo_cons)
		tail_call_modulo_cons=2;
#endif
#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
	if (tail_call_modulo_tuple_cons)
		tail_call_modulo_tuple_cons=2;
#endif

#if GENERATE_CODE_AGAIN
	if (
# if TAIL_CALL_MODULO_CONS_OPTIMIZATION
		tail_call_modulo_cons || 
# endif
		(rule->rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY)){
# if 0
		PrintImpRule (rule,4,StdOut);
# endif
		call_code_generator_again=0;
		
		CurrentAltLabel.lab_post=2;

#if OPTIMIZE_LAZY_TUPLE_RECURSION
		if (rule->rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY){
			int tuple_result_arity;
			StateS result_state_struct[1];
# if SELECTORS_FIRST
			LabDef reduce_error_label;
# endif

			tuple_result_arity=rule->rule_type->type_alt_rhs->type_node_arity;
# if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
			if (tail_call_modulo_tuple_cons){
				int i,n;
				
				n=tuple_result_arity;
				for (i=0; i<n; ++i)
					if (global_same_select_vector & (1<<i))
						--tuple_result_arity;
			}
# endif	

			GenFunctionDescriptorForLazyTupleRecursion (rule_sdef,tuple_result_arity);
						
			result_state_struct[0]=OnAState;
			
# if SELECTORS_FIRST
			{
			LabDef d_lab,n_lab;
			int a_size,b_size;

			ConvertSymbolToDandNLabel (&d_lab,&n_lab,rule_sdef);

			d_lab.lab_post = n_lab.lab_post = 3;
			
			if (rule->rule_mark & RULE_UNBOXED_LAZY_CALL){
				DetermineSizeOfArguments (rule->rule_lazy_call_node->node_arguments,&a_size,&b_size);
			} else {
				a_size=rule_sdef->sdef_arity;
				b_size=0;
			}
			b_size+=a_size;
			a_size=tuple_result_arity;
			
			if (b_size!=0)
				GenNodeEntryDirectiveUnboxed (a_size,b_size,&d_lab,NULL);
			else
				GenNodeEntryDirective (a_size,&d_lab,NULL);
			
			GenOAStackLayout (0);
			GenLabelDefinition (&n_lab);
			GenDAStackLayout (0);
			GenJmp (ReduceError);
			
			reduce_error_label=n_lab;
			/*
			reduce_error_label = CurrentAltLabel;
			reduce_error_label.lab_pref="n";
			reduce_error_label.lab_post=3;
			*/
			ReduceError = &reduce_error_label;
			}
# else
			ReduceError = &empty_lab;
# endif

			ea_lab.lab_post=2;
		
			if (rule->rule_mark & RULE_UNBOXED_LAZY_CALL){
				int args_a_size,args_b_size;
				
				DetermineSizeOfArguments (rule->rule_lazy_call_node->node_arguments,&args_a_size,&args_b_size);
				NodeEntryUnboxed (&result_state_struct[1],rule->rule_lazy_call_node,args_a_size + tuple_result_arity,args_b_size,&ea_lab,rule_sdef);
			} else
				NodeEntry (&result_state_struct[1],rule_sdef->sdef_arity + tuple_result_arity,&ea_lab,rule_sdef);

			if (DoParallel)
				ReduceError = &reserve_lab;
			else
				ReduceError = &cycle_lab;
			
# if SELECTORS_FIRST
			if (rule_sdef->sdef_arity!=0){
				int n;
				
				for (n=tuple_result_arity; n!=0; --n)
					GenPushA (tuple_result_arity-1);
				
				for (n=0; n<rule_sdef->sdef_arity; ++n)
					GenUpdateA (n+tuple_result_arity+tuple_result_arity,n+tuple_result_arity);
				
				for (n=0; n<tuple_result_arity; ++n)
					GenUpdateA (n,n+tuple_result_arity+rule_sdef->sdef_arity);
					
				GenPopA (tuple_result_arity);
			}
# endif

			CurrentAltLabel.lab_pref = s_pref;
			if (rule->rule_mark & RULE_UNBOXED_LAZY_CALL)
				EvalArgsEntryUnboxed (rule,rule_sdef,asize,bsize,maxasize,&ea_lab,tuple_result_arity);
			else
				EvalArgsEntry (rule->rule_state_p,rule_sdef,maxasize,&ea_lab,tuple_result_arity);

			GenOStackLayoutOfStates (a_stack_size_of_strict_entry + tuple_result_arity,init_b_stack_top,rule_sdef->sdef_arity,rule->rule_state_p);
			
			init_a_stack_top += tuple_result_arity;
			lazy_tuple_recursion=1;
		} else
#endif
			GenOStackLayoutOfStates (a_stack_size_of_strict_entry,init_b_stack_top,rule_sdef->sdef_arity,rule->rule_state_p);

		CurrentAltLabel.lab_pref = s_pref;
		GenLabelDefinition (&CurrentAltLabel);

		if (CodeRuleAlt (rule->rule_alts,init_a_stack_top,init_b_stack_top,CurrentAltLabel.lab_post,resultstate)){
			++CurrentAltLabel.lab_post;
					
			CurrentLine=rule->rule_alts->alt_line;	
			MatchError (asize,bsize,rule_sdef,root_node_needed,1);
		}

#if OPTIMIZE_LAZY_TUPLE_RECURSION
		lazy_tuple_recursion=0;
#endif
	}	
#endif

	if (DoTimeProfiling)
		GenPE();
}

void CodeGeneration (ImpMod imod, char *fname)
{
	if (! CompilerError){
		int DoStrictnessAnalysis_and_init_ok;

		CurrentPhase = NULL;

#if 0
		PrintRules (imod->im_rules);
#endif
		DetermineSharedAndAnnotatedNodes (imod->im_rules,&imod->im_symbols);
		ExitOnInterrupt();

#if 0
		PrintRules (imod->im_rules,rules_file);
#endif

		GenerateStatesForRecords (imod->im_symbols);

		DoStrictnessAnalysis_and_init_ok = DoStrictnessAnalysis && init_strictness_analysis (imod);
		
		if (DoStrictnessAnalysis_and_init_ok){
			do_strictness_analysis();
			ExitOnInterrupt();
		}

		ExamineTypesAndLhsOfSymbols	(imod->im_symbols);

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		{
			ImpRuleP rule_p;

			for_l (rule_p,imod->im_rules,rule_next)
				determine_failing_cases_and_adjust_ref_counts_of_rule (rule_p->rule_alts);
		}
#endif

		optimise_strict_tuple_result_functions=DoStrictnessAnalysis;

		generate_states (imod->im_rules,True);

		if (DoStrictnessAnalysis_and_init_ok){
			ExitOnInterrupt();
		
			finish_strictness_analysis();		
		}
		ExitOnInterrupt();

#if 0
		PrintRules (imod->im_rules);
#endif

		optimise_strict_tuple_result_functions=0;

		OptimiseRules (imod->im_rules,imod->im_start);
		ExitOnInterrupt();
#if 0
		PrintRules (imod->im_rules,rules_file);
#endif
		if (DoCode && !CompilerError){
			ImpRuleS *rule;

			Verbose ("Code generation");

			if (!OpenABCFile (fname)){
				StaticMessage (True, "<open file>","Can't create abc file '%s'",fname);
				return;
			}
			
			InitFileInfo (imod);

			if (DoParallel)
				ReduceError = &reserve_lab;
			else
				ReduceError = &cycle_lab; /* in sequential case we have no reservation mechanism */

			GenDependencyList();
#if IMPORT_OBJ_AND_LIB
			{
				struct string_list *sl;
				
				for_l (sl,imod->im_imported_objs,sl_next)
					GenImpObj (sl->sl_string);
				for_l (sl,imod->im_imported_libs,sl_next)
					GenImpLib (sl->sl_string);
			}
#endif

#if WRITE_DCL_MODIFICATION_TIME
			if (WriteModificationTimes){
				GenModuleDescriptor (imod->im_modification_time);
				GenEndInfo();
			} else {
				GenEndInfo();
				GenModuleDescriptor (imod->im_modification_time);
			}
#else
			GenEndInfo();
			GenModuleDescriptor();
#endif
			GenSystemImports();
			FileComment();
			ExitOnInterrupt();

			ReadInlineCode ();

			CreateStackFrames();

			ImportSymbols (imod->im_symbols);

			GenerateCodeForConstructorsAndRecords (imod->im_symbols);

			GenerateForeignExports (imod->im_foreign_exports);

			if (imod->im_start)
				GenStart (imod->im_start);
			ExitOnInterrupt ();

#if SHARE_UPDATE_CODE
			create_result_state_database (imod->im_rules);
#endif

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
			if (OptimizeTailCallModuloCons)
				for_l (rule,imod->im_rules,rule_next)
					if (rule->rule_alts->alt_kind==Contractum){
						CurrentSymbol=rule->rule_root->node_symbol;
						
						if (does_tail_call_modulo_cons (rule->rule_alts->alt_rhs_root,rule->rule_alts->alt_rhs_defs))
							rule->rule_mark |= RULE_TAIL_MODULO_CONS_ENTRY_MASK;
					}
#endif

			update_function_p=&first_update_function;
			for_l (rule,imod->im_rules,rule_next){
				CodeRule (rule);

				*update_function_p=NULL;
				if (first_update_function){
					while (first_update_function){
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
						determine_failing_cases_and_adjust_ref_counts_of_rule (first_update_function->rule_alts);
#endif
						CodeRule (first_update_function);
						
						first_update_function=first_update_function->rule_next;
					}
					update_function_p=&first_update_function;						
				}

				ExitOnInterrupt ();
			}

			GenerateCodeForLazyTupleSelectorEntries (LazyTupleSelectors);
			GenerateCodeForLazyArrayFunctionEntries();
#if STRICT_LISTS
			GenerateCodeForLazyUnboxedRecordListFunctions();
#endif

			import_not_yet_imported_record_r_labels (imod->im_symbols);
			import_not_yet_imported_system_labels();

			WriteLastNewlineToABCFile();

			CloseABCFile (fname);
		}
	}
}
