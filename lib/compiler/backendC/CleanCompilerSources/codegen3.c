/*
	File:    codegen3.c
	Authors: Sjaak Smetsers & John van Groningen
*/

#define FASTER_STRICT_IF /* also in statesgen.c */
#define DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)
#define for_li(v,i,l,n) for(v=(l),i=0;v!=NULL;v=v->n,++i)
#define for_ll(v1,v2,l1,l2,n1,n2) for(v1=(l1),v2=(l2);v1!=NULL;v1=v1->n1,v2=v2->n2)

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "syntaxtr.t"
#include "comsupport.h"

#include "codegen_types.h"
#include "statesgen.h"
#include "optimisations.h"
#include "codegen.h"
#include "codegen1.h"
#include "codegen2.h"

#if GENERATE_CODE_AGAIN
struct saved_node_id_ref_counts {
	NodeIdP								snir_node_id;
	int 								snir_ref_count;
	struct saved_node_id_ref_counts *	snir_next;
};

struct saved_case_node_id_ref_counts {
	NodeIdRefCountListP						scnir_nrcl;
	int 									scnir_ref_count;
	struct saved_case_node_id_ref_counts *	scnir_next;
};
#endif

#include "codegen3.h"
#include "instructions.h"
#include "sizes.h"
#include "statesgen.h"
#include "settings.h"
#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
# include "buildtree.h"
#endif
#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
# include "tuple_tail_recursion.h"
#endif

static void error_in_function (char *m)
{
	ErrorInCompiler ("codegen3.c",m,"");
}

static void UpdateAAndBStack (int aindex,int bindex,int asize,int bsize,int *asp_p,int *bsp_p)
{
	int i,asp,bsp;
#if UPDATE_POP
	int a_popped,b_popped;
	
	a_popped=0;
	b_popped=0;
#endif	
	asp=*asp_p;
	bsp=*bsp_p;
	
	/* copy the values in the right order ! */
	if (aindex < asize){
		for (i=0; i<asize; i++)
			GenUpdateA (asp - (aindex - i), asp - (asize - i));
	} else if (aindex > asize){
		for (i=asize - 1; i >= 0; i--)
#if UPDATE_POP
			if (i==0){
				GenUpdatePopA (asp - aindex, asp - asize);
				a_popped=1;
			} else
#endif
			GenUpdateA (asp - (aindex - i), asp - (asize - i));
	}

	if (bindex < bsize){
		for (i=0; i<bsize; i++)
			GenUpdateB (bsp - (bindex - i), bsp - (bsize - i));
	} else if (bindex > bsize){
		for (i=bsize - 1; i >= 0; i--)
#if UPDATE_POP
			if (i==0){
				GenUpdatePopB (bsp - bindex, bsp - bsize);
				b_popped=1;
			} else
#endif
			GenUpdateB (bsp - (bindex - i), bsp - (bsize - i));
	}
	
#if UPDATE_POP
	if (!a_popped)
#endif
		GenPopA (asp-asize);

	*asp_p=asize;
	
#if UPDATE_POP
	if (!b_popped)
#endif
		GenPopB (bsp-bsize);
	
	*bsp_p=bsize;
}

void RedirectResultAndReturn (int asp,int bsp,int source_a_index,int source_b_index,StateS offstate,StateS demstate,int offasize,int offbsize)
{
	if (IsSimpleState (offstate)){
		if (IsSimpleState (demstate)){
			switch (CoerceStateKind (demstate.state_kind, offstate.state_kind)){
				case Reduce:
					if (demstate.state_kind==StrictRedirection){
#if UPDATE_POP
						GenUpdatePopA (asp-source_a_index, asp - 1);
#else
						GenUpdateA (asp-source_a_index, asp - 1);
						GenPopA (asp - 1);
#endif
						GenPopB (bsp);
						GenJmpEval ();

						return;
					} else {
						Coercions c;
						
						c=CoerceStateKind (demstate.state_kind,StrictOnA);
						
						if (c==AToA || c==AToRoot){
							GenPopB (bsp);
							
							if (source_a_index==0){
								GenPopA (asp);
								GenJmpEval ();

								return;
							} else {
#if UPDATE_POP
								GenUpdatePopA (asp-source_a_index, asp - 1);
#else
								GenUpdateA (asp-source_a_index, asp - 1);
								GenPopA (asp - 1);
#endif

#if ABSTRACT_OBJECT
								if (demstate.state_object!=AbstractObj)
									GenJmpEvalUpdate();
								else {
									GenJsrEval (0);
									GenFillFromA (0, 1, ReleaseAndFill);
									GenPopA (1);
					 				GenRtn (1,0, OnAState);
					 			}
#else
								GenJmpEvalUpdate();
#endif
								return;
							}
						} else {
							GenPopB (bsp);
							GenPopA (asp-source_a_index);
							GenJsrEval (0);
							PushBasicFromAOnB (demstate.state_object, 0);
							GenPopA (source_a_index);
						}
					}
					break;
				case AToB:
					GenPopB (bsp);
					PushBasicFromAOnB (demstate.state_object, asp-source_a_index);
					GenPopA (asp);
					break;
				case BToA:
					GenPopA (asp);
#if STORE_STRICT_CALL_NODES
					if (demstate.state_kind==StrictRedirection){
						BuildBasicFromB (offstate.state_object,bsp-source_b_index);
						++asp;
					} else
#endif
					FillBasicFromB (offstate.state_object,bsp-source_b_index,0,ReleaseAndFill);
					GenPopB (bsp);
					break;
				case BToB:
				{
					int bsize;
					
					bsize = ObjectSizes [demstate.state_object];
					UpdateBasic (bsize,bsp-source_b_index,bsp-bsize);
					GenPopA (asp);
					GenPopB (bsp-bsize);
					break;
				}
				case AToA:
				case AToRoot:
					GenPopB (bsp);
					if (demstate.state_kind==StrictRedirection){
#if UPDATE_POP
						GenUpdatePopA (asp-source_a_index, asp-1);
#else
						GenUpdateA (asp-source_a_index, asp-1);
						GenPopA (asp-1);
#endif
					} else {
						GenFillFromA (asp-source_a_index, asp, ReleaseAndFill);
						GenPopA (asp);
					}
					break;
				case CyclicSpine:
					GenReduceError ();
					StaticMessage (False,CurrentAltLabel.lab_symbol->sdef_ident->ident_name,Co_Wspine);
					break;
				default:
					error_in_function ("RedirectResultAndReturn");
					return;
			}
		} else {
			GenPopB (bsp);

			switch (CoerceStateKind (StrictOnA, offstate.state_kind)){
				case Reduce:
					GenJsrEval (asp-source_a_index);
				default:
#if UPDATE_POP
					GenUpdatePopA (asp-source_a_index, asp-1);
#else
					GenUpdateA (asp-source_a_index, asp-1);
					GenPopA (asp-1);
#endif
					asp = 1;
	
					switch (demstate.state_type){
						case TupleState:
							UnpackTuple (0,&asp,&bsp,True,demstate.state_arity,demstate.state_tuple_arguments);
							break;
						case RecordState:
							UnpackRecord (0,&asp,&bsp,True,demstate.state_arity, demstate.state_record_arguments);
							break;
						case ArrayState:
							UnpackArray (0,&asp,True);
							break;
					}
			}
		}
	} else if (IsSimpleState (demstate)){
		if (demstate.state_kind==StrictRedirection){
			switch (offstate.state_type){
				case TupleState:
					BuildTuple (source_a_index,source_b_index,asp,bsp,
						offstate.state_arity, offstate.state_tuple_arguments,
						offasize, offbsize, 0, ReleaseAndFill,True);
					GenUpdatePopA (0,asp);
					GenPopB (bsp);
					break;
				case RecordState:
					if (source_a_index==asp && (source_b_index==bsp || offbsize==0))
						BuildNewRecordPop (offstate.state_record_symbol,offasize,offbsize);
					else {
						BuildNewRecord (offstate.state_record_symbol,source_a_index,source_b_index,asp,bsp,offasize,offbsize);
						GenUpdatePopA (0,asp);
						GenPopB (bsp);
					}
					break;
				case ArrayState:
					if (asp==source_a_index && asp==1)
						GenBuildArrayPop();
					else {
						GenBuildArray (asp-source_a_index);
						GenUpdatePopA (0,asp);
					}
					GenPopB (bsp);
			}
		} else {
			switch (offstate.state_type){
				case TupleState:
					BuildTuple (source_a_index,source_b_index,asp,bsp,
						offstate.state_arity, offstate.state_tuple_arguments,
						offasize, offbsize, 0, ReleaseAndFill,False);
					GenPopA (asp);
					break;
				case RecordState:
					BuildRecord (offstate.state_record_symbol,source_a_index,source_b_index, asp, bsp,
						offasize, offbsize, 0, ReleaseAndFill,False);
					GenPopA (asp);
					break;
				case ArrayState:
					if (asp==source_a_index && asp==1)
						GenFillArrayAndPop (asp,ReleaseAndFill);
					else {
						GenFillArray (asp-source_a_index,asp,ReleaseAndFill);
						GenPopA (asp);
					}
			}
			GenPopB (bsp);
		}
	} else {
		switch (demstate.state_type){
			case RecordState:
			{	
				int asize, bsize;
			
				DetermineSizeOfStates (demstate.state_arity, demstate.state_record_arguments,&asize, &bsize);
				UpdateAAndBStack (source_a_index,source_b_index, asize, bsize,&asp,&bsp);
				break;
			}	
			case TupleState:
				if (EqualState (demstate, offstate)){
					int asize, bsize;
					
					DetermineSizeOfStates (demstate.state_arity,demstate.state_tuple_arguments,&asize, &bsize);
					UpdateAAndBStack (source_a_index,source_b_index, asize, bsize,&asp,&bsp);
				} else {
					GenPopA (asp-source_a_index);
					GenPopB (bsp-source_b_index);
					asp = source_a_index;
					bsp = source_b_index;
					AdjustTuple (source_a_index,source_b_index, & asp, & bsp,
						demstate.state_arity,
						demstate.state_tuple_arguments,
						offstate.state_tuple_arguments, offasize, offbsize);
				}
				break;
			case ArrayState:
#if UPDATE_POP
				GenUpdatePopA (asp-source_a_index, asp - 1);
#else
				GenUpdateA (asp-source_a_index, asp - 1);
				GenPopA (asp - 1);
#endif
				GenPopB (bsp);
				break;
		}
	}

	if (!function_called_only_curried_or_lazy_with_one_return){
		int asize,bsize;
		
		DetermineSizeOfState (demstate,&asize,&bsize);
		GenRtn (asize, bsize, demstate);
	}
}

static void CodeRedirection (NodeId node_id,int asp,int bsp,StateS demstate,NodeIdListElementS **free_node_ids_l)
{
	int asize,bsize;
	int a_index,b_index;
	StateS offstate;
	
	offstate = node_id->nid_state;
	
	DetermineSizeOfState (offstate,&asize,&bsize);
	RedirectionComment (node_id);

	if (node_id->nid_refcount<0 && node_id->nid_state.state_type!=SimpleState && node_id->nid_node!=NULL){
		if (asize!=0)
			a_index=get_a_index_of_unpacked_lhs_node (node_id->nid_node->node_arguments);
		else
			a_index=0;
		
		if (bsize!=0)
			b_index=get_b_index_of_unpacked_lhs_node (node_id->nid_node->node_arguments);
		else
			b_index=0;		
	} else {
		a_index=node_id->nid_a_index;
		b_index=node_id->nid_b_index;
	}

	RedirectResultAndReturn (asp,bsp,a_index,b_index,offstate,demstate,asize,bsize);

	decrement_reference_count_of_node_id (node_id,free_node_ids_l);
}

static void FillRhsRoot (Label name,Node root,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p)
{
	BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);

	GenFillh (name,root->node_arity,asp,ReleaseAndFill);
	asp-=root->node_arity;

	GenPopA	(asp);
	GenPopB	(bsp);
	GenRtn (1,0,OnAState);
}

static void CreateSemiStrictRootNode (Label name,Label code,Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS demstate)
{
	BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);
	
	GenFill (name,root->node_arity,code,asp-rootid->nid_a_index, NormalFill);
	asp-=root->node_arity;

	RedirectResultAndReturn (asp,bsp,rootid->nid_a_index,rootid->nid_b_index,rootid->nid_state, demstate, 1, 0);
}

#define IsSemiStrictState(state) ((state).state_type==SimpleState && (state).state_kind==SemiStrict)

static Bool NoTupleStateAdjustment (StateS st1,StateS st2)
{
	if (IsSimpleState (st1) && IsSimpleState (st2))
		return st1.state_kind==st2.state_kind || (st1.state_kind==OnA && st2.state_kind==StrictOnA);

	switch (st1.state_type){
		case RecordState:
			return st2.state_type==RecordState;
		case TupleState:
			if (st2.state_type==TupleState && st1.state_arity==st2.state_arity){
				int i;
				
				for (i=0; i<st1.state_arity; i++)
					if (!NoTupleStateAdjustment (st1.state_tuple_arguments[i],st2.state_tuple_arguments[i]))
						return False;
				
				return True;
			} else
				return False;
		case ArrayState:
			return st2.state_type==ArrayState;
		default:
			return False;
	}
}

static Coercions DetermineResultAdjustment (StateS demstate, StateS offstate)
{
	if (IsSimpleState (offstate)){
		if (IsSimpleState (demstate))
			return CoerceStateKind (demstate.state_kind,offstate.state_kind);
		else
			return AToB;
	} else if (IsSimpleState (demstate) || ! NoTupleStateAdjustment (demstate, offstate))
		return BToA;
	else
		return BToB;
}

static Bool ResultNodeNecessary (Coercions moveact, StateS offstate)
{
	return (moveact == AToB && ! (IsSimpleState (offstate) &&
		(offstate.state_kind == StrictRedirection ||
		 offstate.state_kind == LazyRedirection)));
}

static void CodeRootSymbolApplication (Node root,NodeId rootid,SymbDef def,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS resultstate)
{
	LabDef name;
	int symbarity;

	symbarity = def->sdef_arity;
	
	if (symbarity==root->node_arity){
		SDefKind symbkind;
		
		symbkind = (SDefKind)def->sdef_kind;

		switch (symbkind){
			case IMPRULE:
			case DEFRULE:
			case SYSRULE:
				if (IsSemiStrictState (root->node_state)){
					LabDef codelab;
	
					ConvertSymbolToDandNLabel (&name,&codelab,def);

					CreateSemiStrictRootNode (&name,&codelab,root,rootid,asp,bsp,code_gen_node_ids_p,resultstate);
				} else {
					Coercions moveact;

					ConvertSymbolToLabel (&name,def);
					
					moveact = DetermineResultAdjustment (resultstate, root->node_state);
					
					/*
						removal of tail recursion only makes sence when we are sure
						that at run-time after calling the rhs root function
						it is not necessary to return to the calling function
					*/
		
					if (moveact==AToB || moveact==BToA || moveact==AToRoot){
						int result_a_size,result_b_size,new_node;
						int a_size,b_size;

						/* In this case no removal takes place */
						
						new_node=ResultNodeNecessary (moveact,root->node_state);
						if (new_node)
							NewEmptyNode (&asp,-1);
						
						BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);

						DetermineSizeOfArguments (root->node_arguments,&a_size,&b_size);
						if (new_node)
							++a_size;
						UpdateAAndBStack (asp,bsp,a_size,b_size,&asp,&bsp);
						CallFunction (&name,def,True,root);
					
						DetermineSizeOfState (root->node_state,&result_a_size,&result_b_size);

						asp+=result_a_size-a_size;
						bsp+=result_b_size-b_size;
						
						RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,resultstate,result_a_size,result_b_size);
					} else {						
						int a_size,b_size;

						/* BuildNewStackFrame (root->node_arguments,asp,bsp,ResultNodeNecessary (moveact,root->node_state),code_gen_node_ids_p); */
						
						BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);
						DetermineSizeOfArguments (root->node_arguments,&a_size,&b_size);
						UpdateAAndBStack (asp,bsp,a_size,b_size,&asp,&bsp);
								
						CallFunction (&name, def, False, root);
					}
				}
				break;
			case RECORDTYPE:
				if (IsSemiStrictState (root->node_state)){
					LabDef codelab;
	
					if (def->sdef_strict_constructor){
						ConvertSymbolToRecordDandNLabel (&name,&codelab,def);
						CreateSemiStrictRootNode (&name,&codelab,root,rootid,asp,bsp,code_gen_node_ids_p,resultstate);
					} else {
						ConvertSymbolToRLabel (&codelab,def);
												
						BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);

						GenFillR (&codelab,root->node_arity,0,asp-rootid->nid_a_index,0,0,NormalFill,True);
						asp-=root->node_arity;
						
						rootid->nid_state__.state_kind=StrictOnA;
						
						RedirectResultAndReturn (asp,bsp,rootid->nid_a_index,rootid->nid_b_index,rootid->nid_state, resultstate, 1, 0);
					}
				} else {
					int a_size,b_size;
	
					BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);

					DetermineSizeOfArguments (root->node_arguments,&a_size,&b_size);
					
					if (IsSimpleState (root->node_state)){
						LabDef codelab;

						ConvertSymbolToRLabel (&codelab,def);
						GenFillR (&codelab,a_size,b_size,asp,0,0,ReleaseAndFill,False);

						GenPopA	(asp);
						GenPopB	(bsp);
						GenRtn (1,0,OnAState);
					} else {
						/*BuildNewStackFrame (root->node_arguments,asp,bsp,ResultNodeNecessary (BToB,root->node_state),code_gen_node_ids_p); */
						
						UpdateAAndBStack (asp,bsp,a_size,b_size,&asp,&bsp);
						
						if (!function_called_only_curried_or_lazy_with_one_return){
							int asize,bsize;
							
							DetermineSizeOfState (resultstate, &asize, &bsize);
							GenRtn (asize, bsize, resultstate);
						}
					}
				}
				break;
			default: /* a USER or a TYPE constructor */
				if (def->sdef_kind==CONSTRUCTOR && def->sdef_strict_constructor && def->sdef_arity==root->node_arity){
					if (IsSemiStrictState (root->node_state)){
						LabDef codelab;
						
						ConvertSymbolToConstructorDandNLabel (&name,&codelab,def);
						CreateSemiStrictRootNode (&name,&codelab,root,rootid,asp,bsp,code_gen_node_ids_p,resultstate);
					} else {
						LabDef record_label;
						int asize,bsize;
						
						DetermineSizeOfArguments (root->node_arguments,&asize,&bsize);
						BuildArgs (root->node_arguments, &asp, &bsp,code_gen_node_ids_p);
	
						ConvertSymbolToKLabel (&record_label,def);
						
						GenFillR (&record_label,asize,bsize,asp,0,0,ReleaseAndFill,False);
	
						GenPopA	(asp);
						GenPopB	(bsp);
						GenRtn (1,0, OnAState);
					}
				} else {
					if (def->sdef_kind==CONSTRUCTOR)
						ConvertSymbolToConstructorDLabel (&name,def);
					else
						ConvertSymbolToDLabel (&name,def);
					FillRhsRoot (&name, root, asp, bsp,code_gen_node_ids_p);
				}
				break;
		}
	} else {
		/* Symbol has too few arguments */
		if (def->sdef_kind==CONSTRUCTOR)
			ConvertSymbolToConstructorDLabel (&name,def);
		else
			ConvertSymbolToDLabel (&name,def);
		FillRhsRoot (&name, root, asp, bsp,code_gen_node_ids_p);
	}
}

#ifdef NEW_APPLY
extern int build_apply_arguments (ArgP node_args,int *a_size_p,int *b_size_p,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);

static void CodeRootApply (Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS resultstate)
{
	if (IsSemiStrictState (root->node_state)){
		LabDef name,codelab;

		ConvertSymbolToDandNLabel (&name,&codelab,ApplyDef);

		CreateSemiStrictRootNode (&name,&codelab,root,rootid,asp,bsp,code_gen_node_ids_p,resultstate);
	} else {
		int a_size,b_size,n_apply_args,no_tail_call;

		a_size=0;

		if (OptimizeInstanceCalls && root->node_symbol->symb_instance_apply==1){
			Coercions moveact;

			moveact = DetermineResultAdjustment (resultstate, root->node_state);
			if (moveact==AToB || moveact==BToA || moveact==AToRoot){
				no_tail_call=1;
				
				if (ResultNodeNecessary (moveact,root->node_state)){
					NewEmptyNode (&asp,-1);
					a_size=1;
				}
			} else
				no_tail_call=0;
		} else
			no_tail_call = !IsSimpleState (resultstate) || resultstate.state_kind!=StrictRedirection;

		b_size=0;
		n_apply_args=build_apply_arguments (root->node_arguments,&a_size,&b_size,&asp,&bsp,code_gen_node_ids_p);

		UpdateAAndBStack (asp,bsp,a_size,b_size,&asp,&bsp);

		if (root->node_symbol->symb_instance_apply==1){
			struct arg *arg_p;
			struct symbol_def *field_sdef;

			field_sdef = (struct symbol_def *)root->node_symbol->symb_next;

			arg_p=root->node_arguments;
			while (arg_p!=NULL && arg_p->arg_node->node_kind==NormalNode && arg_p->arg_node->node_symbol->symb_kind==apply_symb)
				arg_p=arg_p->arg_node->node_arguments;
			if (arg_p!=NULL && arg_p->arg_node->node_kind==SelectorNode && arg_p->arg_node->node_arity==1){
				struct node *selector_node_p;
				
				selector_node_p=arg_p->arg_node;
				if ((selector_node_p->node_symbol->symb_def->sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE)!=0){
					struct type_alt *member_type_alt;
					
					field_sdef=selector_node_p->node_symbol->symb_def;
					member_type_alt=field_sdef->sdef_member_type_of_field;
					if (DoDebug)
						if (member_type_alt->type_alt_lhs->type_node_arity==n_apply_args+1){
							FPrintF (OutFile, "\n||\t%d %s",root->node_symbol->symb_instance_apply,selector_node_p->node_symbol->symb_def->sdef_ident->ident_name);
						} else
							FPrintF (OutFile, "\n||\t(no dictionary) %s",field_sdef->sdef_ident->ident_name);
				} else if (DoDebug)
					FPrintF (OutFile, "\n||\t(no dictionary) %s",field_sdef->sdef_ident->ident_name);
			} else if (DoDebug)
				FPrintF (OutFile, "\n||\t(no dictionary) %s",field_sdef->sdef_ident->ident_name);

			if (field_sdef!=NULL && OptimizeInstanceCalls){
				struct state *member_states_of_field;
				int member_arity,member_called_with_root_node;
				int a_size,b_size;

				member_states_of_field=field_sdef->sdef_member_states_of_field;
				member_arity=field_sdef->sdef_member_type_of_field->type_alt_lhs->type_node_arity;

				member_called_with_root_node = member_states_of_field[-1].state_type==SimpleState
												&& !(member_states_of_field[-1].state_kind==StrictRedirection || member_states_of_field[-1].state_kind==OnB);

				DetermineSizeOfStates (member_arity-1,&member_states_of_field[1],&a_size,&b_size);
				GenDStackLayoutOfStates (a_size+1+member_called_with_root_node,b_size,member_arity-1,&member_states_of_field[1]);

				if (no_tail_call){
					int result_a_size,result_b_size,jsr_i_result_a_size,jsr_i_result_b_size;

					GenJsrI (n_apply_args);

					DetermineSizeOfState (member_states_of_field[-1],&jsr_i_result_a_size,&jsr_i_result_b_size);
					GenOStackLayoutOfState (jsr_i_result_a_size,jsr_i_result_b_size,member_states_of_field[-1]);				

					DetermineSizeOfState (root->node_state,&result_a_size,&result_b_size);

					asp+=result_a_size-(a_size+1+member_called_with_root_node);
					bsp+=result_b_size-b_size;

					RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,resultstate,result_a_size,result_b_size);
				} else
					GenJmpI (n_apply_args);

				return;
			}
		}

		if (no_tail_call){
			int result_a_size,result_b_size;

			GenJsrAp (n_apply_args);

			DetermineSizeOfState (root->node_state,&result_a_size,&result_b_size);

			asp+=result_a_size-a_size;
			bsp+=result_b_size-b_size;

			RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,resultstate,result_a_size,result_b_size);
		} else
			GenJmpAp (n_apply_args);
	}
}
#endif

static void CodeRootSelection (Node root, NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS demstate)
{
	Args args;
	int	argnr;

	args=root->node_arguments;
	argnr=root->node_arity;

	if (IsSemiStrictState (root->node_state)){
		FillSelectSymbol (SemiStrict,root->node_symbol->symb_arity,argnr,args,&asp,&bsp,rootid,code_gen_node_ids_p);

		RedirectResultAndReturn (asp,bsp,rootid->nid_a_index,rootid->nid_b_index,rootid->nid_state,demstate,1,0);
		return;
	} else {
		Node arg_node;
		int tuparity;
		
		tuparity = args->arg_state.state_arity;

		Assume (tuparity > 1, "codegen","CodeRootSelection");

		arg_node=args->arg_node;
		if (arg_node->node_kind!=NodeIdNode){
			StateS offstate;
			
			offstate= arg_node->node_state;
			
			Build (arg_node,&asp,&bsp,code_gen_node_ids_p);
			
			if (IsSimpleState (offstate)){
				GenPushArg (0, tuparity, argnr);
				asp += 1;

				RedirectResultAndReturn (asp,bsp,asp,0,OnAState,demstate,1,0);
				return;
			} else {	
				int i,a_offset,b_offset,asize,bsize;
							
				a_offset=0;
				b_offset=0;
				for (i=0; i<argnr-1; ++i)
					AddSizeOfState (offstate.state_tuple_arguments[i],&a_offset,&b_offset);

				DetermineSizeOfState (offstate.state_tuple_arguments[argnr-1],&asize,&bsize);

				RedirectResultAndReturn (asp,bsp,asp-a_offset,bsp-b_offset,offstate.state_tuple_arguments[argnr-1],demstate,asize,bsize);
				return;
			}
		} else {
			StateS offstate;
			NodeId arg_node_id;
		
			arg_node_id=arg_node->node_node_id;
			offstate = arg_node_id->nid_state;
			
			if (IsSimpleState (offstate)){
				Bool ontop;
				
				CoerceSimpleStateArgument (demstate, offstate.state_kind, arg_node_id->nid_a_index, & asp, False, & ontop);
				
				GenPushArg (asp - arg_node_id->nid_a_index, tuparity, argnr);
				asp += 1;
				RedirectResultAndReturn (asp, bsp, asp, 0, OnAState, demstate, 1, 0);
				return;
			} else {
				int i,asize,bsize,aindex,bindex,tuple_a_index,tuple_b_index;
				
				aindex=0;
				bindex=0;
				for (i=0; i<argnr-1; i++)
					AddSizeOfState (offstate.state_tuple_arguments[i],&aindex, &bindex);

				if (arg_node_id->nid_refcount<0 && arg_node_id->nid_node!=NULL){
					tuple_a_index=get_a_index_of_unpacked_lhs_node (arg_node_id->nid_node->node_arguments);
					tuple_b_index=get_b_index_of_unpacked_lhs_node (arg_node_id->nid_node->node_arguments);
				} else {
					tuple_a_index=arg_node_id->nid_a_index,
					tuple_b_index=arg_node_id->nid_b_index;
				}
				
				DetermineSizeOfState (offstate.state_tuple_arguments[argnr-1],&asize,&bsize);

				aindex=tuple_a_index-aindex;
				bindex=tuple_b_index-bindex;
								
				RedirectResultAndReturn (asp,bsp,aindex,bindex,offstate.state_tuple_arguments[argnr-1],demstate,asize,bsize);
			}
		}
	}
}

static int CodeRootNode (Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS resultstate,struct esc *esc_p);

static int CodeRhsNodeDefsAndRestoreNodeIdStates (Node root_node,NodeDefs defs,int asp,int bsp,StateS resultstate,struct esc *esc_p,
												NodeIdListElementP a_node_ids,NodeIdListElementP b_node_ids,
												NodeIdListElementP free_node_ids,int doesnt_fail)
{
	SavedNidStateP saved_node_id_states;
	int need_next_alternative;
	
	saved_node_id_states=NULL;
		
	need_next_alternative=CodeRhsNodeDefs (root_node,defs,asp,bsp,&saved_node_id_states,resultstate,esc_p,a_node_ids,b_node_ids,
											free_node_ids,doesnt_fail);

	restore_saved_node_id_states (saved_node_id_states);

	return need_next_alternative;
}

#define BETWEEN(l,h,v) ((unsigned)((v)-(l)) <= (unsigned)((h)-(l)))

#if STRICT_LISTS
extern int simple_expression_without_node_ids (NodeP node_p);
#endif

static void CodeNormalRootNode (Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS resultstate)
{
	Symbol rootsymb;

	rootsymb = root->node_symbol;

	ContractumComment (asp, bsp);
	
	switch (rootsymb->symb_kind){
		case definition:
			CodeRootSymbolApplication (root,rootid,rootsymb->symb_def,asp,bsp,code_gen_node_ids_p,resultstate);
			return;
		case tuple_symb:
			if (IsSemiStrictState (root->node_state))
				CreateSemiStrictRootNode (&tuple_lab,&hnf_lab,root,rootid,asp,bsp,code_gen_node_ids_p,resultstate);
			else {
				if (IsSimpleState (root->node_state))
					FillRhsRoot (&tuple_lab, root, asp, bsp,code_gen_node_ids_p);
				else {
					int asize,bsize;
				
					/* BuildNewStackFrame (root->node_arguments,asp,bsp,ResultNodeNecessary (BToB,root->node_state),code_gen_node_ids_p); */
					
					{
					int a_size,b_size;

					BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);
					DetermineSizeOfArguments (root->node_arguments,&a_size,&b_size);
					UpdateAAndBStack (asp,bsp,a_size,b_size,&asp,&bsp);
					}

					if (!function_called_only_curried_or_lazy_with_one_return){
						DetermineSizeOfState (resultstate, &asize, &bsize);
						GenRtn (asize, bsize, resultstate);
					}
				}
			}
			return;
		case cons_symb:
#if STRICT_LISTS
			if (rootsymb->symb_head_strictness>1 || rootsymb->symb_tail_strictness){
				if (rootsymb->symb_head_strictness==4 && root->node_arity<2){
					CodeRootSymbolApplication (root,rootid,rootsymb->symb_unboxed_cons_sdef_p,asp,bsp,code_gen_node_ids_p,resultstate);
					return;
				} else {
					if (IsSemiStrictState (root->node_state)){
						LabDef d_cons_lab,n_cons_lab;

						if (rootsymb->symb_head_strictness==4){
							MakeSymbolLabel (&d_cons_lab,rootsymb->symb_unboxed_cons_sdef_p->sdef_module,d_pref,rootsymb->symb_unboxed_cons_sdef_p,0);
						} else {
							if (rootsymb->symb_head_strictness>1){
								if (rootsymb->symb_tail_strictness)
									d_cons_lab=conssts_lab;
								else
									d_cons_lab=conss_lab;
							} else
								d_cons_lab=consts_lab;
							
							d_cons_lab.lab_pref=d_pref;
						}
						
						n_cons_lab = d_cons_lab;
						n_cons_lab.lab_pref = n_pref;
							
						CreateSemiStrictRootNode (&d_cons_lab,&n_cons_lab,root,rootid,asp,bsp,code_gen_node_ids_p,resultstate);						
						return;
					} else {
						LabDef *strict_cons_lab_p;
						int a_size;

						BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);

						if (rootsymb->symb_head_strictness==4){
							int b_size;

							strict_cons_lab_p=unboxed_cons_label (rootsymb);
							
							DetermineSizeOfArguments (root->node_arguments,&a_size,&b_size);
							GenFillR (strict_cons_lab_p,a_size,b_size,asp,0,0,ReleaseAndFill,True);
							bsp-=b_size;
						} else {
							if (rootsymb->symb_head_strictness>1){
								if (rootsymb->symb_tail_strictness)
									strict_cons_lab_p=&conssts_lab;
								else
									strict_cons_lab_p=&conss_lab;
							} else
								strict_cons_lab_p=&consts_lab;

							a_size=root->node_arity;
							GenFillh (root->node_arity==2 ? &cons_lab : strict_cons_lab_p,a_size,asp,ReleaseAndFill);
						}

						asp-=a_size;

						GenPopA	(asp);
						GenPopB	(bsp);
						GenRtn (1,0,OnAState);
						return;
					}
				}
			}
#endif
			FillRhsRoot (&cons_lab, root, asp, bsp,code_gen_node_ids_p);
			return;
		case nil_symb:
#if STRICT_LISTS
			if (rootsymb->symb_head_strictness & 1){
				if (!simple_expression_without_node_ids (root->node_arguments->arg_node)){
					BuildArg (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);
					GenPopA (1);
					--asp;
				}

				if (resultstate.state_kind==StrictRedirection){
					GenPopA	(asp);
					GenPopB	(bsp);
					GenBuildh (&nil_lab,0);
				} else {
					GenFillh (&nil_lab,0,asp,ReleaseAndFill);

					GenPopA	(asp);
					GenPopB	(bsp);
				}
				GenRtn (1,0,OnAState);
				return;
			}
#endif

			FillRhsRoot (&nil_lab, root, asp, bsp,code_gen_node_ids_p);
			return;
		case apply_symb:
#ifdef NEW_APPLY
			CodeRootApply (root, rootid, asp, bsp,code_gen_node_ids_p,resultstate);
#else
			CodeRootSymbolApplication (root, rootid, ApplyDef, asp, bsp,code_gen_node_ids_p,resultstate);
#endif
			return;
		case if_symb:
#ifdef FASTER_STRICT_IF
			if (root->node_arity==3 && !IsLazyState (root->node_state) && rootid==NULL){
				LabDef elselab,thenlab;
				Args cond_arg,then_arg;
								
				cond_arg = root->node_arguments;
				
				EvaluateCondition (cond_arg->arg_node,&asp,&bsp,code_gen_node_ids_p,cond_arg->arg_state);

				MakeLabel (&elselab,else_symb,NewLabelNr,no_pref);
				MakeLabel (&thenlab,then_symb,NewLabelNr++,no_pref);
				
				thenlab.lab_mod=notused_string;

				BranchOnCondition (cond_arg->arg_node,asp,bsp,code_gen_node_ids_p,cond_arg->arg_state,&thenlab,&elselab,&thenlab,asp,bsp,asp,bsp);
		
				then_arg=cond_arg->arg_next;
		
				if (thenlab.lab_mod==NULL)
					GenLabelDefinition (&thenlab);

				CodeRhsNodeDefsAndRestoreNodeIdStates (then_arg->arg_node,NULL,asp,bsp,resultstate,NULL,
												code_gen_node_ids_p->a_node_ids,code_gen_node_ids_p->b_node_ids,NULL,True);
				
				GenLabelDefinition (&elselab);

				CodeRhsNodeDefsAndRestoreNodeIdStates (then_arg->arg_next->arg_node,NULL,asp,bsp,resultstate,NULL,
									 			code_gen_node_ids_p->a_node_ids,code_gen_node_ids_p->b_node_ids,NULL,True);
				return;
			} else
#endif
			CodeRootSymbolApplication (root,rootid,IfDef,asp,bsp,code_gen_node_ids_p,resultstate);
			return;
		case select_symb:
			CodeRootSelection (root, rootid, asp, bsp,code_gen_node_ids_p,resultstate);
			return;
		case fail_symb:
#ifdef CLEAN2
		{
			IdentS case_ident_s;
			SymbDefS case_def_s;

			case_ident_s.ident_name=rootsymb->symb_string;
			Assume (case_ident_s.ident_name != NULL, "codegen3", "CodeNormalRootNode (fail_symb)");

			case_def_s.sdef_ident = &case_ident_s;

			if (FunctionMayFailWarningOrError!=0)
				StaticMessage (FunctionMayFailWarningOrError==2, "%D", "case may fail", &case_def_s);

			if (! (IsOnBStack (resultstate) || 
						(IsSimpleState (resultstate) && resultstate.state_kind==StrictRedirection)))
				/* root needed */
				asp++;

			GenCaseNoMatchError (&case_def_s,asp,bsp);

			return;
		}
#else /* ifndef CLEAN2 */
			error_in_function ("CodeNormalRootNode");
/*			JumpToNextAlternative (asp, bsp); */
			return;
#endif
		case string_denot:
			GenPopA (asp);
			GenPopB (bsp);

			GenBuildString (rootsymb->symb_val);
			GenRtn (1, 0, OnAState);
			return;
		case integer_denot:
			GenPopA (asp);
			GenPopB (bsp);

			GenPushZ (rootsymb->symb_val);
			if (IsSimpleState (resultstate)){
				LabDef record_lab;
				
				ConvertSymbolToRLabel (&record_lab,BasicSymbolStates [integer_denot].state_record_symbol);
				GenBuildhr (&record_lab,1,1);
				GenRtn (1,0,OnAState);
			} else
				GenRtn (1,1,resultstate);
			return;
		case seq_symb:
			if (root->node_arity==2){
				int old_asp,old_bsp;
				
				old_asp=asp;
				old_bsp=bsp;
				BuildArg (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);
				if (asp>=old_asp)
					GenPopA (asp-old_asp);
				else
					old_asp=asp;
				if (bsp>=old_bsp)
					GenPopB (bsp-old_bsp);
				else
					old_bsp=bsp;
				/* asp=old_asp; bsp=old_bsp; */

				CodeRootNode (root->node_arguments->arg_next->arg_node,rootid,old_asp,old_bsp,code_gen_node_ids_p,resultstate,NULL);
			} else {
				LabDef name;

				ConvertSymbolToDLabel (&name,SeqDef);
				FillRhsRoot (&name, root, asp, bsp,code_gen_node_ids_p);
			}
			return;
		default:
			if (rootsymb->symb_kind < Nr_Of_Basic_Types)
				FillRhsRoot (&BasicDescriptors[rootsymb->symb_kind], root, asp, bsp,code_gen_node_ids_p);
			else {
				/*	in case of a denotation: */
				
				ObjectKind denottype;

				denottype = (rootsymb->symb_kind < Nr_Of_Predef_Types)
					? BasicSymbolStates [rootsymb->symb_kind].state_object
					: UnknownObj;

				GenPopA (asp);
				GenPopB (bsp);

				if (root->node_state.state_object == denottype){
					if (root->node_state.state_kind == OnB){
						PushBasic (denottype, rootsymb->symb_val);
						if (!function_called_only_curried_or_lazy_with_one_return)
							GenRtn (0, ObjectSizes [denottype], root->node_state);
					} else {
						FillBasic	(denottype, rootsymb->symb_val,0, ReleaseAndFill);
						if (!function_called_only_curried_or_lazy_with_one_return)
							GenRtn (1, 0, OnAState);
					}
				} else {
					StaticMessage (False,CurrentAltLabel.lab_symbol->sdef_ident->ident_name,Co_Wtype);
					GenTypeError();
					GenRtn (0, 0, OnAState);
				}
			}
	}
}

static void CodeRootFieldSelector (Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS demstate)
{
	int	fieldnr;
	SymbDef seldef;
	ArgP arg;

	arg = root->node_arguments;
	seldef = root->node_symbol->symb_def;
	fieldnr	= seldef->sdef_sel_field_number;
	
	if (IsSemiStrictState (root->node_state)){
		BuildArg (arg,&asp,&bsp,code_gen_node_ids_p);

		if (root->node_arity>=SELECTOR_U){
			SymbDef new_select_sdef;
			LabDef name,codelab;

			new_select_sdef=create_select_function (root->node_symbol,root->node_arity);

			ConvertSymbolToDandNLabel (&name,&codelab,new_select_sdef);
			GenFill (&name,1,&codelab,asp-rootid->nid_a_index,PartialFill);
			--asp;
		} else
			BuildOrFillLazyFieldSelector (root->node_symbol->symb_def,root->node_state.state_kind,&asp,rootid);

		RedirectResultAndReturn (asp,bsp,rootid->nid_a_index,rootid->nid_b_index,rootid->nid_state,demstate,1,0);
		return;
	} else {
		int recarity;
		Node arg_node;
		
		recarity = arg->arg_state.state_arity;
		arg_node=arg->arg_node;
		
		if (arg_node->node_kind!=NodeIdNode){
			StateS offstate;
			StateP record_state_p;

			offstate = arg_node->node_state;
			Build (arg_node,&asp,&bsp,code_gen_node_ids_p);

			record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;

			if (root->node_arity>=SELECTOR_U){
				int record_a_size,record_b_size,asize,bsize,aindex,bindex,offstate_a_size,offstate_b_size;
														
				DetermineSizeOfState (offstate,&offstate_a_size,&offstate_b_size);
				CoerceArgumentOnTopOfStack (&asp,&bsp,arg->arg_state,offstate,offstate_a_size,offstate_b_size);

#if BOXED_RECORDS
				if (root->node_arity<SELECTOR_L ?
					arg->arg_state.state_type==SimpleState :
					arg->arg_state.state_type==TupleState && arg->arg_state.state_tuple_arguments[0].state_type==SimpleState
				){
					if (root->node_arity<SELECTOR_L){
						PushField (*record_state_p,fieldnr,0,&asp,&bsp,&asize,&bsize);
						RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,demstate,1+asize,bsize);
					} else {
						ReplaceRecordByField (*record_state_p,fieldnr,&asp,&bsp,&asize,&bsize);
						DetermineSizeOfState (root->node_state,&offstate_a_size,&offstate_b_size);
						RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,demstate,offstate_a_size,offstate_b_size);
					}
				} else {
#endif
				DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&asize,&bsize,&aindex,&bindex,&record_a_size,&record_b_size,record_state_p);
				
				if (root->node_arity<SELECTOR_L){
					int n;
													
					for (n=0; n<asize; ++n)
						GenPushA (aindex+asize-1);
					asp+=asize;
					
					for (n=0; n<bsize; ++n)
						GenPushB (bindex+bsize-1);
					bsp+=bsize;

					RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,demstate,record_a_size+asize,record_b_size+bsize);
				} else {
					ReplaceRecordOnTopOfStackByField (&asp,&bsp,aindex,bindex,asize,bsize,record_a_size,record_b_size);
					DetermineSizeOfState (root->node_state,&offstate_a_size,&offstate_b_size);
					RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,demstate,offstate_a_size,offstate_b_size);
				}
#if BOXED_RECORDS
				}
#endif
				return;
			}
			
			if (offstate.state_type==RecordState){
				int apos,bpos,asize,bsize,aindex,bindex;
						
				DetermineFieldSizeAndPosition (fieldnr,&asize,&bsize,&apos,&bpos,offstate.state_record_arguments);

				aindex = asp-apos;
				bindex = bsp-bpos;
				RedirectResultAndReturn (asp, bsp, aindex, bindex,offstate.state_record_arguments[fieldnr], demstate, asize, bsize);
				return;
			} else {
				int a_size,b_size;
#if 1
				PushField (*record_state_p, fieldnr, 0, & asp, & bsp,&a_size,&b_size);
				RedirectResultAndReturn (asp,bsp,asp,bsp,record_state_p->state_record_arguments[fieldnr],demstate,a_size,b_size);
#else
				PushField (arg->arg_state, fieldnr, 0, & asp, & bsp,&a_size,&b_size);
				RedirectResultAndReturn (asp,bsp,asp,bsp,arg->arg_state.state_record_arguments[fieldnr],demstate,a_size,b_size);
#endif
				return;
			}
		} else {
			StateS offstate;
			NodeId arg_node_id;
			
			arg_node_id=arg_node->node_node_id;
			
			offstate = arg_node_id->nid_state;
			
			if (offstate.state_type==RecordState){
				int	asize,bsize,aindex,bindex,record_a_index,record_b_index;
				
				DetermineFieldSizeAndPosition (fieldnr, &asize, &bsize, &aindex, &bindex,offstate.state_record_arguments);
				
				if (arg_node_id->nid_refcount<0 && arg_node_id->nid_node!=NULL){
					record_a_index=get_a_index_of_unpacked_lhs_node (arg_node_id->nid_node->node_arguments);
					record_b_index=get_b_index_of_unpacked_lhs_node (arg_node_id->nid_node->node_arguments);
				} else {
					record_a_index=arg_node_id->nid_a_index,
					record_b_index=arg_node_id->nid_b_index;
				}

				if (root->node_arity>=SELECTOR_U){
					int	record_a_size,record_b_size,n;

					GenPopA (asp-record_a_index);
					asp=record_a_index;
					GenPopB (bsp-record_b_index);
					bsp=record_b_index;
					
					for (n=0; n<asize; ++n)
						GenPushA (aindex+asize-1);
					asp+=asize;

					for (n=0; n<bsize; ++n)
						GenPushB (bindex+bsize-1);
					bsp+=bsize;

					DetermineSizeOfState (offstate,&record_a_size,&record_b_size);
					
					RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,demstate,record_a_size+asize,record_b_size+bsize);
					return;
				}

				aindex=record_a_index-aindex,
				bindex=record_b_index-bindex;
				
				RedirectResultAndReturn (asp, bsp, aindex, bindex,offstate.state_record_arguments[fieldnr], demstate, asize, bsize);
				return;
			} else {
				Bool ontop;
				int a_size,b_size;
				StateP record_state_p;

				record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
				if (root->node_arity>=SELECTOR_U){
					int	asize,bsize,aindex,bindex,offered_a_size,offered_b_size;
					
					CopyNodeIdArgument (arg->arg_state,arg_node_id,&asp,&bsp);
#if BOXED_RECORDS
					if (arg->arg_state.state_type==SimpleState){
						if (root->node_arity<SELECTOR_L)
							PushField (*record_state_p,fieldnr,0,&asp,&bsp,&asize,&bsize);
						else
							ReplaceRecordByField (*record_state_p,fieldnr,&asp,&bsp,&asize,&bsize);
					} else {
#endif
					DetermineFieldSizeAndPosition (fieldnr,&asize,&bsize,&aindex,&bindex,record_state_p->state_record_arguments);
					
					if (root->node_arity<SELECTOR_L){
						int n;

						for (n=0; n<asize; ++n)
							GenPushA (aindex+asize-1);
						asp+=asize;

						for (n=0; n<bsize; ++n)
							GenPushB (bindex+bsize-1);
						bsp+=bsize;
					} else {
						int record_a_size,record_b_size;
					
						DetermineSizeOfState (*record_state_p,&record_a_size,&record_b_size);
						ReplaceRecordOnTopOfStackByField (&asp,&bsp,aindex,bindex,asize,bsize,record_a_size,record_b_size);
					}
#if BOXED_RECORDS
					}
#endif
					DetermineSizeOfState (root->node_state,&offered_a_size,&offered_b_size);	
					RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,demstate,offered_a_size,offered_b_size);
					return;
				}

				CoerceSimpleStateArgument (demstate, offstate.state_kind,arg_node_id->nid_a_index,&asp,False,&ontop);
#if 1
				PushField (*record_state_p,fieldnr,asp-arg_node_id->nid_a_index,&asp,&bsp,&a_size,&b_size);
				RedirectResultAndReturn (asp, bsp, asp, bsp,record_state_p->state_record_arguments[fieldnr],demstate,a_size,b_size);
#else
				PushField (arg->arg_state,fieldnr,asp-arg_node_id->nid_a_index,&asp,&bsp,&a_size,&b_size);
				RedirectResultAndReturn (asp, bsp, asp, bsp,arg->arg_state.state_record_arguments[fieldnr],demstate,a_size,b_size);
#endif
				return;
			}
		}
	}
}

static void CodeRootMatchNode (Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS demanded_state)
{
	if (IsSemiStrictState (root->node_state)){
		FillMatchNode (root,&asp,&bsp,rootid,code_gen_node_ids_p);

		RedirectResultAndReturn (asp,bsp,rootid->nid_a_index,rootid->nid_b_index,rootid->nid_state,demanded_state,1,0);
	} else {
		int a_size,b_size;

		FillMatchNode (root,&asp,&bsp,NULL,code_gen_node_ids_p);

		DetermineSizeOfState (root->node_state,&a_size,&b_size);
		RedirectResultAndReturn (asp,bsp,asp,bsp,root->node_state,demanded_state,a_size,b_size);
	}
}

static void CodeRootUpdateNode (Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS result_state)
{
	LabDef name;
	SymbDef record_sdef;

	record_sdef=root->node_symbol->symb_def;
	
	ConvertSymbolToLabel (&name,record_sdef);
	
	if (IsSemiStrictState (root->node_state)){
		ArgS *record_arg,*first_field_arg;
		int n_arguments;
		LabDef name,codelab;
		SymbDef new_update_sdef;

		record_arg=root->node_arguments;
		first_field_arg=record_arg->arg_next;

		n_arguments=root->node_arity;
		
		RemoveSelectorsFromUpdateNode (record_arg,first_field_arg);
		
		BuildArgs (root->node_arguments,&asp,&bsp,code_gen_node_ids_p);

		new_update_sdef=CreateUpdateFunction (record_arg,first_field_arg,root
#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
							,False
#endif
						);

#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
		{
			ArgP previous_arg,arg;
			
			previous_arg=record_arg;
			for_l (arg,first_field_arg,arg_next){
				Node field_node;
				
				field_node=arg->arg_node;
				field_node->node_arguments->arg_next=NULL;
										
				previous_arg->arg_next=arg;
				previous_arg=arg;
			}
			previous_arg->arg_next=NULL;
		}
#endif

		ConvertSymbolToDandNLabel (&name,&codelab,new_update_sdef);

		GenFill (&name,n_arguments,&codelab,asp-rootid->nid_a_index,NormalFill);
		asp-=n_arguments;

		RedirectResultAndReturn (asp,bsp,rootid->nid_a_index,rootid->nid_b_index,rootid->nid_state,result_state,1,0);
	} else {
		ArgS *record_arg,*first_field_arg;
		int record_a_size,record_b_size;
		
		record_arg=root->node_arguments;
		first_field_arg=record_arg->arg_next;
		
		RemoveSelectorsFromUpdateNode (record_arg,first_field_arg);

		BuildArgs (record_arg->arg_next,&asp,&bsp,code_gen_node_ids_p);

		if (IsSimpleState (root->node_state) && record_arg->arg_node->node_kind==NodeIdNode){
			NodeIdP record_node_id;
			
			record_node_id=record_arg->arg_node->node_node_id;

			if (is_unique_record_update (record_node_id,record_arg->arg_node) && rootid==NULL){
/*
			if ((record_node_id->nid_state.state_mark & STATE_UNIQUE_MASK)!=0 &&
				(record_node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 &&
				record_node_id->nid_number== -1 &&
				record_node_id->nid_state.state_type==SimpleState &&
				record_node_id->nid_state.state_kind==StrictOnA &&
				!DoReuseUniqueNodes && rootid==NULL)
			{
*/				int n_a_fill_bits,n_b_fill_bits;
				char bits[MaxNodeArity+2];
				LabDef record_lab;

#if BOXED_RECORDS
				record_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
#endif
				DetermineSizeOfState (record_sdef->sdef_record_state,&record_a_size,&record_b_size);
			
				if (record_a_size+record_b_size>2){
#if 0
					BuildArgs (record_arg->arg_next,&asp,&bsp,code_gen_node_ids_p);
#endif			
					compute_bits_and_add_selectors_to_update_node (record_arg,first_field_arg,
						record_sdef->sdef_record_state.state_record_arguments,record_a_size,record_b_size,
						bits,&n_a_fill_bits,&n_b_fill_bits);

					ConvertSymbolToRLabel (&record_lab,record_sdef->sdef_record_state.state_record_symbol);
				
					GenPushA (asp-record_node_id->nid_a_index);
					asp+=1;

					GenFill3R (&record_lab,record_a_size,record_b_size,asp,bits+1);

					asp-=n_a_fill_bits+1;
					bsp-=n_b_fill_bits;

					decrement_reference_count_of_node_id (record_node_id,&code_gen_node_ids_p->free_node_ids);
					
					GenPopA	(asp);
					GenPopB	(bsp);
					GenRtn (1,0,OnAState);
					return;
				}
			}
#if BOXED_RECORDS
			record_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
#endif
		}

		{
		int a_size,b_size;
		StateP record_state_p;

		record_state_p=&root->node_symbol->symb_def->sdef_record_state;

		record_arg->arg_state=*record_state_p;

		BuildArg (record_arg,&asp,&bsp,code_gen_node_ids_p);

		DetermineSizeOfArguments (record_arg,&a_size,&b_size);
		
		if (IsSimpleState (root->node_state)){
			LabDef record_label;
			int end_args_a_offset,end_args_b_offset;

			DetermineSizeOfState (*record_state_p,&record_a_size,&record_b_size);

			UpdateRecordAndAddSelectorsToUpdateNode (record_arg,first_field_arg,
				record_state_p->state_record_arguments,record_a_size,record_b_size,&end_args_a_offset,&end_args_b_offset);

			ConvertSymbolToRLabel (&record_label,record_sdef);
			GenFillR (&record_label,record_a_size,record_b_size,asp,0,0,ReleaseAndFill,False);

			GenPopA	(asp);
			GenPopB	(bsp);
			GenRtn (1,0, OnAState);
		} else {
			int end_args_a_offset,end_args_b_offset;

			DetermineSizeOfState (result_state,&record_a_size,&record_b_size);

			UpdateRecordAndAddSelectorsToUpdateNode (record_arg,first_field_arg,
				result_state.state_record_arguments,record_a_size,record_b_size,&end_args_a_offset,&end_args_b_offset);

			if (!function_called_only_curried_or_lazy_with_one_return){
				UpdateAAndBStack (asp,bsp,record_a_size,record_b_size,&asp,&bsp);
				GenRtn (record_a_size,record_b_size,result_state);
			} else {
				if (CurrentSymbol->symb_def->sdef_mark & SDEF_USED_LAZILY_MASK){
					BuildRecord (record_state_p->state_record_symbol,asp,bsp,asp,bsp,record_a_size,record_b_size,
									0,ReleaseAndFill,False);
					GenPopA (asp);
					GenPopB (bsp);
				} else {
					BuildNewRecordPop (record_state_p->state_record_symbol,record_a_size,record_b_size);
					GenUpdatePopA (0,asp-record_a_size);
					GenPopB (bsp-record_b_size);
				}
				GenRtn (1,0,OnAState);

				function_called_only_curried_or_lazy_with_one_return = 0;
			}
		}
	}

	}
}

#ifdef CLEAN2
extern int contains_fail (NodeP node_p);
#endif

static void fill_strict_root_unique_node (NodeP node,NodeP update_node,char bits[],LabDef *label_p,NodeIdP free_unique_node_id,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p)
{
	int a_size,b_size,n_a_fill_bits,n_b_fill_bits;
#if GENERATE_CODE_AGAIN
	ArgP removed_args=
#endif
	compute_bits_and_remove_unused_arguments_for_strict_node (node,bits,update_node->node_arguments->arg_occurrence,
															  &a_size,&b_size,&n_a_fill_bits,&n_b_fill_bits);
		
	BuildArgs (node->node_arguments,&asp,&bsp,code_gen_node_ids_p);

#if GENERATE_CODE_AGAIN
	if (call_code_generator_again)
		restore_removed_arguments (&node->node_arguments,removed_args,update_node->node_arguments->arg_occurrence,node->node_arity);
#endif
	
	GenPushA (asp-free_unique_node_id->nid_a_index);
	asp+=1;

	GenFill3R (label_p,a_size,b_size,asp,bits+1);

	asp-=n_a_fill_bits+1;
	bsp-=n_b_fill_bits;

	decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);

	GenPopA	(asp);
	GenPopB	(bsp);
	GenRtn (1,0,OnAState);
}

static void CodeRootFillUniqueNode (Node update_node,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p)
{
	unsigned int n_args,node_arity;
	char bits[MaxNodeArity+2];
	NodeIdP free_unique_node_id;
	NodeP node,push_node;
	LabDef name,*label_p;
	SymbolP symbol;

	node=update_node->node_arguments->arg_node;
	push_node=update_node->node_node;
	free_unique_node_id=push_node->node_arguments->arg_node->node_node_id;
	
	symbol=node->node_symbol;
	
	switch (symbol->symb_kind){
		case definition:
		{
			SymbDef sdef;
				
			sdef=node->node_symbol->symb_def;

			node_arity=node->node_arity;
			
			switch (sdef->sdef_kind){
				case CONSTRUCTOR:					
					bits[0]='1';
					
					if (sdef->sdef_strict_constructor){
						ConvertSymbolToKLabel (&name,sdef);
						
						fill_strict_root_unique_node (node,update_node,bits,&name,free_unique_node_id,asp,bsp,code_gen_node_ids_p);
						
						return;
					} else {
						ConvertSymbolToConstructorDLabel (&name,sdef);
						label_p=&name;
					}
					break;
				case RECORDTYPE:
					bits[0]='1';

					ConvertSymbolToRLabel (&name,sdef);

					fill_strict_root_unique_node (node,update_node,bits,&name,free_unique_node_id,asp,bsp,code_gen_node_ids_p);
					
					return;
				default:
					error_in_function ("CodeRootFillUniqueNode");
					return;
			}
			break;
		}
		default:
			error_in_function ("CodeRootFillUniqueNode");
			return;
	}

#if GENERATE_CODE_AGAIN
	{
	ArgP removed_args=
#endif
	compute_bits_and_remove_unused_arguments (node,bits,update_node->node_arguments->arg_occurrence,&n_args);

	BuildLazyArgs (node->node_arguments,&asp,&bsp,code_gen_node_ids_p);

#if GENERATE_CODE_AGAIN
	if (call_code_generator_again)
		restore_removed_arguments (&node->node_arguments,removed_args,update_node->node_arguments->arg_occurrence,node_arity);
	}
#endif

	GenPushA (asp-free_unique_node_id->nid_a_index);
	asp+=1;

	GenFill3 (label_p,node_arity,asp,bits+1);
	asp-=n_args+1;

	decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);
	
	GenPopA	(asp);
	GenPopB	(bsp);
	GenRtn (1,0,OnAState);
}

static int CodeRootNode (Node root,NodeId rootid,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p,StateS resultstate,struct esc *esc_p)
{
	switch (root->node_kind){
		case NormalNode:
			CodeNormalRootNode (root, rootid, asp, bsp,code_gen_node_ids_p,resultstate);
			return 0;
		case SelectorNode:
			CodeRootFieldSelector (root, rootid, asp, bsp,code_gen_node_ids_p,resultstate);
			return 0;
		case UpdateNode:
			CodeRootUpdateNode (root, rootid, asp, bsp,code_gen_node_ids_p,resultstate);
			return 0;
		case MatchNode:
			CodeRootMatchNode (root,rootid,asp,bsp,code_gen_node_ids_p,resultstate);
			return 0;
		case IfNode:
		{
			LabDef elselab,thenlab;
			Args condpart,then_arg;
			struct node *else_node;
			
			condpart = root->node_arguments;
			
			EvaluateCondition (condpart->arg_node,&asp,&bsp,code_gen_node_ids_p,condpart->arg_state);

			MakeLabel (&elselab, else_symb, NewLabelNr, no_pref);
			MakeLabel (&thenlab, then_symb, NewLabelNr++, no_pref); 

			thenlab.lab_mod=notused_string;

			BranchOnCondition (condpart->arg_node,asp,bsp,code_gen_node_ids_p,condpart->arg_state,&thenlab,&elselab,&thenlab,asp,bsp,asp,bsp);
	
			then_arg=condpart->arg_next;
	
			if (thenlab.lab_mod==NULL)
				GenLabelDefinition (&thenlab);

			{
			NodeIdListElementP free_node_ids;
#ifdef CLEAN2
			int needs_next_alt;
#endif
			free_node_ids=NULL;
			
			if (root->node_else_node_id_ref_counts!=NULL)
				subtract_else_ref_counts (root->node_else_node_id_ref_counts,&free_node_ids);

#ifdef CLEAN2
			needs_next_alt=
#endif
			CodeRhsNodeDefsAndRestoreNodeIdStates (then_arg->arg_node,root->node_then_node_defs,asp,bsp,resultstate,esc_p,
				code_gen_node_ids_p->a_node_ids,code_gen_node_ids_p->b_node_ids,
				free_node_ids,
#ifdef CLEAN2
				!contains_fail (then_arg->arg_node)
#else
				True
#endif
/*
				code_gen_node_ids_p->doesnt_fail
*/
				);

			if (root->node_else_node_id_ref_counts!=NULL)
				add_else_ref_counts (root->node_else_node_id_ref_counts);
			
			GenLabelDefinition	(&elselab);

			else_node=then_arg->arg_next->arg_node;

			if (else_node->node_kind==NormalNode && else_node->node_symbol->symb_kind==fail_symb){
				UpdateStackPointers (asp,bsp,esc_p->esc_asp,esc_p->esc_bsp);
				GenJmp (esc_p->esc_label);

#if BOXED_RECORDS
				if (root->node_else_node_id_ref_counts)
					or_then_record_update_marks (root->node_else_node_id_ref_counts);
#endif
				
				return 1;
			} else
#ifdef CLEAN2
				needs_next_alt = needs_next_alt |
#else
				return 
#endif
				CodeRhsNodeDefsAndRestoreNodeIdStates (else_node,root->node_else_node_defs,asp,bsp,resultstate,esc_p,
								 						code_gen_node_ids_p->a_node_ids,code_gen_node_ids_p->b_node_ids,
								 						NULL,code_gen_node_ids_p->doesnt_fail);
#if BOXED_RECORDS
				if (root->node_else_node_id_ref_counts)
					or_then_record_update_marks (root->node_else_node_id_ref_counts);
#endif

#ifdef CLEAN2
				return needs_next_alt;
#endif
			}
		}
		case NodeIdNode:
			if (rootid==NULL){
				CodeRedirection (root->node_node_id,asp,bsp,resultstate,&code_gen_node_ids_p->free_node_ids);
				return 0;
			}
		case FillUniqueNode:
			if (rootid==NULL){
				CodeRootFillUniqueNode (root,asp,bsp,code_gen_node_ids_p);
				return 0;
			}
		default:
			error_in_function ("CodeRootNode");
			return 0;
	}
}

static Bool ExamineRootNodeOnACycle (NodeId rhsid,Node rhsroot,int *asp_p,StateS resultstate)
{
	rhsid->nid_state_=OnAState;

	if (IsSimpleState (resultstate)){
		if (resultstate.state_kind==OnB || resultstate.state_kind==StrictRedirection){
			NewEmptyNode (asp_p,rhsroot->node_arity);
			rhsid->nid_a_index_=*asp_p;
		} else {
			if (rhsroot->node_arity<=2 || NodeOnACycleIsInRootNormalForm (rhsroot)){
				rhsid->nid_a_index_=0;
			} else {
				NewEmptyNode (asp_p,rhsroot->node_arity);
				rhsid->nid_a_index_=*asp_p;
				return True;
			}
		}
	} else {
		if (NodeOnACycleIsInRootNormalForm (rhsroot))
			NewEmptyNode (asp_p,-1);
		else
			NewEmptyNode (asp_p,rhsroot->node_arity);
		rhsid->nid_a_index_=*asp_p;
	}	
	return False;
}

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
extern int tail_call_modulo_cons;

static void generate_code_for_tail_call_modulo_cons (NodeP node_p,NodeId node_def_id,NodeP root_node,NodeP push_node,int asp,int bsp,MovedNodeIdP *moved_node_ids_p,struct code_gen_node_ids *code_gen_node_ids_p)
{
	LabDef name;
	int a_size,b_size;

	ConvertSymbolToLabel (&name,node_p->node_symbol->symb_def);

	DetermineSizeOfArguments (node_p->node_arguments,&a_size,&b_size);
	
	if (push_node==NULL)
		BuildArgsWithNewResultNode (node_p->node_arguments,&asp,&bsp,code_gen_node_ids_p,&a_size,&b_size);
	else
		BuildArgsWithResultNodeOnStack (node_p->node_arguments,push_node->node_arguments->arg_node->node_node_id,&asp,&bsp,code_gen_node_ids_p,&a_size,&b_size);

	asp-=a_size;
	bsp-=b_size;

	cleanup_stack (&asp,&bsp,a_size,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
					&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,code_gen_node_ids_p->doesnt_fail);

	node_def_id->nid_a_index_=asp+1;
	node_def_id->nid_b_index_=bsp;
	node_def_id->nid_state_=node_p->node_state;

	asp+=a_size;
	bsp+=b_size;

	BuildArgs (root_node->node_arguments,&asp,&bsp,code_gen_node_ids_p);

#if STRICT_LISTS
	if (root_node->node_symbol->symb_kind==cons_symb ? root_node->node_symbol->symb_head_strictness!=4 : !root_node->node_symbol->symb_def->sdef_strict_constructor){
#else
	if (root_node->node_symbol->symb_kind==cons_symb || !root_node->node_symbol->symb_def->sdef_strict_constructor){
#endif
		LabDef constructor_name,*constructor_name_p;

		if (root_node->node_symbol->symb_kind==cons_symb)
			constructor_name_p=&cons_lab;
		else {
			ConvertSymbolToConstructorDLabel (&constructor_name,root_node->node_symbol->symb_def);
			constructor_name_p=&constructor_name;
		}
		GenFillh (constructor_name_p,root_node->node_arity,asp,ReleaseAndFill);
		asp-=root_node->node_arity;
	} else {			
		LabDef constructor_name,*constructor_name_p;
		int asize,bsize;

#if STRICT_LISTS
		if (root_node->node_symbol->symb_kind==cons_symb)
			constructor_name_p=unboxed_cons_label (root_node->node_symbol);
		else
#endif
		{
			ConvertSymbolToKLabel (&constructor_name,root_node->node_symbol->symb_def);
			constructor_name_p=&constructor_name;
		}

		DetermineSizeOfArguments (root_node->node_arguments,&asize,&bsize);
	
		if (asize+bsize>2 && push_node!=NULL && push_node->node_push_size>=asize+bsize){
			NodeIdListElementP node_id_list;
			char bits[MaxNodeArity+2];
			unsigned int a_bits,b_bits,a_size,b_size,a_size2,b_size2,arg_n;
			int n,a_destination_offset,b_destination_offset;
			int total_a_size2,total_b_size2;
			int node_arity;
			ArgP arg_p;
			
			total_a_size2=0;
			total_b_size2=0;
			add_sizes_of_states_of_node_ids (push_node->node_node_ids,&total_a_size2,&total_b_size2);

			a_bits=0;
			b_bits=0;
			a_size=0;
			b_size=0;
			a_size2=0;
			b_size2=0;

			arg_p=root_node->node_arguments;
			node_arity=root_node->node_arity;
			node_id_list=push_node->node_node_ids;

			for (arg_n=0; arg_n<node_arity; ++arg_n){
				int arg_a_size,arg_b_size;
				int e_a_size2,e_b_size2;
																
				DetermineSizeOfState (arg_p->arg_state,&arg_a_size,&arg_b_size); 

				if (node_id_list!=NULL){
# ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
					DetermineSizeOfState (*node_id_list->nidl_node_id->nid_lhs_state_p,&e_a_size2,&e_b_size2);
# else
					DetermineSizeOfState (node_id_list->nidl_node_id->nid_state,&e_a_size2,&e_b_size2);
# endif
					if (arg_p->arg_node->node_kind!=NodeIdNode || arg_p->arg_node->node_node_id!=node_id_list->nidl_node_id ||
						arg_a_size!=e_a_size2 || arg_b_size!=e_b_size2 ||
						(arg_a_size!=0 && (a_size!=a_size2)) || (arg_b_size!=0 && (b_size+asize!=b_size2+total_a_size2))
					){
						a_bits |= (~((~0)<<arg_a_size))<<a_size;;
						b_bits |= (~((~0)<<arg_b_size))<<b_size;
					}
					
					a_size2+=e_a_size2;
					b_size2+=e_b_size2;
					node_id_list=node_id_list->nidl_next;
				} else {
					a_bits |= (~((~0)<<arg_a_size))<<a_size;;
					b_bits |= (~((~0)<<arg_b_size))<<b_size;
				}

				arg_p=arg_p->arg_next;
				a_size+=arg_a_size;
				b_size+=arg_b_size;
			}

			if (a_size>0)
				a_bits|=1;
			else
				b_bits|=1;

			a_destination_offset=a_size;
			for (n=a_size-1; n>=0; --n)
				if ((a_bits>>(unsigned)n) & 1){
					bits[n]='1';
					--a_destination_offset;
					if (a_destination_offset!=n)
						GenUpdateA (n,a_destination_offset);
				} else
					bits[n]='0';
			
			if (a_destination_offset!=0){
				GenPopA (a_destination_offset);
				asp-=a_destination_offset;
			}
			
			b_destination_offset=b_size;				
			for (n=b_size-1; n>=0; --n)
				if ((b_bits>>(unsigned)n) & 1){
					bits[n+a_size]='1';
					--b_destination_offset;
					if (b_destination_offset!=n)
						GenUpdateB (n,b_destination_offset);
				} else
					bits[n+a_size]='0';
			
			if (b_destination_offset!=0){
				GenPopB (b_destination_offset);
				bsp-=b_destination_offset;
			}
			
			bits[a_size+b_size]='\0';
													
			GenPushA (asp-node_def_id->nid_a_index);
			GenFill3R (constructor_name_p,asize,bsize,asp+1,bits);

			asp-=asize-a_destination_offset;
			bsp-=bsize-b_destination_offset;
		} else {
			GenFillR (constructor_name_p,asize,bsize,asp,0,0,ReleaseAndFill,True);

			asp-=asize;
			bsp-=bsize;
		}
	}

	if (tail_call_modulo_cons)
		name.lab_post=2;
	
	if (tail_call_modulo_cons==2){
		GenKeep (asp,a_size-1);
		++asp;
		UpdateAAndBStack (asp,bsp,a_size,b_size,&asp,&bsp);
		--asp;
		CallFunction (&name,node_p->node_symbol->symb_def,False,node_p);
	} else {
		CallFunction (&name,node_p->node_symbol->symb_def,True,node_p);
	}
	
	asp-=a_size;
	bsp-=b_size;

	DetermineSizeOfState (node_p->node_state,&a_size,&b_size);

	asp+=a_size;
	bsp+=b_size;

	if (a_size!=0)
		add_node_id_to_list (node_def_id,&code_gen_node_ids_p->a_node_ids);

	if (b_size!=0)
		add_node_id_to_list (node_def_id,&code_gen_node_ids_p->b_node_ids);

	if (tail_call_modulo_cons<2){
		node_def_id->nid_a_index_=asp;
		node_def_id->nid_b_index_=bsp;
		node_def_id->nid_state_=node_p->node_state;

		GenPopA	(asp);
		GenPopB	(bsp);
		GenRtn (1,0,OnAState);
	}
	{
		MovedNodeIdP moved_node_ids;
		
		moved_node_ids=*moved_node_ids_p;
		
		while (moved_node_ids!=NULL){
			moved_node_ids->mnid_node_id->nid_a_index_=moved_node_ids->mnid_a_stack_offset;
			moved_node_ids=moved_node_ids->mnid_next;
		}
	}
}

static int is_tail_call_modulo_cons_node (NodeP node_p)
{
	if (node_p->node_kind==NormalNode && node_p->node_symbol->symb_kind==definition){
		SymbDef sdef;
		
		sdef=node_p->node_symbol->symb_def;
		
		if (sdef->sdef_kind==IMPRULE && sdef->sdef_ancestor==CurrentSymbol->symb_def->sdef_ancestor &&
			sdef->sdef_arity==node_p->node_arity && !IsLazyState (node_p->node_state) &&
			ExpectsResultNode (node_p->node_state) && node_p->node_state.state_kind!=Parallel)
		{
			return 1;
		}
	}
	return 0;
}
#endif

#if OPTIMIZE_LAZY_TUPLE_RECURSION
extern int lazy_tuple_recursion;
NodeP tuple_result_p;

LabDef d_indirection_lab = {NULL, "", False, "d_indirection", 0};
LabDef n_indirection_lab = {NULL, "", False, "n_indirection", 0};

void update_tuple_element_node (StateP state_p,int tuple_element_a_index,int *asp_p,int *bsp_p)
{
	if (state_p->state_type==SimpleState){
		if (state_p->state_kind==StrictOnA){
			GenFillFromA (0,*asp_p-tuple_element_a_index,ReleaseAndFill);
			GenPopA (1);
			--*asp_p;
		} else if (state_p->state_kind==OnB){
			int b_size;

			FillBasicFromB (state_p->state_object,0,*asp_p-tuple_element_a_index,NormalFill);
			b_size=ObjectSizes [state_p->state_object];
			GenPopB (b_size);
			*bsp_p-=b_size;
		} else {
			GenFill (&d_indirection_lab,-2,&n_indirection_lab,*asp_p-tuple_element_a_index,PartialFill);
			--*asp_p;
		}
	} else
		error_in_function ("update_tuple_element_node");
}

#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
extern int tail_call_modulo_tuple_cons;
extern unsigned long global_same_select_vector;
#endif

static void fill_lazy_tuple_result_arguments (Args arg,int *asp_p,int *bsp_p,int tuple_element_n,int tuple_element_a_index,CodeGenNodeIdsP code_gen_node_ids_p)
{
	if (arg==NULL)
		return;
	else {
		NodeP node;
		int asize,bsize;

		fill_lazy_tuple_result_arguments (arg->arg_next,asp_p,bsp_p,tuple_element_n+1,tuple_element_a_index-1,code_gen_node_ids_p);
		
		ArgComment (arg);
		
		node=arg->arg_node;

#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
		if (node->node_kind==FillUniqueNode)
			node=node->node_arguments->arg_node;

		if (tail_call_modulo_tuple_cons==2 && global_same_select_vector & (1<<tuple_element_n)){
			if (node->node_kind!=NodeIdNode){
				Build (node,asp_p,bsp_p,code_gen_node_ids_p);
				DetermineSizeOfState (node->node_state, &asize, &bsize);
				CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,node->node_state,asize,bsize);
			} else {
				NodeId arg_node_id;
				
				arg_node_id=node->node_node_id;
				
# if BOXED_RECORDS
				arg_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
# endif
				if (CopyNodeIdArgument (arg->arg_state,arg_node_id,asp_p,bsp_p))
					ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
				
				decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
			}
		} else
#endif
		if (node->node_kind!=NodeIdNode){
			NodeIdS update_node_id_struct;

			update_node_id_struct.nid_a_index=tuple_element_a_index;
				
			if (node->node_kind==NormalNode && node->node_symbol->symb_kind==select_symb &&
				node->node_arguments->arg_node->node_kind==NodeIdNode &&
				tuple_element_n+1==node->node_arity &&
				(node->node_arguments->arg_node->node_node_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY))
			{
				;
			} else
				FillNodeOnACycle (node,asp_p,bsp_p,&update_node_id_struct,code_gen_node_ids_p);	
						
			GenPushA (*asp_p-tuple_element_a_index);
			++*asp_p;

			DetermineSizeOfState (node->node_state, &asize, &bsize);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,node->node_state,asize,bsize);
		} else {
			NodeId arg_node_id;
			
			arg_node_id=node->node_node_id;

# if BOXED_RECORDS
			arg_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
# endif
			if (arg_node_id->nid_state.state_type==SimpleState && arg_node_id->nid_state.state_kind!=OnB){
				if (CopyNodeIdArgument (arg->arg_state,arg_node_id,asp_p,bsp_p))
					ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
			} else
				CopyNodeIdArgument (arg_node_id->nid_state,arg_node_id,asp_p,bsp_p);

			update_tuple_element_node (&arg_node_id->nid_state,tuple_element_a_index,asp_p,bsp_p);
			
			GenPushA (*asp_p-tuple_element_a_index);
			++*asp_p;
			
			decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
		}
	}
}
#endif

#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
static void fill_strict_tuple_result_arguments (Args arg,ArgP *function_result_tuple_elements_p,int *asp_p,int *bsp_p,int tuple_element_n,int tuple_element_a_index,unsigned long result_and_call_same_select_vector,CodeGenNodeIdsP code_gen_node_ids_p)
{
	if (arg==NULL)
		return;
	else {
		NodeP node;
		int asize,bsize;

		--function_result_tuple_elements_p;

		fill_strict_tuple_result_arguments (arg->arg_next,function_result_tuple_elements_p,asp_p,bsp_p,tuple_element_n+1,tuple_element_a_index-1,result_and_call_same_select_vector,code_gen_node_ids_p);

		if (!(global_same_select_vector & (1<<tuple_element_n))){
			node=arg->arg_node;

			if (!(result_and_call_same_select_vector & (1<<tuple_element_n))){
				ArgComment (arg);
							
				if (node->node_kind==FillUniqueNode)
					node=node->node_arguments->arg_node;

				if (node->node_kind!=NodeIdNode){
					NodeIdS update_node_id_struct;

					update_node_id_struct.nid_a_index=tuple_element_a_index;

					if (node->node_kind==NormalNode && node->node_symbol->symb_kind==select_symb &&
						node->node_arguments->arg_node->node_kind==NodeIdNode &&
						tuple_element_n+1==node->node_arity &&
						(node->node_arguments->arg_node->node_node_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY))
					{
						;
					} else
						FillNodeOnACycle (node,asp_p,bsp_p,&update_node_id_struct,code_gen_node_ids_p);	
									
					GenKeep (*asp_p-tuple_element_a_index,*asp_p-(*function_result_tuple_elements_p)->arg_node->node_node_id->nid_a_index);
	/*
					GenPushA (*asp_p-tuple_element_a_index);
					++*asp_p;

					DetermineSizeOfState (node->node_state, &asize, &bsize);
					CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,node->node_state,asize,bsize);
	*/
				} else {
					NodeId arg_node_id;
					
					arg_node_id=node->node_node_id;
					
#if BOXED_RECORDS
					arg_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
#endif
					if (arg_node_id->nid_state.state_type==SimpleState && arg_node_id->nid_state.state_kind!=OnB){
						if (CopyNodeIdArgument (arg->arg_state,arg_node_id,asp_p,bsp_p))
							ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
					} else
						CopyNodeIdArgument (arg_node_id->nid_state,arg_node_id,asp_p,bsp_p);

					update_tuple_element_node (&arg_node_id->nid_state,tuple_element_a_index,asp_p,bsp_p);

					GenKeep (*asp_p-tuple_element_a_index,*asp_p-(*function_result_tuple_elements_p)->arg_node->node_node_id->nid_a_index);
	/*				
					GenPushA (*asp_p-tuple_element_a_index);
					++*asp_p;
	*/
					decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
				}
			}
		}
	}
}
#endif

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION || TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
static void CallFunctionWithStackSizes (LabDef name,NodeP node_p,int a_size,int b_size,int no_tail_call)
{
	name.lab_post=2;
	
	if (name.lab_mod && name.lab_mod==CurrentModule)
		name.lab_mod = NULL;

	name.lab_pref = s_pref;

	GenDStackLayout (a_size,b_size,node_p->node_arguments);
	if (no_tail_call){
		GenJsr (&name);
	} else
		GenJmp (&name);
}
#endif

int CodeRhsNodeDefs
	(Node root_node,NodeDefs defs,int asp,int bsp,SavedNidStateS **saved_nid_state_l,StateS result_state,
	struct esc *esc_p,NodeIdListElementP a_node_ids,NodeIdListElementP b_node_ids,
	NodeIdListElementP free_node_ids,int doesnt_fail)
{
	int r;
	MovedNodeIdP moved_node_ids;
	struct code_gen_node_ids code_gen_node_ids;
	
	moved_node_ids=NULL;

	code_gen_node_ids.free_node_ids=free_node_ids;
	code_gen_node_ids.saved_nid_state_l=saved_nid_state_l;
	code_gen_node_ids.doesnt_fail=doesnt_fail;
	code_gen_node_ids.moved_node_ids_l=&moved_node_ids;
	code_gen_node_ids.a_node_ids=a_node_ids;
	code_gen_node_ids.b_node_ids=b_node_ids;

	if (root_node->node_kind==NodeIdNode && defs==NULL){
		CodeRedirection (root_node->node_node_id, asp, bsp, result_state ,&free_node_ids);
		return 0;
	}

	if (root_node->node_kind==NodeIdNode && (root_node->node_node_id->nid_mark & ON_A_CYCLE_MASK)){
		NodeId root_node_id;
		NodeDefs rootdef;
		Bool large_lazy_root;

		root_node_id=root_node->node_node_id;
		rootdef=root_node_id->nid_node_def;

		large_lazy_root=ExamineRootNodeOnACycle (root_node_id, rootdef->def_node, &asp, result_state);

		if (defs!=rootdef || defs->def_next || large_lazy_root){
			CodeSharedNodeDefs (defs,rootdef,&asp,&bsp,&code_gen_node_ids);
		
			RedirectResultAndReturn (asp,bsp,root_node_id->nid_a_index,0,StrictOnAState,result_state,0,0);
			r=0;
		} else {
			r=CodeRootNode (rootdef->def_node,root_node_id,asp,bsp,&code_gen_node_ids,result_state,esc_p);
		}
	} else {
#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
		if (root_node->node_kind==NormalNode && root_node->node_symbol->symb_kind==tuple_symb && defs!=NULL){
			NodeIdP tuple_call_node_id_p;
			
			if (is_tuple_tail_call_modulo_cons_root (root_node,&tuple_call_node_id_p) &&
				(tuple_call_node_id_p->nid_node->node_symbol->symb_def->sdef_rule->rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY))
			{
				NodeDefP *last_node_def_h,last_node_def_p;
				
				last_node_def_h=&defs;
				while ((last_node_def_p=*last_node_def_h)->def_next!=NULL && last_node_def_p->def_id!=tuple_call_node_id_p)
					last_node_def_h=&last_node_def_p->def_next;
				
				if (last_node_def_p->def_next==NULL && last_node_def_p->def_id==tuple_call_node_id_p &&
					last_node_def_p->def_node->node_kind==TupleSelectorsNode &&
					last_node_def_p->def_node->node_arguments->arg_node->node_kind==NodeIdNode)
				{
					unsigned long result_and_call_same_select_vector;
					int n,tuple_arity,result_tuple_arity;
					int args_a_size,args_b_size;
					ArgP tuple_element_p,function_result_tuple_element_p;
					LabDef name;
					SymbDef sdef;
					NodeP node,tuple_node;
					ArgP function_result_tuple_elements_a[MaxNodeArity],*function_result_tuple_elements,*function_result_tuple_elements_p;
					
					printf ("Tuple tail call modulo cons %s\n",tuple_call_node_id_p->nid_node->node_symbol->symb_def->sdef_ident->ident_name);

					function_result_tuple_elements_p=&function_result_tuple_elements_a[0];
					for_l (function_result_tuple_element_p,last_node_def_p->def_node->node_arguments,arg_next)
						*function_result_tuple_elements_p++ = function_result_tuple_element_p;
					function_result_tuple_elements=function_result_tuple_elements_p;
					
					result_and_call_same_select_vector=0;

					if (tail_call_modulo_tuple_cons==2)
						for_li (tuple_element_p,n,root_node->node_arguments,arg_next){
							NodeP node_p;
							
							node_p=tuple_element_p->arg_node;
							--function_result_tuple_elements_p;

							if (node_p->node_kind==NodeIdNode && node_p->node_node_id->nid_refcount>0
								&& node_p->node_node_id==(*function_result_tuple_elements_p)->arg_node->node_node_id)
							{
								result_and_call_same_select_vector |= (1<<n);
							}
						}

					tuple_arity=root_node->node_arity;
					result_tuple_arity=tuple_arity;

					for_li (function_result_tuple_element_p,n,last_node_def_p->def_node->node_arguments,arg_next){						
						NodeIdP function_result_tuple_element_node_id_p;
						
						if (!(global_same_select_vector & (1<<(tuple_arity-1-n)))){
							if (result_and_call_same_select_vector & (1<<(tuple_arity-1-n)))
								GenPushA (asp-1-n);
							else
								GenCreate (-1);
							++asp;
						} else
							--result_tuple_arity;

						function_result_tuple_element_node_id_p=function_result_tuple_element_p->arg_node->node_node_id;
						
						function_result_tuple_element_node_id_p->nid_a_index = asp;
						function_result_tuple_element_node_id_p->nid_state = StrictOnAState;
					}
					
					tuple_result_p=last_node_def_p->def_node;

					*last_node_def_h=NULL;
					CodeSharedNodeDefs (defs,NULL,&asp,&bsp,&code_gen_node_ids);
					*last_node_def_h=last_node_def_p;

					if (tail_call_modulo_tuple_cons==1)
						for_li (function_result_tuple_element_p,n,last_node_def_p->def_node->node_arguments,arg_next){
							if (!(global_same_select_vector & (1<<(tuple_arity-1-n)))){
								GenPushA (asp-function_result_tuple_element_p->arg_node->node_node_id->nid_a_index);
								++asp;
							}
						}

					{
						struct arg *arg;
						struct state *tuple_state_p;
						int a_offset,b_offset,i;
						ArgP node_args;
						
						node=last_node_def_p->def_node;
						
						tuple_node=node->node_node;
						
						sdef=tuple_node->node_symbol->symb_def;
						ConvertSymbolToLabel (&name,sdef);
						
						node_args=tuple_node->node_arguments;
						DetermineSizeOfArguments (node_args,&args_a_size,&args_b_size);

						BuildArgs (node_args,&asp,&bsp,&code_gen_node_ids);
						

						asp-=args_a_size;
						bsp-=args_b_size;
						if (tail_call_modulo_tuple_cons==1)
							asp-=result_tuple_arity;
						
						cleanup_stack (&asp,&bsp,tail_call_modulo_tuple_cons==1 ? args_a_size+result_tuple_arity : args_a_size,args_b_size,
										&code_gen_node_ids.a_node_ids,&code_gen_node_ids.b_node_ids,&code_gen_node_ids.free_node_ids,
										code_gen_node_ids.moved_node_ids_l,code_gen_node_ids.doesnt_fail);


						if (tail_call_modulo_tuple_cons==1){
							int n;
							int result_tuple_a_size,result_tuple_b_size;
							StateS new_result_state,element_states[MaxNodeArity];
							
							tuple_state_p=&tuple_node->node_state;
							new_result_state=*tuple_state_p;
							
							result_tuple_a_size=0;
							result_tuple_b_size=0;

							for (n=0; n<root_node->node_arity; ++n)
								if (global_same_select_vector & (1<<n)){
									element_states[n]=tuple_state_p->state_tuple_arguments[n];
									AddSizeOfState (element_states[n],&result_tuple_a_size,&result_tuple_b_size);
								} else {
									element_states[n]=OnAState;
									++result_tuple_a_size;
								}
							

							new_result_state.state_tuple_arguments=element_states;
					
							CallFunctionWithStackSizes (name,tuple_node,args_a_size+result_tuple_arity,args_b_size,True);
							
							GenOStackLayoutOfState (result_tuple_a_size,result_tuple_b_size,new_result_state);

/*
							AddSizeOfState (tuple_node->node_state,&asp,&bsp);
*/
							asp+=result_tuple_a_size;
							bsp+=result_tuple_b_size;

							
							arg=node->node_arguments;
													
							a_offset=result_tuple_a_size;
							b_offset=result_tuple_b_size;

							if (new_result_state.state_type!=TupleState)
								error_in_function ("CodeRhsNodeDefs");

							for (i=new_result_state.state_arity-1; i>=0; --i){
								int a_size,b_size;
								NodeId node_id;	

								DetermineSizeOfState (new_result_state.state_tuple_arguments[i],&a_size,&b_size);
								
								a_offset-=a_size;
								b_offset-=b_size;

								if (global_same_select_vector & (1<<i)){
									if (arg!=NULL && arg->arg_node->node_node_id->nid_number==i){
										node_id=arg->arg_node->node_node_id;
										arg=arg->arg_next;
									} else {
										if (a_size==0 && b_size==0)
											continue;

										node_id=NewNodeId (NULL);								
										add_node_id_to_list (node_id,&code_gen_node_ids.free_node_ids);
									}
																
									node_id->nid_a_index_ = asp - a_offset;
									node_id->nid_b_index_ = bsp - b_offset;		
									node_id->nid_state_ = new_result_state.state_tuple_arguments[i];

									if (a_size!=0)
										add_node_id_to_list (node_id,&code_gen_node_ids.a_node_ids);
									if (b_size!=0)
										add_node_id_to_list (node_id,&code_gen_node_ids.b_node_ids);
								} else
									if (arg!=NULL && arg->arg_node->node_node_id->nid_number==i)
										arg=arg->arg_next;
							}
							
							if (arg!=NULL)
								error_in_function ("CodeRhsNodeDefs");
						} else {
							asp+=args_a_size;
							bsp+=args_b_size;
						}

					}
					
					if (tail_call_modulo_tuple_cons==1){
						r=CodeRootNode (root_node,NULL,asp,bsp,&code_gen_node_ids,result_state,esc_p);
/*
						fill_lazy_tuple_result_arguments (root_node->node_arguments,&asp,&bsp,0,tuple_arity,&code_gen_node_ids);

						UpdateAAndBStack (asp,bsp,args_a_size,args_b_size,&asp,&bsp);

						for (n=0; n<tuple_arity-1; ++n)
							GenKeep (tuple_arity-2-n,tuple_arity-1);

						GenPopA (tuple_arity-1);
						GenRtn (1,0,OnAState);

						r=0;
*/
					} else {
						fill_strict_tuple_result_arguments (root_node->node_arguments,function_result_tuple_elements,&asp,&bsp,0,tuple_arity,result_and_call_same_select_vector,&code_gen_node_ids);

						args_a_size+=result_tuple_arity;

/*						++asp;
*/
						UpdateAAndBStack (asp,bsp,args_a_size,args_b_size,&asp,&bsp);

						CallFunctionWithStackSizes (name,tuple_node,args_a_size,args_b_size,False);

						r=0;
					}

					while (moved_node_ids!=NULL){
						moved_node_ids->mnid_node_id->nid_a_index_=moved_node_ids->mnid_a_stack_offset;
						moved_node_ids=moved_node_ids->mnid_next;
					}

					return r;
				}
			}
		}
#endif

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
		if (OptimizeTailCallModuloCons && root_node->node_kind==NormalNode){
			if ((root_node->node_symbol->symb_kind==cons_symb && root_node->node_arity==2) ||
				 (root_node->node_symbol->symb_kind==definition && root_node->node_symbol->symb_def->sdef_kind==CONSTRUCTOR &&
				  root_node->node_arity==root_node->node_symbol->symb_def->sdef_arity))
			{
				ArgP arg_p,arg_p2;
				
				arg_p2=NULL;
				
				for_l (arg_p,root_node->node_arguments,arg_next){
					NodeP arg_node_p;
					
					arg_node_p=arg_p->arg_node;
					
					if (arg_node_p->node_kind==FillUniqueNode)
						arg_node_p=arg_node_p->node_arguments->arg_node;

					if (is_tail_call_modulo_cons_node (arg_node_p) && (arg_node_p->node_symbol->symb_def->sdef_rule->rule_mark & RULE_TAIL_MODULO_CONS_ENTRY_MASK)){
						arg_p2=arg_p;
						break;
					}
				}

				if (arg_p2==NULL){
					if (defs!=NULL){
						NodeDefP *last_node_def_h,last_node_def_p;
						NodeP node_p;
						
						last_node_def_h=&defs;
						while ((last_node_def_p=*last_node_def_h)->def_next!=NULL)
							last_node_def_h=&last_node_def_p->def_next;
						
						node_p=last_node_def_p->def_node;
						
						if (node_p!=NULL){
							NodeIdP node_def_id;
							NodeP push_node;
							int n_node_id_refs;

							node_def_id=last_node_def_p->def_id;
							push_node=NULL;
							
							if (node_p->node_kind==FillUniqueNode){
								push_node=node_p->node_node;
								node_p=node_p->node_arguments->arg_node;
							}
							
							if (!(node_def_id->nid_mark & ON_A_CYCLE_MASK) && is_tail_call_modulo_cons_node (node_p)
								&& (node_p->node_symbol->symb_def->sdef_rule->rule_mark & RULE_TAIL_MODULO_CONS_ENTRY_MASK))
							{
								n_node_id_refs=node_def_id->nid_refcount;

								for_l (arg_p,root_node->node_arguments,arg_next)
									if (arg_p->arg_node->node_kind==NodeIdNode && arg_p->arg_node->node_node_id==node_def_id){
										--n_node_id_refs;
										if (n_node_id_refs==0)
											break;
									}
								
								if (n_node_id_refs==0){
									*last_node_def_h=NULL;
									CodeSharedNodeDefs (defs,NULL,&asp,&bsp,&code_gen_node_ids);
									*last_node_def_h=last_node_def_p;

									generate_code_for_tail_call_modulo_cons (node_p,node_def_id,root_node,push_node,asp,bsp,&moved_node_ids,&code_gen_node_ids);

									return 0;
								}
							}
						}
					}
				} else {
					NodeP node_p,push_node_p,old_arg_node_p;
					NodeIdP node_id_p;
					
					node_p=arg_p2->arg_node;
					push_node_p=NULL;

					if (node_p->node_kind==FillUniqueNode){
						push_node_p=node_p->node_node;
						node_p=node_p->node_arguments->arg_node;
					}

					node_id_p=NewNodeId (NULL);
					node_id_p->nid_refcount=1;
					
					old_arg_node_p=arg_p2->arg_node;
					arg_p2->arg_node=NewNodeIdNode (node_id_p);

					CodeSharedNodeDefs (defs,NULL,&asp,&bsp,&code_gen_node_ids);
					generate_code_for_tail_call_modulo_cons (node_p,node_id_p,root_node,push_node_p,asp,bsp,&moved_node_ids,&code_gen_node_ids);
					
					arg_p2->arg_node=old_arg_node_p;

					return 0;
				}
			}
		}
#endif

#if OPTIMIZE_LAZY_TUPLE_RECURSION
		if (lazy_tuple_recursion && root_node->node_kind!=IfNode){
			if (root_node->node_kind==NormalNode && root_node->node_symbol->symb_kind==tuple_symb &&
				!(IsSemiStrictState (root_node->node_state) || IsSimpleState (root_node->node_state))
			){
				int a_size,b_size,n,tuple_arity;
				/*
				ArgP tuple_element_p;
				unsigned long result_and_call_same_select_vector;
				
				result_and_call_same_select_vector=0;
				
				for_li (tuple_element_p,n,root_node->node_arguments,arg_next){
					NodeP node_p;
					
					node_p=tuple_element_p->arg_node;
					
					if (node_p->node_symbol->symb_kind==select_symb
						&& node_p->node_arguments->arg_node->node_kind==NodeIdNode
						&& n+1==node_p->node_arity
						&& (node_p->node_arguments->arg_node->node_node_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY)
					)
						result_and_call_same_select_vector |= (1<<n);
				}
				*/
				tuple_result_p=root_node;

				CodeSharedNodeDefs (defs,NULL,&asp,&bsp,&code_gen_node_ids);

				fill_lazy_tuple_result_arguments (root_node->node_arguments,&asp,&bsp,0,root_node->node_arity,&code_gen_node_ids);
				
				tuple_arity=root_node->node_arity;
				
				a_size=tuple_arity;
				b_size=0;
				/*
				DetermineSizeOfArguments (root_node->node_arguments,&a_size,&b_size);
				*/
#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
				if (tail_call_modulo_tuple_cons==0)
					++asp;
				else if (tail_call_modulo_tuple_cons==2){
					ArgP arg_p;
					int n;
					
					for_li (arg_p,n,root_node->node_arguments,arg_next){
						if (global_same_select_vector & (1<<n)){
							--tuple_arity;
							--a_size;
							AddSizeOfState (arg_p->arg_state,&a_size,&b_size);
						}
					}
				}
#else				
				++asp;
#endif
				UpdateAAndBStack (asp,bsp,a_size,b_size,&asp,&bsp);

				for (n=0; n<tuple_arity-1; ++n)
					GenKeep (tuple_arity-2-n,tuple_arity-1);

				GenPopA (tuple_arity-1);

#if TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION
				if (tail_call_modulo_tuple_cons==2){
					int n;
					StateS new_result_state,element_states[MaxNodeArity];
					
					new_result_state=result_state;

					for (n=0; n<root_node->node_arity; ++n)
						if (global_same_select_vector & (1<<n))
							element_states[n]=result_state.state_tuple_arguments[n];
						else
							element_states[n]=OnAState;
					
					new_result_state.state_tuple_arguments=element_states;
					
					GenRtn (a_size-(tuple_arity-1),b_size,new_result_state);
				} else
#endif
				GenRtn (1,0,OnAState);

				r=0;
			} else if (root_node->node_kind==NormalNode && root_node->node_symbol->symb_kind==definition
					&& root_node->node_symbol->symb_def->sdef_kind==IMPRULE
					&& (root_node->node_symbol->symb_def->sdef_rule->rule_mark & RULE_CALL_VIA_LAZY_SELECTIONS_ONLY)
					&& root_node->node_symbol->symb_def->sdef_arity==root_node->node_arity
					&& !IsSemiStrictState (root_node->node_state))
			{
				int a_size,b_size,tuple_arity,n;
				SymbDef sdef;
				LabDef name;
				
				CodeSharedNodeDefs (defs,NULL,&asp,&bsp,&code_gen_node_ids);
								
				sdef=root_node->node_symbol->symb_def;
				tuple_arity=sdef->sdef_rule->rule_type->type_alt_rhs->type_node_arity;

				for (n=0; n<tuple_arity; ++n){
					GenPushA (asp-(n+1));
					++asp;
				}
				
				BuildArgs (root_node->node_arguments,&asp,&bsp,&code_gen_node_ids);
				DetermineSizeOfArguments (root_node->node_arguments,&a_size,&b_size);

				UpdateAAndBStack (asp,bsp,a_size+tuple_arity,b_size,&asp,&bsp);

				ConvertSymbolToLabel (&name,sdef);
				name.lab_post=2;
				
				if (name.lab_mod && name.lab_mod==CurrentModule)
					name.lab_mod = NULL;

				name.lab_pref = s_pref;
		
				GenDStackLayout (a_size+tuple_arity,b_size,root_node->node_arguments);
				GenJmp (&name);
				
				r=0;
			} else {
				error_in_function ("CodeRhsNodeDefs");
				r=0;
			}
		} else
#endif
		{
		CodeSharedNodeDefs (defs,NULL,&asp,&bsp,&code_gen_node_ids);

		r=CodeRootNode (root_node,NULL,asp,bsp,&code_gen_node_ids,result_state,esc_p);
		}
	}

	while (moved_node_ids!=NULL){
		moved_node_ids->mnid_node_id->nid_a_index_=moved_node_ids->mnid_a_stack_offset;
		moved_node_ids=moved_node_ids->mnid_next;
	}

	return r;
}

#if GENERATE_CODE_AGAIN
struct saved_node_id_ref_counts* save_lhs_node_id_ref_counts (NodeP node_p,struct saved_node_id_ref_counts *snir_p)
{
	if (node_p->node_kind==NodeIdNode){
		struct saved_node_id_ref_counts *new_snir_p;
		
		new_snir_p=CompAllocType (struct saved_node_id_ref_counts);
		new_snir_p->snir_node_id=node_p->node_node_id;
		new_snir_p->snir_ref_count=node_p->node_node_id->nid_refcount;
		
		new_snir_p->snir_next=snir_p;
		snir_p=new_snir_p;
	} else {
		ArgP arg_p;
		
		for_l (arg_p,node_p->node_arguments,arg_next)
			snir_p=save_lhs_node_id_ref_counts (arg_p->arg_node,snir_p);
	}

	return snir_p;
}

struct saved_node_id_ref_counts* save_rhs_node_id_ref_counts
	(NodeP node_p,NodeDefP node_defs,struct saved_node_id_ref_counts *snir_p,struct saved_case_node_id_ref_counts ***scnirc_hl)
{
	NodeDefP node_def_p;
	
	switch (node_p->node_kind){
		case PushNode:
		{
			struct node_id_list_element *arg_node_id_list;
			
			for_l (arg_node_id_list,node_p->node_node_ids,nidl_next){
				struct saved_node_id_ref_counts *new_snir_p;
				NodeIdP arg_node_id_p;
						
				arg_node_id_p=arg_node_id_list->nidl_node_id;
	
				new_snir_p=CompAllocType (struct saved_node_id_ref_counts);
				new_snir_p->snir_node_id=arg_node_id_p;
				new_snir_p->snir_ref_count=arg_node_id_p->nid_refcount;
				
				new_snir_p->snir_next=snir_p;
				snir_p=new_snir_p;
			}

			return save_rhs_node_id_ref_counts (node_p->node_arguments->arg_next->arg_node,node_defs,snir_p,scnirc_hl);
		}
		case SwitchNode:
		{
			ArgP arg_p;
			
			for_l (arg_p,node_p->node_arguments,arg_next){
				NodeP case_node_p;
				NodeIdRefCountListP node_id_ref_count_elem_p;

				case_node_p=arg_p->arg_node;
				if (case_node_p->node_kind==OverloadedCaseNode)
					case_node_p=case_node_p->node_node;
				
				for_l (node_id_ref_count_elem_p,case_node_p->node_node_id_ref_counts,nrcl_next){
					struct saved_case_node_id_ref_counts *new_scnirc_p;
					
					new_scnirc_p=CompAllocType (struct saved_case_node_id_ref_counts);
					new_scnirc_p->scnir_nrcl=node_id_ref_count_elem_p;
					new_scnirc_p->scnir_ref_count=node_id_ref_count_elem_p->nrcl_ref_count;

					new_scnirc_p->scnir_next=NULL;
					**scnirc_hl=new_scnirc_p;
					*scnirc_hl=&new_scnirc_p->scnir_next;
				}
				
				snir_p=save_rhs_node_id_ref_counts (case_node_p->node_arguments->arg_node,case_node_p->node_node_defs,snir_p,scnirc_hl);
			}
			break;
		}
		case GuardNode:
		{
			while (node_p->node_kind==GuardNode){
				snir_p=save_rhs_node_id_ref_counts (node_p->node_arguments->arg_node,node_defs,snir_p,scnirc_hl);
		
				node_defs=node_p->node_node_defs;
				node_p=node_p->node_arguments->arg_next->arg_node;
			}

			return save_rhs_node_id_ref_counts (node_p,node_defs,snir_p,scnirc_hl);
		}
		case IfNode:
			snir_p=save_rhs_node_id_ref_counts (node_p->node_arguments->arg_next->arg_node,node_p->node_then_node_defs,snir_p,scnirc_hl);
			snir_p=save_rhs_node_id_ref_counts (node_p->node_arguments->arg_next->arg_next->arg_node,node_p->node_else_node_defs,snir_p,scnirc_hl);
			node_p=node_p->node_arguments->arg_node;
			break;			
	}
	
	for_l (node_def_p,node_defs,def_next)
		if (node_def_p->def_id!=NULL){
			struct saved_node_id_ref_counts *new_snir_p;

			new_snir_p=CompAllocType (struct saved_node_id_ref_counts);
			new_snir_p->snir_node_id=node_def_p->def_id;
			new_snir_p->snir_ref_count=node_def_p->def_id->nid_refcount;
			
			new_snir_p->snir_next=snir_p;
			snir_p=new_snir_p;

			if (node_def_p->def_node!=NULL && node_def_p->def_node->node_kind==TupleSelectorsNode){
				ArgP arg_p;
				
				for_l (arg_p,node_def_p->def_node->node_arguments,arg_next)
					if (arg_p->arg_node->node_kind==NodeIdNode){
						NodeIdP tuple_element_node_id_p;

						tuple_element_node_id_p=arg_p->arg_node->node_node_id;
						
						new_snir_p=CompAllocType (struct saved_node_id_ref_counts);
						new_snir_p->snir_node_id=tuple_element_node_id_p;
						new_snir_p->snir_ref_count=tuple_element_node_id_p->nid_refcount;
						
						new_snir_p->snir_next=snir_p;
						snir_p=new_snir_p;
					}
			}
		}

	return snir_p;
}

void restore_node_id_ref_counts (struct saved_node_id_ref_counts *snir_p,struct saved_case_node_id_ref_counts *scnir_p)
{
	while (snir_p!=NULL){
		snir_p->snir_node_id->nid_refcount=snir_p->snir_ref_count;				
		snir_p=snir_p->snir_next;
	}

	while (scnir_p!=NULL){
		scnir_p->scnir_nrcl->nrcl_ref_count=scnir_p->scnir_ref_count;				
		scnir_p=scnir_p->scnir_next;
	}	
}
#endif

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
static int tail_call_modulo_cons_call (NodeP node_p,NodeDefP node_defs)
{
	if (node_p->node_kind==NormalNode){
		SymbolP node_symbol_p;
		
		node_symbol_p=node_p->node_symbol;
		if	((node_symbol_p->symb_kind==cons_symb && node_p->node_arity==2) ||
			 (node_symbol_p->symb_kind==definition && node_symbol_p->symb_def->sdef_kind==CONSTRUCTOR &&
			  node_p->node_arity==node_symbol_p->symb_def->sdef_arity))
		{
			ArgP arg_p;
			
			for_l (arg_p,node_p->node_arguments,arg_next){
				NodeP arg_node_p;
				
				arg_node_p=arg_p->arg_node;
				
				if (arg_node_p->node_kind==FillUniqueNode)
					arg_node_p=arg_node_p->node_arguments->arg_node;

				if (is_tail_call_modulo_cons_node (arg_node_p))
					return 1;
			}

			if (node_defs!=NULL){
				NodeDefP last_node_def_p;
				NodeP node_def_node_p;
				
				last_node_def_p=node_defs;
				while (last_node_def_p->def_next!=NULL)
					last_node_def_p=last_node_def_p->def_next;
				
				node_def_node_p=last_node_def_p->def_node;
				
				if (node_def_node_p!=NULL){
					NodeIdP node_def_id;
					
					node_def_id=last_node_def_p->def_id;

					if (node_def_node_p->node_kind==FillUniqueNode)
						node_def_node_p=node_def_node_p->node_arguments->arg_node;
					
					if (!(node_def_id->nid_mark & ON_A_CYCLE_MASK) && is_tail_call_modulo_cons_node (node_def_node_p)){
						int n_node_id_refs;
						
						n_node_id_refs=node_def_id->nid_refcount;
						
						for_l (arg_p,node_p->node_arguments,arg_next)
							if (arg_p->arg_node->node_kind==NodeIdNode && arg_p->arg_node->node_node_id==node_def_id){
								--n_node_id_refs;
								if (n_node_id_refs==0)
									break;
							}
						
						if (n_node_id_refs==0)
							return 1;
					}
				}
			}
		}
	}

	return 0;
}

int does_tail_call_modulo_cons (NodeP node_p,NodeDefP node_defs)
{
	switch (node_p->node_kind){
		case SwitchNode:
		{
			ArgP arg_p;
			int r;
			
			r=0;
			for_l (arg_p,node_p->node_arguments,arg_next)
				if (does_tail_call_modulo_cons (arg_p->arg_node->node_arguments->arg_node,arg_p->arg_node->node_node_defs))
					r=1;
			
			return r;
		}
		case PushNode:
			return does_tail_call_modulo_cons (node_p->node_arguments->arg_next->arg_node,node_defs);
		case GuardNode:
		{
			int r;
			
			r=0;
			while (node_p->node_kind==GuardNode){
				if (does_tail_call_modulo_cons (node_p->node_arguments->arg_node,node_defs))
					r=1;
		
				node_defs=node_p->node_node_defs;
				node_p=node_p->node_arguments->arg_next->arg_node;
			}

			if (does_tail_call_modulo_cons (node_p,node_defs))
				r=1;
			
			return r;
		}
		case IfNode:
		{
			int r;
			ArgP then_arg_p;
			NodeP else_node_p;
			
			r=0;
			then_arg_p=node_p->node_arguments->arg_next;
			
			r=does_tail_call_modulo_cons (then_arg_p->arg_node,node_p->node_then_node_defs);
			
			else_node_p=then_arg_p->arg_next->arg_node;

			if (else_node_p->node_kind==NormalNode && else_node_p->node_symbol->symb_kind==fail_symb)
				return r;

			if (does_tail_call_modulo_cons (else_node_p,node_p->node_else_node_defs))
				r=1;			
			
			return r;	
		}
		default:
			return tail_call_modulo_cons_call (node_p,node_defs);
	}
		
	return 0;
}
#endif
