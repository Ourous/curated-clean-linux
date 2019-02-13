/*
	(Concurrent) Clean Compiler: Code Generator

	Authors:	Sjaak Smetsers & John van Groningen
	At:			University of Nijmegen, department of computing science
*/

#define FASTER_STRICT_IF /* also in statesgen.c */
#define DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS
#define FREE_STRICT_LHS_TUPLE_ELEMENTS 1 /* also in codegen1.c */
#define SELECTORS_FIRST 1 /* also in codegen.c */
#define OPTIMIZE_SELECTION_OF_SELECTION_OF_UNBOXED_RECORD_FIELD

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "syntaxtr.t"
#include "comsupport.h"

#include "settings.h"
#include "sizes.h"
#include "checker.h"
#include "codegen_types.h"
#include "statesgen.h"
#include "codegen.h"
#include "codegen1.h"
#include "codegen2.h"
#include "sa.h"
#include "transform.h"
#include "instructions.h"
#include "optimisations.h"
#include "buildtree.h"

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)
#define for_li(v,i,l,n) for(v=(l),i=0;v!=NULL;v=v->n,++i)
#define for_ll(v1,v2,l1,l2,n1,n2) for(v1=(l1),v2=(l2);v1!=NULL;v1=v1->n1,v2=v2->n2)
#define for_la(v1,v2,l1,l2,n1) for(v1=(l1),v2=(l2);v1!=NULL;v1=v1->n1,++v2)

static void error_in_function (char *m)
{
	ErrorInCompiler ("codegen2.c",m,"");
}

char *Co_Wtype 	= "incorrect type";
char *Co_Wspine	= "non-terminating rule specified";

char else_symb[] = "else";
char then_symb[] = "then";
char notused_string[] = "notused";

SymbDef ApplyDef,IfDef,SeqDef;

unsigned NewLabelNr,new_not_eq_z_label_n;

StateS StrictOnAState;
static StateS UnderEvalState,ProcIdState;

StateS OnAState;

Bool LazyTupleSelectors [MaxNodeArity-NrOfGlobalSelectors];

LabDef BasicDescriptors [NrOfObjects];
int ObjectSizes [NrOfObjects];

static void InitBasicDescriptor (ObjectKind kind,char *name,int size)
{
	BasicDescriptors[kind].lab_mod		= NULL;
	BasicDescriptors[kind].lab_pref		= no_pref;
	BasicDescriptors[kind].lab_issymbol	= False;
	BasicDescriptors[kind].lab_name		= name;
	BasicDescriptors[kind].lab_post		= 0;
	ObjectSizes[kind]					= size;
}

Bool EqualState (StateS st1,StateS st2)
{
	if (IsSimpleState (st1) && IsSimpleState (st2))
		return st1.state_kind==st2.state_kind;

	switch (st1.state_type){
		case RecordState:
			return st2.state_type==RecordState;
		case TupleState:
			if (st2.state_type==TupleState && st1.state_arity==st2.state_arity){
				int i;
				
				for (i=0; i<st1.state_arity; i++)
					if (!EqualState (st1.state_tuple_arguments[i],st2.state_tuple_arguments[i]))
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

/* int InitAStackTop,InitBStackTop; */

void NewEmptyNode (int *asp_p,int nrargs)
{
	GenCreate (nrargs);
	*asp_p += SizeOfAStackElem;
}

void save_node_id_state (NodeId node_id,SavedNidStateS **saved_nid_state_l)
{
	SavedNidStateP new_saved_state;
	
	new_saved_state=CompAllocType (SavedNidStateS);
	
	new_saved_state->save_state=node_id->nid_state;
	new_saved_state->save_node_id=node_id;
	
	new_saved_state->save_next=*saved_nid_state_l;
	*saved_nid_state_l=new_saved_state;
}

void restore_saved_node_id_states (SavedNidStateP saved_node_id_states)
{
	while (saved_node_id_states){
		saved_node_id_states->save_node_id->nid_state_=saved_node_id_states->save_state;
		saved_node_id_states=saved_node_id_states->save_next;
	}
}

static Bool CopyArgument (StateS demstate,StateS offstate,int aindex,int bindex,int *asp_p,int *bsp_p,int offasize,int offbsize,Bool newnode);

static void GenProcIdCalculation (Node node,Annotation annot,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	if (annot==ParallelAtAnnot){
		Node procidnode;
	
		procidnode=get_p_at_node (node);
		if (procidnode->node_kind!=NodeIdNode)
			Build (procidnode,asp_p,bsp_p,code_gen_node_ids_p);
		else {
			int asize,bsize;
			NodeId nid;
			
			nid=procidnode->node_node_id;
	
			DetermineSizeOfState (nid->nid_state,&asize,&bsize);
			CopyArgument (ProcIdState,nid->nid_state,nid->nid_a_index,nid->nid_b_index,asp_p,bsp_p,asize,bsize,False);
		}
	} else {
		GenNewP();
		++*bsp_p;
	}
}

static void GenRedIdCalculation (Node redidnode,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	if (redidnode){
		if (redidnode->node_kind!=NodeIdNode)
			Build (redidnode,asp_p,bsp_p,code_gen_node_ids_p);
		else {
			int asize,bsize;
			NodeId nid;
			
			nid=redidnode->node_node_id;
	
			DetermineSizeOfState (nid->nid_state,&asize, &bsize);
			CopyArgument (ProcIdState,nid->nid_state,nid->nid_a_index,nid->nid_b_index,asp_p,bsp_p,asize, bsize, False);
		}
	} else
		GenPushReducerId (-1);
}

static char *GetReducerCode (Annotation annot)
{
	switch (annot)
	{	case ParallelAnnot:
		case ParallelAtAnnot:
			return ext_hnf_reducer_code;
		case ParallelNFAnnot:
			return ext_nf_reducer_code;
		default:
			return "";
	}
}

void UnpackRecord (int aindex,int *asp_p,int *bsp,Bool removeroot,int arity,States argstates)
{
	int	asize,bsize;
	
	DetermineSizeOfStates (arity, argstates, & asize, & bsize);

	if (removeroot)
		GenReplRArgs (asize, bsize);
	else
		GenPushRArgs (*asp_p - aindex, asize , bsize);
	*asp_p += asize;
	*bsp += bsize;
}

static void UnpackArrayOnTopOfStack (void)
{
	GenPushArray (0);
#if UPDATE_POP
	GenUpdatePopA (0,1);
#else
	GenUpdateA (0,1);
	GenPopA (1);
#endif
}

void UnpackArray (int aindex, int *asp_p, Bool removeroot)
{
	if (removeroot){
		GenPushArray (0);
#if UPDATE_POP
		GenUpdatePopA (0,1);
#else
		GenUpdateA (0,1);
		GenPopA (1);
#endif
	} else
		GenPushArray (*asp_p - aindex);
	
	*asp_p += SizeOfAStackElem;	
}

#if 0 /* defined (_WINDOWS_) */
void trace (char *s)
{
	DWORD n_chars;

	n_chars = strlen (s);
	WriteFile (GetStdHandle (STD_OUTPUT_HANDLE),s,n_chars,&n_chars,NULL);
}

void trace_si (char *e,int i)
{
	DWORD n_chars;
	char s[128];
	
	sprintf (s,"%s %d\n",e,i);

	n_chars = strlen (s);
	WriteFile (GetStdHandle (STD_OUTPUT_HANDLE),s,n_chars,&n_chars,NULL);
}
#endif

Coercions CoerceStateKind (StateKind dem_state_kind, StateKind off_state_kind)
{
	if (dem_state_kind==Undefined)
		error_in_function ("CoerceStateKind");

	switch (off_state_kind){
		case OnB:
			if (dem_state_kind == OnB)
				return BToB;
			else
				return BToA;
		case OnA:
		case SemiStrict:
		case LazyRedirection:
			if (dem_state_kind == OnA)
				return AToA;
			else
				return Reduce;
		case StrictOnA:
			if (dem_state_kind == OnB)
				return AToB;
			else
				return AToA;
		case StrictRedirection:
			if (dem_state_kind == OnB)
				return AToB;
			else if (dem_state_kind == StrictRedirection)
				return AToA;
			else
				return AToRoot;
		case Parallel:
			if (dem_state_kind == OnA)
				return AToA;
			else
				StaticMessage (False, "","parallel annotation in strict context ignored");
				return Reduce;
		case UnderEval:
			if (dem_state_kind == OnA)
				return MayBecomeCyclicSpine;
			else
				return CyclicSpine;
		default:
			error_in_function ("CoerceStateKind");
			return AToA;
	}
}

Bool TypeErrorFound, CycleErrorFound;

void GenReduceError (void)
{
	GenDAStackLayout (0);
	GenJsr (&cycle_lab);
	GenOAStackLayout (0);

	CycleErrorFound = True;
}

Coercions CoerceSimpleStateArgument (StateS demstate,StateKind offkind,int aindex,int *asp_p,Bool leaveontop, Bool *ontop)
{
	Coercions c;

	/* Examine the argument states to see whether it has to be reduced */

	if (IsSimpleState (demstate))
		c = CoerceStateKind (demstate.state_kind, offkind);
	else
		c = CoerceStateKind (StrictOnA, offkind);

	switch (c){
		case Reduce:
			if (leaveontop){
				GenPushA (*asp_p - aindex);
				GenJsrEval (0);
				*asp_p += SizeOfAStackElem;
				*ontop = True;
			} else {
				GenJsrEval (*asp_p - aindex);
				*ontop = False;
			}
			break;
		case MayBecomeCyclicSpine:
			GenCreate (-1);
			*asp_p += SizeOfAStackElem;
			*ontop = True;
			break;
		case CyclicSpine:
			GenReduceError ();
			StaticMessage (False,CurrentAltLabel.lab_symbol->sdef_ident->ident_name,Co_Wspine);
			*ontop = False;
			break;
		default:
			*ontop = False;
			break;
	}

	return c;
}

static StateKind AdjustStateKind (StateKind statekind, Coercions c)
{
	switch (c){
		case Reduce:
			return StrictOnA;
		case MayBecomeCyclicSpine:
			return OnA;
		default:
			return statekind;
	}
}

static void CoerceArgumentsUsingStackFrames (int arity,States demstates,States offstates,int aindex,int bindex,
											 int *asp_p, int *bsp, int *anext, int *bnext, int asize, int bsize);
	
void CoerceArgumentUsingStackFrames (StateS demstate, StateS offstate,int aindex,int bindex,int *asp_p,int *bsp,
									 int *anext,int *bnext,int asize,int bsize)
{
	if (IsSimpleState (demstate) && demstate.state_kind==Undefined)
		return;

	if (IsSimpleState (offstate)){
		Coercions c;
		Bool ontop;
		StateKind offkind;

		ontop = False;
		offkind = offstate.state_kind;
		
		c = CoerceSimpleStateArgument (demstate, offkind, aindex, asp_p, False, &ontop);
		offkind = AdjustStateKind (offkind, c);
		
		Assume (! ontop,"codegen","CoerceArgumentUsingStackFrames");

		if (IsSimpleState (demstate)){
			switch (CoerceStateKind (demstate.state_kind, offkind)){
				case AToA:
				case AToRoot:
					PutInAFrames (aindex, anext);
					return;
				case AToB:
					PushBasicFromAOnB (demstate.state_object, *asp_p - aindex);
					*bsp += ObjectSizes [demstate.state_object];
					PutInBFrames (*bsp, bnext, ObjectSizes [demstate.state_object]);
					return;
				case BToA:
					++*asp_p;
					BuildBasicFromB (offstate.state_object,*bsp - bindex);
					PutInAFrames (*asp_p, anext);
					return;
				case BToB:
					PutInBFrames (bindex, bnext, ObjectSizes [demstate.state_object]);
					return;
				default:
					;
			}
		} else {
			switch (demstate.state_type){
				case TupleState:
				/*	
					A tuple is demanded whereas a node is offered.
					Each argument is converted to its demanded state. Note that
					the offered state of each argument after pushing it on
					the stack is 'OnAState'.
				*/
				{
					int i,arity,index;
					States argstates;
	
					arity = demstate.state_arity;
					argstates = demstate.state_tuple_arguments;
				
					GenPushArgs (*asp_p - aindex, arity, arity);
					*asp_p += arity;
					index = *asp_p;
	
					for (i=arity-1; i>=0; i--)
						CoerceArgumentUsingStackFrames (argstates [i], OnAState,index-i, 0, asp_p, bsp, anext, bnext, 1, 0);
					break;
				}
				case RecordState:
				{
					int asize,bsize,arity;
					States argstates;
	
					arity = demstate.state_arity;
					argstates = demstate.state_record_arguments;
					
					DetermineSizeOfStates (arity, argstates, &asize, &bsize);
					GenPushRArgs (*asp_p - aindex, asize , bsize);
					*asp_p += asize;
					*bsp += bsize;
					CoerceArgumentsUsingStackFrames (arity, argstates, argstates,*asp_p,*bsp, asp_p, bsp, anext, bnext, asize, bsize);
					break;
				}
				case ArrayState:
					GenPushArray (*asp_p-aindex);
					*asp_p += 1;
					PutInAFrames (*asp_p, anext);
					break;
			}
		}
	} else if (IsSimpleState (demstate)){
		switch (offstate.state_type){
			case TupleState:
				BuildTuple (aindex, bindex, *asp_p, *bsp,offstate.state_arity, offstate.state_tuple_arguments,
							asize,bsize,*asp_p,NormalFill,True);
				*asp_p += SizeOfAStackElem;
				break;
			case RecordState:
				BuildNewRecord (offstate.state_record_symbol,aindex,bindex,*asp_p,*bsp,asize,bsize);
				*asp_p += SizeOfAStackElem;
				break;
			case ArrayState:
				GenBuildArray (*asp_p-aindex);
				++*asp_p;
				break;
		}
		PutInAFrames (*asp_p, anext);
	} else {
		switch (offstate.state_type){
			case TupleState:		
				CoerceArgumentsUsingStackFrames
					(demstate.state_arity, demstate.state_tuple_arguments,
					 offstate.state_tuple_arguments, aindex, bindex, asp_p, bsp, anext, bnext,
					 asize, bsize);
				break;
			case RecordState:
				CoerceArgumentsUsingStackFrames
					(demstate.state_arity,demstate.state_record_arguments,
					 offstate.state_record_arguments, aindex, bindex, asp_p, bsp, anext, bnext,
					 asize, bsize);
				break;
			case ArrayState:
				PutInAFrames (aindex, anext);
				break;
		}
	}
}

static void CoerceArgumentsUsingStackFrames (int arity, StateS demstates[], StateS offstates[],int aindex, int bindex, 
											int *asp_p, int *bsp, int *anext, int *bnext,int asize, int bsize)
{
	int i;
	
	aindex -= asize;
	bindex -= bsize;

	for (i=arity-1; i>=0; i--){
		int asize,bsize;

		DetermineSizeOfState (offstates[i],&asize, &bsize);
		aindex += asize;
		bindex += bsize;

		CoerceArgumentUsingStackFrames (demstates [i],offstates [i],aindex,bindex,asp_p,bsp,anext,bnext,asize,bsize);
	}
}

void AdjustTuple (int localasp,int localbsp,int *asp_p,int *bsp_p,int arity,StateS demstates[],StateS offstates[],int asize,int bsize)
{
	int a_ind,b_ind,dummy,oldamax,oldbmax,newamax,newbmax;

	a_ind=0;
	b_ind=0;
	dummy = 0,
	
	newamax = localasp + 1 + arity;
	newbmax = localbsp + 1;
	AddStateSizesAndMaxFrameSizes (arity, demstates, &newamax, &dummy, &newbmax);
	
	InitStackConversions  (newamax, newbmax, &oldamax, &oldbmax);
	
	CoerceArgumentsUsingStackFrames (arity, demstates, offstates, localasp, localbsp,
									&localasp, &localbsp, &a_ind, &b_ind, asize, bsize);
	
	GenAStackConversions (localasp,a_ind);
	GenBStackConversions (localbsp,b_ind);
	
	ExitStackConversions (oldamax, oldbmax);

	*asp_p += a_ind-asize;
	*bsp_p += b_ind-bsize;	
}

void UnpackTuple (int aindex,int *asp_p,int *bsp_p,Bool removeroot,int arity,StateS argstates[])
{
	int	aselmts,oldaframesize,locasp,asize,maxasize;

	aselmts = 0;
	locasp = arity;
	asize = 0;
	maxasize = arity;

	if (removeroot)
		GenReplArgs (arity, arity);
	else
		GenPushArgs (*asp_p- aindex, arity, arity);

	AddStateSizesAndMaxFrameSizes (arity, argstates, &maxasize, &asize,bsp_p);

	InitAStackConversions (maxasize+1, &oldaframesize);

	EvaluateAndMoveArguments (arity,argstates,&locasp,&aselmts);

	GenAStackConversions (locasp,aselmts);

	FreeAFrameSpace	(oldaframesize);
	*asp_p += aselmts;
}

static void MoveArgumentsFromBToA (int arity,States argstates,int aindex,int bindex,int asp_p,int bsp,int asize,int bsize)
{
	int i;
	
	aindex -= asize;
	bindex -= bsize;
	
	for (i=arity-1; i>=0; i--){
		DetermineSizeOfState (argstates[i],&asize, &bsize);
		aindex += asize;
		bindex += bsize;

		PackArgument  (argstates[i], aindex, bindex, asp_p, bsp, asize, bsize);

		asp_p++;
	}
}

void BuildTuple (int aindex,int bindex,int asp_p,int bsp,int arity,
				 States argstates,int asize,int bsize,int rootindex,FillKind fkind,Bool newnode)
{
	MoveArgumentsFromBToA (arity, argstates, aindex, bindex, asp_p, bsp, asize, bsize);
	if (newnode)
		GenBuildh (&tuple_lab,arity);
	else
		GenFillh (&tuple_lab,arity,arity+asp_p-rootindex,fkind);
}

void BuildRecord (SymbDef record_sdef,int aindex,int bindex,int asp,int bsp,int asize,int bsize,int rootindex,FillKind fkind,Bool newnode)
{
	LabDef record_lab;
	
	ConvertSymbolToRLabel (&record_lab,record_sdef);

	if (newnode)
		GenBuildR (&record_lab,asize,bsize,asp-aindex,bsp-bindex);
	else
		GenFillR (&record_lab,asize,bsize,asp-rootindex,asp-aindex,bsp-bindex,fkind,False);
}

void BuildNewRecord (SymbDef record_sdef,int aindex,int bindex,int asp,int bsp,int asize,int bsize)
{
	LabDef record_lab;
	
	ConvertSymbolToRLabel (&record_lab,record_sdef);

	GenBuildR (&record_lab,asize,bsize,asp-aindex,bsp-bindex);
}

void BuildNewRecordPop (SymbDef record_sdef,int asize,int bsize)
{
	LabDef record_lab;
	
	ConvertSymbolToRLabel (&record_lab,record_sdef);

	GenBuildhr (&record_lab,asize,bsize);
}

void PackArgument (StateS argstate,int aindex,int bindex,int asp,int bsp,int offasize,int offbsize)
{
	if (IsSimpleState (argstate)){
		if (argstate.state_kind==OnB)
			BuildBasicFromB (argstate.state_object,bsp - bindex);
		else
			GenPushA (asp - aindex);
	} else {
		switch (argstate.state_type){
			case TupleState:
				BuildTuple (aindex, bindex, asp, bsp,argstate.state_arity, argstate.state_tuple_arguments,
							offasize,offbsize,asp,NormalFill,True);
				return;
			case RecordState:
				BuildNewRecord (argstate.state_record_symbol,aindex,bindex,asp,bsp,offasize,offbsize);
				return;
			case ArrayState:
				GenBuildArray (asp - aindex);
				return;
		}
	}		
}

void CoerceArgumentOnTopOfStack (int *asp_p,int *bsp_p,StateS argstate,StateS nodestate,int asize,int bsize)
{
	if (IsSimpleState (argstate) && argstate.state_kind==Undefined){
		GenPopA (asize);
		*asp_p-=asize;
		GenPopB (bsize);
		*bsp_p-=bsize;
	} else if (IsSimpleState (nodestate)){
		if (IsSimpleState (argstate)){
			Coercions c;
		
			c = CoerceStateKind (argstate.state_kind, nodestate.state_kind);
			
			if (c==Reduce){
				GenJsrEval (0);
				c = CoerceStateKind (argstate.state_kind, StrictOnA);
			}
			switch (c){
				case AToB:
					PushBasicFromAOnB (argstate.state_object, 0);
					*bsp_p+=ObjectSizes [argstate.state_object];
					GenPopA (1);
					*asp_p-=1;
					return;
				case BToA:
					++*asp_p;
					BuildBasicFromB (nodestate.state_object,0);
					GenPopB (bsize);
					*bsp_p-=bsize;
					return;
				case AToA:
				case AToRoot:
					return;
				case BToB:
					return;
				default:
					;
			}
		} else {
			if (CoerceStateKind (StrictOnA, nodestate.state_kind)==Reduce)
				GenJsrEval (0);

			switch (argstate.state_type){
				case TupleState: /* a tuple is demanded but not offered */
					*asp_p-=1;
					UnpackTuple (*asp_p,asp_p,bsp_p,True,argstate.state_arity, argstate.state_tuple_arguments);
					break;
				case RecordState:
					*asp_p-=1;
					UnpackRecord (*asp_p,asp_p,bsp_p,True,argstate.state_arity,argstate.state_record_arguments);
					break;
				case ArrayState:
					UnpackArrayOnTopOfStack();
					break;
			}
		}
	} else if (IsSimpleState (argstate)){
		/* a tuple or record is offered but not demanded */

		switch (nodestate.state_type){
			case TupleState:
				BuildTuple (*asp_p,*bsp_p,*asp_p,*bsp_p,nodestate.state_arity,nodestate.state_tuple_arguments,
							asize,bsize,*asp_p,NormalFill,True);
				*asp_p+=1;
				GenUpdatePopA (0,asize);
				*asp_p-=asize;
				GenPopB (bsize);
				*bsp_p-=bsize;
				break;
			case RecordState:
				BuildNewRecordPop (nodestate.state_record_symbol,asize,bsize);
				*asp_p+=1;
				*asp_p-=asize;
				*bsp_p-=bsize;
				break;
			case ArrayState:
				if (asize==1)
					GenBuildArrayPop();
				else {
					GenBuildArray (0);
					++*asp_p;
					GenUpdatePopA (0,asize);
					*asp_p-=asize;
				}
				GenPopB (bsize);
				*bsp_p-=bsize;
				break;
		}
	} else {
		if (argstate.state_type==TupleState)
			AdjustTuple (asize,bsize,asp_p,bsp_p,argstate.state_arity,
						argstate.state_tuple_arguments, nodestate.state_tuple_arguments,asize, bsize);
	}
}

#define HasBeenReduced(c)	((c)==Reduce)

static void CopyArguments (States demstates,States offstates,int arity,int aindex,int bindex,int *asp_p,int *bsp,int aszie,int bsize);

static Bool CopyArgument (StateS demstate,StateS offstate,int aindex,int bindex,int *asp_p,int *bsp_p,int offasize,int offbsize,Bool newnode)
{
	if (IsSimpleState (demstate) && demstate.state_kind==Undefined)
		return False;

	if (IsSimpleState (offstate)){
		Bool leftontop;
		Coercions c;
		StateKind offkind;
		
		offkind = offstate.state_kind;

		c = CoerceSimpleStateArgument (demstate, offkind, aindex, asp_p, True, &leftontop);
		offkind = AdjustStateKind (offkind, c);

		if (IsSimpleState (demstate)){
			StateKind demkind;
		
			demkind = demstate.state_kind;
			switch (CoerceStateKind (demkind, offkind)){
				case AToB:
					PushBasicFromAOnB (demstate.state_object, *asp_p - aindex);
					*bsp_p += ObjectSizes [demstate.state_object];
					if (leftontop){
						GenPopA (1);
						*asp_p -= SizeOfAStackElem;
					}
					break;
				case BToA:
					if (newnode){
						++*asp_p;
						BuildBasicFromB (offstate.state_object,*bsp_p - bindex);
					} else
						FillBasicFromB (offstate.state_object,*bsp_p - bindex,0,NormalFill);
					break;
				case BToB:
					PushBasicOnB (demstate.state_object, *bsp_p - bindex);
					*bsp_p += ObjectSizes [demstate.state_object];
					break;
				case AToA:
				case AToRoot:
					if (leftontop){
						if (!newnode)
							GenFillFromA (0, 1, NormalFill);
					} else {
						if (newnode){
							GenPushA (*asp_p - aindex);
							*asp_p += SizeOfAStackElem;
						} else
							GenFillFromA (*asp_p - aindex, 0, NormalFill);
					}
					break;
				default:
					break;
			}
		} else {
			if (leftontop)
				*asp_p -= SizeOfAStackElem;
			switch (demstate.state_type){
				case TupleState:
					UnpackTuple (aindex, asp_p,bsp_p,leftontop, demstate.state_arity,demstate.state_tuple_arguments);
					break;
				case RecordState:
					UnpackRecord (aindex, asp_p,bsp_p,leftontop, demstate.state_arity,demstate.state_record_arguments);
					break;
				case ArrayState:
					UnpackArray (aindex, asp_p, leftontop);
					break;
			}
		}
		return HasBeenReduced (c);
	}
	else if (IsSimpleState (demstate)){
		switch (offstate.state_type){
			case TupleState:
				BuildTuple (aindex, bindex, *asp_p, *bsp_p,offstate.state_arity, offstate.state_tuple_arguments,
					offasize, offbsize, *asp_p, NormalFill,newnode);
				if (newnode)
					*asp_p += SizeOfAStackElem;
				break;
			case RecordState:
				BuildRecord (offstate.state_record_symbol, aindex, bindex, *asp_p, *bsp_p,
							offasize, offbsize, *asp_p, NormalFill, newnode);
				if (newnode)
					*asp_p += SizeOfAStackElem;					
				break;
			case ArrayState:
				if (newnode){
					GenBuildArray (*asp_p - aindex);
					++*asp_p;
				} else
					GenFillArray (*asp_p - aindex, 0, NormalFill);
				break;
		}
		return False; /** to indicate that the offered object has not been changed **/
	} else {
		switch (offstate.state_type){
			case TupleState:
				CopyArguments (demstate.state_tuple_arguments,
					offstate.state_tuple_arguments, demstate.state_arity,
					aindex, bindex, asp_p, bsp_p, offasize, offbsize);
				break;
			case RecordState:
				CopyArguments (demstate.state_record_arguments,
					offstate.state_record_arguments, demstate.state_arity,
					aindex, bindex, asp_p, bsp_p, offasize, offbsize);
				break;
			case ArrayState:
				GenPushA (*asp_p - aindex);
				*asp_p += SizeOfAStackElem;
				break;
		}
		return False;
	}
}

static void CopyArguments (States demstates,States offstates,int arity,int aindex,int bindex,int *asp_p,int *bsp_p,int asize,int bsize)
{
	int i;
	
	aindex-= asize;
	bindex -= bsize;
	
	for (i=arity-1; i>=0; i--){
		DetermineSizeOfState (offstates[i],&asize, &bsize);
		aindex += asize;
		bindex += bsize;
		CopyArgument  (demstates[i],offstates[i],aindex,bindex,asp_p,bsp_p,asize,bsize,True);
	}
}

static void CreateParallelCode (NodeDefs nds,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	switch (nds->def_node->node_annotation){
		case ParallelAnnot:
		case ParallelAtAnnot:
		case ParallelNFAnnot:
			if (nds->def_id->nid_mark & ON_A_CYCLE_MASK){
				/* the channel has already been created */
				GenSendGraph (GetReducerCode (nds->def_node->node_annotation), 0,*asp_p-nds->def_id->nid_a_index);
				GenPopA (1);
				*asp_p -= SizeOfAStackElem;
			} else {
				GenProcIdCalculation (nds->def_node,nds->def_node->node_annotation,asp_p,bsp_p,code_gen_node_ids_p);
				GenCreateChannel (channel_code);
				--*bsp_p;
				GenSendGraph (GetReducerCode (nds->def_node->node_annotation), 1, 0);
				GenUpdateA (0, 1);
				GenPopA (1);
			}
			break;
		case InterleavedAnnot:
			GenNewInterleavedReducer (*asp_p-nds->def_id->nid_a_index, hnf_reducer_code);
			break;
		case ContinueAnnot:
			if (get_p_at_node (nds->def_node)){
				GenRedIdCalculation (get_p_at_node (nds->def_node),asp_p,bsp_p,code_gen_node_ids_p);
				SetContinueOnReducer (*asp_p-nds->def_id->nid_a_index);
			} else
				SetContinue (*asp_p-nds->def_id->nid_a_index);
			break;
		case ContInterleavedAnnot:
			GenNewContInterleavedReducer (*asp_p-nds->def_id->nid_a_index);
			break;
		case WaitAnnot:
			GenSetRedId (*asp_p-nds->def_id->nid_a_index);
			break;
		case InterleavedNFAnnot:
			GenNewInterleavedReducer (*asp_p-nds->def_id->nid_a_index, nf_reducer_code);
			break;
	}
}

void ChangeEvalStatusKindToStrictOnA (NodeId node_id,SavedNidStateS **saved_nid_state_l)
{
	if (!IsSimpleState (node_id->nid_state))
		error_in_function ("ChangeEvalStatusKindToStrictOnA");

	if (saved_nid_state_l)
		save_node_id_state (node_id,saved_nid_state_l);

	node_id->nid_state__.state_kind = StrictOnA;
}

static void ChangeEvalStatusKind (NodeId noid, StateKind state)
{
	if (noid){
		if (!IsSimpleState (noid->nid_state))
			error_in_function ("ChangeEvalStatusKind");
		noid->nid_state__.state_kind = state;
	}
}

static void ReduceSemiStrictNodes (const NodeDefs nds,int asp)
{
	NodeDefs nd;
	int has_parallel_state;
	
	has_parallel_state=0;
	
	for_l (nd,nds,def_next){
		if (IsSimpleState (nd->def_id->nid_state)){
			switch (nd->def_id->nid_state.state_kind){
				case SemiStrict:
					if (nd->def_node->node_state.state_mark & STATE_PARALLEL_MASK){
						has_parallel_state=1;
						continue;
					}

					ChangeEvalStatusKind (nd->def_id, StrictOnA);
					/* evaluate strict annotated */
					GenJsrEval (asp - nd->def_id->nid_a_index);
					break;
				case Parallel:
					StaticMessage (False, "","parallel annotation ignored(?)");
					break;
			}
		}
	}

	if (has_parallel_state)
		for_l (nd,nds,def_next){
			if (IsSimpleState (nd->def_id->nid_state)){
				if (nd->def_id->nid_state.state_kind==SemiStrict){
					ChangeEvalStatusKind (nd->def_id, StrictOnA);
					/* evaluate strict annotated */
					GenJsrEval (asp - nd->def_id->nid_a_index);
				}
			}
		}
}

void BuildOrFillLazyFieldSelector (SymbDef selector_sdef,StateKind result_state_kind,int *asp_p,NodeId update_node_id)
{
	LabDef nsellab,dsellab;
	char *record_name;
	int fill_arity;
	SymbDef record_sdef;
	StateS *field_result_state_p;

	ConvertSymbolToDandNLabel (&dsellab,&nsellab,selector_sdef);

	record_sdef=selector_sdef->sdef_type->type_lhs->ft_symbol->symb_def;
	record_name=record_sdef->sdef_ident->ident_name;

	field_result_state_p=&record_sdef->sdef_record_state.state_record_arguments [selector_sdef->sdef_sel_field_number];
	fill_arity= IsSimpleState (*field_result_state_p) ? (field_result_state_p->state_kind!=OnB ? -4 : -3) : 1;

	/* we use a negative arity to indicate lazy selectors */
	if (update_node_id==NULL)
		GenBuildFieldSelector (&dsellab,&nsellab,record_name,fill_arity);
	else {
		GenFillFieldSelector (&dsellab,&nsellab,record_name,fill_arity,*asp_p-update_node_id->nid_a_index,result_state_kind==SemiStrict ? PartialFill : NormalFill);
		*asp_p-=1;
	}
}

void ReplaceRecordOnTopOfStackByField (int *asp_p,int *bsp_p,int apos,int bpos,int asize,int bsize,int rec_a_size,int rec_b_size) 
{
	int i;

	rec_a_size -= asize;
	rec_b_size -= bsize;

	for (i = asize - 1; i >= 0; i--)
#if UPDATE_POP
		if (i==0)
			GenUpdatePopA (apos,rec_a_size);
		else
#endif
		GenUpdateA (apos + i, rec_a_size + i);

	for (i = bsize - 1; i >= 0; i--)
#if UPDATE_POP
		if (i==0)
			GenUpdatePopB (bpos,rec_b_size);
		else
#endif
		GenUpdateB (bpos + i, rec_b_size + i);

#if UPDATE_POP
	if (asize==0)
#endif
		GenPopA (rec_a_size);
	*asp_p-=rec_a_size;

#if UPDATE_POP
	if (bsize==0)
#endif
		GenPopB (rec_b_size);

	*bsp_p-=rec_b_size;
}

#define ResultIsNotInRootNormalForm(state) (IsLazyState (state) ||\
	IsSimpleState (state) && (state).state_kind == LazyRedirection)

void add_node_id_to_list (struct node_id *node_id,NodeIdListElementS **node_ids_l)
{
	NodeIdListElementP free_node_id;
							
	free_node_id=CompAllocType (NodeIdListElementS);
	free_node_id->nidl_node_id=node_id;

	free_node_id->nidl_next=*node_ids_l;
	*node_ids_l=free_node_id;
}

#if 0
# include "dbprint.h"
#endif

#if FREE_STRICT_LHS_TUPLE_ELEMENTS
static void add_node_id_or_tuple_node_ids_to_list (NodeIdP node_id,NodeIdListElementS **free_node_ids_l)
{
	if (! (node_id->nid_node!=NULL && !IsSimpleState (node_id->nid_state)))
		add_node_id_to_list (node_id,free_node_ids_l);
	else {
		ArgP arg_p;
		
		for_l (arg_p,node_id->nid_node->node_arguments,arg_next){
			NodeP arg_node_p;
			
			arg_node_p=arg_p->arg_node;
			if (arg_node_p->node_kind==NodeIdNode){
				NodeIdP node_id_p;
				
				node_id_p=arg_node_p->node_node_id;
				if (node_id_p->nid_refcount==-1)
					add_node_id_or_tuple_node_ids_to_list (node_id_p,free_node_ids_l);								
			}
		}
	}
}
#endif

void decrement_reference_count_of_node_id (struct node_id *node_id,NodeIdListElementS **free_node_ids_l)
{
	int ref_count;

#if 0
	printf ("decrement_reference_count_of_node_id ");
	DPrintNodeId (node_id,StdOut);
	printf ("\n");
#endif	

	ref_count=node_id->nid_refcount;

	if (ref_count>0){
		if (--ref_count==0)
			add_node_id_to_list (node_id,free_node_ids_l);

		node_id->nid_refcount=ref_count;
	} else if (ref_count<-1){
		++ref_count;
		node_id->nid_refcount=ref_count;

		if (ref_count==-1){
#if FREE_STRICT_LHS_TUPLE_ELEMENTS
			if (unused_node_id_(node_id))
				add_node_id_or_tuple_node_ids_to_list (node_id,free_node_ids_l);
#else
			if (! (node_id->nid_node!=NULL && !IsSimpleState (node_id->nid_state)) && unused_node_id_(node_id)){
# if 0
				printf ("add to free_node_ids list ");
				DPrintNodeId (node_id,StdOut);
				printf ("\n");
# endif	
				add_node_id_to_list (node_id,free_node_ids_l);
			}
#endif
		}
	}
}

void DetermineFieldSizeAndPositionAndRecordSize
	(int fieldnr,int *asize_p,int *bsize_p,int *apos_p,int *bpos_p,int *rec_asize_p,int *rec_bsize_p,StateS *record_state_p)
{
	int i;
	
	DetermineFieldSizeAndPosition (fieldnr,asize_p,bsize_p,apos_p,bpos_p,record_state_p->state_record_arguments);
			
	*rec_asize_p = *asize_p + *apos_p;
	*rec_bsize_p = *bsize_p + *bpos_p;

	for (i=fieldnr+1; i<record_state_p->state_arity; ++i)
		AddSizeOfState (record_state_p->state_record_arguments[i],rec_asize_p,rec_bsize_p);
}

int get_a_index_of_unpacked_lhs_node (ArgS *arg)
{
	while (arg!=NULL){
		int a_size,b_size;
		
		DetermineSizeOfState (arg->arg_state,&a_size,&b_size);

		if (a_size==0)
			arg=arg->arg_next;
		else {
			Node arg_node;
			
			arg_node=arg->arg_node;
			
			if (arg_node->node_kind==NodeIdNode){
				NodeId node_id;
				node_id=arg->arg_node->node_node_id;
				
				if (a_size!=0){
					if (node_id->nid_refcount<0 && node_id->nid_state.state_type!=SimpleState && node_id->nid_node!=NULL)
						arg=node_id->nid_node->node_arguments;
					else
						return node_id->nid_a_index;
				}
			} else
				arg=arg_node->node_arguments;
		}
	}
	
	return 0;
}

int get_b_index_of_unpacked_lhs_node (ArgS *arg)
{
	while (arg!=NULL){
		int a_size,b_size;
		
		DetermineSizeOfState (arg->arg_state,&a_size,&b_size);

		if (b_size==0)
			arg=arg->arg_next;
		else {
			Node arg_node;

			arg_node=arg->arg_node;

			if (arg_node->node_kind==NodeIdNode){
				NodeId node_id;
				
				node_id=arg->arg_node->node_node_id;
							
				if (node_id->nid_refcount<0 && node_id->nid_state.state_type!=SimpleState && node_id->nid_node!=NULL)
					arg=node_id->nid_node->node_arguments;
				else
					return node_id->nid_b_index;
			} else
				arg=arg_node->node_arguments;
		}
	}
	
	return 0;
}

Bool CopyNodeIdArgument (StateS demstate,NodeId node_id,int *asp_p,int *bsp_p)
{
	int a_size,b_size,a_index,b_index;
	
	DetermineSizeOfState (node_id->nid_state,&a_size,&b_size);

	a_index=node_id->nid_a_index;
	b_index=node_id->nid_b_index;

	if (node_id->nid_refcount<0 && node_id->nid_state.state_type!=SimpleState && node_id->nid_node!=NULL){
		ArgS *args;
		
		args=node_id->nid_node->node_arguments;
		
		if (a_size!=0)
			a_index=get_a_index_of_unpacked_lhs_node (args);
		if (b_size!=0)
			b_index=get_b_index_of_unpacked_lhs_node (args);
	}
	
	return CopyArgument (demstate,node_id->nid_state,a_index,b_index,asp_p,bsp_p,a_size,b_size,True);
}

void PushField (StateS recstate,int fieldnr,int offset,int *asp_p,int *bsp_p,int *a_size_p,int *b_size_p)
{
	int apos,bpos,totasize,totbsize;

	DetermineFieldSizeAndPositionAndRecordSize (fieldnr,a_size_p,b_size_p,&apos,&bpos,&totasize,&totbsize,&recstate);
	
	GenPushRArgB (offset, totasize, totbsize, bpos+1, *b_size_p);
	GenPushRArgA (offset, totasize, totbsize, apos+1, *a_size_p);
	*bsp_p += *b_size_p;
	*asp_p += *a_size_p;
}

void ReplaceRecordByField (StateS recstate,int fieldnr,int *asp_p,int *bsp_p,int *a_size_p,int *b_size_p)
{
	int apos,bpos,totasize,totbsize;

	DetermineFieldSizeAndPositionAndRecordSize (fieldnr,a_size_p,b_size_p,&apos,&bpos,&totasize,&totbsize,&recstate);
	
	GenPushRArgB (0, totasize, totbsize, bpos+1, *b_size_p);
	GenReplRArgA (   totasize, totbsize, apos+1, *a_size_p);
	*bsp_p += *b_size_p;
	*asp_p += *a_size_p - 1;
}

#ifdef OPTIMIZE_SELECTION_OF_SELECTION_OF_UNBOXED_RECORD_FIELD
/* yields NULL if coerced to node_state */
static struct state *FillOrReduceFieldSelection_of_selection_in_single_field_unboxable_record (Node node,SymbDef seldef,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	Node arg_node;
	Args arg;
	int	fieldnr;

	arg=node->node_arguments;
	fieldnr=seldef->sdef_sel_field_number;
	arg_node=arg->arg_node;
	if (arg_node->node_kind!=NodeIdNode){
		if (IsLazyState (node->node_state)){
			if (arg_node->node_kind==SelectorNode && arg_node->node_arity==1){
				SymbDef record_sdef;
				StateP record_state_p;
				
				record_sdef=seldef->sdef_type->type_lhs->ft_symbol->symb_def;
				record_state_p=&record_sdef->sdef_record_state;
				if (record_state_p->state_type==RecordState && record_state_p->state_arity==1 && !record_sdef->sdef_boxed_record){
					int asize,bsize;
					struct state *record_field_state_p;

					/* record with one field may be unboxed in another record */
					ArgComment (arg);

					record_field_state_p = FillOrReduceFieldSelection_of_selection_in_single_field_unboxable_record (arg_node,arg_node->node_symbol->symb_def,asp_p,bsp_p,code_gen_node_ids_p);
					if (record_field_state_p!=NULL){
						DetermineSizeOfState (*record_field_state_p, &asize, &bsize);
						if (record_field_state_p->state_type==RecordState){
							CoerceArgumentOnTopOfStack (asp_p,bsp_p,*record_state_p,*record_field_state_p,asize,bsize);
							return &record_state_p->state_record_arguments[fieldnr];
						}

						CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,*record_field_state_p,asize,bsize);
					} else {
						DetermineSizeOfState (arg_node->node_state, &asize, &bsize);
						CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,arg_node->node_state,asize,bsize);
					}
				} else
					BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);
			} else
				BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);

#ifdef DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS
			if (!ResultIsNotInRootNormalForm (arg_node->node_state)){
				int asize,bsize,apos,bpos,tot_asize,tot_bsize;
				StateP record_state_p;
			
				record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
				if (record_state_p->state_type!=RecordState)
					error_in_function ("FillOrReduceFieldSelection_of_selection_in_single_field_unboxable_record");

				DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&asize,&bsize,&apos,&bpos,&tot_asize,&tot_bsize,record_state_p);
				
				GenPushRArgB (0,tot_asize,tot_bsize,bpos+1,bsize);
				GenReplRArgA (tot_asize,tot_bsize,apos+1,asize);
				
				*asp_p -= 1-asize;
				*bsp_p += bsize;
				
				return &record_state_p->state_record_arguments[fieldnr];
			} else
#endif
			BuildOrFillLazyFieldSelector (seldef,node->node_state.state_kind,asp_p,NULL);
		} else {
			int asize,bsize,apos,bpos,tot_asize,tot_bsize;
			StateP record_state_p;

			record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;

			Build (arg_node,asp_p,bsp_p,code_gen_node_ids_p);

			DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&asize,&bsize,&apos,&bpos,&tot_asize,&tot_bsize,record_state_p);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,*record_state_p,arg_node->node_state,tot_asize,tot_bsize);

			ReplaceRecordOnTopOfStackByField (asp_p,bsp_p,apos,bpos,asize,bsize,tot_asize,tot_bsize);
			
			return &record_state_p->state_record_arguments[fieldnr];
		}
	} else {
		StateS recstate;
		NodeId arg_node_id;
		
		arg_node_id=arg_node->node_node_id;
		recstate=arg_node_id->nid_state;
		if (recstate.state_type==RecordState){
			int a_size,b_size,apos,bpos,record_a_index,record_b_index;
			StateP field_state_p;

			DetermineFieldSizeAndPosition (fieldnr,&a_size,&b_size,&apos,&bpos,recstate.state_record_arguments);
			
			if (arg_node_id->nid_refcount<0 && arg_node_id->nid_node!=NULL){
				ArgS *args;
				
				args=arg_node_id->nid_node->node_arguments;
				record_a_index=get_a_index_of_unpacked_lhs_node (args);				
				record_b_index=get_b_index_of_unpacked_lhs_node (args);
			} else {
				record_a_index=arg_node_id->nid_a_index;
				record_b_index=arg_node_id->nid_b_index;
			}

			field_state_p=&recstate.state_record_arguments[fieldnr];

			/* maybe better way than calling CopyArgument with identical demanded and offered state ? */
			CopyArgument (*field_state_p,*field_state_p,record_a_index-apos,record_b_index-bpos,asp_p,bsp_p,a_size,b_size,True);

			decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
			
			return field_state_p;
		} else if (IsLazyState (node->node_state)){
#ifdef DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS
			if ((recstate.state_kind==StrictOnA || recstate.state_kind==StrictRedirection)){
				int asize,bsize,apos,bpos,tot_asize,tot_bsize,recindex;
				SymbDef record_sdef;
				StateP record_state_p;
			
				recindex = arg_node_id->nid_a_index;
				record_sdef=seldef->sdef_type->type_lhs->ft_symbol->symb_def;
				record_state_p=&record_sdef->sdef_record_state;

				if (record_state_p->state_type!=RecordState)
					error_in_function ("FillOrReduceFieldSelection_of_selection_in_single_field_unboxable_record");

				DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&asize,&bsize,&apos,&bpos,&tot_asize,&tot_bsize,record_state_p);
# if BOXED_RECORDS
 				if (record_sdef->sdef_boxed_record && (arg_node_id->nid_mark2 & (NID_RECORD_USED_BY_UPDATE | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_RECORD_USED_BY_UPDATE
					&&
					(((arg_node_id->nid_state.state_mark & STATE_UNIQUE_MASK)!=0
/*
					&&
					(arg_node_id->nid_refcount==-2 || ((arg_node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 && arg_node_id->nid_number== -1))
*/
					)
					||
					((arg_node_id->nid_mark2 & NID_SELECTION_NODE_ID)==0
					  ?	(arg_node_id->nid_refcount>=0 && arg_node_id->nid_node->node_kind==NodeIdNode && 
						 (arg_node_id->nid_node->node_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID)
					  :	(arg_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID
					  )
					)
				){
					GenPushRArgU (*asp_p-recindex,tot_asize,tot_bsize,apos+1,asize,bpos+1,bsize);
				} else
# endif
				{
					GenPushRArgB (*asp_p-recindex,tot_asize,tot_bsize,bpos+1,bsize);
					GenPushRArgA (*asp_p-recindex,tot_asize,tot_bsize,apos+1,asize);
				}
				
				*asp_p+=asize;
				*bsp_p+=bsize;
				
				decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
				
				return &record_state_p->state_record_arguments [fieldnr];
			} else
#endif
			{
				BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);
				
				BuildOrFillLazyFieldSelector (seldef,node->node_state.state_kind,asp_p,NULL);
			}
		} else {
			int a_size,b_size,apos, bpos, tot_asize, tot_bsize,recindex;
			SymbDef record_sdef;

			/* the selector is strict but the record is not */
			recindex = arg_node_id->nid_a_index;

			if (ResultIsNotInRootNormalForm (recstate)){
				GenJsrEval (*asp_p-recindex);
				ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
				
				recstate.state_kind = StrictOnA;
			}

			record_sdef=seldef->sdef_type->type_lhs->ft_symbol->symb_def;
			DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&a_size,&b_size,&apos, &bpos,&tot_asize,&tot_bsize,&record_sdef->sdef_record_state);
# if BOXED_RECORDS
			if (record_sdef->sdef_boxed_record && (arg_node_id->nid_mark2 & (NID_RECORD_USED_BY_UPDATE | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_RECORD_USED_BY_UPDATE
				&&
				((	(arg_node_id->nid_state.state_mark & STATE_UNIQUE_MASK)!=0
/*					&&
					(arg_node_id->nid_refcount==-2 || ((arg_node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 && arg_node_id->nid_number== -1))
*/
				)
				||
				((arg_node_id->nid_mark2 & NID_SELECTION_NODE_ID)==0
				  ?	(arg_node_id->nid_refcount>=0 && arg_node_id->nid_node->node_kind==NodeIdNode && 
						 (arg_node_id->nid_node->node_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID)
					  :	(arg_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID
					  )
				)
			){
				GenPushRArgU (*asp_p-recindex,tot_asize,tot_bsize,apos+1,a_size,bpos+1,b_size);
			} else
# endif
			{
				GenPushRArgB (*asp_p-recindex, tot_asize, tot_bsize, bpos+1,b_size);
				GenPushRArgA (*asp_p-recindex, tot_asize, tot_bsize, apos+1,a_size);
			}
			
			*asp_p+=a_size;
			*bsp_p+=b_size;

			decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
			
			return &record_sdef->sdef_record_state.state_record_arguments[fieldnr];
		}
	}

	return NULL;
}
#endif

static void FillOrReduceFieldSelection (Node node,SymbDef seldef,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	Node arg_node;
	Args arg;
	int	fieldnr;

	arg = node->node_arguments;
	fieldnr = seldef->sdef_sel_field_number;
	
	arg_node=arg->arg_node;

	if (node->node_arity>=SELECTOR_U){
		if (IsLazyState (node->node_state)){
			SymbDef new_select_sdef;
			LabDef name,codelab;

			BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);

			new_select_sdef=create_select_function (node->node_symbol,node->node_arity);

			ConvertSymbolToDandNLabel (&name,&codelab,new_select_sdef);
			
			if (update_node_id==NULL)
				GenBuild (&name,1,&codelab);
			else {
				GenFill (&name,1,&codelab,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
				--*asp_p;
			}
		} else {
			if (arg_node->node_kind!=NodeIdNode){
				int asize,bsize,aindex,bindex;
				StateP record_state_p;

				BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);
				
				record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
#if BOXED_RECORDS
				if (node->node_arity<SELECTOR_L ?
					arg->arg_state.state_type==SimpleState :
					arg->arg_state.state_type==TupleState && arg->arg_state.state_tuple_arguments[0].state_type==SimpleState
				){
					if (node->node_arity<SELECTOR_L)
						PushField (*record_state_p,fieldnr,0,asp_p,bsp_p,&asize,&bsize);
					else
						ReplaceRecordByField (*record_state_p,fieldnr,asp_p,bsp_p,&asize,&bsize);
				} else {
#endif		
				DetermineFieldSizeAndPosition (fieldnr,&asize,&bsize,&aindex,&bindex,record_state_p->state_record_arguments);

				if (node->node_arity<SELECTOR_L){
					int n;
					
					for (n=0; n<asize; ++n)
						GenPushA (aindex+asize-1);
					*asp_p+=asize;
					
					for (n=0; n<bsize; ++n)
						GenPushB (bindex+bsize-1);
					*bsp_p+=bsize;					
				} else {
					int record_a_size,record_b_size;
					
					DetermineSizeOfState (*record_state_p,&record_a_size,&record_b_size);
					ReplaceRecordOnTopOfStackByField (asp_p,bsp_p,aindex,bindex,asize,bsize,record_a_size,record_b_size);					
				}
#if BOXED_RECORDS
				}
#endif
			} else {
				int	a_size,b_size,apos,bpos,record_a_size,record_b_size,n;
				StateS tuple_state,tuple_state_arguments[2],*record_state_p;
				NodeId arg_node_id;
			
				arg_node_id=arg_node->node_node_id;

				record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;

				tuple_state.state_type=TupleState;
				tuple_state.state_arity=2;
				tuple_state.state_tuple_arguments=tuple_state_arguments;
				
				tuple_state_arguments[0]=record_state_p->state_record_arguments[fieldnr];

				CopyNodeIdArgument (arg->arg_state,arg_node_id,asp_p,bsp_p);

#if BOXED_RECORDS
				if (arg->arg_state.state_type==SimpleState){
					PushField (*record_state_p,fieldnr,0,asp_p,bsp_p,&a_size,&b_size);
					
					tuple_state_arguments[1]=arg->arg_state;
					
					CoerceArgumentOnTopOfStack (asp_p,bsp_p,tuple_state,node->node_state,1+a_size,b_size);
				} else {
#endif				
				DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&a_size,&b_size,&apos,&bpos,&record_a_size,&record_b_size,record_state_p);
			
				for (n=0; n<a_size; ++n)
					GenPushA (apos+a_size-1);
				*asp_p+=a_size;

				for (n=0; n<b_size; ++n)
					GenPushB (bpos+b_size-1);
				*bsp_p+=b_size;
				
				tuple_state_arguments[1]=*record_state_p;
				
				CoerceArgumentOnTopOfStack (asp_p,bsp_p,tuple_state,node->node_state,record_a_size+a_size,record_b_size+b_size);
#if BOXED_RECORDS
				}
#endif
				decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
			}
		}
		return;
	}

	if (arg_node->node_kind!=NodeIdNode){
		if (IsLazyState (node->node_state)){

#ifdef OPTIMIZE_SELECTION_OF_SELECTION_OF_UNBOXED_RECORD_FIELD
			if (arg_node->node_kind==SelectorNode && arg_node->node_arity==1 && update_node_id==NULL){
				SymbDef record_sdef;
				StateP record_state_p;
				
				record_sdef=seldef->sdef_type->type_lhs->ft_symbol->symb_def;
				record_state_p=&record_sdef->sdef_record_state;
				if (record_state_p->state_type==RecordState && record_state_p->state_arity==1 && !record_sdef->sdef_boxed_record){
					int asize,bsize;
					struct state *record_field_state_p;

					/* record with one field may be unboxed in another record */
					ArgComment (arg);

					record_field_state_p = FillOrReduceFieldSelection_of_selection_in_single_field_unboxable_record (arg_node,arg_node->node_symbol->symb_def,asp_p,bsp_p,code_gen_node_ids_p);
					if (record_field_state_p!=NULL){
						DetermineSizeOfState (*record_field_state_p, &asize, &bsize);						
						if (record_field_state_p->state_type==RecordState){
							CoerceArgumentOnTopOfStack (asp_p,bsp_p,*record_state_p,*record_field_state_p,asize,bsize);
							DetermineSizeOfState (record_state_p->state_record_arguments[fieldnr], &asize, &bsize);
							CoerceArgumentOnTopOfStack (asp_p,bsp_p,node->node_state,record_state_p->state_record_arguments[fieldnr],asize,bsize);
							
							if (node->node_state.state_kind==OnA)
								node->node_state.state_kind=StrictOnA;

							return;
						}
						CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,*record_field_state_p,asize,bsize);
					} else {
						DetermineSizeOfState (arg_node->node_state, &asize, &bsize);
						CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,arg_node->node_state,asize,bsize);
					}
				} else
					BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);
			} else
#endif
			BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);

#ifdef DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS
			if (!ResultIsNotInRootNormalForm (arg_node->node_state) && update_node_id==NULL){
				int asize,bsize,apos,bpos,tot_asize,tot_bsize;
				StateP record_state_p,field_state_p;
			
				record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
				if (record_state_p->state_type!=RecordState)
					error_in_function ("FillOrReduceFieldSelection");

				DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&asize,&bsize,&apos,&bpos,&tot_asize,&tot_bsize,record_state_p);
				
				GenPushRArgB (0,tot_asize,tot_bsize,bpos+1,bsize);
				GenReplRArgA (tot_asize,tot_bsize,apos+1,asize);
				
				*asp_p -= 1-asize;
				*bsp_p += bsize;
				
				field_state_p=&record_state_p->state_record_arguments [fieldnr];
				CoerceArgumentOnTopOfStack (asp_p,bsp_p,node->node_state,*field_state_p,asize,bsize);
				
				if (node->node_state.state_kind==OnA && !ResultIsNotInRootNormalForm (*field_state_p))
					node->node_state.state_kind=StrictOnA;
			} else
#endif

			BuildOrFillLazyFieldSelector (seldef,node->node_state.state_kind,asp_p,update_node_id);
		} else {
			int asize,bsize,apos,bpos,tot_asize,tot_bsize;
			StateP record_state_p;
#if 1
			record_state_p=&seldef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
#else
			record_state_p=&arg->arg_state;
#endif
			Build (arg_node,asp_p,bsp_p,code_gen_node_ids_p);

			DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&asize,&bsize,&apos,&bpos,&tot_asize,&tot_bsize,record_state_p);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,*record_state_p,arg_node->node_state,tot_asize,tot_bsize);

			ReplaceRecordOnTopOfStackByField (asp_p,bsp_p,apos,bpos,asize,bsize,tot_asize,tot_bsize);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,node->node_state,record_state_p->state_record_arguments[fieldnr],asize,bsize);
		}
	} else {
		StateS recstate;
		NodeId arg_node_id;
		
		arg_node_id=arg_node->node_node_id;
		
		recstate=arg_node_id->nid_state;

		if (recstate.state_type==RecordState){
			int a_size,b_size,apos,bpos,record_a_index,record_b_index;
			StateP field_state_p;

			DetermineFieldSizeAndPosition (fieldnr,&a_size,&b_size,&apos,&bpos,recstate.state_record_arguments);
			
			if (arg_node_id->nid_refcount<0 && arg_node_id->nid_node!=NULL){
				ArgS *args;
				
				args=arg_node_id->nid_node->node_arguments;
				record_a_index=get_a_index_of_unpacked_lhs_node (args);				
				record_b_index=get_b_index_of_unpacked_lhs_node (args);
			} else {
				record_a_index=arg_node_id->nid_a_index;
				record_b_index=arg_node_id->nid_b_index;
			}

			field_state_p=&recstate.state_record_arguments[fieldnr];

			if (update_node_id==NULL){
				CopyArgument (node->node_state,*field_state_p,record_a_index-apos,record_b_index-bpos,asp_p,bsp_p,a_size,b_size,True);
			} else {
				int locasp;
				
				locasp = *asp_p;

				GenPushA (*asp_p-update_node_id->nid_a_index);
				*asp_p+=1;
				
				CopyArgument (node->node_state,*field_state_p,record_a_index-apos,record_b_index-bpos,asp_p,bsp_p,a_size,b_size,False);

				GenPopA (*asp_p-locasp);
				*asp_p=locasp;
			}

#ifdef DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS			
			if (node->node_state.state_type==SimpleState && node->node_state.state_kind==OnA && !ResultIsNotInRootNormalForm (*field_state_p))
				node->node_state.state_kind=StrictOnA;
#endif
			decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
		} else if (IsLazyState (node->node_state)){
#ifdef DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS
			if ((recstate.state_kind==StrictOnA || recstate.state_kind==StrictRedirection) && update_node_id==NULL){
				int asize,bsize,apos,bpos,tot_asize,tot_bsize,recindex;
				SymbDef record_sdef;
				StateP record_state_p,field_state_p;
			
				recindex = arg_node_id->nid_a_index;
				record_sdef=seldef->sdef_type->type_lhs->ft_symbol->symb_def;
				record_state_p=&record_sdef->sdef_record_state;

				if (record_state_p->state_type!=RecordState)
					error_in_function ("FillOrReduceFieldSelection");

				DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&asize,&bsize,&apos,&bpos,&tot_asize,&tot_bsize,record_state_p);
# if BOXED_RECORDS
 				if (record_sdef->sdef_boxed_record && (arg_node_id->nid_mark2 & (NID_RECORD_USED_BY_UPDATE | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_RECORD_USED_BY_UPDATE
					&&
					(((arg_node_id->nid_state.state_mark & STATE_UNIQUE_MASK)!=0
/*
					&&
					(arg_node_id->nid_refcount==-2 || ((arg_node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 && arg_node_id->nid_number== -1))
*/
					)
					||
					((arg_node_id->nid_mark2 & NID_SELECTION_NODE_ID)==0
					  ?	(arg_node_id->nid_refcount>=0 && arg_node_id->nid_node->node_kind==NodeIdNode && 
						 (arg_node_id->nid_node->node_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID)
					  :	(arg_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID
					  )
					)
				){
					GenPushRArgU (*asp_p-recindex,tot_asize,tot_bsize,apos+1,asize,bpos+1,bsize);
				} else
# endif
				{
					GenPushRArgB (*asp_p-recindex,tot_asize,tot_bsize,bpos+1,bsize);
					GenPushRArgA (*asp_p-recindex,tot_asize,tot_bsize,apos+1,asize);
				}
				
				*asp_p+=asize;
				*bsp_p+=bsize;
				
				field_state_p=&record_state_p->state_record_arguments [fieldnr];
				CoerceArgumentOnTopOfStack (asp_p,bsp_p,node->node_state,*field_state_p,asize,bsize);
				
				if (node->node_state.state_kind==OnA && !ResultIsNotInRootNormalForm (*field_state_p))
					node->node_state.state_kind=StrictOnA;

				decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
			} else
#endif
			{
				BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);
				
				BuildOrFillLazyFieldSelector (seldef,node->node_state.state_kind,asp_p,update_node_id);
			}
		} else {
			int a_size,b_size,apos, bpos, tot_asize, tot_bsize,recindex;
			SymbDef record_sdef;

			/* the selector is strict but the record is not */
		
			recindex = arg_node_id->nid_a_index;

			if (ResultIsNotInRootNormalForm (recstate)){
				GenJsrEval (*asp_p-recindex);
				ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
				
				recstate.state_kind = StrictOnA;
			}

			record_sdef=seldef->sdef_type->type_lhs->ft_symbol->symb_def;
			DetermineFieldSizeAndPositionAndRecordSize (fieldnr,&a_size,&b_size,&apos, &bpos,&tot_asize,&tot_bsize,&record_sdef->sdef_record_state);
# if BOXED_RECORDS
			if (record_sdef->sdef_boxed_record && (arg_node_id->nid_mark2 & (NID_RECORD_USED_BY_UPDATE | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_RECORD_USED_BY_UPDATE
				&&
				((	(arg_node_id->nid_state.state_mark & STATE_UNIQUE_MASK)!=0
/*					&&
					(arg_node_id->nid_refcount==-2 || ((arg_node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 && arg_node_id->nid_number== -1))
*/
				)
				||
				((arg_node_id->nid_mark2 & NID_SELECTION_NODE_ID)==0
				  ?	(arg_node_id->nid_refcount>=0 && arg_node_id->nid_node->node_kind==NodeIdNode && 
						 (arg_node_id->nid_node->node_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID)
					  :	(arg_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID
					  )
				)
			){
				GenPushRArgU (*asp_p-recindex,tot_asize,tot_bsize,apos+1,a_size,bpos+1,b_size);
			} else
# endif
			{
				GenPushRArgB (*asp_p-recindex, tot_asize, tot_bsize, bpos+1,b_size);
				GenPushRArgA (*asp_p-recindex, tot_asize, tot_bsize, apos+1,a_size);
			}
			
			*asp_p+=a_size;
			*bsp_p+=b_size;
			
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,node->node_state,record_sdef->sdef_record_state.state_record_arguments[fieldnr],a_size,b_size);
			decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
		}
	}
}

void FillSelectSymbol (StateKind result_state_kind,int arity,int argnr,Args arg,int *asp_p,int *bsp_p,
								NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	LabDef sellab, nsellab;

	BuildLazyTupleSelectorLabel (&nsellab,arity,argnr);

	BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);

	sellab			= nsellab;
	sellab.lab_pref	= d_pref;

	/* we use a negative arity to indicate lazy selectors */
	if (update_node_id==NULL)
		GenBuild (&sellab,-1,&nsellab);
	else {
		GenFill (&sellab,-1,&nsellab,*asp_p-update_node_id->nid_a_index,result_state_kind==SemiStrict ? PartialFill : NormalFill);
		*asp_p-=1;
	}
}

#if defined (THUNK_LIFT_SELECTORS)
void FillSelectAndRemoveSymbol (StateKind result_state_kind,int arity,int argnr,Args arg,int *asp_p,int *bsp_p,
								NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	LabDef sellab, nsellab;

	BuildLazyTupleSelectorAndRemoveLabel (&nsellab,arity,argnr);

	BuildArg (arg,asp_p,bsp_p,code_gen_node_ids_p);

	sellab			= nsellab;
	sellab.lab_pref	= d_pref;

	/* we use a negative arity to indicate lazy selectors */
	if (update_node_id==NULL)
		GenBuild (&sellab,-1,&nsellab);
	else {
		GenFill (&sellab,-1,&nsellab,*asp_p-update_node_id->nid_a_index,result_state_kind==SemiStrict ? PartialFill : NormalFill);
		*asp_p-=1;
	}
}
#endif

#if OPTIMIZE_LAZY_TUPLE_RECURSION
extern int lazy_tuple_recursion;
extern void update_tuple_element_node (StateP state_p,int tuple_element_a_index,int *asp_p,int *bsp_p);
#endif

static void FillOrReduceSelectSymbol (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	Args arg;
	int argnr;

	arg = node->node_arguments;
	argnr = node->node_arity;
	
	if (arg->arg_node->node_kind!=NodeIdNode){
		if (IsLazyState (node->node_state))
			FillSelectSymbol (node->node_state.state_kind,node->node_symbol->symb_arity,argnr,arg,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
		else {
			Node argnode;
			int asize,bsize;
			
			argnode = arg->arg_node;

			DetermineSizeOfState (argnode->node_state, &asize, &bsize);
			Build (argnode,asp_p,bsp_p,code_gen_node_ids_p);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,argnode->node_state, asize, bsize);
		}
	} else {
		StateS tupstate;
		NodeId arg_node_id;

		/* the tuple is shared */
		
		arg_node_id=arg->arg_node->node_node_id;
#if OPTIMIZE_LAZY_TUPLE_RECURSION
		if ((arg_node_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY) && update_node_id==NULL){
			int select_node_index;

			select_node_index=arg_node_id->nid_a_index-argnr;

			GenPushA (*asp_p-select_node_index);
			++*asp_p;

			return;
		}
#endif
		
		tupstate = arg_node_id->nid_state;
		
		if (IsSimpleState (tupstate)){
	 		if (IsLazyState (node->node_state)){
	 			/* added 10-8-1999 */
	 			if (!IsLazyStateKind (tupstate.state_kind)){
					GenPushArg (*asp_p-arg_node_id->nid_a_index,node->node_symbol->symb_arity,argnr);
					*asp_p+=1;

					decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);	 				
	 			} else
	 			/* */
#if defined (THUNK_LIFT_SELECTORS)
				if (arg_node_id->nid_refcount>0 && (arg_node_id->nid_node_def->def_mark & NODE_DEF_SELECT_AND_REMOVE_MASK)!=0)
					FillSelectAndRemoveSymbol (node->node_state.state_kind,node->node_symbol->symb_arity,argnr,arg,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
				else
#endif
				FillSelectSymbol (node->node_state.state_kind,node->node_symbol->symb_arity,argnr,arg,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			} else {
				int arity,tupindex;
				StateS selectstate;

				/* the selector is strict but the tuple is not */

				arity = arg->arg_state.state_arity;
				tupindex = arg_node_id->nid_a_index;
				selectstate	= arg->arg_state.state_tuple_arguments[argnr-1];
				
				if (ResultIsNotInRootNormalForm (tupstate)){
					GenJsrEval (*asp_p-tupindex);
					ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
					tupstate.state_kind = StrictOnA;
				}

#if defined (THUNK_LIFT_SELECTORS)
				if (node->node_number!=0){
					char bits[MaxNodeArity+2];
					int n;
					
					GenPushArgsU (*asp_p-tupindex,arity,argnr);
					if (argnr>1)
						GenPopA (argnr-1);

					*asp_p+=1;

					for (n=0; n<=arity; ++n)
						bits[n]='0';
					bits[arity+1]='\0';

					bits[argnr]='1';

					GenBuildh (&nil_lab,0);
					
					if (arity<=2)
						GenFill1 (&tuple_lab,arity,*asp_p+1-tupindex,bits);
					else
						GenFill2 (&tuple_lab,arity,*asp_p+1-tupindex,bits);
					
					GenKeep (*asp_p-tupindex,0);
				} else {
					GenPushArg (*asp_p-tupindex,arity,argnr);
					*asp_p+=1;
				}
#else

				GenPushArg (*asp_p-tupindex,arity,argnr);
				*asp_p+=1;
#endif
				if (!ResultIsNotInRootNormalForm (selectstate))
					GenJsrEval (0);

				CoerceArgumentOnTopOfStack (asp_p,bsp_p,selectstate,tupstate,1,0);

#if OPTIMIZE_LAZY_TUPLE_RECURSION
				if (update_node_id!=NULL)
					update_tuple_element_node (&selectstate,update_node_id->nid_a_index,asp_p,bsp_p);
#endif				
				decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
			}
		} else {
			int a_size,b_size,i,argasize,argbsize,a_index,b_index;
			StateS selectstate;
			
			a_size=0;
			b_size=0;
			
			for (i=0; i<argnr-1; i++)
				AddSizeOfState (tupstate.state_tuple_arguments[i],&a_size,&b_size);
			
			if (IsSimpleState (arg->arg_state))
				selectstate = arg->arg_state;
			else
				selectstate = arg->arg_state.state_tuple_arguments[i];
			
			DetermineSizeOfState (tupstate.state_tuple_arguments[i],&argasize, &argbsize);

			a_index=arg_node_id->nid_a_index;
			b_index=arg_node_id->nid_b_index;

			if (arg_node_id->nid_refcount<0 && arg_node_id->nid_node!=NULL){
				ArgP args;
				
				args=arg_node_id->nid_node->node_arguments;				
				a_index=get_a_index_of_unpacked_lhs_node (args);			
				b_index=get_b_index_of_unpacked_lhs_node (args);
			}
			
			if (update_node_id==NULL)
				CopyArgument (selectstate,tupstate.state_tuple_arguments[i],
							a_index - a_size,b_index - b_size,asp_p,bsp_p, argasize, argbsize, True);
			else {
				int locasp;
				
				locasp = *asp_p;
			
				GenPushA (*asp_p-update_node_id->nid_a_index);
				++*asp_p;

				CopyArgument (selectstate,tupstate.state_tuple_arguments[i],
							a_index - a_size,b_index - b_size,asp_p,bsp_p, argasize, argbsize, False);

				GenPopA (*asp_p-locasp);
				*asp_p=locasp;
			}

			decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
		}
	}
}

void DetermineArrayElemDescr (StateS elemstate,Label lab)
{
	if (elemstate.state_type==SimpleState)
		*lab = BasicDescriptors [elemstate.state_object];
	else if (elemstate.state_type==RecordState){
		ConvertSymbolToRLabel (lab,elemstate.state_record_symbol);
	} else
		*lab = BasicDescriptors [UnknownObj];
}

#define UNUSED_NODE_ID_INDEX 30000

#if 0
#include "dbprint.h"
#endif

void cleanup_stack
	(int *asp_p,int *bsp_p,int a_size,int b_size,NodeIdListElementS **a_node_ids_l,NodeIdListElementS **b_node_ids_l,
	 NodeIdListElementS **free_node_ids_l,MovedNodeIdP *moved_node_ids_l,int compact_stack_ok)
{
	NodeIdListElementP p_node_ids;
	int asp,bsp;
	int n_a_elements_popped;

	if (DoDebug){
		PrintComment ();
		FPrintF (OutFile,compact_stack_ok ? "Remove unused stack elements" : "Remove unused stack elements without moving");
	}
	
	asp=*asp_p;
	bsp=*bsp_p;
	
	n_a_elements_popped=0;

#if 0
	printf ("cleanup_stack a_node_ids ");
	for_l (p_node_ids,*a_node_ids_l,nidl_next){
		DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
		printf (" ");
	}
	printf ("\n");
#endif

#if 0
	printf ("cleanup_stack b_node_ids ");
	for_l (p_node_ids,*b_node_ids_l,nidl_next){
		DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
		printf (" ");
	}
	printf ("\n");
#endif
	
	p_node_ids=*a_node_ids_l;
	while (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index==UNUSED_NODE_ID_INDEX){
#if 0
		printf ("cleanup_stack00 ");
		printf ("%s ",CurrentAltLabel.lab_symbol->sdef_ident->ident_name);
		DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
		printf ("\n");
#endif
		p_node_ids=p_node_ids->nidl_next;
	}
	
	if (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index==asp && (unused_node_id (p_node_ids->nidl_node_id))){
		int n_a_elements,n_b_elements;
		
		n_a_elements=0;
		n_b_elements=0;
		
		do {
#if 0
			printf ("cleanup_stack01 ");
			printf ("%s ",CurrentAltLabel.lab_symbol->sdef_ident->ident_name);
			DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
			printf ("\n");
#endif

			AddSizeOfState (p_node_ids->nidl_node_id->nid_state,&n_a_elements,&n_b_elements);
			/* free p_node_ids */

			p_node_ids=p_node_ids->nidl_next;
			while (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index==UNUSED_NODE_ID_INDEX){
#if 0
				printf ("cleanup_stack02 ");
				printf ("%s ",CurrentAltLabel.lab_symbol->sdef_ident->ident_name);
				DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
				printf ("\n");
#endif
				p_node_ids=p_node_ids->nidl_next;
			}

		} while (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index==asp-n_a_elements && (unused_node_id (p_node_ids->nidl_node_id)));

		*a_node_ids_l=p_node_ids;
	
		n_a_elements_popped=n_a_elements;
	}

	p_node_ids=*b_node_ids_l;
	if (p_node_ids!=NULL && (unused_node_id (p_node_ids->nidl_node_id)) && p_node_ids->nidl_node_id->nid_b_index==bsp){
		int n_a_elements,n_b_elements;
		
		n_a_elements=0;
		n_b_elements=0;
		
		do {
			AddSizeOfState (p_node_ids->nidl_node_id->nid_state,&n_a_elements,&n_b_elements);
			/* free p_node_ids */
			p_node_ids=p_node_ids->nidl_next;
		} while (p_node_ids!=NULL && (unused_node_id (p_node_ids->nidl_node_id)) && p_node_ids->nidl_node_id->nid_b_index==bsp-n_b_elements);

		*b_node_ids_l=p_node_ids;
				
		if (n_b_elements!=0){
			int i;

			for (i=b_size-1; i>=0; --i)
#if UPDATE_POP
				if (i==0)
					GenUpdatePopB (0,n_b_elements);
				else
#endif
				GenUpdateB (i,i+n_b_elements);
		
#if UPDATE_POP
			if (b_size==0)
#endif
				GenPopB (n_b_elements);

			*bsp_p-=n_b_elements;
		}
	}

	if (compact_stack_ok){
		NodeIdListElementP free_node_id,keep_node_ids;
		int node_id_a_size,node_id_b_size;
		int free_size,used_size,move_free_size,move_used_size;
		
		node_id_a_size=0;
		node_id_b_size=0;

		asp=*asp_p-n_a_elements_popped;
		
		for_l (free_node_id,*free_node_ids_l,nidl_next){
			struct node_id *node_id;
	
			node_id=free_node_id->nidl_node_id;

			if (node_id->nid_a_index < asp)
				AddSizeOfState (node_id->nid_state,&node_id_a_size,&node_id_b_size);
		}
		
		free_size=0;
		used_size=0;

		move_free_size=0;
		move_used_size=0;
		keep_node_ids=NULL;


		p_node_ids=*a_node_ids_l;
		
		while (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index==UNUSED_NODE_ID_INDEX){
#if 0
			printf ("cleanup_stack03 ");
			printf ("%s ",CurrentAltLabel.lab_symbol->sdef_ident->ident_name);
			DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
			printf ("\n");
#endif
			p_node_ids=p_node_ids->nidl_next;
		}

#if 0
		printf ("cleanup_stack1 ");
		printf ("%s\n",CurrentAltLabel.lab_symbol->sdef_ident->ident_name);

		if (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index!=asp){
			printf ("asp=%d nid_a_index=%d ",asp,p_node_ids->nidl_node_id->nid_a_index);
			DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
			printf ("\n");			
		}
#endif

		while (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index==asp){
			int element_a_size,element_b_size;
			struct node_id *node_id;
			
			node_id=p_node_ids->nidl_node_id;
			DetermineSizeOfState (node_id->nid_state,&element_a_size,&element_b_size);
			
#if 0
			DPrintNodeId (node_id,StdOut);
			printf ("\n");
#endif
			
			if (unused_node_id (node_id)){
				free_size+=element_a_size;
			} else {
				if (free_size+used_size > node_id_a_size+node_id_a_size)
					break;
			
				used_size+=element_a_size;
			}
			
			asp-=element_a_size;

			p_node_ids=p_node_ids->nidl_next;
			while (p_node_ids!=NULL && p_node_ids->nidl_node_id->nid_a_index==UNUSED_NODE_ID_INDEX){
#if 0
				printf ("cleanup_stack11 ");
				printf ("%s ",CurrentAltLabel.lab_symbol->sdef_ident->ident_name);
				DPrintNodeId (p_node_ids->nidl_node_id,StdOut);
				printf ("\n");
#endif
				p_node_ids=p_node_ids->nidl_next;
			}
			
			if (free_size>=used_size){
				move_free_size=free_size;
				move_used_size=used_size;
				keep_node_ids=p_node_ids;
			}
		}
		
		if (move_free_size!=0){
			NodeIdListElementP reversed_node_ids;
			int move_a_offset;
			int source_asp,dest_asp;

			move_a_offset=move_free_size;
			
			source_asp=*asp_p-n_a_elements_popped-(move_free_size+move_used_size);
			dest_asp=source_asp;
			
			reversed_node_ids=NULL;
			p_node_ids=*a_node_ids_l;
			
			while (p_node_ids!=keep_node_ids){
				NodeIdListElementP next_p_node_ids;
				
				next_p_node_ids=p_node_ids->nidl_next;
				p_node_ids->nidl_next=reversed_node_ids;
				reversed_node_ids=p_node_ids;
				p_node_ids=next_p_node_ids;
			}
			
			while (reversed_node_ids!=NULL){
				NodeIdListElementP next_reversed_node_ids;
				int element_a_size,element_b_size;
				MovedNodeIdP new_moved_node_id;
				struct node_id *node_id;
				
				node_id=reversed_node_ids->nidl_node_id;

				if (node_id->nid_a_index!=UNUSED_NODE_ID_INDEX){
					DetermineSizeOfState (node_id->nid_state,&element_a_size,&element_b_size);

					new_moved_node_id=CompAllocType (MovedNodeIdS);
					new_moved_node_id->mnid_node_id=node_id;
					new_moved_node_id->mnid_a_stack_offset=node_id->nid_a_index;
					
					new_moved_node_id->mnid_next=*moved_node_ids_l;
					*moved_node_ids_l=new_moved_node_id;

#if 0
					printf ("cleanup_stack2 ");
					DPrintNodeId (node_id,StdOut);
					printf ("\n");
#endif
						
					if (unused_node_id (node_id)){
						source_asp+=element_a_size;
						
						node_id->nid_a_index_=UNUSED_NODE_ID_INDEX;
					} else {
						int n;
						
						for (n=element_a_size; n!=0; --n){
							++source_asp;
							++dest_asp;
							GenUpdateA (*asp_p+a_size-source_asp,*asp_p+a_size-dest_asp);
						}
						
						node_id->nid_a_index_=dest_asp;
					}
				}

				next_reversed_node_ids=reversed_node_ids->nidl_next;
				reversed_node_ids->nidl_next=p_node_ids;
				p_node_ids=reversed_node_ids;
				reversed_node_ids=next_reversed_node_ids;
			}
			
			*a_node_ids_l=p_node_ids;
/*			*a_node_ids_l=keep_node_ids; */

			n_a_elements_popped+=move_a_offset;
		}
	}

	if (n_a_elements_popped!=0){
		int i;
			
		for (i=a_size-1; i>=0; --i)
#if UPDATE_POP
			if (i==0)
				GenUpdatePopA (0,n_a_elements_popped);
			else
#endif
			GenUpdateA (i,i+n_a_elements_popped);
	
#if UPDATE_POP
		if (a_size==0)
#endif
			GenPopA (n_a_elements_popped);

		*asp_p-=n_a_elements_popped;
	}

	{
		NodeIdListElementP free_node_id;
		int nil_on_stack;
		
		nil_on_stack=0;
		asp=*asp_p;
		
		for_l (free_node_id,*free_node_ids_l,nidl_next){
			struct node_id *node_id;
			
			node_id=free_node_id->nidl_node_id;

#if 0
			printf ("cleanup_stack3 ");
			DPrintNodeId (node_id,StdOut);
			printf ("\n");
#endif

			if (node_id->nid_a_index < asp){
				int node_id_a_size,node_id_b_size,a_index;

				DetermineSizeOfState (node_id->nid_state,&node_id_a_size,&node_id_b_size);

				if (node_id_a_size>0){
					a_index=asp+a_size-node_id->nid_a_index;
					
					NodeIdComment (node_id);
						
					while (node_id_a_size>0){
						if (!nil_on_stack){
							GenBuildh (&nil_lab,0);
							nil_on_stack=1;
						}
						
						GenUpdateA (0,1+a_index);

						++a_index;
						--node_id_a_size;
					}
				}
			}
		}
		*free_node_ids_l=free_node_id;
		
		if (nil_on_stack)
			GenPopA (1);		
	}
}

static void SubSizeOfStates (int arity,States states,int *a_offset_p,int *b_offset_p)
{
	for (; arity; arity--)
		SubSizeOfState (states [arity-1],a_offset_p,b_offset_p);
}

void SubSizeOfState (StateS state,int *a_offset_p,int *b_offset_p)
{
	if (IsSimpleState (state)){
		if (state.state_kind==OnB)
			*b_offset_p -= ObjectSizes [state.state_object];
		else if (state.state_kind != Undefined)
			*a_offset_p -= 1;
	} else {
		switch (state.state_type){
			case RecordState:
				SubSizeOfStates (state.state_arity,state.state_record_arguments,a_offset_p,b_offset_p);
				break;
			case TupleState:
				SubSizeOfStates (state.state_arity,state.state_tuple_arguments,a_offset_p,b_offset_p);
				break;
			case ArrayState:
				*a_offset_p -= 1;
				break;
		}
	}
}

static void SubSizeOfArguments (ArgS *args,int *a_offset_p,int *b_offset_p)
{
	ArgS *arg;
	
	for_l (arg,args,arg_next)
		SubSizeOfState (arg->arg_state,a_offset_p,b_offset_p);
}

static void AddSizeOfArguments (ArgS *args,int *a_offset_p,int *b_offset_p)
{
	ArgS *arg;
	
	for_l (arg,args,arg_next)
		AddSizeOfState (arg->arg_state,a_offset_p,b_offset_p);
}

void DetermineSizeOfArguments (ArgS *args,int *a_offset_p,int *b_offset_p)
{
	ArgS *arg;
	
	*a_offset_p=0;
	*b_offset_p=0;
	
	for_l (arg,args,arg_next)
		AddSizeOfState (arg->arg_state,a_offset_p,b_offset_p);
}

static Bool BuildNonParArgs (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);
static void BuildParArgs (ArgS* args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);
static void ReorderParallelAndNonParallelArgsWithResultNode (Args args,int *asize_p,int *bsize_p);

#define BETWEEN(l,h,v) ((unsigned)((v)-(l)) <= (unsigned)((h)-(l)))

static int ChangeArgumentNodeStatesIfStricter (Args offered_args,States demanded_states)
{
	StateP demanded_state_p;
	ArgP arg_p;
	
	for_la (arg_p,demanded_state_p,offered_args,demanded_states,arg_next){
		Node arg_node;
		int node_kind;
		
		arg_node=arg_p->arg_node;
		
		node_kind=arg_node->node_kind;
		if (node_kind!=NodeIdNode){
			if (node_kind==NormalNode && (BETWEEN (int_denot,real_denot,arg_node->node_symbol->symb_kind) || arg_node->node_symbol->symb_kind==string_denot))
				;
			else
				if (!FirstStateIsStricter (arg_node->node_state,*demanded_state_p))
					return 1;		
		} else
			if (!FirstStateIsStricter (arg_node->node_node_id->nid_state,*demanded_state_p))
				return 1;
	}

	for_la (arg_p,demanded_state_p,offered_args,demanded_states,arg_next){
		Node arg_node;
		
		arg_node=arg_p->arg_node;
		if (arg_node->node_kind==NormalNode &&
			(BETWEEN (int_denot,real_denot,arg_node->node_symbol->symb_kind) || arg_node->node_symbol->symb_kind==string_denot)
		){
			arg_node->node_state=*demanded_state_p;
		}

		arg_p->arg_state=*demanded_state_p;
	}

	return 0;
}

void BuildArgsWithNewResultNode (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p,int *a_size_p,int *b_size_p)
{
	BuildNonParArgs (args,asp_p,bsp_p,code_gen_node_ids_p);
	NewEmptyNode (asp_p,-1);
	BuildParArgs (args,asp_p,bsp_p,code_gen_node_ids_p);
	ReorderParallelAndNonParallelArgsWithResultNode (args,a_size_p,b_size_p);
}

void BuildArgsWithResultNodeOnStack (Args args,NodeIdP free_unique_node_id,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p,int *a_size_p,int *b_size_p)
{
	BuildNonParArgs (args,asp_p,bsp_p,code_gen_node_ids_p);
	GenPushA (*asp_p-free_unique_node_id->nid_a_index);
	*asp_p+=1;
	decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);
	BuildParArgs (args,asp_p,bsp_p,code_gen_node_ids_p);
	ReorderParallelAndNonParallelArgsWithResultNode (args,a_size_p,b_size_p);
}

#if OPTIMIZE_LAZY_TUPLE_RECURSION
extern LabDef d_indirection_lab,n_indirection_lab;
#endif

static void FillSymbol (Node node,SymbDef sdef,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	LabDef name;
	int symbarity;
	
	symbarity = sdef->sdef_arity;

	if (symbarity==node->node_arity){
		switch (sdef->sdef_kind){
			case IMPRULE:
			case DEFRULE:
			case SYSRULE:
				if (IsLazyState (node->node_state)){
					LabDef codelab;

					ConvertSymbolToDandNLabel (&name,&codelab,sdef);

					if (sdef->sdef_kind==IMPRULE && (sdef->sdef_rule->rule_mark & RULE_UNBOXED_LAZY_CALL)){
						int a_size,b_size;

#ifndef OPTIMIZE_LAZY_TUPLE_RECURSION
						if (update_node_id!=NULL)
							error_in_function ("FillSymbol");
#endif
						
						DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
						
						BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
						
#if OPTIMIZE_LAZY_TUPLE_RECURSION
						if (update_node_id!=NULL){
							if (a_size+b_size<=2){
								if (b_size!=0){
									GenFillU (&name,a_size,b_size,&codelab,*asp_p-update_node_id->nid_a_index);
									*bsp_p -= b_size;									
								} else
									GenFill (&name,a_size,&codelab,*asp_p-update_node_id->nid_a_index,NormalFill);
								*asp_p-=a_size;

								GenPushA (*asp_p-update_node_id->nid_a_index);
								*asp_p+=1;							
							} else {
								if (b_size!=0)
									GenBuildU (&name,a_size,b_size,&codelab);
								else
									GenBuild (&name,a_size,&codelab);
								*asp_p += 1-a_size;
								*bsp_p -= b_size;
							
								GenFill (&d_indirection_lab,-2,&n_indirection_lab,*asp_p-update_node_id->nid_a_index,NormalFill);
								--*asp_p;
							}
						} else
#endif
						{
						*asp_p += 1-a_size;
						*bsp_p -= b_size;

						if (b_size!=0)
							GenBuildU (&name,a_size,b_size,&codelab);
						else
							GenBuild (&name,a_size,&codelab);
						}
						return;
					}

					BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
					
					if (update_node_id==NULL){
						*asp_p += 1-symbarity;
						GenBuild (&name,symbarity,&codelab);
					} else {
#if OPTIMIZE_LAZY_TUPLE_RECURSION
						if ((update_node_id->nid_mark & ON_A_CYCLE_MASK)!=0 || symbarity<=2){
							GenFill (&name,symbarity,&codelab,*asp_p-update_node_id->nid_a_index,PartialFill);
							*asp_p-=symbarity;
						} else {
							GenBuild (&name,symbarity,&codelab);
							*asp_p+=1-symbarity;
							GenFill (&d_indirection_lab,-2,&n_indirection_lab,*asp_p-update_node_id->nid_a_index,NormalFill);
							--*asp_p;
						}
#else
						GenFill (&name,symbarity,&codelab,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
						*asp_p -= symbarity;
#endif
					}
				} else {
					int newnode,a_size,b_size;
					
					ConvertSymbolToLabel (&name,sdef);
				
					newnode=False;
					
					if (update_node_id==NULL && ExpectsResultNode (node->node_state)){
						BuildArgsWithNewResultNode (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p,&a_size,&b_size);

						*asp_p-=a_size;
						*bsp_p-=b_size;

						if (! (sdef->sdef_kind==SYSRULE
								&& sdef->sdef_ident->ident_instructions!=NULL
								&& *sdef->sdef_ident->ident_instructions!='\0'
								&& *sdef->sdef_ident->ident_instructions!='.'))
						{
							cleanup_stack (asp_p,bsp_p,a_size,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
											&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,
											code_gen_node_ids_p->doesnt_fail);
						}
						CallFunction (&name,sdef,True,node);

						AddSizeOfState (node->node_state,asp_p,bsp_p);

						return;
					}

					DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);

					if (update_node_id!=NULL && update_node_id->nid_a_index!=*asp_p){
						GenPushA (*asp_p-update_node_id->nid_a_index);
						*asp_p += SizeOfAStackElem;

						BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
						
						*asp_p-=a_size+1;
						*bsp_p-=b_size;

						if (! (sdef->sdef_kind==SYSRULE
								&& sdef->sdef_ident->ident_instructions!=NULL
								&& *sdef->sdef_ident->ident_instructions!='\0'
								&& *sdef->sdef_ident->ident_instructions!='.'))
						{
							cleanup_stack (asp_p,bsp_p,a_size+1,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
											&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,
											code_gen_node_ids_p->doesnt_fail);
						}

						CallFunction (&name,sdef,True,node);
											
						AddSizeOfState (node->node_state,asp_p,bsp_p);
						
						GenPopA (1);
						*asp_p-=1;
					} else {
						if (newnode)
							++a_size;						

						BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
						
						*asp_p-=a_size;
						*bsp_p-=b_size;

						if (! (sdef->sdef_kind==SYSRULE
								&& sdef->sdef_ident->ident_instructions!=NULL
								&& *sdef->sdef_ident->ident_instructions!='\0'
								&& *sdef->sdef_ident->ident_instructions!='.'))
						{
							cleanup_stack (asp_p,bsp_p,a_size,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
											&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,
											code_gen_node_ids_p->doesnt_fail);
						}

						CallFunction (&name,sdef,True,node);

						AddSizeOfState (node->node_state,asp_p,bsp_p);
					}
				}
				return;
			case CONSTRUCTOR:
				if (sdef->sdef_strict_constructor){
					int lazy_fill;

					ConvertSymbolToLabel (&name,sdef);

					lazy_fill=IsLazyState (node->node_state);
					
					if (lazy_fill)
						lazy_fill=ChangeArgumentNodeStatesIfStricter (node->node_arguments,sdef->sdef_constructor->cl_state_p);

					BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
					
					if (lazy_fill){
						LabDef reclab, contlab;

						ConvertSymbolToConstructorDandNLabel (&reclab,&contlab,sdef);
						
						if (update_node_id==NULL){
							*asp_p+=1-symbarity;
							GenBuild (&reclab,symbarity,&contlab);
						} else {
#if OPTIMIZE_LAZY_TUPLE_RECURSION
							if ((update_node_id->nid_mark & ON_A_CYCLE_MASK)!=0 || symbarity<=2){
								GenFill (&reclab,symbarity,&contlab,*asp_p-update_node_id->nid_a_index,ReleaseAndFill);
								*asp_p-=symbarity;
							} else {
								GenBuild (&reclab,symbarity,&contlab);
								*asp_p+=1-symbarity;
								GenFill (&d_indirection_lab,-2,&n_indirection_lab,*asp_p-update_node_id->nid_a_index,NormalFill);
								--*asp_p;
							}
#else
							GenFill (&reclab,symbarity,&contlab,*asp_p-update_node_id->nid_a_index,
									 node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill);
							*asp_p-=symbarity;
#endif
						}
					} else {
						int asize,bsize;
						LabDef record_label;

						DetermineSizeOfArguments (node->node_arguments,&asize,&bsize);
	
						ConvertSymbolToKLabel (&record_label,sdef);

						*asp_p-=asize;
						*bsp_p-=bsize;
						
						if (update_node_id==NULL){
							GenBuildhr (&record_label,asize,bsize);
							*asp_p+=1;
						} else {
							GenFillR (&record_label,asize,bsize,*asp_p+asize-update_node_id->nid_a_index,0,0,node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill,True);
						}
					}
				} else {
					ConvertSymbolToConstructorDLabel (&name,sdef);
					
					BuildLazyArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
									
					if (update_node_id==NULL){
						*asp_p+=1-node->node_arity;
						GenBuildh (&name,node->node_arity);
					} else {
						GenFillh (&name,node->node_arity,*asp_p-update_node_id->nid_a_index,
								 node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill);
						*asp_p-=node->node_arity;
					}
				}
				return;
			case RECORDTYPE:
				ConvertSymbolToLabel (&name,sdef);

				if (IsSimpleState (node->node_state)){
					LabDef record_label;
					int lazy_fill;
					
					lazy_fill=sdef->sdef_strict_constructor && IsLazyState (node->node_state);

					if (lazy_fill)
						lazy_fill=ChangeArgumentNodeStatesIfStricter (node->node_arguments,sdef->sdef_record_state.state_record_arguments);

					BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
					
					if (lazy_fill){
						LabDef contlab;

						ConvertSymbolToRecordDandNLabel (&record_label,&contlab,sdef);

						if (update_node_id==NULL){
							*asp_p+=1-symbarity;
							GenBuild (&record_label,symbarity,&contlab);
						} else {
#if OPTIMIZE_LAZY_TUPLE_RECURSION
							if ((update_node_id->nid_mark & ON_A_CYCLE_MASK)!=0 || symbarity<=2){
								GenFill (&record_label,symbarity,&contlab,*asp_p-update_node_id->nid_a_index,ReleaseAndFill);
								*asp_p-=symbarity;
							} else {
								GenBuild (&record_label,symbarity,&contlab);
								*asp_p+=1-symbarity;
								GenFill (&d_indirection_lab,-2,&n_indirection_lab,*asp_p-update_node_id->nid_a_index,NormalFill);
								--*asp_p;
							}
#else
							GenFill (&record_label,symbarity,&contlab,*asp_p-update_node_id->nid_a_index,
									 node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill);
							*asp_p-=symbarity;
#endif
						}
					} else {
						int asize,bsize;

						ConvertSymbolToRLabel (&record_label,sdef);
					
						DetermineSizeOfArguments (node->node_arguments,&asize,&bsize);

						*asp_p-=asize;
						*bsp_p-=bsize;

						if (update_node_id==NULL){
							*asp_p+=1;
							GenBuildhr (&record_label,asize,bsize);
						} else {
							GenFillR (&record_label,asize,bsize,*asp_p+asize-update_node_id->nid_a_index,0,0,node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill,True);
						}
					}
				} else
					BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
				return;
			default:
				if (update_node_id==NULL)
					NewEmptyNode (asp_p,-1);
				return;
		}
	} else {
		if (sdef->sdef_kind==CONSTRUCTOR)
			ConvertSymbolToConstructorDLabel (&name,sdef);
		else
			ConvertSymbolToDLabel (&name,sdef);

		/* Symbol has too few arguments */
		BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
				
		if (update_node_id==NULL){
			*asp_p+=1-node->node_arity;
			GenBuildPartialFunctionh (&name,node->node_arity);
		} else {
			GenFillh (&name,node->node_arity,*asp_p-update_node_id->nid_a_index,NormalFill);
			*asp_p-=node->node_arity;
		}
	}
}

#ifdef NEW_APPLY
int build_apply_arguments (ArgP node_args,int *a_size_p,int *b_size_p,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	if (node_args->arg_node->node_kind==NormalNode && node_args->arg_node->node_symbol->symb_kind==apply_symb){
		ArgP next_arg;
		
		next_arg=node_args->arg_next;
		AddSizeOfState (next_arg->arg_state,a_size_p,b_size_p);
		BuildArg (next_arg,asp_p,bsp_p,code_gen_node_ids_p);
		
		return 1+build_apply_arguments (node_args->arg_node->node_arguments,a_size_p,b_size_p,asp_p,bsp_p,code_gen_node_ids_p);
	} else {
		AddSizeOfArguments (node_args,a_size_p,b_size_p);
		BuildArgs (node_args,asp_p,bsp_p,code_gen_node_ids_p);	
		return 1;
	}
}

static void generate_code_for_apply_in_strict_context (NodeP node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	int a_size,b_size,n_apply_args;
	ArgP node_args;

	a_size=0;

	if (OptimizeInstanceCalls && node->node_symbol->symb_instance_apply==1 && ExpectsResultNode (node->node_state)){
		NewEmptyNode (asp_p,-1);
		a_size=1;
	}

	node_args=node->node_arguments;
	b_size=0;

	n_apply_args=build_apply_arguments (node_args,&a_size,&b_size,asp_p,bsp_p,code_gen_node_ids_p);
	
	*asp_p-=a_size;
	*bsp_p-=b_size;
	cleanup_stack (asp_p,bsp_p,a_size,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
					&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,
					code_gen_node_ids_p->doesnt_fail);

	if (node->node_symbol->symb_instance_apply==1){
		struct arg *arg_p;
		struct symbol_def *field_sdef;

		field_sdef = (struct symbol_def *)node->node_symbol->symb_next;

		arg_p=node_args;
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
						FPrintF (OutFile, "\n||\t%s",selector_node_p->node_symbol->symb_def->sdef_ident->ident_name);
					} else
						FPrintF (OutFile, "\n||\t(no dictionary) %s",field_sdef->sdef_ident->ident_name);
			} else if (DoDebug)
				FPrintF (OutFile, "\n||\t(no dictionary) %s",field_sdef->sdef_ident->ident_name);
		} else if (DoDebug)
			FPrintF (OutFile, "\n||\t(no dictionary) %s",field_sdef->sdef_ident->ident_name);

		if (field_sdef!=NULL && OptimizeInstanceCalls){
			struct state *member_states_of_field;
			int member_arity,member_called_with_root_node,a_size,b_size;

			member_states_of_field=field_sdef->sdef_member_states_of_field;
			member_arity=field_sdef->sdef_member_type_of_field->type_alt_lhs->type_node_arity;

			member_called_with_root_node = member_states_of_field[-1].state_type==SimpleState
											&& !(member_states_of_field[-1].state_kind==StrictRedirection || member_states_of_field[-1].state_kind==OnB);

			DetermineSizeOfStates (member_arity-1,&member_states_of_field[1],&a_size,&b_size);
			GenDStackLayoutOfStates (a_size+1+member_called_with_root_node,b_size,member_arity-1,&member_states_of_field[1]);

			GenJsrI (n_apply_args);

			AddSizeOfState (node->node_state,asp_p,bsp_p);

			DetermineSizeOfState (member_states_of_field[-1],&a_size,&b_size);
			GenOStackLayoutOfState (a_size,b_size,member_states_of_field[-1]);

			return;
		}
	}

	GenJsrAp (n_apply_args);

	AddSizeOfState (node->node_state,asp_p,bsp_p);
}

static void FillApply (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	LabDef name;
	
	if (IsLazyState (node->node_state)){
		LabDef codelab;

		ConvertSymbolToDandNLabel (&name,&codelab,ApplyDef);

		BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
		
		if (update_node_id==NULL){
			*asp_p += 1-2;
			GenBuild (&name,2,&codelab);
		} else {
			GenFill (&name,2,&codelab,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
			*asp_p -= 2;
		}
	} else {
		if (update_node_id!=NULL && update_node_id->nid_a_index!=*asp_p){
			int a_size,b_size;
			
			DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
			
			GenPushA (*asp_p-update_node_id->nid_a_index);
			*asp_p += SizeOfAStackElem;

			BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
			
			*asp_p-=a_size+1;
			*bsp_p-=b_size;

			cleanup_stack (asp_p,bsp_p,a_size+1,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
							&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,
							code_gen_node_ids_p->doesnt_fail);

			GenJsrAp (1);
			
			AddSizeOfState (node->node_state,asp_p,bsp_p);
			
			GenPopA (1);
			*asp_p-=1;
		} else
			generate_code_for_apply_in_strict_context (node,asp_p,bsp_p,code_gen_node_ids_p);
	}
}
#endif

void GenTypeError (void)
{
	GenDAStackLayout (0);
	GenJsr (&type_error_lab);
	GenOAStackLayout (0);

	TypeErrorFound = True;
}

static void decrement_reference_count_of_node_ids_in_graph (Node node,NodeIdListElementS **free_node_ids_l)
{
	if (node->node_kind!=NodeIdNode){
		struct arg *arg;
		
		for_l (arg,node->node_arguments,arg_next)
			decrement_reference_count_of_node_ids_in_graph (arg->arg_node,free_node_ids_l);
	} else
		decrement_reference_count_of_node_id (node->node_node_id,free_node_ids_l);
}

static void increment_reference_count_of_node_ids_in_graph (Node node)
{
	if (node->node_kind!=NodeIdNode){
		struct arg *arg;
		
		for_l (arg,node->node_arguments,arg_next)
			increment_reference_count_of_node_ids_in_graph (arg->arg_node);
	} else {
		struct node_id *node_id;
		int ref_count;
	
		node_id=node->node_node_id;
		ref_count=node_id->nid_refcount;
		
		if (ref_count>=0)
			node_id->nid_refcount=ref_count+1;
		else
			node_id->nid_refcount=ref_count-1;
	}
}

#ifdef FASTER_STRICT_IF

static void build_strict_then_or_else (Node then_or_else_node,Node else_node,int *asp_p,int *bsp_p,
									   CodeGenNodeIdsP code_gen_node_ids_p,StateS result_state)
{
	if (then_or_else_node->node_kind!=NodeIdNode){
		SavedNidStateP saved_node_id_states;
		struct code_gen_node_ids code_gen_node_ids;
		MovedNodeIdP moved_node_ids;
		int a_size,b_size;
	
		saved_node_id_states=NULL;
		moved_node_ids=NULL;

		code_gen_node_ids.free_node_ids=code_gen_node_ids_p->free_node_ids;
		code_gen_node_ids.saved_nid_state_l=&saved_node_id_states;
		code_gen_node_ids.doesnt_fail=False;
		code_gen_node_ids.moved_node_ids_l=&moved_node_ids;
		code_gen_node_ids.a_node_ids=code_gen_node_ids_p->a_node_ids;
		code_gen_node_ids.b_node_ids=code_gen_node_ids_p->b_node_ids;

		if (else_node!=NULL)
			decrement_reference_count_of_node_ids_in_graph (else_node,&code_gen_node_ids.free_node_ids);
			
		Build (then_or_else_node,asp_p,bsp_p,&code_gen_node_ids);

		if (else_node!=NULL)
			increment_reference_count_of_node_ids_in_graph (else_node);

		restore_saved_node_id_states (saved_node_id_states);

		DetermineSizeOfState (then_or_else_node->node_state,&a_size,&b_size);
		CoerceArgumentOnTopOfStack (asp_p,bsp_p,result_state,then_or_else_node->node_state,a_size,b_size);
	} else {
		NodeId nid;
		int a_size,b_size;

		nid=then_or_else_node->node_node_id;
#if BOXED_RECORDS
		nid->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
#endif
		DetermineSizeOfState (nid->nid_state,&a_size,&b_size);
		CopyArgument (result_state,nid->nid_state,nid->nid_a_index,nid->nid_b_index,asp_p,bsp_p,a_size,b_size,True);
	}
}

static void fill_strict_if_node (Node node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	StateS condition_result_state;
	LabDef else_label,endif_label;
	Args arguments,then_arg,else_arg;
	int else_asp,else_bsp;
	
	arguments = node->node_arguments;
	
	SetUnaryState (&condition_result_state,OnB,BoolObj);
	EvaluateCondition (arguments->arg_node,asp_p,bsp_p,code_gen_node_ids_p,condition_result_state);

	MakeLabel (&else_label,"else",NewLabelNr,no_pref);
	MakeLabel (&endif_label,"endif",NewLabelNr++,no_pref);

	GenJmpFalse (&else_label);

	then_arg=arguments->arg_next;
	else_arg=then_arg->arg_next;

	else_asp=*asp_p;
	else_bsp=*bsp_p;
	
	build_strict_then_or_else (then_arg->arg_node,else_arg->arg_node,asp_p,bsp_p,code_gen_node_ids_p,node->node_state);
	
	GenJmp (&endif_label);
	
	GenLabelDefinition (&else_label);

	build_strict_then_or_else (else_arg->arg_node,NULL,&else_asp,&else_bsp,code_gen_node_ids_p,node->node_state);
	
	if (else_asp!=*asp_p || else_bsp!=*bsp_p){
		int a_size,b_size;
		
		DetermineSizeOfState (node->node_state,&a_size,&b_size);
		
		if (else_asp>*asp_p){
			int difference,i;
			
			difference=else_asp - *asp_p;
			for (i=a_size-1; i>=0; --i)
#if UPDATE_POP
				if (i==0)
					GenUpdatePopA (0,difference);
				else
#endif
				GenUpdateA (i,i+difference);
			
#if UPDATE_POP
			if (a_size==0)
#endif
			GenPopA (difference);
		} else if (else_asp<*asp_p){
			int difference,i;
			
			difference=*asp_p - else_asp;
			
			if (difference>a_size){
				int n;
				
				GenBuildh (&nil_lab,0);

				n=(difference-a_size)-1;

				for (i=0; i<n; ++i)
					GenPushA (i);
				
				for (i=a_size-1; i>=0; --i)
					GenPushA (difference-1);
				
				for (i=0; i<a_size; ++i)
					GenUpdateA (difference-1/*offset of _Nil*/,difference+i);
			} else {
				for (i=difference-1; i>=0; --i)
					GenPushA (difference-1);
			
				if (difference<a_size){
					GenBuildh (&nil_lab,0);
					
					for (i=difference; i<a_size; ++i){
						GenUpdateA (i+difference+1,i+1);
						GenUpdateA (0,i+difference+1);
					}
					GenPopA (1);
				}
			}						
		}
								
		if (else_bsp>*bsp_p){
			int difference,i;
			
			difference=else_bsp - *bsp_p;
			for (i=b_size-1; i>=0; --i)
#if UPDATE_POP
				if (i==0)
					GenUpdatePopB (0,difference);
				else
#endif
				GenUpdateB (i,i+difference);
#if UPDATE_POP
			if (b_size==0)
#endif			
			GenPopB (difference);
		} else if (else_bsp<*bsp_p){
			int difference,i;
			SymbValue sv;
				
			sv.val_int="0";
			
			difference=*bsp_p - else_bsp;
			
			if (difference>b_size){
				int n;

				PushBasic (IntObj,sv);

				n=(difference-b_size)-1;

				for (i=0; i<n; ++i)
					GenPushB (i);
				
				for (i=b_size-1; i>=0; --i)
					GenPushB (difference-1);

				for (i=0; i<b_size; ++i)
					GenUpdateB (difference-1/*offset of int 0*/,difference+i);
			} else {
				for (i=difference-1; i>=0; --i)
					GenPushB (difference-1);
			
				if (difference<b_size){
					PushBasic (IntObj,sv);
					
					for (i=difference; i<b_size; ++i){
						GenUpdateB (i+difference+1,i+1);
						GenUpdateB (0,i+difference+1);
					}
					GenPopB (1);
				}
			}
		}					
	}
	
	{
		int result_a_size,result_b_size;
		
		DetermineSizeOfState (node->node_state,&result_a_size,&result_b_size);
		
		if (code_gen_node_ids_p->a_node_ids!=NULL){
			int asp_without_result;
			NodeIdListElementP a_node_ids,a_node_id_p;

			asp_without_result=*asp_p-result_a_size;
			a_node_ids=code_gen_node_ids_p->a_node_ids;

			/* JVG: changed 28-10-1999 */
			a_node_id_p=a_node_ids;
			while (a_node_id_p!=NULL && a_node_id_p->nidl_node_id->nid_a_index>asp_without_result)
				if (a_node_id_p->nidl_node_id->nid_a_index!=UNUSED_NODE_ID_INDEX){
					a_node_id_p=a_node_id_p->nidl_next;
					a_node_ids=a_node_id_p;
				} else
					a_node_id_p=a_node_id_p->nidl_next;
			/*
			while (a_node_ids!=NULL && 
					a_node_ids->nidl_node_id->nid_a_index>asp_without_result && a_node_ids->nidl_node_id->nid_a_index!=UNUSED_NODE_ID_INDEX)
			{
				a_node_ids=a_node_ids->nidl_next;
			}
			*/
			code_gen_node_ids_p->a_node_ids=a_node_ids;
		}

		if (code_gen_node_ids_p->b_node_ids!=NULL){
			int bsp_without_result;
			NodeIdListElementP b_node_ids,b_node_id_p;
		
			bsp_without_result=*bsp_p-result_b_size;
			b_node_ids=code_gen_node_ids_p->b_node_ids;

			/* JVG: changed 28-10-1999 */
			b_node_id_p=b_node_ids;
			while (b_node_id_p!=NULL && b_node_id_p->nidl_node_id->nid_b_index>bsp_without_result)
				if (b_node_id_p->nidl_node_id->nid_b_index!=UNUSED_NODE_ID_INDEX){
					b_node_id_p=b_node_id_p->nidl_next;
					b_node_ids=b_node_id_p;
				} else
					b_node_id_p=b_node_id_p->nidl_next;
			/*
			while (b_node_ids!=NULL && 
					b_node_ids->nidl_node_id->nid_b_index>bsp_without_result && b_node_ids->nidl_node_id->nid_b_index!=UNUSED_NODE_ID_INDEX)
			{
				b_node_ids=b_node_ids->nidl_next;
			}
			*/
			code_gen_node_ids_p->b_node_ids=b_node_ids;
		}
	}
	
	GenLabelDefinition (&endif_label);
}
#endif

#if STRICT_LISTS
#include "backendsupport.h"

LabDef *unboxed_cons_label (SymbolP cons_symbol_p)
{ 
	static LabDef unboxed_record_cons_lab;
	
	if (cons_symbol_p->symb_unboxed_cons_state_p->state_type==SimpleState && BETWEEN (IntObj,FileObj,cons_symbol_p->symb_unboxed_cons_state_p->state_object))
		return &unboxed_cons_labels[cons_symbol_p->symb_unboxed_cons_state_p->state_object-IntObj][cons_symbol_p->symb_tail_strictness];
	else if (cons_symbol_p->symb_unboxed_cons_state_p->state_type==RecordState){
		if (ExportLocalLabels){
			unboxed_record_cons_lab.lab_mod=CurrentModule;
			unboxed_record_cons_lab.lab_symbol=cons_symbol_p->symb_unboxed_cons_state_p->state_record_symbol;			
			unboxed_record_cons_lab.lab_issymbol=True;
		} else {
			unboxed_record_cons_lab.lab_name=cons_symbol_p->symb_unboxed_cons_state_p->state_record_symbol->sdef_ident->ident_name;
			unboxed_record_cons_lab.lab_issymbol=False;
		}
		unboxed_record_cons_lab.lab_pref=cons_symbol_p->symb_tail_strictness ? "r_Cons#!" : "r_Cons#";
		unboxed_record_cons_lab.lab_post='\0';

		return &unboxed_record_cons_lab;
	} else if (cons_symbol_p->symb_unboxed_cons_state_p->state_type==ArrayState){		
		return &unboxed_cons_array_label;
	} else
		error_in_function ("unboxed_cons_label");
}
#endif

#if STRICT_LISTS
int simple_expression_without_node_ids (NodeP node_p)
{
	if (node_p->node_kind==NormalNode){
		ArgP arg_p;
		
		for_l (arg_p,node_p->node_arguments,arg_next)
			if (!simple_expression_without_node_ids (arg_p->arg_node))
				return False;

		return True;
	}
	return False;
}
#endif

void push_rational (SymbolP symb)
{
	LabDef integer_record_lab;

	ConvertSymbolToRLabel (&integer_record_lab,BasicSymbolStates [integer_denot].state_record_symbol);

	GenPushZR (symb->symb_val);
	GenBuildR (&integer_record_lab,1,1,1,1);
	GenBuildR (&integer_record_lab,1,1,0+1,0);
	GenPopB (2);
	GenUpdateA (1,3);
	GenUpdateA (0,2);
	GenPopA (2);
}

static void FillNormalNode (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	Symbol symb;
	
	symb = node->node_symbol;

	switch (symb->symb_kind){
		case definition:
			FillSymbol (node,symb->symb_def,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			return;
		case select_symb:
			FillOrReduceSelectSymbol (node,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			return;
		case apply_symb:
#ifdef NEW_APPLY
			FillApply (node,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
#else
			FillSymbol (node,ApplyDef,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
#endif
			return;
		case if_symb:
#ifdef FASTER_STRICT_IF
			if (node->node_arity==3 && !IsLazyState (node->node_state) && update_node_id==NULL)
				fill_strict_if_node (node,asp_p,bsp_p,code_gen_node_ids_p);
			else
#endif
			FillSymbol (node,IfDef,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			return;
		case tuple_symb:
			BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
			if (IsSimpleState (node->node_state)){	
				if (update_node_id==NULL){
					*asp_p+=1-node->node_arity;
					GenBuildh (&tuple_lab,node->node_arity);
				} else {
					GenFillh (&tuple_lab,node->node_arity,*asp_p-update_node_id->nid_a_index,
						node->node_state.state_kind == SemiStrict ? ReleaseAndFill : NormalFill);
					*asp_p-=node->node_arity;
				}
			}
			return;
		case cons_symb:
#if STRICT_LISTS
			if (symb->symb_head_strictness>1 || symb->symb_tail_strictness){
				if (symb->symb_head_strictness==4 && node->node_arity<2){
					FillSymbol (node,symb->symb_unboxed_cons_sdef_p,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
					return;
				} else {
					int lazy_fill;
					
					if (node->node_arity==2){
						lazy_fill=IsLazyState (node->node_state);

						if (lazy_fill){
							int has_unevaluated_strict_arg;
							
							has_unevaluated_strict_arg=0;
							
							if (symb->symb_head_strictness>1){
								NodeP arg_node_p;
								
								arg_node_p=node->node_arguments->arg_node;
								if (arg_node_p->node_kind!=NodeIdNode){
									if (arg_node_p->node_kind==NormalNode &&
										(BETWEEN (int_denot,real_denot,arg_node_p->node_symbol->symb_kind) || arg_node_p->node_symbol->symb_kind==string_denot))
										;
									else
										if (IsLazyState (arg_node_p->node_state))
											has_unevaluated_strict_arg=1;
								} else
									if (IsLazyState (arg_node_p->node_node_id->nid_state))
										has_unevaluated_strict_arg=1;
							}

							if (symb->symb_tail_strictness){
								NodeP arg_node_p;

								arg_node_p=node->node_arguments->arg_next->arg_node;
								if (arg_node_p->node_kind!=NodeIdNode){
									if (IsLazyState (arg_node_p->node_state))
										has_unevaluated_strict_arg=1;
								} else
									if (IsLazyState (arg_node_p->node_node_id->nid_state))
										has_unevaluated_strict_arg=1;
							}

							if (!has_unevaluated_strict_arg){
								if (symb->symb_head_strictness>1){
									NodeP arg_node_p;
									StateP element_state_p;

									if (symb->symb_head_strictness==4)
										element_state_p=symb->symb_unboxed_cons_state_p;
									else
										element_state_p=&StrictState;
									
									arg_node_p=node->node_arguments->arg_node;
									if (arg_node_p->node_kind==NormalNode &&
										(BETWEEN (int_denot,real_denot,arg_node_p->node_symbol->symb_kind) || arg_node_p->node_symbol->symb_kind==string_denot))
									{
										arg_node_p->node_state=*element_state_p;
									}
									
									node->node_arguments->arg_state=*element_state_p;
								}

								if (symb->symb_tail_strictness)
									node->node_arguments->arg_next->arg_state=StrictState;

								lazy_fill=0;
							}
						}
					} else
						lazy_fill=0;

					BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

					{
						LabDef *strict_cons_lab_p,strict_cons_lab;
						int a_size,b_size;

						if (symb->symb_head_strictness==4){
							if (lazy_fill){
								MakeSymbolLabel (&strict_cons_lab,symb->symb_unboxed_cons_sdef_p->sdef_module,d_pref,symb->symb_unboxed_cons_sdef_p,0);
								strict_cons_lab_p=&strict_cons_lab;
							} else
								strict_cons_lab_p=unboxed_cons_label (symb);

							DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
						} else {
							if (symb->symb_head_strictness>1){
								if (symb->symb_tail_strictness)
									strict_cons_lab_p=&conssts_lab;
								else
									strict_cons_lab_p=&conss_lab;
							} else
								strict_cons_lab_p=&consts_lab;

							a_size=node->node_arity;
							b_size=0;
						}

						if (lazy_fill){
							LabDef n_strict_cons_lab;
							
							n_strict_cons_lab = *strict_cons_lab_p;
							n_strict_cons_lab.lab_pref = n_pref;
							
							if (update_node_id==NULL){
								*asp_p+=1-a_size;
								GenBuild (strict_cons_lab_p,a_size,&n_strict_cons_lab);
							} else {
								GenFill (strict_cons_lab_p,a_size,&n_strict_cons_lab,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
								*asp_p-=a_size;
							}
						} else {
							if (update_node_id==NULL){
								*asp_p+=1-a_size;
								if (symb->symb_head_strictness==4)
									GenBuildhr (strict_cons_lab_p,a_size,b_size);
								else
									GenBuildh (node->node_arity==2 ? &cons_lab : strict_cons_lab_p,a_size);									
							} else {
								if (symb->symb_head_strictness==4)
									GenFillR (strict_cons_lab_p,a_size,b_size,*asp_p-update_node_id->nid_a_index,0,0,node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill,True);
								else
									GenFillh (node->node_arity==2 ? &cons_lab : strict_cons_lab_p,a_size,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill);
								*asp_p-=a_size;
							}
						}
						*bsp_p-=b_size;
					}
					return;
				}
			}
#endif
			BuildLazyArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
			if (update_node_id==NULL){
				*asp_p+=1-node->node_arity;
				GenBuildh (&cons_lab,node->node_arity);
			} else {
				GenFillh (&cons_lab, node->node_arity,*asp_p-update_node_id->nid_a_index,
					node->node_state.state_kind == SemiStrict ? ReleaseAndFill : NormalFill);
				*asp_p-=node->node_arity;
			}
			return;
		case nil_symb:
#if STRICT_LISTS
			if ((symb->symb_head_strictness & 1) && !simple_expression_without_node_ids (node->node_arguments->arg_node)){
				BuildArg (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
				GenPopA (1);
				--*asp_p;
			}
#endif
			if (update_node_id==NULL){
				*asp_p+=1;
				GenBuildh (&nil_lab,0);
			} else
				GenFillh (&nil_lab,0,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind == SemiStrict ? ReleaseAndFill : NormalFill);
			return;
		case string_denot:
			GenBuildString (symb->symb_val);
			*asp_p+=1;
			if (IsSimpleState (node->node_state)){
				if (update_node_id==NULL){
					GenBuildh (&BasicDescriptors[ArrayObj],1);
				} else {
					GenFillh (&BasicDescriptors[ArrayObj],1,*asp_p-update_node_id->nid_a_index,ReleaseAndFill);
					*asp_p-=1;
				}
			}
			return;
		case integer_denot:
			GenPushZ (symb->symb_val);
			*asp_p+=1;
			if (IsSimpleState (node->node_state)){			
				LabDef record_lab;

				ConvertSymbolToRLabel (&record_lab,BasicSymbolStates [integer_denot].state_record_symbol);

				if (update_node_id==NULL)
					GenBuildhr (&record_lab,1,1);
				else {
					GenFillR (&record_lab,1,1,*asp_p-update_node_id->nid_a_index,0,0,node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill,True);
					*asp_p-=1;
				}
			} else
				*bsp_p+=1;
			return;
		case rational_denot:
		{
			LabDef ratio_record_lab;

			push_rational (symb);

			ConvertSymbolToKLabel (&ratio_record_lab,special_types[1]->sdef_type->type_constructors->cl_constructor->type_node_symbol->symb_def);

			if (update_node_id==NULL){
				GenBuildhr (&ratio_record_lab,2,0);
				*asp_p+=1;
			} else {
				GenFillR (&ratio_record_lab,2,0,*asp_p+2-update_node_id->nid_a_index,0,0,node->node_state.state_kind==SemiStrict ? ReleaseAndFill : NormalFill,True);
			}
			return;
		}
		case seq_symb:
			if (node->node_arity==2){
				if (IsLazyState (node->node_state)){
					FillSymbol (node,SeqDef,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
				} else {
					int old_asp,old_bsp;

					old_asp=*asp_p;
					old_bsp=*bsp_p;
					BuildArg (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
					if (*asp_p>=old_asp){
						GenPopA (*asp_p-old_asp);
						*asp_p=old_asp;
					}
					if (*bsp_p>=old_bsp){
						GenPopB (*bsp_p-old_bsp);
						*bsp_p=old_bsp;
					}

					if (update_node_id==NULL){
						ArgP arg2_arg;
					
						arg2_arg=node->node_arguments->arg_next;
						if (arg2_arg->arg_node->node_kind!=NodeIdNode){
							Build (arg2_arg->arg_node,asp_p,bsp_p,code_gen_node_ids_p);
						} else {
							NodeId arg_node_id;
							
							arg_node_id=arg2_arg->arg_node->node_node_id;
#if BOXED_RECORDS
							arg_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
#endif
							if (CopyNodeIdArgument (arg2_arg->arg_state,arg_node_id,asp_p,bsp_p))
								ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
							
							decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
						}
					} else
						FillNodeOnACycle (node->node_arguments->arg_next->arg_node,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
				}
			} else {
				LabDef name;
				
				ConvertSymbolToConstructorDLabel (&name,SeqDef);

				BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
						
				if (update_node_id==NULL){
					*asp_p+=1-node->node_arity;
					GenBuildPartialFunctionh (&name,node->node_arity);
				} else {
					GenFillh (&name,node->node_arity,*asp_p-update_node_id->nid_a_index,NormalFill);
					*asp_p-=node->node_arity;
				}
			}
			return;
		default:
			if (symb->symb_kind<Nr_Of_Basic_Types){
				if (update_node_id==NULL){
					*asp_p+=1;
					GenBuildh (&BasicDescriptors[symb->symb_kind],0);
				} else
					GenFillh (&BasicDescriptors[symb->symb_kind],0,*asp_p-update_node_id->nid_a_index,
						node->node_state.state_kind == SemiStrict ? ReleaseAndFill : NormalFill);
			} else {
				ObjectKind denottype;

				denottype = (symb->symb_kind < Nr_Of_Predef_Types)
					? BasicSymbolStates [symb->symb_kind].state_object
					: UnknownObj;

				if (node->node_state.state_object==denottype ||
					node->node_state.state_object==UnknownObj || denottype==UnknownObj
#if ABSTRACT_OBJECT
				|| node->node_state.state_object==AbstractObj || denottype==AbstractObj
#endif
					)
				{
					if (node->node_state.state_kind==OnB){
						*bsp_p += ObjectSizes [denottype];
						PushBasic (denottype, symb->symb_val);
					} else {
						if (update_node_id==NULL){
							*asp_p+=1;
							BuildBasic (denottype,symb->symb_val);
						} else {
							FillBasic (denottype,symb->symb_val,*asp_p-update_node_id->nid_a_index,
								node->node_state.state_kind == SemiStrict ? ReleaseAndFill : NormalFill);
						}
					}
				} else {
					StaticMessage (False,CurrentAltLabel.lab_symbol->sdef_ident->ident_name,Co_Wtype);
					GenTypeError();
				}
			}
	}
}

void RemoveSelectorsFromUpdateNode (ArgS *previous_arg,ArgS *arg)
{
	while (arg!=NULL){
		ArgS *field_arg;
		
		field_arg=arg->arg_node->node_arguments;
		
		previous_arg->arg_next=field_arg;
		previous_arg=field_arg;

		arg=arg->arg_next;
	}
	previous_arg->arg_next=NULL;
}

void UpdateRecordAndAddSelectorsToUpdateNode
	(ArgS *record_arg,ArgS *first_field_arg,StateS *field_states,int record_a_size,int record_b_size,int *end_args_a_offset_p,int *end_args_b_offset_p)
{
	ArgS *arg,*previous_arg;
	int a_offset,b_offset,arg_a_offset,arg_b_offset,previous_field_number;
	
	a_offset=0;
	b_offset=0;
	arg_a_offset=record_a_size;
	arg_b_offset=record_b_size;
	
	previous_field_number=0;
	
	previous_arg=record_arg;
	for_l (arg,first_field_arg,arg_next){
		int field_number,arg_a_size,arg_b_size;
		Node field_node;
		
		field_node=arg->arg_node;
		field_node->node_arguments->arg_next=NULL;
		
		field_number=field_node->node_symbol->symb_def->sdef_sel_field_number;
		
		while (field_number!=previous_field_number){
			AddSizeOfState (field_states[previous_field_number],&a_offset,&b_offset);
			++previous_field_number;
		}
		
		DetermineSizeOfState (field_states[field_number],&arg_a_size,&arg_b_size);
		
		while (arg_a_size){
			GenUpdateA (arg_a_offset,a_offset);
			++arg_a_offset;
			++a_offset;
			--arg_a_size;
		}
	
		while (arg_b_size){
			GenUpdateB (arg_b_offset,b_offset);
			++arg_b_offset;
			++b_offset;
			--arg_b_size;
		}
	
		++previous_field_number;
						
		previous_arg->arg_next=arg;
		previous_arg=arg;
	}
	previous_arg->arg_next=NULL;
	
	*end_args_a_offset_p = arg_a_offset;
	*end_args_b_offset_p = arg_b_offset;
}

void RemoveFieldsFromStackAfterUpdate (int arg_a_offset,int arg_b_offset,int record_a_size,int record_b_size,int *asp_p,int *bsp_p)
{
	if (arg_a_offset!=record_a_size){
		if (record_a_size!=0){
			int a_offset;

			a_offset=record_a_size;
			while (a_offset>0){
				--a_offset;
				--arg_a_offset;
				if (a_offset==0)
					GenUpdatePopA (a_offset,arg_a_offset);
				else
					GenUpdateA (a_offset,arg_a_offset);
			}
		} else
			GenPopA (arg_a_offset);

		*asp_p -= arg_a_offset;
	}
	
	if (arg_b_offset!=record_b_size){
		if (record_b_size!=0){
			int b_offset;

			b_offset=record_b_size;
			while (b_offset>0){
				--b_offset;
				--arg_b_offset;
				if (b_offset==0)
					GenUpdatePopB (b_offset,arg_b_offset);
				else
					GenUpdateB (b_offset,arg_b_offset);
			}
		} else
			GenPopB (arg_b_offset);

		*bsp_p -= arg_b_offset;
	}
}

#ifdef DESTRUCTIVE_RECORD_UPDATES
void compute_bits_and_add_selectors_to_update_node
	(ArgS *record_arg,ArgS *first_field_arg,StateS *field_states,int record_a_size,int record_b_size,
	char bits[],int *n_a_fill_bits_p,int *n_b_fill_bits_p)
{
	ArgP arg,previous_arg;
	int a_offset,b_offset,previous_field_number;
	unsigned int a_bits,b_bits,n;
	int n_a_fill_bits,n_b_fill_bits;

	a_bits=0;
	b_bits=0;
	n_a_fill_bits=0;
	n_b_fill_bits=0;

	a_offset=0;
	b_offset=0;
	
	previous_field_number=0;
	
	previous_arg=record_arg;
	for_l (arg,first_field_arg,arg_next){
		int field_number,arg_a_size,arg_b_size;
		Node field_node;
		
		field_node=arg->arg_node;
		field_node->node_arguments->arg_next=NULL;
		
		field_number=field_node->node_symbol->symb_def->sdef_sel_field_number;
		
		while (field_number!=previous_field_number){
			AddSizeOfState (field_states[previous_field_number],&a_offset,&b_offset);
			++previous_field_number;
		}
		
		DetermineSizeOfState (field_states[field_number],&arg_a_size,&arg_b_size);

		a_bits |= (~((~0)<<arg_a_size))<<a_offset;
		b_bits |= (~((~0)<<arg_b_size))<<b_offset;

		n_a_fill_bits+=arg_a_size;
		n_b_fill_bits+=arg_b_size;
		
		a_offset+=arg_a_size;
		b_offset+=arg_b_size;
	
		++previous_field_number;
						
		previous_arg->arg_next=arg;
		previous_arg=arg;
	}
	previous_arg->arg_next=NULL;

	bits[0]='0';

	for (n=0; n<record_a_size; ++n)
		bits[n+1]='0' + ((a_bits>>n) & 1);
	
	for (n=0; n<record_b_size; ++n)
		bits[n+record_a_size+1]='0' + ((b_bits>>n) & 1);

	bits[record_a_size+record_b_size+1]='\0';

	*n_a_fill_bits_p=n_a_fill_bits;
	*n_b_fill_bits_p=n_b_fill_bits;
}
#endif

#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
static void remove_updated_fields_from_record (int field_number,ArgP field_arg,int a_from_offset,int b_from_offset,int *a_to_offset_p,int *b_to_offset_p,int n_fields,StateP field_states)
{
	if (field_number<n_fields){
		int field_a_size,field_b_size;

		DetermineSizeOfState (field_states[field_number],&field_a_size,&field_b_size);

		a_from_offset+=field_a_size;
		b_from_offset+=field_b_size;

		if (field_arg==NULL || field_arg->arg_node->node_symbol->symb_def->sdef_sel_field_number!=field_number){
			remove_updated_fields_from_record (field_number+1,field_arg,a_from_offset,b_from_offset,a_to_offset_p,b_to_offset_p,n_fields,field_states);
				
			while (field_a_size){
				--a_from_offset;
				--*a_to_offset_p;
				if (a_from_offset!=*a_to_offset_p)
					GenUpdateA (a_from_offset,*a_to_offset_p);
				--field_a_size;
			}
			
			while (field_b_size){
				--b_from_offset;
				--*b_to_offset_p;
				if (b_from_offset!=*b_to_offset_p)
					GenUpdateB (b_from_offset,*b_to_offset_p);
				--field_b_size;
			}		
		} else
			remove_updated_fields_from_record (field_number+1,field_arg->arg_next,a_from_offset,b_from_offset,a_to_offset_p,b_to_offset_p,n_fields,field_states);
	}
}
#endif

#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
static void adjust_state_of_unbox_update_function_argument (ArgP call_arg_p,ArgP rhs_arg_p,ArgP lhs_arg_p,StateP update_rule_arg_p)
{
	StateP arg_state_p;

	if (call_arg_p->arg_node->node_kind!=NodeIdNode)
		arg_state_p=&call_arg_p->arg_node->node_state;
	else
		arg_state_p=&call_arg_p->arg_node->node_node_id->nid_state;

	if (rhs_arg_p->arg_state.state_type==SimpleState){
		if (rhs_arg_p->arg_state.state_kind==OnB && (arg_state_p->state_type==SimpleState && arg_state_p->state_kind==OnB)){
			call_arg_p->arg_state=*arg_state_p;
			lhs_arg_p->arg_state=*arg_state_p;
			*update_rule_arg_p=*arg_state_p;									
		} else if (!IsLazyState (*arg_state_p) && !IsLazyStateKind (rhs_arg_p->arg_state.state_kind)){
			lhs_arg_p->arg_state.state_kind=StrictOnA;
			update_rule_arg_p->state_kind=StrictOnA;										
		}
	} else {
		if ((rhs_arg_p->arg_state.state_type==ArrayState && arg_state_p->state_type==ArrayState) ||
			(rhs_arg_p->arg_state.state_type==RecordState && arg_state_p->state_type==RecordState))
		{
			call_arg_p->arg_state=*arg_state_p;
			lhs_arg_p->arg_state=*arg_state_p;
			*update_rule_arg_p=*arg_state_p;									
		} else if (!IsLazyState (*arg_state_p)){
			lhs_arg_p->arg_state.state_kind=StrictOnA;
			update_rule_arg_p->state_kind=StrictOnA;									
		}
	}
}
#endif

int is_unique_record_update (NodeIdP record_node_id,NodeP record_node)
{
	NodeP selector_node_p;
	
	if (!DoReuseUniqueNodes)
		return 0;
#if 0
	printf ("is_unique_record_update\n");
#endif

	if ((record_node_id->nid_state.state_mark & STATE_UNIQUE_MASK)!=0 &&
#if BOXED_RECORDS
		(
			((record_node_id->nid_mark2 & (NID_RECORD_USED_BY_UPDATE | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_RECORD_USED_BY_UPDATE 
			&& record_node_id->nid_refcount==-2)
			||
			((record_node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 && record_node_id->nid_number== -1)
		) &&
#else
		(record_node_id->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 &&
		record_node_id->nid_number== -1 &&
#endif
		record_node_id->nid_state.state_type==SimpleState &&
		record_node_id->nid_state.state_kind==StrictOnA)
	{
		return 1;
	}

#if BOXED_RECORDS
#  if 0
	printf ("%d %d %d %d\n",record_node_id->nid_state.state_type==SimpleState,record_node_id->nid_mark2,record_node_id->nid_mark,record_node_id->nid_refcount);
#  endif
	if (record_node_id->nid_state.state_type==SimpleState && (record_node_id->nid_mark2 & (NID_RECORD_USED_BY_UPDATE | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_RECORD_USED_BY_UPDATE){
#  if 0
		printf ("is_unique_record_update 2\n");
#  endif
		selector_node_p=record_node;
		
		if ((record_node_id->nid_mark2 & NID_SELECTION_NODE_ID)==0){
			if (record_node_id->nid_refcount==1 && record_node_id->nid_node->node_kind==NodeIdNode){
#  if 0
				printf ("is_unique_record_update_3 %d %d %d\n",record_node_id->nid_mark2,record_node_id->nid_mark,record_node_id->nid_refcount);
#  endif
				selector_node_p=record_node_id->nid_node;
				record_node_id=selector_node_p->node_node_id;
#  if 0
				printf ("is_unique_record_update 3 %d %d %d\n",record_node_id->nid_mark2,record_node_id->nid_mark,record_node_id->nid_refcount);
#  endif
			} else {
#  if 0
				printf ("is_unique_record_update 4 %d %d %d\n",record_node_id->nid_mark2,record_node_id->nid_mark,record_node_id->nid_refcount);
#  endif
				if (record_node_id->nid_refcount>=0 && record_node_id->nid_node->node_kind==NodeIdNode){
					record_node_id=record_node_id->nid_node->node_node_id;
#  if 0
					printf ("is_unique_record_update_4 %d %d %d\n",record_node_id->nid_mark2,record_node_id->nid_mark,record_node_id->nid_refcount);					
#  endif
				}
				return 0;
			}
		}
		
		if ((record_node_id->nid_mark2 & (NID_SELECTION_NODE_ID | NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES))==NID_SELECTION_NODE_ID
			
			&& ((record_node_id->nid_mark & NID_SHARED_SELECTION_NODE_ID)==0 || record_node_id->nid_refcount==0)
# if 0
			&& (record_node_id->nid_mark & NID_SHARED_SELECTION_NODE_ID)==0
# endif
			){
# if 0
			printf ("UpdateNode NodeId NID_UNSHARED_SELECTION_NODE_ID\n");
# endif	
			if (selector_node_p->node_arguments->arg_node->node_kind==NodeIdNode){
				NodeIdP tuple_node_id;
# if 0
				printf ("UpdateNode sel NodeIdNode\n");
# endif
				tuple_node_id=selector_node_p->node_arguments->arg_node->node_node_id;

# if 0
				if (tuple_node_id->nid_node_def->def_node->node_kind==TupleSelectorsNode)
					printf ("UpdateNode sel TupleSelectorsNode %d\n",record_node_id->nid_number);
# endif
				if (tuple_node_id->nid_node->node_kind==NormalNode && tuple_node_id->nid_node->node_symbol->symb_kind==definition){
					StateP tuple_result_state_p;
					
					switch (tuple_node_id->nid_node->node_symbol->symb_def->sdef_kind){
						case IMPRULE:
# if 0
							printf ("UpdateNode sel IMPRULE\n");
# endif
							tuple_result_state_p=&tuple_node_id->nid_node->node_symbol->symb_def->sdef_rule->rule_state_p[-1];
							break;
						case DEFRULE:
						case SYSRULE:
# if 0
							printf ("UpdateNode sel DEFRULE or SYSRULE\n");
# endif
							tuple_result_state_p=&tuple_node_id->nid_node->node_symbol->symb_def->sdef_rule_type->rule_type_state_p[-1];
							break;
						default:
							return 0;
					}
					
					if (tuple_result_state_p->state_type==TupleState
						&& (tuple_result_state_p->state_tuple_arguments[record_node_id->nid_number].state_mark & STATE_UNIQUE_MASK)!=0)
					{
# if 0
						printf ("UpdateNode * sel RULE\n");
# endif
						return 1;
					}
				}
			}
		}
	}
#endif
	
	return 0;
}

static void FillUpdateNode (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	ArgS *record_arg,*first_field_arg;
	int record_a_size,record_b_size;

	record_arg=node->node_arguments;
	first_field_arg=record_arg->arg_next;

	RemoveSelectorsFromUpdateNode (record_arg,first_field_arg);

#if 1
	{
		ArgP node_arg;

		for_l (node_arg,record_arg->arg_next,arg_next){
			NodeP arg_node;

			arg_node=node_arg->arg_node;
			if (arg_node->node_kind==SelectorNode && arg_node->node_arity==1 && IsLazyState (arg_node->node_state)){
				NodeP node_id_node;
				
				node_id_node=arg_node->node_arguments->arg_node;
				if (node_id_node->node_kind==NodeIdNode && node_id_node->node_node_id->nid_state.state_type==RecordState){
					int field_number;
					
					field_number=arg_node->node_symbol->symb_def->sdef_sel_field_number;
					arg_node->node_state=node_id_node->node_node_id->nid_state.state_record_arguments[field_number];
				}
			}
		}
	}
#endif
	
	if (IsSimpleState (node->node_state)){
		int n_arguments;
		LabDef name,codelab;
		SymbDef new_update_sdef;
		struct node *record_node;
#if DESTRUCTIVE_RECORD_UPDATES
		int update_immediately;
		StateP record_state_p;

		record_node=record_arg->arg_node;

		if (node->node_state.state_kind==StrictOnA){
			update_immediately=1;
			record_state_p=&node->node_symbol->symb_def->sdef_record_state;
		} else {
			update_immediately=0;

			if (record_node->node_kind==NodeIdNode)
				record_state_p=&record_node->node_node_id->nid_state;
			else
				record_state_p=&record_node->node_state;
			
			if (record_state_p->state_type==SimpleState && record_state_p->state_kind==StrictOnA)
				record_state_p=&node->node_symbol->symb_def->sdef_record_state;

			if (record_state_p->state_type==RecordState){
#else
		record_node=record_arg->arg_node;

		if (record_node->node_kind==NodeIdNode){
			StateP record_state_p;

			record_state_p=&record_node->node_node_id->nid_state;
			
			if (record_state_p->state_type==SimpleState && record_state_p->state_kind==StrictOnA)
				record_state_p=&node->node_symbol->symb_def->sdef_record_state;

			if (record_state_p->state_type==RecordState){
				int update_immediately;

#endif
				update_immediately=1;

				if (record_state_p->state_record_symbol->sdef_strict_constructor){
					StateS *record_states;
					
					record_states=record_state_p->state_record_arguments;

					if (!FieldArgumentNodeStatesAreStricter (record_arg->arg_next,first_field_arg,record_states))
						update_immediately=0;
					else {
						ArgP node_arg,field_arg;
						
						for_ll (node_arg,field_arg,record_arg->arg_next,first_field_arg,arg_next,arg_next){
							Node arg_node;
							int field_number;
		
							field_number=field_arg->arg_node->node_symbol->symb_def->sdef_sel_field_number;
		
							arg_node=node_arg->arg_node;
							if (arg_node->node_kind==NormalNode &&
								(BETWEEN (int_denot,real_denot,arg_node->node_symbol->symb_kind) || arg_node->node_symbol->symb_kind==string_denot))
							{
								arg_node->node_state=record_states[field_number];
							}
							
							node_arg->arg_state=record_states[field_number];
						}
					}
				}
#if DESTRUCTIVE_RECORD_UPDATES
			}
		}

		if (update_immediately){
			BuildArgs (record_arg->arg_next,asp_p,bsp_p,code_gen_node_ids_p);

			if (record_node->node_kind==NodeIdNode){
				NodeIdP record_node_id;
				
				record_node_id=record_node->node_node_id;

				if (is_unique_record_update (record_node_id,record_node) && update_node_id==NULL){
					int n_a_fill_bits,n_b_fill_bits;
					char bits[MaxNodeArity+2];
					LabDef record_lab;

#if BOXED_RECORDS
					record_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
#endif
					DetermineSizeOfState (*record_state_p,&record_a_size,&record_b_size);
				
					compute_bits_and_add_selectors_to_update_node (record_arg,first_field_arg,
						record_state_p->state_record_arguments,record_a_size,record_b_size,
						bits,&n_a_fill_bits,&n_b_fill_bits);

					ConvertSymbolToRLabel (&record_lab,record_state_p->state_record_symbol);

					if (record_a_size+record_b_size>2)
						GenFill2R (&record_lab,record_a_size,record_b_size,*asp_p-record_node_id->nid_a_index,bits);
					else
						GenFill1R (&record_lab,record_a_size,record_b_size,*asp_p-record_node_id->nid_a_index,bits);
				
					*asp_p-=n_a_fill_bits;
					*bsp_p-=n_b_fill_bits;

					GenPushA (*asp_p-record_node_id->nid_a_index);
					*asp_p+=1;

					decrement_reference_count_of_node_id (record_node_id,&code_gen_node_ids_p->free_node_ids);
					
					return;
				}
# if BOXED_RECORDS
				record_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
# endif
			}
#else
				if (update_immediately){
#endif

			record_arg->arg_state=*record_state_p;

			BuildArg (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

			DetermineSizeOfState (*record_state_p,&record_a_size,&record_b_size);

			{
			int end_args_a_offset,end_args_b_offset;
			
			UpdateRecordAndAddSelectorsToUpdateNode (record_arg,first_field_arg,
				record_state_p->state_record_arguments,record_a_size,record_b_size,&end_args_a_offset,&end_args_b_offset);

			if (update_node_id==NULL){
				BuildNewRecordPop (record_state_p->state_record_symbol,record_a_size,record_b_size);
				GenUpdatePopA (0,end_args_a_offset-record_a_size);
				GenPopB (end_args_b_offset-record_b_size);
				*asp_p+=1-end_args_a_offset;
				*bsp_p-=end_args_b_offset;
			} else {
				BuildRecord (record_state_p->state_record_symbol,*asp_p,*bsp_p,*asp_p,*bsp_p,record_a_size,record_b_size,
						*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill,False);
				GenPopA (end_args_a_offset);
				*asp_p-=end_args_a_offset;
				GenPopB (end_args_b_offset);
				*bsp_p-=end_args_b_offset;
			}
			}

			return;
#if DESTRUCTIVE_RECORD_UPDATES
		}
#else
				}
			}
		}
#endif

		n_arguments=node->node_arity;

#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
		if (update_node_id==NULL){
			ArgP call_arg_p,lhs_arg_p,rhs_arg_p,*call_arg_h;
			StateP record_state_p;
			int i,unbox_record;
			ImpRuleP update_rule_p;
			NodeP call_node_p;

			call_arg_p=node->node_arguments;

			unbox_record =	call_arg_p->arg_node->node_kind!=NodeIdNode ? 
							call_arg_p->arg_node->node_state.state_type==RecordState :
							call_arg_p->arg_node->node_node_id->nid_state.state_type==RecordState;

			new_update_sdef=CreateUpdateFunction (record_arg,first_field_arg,node,unbox_record);

			update_rule_p=new_update_sdef->sdef_rule;
			
			lhs_arg_p=update_rule_p->rule_alts->alt_lhs_root->node_arguments;
			rhs_arg_p=update_rule_p->rule_alts->alt_rhs_root->node_arguments;
			i=0;
			
			record_state_p=&node->node_symbol->symb_def->sdef_record_state;

			if (unbox_record){
				int record_size,n_old_fields,field_number;
				ArgP field_arg_p;
				
				record_size=record_state_p->state_arity;
				
				n_old_fields=record_size-(node->node_arity-1);

				call_node_p=NewNode (NULL,NULL,record_size);
				call_arg_h=&call_node_p->node_arguments;
				
				field_arg_p=first_field_arg;
				field_number=0;
				
				while (n_old_fields){
					if (field_arg_p==NULL || field_arg_p->arg_node->node_symbol->symb_def->sdef_sel_field_number!=field_number){
						StateP arg_state_p;
						NodeP new_node_p;
						ArgP new_arg_p;
					
						new_node_p=NewNode (NULL,NULL,0);
						new_arg_p=NewArgument (new_node_p);
						
						arg_state_p=&record_state_p->state_record_arguments[field_number];
						
						new_arg_p->arg_state=*arg_state_p;
						new_node_p->node_state=*arg_state_p;
						lhs_arg_p->arg_state=*arg_state_p;
						update_rule_p->rule_state_p[i]=*arg_state_p;
						
						*call_arg_h=new_arg_p;
						call_arg_h=&new_arg_p->arg_next;
						
						lhs_arg_p=lhs_arg_p->arg_next;
						++i;
						--n_old_fields;
					} else
						field_arg_p=field_arg_p->arg_next;
					
					++field_number;
				}
				
				call_arg_p->arg_state=*record_state_p;

				call_arg_p=call_arg_p->arg_next;
				*call_arg_h=call_arg_p;

				while (call_arg_p!=NULL){
					while (rhs_arg_p->arg_node->node_node_id!=lhs_arg_p->arg_node->node_node_id)
						rhs_arg_p=rhs_arg_p->arg_next;
						
					adjust_state_of_unbox_update_function_argument (call_arg_p,rhs_arg_p,lhs_arg_p,&update_rule_p->rule_state_p[i]);

					call_arg_p=call_arg_p->arg_next;
					lhs_arg_p=lhs_arg_p->arg_next;
					rhs_arg_p=rhs_arg_p->arg_next;
					++i;
				}
			} else {
				while (call_arg_p!=NULL){
					adjust_state_of_unbox_update_function_argument (call_arg_p,rhs_arg_p,lhs_arg_p,&update_rule_p->rule_state_p[i]);

					call_arg_p=call_arg_p->arg_next;
					lhs_arg_p=lhs_arg_p->arg_next;
					rhs_arg_p=rhs_arg_p->arg_next;
					++i;
				}
				{
				ArgP arg1_p;
				
				arg1_p=NewArgument (NULL);
				*arg1_p=*node->node_arguments;
				
				call_node_p=NewNode (NULL,arg1_p,node->node_arity);
				call_arg_h=&arg1_p->arg_next;
				}
			}
			
			while (*call_arg_h){
				ArgP new_call_arg_p;
				NodeP new_node_p;
					
				new_node_p=NewNode (NULL,NULL,0);
				new_call_arg_p=NewArgument (NULL);
				*new_call_arg_p=**call_arg_h;
			
				new_call_arg_p->arg_node=new_node_p;
				new_node_p->node_state=(*call_arg_h)->arg_state;
			
				new_call_arg_p->arg_next=(*call_arg_h)->arg_next;
				
				*call_arg_h=new_call_arg_p;
				call_arg_h=&new_call_arg_p->arg_next;
			}
			
			update_rule_p->rule_mark |= RULE_UNBOXED_LAZY_CALL;
			update_rule_p->rule_lazy_call_node=call_node_p;

			BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

			{
			int a_size,b_size;
			
			DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);

			if (unbox_record){
				int a_to_offset,b_to_offset;
								
				DetermineSizeOfState (*record_state_p,&record_a_size,&record_b_size);

				a_to_offset=record_a_size;
				b_to_offset=record_b_size;
				
				remove_updated_fields_from_record (0,first_field_arg,0,0,&a_to_offset,&b_to_offset,record_state_p->state_arity,record_state_p->state_record_arguments);
				
				GenPopA (a_to_offset);
				GenPopB (b_to_offset);

				*asp_p-=a_to_offset;
				*bsp_p-=b_to_offset;

				a_size-=a_to_offset;
				b_size-=b_to_offset;
			}

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
			
			ConvertSymbolToDandNLabel (&name,&codelab,new_update_sdef);
										
			*asp_p += 1-a_size;
			*bsp_p -= b_size;

			if (b_size!=0)
				GenBuildU (&name,a_size,b_size,&codelab);
			else
				GenBuild (&name,a_size,&codelab);

			return;
			}
		} else {
			BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

			new_update_sdef=CreateUpdateFunction (record_arg,first_field_arg,node,False);
		}
#else
		BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

		new_update_sdef=CreateUpdateFunction (record_arg,first_field_arg,node);
#endif

		ConvertSymbolToDandNLabel (&name,&codelab,new_update_sdef);

		if (update_node_id==NULL){
			GenBuild (&name,n_arguments,&codelab);
			*asp_p+=1-n_arguments;
		} else {
			GenFill (&name,n_arguments,&codelab,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
			*asp_p-=n_arguments;
		}
	} else {
		int end_args_a_offset,end_args_b_offset;

#if BOXED_RECORDS
		node->node_arguments->arg_state=node->node_symbol->symb_def->sdef_record_state;
#endif		
		BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

		DetermineSizeOfState (node->node_state,&record_a_size,&record_b_size);
		UpdateRecordAndAddSelectorsToUpdateNode (record_arg,first_field_arg,
			node->node_state.state_record_arguments,record_a_size,record_b_size,&end_args_a_offset,&end_args_b_offset);
		RemoveFieldsFromStackAfterUpdate (end_args_a_offset,end_args_b_offset,record_a_size,record_b_size,asp_p,bsp_p);
	}
}

int selector_m_error_lab_used = 0;

LabDef selector_m_error_lab = {NULL,"",False,"selector_m_error",0};

void FillMatchNode (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	int node_arity_eq_one,n_dictionaries;
	Symbol symbol;

	BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

	symbol=node->node_symbol;

	if (symbol->symb_kind==definition && symbol->symb_def->sdef_kind==CONSTRUCTOR){
		n_dictionaries = symbol->symb_def->sdef_arity - node->node_arity;
		if (node->node_arity==1)
			node_arity_eq_one=1;
		else
			node_arity_eq_one=0;
	} else {
		n_dictionaries=0;
		node_arity_eq_one=0;
	}

	if (IsSimpleState (node->node_state) && !(node_arity_eq_one && !IsLazyState (node->node_state))){
		int n_arguments,strict_constructor;
		LabDef name,codelab;
		SymbDef new_match_sdef;

		strict_constructor=0;
		
		if (symbol->symb_kind==definition && symbol->symb_def->sdef_kind==CONSTRUCTOR){
			if (symbol->symb_def->sdef_strict_constructor)
				strict_constructor=1;
			else
				if (symbol->symb_def->sdef_type->type_nr_of_constructors==1 && n_dictionaries==0){
					if (node_arity_eq_one){
						LabDef sellab, nsellab;

						BuildLazyTupleSelectorLabel (&nsellab,1,1);
											
						sellab = nsellab;
						sellab.lab_pref	= d_pref;
					
						if (update_node_id==NULL){
							GenBuild (&sellab,-1,&nsellab);
						} else {
							GenFill (&sellab,-1,&nsellab,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
							*asp_p-=1;
						}
					} else
						if (update_node_id!=NULL){
							GenFillFromA (0,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
							GenPopA (1);
							*asp_p-=1;
						}
					
					return;
				}
		}

		if (!node_arity_eq_one){
#if STRICT_LISTS
			if (symbol->symb_kind==cons_symb && (symbol->symb_head_strictness>1 || symbol->symb_tail_strictness))
				strict_constructor=1;
#endif
			new_match_sdef=create_match_function (symbol,node->node_arity,n_dictionaries,strict_constructor);
		} else
			new_match_sdef=create_select_and_match_function (symbol,n_dictionaries,strict_constructor);
	
		ConvertSymbolToDandNLabel (&name,&codelab,new_match_sdef);
		
		n_arguments=1;

		if (update_node_id==NULL){
			GenBuild (&name,n_arguments,&codelab);
		} else {
			GenFill (&name,n_arguments,&codelab,*asp_p-update_node_id->nid_a_index,node->node_state.state_kind==SemiStrict ? PartialFill : NormalFill);
			*asp_p-=1;
		}
	} else {
		struct state *demanded_state_array;
		int demanded_state_arity;
		int a_size,b_size;
		struct arg *argument;
		struct symbol *symbol;
		int branch;

		argument = node->node_arguments;
		
		DetermineSizeOfState (argument->arg_state,&a_size,&b_size);

		if (CoerceStateKind (StrictOnA,argument->arg_state.state_kind)==Reduce)
			GenJsrEval (0);

		symbol=node->node_symbol;
		
		branch=1;		

		switch (symbol->symb_kind){
			case cons_symb:
#if STRICT_LISTS
				if (symbol->symb_head_strictness==1 || symbol->symb_head_strictness>=3){
					GenEqDesc (&nil_lab,0,0);
					GenNotB();
				} else
#endif
				GenEqDesc (&cons_lab,2,0);
				break;
			case definition:
			{
				SymbDef sdef;

				sdef=symbol->symb_def;
				
				if (sdef->sdef_kind==CONSTRUCTOR){
					if (sdef->sdef_type->type_nr_of_constructors==1){
						branch=0;
					} else {
						LabDef symbol_label;

						if (sdef->sdef_strict_constructor){
							ConvertSymbolToKLabel (&symbol_label,sdef);
							GenEqDesc (&symbol_label,0,0);
						} else {
							ConvertSymbolToConstructorDLabel (&symbol_label,sdef);
							GenEqDesc (&symbol_label,node->node_arity,0);
						}
					}
					break;
				}
			}
			default:
				error_in_function ("FillMatchNode");
		}

		if (branch){
#if 1
			selector_m_error_lab_used=1;
			GenExitFalse (&selector_m_error_lab);
#else
			LabDef local_label;
	
			MakeLabel (&local_label,m_symb,NewLabelNr++,no_pref);
			GenJmpTrue (&local_label);
	
			GenJmp (&selector_m_error_lab);
	
			GenLabelDefinition	(&local_label);
#endif
		}

		if (node_arity_eq_one){
			demanded_state_array=&node->node_state;
			demanded_state_arity=1;
		} else {
			demanded_state_array=node->node_state.state_tuple_arguments;
			demanded_state_arity=node->node_state.state_arity;
		}

		if (symbol->symb_kind==definition && symbol->symb_def->sdef_kind==CONSTRUCTOR && symbol->symb_def->sdef_strict_constructor){
			StateP constructor_args_state_p;
			int a_size,b_size,arity;
			
			arity=symbol->symb_def->sdef_arity;		

			constructor_args_state_p=symbol->symb_def->sdef_constructor->cl_state_p;
			DetermineSizeOfStates (arity,constructor_args_state_p,&a_size,&b_size);

			GenReplRArgs (a_size,b_size);
			*asp_p -= 1-a_size;
			*bsp_p += b_size;

			if (n_dictionaries!=0){
				int dictionaries_asize,dictionaries_bsize;

				DetermineSizeOfStates (n_dictionaries,constructor_args_state_p,&dictionaries_asize,&dictionaries_bsize);
				GenPopA (dictionaries_asize);
				GenPopB (dictionaries_bsize);
				*asp_p -= dictionaries_asize;
				*bsp_p -= dictionaries_bsize;

				a_size-=dictionaries_asize;
				b_size-=dictionaries_bsize;
				constructor_args_state_p=&constructor_args_state_p[n_dictionaries];

				arity-=n_dictionaries;
			}

			AdjustTuple (a_size,b_size,asp_p,bsp_p,arity,demanded_state_array,constructor_args_state_p,a_size,b_size);
		} else
#if STRICT_LISTS
		if (symbol->symb_kind==cons_symb && (symbol->symb_head_strictness>1 || symbol->symb_tail_strictness)){
			StateS head_and_tail_states[2];
			
			if (symbol->symb_head_strictness>1){
				if (symbol->symb_head_strictness==4)
					head_and_tail_states[0]=*symbol->symb_state_p;
				else
					head_and_tail_states[0]=StrictState;
			} else
				head_and_tail_states[0]=LazyState;
			
			if (symbol->symb_tail_strictness)
				head_and_tail_states[1]=StrictState;
			else
				head_and_tail_states[1]=LazyState;
			
			if (symbol->symb_head_strictness==4){
				DetermineSizeOfState (head_and_tail_states[0],&a_size,&b_size);
				++a_size;

				GenReplRArgs (a_size,b_size);
				*asp_p -= 1-a_size;
				*bsp_p += b_size;

				AdjustTuple (a_size,b_size,asp_p,bsp_p,2,demanded_state_array,head_and_tail_states,a_size,b_size);
			} else {
				GenReplArgs (2,2);
				*asp_p -= 1-2;
				
				AdjustTuple (2,0,asp_p,bsp_p,2,demanded_state_array,head_and_tail_states,2,0);			
			}
		} else
#endif
		{
			*asp_p-=1;

			if (n_dictionaries==0)
				UnpackTuple (*asp_p,asp_p,bsp_p,True,demanded_state_arity,demanded_state_array);
			else {
				int arity,aselmts,oldaframesize,locasp,asize,maxasize;
				StateP argstates;

				/* untested code, probably not used because dictionaries are strict */

				GenReplArgs (symbol->symb_def->sdef_arity,symbol->symb_def->sdef_arity);

				GenPopA (n_dictionaries);

				arity=demanded_state_arity;
				argstates=demanded_state_array;

				aselmts = 0;
				locasp = arity;
				asize = 0;
				maxasize = arity;

				AddStateSizesAndMaxFrameSizes (arity, argstates, &maxasize, &asize,bsp_p);

				InitAStackConversions (maxasize+1, &oldaframesize);

				EvaluateAndMoveArguments (arity,argstates,&locasp,&aselmts);

				GenAStackConversions (locasp,aselmts);

				FreeAFrameSpace	(oldaframesize);
				*asp_p += aselmts;
			}
		}
	}	
}

#ifdef REUSE_UNIQUE_NODES
# if GENERATE_CODE_AGAIN
void restore_removed_arguments (ArgP *arg_h,ArgP removed_args,unsigned int argument_overwrite_bits,int node_arity)
{
	int arg_n;
	ArgP not_removed_args;
	
	not_removed_args=*arg_h;
	
	for (arg_n=0; arg_n<node_arity; ++arg_n){
		if (argument_overwrite_bits & (1<<arg_n)){
			*arg_h=not_removed_args;
			arg_h=&not_removed_args->arg_next;
			not_removed_args=not_removed_args->arg_next;
		} else {
			*arg_h=removed_args;
			arg_h=&removed_args->arg_next;
			removed_args=removed_args->arg_next;
		}
	}	
}
# endif

#if GENERATE_CODE_AGAIN
 ArgP
#else
 void
#endif
	compute_bits_and_remove_unused_arguments_for_strict_node (NodeP node,char bits[],unsigned int argument_overwrite_bits,
															  int *a_size_p,int *b_size_p,int *n_a_fill_bits_p,int *n_b_fill_bits_p)
{
	unsigned int a_bits,b_bits,a_size,b_size,n,arg_n;
	int n_a_fill_bits,n_b_fill_bits,node_arity;
	ArgS **arg_l;
#if GENERATE_CODE_AGAIN
	ArgP removed_args,*removed_args_l;
	
	removed_args_l=&removed_args;
#endif
	
	arg_l=&node->node_arguments;
	node_arity=node->node_arity;
	
	a_bits=0;
	b_bits=0;
	a_size=0;
	b_size=0;
	n_a_fill_bits=0;
	n_b_fill_bits=0;

	for (arg_n=0; arg_n<node_arity; ++arg_n){
		ArgP arg_p;
		int arg_a_size,arg_b_size;
		
		arg_p=*arg_l;
		
		DetermineSizeOfState (arg_p->arg_state,&arg_a_size,&arg_b_size); 
		
		if (argument_overwrite_bits & (1<<arg_n)){
			a_bits |= (~((~0)<<arg_a_size))<<a_size;
			b_bits |= (~((~0)<<arg_b_size))<<b_size;

			n_a_fill_bits+=arg_a_size;
			n_b_fill_bits+=arg_b_size;
			
			arg_l=&arg_p->arg_next;
		} else {
			*arg_l=arg_p->arg_next;
#if GENERATE_CODE_AGAIN
			*removed_args_l=arg_p;
			removed_args_l=&arg_p->arg_next;
#endif
		}

		a_size+=arg_a_size;
		b_size+=arg_b_size;
	}
#if GENERATE_CODE_AGAIN
	*removed_args_l=NULL;
#endif

	for (n=0; n<a_size; ++n)
		bits[n+1]='0' + ((a_bits>>n) & 1);
	
	for (n=0; n<b_size; ++n)
		bits[n+a_size+1]='0' + ((b_bits>>n) & 1);

	bits[a_size+b_size+1]='\0';

	*a_size_p=a_size;
	*b_size_p=b_size;
	*n_a_fill_bits_p=n_a_fill_bits;
	*n_b_fill_bits_p=n_b_fill_bits;

#if GENERATE_CODE_AGAIN
	return removed_args;
#endif
}

#if GENERATE_CODE_AGAIN
 ArgP
#else
 void
#endif
	compute_bits_and_remove_unused_arguments (NodeP node,char bits[],unsigned int argument_overwrite_bits,unsigned int *n_args_p)
{
	unsigned int arg_n;
	int node_arity,n_args;
	ArgS **arg_l;
#if GENERATE_CODE_AGAIN
	ArgP removed_args,*removed_args_l;
	
	removed_args_l=&removed_args;
#endif

	arg_l=&node->node_arguments;
	node_arity=node->node_arity;

	n_args=0;
	
	for (arg_n=0; arg_n<node_arity; ++arg_n){
		ArgP arg_p;
		
		arg_p=*arg_l;
		if (argument_overwrite_bits & (1<<arg_n)){
			bits[arg_n+1]='1';
			arg_l=&(arg_p->arg_next);
			++n_args;
		} else {
			bits[arg_n+1]='0';
			*arg_l=arg_p->arg_next;
#if GENERATE_CODE_AGAIN
			*removed_args_l=arg_p;
			removed_args_l=&arg_p->arg_next;
#endif
		}
	}

	bits[arg_n+1]='\0';
	*n_args_p=n_args;

#if GENERATE_CODE_AGAIN
	*removed_args_l=NULL;

	return removed_args;
#endif
}

static void fill_strict_unique_node (NodeP node,NodeP update_node,char bits[],LabDef *label_p,NodeIdP free_unique_node_id,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	int a_size,b_size,n_a_fill_bits,n_b_fill_bits;
#if GENERATE_CODE_AGAIN
	ArgP removed_args=
#endif
	compute_bits_and_remove_unused_arguments_for_strict_node (node,bits,update_node->node_arguments->arg_occurrence,
															  &a_size,&b_size,&n_a_fill_bits,&n_b_fill_bits);
		
	BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

#if GENERATE_CODE_AGAIN
	if (call_code_generator_again)
		restore_removed_arguments (&node->node_arguments,removed_args,update_node->node_arguments->arg_occurrence,node->node_arity);
#endif
	
	if (a_size+b_size>2)
		GenFill2R (label_p,a_size,b_size,*asp_p-free_unique_node_id->nid_a_index,bits);
	else
		GenFill1R (label_p,a_size,b_size,*asp_p-free_unique_node_id->nid_a_index,bits);

	*asp_p-=n_a_fill_bits;
	*bsp_p-=n_b_fill_bits;

	GenPushA (*asp_p-free_unique_node_id->nid_a_index);
	*asp_p+=1;

	decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);
}

static void FillUniqueNodeWithNode (NodeP update_node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
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
					if (push_node->node_push_symbol==node->node_symbol && push_node->node_arity==node_arity)
						bits[0]='0';
					else
						bits[0]='1';
					
					if (sdef->sdef_strict_constructor){
						ConvertSymbolToKLabel (&name,sdef);
						
						fill_strict_unique_node (node,update_node,bits,&name,free_unique_node_id,asp_p,bsp_p,code_gen_node_ids_p);
						
						return;
					} else {
						ConvertSymbolToConstructorDLabel (&name,sdef);
						label_p=&name;
					}
					break;
				case RECORDTYPE:
					if (push_node->node_push_symbol==node->node_symbol && push_node->node_arity==node_arity)
						bits[0]='0';
					else
						bits[0]='1';

					ConvertSymbolToRLabel (&name,sdef);

					fill_strict_unique_node (node,update_node,bits,&name,free_unique_node_id,asp_p,bsp_p,code_gen_node_ids_p);
					
					return;
				case IMPRULE:
				case DEFRULE:
				case SYSRULE:
					if (IsLazyState (node->node_state)){
						LabDef codelab;

						ConvertSymbolToDandNLabel (&name,&codelab,sdef);

						BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
						
						if (sdef->sdef_kind==IMPRULE && (sdef->sdef_rule->rule_mark & RULE_UNBOXED_LAZY_CALL)){
							int a_size,b_size;
														
							DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);

							if (a_size+b_size>node->node_arity){
								*asp_p += 1-a_size;
								*bsp_p -= b_size;
								if (b_size!=0)
									GenBuildU (&name,a_size,b_size,&codelab);
								else
									GenBuild (&name,a_size,&codelab);
							} else {
								if (b_size!=0){
									GenFillU (&name,a_size,b_size,&codelab,*asp_p-free_unique_node_id->nid_a_index);
									*bsp_p -= b_size;									
								} else
									GenFill (&name,a_size,&codelab,*asp_p-free_unique_node_id->nid_a_index,NormalFill);
								*asp_p-=a_size;

								GenPushA (*asp_p-free_unique_node_id->nid_a_index);
								*asp_p+=1;							
							}
						} else {
							GenFill (&name,node->node_arity,&codelab,*asp_p-free_unique_node_id->nid_a_index,NormalFill);
							*asp_p-=node->node_arity;

							GenPushA (*asp_p-free_unique_node_id->nid_a_index);
							*asp_p+=1;
						}
						decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);
						
						return;
					} else {
						int a_size,b_size;

						ConvertSymbolToLabel (&name,sdef);
						
						BuildArgsWithResultNodeOnStack (node->node_arguments,free_unique_node_id,asp_p,bsp_p,code_gen_node_ids_p,&a_size,&b_size);
													
						*asp_p-=a_size;
						*bsp_p-=b_size;

						if (! (sdef->sdef_kind==SYSRULE
								&& sdef->sdef_ident->ident_instructions!=NULL
								&& *sdef->sdef_ident->ident_instructions!='\0'
								&& *sdef->sdef_ident->ident_instructions!='.'))
						{
							cleanup_stack (asp_p,bsp_p,a_size,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
											&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,
											code_gen_node_ids_p->doesnt_fail);
						}

						CallFunction (&name,sdef,True,node);
						
						AddSizeOfState (node->node_state,asp_p,bsp_p);

						return;
					}						
				default:
					error_in_function ("FillUniqueNodeWithNode");
					return;
			}
			break;
		}
		case cons_symb:
			node_arity=2;

#if STRICT_LISTS
			if (symbol->symb_head_strictness>1 || symbol->symb_tail_strictness){
				SymbolP pattern_symbol_p;
				
				pattern_symbol_p=push_node->node_push_symbol;
				if (pattern_symbol_p->symb_kind==cons_symb && push_node->node_arity==node_arity
					&& ((pattern_symbol_p->symb_head_strictness<3 && symbol->symb_head_strictness<3)
						|| (pattern_symbol_p->symb_head_strictness==4 && symbol->symb_head_strictness==4
							&& pattern_symbol_p->symb_tail_strictness==symbol->symb_tail_strictness
							&& EqualState (*pattern_symbol_p->symb_state_p,*symbol->symb_unboxed_cons_state_p))))
					bits[0]='0';
				else
					bits[0]='1';

				if (symbol->symb_head_strictness==4){
					LabDef *strict_cons_lab_p;
					
					strict_cons_lab_p=unboxed_cons_label (symbol);
						
					fill_strict_unique_node (node,update_node,bits,strict_cons_lab_p,free_unique_node_id,asp_p,bsp_p,code_gen_node_ids_p);
				} else {					
#if GENERATE_CODE_AGAIN
					ArgP removed_args=
#endif
					compute_bits_and_remove_unused_arguments (node,bits,update_node->node_arguments->arg_occurrence,&n_args);

					BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

#if GENERATE_CODE_AGAIN
					if (call_code_generator_again)
						restore_removed_arguments (&node->node_arguments,removed_args,update_node->node_arguments->arg_occurrence,node_arity);
#endif
					GenFill1 (&cons_lab,node_arity,*asp_p-free_unique_node_id->nid_a_index,bits);

					*asp_p-=n_args;

					GenPushA (*asp_p-free_unique_node_id->nid_a_index);
					*asp_p+=1;

					decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);
				}								
				return;
			}
#endif

			if (push_node->node_push_symbol->symb_kind==cons_symb && push_node->node_arity==node_arity)
				bits[0]='0';
			else
				bits[0]='1';

			label_p=&cons_lab;
			break;
		case tuple_symb:
			node_arity=node->node_arity;

			if (push_node->node_push_symbol->symb_kind==tuple_symb && push_node->node_arity==node_arity)
				bits[0]='0';
			else
				bits[0]='1';

			label_p=&tuple_lab;
			break;
		default:
			error_in_function ("FillUniqueNodeWithNode");
			return;
	}

#if GENERATE_CODE_AGAIN
	{
	ArgP removed_args=
#endif
	compute_bits_and_remove_unused_arguments (node,bits,update_node->node_arguments->arg_occurrence,&n_args);

	BuildLazyArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);

#if GENERATE_CODE_AGAIN
	if (call_code_generator_again)
		restore_removed_arguments (&node->node_arguments,removed_args,update_node->node_arguments->arg_occurrence,node_arity);
	}
#endif

	if (node_arity<=2)
		GenFill1 (label_p,node_arity,*asp_p-free_unique_node_id->nid_a_index,bits);
	else
		GenFill2 (label_p,node_arity,*asp_p-free_unique_node_id->nid_a_index,bits);

	*asp_p-=n_args;

	GenPushA (*asp_p-free_unique_node_id->nid_a_index);
	*asp_p+=1;

	decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);
}
#endif

#if ! OPTIMIZE_LAZY_TUPLE_RECURSION
static
#endif
void FillNodeOnACycle (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p)
{
	switch (node->node_kind){
		case NormalNode:
			FillNormalNode (node,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			break;
		case SelectorNode:
			FillOrReduceFieldSelection (node,node->node_symbol->symb_def,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			break;
		case UpdateNode:
			FillUpdateNode (node,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			break;
		case MatchNode:
			FillMatchNode (node,asp_p,bsp_p,update_node_id,code_gen_node_ids_p);
			break;
		default:
			error_in_function ("FillNodeOnACycle");
	}	
}

void Build (Node node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	switch (node->node_kind){
		case NormalNode:
			FillNormalNode (node,asp_p,bsp_p,NULL,code_gen_node_ids_p);
			break;
		case SelectorNode:
			FillOrReduceFieldSelection (node,node->node_symbol->symb_def,asp_p,bsp_p,NULL,code_gen_node_ids_p);
			break;
		case UpdateNode:
			FillUpdateNode (node,asp_p,bsp_p,NULL,code_gen_node_ids_p);
			break;
		case MatchNode:
			FillMatchNode (node,asp_p,bsp_p,NULL,code_gen_node_ids_p);
			break;
#ifdef REUSE_UNIQUE_NODES
		case FillUniqueNode:
			FillUniqueNodeWithNode (node,asp_p,bsp_p,code_gen_node_ids_p);
			break;
#endif
		default:
			error_in_function ("Build");
	}
}

void build_and_cleanup (Node node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	SymbDef sdef;
		
	sdef=NULL;

	if (node->node_kind==NormalNode){
		switch (node->node_symbol->symb_kind){
			case definition:
				sdef=node->node_symbol->symb_def;
				break;
			case apply_symb:
#ifdef NEW_APPLY
				if (!IsLazyState (node->node_state))
					generate_code_for_apply_in_strict_context (node,asp_p,bsp_p,code_gen_node_ids_p);
				else
					Build (node,asp_p,bsp_p,code_gen_node_ids_p);
				return;
#else
				sdef=ApplyDef;
				break;
#endif
#ifndef FASTER_STRICT_IF
			case if_symb:
				sdef=IfDef;
				break;
#endif
		}
	}
	
	if (sdef!=NULL){
		int sdef_kind;

		sdef_kind=sdef->sdef_kind;

		if ((sdef_kind==IMPRULE || sdef_kind==DEFRULE || sdef_kind==SYSRULE) &&
			sdef->sdef_arity==node->node_arity && !IsLazyState (node->node_state))
		{
			LabDef name;
			int a_size,b_size;
			ArgP node_args;

			ConvertSymbolToLabel (&name,sdef);

			node_args=node->node_arguments;
			DetermineSizeOfArguments (node_args,&a_size,&b_size);

			if (ExpectsResultNode (node->node_state))
				BuildArgsWithNewResultNode (node_args,asp_p,bsp_p,code_gen_node_ids_p,&a_size,&b_size);
			else
				BuildArgs (node_args,asp_p,bsp_p,code_gen_node_ids_p);

			*asp_p-=a_size;
			*bsp_p-=b_size;

			cleanup_stack (asp_p,bsp_p,a_size,b_size,&code_gen_node_ids_p->a_node_ids,&code_gen_node_ids_p->b_node_ids,
							&code_gen_node_ids_p->free_node_ids,code_gen_node_ids_p->moved_node_ids_l,
							code_gen_node_ids_p->doesnt_fail);
			
			CallFunction (&name,sdef,True,node);

			AddSizeOfState (node->node_state,asp_p,bsp_p);

			return;
		}
	}

	Build (node,asp_p,bsp_p,code_gen_node_ids_p);
}

void BuildArg (Args arg,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	NodeP node;
	int asize,bsize;
	
	ArgComment (arg);
	
	node=arg->arg_node;
	
	if (node->node_kind!=NodeIdNode){
		Build (node,asp_p,bsp_p,code_gen_node_ids_p);
		DetermineSizeOfState (node->node_state, &asize, &bsize);
		CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,node->node_state,asize,bsize);
	} else {
		NodeId arg_node_id;
		
		arg_node_id=node->node_node_id;
#if BOXED_RECORDS
		arg_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
#endif
		if (CopyNodeIdArgument (arg->arg_state,arg_node_id,asp_p,bsp_p))
			ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
		
		decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
	}
}

static Bool LazyStates (StateS states[],int n_states)
{
	int n;
	
	for (n=0; n<n_states; ++n)
		if (!IsLazyState (states[n]))
			return False;
			
	return True;
}

static Bool PushArgumentLater (StateS demstate,StateS offstate)
{
	if (demstate.state_type==SimpleState && demstate.state_kind==Undefined)
		return False;

	if (offstate.state_type==SimpleState){
		Coercions c;
		StateKind offkind;
		
		offkind = offstate.state_kind;

		if (demstate.state_type==SimpleState){
			c = CoerceStateKind (demstate.state_kind, offkind);

			if (c==Reduce || c==MayBecomeCyclicSpine || c==CyclicSpine)
				return False;
			else
				return True;
		} else {
			c = CoerceStateKind (StrictOnA, offkind);

			if (c==Reduce || c==MayBecomeCyclicSpine || c==CyclicSpine)
				return False;

			switch (demstate.state_type){
				case TupleState:
					return LazyStates (demstate.state_tuple_arguments,demstate.state_arity);
				case RecordState:
					return LazyStates (demstate.state_record_arguments,demstate.state_arity);
				case ArrayState:
					return True;
			}
		}
	} else if (demstate.state_type==SimpleState){
		switch (offstate.state_type){
			case TupleState:
				/*
				BuildTuple (aindex,bindex,*asp_p,*bsp_p,offstate.state_arity,offstate.state_tuple_arguments,
							offasize,offbsize,*asp_p,NormalFill,newnode);
				*/
				return False;
			case RecordState:
				/*
				BuildRecord (offstate.state_record_symbol,aindex,bindex,*asp_p,*bsp_p,offasize,offbsize,*asp_p,NormalFill,newnode);
				*/
				return False;
			case ArrayState:
				return True;
		}
	} else {
		if (offstate.state_type!=demstate.state_type)
			return False;
		
		switch (offstate.state_type){
			case TupleState:
				{
					int n,n_states;
					
					n_states=demstate.state_arity;
					
					for (n=0; n<n_states; ++n)
						if (!PushArgumentLater (demstate.state_tuple_arguments[n],offstate.state_tuple_arguments[n]))
							return False;
				}
				return True;
			case RecordState:
				{
					int n,n_states;
					
					n_states=demstate.state_arity;
					
					for (n=0; n<n_states; ++n)
						if (!PushArgumentLater (demstate.state_record_arguments[n],offstate.state_record_arguments[n]))
							return False;
				}
				return True;
			case ArrayState:
				return True;
		}
	}
	return False;
}

static Bool BuildNonParArgs (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	Bool parallel;
	
	if (args==NULL)
		return False;
	
	parallel = BuildNonParArgs (args->arg_next,asp_p,bsp_p,code_gen_node_ids_p);

	if (args->arg_state.state_mark & STATE_PARALLEL_MASK)
		return True;
		
	{
		Node node;
		int asize,bsize;
		
		node=args->arg_node;
		
		if (node->node_kind!=NodeIdNode){
			if (node->node_kind==NormalNode){
				switch (node->node_symbol->symb_kind){
					case int_denot:
					case bool_denot:
					case char_denot:
					case real_denot:
					case string_denot:
						args->arg_state.state_mark |= STATE_PARALLEL_MASK;
						return True;						
				}
			}

			ArgComment (args);

			Build (node,asp_p,bsp_p,code_gen_node_ids_p);
			DetermineSizeOfState (node->node_state, &asize, &bsize);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,args->arg_state, node->node_state, asize, bsize);
		} else {
			NodeId arg_node_id;
			
			arg_node_id=node->node_node_id;

			if (PushArgumentLater (args->arg_state,arg_node_id->nid_state)){
				args->arg_state.state_mark |= STATE_PARALLEL_MASK;
				return True;
			} else {
				ArgComment (args);
				
#if BOXED_RECORDS
				arg_node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
#endif
				if (CopyNodeIdArgument (args->arg_state,arg_node_id,asp_p,bsp_p))
					ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);
				
				decrement_reference_count_of_node_id (arg_node_id,&code_gen_node_ids_p->free_node_ids);
			}
		}
	}

	return parallel;
}

#if 0
	static void PutArgInFrames (int *anext,int *bnext,int asp,int bsp,StateS state,int asize,int bsize)
	{
		if (IsSimpleState (state)){
			if (state.state_kind == OnB)
				PutInBFrames (bsp, bnext, bsize);
			else if (state.state_kind != Undefined)
				PutInAFrames (asp, anext);
		} else {
			int i, arity;
			
			arity = state.state_arity;
			
			switch (state.state_type){
				case TupleState:
				{	States argstates = state.state_tuple_arguments;
					asp -= asize;
					bsp -= bsize;
					for (i=arity-1; i>=0; i--){	
						DetermineSizeOfState (argstates [i],&asize, &bsize);
						asp += asize;
						bsp += bsize;
						PutArgInFrames (anext, bnext, asp, bsp, argstates [i], asize, bsize);
					}
					break;
				}
				case RecordState:
				{	States argstates = state.state_record_arguments;
					asp -= asize;
					bsp -= bsize;
					for (i=arity-1; i>=0; i--){
						DetermineSizeOfState (argstates [i],&asize, &bsize);
						asp += asize;
						bsp += bsize;
						PutArgInFrames (anext, bnext, asp, bsp, argstates [i], asize, bsize);
					}
					break;
				}
				case ArrayState:
					PutInAFrames (asp, anext);
					break;
			}
		}
	}
#endif

static void PutParAndNormalArgsInFrames (Args args,int *npar_a_offset_p,int *npar_b_offset_p,int *par_a_offset_p,int *par_b_offset_p,int *aind_p,int *bind_p)
{	
	if (args!=NULL){
		int asize,bsize;
		
		PutParAndNormalArgsInFrames (args->arg_next,npar_a_offset_p,npar_b_offset_p,par_a_offset_p,par_b_offset_p,aind_p,bind_p);

		DetermineSizeOfState (args->arg_state,&asize,&bsize);
			
		if (args->arg_state.state_mark & STATE_PARALLEL_MASK){
			if (bsize!=0){
				*par_b_offset_p+=bsize;
				PutInBFrames (*par_b_offset_p,bind_p,bsize);
			}
			while (asize!=0){
				++*par_a_offset_p;
				PutInAFrames (*par_a_offset_p,aind_p);
				--asize;
			}
		} else {
			if (bsize!=0){
				*npar_b_offset_p+=bsize;
				PutInBFrames (*npar_b_offset_p,bind_p,bsize);
			}
			while (asize!=0){
				++*npar_a_offset_p;
				PutInAFrames (*npar_a_offset_p,aind_p);
				--asize;
			}
		}
	}
}

static void ReorderParallelAndNonParallelArgsWithResultNode (Args args,int *asize_p,int *bsize_p)
{
	int par_a_size,par_b_size;
	int npar_a_size,npar_b_size;
	int asize,bsize;
	int oldamax,oldbmax;
	int aind,bind;
	ArgS *arg;
	
	par_a_size=1;
	par_b_size=0;
	npar_a_size=0;
	npar_b_size=0;

	for_l (arg,args,arg_next)
		if (arg->arg_state.state_mark & STATE_PARALLEL_MASK)
			AddSizeOfState (arg->arg_state,&par_a_size,&par_b_size);
		else
			AddSizeOfState (arg->arg_state,&npar_a_size,&npar_b_size);
	
	asize = par_a_size+npar_a_size;
	bsize = par_b_size+npar_b_size;
	
	*asize_p=asize;
	*bsize_p=bsize;

	if ((par_a_size==0 || npar_a_size==0) && (par_b_size==0 || npar_b_size==0))
		return;

	InitStackConversions (asize+2,bsize+2,&oldamax,&oldbmax);

	aind = 0;
	bind = 0;
	{
		int npar_a_offset,npar_b_offset,par_a_offset,par_b_offset;

		npar_a_offset=0;
		npar_b_offset=0;
		par_a_offset=npar_a_size;
		par_b_offset=npar_b_size;
		
		par_a_offset+=1;
		PutInAFrames (par_a_offset,&aind);

		PutParAndNormalArgsInFrames (args,&npar_a_offset,&npar_b_offset,&par_a_offset,&par_b_offset,&aind,&bind);
	}

	GenAStackConversions (asize,aind);
	GenBStackConversions (bsize,bind);

	ExitStackConversions (oldamax,oldbmax);
}

static void ReorderParallelAndNonParallelArgs (Args args)
{
	int par_a_size,par_b_size;
	int npar_a_size,npar_b_size;
	int asize,bsize;
	int oldamax,oldbmax;
	int aind,bind;
	ArgS *arg;
	
	par_a_size=0;
	par_b_size=0;
	npar_a_size=0;
	npar_b_size=0;

	for_l (arg,args,arg_next)
		if (arg->arg_state.state_mark & STATE_PARALLEL_MASK)
			AddSizeOfState (arg->arg_state,&par_a_size,&par_b_size);
		else
			AddSizeOfState (arg->arg_state,&npar_a_size,&npar_b_size);

	if ((par_a_size==0 || npar_a_size==0) && (par_b_size==0 || npar_b_size==0))
		return;
	
	asize = par_a_size+npar_a_size;
	bsize = par_b_size+npar_b_size;

	InitStackConversions (asize+2,bsize+2,&oldamax,&oldbmax);

	aind = 0;
	bind = 0;
	{
		int npar_a_offset,npar_b_offset,par_a_offset,par_b_offset;

		npar_a_offset=0;
		npar_b_offset=0;
		par_a_offset=npar_a_size;
		par_b_offset=npar_b_size;
		PutParAndNormalArgsInFrames (args,&npar_a_offset,&npar_b_offset,&par_a_offset,&par_b_offset,&aind,&bind);
	}

	GenAStackConversions (asize,aind);
	GenBStackConversions (bsize,bind);

	ExitStackConversions (oldamax,oldbmax);
}

static void BuildParArgs (ArgS* args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{	
	if (args==NULL)
		return;

	BuildParArgs (args->arg_next,asp_p,bsp_p,code_gen_node_ids_p);
				
	if (args->arg_state.state_mark & STATE_PARALLEL_MASK){
/*		ParComment (args); */
		BuildArg (args,asp_p,bsp_p,code_gen_node_ids_p);
	}
}

void BuildArgs (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	if (BuildNonParArgs (args,asp_p,bsp_p,code_gen_node_ids_p)){
		BuildParArgs (args,asp_p,bsp_p,code_gen_node_ids_p);
		ReorderParallelAndNonParallelArgs (args);
	}
}

void BuildLazyArgs (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	if (args==NULL)
		return;
	
	BuildLazyArgs (args->arg_next,asp_p,bsp_p,code_gen_node_ids_p);
		
	BuildArg (args,asp_p,bsp_p,code_gen_node_ids_p);
}

static void CreateCyclicExternalReducers (NodeDefs nds,int node_id_number,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	for (; nds && nds->def_id->nid_number==node_id_number; nds=nds->def_next){
		if (nds->def_node && (nds->def_id->nid_mark & ON_A_CYCLE_MASK) && HasExternalAnnot (nds->def_node)){
			NewEmptyNode (asp_p,-1);
		
			/* fill cycle and start reducer */
		
			FillNodeOnACycle (nds->def_node,asp_p,bsp_p,nds->def_id,code_gen_node_ids_p);
				
			CreateParallelCode (nds,asp_p,bsp_p,code_gen_node_ids_p);
		
			ChangeEvalStatusKind (nds->def_id,OnA);
		}
	}
}

#if OPTIMIZE_LAZY_TUPLE_RECURSION
extern NodeP tuple_result_p;

static void generate_code_for_lazy_tuple_recursive_call (NodeP node,NodeIdP node_id_p,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	unsigned long result_and_call_same_select_vector;
	NodeIdP first_tuple_call_node_id_p;
	int n,arity,tuple_arity;
	LabDef name,codelab;
	NodeP fill_nodes;
	SymbDef sdef;
	
	fill_nodes=node;
	while (node->node_kind==FillUniqueNode)
		node=node->node_arguments->arg_node;
	
	result_and_call_same_select_vector=0;
	first_tuple_call_node_id_p=NULL;

	if (lazy_tuple_recursion){
		ArgP tuple_element_p;

		for_li (tuple_element_p,n,tuple_result_p->node_arguments,arg_next){
			NodeP node_p;
			
			node_p=tuple_element_p->arg_node;
			
			if (node_p->node_symbol->symb_kind==select_symb
				&& node_p->node_arguments->arg_node->node_kind==NodeIdNode
				&& n+1==node_p->node_arity
				&& (node_p->node_arguments->arg_node->node_node_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY)
			){
				NodeIdP tuple_call_node_id_p;
				
				tuple_call_node_id_p=node_p->node_arguments->arg_node->node_node_id;
				if (first_tuple_call_node_id_p==NULL)
					first_tuple_call_node_id_p=tuple_call_node_id_p;
				
				if (tuple_call_node_id_p==node_id_p)
					result_and_call_same_select_vector |= (1<<n);
			}
		}
	}
	
	tuple_arity=node->node_symbol->symb_def->sdef_rule->rule_type->type_alt_rhs->type_node_arity;
	if (lazy_tuple_recursion){
		for (n=tuple_arity-1; n>=0; --n){
			if (result_and_call_same_select_vector & (1<<n))
				GenPushA (*asp_p - (tuple_arity-n));
			else {
				if (fill_nodes!=node){
					NodeIdP free_unique_node_id;
					
					free_unique_node_id=fill_nodes->node_node->node_arguments->arg_node->node_node_id;
					GenPushA (*asp_p-free_unique_node_id->nid_a_index);
					
					decrement_reference_count_of_node_id (free_unique_node_id,&code_gen_node_ids_p->free_node_ids);

					fill_nodes=fill_nodes->node_arguments->arg_node;
				} else
					GenCreate (-1);
			}
			++*asp_p;
		}
#if ! SELECTORS_FIRST
		{
		int offset;
		
		offset=tuple_arity-1;
		for (n=tuple_arity-1; n>=0; --n){
			if (result_and_call_same_select_vector & (1<<n)){
				--offset;
			} else {
				GenPushA (offset);
				++*asp_p;
			}
		}
		}
#endif
	} else {
		for (n=tuple_arity-1; n>=0; --n){
			GenCreate (-1);
			++*asp_p;
		}
#if ! SELECTORS_FIRST
		for (n=tuple_arity-1; n>=0; --n){
			GenPushA (tuple_arity-1);
			++*asp_p;
		}
#endif
	}


	arity=node->node_arity;
	
	if (node->node_kind!=NormalNode || node->node_symbol->symb_kind!=definition || node->node_symbol->symb_def->sdef_kind!=IMPRULE
		|| arity!=node->node_symbol->symb_def->sdef_arity || !IsLazyState (node->node_state))
		error_in_function ("generate_code_for_lazy_tuple_recursive_call");

	sdef=node->node_symbol->symb_def;

	ConvertSymbolToDandNLabel (&name,&codelab,sdef);

	codelab.lab_post=2;
	name.lab_post=2;

	BuildArgs (node->node_arguments,asp_p,bsp_p,code_gen_node_ids_p);
	
	if (!lazy_tuple_recursion || first_tuple_call_node_id_p!=node_id_p){
		if (node->node_symbol->symb_def->sdef_rule->rule_mark & RULE_UNBOXED_LAZY_CALL){
			int a_size,b_size;
			
			DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);

#  if SELECTORS_FIRST
			for (n=tuple_arity-1; n>=0; --n){
				GenPushA (a_size+tuple_arity-1);
				++*asp_p;
			}
#  endif

			if (b_size!=0)
				GenBuildU (&name,a_size+tuple_arity,b_size,&codelab);
			else
				GenBuild (&name,arity+tuple_arity,&codelab);

			*bsp_p -= b_size;
			*asp_p += 1-(a_size+tuple_arity);
		} else {
# if SELECTORS_FIRST
			for (n=tuple_arity-1; n>=0; --n){
				GenPushA (arity+tuple_arity-1);
				++*asp_p;
			}
# endif
			GenBuild (&name,arity+tuple_arity,&codelab);		
			*asp_p += 1-(arity+tuple_arity);
		}
	} else {
		char bits[MaxNodeArity+2],*bits_p;
		int n,n_updated_tuple_elements;

		bits_p=bits;
# if SELECTORS_FIRST
		*bits_p++='1';
# else
		*bits_p++='0';
# endif
		n_updated_tuple_elements=0;
		
# if SELECTORS_FIRST
		for (n=0; n<tuple_arity; ++n)
			if (result_and_call_same_select_vector & (1<<n)){
				*bits_p++ = '0';
			} else {
				*bits_p++ = '1';
				++n_updated_tuple_elements;
			}
# endif

		if (node->node_symbol->symb_def->sdef_rule->rule_mark & RULE_UNBOXED_LAZY_CALL){
			int a_size,b_size;
			
			DetermineSizeOfArguments (node->node_arguments,&a_size,&b_size);
#  if SELECTORS_FIRST
			{
			int offset;
			
			offset=tuple_arity-1;
			for (n=tuple_arity-1; n>=0; --n){
				if (result_and_call_same_select_vector & (1<<n)){
					--offset;
				} else {
					GenPushA (a_size+offset);
					++*asp_p;
				}
			}
			}
#  endif

			for (n=0; n<a_size; ++n)
				*bits_p++ = '1';

#  if !SELECTORS_FIRST
			for (n=0; n<tuple_arity; ++n)
				if (result_and_call_same_select_vector & (1<<n)){
					*bits_p++ = '0';
				} else {
					*bits_p++ = '1';
					++n_updated_tuple_elements;
				}
#  endif
						
			for (n=0; n<b_size; ++n)
				*bits_p++ = '1';

			*bits_p++='\0';
			
			if (b_size!=0)
				GenFillcpU (&name,a_size+tuple_arity,b_size,&codelab,*asp_p,bits);
			else
				GenFillcp (&name,a_size+tuple_arity,&codelab,*asp_p,bits);
			
			*asp_p -= a_size+n_updated_tuple_elements;
			*bsp_p -= b_size;
		} else {
#  if SELECTORS_FIRST
			{
			int offset;
			
			offset=tuple_arity-1;
			for (n=tuple_arity-1; n>=0; --n){
				if (result_and_call_same_select_vector & (1<<n)){
					--offset;
				} else {
					GenPushA (arity+offset);
					++*asp_p;
				}
			}
			}
#  endif

		for (n=0; n<arity; ++n)
			*bits_p++ = '1';

# if !SELECTORS_FIRST		
		for (n=0; n<tuple_arity; ++n)
			if (result_and_call_same_select_vector & (1<<n)){
				*bits_p++ = '0';
			} else {
				*bits_p++ = '1';
				++n_updated_tuple_elements;
			}
# endif					
		*bits_p++='\0';
		
		GenFillcp (&name,arity+tuple_arity,&codelab,*asp_p,bits);
		*asp_p -= arity+n_updated_tuple_elements;
		}		

		GenPushA (*asp_p);
		++*asp_p;
	}

	{
		int offset;
		
		offset=1;
		for (n=0; n<tuple_arity; ++n){
			if (!lazy_tuple_recursion || !(result_and_call_same_select_vector & (1<<n))){
				LabDef sellab,nsellab;
				
				MakeLabel (&nsellab,"_Sel",0,n_pref);
				
				sellab			= nsellab;
				sellab.lab_pref	= d_pref;

				GenPushA (0);
				GenFill (&sellab,1,&nsellab,offset+1,NormalFill);
			}
			++offset;
		}
	}
}
#endif

static int FillNodeDefs (NodeDefs nds,int node_id_number,int *asp_p,int *bsp_p,NodeDefs *rest,CodeGenNodeIdsP code_gen_node_ids_p)
{
	int hasCyclicExternalReducer;
	
	hasCyclicExternalReducer=0;
	
	for (; nds!=NULL && nds->def_id->nid_number==node_id_number; nds=nds->def_next){
		Node node;

		node=nds->def_node;

		if (node==NULL){
			NodeId node_id;
			
			node_id=nds->def_id;
			
			/* we have a strict annotated left hand side nodeid */
			StrictIdComment (node_id);
			
			/* evaluate strict arg */
			if (node_id->nid_state.state_type==SimpleState)
				ReduceArgumentToHnf (node_id,node_id->nid_state,*asp_p-node_id->nid_a_index,code_gen_node_ids_p->saved_nid_state_l);
		} else {
			struct state *result_state_p;

			result_state_p=&node->node_state;

			if (nds->def_id->nid_mark & ON_A_CYCLE_MASK){
				if (HasExternalAnnot (node)){
					hasCyclicExternalReducer=1;
					continue;
				}

				/* fill cycle */

				FillNodeOnACycle (node,asp_p,bsp_p,nds->def_id,code_gen_node_ids_p);
			} else {
				int a_size,b_size;
				
				NodeDefComment (nds, "shared or annotated");

#if OPTIMIZE_LAZY_TUPLE_RECURSION
				if (nds->def_id->nid_mark2 & NID_CALL_VIA_LAZY_SELECTIONS_ONLY)
					generate_code_for_lazy_tuple_recursive_call (node,nds->def_id,asp_p,bsp_p,code_gen_node_ids_p);
				else
#endif
				if (node->node_kind==TupleSelectorsNode){
					struct arg *arg;
					struct node *tuple_node;
					struct state *tuple_state_p;
					
					tuple_node=node->node_node;

					if (tuple_node->node_kind!=NodeIdNode){
						build_and_cleanup (tuple_node,asp_p,bsp_p,code_gen_node_ids_p);
						tuple_state_p=&tuple_node->node_state;
					} else {
						NodeId node_id;
						
						node_id=tuple_node->node_node_id;
#if BOXED_RECORDS
						node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
#endif
						if (CopyNodeIdArgument (tuple_node->node_arguments->arg_state,node_id,asp_p,bsp_p))
							ChangeEvalStatusKindToStrictOnA (node_id,code_gen_node_ids_p->saved_nid_state_l);
						
						tuple_state_p=&tuple_node->node_arguments->arg_state;

						decrement_reference_count_of_node_id (node_id,&code_gen_node_ids_p->free_node_ids);
					}

					/* added 22-10-2001 */
					result_state_p=tuple_state_p;
					/* */
					
					arg=node->node_arguments;				
					if (arg->arg_node->node_kind==NodeIdNode){
						int a_offset,b_offset,i;
						
						DetermineSizeOfState (*tuple_state_p,&a_offset,&b_offset);

						if (tuple_state_p->state_type!=TupleState)
							error_in_function ("FillNodeDefs");

						for (i=tuple_state_p->state_arity-1; i>=0; --i){
							int a_size,b_size;
							NodeId node_id;	

							DetermineSizeOfState (tuple_state_p->state_tuple_arguments[i],&a_size,&b_size);

							a_offset-=a_size;
							b_offset-=b_size;

							if (arg!=NULL && arg->arg_node->node_node_id->nid_number==i){
								node_id=arg->arg_node->node_node_id;
								arg=arg->arg_next;
							} else {
								if (a_size==0 && b_size==0)
									continue;

								node_id=NewNodeId (NULL);								
								add_node_id_to_list (node_id,&code_gen_node_ids_p->free_node_ids);
							}
														
							node_id->nid_a_index_=*asp_p - a_offset;
							node_id->nid_b_index_=*bsp_p - b_offset;		
							node_id->nid_state_ = tuple_state_p->state_tuple_arguments[i];

							if (a_size!=0)
								add_node_id_to_list (node_id,&code_gen_node_ids_p->a_node_ids);
							if (b_size!=0)
								add_node_id_to_list (node_id,&code_gen_node_ids_p->b_node_ids);
						}
						
						if (arg!=NULL)
							error_in_function ("FillNodeDefs");
						
						continue;
					}				
				} else if (node->node_kind==NodeIdNode){
					NodeId node_id;
					
					node_id=node->node_node_id;
					
					if ((node_id->nid_mark & NID_SHARED_SELECTION_NODE_ID)==0 && EqualState (node->node_arguments->arg_state,node->node_state)){
						int a_size,b_size;
						
						nds->def_id->nid_a_index_=node_id->nid_a_index;
						nds->def_id->nid_b_index_=node_id->nid_b_index;
						nds->def_id->nid_state_=node_id->nid_state;

						DetermineSizeOfState (node_id->nid_state,&a_size,&b_size);

						if (a_size!=0){
							NodeIdListElementP p_node_id;
							
							for_l (p_node_id,code_gen_node_ids_p->a_node_ids,nidl_next)
								if (p_node_id->nidl_node_id==node_id){
									p_node_id->nidl_node_id=nds->def_id;
									break;
								}						
						}

						if (b_size!=0){
							NodeIdListElementP p_node_id;
							
							for_l (p_node_id,code_gen_node_ids_p->a_node_ids,nidl_next)
								if (p_node_id->nidl_node_id==node_id){
									p_node_id->nidl_node_id=nds->def_id;
									break;
								}						
						}
						
						continue;
					} else {
						result_state_p=&node->node_arguments->arg_state;

#ifdef DO_LAZY_SELECTORS_FROM_BOXED_STRICT_RECORDS
						if (result_state_p->state_type==SimpleState && result_state_p->state_kind==OnA && !ResultIsNotInRootNormalForm (node_id->nid_state))
							result_state_p->state_kind=StrictOnA;
#endif					
#if BOXED_RECORDS
						if (node_id->nid_refcount>1)
							node_id->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
#endif
						if (CopyNodeIdArgument (*result_state_p,node_id,asp_p,bsp_p))
							ChangeEvalStatusKindToStrictOnA (node_id,code_gen_node_ids_p->saved_nid_state_l);
# if 0
						printf ("decrement_reference_count_of_node_id %d\n",node_id->nid_refcount);
# endif										
						decrement_reference_count_of_node_id (node_id,&code_gen_node_ids_p->free_node_ids);
					}
				} else
					build_and_cleanup (node,asp_p,bsp_p,code_gen_node_ids_p);

				/* IsLazyState (nds->def_node->node_state) ? build shared or annotated : build and reduce */

				DetermineSizeOfState (*result_state_p,&a_size,&b_size);

				if (a_size!=0)
					add_node_id_to_list (nds->def_id,&code_gen_node_ids_p->a_node_ids);

				if (b_size!=0)
					add_node_id_to_list (nds->def_id,&code_gen_node_ids_p->b_node_ids);

				nds->def_id->nid_a_index_=*asp_p;
				nds->def_id->nid_b_index_=*bsp_p;
			}
			
			/* start reducer, and (if a node is filled) set eval status */
			if (IsSimpleState (*result_state_p) && result_state_p->state_kind==Parallel){
				if (!((nds->def_id->nid_mark & ON_A_CYCLE_MASK) && HasExternalAnnot (node))){
					CreateParallelCode (nds,asp_p,bsp_p,code_gen_node_ids_p);
					/* start reducer */
					ChangeEvalStatusKind (nds->def_id, OnA);
				}
			} else
				nds->def_id->nid_state_=*result_state_p;
		}
	}
	
	*rest = nds;

	return hasCyclicExternalReducer;
}

Bool NodeOnACycleIsInRootNormalForm (Node node)
{
	Symbol symb;
	
	symb=node->node_symbol;

	switch (symb->symb_kind){
		case select_symb:
		case apply_symb:
			return False;
		case if_symb:
			return (node->node_arity!=3);
		case definition:
		{
			SymbDef sdef;
			
			sdef=symb->symb_def;
	
			if (node->node_kind!=NormalNode)
				return False;
	
			if (sdef->sdef_kind==RECORDTYPE)
				if (!sdef->sdef_strict_constructor)
					return True;
				else
					return False;
			
			if (sdef->sdef_arity==node->node_arity)
				switch (sdef->sdef_kind){
					case CONSTRUCTOR:
						if (sdef->sdef_strict_constructor)
							return False;
					case DEFRULE:
					case SYSRULE:
					case IMPRULE:
						return False;
					default:
						return True;
				}

			return True;
		}
		default:
			return True;
	}
}

static void CreateCycleNodesAndChannels (NodeDefs nds,NodeDefs rootdef,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	for (; nds!=NULL; nds=nds->def_next){
		if (! nds->def_node || nds==rootdef){
			/* we have a strict annotated left hand side nodeid, or a root (with a node) */
			continue;
		} else if (nds->def_id->nid_mark & ON_A_CYCLE_MASK){
			if (HasExternalAnnot (nds->def_node)){
				NodeDefComment (nds, "Cycle containing a channel");
				GenProcIdCalculation (nds->def_node,nds->def_node->node_annotation,asp_p,bsp_p,code_gen_node_ids_p);
				GenCreateChannel (channel_code);
				--*bsp_p;
				nds->def_id->nid_state_=nds->def_node->node_state;
			} else {
				NodeDefComment (nds, "OnACycle");
				if (NodeOnACycleIsInRootNormalForm (nds->def_node))
					GenCreate (-1);
				else
					GenCreate (nds->def_node->node_arity);
				nds->def_id->nid_state_=OnAState;
			}
			++*asp_p;
			nds->def_id->nid_a_index_=*asp_p;
		} else
			nds->def_id->nid_state_=UnderEvalState;
	}
}

void CodeSharedNodeDefs (NodeDefs nds, NodeDefs rootdef,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p)
{
	NodeDefs rest,new_rest;
	
	CreateCycleNodesAndChannels (nds, rootdef,asp_p,bsp_p,code_gen_node_ids_p);

	for (rest=nds; rest!=NULL; rest=new_rest){
		int hasCyclicExternalReducer;
		
		hasCyclicExternalReducer=FillNodeDefs (rest,rest->def_id->nid_number,asp_p,bsp_p,&new_rest,code_gen_node_ids_p);
		
		if (hasCyclicExternalReducer)
			CreateCyclicExternalReducers (rest, rest->def_id->nid_number,asp_p,bsp_p,code_gen_node_ids_p);
	}

	ReduceSemiStrictNodes (nds,*asp_p);
}

#if 0
	static void BuildStackFrameEntry (Args arg,int *asp_p,int *bsp_p,int *a_ind,int *b_ind,CodeGenNodeIdsP code_gen_node_ids_p)
	{
		int asize, bsize;
		Node pattern;
		
		pattern=arg->arg_node;
		
		if (pattern->node_kind!=NodeIdNode){
			Build (pattern,asp_p,bsp_p,code_gen_node_ids_p);
		
			DetermineSizeOfState (pattern->node_state, &asize, &bsize);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,arg->arg_state,pattern->node_state,asize,bsize);
			
			DetermineSizeOfState (arg->arg_state,&asize,&bsize);
			PutArgInFrames (a_ind,b_ind,*asp_p,*bsp_p,arg->arg_state,asize,bsize);
		} else {
			StateS offstate;
			int aindex;
			NodeId arg_node_id;
			
			arg_node_id=pattern->node_node_id;
			offstate = arg_node_id->nid_state;
			aindex = arg_node_id->nid_a_index;

			if (IsSimpleState (offstate)){
				Bool leftontop;
				Coercions c;

				c = CoerceSimpleStateArgument (arg->arg_state,offstate.state_kind,aindex,asp_p,False, &leftontop);
				offstate.state_kind = AdjustStateKind (offstate.state_kind, c);

				if (HasBeenReduced (c))
					ChangeEvalStatusKindToStrictOnA (arg_node_id,code_gen_node_ids_p->saved_nid_state_l);

				if (leftontop)
					aindex = *asp_p;
			}

			DetermineSizeOfState (offstate, &asize, &bsize);

			CoerceArgumentUsingStackFrames (arg->arg_state,offstate,aindex,arg_node_id->nid_b_index,asp_p,bsp_p,a_ind, b_ind, asize, bsize);
		}
	}

	static Bool BuildStackFrameEntries (Args args,int *asp_p,int *bsp_p,int *a_ind,int *b_ind,CodeGenNodeIdsP code_gen_node_ids_p)
	{
		int parallel;
		
		parallel = False;

		if (args){
			if (BuildStackFrameEntries (args->arg_next,asp_p,bsp_p, a_ind, b_ind,code_gen_node_ids_p))
				parallel = True;
			if (args->arg_state.state_parallel)
				parallel = True;
			else
				BuildStackFrameEntry (args,asp_p,bsp_p, a_ind, b_ind,code_gen_node_ids_p);
		}	
		return parallel;
	}

	static void BuildParallelStackFrameEntries (Args args,int *asp_p,int *bsp_p,int *a_ind,int *b_ind,CodeGenNodeIdsP code_gen_node_ids_p)
	{	
		if (args){
			BuildParallelStackFrameEntries (args->arg_next,asp_p,bsp_p, a_ind, b_ind,code_gen_node_ids_p);

			if (args->arg_state.state_parallel){
				ParComment (args);
				BuildStackFrameEntry (args, asp_p,bsp_p, a_ind, b_ind,code_gen_node_ids_p);
			}
		}	
	}

	static void CopyToNewFrame (int *demframe, int *newdemframe, int size, int *sp1, int *sp2)
	{
		int i, j, k;

		for (i = 0, j = *sp1, k = *sp2; i < size; i++, j--, k--)
			newdemframe[k] = demframe[j];

		*sp1 -= size;
		*sp2 -= size;
	}

	static void AdjustDemandedFrames (Args args)
	{
		int *newdemaframe, *newdembframe;
		int asp, parasp, newasp, bsp, parbsp, newbsp, asize, bsize, parasize, parbsize;
		Args arg;
		
		/* determine sizes of (non) parallel part */
		asize = bsize = parasize = parbsize = 0;

		for (arg = args; arg; arg = arg->arg_next){
			if (arg->arg_state.state_parallel)
				AddSizeOfState (arg->arg_state, &parasize, &parbsize);
			else
				AddSizeOfState (arg->arg_state, &asize, &bsize);
		}
		
		if (parasize == 0 && parbsize == 0)
			return;

		/* allocate space for temporary stack frames */
		newdemaframe = AllocTempDemandedAFrame (CurrentAFrameSize);
		newdembframe = AllocTempDemandedBFrame (CurrentBFrameSize);
		
		/* copy the arguments to the temporary frames */
		parasp = newasp = asize + parasize;
		parbsp = newbsp = bsize + parbsize;
		asp    = asize;
		bsp    = bsize;
		
		for_l (arg,args,arg_next){
			int asize,bsize;
		
			DetermineSizeOfState (arg->arg_state, &asize, &bsize);
		
			if (arg->arg_state.state_parallel){
				CopyToNewFrame (DemandedAFrame, newdemaframe, asize, &parasp, &newasp);
				CopyToNewFrame (DemandedBFrame, newdembframe, bsize, &parbsp, &newbsp);
			} else {
				CopyToNewFrame (DemandedAFrame, newdemaframe, asize, &asp, &newasp);
				CopyToNewFrame (DemandedBFrame, newdembframe, bsize, &bsp, &newbsp);
			}
		}

		/* copy the new frame */
		for (asp = 1; asp <= asize + parasize; asp++)
			DemandedAFrame[asp] = newdemaframe[asp];
		for (bsp = 1; bsp <= bsize + parbsize; bsp++)
			DemandedBFrame[bsp] = newdembframe[bsp];
	}

	static void BuildNewStackFrame (ArgS *args,int asp,int bsp,Bool result_node_necessary,CodeGenNodeIdsP code_gen_node_ids_p)
	{
		int a_ind, b_ind, oldamax, oldbmax, newamax, newbmax, dummy;
		Args arg;
		
		a_ind = 0;
		b_ind = 0;
		dummy=0;
		
		newamax = asp + 2;
		newbmax = bsp + 2;

		for_l (arg,args,arg_next)
			AddStateSizeAndMaxFrameSize (arg->arg_state,& newamax, & dummy, & newbmax);

		InitStackConversions  (newamax, newbmax, &oldamax, &oldbmax);

		if (result_node_necessary){
			NewEmptyNode (&asp, -1);
			PutInAFrames (asp, &a_ind);
		}

		TypeErrorFound = False;
		CycleErrorFound = False;
		
		if (BuildStackFrameEntries (args, &asp, &bsp,&a_ind, &b_ind,code_gen_node_ids_p)){
			BuildParallelStackFrameEntries (args, &asp, &bsp,&a_ind, &b_ind,code_gen_node_ids_p);
			AdjustDemandedFrames (args);
		}

		if (! (TypeErrorFound || CycleErrorFound)){
			GenAStackConversions (asp,a_ind);
			GenBStackConversions (bsp,b_ind);
		}

		ExitStackConversions (oldamax, oldbmax);
	}
#endif

static void move_a_stack_pointer (int old_asp,int new_asp)
{
	if (old_asp<new_asp){
		int offset;
		
		offset=0;
		GenBuildh (&nil_lab,0);
		++old_asp;
		
		while (old_asp<new_asp){
			GenPushA (offset);
			++offset;
			++old_asp;
		}
	} else
		GenPopA (old_asp-new_asp);
}

void UpdateStackPointers (int old_asp,int old_bsp,int new_asp,int new_bsp)
{
	move_a_stack_pointer (old_asp,new_asp);

	if (old_bsp<new_bsp){
		int offset;
		SymbValue sv;
		
		offset=0;
		sv.val_int="0";
		PushBasic (IntObj,sv);
		++old_bsp;
		
		while (old_bsp<new_bsp){
			GenPushB (offset);
			++offset;
			++old_bsp;
		}
	} else
		GenPopB (old_bsp-new_bsp);
}

static void AdjustStacksAndJumpToThenOrElseLabel
	(Label truelab,Label falselab,Label next_label,int asp,int bsp,int bsize,int then_asp,int then_bsp,int else_asp,int else_bsp)
{
	if (then_asp==else_asp){
		move_a_stack_pointer (asp,then_asp);
		then_asp = else_asp = asp;
	}
	if (then_bsp==else_bsp){
		if (bsp-bsize<then_bsp){
			int offset,n;
			SymbValue sv;
			
			offset=0;
			sv.val_int="0";
			PushBasic (IntObj,sv);
			++bsp;
			
			while (bsp-bsize<then_bsp){
				GenPushB (offset);
				++offset;
				++bsp;
			}
			++offset;
			
			for (n=0; n<bsize; ++n)
				GenUpdateB (n+offset,n);			
		} else {
			UpdateBasic (bsize,bsize-1,bsp-then_bsp-bsize);
			GenPopB (bsp-then_bsp-bsize);
		}
		then_bsp = else_bsp = bsp - bsize;
	}

	if (asp==else_asp && bsp - else_bsp - bsize == 0){
		if (falselab==next_label && asp==then_asp && bsp-bsize==then_bsp){
			GenJmpTrue (truelab);
			truelab->lab_mod=NULL;		
		} else {
			GenJmpFalse (falselab);
			falselab->lab_mod=NULL;
			
			UpdateStackPointers (asp, bsp-bsize, then_asp, then_bsp);

			if (truelab!=next_label){
				GenJmp	(truelab);
				truelab->lab_mod=NULL;
			}
		}
	} else if (asp==then_asp && bsp - then_bsp - bsize == 0){
		if (truelab==next_label && asp==else_asp && bsp-bsize==else_bsp){
			GenJmpTrue (falselab);
			falselab->lab_mod=NULL;			
		} else {
			GenJmpTrue (truelab);
			truelab->lab_mod=NULL;

			UpdateStackPointers (asp, bsp-bsize, else_asp, else_bsp);

			if (falselab!=next_label){
				GenJmp (falselab);
				falselab->lab_mod=NULL;
			}
		}
	} else {
		LabDef loclab;
	
		MakeLabel (&loclab, m_symb, NewLabelNr++, no_pref);
		GenJmpFalse (&loclab);

		UpdateStackPointers (asp, bsp-bsize, then_asp, then_bsp);
		GenJmp	(truelab);
		truelab->lab_mod=NULL;

		GenLabelDefinition	(&loclab);
		UpdateStackPointers (asp, bsp-bsize, else_asp, else_bsp);

		if (falselab!=next_label){
			GenJmp (falselab);
			falselab->lab_mod=NULL;
		}
	}
}

void EvaluateCondition (Node cond_node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p,StateS resultstate)
{
	switch (cond_node->node_kind){
		case NodeIdNode:
		{
			NodeId nid;
			int boolean_b_size;

			nid=cond_node->node_node_id;
			
#if BOXED_RECORDS
			nid->nid_mark2 |= NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;		
#endif
			CopyNodeIdArgument (resultstate,nid,asp_p,bsp_p);
			
			decrement_reference_count_of_node_id (nid,&code_gen_node_ids_p->free_node_ids);

			boolean_b_size = ObjectSizes [resultstate.state_object];
			*bsp_p-=boolean_b_size;
			break;
		}
		case NormalNode:
		case SelectorNode:
		case MatchNode:
		{
			int asize,bsize,boolean_b_size;
		
			Build (cond_node,asp_p,bsp_p,code_gen_node_ids_p);
			DetermineSizeOfState (cond_node->node_state,&asize,&bsize);
			CoerceArgumentOnTopOfStack (asp_p,bsp_p,resultstate,cond_node->node_state,asize,bsize);
			boolean_b_size = ObjectSizes [resultstate.state_object];
			*bsp_p-=boolean_b_size;
			break;
		}
		case IfNode:
			EvaluateCondition (cond_node->node_arguments->arg_node,asp_p,bsp_p,code_gen_node_ids_p,resultstate);
			break;
		default:
			error_in_function ("EvaluateCondition");
	}
}

static Bool IsBooleanValue (Node node, Bool *val)
{
	if (node->node_kind==NormalNode && node->node_symbol->symb_kind==bool_denot){
		*val = node->node_symbol->symb_bool;
		return True;
	} else
		return False;
}

void subtract_else_ref_counts (struct node_id_ref_count_list *else_node_id_ref_counts,NodeIdListElementS **free_node_ids_l)
{
	struct node_id_ref_count_list *else_node_id_ref_count;
	
	for_l (else_node_id_ref_count,else_node_id_ref_counts,nrcl_next){
		struct node_id *node_id;
		int ref_count;

		node_id=else_node_id_ref_count->nrcl_node_id;
#if BOXED_RECORDS
		else_node_id_ref_count->nrcl_mark2=node_id->nid_mark2 & NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
#endif
		ref_count=node_id->nid_refcount;
		if (ref_count>=0){
			ref_count -= else_node_id_ref_count->nrcl_ref_count;
			node_id->nid_refcount=ref_count;
			
			if (ref_count==0)
				add_node_id_to_list (node_id,free_node_ids_l);			
		} else {
			ref_count += else_node_id_ref_count->nrcl_ref_count;
			node_id->nid_refcount=ref_count;

			if (ref_count==-1){
				if (! (node_id->nid_node!=NULL && !IsSimpleState (node_id->nid_state)) && unused_node_id_(node_id))
					add_node_id_to_list (node_id,free_node_ids_l);
			}
		}
	}
}

void add_else_ref_counts (struct node_id_ref_count_list *else_node_id_ref_counts)
{
	struct node_id_ref_count_list *else_node_id_ref_count;
	
	for_l (else_node_id_ref_count,else_node_id_ref_counts,nrcl_next){
		struct node_id *node_id;
#if BOXED_RECORDS
		unsigned int node_id_mark2;
#endif		
		node_id=else_node_id_ref_count->nrcl_node_id;
		
#if BOXED_RECORDS
		node_id_mark2=node_id->nid_mark2;
		node_id->nid_mark2=(node_id_mark2 & ~NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES) | else_node_id_ref_count->nrcl_mark2;
		else_node_id_ref_count->nrcl_mark2=node_id_mark2 & NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
#endif

		if (node_id->nid_refcount>=0)
			node_id->nid_refcount += else_node_id_ref_count->nrcl_ref_count;
		else
			node_id->nid_refcount -= else_node_id_ref_count->nrcl_ref_count;
	}
}

#if BOXED_RECORDS
void or_then_record_update_marks (struct node_id_ref_count_list *else_node_id_ref_counts)
{
	struct node_id_ref_count_list *else_node_id_ref_count;
	
	for_l (else_node_id_ref_count,else_node_id_ref_counts,nrcl_next)
		else_node_id_ref_count->nrcl_node_id->nid_mark2 |= else_node_id_ref_counts->nrcl_mark2;
}
#endif

static void EvaluateThenOrElsePartOfCondition
	(NodeDefs defs,Node node,int asp,int bsp,StateS resultstate, Label truelab, Label falselab,Label next_label,
	 int then_asp,int then_bsp,int else_asp,int else_bsp,NodeIdListElementP a_node_ids,NodeIdListElementP b_node_ids,
	 NodeIdListElementP free_node_ids);

void BranchOnCondition (Node condnode,int asp,int bsp,CodeGenNodeIdsP code_gen_node_ids_p, StateS resultstate,
						Label truelab,Label falselab,Label next_label,int then_asp,int then_bsp,int else_asp,int else_bsp)
{
	switch (condnode->node_kind){
		case NodeIdNode:
		case NormalNode:
		case SelectorNode:
		case MatchNode:
		{
			int boolean_b_size;
			boolean_b_size = ObjectSizes [resultstate.state_object];
			AdjustStacksAndJumpToThenOrElseLabel (truelab,falselab,next_label,asp,bsp+boolean_b_size,boolean_b_size,then_asp,then_bsp,else_asp,else_bsp);
			break;
		}
		case IfNode:
		{
			Bool bool_;
			Label thenlabel,elselabel;
			LabDef thenlab,elselab;
			int	new_then_asp,new_then_bsp,new_else_asp,new_else_bsp;
			Args condpart;

			new_then_asp = asp;
			new_then_bsp = bsp,
			new_else_asp = asp;
			new_else_bsp = bsp;
			condpart = condnode->node_arguments;
			
			if (IsBooleanValue (condpart->arg_next->arg_node,&bool_)){
				if (bool_){
					thenlabel = truelab;
					new_then_asp = then_asp;
					new_then_bsp = then_bsp;
				} else {
					thenlabel = falselab;
					new_then_asp = else_asp;
					new_then_bsp = else_bsp;
				}
			} else {
				thenlabel = NULL;
				MakeLabel (&thenlab, then_symb, NewLabelNr++, no_pref);
				thenlab.lab_mod=notused_string;
			}

			if (IsBooleanValue (condpart->arg_next->arg_next->arg_node,&bool_)){
				if (bool_){
					elselabel = truelab;
					new_else_asp = then_asp;
					new_else_bsp = then_bsp;
				} else {
					elselabel = falselab;
					new_else_asp = else_asp;
					new_else_bsp = else_bsp;
				}
			} else {
				elselabel = NULL;
				MakeLabel (&elselab, else_symb, NewLabelNr++, no_pref);
				elselab.lab_mod=notused_string;
			}

			BranchOnCondition (condpart->arg_node,asp,bsp,code_gen_node_ids_p,resultstate,
								thenlabel ? thenlabel : &thenlab, elselabel ? elselabel : &elselab,
								!thenlabel ? &thenlab : !elselabel ? &elselab : next_label,
								new_then_asp, new_then_bsp, new_else_asp, new_else_bsp);

			if (!thenlabel){
				NodeIdListElementP free_node_ids;
				
				if (thenlab.lab_mod==NULL)
					GenLabelDefinition (&thenlab);

				free_node_ids=code_gen_node_ids_p->free_node_ids;

				if (condnode->node_else_node_id_ref_counts!=NULL)
					subtract_else_ref_counts (condnode->node_else_node_id_ref_counts,&free_node_ids);

				EvaluateThenOrElsePartOfCondition (condnode->node_then_node_defs,
					condpart->arg_next->arg_node, asp,bsp,resultstate,truelab,falselab,!elselabel ? &elselab : next_label,
					then_asp,then_bsp,else_asp,else_bsp,code_gen_node_ids_p->a_node_ids,code_gen_node_ids_p->b_node_ids,
					free_node_ids);

				if (condnode->node_else_node_id_ref_counts!=NULL)
					add_else_ref_counts (condnode->node_else_node_id_ref_counts);
			}

			if (!elselabel){
				if (elselab.lab_mod==NULL)
					GenLabelDefinition (&elselab);

				EvaluateThenOrElsePartOfCondition (condnode->node_else_node_defs,
					condpart->arg_next->arg_next->arg_node,asp,bsp,resultstate,truelab,falselab,next_label,
					then_asp,then_bsp,else_asp,else_bsp,code_gen_node_ids_p->a_node_ids,code_gen_node_ids_p->b_node_ids,
					code_gen_node_ids_p->free_node_ids);
			}

#if BOXED_RECORDS
			if (!thenlabel && condnode->node_else_node_id_ref_counts)

				or_then_record_update_marks (condnode->node_else_node_id_ref_counts);
#endif
			break;
		}
		default:
			error_in_function ("BranchOnCondition");
	}
}

static void EvaluateThenOrElsePartOfCondition
	(NodeDefs defs,Node node,int asp,int bsp,StateS resultstate, Label truelab, Label falselab,Label next_label,
	 int then_asp,int then_bsp,int else_asp,int else_bsp,NodeIdListElementP a_node_ids,NodeIdListElementP b_node_ids,
	 NodeIdListElementP free_node_ids)
{
	SavedNidStateP saved_node_id_states;
	MovedNodeIdP moved_node_ids;
	struct code_gen_node_ids code_gen_node_ids;
			
	saved_node_id_states=NULL;
	moved_node_ids=NULL;

	code_gen_node_ids.free_node_ids=free_node_ids;
	code_gen_node_ids.saved_nid_state_l=&saved_node_id_states;
	code_gen_node_ids.doesnt_fail=False;
	code_gen_node_ids.moved_node_ids_l=&moved_node_ids;
	code_gen_node_ids.a_node_ids=a_node_ids;
	code_gen_node_ids.b_node_ids=b_node_ids;
	
	CodeSharedNodeDefs (defs,NULL,&asp,&bsp,&code_gen_node_ids);

	EvaluateCondition (node,&asp,&bsp,&code_gen_node_ids,resultstate);
	
	BranchOnCondition (node,asp,bsp,&code_gen_node_ids,resultstate,truelab,falselab,next_label,then_asp,then_bsp,else_asp,else_bsp);

	restore_saved_node_id_states (saved_node_id_states);
}

void InitCoding (void)
{
	int i;

	NewLabelNr	  = 1;
	new_not_eq_z_label_n=1;
	SetUnaryState (& StrictOnAState, StrictOnA, UnknownObj);
	SetUnaryState (& OnAState, OnA, UnknownObj);
	SetUnaryState (& UnderEvalState, UnderEval, UnknownObj);
	SetUnaryState (& ProcIdState, OnB, ProcIdObj);

	ApplyDef=MakeNewSymbolDefinition ("system", ApplyId, 2, DEFRULE);
	ApplyDef->sdef_number=0;

	IfDef=MakeNewSymbolDefinition ("system", IfId, 3, DEFRULE);
	IfDef->sdef_number=0;

	SeqDef=MakeNewSymbolDefinition ("system", system_seq_id, 2, DEFRULE);
	SeqDef->sdef_number=0;

	InitBasicDescriptor (UnknownObj, "_", SizeOfAStackElem);
#if ABSTRACT_OBJECT
	InitBasicDescriptor (AbstractObj, "_", SizeOfAStackElem);
#endif
	InitBasicDescriptor (IntObj, "INT", SizeOfInt);
	InitBasicDescriptor (BoolObj, "BOOL", SizeOfBool);
	InitBasicDescriptor (CharObj, "CHAR", SizeOfChar);
	InitBasicDescriptor (StringObj, "STRING", SizeOfAStackElem);
#ifdef G_A64
	InitBasicDescriptor (RealObj, "REAL", 1);
#else
	InitBasicDescriptor (RealObj, "REAL", SizeOfReal);
#endif
	InitBasicDescriptor (FileObj, "FILE", SizeOfFile);
	InitBasicDescriptor (ArrayObj, "ARRAY", SizeOfAStackElem);
	InitBasicDescriptor (UnboxedArrayObj, "ARRAY", SizeOfAStackElem);

	InitBasicDescriptor (WorldObj, "WORLD", SizeOfAStackElem);
	InitBasicDescriptor (ProcIdObj, "PROCID", SizeOfProcId);
	InitBasicDescriptor (RedIdObj, "REDID", SizeOfInt);

	for (i=0; i<MaxNodeArity-NrOfGlobalSelectors; i++)
		LazyTupleSelectors [i] = False;
	
	for (i=0; i<5; ++i){
		unboxed_cons_mark[i][0]=0;
		unboxed_cons_mark[i][1]=0;
	}
	unboxed_cons_array_mark=0;

	next_update_function_n=0;
	next_match_function_n=0;
}
