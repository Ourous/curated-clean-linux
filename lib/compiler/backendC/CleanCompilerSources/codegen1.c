/*
	File:	codegen1.c
	Authors:Sjaak Smetsers & John van Groningen
*/

#define SHARE_UPDATE_CODE 0 /* also in codegen.c */
#define FREE_STRICT_LHS_TUPLE_ELEMENTS 1 /* also in codegen2.c */
#define BIND_UNBOXED_LHS_TUPLE_AND_RECORD_ARGUMENTS_IN_BIND_ARGUMENTS 1

#include "compiledefines.h"
#include "types.t"
#include "system.h"
#include "settings.h"
#include "syntaxtr.t"
#include "comsupport.h"
#include "sizes.h"
#include "checker.h"
#include "codegen_types.h"
#include "codegen1.h"
#include "sa.h"
#include "statesgen.h"
#include "transform.h"
#include "codegen.h"
#include "codegen2.h"
#include "codegen3.h"
#include "instructions.h"
#include "scanner.h"
#include "buildtree.h"
#include "pattern_match.h"
#if SHARE_UPDATE_CODE
#	include "result_state_database.h"
#endif

extern int VERSION;

#define for_l(v,l,n) for(v=(l);v!=NULL;v=v->n)

#define RECORD_N_PREFIX c_pref
#define RECORD_D_PREFIX t_pref
#define CONSTRUCTOR_R_PREFIX k_pref

static char c_pref[] = "c";
static char t_pref[] = "t";
static char k_pref[] = "k";
static char r_pref[] = "r";

char no_pref[]	= "";
char d_pref[] 	= "d";
char n_pref[] 	= "n";

char ea_pref[]	= "ea";
char eu_pref[]	= "eu";
char l_pref[] 	= "l";
char s_pref[] 	= "s";

char caf_pref[]	= "v";

char glob_sel[]	= "_S";
char m_symb[]	= "m";

#ifdef THUNK_LIFT_SELECTORS
char glob_selr[]= "_Sr";
#endif

char channel_code[] = "_channel_code";
char hnf_reducer_code[]	= "_hnf_reducer";
char ext_hnf_reducer_code[]	= "_HnfReducer";
char ext_nf_reducer_code[] = "_NfReducer";
char nf_reducer_code[] = "_nf_reducer";

static char loc_sel[] = "t";

/*
	Each label is represented by 4 items, namely, a module name,a prefix (which is a string), the actual name and a postfix
	(which is a number). Only the third item is always present; all the others are optional (their absence is indicated by resp.
	'NULL', 'no_pref' and 'no_num').
*/

LabDef cycle_lab			= {NULL, "", False, "_cycle_in_spine", 0};
LabDef reserve_lab			= {NULL, "", False, "_reserve", 0};
LabDef type_error_lab		= {NULL, "", False, "_type_error", 0};
LabDef indirection_lab		= {NULL, "", False, "e_system_nind", 0};
LabDef ind_lab				= {NULL, "", False, "e_system_dind", 0};
LabDef hnf_lab				= {NULL, "", False, "_hnf", 0};
LabDef cons_lab				= {NULL, "", False, "_Cons", 0};
LabDef nil_lab				= {NULL, "", False, "_Nil", 0};
LabDef tuple_lab			= {NULL, "", False, "_Tuple", 0};
LabDef empty_lab			= {NULL, "", False, "_", 0};
LabDef add_arg_lab			= {NULL, "", False, "_add_arg", 0};
LabDef match_error_lab		= {NULL, "", False, "_match_error", 0};
#if STRICT_LISTS
LabDef conss_lab			= {NULL, "", False, "_Conss", 0};
LabDef consts_lab			= {NULL, "", False, "_Consts", 0};
LabDef conssts_lab			= {NULL, "", False, "_Conssts", 0};

LabDef unboxed_cons_labels[5][2] = {
	/*IntObj*/	{{NULL, "", False, "_Consi", 0}, {NULL, "", False, "_Consits", 0}},
	/*BoolObj*/	{{NULL, "", False, "_Consb", 0}, {NULL, "", False, "_Consbts", 0}},
	/*CharObj*/	{{NULL, "", False, "_Consc", 0}, {NULL, "", False, "_Conscts", 0}},
	/*RealObj*/	{{NULL, "", False, "_Consr", 0}, {NULL, "", False, "_Consrts", 0}},
	/*FileObj*/	{{NULL, "", False, "_Consf", 0}, {NULL, "", False, "_Consfts", 0}}
	};

LabDef unboxed_cons_array_label = {NULL, "", False, "_Consa", 0};

int unboxed_cons_mark[5][2];
int unboxed_cons_array_mark;

#endif
#ifdef CLEAN2
LabDef select_with_dictionary_lab	= {NULL, "", False, "_select_with_dictionary", 0};
LabDef update_with_dictionary_lab	= {NULL, "", False, "_update_with_dictionary", 0};
#endif

LabDef CurrentAltLabel; /* Containing the name of the next rule alternative */
Label ReduceError;


static void error_in_function (char *m)
{
	ErrorInCompiler ("codegen1.c",m,"");
}

void MakeLabel (Label lab, char *name, unsigned num, char *pref)
{
	lab->lab_issymbol	= False;
	lab->lab_name		= name;
	lab->lab_post		= num;
	lab->lab_pref		= pref;
}

void MakeSymbolLabel (Label lab, char *mod, char *pref,SymbDef sdef, unsigned num)
{
	lab->lab_mod        = mod;
	lab->lab_pref       = pref;
	lab->lab_issymbol   = True;
	lab->lab_symbol 	= sdef;
	lab->lab_post       = num;
}

void ConvertSymbolToLabel (LabDef *slab,SymbDef sdef)
{
	char *modname;

	if (sdef->sdef_module==CurrentModule){
		if (sdef->sdef_exported)
			modname = CurrentModule;
		else
			modname = NULL;
	} else
		modname = sdef->sdef_module;

	MakeSymbolLabel (slab,modname,no_pref,sdef, 0);
}

void ConvertSymbolToDLabel (LabDef *slab,SymbDef sdef)
{
	char *modname;

	if (sdef->sdef_module==CurrentModule){
		if (sdef->sdef_exported)
			modname = CurrentModule;
		else
			modname = NULL;
	} else
		modname = sdef->sdef_module;

	MakeSymbolLabel (slab,modname,d_pref,sdef,0);
}

void ConvertSymbolToConstructorDLabel (LabDef *slab,SymbDef sdef)
{
	char *modname;

	if (!sdef->sdef_exported && sdef->sdef_module==CurrentModule && !ExportLocalLabels)
		modname = NULL;
	else
		modname = sdef->sdef_module;

	MakeSymbolLabel (slab,modname,d_pref,sdef,0);
}

void ConvertSymbolToDandNLabel (LabDef *d_lab,LabDef *n_lab,SymbDef sdef)
{
	char *modname;

	if (sdef->sdef_module==CurrentModule){
		if (sdef->sdef_exported)
			modname = CurrentModule;
		else
			modname = NULL;
	} else
		modname = sdef->sdef_module;

	MakeSymbolLabel (d_lab,modname,d_pref,sdef,0);

	*n_lab = *d_lab;
	n_lab->lab_pref = n_pref;
}

void ConvertSymbolToConstructorDandNLabel (LabDef *d_lab,LabDef *n_lab,SymbDef sdef)
{
	char *modname;

	if (!sdef->sdef_exported && sdef->sdef_module==CurrentModule && !(ExportLocalLabels && (sdef->sdef_mark & SDEF_USED_CURRIED_MASK)!=0))
		modname = NULL;
	else
		modname = sdef->sdef_module;

	MakeSymbolLabel (d_lab,modname,d_pref,sdef,0);

	if (modname==NULL && ExportLocalLabels)
		d_lab->lab_mod = CurrentModule;	

	*n_lab = *d_lab;
	n_lab->lab_pref = n_pref;	
}

void ConvertSymbolToRecordDandNLabel (LabDef *d_lab,LabDef *n_lab,SymbDef sdef)
{
	char *modname;

	if (!sdef->sdef_exported && sdef->sdef_module==CurrentModule)
		modname = NULL;
	else
		modname = sdef->sdef_module;

	MakeSymbolLabel (d_lab,modname,RECORD_D_PREFIX,sdef,0);

	if (modname==NULL && ExportLocalLabels)
		d_lab->lab_mod = CurrentModule;

	*n_lab = *d_lab;
	n_lab->lab_pref = RECORD_N_PREFIX;
}

void ConvertSymbolToKLabel (LabDef *slab,SymbDef sdef)
{
	char *modname;

	if (!sdef->sdef_exported && sdef->sdef_module==CurrentModule && !ExportLocalLabels)
		modname = NULL;
	else
		modname = sdef->sdef_module;

	MakeSymbolLabel (slab,modname,CONSTRUCTOR_R_PREFIX,sdef,0);
}

void ConvertSymbolToRLabel (LabDef *slab,SymbDef sdef)
{
	char *modname;

	if (!sdef->sdef_exported && sdef->sdef_module==CurrentModule && !ExportLocalLabels)
		modname = NULL;
	else {
		modname = sdef->sdef_module;
		if ((sdef->sdef_mark & SDEF_RECORD_R_LABEL_IMPORTED_MASK)!=0){
			sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK;
		} else {
			sdef->sdef_mark |= SDEF_USED_STRICTLY_MASK | SDEF_RECORD_R_LABEL_IMPORTED_MASK;

			GenImpRecordDesc (modname,sdef->sdef_ident->ident_name);
		}
	}
	MakeSymbolLabel (slab,modname,r_pref,sdef,0);
}

static SymbDefS lazy_tuple_selector_sdef;
static IdentS lazy_tuple_selector_ident;

void BuildLazyTupleSelectorLabel (Label slab, int arity, int argnr)
{
	if (argnr > NrOfGlobalSelectors){
		LazyTupleSelectors [argnr - NrOfGlobalSelectors- 1] = True;
		if (ExportLocalLabels){
			lazy_tuple_selector_sdef.sdef_exported=True;
			lazy_tuple_selector_sdef.sdef_ident=&lazy_tuple_selector_ident;
			lazy_tuple_selector_ident.ident_name=loc_sel;
			MakeSymbolLabel (slab,CurrentModule,n_pref,&lazy_tuple_selector_sdef,argnr);
		} else {
			LazyTupleSelectors [argnr - NrOfGlobalSelectors- 1] = True;
			MakeLabel (slab,loc_sel,argnr,n_pref);
		}
	} else
		MakeLabel (slab,glob_sel,argnr,n_pref);
}

#if defined (THUNK_LIFT_SELECTORS)
void BuildLazyTupleSelectorAndRemoveLabel (Label slab,int arity,int argnr)
{
	if (argnr > NrOfGlobalSelectors){
		MakeLabel (slab,glob_selr,argnr,n_pref);
#if 0
		error_in_function ("BuildLazyTupleSelectorAndRemoveLabel");
#endif
	} else 
		MakeLabel (slab,glob_selr,argnr,n_pref);
}
#endif

void FileComment (void)
{
	if (DoDebug)
		FPrintF (OutFile, "\n||\tConcurrent Clean Code Generator (Version %d.%d)",VERSION / 1000, VERSION % 1000);
}

void PrintNodeId (NodeId nid)
{
	if (nid && nid->nid_ident && nid->nid_ident->ident_name)
		FPrintF (OutFile, "%s", nid->nid_ident->ident_name);
	else
		FPrintF (OutFile, "_");
}

void PrintComment (void)
{
	FPrintF (OutFile, "\n\t\t\t||\t");
}

void LhsComment (unsigned int altnr, int asp, int bsp)
{
	if (DoDebug){
		PrintComment ();
#if 1
		FPrintF (OutFile,"Match code, stacksizes A: %d B: %d",asp,bsp);
#else
		FPrintF (OutFile,"Match code for alternative %d, stacksizes A: %d B: %d",altnr, asp, bsp);
#endif
	}
}

void StrictIdComment (NodeId id)
{
	if (DoDebug){
		PrintComment ();
		PrintNodeId  (id);
		FPrintF (OutFile, ": strict annotated");
	}
}

void NodeDefComment (NodeDefs nd, char *msg)
{
	if (DoDebug){
		PrintComment ();
		FPrintF (OutFile, "Node definition ");
		PrintNodeId  (nd->def_id);

		if (nd->def_node && (nd->def_node->node_kind==NormalNode || nd->def_node->node_kind==SelectorNode)){
			FPrintF (OutFile, ": ");
			PrintSymbol  (nd->def_node->node_symbol, OutFile);
		}
		FPrintF (OutFile, " (%s)", msg);
	}
}

void ContractumComment (int asp, int bsp)
{
	if (DoDebug){
		PrintComment ();
		FPrintF (OutFile,"Building the contractum, Stacksizes A: %d B: %d",asp, bsp);
	}
}

void RedirectionComment (NodeId nid)
{
	if (DoDebug){
		PrintComment();
		FPrintF	   (OutFile, "Redirecting the root to: ");
		PrintNodeId  (nid);
	}
}

void ArgComment (Args arg)
{
	if (DoDebug){
		Node arg_node;
		
		arg_node=arg->arg_node;
		
		PrintComment();

		if (arg_node->node_kind==NodeIdNode){
			PrintNodeId (arg_node->node_node_id);
			
			if (arg_node->node_node_id->nid_node){
				Node node;
				
				node=arg_node->node_node_id->nid_node;
				
				if (node->node_kind==NormalNode || node->node_kind==SelectorNode){
					FPrintF (OutFile, ": ");
					PrintSymbol (node->node_symbol, OutFile);
				}
			}
		} else if (arg_node->node_kind==NormalNode || arg_node->node_kind==SelectorNode)
			PrintSymbol (arg->arg_node->node_symbol, OutFile);
	}
}

void NodeIdComment (NodeId node_id)
{
	if (DoDebug){
		PrintComment();

		PrintNodeId (node_id);

		if (node_id->nid_node){
			Node node;
				
			node=node_id->nid_node;
				
			if (node->node_kind==NormalNode || node->node_kind==SelectorNode){
				FPrintF (OutFile, ": ");
				PrintSymbol (node->node_symbol, OutFile);
			}
		}
	}
}

void ParComment (Args arg)
{
	if (DoDebug){
		PrintComment ();
/*		if (arg->arg_id)
			PrintNodeId (arg->arg_id);
		else
			PrintSymbol (arg->arg_pattern->node_symbol,OutFile);
*/
		FPrintF	   (OutFile, ": parallel subgraph");
	}
}

void DetermineSizeOfStates (int arity, States states, int *asize, int *bsize)
{
	*asize=0;
	*bsize=0;

	for (; arity; arity--)
		AddSizeOfState (states [arity-1], asize, bsize);
}

static void AddSizeOfStates (int arity, States states, int *asize, int *bsize)
{	
	for (; arity; arity--)
		AddSizeOfState (states [arity-1], asize, bsize);
}

void DetermineSizeOfState (StateS state, int *asize, int *bsize)
{
	*asize=0;
	*bsize=0;
	AddSizeOfState (state,asize,bsize);
}

void AddSizeOfState (StateS state, int *asize, int *bsize)
{
	if (IsSimpleState (state)){
		if (state.state_kind == OnB)
			*bsize += ObjectSizes [state.state_object];
		else if (state.state_kind != Undefined)
			*asize += SizeOfAStackElem;
	} else {
		switch (state.state_type){
			case RecordState:
				AddSizeOfStates (state.state_arity, state.state_record_arguments, asize, bsize);
				break;
			case TupleState:
				AddSizeOfStates (state.state_arity, state.state_tuple_arguments, asize, bsize);
				break;
			case ArrayState:
				*asize += SizeOfAStackElem;
				break;
		}
	}
}
	
void AddStateSizesAndMaxFrameSizes (int arity,States states,int *maxasize,int *asize,int *bsize)
{
	for (arity--; arity>=0; arity--)
		AddStateSizeAndMaxFrameSize (states [arity], maxasize, asize, bsize);
}

static void DetermineStateSizesAndMaxFrameSizes (int arity,States states,int *maxasize,int *asize,int *bsize)
{
	*asize=0;
	*bsize=0;
	*maxasize=0;

	for (arity--; arity>=0; arity--)
		AddStateSizeAndMaxFrameSize (states [arity], maxasize, asize, bsize);
}

void AddStateSizeAndMaxFrameSize (StateS state,int *maxasize,int *asize,int *bsize)
{
	if (IsSimpleState (state)){
		if (state.state_kind == OnB)
			(*bsize)	  += ObjectSizes [state.state_object];
		else if (state.state_kind != Undefined){
			(*asize)	  += SizeOfAStackElem;
			(*maxasize) += SizeOfAStackElem;
		}
	} else {
		switch (state.state_type){
			case RecordState:
				AddStateSizesAndMaxFrameSizes (state.state_arity,state.state_record_arguments,maxasize,asize,bsize);
				break;
			case TupleState:
				(*maxasize) += state.state_arity;
				AddStateSizesAndMaxFrameSizes (state.state_arity,state.state_tuple_arguments,maxasize,asize,bsize);
				break;
			case ArrayState:
				(*asize)	  += SizeOfAStackElem;
				(*maxasize) += SizeOfAStackElem;
				break;
		}
	}
}

void AddStateSizesAndMaxFrameSizesOfArguments (Args args,int *maxasize,int *asize,int *bsize)
{
	for (; args!=NULL; args=args->arg_next)
		AddStateSizeAndMaxFrameSize (args->arg_state,maxasize,asize,bsize);
}

/* The layout of the A and B stack frames are computed compile time. */

static int *OfferedAFrame, *DefAFrame, *OfferedBFrame, *DefBFrame,
	*InitOfferedAFrame, *InitDemandedAFrame, *InitDefAFrame,
	*InitOfferedBFrame, *InitDemandedBFrame, *InitDefBFrame;

int *DemandedAFrame,*DemandedBFrame,CurrentAFrameSize,CurrentBFrameSize;

/*
	CreateStackFrames, InitStackConversions, PutInBFrames and PutInAFrames
	are routines which manipulate the stack frame administration. The latter
	is used for a rather efficient way of converting one frame to another.
	At the end of this part the main routine called 'GenStackConversions'
	is given which generates ABC code for the requested conversion.
*/

#define AFRAMESIZE 1000
#define BFRAMESIZE 2000

void CreateStackFrames (void)
{
	CurrentAFrameSize = 0;
	CurrentBFrameSize = 0;

	OfferedAFrame = InitOfferedAFrame = (int*)CompAlloc ((SizeT) (AFRAMESIZE * SizeOf (int)));
	DemandedAFrame = InitDemandedAFrame = (int*)CompAlloc ((SizeT) (AFRAMESIZE * SizeOf (int)));
	DefAFrame = InitDefAFrame = (int*)CompAlloc ((SizeT) (AFRAMESIZE * SizeOf (int)));
	OfferedBFrame = InitOfferedBFrame = (int*)CompAlloc ((SizeT) (BFRAMESIZE * SizeOf (int)));
	DemandedBFrame = InitDemandedBFrame = (int*)CompAlloc ((SizeT) (BFRAMESIZE * SizeOf (int)));
	DefBFrame = InitDefBFrame = (int*)CompAlloc ((SizeT) (BFRAMESIZE * SizeOf (int)));
}	

int *AllocTempDemandedAFrame (int size)
{
	if (OfferedAFrame + size > InitOfferedAFrame + AFRAMESIZE)
		FatalCompError ("codegen", "ReserveAFrameSpace", "stack frame too big");

	return DemandedAFrame + CurrentAFrameSize;
}

int *AllocTempDemandedBFrame (int size)
{
	if (OfferedBFrame + size > InitOfferedBFrame + AFRAMESIZE)
		FatalCompError ("codegen", "ReserveBFrameSpace", "stack frame too big");

	return DemandedBFrame + CurrentBFrameSize;
}

static void ReserveAFrameSpace (int size, int *oldsize)
{
	if (OfferedAFrame + size > InitOfferedAFrame + AFRAMESIZE)
		FatalCompError ("codegen", "ReserveAFrameSpace","stack frame too big");

	DemandedAFrame	+= CurrentAFrameSize;
	OfferedAFrame	+= CurrentAFrameSize;
	DefAFrame		+= CurrentAFrameSize;

	*oldsize = CurrentAFrameSize;
	CurrentAFrameSize = size;
}

void FreeAFrameSpace (int previoussize)
{
	CurrentAFrameSize	 = previoussize;
	DemandedAFrame	-= previoussize;
	OfferedAFrame	-= previoussize;
	DefAFrame		-= previoussize;
}

void ReserveBFrameSpace (int size, int *oldsize)
{
	if ( OfferedBFrame + size > InitOfferedBFrame + BFRAMESIZE)
		FatalCompError ("codegen","ReserveBFrameSpace","stack frame too big");

	DemandedBFrame	+= CurrentBFrameSize;
	OfferedBFrame	+= CurrentBFrameSize;
	DefBFrame		+= CurrentBFrameSize;

	*oldsize = CurrentBFrameSize;
	CurrentBFrameSize = size;
}

void FreeBFrameSpace (int previoussize)
{
	CurrentBFrameSize	 = previoussize;
	DemandedBFrame	-= previoussize;
	OfferedBFrame	-= previoussize;
	DefBFrame		-= previoussize;
}

void InitStackFrame (int offframe[],int defframe [],int max)
{
	int i;

	for (i=0; i<max; i++){
		offframe [i] = max;
		defframe [i] = i;
	}
}

void InitStackConversions (int maxa,int maxb,int *oldamax_p,int *oldbmax_p)
{
	ReserveAFrameSpace (maxa,oldamax_p);
	ReserveBFrameSpace (maxb,oldbmax_p);

	DemandedAFrame [0] = 0;
	InitStackFrame (OfferedAFrame, DefAFrame, CurrentAFrameSize);
	InitStackFrame (OfferedBFrame, DefBFrame, CurrentBFrameSize);
}

void InitAStackConversions (int maxa,int *oldamax_p)
{
	ReserveAFrameSpace (maxa,oldamax_p);
	InitStackFrame (OfferedAFrame, DefAFrame, CurrentAFrameSize);
}

void ExitStackConversions (int oldamax, int oldbmax)
{
	FreeAFrameSpace (oldamax);
	FreeBFrameSpace (oldbmax);
}

#ifdef _FRAMECHECK_
	static void UpdateAFrame (int frame[], int offset, int index)
	{
		if (offset >= CurrentAFrameSize || offset < 0 || index >= CurrentAFrameSize	|| index < 0)
			ErrorInCompiler ("codegen1.c", "UpdateAFrame","index out of range");
		else
			frame [offset] = index;
	}
	
	static void UpdateBFrame (int frame[], int offset, int index)
	{
		if (offset >= CurrentBFrameSize || offset < 0 || index >= CurrentBFrameSize	|| index < 0)
			ErrorInCompiler ("codegen1.c", "UpdateBFrame","index out of range");
		else
			frame [offset] = index;
	}
#else

#define UpdateAFrame(frame,offset,index) ((frame)[offset] = (index))
#define UpdateBFrame(frame,offset,index) ((frame)[offset] = (index))

#endif
	
void PutInBFrames (int bsp, int *b_ind, int size)
{
	int j;
	
	*b_ind += size;
	
	for (j=0; j<size; j++){
		UpdateBFrame (OfferedBFrame, bsp-j, bsp-j);
		UpdateBFrame (DemandedBFrame, (*b_ind)-j, bsp-j);
	}
}

void PutInAFrames (int asp, int *a_ind)
{
	++ *a_ind;
	
	UpdateAFrame (OfferedAFrame,asp,asp);
	UpdateAFrame (DemandedAFrame,*a_ind,asp);
}

#ifdef _FRAMECHECK_
	static void UpdateFrame (int frame[],int offset,int index, int offframe[])
	{
		if (offframe == OfferedAFrame)
			UpdateAFrame (frame,offset,index);
		else
			UpdateBFrame (frame,offset,index);
	}
#else

#	define UpdateFrame(frame,offset,index,offframe) ((frame)[offset] = (index))

#endif

static void CopyEntry (int offset, int *sp, int offframe [])
{
	if (offframe == OfferedAFrame)
		GenPushA (*sp-offset); 
	else
		GenPushB (*sp-offset); 
	++ *sp;
	UpdateFrame  (offframe, *sp, offframe[offset], offframe);
}

static void UpdateEntry (int srcoffset, int dstoffset, int sp, int offframe [])
{
	if (offframe == OfferedAFrame)
		GenUpdateA (sp-srcoffset, sp-dstoffset);
	else
		GenUpdateB (sp-srcoffset, sp-dstoffset);
	UpdateFrame (offframe, dstoffset, offframe [srcoffset], offframe);
}

static void FillHole (int sp,int offframe[],int demframe [],int defframe [],int offsize,int demsize)
{
	if (sp > demsize)
		return;

	do {
			int newdef;
			
			newdef = defframe [demframe[sp]];
			UpdateEntry (newdef, sp, offsize, offframe);
			UpdateFrame (defframe, demframe[sp], sp, offframe);
			sp = newdef;
	} while (sp <= demsize && offframe[sp] != demframe[sp]);
}

static void GenStackConversions (int *sp,int demsize,int offframe[],int demframe[],int defframe[],int hole)
{
	int mysp;
	Bool topused;

	topused = False;

	for (mysp = 1; mysp <= *sp; mysp++){
		if (offframe  [mysp] == hole) /* Indicating a hole */
			FillHole (mysp, offframe, demframe, defframe,*sp, demsize);
	}

	for (;mysp <= demsize; mysp++){
		int olddef;
		
		olddef = defframe [demframe [mysp]];
		CopyEntry (olddef, sp, offframe);
		if (offframe [olddef] != demframe [olddef]){
			UpdateFrame (defframe, demframe [mysp], mysp, offframe);
			FillHole (olddef, offframe, demframe, defframe, *sp, demsize);
		}
	}

	for (mysp = 1; mysp <= demsize; mysp++){
		if (offframe [mysp] != demframe [mysp]){
			if (topused)
				UpdateEntry (mysp, *sp, *sp, offframe);
			else {
				topused = True;
				CopyEntry (mysp, sp, offframe);
			}
			UpdateFrame (defframe, offframe [mysp], *sp, offframe);
			FillHole (mysp, offframe, demframe, defframe, *sp, demsize);
		}
	}	
}

void GenAStackConversions (int sp,int demsize)
{
	GenStackConversions (&sp,demsize,OfferedAFrame,DemandedAFrame,DefAFrame,CurrentAFrameSize);
	GenPopA (sp-demsize);
}

void GenBStackConversions (int sp,int demsize)
{
	GenStackConversions (&sp,demsize,OfferedBFrame,DemandedBFrame,DefBFrame,CurrentBFrameSize);
	GenPopB (sp-demsize);
}

/*	End of the stack frame conversion routines */

static void JmpEvalArgsEntry (int args_asp,Label ea_lab)
{
	GenDAStackLayout (args_asp);
	if (DoTimeProfiling)
		GenPN();
	GenJmp (ea_lab);
}

static void CallEvalArgsEntry (int args_a_size,StateP function_state_p,int result_asize,int result_bsize,Label ea_lab)
{
	GenDAStackLayout (args_a_size);
	GenJsr (ea_lab);
	GenOStackLayoutOfState (result_asize,result_bsize,function_state_p[-1]);
}

static void CallEvalArgsEntryUnboxed (int args_a_size,int args_b_size,ArgP arguments,StateP function_state_p,int result_asize,int result_bsize,Label ea_lab)
{
	GenDStackLayout (args_a_size,args_b_size,arguments);
	GenJsr (ea_lab);
	GenOStackLayoutOfState (result_asize,result_bsize,function_state_p[-1]);
}

static void GenerateLazyConstructorDescriptorAndFunctionForStrictConstructor (ConstructorList constructor)
{
	SymbDef constructor_def;

	constructor_def=constructor->cl_constructor->type_node_symbol->symb_def;

	if (constructor_def->sdef_exported || (constructor_def->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK)) || ExportLocalLabels){
		LabDef constructor_label,ealab,n_lab,d_lab;
		int maxasize,asize,bsize;
		int asp,bsp,arity;

		asp = constructor_def->sdef_arity;
		bsp = 0;
		arity = asp;
		
		ConvertSymbolToLabel (&CurrentAltLabel,constructor_def);
					
		if (constructor_def->sdef_exported)
			GenExportEaEntry (constructor_def);
		
		GenConstructorFunctionDescriptorAndExportNodeAndDescriptor (constructor_def);

		if (DoTimeProfiling)
			GenPB (constructor_def->sdef_ident->ident_name);

		MakeSymbolLabel (&ealab,constructor_def->sdef_exported ? CurrentModule : NULL,ea_pref,constructor_def,0);
		
		if (constructor_def->sdef_exported || (constructor_def->sdef_mark & SDEF_USED_CURRIED_MASK) || ExportLocalLabels){
			CurrentAltLabel.lab_pref  = l_pref;

			if (DoTimeProfiling)
				GenPL();

#ifdef NEW_APPLY
			if (arity>=2)
				GenApplyEntryDirective (arity,&ealab);
#endif

			GenOAStackLayout (2);
			GenLabelDefinition (&CurrentAltLabel);
			
			GenPushArgs (0,arity-1,arity-1);
			GenUpdateA (arity,arity-1);	
			GenCreate (-1);
			GenUpdateA (0,arity+1);
			GenPopA (1);
			JmpEvalArgsEntry (arity+1,&ealab);
		}

		ConvertSymbolToConstructorDandNLabel (&d_lab,&n_lab,constructor_def);

		GenNodeEntryDirective (arity,&d_lab,&ealab);
		GenOAStackLayout (1);
		GenLabelDefinition (&n_lab);
		GenPushNode (ReduceError,asp);

		GenOAStackLayout (arity+1);
		if (DoTimeProfiling)
			GenPN();
		GenLabelDefinition (&ealab);

		asize=0;
		bsize=0;
		maxasize=0;

		AddStateSizesAndMaxFrameSizes (arity,constructor->cl_state_p,&maxasize,&asize,&bsize);

		EvaluateAndMoveStateArguments (arity,constructor->cl_state_p,asp,maxasize);

		ConvertSymbolToKLabel (&constructor_label,constructor_def);

		GenFillR (&constructor_label,asize,bsize,asize,0,0,ReleaseAndFill,True);		

	 	GenRtn (1,0,OnAState);

 		if (DoTimeProfiling)
			GenPE();
	 }
}

static void GenLazyRecordEntry (SymbDef rdef)
{
	LabDef record_label,d_label,eu_label,*eu_label_p;
	States argstates;
	int asp,bsp,arity;
	int maxasize,asize,bsize;

	argstates = rdef->sdef_record_state.state_record_arguments;

	asp = rdef->sdef_arity;
	bsp = 0;
	arity = asp;

	ConvertSymbolToRecordDandNLabel (&d_label,&CurrentAltLabel,rdef);

	if (rdef->sdef_boxed_record){
		if (rdef->sdef_exported){
			GenExportEuEntry (rdef);
			MakeSymbolLabel (&eu_label,CurrentModule,eu_pref,rdef,0);
		} else {
			MakeSymbolLabel (&eu_label,NULL,eu_pref,rdef,0);		
		}
		eu_label_p=&eu_label;
	} else
		eu_label_p=NULL;
	
	if (DoTimeProfiling)
		GenPB (rdef->sdef_ident->ident_name);

	GenLazyRecordDescriptorAndExport (rdef);

	GenLazyRecordNodeEntryDirective (arity,&d_label,eu_label_p);

	GenOAStackLayout (1);
	GenLabelDefinition (&CurrentAltLabel);
	GenPushNode (ReduceError,asp);

	if (eu_label_p!=NULL){
		GenOAStackLayout (arity+1);
		if (DoTimeProfiling)
			GenPN();
		GenLabelDefinition (&eu_label);
	}

	asize=0;
	bsize=0;
	maxasize=0;
	
	AddStateSizesAndMaxFrameSizes (arity,argstates,&maxasize,&asize,&bsize);

	EvaluateAndMoveStateArguments (arity,argstates,asp,maxasize);

	ConvertSymbolToRLabel (&record_label, rdef);

	GenFillR (&record_label,asize,bsize,asize,0,0,ReleaseAndFill,True);

 	GenRtn (1,0,OnAState);

	if (DoTimeProfiling)
		GenPE();
}

void DetermineFieldSizeAndPosition (int fieldnr,int *asize,int *bsize,int *apos,int *bpos,States argstates)
{
	int i;

	*asize = *bsize = *apos = *bpos = 0;

	for (i=0; i < fieldnr; i++)
		AddSizeOfState (argstates [i], apos, bpos);

	AddSizeOfState (argstates [i], asize, bsize);
}

static void GenLazyFieldSelectorEntry (SymbDef field_def,StateS recstate,int tot_a_size,int tot_b_size)
{
	if (field_def->sdef_exported || field_def->sdef_mark & SDEF_USED_LAZILY_MASK){
		LabDef newealab,loclab,ealab,d_lab,n_lab;
		Bool update_root_node;
		int fieldnr,apos,bpos,asize,bsize;
		StateS offfieldstate,demfieldstate;
		char *record_name;
		LabDef *ea_label_p;
		int node_directive_arity;
		
		fieldnr = field_def->sdef_sel_field_number;

		offfieldstate = recstate.state_record_arguments [fieldnr];
		demfieldstate = field_def->sdef_sel_field->fl_state;

		DetermineFieldSizeAndPosition (fieldnr,&asize,&bsize,&apos,&bpos,recstate.state_record_arguments);
		
		ConvertSymbolToLabel (&CurrentAltLabel,field_def);
		
		if (field_def->sdef_exported || ExportLocalLabels)
			GenExportFieldSelector (field_def);

#ifdef NEW_SELECTOR_DESCRIPTORS
		GenFieldSelectorDescriptor (field_def,offfieldstate,apos,bpos,tot_a_size,tot_b_size);
#else
		GenFieldSelectorDescriptor (field_def,IsSimpleState (offfieldstate));
#endif

		if (DoTimeProfiling)
			GenPB (field_def->sdef_ident->ident_name);

		update_root_node = ! ExpectsResultNode (offfieldstate);

		record_name=field_def->sdef_type->type_lhs->ft_symbol->symb_def->sdef_ident->ident_name;
		
		if (field_def->sdef_calledwithrootnode){
			ealab = CurrentAltLabel;
			ealab.lab_pref = ea_pref;
			
			if (update_root_node){
				newealab = ealab;
				newealab.lab_mod = CurrentModule;
				ealab.lab_mod = NULL;
				ea_label_p=&newealab;
			} else
				ea_label_p=&ealab;
		} else if (field_def->sdef_returnsnode || (IsSimpleState (demfieldstate) && demfieldstate.state_kind==LazyRedirection))
			ea_label_p=&empty_lab;
		else
			ea_label_p=NULL;
		
		node_directive_arity = IsSimpleState (offfieldstate) ? (offfieldstate.state_kind!=OnB ? -4 : -3) : field_def->sdef_arity;

		ConvertSymbolToDandNLabel (&d_lab,&n_lab,field_def);
		
		GenFieldNodeEntryDirective (node_directive_arity,&d_lab,ea_label_p,record_name);

		GenOAStackLayout (1);
		GenFieldLabelDefinition (&n_lab,record_name);

		GenPushNode (ReduceError,field_def->sdef_arity);
		
		if (field_def->sdef_calledwithrootnode){
			if (update_root_node){
				MakeLabel (&loclab, m_symb,NewLabelNr++,no_pref);
				GenOAStackLayout (field_def->sdef_arity);
				if (DoTimeProfiling)
					GenPN();
				GenLabelDefinition (&loclab);
			} else {
				GenOAStackLayout (field_def->sdef_arity+1);
				if (DoTimeProfiling)
					GenPN();
				GenFieldLabelDefinition (&ealab,record_name);
			}
		}
		
		GenJsrEval (0);

#ifndef NEW_SELECTOR_DESCRIPTORS
		if (IsSimpleState (offfieldstate) && offfieldstate.state_kind==OnB && !DoTimeProfiling){
			LabDef gc_apply_label;

			gc_apply_label=CurrentAltLabel;
			gc_apply_label.lab_pref = l_pref;

			GenOAStackLayout (2);
			GenFieldLabelDefinition (&gc_apply_label,record_name);
		}
#endif

		GenPushRArgB (0, tot_a_size, tot_b_size, bpos + 1, bsize); 
		GenReplRArgA (tot_a_size, tot_b_size, apos + 1, asize);
	
		if (IsSimpleState (offfieldstate)){
			if (offfieldstate.state_kind==OnB){
				FillBasicFromB (offfieldstate.state_object, 0, 0, ReleaseAndFill);
				GenPopB (ObjectSizes [offfieldstate.state_object]);
	 			GenRtn (1,0,OnAState);
			} else {
				if (IsLazyState (offfieldstate)){
					if (ExpectsResultNode (demfieldstate)
#if ABSTRACT_OBJECT
						&& demfieldstate.state_object!=AbstractObj
#endif
					)
						GenJmpEvalUpdate ();
					else {
						GenJsrEval (0);
						GenFillFromA (0, 1, ReleaseAndFill);
						GenPopA (1);
		 				GenRtn (1,0, OnAState);
					}
				} else {
					GenFillFromA (0, 1, ReleaseAndFill);
					GenPopA (1);
	 				GenRtn (1,0,OnAState);
				}
			}
		} else {
			switch (offfieldstate.state_type){
				case TupleState:
					BuildTuple (asize,bsize,asize,bsize,
								offfieldstate.state_arity,offfieldstate.state_tuple_arguments,
								asize,bsize,0,ReleaseAndFill,False);
					GenPopA (asize);
					break;
				case ArrayState:
					GenFillArrayAndPop (1,ReleaseAndFill);
					break;
#ifdef ADD_ARGUMENTS_TO_HIGHER_ORDER_FUNCTIONS
				case RecordState:
					BuildRecord (offfieldstate.state_record_symbol,asize,bsize,asize,bsize,
									asize,bsize,0,ReleaseAndFill,False);
					GenPopA (asize);
					break;
#endif
			}
			GenPopB (bsize);
		 	GenRtn (1,0,OnAState);
		}
		
		if (field_def->sdef_calledwithrootnode && update_root_node){
			GenOAStackLayout (field_def->sdef_arity + 1);
			if (DoTimeProfiling)
				GenPN();
			GenFieldLabelDefinition (&newealab,record_name);
			GenDAStackLayout (field_def->sdef_arity);
			if (DoTimeProfiling)
				GenPN();
			GenJmp (&loclab);
		}

		if (DoTimeProfiling)
			GenPE();

#ifndef NEW_SELECTOR_DESCRIPTORS
		/* generate apply entry for the garbage collector: */
		if (IsSimpleState (offfieldstate)){
			LabDef gc_apply_label;

			gc_apply_label=CurrentAltLabel;
			gc_apply_label.lab_pref = l_pref;

			if (offfieldstate.state_kind==OnB){
				if (DoTimeProfiling){
					GenOAStackLayout (2);
					GenFieldLabelDefinition (&gc_apply_label,record_name);
					
					GenPushRArgB (0,tot_a_size,tot_b_size,bpos+1,bsize);
					GenReplRArgA (tot_a_size,tot_b_size,apos+1, asize);

					FillBasicFromB (offfieldstate.state_object,0,0,ReleaseAndFill);
					GenPopB (ObjectSizes [offfieldstate.state_object]);
		 			GenRtn (1,0, OnAState);
	 			}
			} else {
				GenOAStackLayout (1);
				GenFieldLabelDefinition (&gc_apply_label,record_name);
				
				GenReplRArgA (tot_a_size, tot_b_size, apos + 1, asize);
	 			GenRtn (1,0, OnAState);
			}
		}
#endif
	 }
}

static void coerce_args_from_class_to_instance_member
	(int create_new_node,int n_dictionary_args,int n_args,StateP ext_arg_state_p,StateP int_arg_state_p,int aindex,int bindex,int *asp_p,int *bsp_p,int *a_ind_p,int *b_ind_p)
{
	if (create_new_node){
		PutInAFrames (aindex,a_ind_p);
		coerce_args_from_class_to_instance_member (0,n_dictionary_args,n_args,ext_arg_state_p,int_arg_state_p,aindex-1,bindex,asp_p,bsp_p,a_ind_p,b_ind_p);
	} else if (n_dictionary_args>0){
		coerce_args_from_class_to_instance_member (0,n_dictionary_args-1,n_args,ext_arg_state_p,int_arg_state_p+1,aindex-1,bindex,asp_p,bsp_p,a_ind_p,b_ind_p);
		CoerceArgumentUsingStackFrames (*int_arg_state_p,OnAState,aindex,bindex,asp_p,bsp_p,a_ind_p,b_ind_p,1,0);	
	} else if (n_args>0){
		int asize, bsize;
	
		DetermineSizeOfState (*ext_arg_state_p,&asize,&bsize);		
		coerce_args_from_class_to_instance_member (0,n_dictionary_args,n_args-1,ext_arg_state_p+1,int_arg_state_p+1,aindex-asize,bindex-bsize,asp_p,bsp_p,a_ind_p,b_ind_p);
		CoerceArgumentUsingStackFrames (*int_arg_state_p,*ext_arg_state_p,aindex,bindex,asp_p,bsp_p,a_ind_p,b_ind_p,asize,bsize);
	}
}

static int generate_instance_entry_arguments
	(struct symbol_def *dictionary_field,int function_arity,struct state *function_state_p,struct label *i_label_p,int *asize_p,int *bsize_p)
{
	struct type_alt *field_type_alt;
	struct state *member_state_p;
	int member_arity,n_dictionary_args;
	int arg_n,asp,bsp,asize,bsize,oldamax,oldbmax,a_ind,b_ind,maxasize;
	int member_called_with_root_node,function_updates_node,create_new_node;
	
	field_type_alt=dictionary_field->sdef_member_type_of_field; 
	member_arity=field_type_alt->type_alt_lhs->type_node_arity-1;
	member_state_p=dictionary_field->sdef_member_states_of_field;
	
	if (DoDebug)
		FPrintF (OutFile, "\n||\tmember type %s %d %d",dictionary_field->sdef_ident->ident_name,member_arity,function_arity);
	
	n_dictionary_args = function_arity-member_arity;

	++member_state_p;

	DetermineSizeOfStates (member_arity,member_state_p,&asp,&bsp);

	asize=0;
	bsize=0;
	maxasize=0;
	for (arg_n=0; arg_n<function_arity; ++arg_n)
		AddStateSizeAndMaxFrameSize (function_state_p[arg_n],&maxasize,&asize,&bsize);

	member_called_with_root_node = member_state_p[-2].state_type==SimpleState
									&& !(member_state_p[-2].state_kind==StrictRedirection || member_state_p[-2].state_kind==OnB);
	function_updates_node = function_state_p[-1].state_type==SimpleState
							&& !(function_state_p[-1].state_kind==StrictRedirection || function_state_p[-1].state_kind==OnB);

	*i_label_p = CurrentAltLabel;
	i_label_p->lab_pref = "i";
	i_label_p->lab_post = 0;
	GenOStackLayoutOfStates (asp+1+member_called_with_root_node,bsp,member_arity,member_state_p);
	GenLabelDefinition (i_label_p);

	if (n_dictionary_args==0)
		GenPopA (1);
	else
		GenReplArgs (n_dictionary_args,n_dictionary_args);

	create_new_node = function_updates_node && !member_called_with_root_node;
	if (create_new_node)
		GenCreate (-1);

	asp+=create_new_node+n_dictionary_args;
	InitStackConversions (asp+maxasize+1, bsp+bsize+1, &oldamax, &oldbmax);
	a_ind=0;
	b_ind=0;
	coerce_args_from_class_to_instance_member (create_new_node,n_dictionary_args,member_arity,member_state_p,function_state_p,asp,bsp,&asp,&bsp,&a_ind,&b_ind);
	GenAStackConversions (asp,a_ind);
	GenBStackConversions (bsp,b_ind);
	ExitStackConversions (oldamax, oldbmax);
	
	*asize_p=asize;
	*bsize_p=bsize;
	return function_updates_node;
}

static int generate_unboxed_record_cons_instance_entry
	(struct symbol_def *rule_sdef,struct label *unboxed_record_cons_lab_p,struct state *function_state_p,int a_size,int b_size,struct label *i_label_p)
{
	struct symbol_def *dictionary_field;
	int dictionary_sdef_mark;
	
	if (rule_sdef->sdef_mark & SDEF_RULE_INSTANCE_RULE_P){
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance fused");
		if (rule_sdef->sdef_instance_rule->sdef_mark & SDEF_INSTANCE_RULE_WITH_FIELD_P){
			dictionary_field=rule_sdef->sdef_instance_rule->sdef_dictionary_field;
			dictionary_sdef_mark=dictionary_field->sdef_mark;
		} else {
			dictionary_sdef_mark=0;
		}
	} else {
		/* SDEF_INSTANCE_RULE_WITH_FIELD_P */
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance");
		dictionary_field=rule_sdef->sdef_dictionary_field;
		dictionary_sdef_mark=dictionary_field->sdef_mark;
	}
	
	if (dictionary_sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE){
		struct state *member_state_p;
		int asize,bsize,function_updates_node;

		function_updates_node = generate_instance_entry_arguments (dictionary_field,rule_sdef->sdef_arity,function_state_p,i_label_p,&asize,&bsize);

		member_state_p=dictionary_field->sdef_member_states_of_field;
		
		++member_state_p;

		GenFillR (unboxed_record_cons_lab_p,a_size,b_size,a_size,0,0,ReleaseAndFill,True);

		if (function_updates_node || EqualState (function_state_p[-1],member_state_p[-2])){
			GenRtn (1,0,OnAState);
		} else {
			int result_asize,result_bsize;
			
			DetermineSizeOfState (function_state_p[-1],&result_asize,&result_bsize);
			RedirectResultAndReturn (result_asize,result_bsize,result_asize,result_bsize,function_state_p[-1],member_state_p[-2],result_asize,result_bsize);
		}

		return 1;
	}
	return 0;
}

static int generate_unboxed_record_decons_instance_entry (struct symbol_def *rule_sdef,struct state *function_state_p,struct label *i_label_p)
{
	struct symbol_def *dictionary_field;
	int dictionary_sdef_mark;
	
	if (rule_sdef->sdef_mark & SDEF_RULE_INSTANCE_RULE_P){
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance fused");
		if (rule_sdef->sdef_instance_rule->sdef_mark & SDEF_INSTANCE_RULE_WITH_FIELD_P){
			dictionary_field=rule_sdef->sdef_instance_rule->sdef_dictionary_field;
			dictionary_sdef_mark=dictionary_field->sdef_mark;
		} else {
			dictionary_sdef_mark=0;
		}
	} else {
		/* SDEF_INSTANCE_RULE_WITH_FIELD_P */
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance");
		dictionary_field=rule_sdef->sdef_dictionary_field;
		dictionary_sdef_mark=dictionary_field->sdef_mark;
	}
	
	if (dictionary_sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE){
		struct state *member_state_p;
		int asize,bsize,function_updates_node,result_asize,result_bsize;

		function_updates_node = generate_instance_entry_arguments (dictionary_field,rule_sdef->sdef_arity,function_state_p,i_label_p,&asize,&bsize);

		member_state_p=dictionary_field->sdef_member_states_of_field;
		
		++member_state_p;

		DetermineSizeOfState (function_state_p[-1],&result_asize,&result_bsize);

		if (result_bsize==0)
			GenReplArgs (result_asize,result_asize);
		else
			GenReplRArgs (result_asize,result_bsize);

		if (function_updates_node || EqualState (function_state_p[-1],member_state_p[-2]))
			GenRtn (result_asize,result_bsize,function_state_p[-1]);
		else 
			RedirectResultAndReturn (result_asize,result_bsize,result_asize,result_bsize,function_state_p[-1],member_state_p[-2],result_asize,result_bsize);

		return 1;
	}
	return 0;
}

static int generate_unboxed_record_instance_entry (struct symbol_def *rule_sdef,struct state *function_state_p,struct label *i_label_p)
{
	struct symbol_def *dictionary_field;
	int dictionary_sdef_mark;
	
	if (rule_sdef->sdef_mark & SDEF_RULE_INSTANCE_RULE_P){
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance fused");
		if (rule_sdef->sdef_instance_rule->sdef_mark & SDEF_INSTANCE_RULE_WITH_FIELD_P){
			dictionary_field=rule_sdef->sdef_instance_rule->sdef_dictionary_field;
			dictionary_sdef_mark=dictionary_field->sdef_mark;
		} else {
			dictionary_sdef_mark=0;
		}
	} else {
		/* SDEF_INSTANCE_RULE_WITH_FIELD_P */
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance");
		dictionary_field=rule_sdef->sdef_dictionary_field;
		dictionary_sdef_mark=dictionary_field->sdef_mark;
	}
	
	if (dictionary_sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE){
		struct state *member_state_p;
		int asize,bsize,function_updates_node;
		function_updates_node = generate_instance_entry_arguments (dictionary_field,rule_sdef->sdef_arity,function_state_p,i_label_p,&asize,&bsize);

		member_state_p=dictionary_field->sdef_member_states_of_field;
		
		++member_state_p;

		if (function_updates_node || EqualState (function_state_p[-1],member_state_p[-2]))
			CallArrayFunction (rule_sdef,False,&rule_sdef->sdef_rule_type->rule_type_state_p[-1]);
		else {
			int result_asize,result_bsize;

			CallArrayFunction (rule_sdef,True,&rule_sdef->sdef_rule_type->rule_type_state_p[-1]);
			DetermineSizeOfState (function_state_p[-1],&result_asize,&result_bsize);
			RedirectResultAndReturn (result_asize,result_bsize,result_asize,result_bsize,function_state_p[-1],member_state_p[-2],result_asize,result_bsize);
		}

		return 1;
	}
	return 0;
}

static void GenUnboxedRecordConsApplyAndNodeEntries
	(SymbDef fun_def,struct label *unboxed_record_cons_lab_p,int *a_size_p,int *b_size_p)
{
	int asize,bsize,maxasize,arity,n_result_nodes_on_a_stack;
	struct state *rule_type_state_p;
	LabDef ealab;

	n_result_nodes_on_a_stack=1;

	rule_type_state_p = fun_def->sdef_rule_type->rule_type_state_p;
	arity = fun_def->sdef_arity;

	MakeSymbolLabel (&CurrentAltLabel,NULL,no_pref,fun_def,0);

	ealab			= CurrentAltLabel;
	ealab.lab_pref	= ea_pref;
	
	DetermineStateSizesAndMaxFrameSizes (arity,rule_type_state_p,&maxasize,&asize,&bsize);

	if ((fun_def->sdef_mark & SDEF_USED_CURRIED_MASK) || DoDescriptors || DoParallel)
		GenArrayFunctionDescriptor (fun_def,&CurrentAltLabel,arity);

	if (DoTimeProfiling)
		GenPB (fun_def->sdef_ident->ident_name);

	if (fun_def->sdef_mark & SDEF_USED_CURRIED_MASK){
		struct label i_label;

		if (fun_def->sdef_mark & (SDEF_INSTANCE_RULE_WITH_FIELD_P | SDEF_RULE_INSTANCE_RULE_P) &&
			generate_unboxed_record_cons_instance_entry (fun_def,unboxed_record_cons_lab_p,rule_type_state_p,asize,bsize,&i_label))
		{
			ApplyInstanceEntry (rule_type_state_p,arity,&ealab,&i_label,!(fun_def->sdef_mark & SDEF_USED_LAZILY_MASK));
		} else
			ApplyEntry (rule_type_state_p,arity,&ealab,!(fun_def->sdef_mark & SDEF_USED_LAZILY_MASK));
	}

	if (fun_def->sdef_mark & SDEF_USED_LAZILY_MASK)
		NodeEntry (rule_type_state_p,arity,&ealab,fun_def);

	EvalArgsEntry (rule_type_state_p,fun_def,maxasize,&ealab,n_result_nodes_on_a_stack);
	
	*a_size_p=asize;
	*b_size_p=bsize;
}

static void GenUnboxedRecordDeconsApplyAndNodeEntries (SymbDef fun_def,int *a_size_p,int *b_size_p)
{
	LabDef ealab;
	int asize,bsize,maxasize,arity,n_result_nodes_on_a_stack;
	struct state *rule_type_state_p;
	
	n_result_nodes_on_a_stack=0;

	rule_type_state_p = fun_def->sdef_rule_type->rule_type_state_p;
	arity = fun_def->sdef_arity;

	MakeSymbolLabel (&CurrentAltLabel,NULL,no_pref,fun_def,0);

	ealab			= CurrentAltLabel;
	ealab.lab_pref	= ea_pref;
	
	DetermineStateSizesAndMaxFrameSizes (arity,rule_type_state_p,&maxasize,&asize,&bsize);

	if ((fun_def->sdef_mark & SDEF_USED_CURRIED_MASK) || DoDescriptors || DoParallel)
		GenArrayFunctionDescriptor (fun_def,&CurrentAltLabel,arity);

	if (DoTimeProfiling)
		GenPB (fun_def->sdef_ident->ident_name);

	if (fun_def->sdef_mark & SDEF_USED_CURRIED_MASK){
		struct label i_label;

		if (fun_def->sdef_mark & (SDEF_INSTANCE_RULE_WITH_FIELD_P | SDEF_RULE_INSTANCE_RULE_P) &&
			generate_unboxed_record_decons_instance_entry (fun_def,rule_type_state_p,&i_label))
		{
			ApplyInstanceEntry (rule_type_state_p,arity,&ealab,&i_label,!(fun_def->sdef_mark & SDEF_USED_LAZILY_MASK));
		} else
			ApplyEntry (rule_type_state_p,arity,&ealab,!(fun_def->sdef_mark & SDEF_USED_LAZILY_MASK));
	}

	if (fun_def->sdef_mark & SDEF_USED_LAZILY_MASK)
		NodeEntry (rule_type_state_p,arity,&ealab,fun_def);

	EvalArgsEntry (rule_type_state_p,fun_def,maxasize,&ealab,n_result_nodes_on_a_stack);
	
	*a_size_p=asize;
	*b_size_p=bsize;
}

static void GenUnboxedRecordApplyAndNodeEntries (SymbDef fun_def,int *a_size_p,int *b_size_p)
{
	LabDef ealab;
	int asize,bsize,maxasize,n_result_nodes_on_a_stack;
	struct state *rule_type_state_p;
	int arity;

	n_result_nodes_on_a_stack=0;

	rule_type_state_p = fun_def->sdef_rule_type->rule_type_state_p;
	arity = fun_def->sdef_arity;

	MakeSymbolLabel (&CurrentAltLabel,NULL,no_pref,fun_def,0);

	ealab			= CurrentAltLabel;
	ealab.lab_pref	= ea_pref;
	
	DetermineStateSizesAndMaxFrameSizes (arity,rule_type_state_p,&maxasize,&asize,&bsize);

	if ((fun_def->sdef_mark & SDEF_USED_CURRIED_MASK) || DoDescriptors || DoParallel)
		GenArrayFunctionDescriptor (fun_def,&CurrentAltLabel,arity);

	if (DoTimeProfiling)
		GenPB (fun_def->sdef_ident->ident_name);

	if (fun_def->sdef_mark & SDEF_USED_CURRIED_MASK){
		struct label i_label;

		if (fun_def->sdef_mark & (SDEF_INSTANCE_RULE_WITH_FIELD_P | SDEF_RULE_INSTANCE_RULE_P) &&
			generate_unboxed_record_instance_entry (fun_def,rule_type_state_p,&i_label))
		{
			ApplyInstanceEntry (rule_type_state_p,arity,&ealab,&i_label,!(fun_def->sdef_mark & SDEF_USED_LAZILY_MASK));
		} else
			ApplyEntry (rule_type_state_p,arity,&ealab,!(fun_def->sdef_mark & SDEF_USED_LAZILY_MASK));
	}

	if (fun_def->sdef_mark & SDEF_USED_LAZILY_MASK)
		NodeEntry (rule_type_state_p,arity,&ealab,fun_def);

	EvalArgsEntry (rule_type_state_p,fun_def,maxasize,&ealab,n_result_nodes_on_a_stack);
	
	*a_size_p=asize;
	*b_size_p=bsize;
}

#if STRICT_LISTS
extern PolyList unboxed_record_cons_list,unboxed_record_decons_list;

void GenerateCodeForLazyUnboxedRecordListFunctions (void)
{
	PolyList unboxed_record_cons_elem,unboxed_record_decons_elem;
	
	for_l (unboxed_record_cons_elem,unboxed_record_cons_list,pl_next){
		SymbDef fun_def;
		
		fun_def=unboxed_record_cons_elem->pl_elem;
		if (fun_def->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK)){
			int a_size,b_size;
			TypeArgs type_node_arguments_p;
			LabDef unboxed_record_cons_lab;
			int tail_strict;
					
			type_node_arguments_p=fun_def->sdef_rule_type->rule_type_rule->type_alt_lhs->type_node_arguments;
			tail_strict=type_node_arguments_p->type_arg_next->type_arg_node->type_node_symbol->symb_tail_strictness;
			
			if (ExportLocalLabels){
				unboxed_record_cons_lab.lab_mod=CurrentModule;
				unboxed_record_cons_lab.lab_symbol=type_node_arguments_p->type_arg_node->type_node_symbol->symb_def;
				unboxed_record_cons_lab.lab_issymbol=True;
			} else {
				unboxed_record_cons_lab.lab_name=type_node_arguments_p->type_arg_node->type_node_symbol->symb_def->sdef_ident->ident_name;
				unboxed_record_cons_lab.lab_issymbol=False;			
			}
			unboxed_record_cons_lab.lab_pref=tail_strict ? "r_Cons#!" : "r_Cons#";
			unboxed_record_cons_lab.lab_post='\0';
			
			GenUnboxedRecordConsApplyAndNodeEntries (fun_def,&unboxed_record_cons_lab,&a_size,&b_size);

			GenFillR (&unboxed_record_cons_lab,a_size,b_size,a_size,0,0,ReleaseAndFill,True);

			GenRtn (1,0,OnAState);
			
			if (DoTimeProfiling)
				GenPE();
		}
	}

	for_l (unboxed_record_decons_elem,unboxed_record_decons_list,pl_next){
		SymbDef fun_def;
		
		fun_def=unboxed_record_decons_elem->pl_elem;
		if (fun_def->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK)){
			int a_size,b_size;
			StateP result_state_p;

			GenUnboxedRecordDeconsApplyAndNodeEntries (fun_def,&a_size,&b_size);
			
			result_state_p=&fun_def->sdef_rule_type->rule_type_state_p[-1];

			DetermineSizeOfState (*result_state_p,&a_size,&b_size);
			
			if (b_size==0)
				GenReplArgs (a_size,a_size);
			else
				GenReplRArgs (a_size,b_size);
					
			GenRtn (a_size,b_size,*result_state_p);
			
			if (DoTimeProfiling)
				GenPE();
		}
	}
}
#endif

extern PolyList UserDefinedArrayFunctions;

void GenerateCodeForLazyArrayFunctionEntries (void)
{
	PolyList next_fun;
	
	for (next_fun = UserDefinedArrayFunctions; next_fun; next_fun = next_fun -> pl_next){
		SymbDef arr_fun_def;
		
		arr_fun_def = ((Symbol)next_fun->pl_elem)->symb_def;

		if (arr_fun_def ->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_CURRIED_MASK)){
			int a_size,b_size;
			
			GenUnboxedRecordApplyAndNodeEntries (arr_fun_def,&a_size,&b_size);

			CallArrayFunction (arr_fun_def,False,&arr_fun_def->sdef_rule_type->rule_type_state_p[-1]);

			if (DoTimeProfiling)
				GenPE();
		}
	}
}

void GenerateCodeForConstructorsAndRecords (Symbol symbols)
{
	Symbol symbol_p;
#if STRICT_LISTS
	PolyList unboxed_record_cons_element;
#endif
	
	for_l (symbol_p,symbols,symb_next){
		if (symbol_p->symb_kind==definition){
			SymbDef def;
		
			def = symbol_p->symb_def;

			if (def->sdef_module==CurrentModule){
				if (def->sdef_kind==TYPE){
					ConstructorList alt;

					for_l (alt,def->sdef_type->type_constructors,cl_next)
						if (alt->cl_constructor->type_node_symbol->symb_def->sdef_arity!=0)
							break;
					
					if (alt==NULL){
						int constructor_n;
						
						constructor_n=0;
						for_l (alt,def->sdef_type->type_constructors,cl_next){
							GenConstructor0DescriptorAndExport (alt->cl_constructor->type_node_symbol->symb_def,constructor_n);
							++constructor_n;
						}
					} else {
						for_l (alt,def->sdef_type->type_constructors,cl_next){
							SymbDef constructor_def;

							constructor_def=alt->cl_constructor->type_node_symbol->symb_def;
							if (constructor_def->sdef_kind==CONSTRUCTOR && constructor_def->sdef_strict_constructor)
								GenerateLazyConstructorDescriptorAndFunctionForStrictConstructor (alt);
						}

						for_l (alt,def->sdef_type->type_constructors,cl_next){
							SymbDef constructor_def;

							constructor_def=alt->cl_constructor->type_node_symbol->symb_def;							
							if (constructor_def->sdef_kind==CONSTRUCTOR && constructor_def->sdef_strict_constructor)
								GenStrictConstructorDescriptor (constructor_def,alt->cl_state_p);
							else
								GenConstructorDescriptorAndExport (constructor_def);
						}
					}
				} else if (def->sdef_kind==RECORDTYPE){
					FieldList fields;
					int asize, bsize;
					ConstructorList constructor;

					constructor = def->sdef_type->type_constructors;
					if (!(def->sdef_isused || def->sdef_exported || (def->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_STRICTLY_MASK | SDEF_USED_CURRIED_MASK)))){
						for_l (fields,constructor->cl_fields,fl_next){
							SymbDef field_def;
							
							field_def=fields->fl_symbol->symb_def;
							if (field_def->sdef_isused || field_def->sdef_mark & (SDEF_USED_LAZILY_MASK | SDEF_USED_STRICTLY_MASK | SDEF_USED_CURRIED_MASK))
								break;
						}

						if (fields==NULL)
							continue;
					}

					DetermineSizeOfState (def->sdef_record_state, &asize, &bsize);

					GenRecordDescriptor (def);

					if (def->sdef_strict_constructor && (def->sdef_exported || (def->sdef_mark & SDEF_USED_LAZILY_MASK) || ExportLocalLabels))
						GenLazyRecordEntry (def);

					for_l (fields,constructor->cl_fields,fl_next)
						GenLazyFieldSelectorEntry (fields->fl_symbol->symb_def,def->sdef_record_state, asize, bsize);
				}
			}
		}
	}

#if STRICT_LISTS
	for_l (unboxed_record_cons_element,unboxed_record_cons_list,pl_next){
		SymbDef cons_instance_sdef,record_sdef;
		TypeArgs type_node_arguments_p;
		int tail_strict;
		
		cons_instance_sdef=unboxed_record_cons_element->pl_elem;
		type_node_arguments_p=cons_instance_sdef->sdef_rule_type->rule_type_rule->type_alt_lhs->type_node_arguments;
		record_sdef=type_node_arguments_p->type_arg_node->type_node_symbol->symb_def;
		tail_strict=type_node_arguments_p->type_arg_next->type_arg_node->type_node_symbol->symb_tail_strictness;
		
		GenUnboxedConsRecordDescriptor (record_sdef,tail_strict);
	}
#endif
}

Bool NodeEntry (StateS *const function_state_p,int arity,Label ealab,SymbDef rootsymb)
{
	Bool update_root_node;
	LabDef newealab,loclab,d_lab,n_lab,*ea_label_in_node_directive;

	ConvertSymbolToDandNLabel (&d_lab,&n_lab,rootsymb);

	d_lab.lab_post = n_lab.lab_post = CurrentAltLabel.lab_post;

	update_root_node = ! ExpectsResultNode (function_state_p[-1]);

	if (update_root_node && DoTimeProfiling && !function_called_only_curried_or_lazy_with_one_return)
		GenPD();

	if (rootsymb->sdef_calledwithrootnode){
		if (update_root_node){
			newealab = *ealab;
			newealab.lab_mod = CurrentModule;
			ea_label_in_node_directive=&newealab;
		} else
			ea_label_in_node_directive=ealab;
	} else if (rootsymb->sdef_returnsnode)
		ea_label_in_node_directive=&empty_lab;
	else
		ea_label_in_node_directive=NULL;

	GenNodeEntryDirective (arity,&d_lab,ea_label_in_node_directive);
	
	GenOAStackLayout (1);
	GenNodeEntryLabelDefinition (&n_lab);
	GenPushNode (ReduceError,arity);

	if (! update_root_node)
		return True;

	if (rootsymb->sdef_calledwithrootnode){
		MakeLabel (&loclab, m_symb, NewLabelNr++, no_pref);
		GenOAStackLayout (arity);
		if (DoTimeProfiling)
			GenPN();
		GenLabelDefinition (&loclab);
	}

	if (IsSimpleState (function_state_p[-1])){
		if (function_state_p[-1].state_kind==OnB){
#if SHARE_UPDATE_CODE
			int result,label_number;
#endif
			if (function_called_only_curried_or_lazy_with_one_return)
				return False;

			CallEvalArgsEntry (arity,function_state_p,0,ObjectSizes [function_state_p[-1].state_object],ealab);

#if SHARE_UPDATE_CODE
			result=get_label_number_from_result_state_database (type,1,&label_number);

			if (result==2){
				LabDef update_label;

				MakeLabel (&update_label,"u",label_number,no_pref);
				GenStackLayoutOfState (DemStackDir,0,ObjectSizes [function_state_p[-1].state_object],function_state_p[-1]);
				GenJmp (&update_label);
			} else {
				if (result==1){
					LabDef update_label;

					MakeLabel (&update_label,"u",label_number,no_pref);
					GenOStackLayoutOfState (0,ObjectSizes [function_state_p[-1].state_object],function_state_p[-1]);
					GenLabelDefinition (&update_label);
				}
#endif

			FillBasicFromB (function_state_p[-1].state_object, 0, 0, ReleaseAndFill);
			GenPopB (ObjectSizes [function_state_p[-1].state_object]);
			GenRtn (1,0,OnAState);

#if SHARE_UPDATE_CODE
			}
#endif
		} else if (function_state_p[-1].state_kind==StrictRedirection || function_state_p[-1].state_kind==LazyRedirection){
#ifdef JMP_UPD
			GenDAStackLayout (arity);
			GenJmpUpd (ealab);
#else
			CallEvalArgsEntry (arity,function_state_p,1,0,ealab);
			GenFillFromA (0, 1, ReleaseAndFill);
			GenPopA (1);
			GenRtn (1,0,OnAState);
#endif
		}
	} else {
		int asize, bsize;
#if SHARE_UPDATE_CODE
		int result,label_number;
#endif
		
		if (function_called_only_curried_or_lazy_with_one_return)
			return False;

		DetermineSizeOfState (function_state_p[-1], &asize, &bsize);
		CallEvalArgsEntry (arity,function_state_p,asize,bsize,ealab);

#if SHARE_UPDATE_CODE
		result=get_label_number_from_result_state_database (type,1,&label_number);

		if (result==2){
			LabDef update_label;

			MakeLabel (&update_label,"u",label_number,no_pref);
			GenStackLayoutOfState (DemStackDir,asize,bsize,function_state_p[-1]);
			GenJmp (&update_label);
		} else {
			if (result==1){
				LabDef update_label;

				MakeLabel (&update_label,"u",label_number,no_pref);
				GenOStackLayoutOfState (asize,bsize,function_state_p[-1]);
				GenLabelDefinition (&update_label);
			}
#endif

		switch (function_state_p[-1].state_type){
			case TupleState:
				BuildTuple (asize,bsize,asize,bsize,function_state_p[-1].state_arity,
					function_state_p[-1].state_tuple_arguments,asize, bsize, 0, ReleaseAndFill,False);
				GenPopA (asize);
				break;
			case RecordState:
				BuildRecord (function_state_p[-1].state_record_symbol,asize,bsize,asize,bsize,
								asize,bsize,0,ReleaseAndFill,False);
				GenPopA (asize);
				break;
			case ArrayState:
				GenFillArrayAndPop (1, ReleaseAndFill);
				break;
		}

		GenPopB (bsize);
		GenRtn (1,0,OnAState);
#if SHARE_UPDATE_CODE
		}
#endif
	}
	
	if (rootsymb->sdef_calledwithrootnode){
		GenOAStackLayout (arity + 1);
		GenLabelDefinition (&newealab);
		GenDAStackLayout (arity);
		if (DoTimeProfiling)
			GenPN();
		GenJmp (&loclab);
	}

	return False;
}

Bool NodeEntryUnboxed (StateS *const function_state_p,NodeP call_node_p,int args_a_size,int args_b_size,Label ealab,SymbDef rootsymb)
{
	Bool update_root_node;
	LabDef newealab,loclab,d_lab,n_lab,*ea_label_in_node_directive;

	ConvertSymbolToDandNLabel (&d_lab,&n_lab,rootsymb);

	d_lab.lab_post = n_lab.lab_post = CurrentAltLabel.lab_post;

	update_root_node = ! ExpectsResultNode (function_state_p[-1]);

	if (update_root_node && DoTimeProfiling && !function_called_only_curried_or_lazy_with_one_return)
		GenPD();
	
	if (rootsymb->sdef_calledwithrootnode){
		/* jmp_eval_upd not yet implemented for closures with unboxed elements */
		if (args_b_size!=0){
			ea_label_in_node_directive=&empty_lab;
		} else {
			if (update_root_node){
				newealab = *ealab;
				newealab.lab_mod = CurrentModule;
				ea_label_in_node_directive=&newealab;
			} else
				ea_label_in_node_directive=ealab;	
		}
	} else if (rootsymb->sdef_returnsnode)
		ea_label_in_node_directive=&empty_lab;
	else
		ea_label_in_node_directive=NULL;

	if (args_b_size!=0)
		GenNodeEntryDirectiveUnboxed (args_a_size,args_b_size,&d_lab,ea_label_in_node_directive);
	else
		GenNodeEntryDirective (args_a_size,&d_lab,ea_label_in_node_directive);
	
	GenOAStackLayout (1);
	GenNodeEntryLabelDefinition (&n_lab);
	if (args_b_size!=0)
		GenPushNodeU (ReduceError,args_a_size,args_b_size);
	else
		GenPushNode (ReduceError,args_a_size);

	if (! update_root_node)
		return True;

	if (args_b_size==0 && rootsymb->sdef_calledwithrootnode){
		MakeLabel (&loclab, m_symb, NewLabelNr++, no_pref);
		GenOAStackLayout (args_a_size);
		if (DoTimeProfiling)
			GenPN();
		GenLabelDefinition (&loclab);
	}

	if (IsSimpleState (function_state_p[-1])){
		if (function_state_p[-1].state_kind==OnB){
#	if SHARE_UPDATE_CODE
			int result,label_number;
#	endif
			if (function_called_only_curried_or_lazy_with_one_return)
				return False;

			CallEvalArgsEntryUnboxed (args_a_size,args_b_size,call_node_p->node_arguments,function_state_p,0,ObjectSizes [function_state_p[-1].state_object],ealab);

#	if SHARE_UPDATE_CODE
			result=get_label_number_from_result_state_database (type,1,&label_number);

			if (result==2){
				LabDef update_label;

				MakeLabel (&update_label,"u",label_number,no_pref);
				GenStackLayoutOfState (DemStackDir,0,ObjectSizes [function_state_p[-1].state_object],function_state_p[-1]);
				GenJmp (&update_label);
			} else {
				if (result==1){
					LabDef update_label;

					MakeLabel (&update_label,"u",label_number,no_pref);
					GenOStackLayoutOfState (0,ObjectSizes [function_state_p[-1].state_object],function_state_p[-1]);
					GenLabelDefinition (&update_label);
				}
#	endif

			FillBasicFromB (function_state_p[-1].state_object, 0, 0, ReleaseAndFill);
			GenPopB (ObjectSizes [function_state_p[-1].state_object]);
			GenRtn (1,0,OnAState);

#	if SHARE_UPDATE_CODE
			}
#	endif
		} else if (function_state_p[-1].state_kind==StrictRedirection || function_state_p[-1].state_kind==LazyRedirection){
#ifdef JMP_UPD
			if (args_b_size==0){
				GenDAStackLayout (args_a_size);
				GenJmpUpd (ealab);	
			} else {
#endif
			CallEvalArgsEntryUnboxed (args_a_size,args_b_size,call_node_p->node_arguments,function_state_p,1,0,ealab);
			GenFillFromA (0, 1, ReleaseAndFill);
			GenPopA (1);
			GenRtn (1,0,OnAState);
#ifdef JMP_UPD
			}
#endif
		}
	} else {
		int asize, bsize;
#	if SHARE_UPDATE_CODE
		int result,label_number;
#	endif
		
		if (function_called_only_curried_or_lazy_with_one_return)
			return False;

		DetermineSizeOfState (function_state_p[-1], &asize, &bsize);
		CallEvalArgsEntryUnboxed (args_a_size,args_b_size,call_node_p->node_arguments,function_state_p,asize,bsize,ealab);

#	if SHARE_UPDATE_CODE
		result=get_label_number_from_result_state_database (type,1,&label_number);

		if (result==2){
			LabDef update_label;

			MakeLabel (&update_label,"u",label_number,no_pref);
			GenStackLayoutOfState (DemStackDir,asize,bsize,function_state_p[-1]);
			GenJmp (&update_label);
		} else {
			if (result==1){
				LabDef update_label;

				MakeLabel (&update_label,"u",label_number,no_pref);
				GenOStackLayoutOfState (asize,bsize,function_state_p[-1]);
				GenLabelDefinition (&update_label);
			}
#	endif

		switch (function_state_p[-1].state_type){
			case TupleState:
				BuildTuple (asize,bsize,asize,bsize,function_state_p[-1].state_arity,
					function_state_p[-1].state_tuple_arguments,asize, bsize, 0, ReleaseAndFill,False);
				GenPopA (asize);
				break;
			case RecordState:
				BuildRecord (function_state_p[-1].state_record_symbol,asize,bsize,asize,bsize,
								asize,bsize,0,ReleaseAndFill,False);
				GenPopA (asize);
				break;
			case ArrayState:
				GenFillArrayAndPop (1, ReleaseAndFill);
				break;
		}

		GenPopB (bsize);
		GenRtn (1,0,OnAState);
#	if SHARE_UPDATE_CODE
		}
#	endif
	}

	if (args_b_size==0 && rootsymb->sdef_calledwithrootnode){
		GenOAStackLayout (args_a_size + 1);
		GenLabelDefinition (&newealab);
		GenDAStackLayout (args_a_size);
		if (DoTimeProfiling)
			GenPN();
		GenJmp (&loclab);
	}

	return False;
}

int generate_instance_entry (struct symbol_def *rule_sdef,struct state *function_state_p,struct label *i_label_p)
{
	struct symbol_def *dictionary_field;
	int dictionary_sdef_mark;
	
	if (rule_sdef->sdef_mark & SDEF_RULE_INSTANCE_RULE_P){
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance fused");
		if (rule_sdef->sdef_instance_rule->sdef_mark & SDEF_INSTANCE_RULE_WITH_FIELD_P){
			dictionary_field=rule_sdef->sdef_instance_rule->sdef_dictionary_field;
			dictionary_sdef_mark=dictionary_field->sdef_mark;
		} else {
			dictionary_sdef_mark=0;
		}
	} else {
		/* SDEF_INSTANCE_RULE_WITH_FIELD_P */
		if (DoDebug)
			FPrintF (OutFile, "\n||\tinstance");
		dictionary_field=rule_sdef->sdef_dictionary_field;
		dictionary_sdef_mark=dictionary_field->sdef_mark;
	}
	
	if (dictionary_sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE){
		struct state *member_state_p;
		int asize,bsize,function_updates_node;
		LabDef current_alt_label;

		function_updates_node = generate_instance_entry_arguments (dictionary_field,rule_sdef->sdef_arity,function_state_p,i_label_p,&asize,&bsize);

		member_state_p=dictionary_field->sdef_member_states_of_field;
		
		++member_state_p;

		GenDStackLayoutOfStates (asize+function_updates_node,bsize,rule_sdef->sdef_arity,function_state_p);

		current_alt_label = CurrentAltLabel;
		current_alt_label.lab_pref = s_pref;
		current_alt_label.lab_post = 0;
		if (rule_sdef->sdef_exported)
			current_alt_label.lab_mod = NULL;

		if (function_updates_node || EqualState (function_state_p[-1],member_state_p[-2]))
			GenJmp (&current_alt_label);
		else {
			int result_asize,result_bsize;
			
			GenJsr (&current_alt_label);

			DetermineSizeOfState (function_state_p[-1],&result_asize,&result_bsize);
			GenOStackLayoutOfState (result_asize,result_bsize,function_state_p[-1]);
			RedirectResultAndReturn (result_asize,result_bsize,result_asize,result_bsize,function_state_p[-1],member_state_p[-2],result_asize,result_bsize);
		}

		return 1;
	}
	return 0;
}

static void ApplyEntry2 (StateS *const function_state_p,int arity,Label ea_lab,int ea_label_follows);

void ApplyEntry (StateS *const function_state_p,int arity,Label ea_lab,int ea_label_follows)
{
	CurrentAltLabel.lab_pref = l_pref;

	if (arity==0){
		GenOAStackLayout (1);
		GenLabelDefinition (&CurrentAltLabel);
		GenHalt();
		return;
	}

	if (DoTimeProfiling){
		if ((!IsSimpleState (function_state_p[-1]) || function_state_p[-1].state_kind==OnB) && !function_called_only_curried_or_lazy_with_one_return)
			GenPLD();
		else
			GenPL();
	}

	if (arity>=2){
		if (IsSimpleState (function_state_p[-1])){
			if (function_state_p[-1].state_kind==OnB){
				LabDef a_lab;	
				
				a_lab=*ea_lab;
				a_lab.lab_pref="a";
				GenApplyEntryDirective (0,&a_lab);
			} else if (function_state_p[-1].state_kind==StrictRedirection || function_state_p[-1].state_kind==LazyRedirection){
				GenApplyEntryDirective (0,ea_lab);
			} else {
				GenApplyEntryDirective (arity,ea_lab);
			}
		} else {
			LabDef a_lab;
			
			a_lab=*ea_lab;
			a_lab.lab_pref="a";
			GenApplyEntryDirective (0,&a_lab);		
		}
	}

	ApplyEntry2 (function_state_p,arity,ea_lab,ea_label_follows);
}

void ApplyInstanceEntry (StateS *const function_state_p,int arity,Label ea_lab,struct label *i_label_p,int ea_label_follows)
{
	CurrentAltLabel.lab_pref = l_pref;

	if (arity==0){
		GenOAStackLayout (1);
		GenLabelDefinition (&CurrentAltLabel);
		GenHalt();
		return;
	}

	if (DoTimeProfiling){
		if ((!IsSimpleState (function_state_p[-1]) || function_state_p[-1].state_kind==OnB) && !function_called_only_curried_or_lazy_with_one_return)
			GenPLD();
		else
			GenPL();
	}

	if (arity>=2){
		if (!IsSimpleState (function_state_p[-1]) || function_state_p[-1].state_kind==OnB){
			LabDef a_lab;	
			
			a_lab=*ea_lab;
			a_lab.lab_pref="a";
			GenApplyInstanceEntryDirective (0,&a_lab,i_label_p);
		} else if (function_state_p[-1].state_kind==StrictRedirection || function_state_p[-1].state_kind==LazyRedirection){
			GenApplyInstanceEntryDirective (0,ea_lab,i_label_p);
		} else {
			GenApplyInstanceEntryDirective (arity,ea_lab,i_label_p);
		}
	} else {
		GenApplyInstanceEntryDirective (arity,NULL,i_label_p);	
	}

	ApplyEntry2 (function_state_p,arity,ea_lab,ea_label_follows);
}

static void ApplyEntry2 (StateS *const function_state_p,int arity,Label ea_lab,int ea_label_follows)
{
	GenOAStackLayout (2);
	GenLabelDefinition (&CurrentAltLabel);
	
	if (IsSimpleState (function_state_p[-1])){
		if (function_state_p[-1].state_kind==OnB){
#if SHARE_UPDATE_CODE
			int result,label_number;
#endif
			GenReplArgs (arity-1,arity-1);

#ifdef NEW_APPLY
			if (arity>=2){
				LabDef a_lab;	
				
				a_lab=*ea_lab;
				a_lab.lab_pref="a";
				GenOAStackLayout (arity);
				if (DoTimeProfiling)
					GenPN();
				GenLabelDefinition (&a_lab);
			}
#endif

			if (function_called_only_curried_or_lazy_with_one_return)
				return;

			CallEvalArgsEntry (arity,function_state_p,0,ObjectSizes [function_state_p[-1].state_object],ea_lab);

#if SHARE_UPDATE_CODE
			result=get_label_number_from_result_state_database (&function_state_p[-1],2,&label_number);
			if (result==2){
				LabDef update_label;

				MakeLabel (&update_label,"v",label_number,no_pref);
				GenStackLayoutOfState (DemStackDir,0,ObjectSizes [function_state_p[-1].state_object],function_state_p[-1]);
				GenJmp (&update_label);
			} else {
				if (result==1){
					LabDef update_label;

					MakeLabel (&update_label,"v",label_number,no_pref);
					GenOStackLayoutOfState (0,ObjectSizes [function_state_p[-1].state_object],function_state_p[-1]);
					GenLabelDefinition (&update_label);
				}
#endif

			BuildBasicFromB (function_state_p[-1].state_object,0);

			GenPopB (ObjectSizes [function_state_p[-1].state_object]);
			GenRtn (1,0,OnAState);
#if SHARE_UPDATE_CODE
			}
#endif
		} else if (function_state_p[-1].state_kind == StrictRedirection || function_state_p[-1].state_kind == LazyRedirection){
			GenReplArgs (arity-1, arity-1);
			if (!ea_label_follows)
				JmpEvalArgsEntry (arity,ea_lab);
		} else {
			GenPushArgs	(0, arity-1, arity-1);
			GenUpdateA	(arity, arity-1);	
			GenCreate	(-1);
			GenUpdateA	(0, arity+1);
			GenPopA		(1);
			if (!ea_label_follows)
				JmpEvalArgsEntry (arity+1,ea_lab);
		}
	} else {	
		int asize, bsize;
#if SHARE_UPDATE_CODE
		int result,label_number;
#endif
		GenReplArgs (arity-1, arity-1);

#ifdef NEW_APPLY
		if (arity>=2){
			LabDef a_lab;	
			
			a_lab=*ea_lab;
			a_lab.lab_pref="a";
			GenOAStackLayout (arity);
			if (DoTimeProfiling)
				GenPN();
			GenLabelDefinition (&a_lab);
		}
#endif

		if (function_called_only_curried_or_lazy_with_one_return)
			return;

		DetermineSizeOfState (function_state_p[-1], &asize, &bsize);
		CallEvalArgsEntry (arity,function_state_p,asize,bsize,ea_lab);

#if SHARE_UPDATE_CODE
		result=get_label_number_from_result_state_database (&function_state_p[-1],2,&label_number);
		if (result==2){
			LabDef update_label;

			MakeLabel (&update_label,"v",label_number,no_pref);
			GenStackLayoutOfState (DemStackDir,asize,bsize,function_state_p[-1]);
			GenJmp (&update_label);
		} else {
			if (result==1){
				LabDef update_label;

				MakeLabel (&update_label,"v",label_number,no_pref);
				GenOStackLayoutOfState (asize,bsize,function_state_p[-1]);
				GenLabelDefinition (&update_label);
			}
#endif
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
		GenRtn (1,0,OnAState);
#if SHARE_UPDATE_CODE
		}
#endif
	}
}

static void GenExternalLabel (int n_states,StateS *const function_state_p,int asp,int bsp,Label extlab)
{
	if (IsOnBStack (function_state_p[-1]) ||
		(IsSimpleState (function_state_p[-1]) && function_state_p[-1].state_kind==StrictRedirection))
		GenOStackLayoutOfStates (asp,bsp,n_states,function_state_p);
	else
		GenOStackLayoutOfStates (asp+1,bsp,n_states,function_state_p);
	GenLabelDefinition (extlab);
}

static void CoerceArgsFromExtToInt (int n_args,StateP ext_arg_state_p,StateP int_arg_state_p,int aindex,int bindex,
									int *asp_p,int *bsp_p,int *a_ind_p,int *b_ind_p)
{
	if (n_args>0){
		int asize, bsize;
	
		DetermineSizeOfState (*ext_arg_state_p,&asize,&bsize);
		
		CoerceArgsFromExtToInt (n_args-1,ext_arg_state_p+1,int_arg_state_p+1,aindex-asize, bindex-bsize,asp_p,bsp_p,a_ind_p,b_ind_p);
		
		CoerceArgumentUsingStackFrames (*int_arg_state_p,*ext_arg_state_p,aindex, bindex,asp_p,bsp_p,a_ind_p,b_ind_p,asize,bsize);
	}
}

/*
	When a function is exported to another module it may happen that
	the exported type differs from the type in the implementation module.
	This can be the case when one or more instances of abstract types
	are appearing in the exported type specification. In order to bring 
	the external calls into agreement with the inter call a special entry
	is generated.
*/

Bool ConvertExternalToInternalCall (int arity,StateS *const ext_function_state_p,StateS *const int_function_state_p,
									Bool skip_entry,int intasp,int intbsp,Label ealab,Label extlab,Bool root_node_needed)
{
	int arg_n,asp,bsp,asize,bsize,oldamax,oldbmax,a_ind,b_ind;
	Bool adjust_arg,adjust_result,all_args_lazy;
	
	adjust_arg = False;
	all_args_lazy = True;

	adjust_result =  ! EqualState (ext_function_state_p[-1],int_function_state_p[-1]);

	for (arg_n=0; arg_n<arity; ++arg_n){
		if (!IsLazyState (ext_function_state_p[arg_n]))
			all_args_lazy = False;

		if (!EqualState (ext_function_state_p[arg_n],int_function_state_p[arg_n]))
			adjust_arg = True;
	}

	asp=0;
	bsp=0;
	asize=0;
	bsize=0;
	a_ind=0;
	b_ind=0;

	if (! (adjust_arg || adjust_result))
		return True;

	if (all_args_lazy){
		if (adjust_result){
			if (skip_entry)
				JmpEvalArgsEntry (root_node_needed ? arity+1 : arity, ealab);
			
			if (DoTimeProfiling)
				GenPD();
			
			GenExternalLabel (arity,ext_function_state_p,arity,0,extlab);
			
			DetermineSizeOfState (int_function_state_p[-1], &asize, &bsize);
			CallEvalArgsEntry (root_node_needed ? arity+1 : arity,int_function_state_p,asize,bsize,ealab);
			RedirectResultAndReturn (asize,bsize,asize,bsize,int_function_state_p[-1],ext_function_state_p[-1],asize,bsize);
			return False;
		} else {
			GenExternalLabel (arity,ext_function_state_p,arity,0,extlab);
			
			if (DoTimeProfiling){
				GenPD();
				JmpEvalArgsEntry (root_node_needed ? arity+1 : arity,ealab);				
			}
			
			return False;
		}
	} else {
		if (skip_entry)
			JmpEvalArgsEntry (root_node_needed ? arity+1 : arity, ealab);

		if (adjust_arg){
			int maxasize;

			maxasize=0;
			
			for (arg_n=0; arg_n<arity; ++arg_n){
				AddSizeOfState (ext_function_state_p[arg_n],&asp,&bsp);
				AddStateSizeAndMaxFrameSize (int_function_state_p[arg_n],&maxasize,&asize,&bsize);
			}
/*			
			if (adjust_result && DoTimeProfiling)
				GenPD();
*/			
 			GenExternalLabel (arity,ext_function_state_p,asp,bsp,extlab);
			InitStackConversions (asp+maxasize+1, bsp+bsize+1, &oldamax, &oldbmax);
			
			CoerceArgsFromExtToInt (arity,ext_function_state_p,int_function_state_p,asp,bsp,&asp,&bsp,&a_ind,&b_ind);
			
			GenAStackConversions (asp,a_ind);
			GenBStackConversions (bsp,b_ind);

			ExitStackConversions (oldamax, oldbmax);
		} else {
/*
			if (adjust_result && DoTimeProfiling)
				GenPD();
*/
			GenExternalLabel (arity,ext_function_state_p,intasp,intbsp,extlab);
		}
		/* now we call the internal strict entry */
		
		GenDStackLayoutOfStates (root_node_needed ? intasp+1 : intasp,intbsp,arity,int_function_state_p);

		if (adjust_result){
			GenJsr (&CurrentAltLabel);
			
			DetermineSizeOfState (int_function_state_p[-1], &asize, &bsize);
			GenOStackLayoutOfState (asize, bsize, int_function_state_p[-1]);
			RedirectResultAndReturn (asize,bsize,asize,bsize,int_function_state_p[-1],ext_function_state_p[-1],asize,bsize);
		} else {
			if (DoTimeProfiling)
				GenPT();
			GenJmp (&CurrentAltLabel);
		}

		return False;
	}
}

static char g_pref[] = "g";

static void GenerateCodeForLazyTupleSelectorEntry (int argnr)
{
	LabDef sellab,easellab,descriptor_label;

	BuildLazyTupleSelectorLabel	(&sellab, MaxNodeArity, argnr);
#ifdef NEW_SELECTOR_DESCRIPTORS
	GenSelectorDescriptor (&sellab,argnr);
#else
	GenSelectorDescriptor (&sellab,g_pref);
#endif

	easellab = sellab;
	easellab.lab_pref = ea_pref;

	descriptor_label=sellab;
	descriptor_label.lab_pref=d_pref;
	GenNodeEntryDirectiveForLabelWithoutSymbol (-1,&descriptor_label,&easellab);

	GenOAStackLayout (1);
	GenLabelDefinition	(&sellab);
	GenPushNode			(ReduceError, 1);
	GenJsrEval			(0);
	GenGetNodeArity		(0);
	GenPushArgNr		(argnr);
	GenPushArgB			(0);
	GenJsrEval			(0);
	GenFillFromA		(0, 2, ReleaseAndFill);
	GenPopA				(2);
	GenRtn 				(1,0,OnAState);

#ifndef NEW_SELECTOR_DESCRIPTORS
	GenOAStackLayout (1);
	sellab.lab_pref = g_pref;
	GenLabelDefinition	(&sellab);
	GenGetNodeArity		(0);
	GenPushArgNr		(argnr);
	GenPushArgB			(0);
	GenUpdatePopA		(0, 1);
	GenRtn 				(1,0,OnAState);
#endif

	GenOAStackLayout (2);
	GenLabelDefinition	(&easellab);
	GenPushArg			(0,1,1);
	GenPushA			(2);
	GenKeep (1,0);
	GenFill (& ind_lab, -2, &indirection_lab, 2, PartialFill);
	GenKeep (1,0);
	GenUpdatePopA		(0, 1);
	GenJsrEval			(0);
	GenGetNodeArity		(0);
	GenPushArgNr		(argnr);
	GenPushArgB			(0);
	GenUpdatePopA		(0, 1);
	GenJmpEvalUpdate();
}

void GenerateCodeForLazyTupleSelectorEntries (Bool *selectors)
{
	int i;

	for (i = NrOfGlobalSelectors; i < MaxNodeArity; i++)
		if (selectors[i - NrOfGlobalSelectors])
			GenerateCodeForLazyTupleSelectorEntry (i+1);
}

#define allocate_function_state(arity) (((StateP)(CompAlloc (sizeof(StateS)*((arity)+1))))+1)

static StateP create_function_state_for_update_function (StateP record_state_p,int n_arguments)
{
	StateP function_state_p;
	int arg_n;

	function_state_p = allocate_function_state (n_arguments);

	for (arg_n=0; arg_n<n_arguments; ++arg_n)
		function_state_p[arg_n]=LazyState;

	function_state_p[-1]=*record_state_p;

	return function_state_p;
}

static StateP create_function_state_for_match_function (void)
{
	StateP function_state_p;

	function_state_p = allocate_function_state (1);

	function_state_p[0]=StrictState;	
	function_state_p[-1]=StrictState;
	
	return function_state_p;
}

int next_update_function_n,next_match_function_n;

ImpRuleP first_update_function,*update_function_p;

ImpRuleP create_simple_imp_rule (NodeP lhs_root,NodeP rhs_root,SymbDefP function_sdef)
{
	ImpRuleS *imp_rule;
	RuleAltS *rule_alt;

	rule_alt=CompAllocType (RuleAltS);
	rule_alt->alt_lhs_root=lhs_root;
	rule_alt->alt_lhs_defs=NULL;
	rule_alt->alt_rhs_root=rhs_root;
	rule_alt->alt_rhs_defs=NULL;
	rule_alt->alt_strict_node_ids=NULL;
	rule_alt->alt_next=NULL;
	rule_alt->alt_line=0;
	rule_alt->alt_kind=Contractum;

	imp_rule = CompAllocType (ImpRuleS);
	imp_rule->rule_alts = rule_alt;
	imp_rule->rule_root = lhs_root;
	imp_rule->rule_mark = 0;
	imp_rule->rule_type = NULL;

	function_sdef->sdef_rule=imp_rule;

	return imp_rule;
}

SymbDef CreateUpdateFunction (ArgS *record_arg,ArgS *first_field_arg,Node node
#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
	,int unbox_record
#endif
	)
{
	static char update_function_name[16];
	SymbDef update_function_sdef;
	Ident update_function_ident;
	Symbol update_function_symbol;
	ArgS *previous_arg,*arg;
	Node lhs_root,rhs_root;
	int n_arguments;
	ImpRuleS *update_imp_rule;
	StateS record_state,boxed_record_state;
	StateP strict_record_state_p;

	sprintf (update_function_name,"_upd%d",next_update_function_n);
	++next_update_function_n;
	
	n_arguments=node->node_arity;
	record_state=node->node_symbol->symb_def->sdef_record_state;
#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
	if (unbox_record)
		n_arguments=record_state.state_arity;
#endif
	if (node->node_symbol->symb_def->sdef_boxed_record){
		SetUnaryState (&boxed_record_state,StrictOnA,RecordObj);
		strict_record_state_p = &boxed_record_state;
	} else
		strict_record_state_p = &record_state;

	update_function_ident=PutStringInHashTable (update_function_name,SymbolIdTable);
	update_function_sdef=MakeNewSymbolDefinition (CurrentModule,update_function_ident,n_arguments,IMPRULE);

	update_function_sdef->sdef_number=next_def_number++;
	update_function_sdef->sdef_isused=True;
	update_function_sdef->sdef_mark |= SDEF_USED_LAZILY_MASK;

	if (node->node_symbol->symb_def->sdef_boxed_record){
		update_function_sdef->sdef_returnsnode=True;
		update_function_sdef->sdef_calledwithrootnode=True;
	} else {
		update_function_sdef->sdef_returnsnode=False;
		update_function_sdef->sdef_calledwithrootnode=False;
	}

	update_function_symbol=NewSymbol (definition);
	update_function_symbol->symb_def=update_function_sdef;

#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
	if (unbox_record){
		ArgS **lhs_new_fields_arg_p,**lhs_old_fields_arg_p,*lhs_new_fields_p,**rhs_arg_p;
		int field_number;
		
		lhs_root=NewNode (update_function_symbol,NULL,n_arguments);
		lhs_root->node_state=*strict_record_state_p;

		rhs_root=NewNode (node->node_symbol,NULL,n_arguments);
		rhs_root->node_state=*strict_record_state_p;
		rhs_root->node_number=0;

		lhs_old_fields_arg_p=&lhs_root->node_arguments;
		lhs_new_fields_arg_p=&lhs_new_fields_p;
		rhs_arg_p=&rhs_root->node_arguments;
		
		for (field_number=0; field_number<n_arguments; ++field_number){
			ArgS *rhs_arg,*lhs_arg;
			NodeId arg_node_id;
			StateS *state_p;

			state_p=&record_state.state_record_arguments [field_number];

			arg_node_id=NewNodeId (NULL);
			arg_node_id->nid_refcount=-2;
			
			lhs_arg=NewArgument (NewNodeIdNode (arg_node_id));
			lhs_arg->arg_state=LazyState;
			
			arg_node_id->nid_lhs_state_p_=&lhs_arg->arg_state;

			rhs_arg=NewArgument (NewNodeIdNode (arg_node_id));
			rhs_arg->arg_state=*state_p;

			*rhs_arg_p=rhs_arg;			
			rhs_arg_p=&rhs_arg->arg_next;

			if (first_field_arg==NULL || first_field_arg->arg_node->node_symbol->symb_def->sdef_sel_field_number!=field_number){
				*lhs_old_fields_arg_p=lhs_arg;
				lhs_old_fields_arg_p=&lhs_arg->arg_next;
			
				lhs_arg->arg_state=*state_p;
			} else {
				*lhs_new_fields_arg_p=lhs_arg;
				lhs_new_fields_arg_p=&lhs_arg->arg_next;

				first_field_arg=first_field_arg->arg_next;
			}
		}
		*lhs_old_fields_arg_p=lhs_new_fields_p;
		*lhs_new_fields_arg_p=NULL;
		*rhs_arg_p=NULL;
	} else
#endif
	{
		NodeId record_node_id;
		ArgS *lhs_record_arg,*rhs_record_arg,**lhs_arg_p,**rhs_arg_p;
		
		record_node_id=NewNodeId (NULL);
		record_node_id->nid_refcount=-1;
		
		lhs_record_arg=NewArgument (NewNodeIdNode (record_node_id));
		lhs_record_arg->arg_state=LazyState;

		record_node_id->nid_lhs_state_p_=&lhs_record_arg->arg_state;

		rhs_record_arg=NewArgument (NewNodeIdNode (record_node_id));
		rhs_record_arg->arg_state=record_state;

		lhs_root=NewNode (update_function_symbol,lhs_record_arg,n_arguments);
		lhs_root->node_state=*strict_record_state_p;

		rhs_root=NewUpdateNode (node->node_symbol,rhs_record_arg,n_arguments);
		rhs_root->node_state=*strict_record_state_p;
		rhs_root->node_number=0;

		lhs_arg_p=&lhs_record_arg->arg_next;
		rhs_arg_p=&rhs_record_arg->arg_next;
		
		previous_arg=record_arg;
		for_l (arg,first_field_arg,arg_next){
			ArgS *rhs_arg,*lhs_arg,*field_value_arg;
			NodeId arg_node_id;
			int field_number;
			Node field_node;
			StateS *state_p;

			field_node=arg->arg_node;
			field_number=field_node->node_symbol->symb_def->sdef_sel_field_number;

			arg_node_id=NewNodeId (NULL);
			arg_node_id->nid_refcount=-2;
			
			lhs_arg=NewArgument (NewNodeIdNode (arg_node_id));
			lhs_arg->arg_state=LazyState;

			arg_node_id->nid_lhs_state_p_=&lhs_arg->arg_state;

			field_value_arg=NewArgument (NewNodeIdNode (arg_node_id));
			state_p=&record_state.state_record_arguments [field_number];
			field_value_arg->arg_state=*state_p;

			rhs_arg=NewArgument (NewSelectorNode (field_node->node_symbol,field_value_arg,1));
			rhs_arg->arg_state=*state_p;
			
			*lhs_arg_p=lhs_arg;
			*rhs_arg_p=rhs_arg;
			
			lhs_arg_p=&lhs_arg->arg_next;
			rhs_arg_p=&rhs_arg->arg_next;
#if !UNBOX_UPDATE_FUNCTION_ARGUMENTS
			field_node->node_arguments->arg_next=NULL;
			previous_arg->arg_next=arg;
#endif
			previous_arg=arg;
		}
#if !UNBOX_UPDATE_FUNCTION_ARGUMENTS
		previous_arg->arg_next=NULL;
#endif	
		*lhs_arg_p=NULL;
		*rhs_arg_p=NULL;
	}

	update_imp_rule=create_simple_imp_rule (lhs_root,rhs_root,update_function_sdef);

	update_imp_rule->rule_state_p = create_function_state_for_update_function (strict_record_state_p,n_arguments);

	*update_function_p=update_imp_rule;
	update_function_p=&update_imp_rule->rule_next;
	
	return update_function_sdef;
}

#define R4(r,f1,f2,f3,f4) (r).f1;(r).f2;(r).f3;(r).f4
#define U5(r,f1,f2,f3,f4,f5) (r)->f1;(r)->f2;(r)->f3;(r)->f4;(r)->f5

SymbDef create_select_function (Symbol selector_symbol,int selector_kind)
{
	static char select_function_name[16];
	SymbDef select_function_sdef;
	Ident select_function_ident;
	Symbol select_function_symbol;
	NodeP lhs_root,rhs_root;
	ImpRuleS *update_imp_rule;
	SymbDef selector_sdef;
	ArgP lhs_record_arg,rhs_record_arg;
	NodeIdP record_node_id;
	StateP tuple_state_arguments,function_state_p,record_state_p,arg_state_p;
	StateS selector_arg_state;
	int fieldnr;

	selector_sdef=selector_symbol->symb_def;
	
	sprintf (select_function_name,"_sel%d",next_update_function_n);
	++next_update_function_n;

	select_function_ident=PutStringInHashTable (select_function_name,SymbolIdTable);
	select_function_sdef=MakeNewSymbolDefinition (CurrentModule,select_function_ident,1,IMPRULE);

	U5 (select_function_sdef,	sdef_number=next_def_number++,
								sdef_isused=True,
								sdef_mark |= SDEF_USED_LAZILY_MASK,
								sdef_returnsnode=False,
								sdef_calledwithrootnode=False);
	
	select_function_symbol=NewSymbol (definition);
	select_function_symbol->symb_def=select_function_sdef;

	record_state_p=&selector_sdef->sdef_type->type_lhs->ft_symbol->symb_def->sdef_record_state;
	fieldnr = selector_sdef->sdef_sel_field_number;
	
	record_node_id=NewNodeId (NULL);
	record_node_id->nid_refcount=-2;

	tuple_state_arguments=CompAllocArray (2,StateS);
	tuple_state_arguments[0]=record_state_p->state_record_arguments[fieldnr];
	if (selector_kind<SELECTOR_L){
		tuple_state_arguments[1]=*record_state_p;
		arg_state_p=record_state_p;
	} else {
		StateP selector_arg_tuple_args;
		
		tuple_state_arguments[1]=StrictState;

		selector_arg_tuple_args=CompAllocArray (2,StateS);
		selector_arg_tuple_args[0]=*record_state_p;
		selector_arg_tuple_args[1]=StrictState;
		
		selector_arg_state.state_type=TupleState;
		selector_arg_state.state_arity=2;
		selector_arg_state.state_mark=0;
		selector_arg_state.state_tuple_arguments=selector_arg_tuple_args;
		arg_state_p=&selector_arg_state;
	}
	
	lhs_record_arg=NewArgument (NewNodeIdNode (record_node_id));
	lhs_record_arg->arg_state=*arg_state_p;

	record_node_id->nid_lhs_state_p_=&lhs_record_arg->arg_state;

	lhs_root=NewNode (select_function_symbol,lhs_record_arg,1);
	R4 (lhs_root->node_state,	state_type=TupleState,
								state_arity=2,
								state_mark=0,
								state_tuple_arguments=tuple_state_arguments);

	rhs_record_arg=NewArgument (NewNodeIdNode (record_node_id));
	rhs_record_arg->arg_state=*arg_state_p;

	rhs_root=NewSelectorNode (selector_symbol,rhs_record_arg,selector_kind);
	
	R4 (rhs_root->node_state,	state_type=TupleState,
								state_arity=2,
								state_mark=0,
								state_tuple_arguments=tuple_state_arguments);
			
	rhs_root->node_number=0;

	update_imp_rule=create_simple_imp_rule (lhs_root,rhs_root,select_function_sdef);

	function_state_p = allocate_function_state (1);
	function_state_p[0]=*arg_state_p;

	R4 (function_state_p[-1],	state_type=TupleState,
								state_arity=2,
								state_mark=0,
								state_tuple_arguments=tuple_state_arguments);

	update_imp_rule->rule_state_p=function_state_p;

	*update_function_p=update_imp_rule;
	update_function_p=&update_imp_rule->rule_next;
	
	return select_function_sdef;
}

static SymbDef create_match_function_sdef (void)
{
	char match_function_name[16];
	Ident match_function_ident;
	SymbDef match_function_sdef;

	sprintf (match_function_name,"_match%d",next_match_function_n);
	++next_match_function_n;

	match_function_ident=PutStringInHashTable (match_function_name,SymbolIdTable);
	match_function_sdef=MakeNewSymbolDefinition (CurrentModule,match_function_ident,1,IMPRULE);

	U5 (match_function_sdef,	sdef_number=next_def_number++,
								sdef_isused=True,
								sdef_mark |= SDEF_USED_LAZILY_MASK,
								sdef_returnsnode=True,
								sdef_calledwithrootnode=True);

	return match_function_sdef;
}

SymbDef create_match_function (SymbolP constructor_symbol,int result_arity,int n_dictionaries,int strict_constructor)
{
	SymbDef match_function_sdef;
	Symbol match_function_symbol;
	struct arg *lhs_function_arg;
	int n;
	struct node *lhs_root,*rhs_root,*switch_node,*case_node;
	ImpRuleS *match_imp_rule;
	struct node_id *constructor_node_node_id;

	match_function_sdef=create_match_function_sdef();

	match_function_symbol=NewSymbol (definition);
	match_function_symbol->symb_def=match_function_sdef;

	constructor_node_node_id=NewNodeId (NULL);
	constructor_node_node_id->nid_refcount=-2;
	constructor_node_node_id->nid_node=NULL;

	if (strict_constructor || n_dictionaries!=0){
		struct arg **rhs_arg_p;
		StateP constructor_arg_state_p;
#if STRICT_LISTS
		StateS head_and_tail_states[2];
#endif
		struct node *push_node;
		NodeIdListElementP *last_node_id_p;
		ArgP arg1,arg2;

		if (strict_constructor){
#if STRICT_LISTS
			if (constructor_symbol->symb_kind==cons_symb && (constructor_symbol->symb_head_strictness>1 || constructor_symbol->symb_tail_strictness)){
				if (constructor_symbol->symb_head_strictness>1){
					if (constructor_symbol->symb_head_strictness==4)
						head_and_tail_states[0]=*constructor_symbol->symb_state_p;
					else
						head_and_tail_states[0]=StrictState;
				} else
					head_and_tail_states[0]=LazyState;
				
				if (constructor_symbol->symb_tail_strictness)
					head_and_tail_states[1]=StrictState;
				else
					head_and_tail_states[1]=LazyState;
				
				constructor_arg_state_p=head_and_tail_states;
			} else
#endif
			constructor_arg_state_p=constructor_symbol->symb_def->sdef_constructor->cl_state_p;
		}

		rhs_root=NewNode (TupleSymbol,NULL,result_arity);
		rhs_root->node_state=StrictState;
		rhs_arg_p=&rhs_root->node_arguments;

		arg2=NewArgument (rhs_root);
		arg1=NewArgument (NewNodeIdNode (constructor_node_node_id));
		arg1->arg_next=arg2;

		push_node=CompAllocType (NodeS);

		push_node->node_kind=PushNode;
		push_node->node_arity=result_arity+n_dictionaries;
		push_node->node_arguments=arg1;
		push_node->node_push_symbol=constructor_symbol;
		push_node->node_number=0;	/* if !=0 then unique */

		last_node_id_p=&push_node->node_node_ids;

		if (strict_constructor){
			for (n=0; n<n_dictionaries; ++n){
				struct node_id *arg_node_id;

				arg_node_id=NewNodeId (NULL);
				arg_node_id->nid_refcount=-1;
				arg_node_id->nid_lhs_state_p_=constructor_arg_state_p;

				*last_node_id_p=CompAllocType (NodeIdListElementS);
				(*last_node_id_p)->nidl_node_id=arg_node_id;
				last_node_id_p=&(*last_node_id_p)->nidl_next;

				++constructor_arg_state_p;
			}

			for (n=0; n<result_arity; ++n){
				struct arg *rhs_arg;
				struct node_id *arg_node_id;

				arg_node_id=NewNodeId (NULL);
				arg_node_id->nid_refcount=-2;
				arg_node_id->nid_lhs_state_p_=constructor_arg_state_p;

				rhs_arg=NewArgument (NewNodeIdNode (arg_node_id));
				rhs_arg->arg_state=LazyState;

				*last_node_id_p=CompAllocType (NodeIdListElementS);
				(*last_node_id_p)->nidl_node_id=arg_node_id;
				last_node_id_p=&(*last_node_id_p)->nidl_next;

				*rhs_arg_p=rhs_arg;
				rhs_arg_p=&rhs_arg->arg_next;

				++constructor_arg_state_p;
			}
		} else {
			for (n=0; n<n_dictionaries; ++n){
				struct node_id *arg_node_id;

				arg_node_id=NewNodeId (NULL);
				arg_node_id->nid_refcount=-1;
				arg_node_id->nid_lhs_state_p_=&LazyState;

				*last_node_id_p=CompAllocType (NodeIdListElementS);
				(*last_node_id_p)->nidl_node_id=arg_node_id;
				last_node_id_p=&(*last_node_id_p)->nidl_next;
			}

			for (n=0; n<result_arity; ++n){
				struct arg *rhs_arg;
				struct node_id *arg_node_id;

				arg_node_id=NewNodeId (NULL);
				arg_node_id->nid_refcount=-2;
				arg_node_id->nid_lhs_state_p_=&LazyState;

				rhs_arg=NewArgument (NewNodeIdNode (arg_node_id));
				rhs_arg->arg_state=LazyState;

				*last_node_id_p=CompAllocType (NodeIdListElementS);
				(*last_node_id_p)->nidl_node_id=arg_node_id;
				last_node_id_p=&(*last_node_id_p)->nidl_next;

				*rhs_arg_p=rhs_arg;
				rhs_arg_p=&rhs_arg->arg_next;
			}
		}

		*rhs_arg_p=NULL;
		*last_node_id_p=NULL;

		lhs_function_arg=NewArgument (NewNodeIdNode (constructor_node_node_id));
		lhs_function_arg->arg_state=StrictState;
		
		rhs_root=push_node;

		constructor_node_node_id->nid_lhs_state_p_=&lhs_function_arg->arg_state;
	} else {
		lhs_function_arg=NewArgument (NewNodeIdNode (constructor_node_node_id));
		lhs_function_arg->arg_state=StrictState;

		rhs_root=NewNodeIdNode (constructor_node_node_id);

		--constructor_node_node_id->nid_refcount;
	}

	case_node=CompAllocType (NodeS);
	
	case_node->node_kind=CaseNode;
	case_node->node_symbol=constructor_symbol;
	case_node->node_arity=result_arity;
	case_node->node_arguments=NewArgument (rhs_root);
	case_node->node_su.su_u.u_case=CompAllocType (CaseNodeContentsS);
	case_node->node_strict_node_ids=NULL;
	case_node->node_node_id_ref_counts=NULL;
	case_node->node_node_defs=NULL;
	case_node->node_strict_node_ids=NULL;

	switch_node=CompAllocType (NodeS);

	switch_node->node_kind=SwitchNode;
	switch_node->node_node_id=constructor_node_node_id;
	switch_node->node_arity=1;
	switch_node->node_arguments=NewArgument (case_node);
	switch_node->node_state=lhs_function_arg->arg_state;
	
	constructor_node_node_id->nid_lhs_state_p_=&lhs_function_arg->arg_state;

	rhs_root=switch_node;

	lhs_root=NewNode (match_function_symbol,lhs_function_arg,1);
	lhs_root->node_state=StrictState;

	rhs_root->node_state=StrictState;
	rhs_root->node_number=0;
	
	match_imp_rule=create_simple_imp_rule (lhs_root,rhs_root,match_function_sdef);

	match_imp_rule->rule_state_p = create_function_state_for_match_function();

	*update_function_p=match_imp_rule;
	update_function_p=&match_imp_rule->rule_next;
	
	return match_function_sdef;
}

SymbDef create_select_and_match_function (SymbolP constructor_symbol,int n_dictionaries,int strict_constructor)
{
	SymbDef match_function_sdef;
	Symbol match_function_symbol;
	ArgP lhs_function_arg;
	NodeP lhs_root,rhs_root,constructor_node;
	NodeIdP node_id;
	ImpRuleS *match_imp_rule;
	struct node *push_node,*case_node,*switch_node;
	struct node_id *constructor_node_node_id;
	ArgP arg1,arg2;

	match_function_sdef=create_match_function_sdef();

	match_function_symbol=NewSymbol (definition);
	match_function_symbol->symb_def=match_function_sdef;

	node_id=NewNodeId (NULL);
	node_id->nid_refcount=-2;

	constructor_node_node_id=NewNodeId (NULL);
	constructor_node_node_id->nid_refcount=-2;
	constructor_node_node_id->nid_node=NULL;

	rhs_root=NewNodeIdNode (node_id);
	rhs_root->node_state=StrictState;
	rhs_root->node_number=0;

	arg2=NewArgument (rhs_root);
	arg1=NewArgument (NewNodeIdNode (constructor_node_node_id));
	arg1->arg_next=arg2;

	push_node=CompAllocType (NodeS);

	push_node->node_kind=PushNode;
	push_node->node_arity=1+n_dictionaries;
	push_node->node_arguments=arg1;
	push_node->node_push_symbol=constructor_symbol;
	push_node->node_number=0;	/* if !=0 then unique */

	if (n_dictionaries==0){
		push_node->node_node_ids=CompAllocType (NodeIdListElementS);
		push_node->node_node_ids->nidl_node_id=node_id;
		push_node->node_node_ids->nidl_next=NULL;
	} else {
		NodeIdListElementP *last_node_id_p;
		int n;

		last_node_id_p=&push_node->node_node_ids;

		for (n=0; n<n_dictionaries; ++n){
			struct node_id *arg_node_id;

			arg_node_id=NewNodeId (NULL);
			arg_node_id->nid_refcount=-1;
			if (strict_constructor)
				arg_node_id->nid_lhs_state_p_=&constructor_symbol->symb_def->sdef_constructor->cl_state_p[n];
			else
				arg_node_id->nid_lhs_state_p_=&LazyState;

			*last_node_id_p=CompAllocType (NodeIdListElementS);
			(*last_node_id_p)->nidl_node_id=arg_node_id;
			last_node_id_p=&(*last_node_id_p)->nidl_next;
		}

		*last_node_id_p=CompAllocType (NodeIdListElementS);
		(*last_node_id_p)->nidl_node_id=node_id;
		(*last_node_id_p)->nidl_next=NULL;
	}

	lhs_function_arg=NewArgument (NewNodeIdNode (constructor_node_node_id));
	lhs_function_arg->arg_state=StrictState;

	if (strict_constructor)
		node_id->nid_lhs_state_p_=&constructor_symbol->symb_def->sdef_constructor->cl_state_p[n_dictionaries];
	else
		node_id->nid_lhs_state_p_=&LazyState;
	
	rhs_root=push_node;

	constructor_node_node_id->nid_lhs_state_p_=&lhs_function_arg->arg_state;

	case_node=CompAllocType (NodeS);
	
	case_node->node_kind=CaseNode;
	case_node->node_symbol=constructor_symbol;
	case_node->node_arity=1;
	case_node->node_arguments=NewArgument (push_node);
	case_node->node_su.su_u.u_case=CompAllocType (CaseNodeContentsS);
	case_node->node_strict_node_ids=NULL;
	case_node->node_node_id_ref_counts=NULL;
	case_node->node_node_defs=NULL;
	case_node->node_strict_node_ids=NULL;

	switch_node=CompAllocType (NodeS);

	switch_node->node_kind=SwitchNode;
	switch_node->node_node_id=constructor_node_node_id;
	switch_node->node_arity=1;
	switch_node->node_arguments=NewArgument (case_node);
	switch_node->node_state=lhs_function_arg->arg_state;
	
	constructor_node_node_id->nid_lhs_state_p_=&lhs_function_arg->arg_state;

	lhs_root=NewNode (match_function_symbol,lhs_function_arg,1);
	lhs_root->node_state=StrictState;
	
	rhs_root = switch_node;
	
	match_imp_rule=create_simple_imp_rule (lhs_root,rhs_root,match_function_sdef);

	match_imp_rule->rule_state_p = create_function_state_for_match_function();

	{
		TypeNode type_node;
		StateP lhs_type_root_state_p;
			
		if (n_dictionaries==0)
			type_node=constructor_symbol->symb_def->sdef_constructor->cl_constructor->type_node_arguments->type_arg_node;
		else {
			struct type_arg *type_arg;
			int n;
			
			type_arg=constructor_symbol->symb_def->sdef_constructor->cl_constructor->type_node_arguments;
			for (n=0; n<n_dictionaries; ++n)
				type_arg=type_arg->type_arg_next;
			type_node=type_arg->type_arg_node;			
		}
		
		lhs_type_root_state_p=&match_imp_rule->rule_state_p[-1];
		if (!(type_node->type_node_is_var || type_node->type_node_symbol->symb_kind==apply_symb)
			&& !IsLazyState (constructor_symbol->symb_def->sdef_constructor->cl_state_p[n_dictionaries]))
		{
			*lhs_type_root_state_p=constructor_symbol->symb_def->sdef_constructor->cl_state_p[n_dictionaries];
		} else
			lhs_type_root_state_p->state_kind=StrictRedirection;
		lhs_root->node_state=*lhs_type_root_state_p;
		
		if (IsSimpleState (*lhs_type_root_state_p)){
			if (lhs_type_root_state_p->state_kind==OnA || lhs_type_root_state_p->state_kind==StrictOnA){
				match_function_sdef->sdef_calledwithrootnode = True;
				match_function_sdef->sdef_returnsnode = True;
			} else if (lhs_type_root_state_p->state_kind==StrictRedirection){
				match_function_sdef->sdef_calledwithrootnode = False;
				match_function_sdef->sdef_returnsnode = True;
			} else {
				match_function_sdef->sdef_calledwithrootnode = False;
				match_function_sdef->sdef_returnsnode = False;
			}
		} else {
			match_function_sdef->sdef_calledwithrootnode = False;
			match_function_sdef->sdef_returnsnode = False;
		}
	}

	*update_function_p=match_imp_rule;
	update_function_p=&match_imp_rule->rule_next;
		
	return match_function_sdef;
}

struct update {
	int a_from_offset;
	int a_to_offset;
	int a_size;
	int b_from_offset;
	int b_to_offset;
	int b_size;
};

#if BIND_UNBOXED_LHS_TUPLE_AND_RECORD_ARGUMENTS_IN_BIND_ARGUMENTS /* added 9-4-1999 */
void bind_tuple_and_record_arguments (ArgP arguments,NodeId tuple_node_id,int a_offset,int b_offset,
									  NodeIdListElementS ***a_node_ids_h,NodeIdListElementS ***b_node_ids_h)
{
	NodeIdListElementS **a_node_ids_p,**b_node_ids_p;
	ArgP arg_p;
	
	a_node_ids_p=*a_node_ids_h;
	b_node_ids_p=*b_node_ids_h;
	
	for_l (arg_p,arguments,arg_next){
		if (arg_p->arg_node->node_kind==NodeIdNode){
			struct node_id *node_id;
			
			node_id=arg_p->arg_node->node_node_id;

			if (tuple_node_id!=NULL){
				node_id->nid_mark |= NID_STRICT_LHS_TUPLE_ELEMENT_MASK;
				node_id->nid_lhs_tuple_node_id_=tuple_node_id;
			}

			node_id->nid_a_index_ = a_offset;
 			node_id->nid_b_index_ = b_offset;

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			node_id->nid_mark2 |= NID_LHS_PUSHED;
			node_id->nid_state = *node_id->nid_lhs_state_p;
#endif 			
			if (IsSimpleState (arg_p->arg_state)){
				if (arg_p->arg_state.state_kind==OnB){
					struct node_id_list_element *new_p_node_id;
					
					new_p_node_id=CompAllocType (struct node_id_list_element);
					new_p_node_id->nidl_node_id=node_id;

					*b_node_ids_p=new_p_node_id;
					b_node_ids_p=&new_p_node_id->nidl_next;
				} else {
					struct node_id_list_element *new_p_node_id;
					
					new_p_node_id=CompAllocType (struct node_id_list_element);
					new_p_node_id->nidl_node_id=node_id;

					*a_node_ids_p=new_p_node_id;
					a_node_ids_p=&new_p_node_id->nidl_next;
				}
			} else {
				if (node_id->nid_node==NULL){
					int asize,bsize;

					DetermineSizeOfState (arg_p->arg_state, &asize, &bsize);
					
					if (asize!=0){
						struct node_id_list_element *new_p_node_id;
						
						new_p_node_id=CompAllocType (struct node_id_list_element);
						new_p_node_id->nidl_node_id=node_id;

						*a_node_ids_p=new_p_node_id;
						a_node_ids_p=&new_p_node_id->nidl_next;
					}
					
					if (bsize!=0){
						struct node_id_list_element *new_p_node_id;

						new_p_node_id=CompAllocType (struct node_id_list_element);
						new_p_node_id->nidl_node_id=node_id;

						*b_node_ids_p=new_p_node_id;
						b_node_ids_p=&new_p_node_id->nidl_next;
					}
				} else {
					NodeIdListElementS **a_node_ids_p_c,**b_node_ids_p_c;
						
					a_node_ids_p_c=a_node_ids_p;
					b_node_ids_p_c=b_node_ids_p;
					
					bind_tuple_and_record_arguments (node_id->nid_node->node_arguments,node_id,a_offset,b_offset,&a_node_ids_p_c,&b_node_ids_p_c);
					
					a_node_ids_p=a_node_ids_p_c;
					b_node_ids_p=b_node_ids_p_c;
				}
			}
 		}
		else if (!IsSimpleState (arg_p->arg_state)){
			NodeIdListElementS **a_node_ids_p_c,**b_node_ids_p_c;
			
			a_node_ids_p_c=a_node_ids_p;
			b_node_ids_p_c=b_node_ids_p;
			
			bind_tuple_and_record_arguments (arg_p->arg_node->node_arguments,tuple_node_id /* !!!, not NULL */,a_offset,b_offset,&a_node_ids_p_c,&b_node_ids_p_c);
			
			a_node_ids_p=a_node_ids_p_c;
			b_node_ids_p=b_node_ids_p_c;			
		}

		if (IsSimpleState (arg_p->arg_state)){
			if (arg_p->arg_state.state_kind==OnB)
				b_offset -= ObjectSizes [arg_p->arg_state.state_object];
			else
				a_offset -= SizeOfAStackElem;
		} else {
			int asize,bsize;

			DetermineSizeOfState (arg_p->arg_state, &asize, &bsize);
			a_offset -= asize;
			b_offset -= bsize;
		}
	}

	*a_node_ids_h=a_node_ids_p;
	*b_node_ids_h=b_node_ids_p;
}

#else
static void set_lhs_tuple_node_ids (ArgS *args,NodeId node_id)
{
	ArgS *arg;
	
	for_l (arg,args,arg_next){
		Node arg_node;
		
		arg_node=arg->arg_node;
		if (arg_node->node_kind==NodeIdNode){
			arg_node->node_node_id->nid_mark |= NID_STRICT_LHS_TUPLE_ELEMENT_MASK;
			arg_node->node_node_id->nid_lhs_tuple_node_id_=node_id;
		} else
			set_lhs_tuple_node_ids (arg_node->node_arguments,node_id);
	}
}
#endif

void bind_arguments (ArgP arguments,int a_offset,int b_offset,AbNodeIdsP ab_node_ids_p)
{
	NodeIdListElementS **a_node_ids_p,**b_node_ids_p,*a_node_ids,*b_node_ids;
	ArgP arg_p;

	a_node_ids=ab_node_ids_p->a_node_ids;
	b_node_ids=ab_node_ids_p->b_node_ids;
	
	a_node_ids_p=&ab_node_ids_p->a_node_ids;
	b_node_ids_p=&ab_node_ids_p->b_node_ids;

	for_l (arg_p,arguments,arg_next){
		if (arg_p->arg_node->node_kind==NodeIdNode){
			struct node_id *node_id;
			
			node_id=arg_p->arg_node->node_node_id;
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			node_id->nid_mark2 |= NID_LHS_PUSHED;
			node_id->nid_state = *node_id->nid_lhs_state_p;
#endif
			node_id->nid_a_index_ = a_offset;
 			node_id->nid_b_index_ = b_offset;
 			
			if (IsSimpleState (arg_p->arg_state)){
				if (arg_p->arg_state.state_kind==OnB){
					struct node_id_list_element *new_p_node_id;
					
					new_p_node_id=CompAllocType (struct node_id_list_element);
					new_p_node_id->nidl_node_id=node_id;

					*b_node_ids_p=new_p_node_id;
					b_node_ids_p=&new_p_node_id->nidl_next;
				} else {
					struct node_id_list_element *new_p_node_id;
					
					new_p_node_id=CompAllocType (struct node_id_list_element);
					new_p_node_id->nidl_node_id=node_id;

					*a_node_ids_p=new_p_node_id;
					a_node_ids_p=&new_p_node_id->nidl_next;
				}
			} else {
				if (node_id->nid_node==NULL){
					int asize,bsize;

					DetermineSizeOfState (arg_p->arg_state, &asize, &bsize);
					
					if (asize!=0){
						struct node_id_list_element *new_p_node_id;
						
						new_p_node_id=CompAllocType (struct node_id_list_element);
						new_p_node_id->nidl_node_id=node_id;

						*a_node_ids_p=new_p_node_id;
						a_node_ids_p=&new_p_node_id->nidl_next;
					}
					
					if (bsize!=0){
						struct node_id_list_element *new_p_node_id;

						new_p_node_id=CompAllocType (struct node_id_list_element);
						new_p_node_id->nidl_node_id=node_id;

						*b_node_ids_p=new_p_node_id;
						b_node_ids_p=&new_p_node_id->nidl_next;
					}		
				} else
#if BIND_UNBOXED_LHS_TUPLE_AND_RECORD_ARGUMENTS_IN_BIND_ARGUMENTS /* added 9-4-1999 */
				{
					NodeIdListElementS **a_node_ids_p_c,**b_node_ids_p_c;
					
					a_node_ids_p_c=a_node_ids_p;
					b_node_ids_p_c=b_node_ids_p;
					
					bind_tuple_and_record_arguments (node_id->nid_node->node_arguments,node_id,a_offset,b_offset,&a_node_ids_p_c,&b_node_ids_p_c);
					
					a_node_ids_p=a_node_ids_p_c;
					b_node_ids_p=b_node_ids_p_c;
				}
#else
					set_lhs_tuple_node_ids (node_id->nid_node->node_arguments,node_id);
#endif
			}
 		}
#if BIND_UNBOXED_LHS_TUPLE_AND_RECORD_ARGUMENTS_IN_BIND_ARGUMENTS /* added 9-4-1999 */
		else if (!IsSimpleState (arg_p->arg_state)){
			NodeIdListElementS **a_node_ids_p_c,**b_node_ids_p_c;
			
			a_node_ids_p_c=a_node_ids_p;
			b_node_ids_p_c=b_node_ids_p;
			
			bind_tuple_and_record_arguments (arg_p->arg_node->node_arguments,NULL,a_offset,b_offset,&a_node_ids_p_c,&b_node_ids_p_c);
			
			a_node_ids_p=a_node_ids_p_c;
			b_node_ids_p=b_node_ids_p_c;			
		}
#endif

		if (IsSimpleState (arg_p->arg_state)){
			if (arg_p->arg_state.state_kind==OnB)
				b_offset -= ObjectSizes [arg_p->arg_state.state_object];
			else
				a_offset -= SizeOfAStackElem;
		} else {
			int asize,bsize;

			DetermineSizeOfState (arg_p->arg_state, &asize, &bsize);
			a_offset -= asize;
			b_offset -= bsize;
		}
	}

	*a_node_ids_p=a_node_ids;
	*b_node_ids_p=b_node_ids;
}

void ReduceArgumentToHnf (NodeIdP node_id,StateS state,int offset,SavedNidStateS **ifrule)
{
	if (IsSimpleState (state) && state.state_kind==OnA){
		GenJsrEval (offset);
		state.state_kind = StrictOnA;
		
		if (ifrule && node_id){
			save_node_id_state (node_id,ifrule);			
			node_id->nid_state_ = state;
		}
	}

	if (ifrule==NULL && node_id!=NULL)
		node_id->nid_state_=state;
}

static void MatchLhsNode (NodeP node,StateS demstate,int aindex,int bindex,int asp,int bsp,struct ab_node_ids *ab_node_ids_p);

void MatchArgs (Args args,int aindex,int bindex,int asp,int bsp,struct ab_node_ids *ab_node_ids_p)
{
	for (; args; args=args->arg_next){
		Node arg_node;
		int asize, bsize;

		arg_node=args->arg_node;
		
		if (arg_node->node_kind!=NodeIdNode){
			ReduceArgumentToHnf (NULL,args->arg_state,asp-aindex,NULL);
			MatchLhsNode (arg_node,args->arg_state,aindex,bindex,asp,bsp,ab_node_ids_p);
		} else {
			NodeId node_id;
			
			node_id=arg_node->node_node_id;
			arg_node=node_id->nid_node;

#ifndef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
			if (arg_node!=NULL){
				ReduceArgumentToHnf (node_id,args->arg_state,asp-aindex,NULL);
				MatchLhsNode (arg_node,args->arg_state,aindex,bindex,asp,bsp,ab_node_ids_p);				
			} else
#endif
			{
				node_id->nid_state_=args->arg_state;
			}
		}
		
		DetermineSizeOfState (args->arg_state,&asize,&bsize);
		aindex -= asize;
		bindex -= bsize;
	}
}

static void MatchLhsNode (NodeP node,StateS demstate,int aindex,int bindex,int asp,int bsp,struct ab_node_ids *ab_node_ids_p)
{
	Symbol symb;
	
	symb = node->node_symbol;
	
	switch (symb->symb_kind){
		case tuple_symb:
			if (!IsSimpleState (demstate)){
#if !BIND_UNBOXED_LHS_TUPLE_AND_RECORD_ARGUMENTS_IN_BIND_ARGUMENTS
				bind_arguments (node->node_arguments,aindex,bindex,ab_node_ids_p);
#endif
				MatchArgs (node->node_arguments,aindex,bindex,asp,bsp,ab_node_ids_p);
				return;
			}
			break;
		case definition:
		{
			SymbDef def;
			
			def = symb->symb_def;
			if (def->sdef_kind==RECORDTYPE){
				if (demstate.state_type==RecordState){
#if !BIND_UNBOXED_LHS_TUPLE_AND_RECORD_ARGUMENTS_IN_BIND_ARGUMENTS
					bind_arguments (node->node_arguments,aindex,bindex,ab_node_ids_p);
#endif
					MatchArgs (node->node_arguments,aindex,bindex,asp,bsp,ab_node_ids_p);
					return;
				}
			}
		}
	}
	error_in_function ("MatchLhsNode");
}

/*
	static void BindArgs (Args args,int ara,int arb)
	{
		for (; args; args = args->arg_next){
			if (IsSimpleState (args->arg_state)){
				if (args->arg_state.state_kind==OnB){
		 			if (args->arg_node->node_kind==NodeIdNode)
			 			args->arg_node->node_node_id->nid_b_index = arb;
					arb -= ObjectSizes [args->arg_state.state_object];
				} else {
		 			if (args->arg_node->node_kind==NodeIdNode)
			 			args->arg_node->node_node_id->nid_a_index = ara;
					ara -= SizeOfAStackElem;
				}
			} else {
				int asize,bsize;

				if (args->arg_node->node_kind==NodeIdNode){
		 			args->arg_node->node_node_id->nid_a_index = ara;
		 			args->arg_node->node_node_id->nid_b_index = arb;
		 		}

				DetermineSizeOfState (args->arg_state, &asize, &bsize);
				ara -= asize;
				arb -= bsize;
			}
		}
	}
*/

static void jump_false_to_next_alternative (LabDef *esclabel,int remove_a,int remove_b)
{
	if (remove_a==0 && remove_b==0)
		GenJmpFalse (esclabel);
	else {
		LabDef to;

		MakeLabel (&to,m_symb,NewLabelNr++,no_pref);
		GenJmpTrue (&to);

		GenPopA (remove_a);
		GenPopB (remove_b);
		GenJmp (esclabel);
		GenLabelDefinition (&to);
	}
}

void generate_is_constructor (ImpRuleP rule)
{
	NodeP case_node;
	LabDef symbol_label;

	case_node = rule->rule_alts->alt_rhs_root->node_arguments->arg_node;

	if (case_node->node_symbol->symb_kind==nil_symb)
		GenEqDesc (&nil_lab,case_node->node_arity,0);
	else if (case_node->node_symbol->symb_kind==cons_symb){
		struct symbol *symbol;
		
		symbol=case_node->node_symbol;
		if (symbol->symb_head_strictness==1 || symbol->symb_head_strictness>=3){
			GenEqDesc (&nil_lab,0,0);
			GenNotB();
		} else
			GenEqDesc (&cons_lab,case_node->node_arity,0);
	} else {
		SymbDef sdef;
		sdef=case_node->node_symbol->symb_def;

		if (sdef->sdef_kind==CONSTRUCTOR && sdef->sdef_strict_constructor
			&& sdef->sdef_arity==case_node->node_arity)
		{
			ConvertSymbolToKLabel (&symbol_label,sdef);
			GenEqDesc (&symbol_label,0,0);
		} else {
			ConvertSymbolToConstructorDLabel (&symbol_label,sdef);
			GenEqDesc (&symbol_label,case_node->node_arity,0);
		}
	}
	
	GenPopA (1);
}

static void CheckSymbol (Label symblab,int arity,int stackpos,int remove_a,int remove_b,Label esclabel)
{
	GenEqDesc (symblab, arity, stackpos);

	jump_false_to_next_alternative (esclabel,remove_a,remove_b);
}

static void GenNoMatchOnApplies (void)
{
	GenDumpString ("Runtime Error: left-hand-side application encountered\\n");
	GenHalt ();
}

static void GenNoMatchOnIfs (void)
{
	GenDumpString ("Runtime Error: left-hand-side application of IF encountered\\n");
	GenHalt ();
}

#ifdef GENERATE_RECORD_STATES_DURING_MATCH
static void SetArgumentStates (Args args, States argstates)
{
	int i;

	for (i = 0; args ; args = args->arg_next, i++)
		args->arg_state = argstates [i];
}
#endif

void MatchError (int aselmts,int bselmts,SymbDef sdef,Bool root_node_needed,int string_already_generated)
{
	GenLabelDefinition (&CurrentAltLabel);

	CurrentAltLabel.lab_pref  = no_pref;
	CurrentAltLabel.lab_post  = 0;

	if (sdef->sdef_exported)
		CurrentAltLabel.lab_mod = CurrentModule;

	GenNoMatchError (sdef,root_node_needed ? aselmts+1 : aselmts,bselmts,string_already_generated);

	if (sdef->sdef_exported)
		CurrentAltLabel.lab_mod = NULL;
}

static char case_symb[] = "case";

static int generate_int_char_or_bool_match (struct arg *first_arg,int *matches_always_p)
{
	struct arg *arg;
	int case_number;

	case_number=0;
	for_l (arg,first_arg,arg_next){
		struct node *case_node;
		struct symbol *symbol;
		
		case_node=arg->arg_node;

		switch (case_node->node_kind){
			case CaseNode:
			{
				LabDef case_label;

				symbol=case_node->node_symbol;
				
				MakeLabel (&case_label,case_symb,NewLabelNr,no_pref);
				
				if (symbol->symb_kind < Nr_Of_Predef_Types){
					if (symbol->symb_kind==bool_denot && case_number==1){
						GenJmp (&case_label);
						*matches_always_p=1;
					} else {
						EqBasic	(BasicSymbolStates [symbol->symb_kind].state_object,symbol->symb_val,0);
						GenJmpTrue (&case_label);
					}
				} else
					error_in_function ("generate_int_char_or_bool_match");
				
				++NewLabelNr;
				break;
			}
			case DefaultNode:
				return 1;
			default:
				error_in_function ("generate_int_char_or_bool_match");
		}
		
		++case_number;
	}

	return 0;
}

static int generate_constructor_match (ArgP first_arg,int *matches_always_p)
{
	ArgP arg;
	int case_number;

	for (arg=first_arg,case_number=0; arg!=NULL; arg=arg->arg_next,++case_number){
		struct node *case_node;
		struct symbol *symbol;
		
		case_node=arg->arg_node;

		switch (case_node->node_kind){
			case DefaultNode:
				return 1;
			case CaseNode:
			{
				LabDef case_label;

				symbol=case_node->node_symbol;
				
				MakeLabel (&case_label,case_symb,NewLabelNr,no_pref);
				++NewLabelNr;

				if (symbol->symb_kind==definition){
					LabDef symbol_label;
					SymbDef sdef;

					sdef=symbol->symb_def;

					if (sdef->sdef_kind==CONSTRUCTOR && sdef->sdef_type->type_nr_of_constructors==case_number+1){
						GenJmp (&case_label);
						*matches_always_p=1;
					} else {
						if (sdef->sdef_kind==CONSTRUCTOR && sdef->sdef_strict_constructor && sdef->sdef_arity==case_node->node_arity){
							ConvertSymbolToKLabel (&symbol_label,sdef);
							GenEqD_b (&symbol_label,0);
						} else {
							ConvertSymbolToConstructorDLabel (&symbol_label,sdef);
							GenEqD_b (&symbol_label,case_node->node_arity);
						}
						GenJmpTrue (&case_label);
					}
					break;
				}
			}
			default:
				error_in_function ("generate_constructor_match");
		}
	}

	return 0;
}

#if 0
extern char *node_id_name (NodeId node_id);
#endif

#if FREE_STRICT_LHS_TUPLE_ELEMENTS
static void add_node_id_or_tuple_node_ids_to_list (NodeIdP node_id,NodeIdP push_node_id_p,NodeIdListElementS **free_node_ids_l)
{
	if (! (node_id->nid_node!=NULL && !IsSimpleState (node_id->nid_state))){
#if defined (TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS)
		if (!(node_id->nid_refcount<0 && (node_id->nid_mark2 & NID_LHS_PUSHED)==0))
#endif
		add_node_id_to_list (node_id,free_node_ids_l);
	} else {
		ArgP arg_p;
		
		for_l (arg_p,node_id->nid_node->node_arguments,arg_next){
			NodeP arg_node_p;
			
			arg_node_p=arg_p->arg_node;
			if (arg_node_p->node_kind==NodeIdNode){
				NodeIdP node_id_p;
				
				node_id_p=arg_node_p->node_node_id;
				if (node_id_p->nid_refcount==-1 && node_id!=push_node_id_p)
#if defined (TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS)
					if (!(node_id_p->nid_refcount<0 && (node_id_p->nid_mark2 & NID_LHS_PUSHED)==0))
#endif
					add_node_id_or_tuple_node_ids_to_list (node_id_p,push_node_id_p,free_node_ids_l);								
			}
		}
	}
}
#endif

#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
void set_local_reference_counts_and_add_free_node_ids (NodeP case_node,NodeIdListElementS **free_node_ids_l)
{
	NodeIdRefCountListP node_id_ref_count_elem;
	NodeIdP push_node_id_p;

	if (case_node->node_arguments->arg_node->node_kind==PushNode)
		push_node_id_p=case_node->node_arguments->arg_node->node_arguments->arg_node->node_node_id;
	else
		push_node_id_p=NULL;

	for_l (node_id_ref_count_elem,case_node->node_node_id_ref_counts,nrcl_next){
		int local_ref_count;
		NodeIdP node_id;

		node_id=node_id_ref_count_elem->nrcl_node_id;
		local_ref_count=node_id_ref_count_elem->nrcl_ref_count;

# if BOXED_RECORDS
		node_id_ref_count_elem->nrcl_mark2=node_id->nid_mark2 & NID_RECORD_USED_BY_NON_SELECTOR_OR_UPDATES;
# endif

# if 0
		printf ("global_to_local_ %s %d %d ",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count);
# endif

		if (local_ref_count==-1 && node_id!=push_node_id_p){
			if (unused_node_id_(node_id)){
#if FREE_STRICT_LHS_TUPLE_ELEMENTS
# if 0
				printf ("global_to_local__ %s %d %d %d ",node_id_name (node_id),node_id->nid_refcount,node_id_ref_count_elem->nrcl_ref_count,node_id->nid_a_index);
# endif

				add_node_id_or_tuple_node_ids_to_list (node_id,push_node_id_p,free_node_ids_l);
#else
				if (! (node_id->nid_node!=NULL && !IsSimpleState (node_id->nid_state)))
					add_node_id_to_list (node_id,free_node_ids_l);
#endif
			}
		}

		node_id_ref_count_elem->nrcl_ref_count=node_id->nid_refcount - local_ref_count;
		node_id->nid_refcount = local_ref_count;
	}

# if 0
	printf ("\n");
# endif
}
#endif

static SymbDef sdef_of_function (NodeP node_p,int arity)
{
	if (node_p->node_kind==NormalNode && node_p->node_symbol->symb_kind==definition){
		SymbDef sdef;

		sdef=node_p->node_symbol->symb_def;
		if ((sdef->sdef_kind==IMPRULE || sdef->sdef_kind==DEFRULE || sdef->sdef_kind==SYSRULE) &&
			sdef->sdef_arity==arity && sdef->sdef_arfun==NoArrayFun)

			return sdef;
	}
	
	return NULL;
}

static int generate_code_for_switch_node (NodeP node,int asp,int bsp,struct esc *esc_p,StateP result_state_p,
										  SavedNidStateS **save_states_p,AbNodeIdsP ab_node_ids_p)
{
	int has_default,need_next_alternative,matches_always;
	unsigned int first_case_label_number,case_number;
	struct node_id *node_id;
	struct arg *arg,*first_arg;
	int a_index,b_index;
	struct esc esc,old_esc;
	int match_b_stack_top_element;
	LabDef esc_case_label;

	node_id=node->node_node_id;
	a_index=node_id->nid_a_index;
	b_index=node_id->nid_b_index;

#if 0
	ReduceArgumentToHnf (node_id,node->node_state,asp-a_index,save_states_p);
#else
	if (node_id->nid_state.state_type!=SimpleState || node_id->nid_state.state_kind==OnB){
		node->node_state=node_id->nid_state;
	} else
		ReduceArgumentToHnf (node_id,node->node_state,asp-a_index,save_states_p);
#endif
	
	first_case_label_number=NewLabelNr;

	esc=*esc_p;

	need_next_alternative=0;
	matches_always=0;
	has_default=0;

	first_arg=node->node_arguments;
	match_b_stack_top_element=0;

	if (first_arg->arg_node->node_kind==CaseNode && first_arg->arg_next!=NULL && first_arg->arg_next->arg_node->node_kind==CaseNode){
		if (node->node_state.state_type==SimpleState && (node->node_state.state_kind==OnA || node->node_state.state_kind==StrictOnA)){
			int first_case_symbol_kind;
			Symbol symbol;

			symbol=first_arg->arg_node->node_symbol;
			first_case_symbol_kind=symbol->symb_kind;

			if (first_case_symbol_kind==int_denot || first_case_symbol_kind==char_denot || first_case_symbol_kind==bool_denot){
				PushBasicFromAOnB (BasicSymbolStates [first_case_symbol_kind].state_object,asp-a_index);
				match_b_stack_top_element=1;
				
				has_default=generate_int_char_or_bool_match (first_arg,&matches_always);
			}
			else if (first_case_symbol_kind==definition){
				SymbDef sdef;

				sdef=symbol->symb_def;
				if (sdef->sdef_kind==CONSTRUCTOR){
					Symbol next_case_node_symbol;
					SymbDef next_sdef;

					next_case_node_symbol=first_arg->arg_next->arg_node->node_symbol;
				
					if (! (next_case_node_symbol->symb_kind==definition && (next_sdef=next_case_node_symbol->symb_def,
						next_sdef->sdef_kind==CONSTRUCTOR && next_sdef->sdef_type->type_nr_of_constructors==2)))
					{
						GenPushD_a (asp-a_index);
						match_b_stack_top_element=1;
					
						has_default=generate_constructor_match (first_arg,&matches_always);
					}
				}
			}
		}
	}

#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
	++node_id->nid_refcount;

	for_l (arg,node->node_arguments,arg_next){
		NodeIdRefCountListP *node_id_ref_count_elem_h,node_id_ref_count_elem_p;
		struct node *case_node;

		case_node=arg->arg_node;
		
		if (case_node->node_kind==OverloadedCaseNode)
			case_node=case_node->node_node;
		
		node_id_ref_count_elem_h=&case_node->node_node_id_ref_counts;

		while ((node_id_ref_count_elem_p=*node_id_ref_count_elem_h)!=NULL){
			if (node_id_ref_count_elem_p->nrcl_node_id->nid_refcount==-1 && node_id_ref_count_elem_p->nrcl_ref_count==-1)
				*node_id_ref_count_elem_h=node_id_ref_count_elem_p->nrcl_next;
			else
			 	node_id_ref_count_elem_h=&node_id_ref_count_elem_p->nrcl_next;
		 }
	}

	--node_id->nid_refcount;
#endif

	if (!match_b_stack_top_element)
		for (arg=first_arg,case_number=0; arg!=NULL; arg=arg->arg_next,++case_number){
			struct node *case_node;
			struct symbol *symbol;
			
			case_node=arg->arg_node;
	
			switch (case_node->node_kind){
				case CaseNode:
				{
					LabDef case_label;
	
					symbol=case_node->node_symbol;
					
					MakeLabel (&case_label,case_symb,NewLabelNr,no_pref);
					
					switch (symbol->symb_kind){
						case definition:
						{
							LabDef symbol_label;
							SymbDef sdef;
	
							sdef=symbol->symb_def;
							if (sdef->sdef_kind==RECORDTYPE || (sdef->sdef_kind==CONSTRUCTOR 
								&& sdef->sdef_type->type_nr_of_constructors==case_number+1))
							{
								if (case_number==0 && arg->arg_next==NULL){
									SavedNidStateP saved_node_id_states;
									int need_next_alternative;
	
									saved_node_id_states=NULL;
								
									++node_id->nid_refcount;
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
									{
									NodeIdListElementP old_free_node_ids;
									
									old_free_node_ids=ab_node_ids_p->free_node_ids;
									set_local_reference_counts_and_add_free_node_ids (case_node,&ab_node_ids_p->free_node_ids);
#else
 									set_local_reference_counts (case_node);
#endif
									need_next_alternative=
										generate_code_for_root_node
											(case_node->node_arguments->arg_node,asp,bsp,&esc,
											case_node->node_node_defs,result_state_p,&saved_node_id_states ,ab_node_ids_p);
								
									set_global_reference_counts (case_node);
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
									ab_node_ids_p->free_node_ids=old_free_node_ids;
									}
#endif								

									--node_id->nid_refcount;

									restore_saved_node_id_states (saved_node_id_states);
									return need_next_alternative;
								}
								
								GenJmp (&case_label);
								matches_always=1;
							} else {
								if (sdef->sdef_kind==CONSTRUCTOR && sdef->sdef_strict_constructor
									&& sdef->sdef_arity==case_node->node_arity)
								{
									ConvertSymbolToKLabel (&symbol_label,sdef);
									GenEqDesc (&symbol_label,0,asp-a_index);
								} else {
									ConvertSymbolToConstructorDLabel (&symbol_label,sdef);
									GenEqDesc (&symbol_label,case_node->node_arity,asp-a_index);
								}
								GenJmpTrue (&case_label);
							}
							break;
						}
						case cons_symb:
							if (case_number==1){
								GenJmp (&case_label);
								matches_always=1;
							} else {
#if STRICT_LISTS
								if (symbol->symb_head_strictness==1 || symbol->symb_head_strictness>=3){
									GenEqDesc (&nil_lab,0,asp-a_index);
									GenJmpFalse (&case_label);
								} else
#endif
								{
									GenEqDesc (&cons_lab,case_node->node_arity,asp-a_index);
									GenJmpTrue (&case_label);
								}
							}
							break;
						case nil_symb:
							if (case_number==1){
								GenJmp (&case_label);
								matches_always=1;
							} else {
								GenEqDesc (&nil_lab,case_node->node_arity,asp-a_index);
								GenJmpTrue (&case_label);
							}
							break;
						case tuple_symb:
							if (case_number==0 && arg->arg_next==NULL){
								SavedNidStateP saved_node_id_states;
								int need_next_alternative;
	
								saved_node_id_states=NULL;
	
								++node_id->nid_refcount;
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
								{
								NodeIdListElementP old_free_node_ids;
								
								old_free_node_ids=ab_node_ids_p->free_node_ids;
								set_local_reference_counts_and_add_free_node_ids (case_node,&ab_node_ids_p->free_node_ids);
#else
								set_local_reference_counts (case_node);
#endif

								need_next_alternative=
									generate_code_for_root_node
										(case_node->node_arguments->arg_node,asp,bsp,&esc,
										case_node->node_node_defs,result_state_p,&saved_node_id_states ,ab_node_ids_p);
	
								set_global_reference_counts (case_node);
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
								ab_node_ids_p->free_node_ids=old_free_node_ids;
								}
#endif	
								--node_id->nid_refcount;

								restore_saved_node_id_states (saved_node_id_states);
								return need_next_alternative;
							}
#if defined (TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS)
							if (!(arg->arg_next!=NULL && arg->arg_next->arg_node->node_kind==DefaultNode))
#endif
							GenJmp (&case_label);
							matches_always=1;
							break;
						case apply_symb:
						case if_symb:
							error_in_function ("generate_code_for_switch_node");
							return 0;
						case string_denot:
							if (IsSimpleState (node->node_state)){
								GenPushArray (asp-a_index);
								IsString (symbol->symb_val);
							} else {
								GenPushA (asp-a_index);
								IsString (symbol->symb_val);
							}
							GenJmpTrue (&case_label);
							break;
						case integer_denot:
						{
							LabDef not_eq_z_label;

							MakeLabel (&not_eq_z_label,"not_eq_z",new_not_eq_z_label_n,no_pref);
							++new_not_eq_z_label_n;

							if (IsSimpleState (node->node_state)){
								GenPushRArgs (asp-a_index,1,1);

								GenJmpNotEqZ (symbol->symb_val,&not_eq_z_label);

								GenPopA (1);
								GenPopB (1);				
								GenJmp (&case_label);
		
								GenLabelDefinition (&not_eq_z_label);

								GenPopA (1);
								GenPopB (1);				
							} else {
								if (asp!=a_index)
									GenPushA (asp-a_index);
								if (bsp!=b_index)
									GenPushB (bsp-b_index);
								
								GenJmpNotEqZ (symbol->symb_val,&not_eq_z_label);

								if (asp!=a_index)
									GenPopA (1);
								if (bsp!=b_index)
									GenPopB (1);				
								GenJmp (&case_label);
		
								GenLabelDefinition (&not_eq_z_label);

								if (asp!=a_index)
									GenPopA (1);
								if (bsp!=b_index)
									GenPopB (1);				
							}
							break;
						}
						default:
							if (symbol->symb_kind < Nr_Of_Predef_Types){
								ObjectKind denot_type;

								denot_type = BasicSymbolStates [symbol->symb_kind].state_object;

								if (node->node_state.state_object==denot_type){
									if (symbol->symb_kind==bool_denot && case_number==1){
										GenJmp (&case_label);
										matches_always=1;					
									} else {
										if (node->node_state.state_kind==OnB)									
											EqBasic	(denot_type,symbol->symb_val,bsp-b_index);
										else
											IsBasic	(denot_type,symbol->symb_val,asp-a_index);
						
										GenJmpTrue (&case_label);
									}
									break;
								} else if (node->node_state.state_object==UnknownObj
#if ABSTRACT_OBJECT
										|| node->node_state.state_object==AbstractObj
#endif
								){
									IsBasic	(denot_type,symbol->symb_val,asp-a_index);
									GenJmpTrue (&case_label);
								} else
									error_in_function ("generate_code_for_switch_node");
							} else
								error_in_function ("generate_code_for_switch_node");
					}
					
					++NewLabelNr;
					break;
				}
				
				case OverloadedCaseNode:
				{
					CodeGenNodeIdsS code_gen_node_ids;
					LabDef case_label;
					NodeP from_node_p,equal_node_p;
					SymbDef from_sdef,equal_sdef;
					StateS demanded_from_result_state;

					symbol=case_node->node_node->node_symbol;
					MakeLabel (&case_label,case_symb,NewLabelNr,no_pref);
	
					code_gen_node_ids.saved_nid_state_l=save_states_p;
					code_gen_node_ids.free_node_ids=ab_node_ids_p->free_node_ids;
					code_gen_node_ids.moved_node_ids_l=NULL;
					code_gen_node_ids.a_node_ids=ab_node_ids_p->a_node_ids;
					code_gen_node_ids.b_node_ids=ab_node_ids_p->b_node_ids;
					code_gen_node_ids.doesnt_fail=0;					

					equal_node_p=case_node->node_arguments->arg_node;
					from_node_p=case_node->node_arguments->arg_next->arg_node;

					equal_sdef = sdef_of_function (equal_node_p,2);
					from_sdef = sdef_of_function (from_node_p,1);

					if (equal_sdef==NULL)
						demanded_from_result_state=LazyState;
					else {
						if (equal_sdef->sdef_kind==IMPRULE)
							demanded_from_result_state=equal_sdef->sdef_rule->rule_state_p[1];
						else
							demanded_from_result_state=equal_sdef->sdef_rule_type->rule_type_state_p[1];
					}

					if (from_sdef!=NULL){
						StateP state_p;
						LabDef name;
						StateS result_state;
						int a_size,b_size;
						ArgS arg;
						
						if (from_sdef->sdef_kind==IMPRULE)
							state_p=from_sdef->sdef_rule->rule_state_p;
						else
							state_p=from_sdef->sdef_rule_type->rule_type_state_p;

						result_state=state_p[-1];
						
						if (ExpectsResultNode (result_state))
							GenCreate (-1);

						if (state_p[0].state_type==SimpleState && state_p[0].state_kind==OnB)
							PushBasic (state_p[0].state_object,symbol->symb_val);
						else {
							if (symbol->symb_kind==integer_denot){
								GenPushZ (symbol->symb_val);
								if (state_p[0].state_type!=RecordState){
									LabDef record_lab;

									ConvertSymbolToRLabel (&record_lab,BasicSymbolStates [integer_denot].state_record_symbol);
									GenBuildhr (&record_lab,1,1);
								}
							} else if (symbol->symb_kind==rational_denot){
								push_rational (symbol);
								if (state_p[0].state_type!=RecordState){
									LabDef ratio_record_lab;

									ConvertSymbolToKLabel (&ratio_record_lab,special_types[1]->sdef_type->type_constructors->cl_constructor->type_node_symbol->symb_def);
									GenBuildhr (&ratio_record_lab,2,0);
								}
							} else
								BuildBasic (BasicSymbolStates [symbol->symb_kind].state_object,symbol->symb_val);
						}
						
						arg.arg_state=state_p[0];
						arg.arg_next=NULL;
						
						ConvertSymbolToLabel (&name,from_sdef);
						CallFunction1 (&name,from_sdef,result_state,&arg,1);

						DetermineSizeOfState (result_state,&a_size,&b_size);
						asp+=a_size;
						bsp+=b_size;
						CoerceArgumentOnTopOfStack (&asp,&bsp,demanded_from_result_state,result_state,a_size,b_size);
					} else {
						asp += 1;
						
						if (symbol->symb_kind==integer_denot){
							LabDef record_lab;

							GenPushZ (symbol->symb_val);
							ConvertSymbolToRLabel (&record_lab,BasicSymbolStates [integer_denot].state_record_symbol);
							GenBuildhr (&record_lab,1,1);
						} else if (symbol->symb_kind==rational_denot){
							LabDef ratio_record_lab;

							push_rational (symbol);

							ConvertSymbolToKLabel (&ratio_record_lab,special_types[1]->sdef_type->type_constructors->cl_constructor->type_node_symbol->symb_def);

							GenBuildhr (&ratio_record_lab,2,0);
						} else
							BuildBasic (BasicSymbolStates [symbol->symb_kind].state_object,symbol->symb_val);

						Build (from_node_p,&asp,&bsp,&code_gen_node_ids);

						asp -= 1;
						GenJsrAp (1);

						if (equal_sdef!=NULL)
							CoerceArgumentOnTopOfStack (&asp,&bsp,demanded_from_result_state,StrictState,1,0);
					}

					if (equal_sdef!=NULL){
						StateP state_p;
						LabDef name;
						StateS result_state;
						int a_size,b_size;
						ArgS arg1,arg2;
						
						if (equal_sdef->sdef_kind==IMPRULE)
							state_p=equal_sdef->sdef_rule->rule_state_p;
						else
							state_p=equal_sdef->sdef_rule_type->rule_type_state_p;
				
						arg2.arg_state=state_p[1];
						arg2.arg_next=NULL;
						arg1.arg_state=state_p[0];
						arg1.arg_next=&arg2;

						result_state=state_p[-1];

						{
							int arg_asp,arg_bsp;

							arg_asp=asp;
							arg_bsp=bsp;
							CopyNodeIdArgument (arg1.arg_state,node_id,&arg_asp,&arg_bsp);
						}

						SubSizeOfState (arg2.arg_state,&asp,&bsp);
						
						ConvertSymbolToLabel (&name,equal_sdef);
						CallFunction1 (&name,equal_sdef,result_state,&arg1,2);

						DetermineSizeOfState (result_state,&a_size,&b_size);
						asp+=a_size;
						bsp+=b_size;
						CoerceArgumentOnTopOfStack (&asp,&bsp,BasicSymbolStates [bool_type],result_state,a_size,b_size);

						bsp -= 1;
					} else {
						CopyNodeIdArgument (LazyState,node_id,&asp,&bsp);

						Build (equal_node_p,&asp,&bsp,&code_gen_node_ids);

						asp -= 2;
						GenJsrAp (2);

						PushBasicFromAOnB (BoolObj,0);
						asp -= 1;
						GenPopA (1);
					}

					ab_node_ids_p->free_node_ids=code_gen_node_ids.free_node_ids;
					ab_node_ids_p->a_node_ids=code_gen_node_ids.a_node_ids;
					ab_node_ids_p->b_node_ids=code_gen_node_ids.b_node_ids;

					GenJmpTrue (&case_label);

					++NewLabelNr;
					break;
				}
				case DefaultNode:
					has_default=1;
					break;
				default:
					error_in_function ("generate_code_for_switch_node");
			}
		}

	if (has_default){
		MakeLabel (&esc_case_label,case_symb,NewLabelNr,no_pref);
		++NewLabelNr;

		if (!matches_always){
			if (match_b_stack_top_element)
				GenPopB (1);
			GenJmp (&esc_case_label);
		}

		old_esc=esc;

		esc.esc_asp=asp;
		esc.esc_bsp=bsp;
		esc.esc_label=&esc_case_label;
	} else
	if (/* !has_default && */ !matches_always){
		int n_pop_a;

		need_next_alternative=1;
		
		n_pop_a=asp-esc.esc_asp;

		if (n_pop_a>0)
			GenPopA (n_pop_a);
		else if (n_pop_a<0){
			int offset;
			
			GenBuildh (&nil_lab,0);
			
			offset=0;
			while (++n_pop_a!=0)
				GenPushA (offset++);
		}

		if (match_b_stack_top_element)
			GenPopB (bsp+1-esc.esc_bsp);
		else
			GenPopB (bsp-esc.esc_bsp);

		GenJmp (esc.esc_label);
	}

	for_l (arg,node->node_arguments,arg_next){
		struct node *case_node;
		LabDef case_label;
		SavedNidStateP saved_node_id_states;

		case_node=arg->arg_node;
		
		if (case_node->node_kind==OverloadedCaseNode)
			case_node=case_node->node_node;

		MakeLabel (&case_label,case_symb,first_case_label_number,no_pref);
		++first_case_label_number;

		GenLabelDefinition	(&case_label);

		saved_node_id_states=NULL;

		++node_id->nid_refcount;
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
		{
		NodeIdListElementP old_free_node_ids;
		
		old_free_node_ids=ab_node_ids_p->free_node_ids;
		set_local_reference_counts_and_add_free_node_ids (case_node,&ab_node_ids_p->free_node_ids);
#else
		set_local_reference_counts (case_node);
#endif

		if (case_node->node_kind==CaseNode){
			if (match_b_stack_top_element)
				GenPopB (1);

			if (generate_code_for_root_node
				(case_node->node_arguments->arg_node,asp,bsp,&esc,case_node->node_node_defs,
					result_state_p,&saved_node_id_states,ab_node_ids_p) && !has_default)
			{
				need_next_alternative=1;
			}
#if BOXED_RECORDS
			set_global_reference_counts_and_exchange_record_update_marks (case_node);
#endif
		} else {
#if BOXED_RECORDS
			ArgP arg2;
			
			for_l (arg2,node->node_arguments,arg_next){
				if (arg2->arg_node->node_kind==CaseNode && arg2->arg_node->node_number)
					or_then_record_update_marks (case_node->node_node_id_ref_counts);
			}
#endif
			if (generate_code_for_root_node
				(case_node->node_arguments->arg_node,asp,bsp,&old_esc,case_node->node_node_defs,
					result_state_p,&saved_node_id_states,ab_node_ids_p))
			{
				need_next_alternative=1;
			}
#if BOXED_RECORDS
			set_global_reference_counts (case_node);
#endif
		}

#if !BOXED_RECORDS
		set_global_reference_counts (case_node);
#endif
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
		ab_node_ids_p->free_node_ids=old_free_node_ids;
		}
#endif
		--node_id->nid_refcount;

		restore_saved_node_id_states (saved_node_id_states);
	}

	return need_next_alternative;
}

/* #define unused_node_id(node_id) ((node_id)->nid_refcount!=-1 ? (node_id)->nid_refcount==0 : unused_node_id_ (node_id)) */

int unused_node_id_ (NodeId node_id)
{
	if (!(node_id->nid_mark & NID_STRICT_LHS_TUPLE_ELEMENT_MASK))
		return True;

	node_id=node_id->nid_lhs_tuple_node_id;
	
	while (node_id->nid_refcount==-1){
		if (!(node_id->nid_mark & NID_STRICT_LHS_TUPLE_ELEMENT_MASK))
			return True;

		node_id=node_id->nid_lhs_tuple_node_id;		
	}

	return False;
}

#if STRICT_LISTS
static void repl_overloaded_cons_arguments (NodeP node_p,int *asp_p,int *bsp_p,SavedNidStateS **save_states_p,AbNodeIdsP ab_node_ids_p)
{
	CodeGenNodeIdsS code_gen_node_ids;
	
	code_gen_node_ids.saved_nid_state_l=save_states_p;
	code_gen_node_ids.free_node_ids=ab_node_ids_p->free_node_ids;
	code_gen_node_ids.moved_node_ids_l=NULL;
	code_gen_node_ids.a_node_ids=ab_node_ids_p->a_node_ids;
	code_gen_node_ids.b_node_ids=ab_node_ids_p->b_node_ids;
	code_gen_node_ids.doesnt_fail=0;
	
	Build (node_p->node_decons_node,asp_p,bsp_p,&code_gen_node_ids);

	*asp_p -= 2;

	ab_node_ids_p->free_node_ids=code_gen_node_ids.free_node_ids;
	ab_node_ids_p->a_node_ids=code_gen_node_ids.a_node_ids;
	ab_node_ids_p->b_node_ids=code_gen_node_ids.b_node_ids;

	if (DoDebug)
		FPrintF (OutFile, "\n||\tinstance %d %d",node_p->node_push_symbol->symb_head_strictness,node_p->node_push_symbol->symb_instance_apply);

	if (node_p->node_decons_node->node_kind==SelectorNode &&
		(node_p->node_decons_node->node_symbol->symb_def->sdef_mark & SDEF_FIELD_HAS_MEMBER_TYPE)!=0)
	{
		struct symbol_def *field_sdef;

		field_sdef=node_p->node_decons_node->node_symbol->symb_def;

		if (DoDebug)
			FPrintF (OutFile, "\n||\t%s",field_sdef->sdef_ident->ident_name);

		if (OptimizeInstanceCalls){
			struct state *member_states_of_field;
			int member_arity,member_called_with_root_node;

			member_states_of_field=field_sdef->sdef_member_states_of_field;

			member_arity=field_sdef->sdef_member_type_of_field->type_alt_lhs->type_node_arity;

			member_called_with_root_node = member_states_of_field[-1].state_type==SimpleState
											&& !(member_states_of_field[-1].state_kind==StrictRedirection || member_states_of_field[-1].state_kind==OnB);

			if (member_states_of_field[-1].state_type==TupleState){
				int a_size,b_size;

				DetermineSizeOfStates (member_arity-1,&member_states_of_field[1],&a_size,&b_size);
				GenDStackLayoutOfStates (a_size+1+member_called_with_root_node,b_size,member_arity-1,&member_states_of_field[1]);

				GenJsrI (1);

				DetermineSizeOfState (member_states_of_field[-1],&a_size,&b_size);
				GenOStackLayoutOfState (a_size,b_size,member_states_of_field[-1]);
				return;
			}
		}
	}

	GenJsrAp (1);

	GenReplArgs (2,2);
}
#endif

static int generate_code_for_push_node (NodeP node,int asp,int bsp,struct esc *esc_p,NodeDefs defs,StateP result_state_p,
										SavedNidStateS **save_states_p,AbNodeIdsP ab_node_ids_p)
{
	NodeIdP node_id_p;
	struct node_id_list_element *arg_node_id_list;
	int a_index,b_index;
	struct arg *arguments;
	int a_size,b_size;
	int a_remove,b_remove;
	int source_a_index,source_b_index;
	int update_stack_size;
	struct ab_node_ids ab_node_ids;
	struct update updates[MaxNodeArity];
	
	ab_node_ids=*ab_node_ids_p;

	arguments=node->node_arguments;

	node_id_p=arguments->arg_node->node_node_id;

#if defined (TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS)
	if (!IsSimpleState (node_id_p->nid_state) && node_id_p->nid_refcount<0 && node_id_p->nid_node!=NULL){
		for_l (arg_node_id_list,node->node_node_ids,nidl_next){
			NodeIdP arg_node_id;
		
			arg_node_id=arg_node_id_list->nidl_node_id;
			arg_node_id->nid_mark2 |= NID_LHS_PUSHED;
			arg_node_id->nid_state = *arg_node_id->nid_lhs_state_p;
		}

		return generate_code_for_root_node (arguments->arg_next->arg_node,asp,bsp,esc_p,defs,result_state_p,save_states_p,&ab_node_ids);
	}
#endif

	a_size=0;
	b_size=0;

	a_remove=0;
	b_remove=0;

	for_l (arg_node_id_list,node->node_node_ids,nidl_next){
		NodeIdP arg_node_id;
		
		arg_node_id=arg_node_id_list->nidl_node_id;

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
		arg_node_id->nid_mark2 |= NID_LHS_PUSHED;
		arg_node_id->nid_state = *arg_node_id->nid_lhs_state_p;
#endif

		if (arg_node_id->nid_refcount==-1){
			if (IsSimpleState (arg_node_id->nid_state)){
				if (arg_node_id->nid_state.state_kind==OnB)
					b_remove += ObjectSizes [arg_node_id->nid_state.state_object];
				else
					a_remove += SizeOfAStackElem;
			} else
				AddSizeOfState (arg_node_id->nid_state,&a_remove,&b_remove);			
		}

		if (IsSimpleState (arg_node_id->nid_state)){
			if (arg_node_id->nid_state.state_kind==OnB)
				b_size += ObjectSizes [arg_node_id->nid_state.state_object];
			else
				a_size += SizeOfAStackElem;
		} else {
			/* added 6-8-1999 */
#if defined (TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS)
			arg_node_id->nid_node=NULL;
#endif
			/* */
			AddSizeOfState (arg_node_id->nid_state,&a_size,&b_size);
		}
	}


#if FREE_STRICT_LHS_TUPLE_ELEMENTS /* added 26-4-2000 */
	{
	NodeIdListElementP removed_free_node_id_p,*removed_free_node_id_h;
	
	removed_free_node_id_p=NULL;
#endif

	if (!IsSimpleState (node_id_p->nid_state)){
		int a_size,b_size;
		
		DetermineSizeOfState (node_id_p->nid_state,&a_size,&b_size);
		
		if (unused_node_id (node_id_p) && (a_size==0 || node_id_p->nid_a_index==asp) && (b_size==0 || node_id_p->nid_b_index==bsp)){
			asp-=a_size;
			bsp-=b_size;

			if (ab_node_ids.a_node_ids!=NULL && ab_node_ids.a_node_ids->nidl_node_id==node_id_p)
				ab_node_ids.a_node_ids=ab_node_ids.a_node_ids->nidl_next;
			
			if (ab_node_ids.b_node_ids!=NULL && ab_node_ids.b_node_ids->nidl_node_id==node_id_p)
				ab_node_ids.b_node_ids=ab_node_ids.b_node_ids->nidl_next;
		} else {
			int a_offset,b_offset;

			a_offset=(asp-node_id_p->nid_a_index)+a_size-1;
			while (a_size){
				GenPushA (a_offset);
				--a_size;
			}

			b_offset=(bsp-node_id_p->nid_b_index)+b_size-1;
			while (b_size){
				GenPushB (b_offset);
				--b_size;
			}
			
			node_id_p->nid_node=NULL; /* to prevent codegen2 from using a_index and b_index of elements */
		}
	} else {
#ifdef REUSE_UNIQUE_NODES
		if (node->node_number!=0){
			if (b_size==0)
				GenPushArgsU (asp-node_id_p->nid_a_index,a_size,a_size);
			else
				GenPushRArgsU (asp-node_id_p->nid_a_index,a_size,b_size);				
		}
# ifdef DESTRUCTIVE_RECORD_UPDATES
		else if (node->node_push_symbol->symb_kind==definition &&
				node->node_push_symbol->symb_def->sdef_kind==RECORDTYPE &&
				(((node_id_p->nid_mark2 & NID_HAS_REFCOUNT_WITHOUT_UPDATES)!=0 &&
				node_id_p->nid_number==-2)
#  if BOXED_RECORDS
				|| (node_id_p->nid_mark2 & NID_RECORD_USED_BY_UPDATE)!=0
#  endif
		)){
			node_id_p->nid_number=-1;
			if (b_size==0)
				GenPushArgsU (asp-node_id_p->nid_a_index,a_size,a_size);
			else
				GenPushRArgsU (asp-node_id_p->nid_a_index,a_size,b_size);				
		}
# endif
		else {
#endif

		if (unused_node_id (node_id_p)){
			if (node_id_p->nid_a_index==asp){
				if (b_size==0){
#if STRICT_LISTS
					if (node->node_push_symbol->symb_kind==cons_symb && (node->node_push_symbol->symb_head_strictness & 1)){
						repl_overloaded_cons_arguments (node,&asp,&bsp,save_states_p,ab_node_ids_p);
					} else
#endif
					GenReplArgs (a_size,a_size);
				} else
					GenReplRArgs (a_size,b_size);

				if (ab_node_ids.a_node_ids!=NULL && ab_node_ids.a_node_ids->nidl_node_id==node_id_p)
					ab_node_ids.a_node_ids=ab_node_ids.a_node_ids->nidl_next;

#if FREE_STRICT_LHS_TUPLE_ELEMENTS /* added 26-4-2000 */
				removed_free_node_id_h=&ab_node_ids.free_node_ids;

				while ((removed_free_node_id_p=*removed_free_node_id_h)!=NULL){
					if (removed_free_node_id_p->nidl_node_id==node_id_p){
						*removed_free_node_id_h=removed_free_node_id_p->nidl_next;
						break;
					}
					removed_free_node_id_h=&removed_free_node_id_p->nidl_next;
				}				
#endif

				--asp;
			} else {
				if (b_size==0){
#if STRICT_LISTS
					if (node->node_push_symbol->symb_kind==cons_symb && (node->node_push_symbol->symb_head_strictness & 1)){
						GenPushA (asp-node_id_p->nid_a_index);
						++asp;
						
						repl_overloaded_cons_arguments (node,&asp,&bsp,save_states_p,ab_node_ids_p);
					} else
#endif
					GenPushArgs (asp-node_id_p->nid_a_index,a_size,a_size);
				} else
					GenPushRArgs (asp-node_id_p->nid_a_index,a_size,b_size);
				
				GenBuildh (&nil_lab,0);
				GenUpdateA (0,1+a_size+asp-node_id_p->nid_a_index);
				GenPopA (1);
			}
		} else {
			if (b_size==0){
#if STRICT_LISTS
				if (node->node_push_symbol->symb_kind==cons_symb && (node->node_push_symbol->symb_head_strictness & 1)){
					GenPushA (asp-node_id_p->nid_a_index);
					++asp;
						
					repl_overloaded_cons_arguments (node,&asp,&bsp,save_states_p,ab_node_ids_p);
				} else
#endif
				GenPushArgs (asp-node_id_p->nid_a_index,a_size,a_size);
}			else
				GenPushRArgs (asp-node_id_p->nid_a_index,a_size,b_size);
		}

#ifdef REUSE_UNIQUE_NODES
		}
#endif
	}

	asp+=a_size;
	bsp+=b_size;

	source_a_index=asp;
	source_b_index=bsp;

	a_index = source_a_index-a_remove;
	b_index = source_b_index-b_remove;
	
	update_stack_size=0;

	{
		struct node_id_list_element **a_node_ids_p,**b_node_ids_p,*a_node_ids,*b_node_ids;

		a_node_ids=ab_node_ids.a_node_ids;
		b_node_ids=ab_node_ids.b_node_ids;
	
		a_node_ids_p=&ab_node_ids.a_node_ids;
		b_node_ids_p=&ab_node_ids.b_node_ids;

		for_l (arg_node_id_list,node->node_node_ids,nidl_next){
			int asize,bsize;
			struct node_id *arg_node_id;
			
			arg_node_id=arg_node_id_list->nidl_node_id;

			DetermineSizeOfState (arg_node_id->nid_state,&asize,&bsize);

			arg_node_id->nid_a_index_ = a_index;
			arg_node_id->nid_b_index_ = b_index;
			
			if (arg_node_id->nid_refcount==-1){
				source_a_index -= asize;
				source_b_index -= bsize;
				
				continue;
			}

			if (IsSimpleState (arg_node_id->nid_state) || arg_node_id->nid_node==NULL){
				struct node_id_list_element *new_p_node_id;

				if (asize!=0){
					new_p_node_id=CompAllocType (struct node_id_list_element);
					new_p_node_id->nidl_node_id=arg_node_id;
		
					*a_node_ids_p=new_p_node_id;
					a_node_ids_p=&new_p_node_id->nidl_next;
				}

				if (bsize!=0){
					new_p_node_id=CompAllocType (struct node_id_list_element);
					new_p_node_id->nidl_node_id=arg_node_id;
		
					*b_node_ids_p=new_p_node_id;
					b_node_ids_p=&new_p_node_id->nidl_next;
				}
			}
			
			if (a_index!=source_a_index || b_index!=source_b_index){
				struct update *update_p;
				
				update_p=&updates[update_stack_size++];
				
				update_p->a_from_offset=source_a_index;
				update_p->a_to_offset=a_index;
				update_p->a_size=asize;
				update_p->b_from_offset=source_b_index;
				update_p->b_to_offset=b_index;
				update_p->b_size=bsize;
			}
			
			a_index -= asize;
			b_index -= bsize;
			source_a_index -= asize;
			source_b_index -= bsize;
		}

		*a_node_ids_p=a_node_ids;
		*b_node_ids_p=b_node_ids;
	}

	while (update_stack_size!=0){
		struct update *update_p;
		int to,from,size;

		update_p=&updates[--update_stack_size];
		
		size=update_p->a_size;
		from=update_p->a_from_offset;
		to=update_p->a_to_offset;
		while (size!=0){
			--size;
			GenUpdateA (asp-(from-size),asp-(to-size));
		}

		size=update_p->b_size;
		from=update_p->b_from_offset;
		to=update_p->b_to_offset;
		while (size!=0){
			--size;
			GenUpdateB (bsp-(from-size),bsp-(to-size));
		}		
	}
	
	GenPopA (a_remove);
	GenPopB (b_remove);
	
	asp-=a_remove;
	bsp-=b_remove;

#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
	{
		int r;

		r=generate_code_for_root_node (arguments->arg_next->arg_node,asp,bsp,esc_p,defs,result_state_p,save_states_p,&ab_node_ids);

#if FREE_STRICT_LHS_TUPLE_ELEMENTS /* added 26-4-2000 */
		if (removed_free_node_id_p!=NULL)
			*removed_free_node_id_h=removed_free_node_id_p;
#endif
		for_l (arg_node_id_list,node->node_node_ids,nidl_next){
			NodeIdP arg_node_id;
		
			arg_node_id=arg_node_id_list->nidl_node_id;

			arg_node_id->nid_mark2 &= ~NID_LHS_PUSHED;
		}

		return r;
	}
#else
	return generate_code_for_root_node (arguments->arg_next->arg_node,asp,bsp,esc_p,defs,result_state_p,save_states_p,&ab_node_ids);
#endif

#if FREE_STRICT_LHS_TUPLE_ELEMENTS /* added 26-4-2000 */
	}
#endif
}

#ifdef CLEAN2
int contains_fail (NodeP node_p)
{
	while (node_p->node_kind==IfNode){
		NodeP then_node_p,else_node_p;
	
		then_node_p=node_p->node_arguments->arg_next->arg_node;
		else_node_p=node_p->node_arguments->arg_next->arg_next->arg_node;

		if (else_node_p->node_kind==NormalNode && else_node_p->node_symbol->symb_kind==fail_symb)
			return 1;

		if (then_node_p->node_kind==IfNode && contains_fail (then_node_p))
			return 1;
		
		node_p=else_node_p;
	}

	return 0;
}
#endif

int generate_code_for_root_node (NodeP node,int asp,int bsp,struct esc *esc_p,NodeDefP defs,StateP result_state_p,
								 SavedNidStateS **save_states_p,AbNodeIdsP ab_node_ids_p)
{
	switch (node->node_kind){
		case SwitchNode:
			return generate_code_for_switch_node (node,asp,bsp,esc_p,result_state_p,save_states_p,ab_node_ids_p);
		case PushNode:
			return generate_code_for_push_node (node,asp,bsp,esc_p,defs,result_state_p,save_states_p,ab_node_ids_p);
		case GuardNode:
			while (node->node_kind==GuardNode){
				SavedNidStateP saved_node_id_states;
				ArgP arguments;
				int fail_label_number;
				LabDef fail_label;
				struct esc guard_esc;

				fail_label_number=NewLabelNr++;
				MakeLabel (&fail_label,"fail",fail_label_number,no_pref);

				arguments=node->node_arguments;

				saved_node_id_states=NULL;

				guard_esc.esc_asp=asp;
				guard_esc.esc_bsp=bsp;
				guard_esc.esc_label=&fail_label;

#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
				{
				NodeIdListElement old_free_node_ids;
				
				old_free_node_ids=ab_node_ids_p->free_node_ids;
#endif
				generate_code_for_root_node (arguments->arg_node,asp,bsp,&guard_esc,defs,result_state_p,&saved_node_id_states,ab_node_ids_p);

#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
				ab_node_ids_p->free_node_ids=old_free_node_ids;
				}
#endif
		
				restore_saved_node_id_states (saved_node_id_states);

				GenLabelDefinition	(&fail_label);

				defs=node->node_node_defs;
				node=arguments->arg_next->arg_node;
			}

			return generate_code_for_root_node (node,asp,bsp,esc_p,defs,result_state_p,save_states_p,ab_node_ids_p);
		default:
		{
#ifndef CLEAN2
			NodeP else_node;	

			else_node=node;
			while (else_node->node_kind==IfNode)
				else_node=else_node->node_arguments->arg_next->arg_next->arg_node;
#endif

			return CodeRhsNodeDefs (node,defs,asp,bsp,save_states_p,*result_state_p,esc_p,ab_node_ids_p->a_node_ids,
					ab_node_ids_p->b_node_ids,
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
					ab_node_ids_p->free_node_ids,
#else
					NULL,
#endif
#ifdef CLEAN2
					!contains_fail (node));
#else
					!(else_node->node_kind==NormalNode && else_node->node_symbol->symb_kind==fail_symb));
#endif
		}
	}
}
