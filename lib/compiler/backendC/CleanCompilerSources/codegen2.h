
extern void bind_arguments (ArgS *arguments,int a_offset,int b_offset,struct ab_node_ids *ab_node_ids_p);

typedef
	enum
	{	NormalFill, ReleaseAndFill, PartialFill
	} FillKind;

typedef enum {
	AToA, AToB, BToA, BToB, Reduce,AToRoot, MayBecomeCyclicSpine, CyclicSpine
} Coercions;

STRUCT (moved_node_id,MovedNodeId){
	struct node_id *		mnid_node_id;
	struct moved_node_id *	mnid_next;
	int						mnid_a_stack_offset;
};

STRUCT (code_gen_node_ids,CodeGenNodeIds){
	struct saved_nid_state **saved_nid_state_l;
	struct node_id_list_element *free_node_ids;
	struct moved_node_id **moved_node_ids_l;
	struct node_id_list_element *a_node_ids;
	struct node_id_list_element *b_node_ids;
	int doesnt_fail;
};

extern StateS OnAState;
extern LabDef BasicDescriptors [];
extern unsigned NewLabelNr,new_not_eq_z_label_n;
extern Bool LazyTupleSelectors [];
extern int ObjectSizes [];
 
#define IsOnACycle(nodenum) (nodenum < 0)
#define IsOnBStack(state) (! IsSimpleState (state) || (state).state_kind == OnB)

extern int selector_m_error_lab_used;
extern LabDef selector_m_error_lab;
extern LabDef *unboxed_cons_label (SymbolP cons_symbol_p);

extern Bool EqualState (StateS st1, StateS st2);
extern void DetermineSizeOfArguments (ArgS *args,int *a_offset_p,int *b_offset_p);
extern void SubSizeOfState (StateS state,int *a_offset_p,int *b_offset_p);
extern void BuildTuple (int aindex, int bindex, int asp, int bsp, int arity,
						States argstates,int asize,int bsize,int rootindex,FillKind fkind,Bool newnode);

extern void BuildRecord (SymbDef seldef, int aindex, int bindex, int asp, int bsp,
						int asize, int bsize, int rootindex,FillKind fkind, Bool popargs);
extern void BuildNewRecord (SymbDef seldef, int aindex, int bindex, int asp, int bsp, int asize, int bsize);
extern void BuildNewRecordPop (SymbDef seldef, int asize, int bsize);
extern void CoerceArgumentUsingStackFrames (StateS demstate, StateS offstate,
											int aindex,int bindex,int *asp,int *bsp,int *anext,int *bnext,int asize,int bsize);
extern void DetermineArrayElemDescr (StateS elemstate, Label lab);
extern void InitCoding (void);

/* extern int InitAStackTop, InitBStackTop; */
extern Bool NeedNextAlternative;
extern void PackArgument (StateS argstate,int aindex,int bindex,int asp,int bsp,int offasize,int offbsize);

extern void save_node_id_state (NodeId node_id,struct saved_nid_state **ifrule);
extern void restore_saved_node_id_states (struct saved_nid_state *saved_node_id_states);

#if GENERATE_CODE_AGAIN
extern ArgP
#else
extern void
#endif
	compute_bits_and_remove_unused_arguments (NodeP node,char bits[],unsigned int argument_overwrite_bits,unsigned int *n_args_p);
#if GENERATE_CODE_AGAIN
extern ArgP
#else
extern void
#endif
	compute_bits_and_remove_unused_arguments_for_strict_node (NodeP node,char bits[],unsigned int argument_overwrite_bits,
															  int *a_size_p,int *b_size_p,int *n_a_fill_bits_p,int *n_b_fill_bits_p);
#if GENERATE_CODE_AGAIN
extern void restore_removed_arguments (ArgP *arg_h,ArgP removed_args,unsigned int argument_overwrite_bits,int node_arity);
#endif

#ifdef DESTRUCTIVE_RECORD_UPDATES
extern void compute_bits_and_add_selectors_to_update_node
	(ArgS *record_arg,ArgS *first_field_arg,StateS *field_states,int record_a_size,int record_b_size,
	char bits[],int *n_a_fill_bits_p,int *n_b_fill_bits_p);
#endif
int is_unique_record_update (NodeIdP record_node_id,NodeP record_node);

Coercions CoerceStateKind (StateKind dem_state_kind, StateKind off_state_kind);
void GenReduceError (void);
void UnpackTuple (int aindex,int *asp_p,int *bsp_p,Bool removeroot,int arity,StateS argstates[]);
void UnpackRecord (int aindex,int *asp_p,int *bsp,Bool removeroot,int arity,States argstates);
void UnpackArray (int aindex, int *asp_p, Bool removeroot);
void NewEmptyNode (int *asp_p,int nrargs);
void AdjustTuple (int localasp,int localbsp,int *asp_p,int *bsp_p,int arity,StateS demstates[],StateS offstates[],int asize,int bsize);
int get_a_index_of_unpacked_lhs_node (ArgS *arg);
int get_b_index_of_unpacked_lhs_node (ArgS *arg);
void decrement_reference_count_of_node_id (struct node_id *node_id,NodeIdListElementS **free_node_ids_l);

void BuildArgs (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);
void BuildLazyArgs (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);
void build_and_cleanup (Node node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);

#define RECORD_N_PREF c_pref
#define RECORD_D_PREF t_pref
#define CONSTRUCTOR_R_PREF k_pref

extern char *Co_Wtype,*Co_Wspine,else_symb[],then_symb[],notused_string[];
extern SymbDef ApplyDef,IfDef,SeqDef;
extern StateS StrictOnAState;

void FillSelectSymbol (StateKind result_state_kind,int arity,int argnr,Args arg,int *asp_p,int *bsp_p,
								NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p);
void Build (Node node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);
Coercions CoerceSimpleStateArgument (StateS demstate,StateKind offkind,int aindex,int *asp_p,Bool leaveontop, Bool *ontop);
void subtract_else_ref_counts (struct node_id_ref_count_list *else_node_id_ref_counts,NodeIdListElementS **free_node_ids_l);
void add_else_ref_counts (struct node_id_ref_count_list *else_node_id_ref_counts);
#if BOXED_RECORDS
	void or_then_record_update_marks (struct node_id_ref_count_list *else_node_id_ref_counts);
#endif
void EvaluateCondition (Node cond_node,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p,StateS resultstate);
void DetermineFieldSizeAndPositionAndRecordSize
	(int fieldnr,int *asize_p,int *bsize_p,int *apos_p,int *bpos_p,int *rec_asize_p,int *rec_bsize_p,StateS *record_state_p);
void CodeSharedNodeDefs (NodeDefs nds, NodeDefs rootdef,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);
void FillMatchNode (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p);
void BranchOnCondition (Node condnode, int asp, int bsp,CodeGenNodeIdsP code_gen_node_ids_p, StateS resultstate,
						Label truelab,Label falselab,Label next_label,int then_asp, int then_bsp, int else_asp, int else_bsp);
void GenTypeError (void);
void BuildArg (Args arg,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p);
Bool NodeOnACycleIsInRootNormalForm (Node node);
void UpdateStackPointers (int old_asp,int old_bsp,int new_asp,int new_bsp);
void UpdateRecordAndAddSelectorsToUpdateNode
	(ArgS *record_arg,ArgS *first_field_arg,StateS *field_states,int record_a_size,int record_b_size,int *end_args_a_offset_p,int *end_args_b_offset_p);
void RemoveFieldsFromStackAfterUpdate (int arg_a_offset,int arg_b_offset,int record_a_size,int record_b_size,int *asp_p,int *bsp_p);
void RemoveSelectorsFromUpdateNode (ArgS *previous_arg,ArgS *arg);
void BuildOrFillLazyFieldSelector (SymbDef selector_sdef,StateKind result_state_kind,int *asp_p,NodeId update_node_id);
void CoerceArgumentOnTopOfStack (int *asp_p,int *bsp_p,StateS argstate,StateS nodestate,int asize,int bsize);
void ReplaceRecordOnTopOfStackByField (int *asp_p,int *bsp_p,int apos,int bpos,int asize,int bsize,int rec_a_size,int rec_b_size) ;
Bool CopyNodeIdArgument (StateS demstate,NodeId node_id,int *asp_p,int *bsp_p);

void add_node_id_to_list (struct node_id *node_id,NodeIdListElementS **node_ids_l);
void BuildArgsWithNewResultNode (Args args,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p,int *a_size_p,int *b_size_p);
void BuildArgsWithResultNodeOnStack (Args args,NodeIdP free_unique_node_id,int *asp_p,int *bsp_p,CodeGenNodeIdsP code_gen_node_ids_p,int *a_size_p,int *b_size_p);
void cleanup_stack
	(int *asp_p,int *bsp_p,int a_size,int b_size,NodeIdListElementS **a_node_ids_l,NodeIdListElementS **b_node_ids_l,
	 NodeIdListElementS **free_node_ids_l,MovedNodeIdP *moved_node_ids_l,int compact_stack_ok);

void ChangeEvalStatusKindToStrictOnA (NodeId node_id,SavedNidStateS **saved_nid_state_l);
#if OPTIMIZE_LAZY_TUPLE_RECURSION
void FillNodeOnACycle (Node node,int *asp_p,int *bsp_p,NodeId update_node_id,CodeGenNodeIdsP code_gen_node_ids_p);
#endif
void PushField (StateS recstate,int fieldnr,int offset,int *asp_p,int *bsp_p,int *a_size_p,int *b_size_p);
void ReplaceRecordByField (StateS recstate,int fieldnr,int *asp_p,int *bsp_p,int *a_size_p,int *b_size_p);

void push_rational (SymbolP symb);
