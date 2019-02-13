
struct esc {
	int	 			esc_asp;
	int				esc_bsp;
	struct label *	esc_label;
};

extern char d_pref[],s_pref[],n_pref[],l_pref[],no_pref[],ea_pref[],caf_pref[],
			glob_sel[],m_symb[];
#ifdef THUNK_LIFT_SELECTORS
extern char glob_selr[];
#endif

extern char channel_code [],ext_nf_reducer_code[],nf_reducer_code[],hnf_reducer_code[],ext_hnf_reducer_code[];

extern LabDef
	cycle_lab, reserve_lab, type_error_lab, indirection_lab, ind_lab,
	hnf_lab, cons_lab, nil_lab, tuple_lab, empty_lab, add_arg_lab, match_error_lab,
#if STRICT_LISTS
	conss_lab,consts_lab,conssts_lab,unboxed_cons_labels[5][2],unboxed_cons_array_label,
#endif
#ifdef CLEAN2
	select_with_dictionary_lab, update_with_dictionary_lab,
#endif
	CurrentAltLabel;

extern int unboxed_cons_mark[5][2];
extern int unboxed_cons_array_mark;

extern Label ReduceError;

#define ExpectsResultNode(state) ((state).state_type==SimpleState && (state).state_kind>StrictRedirection)

#define IsSimpleState(state) ((state).state_type==SimpleState)

extern void FileComment (void);

extern void PrintNodeId (NodeId nid);

extern void PrintComment (void);
extern void LhsComment (unsigned int altnr, int asp, int bsp);
extern void StrictIdComment (NodeId id);
extern void NodeDefComment (NodeDefs nd, char *msg);
extern void ContractumComment (int asp, int bsp);
extern void RedirectionComment (NodeId nid);
extern void ArgComment (Args arg);
extern void NodeIdComment (NodeId node_id);
extern void ParComment (Args arg);

extern void InitStackFrame (int offframe[], int defframe [], int max);
extern void InitStackConversions (int maxa, int maxb, int *oldamax, int *oldbmax);
extern void InitAStackConversions (int maxa,int *oldamax_p);
extern void ExitStackConversions (int oldamax, int oldbmax);
extern void GenAStackConversions (int sp,int demsize);
extern void GenBStackConversions (int sp,int demsize);

extern int *DemandedAFrame,*DemandedBFrame,CurrentAFrameSize,CurrentBFrameSize;

extern void FreeAFrameSpace (int previoussize);
extern void ReserveBFrameSpace (int size, int *oldsize);
extern void FreeBFrameSpace (int previoussize);
extern int *AllocTempDemandedAFrame (int size);
extern int *AllocTempDemandedBFrame (int size);
extern void CreateStackFrames (void);
extern void PutInBFrames (int bsp, int *b_ind, int size);
extern void PutInAFrames (int asp, int *a_ind);

extern void MakeLabel (Label lab, char *name, unsigned num, char *pref);
extern void MakeSymbolLabel (Label lab, char *mod, char *pref,SymbDef sdef, unsigned num);

extern void ConvertSymbolToLabel (LabDef *slab,SymbDef sdef);
extern void ConvertSymbolToDLabel (LabDef *slab,SymbDef sdef);
extern void ConvertSymbolToKLabel (LabDef *slab,SymbDef sdef);
extern void ConvertSymbolToRLabel (LabDef *slab,SymbDef sdef);
extern void ConvertSymbolToDandNLabel (LabDef *d_lab,LabDef *n_lab,SymbDef sdef);
extern void ConvertSymbolToConstructorDLabel (LabDef *slab,SymbDef sdef);
extern void ConvertSymbolToConstructorDandNLabel (LabDef *d_lab,LabDef *n_lab,SymbDef sdef);
extern void ConvertSymbolToRecordDandNLabel (LabDef *d_lab,LabDef *n_lab,SymbDef sdef);

extern void BuildLazyTupleSelectorLabel (Label slab,int arity,int argnr);
#if defined (THUNK_LIFT_SELECTORS)
extern void BuildLazyTupleSelectorAndRemoveLabel (Label slab,int arity,int argnr);
#endif

extern void DetermineSizeOfStates (int arity, States states, int *asize, int *bsize);
extern void DetermineSizeOfState (StateS state, int *asize, int *bsize);
extern void AddSizeOfState (StateS state, int *asize, int *bsize);

extern void AddStateSizeAndMaxFrameSize (StateS state, int *maxasize,int *asize, int *bsize);
extern void AddStateSizesAndMaxFrameSizes (int arity, States states,int *maxasize, int *asize, int *bsize);
extern void AddStateSizesAndMaxFrameSizesOfArguments (Args args,int *maxasize, int *asize, int *bsize);
extern void DetermineFieldSizeAndPosition (int fieldnr, int *asize, int *bsize,int *apos, int *bpos, States argstates);
	
extern void GenerateCodeForConstructorsAndRecords (Symbol symbs);
extern void GenerateStatesForRecords (Symbol symbs);

extern Bool NodeEntry (StateS *const function_state_p,int arity,Label ealab,SymbDef rootsymb);
extern Bool NodeEntryUnboxed (StateS *const function_state_p,NodeP call_node_p,int args_a_size,int args_b_size,Label ealab,SymbDef rootsymb);

extern int generate_instance_entry (struct symbol_def *rule_sdef,struct state *function_state_p,struct label *i_label_p);
extern void ApplyInstanceEntry (StateS *const function_state_p,int arity,Label ea_lab,struct label *i_label_p,int ea_label_follows);
extern void ApplyEntry (StateS *const function_state_p,int arity,Label ealab,int ea_label_follows);

extern Bool ConvertExternalToInternalCall (int arity,StateS *const ext_function_state_p,StateS *const int_function_state_p,
										Bool skip_entry,int intasp,int intbsp,Label ealab,Label extlab,Bool root_node_needed);
extern void GenerateCodeForLazyTupleSelectorEntries (Bool *selectors);
extern void GenerateCodeForLazyArrayFunctionEntries (void);
#if STRICT_LISTS
void GenerateCodeForLazyUnboxedRecordListFunctions (void);
#endif
extern int next_update_function_n,next_match_function_n;

extern ImpRuleS *first_update_function,**update_function_p;
extern SymbDef CreateUpdateFunction (ArgS *record_arg,ArgS *first_field_arg,Node node
#if UNBOX_UPDATE_FUNCTION_ARGUMENTS
	,int unbox_record
#endif
);

extern SymbDef create_select_function (Symbol selector_symbol,int selector_kind);
extern SymbDef create_match_function (struct symbol *constructor_symbol,int result_arity,int n_dictionaries,int strict_constructor);
extern SymbDef create_select_and_match_function (struct symbol *constructor_symbol,int n_dictionaries,int strict_constructor);

extern void ReduceArgumentToHnf (NodeId node_id,StateS state,int offset,struct saved_nid_state **save_states_p);
extern void BindArgs (Args args,int ara,int arb);
extern void MatchArgs (Args args,int aindex,int bindex,int asp,int bsp,struct ab_node_ids *ab_node_ids_p);

extern void MatchError (int aselmts,int bselmts,SymbDef sdef,Bool root_node_needed,int string_already_generated);

extern int generate_code_for_root_node
	(struct node *node,int asp,int bsp,struct esc *esc_p,NodeDefs defs,
	struct state *result_state_p,struct saved_nid_state **save_states_p ,struct ab_node_ids *ab_node_ids_p);

extern ImpRuleS *create_simple_imp_rule (struct node *lhs_root,struct node *rhs_root,SymbDefP function_sdef);

#define unused_node_id(node_id) ((node_id)->nid_refcount!=-1 ? (node_id)->nid_refcount==0 : unused_node_id_ (node_id))
extern int unused_node_id_ (NodeId node_id);

extern void generate_is_constructor (ImpRuleP rule);
