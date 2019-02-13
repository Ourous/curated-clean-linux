
extern File OutFile;
extern char *ABCFileName;

Bool OpenABCFile (char *fname);

void CloseABCFile (char *fname);

void BuildBasicFromB (ObjectKind kind,int b_offset);
void FillBasicFromB (ObjectKind kind, int boffs, int aoffs, FillKind fkind);
void BuildBasic (ObjectKind obj,SymbValue val);
void FillBasic (ObjectKind obj, SymbValue val, int offset, FillKind fkind);

void IsBasic (ObjectKind obj, SymbValue val, int offset);
void IsString (SymbValue val);

void PushBasic (ObjectKind obj, SymbValue val);

void EqBasic (ObjectKind obj, SymbValue val, int offset);
void GenNotB(void);

void PushBasicFromAOnB (ObjectKind kind,int offset);
void GenPushD_a (int a_offset);

void PushBasicOnB (ObjectKind state, int offset);

void UpdateBasic (int size, int srcoffset, int dstoffset);

void CallFunction (Label label, SymbDef def, Bool isjsr, Node root);
void CallFunction1 (Label label, SymbDef def, StateS root_state, Args fun_args, int arity);

void CallArrayFunction (SymbDef def,Bool isjsr,StateP node_state_p);

void GenNewContext (Label contlab, int offset);

void GenPushArgs (int offset,int arity,int nrargs);
void GenPushArgsU (int offset,int arity,int nrargs);
void GenPushArg (int offset,int arity,int argnr);

void GenPushRArgs (int offset,int nr_a_args,int nr_b_args);
void GenPushRArgsU (int offset,int n_a_args,int n_b_args);
void GenPushRArgA (int offset,int tot_nr_a_args,int tot_nr_b_args,int args_nr,int nr_a_args);
void GenPushRArgB (int offset,int tot_nr_a_args,int tot_nr_b_args,int args_nr,int nr_b_args);
void GenPushRArgU (int offset,int tot_nr_a_args,int tot_nr_b_args,int args_a_nr,int nr_a_args,int args_b_nr,int nr_b_args);

void GenReplArgs (int arity, int nrargs);
void GenReplArg (int arity, int argnr);

void GenReplRArgs (int nr_a_args, int nr_b_args);
void GenReplRArgA (int tot_nr_a_args, int tot_nr_b_args, int args_nr, int nr_a_args);

void GenPushNode (Label contlab, int arity);
void GenPushNodeU (Label contlab,int a_size,int b_size);

void GenFill (Label symblab, int arity,Label contlab, int offset, FillKind fkind);
void GenFillcp (Label symblab,int arity,Label contlab,int offset,char bits[]);
void GenFillU (Label symblab,int a_size,int b_size,Label contlab,int offset);
void GenFillcpU (Label symblab,int a_size,int b_size,Label contlab,int offset,char bits[]);
void GenFillh (Label symblab, int arity,int offset, FillKind fkind);
void GenFill1 (Label symblab,int arity,int offset,char bits[]);
void GenFill2 (Label symblab, int arity,int offset,char bits[]);
void GenFill3 (Label symblab,int arity,int offset,char bits[]);
void GenBuild (Label symblab,int arity,Label contlab);
void GenBuildh (Label symblab,int arity);
void GenBuildPartialFunctionh (Label symblab,int arity);
void GenBuildU (Label symblab,int a_size,int b_size,Label contlab);
void GenBuildArrayPop (void);
void GenBuildArray (int argoffset);
void GenBuildString (SymbValue val);

void GenPushZ (SymbValue val);
void GenPushZR (SymbValue val);

void GenBuildFieldSelector (Label symblab,Label contlab,char *record_name,int arity);
void GenFillFieldSelector (Label symblab,Label contlab,char *record_name,int arity,int offset,FillKind fkind);

void GenFillFromA (int src, int dst, FillKind fkind);

void GenFillR (Label symblab,int nr_a_args,int nr_b_args,int rootoffset,int a_offset,int b_offset,FillKind fkind,Bool pop_args);
void GenFill1R (Label symblab,int n_a_args,int n_b_args,int rootoffset,char bits[]);
void GenFill2R (Label symblab,int n_a_args,int n_b_args,int rootoffset,char bits[]);
void GenFill3R (Label symblab,int n_a_args,int n_b_args,int rootoffset,char bits[]);
void GenBuildhr (Label symblab,int nr_a_args,int nr_b_args);
void GenBuildR (Label symblab,int nr_a_args,int nr_b_args,int a_offset,int b_offset);

void GenFillArrayAndPop (int rootoffset, FillKind fkind);
void GenFillArray (int argoffset, int rootoffset, FillKind fkind);

void GenPushArray (int offset);

void GenRtn (int asize, int bsize, StateS resultstate);

void GenPushA (int offset);
void GenPushB (int offset);

void GenJsrEval (int offset);
void GenJsrAp (int n_args);
void GenJsrI (int n_args);
void GenJmpEval (void);
void GenJmpAp (int n_args);
void GenJmpApUpd (int n_args);
void GenJmpI (int n_args);
void GenJmpNotEqZ (SymbValue val,Label tolab);
void GenJmpUpd (Label tolab);
void GenPopA (int nr);
void GenPopB (int nr);

void GenEqDesc (Label symblab, int arity, int offset);
void GenEqD_b (Label symblab,int arity);

void GenExitFalse (Label to);
void GenJmpFalse (Label to);
void GenJmpTrue (Label to);

void GenJmp (Label tolab);
void GenJsr (Label tolab);

void GenCreate (int arity);

void GenDumpString (char *str);

void GenLabelDefinition (Label lab);
void GenNodeEntryLabelDefinition (Label lab);

void GenFieldLabelDefinition (Label label,char *record_name);

void GenUpdateA (int src, int dst);
void GenUpdateB (int src, int dst);

#ifdef UPDATE_POP
void GenUpdatePopA (int src, int dst);
void GenUpdatePopB (int src, int dst);
#endif

void GenFillArray (int argoffset, int rootoffset, FillKind fkind);

void GenPushArray (int rootoffset);

void GenNewParallelReducer (int offset, char *reducer_code);

void GenNewInterleavedReducer (int offset, char *reducer_code);

void GenNewContInterleavedReducer (int offset);

void GenSendGraph (char *code, int graphoffs, int chanoffs);
void GenCreateChannel (char *code);
void GenNewP (void);
void GenPushReducerId (int i);
void GenSetRedId (int offset);
void GenSetDefer (int offset);
void SetContinue (int offset);
void SetContinueOnReducer (int offset);
void GenImpRecordDesc (char *module_name,char *record_name);
void GenImport (SymbDef sdef);
void GenExportRecord (SymbDef sdef);
void GenExportFieldSelector (SymbDef sdef);
void GenExportStrictAndEaEntry (SymbDef sdef);
void GenExportEaEntry (SymbDef sdef);
void GenExportEuEntry (SymbDef sdef);

void GenDAStackLayout (int asize);
void GenDStackLayoutOfStates (int asize,int bsize,int n_states,StateP state_p);
void GenDStackLayoutOfState (int asize, int bsize, StateS resultstate);

void GenOAStackLayout (int asize);
void GenOStackLayoutOfStates (int asize,int bsize,int n_states,StateP state_p);
void GenOStackLayoutOfState (int asize, int bsize, StateS resultstate);

void GenDStackLayout (int asize,int bsize,Args fun_args);
void GenOStackLayout (int asize,int bsize,Args fun_args);

void GenNodeEntryDirective (int arity,Label label,Label label2);
void GenNodeEntryDirectiveForLabelWithoutSymbol (int arity,Label label,Label label2);
void GenNodeEntryDirectiveUnboxed (int a_size,int b_size,Label label,Label label2);
void GenApplyEntryDirective (int arity,Label label);
void GenApplyInstanceEntryDirective (int arity,Label label,Label label2);
void GenLazyRecordNodeEntryDirective (int arity,Label label,Label label2);
void GenFieldNodeEntryDirective (int arity, Label label, Label label2,char *record_name);
void GenConstructorDescriptorAndExport (SymbDef sdef);
void GenConstructor0DescriptorAndExport (SymbDef sdef,int constructor_n);
void GenFunctionDescriptorAndExportNodeAndDescriptor (SymbDef sdef);
void GenConstructorFunctionDescriptorAndExportNodeAndDescriptor (SymbDef sdef);
#if OPTIMIZE_LAZY_TUPLE_RECURSION
void GenFunctionDescriptorForLazyTupleRecursion (SymbDef sdef,int tuple_result_arity);
#endif
void GenLazyRecordDescriptorAndExport (SymbDef sdef);
#ifdef NEW_SELECTOR_DESCRIPTORS
void GenFieldSelectorDescriptor (SymbDef sdef,StateS field_state,int a_pos,int b_pos,int record_a_size,int record_b_size);
#else
void GenFieldSelectorDescriptor (SymbDef sdef,int has_gc_apply_entry);
#endif
void GenRecordDescriptor (SymbDef sdef);
#ifdef STRICT_LISTS
void GenUnboxedConsRecordDescriptor (SymbDef sdef,int tail_strict);
#endif
void GenStrictConstructorDescriptor (SymbDef sdef,StateP constructor_arg_state_p);
void GenArrayFunctionDescriptor (SymbDef arr_fun_def, Label desclab, int arity);

#if defined(WRITE_DCL_MODIFICATION_TIME) && WRITE_DCL_MODIFICATION_TIME
void GenModuleDescriptor (ModuleFileTime file_time);
void GenDepend (char *modname,ModuleFileTime file_time);
#else
void GenModuleDescriptor (void);
void GenDepend (char *modname);
#endif
void GenEndInfo (void);
void GenImpMod (char *module_name);
void GenSystemImports (void);
void import_not_yet_imported_system_labels (void);
void GenerateForeignExports (struct foreign_export_list *foreign_export_p);
void GenStart (SymbDef startsymb);
void InitFileInfo (ImpMod imod);

/* void GenFileInfo (void); */

void GenNoMatchError (SymbDef sdef,int asp,int bsp,int string_already_generated);
#if CLEAN2
void GenCaseNoMatchError (SymbDefP case_def,int asp,int bsp);
#endif

void InitInstructions (void);

void GenHalt (void);
void GenParameters (Bool input, Parameters params, int asp, int bsp);
void GenInstructions (Instructions ilist);

void GenJmpEvalUpdate (void);

#ifdef NEW_SELECTOR_DESCRIPTORS
void GenSelectorDescriptor (Label sellab,int element_n);
#else
void GenSelectorDescriptor (Label sellab,char *g_pref);
#endif
void GenGetNodeArity (int offset);
void GenPushArgNr (int argnr);
void GenPushArgB (int offset);

void GenTestCaf (Label label);
void GenPushCaf (Label label,int a_stack_size,int b_stack_size);
void GenFillCaf (Label label,int a_stack_size,int b_stack_size);
void GenCaf (Label label,int a_stack_size,int b_stack_size);

void GenPB (char *function_name);
void GenPB_ident (IdentP ident,unsigned int line_n);
void GenPB_with_line_number (char *function_name,int line_number);
void GenPD (void);
void GenPN (void);
void GenPL (void);
void GenPLD (void);
void GenPT (void);
void GenPE (void);

void GenKeep (int a_offset1,int a_offset2);

void WriteLastNewlineToABCFile (void);
#if IMPORT_OBJ_AND_LIB
void GenImpObj (char *obj_name);
void GenImpLib (char *lib_name);
#endif
