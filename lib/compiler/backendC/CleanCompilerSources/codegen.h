
void CodeGeneration (ImpMod imod, char *fname);
void EvalArgsEntry (StateS *const function_state_p,SymbDef rule_sdef,int maxasize,Label ea_lab,int n_result_nodes_on_a_stack);
void EvaluateAndMoveStateArguments (int state_arity,States states,int oldasp,int maxassize);
void EvaluateAndMoveArguments (int arity,StateP argstates,int *locasp_p,int *aselmts_p);

extern int function_called_only_curried_or_lazy_with_one_return;
#if GENERATE_CODE_AGAIN
 extern int call_code_generator_again;
#endif
