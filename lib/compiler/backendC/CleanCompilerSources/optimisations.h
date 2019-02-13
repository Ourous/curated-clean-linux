void OptimiseRules (ImpRules rules,SymbDef start_sdef);
SymbolP copy_imp_rule_and_type (SymbDef old_sdef);
void copy_imp_rule_nodes (ImpRuleP old_rule_p,ImpRuleP new_rule_p);
int optimise_tuple_result_function (Node node,StateS demanded_state);
void generate_states (struct imp_rule *rules,int do_strictness_analysis);
StateP state_of_node_or_node_id (NodeP node_p);
void add_sizes_of_states_of_node_ids (NodeIdListElementP node_id_list,int *total_a_size_p,int *total_b_size_p);
