extern void set_local_reference_counts (struct node *case_node);
extern void set_global_reference_counts (struct node *case_node);
extern struct node_id_ref_count_list *new_node_id_ref_count (struct node_id_ref_count_list *node_id_ref_count_list,struct node_id *node_id,int ref_count);
#ifdef TRANSFORM_PATTERNS_BEFORE_STRICTNESS_ANALYSIS
extern void determine_failing_cases_and_adjust_ref_counts_of_rule (RuleAltP first_alt);
#endif
#if BOXED_RECORDS
void set_global_reference_counts_and_exchange_record_update_marks (NodeP case_node);
#endif
