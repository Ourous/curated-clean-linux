extern void create_result_state_database (struct imp_rule *imp_rules);
#if 1
extern int get_label_number_from_result_state_database (StateP result_state_p,int mask,int *label_number_p);
#else
extern int get_label_number_from_result_state_database (TypeAlts type_alt,int mask,int *label_number_p);
#endif
