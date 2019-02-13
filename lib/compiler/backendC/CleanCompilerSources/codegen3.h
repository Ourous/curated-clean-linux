
extern void RedirectResultAndReturn (int asp,int bsp,int aindex,int bindex,
									StateS offstate,StateS demstate,int offasize,int offbsize);

extern int CodeRhsNodeDefs
	(Node root_node,NodeDefs defs,int asp,int bsp,struct saved_nid_state **then_or_else,StateS resultstate,struct esc *esc_p,
	struct node_id_list_element *a_node_ids,struct node_id_list_element *b_node_ids,
	struct node_id_list_element *free_node_ids,int doesnt_fail);

struct saved_node_id_ref_counts;
struct saved_node_id_ref_counts* save_lhs_node_id_ref_counts (NodeP node_p,struct saved_node_id_ref_counts *snir_p);
struct saved_case_node_id_ref_counts;
struct saved_node_id_ref_counts* save_rhs_node_id_ref_counts (NodeP node_p,NodeDefP node_defs,
											struct saved_node_id_ref_counts *snir_p,struct saved_case_node_id_ref_counts ***scnirc_h);
void restore_node_id_ref_counts (struct saved_node_id_ref_counts *snir_p,struct saved_case_node_id_ref_counts *scnirc_p);

#if TAIL_CALL_MODULO_CONS_OPTIMIZATION
int does_tail_call_modulo_cons (NodeP node_p,NodeDefP node_defs);
#endif

#if OPTIMIZE_LAZY_TUPLE_RECURSION
void update_tuple_element_node (StateP state_p,int tuple_element_a_index,int *asp_p,int *bsp_p);
#endif
