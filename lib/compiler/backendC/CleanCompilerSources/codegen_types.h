
#define REUSE_UNIQUE_NODES
#define UPDATE_POP 1
#define BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH 1 /* added 13-4-1999 */
#define TAIL_CALL_MODULO_CONS_OPTIMIZATION 1
#define OPTIMIZE_LAZY_TUPLE_RECURSION 1
#define UNBOX_UPDATE_FUNCTION_ARGUMENTS 1

#define TAIL_CALL_MODULO_TUPLE_CONS_OPTIMIZATION 0
#undef THUNK_LIFT_SELECTORS

#define GENERATE_CODE_AGAIN TAIL_CALL_MODULO_CONS_OPTIMIZATION || OPTIMIZE_LAZY_TUPLE_RECURSION

/* node_id_list: used in PushNode and during code generation */

STRUCT (node_id_list_element,NodeIdListElement){
	struct node_id *				nidl_node_id;
	struct node_id_list_element *	nidl_next;
};

STRUCT (node_id_ref_count_list,NodeIdRefCountList){
	struct node_id_ref_count_list *	nrcl_next;
	struct node_id *				nrcl_node_id;
	int								nrcl_ref_count;
#if BOXED_RECORDS
	int								nrcl_mark2;
#endif
};

STRUCT (free_unique_node_ids,FreeUniqueNodeIds){
	struct node *					fnid_push_node;
	int								fnid_node_size;
	struct free_unique_node_ids	*	fnid_next;
};

STRUCT (ab_node_ids,AbNodeIds){
	struct node_id_list_element *	a_node_ids;
	struct node_id_list_element *	b_node_ids;
#if BUILD_FREE_NODE_ID_LIST_DURING_PATTER_MATCH
	struct node_id_list_element *	free_node_ids;
#endif
};

STRUCT (saved_nid_state,SavedNidState){
	StateS						save_state;
	NodeId						save_node_id;
	struct saved_nid_state *	save_next;
};

typedef struct label {
	char 		*lab_mod;
	char 		*lab_pref;
	Bool		lab_issymbol;
	union{
			char 	*u_name;
			SymbDef	 u_symbol;
	} lab_u;
	unsigned	lab_post;
} *Label, LabDef;

#define lab_name	lab_u.u_name
#define lab_symbol	lab_u.u_symbol
