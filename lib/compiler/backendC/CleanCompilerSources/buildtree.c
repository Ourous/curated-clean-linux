
# include "compiledefines.h"
# include "types.t"
# include "syntaxtr.t"
# include "comsupport.h"
# include "sizes.h"
# include "buildtree.h"
# include "checker.h"
# include "scanner.h"

SymbolP	BasicTypeSymbols [Nr_Of_Basic_Types],
		ArraySymbols [NrOfArrayInstances],

		ApplyTypeSymbol, TrueSymbol, FalseSymbol,
		TupleSymbol, ListSymbol, ConsSymbol, NilSymbol,
		SelectSymbols [MaxNodeArity], ApplySymbol, IfSymbol, FailSymbol,
		EmptyTypeSymbol,
		TupleTypeSymbols [MaxNodeArity];

#if STRICT_LISTS
SymbolP
	StrictListSymbol, StrictConsSymbol, StrictNilSymbol,
	UnboxedListSymbol, UnboxedConsSymbol, UnboxedNilSymbol,
	TailStrictListSymbol, TailStrictConsSymbol, TailStrictNilSymbol,
	StrictTailStrictListSymbol, StrictTailStrictConsSymbol, StrictTailStrictNilSymbol,
	UnboxedTailStrictListSymbol, UnboxedTailStrictConsSymbol, UnboxedTailStrictNilSymbol;
#endif

IdentP gArrayIdents [NrOfArrayInstances];

TypeArgs
NewTypeArgument (TypeNode pattern)
{
	TypeArgs newarg;

	newarg = CompAllocType (TypeArg);

	newarg->type_arg_node	= pattern;
	newarg->type_arg_next	= NIL;

	return (newarg);
} /* NewTypeArgument */

Args
NewArgument (NodeP node)
{
	Args newarg;
	
	newarg	= CompAllocType (ArgS);

	newarg->arg_node		= node;
	newarg->arg_occurrence	= 0;
	newarg->arg_next		= NIL;

	return (newarg);
} /* NewArgument */

NodeIdP
NewNodeId (IdentP nid)
{
	NodeIdP	newnid;

	newnid	= CompAllocType (struct node_id);

	newnid->nid_ident		= nid;
	newnid->nid_refcount	= 0;
	newnid->nid_ref_count_copy	= 0;
	newnid->nid_forward_node_id		= NIL;
	newnid->nid_node_def	= NIL;
	newnid->nid_node		= NIL;
	newnid->nid_scope		= 0;
	newnid->nid_mark		= 0;
	newnid->nid_mark2		= 0;

	return (newnid);
} /* NewNodeId */

static StrictNodeIdP
NewStrict (StrictNodeIdP next)
{
	StrictNodeIdP	strictNodeId;
	
	strictNodeId					= CompAllocType (StrictNodeIdS);

#ifdef OBSERVE_ARRAY_SELECTS_IN_PATTERN
	strictNodeId->snid_array_select_in_pattern=0;
#endif
	strictNodeId->snid_next		= next;
	
	return (strictNodeId);
} /* NewStrict */

StrictNodeIdP
NewStrictNodeId (NodeId nodeId, StrictNodeIdP next)
{
	StrictNodeIdP	strictNodeId;

	strictNodeId	=	NewStrict (next);

	strictNodeId->snid_node_id	= nodeId;

	return (strictNodeId);
} /* NewStrictNodeId */

TypeVar
NewTypeVar (IdentP nid)
{
	TypeVar	newnid;

	newnid	= CompAllocType (struct type_var);

	newnid->tv_ident			= nid;
	newnid->tv_argument_nr		= 0;

	return (newnid);
}

NodeP
NewNodeIdNode (NodeIdP node_id)
{
	NodeP node				= CompAllocType (struct node);

	node->node_annotation	= NoAnnot;
	node->node_number		= 0;
	node->node_kind			= NodeIdNode;
	node->node_node_id		= node_id;
	node->node_arguments	= NIL;
	node->node_arity		= 0;
	
	return (node);
} /* NewNodeIdNode */

TypeNode
NewTypeNode (Annotation annot, AttributeKind attr, SymbolP symb, TypeArgs args, int arity)
{
	TypeNode node;
	
	node = CompAllocType (struct type_node);

	node->type_node_annotation	= annot;
	node->type_node_attribute	= attr;
	node->type_node_is_var		= False;
	node->type_node_arguments	= args;
	node->type_node_symbol		= symb;
	node->type_node_arity		= arity;

	if (arity > MaxNodeArity)
		StaticMessage (True, "<type node>", "\"%S\" %s", symb, "Too many arguments (> 32)"); 
#if 0
	node->type_node_state.state_arity		= 1;
	node->type_node_state.state_kind		= OnA;
	node->type_node_state.state_object		= UnknownObj;
	node->type_node_state.state_type		= SimpleState;
	node->type_node_state.state_mark		= 0;
#endif
	return (node);
} /* NewTypeNode */

TypeNode
NewTypeVarNode (TypeVar type_var, Annotation annot, AttributeKind attrib)
{
	TypeNode node;
	
	node = CompAllocType (struct type_node);

	node->type_node_is_var		= True;
	node->type_node_tv			= type_var;
	node->type_node_arguments	= NIL;
	node->type_node_annotation	= annot;
	node->type_node_attribute	= attrib;
#if 0
	node->type_node_state.state_arity		= 1;
	node->type_node_state.state_kind		= OnA;
	node->type_node_state.state_object		= UnknownObj;
	node->type_node_state.state_type		= SimpleState;
	node->type_node_state.state_mark		= 0;
#endif
	return (node);
} /* NewTypeVarNode */

NodeP
NewSelectorNode (SymbolP symb, Args args, int arity)
{
	NodeP node;

	node	= CompAllocType (struct node);

	node->node_annotation	= NoAnnot;
	node->node_number		= 0;
	node->node_kind			= SelectorNode;
	node->node_arguments	= args;
	node->node_symbol		= symb;
	node->node_arity		= arity;

	return (node);
} /* NewSelectorNode */

NodeP
NewNodeByKind (NodeKind nodeKind, SymbolP symb, Args args, int arity)
{
	NodeP node;

	node = CompAllocType (struct node);

	node->node_annotation	= NoAnnot;
	node->node_number		= 0;
	node->node_kind			= nodeKind;
	node->node_arguments	= args;
	node->node_symbol		= symb;
	node->node_arity		= arity;

	if (arity > MaxNodeArity)
		StaticMessage (True, "<node>", "\"%S\" %s", symb, "Too many arguments (> 32)"); 

	return (node);
} /* NewNodeByKind */

NodeP
NewNode (SymbolP symb, Args args, int arity)
{
	return (NewNodeByKind (NormalNode, symb, args, arity));
} /* NewNode */

NodeP
NewUpdateNode (SymbolP symb, Args args, int arity)
{
	return (NewNodeByKind (UpdateNode, symb, args, arity));
} /* NewUpdateNode */

NodeP
NewIfNode (void)
{
	NodeP node;
	struct if_node_contents *then_else_info;

	node = CompAllocType (struct node);
	then_else_info = CompAllocType (struct if_node_contents);

	node->node_annotation	= NoAnnot;
	node->node_number		= 0;
	node->node_kind			= IfNode;

	node->node_contents.contents_if=then_else_info;

	then_else_info->if_then_node_defs		= NIL;
	then_else_info->if_then_strict_node_ids	= NIL;
	then_else_info->if_else_node_defs		= NIL;
	then_else_info->if_else_strict_node_ids = NIL;

	return (node);
} /* NewIfNode */

NodeP
NewSelectNode (SymbolP selectSymbol, NodeIdP selectId, int arity)
{
	Args selectArg;

	selectArg	= NewArgument (NewNodeIdNode (selectId));

	return (NewNode (selectSymbol, selectArg, arity));
} /* NewSelectNode */

NodeDefs
NewNodeDefinition (NodeIdP nid, NodeP node)
{
	NodeDefs def;
	
	def	= CompAllocType (NodeDefS);

	def->def_mark	= 0;
	def->def_id		= nid;
	def->def_node	= node;

	return (def);
} /* NewNodeDefinition */

NodeIdP
FreshNodeId (NodeP node, NodeDefs **node_defs_h)
{
	NodeIdP		nodeId;
	NodeDefs	def;

	nodeId	= NewNodeId (NIL);

	def	= NewNodeDefinition (nodeId, node);

	**node_defs_h	= def;
	*node_defs_h	= &def->def_next;

	return (nodeId);
} /* FreshNodeId */

SymbolP
NewSymbol (SymbKind symbolKind)
{
	SymbolP symbol;
	
	symbol	= CompAllocType (SymbolS);

	symbol->symb_kind	= symbolKind;

	return (symbol);
} /* NewSymbol */	

NodeP
NewIntNode (int value)
{
	char	buffer [10], *valueString;
	SymbolP	symbol;
	NodeP	node;
	int		length;

	sprintf (buffer, "%d", value);
	length	= strlen (buffer);

	valueString	= (char *) CompAlloc (length+1);
	strcpy (valueString, buffer);

	symbol	= NewSymbol (int_denot);
	symbol->symb_int = valueString;	

	node	= NewNormalNode (symbol, NIL, 0);

	return (node);
} /* NewIntNode */

SymbolP
NewTupleTypeSymbol (int arity)
{
	SymbolP tuple;

	if ((tuple =TupleTypeSymbols [arity-1]) == NIL)
	{
		TupleTypeSymbols [arity-1] = tuple = NewSymbol (tuple_type);
		tuple -> symb_arity = arity;
	}

	return tuple;

} /* NewTupleTypeSymbol */

SymbolP
NewSelectSymbol (int arity)
{
	SymbolP select;

	if ((select = SelectSymbols [arity-1]) == NIL)
	{
		select	= NewSymbol (select_symb);
		select->symb_arity = arity;
		SelectSymbols [arity-1]		= select;
	}

	return (select);
} /* NewSelectSymbol */

ImpRules
NewImpRule (unsigned line_number,TypeAlts typeAlternative,NodeP rule_root)
{
	ImpRules	impRule;

	impRule	= CompAllocType (ImpRuleS);

	impRule->rule_alts = NIL;
	impRule->rule_root = rule_root;
	impRule->rule_type = typeAlternative;
	impRule->rule_next_changed_function = NULL;

	impRule->rule_mark = 0;
	impRule->rule_next = NIL;

	return impRule;
} /* NewImpRule */

RuleAltP
NewRuleAlt (void)
{
	RuleAltP alt;

	alt = CompAllocType (RuleAltS);
	
	alt->alt_kind 				= Contractum;
	alt->alt_lhs_root			= NIL;
	alt->alt_lhs_defs			= NIL;
	alt->alt_rhs_defs			= NIL;
	alt->alt_strict_node_ids	= NIL;
	alt->alt_next 				= NIL;
	alt->alt_line				= 0;

	return (alt);
} /* NewRuleAlt */

struct p_at_node_tree {
	NodeP					annoted_node;
	NodeP					at_node;
	struct p_at_node_tree *	left;
	struct p_at_node_tree *	right;
};

static struct p_at_node_tree *p_at_node_tree;

void clear_p_at_node_tree (void)
{
	p_at_node_tree=NULL;
}

static NodeP reorder_bits (NodeP node)
{
#ifdef _WIN64
	unsigned __int64 n,m;
	
	n=(unsigned __int64)node;

	m=n & 0x000ffffffff;
	n= (m<<32) | ((n^m)>>32);	
#else
	unsigned long n,m;
	
	n=(long)node;
#endif

	m=n & 0x000ffffL;
	n= (m<<16) | ((n^m)>>16);
	m=n & 0x00ff00ffL;
	n= (m<<8) | ((n^m)>>8);
	m=n & 0x0f0f0f0fL;
	n= (m<<4) | ((n^m)>>4);
	
	return (NodeP)n;
}

void store_p_at_node (NodeP annoted_node,NodeP at_node)
{
	struct p_at_node_tree *tree_node,**tree_node_p;
	
	/* without reordering the tree becomes a list */
	annoted_node=reorder_bits (annoted_node);
	
	tree_node_p=&p_at_node_tree;
	while ((tree_node=*tree_node_p)!=NULL)
		if (annoted_node < tree_node->annoted_node)
			tree_node_p=&tree_node->left;
		else
			tree_node_p=&tree_node->right;
	
	tree_node=CompAllocType (struct p_at_node_tree);

	tree_node->annoted_node=annoted_node;
	tree_node->at_node=at_node;
	tree_node->left=NULL;
	tree_node->right=NULL;
	
	*tree_node_p=tree_node;
}

NodeP *get_p_at_node_p (NodeP annoted_node)
{
	struct p_at_node_tree *tree_node;

	annoted_node=reorder_bits (annoted_node);
	
	tree_node=p_at_node_tree;
	while (tree_node!=NULL)
		if (annoted_node < tree_node->annoted_node)
			tree_node=tree_node->left;
		else if (annoted_node > tree_node->annoted_node)
			tree_node=tree_node->right;
		else
			return &tree_node->at_node;
	
	ErrorInCompiler (NULL,"get_p_at_node_p",NULL);
	
	return NULL;
}

NodeP get_p_at_node (NodeP annoted_node)
{
	NodeP *node_p;
	
	node_p=get_p_at_node_p (annoted_node);
	
	if (node_p!=NULL)
		return *node_p;
	else
		return NULL;
}

unsigned import_system_functions, import_system_array_functions;

#ifndef CLEAN2
IdentP
UseArrayFunctionId (ArrayFunKind kind)
{
	if (import_system_array_functions == 0)
		import_system_array_functions	= gCurrentToken.lineNumber;

	return (ArrayFunctionIds [kind]);
} /* UseArrayFunctionId */
#endif
