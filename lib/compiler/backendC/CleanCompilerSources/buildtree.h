
typedef enum
{
	LazyArrayInstance, StrictArrayInstance, UnboxedArrayInstance, NrOfArrayInstances
} ArrayInstance;

typedef enum
{
	NoQuantifier, AllQuantifier, ExistQuantifier, ExistAttributeQuantifier
} Quantifier;

typedef enum
{
										/* defining symbol */
	kUnknownRuleAlternativeKind,		/* ':==', '=:', '=>' or '=' */
	kUnknownFunctionAlternativeKind,	/* '=>' or '=' */
	kFunctionAlternativeKind,			/* '=' */
	kExplicitFunctionAlternativeKind,	/* '=>' */
	kCAFAlternativeKind,				/* '=:' */
	kArrowAlternativeKind				/* '->' */
} RuleAltKind;

extern Args NewArgument (NodeP pattern);
extern NodeP NewNode (SymbolP symb, Args args, int arity);
extern NodeP NewIfNode (void);
extern NodeP NewSelectorNode (SymbolP symb, Args args, int arity);
extern NodeP NewNodeIdNode (NodeIdP node_id);
extern NodeP NewUpdateNode (SymbolP symb,Args args,int arity);
extern NodeP NewNodeByKind (NodeKind nodeKind, SymbolP symb, Args args, int arity);
# define	NewNormalNode(symb, args, arity)	NewNodeByKind (NormalNode, (symb), (args), (arity))
# define	NewRecordNode(symb, args, arity)	NewNodeByKind (RecordNode, (symb), (args), (arity))
# define	NewMatchNode(symb, args, arity)		NewNodeByKind (MatchNode, (symb), (args), (arity))
# define	NewCons(element)	NewNormalNode (ConsSymbol, element, 2)
# define	NewNil()			NewNormalNode (NilSymbol, NIL, 0)
# define	NewFalse()			NewNormalNode (FalseSymbol, NIL, 0)
# define	NewTrue()			NewNormalNode (TrueSymbol, NIL, 0)

#if STRICT_LISTS
# define	NewStrictNil() NewNormalNode (StrictNilSymbol, NIL, 0)
# define	NewUnboxedNil() NewNormalNode (UnboxedNilSymbol, NIL, 0)
# define	NewTailStrictNil() NewNormalNode (TailStrictNilSymbol, NIL, 0)
# define	NewStrictTailStrictNil() NewNormalNode (StrictTailStrictNilSymbol, NIL, 0)
# define	NewUnboxedTailStrictNil() NewNormalNode (UnboxedTailStrictNilSymbol, NIL, 0)
#endif

extern	NodeP NewIntNode (int value);

extern NodeIdP NewNodeId (IdentP nid);
extern StrictNodeIdP NewStrictNodeId (NodeIdP node_id, StrictNodeIdP next);
extern TypeVar NewTypeVar (IdentP nid);
extern NodeDefs NewNodeDefinition (NodeIdP nid, NodeP node);
extern SymbolP NewSymbol (SymbKind symbolKind);
extern TypeNode NewTypeNode (Annotation annot, AttributeKind attr, SymbolP symb, TypeArgs args, int arity);
extern TypeArgs NewTypeArgument (TypeNode pattern);
extern TypeNode NewTypeVarNode (TypeVar node_id,Annotation annot, AttributeKind attr);

extern NodeP NewSelectNode (SymbolP selectSymbol, NodeIdP selectId, int arity);
extern NodeIdP BuildSelect (NodeP node, NodeDefs **node_defs_p);
extern NodeIdP BuildSelectors (NodeP pattern, NodeP node, NodeDefs **node_defs_p);

extern SymbolP NewSelectSymbol (int arity);
extern SymbolP NewTupleTypeSymbol (int arity);
extern SymbolP NewListFunctionSymbol (void);

extern	ImpRules	NewImpRule (unsigned line_number,TypeAlts typeAlternative,NodeP rule_root);
extern RuleAltP	NewRuleAlt (void);

extern NodeIdP FreshNodeId (NodeP node, NodeDefs **node_defs_h);

extern TypeArgs ConvertFieldsToTypeArguments (FieldList fields);

extern char *CopyString (char *to, char *from, int *rest_size);

extern IdentP DetermineNewSymbolId (char *prefix, TypeNode inst_type, TableKind table);

extern	IdentP	gArrayIdents [];

extern SymbolP	BasicTypeSymbols [],
				ArraySymbols [],
				TrueSymbol, FalseSymbol, TupleSymbol, ListSymbol, ConsSymbol, NilSymbol,
				ApplySymbol, ApplyTypeSymbol, SelectSymbols[],
				FailSymbol, IfSymbol;
#if STRICT_LISTS
extern SymbolP
	StrictListSymbol, StrictConsSymbol, StrictNilSymbol,
	UnboxedListSymbol, UnboxedConsSymbol, UnboxedNilSymbol,
	TailStrictListSymbol, TailStrictConsSymbol, TailStrictNilSymbol,
	StrictTailStrictListSymbol, StrictTailStrictConsSymbol, StrictTailStrictNilSymbol,
	UnboxedTailStrictListSymbol, UnboxedTailStrictConsSymbol, UnboxedTailStrictNilSymbol;
#endif

extern	SymbolP	TupleTypeSymbols [];
IdentP UseArrayFunctionId (ArrayFunKind kind);
void InitialiseEnumFunctionIds (void);

extern unsigned import_system_functions, import_system_array_functions;

void clear_p_at_node_tree (void);
void store_p_at_node (NodeP annoted_node,NodeP at_node);
NodeP *get_p_at_node_p (NodeP annoted_node);
NodeP get_p_at_node (NodeP annoted_node);

# define	kCasePrefix				"_case"
# define	kLambdaPrefix			"_lambda"
# define	kArrayGeneratorPrefix	"_array"
# define	kListGeneratorPrefix	"_list"
# define	kFromPrefix				"_from"
# define	kFromThenPrefix			"_from_then"
# define	kFromToPrefix			"_from_to"
# define	kFromThenToPrefix		"_from_then_to"

