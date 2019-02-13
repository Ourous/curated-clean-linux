
extern Ident AnnotatedId, ListId, TupleId, ConsId, NilId, ApplyId, SelectId, IfId, FailId, StdBoolId,
			 AndId, OrId, StdArrayId, ArrayFunctionIds [], ArrayId, StrictArrayId, UnboxedArrayId, ArrayClassId;
#if STRICT_LISTS
extern Ident StrictListId,UnboxedListId,TailStrictListId,StrictTailStrictListId,UnboxedTailStrictListId;
#endif
#ifdef CLEAN2
extern Ident DynamicId;
#endif
#if SA_RECOGNIZES_ABORT_AND_UNDEF
extern Ident StdMiscId,abort_id,undef_id;
#endif
extern Ident PreludeId,seq_id,system_seq_id;

extern Symbol StartSymbol;
extern SymbDef scc_dependency_list;

SymbDef MakeNewSymbolDefinition (char * module, Ident name, int arity, SDefKind kind);
char *ConvertSymbolToString (Symbol symb);
void ReadInlineCode (void);
void InitChecker (void);
void GenDependencyList (void);
NodeDefs NewNodeDef (NodeId nid, Node node);

#ifdef CLEAN2
void ClearOpenDefinitionModules (void);
void AddOpenDefinitionModule (SymbolP moduleNameSymbol, DefMod definitionModule);
#endif
