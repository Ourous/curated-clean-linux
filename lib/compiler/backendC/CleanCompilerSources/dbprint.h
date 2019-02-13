
extern void PrintRuleNode (Node node,Bool brackets,int n_leading_spaces,File file);
extern void PrintRuleAlt (RuleAlts rulealt,int n_leading_spaces,File file);
extern void PrintNodeDef (NodeDefP def_p,int n_leading_spaces,File file);
extern void PrintNodeDefs (NodeDefs defs,int n_leading_spaces,File file);
extern void PrintImpRule (ImpRules rule,int n_leading_spaces,File file);
extern void PrintTypeNode (TypeNode node, File file);
extern void DPrintNodeId (NodeId nid, File file);
extern void DPrintNodeIdS (char *s,NodeId nid, File file);
extern void PrintTypeAlt (TypeAlts type_alts, File file, Bool with_equats);
extern void PrintState (StateS state, File file);

extern void PrintRules (ImpRules rules,File file);
