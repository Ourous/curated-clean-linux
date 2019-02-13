
#define PrintAnnotation(annot)

static void PrintUnaryState (StateKind kind, ObjectKind obj)
{	
	switch (kind)
	{	case OnB: FPutS ("OnB ", StdError); break;
		case OnA: FPutS ("OnA ", StdError); break;
		case StrictOnA: FPutS ("StrictOnA ", StdError); break;
		case SemiStrict: FPutS ("SemiStrict ", StdError); break;
		case StrictRedirection: FPutS ("StrictRedirection ", StdError); break;
		case Parallel: FPutS ("Parallel ", StdError); break;
		case Undefined: FPutS ("Undefined ", StdError); break;
		case UnderEval: FPutS ("UnderEval ", StdError); break;
	}
	switch (obj){
		case UnknownObj: FPutS ("???", StdError); break;
		case IntObj: FPutS ("Int ", StdError); break;
		case BoolObj: FPutS ("Bool ", StdError); break;
		case CharObj: FPutS ("Char ", StdError); break;
		case StringObj: FPutS ("String ", StdError); break;
		case RealObj: FPutS ("Real ", StdError); break;
		case FileObj: FPutS ("File ", StdError); break;
		case TupleObj: FPutS ("Tuple ", StdError); break;
		case ListObj: FPutS ("List ", StdError); break;
		case ProcIdObj: FPutS ("ProcId ", StdError); break;
	}
} /* PrintUnaryState */


static void PrintState (State state)
{
	return;
	if (ArityOfState (state) == 1)
		PrintUnaryState (KindOfState(state),ObjectOfState(state));
} /* PrintState */
	

static void PrintNode (Node node, Bool brackets);

static void PrintNodeId (NodeId nid, Node follow)
{
	if (nid)
	{	if (nid -> nid_name)
			FPrintF (StdError, "%d.%s",nid->nid_refcount, nid -> nid_name -> Name);
		else
			FPrintF (StdError, "%d.sel%ld",nid->nid_refcount, (long) nid);
		if (follow)
			FPutC (':', StdError);
	}
} /* PrintNodeId */

static void PrintArgument (Args arg, Bool brackets)
{
	PrintState (arg -> arg_state);
	PrintNodeId (arg -> arg_id, arg -> arg_pattern);
	if (arg -> arg_pattern)
		PrintNode (arg -> arg_pattern, brackets);

} /* PrintArgument */

static void PrintArguments (Args args, char separator, Bool brackets)
{
	if (args)
	{	PrintArgument (args, brackets);
		for (args = args -> arg_next; args; args = args -> arg_next)
		{	FPutC (separator, StdError);
			PrintArgument (args, brackets);
		}
	}
} /* PrintArguments */

static void PrintNodeDefs (NodeDefs defs, Bool brackets);

static void PrintNode (Node node, Bool brackets)
{
	if (IsOnACycle (node -> node_number))
		FPutS ("<C>", StdError);
	PrintState (node -> node_state);
	if (node -> node_kind == NormalNodeKind)
	{	if (node->node_symbol -> symb_kind == tuple_denot)
		{	FPutC ('(', StdError);
			PrintArguments (GetNodeArguments(node), ',', False);
			FPutC (')', StdError);
		}
		else if (node->node_symbol -> symb_kind == list_type)
		{	FPutC ('[', StdError);
			PrintArguments (GetNodeArguments(node), ',', False);
			FPutC (']', StdError);
		}
		else
		{	if (brackets && GetNodeArguments(node))
				FPutC ('(', StdError);
			FPutS (ConvertSymbolToString (node->node_symbol), StdError);
			if (GetNodeArguments(node))
			{	FPutC (' ', StdError);
				PrintArguments (GetNodeArguments(node),' ', True);
				if (brackets)
					FPutC (')', StdError);
			}
		}
	}
	else
	{	Args elsepart, thenpart, condpart = GetIfArguments(node);
		thenpart = condpart -> arg_next;
		elsepart = thenpart -> arg_next;
		if (brackets)
			FPutC ('(', StdError);
		FPutS ("IF ", StdError);
		PrintArgument (condpart, True);
		FPutC ('\n', StdError);
		FPutS ("THEN ", StdError);
		PrintArgument (thenpart, True);
		if (node->node_then_node_defs)
		{	FPutS ("[", StdError);
			PrintNodeDefs (node->node_then_node_defs, False);
			FPutS ("]\nELSE ", StdError);
		}
		else
			FPutS ("\nELSE ", StdError);
		PrintArgument (elsepart, True);
		if (GetElseShared(node))
		{	FPutS ("[\n", StdError);
			PrintNodeDefs (GetElseShared(node), False);
			FPutS ("]\n", StdError);
		}
		if (brackets)
			FPutC (')', StdError);
	}

} /* PrintNode */

static void PrintNodeDefs (NodeDefs defs, Bool brackets)
{
	for ( ; defs; defs = defs -> def_next)
	{	FPrintF (StdError, "%d: ", (int) defs->def_number);
		PrintNodeId (defs -> def_id, defs -> def_node);
		if (defs -> def_node)
			PrintNode  (defs -> def_node, False);
		FPutS ("\n", StdError);
	}

} /* PrintNodeDefs */

static void PrintRuleAlt (RuleAlts rulealt)
{
	PrintNode (rulealt -> alt_lhs -> def_node, False);
	FPutS (" -> ", StdError);
	if (rulealt -> alt_kind == Contractum)
		PrintNodeDefs (rulealt -> alt_rhs.alt_rhs_graph, False);
	else
		FPutS (rulealt -> alt_rhs.alt_rhs_root -> redir_id -> nid_name -> Name,
			  StdError);
	FPutS (";\n", StdError);

} /* PrintRuleAlt  */

static void PrintRule (Rule rule)
{
	RuleAlts rulealt = rule -> rule_alts;
	
	for (; rulealt; rulealt = rulealt -> alt_next);
		PrintRuleAlt (rulealt);

} /* PrintRule */	
