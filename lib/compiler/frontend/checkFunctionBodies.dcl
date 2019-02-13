definition module checkFunctionBodies

import syntax, checksupport

::	Dynamics		:== [ExprInfoPtr]

::	ExpressionState =
	{	es_expr_heap	:: !.ExpressionHeap
	,	es_var_heap			:: !.VarHeap
	,	es_type_heaps		:: !.TypeHeaps
	,	es_generic_heap		:: !.GenericHeap
	,	es_calls			:: ![FunCall]
	,	es_dynamics			:: ![ExprInfoPtr]
	,	es_fun_defs			:: !.{# FunDef}
	}
	
::	ExpressionInput =
	{	ei_expr_level	:: !Level
	,	ei_fun_index	:: !FunctionOrMacroIndex
	,	ei_fun_level	:: !Level
	,	ei_mod_index	:: !Index
	,	ei_local_functions_index_offset :: !Int
	}

checkFunctionBodies :: !FunctionBody !Ident !.ExpressionInput !*ExpressionState !*ExpressionInfo !*CheckState
	-> (!FunctionBody, ![FreeVar], !*ExpressionState, !*ExpressionInfo, !*CheckState)
