definition module overloading

import StdEnv
import syntax, typesupport

::	InstanceTree = IT_Node !(Global Index) !InstanceTree !InstanceTree | IT_Empty 

::	ClassInstanceInfo :== {# {! .InstanceTree}}

::	ArrayInstance =
	{	ai_record		:: !TypeSymbIdent
	,	ai_members		:: !{#ClassInstanceMember}
	}

::	GlobalTCInstance =
	{	gtci_type		:: !GlobalTCType
	,	gtci_index		:: !Index
	}

::	SpecialInstances =
	{	si_next_array_member_index			:: !Index
	,	si_array_instances					:: ![ArrayInstance]
	,	si_list_instances					:: ![ArrayInstance]
	,	si_tail_strict_list_instances		:: ![ArrayInstance]
	}
	
::	OverloadingState =
	{	os_type_heaps			:: !.TypeHeaps
	,	os_var_heap				:: !.VarHeap
	,	os_symbol_heap			:: !.ExpressionHeap
	,	os_generic_heap			:: !.GenericHeap
	,	os_predef_symbols		:: !.PredefinedSymbols
	,	os_special_instances	:: !.SpecialInstances
	,	os_error				:: !.ErrorAdmin				
	}

::	LocalTypePatternVariable
::	DictionaryTypes :== [(Index, [ExprInfoPtr])]

tryToSolveOverloading :: ![(Optional [TypeContext], [ExprInfoPtr], IdentPos, Index)] !Int !{# CommonDefs } !ClassInstanceInfo !*Coercions !*OverloadingState !{# DclModule}
	-> (![TypeContext], !*Coercions, ![LocalTypePatternVariable], DictionaryTypes, !*OverloadingState)

::	TypeCodeInfo =
	{	tci_type_var_heap					:: !.TypeVarHeap
	,	tci_attr_var_heap					:: !.AttrVarHeap
	,	tci_dcl_modules						:: !{# DclModule}
	,	tci_common_defs						:: !{# CommonDefs }
	}

removeOverloadedFunctions :: ![Index] ![LocalTypePatternVariable] !Int !*{#FunDef} !*{! FunctionType} !*ExpressionHeap
	!*TypeCodeInfo !*VarHeap !*ErrorAdmin !*{#PredefinedSymbol} //!*{#PredefinedSymbol}
		-> (!*{#FunDef}, !*{! FunctionType}, !*ExpressionHeap, !*TypeCodeInfo, !*VarHeap, !*ErrorAdmin, !*{#PredefinedSymbol})
