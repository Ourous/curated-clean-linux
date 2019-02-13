definition module transform

import syntax, checksupport

:: PredefSymbolsForTransform = { predef_alias_dummy :: !PredefinedSymbol, predef_and :: !PredefinedSymbol, predef_or :: !PredefinedSymbol };

partitionateDclMacros :: !IndexRange !Index !PredefSymbolsForTransform !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
																   -> (!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin )

partitionateIclMacros :: !IndexRange !Index !PredefSymbolsForTransform !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
																   -> (!*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin )

partitionateAndLiftFunctions :: ![IndexRange] !Index !PredefSymbolsForTransform !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
																-> (!*{!Group}, !*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin)

:: UnexpandedDclMacros:==[(Int,Int,FunDef)]

partitionateAndLiftMacro :: !Int !Int !Index !PredefinedSymbols !Int !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
								   -> (![[Int]],!UnexpandedDclMacros,!*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin)

restore_unexpanded_dcl_macros :: !UnexpandedDclMacros !*{#*{#FunDef}} -> *{#*{#FunDef}}

::	CopiedLocalFunctions

::	CollectState =
	{	cos_var_heap		:: !.VarHeap
	,	cos_expression_heap :: !.ExpressionHeap
	,	cos_error			:: !.ErrorAdmin
	,	cos_predef_symbols_for_transform :: !PredefSymbolsForTransform
	}

determineVariablesAndRefCounts :: ![FreeVar] !Expression !*CollectState -> (!Expression , ![FreeVar], ![FreeVar], ![DynamicPtr], !*CollectState)

::	UnfoldState =
	{	us_var_heap				:: !.VarHeap
	,	us_symbol_heap			:: !.ExpressionHeap
	,	us_local_macro_functions :: !Optional CopiedLocalFunctions
	}

class unfold a :: !a !*UnfoldState -> (!a, !*UnfoldState)
instance unfold Expression, CasePatterns

::	CopiedLocalFunction = { old_function_n :: !FunctionOrMacroIndex, new_function_n :: !Int }

copy_macro_and_local_functions ::		!FunDef !Int !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap
	-> (!FunDef,![(CopiedLocalFunction,FunDef)],!Int,!*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap)
