definition module type

import StdArray
import syntax, check

typeProgram :: !{! Group} !Int !*{# FunDef} !IndexRange  !(Optional Bool) !CommonDefs ![!GlobalInstanceIndex!] !{# DclModule} !NumberSet
  																						 !*TypeDefInfos !*Heaps !*PredefinedSymbols !*File !*File
	-> (!Bool, !*{# FunDef}, !ArrayAndListInstances, !{# CommonDefs}, !{# {# FunType} }, !*TypeDefInfos,!*Heaps,!*PredefinedSymbols,!*File,!*File)

addPropagationAttributesToAType :: {#CommonDefs} !AType !*PropState -> *(!AType,!*PropState);

tryToExpand :: !Type !TypeAttribute !{# CommonDefs} !*TypeHeaps -> (!Bool, !Type, !*TypeHeaps)

::	PropState =
	{	prop_type_heaps	:: !.TypeHeaps
	,	prop_td_infos	:: !.TypeDefInfos
	,	prop_attr_vars	:: ![AttributeVar]
	,	prop_attr_env	:: ![AttrInequality]
	,	prop_error		:: !.Optional .ErrorAdmin
	}

class unify a :: !a !a !TypeInput !*{! Type} !*TypeHeaps -> (!Bool, !*{! Type}, !*TypeHeaps)

instance unify AType

::	TypeInput =
	! {	ti_common_defs	:: !{# CommonDefs }
	,	ti_functions	:: !{# {# FunType }}
	,	ti_main_dcl_module_n :: !Int
	,	ti_expand_newtypes :: !Bool
	}

class arraySubst type :: !type !u:{!Type} -> (!Bool,!type, !u:{! Type})

instance arraySubst AType
