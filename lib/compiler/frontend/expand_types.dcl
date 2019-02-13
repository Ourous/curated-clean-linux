definition module expand_types

import syntax

simplifyTypeApplication :: !Type ![AType] -> Type

convertSymbolType :: !Bool !{#CommonDefs} !SymbolType !Int !*ImportedTypes !ImportedConstructors !*TypeHeaps !*VarHeap 
										  -> (!SymbolType, !*ImportedTypes,!ImportedConstructors,!*TypeHeaps,!*VarHeap)

convertSymbolTypeWithoutExpandingAbstractSynTypes :: !Bool !{#CommonDefs} !SymbolType !Int
							!*ImportedTypes !ImportedConstructors !*TypeHeaps !*VarHeap 
	-> (!SymbolType, !Bool, !*ImportedTypes,!ImportedConstructors,!*TypeHeaps,!*VarHeap)

convertSymbolTypeWithoutCollectingImportedConstructors :: !Bool !{#CommonDefs} !SymbolType !Int !*ImportedTypes !*TypeHeaps !*VarHeap 
																				-> (!SymbolType,!*ImportedTypes,!*TypeHeaps,!*VarHeap)

addTypesOfDictionaries :: !{#CommonDefs} ![TypeContext] ![AType] -> [AType]

RemoveAnnotationsMask:==1
ExpandAbstractSynTypesMask:==2
DontCollectImportedConstructors:==4

::	ExpandTypeState =
	{	ets_type_defs			:: !.{#{#CheckedTypeDef}}
	,	ets_collected_conses	:: !ImportedConstructors
	,	ets_type_heaps			:: !.TypeHeaps
	,	ets_var_heap			:: !.VarHeap
	,	ets_main_dcl_module_n :: !Int
	,	ets_contains_unexpanded_abs_syn_type :: !Bool
	}

class expandSynTypes a :: !Int !{#CommonDefs} !a !*ExpandTypeState -> (!Bool,!a, !*ExpandTypeState)

instance expandSynTypes (a,b) | expandSynTypes a & expandSynTypes b special a=[AType],b=AType

class substitute a :: !a !*TypeHeaps -> (!Bool, !a, !*TypeHeaps)

instance substitute Type,AType,TypeContext,AttrInequality,CaseType
instance substitute [a] | substitute a special a=AType; a=TypeContext; a=AttrInequality

class removeAnnotations a :: !a  -> (!Bool, !a)

instance removeAnnotations Type,SymbolType
