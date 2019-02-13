definition module checktypes

import checksupport

checkTypeDefs :: !Index !(Optional (CopiedDefinitions, Int))
		!*{#CheckedTypeDef} !*{#ConsDef} !*{#SelectorDef} !v:{#ClassDef} !*{#DclModule} !*Heaps !*CheckState
	-> (!*{#CheckedTypeDef},!*{#ConsDef},!*{#SelectorDef},!v:{#ClassDef},!*{#DclModule},!*Heaps,!*CheckState)

checkFunctionType :: !Index !SymbolType !FunSpecials !u:{#CheckedTypeDef} !v:{#ClassDef} !u:{#DclModule} !*TypeHeaps !*CheckState
						-> (!SymbolType,!FunSpecials,!u:{#CheckedTypeDef},!v:{#ClassDef},!u:{#DclModule},!*TypeHeaps,!*CheckState)

checkMemberType :: !Index !SymbolType !u:{# CheckedTypeDef} !v:{# ClassDef} !u:{# DclModule} !*TypeHeaps !*CheckState
	-> (!SymbolType, !u:{# CheckedTypeDef}, !v:{# ClassDef}, !u:{# DclModule}, !*TypeHeaps, !*CheckState)

checkInstanceType :: !Index !GlobalIndex !ClassIdent !InstanceType !Specials !u:{# CheckedTypeDef} !v:{# ClassDef} !u:{# DclModule} !*TypeHeaps !*CheckState
												 -> (!InstanceType,!Specials,!u:{# CheckedTypeDef},!v:{# ClassDef},!u:{# DclModule},!*TypeHeaps,!*CheckState)

checkSuperClasses :: ![TypeVar] ![TypeContext] !Index !u:{# CheckedTypeDef} !v:{# ClassDef} !u:{# DclModule} !*TypeHeaps !*CheckState
	-> (![TypeVar], ![TypeContext], !u:{#CheckedTypeDef}, !v:{# ClassDef}, !u:{# DclModule}, !*TypeHeaps, !*CheckState)

checkDynamicTypes :: !Index ![ExprInfoPtr] !(Optional SymbolType)
		!u:{#CheckedTypeDef} !v:{#ClassDef} !u:{#DclModule} !*TypeHeaps !*ExpressionHeap !*CheckState
	-> (!u:{#CheckedTypeDef},!v:{#ClassDef},!u:{#DclModule},!*TypeHeaps,!*ExpressionHeap,!*CheckState)

createClassDictionaries ::		  !Bool !Index !Index !Index !Index !*{#CheckedTypeDef} !*{# SelectorDef} !*{# ConsDef} !*{#ClassDef} !*{#DclModule} !*TypeVarHeap !*VarHeap !*SymbolTable
	-> (![CheckedTypeDef],![SelectorDef],![ConsDef],!DictionaryInfo,!*{#CheckedTypeDef},!*{# SelectorDef},!*{# ConsDef},!*{#ClassDef},!*{#DclModule},!*TypeVarHeap,!*VarHeap,!*SymbolTable)

createMoreClassDictionaries ::	   !Int !Index !Index !Index !Index !*{#CheckedTypeDef} !*{#SelectorDef} !*{#ConsDef} !*{#ClassDef} !*{#DclModule} !*TypeVarHeap !*VarHeap !*SymbolTable
					-> (![CheckedTypeDef],![SelectorDef],![ConsDef],!*{#CheckedTypeDef},!*{#SelectorDef},!*{#ConsDef},!*{#ClassDef},!*{#DclModule},!*TypeVarHeap,!*VarHeap,!*SymbolTable)

removeVariablesFromSymbolTable :: !Int ![TypeVar] !*SymbolTable -> *SymbolTable
