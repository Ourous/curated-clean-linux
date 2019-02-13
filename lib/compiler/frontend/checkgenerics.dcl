definition module checkgenerics

import syntax
from checksupport import ::Heaps,::CheckState,::ErrorAdmin

checkGenericDefs :: !Index !(Optional (CopiedDefinitions, Int))
		!*{#GenericDef} !*{#CheckedTypeDef} !*{#ClassDef} !*{#DclModule} !*Heaps !*CheckState
	-> (!*{#GenericDef},!*{#CheckedTypeDef},!*{#ClassDef},!*{#DclModule},!*Heaps,!*CheckState)

checkGenericCaseDefs :: !Index !*{#GenericCaseDef} !*{#GenericDef} !u:{#CheckedTypeDef} !*{#ClassDef} !*{#DclModule} !*Heaps !*CheckState
						   -> (!*{#GenericCaseDef},!*{#GenericDef},!u:{#CheckedTypeDef},!*{#ClassDef},!*{#DclModule},!.Heaps,!.CheckState)

convert_generic_instances :: !Int !Int !*{#GenericCaseDef} !*{#ClassDef} !*SymbolTable !*ErrorAdmin !*{#DclModule}
						-> (!.[FunDef],!*{#GenericCaseDef},!*{#ClassDef},!*SymbolTable,!*ErrorAdmin,!*{#DclModule})

create_gencase_funtypes :: !Index !*{#GenericCaseDef} !*Heaps
			-> (!Index,![FunType],!*{#GenericCaseDef},!*Heaps)
