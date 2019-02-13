definition module convertimportedtypes

import syntax, transform, trans

convertIclModule :: !Int !{# CommonDefs} !*{#{# CheckedTypeDef}} !ImportedConstructors !*VarHeap !*TypeHeaps
									 -> (!*{#{# CheckedTypeDef}},!ImportedConstructors,!*VarHeap,!*TypeHeaps)

convertDclModule :: !Int !{# DclModule} !{# CommonDefs} !*{#{# CheckedTypeDef}} !ImportedConstructors !*VarHeap !*TypeHeaps
													-> (!*{#{# CheckedTypeDef}},!ImportedConstructors,!*VarHeap,!*TypeHeaps)

convertMemberTypes :: !Int !{#DclModule} !{#CommonDefs} !NumberSet !*{#{#CheckedTypeDef}} !ImportedConstructors !*VarHeap !*TypeHeaps
															   -> (!*{#{#CheckedTypeDef}},!ImportedConstructors,!*VarHeap,!*TypeHeaps)

convertImportedTypeSpecifications :: !Int !{# DclModule}  !{# {# FunType} } !{# CommonDefs} !ImportedConstructors !ImportedFunctions
	!*{# {#CheckedTypeDef}} !*TypeHeaps !*VarHeap -> (!*{#{#CheckedTypeDef}}, !*TypeHeaps, !*VarHeap)
