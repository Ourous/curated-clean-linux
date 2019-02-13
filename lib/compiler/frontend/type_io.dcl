/*
	module owner: Martijn Vervoort
*/
definition module type_io

import scanner, general, Heap, typeproperties, utilities, checksupport
import StdEnv

:: WriteTypeInfoState
	= { wtis_n_type_vars		:: !Int
	,	wtis_common_defs		:: !{#CommonDefs}	
	,	wtis_type_defs			:: !.{#{#CheckedTypeDef}}
	,	wtis_type_heaps			:: !.TypeHeaps
	,	wtis_var_heap			:: !.VarHeap
	,	wtis_main_dcl_module_n 	:: !Int
	,	wtis_icl_generic_defs	:: !{#GenericDef}
	};

write_type_info_of_types_and_constructors :: !CommonDefs !Int !Int !*File !*WriteTypeInfoState -> (!*File,!*WriteTypeInfoState)

class WriteTypeInfo a 
where
	write_type_info :: a !*File !*WriteTypeInfoState -> (!*File,!*WriteTypeInfoState)

instance WriteTypeInfo Char,[a] | WriteTypeInfo a, {#b} | Array {#} b & WriteTypeInfo b
