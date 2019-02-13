definition module hashtable

import syntax

::	.HashTableEntry 

::	HashTable =
	{	hte_symbol_heap	:: !.SymbolTable
	,	hte_entries		:: !.{! .HashTableEntry}
	,	hte_mark	:: !Int // 1 for .icl modules, otherwise 0
	}

newHashTable :: !*SymbolTable -> *HashTable

set_hte_mark :: !Int !*HashTable -> *HashTable

::	IdentClass	= IC_Expression
				| IC_Type
				| IC_TypeAttr
				| IC_Class
				| IC_Module !QualifiedIdents
				| IC_Field !Ident
				| IC_Selector
				| IC_Instance ![Type]
				| IC_InstanceMember ![Type]
				| IC_Generic
				| IC_GenericCase !Type
				| IC_GenericDeriveClass !Type
				| IC_TypeExtension !{#Char}/*module name*/
				| IC_Unknown

::	QualifiedIdents	= QualifiedIdents !Ident !IdentClass !QualifiedIdents
					| NoQualifiedIdents;

:: BoxedIdent = {boxed_ident::!Ident}

putIdentInHashTable :: !String !IdentClass !*HashTable -> (!BoxedIdent, !*HashTable)
putQualifiedIdentInHashTable :: !String !BoxedIdent !IdentClass !*HashTable -> (!BoxedIdent, !*HashTable)
putPredefinedIdentInHashTable :: !Ident !IdentClass !*HashTable -> *HashTable

get_qualified_idents_from_hash_table :: !Ident !*HashTable -> (!QualifiedIdents,!*HashTable)

remove_icl_symbols_from_hash_table :: !*HashTable -> *HashTable
