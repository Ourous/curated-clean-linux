definition module genericsupport

import syntax

lookupGenericClassInfo :: 
		!TypeKind 
		!GenericClassInfos	
	-> 	(Optional GenericClassInfo)

addGenericClassInfo :: 
		!GenericClassInfo 
		!GenericClassInfos 
	->	GenericClassInfos

getGenericClassInfo ::
	!(Global Index)
	!TypeKind
	!{#CommonDefs}		
	!*GenericHeap
	-> 		
	( Optional GenericClassInfo
	, !*GenericHeap					
	)
getGenericMember :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)

getGenericClass :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)


//****************************************************************************************
//	Ident Helpers
//****************************************************************************************
makeIdent 					:: !String -> Ident
postfixIdent 				:: !String !String -> Ident
genericIdentToClassIdent 	:: !String !TypeKind -> Ident
genericIdentToMemberIdent 	:: !String !TypeKind -> Ident
genericIdentToFunIdent 		:: !String !TypeCons -> Ident
type_cons_to_string :: !TypeCons -> {#Char}
kind_to_short_string :: !TypeKind -> {#Char}

field_n_of_GenericTypeDefDescriptor :: !String -> Int
field_n_of_GenericConsDescriptor :: !String -> Int
field_n_of_GenericFieldDescriptor :: !String -> Int
field_n_of_GenericRecordDescriptor :: !String -> Int

field_0_name_of_generic_info :: !Int -> String
field_1_name_of_generic_info :: !Int -> String
field_2_name_of_generic_info :: !Int -> String
field_3_name_of_generic_info :: !Int -> String
field_4_name_of_generic_info :: !Int -> String
field_5_name_of_generic_info :: !Int -> String
