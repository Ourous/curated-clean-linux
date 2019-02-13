implementation module genericsupport

import syntax

getGenericClassInfo ::
	!(Global Index)
	!TypeKind
	!{#CommonDefs}		
	!*GenericHeap
	-> 		
	( Optional GenericClassInfo
	, !*GenericHeap					
	)
getGenericClassInfo {glob_module, glob_object} kind modules generic_heap
	#! (gen_def=:{gen_info_ptr}) = modules.[glob_module].com_generic_defs.[glob_object]
	#! ({gen_classes}, generic_heap) = readPtr gen_info_ptr generic_heap
	#! opt_class_info = lookupGenericClassInfo kind gen_classes
	= (opt_class_info, generic_heap)

getGenericMember :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)
getGenericMember gen kind modules generic_heap
	# (opt_class_info, generic_heap) = getGenericClassInfo  gen kind modules generic_heap
	= case opt_class_info of
		No -> (No, generic_heap) 
		Yes {gci_module, gci_member}
			#! member_glob = {glob_module = gci_module, glob_object = gci_member}
			-> (Yes member_glob, generic_heap)

getGenericClass :: 
	!(Global Index) 	// generic
	!TypeKind 			// kind argument
	!{#CommonDefs} 		// modules
	!*GenericHeap
	-> 		
	( Optional (Global Index)
	, !*GenericHeap					
	)
getGenericClass gen kind modules generic_heap
	# (opt_class_info, generic_heap) = getGenericClassInfo  gen kind modules generic_heap
	= case opt_class_info of
		No -> (No, generic_heap) 
		Yes {gci_module, gci_class}
			#! class_glob = {glob_module = gci_module, glob_object = gci_class}
			-> (Yes class_glob, generic_heap)

lookupGenericClassInfo :: !TypeKind !GenericClassInfos	-> (Optional GenericClassInfo)
lookupGenericClassInfo kind class_infos
	#! hash_index = case kind of
		KindConst -> 0
		KindArrow kinds -> length kinds
	= lookup kind class_infos.[hash_index] 		
where
	lookup kind [] = No
	lookup kind [gci:gcis]
		| gci.gci_kind == kind 	= Yes gci
								= lookup kind gcis

addGenericClassInfo :: !GenericClassInfo !GenericClassInfos -> GenericClassInfos
addGenericClassInfo class_info=:{gci_kind} class_infos
	#! hash_index = case gci_kind of
		KindConst -> 0
		KindArrow kinds -> length kinds
	#! (class_infos1, class_infos) = class_infos ! [hash_index]
	#! class_infos1 = [class_info:class_infos1]	
	= {{x\\x<-:class_infos} & [hash_index] = class_infos1 }

//****************************************************************************************
//	Ident Helpers
//****************************************************************************************
makeIdent :: !String -> Ident
makeIdent str = {id_name = str, id_info = nilPtr} 

postfixIdent :: !String !String -> Ident
postfixIdent id_name postfix = makeIdent (id_name +++ postfix)

genericIdentToClassIdent :: !String !TypeKind -> Ident
genericIdentToClassIdent id_name kind
	= postfixIdent id_name ("_" +++ kind_to_short_string kind) 

kind_to_short_string :: !TypeKind -> {#Char}
kind_to_short_string KindConst = "s"
kind_to_short_string (KindArrow kinds) = kinds_to_str kinds +++ "s"
where
	kinds_to_str [] = ""
	kinds_to_str [KindConst:ks] = "s" +++ kinds_to_str ks
	kinds_to_str [k:ks] = "o" +++ (kind_to_short_string k) +++ "c" +++ kinds_to_str ks	

genericIdentToMemberIdent :: !String !TypeKind -> Ident
genericIdentToMemberIdent id_name kind
	= genericIdentToClassIdent id_name kind

genericIdentToFunIdent :: !String !TypeCons -> Ident
genericIdentToFunIdent id_name type_cons
	= postfixIdent id_name ("_" +++ type_cons_to_string type_cons)

type_cons_to_string :: !TypeCons -> {#Char}
type_cons_to_string (TypeConsSymb {type_ident}) = toString type_ident
type_cons_to_string (TypeConsBasic bt) = toString bt
type_cons_to_string TypeConsArrow = "ARROW"
type_cons_to_string (TypeConsVar tv) = tv.tv_ident.id_name
type_cons_to_string (TypeConsQualifiedIdent _ type_name) = type_name

field_n_of_GenericTypeDefDescriptor :: !String -> Int
field_n_of_GenericTypeDefDescriptor "gtd_name" = 0
field_n_of_GenericTypeDefDescriptor "gtd_arity" = 1
field_n_of_GenericTypeDefDescriptor "gtd_num_conses" = 2
field_n_of_GenericTypeDefDescriptor "gtd_conses" = 3
field_n_of_GenericTypeDefDescriptor _ = -1

field_n_of_GenericConsDescriptor :: !String -> Int
field_n_of_GenericConsDescriptor "gcd_name" = 0
field_n_of_GenericConsDescriptor "gcd_arity" = 1
field_n_of_GenericConsDescriptor "gcd_prio" = 2
field_n_of_GenericConsDescriptor "gcd_type_def" = 3
field_n_of_GenericConsDescriptor "gcd_type" = 4
field_n_of_GenericConsDescriptor "gcd_index" = 5
field_n_of_GenericConsDescriptor  _ = -1

field_n_of_GenericFieldDescriptor :: !String -> Int
field_n_of_GenericFieldDescriptor "gfd_name" = 0
field_n_of_GenericFieldDescriptor "gfd_index" = 1
field_n_of_GenericFieldDescriptor "gfd_cons" = 2
field_n_of_GenericFieldDescriptor _ = -1

field_n_of_GenericRecordDescriptor :: !String -> Int
field_n_of_GenericRecordDescriptor "grd_name" = 0
field_n_of_GenericRecordDescriptor "grd_arity" = 1
field_n_of_GenericRecordDescriptor "grd_type_arity" = 2
field_n_of_GenericRecordDescriptor "grd_type" = 3
field_n_of_GenericRecordDescriptor "grd_fields" = 4
field_n_of_GenericRecordDescriptor _ = -1

field_0_name_of_generic_info :: !Int -> String
field_0_name_of_generic_info 0 = "gtd_name"
field_0_name_of_generic_info 1 = "gcd_name"
field_0_name_of_generic_info 2 = "grd_name"
field_0_name_of_generic_info 3 = "gfd_name"

field_1_name_of_generic_info :: !Int -> String
field_1_name_of_generic_info 0 = "gtd_arity"
field_1_name_of_generic_info 1 = "gcd_arity"
field_1_name_of_generic_info 2 = "grd_arity"
field_1_name_of_generic_info 3 = "gfd_index"

field_2_name_of_generic_info :: !Int -> String
field_2_name_of_generic_info 0 = "gtd_num_conses"
field_2_name_of_generic_info 1 = "gcd_prio"
field_2_name_of_generic_info 2 = "grd_type_arity"
field_2_name_of_generic_info 3 = "gfd_cons"

field_3_name_of_generic_info :: !Int -> String
field_3_name_of_generic_info 0 = "gtd_conses"
field_3_name_of_generic_info 1 = "gcd_type_def"
field_3_name_of_generic_info 2 = "grd_type"

field_4_name_of_generic_info :: !Int -> String
field_4_name_of_generic_info 1 = "gcd_type"
field_4_name_of_generic_info 2 = "grd_fields"

field_5_name_of_generic_info :: !Int -> String
field_5_name_of_generic_info 1 = "gcd_index"
