implementation module graph_copy_with_names;

import StdEnv;
import _SystemStrictLists;

import code from "copy_graph_to_string_interface.";
import code from "copy_graph_to_string.";
import code from "copy_string_to_graph_interface.";
import code from "copy_string_to_graph.";

import symbols_in_program;

copy_to_string :: !.a -> *{#Char};
copy_to_string g = code {
    .d 1 0
        jsr _copy_graph_to_string
    .o 1 0
}

copy_from_string :: !*{#Char} -> (.a,!Int);
copy_from_string g = code {
    .d 1 0
        jsr _copy_string_to_graph
    .o 1 0
        pushI 0
}
get_D_from_string s i :== IF_INT_64_OR_32 (get_D_from_string_64 s i) (get_D_from_string_32 s i);

get_D_from_string_32 :: !{#Char} !Int -> Int;
get_D_from_string_32 s i = code inline {
	push_a_b 0
	pop_a 1
	addI
	load_i 8
}

get_D_from_string_64 :: !{#Char} !Int -> Int;
get_D_from_string_64 s i = code inline {
	push_a_b 0
	pop_a 1
	addI
	load_i 16
}

get_D_node_arity :: !Int -> Int;
get_D_node_arity d = code inline {
	load_si16 -2
}

get_D_record_a_arity :: !Int -> Int;
get_D_record_a_arity d = code inline {
	load_si16 0
}

get_thunk_arity a :== IF_INT_64_OR_32 (get_thunk_arity_64 a) (get_thunk_arity_32 a);

get_thunk_arity_32:: !Int -> Int;
get_thunk_arity_32 a = code {
	load_i -4
}

get_thunk_arity_64 :: !Int -> Int;
get_thunk_arity_64 a = code {
	load_si32 -4
}

get_thunk_descriptor a :== IF_INT_64_OR_32 (get_thunk_descriptor_64_platform a) (get_thunk_descriptor_32_platform a);

get_thunk_descriptor_32_platform :: !Int -> Int;
get_thunk_descriptor_32_platform a = if (is_using_desc_relative_to_array == 0) (get_thunk_descriptor_32 a) (get_thunk_descriptor_32_PIC a);

get_thunk_descriptor_64_platform :: !Int -> Int;
get_thunk_descriptor_64_platform a = if (not isMacOS) (get_thunk_descriptor_64 a) (get_thunk_descriptor_64_MacOS a);

get_thunk_descriptor_64 :: !Int -> Int;
get_thunk_descriptor_64 a = code {
	load_si32 -8
}

get_thunk_descriptor_64_MacOS :: !Int -> Int;
get_thunk_descriptor_64_MacOS a = code {
	push_b 0
	load_si32 -8
	addI
	pushI -8
	addI
}

get_thunk_descriptor_32 :: !Int -> Int;
get_thunk_descriptor_32 a = code {
	load_i -8
}

get_thunk_descriptor_32_PIC :: !Int -> Int;
get_thunk_descriptor_32_PIC a = code {
	push_b 0
	load_i -8
	addI
	pushI -8
	addI
}

is_Int_D :: !Int -> Bool;
is_Int_D d = code inline {
	eq_desc_b INT 0
}

is_Char_D :: !Int -> Bool;
is_Char_D d = code inline {
	eq_desc_b CHAR 0
}

is_Real_D :: !Int -> Bool;
is_Real_D d = code inline {
	eq_desc_b REAL 0
}

is_Bool_D :: !Int -> Bool;
is_Bool_D d = code inline {
	eq_desc_b BOOL 0
}

is__String__D :: !Int -> Bool;
is__String__D d = code inline {
	eq_desc_b _STRING_ 0
}

is__Array__D :: !Int -> Bool;
is__Array__D d = code inline {
	eq_desc_b _ARRAY_ 0
}

is_Array_D :: !Int -> Bool;
is_Array_D d = code inline {
	eq_desc_b ARRAY 1
}

// The c function is_using_desc_relative_to_array is defined in copy_graph_to_string.c
// the function returns 1 if positions are relative to _ARRAY_ and 0 when not.
is_using_desc_relative_to_array :: Int;
is_using_desc_relative_to_array = code {
	ccall is_using_desc_relative_to_array ":I"
}

size_element_descriptor_currying :: Int;
size_element_descriptor_currying = code {
	ccall size_element_descriptor_currying ":I"
}

isMacOS :: Bool;
isMacOS = size_element_descriptor_currying == 16;

get_array_D :: !{#Int} -> Int;
get_array_D a = code {
	pushD_a 0
	pop_a 1
}

get_DESC_D :: !a -> Int;
get_DESC_D a = code {
	pushD_a 0
	pop_a 1	
}

get_D_name :: !Int -> {#Char};
get_D_name d = code {
	.d 0 1 i
	jsr DtoAC
	.o 1 0
}

get_D_cons_module d :== IF_INT_64_OR_32 (get_D_cons_module_64_platform d) (get_D_cons_module_32 d);

get_D_cons_module_32 :: !Int -> Int;
get_D_cons_module_32 d = code {
	push_b 0
	load_si16 0
	addI
	load_i 6
}

get_D_cons_module_64_platform :: !Int -> Int;
get_D_cons_module_64_platform d = if (not isMacOS) (get_D_cons_module_64 d) (get_D_cons_module_64_MacOS d);

get_D_cons_module_64 :: !Int -> Int;
get_D_cons_module_64 d = code {
	push_b 0
	load_si16 0
	addI
	load_si32 6
}

get_D_cons_module_64_MacOS :: !Int -> Int;
get_D_cons_module_64_MacOS d = code {
	push_b 0
	load_si16 0
	addI
	push_b 0
	load_si32 6
	addI
	pushI 6
	addI
}

get_D_cons_flag d :== IF_INT_64_OR_32 (get_D_cons_flag_64 d) (get_D_cons_flag_32 d);

get_D_cons_flag_32 :: !Int -> Int;
get_D_cons_flag_32 d = code {
	push_b 0
	load_si16 0
	addI
	load_i 2
}

get_D_cons_flag_64 :: !Int -> Int;
get_D_cons_flag_64 d = code {
	push_b 0
	load_si16 0
	addI
	load_si32 2
}

get_record_type_char :: !Int !Int -> Char;
get_record_type_char a i = code {
	addI
	load_ui8 2
}

get_D_record_module d :== IF_INT_64_OR_32 (get_D_record_module_64_platform d) (get_D_record_module_32 d);

get_D_record_module_32 :: !Int -> Int;
get_D_record_module_32 d = code {
	load_i -10
}

get_D_record_module_64_platform :: !Int -> Int;
get_D_record_module_64_platform d = if (not isMacOS) (get_D_record_module_64 d) (get_D_record_module_64_MacOS d);

get_D_record_module_64 :: !Int -> Int;
get_D_record_module_64 d = code {
	load_si32 -10
}

get_D_record_module_64_MacOS :: !Int -> Int;
get_D_record_module_64_MacOS d = code {
	push_b 0
	load_si32 -10
	addI
	pushI -10
	addI
}

get_module_name_size a :== IF_INT_64_OR_32 (get_module_name_size_64 a) (get_module_name_size_32 a);

get_module_name_size_32 :: !Int -> Int;
get_module_name_size_32 a = code {
	load_i 0
}

get_module_name_size_64 :: !Int -> Int;
get_module_name_size_64 a = code {
	load_si32 0
}

get_module_name_char :: !Int !Int -> Char;
get_module_name_char a i = code {
	addI
	load_ui8 4
}

get_module_name :: !Int -> {#Char};
get_module_name m
	= {get_module_name_char m i\\i<-[0..get_module_name_size m-1]};

:: DescOrModTree
	= DescOrModTreeNode /*descriptor or module*/!Int /*descriptor_n or module_n*/!Int !DescOrModTree !DescOrModTree
	| EmptyDescOrModTree;

search_desc_or_mod_n_in_tree :: !Int !DescOrModTree -> Int;
search_desc_or_mod_n_in_tree desc_or_mod (DescOrModTreeNode tree_desc_or_mod tree_desc_or_mod_n left_desc_tree right_desc_tree)
	| desc_or_mod==tree_desc_or_mod
		= tree_desc_or_mod_n;
	| desc_or_mod<tree_desc_or_mod
		= search_desc_or_mod_n_in_tree desc_or_mod left_desc_tree;
		= search_desc_or_mod_n_in_tree desc_or_mod right_desc_tree;
search_desc_or_mod_n_in_tree desc_or_mod EmptyDescOrModTree
	= -1;

add_desc_or_mod_to_tree :: !Int !Int !u:DescOrModTree -> u:DescOrModTree;
add_desc_or_mod_to_tree desc desc_n (DescOrModTreeNode descriptor descriptor_n left_desc_tree right_desc_tree)
	| desc==descriptor
		= abort "add_desc_or_mod_to_tree: desc already in tree";
	| desc<descriptor
		= DescOrModTreeNode descriptor descriptor_n (add_desc_or_mod_to_tree desc desc_n left_desc_tree) right_desc_tree;
		= DescOrModTreeNode descriptor descriptor_n left_desc_tree (add_desc_or_mod_to_tree desc desc_n right_desc_tree);
add_desc_or_mod_to_tree desc desc_n EmptyDescOrModTree
	= DescOrModTreeNode desc desc_n EmptyDescOrModTree EmptyDescOrModTree;

store_int_in_string :: !*{#Char} !Int !Int -> *{#Char};
store_int_in_string s i n
	= IF_INT_64_OR_32
		{s & [i]=toChar n,[i+1]=toChar (n>>8),[i+2]=toChar (n>>16),[i+3]=toChar (n>>24),
			 [i+4]=toChar (n >> 32),[i+5]=toChar (n>>40),[i+6]=toChar (n>>48),[i+7]=toChar (n>>56)}
		{s & [i]=toChar n,[i+1]=toChar (n>>8),[i+2]=toChar (n>>16),[i+3]=toChar (n>>24)};

PREFIX_N :== 0;
PREFIX_C :== 1;
PREFIX_R :== 2;
PREFIX_K :== 3;
PREFIX_D :== 4;

get_descriptor_info :: !Int -> (!{#Char},!{#Char},!Int,!Int,!Int,!Bool);
get_descriptor_info d
	# arity = get_D_node_arity d;
	| arity==0
        | is_Int_D d
			= ("_system","INT",PREFIX_D,0,1,True);
        | is_Char_D d
			= ("_system","CHAR",PREFIX_D,0,1,True);
        | is_Real_D d
			= ("_system","REAL",PREFIX_D,0,IF_INT_64_OR_32 1 2,True);
        | is_Bool_D d
			= ("_system","BOOL",PREFIX_D,0,1,True);
        | is__String__D d
			= ("_system","_STRING_",PREFIX_D,0,0,False);
        | is__Array__D d
			= ("_system","_ARRAY_",PREFIX_D,0,1,False);
        | is_Array_D d
			= ("_system","ARRAY",PREFIX_D,0,0,True);
			# desc_name = get_D_name d;
			# module_name = get_module_name (get_D_cons_module d);
			= (module_name,desc_name,PREFIX_D,0,0,True);
	| arity<256
		# desc_name = get_D_name d;
		# module_name = get_module_name (get_D_cons_module d);
		= (module_name,desc_name,PREFIX_D,arity,0,True);
		# record_a_arity = get_D_record_a_arity d;
		# record_b_arity = arity-256-record_a_arity;
		# desc_name = get_D_name d;
		# module_name = get_module_name (get_D_record_module d);
		| get_record_type_char d 0<>'d'
			= (module_name,desc_name,PREFIX_R,0,record_b_arity,True);
			= (module_name,desc_name,PREFIX_K,0,record_b_arity,True);

get_descriptor_n_non_pointers_and_not_array :: !Int -> (!Int,!Bool);
get_descriptor_n_non_pointers_and_not_array d
	# arity = get_D_node_arity d;
	| arity==0
        | is_Int_D d
			= (1,True);
        | is_Char_D d
			= (1,True);
        | is_Real_D d
			= (IF_INT_64_OR_32 1 2,True);
        | is_Bool_D d
			= (1,True);
        | is__String__D d
			= (0,False);
        | is__Array__D d
			= (1,False);
			= (0,True);
	| arity<256
		= (0,True);
		# record_a_arity = get_D_record_a_arity d;
		# record_b_arity = arity-256-record_a_arity;
		= (record_b_arity,True);

get_thunk_info :: !Int -> (!{#Char},!{#Char},!Int,!Int);
get_thunk_info d
	# arity = get_thunk_arity d;
	# desc = get_thunk_descriptor d;	
	| arity<256
		| arity>=0
			# desc_name = get_D_name (desc+2);
			# module_name = get_module_name (get_D_cons_module (desc+2));
			| get_D_cons_flag (desc+2) bitand 1==0
				= (module_name,desc_name,PREFIX_N,0);
				= (module_name,desc_name,PREFIX_C,0);
			# desc_name = get_D_name (desc+2);
			# module_name = get_module_name (get_D_cons_module (desc+2));
			= (module_name,desc_name,PREFIX_N,0);
		# desc_name = get_D_name (desc+2);
		# module_name = get_module_name (get_D_cons_module (desc+2));
		# b_size = arity>>8;
//		# a_plus_b_size = arity bitand 255;
		= (module_name,desc_name,PREFIX_N,b_size);

get_thunk_n_non_pointers:: !Int -> Int;
get_thunk_n_non_pointers d
	# arity = get_thunk_arity d;
	| arity<256
		= 0;
		# b_size = arity>>8;
		= b_size;

make_symbol_name "_system" descriptor_name prefix
	= case descriptor_name of {
		"INT"
			-> IF_INT_64_OR_32 "dINT" descriptor_name;
		"CHAR"
			-> descriptor_name;
		"REAL"
			-> descriptor_name;
		"BOOL"
			-> descriptor_name;
		"_STRING_"
			-> "__STRING__";
		"_ARRAY_"
			-> "__ARRAY__";
		"ARRAY"
			-> descriptor_name;
		"Cons"
			-> "__Cons";
		"Nil"
			-> "__Nil";
		"_Tuple"
			-> "__Tuple";
		"Cons!"
			-> prefix_char prefix+++"__Conss";
		"Cons!!"
			-> prefix_char prefix+++"__Conssts";
		"Cons?!"
			-> prefix_char prefix+++"__Consts";
		"[#Int]"
			-> "__Consi";
		"[#Int!]"
			-> "__Consits";
		"[#Char]"
			-> "__Consc";
		"[#Char!]"
			-> "__Conscts";
		"[#Real]"
			-> "__Consr";
		"[#Real!]"
			-> "__Consrts";
		"[#Bool]"
			-> "__Consb";
		"[#Bool!]"
			-> "__Consbts";
		"[#File]"
			-> "__Consf";
		"[#File!]"
			-> "__Consfts";
		"[#{}]"
			-> "__Consa";
		"AP"
			-> "e__system__"+++prefix_char prefix+++descriptor_name;
		"_ind"
			-> "e__system__"+++prefix_char prefix+++"ind";
		"_Unit"
			-> "e____predef__d__Unit";
		_
//			-> "e____system__"+++prefix_char prefix+++expand_special_characters 0 descriptor_name;
			-> prefix_char prefix+++expand_special_characters 0 descriptor_name;
	};
make_symbol_name module_name descriptor_name prefix
	= "e__"+++expand_special_characters 0 module_name+++"__"+++
		prefix_char prefix+++expand_special_characters 0 descriptor_name;

expand_special_characters i s
	| i<size s
		# c=s.[i];
		| c>='a' && c<='z'
			= expand_special_characters (i+1) s;
		| c>='A' && c<='Z'
			= expand_special_characters (i+1) s;
		| c>='0' && c<='9'
			= expand_special_characters (i+1) s;
		| c=='_'
			# s = (s % (0,i-1)) +++ "_" +++ (s % (i,size s-1));
			= expand_special_characters (i+2) s;
		# ic=toInt c;
		| ic>=32 && ic<127
			//    " !"#$%&'01*+4-./8901234567:;<=>?@567890123456789012345678901\3^5`789012345678901234567890123|5~"
			# c = " eNHdpas01MA4SPD8901234567CILEGQt567890123456789012345678901b3c5B789012345678901234567890123O5T".[ic-32];
			| c>='A' && c<='Z'
				# s = (s % (0,i-1)) +++ {'_',c} +++ (s % (i+1,size s-1));
				= expand_special_characters (i+2) s;
			| c>='a' && c<='z'
				# s = (s % (0,i-1)) +++ {'_','N',toChar (toInt c-32)} +++ (s % (i+1,size s-1));
				= expand_special_characters (i+3) s;
				= abort ("expand special characters "+++{#s.[i],' '}+++toString c);
			= abort ("expand special characters "+++{#s.[i],' '}+++toString c);
		= s;

prefix_char PREFIX_D = "d";
prefix_char PREFIX_R = "r";
prefix_char PREFIX_K = "k";
prefix_char PREFIX_N = "n";
prefix_char PREFIX_C = "c";

get_module desc
	| desc bitand 2==0
		= get_D_cons_module (get_thunk_descriptor desc+2);
	# arity = get_D_node_arity desc;
	| arity<256
		= get_D_cons_module desc;
		= get_D_record_module desc;

:: DescInfo = {di_prefix_arity_and_mod :: !Int, di_name :: !{#Char}};

info_of_desc_and_mod {desc,desc_mod_n} array_desc
	| desc bitand 2==0
		# arity = get_thunk_arity desc;
		# desc = get_thunk_descriptor desc;	
		#! desc_name = get_D_name (desc+2);
		| (arity<256 && arity>=0) && get_D_cons_flag (desc+2) bitand 1<>0
			= {di_prefix_arity_and_mod = PREFIX_C + (desc_mod_n<<8), di_name = desc_name};
			= {di_prefix_arity_and_mod = PREFIX_N + (desc_mod_n<<8), di_name = desc_name};
	# arity = get_D_node_arity desc;
	#! desc_name = get_D_name desc;
	| arity==0
		= {di_prefix_arity_and_mod = PREFIX_D + (desc_mod_n<<8), di_name = desc_name};
	| arity<256
		= {di_prefix_arity_and_mod = (PREFIX_D + arity) + (desc_mod_n<<8), di_name = desc_name};
	| get_record_type_char desc 0<>'d'
		= {di_prefix_arity_and_mod = PREFIX_R + (desc_mod_n<<8), di_name = desc_name};
		= {di_prefix_arity_and_mod = PREFIX_K + (desc_mod_n<<8), di_name = desc_name};

lookup_desc desc symbols
	| desc bitand 2==0
		# (module_name,descriptor_name,prefix,d) = get_thunk_info desc;
		# symbol_name = make_symbol_name module_name descriptor_name prefix;
		# symbol_value = get_symbol_value symbol_name symbols;
		= True;
		# (module_name,descriptor_name,prefix,arity,d,not_array) = get_descriptor_info desc;
		# symbol_name = make_symbol_name module_name descriptor_name prefix;
		# symbol_value = get_symbol_value symbol_name symbols;
		= True;

lookup_desc_array i a symbols
	| i<size a
		# desc = a.[i].desc;
		= lookup_desc desc symbols && lookup_desc_array (i+1) a symbols;
		= True;

lookup_descs :: !Int !*{#Char} !Int {#Symbol} -> (!*{#Char},!Int);
lookup_descs i s n_descs symbols
	| i>=size s
		| i==size s
			= (s,n_descs);
			= abort "error in lookup_descs";
	#! desc=get_D_from_string s i;
	| desc bitand 1<>0
		= lookup_descs (i+IF_INT_64_OR_32 8 4) s n_descs symbols;
	| desc bitand 2==0
		# (module_name,descriptor_name,prefix,d) = get_thunk_info desc;
		# symbol_name = make_symbol_name module_name descriptor_name prefix;
		# symbol_value = get_symbol_value symbol_name symbols;
		= lookup_descs (i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s n_descs symbols;
	# (module_name,descriptor_name,prefix,arity,d,not_array) = get_descriptor_info desc;
	# symbol_name = make_symbol_name module_name descriptor_name prefix;
	# symbol_value = get_symbol_value symbol_name symbols;
	| not_array
		= lookup_descs (i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s n_descs symbols;
	| d==0 // _STRING_
		#! l = get_D_from_string s (i+IF_INT_64_OR_32 8 4);
		# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4);
		= lookup_descs (i+(IF_INT_64_OR_32 16 8)+l) s n_descs symbols;
	| d==1 // _ARRAY_
		#! l = get_D_from_string s (i+IF_INT_64_OR_32 8 4);
		#! d = get_D_from_string s (i+IF_INT_64_OR_32 16 8);
		| d==0
			= lookup_descs (i+(IF_INT_64_OR_32 24 12)) s n_descs symbols;
        | is_Int_D d
			# symbol_name = "INT";
			# symbol_value = get_symbol_value symbol_name symbols;
			# l = l << IF_INT_64_OR_32 3 2;
			= lookup_descs (i+(IF_INT_64_OR_32 24 12)+l) s n_descs symbols;
        | is_Real_D d
			# symbol_name = "REAL";
			# symbol_value = get_symbol_value symbol_name symbols;
			# l = l << 3;
			= lookup_descs (i+(IF_INT_64_OR_32 24 12)+l) s n_descs symbols;
        | is_Bool_D d
			# symbol_name = "BOOL";
			# symbol_value = get_symbol_value symbol_name symbols;
			# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4);
			= lookup_descs (i+(IF_INT_64_OR_32 24 12)+l) s n_descs symbols;
		# arity = get_D_node_arity d;
		| arity>=256
			# record_a_arity = get_D_record_a_arity d;
			# record_b_arity = arity-256-record_a_arity;
			# descriptor_name = get_D_name d;
			# module_name = get_module_name (get_D_record_module d);
			# symbol_name = make_symbol_name module_name descriptor_name 1;
			# symbol_value = get_symbol_value symbol_name symbols;
			# l = (l * record_b_arity) << IF_INT_64_OR_32 3 2;
			= lookup_descs (i+(IF_INT_64_OR_32 24 12)+l) s n_descs symbols;
		= abort (toString l+++" "+++toString d);

replace_descs_by_desc_numbers_and_build_desc_tree :: !Int !*{#Char} !Int !Int !DescOrModTree
	-> (!*{#Char},!Int,!DescOrModTree);
replace_descs_by_desc_numbers_and_build_desc_tree i s n_descs array_desc desc_tree
	| i>=size s
		| i==size s
			= (s,n_descs,desc_tree);
			= abort "error in replace_descs_by_desc_numbers_and_build_desc_tree";
	#! desc=get_D_from_string s i;
	#! desc=desc+array_desc;
	| desc bitand 1<>0
		= replace_descs_by_desc_numbers_and_build_desc_tree (i+IF_INT_64_OR_32 8 4) s n_descs array_desc desc_tree;
	# (s,n_descs,desc_tree) = store_desc_n_and_add_desc desc i s n_descs desc_tree;
	| desc bitand 2==0
		# d = get_thunk_n_non_pointers desc;
		= replace_descs_by_desc_numbers_and_build_desc_tree
			(i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s n_descs array_desc desc_tree;
	# (d,not_array) = get_descriptor_n_non_pointers_and_not_array desc;
	| not_array
		= replace_descs_by_desc_numbers_and_build_desc_tree
			(i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s n_descs array_desc desc_tree;
	| d==0 // _STRING_
		#! l = get_D_from_string s (i+IF_INT_64_OR_32 8 4);
		# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4);
		= replace_descs_by_desc_numbers_and_build_desc_tree (i+(IF_INT_64_OR_32 16 8)+l) s n_descs array_desc desc_tree;
	| d==1 // _ARRAY_
		#! d = get_D_from_string s (i+IF_INT_64_OR_32 16 8);
		| d==0
			= replace_descs_by_desc_numbers_and_build_desc_tree (i+(IF_INT_64_OR_32 24 12)) s n_descs array_desc desc_tree;
		# (s,n_descs,desc_tree) = store_desc_n_and_add_desc d (i+IF_INT_64_OR_32 16 8) s n_descs desc_tree;
		#! l = get_D_from_string s (i+IF_INT_64_OR_32 8 4);
        | is_Int_D d
			# l = l << IF_INT_64_OR_32 3 2;
			= replace_descs_by_desc_numbers_and_build_desc_tree (i+(IF_INT_64_OR_32 24 12)+l) s n_descs array_desc desc_tree;
        | is_Real_D d
			# l = l << 3;
			= replace_descs_by_desc_numbers_and_build_desc_tree (i+(IF_INT_64_OR_32 24 12)+l) s n_descs array_desc desc_tree;
        | is_Bool_D d
			# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4);
			= replace_descs_by_desc_numbers_and_build_desc_tree (i+(IF_INT_64_OR_32 24 12)+l) s n_descs array_desc desc_tree;
		# arity = get_D_node_arity d;
		| arity>=256
			# record_a_arity = get_D_record_a_arity d;
			# record_b_arity = arity-256-record_a_arity;
			# l = (l * record_b_arity) << IF_INT_64_OR_32 3 2;
			= replace_descs_by_desc_numbers_and_build_desc_tree (i+(IF_INT_64_OR_32 24 12)+l) s n_descs array_desc desc_tree;
		= abort (toString l+++" "+++toString d);

store_desc_n_and_add_desc :: Int Int !*{#Char} !Int !DescOrModTree -> (!*{#Char},!Int,!DescOrModTree);
store_desc_n_and_add_desc desc i s n_descs desc_tree
	# desc_n=search_desc_or_mod_n_in_tree desc desc_tree;
	| desc_n>=0
		# s=store_int_in_string s i (desc_n+1);  // add 1 because 0 is used as element descriptor for lazy/boxed arrays
		= (s,n_descs,desc_tree);
		# desc_tree = add_desc_or_mod_to_tree desc n_descs desc_tree;
		# s=store_int_in_string s i (n_descs+1); // add 1 because 0 is used as element descriptor for lazy/boxed arrays
		= (s,n_descs+1,desc_tree);

:: Desc_ModuleN = {desc::!Int,desc_mod_n::!Int};

make_desc_array :: !Int !DescOrModTree -> *{#Desc_ModuleN};
make_desc_array n_descs desc_tree
    = fill_desc_array desc_tree (createArray n_descs {desc=0,desc_mod_n=0});
{
	fill_desc_array :: !DescOrModTree !*{#Desc_ModuleN} -> *{#Desc_ModuleN};
	fill_desc_array (DescOrModTreeNode descriptor descriptor_n left_desc_tree right_desc_tree) a
		= fill_desc_array right_desc_tree (fill_desc_array left_desc_tree {a & [descriptor_n].desc=descriptor});
    fill_desc_array EmptyDescOrModTree a
		= a;
}

make_module_tree :: !*{#Desc_ModuleN} -> (!*{#Desc_ModuleN},!Int,!DescOrModTree);
make_module_tree a
	= add_modules 0 a 0 EmptyDescOrModTree;
{
	add_modules i a n_mods mod_tree
 		| i<size a
			# (desc,a)=a![i].desc;
			# mod=get_module desc;
//			| mod==0
//				= add_modules (i+1) a n_mods mod_tree;
			# mod_n=search_desc_or_mod_n_in_tree mod mod_tree;
			| mod_n>=0
				# a = {a & [i].desc_mod_n=mod_n+1};
				= add_modules (i+1) a n_mods mod_tree;
				# mod_tree = add_desc_or_mod_to_tree mod n_mods mod_tree;
				# a = {a & [i].desc_mod_n=n_mods+1};
				= add_modules (i+1) a (n_mods+1) mod_tree;
			= (a,n_mods,mod_tree);
}

make_mod_array :: !Int !DescOrModTree -> *{#Int};
make_mod_array n_mods mod_tree
    = fill_desc_array mod_tree (createArray n_mods 0);
{
	fill_desc_array :: !DescOrModTree !*{#Int} -> *{#Int};
	fill_desc_array (DescOrModTreeNode descriptor descriptor_n left_mod_tree right_mod_tree) a
		= fill_desc_array right_mod_tree (fill_desc_array left_mod_tree {a & [descriptor_n]=descriptor});
    fill_desc_array EmptyDescOrModTree a
		= a;
}

copy_to_string_with_names :: a -> (!*{#Char},!*{#DescInfo},!*{#String});
copy_to_string_with_names g
	# array_desc = if (is_using_desc_relative_to_array == 1) (get_array_D {} - 2) 0;
	# s = copy_to_string g;
	# (s,n_descs,desc_tree) = replace_descs_by_desc_numbers_and_build_desc_tree 0 s 0 array_desc EmptyDescOrModTree;
	# desc_a = make_desc_array n_descs desc_tree;
	# (desc_a,n_mods,mod_tree) = make_module_tree desc_a;
	# mod_a = make_mod_array n_mods mod_tree;
	# mod_s_a = {#get_module_name mod \\ mod<-:mod_a};
	# desc_s_a = {#info_of_desc_and_mod desc_and_mod array_desc \\ desc_and_mod <-:desc_a};
	= (s,desc_s_a,mod_s_a);

lookup_symbol_value {di_prefix_arity_and_mod,di_name} mod_a symbols
	# prefix_n = di_prefix_arity_and_mod bitand 0xff;
	# module_n = (di_prefix_arity_and_mod >> 8)-1;
	# module_name = mod_a.[module_n];
	| prefix_n<PREFIX_D
		# symbol_name = make_symbol_name module_name di_name prefix_n;
		# symbol_value = get_symbol_value symbol_name symbols;
		| prefix_n<=1
			| symbol_value== -1
				= abort ("lookup_desc_info not found "+++symbol_name); 
				= symbol_value;
			| symbol_value== -1
				= abort ("lookup_desc_info not found "+++symbol_name); 
				= symbol_value+2;
		# symbol_name = make_symbol_name module_name di_name PREFIX_D;
		# symbol_value = get_symbol_value symbol_name symbols;
		| symbol_value== -1
			= abort ("lookup_desc_info not found "+++symbol_name); 
			# arity = prefix_n - PREFIX_D;
			= symbol_value+(arity*size_element_descriptor_currying)+2;

lookup_symbol_values desc_info_a mod_a symbols
	= {#lookup_symbol_value desc_info mod_a symbols \\ desc_info <-: desc_info_a};

replace_desc_numbers_by_descs :: !Int !*{#Char} !{#Int} !Int !Int -> *{#Char};
replace_desc_numbers_by_descs i s symbol_a symbol_offset array_desc
	| i>=size s
		| i==size s
			= s;
			= abort ("error in replace_desc_numbers_by_descs "+++toString i);
	#! desc=get_D_from_string s i;
	| desc<0
		= replace_desc_numbers_by_descs (i+IF_INT_64_OR_32 8 4) s symbol_a symbol_offset array_desc;
	# desc = symbol_a.[desc-1];
	# desc=desc+symbol_offset;
	# s=store_int_in_string s i (desc - array_desc);
	| desc bitand 2==0
		# d = get_thunk_n_non_pointers desc;
		= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s symbol_a symbol_offset array_desc;
	# (d,not_array) = get_descriptor_n_non_pointers_and_not_array desc;
	| not_array
		= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s symbol_a symbol_offset array_desc;
	| d==0 // _STRING_
		#! l = get_D_from_string s (i+IF_INT_64_OR_32 8 4);
		# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4);
		= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 16 8)+l) s symbol_a symbol_offset array_desc;
	| d==1 // _ARRAY_
		#! d = get_D_from_string s (i+IF_INT_64_OR_32 16 8);
		| d==0
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)) s symbol_a symbol_offset array_desc;
		# d = symbol_a.[d-1];
		# d = d+symbol_offset;
		# s=store_int_in_string s (i+IF_INT_64_OR_32 16 8) array_desc;
		#! l = get_D_from_string s (i+IF_INT_64_OR_32 8 4);
        | is_Int_D d
			# l = l << IF_INT_64_OR_32 3 2;
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc;
        | is_Real_D d
			# l = l << 3;
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc;
        | is_Bool_D d
			# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4);
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc;
		# arity = get_D_node_arity d;
		| arity>=256
			# record_a_arity = get_D_record_a_arity d;
			# record_b_arity = arity-256-record_a_arity;
			# l = (l * record_b_arity) << IF_INT_64_OR_32 3 2;
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc;
		= abort (toString l+++" "+++toString d);

copy_from_string_with_names :: !*{#Char} !*{#DescInfo} !*{#String} !{#Symbol} -> (.a,!Int);
copy_from_string_with_names s desc_s_a mod_s_a symbols
	# symbol_offset = if (is_using_desc_relative_to_array == 1) (((get_array_D {}) - (get_symbol_value "__ARRAY__" symbols)) - 2) 0;
	# array_desc = if (is_using_desc_relative_to_array == 1) ((get_array_D {} - 2)) 0;
	# symbol_a = lookup_symbol_values desc_s_a mod_s_a symbols;
	# s = replace_desc_numbers_by_descs 0 s symbol_a symbol_offset array_desc;
	= copy_from_string s;


