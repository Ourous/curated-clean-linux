implementation module Gast.ThunkNames

import StdEnv
import System.OS

// ================================================================
// :( :( dirty low level hacking to obtain names of functions :( :(

thunk_name_to_string :: !a -> String
thunk_name_to_string a = code {
    pushD_a 0
    pop_a 1
    .d 0 1 i
        jsr DtoAC
    .o 1 0
}

thunk_to_module_name_pointer :: (a -> Int)
thunk_to_module_name_pointer = IF_MAC v64mac (IF_INT_64_OR_32 v64 v32)
where
	v32 :: !a -> Int
	v32 _ = code {
		pushD_a 0
		pop_a 1
		push_b 0
		load_si16 0
		addI
		load_i 6
	}

	v64 :: !a -> Int
	v64 _ = code {
		pushD_a 0
		pop_a 1
		push_b 0
		load_si16 0
		addI
		load_si32 6
	}

	v64mac :: !a -> Int
	v64mac _ = code {
		pushD_a 0
		pop_a 1
		push_b 0
		load_si16 0
		addI
		push_b 0
		load_si32 6
		addI
		pushI 6
		addI
	}

thunk_to_module_name_string :: !a -> String;
thunk_to_module_name_string a
    = get_module_name (thunk_to_module_name_pointer a);

get_module_name :: !Int -> {#Char};
get_module_name m
    = {get_module_name_char m i\\i<-[0..get_module_name_size m-1]};

get_module_name_size :: (Int -> Int)
get_module_name_size = IF_INT_64_OR_32 v64 v32
where
	v32 :: !Int -> Int
	v32 _ = code {
		load_i 0
	}

	v64 :: !Int -> Int
	v64 _ = code {
		load_si32 0
	}

get_module_name_char :: !Int !Int -> Char;
get_module_name_char a i = code {
    addI
    load_ui8 4
}
