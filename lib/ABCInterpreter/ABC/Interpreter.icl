implementation module ABC.Interpreter

import StdArray
import StdBool
import StdClass
import StdFile
import StdInt
import StdList
import StdMaybe
import StdMisc
import StdOrdList

import graph_copy_with_names
import symbols_in_program

import ABC.Interpreter.Internal
import ABC.Interpreter.Util

defaultDeserializationSettings :: DeserializationSettings
defaultDeserializationSettings =
	{ heap_size  = 2 << 20
	, stack_size = (512 << 10) * 2
	}

:: *SerializedGraph =
	{ graph    :: !*String
	, descinfo :: !{#DescInfo}
	, modules  :: !{#String}
	, bytecode :: !String
	}

// The arguments are:
// - Pointer to C function;
// - Argument for function (in our case, pointer to the interpret node)
// - Pointer to rest of the finalizers (dealt with in the RTS)
:: Finalizer = Finalizer !Int !Int !Int
:: InterpretedExpression :== Finalizer
:: *InterpretationEnvironment = E.a:
	{ ie_finalizer :: !Finalizer
	, ie_snode_ptr :: !Int
	, ie_snodes    :: !*{a}
	}

serialize :: a !String !*World -> *(!Maybe SerializedGraph, !*World)
serialize graph bcfile w
# (graph,descinfo,modules) = copy_to_string_with_names graph

# (bytecode,w) = readFile bcfile w
| isNothing bytecode = (Nothing, w)
# bytecode = fromJust bytecode

#! (len,bytecodep) = strip_bytecode False bytecode {#symbol_name di modules \\ di <-: descinfo}
#! bytecode = derefCharArray bytecodep len
| free_to_false bytecodep = (Nothing, w)

# rec =
	{ graph    = graph
	, descinfo = descinfo
	, modules  = modules
	, bytecode = bytecode
	}
= (Just rec, w)
where
	symbol_name :: !DescInfo !{#String} -> String
	symbol_name {di_prefix_arity_and_mod,di_name} mod_a
	# prefix_n = di_prefix_arity_and_mod bitand 0xff
	# module_n = (di_prefix_arity_and_mod >> 8)-1
	# module_name = mod_a.[module_n]
	= make_symbol_name module_name di_name (min prefix_n PREFIX_D) +++ "\0"
	where
		PREFIX_D = 4

	strip_bytecode :: !Bool !String !{#String} -> (!Int, !Pointer)
	strip_bytecode include_symbol_table bytecode descriptors = code {
		ccall strip_bytecode "IsA:VIp"
	}

deserialize :: !DeserializationSettings !SerializedGraph !String !*World -> *(!Maybe a, !*World)
deserialize dsets graph thisexe w = deserialize` False dsets graph thisexe w

deserialize_strict :: !DeserializationSettings !SerializedGraph !String !*World -> *(!DeserializedValue a, !*World)
deserialize_strict dsets graph thisexe w = case deserialize` True dsets graph thisexe w of
	(Nothing,w) -> (DV_ParseError,w)
	(Just v,w)  -> (v,w)

deserialize` :: !Bool !DeserializationSettings !SerializedGraph !String !*World -> *(Maybe a, !*World)
deserialize` strict dsets {graph,descinfo,modules,bytecode} thisexe w
| not ensure_interpreter_init = abort "internal error in deserialize`\n"

# (host_syms,w) = accFiles (read_symbols thisexe) w

# pgm = parse host_syms bytecode
| isNothing pgm = (Nothing, w)
# pgm = fromJust pgm
# int_syms = {#s \\ s <- getInterpreterSymbols pgm}
# int_syms = {#lookup_symbol_value d modules int_syms \\ d <-: descinfo}

# stack = malloc (IF_INT_64_OR_32 8 4 * dsets.stack_size)
# asp = stack
# bsp = stack + IF_INT_64_OR_32 8 4 * (dsets.stack_size-1)
# csp = stack + IF_INT_64_OR_32 4 2 * dsets.stack_size
# heap = malloc (IF_INT_64_OR_32 8 4 * (dsets.heap_size+4))
# ie_settings = build_interpretation_environment
	pgm
	heap dsets.heap_size stack dsets.stack_size
	asp bsp csp heap
	strict
# graph = replace_desc_numbers_by_descs 0 graph int_syms 0 0
# graph_node = string_to_interpreter graph ie_settings
#! (ie,_) = make_finalizer ie_settings
# ie = {ie_finalizer=ie, ie_snode_ptr=0, ie_snodes=create_array_ 1}
= (Just (interpret ie (Finalizer 0 0 (graph_node + if strict 2 0))), w)
where
	string_to_interpreter :: !String !Pointer -> Pointer
	string_to_interpreter graph ie = code {
		ccall string_to_interpreter "Sp:p"
	}

getInterpreterSymbols :: !Pointer -> [Symbol]
getInterpreterSymbols pgm = takeWhile (\s -> size s.symbol_name <> 0)
	[getSymbol i \\ i <- [0..get_symbol_table_size pgm-1]]
where
	symbol_table = get_symbol_table pgm

	getSymbol :: !Int -> Symbol
	getSymbol i
	#! offset = symbol_table + i * IF_INT_64_OR_32 16 8 /* size of struct host_symbol */
	#! loc = derefInt offset
	#! name = derefString (derefInt (offset + IF_INT_64_OR_32 8 4))
	= {symbol_name=name, symbol_value=loc}

	get_symbol_table_size :: !Pointer -> Int
	get_symbol_table_size pgm = code {
		ccall get_symbol_table_size "p:I"
	}

	get_symbol_table :: !Pointer -> Pointer
	get_symbol_table pgm = code {
		ccall get_symbol_table "p:p"
	}

lookup_symbol_value :: !DescInfo !{#String} !{#Symbol} -> Int
lookup_symbol_value {di_prefix_arity_and_mod,di_name} mod_a symbols
	# prefix_n = di_prefix_arity_and_mod bitand 0xff
	# module_n = (di_prefix_arity_and_mod >> 8)-1
	# module_name = mod_a.[module_n]
	| prefix_n<PREFIX_D
		# symbol_name = make_symbol_name module_name di_name prefix_n
		# symbol_value = get_symbol_value symbol_name symbols
		| prefix_n<=1
			| symbol_value== -1
				= abort ("lookup_desc_info not found "+++symbol_name+++"\n")
				= symbol_value
			| symbol_value== -1
				= abort ("lookup_desc_info not found "+++symbol_name+++"\n")
				= symbol_value+2
		# symbol_name = make_symbol_name module_name di_name PREFIX_D
		# symbol_value = get_symbol_value symbol_name symbols
		| symbol_value== -1
			= abort ("lookup_desc_info not found "+++symbol_name+++"\n")
			# arity = prefix_n - PREFIX_D
			= symbol_value+(arity*8*2)+2
where
	PREFIX_D = 4

get_start_rule_as_expression :: !DeserializationSettings !String !String !*World -> *(Maybe a, !*World)
get_start_rule_as_expression dsets filename prog w
| not ensure_interpreter_init = abort "internal error in get_start_rule_as_expression\n"
# (syms,w) = accFiles (read_symbols prog) w
# (bc,w) = readFile filename w
| isNothing bc = (Nothing, w)
# bc = fromJust bc
# pgm = parse syms bc
| isNothing pgm = (Nothing, w)
# pgm = fromJust pgm
# stack = malloc (IF_INT_64_OR_32 8 4 * dsets.stack_size)
# asp = stack
# bsp = stack + IF_INT_64_OR_32 8 4 * (dsets.stack_size-1)
# csp = stack + IF_INT_64_OR_32 4 2 * dsets.stack_size
# heap = malloc (IF_INT_64_OR_32 8 4 * dsets.heap_size)
# ie_settings = build_interpretation_environment
	pgm
	heap dsets.heap_size stack dsets.stack_size
	asp bsp csp heap
	False
# start_node = build_start_node ie_settings
#! (ie,_) = make_finalizer ie_settings
# ie = {ie_finalizer=ie, ie_snode_ptr=0, ie_snodes=create_array_ 1}
= (Just (interpret ie (Finalizer 0 0 start_node)), w)
	// Obviously, this is not a "valid" finalizer in the sense that it can be
	// called from the garbage collector. But that's okay, because we don't add
	// it to the finalizer_list anyway. This is just to ensure that the first
	// call to interpret gets the right argument.

build_interpretation_environment :: !Pointer !Pointer !Int !Pointer !Int !Pointer !Pointer !Pointer !Pointer !Bool -> Pointer
build_interpretation_environment pgm heap hsize stack ssize asp bsp csp hp strict = code {
	ccall build_interpretation_environment "ppIpIppppI:p"
}

build_start_node :: !Pointer -> Pointer
build_start_node ie = code {
	ccall build_start_node "p:p"
}

make_finalizer :: !Int -> (!.Finalizer,!Int)
make_finalizer ie_settings = code {
	push_finalizers
	ccall get_interpretation_environment_finalizer ":p"
	push_a_b 0
	pop_a 1
	build_r e__system_kFinalizer 0 3 0 0
	pop_b 3
	set_finalizers
	pushI 0
}

graph_to_string :: !*SerializedGraph -> *(!.String, !*SerializedGraph)
graph_to_string g=:{graph,descinfo,modules,bytecode}
# (graph_cpy,graph,graph_size) = copy graph
# g & graph = graph
# string_size = sum
	[ 4 + graph_size
	, 4 + sum [4 + size di.di_name + 1 \\ di <-: descinfo]
	, 4 + sum [size mod + 1 \\ mod <-: modules]
	, 4 + size bytecode
	]
# s = createArray string_size '\0'
# (i,s) = writeInt graph_size 0 s
# (i,s) = writeString {#c \\ c <- graph_cpy} i s
# (i,s) = writeInt (size descinfo) i s
# (i,s) = writeArray writeDescInfo 0 descinfo i s
# (i,s) = writeInt (size modules) i s
# (i,s) = writeArray writeTerminatedString 0 modules i s
# (i,s) = writeInt (size bytecode) i s
# (i,s) = writeString bytecode i s
| i <> string_size = abort "internal error in graphToString\n"
= (s,g)
where
	copy :: !*(b a) -> *(![a], !*b a, !Int) | Array b a
	copy arr
	# (s,arr) = usize arr
	# (cpy,arr) = copy (s-1) arr []
	= (cpy,arr,s)
	where
		copy :: !Int !*(b a) ![a] -> *(![a], !*b a) | Array b a
		copy -1 arr cpy = (cpy, arr)
		copy i  arr cpy
		# (x,arr) = arr![i]
		= copy (i-1) arr [x:cpy]

	writeInt :: !Int !Int !*String -> (!Int, !.String)
	writeInt n i s = (i+4, {s & [i]=toChar n, [i+1]=toChar (n>>8), [i+2]=toChar (n>>16), [i+3]=toChar (n>>24)})

	writeString :: !String !Int !*String -> (!Int, !.String)
	writeString src i s = (i+size src, {s & [j]=c \\ j <- [i..] & c <-: src})

	writeTerminatedString :: !String !Int !*String -> (!Int, !.String)
	writeTerminatedString src i s
	# (i,s) = writeString src i s
	= (i+1, {s & [i]='\0'})

	writeDescInfo :: !DescInfo !Int !*String -> (!Int, !.String)
	writeDescInfo di i s
	# (i,s) = writeInt di.di_prefix_arity_and_mod i s
	= writeTerminatedString di.di_name i s

	writeArray :: !(a Int *String -> (Int, *String)) !Int !{#a} !Int !*String -> (!Int, !.String) | Array {#} a
	writeArray write n arr i s
	| n >= size arr = (i,s)
	# (i,s) = write arr.[n] i s
	= writeArray write (n+1) arr i s

graph_from_string :: !String -> Maybe *SerializedGraph
graph_from_string s = read 0 s
where
	read :: !Int !String -> Maybe *SerializedGraph
	read i s
	# (i,graph_size) = readInt i s
	| i < 0 = Nothing
	# (i,graph) = readString i graph_size s
	| i < 0 = Nothing
	# graph = {c \\ c <-: graph}

	# (i,descinfo_size) = readInt i s
	| i < 0 = Nothing
	# (i,descinfo) = readArray readDescInfo descinfo_size i s
	| i < 0 = Nothing
	# descinfo = {di \\ di <- descinfo}

	# (i,modules_size) = readInt i s
	| i < 0 = Nothing
	# (i,modules) = readArray readTerminatedString modules_size i s
	| i < 0 = Nothing
	# modules = {mod \\ mod <- modules}

	# (i,bytecode_size) = readInt i s
	| i < 0 = Nothing
	# (i,bytecode) = readString i bytecode_size s
	| i < 0 = Nothing

	| i <> size s = Nothing
	= Just {graph=graph,descinfo=descinfo,modules=modules,bytecode=bytecode}

	readInt :: !Int !String -> (!Int, !Int)
	readInt i s
	| i >= size s-4 = (-1,0)
	| otherwise = (i+4, sum [toInt s.[i+j] << (8*j) \\ j <- [0,1,2,3]])

	readString :: !Int !Int !String -> (!Int, !String)
	readString i len s
	| i > size s - len = (-1,"")
	| otherwise = (i+len, s % (i,i+len-1))

	readTerminatedString :: !Int !String -> (!Int, !String)
	readTerminatedString i s
	# len = findNull i s - i
	| len < 0 = (-1, "")
	# (i,s`) = readString i len s
	| i < 0 = (i,s`)
	= (i+1,s`)
	where
		findNull :: !Int !String -> Int
		findNull i s
		| i >= size s   = -1
		| s.[i] == '\0' = i
		| otherwise     = findNull (i+1) s

	readDescInfo :: !Int !String -> (!Int, !DescInfo)
	readDescInfo i s
	| i >= size s = (-1, {di_prefix_arity_and_mod=0, di_name=""})
	# (i,arity) = readInt i s
	| i < 0 = (-1, {di_prefix_arity_and_mod=0, di_name=""})
	# (i,name) = readTerminatedString i s
	| i < 0 = (-1, {di_prefix_arity_and_mod=0, di_name=""})
	= (i, {di_prefix_arity_and_mod=arity, di_name=name})

	readArray :: !(Int String -> (Int, a)) !Int !Int !String -> (!Int, ![a])
	readArray read len i s
	| len <= 0 = (i,[])
	# (i,x) = read i s
	| i < 0 = (i,[])
	# (i,xs) = readArray read (len-1) i s
	= (i,[x:xs])

graph_to_file :: !*SerializedGraph !*File -> *(!*SerializedGraph, !*File)
graph_to_file g f
# (s,g) = graph_to_string g
# f = f <<< size s <<< s
= (g,f)

graph_from_file :: !*File -> *(!Maybe *SerializedGraph, !*File)
graph_from_file f
# (_,size,f) = freadi f
# (s,f) = freads f size
# g = graph_from_string s
= (g,f)

malloc :: !Int -> Pointer
malloc _ = code {
	ccall malloc "I:p"
}

readFile :: !String !*World -> (!Maybe String, !*World)
readFile fname w
# (ok,f,w) = fopen fname FReadData w
| not ok = (Nothing, w)
# (ok,f) = fseek f 0 FSeekEnd
| not ok
	# (_,w) = fclose f w
	= (Nothing, w)
# (size,f) = fposition f
# (ok,f) = fseek f 0 FSeekSet
| not ok
	# (_,w) = fclose f w
	= (Nothing, w)
# (s,f) = freads f size
# (_,w) = fclose f w
= (Just s,w)

:: Pointer :== Int

derefInt :: !Pointer -> Int
derefInt ptr = code {
	load_i 0
}

derefChar :: !Pointer -> Char
derefChar ptr = code inline {
	load_ui8 0
}

derefCharArray :: !Pointer !Int -> {#Char}
derefCharArray ptr len = copy 0 (createArray len '\0')
where
	copy :: !Int *{#Char} -> *{#Char}
	copy i arr
	| i == len = arr
	# c = derefChar (ptr+i)
	= copy (i+1) {arr & [i]=c}

derefString :: !Pointer -> String
derefString ptr
# len = findNull ptr - ptr
= derefCharArray ptr len
where
	findNull :: !Pointer -> Pointer
	findNull ptr = case derefChar ptr of
		'\0' -> ptr
		_    -> findNull (ptr+1)

:: PrelinkedInterpretationEnvironment =
	{ pie_symbols    :: !{#Symbol}
	, pie_code_start :: !Int
	}

prepare_prelinked_interpretation :: !String !*World -> *(!Maybe PrelinkedInterpretationEnvironment, !*World)
prepare_prelinked_interpretation bcfile w
# (bytecode,w) = readFile bcfile w
| isNothing bytecode = (Nothing, w)
# bytecode = fromJust bytecode

# pgm = parse {} bytecode // No matching with the host is required
| isNothing pgm = (Nothing, w)
# pgm = fromJust pgm
# code_start = get_code pgm

# int_syms = {#s \\ s <- getInterpreterSymbols pgm}
= (Just {pie_symbols=int_syms,pie_code_start=code_start}, w)
where
	get_code :: !Int -> Int
	get_code pgm = code {
		ccall get_code "p:p"
	}

serialize_for_prelinked_interpretation :: a !PrelinkedInterpretationEnvironment -> String
serialize_for_prelinked_interpretation graph pie
# (graph,descinfo,modules) = copy_to_string_with_names graph
# syms = {#predef_or_lookup_symbol pie.pie_code_start d modules pie.pie_symbols \\ d <-: descinfo}
= replace_desc_numbers_by_descs 0 graph syms 0 pie.pie_code_start
where
	predef_or_lookup_symbol :: !Int !DescInfo !{#String} !{#Symbol} -> Int
	predef_or_lookup_symbol code_start di mods syms = case di.di_name of
		"_ARRAY_"  -> code_start-1*8+2
		"_STRING_" -> code_start-2*8+2
		"BOOL"     -> code_start-3*8+2
		"CHAR"     -> code_start-4*8+2
		"REAL"     -> code_start-5*8+2
		"INT"      -> code_start-6*8+2
		"dINT"     -> code_start-6*8+2
		_          -> lookup_symbol_value di mods syms

	// This is like the function with the same name in GraphCopy's
	// graph_copy_with_names, but it assigns even negative descriptor numbers
	// to predefined symbols so that it matches predef_or_lookup_symbol above.
	replace_desc_numbers_by_descs :: !Int !*{#Char} !{#Int} !Int !Int -> *{#Char}
	replace_desc_numbers_by_descs i s symbol_a symbol_offset array_desc
	| i>=size s
		| i==size s = s
		| otherwise = abort "error in replace_desc_numbers_by_descs\n"
	#! desc=get_word_from_string s i
	| desc<0
		= replace_desc_numbers_by_descs (i+IF_INT_64_OR_32 8 4) s symbol_a symbol_offset array_desc
	# desc = symbol_a.[desc-1]
	# desc=desc+symbol_offset
	# s=store_int_in_string s i (desc-array_desc)
	| desc bitand 2==0
		# d = get_thunk_n_non_pointers desc
		= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s symbol_a symbol_offset array_desc
	# (d,not_array) = get_descriptor_n_non_pointers_and_not_array desc
	| not_array
		= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 8 4)+(d<<(IF_INT_64_OR_32 3 2))) s symbol_a symbol_offset array_desc
	| d==0 // _STRING_
		#! l = get_word_from_string s (i+IF_INT_64_OR_32 8 4)
		# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4)
		= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 16 8)+l) s symbol_a symbol_offset array_desc
	| d==1 // _ARRAY_
		#! d = get_word_from_string s (i+IF_INT_64_OR_32 16 8)
		| d==0
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)) s symbol_a symbol_offset array_desc
		# d = symbol_a.[d-1]
		# d = d+symbol_offset
		# s=store_int_in_string s (i+IF_INT_64_OR_32 16 8) (d-array_desc)
		#! l = get_word_from_string s (i+IF_INT_64_OR_32 8 4)
		| d==array_desc-5*8+2 // REAL
			# l = l << IF_INT_64_OR_32 3 2
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc
		| d==array_desc-6*8+2 // INT
			# l = l << 3
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc
		| d==array_desc-3*8+2 // BOOL
			# l = IF_INT_64_OR_32 ((l+7) bitand -8) ((l+3) bitand -4)
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc
		# arity = get_D_node_arity d
		| arity>=256
			# record_a_arity = get_D_record_a_arity d
			# record_b_arity = arity-256-record_a_arity
			# l = (l * record_b_arity) << IF_INT_64_OR_32 3 2
			= replace_desc_numbers_by_descs (i+(IF_INT_64_OR_32 24 12)+l) s symbol_a symbol_offset array_desc
		= abort (toString l+++" "+++toString d)
	where
		get_word_from_string :: !{#Char} !Int -> Int // get_D_from_string_64
		get_word_from_string s i = code inline {
			push_a_b 0
			pop_a 1
			addI
			load_i 16
		}

		store_int_in_string :: !*{#Char} !Int !Int -> *{#Char} // 64-bit variant
		store_int_in_string s i n =
			{s & [i]=toChar n,[i+1]=toChar (n>>8),[i+2]=toChar (n>>16),[i+3]=toChar (n>>24),
				 [i+4]=toChar (n >> 32),[i+5]=toChar (n>>40),[i+6]=toChar (n>>48),[i+7]=toChar (n>>56)}

		get_thunk_n_non_pointers:: !Int -> Int
		get_thunk_n_non_pointers d
		# arity = get_thunk_arity d
		| arity<256
			= 0
			# b_size = arity>>8
			= b_size
		where
			get_thunk_arity :: !Int -> Int // 64-bit version
			get_thunk_arity a = code {
				load_si32 -4
			}

		get_descriptor_n_non_pointers_and_not_array :: !Int -> (!Int,!Bool)
		get_descriptor_n_non_pointers_and_not_array d
		| d<array_desc
			| d==array_desc-1*8+2 = (1,False) // _ARRAY_
			| d==array_desc-2*8+2 = (0,False) // _STRING_
			| d==array_desc-3*8+2 = (1,True)  // BOOL
			| d==array_desc-4*8+2 = (1,True)  // CHAR
			| d==array_desc-5*8+2 = (IF_INT_64_OR_32 1 2,True) // REAL
			| d==array_desc-6*8+2 = (1,True)  // INT/dINT
			| otherwise = abort "internal error in serialize_for_prelinked_interpretation\n"
		# arity = get_D_node_arity d
		| arity<256 = (0,True)
		# record_a_arity = get_D_record_a_arity d
		# record_b_arity = arity-256-record_a_arity
		= (record_b_arity,True)

		get_D_node_arity :: !Int -> Int
		get_D_node_arity d = code inline {
			load_si16 -2
		}

		get_D_record_a_arity :: !Int -> Int
		get_D_record_a_arity d = code inline {
			load_si16 0
		}
