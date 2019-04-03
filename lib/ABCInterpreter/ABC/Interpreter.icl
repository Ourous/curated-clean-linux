implementation module ABC.Interpreter

import StdArray
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
# (graph,descs,mods) = copy_to_string_with_names graph

# (bytecode,w) = readFile bcfile w
| isNothing bytecode = (Nothing, w)
# bytecode = fromJust bytecode

#! (len,bytecodep) = strip_bytecode bytecode {#symbol_name di mods \\ di <-: descs}
#! bytecode = derefCharArray bytecodep len
| free_to_false bytecodep = (Nothing, w)

# rec =
	{ graph    = graph
	, descinfo = descs
	, modules  = mods
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

	strip_bytecode :: !String !{#String} -> (!Int, !Pointer)
	strip_bytecode bytecode descriptors = code {
		ccall strip_bytecode "sA:VIp"
	}

deserialize :: !DeserializationSettings !SerializedGraph !String !*World -> *(!Maybe a, !*World)
deserialize dsets graph thisexe w = deserialize` False dsets graph thisexe w

deserialize_strict :: !DeserializationSettings !SerializedGraph !String !*World -> *(!DeserializedValue a, !*World)
deserialize_strict dsets graph thisexe w = case deserialize` True dsets graph thisexe w of
	(Nothing,w) -> (DV_ParseError,w)
	(Just v,w)  -> (v,w)

deserialize` :: !Bool !DeserializationSettings !SerializedGraph !String !*World -> *(Maybe a, !*World)
deserialize` strict dsets {graph,descinfo,modules,bytecode} thisexe w
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
