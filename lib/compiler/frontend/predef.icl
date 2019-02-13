implementation module predef

import syntax, hashtable

(<<=) infixl
(<<=) symbol_table val
	:==	let (predefined_idents, index) = val
	  	in writePtr predefined_idents.[index].id_info EmptySymbolTableEntry symbol_table

(<<-) infixl
(<<-) hash_table val
	:== let (predefined_idents, table_kind, index) = val
		in putPredefinedIdentInHashTable predefined_idents.[index] table_kind hash_table

GetTupleConsIndex tup_arity :== PD_Arity2TupleSymbol + tup_arity - 2
GetTupleTypeIndex tup_arity :== PD_Arity2TupleType + tup_arity - 2

predefined_idents :: {!Ident}
predefined_idents
	# idents = createArray PD_NrOfPredefSymbols {id_name="",id_info=nilPtr}
	# idents = { idents &
					[PD_ConsSymbol] = i PD_ConsSymbol_String,
					[PD_StrictConsSymbol] = i "_!Cons",
					[PD_UnboxedConsSymbol] = i "_#Cons",
					[PD_TailStrictConsSymbol] = i "_Cons!",
					[PD_StrictTailStrictConsSymbol] = i "_!Cons!",
					[PD_UnboxedTailStrictConsSymbol] = i "_#Cons!",
					[PD_OverloadedConsSymbol] = i "_|Cons",
					[PD_NilSymbol] = i PD_NilSymbol_String,
					[PD_StrictNilSymbol] = i "_!Nil",
					[PD_UnboxedNilSymbol] = i "_#Nil",
					[PD_TailStrictNilSymbol] = 	i "_Nil!",
					[PD_StrictTailStrictNilSymbol] = i "_!Nil!",
					[PD_UnboxedTailStrictNilSymbol] = i "_#Nil!",
					[PD_OverloadedNilSymbol] = i "_|Nil",
					[PD_UnitConsSymbol] = i "_Unit",

					[PD_PredefinedModule] = i "_predefined",
					[PD_StringType] = i "_String",
					[PD_ListType] = i PD_ListType_String,
					[PD_StrictListType] = i "_!List",
					[PD_UnboxedListType] = i "_#List",
					[PD_TailStrictListType] = i "_List!",
					[PD_StrictTailStrictListType] = i "_!List!",
					[PD_UnboxedTailStrictListType] = i "_#List!",
					[PD_OverloadedListType] = i "_|List",
					[PD_LazyArrayType] = i "_Array",
					[PD_StrictArrayType] = i "_!Array",
					[PD_UnboxedArrayType] = i PD_UnboxedArray_String,
					[PD_UnitType] = i "_Unit",
					[PD_TypeCodeMember] = i "_type_code",
					[PD_DummyForStrictAliasFun] = i "_dummyForStrictAlias"
				}
	# idents = build_tuples 2 32 idents
	# idents = build_variables 0 32 idents
	
	# idents = { idents &
					[PD_StdArray] = i "_SystemArray",
					[PD_StdEnum] = i "StdEnum",
					[PD_StdBool] = i "StdBool",
					[PD_AndOp] = i "&&",
					[PD_OrOp] = i "||",
					[PD_ArrayClass] = i "Array",
					[PD_CreateArrayFun] = i "createArray",
					[PD__CreateArrayFun] = i "_createArray",
					[PD_ArraySelectFun] = i "select",
					[PD_UnqArraySelectFun] = i "uselect",
					[PD_ArrayUpdateFun] = i "update",
					[PD_ArrayReplaceFun] = i "replace",
					[PD_ArraySizeFun] = i "size",
					[PD_UnqArraySizeFun] = i "usize",
				
					[PD_StdStrictLists] = i "_SystemStrictLists",
					[PD_cons] = i "_cons",
					[PD_decons] = i "_decons",
					[PD_nil] = i "_nil",
				
					[PD_cons_u] = i "_cons_u",
					[PD_decons_u] = i "_decons_u",
					[PD_nil_u] = i "_nil_u",
				
					[PD_cons_uts] = i "_cons_uts",
					[PD_decons_uts] = i "_decons_uts",
					[PD_nil_uts] = i "_nil_uts",

					[PD_ListClass] = i "List",
					[PD_UListClass] = i "UList",
					[PD_UTSListClass] = i "UTSList",
					
					[PD_SmallerFun] = i "<",
					[PD_LessOrEqualFun] = i "<=",
					[PD_IncFun] = i "inc",
					[PD_SubFun] = i "-",
				
					[PD_From] = i "_from",
					[PD_FromThen] = i "_from_then",
					[PD_FromTo] = i "_from_to",
					[PD_FromThenTo] = i "_from_then_to",
					
					[PD_TypeCodeClass] = i "TC",
					[PD_StdDynamic] = i UnderscoreSystemDynamicModule_String,
					[PD_Dyn_DynamicTemp] = i DynamicRepresentation_String,

					[PD_Dyn_TypeCode] = i "TypeCode",
					[PD_Dyn_UnificationEnvironment] = i "_UnificationEnvironment",
					[PD_Dyn_TypeScheme] = i "TypeScheme",
					[PD_Dyn_TypeApp] = i "TypeApp",
					[PD_Dyn_TypeVar] = i "TypeVar",
					[PD_Dyn_TypeCons] = i "TypeCons",
					[PD_Dyn_TypeUnique] = i "TypeUnique",
					[PD_Dyn__TypeFixedVar] = i "_TypeFixedVar",
					[PD_Dyn_initial_unification_environment] = i "_initial_unification_environment",
					[PD_Dyn_bind_global_type_pattern_var] = i "_bind_global_type_pattern_var",
					[PD_Dyn_unify] = i "_unify",
					[PD_Dyn_normalise] = i "_normalise",

					[PD_Dyn__to_TypeCodeConstructor] = i "_to_TypeCodeConstructor",
					[PD_TypeCodeConstructor] = i "TypeCodeConstructor",

					[PD_TC_Int] = i "TC_Int",
					[PD_TC_Char] = i "TC_Char",
					[PD_TC_Real] = i "TC_Real",
					[PD_TC_Bool] = i "TC_Bool",
					[PD_TC_Dynamic] = i "TC_Dynamic",
					[PD_TC_File] = i "TC_File",
					[PD_TC_World] = i "TC_World",

					[PD_TC__Arrow] = i "TC__Arrow",

					[PD_TC__List] = i "TC__List",
					[PD_TC__StrictList] = i "TC__StrictList",
					[PD_TC__UnboxedList] = i "TC__UnboxedList",
					[PD_TC__TailStrictList] = i "TC__TailStrictList",
					[PD_TC__StrictTailStrictList] = i "TC__StrictTailStrictList",
					[PD_TC__UnboxedTailStrictList] = i "TC__UnboxedTailStrictList",

					[PD_TC__Tuple2] = i "TC__Tuple2",
					[PD_TC__Tuple3] = i "TC__Tuple3",
					[PD_TC__Tuple4] = i "TC__Tuple4",
					[PD_TC__Tuple5] = i "TC__Tuple5",
					[PD_TC__Tuple6] = i "TC__Tuple6",
					[PD_TC__Tuple7] = i "TC__Tuple7",
					[PD_TC__Tuple8] = i "TC__Tuple8",
					[PD_TC__Tuple9] = i "TC__Tuple9",
					[PD_TC__Tuple10] = i "TC__Tuple10",
					[PD_TC__Tuple11] = i "TC__Tuple11",
					[PD_TC__Tuple12] = i "TC__Tuple12",
					[PD_TC__Tuple13] = i "TC__Tuple13",
					[PD_TC__Tuple14] = i "TC__Tuple14",
					[PD_TC__Tuple15] = i "TC__Tuple15",
					[PD_TC__Tuple16] = i "TC__Tuple16",
					[PD_TC__Tuple17] = i "TC__Tuple17",
					[PD_TC__Tuple18] = i "TC__Tuple18",
					[PD_TC__Tuple19] = i "TC__Tuple19",
					[PD_TC__Tuple20] = i "TC__Tuple20",
					[PD_TC__Tuple21] = i "TC__Tuple21",
					[PD_TC__Tuple22] = i "TC__Tuple22",
					[PD_TC__Tuple23] = i "TC__Tuple23",
					[PD_TC__Tuple24] = i "TC__Tuple24",
					[PD_TC__Tuple25] = i "TC__Tuple25",
					[PD_TC__Tuple26] = i "TC__Tuple26",
					[PD_TC__Tuple27] = i "TC__Tuple27",
					[PD_TC__Tuple28] = i "TC__Tuple28",
					[PD_TC__Tuple29] = i "TC__Tuple29",
					[PD_TC__Tuple30] = i "TC__Tuple30",
					[PD_TC__Tuple31] = i "TC__Tuple31",
					[PD_TC__Tuple32] = i "TC__Tuple32",

					[PD_TC__LazyArray] = i "TC__LazyArray",
					[PD_TC__StrictArray] = i "TC__StrictArray",
					[PD_TC__UnboxedArray] = i "TC__UnboxedArray",

					[PD_TC__Unit] = i "TC__Unit",

					[PD_StdGeneric] = i "StdGeneric",
					[PD_TypeBimap] = i "Bimap",
					[PD_ConsBimap] = i "_Bimap",
					[PD_map_to] = i "map_to",
					[PD_map_from] = i "map_from",
					[PD_TypeUNIT] = i "UNIT",
					[PD_ConsUNIT] = i "UNIT",
					[PD_TypeEITHER] = i "EITHER",
					[PD_ConsLEFT] = i "LEFT",
					[PD_ConsRIGHT] = i "RIGHT",
					[PD_TypePAIR] = i "PAIR",
					[PD_ConsPAIR] = i "PAIR",					
					[PD_TypeCONS] = i "CONS",
					[PD_ConsCONS] = i "CONS",
					[PD_TypeRECORD] = i "RECORD",
					[PD_ConsRECORD] = i "RECORD",
					[PD_TypeFIELD] = i "FIELD",
					[PD_ConsFIELD] = i "FIELD",
					[PD_TypeOBJECT] = i "OBJECT",
					[PD_ConsOBJECT] = i "OBJECT",
					[PD_TGenericConsDescriptor] = i "GenericConsDescriptor",
					[PD_CGenericConsDescriptor] = i "_GenericConsDescriptor",
					[PD_TGenericRecordDescriptor] = i "GenericRecordDescriptor",
					[PD_CGenericRecordDescriptor] = i "_GenericRecordDescriptor",
					[PD_TGenericFieldDescriptor] = i "GenericFieldDescriptor",
					[PD_CGenericFieldDescriptor] = i "_GenericFieldDescriptor",
					[PD_TGenericTypeDefDescriptor] = i "GenericTypeDefDescriptor",
					[PD_CGenericTypeDefDescriptor] = i "_GenericTypeDefDescriptor",
					[PD_TGenConsPrio] = i "GenConsPrio",
					[PD_CGenConsNoPrio] = i "GenConsNoPrio",
					[PD_CGenConsPrio] = i "GenConsPrio",
					[PD_TGenConsAssoc] = i "GenConsAssoc",
					[PD_CGenConsAssocNone] = i "GenConsAssocNone",
					[PD_CGenConsAssocLeft] = i "GenConsAssocLeft",
					[PD_CGenConsAssocRight] = i "GenConsAssocRight",
					[PD_TGenType] = i "GenType",
					[PD_CGenTypeCons] = i "GenTypeCons",
					[PD_CGenTypeVar] = i "GenTypeVar",
					[PD_CGenTypeArrow] = i "GenTypeArrow",
					[PD_CGenTypeApp] = i "GenTypeApp",
				
					[PD_GenericBimap] = i "bimap",
					[PD_bimapId] = i "bimapId",
				
					[PD_TypeGenericDict] = i "GenericDict",

					[PD_StdMisc] = i "StdMisc",
					[PD_abort] = i "abort",
					[PD_undef] = i "undef",
					
					[PD_Start] = i "Start",

					[PD_FromS]= i "_from_s",
					[PD_FromTS]= i "_from_ts",
					[PD_FromSTS]= i "_from_sts",
					[PD_FromU]= i "_from_u",
					[PD_FromUTS]= i "_from_uts",
					[PD_FromO]= i "_from_o",

					[PD_FromThenS]= i "_from_then_s",
					[PD_FromThenTS]= i "_from_then_ts",
					[PD_FromThenSTS]= i "_from_then_sts",
					[PD_FromThenU]= i "_from_then_u",
					[PD_FromThenUTS]= i "_from_then_uts",
					[PD_FromThenO]= i "_from_then_o",

					[PD_FromToS]= i "_from_to_s",
					[PD_FromToTS]= i "_from_to_ts",
					[PD_FromToSTS]= i "_from_to_sts",
					[PD_FromToU]= i "_from_to_u",
					[PD_FromToUTS]= i "_from_to_uts",
					[PD_FromToO]= i "_from_to_o",

					[PD_FromThenToS]= i "_from_then_to_s",
					[PD_FromThenToTS]= i "_from_then_to_ts",
					[PD_FromThenToSTS]= i "_from_then_to_sts",
					[PD_FromThenToU]= i "_from_then_to_u",
					[PD_FromThenToUTS]= i "_from_then_to_uts",
					[PD_FromThenToO]= i "_from_then_to_o"
		}
	=: idents
	where
		i s = { id_name = s, id_info = allocPtr };

		build_tuples tup_arity max_arity idents
			| tup_arity > max_arity
				= idents
				# tup_name = "_Tuple" +++ toString tup_arity
				# idents = {idents & [GetTupleTypeIndex tup_arity]=i tup_name, [GetTupleConsIndex tup_arity]=i tup_name}
				= build_tuples (inc tup_arity) max_arity idents

		build_variables var_number max_arity idents
			| var_number == max_arity
				= idents
				# var_ident = "a" +++ toString var_number
				# idents = {idents & [PD_TypeVar_a0 + var_number] = i var_ident}
				= build_variables (inc var_number) max_arity idents

init_identifiers :: !*SymbolTable !*World -> (!*SymbolTable,!*World)
init_identifiers heap world
	# local_predefined_idents = predefined_idents
	# (heap,world) = init_predefined_idents 0 heap world
		with
			init_predefined_idents i heap world
				| i<size local_predefined_idents
					| size local_predefined_idents.[i].id_name>0
						# (heap,world) = initPtr local_predefined_idents.[i].id_info EmptySymbolTableEntry heap world
						= init_predefined_idents (i+1) heap world
						= init_predefined_idents (i+1) heap world
					= (heap,world)
	= (heap,world)

put_predefined_idents_in_hash_table :: !Int !Int !IdentClass !{!Ident} !*HashTable -> *HashTable
put_predefined_idents_in_hash_table index last_index table_kind local_predefined_idents hash_table
	| index<=last_index
		# hash_table = putPredefinedIdentInHashTable predefined_idents.[index] table_kind hash_table
		= put_predefined_idents_in_hash_table (index+1) last_index table_kind local_predefined_idents hash_table
		= hash_table

buildPredefinedSymbols :: !*HashTable -> (!.PredefinedSymbols,!*HashTable)
buildPredefinedSymbols hash_table=:{hte_symbol_heap}
	# predef_symbol_table = createArray PD_NrOfPredefSymbols { pds_module = NoIndex, pds_def = NoIndex }
	  hte_symbol_heap = fill_table_without_hashing hte_symbol_heap
	= (predef_symbol_table,fill_table_with_hashing { hash_table & hte_symbol_heap = hte_symbol_heap })
where
	local_predefined_idents = predefined_idents
	
	fill_table_without_hashing hash_table
		= build_variables 0 32 (build_tuples 2 32 hash_table)
			<<= (local_predefined_idents, PD_PredefinedModule)
			<<= (local_predefined_idents, PD_StringType)
			<<= (local_predefined_idents, PD_ListType) <<= (local_predefined_idents, PD_ConsSymbol)<<= (local_predefined_idents, PD_NilSymbol)
			<<= (local_predefined_idents, PD_StrictListType) <<= (local_predefined_idents, PD_StrictConsSymbol) <<= (local_predefined_idents, PD_StrictNilSymbol)
			<<= (local_predefined_idents, PD_UnboxedListType) <<= (local_predefined_idents, PD_UnboxedConsSymbol) <<= (local_predefined_idents, PD_UnboxedNilSymbol)
			<<= (local_predefined_idents, PD_TailStrictListType) <<= (local_predefined_idents, PD_TailStrictConsSymbol) <<= (local_predefined_idents, PD_TailStrictNilSymbol)
			<<= (local_predefined_idents, PD_StrictTailStrictListType) <<= (local_predefined_idents, PD_StrictTailStrictConsSymbol) <<= (local_predefined_idents, PD_StrictTailStrictNilSymbol)
			<<= (local_predefined_idents, PD_UnboxedTailStrictListType) <<= (local_predefined_idents, PD_UnboxedTailStrictConsSymbol) <<= (local_predefined_idents, PD_UnboxedTailStrictNilSymbol)
			<<= (local_predefined_idents, PD_OverloadedListType) <<= (local_predefined_idents, PD_OverloadedConsSymbol) <<= (local_predefined_idents, PD_OverloadedNilSymbol)
			<<= (local_predefined_idents, PD_LazyArrayType)
			<<= (local_predefined_idents, PD_StrictArrayType)
			<<= (local_predefined_idents, PD_UnboxedArrayType)
			<<= (local_predefined_idents, PD_UnitType) <<= (local_predefined_idents, PD_UnitConsSymbol)
			<<= (local_predefined_idents, PD_TypeCodeMember)
			<<= (local_predefined_idents, PD_DummyForStrictAliasFun) // MW++
	where
		build_tuples tup_arity max_arity hash_table
			| tup_arity > max_arity
				= hash_table
				= build_tuples (inc tup_arity) max_arity (hash_table <<= (local_predefined_idents, GetTupleTypeIndex tup_arity)
						<<= (local_predefined_idents, GetTupleConsIndex tup_arity))

		build_variables var_number max_arity hash_table
			| var_number == max_arity
				= hash_table
				= build_variables (inc var_number) max_arity (hash_table <<= (local_predefined_idents, PD_TypeVar_a0 + var_number))

	fill_table_with_hashing hash_table
		# hash_table = hash_table
					<<- (local_predefined_idents, IC_Module NoQualifiedIdents, PD_StdArray)
					<<- (local_predefined_idents, IC_Module NoQualifiedIdents, PD_StdEnum)
					<<- (local_predefined_idents, IC_Module NoQualifiedIdents, PD_StdBool)
					<<- (local_predefined_idents, IC_Expression, PD_AndOp)
					<<- (local_predefined_idents, IC_Expression, PD_OrOp)
					<<- (local_predefined_idents, IC_Class, PD_ArrayClass)
					<<- (local_predefined_idents, IC_Expression, PD_CreateArrayFun)
					<<- (local_predefined_idents, IC_Expression, PD__CreateArrayFun)
					<<- (local_predefined_idents, IC_Expression, PD_ArraySelectFun)
					<<- (local_predefined_idents, IC_Expression, PD_UnqArraySelectFun)
					<<- (local_predefined_idents, IC_Expression, PD_ArrayUpdateFun)
					<<- (local_predefined_idents, IC_Expression, PD_ArrayReplaceFun)
					<<- (local_predefined_idents, IC_Expression, PD_ArraySizeFun)
					<<- (local_predefined_idents, IC_Expression, PD_UnqArraySizeFun)

					<<- (local_predefined_idents, IC_Module NoQualifiedIdents, PD_StdStrictLists)
		# hash_table = put_predefined_idents_in_hash_table PD_cons PD_nil_uts IC_Expression local_predefined_idents hash_table
					<<- (local_predefined_idents, IC_Class, PD_ListClass)
					<<- (local_predefined_idents, IC_Class, PD_UListClass)
					<<- (local_predefined_idents, IC_Class, PD_UTSListClass)
					
					<<- (local_predefined_idents, IC_Expression, PD_SmallerFun)
					<<- (local_predefined_idents, IC_Expression, PD_LessOrEqualFun)
					<<- (local_predefined_idents, IC_Expression, PD_IncFun)
					<<- (local_predefined_idents, IC_Expression, PD_SubFun)

					<<- (local_predefined_idents, IC_Expression, PD_From)
					<<- (local_predefined_idents, IC_Expression, PD_FromThen)
					<<- (local_predefined_idents, IC_Expression, PD_FromTo)
					<<- (local_predefined_idents, IC_Expression, PD_FromThenTo)
					
					<<- (local_predefined_idents,	IC_Class, PD_TypeCodeClass)

					<<- (local_predefined_idents,	IC_Module NoQualifiedIdents, PD_StdDynamic)

					<<- (local_predefined_idents,	IC_Type, PD_Dyn_DynamicTemp)
					<<- (local_predefined_idents,	IC_Type, PD_Dyn_TypeCode)
					<<- (local_predefined_idents,	IC_Type, PD_Dyn_UnificationEnvironment)
		# hash_table = put_predefined_idents_in_hash_table PD_Dyn_TypeScheme PD_Dyn_normalise IC_Expression local_predefined_idents hash_table
					<<- (local_predefined_idents,	IC_Expression, PD_Dyn__to_TypeCodeConstructor)

					<<- (local_predefined_idents,	IC_Type, PD_TypeCodeConstructor)

					<<- (local_predefined_idents,	IC_Module NoQualifiedIdents, PD_StdGeneric)
		# hash_table = put_predefined_idents_in_hash_table PD_TC_Int PD_TC__Unit IC_Expression local_predefined_idents hash_table

		# hash_table = put_predefined_idents_in_hash_table PD_TypeBimap PD_TypeGenericDict IC_Type local_predefined_idents hash_table
		# hash_table = put_predefined_idents_in_hash_table PD_ConsBimap PD_bimapId IC_Expression local_predefined_idents hash_table
					<<- (local_predefined_idents,	IC_Generic,		PD_GenericBimap)	
		# bimap_type = local_predefined_idents.[PD_TypeBimap]
		# hash_table = hash_table 
					<<- (local_predefined_idents, IC_Field bimap_type, PD_map_to)
					<<- (local_predefined_idents, IC_Field bimap_type, PD_map_from)
									
					<<- (local_predefined_idents,	IC_Module NoQualifiedIdents, PD_StdMisc)

					<<- (local_predefined_idents,	IC_Expression, 	PD_abort)
					<<- (local_predefined_idents,	IC_Expression, 	PD_undef)					

					<<- (local_predefined_idents,	IC_Expression, PD_Start)

		# hash_table = put_predefined_idents_in_hash_table PD_FromS PD_FromThenToO IC_Expression local_predefined_idents hash_table

		= hash_table

MakeTupleConsSymbIndex arity 	:== arity - 2 + (PD_Arity2TupleSymbol-FirstConstructorPredefinedSymbolIndex)

MaxTupleArity				:== 32

cTCClassSymbIndex			:== 0

cTCMemberSymbIndex			:== 0

cTCInstanceSymbIndex		:== 0

make_type_def :: !Int ![TypeVar] !a !*{#PredefinedSymbol} -> (!TypeDef a,!.{#PredefinedSymbol})
make_type_def type_cons_index type_vars type_rhs pre_def_symbols
	# type_cons_ident = predefined_idents.[type_cons_index]
	= (MakeTypeDef type_cons_ident (map (\tv -> MakeAttributedTypeVar tv) type_vars) type_rhs TA_None NoPos, pre_def_symbols)

make_list_definition :: Int Int Int Ident TypeVar AType StrictnessList *{#PredefinedSymbol} -> (!TypeDef TypeRhs,!ConsDef,!ConsDef,!.{#PredefinedSymbol})
make_list_definition list_type_pre_def_symbol_index cons_pre_def_symbol_index nil_pre_def_symbol_index pre_mod_id type_var type_var_with_attr cons_strictness pre_def_symbols
	# cons_ident = predefined_idents.[cons_pre_def_symbol_index]
	  nil_ident = predefined_idents.[nil_pre_def_symbol_index]
	  list_ident = predefined_idents.[list_type_pre_def_symbol_index] 
	  
	  cons_ds = { ds_ident = cons_ident, ds_arity = 2, ds_index = cons_pre_def_symbol_index-FirstConstructorPredefinedSymbolIndex }
	  nil_symb = { ds_ident = nil_ident, ds_arity=0 ,ds_index = nil_pre_def_symbol_index-FirstConstructorPredefinedSymbolIndex }
	  (list_def, pre_def_symbols) = make_type_def list_type_pre_def_symbol_index [type_var] (AlgType [cons_ds,nil_symb]) pre_def_symbols	
	  list_type = MakeAttributedType (TA (MakeNewTypeSymbIdent list_ident 1) [type_var_with_attr])
	  cons_def = {	pc_cons_ident = cons_ident, pc_cons_arity = 2, pc_arg_types = [type_var_with_attr, list_type], pc_context = [],
				 	pc_args_strictness=cons_strictness,	pc_cons_prio =  NoPrio, pc_exi_vars = [], pc_cons_pos = PreDefPos pre_mod_id}
	  nil_def = {	pc_cons_ident = nil_ident, pc_cons_arity = 0, pc_arg_types = [], pc_args_strictness=NotStrict, pc_context = [],
	  				pc_cons_prio =  NoPrio, pc_exi_vars = [], pc_cons_pos = PreDefPos pre_mod_id}
	= (list_def,ParsedConstructorToConsDef cons_def,ParsedConstructorToConsDef nil_def,pre_def_symbols);

make_unit_definition :: Ident *{#PredefinedSymbol} -> (!TypeDef TypeRhs,!ConsDef,!.{#PredefinedSymbol})
make_unit_definition pre_mod_id pre_def_symbols
	# unit_cons_ident = predefined_idents.[PD_UnitConsSymbol]
	  unit_cons_symb = {ds_ident = unit_cons_ident, ds_arity=0 ,ds_index = PD_UnitConsSymbol-FirstConstructorPredefinedSymbolIndex}
	  (unit_type_def, pre_def_symbols) = make_type_def PD_UnitType [] (AlgType [unit_cons_symb]) pre_def_symbols	
	  unit_cons_def = {pc_cons_ident = unit_cons_ident, pc_cons_arity = 0, pc_arg_types = [], pc_args_strictness=NotStrict, pc_context = [],
		  			   pc_cons_prio =  NoPrio, pc_exi_vars = [], pc_cons_pos = PreDefPos pre_mod_id}
	= (unit_type_def,ParsedConstructorToConsDef unit_cons_def,pre_def_symbols);

buildPredefinedModule :: !Bool !*PredefinedSymbols -> (!ScannedModule, !.PredefinedSymbols)
buildPredefinedModule support_dynamics pre_def_symbols 
	# type_var_ident = predefined_idents.[PD_TypeVar_a0]
	  string_ident = predefined_idents.[PD_StringType]
	  unb_array_ident = predefined_idents.[PD_UnboxedArrayType]
	  pre_mod_ident = predefined_idents.[PD_PredefinedModule]
	  alias_dummy_ident = predefined_idents.[PD_DummyForStrictAliasFun]

	  type_var						= MakeTypeVar type_var_ident
	  type_var_with_attr			= MakeAttributedType (TV type_var)
	  unb_arr_of_char_type			= MakeAttributedType (TA (MakeNewTypeSymbIdent unb_array_ident 1) [MakeAttributedType (TB BT_Char)])

	  (string_def, pre_def_symbols)	= make_type_def PD_StringType [] (SynType unb_arr_of_char_type) pre_def_symbols
	
	  (list_def,cons_def,nil_def,pre_def_symbols)
		= make_list_definition PD_ListType PD_ConsSymbol PD_NilSymbol pre_mod_ident type_var type_var_with_attr NotStrict pre_def_symbols
	  (strict_list_def,strict_cons_def,strict_nil_def,pre_def_symbols)
		= make_list_definition PD_StrictListType PD_StrictConsSymbol PD_StrictNilSymbol pre_mod_ident type_var type_var_with_attr (Strict 1) pre_def_symbols
	  (unboxed_list_def,unboxed_cons_def,unboxed_nil_def,pre_def_symbols)
		= make_list_definition PD_UnboxedListType PD_UnboxedConsSymbol PD_UnboxedNilSymbol pre_mod_ident type_var type_var_with_attr (Strict 1) pre_def_symbols
	  (tail_strict_list_def,tail_strict_cons_def,tail_strict_nil_def,pre_def_symbols)
		= make_list_definition PD_TailStrictListType PD_TailStrictConsSymbol PD_TailStrictNilSymbol pre_mod_ident type_var type_var_with_attr (Strict 2) pre_def_symbols
	  (strict_tail_strict_list_def,strict_tail_strict_cons_def,strict_tail_strict_nil_def,pre_def_symbols)
		= make_list_definition PD_StrictTailStrictListType PD_StrictTailStrictConsSymbol PD_StrictTailStrictNilSymbol pre_mod_ident type_var type_var_with_attr (Strict 3) pre_def_symbols
	  (unboxed_tail_strict_list_def,unboxed_tail_strict_cons_def,unboxed_tail_strict_nil_def,pre_def_symbols)
		= make_list_definition PD_UnboxedTailStrictListType PD_UnboxedTailStrictConsSymbol PD_UnboxedTailStrictNilSymbol pre_mod_ident type_var type_var_with_attr (Strict 3) pre_def_symbols
	  (overloaded_list_def,overloaded_cons_def,overloaded_nil_def,pre_def_symbols)
		= make_list_definition PD_OverloadedListType PD_OverloadedConsSymbol PD_OverloadedNilSymbol pre_mod_ident type_var type_var_with_attr NotStrict pre_def_symbols

	  (array_def, pre_def_symbols)		= make_type_def PD_LazyArrayType [type_var] (AbstractType cAllBitsClear) pre_def_symbols
	  (strict_def, pre_def_symbols)		= make_type_def PD_StrictArrayType [type_var] (AbstractType cAllBitsClear) pre_def_symbols
	  (unboxed_def, pre_def_symbols)	= make_type_def PD_UnboxedArrayType [type_var] (AbstractType cAllBitsClear) pre_def_symbols

	  (unit_type_def,unit_cons_def,pre_def_symbols) = make_unit_definition pre_mod_ident pre_def_symbols

	  array_and_unit_type_defs = [array_def,strict_def,unboxed_def,unit_type_def]
	  (type_defs, cons_defs, pre_def_symbols)	= add_tuple_defs pre_mod_ident MaxTupleArity array_and_unit_type_defs [unit_cons_def] pre_def_symbols

	  alias_dummy_type = make_identity_fun_type alias_dummy_ident type_var
	  (def_classes, def_members) = make_predefined_classes_and_members support_dynamics 
	= ({ mod_ident = pre_mod_ident, mod_modification_time = "", mod_type = MK_System, mod_imports = [],mod_foreign_exports=[], mod_imported_objects = [],
		 mod_defs = {
			def_types = [string_def, list_def,strict_list_def,unboxed_list_def,tail_strict_list_def,strict_tail_strict_list_def,unboxed_tail_strict_list_def,overloaded_list_def : type_defs],
						def_constructors = [cons_def,strict_cons_def,unboxed_cons_def,tail_strict_cons_def,strict_tail_strict_cons_def,unboxed_tail_strict_cons_def,overloaded_cons_def,
											nil_def,strict_nil_def,unboxed_nil_def,tail_strict_nil_def,strict_tail_strict_nil_def,unboxed_tail_strict_nil_def,overloaded_nil_def : cons_defs],
						def_selectors = [], def_classes = def_classes,
			def_macro_indices= { ir_from = 0, ir_to = 0 },def_macros=[],def_members = def_members, def_funtypes = [alias_dummy_type], def_instances = [], 
			def_generics = [], def_generic_cases = []}}, pre_def_symbols)
where

	add_tuple_defs pre_mod_id tup_arity type_defs cons_defs pre_def_symbols
		| tup_arity >= 2
			# (type_vars, pre_def_symbols)		= make_type_vars tup_arity [] pre_def_symbols
			  tuple_ident = predefined_idents.[GetTupleConsIndex tup_arity]
			  tuple_cons_symb					= { ds_ident = tuple_ident, ds_index = MakeTupleConsSymbIndex tup_arity, ds_arity = tup_arity }
			  
			  (tuple_type_def, pre_def_symbols)	= make_type_def (GetTupleTypeIndex tup_arity) type_vars (AlgType [tuple_cons_symb]) pre_def_symbols
			  tuple_cons_def	= { pc_cons_ident = tuple_ident, pc_cons_arity = tup_arity, pc_cons_pos = PreDefPos pre_mod_id,
			  						pc_arg_types = [ MakeAttributedType (TV tv) \\ tv <- type_vars],
			  						pc_args_strictness = NotStrict, pc_context = [],
			  						pc_cons_prio =  NoPrio, pc_exi_vars = []}
			= add_tuple_defs pre_mod_id (dec tup_arity) [tuple_type_def : type_defs] [ParsedConstructorToConsDef tuple_cons_def : cons_defs] pre_def_symbols
			= (type_defs, cons_defs, pre_def_symbols)
	where
		make_type_vars nr_of_vars type_vars pre_def_symbols
			| nr_of_vars == 0
				= (type_vars, pre_def_symbols)
				# nr_of_vars = dec nr_of_vars
				# var_ident = predefined_idents.[PD_TypeVar_a0 + nr_of_vars]
				= make_type_vars nr_of_vars [MakeTypeVar var_ident : type_vars] pre_def_symbols

	make_predefined_classes_and_members support_dynamics
		| not support_dynamics
			= ([], []);
			# tc_class_name = predefined_idents.[PD_TypeCodeClass]
			  type_var_ident = predefined_idents.[PD_TypeVar_a0]
			  tc_member_name = predefined_idents.[PD_TypeCodeMember]
			
			  class_var = MakeTypeVar type_var_ident

			  me_type = { st_vars = [], st_args = [], st_args_strictness=NotStrict, st_arity = 0,
						  st_result = { at_attribute = TA_None, at_type = TV class_var },
						  st_context = [ {tc_class = TCClass {glob_module = NoIndex, glob_object = {ds_ident = tc_class_name, ds_arity = 1, ds_index = NoIndex }},
						   				tc_types = [ TV class_var ], tc_var = nilPtr}],
						  st_attr_vars = [], st_attr_env = [] }

			  tc_member_def = { me_ident = tc_member_name, me_type = me_type, me_pos = NoPos, me_priority = NoPrio, me_default_implementation = No,
								me_offset = NoIndex, me_class_vars = [], me_class = { glob_module = NoIndex, glob_object = NoIndex}, me_type_ptr = nilPtr }
			
			  tc_class_def = { class_ident = tc_class_name, class_arity = 1, class_args = [class_var], class_context = [],
			  				   class_members = {{ds_ident = tc_member_name, ds_index = cTCMemberSymbIndex, ds_arity = 0 }}, class_cons_vars = 0,
			  				   class_macro_members = {},
							   class_dictionary = { ds_ident = { tc_class_name & id_info = nilPtr }, ds_arity = 0, ds_index = NoIndex }, class_pos = NoPos
							 }
	
			= ([tc_class_def], [tc_member_def])

	make_identity_fun_type alias_dummy_id type_var
		# a = { at_attribute = TA_Anonymous, at_type = TV type_var }
		  id_symbol_type = { st_vars = [], st_args = [a], st_args_strictness = Strict 1, st_arity = 1, st_result = a, st_context = [], 
							st_attr_vars = [], st_attr_env = [] } // !.a -> .a
		= { ft_ident = alias_dummy_id, ft_arity = 1, ft_priority = NoPrio, ft_type = id_symbol_type, ft_pos = NoPos,
			ft_specials = FSP_None, ft_type_ptr = nilPtr }

DynamicRepresentation_String			:== "DynamicTemp" // "_DynamicTemp"		


