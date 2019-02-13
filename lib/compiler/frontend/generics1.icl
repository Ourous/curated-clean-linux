//**************************************************************************************
// Generic programming features 
//**************************************************************************************

implementation module generics1

import StdEnv,StdOverloadedList,compare_types
import check
from checktypes import createMoreClassDictionaries
from transform import ::Group
import genericsupport

// Data types

:: FunDefs :== {#FunDef}
:: Modules :== {#CommonDefs}
:: DclModules :== {#DclModule}
:: Groups :== {!Group}
:: *DclMacros :== *{#*{#FunDef}}

:: FunsAndGroups= ! {
		fg_fun_index :: !Index,
		fg_group_index :: !Index,
		fg_funs :: ![FunDef],
		fg_groups :: ![Group],
		fg_bimap_functions :: !BimapFunctions
	}

:: BimapFunctions = {
		bimap_id_function :: !FunctionIndexAndIdent,
		bimap_fromto_function :: !FunctionIndexAndIdent,
		bimap_tofrom_function :: !FunctionIndexAndIdent,
		bimap_to_function :: !FunctionIndexAndIdent,
		bimap_from_function :: !FunctionIndexAndIdent,
		bimap_PAIR_function :: !FunctionIndexAndIdent,
		bimap_EITHER_function :: !FunctionIndexAndIdent,
		bimap_OBJECT_function :: !FunctionIndexAndIdent,
		bimap_CONS_function :: !FunctionIndexAndIdent,
		bimap_RECORD_function :: !FunctionIndexAndIdent,
		bimap_FIELD_function :: !FunctionIndexAndIdent
	}

:: FunctionIndexAndIdent = {	
		fii_index :: !Index,
		fii_ident :: Ident
	}

OBJECT_NewType_Mask:==1;
CONS_NewType_Mask:==2;
RECORD_NewType_Mask:==4;
FIELD_NewType_Mask:==8;

:: PredefinedSymbolsData = !{psd_predefs_a :: !{#PredefinedSymbol}, psd_generic_newtypes::!Int}

:: TypeVarInfo
	| TVI_Iso !DefinedSymbol !DefinedSymbol
	| TVI_BimapExpr !Bool !Expression !Expression // Expression corresponding to the type var during generic specialization
	| TVI_Exprs ![((GlobalIndex,[Int]), Expression)] // List of expressions corresponding to the type var during generic specialization
	| TVI_SimpleBimapArgExpr !Expression

:: *GenericState = 
	{ gs_modules :: !*Modules
	, gs_exprh :: !*ExpressionHeap
	, gs_genh :: !*GenericHeap
	, gs_varh :: !*VarHeap
	, gs_tvarh :: !*TypeVarHeap
	, gs_avarh :: !*AttrVarHeap 
	, gs_error :: !*ErrorAdmin
	, gs_symtab :: !*SymbolTable
	, gs_dcl_modules :: !*DclModules
	, gs_td_infos :: !*TypeDefInfos
	, gs_funs :: !*{#FunDef}
	, gs_groups :: {!Group}
	// non-unique, read only
	, gs_predefs :: !PredefinedSymbolsData
	, gs_main_module :: !Index
	, gs_used_modules :: !NumberSet
	}

// Exported functions

convertGenerics :: 
		!Int 					// index of the main dcl module
		!NumberSet				// set of used modules
		!{#CommonDefs} 			// common definitions of all modules
		!{!Group} 				// groups of functions
		!*{# FunDef} 			// functions
		!*TypeDefInfos 			// type definition information of all modules
		!*Heaps 				// all heaps
		!*HashTable 			// needed for what creating class dictionaries
		!*PredefinedSymbols 	// predefined symbols
		!u:{# DclModule}		// dcl modules
		!*{#*{#FunDef}}			// dcl macros
		!*ErrorAdmin 			// to report errors
	->  ( !{#CommonDefs}		// common definitions of all modules
		, !{!Group}				// groups of functions
		, !*{# FunDef}			// function definitions
		, !*TypeDefInfos		// type definition infos
		, !*Heaps				// all heaps
		, !*HashTable			// needed for creating class dictinaries
		, !*PredefinedSymbols	// predefined symbols	
		, !u:{# DclModule}		// dcl modules
		, !*{#*{#FunDef}}		// dcl macros
		, !*ErrorAdmin			// to report errors
		)
convertGenerics main_dcl_module_n used_module_numbers modules groups funs td_infos heaps hash_table u_predefs dcl_modules dcl_macros error
	#! modules = {x \\ x <-: modules} 			// unique copy
	#! dcl_modules = { x \\ x <-: dcl_modules } 	// unique copy
	#! size_predefs = size u_predefs
	#! (predefs, u_predefs) = arrayCopyBegin u_predefs size_predefs // non-unique copy

	#! td_infos = clearTypeDefInfos td_infos
	#! (modules, heaps) = clearGenericDefs modules heaps

	#! generic_newtypes = determine_generic_newtypes predefs modules

	# {hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}, hp_expression_heap} = heaps
	# gs = 
		{ gs_modules = modules
		, gs_symtab = hash_table.hte_symbol_heap
		, gs_dcl_modules = dcl_modules
		, gs_td_infos = td_infos
		, gs_exprh = hp_expression_heap	
		, gs_genh = hp_generic_heap	
		, gs_varh = hp_var_heap
		, gs_tvarh = th_vars
		, gs_avarh = th_attrs
		, gs_error = error
		, gs_funs = funs
		, gs_groups = groups
		, gs_predefs = {psd_predefs_a=predefs,psd_generic_newtypes=generic_newtypes}
		, gs_main_module = main_dcl_module_n
		, gs_used_modules = used_module_numbers
		} 

	# (dcl_macros, gs) = convert_generics dcl_macros gs

	#	{ 	gs_modules = modules, gs_symtab, gs_dcl_modules = dcl_modules, gs_td_infos = td_infos, 
			gs_genh = hp_generic_heap, gs_varh = hp_var_heap, gs_tvarh = th_vars, gs_avarh = th_attrs, 
			gs_exprh = hp_expression_heap,	
			gs_error = error, gs_funs = funs, gs_groups = groups,
			gs_predefs = predefs, gs_main_module = main_dcl_module_n, gs_used_modules = used_module_numbers} = gs
	#! hash_table = { hash_table & hte_symbol_heap = gs_symtab }
	#! heaps = 
		{ hp_expression_heap = hp_expression_heap
		, hp_var_heap = hp_var_heap
		, hp_generic_heap = hp_generic_heap
		, hp_type_heaps = { th_vars = th_vars, th_attrs = th_attrs }
		}
	= (modules, groups, funs, td_infos, heaps, hash_table, u_predefs, dcl_modules, dcl_macros, error)
where
	convert_generics :: !*DclMacros !*GenericState -> (!*DclMacros, !*GenericState)
	convert_generics dcl_macros gs
		# (bimap_functions, gs) = buildGenericRepresentations gs
		| not gs.gs_error.ea_ok = (dcl_macros, gs)

		# gs = buildClasses gs
		| not gs.gs_error.ea_ok = (dcl_macros, gs)

		# (dcl_macros, gs) = convertGenericCases bimap_functions dcl_macros gs
		| not gs.gs_error.ea_ok = (dcl_macros, gs)

		#! gs = convertGenericTypeContexts gs

		= (dcl_macros, gs)

determine_generic_newtypes :: !{#PredefinedSymbol} !{#CommonDefs} -> Int
determine_generic_newtypes predefs_a modules_cd
	= add_if_generic_newtype PD_TypeOBJECT OBJECT_NewType_Mask
	 (add_if_generic_newtype PD_TypeCONS CONS_NewType_Mask
	 (add_if_generic_newtype PD_TypeRECORD RECORD_NewType_Mask
	 (add_if_generic_newtype PD_TypeFIELD FIELD_NewType_Mask 0)))
	where
		add_if_generic_newtype :: !Int !Int !Int -> Int
		add_if_generic_newtype generic_newtype_predef_index generic_newtype_mask generic_newtypes
			# {pds_module,pds_def} = predefs_a.[generic_newtype_predef_index]
			| pds_module>=0 && pds_module<size modules_cd && pds_def>=0 && pds_def<size modules_cd.[pds_module].com_type_defs
				= case modules_cd.[pds_module].com_type_defs.[pds_def].td_rhs of
					NewType _
						-> generic_newtypes bitor generic_newtype_mask
					_
						-> generic_newtypes
			= generic_newtypes

// clear stuff that might have been left over
// from compilation of other icl modules

clearTypeDefInfos :: !*{#*{#TypeDefInfo}} -> *{#*{#TypeDefInfo}}
clearTypeDefInfos td_infos
	= clear_modules 0 td_infos
where
	clear_modules n td_infos
		| n == size td_infos
			= td_infos
			#! (td_infos1, td_infos) = td_infos![n]
			#! td_infos1 = clear_td_infos 0 td_infos1
			#! td_infos = {td_infos & [n]=td_infos1}
			= clear_modules (inc n) td_infos 
			
	clear_td_infos n td_infos 			
		| n == size td_infos
			= td_infos
			#! td_infos & [n].tdi_gen_rep = NoGenericTypeReps
			= clear_td_infos (inc n) td_infos 

clearGenericDefs :: !*{#CommonDefs} !*Heaps -> (!*{#CommonDefs},!*Heaps)
clearGenericDefs modules heaps
	= clear_module 0 modules  heaps
where
	initial_gen_classes
		= createArray 32 []
	initial_gen_rep_conses
		= createArray 7 {grc_module = -1, grc_index = GCB_None, grc_local_fun_index = -1, grc_generic_info = -1,
						 grc_generic_instance_deps = AllGenericInstanceDependencies,
						 grc_ident={id_name="",id_info=nilPtr},
						 grc_optional_fun_type=No}

	clear_module n modules heaps
		| n == size modules
			= (modules, heaps)
			#! ({com_generic_defs}, modules) = modules![n]
			#! (com_generic_defs, heaps) = updateArraySt clear_generic_def {x\\x<-:com_generic_defs} heaps 			
			#! modules = {modules & [n].com_generic_defs = com_generic_defs}
			= clear_module (inc n) modules heaps

	clear_generic_def generic_def=:{gen_info_ptr} heaps=:{hp_generic_heap}
		#! (gen_info, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
		# gen_info & gen_classes = initial_gen_classes, gen_rep_conses = initial_gen_rep_conses
		#! hp_generic_heap = writePtr gen_info_ptr gen_info hp_generic_heap
		= (generic_def, {heaps & hp_generic_heap = hp_generic_heap})
		
//	generic type representation

// generic representation is built for each type argument of
// generic cases of the current module
buildGenericRepresentations :: !*GenericState -> (!BimapFunctions,!*GenericState)
buildGenericRepresentations gs=:{gs_main_module, gs_modules, gs_funs, gs_groups}
	#! (size_funs, gs_funs) = usize gs_funs
	#! size_groups = size gs_groups
	#! ({com_gencase_defs}, gs_modules) = gs_modules![gs_main_module]
	
	#! gs = { gs & gs_modules = gs_modules, gs_funs = gs_funs, gs_groups = gs_groups }
	
	# undefined_function_and_ident = {fii_index = -1,fii_ident = undef}
	  bimap_functions = {
				bimap_id_function = undefined_function_and_ident,
				bimap_fromto_function = undefined_function_and_ident,
				bimap_tofrom_function = undefined_function_and_ident,
				bimap_to_function = undefined_function_and_ident,
				bimap_from_function = undefined_function_and_ident,
		  		bimap_PAIR_function = undefined_function_and_ident,
		  		bimap_EITHER_function = undefined_function_and_ident,
		  		bimap_OBJECT_function = undefined_function_and_ident,
		  		bimap_CONS_function = undefined_function_and_ident,
		  		bimap_RECORD_function = undefined_function_and_ident,
		  		bimap_FIELD_function = undefined_function_and_ident
	  		}
	  funs_and_groups = {fg_fun_index=size_funs, fg_group_index=size_groups, fg_funs=[], fg_groups=[],fg_bimap_functions=bimap_functions}
	#! (funs_and_groups, gs)
		= foldArraySt build_generic_representation com_gencase_defs (funs_and_groups, gs)

	# {fg_funs=new_funs,fg_groups=new_groups,fg_bimap_functions} = funs_and_groups 
	# {gs_funs, gs_groups} = gs
	#! gs_funs = arrayPlusRevList gs_funs new_funs
	#! gs_groups = arrayPlusRevList gs_groups new_groups

	= (fg_bimap_functions, {gs & gs_funs = gs_funs, gs_groups = gs_groups})
where
	build_generic_representation
			{gc_type_cons=TypeConsSymb {type_index={glob_module,glob_object}, type_ident},gc_gcf,gc_pos}
			(funs_and_groups, gs)
		# (type_def,gs) = gs!gs_modules.[glob_module].com_type_defs.[glob_object]
		# (td_info, gs) = gs!gs_td_infos.[glob_module,glob_object]
		= case gc_gcf of
			GCF gc_ident {gcf_body=GCB_FunIndex fun_index,gcf_generic}
				-> case gs.gs_funs.[fun_index].fun_body of
					TransformedBody _ 
						// does not need a generic representation
						-> (funs_and_groups, gs)
					GeneratedBody	
						// needs a generic representation
						# generic_bimap = gs.gs_predefs.psd_predefs_a.[PD_GenericBimap]
						#! is_generic_bimap = gcf_generic.gi_module==generic_bimap.pds_module && gcf_generic.gi_index==generic_bimap.pds_def
						-> build_generic_type_rep type_def.td_rhs type_def.td_ident glob_module glob_object (not is_generic_bimap) is_generic_bimap td_info gc_ident.id_name gc_pos funs_and_groups gs
			GCFS gcfs
				# ({pds_module=generic_bimap_module,pds_def=generic_bimap_index},gs) = gs!gs_predefs.psd_predefs_a.[PD_GenericBimap]
				#! build_type_rep = Any (\ {gcf_generic={gi_module,gi_index}} -> not (gi_module==generic_bimap_module && gi_index==generic_bimap_index)) gcfs
				#! build_bimap_type_rep = Any (\ {gcf_generic={gi_module,gi_index}} -> gi_module==generic_bimap_module && gi_index==generic_bimap_index) gcfs
				-> build_generic_type_rep type_def.td_rhs type_def.td_ident glob_module glob_object build_type_rep build_bimap_type_rep td_info "derive generic superclass" gc_pos funs_and_groups gs
	build_generic_representation _ st
		= st

	build_generic_type_rep td_rhs type_def_ident glob_module glob_object build_type_rep build_bimap_type_rep td_info g_ident_name gc_pos funs_and_groups gs
		= case td_rhs of
			SynType _
				#  gs_error = report_derive_error g_ident_name gc_pos "a synonym type " type_def_ident.id_name gs.gs_error
				-> (funs_and_groups, {gs & gs_error = gs_error})
			AbstractType _
				#  gs_error = report_derive_error g_ident_name gc_pos "an abstract type " type_def_ident.id_name gs.gs_error
				-> (funs_and_groups, {gs & gs_error = gs_error})
			ExtensibleAlgType _
				#  gs_error = report_derive_error g_ident_name gc_pos "an extensible algebraic type " type_def_ident.id_name gs.gs_error
				-> (funs_and_groups, {gs & gs_error = gs_error})
			AlgConses _ _
				#  gs_error = report_derive_error g_ident_name gc_pos "an extensible algebraic type " type_def_ident.id_name gs.gs_error
				-> (funs_and_groups, {gs & gs_error = gs_error})
			_
				# type_def_gi = {gi_module=glob_module,gi_index=glob_object}
				-> case td_info.tdi_gen_rep of
					GenericTypeRepAndBimapTypeRep _ _
						-> (funs_and_groups, gs)
					GenericTypeRep gen_type_rep
						| not build_bimap_type_rep
							-> (funs_and_groups, gs)
							# (gen_bimap_type_rep, funs_and_groups, gs) = buildBimapGenericTypeRep type_def_gi funs_and_groups gs
							# gs & gs_td_infos.[glob_module,glob_object].tdi_gen_rep = GenericTypeRepAndBimapTypeRep gen_type_rep gen_bimap_type_rep
							-> (funs_and_groups, gs)
					GenericBimapTypeRep gen_bimap_type_rep
						| not build_type_rep
							-> (funs_and_groups, gs)
							# (gen_type_rep, funs_and_groups, gs) = buildGenericTypeRep type_def_gi funs_and_groups gs
							# gs & gs_td_infos.[glob_module,glob_object].tdi_gen_rep = GenericTypeRepAndBimapTypeRep gen_type_rep gen_bimap_type_rep
							-> (funs_and_groups, gs)
					NoGenericTypeReps
						| build_type_rep
							# (gen_type_rep, funs_and_groups, gs) = buildGenericTypeRep type_def_gi funs_and_groups gs
							| build_bimap_type_rep
								# (gen_bimap_type_rep, funs_and_groups, gs) = buildBimapGenericTypeRep type_def_gi funs_and_groups gs
								# gs & gs_td_infos.[glob_module,glob_object].tdi_gen_rep = GenericTypeRepAndBimapTypeRep gen_type_rep gen_bimap_type_rep
								-> (funs_and_groups, gs)
								# gs & gs_td_infos.[glob_module,glob_object].tdi_gen_rep = GenericTypeRep gen_type_rep
								-> (funs_and_groups, gs)
							| build_bimap_type_rep
								# (gen_bimap_type_rep, funs_and_groups, gs) = buildBimapGenericTypeRep type_def_gi funs_and_groups gs
								# gs & gs_td_infos.[glob_module,glob_object].tdi_gen_rep = GenericBimapTypeRep gen_bimap_type_rep
								-> (funs_and_groups, gs)

	report_derive_error g_ident_name gc_pos kind_of_type_string type_def_ident_name gs_error
		= reportError g_ident_name gc_pos ("cannot derive a generic instance for "+++kind_of_type_string+++type_def_ident_name) gs_error

:: TypeInfos
	= AlgebraicInfo !DefinedSymbol !DefinedSymbol ![DefinedSymbol] ![DefinedSymbol]
	| RecordInfo !DefinedSymbol !DefinedSymbol !DefinedSymbol ![DefinedSymbol]

buildGenericTypeRep :: !GlobalIndex /*type def index*/ !FunsAndGroups !*GenericState ->	(!GenericTypeRep,!FunsAndGroups,!*GenericState)
buildGenericTypeRep type_index funs_and_groups
		gs=:{gs_modules, gs_predefs, gs_main_module, gs_error, gs_td_infos, gs_exprh, gs_varh, gs_genh, gs_avarh, gs_tvarh}
	# heaps = 
		{ hp_expression_heap = gs_exprh
		, hp_var_heap = gs_varh
		, hp_generic_heap = gs_genh
		, hp_type_heaps = { th_vars = gs_tvarh, th_attrs = gs_avarh }
		}

	# (type_def, gs_modules) = gs_modules![type_index.gi_module].com_type_defs.[type_index.gi_index]

	# (type_infos, funs_and_groups, gs_modules, heaps, gs_error)
		= buildTypeDefInfo type_def type_index.gi_module gs_main_module gs_predefs funs_and_groups gs_modules heaps gs_error

	# (atype, (gs_modules, gs_td_infos, heaps, gs_error)) 
		= buildStructType type_index type_infos gs_predefs (gs_modules, gs_td_infos, heaps, gs_error)
	
	# (from_fun_ds, funs_and_groups, heaps, gs_error)
		= buildConversionFrom type_index.gi_module type_def gs_main_module gs_predefs funs_and_groups heaps gs_error

	# (to_fun_ds, funs_and_groups, heaps, gs_error)
		= buildConversionTo type_index.gi_module type_def gs_main_module gs_predefs funs_and_groups heaps gs_error

	# {hp_expression_heap, hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}} = heaps
	# gs = {gs	& gs_modules = gs_modules
				, gs_td_infos = gs_td_infos
				, gs_error = gs_error
				, gs_avarh = th_attrs
				, gs_tvarh = th_vars
				, gs_varh = hp_var_heap
				, gs_genh = hp_generic_heap
				, gs_exprh = hp_expression_heap
		   }
	= ({gtr_type=atype,gtr_to=to_fun_ds,gtr_from=from_fun_ds}, funs_and_groups, gs)

buildBimapGenericTypeRep :: !GlobalIndex /*type def index*/ !FunsAndGroups !*GenericState ->	(!GenericTypeRep,!FunsAndGroups,!*GenericState)
buildBimapGenericTypeRep type_index funs_and_groups
		gs=:{gs_modules, gs_predefs, gs_main_module, gs_error, gs_td_infos, gs_exprh, gs_varh, gs_genh, gs_avarh, gs_tvarh}
	# (type_def, gs_modules) = gs_modules![type_index.gi_module].com_type_defs.[type_index.gi_index]
	// remove TVI_TypeKind's, otherwise: abort "type var is not empty", buildTypeDefInfo seems to do this in buildGenericTypeRep
	  gs_tvarh = remove_type_argument_numbers type_def.td_args gs_tvarh
	  heaps = {hp_expression_heap=gs_exprh, hp_var_heap=gs_varh, hp_generic_heap=gs_genh, hp_type_heaps={th_vars=gs_tvarh, th_attrs=gs_avarh}}
	  (atype, (gs_modules, gs_td_infos, heaps, gs_error)) 
		= buildBimapStructType type_index gs_predefs (gs_modules, gs_td_infos, heaps, gs_error)
	  (from_fun_ds, funs_and_groups, heaps, gs_error)
		= buildBimapConversionFrom type_index.gi_module type_def gs_main_module gs_predefs funs_and_groups heaps gs_error
	  (to_fun_ds, funs_and_groups, heaps, gs_error)
		= buildBimapConversionTo type_index.gi_module type_def gs_main_module gs_predefs funs_and_groups heaps gs_error
	  {hp_expression_heap, hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}} = heaps
	  gs & gs_modules = gs_modules, gs_td_infos = gs_td_infos, gs_error = gs_error, gs_avarh = th_attrs,
		   gs_tvarh = th_vars, gs_varh = hp_var_heap, gs_genh = hp_generic_heap, gs_exprh = hp_expression_heap
	= ({gtr_type=atype,gtr_to=to_fun_ds,gtr_from=from_fun_ds}, funs_and_groups, gs)
	
//	the structure type

convertATypeToGenTypeStruct :: !Ident !Position !PredefinedSymbolsData !AType (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
														   -> (GenTypeStruct, (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))
convertATypeToGenTypeStruct ident pos {psd_predefs_a} type st
	= convert type st
where	
	convert {at_type=TA type_symb args, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=TAS type_symb args _, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=(CV tv) :@: args} st
		#! (args, st) = mapSt convert args st
		= (GTSAppVar tv args, st)
	convert {at_type=x --> y} st
		#! (x, st) = convert x st
		#! (y, st) = convert y st
		= (GTSArrow x y, st)
	convert {at_type=TV tv} st
		= (GTSVar tv, st)  
	convert {at_type=TB _} st
		= (GTSAppCons KindConst [], st)  
	convert {at_type=type} (modules, td_infos, heaps, error)
		# error = reportError ident.id_name pos ("can not build generic representation for this type", type) error
		= (GTSE, (modules, td_infos, heaps, error))

	convert_type_app {type_index} attr args (modules, td_infos, heaps, error)	
		# (type_def, modules) = modules![type_index.glob_module].com_type_defs.[type_index.glob_object]
		= case type_def.td_rhs of 
			SynType atype
				# (expanded_type, th) = expandSynonymType type_def attr args heaps.hp_type_heaps 
				-> convert {at_type = expanded_type, at_attribute = attr} 
					(modules, td_infos, {heaps & hp_type_heaps = th}, error) 
			_
				#! {pds_module, pds_def} = psd_predefs_a.[PD_UnboxedArrayType]
				| 	type_index.glob_module == pds_module
					&& type_index.glob_object == pds_def
					&& (case args of [{at_type=TB _}] -> True; _ -> False)
					-> (GTSAppCons KindConst [], (modules, td_infos, heaps, error))
				| otherwise
					#! ({tdi_kinds}, td_infos) = td_infos ! [type_index.glob_module,type_index.glob_object]
					#! kind = if (isEmpty tdi_kinds) KindConst (KindArrow tdi_kinds)
					#! (args, st) = mapSt  convert args (modules, td_infos, heaps, error)
					-> (GTSAppCons kind args, st)  

convert_bimap_AType_to_GenTypeStruct :: !AType !Position !PredefinedSymbolsData (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
															 -> (GenTypeStruct, (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin))
convert_bimap_AType_to_GenTypeStruct type pos {psd_predefs_a} st
	= convert type st
where
	convert {at_type=TA type_symb args, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=TAS type_symb args _, at_attribute} st
		= convert_type_app type_symb at_attribute args st
	convert {at_type=(CV tv) :@: args} st
		#! (args, st) = mapSt convert args st
		= (GTSAppVar tv args, st)
	convert {at_type=x --> y} st
		#! (x, st) = convert x st
		#! (y, st) = convert y st
		= (GTSArrow x y, st)  
	convert {at_type=TV tv} st
		= (GTSVar tv, st)  
	convert {at_type=TB _} st
		= (GTSAppCons KindConst [], st)  
	convert {at_type=type} (modules, td_infos, heaps, error)
		# error = reportError predefined_idents.[PD_GenericBimap].id_name pos ("can not build generic representation for this type", type) error
		= (GTSE, (modules, td_infos, heaps, error))

	convert_type_app {type_index=type_index=:{glob_module,glob_object},type_arity} attr args (modules, td_infos, heaps, error)
		# (type_def, modules) = modules![glob_module].com_type_defs.[glob_object]
		= case type_def.td_rhs of 
			SynType atype
				# (expanded_type, th) = expandSynonymType type_def attr args heaps.hp_type_heaps
				-> convert {at_type = expanded_type, at_attribute = attr} 
					(modules, td_infos, {heaps & hp_type_heaps = th}, error) 
			AbstractType _
				#! {pds_module, pds_def} = psd_predefs_a.[PD_UnboxedArrayType]
				| glob_module == pds_module && glob_object == pds_def
					&& (case args of [{at_type=TB _}] -> True; _ -> False)
					-> (GTSAppCons KindConst [], (modules, td_infos, heaps, error))
			AlgType alts
				# n_args = length args
				| n_args>0 && type_arity==n_args
					# (can_generate_bimap_to_or_from,modules,heaps)
						= can_generate_bimap_to_or_from_for_this_type type_def.td_args glob_module alts modules heaps
					| can_generate_bimap_to_or_from
						#! (tdi_kinds,td_infos) = td_infos![glob_module,glob_object].tdi_kinds			
						#! (args, st) = convert_args args (modules, td_infos, heaps, error)
						-> (GTSAppConsSimpleType {gi_module=type_index.glob_module,gi_index=type_index.glob_object} (KindArrow tdi_kinds) args, st)
						-> 	convert_type_app_to_GTSAppCons glob_module glob_object args modules td_infos heaps error
			_
				-> 	convert_type_app_to_GTSAppCons glob_module glob_object args modules td_infos heaps error
	where
		convert_type_app_to_GTSAppCons glob_module glob_object args modules td_infos heaps error
			#! (tdi_kinds,td_infos) = td_infos![glob_module,glob_object].tdi_kinds
			#! kind = if (isEmpty tdi_kinds) KindConst (KindArrow tdi_kinds)
			#! (args, st) = convert_args args (modules, td_infos, heaps, error)
			= (GTSAppCons kind args, st)

	convert_args args st
		= mapSt convert args st

can_generate_bimap_to_or_from_for_this_type :: ![ATypeVar] !Index ![DefinedSymbol] !*Modules !*Heaps -> (!Bool,!*Modules,!*Heaps)
can_generate_bimap_to_or_from_for_this_type td_args type_def_module_n alts modules heaps=:{hp_type_heaps}
	# th_vars = number_type_arguments td_args 0 hp_type_heaps.th_vars
	#! ok = check_constructors alts type_def_module_n modules th_vars
	# th_vars = remove_type_argument_numbers td_args th_vars
	# heaps = {heaps & hp_type_heaps={hp_type_heaps & th_vars=th_vars}}
	= (ok,modules,heaps)
where
	check_constructors :: ![DefinedSymbol] !Index !Modules !TypeVarHeap -> Bool
	check_constructors [{ds_index}:constructors] type_def_module_n modules th_vars
		# {cons_type,cons_exi_vars} = modules.[type_def_module_n].com_cons_defs.[ds_index]
		= isEmpty cons_exi_vars &&
		  isEmpty cons_type.st_context &&
		  check_constructor cons_type.st_args 0 th_vars &&
		  check_constructors constructors type_def_module_n modules th_vars
	check_constructors [] type_def_module_n modules th_vars
		= True

	check_constructor :: ![AType] !Int !TypeVarHeap -> Bool
	check_constructor [{at_type=TV {tv_info_ptr}}:atypes] used_type_vars th_vars
		= case sreadPtr tv_info_ptr th_vars of
			TVI_GenTypeVarNumber arg_n
				# arg_mask = 1<<arg_n
				| used_type_vars bitand arg_mask<>0
					-> False
					# used_type_vars = used_type_vars bitor arg_mask
					-> check_constructor atypes used_type_vars th_vars
	check_constructor [_:_] used_type_vars th_vars
		= False
	check_constructor [] used_type_vars th_vars
		= True

// the structure type of a generic type can often be simplified
// because bimaps for types not containing generic variables are indentity bimaps
simplify_bimap_GenTypeStruct :: ![TypeVar] !GenTypeStruct !*Heaps -> (!GenTypeStruct, !*Heaps)
simplify_bimap_GenTypeStruct gvars type heaps=:{hp_type_heaps=hp_type_heaps=:{th_vars}} 
	#! th_vars = foldSt mark_type_var gvars th_vars
	#! (type, th_vars) = simplify type th_vars
	#! th_vars = foldSt clear_type_var gvars th_vars 
	= (type, { heaps & hp_type_heaps = { hp_type_heaps & th_vars = th_vars}})
where
	simplify t=:(GTSAppCons KindConst [])  st
		= (t, st)
	simplify (GTSAppCons kind=:(KindArrow kinds) args) st
		# formal_arity = length kinds
		# actual_arity = length args
		# contains_gen_vars = occurs_list args st
		| formal_arity == actual_arity && not contains_gen_vars
			= (GTSAppConsBimapKindConst, st)
			# (args, st) = mapSt simplify args st
			= (GTSAppCons kind args, st)
	simplify (GTSAppConsSimpleType type_symbol_n kind args) st
		# contains_gen_vars = occurs_list args st
		| not contains_gen_vars
			= (GTSAppConsBimapKindConst, st)
			# (args, st) = mapSt simplify args st
			= (GTSAppConsSimpleType type_symbol_n kind args, st)
	simplify (GTSArrow x y) st
		# contains_gen_vars = occurs2 x y st
		| not contains_gen_vars
			= (GTSAppConsBimapKindConst, st)
			# (x, st) = simplify x st
			# (y, st) = simplify y st
			= (GTSArrow x y, st)
	simplify (GTSAppVar tv args) st
		# (args, st) = mapSt simplify args st
		= (GTSAppVar tv args, st)	
	simplify t=:(GTSVar tv) st
		= (t, st)
	simplify (GTSPair x y) st
		# (x, st) = simplify x st
		# (y, st) = simplify y st
		= (GTSPair x y, st)
	simplify (GTSEither x y) st
		# (x, st) = simplify x st
		# (y, st) = simplify y st
		= (GTSEither x y, st)
	simplify GTSUnit st
		= (GTSUnit, st)
	simplify (GTSCons1Bimap x) st
		# (x, st) = simplify x st
		= (GTSCons1Bimap x, st)
	simplify (GTSRecord1Bimap x) st
		# (x, st) = simplify x st
		= (GTSRecord1Bimap x, st)
	simplify (GTSCons cons_info_ds cons_index type_info gen_type_ds x) st
		# (x, st) = simplify x st
		= (GTSCons cons_info_ds cons_index type_info gen_type_ds x, st)
	simplify (GTSRecord cons_info_ds type_index gen_type_ds field_list_ds x) st
		# (x, st) = simplify x st
		= (GTSRecord cons_info_ds type_index gen_type_ds field_list_ds x, st)
	simplify (GTSField field_info_ds field_index record_info_ds x) st
		# (x, st) = simplify x st
		= (GTSField field_info_ds field_index record_info_ds x, st)
	simplify (GTSObject type_info_ds type_index cons_desc_list_ds x) st
		# (x, st) = simplify x st
		= (GTSObject type_info_ds type_index cons_desc_list_ds x, st)

	occurs (GTSAppCons _ args) st 	= occurs_list args st
	occurs (GTSAppConsSimpleType _ _ args) st 	= occurs_list args st
	occurs (GTSAppVar tv args) st 	= type_var_occurs tv st || occurs_list args st		
	occurs (GTSVar tv) st			= type_var_occurs tv st
	occurs (GTSArrow x y) st 		= occurs2 x y st
	occurs (GTSPair x y) st			= occurs2 x y st
	occurs (GTSEither x y) st		= occurs2 x y st
	occurs GTSUnit st				= False
	occurs (GTSCons1Bimap arg) st	= occurs arg st
	occurs (GTSRecord1Bimap arg) st = occurs arg st
	occurs (GTSCons _ _ _ _ arg) st = occurs arg st
	occurs (GTSRecord _ _ _ _ arg) st = occurs arg st
	occurs (GTSField _ _ _ arg) st	= occurs arg st	
	occurs (GTSObject _ _ _ arg) st	= occurs arg st	
	occurs GTSE st 					= False

	occurs2 x y st
		= occurs x st || occurs y st

	occurs_list [] st
		= False
	occurs_list [x:xs] st 
		= occurs x st || occurs_list xs st

	type_var_occurs tv th_vars
		= case sreadPtr tv.tv_info_ptr th_vars of
			TVI_Empty = False
			TVI_Used = True

	mark_type_var tv=:{tv_info_ptr} th_vars 
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars
		= case tv_info of
			TVI_Empty = writePtr tv_info_ptr TVI_Used th_vars
			_ = abort "type var is not empty"

	clear_type_var {tv_info_ptr} th_vars
		= writePtr tv_info_ptr TVI_Empty th_vars 

buildStructType ::
		!GlobalIndex				// type def global index
		!TypeInfos					// type, constructor and field info symbols
		!PredefinedSymbolsData
		(!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
	-> 	( !GenTypeStruct			// the structure type
		, (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
		)
buildStructType {gi_module,gi_index} type_infos predefs (modules, td_infos, heaps, error)
	# (type_def=:{td_ident}, modules) = modules![gi_module].com_type_defs.[gi_index]	
	= build_type type_def type_infos (modules, td_infos, heaps, error)	
where
	build_type {td_rhs=AlgType alts, td_ident, td_pos} (AlgebraicInfo type_info cons_desc_list_ds gen_type_dss cons_infos) st
		# (cons_args, st) = zipWith3St (build_alt td_ident td_pos type_info) alts cons_infos gen_type_dss st
		# type = build_sum_type cons_args
		= (GTSObject type_info {gi_module=gi_module,gi_index=gi_index} cons_desc_list_ds type, st)
	build_type
			{td_rhs=RecordType {rt_constructor,rt_fields}, td_ident, td_pos} 
			(RecordInfo ci_record_info gen_type_ds field_list_ds ci_field_infos)
			(modules, td_infos, heaps, error)
		# ({cons_type={st_args},cons_exi_vars}, modules) = modules![gi_module].com_cons_defs.[rt_constructor.ds_index]
		| isEmpty cons_exi_vars
			# (args, st) = mapSt (convertATypeToGenTypeStruct td_ident td_pos predefs) st_args (modules, td_infos, heaps, error)		
			# args = [GTSField fi {gi_module=gi_module,gi_index=fs_index} ci_record_info arg \\ arg <- args & fi <- ci_field_infos & {fs_index}<-:rt_fields]
			# prod_type = build_prod_type args
			= (GTSRecord ci_record_info {gi_module=gi_module,gi_index=gi_index} gen_type_ds field_list_ds prod_type, st)
			# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an existential type" error
			= (GTSE, (modules, td_infos, heaps, error))
	build_type {td_rhs=NewType cons, td_ident, td_pos} (AlgebraicInfo type_info cons_desc_list_ds _ _) st
		# (type, st) = build_newtype_alt td_ident td_pos cons gi_module predefs st
		= (GTSObject type_info {gi_module=gi_module,gi_index=gi_index} cons_desc_list_ds type, st)
	build_type {td_rhs=SynType type,td_ident, td_pos} type_infos (modules, td_infos, heaps, error)
		# error = reportError td_ident.id_name td_pos "cannot build a generic representation of a synonym type" error
		= (GTSE, (modules, td_infos, heaps, error))
	build_type td=:{td_rhs=AbstractType _,td_ident, td_arity, td_pos} type_infos (modules, td_infos, heaps, error)
		# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an abstract type" error
		= (GTSE, (modules, td_infos, heaps, error))

	build_alt td_ident td_pos type_info cons_def_sym=:{ds_index} cons_info gen_type_ds (modules, td_infos, heaps, error)
		# ({cons_type={st_args},cons_exi_vars}, modules) = modules![gi_module].com_cons_defs.[ds_index]
		| isEmpty cons_exi_vars
			# (args, st) = mapSt (convertATypeToGenTypeStruct td_ident td_pos predefs) st_args (modules, td_infos, heaps, error)	
			# prod_type = build_prod_type args
			= (GTSCons cons_info {gi_module=gi_module,gi_index=ds_index} type_info gen_type_ds prod_type, st)
			# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an existential type" error
			= (GTSE, (modules, td_infos, heaps, error))

buildBimapStructType ::
		!GlobalIndex				// type def global index
		!PredefinedSymbolsData
		(!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
	-> 	( !GenTypeStruct			// the structure type
		, (!*Modules, !*TypeDefInfos, !*Heaps, !*ErrorAdmin)
		)
buildBimapStructType {gi_module,gi_index} predefs (modules, td_infos, heaps, error)
	# (type_def=:{td_ident}, modules) = modules![gi_module].com_type_defs.[gi_index]	
	= build_type type_def (modules, td_infos, heaps, error)	
where
	build_type {td_rhs=AlgType alts, td_ident, td_pos} st
		# (cons_args, st) = mapSt (build_alt td_ident td_pos) alts st
		= (build_sum_type cons_args, st)
	build_type {td_rhs=RecordType {rt_constructor,rt_fields}, td_ident, td_pos} (modules, td_infos, heaps, error)
		# ({cons_type={st_args},cons_exi_vars}, modules) = modules![gi_module].com_cons_defs.[rt_constructor.ds_index]
		| isEmpty cons_exi_vars
			# (args, st) = mapSt (convertATypeToGenTypeStruct td_ident td_pos predefs) st_args (modules, td_infos, heaps, error)
			= case args of
				[arg]
					// GTSRecord if 1 field
					-> (GTSRecord1Bimap arg, st)
				_
					-> (build_prod_type args, st)
			# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an existential type" error
			= (GTSE, (modules, td_infos, heaps, error))
	build_type {td_rhs=NewType cons, td_ident, td_pos} st
		= build_newtype_alt td_ident td_pos cons gi_module predefs st
	build_type {td_rhs=SynType type,td_ident, td_pos} (modules, td_infos, heaps, error)
		# error = reportError td_ident.id_name td_pos "cannot build a generic representation of a synonym type" error
		= (GTSE, (modules, td_infos, heaps, error))
	build_type td=:{td_rhs=AbstractType _,td_ident, td_arity, td_pos} (modules, td_infos, heaps, error)
		# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an abstract type" error
		= (GTSE, (modules, td_infos, heaps, error))

	build_alt td_ident td_pos cons_def_sym=:{ds_index} (modules, td_infos, heaps, error)
		# ({cons_type={st_args},cons_exi_vars}, modules) = modules![gi_module].com_cons_defs.[ds_index]
		| isEmpty cons_exi_vars
			# (args, st) = mapSt (convertATypeToGenTypeStruct td_ident td_pos predefs) st_args (modules, td_infos, heaps, error)
			= case args of
				[arg]
					// GTSCons if 1 element
					-> (GTSCons1Bimap arg, st)
					-> (build_prod_type args, st)
			# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an existential type" error
			= (GTSE, (modules, td_infos, heaps, error))

build_newtype_alt td_ident td_pos cons_def_sym=:{ds_index} gi_module predefs (modules, td_infos, heaps, error)
	# ({cons_type={st_args},cons_exi_vars}, modules) = modules![gi_module].com_cons_defs.[ds_index]
	| isEmpty cons_exi_vars
		# st_arg = case st_args of [st_arg] -> st_arg;
		= convertATypeToGenTypeStruct td_ident td_pos predefs st_arg (modules, td_infos, heaps, error)	
		# error = reportError td_ident.id_name td_pos "cannot build a generic representation of an existential type" error
		= (GTSE, (modules, td_infos, heaps, error))

build_prod_type :: [GenTypeStruct] -> GenTypeStruct
build_prod_type types 
	= listToBin build_pair build_unit types	
where
	build_pair x y = GTSPair x y
	build_unit = GTSUnit // GTSAppCons KindConst []	

build_sum_type :: [GenTypeStruct] -> GenTypeStruct
build_sum_type types
	= listToBin build_either build_void types
where
	build_either x y = GTSEither x y
	build_void = abort "sanity check: no alternatives in a type\n"		

// build a binary representation of a list
listToBin :: (a a -> a) a [a] -> a 
listToBin bin tip [] = tip
listToBin bin tip [x] = x
listToBin bin tip xs
	# (l,r) = splitAt ((length xs) / 2) xs
	= bin (listToBin bin tip l) (listToBin bin tip r)

//	build type infos
buildTypeDefInfo :: 
		!CheckedTypeDef		// the type definition
		!Index 				// type def module
		!Index				// icl module
		!PredefinedSymbolsData
		!FunsAndGroups !*Modules !*Heaps !*ErrorAdmin
	-> 	(!TypeInfos, !FunsAndGroups, !*Modules, !*Heaps, !*ErrorAdmin)
buildTypeDefInfo td=:{td_rhs = AlgType alts} td_module main_module_index predefs funs_and_groups modules heaps error
	= buildAlgebraicTypeDefInfo td alts td_module main_module_index predefs funs_and_groups modules heaps error
buildTypeDefInfo td=:{td_rhs = RecordType {rt_constructor, rt_fields}} td_module main_module_index predefs funs_and_groups modules heaps error
	= buildRecordTypeDefInfo td rt_constructor [x\\x<-:rt_fields] td_module main_module_index predefs funs_and_groups modules heaps error
buildTypeDefInfo td=:{td_rhs = NewType cons} td_module main_module_index predefs funs_and_groups modules heaps error
	= buildAlgebraicTypeDefInfo td [cons] td_module main_module_index predefs funs_and_groups modules heaps error
buildTypeDefInfo td=:{td_rhs = SynType type, td_ident, td_pos} td_module main_module_index predefs funs_and_groups modules heaps error
	# error = reportError td_ident.id_name td_pos "cannot build constructor information for a synonym type" error
	= buildAlgebraicTypeDefInfo td [] td_module main_module_index predefs funs_and_groups modules heaps error
buildTypeDefInfo td=:{td_rhs = AbstractType _, td_ident, td_pos} td_module main_module_index predefs funs_and_groups modules heaps error
	# error = reportError td_ident.id_name td_pos "cannot build constructor information for an abstract type" error
	= buildAlgebraicTypeDefInfo td [] td_module main_module_index predefs funs_and_groups modules heaps error

buildAlgebraicTypeDefInfo {td_ident,td_pos,td_arity,td_rhs} alts td_module main_module_index predefs
				funs_and_groups=:{fg_fun_index=fun_index,fg_group_index=group_index,fg_funs=funs,fg_groups=groups} modules heaps error

	# num_conses = length alts
	# new_group_index = inc group_index

	# cons_desc_list_index = fun_index
	  type_def_dsc_index = cons_desc_list_index + 1
	  first_gen_type_index = type_def_dsc_index + 1
	  first_cons_dsc_index = first_gen_type_index + num_conses
	  new_fun_index = first_cons_dsc_index + num_conses

	# group = {group_members = [fun_index .. new_fun_index - 1]}
	# new_groups = [group:groups]

	# cons_desc_list_ds = {ds_arity=0, ds_ident=makeIdent ("cli_"+++td_ident.id_name), ds_index=cons_desc_list_index}
	  type_def_dsc_ds = {ds_arity=0, ds_ident=makeIdent ("tdi_"+++td_ident.id_name), ds_index=type_def_dsc_index}
	  gen_type_dss = [ {ds_arity=0, ds_ident=makeIdent ("gti_"+++ds_ident.id_name), ds_index=i} \\ 
		{ds_ident} <- alts & i <- [first_gen_type_index .. first_gen_type_index + num_conses - 1]]
	  cons_dsc_dss = [ {ds_arity=0, ds_ident=makeIdent ("cdi_"+++ds_ident.id_name), ds_index=i} \\ 
		{ds_ident} <- alts & i <- [first_cons_dsc_index .. first_cons_dsc_index + num_conses - 1]]

	# (cons_desc_list_fun, heaps) = build_cons_desc_list_function group_index cons_desc_list_ds cons_dsc_dss heaps	

	  (type_def_dsc_fun, heaps) = build_type_def_dsc group_index /*cons_dsc_dss*/ type_def_dsc_ds cons_desc_list_ds heaps	

	  (gen_type_dsc_funs, (modules, heaps)) = zipWithSt (build_gen_type_function group_index main_module_index td_module td_pos predefs) gen_type_dss alts (modules, heaps)

	  (cons_dsc_funs, (modules, heaps)) = zipWith3St (build_cons_dsc group_index type_def_dsc_ds) cons_dsc_dss gen_type_dss alts (modules, heaps)

	// NOTE: reverse order (new functions are added at the head) 
	# new_funs = reverse cons_dsc_funs ++ reverse gen_type_dsc_funs ++ [type_def_dsc_fun, cons_desc_list_fun : funs] 

	# funs_and_groups = {funs_and_groups & fg_fun_index=new_fun_index, fg_group_index=new_group_index, fg_funs=new_funs, fg_groups=new_groups}

	# cons_infos = AlgebraicInfo type_def_dsc_ds cons_desc_list_ds gen_type_dss cons_dsc_dss

	= (cons_infos, funs_and_groups, modules, heaps, error)
where
	build_cons_desc_list_function group_index {ds_ident} cons_info_dss heaps
		# (cons_info_exprs, heaps) = mapSt (\x st->buildFunApp main_module_index x [] st) cons_info_dss heaps
		# (gtd_conses_expr, heaps) = makeListExpr cons_info_exprs predefs heaps // gtd_conses
		# fun = makeFunction ds_ident group_index [] gtd_conses_expr No main_module_index td_pos
		= (fun, heaps)

	build_type_def_dsc group_index /*cons_info_dss*/ {ds_ident} cons_desc_list_ds heaps
		# td_name_expr = makeStringExpr td_ident.id_name // gtd_name
		# td_arity_expr = makeIntExpr td_arity // gtd_arity
		# num_conses_expr = makeIntExpr (case td_rhs of AlgType alts -> length alts; _ -> 0) // gtd_num_conses
		# (gtd_conses_expr, heaps) = buildFunApp main_module_index cons_desc_list_ds [] heaps // gtd_conses
		# (body_expr, heaps) = buildPredefConsApp PD_CGenericTypeDefDescriptor
			[td_name_expr, td_arity_expr, num_conses_expr, gtd_conses_expr] // TODO: module_name_expr
			predefs heaps
		# fun = makeFunction ds_ident group_index [] body_expr No main_module_index td_pos
		= (fun, heaps)

	build_cons_dsc group_index type_def_info_ds {ds_ident} gen_type_ds cons_ds (modules, heaps)
		# ({cons_ident,cons_type,cons_priority,cons_number,cons_exi_vars}, modules)
			= modules![td_module].com_cons_defs.[cons_ds.ds_index]
		# name_expr 			 = makeStringExpr cons_ident.id_name // gcd_name
		# arity_expr 			 = makeIntExpr cons_type.st_arity // gcd_arity
		# (prio_expr, heaps)	 = make_prio_expr cons_priority predefs heaps // gcd_prio
		# (type_def_expr, heaps) = buildFunApp main_module_index type_def_info_ds [] heaps // gcd_type_def
		# (type_expr, heaps)	 = buildFunApp main_module_index gen_type_ds [] heaps // gcd_type
		# cons_index_expr		 = makeIntExpr cons_number // gcd_index
		# (body_expr, heaps)
			= buildPredefConsApp PD_CGenericConsDescriptor 
				[name_expr, arity_expr, prio_expr, type_def_expr, type_expr, cons_index_expr]
				predefs heaps
		# fun = makeFunction ds_ident group_index [] body_expr No main_module_index td_pos		
		= (fun, (modules, heaps))

make_prio_expr NoPrio predefs heaps
	= buildPredefConsApp PD_CGenConsNoPrio [] predefs heaps
make_prio_expr (Prio assoc prio) predefs heaps
	# assoc_predef = case assoc of
		NoAssoc 	-> PD_CGenConsAssocNone 
		LeftAssoc 	-> PD_CGenConsAssocLeft
		RightAssoc 	-> PD_CGenConsAssocRight
	# (assoc_expr, heaps) = buildPredefConsApp assoc_predef [] predefs heaps 	
	# prio_expr = makeIntExpr prio
	= buildPredefConsApp PD_CGenConsPrio [assoc_expr, prio_expr] predefs heaps 

buildRecordTypeDefInfo {td_ident, td_pos, td_arity} alt fields td_module main_module_index predefs
				funs_and_groups=:{fg_fun_index=fun_index,fg_group_index=group_index,fg_funs=funs,fg_groups=groups} modules heaps error

	# num_fields = length fields
	# new_group_index = inc group_index

	# gen_type_index = fun_index
	  field_list_index = gen_type_index + 1
	  cons_dsc_index = field_list_index + 1
	  first_field_dsc_index = cons_dsc_index + 1
	  new_fun_index = first_field_dsc_index + num_fields

	# group = {group_members = [fun_index .. new_fun_index - 1]}
	# new_groups = [group:groups]

	# gen_type_ds = {ds_arity=0, ds_ident=makeIdent ("gti_"+++alt.ds_ident.id_name), ds_index=gen_type_index}
	  field_list_ds = {ds_arity=0, ds_ident=makeIdent ("fli_"+++alt.ds_ident.id_name), ds_index=field_list_index}
	  record_dsc_ds = {ds_arity=0, ds_ident=makeIdent ("rdi_"+++alt.ds_ident.id_name), ds_index=cons_dsc_index}
	  field_dsc_dss = [ {ds_arity=0, ds_ident=makeIdent ("fdi_"+++fs_ident.id_name), ds_index=i} \\ 
		{fs_ident} <- fields & i <- [first_field_dsc_index .. first_field_dsc_index + num_fields - 1]]

	# (gen_type_dsc_fun, (modules, heaps)) = build_gen_type_function group_index main_module_index td_module td_pos predefs gen_type_ds alt (modules, heaps)

	  (field_list_fun, (modules, heaps)) = build_field_list_function group_index field_list_ds (modules, heaps)

	  (record_dsc_fun, (modules, heaps)) = build_record_dsc group_index td_ident record_dsc_ds gen_type_ds field_list_ds alt (modules, heaps)

	  (field_dsc_funs, (modules, heaps)) = zipWithSt (build_field_dsc group_index record_dsc_ds) field_dsc_dss fields (modules, heaps)
	 
	// NOTE: reverse order (new functions are added at the head) 
	# new_funs = reverse field_dsc_funs ++ [record_dsc_fun, field_list_fun, gen_type_dsc_fun : funs]

	# funs_and_groups = {funs_and_groups & fg_fun_index=new_fun_index, fg_group_index=new_group_index, fg_funs=new_funs, fg_groups=new_groups}

	# cons_infos = RecordInfo record_dsc_ds gen_type_ds field_list_ds field_dsc_dss

	= (cons_infos, funs_and_groups, modules, heaps, error)
where
	build_field_list_function group_index field_list_ds (modules, heaps)
		# field_exprs		 	 = [makeStringExpr id_name \\ {fs_ident={id_name}}<-fields]
		# (fields_expr, heaps)   = makeListExpr field_exprs predefs heaps // grd_fields
		# fun = makeFunction field_list_ds.ds_ident group_index [] fields_expr No main_module_index td_pos		
		= (fun, (modules, heaps))

	build_record_dsc group_index td_ident cons_info_ds gen_type_ds field_list_ds cons_ds (modules, heaps)
		# ({cons_ident,cons_type,cons_priority,cons_number}, modules)
			= modules![td_module].com_cons_defs.[cons_ds.ds_index]
		# name_expr 			 = makeStringExpr td_ident.id_name /*cons_ident.id_name*/ // grd_name
		# arity_expr 			 = makeIntExpr cons_type.st_arity // grd_arity
		# td_arity_expr 		 = makeIntExpr td_arity // grd_type_arity
		# (type_expr, heaps)	 = buildFunApp main_module_index gen_type_ds [] heaps // grd_type
		# (fields_expr, heaps)	 = buildFunApp main_module_index field_list_ds [] heaps // grd_fields
		# (body_expr, heaps)
			= buildPredefConsApp PD_CGenericRecordDescriptor
				[name_expr, arity_expr, td_arity_expr, type_expr, fields_expr]
				predefs heaps
		# fun = makeFunction cons_info_ds.ds_ident group_index [] body_expr No main_module_index td_pos		
		= (fun, (modules, heaps))

	build_field_dsc group_index record_dsc_ds field_dsc_ds {fs_ident, fs_index} (modules, heaps)
		# ({sd_field_nr}, modules)
			= modules![td_module].com_selector_defs.[fs_index]
		# name_expr = makeStringExpr fs_ident.id_name // gfd_name
		# index_expr = makeIntExpr sd_field_nr // gfd_index
		# (cons_expr, heaps) = buildFunApp main_module_index record_dsc_ds [] heaps // gfd_cons
		# (body_expr, heaps) 
			= buildPredefConsApp PD_CGenericFieldDescriptor 
				[name_expr, index_expr, cons_expr]
				predefs heaps
		# fun = makeFunction field_dsc_ds.ds_ident group_index [] body_expr No main_module_index td_pos		
		= (fun, (modules, heaps))

build_gen_type_function group_index main_module_index td_module td_pos predefs cons_info_ds cons_ds (modules, heaps)
	# ({cons_type,cons_exi_vars}, modules) = modules![td_module].com_cons_defs.[cons_ds.ds_index]
	# (type_expr, heaps) 	 = make_type_expr cons_exi_vars cons_type heaps
	# fun = makeFunction cons_info_ds.ds_ident group_index [] type_expr No main_module_index td_pos		
	= (fun, (modules, heaps))
where
	make_type_expr [] {st_vars, st_args, st_result} heaps=:{hp_type_heaps=type_heaps=:{th_vars}}
		# (_,th_vars) = foldSt (\ {tv_info_ptr} (n, th_vars) -> (n+1, writePtr tv_info_ptr (TVI_GenTypeVarNumber n) th_vars)) st_vars (0,th_vars)
		# heaps = {heaps & hp_type_heaps={type_heaps & th_vars=th_vars}}
		# (arg_exprs, heaps) = mapSt make_expr1 st_args heaps
		# (result_expr, heaps) = make_expr1 st_result heaps
		# {hp_type_heaps=type_heaps=:{th_vars}} = heaps
		# th_vars = foldSt (\ {tv_info_ptr} th_vars -> writePtr tv_info_ptr TVI_Empty th_vars) st_vars th_vars
		# heaps = {heaps & hp_type_heaps={type_heaps & th_vars=th_vars}}
		= curry arg_exprs result_expr heaps
	where
		curry [] result_expr heaps 
			= (result_expr, heaps)
		curry [x:xs] result_expr heaps
			# (y, heaps) = curry xs result_expr heaps
			= make_arrow x y heaps

		make_expr1 :: !AType !*Heaps -> (!Expression, !*Heaps)
		make_expr1 {at_type} heaps = make_expr at_type heaps

		make_expr :: !Type !*Heaps -> (!Expression, !*Heaps)
		make_expr (TA type_symb arg_types) heaps
			# (arg_exprs, heaps) = mapSt make_expr1 arg_types heaps
			# (type_cons, heaps) = make_type_cons type_symb.type_ident.id_name heaps 
			= make_apps type_cons arg_exprs heaps
		make_expr (TAS type_symb arg_types _) heaps
			# (arg_exprs, heaps) = mapSt make_expr1 arg_types heaps
			# (type_cons, heaps) = make_type_cons type_symb.type_ident.id_name heaps 
			= make_apps type_cons arg_exprs heaps
		make_expr (x --> y) heaps
			# (x, heaps) = make_expr1 x heaps
			# (y, heaps) = make_expr1 y heaps				
			= make_arrow x y heaps
		make_expr TArrow heaps 
			= make_type_cons "(->)" heaps
		make_expr (TArrow1 type) heaps
			# (arg_expr, heaps) = make_expr1 type heaps 
			# (arrow_expr, heaps) = make_type_cons "(->)" heaps
			= make_app arrow_expr arg_expr heaps
		make_expr (CV {tv_info_ptr} :@: arg_types) heaps
			# (arg_exprs, heaps) = mapSt make_expr1 arg_types heaps
			# (tv_expr, heaps) = make_type_var tv_info_ptr heaps
			= make_apps tv_expr arg_exprs heaps
		make_expr (TB bt) heaps
			= make_type_cons (toString bt) heaps	
		make_expr (TV {tv_info_ptr}) heaps 
			= make_type_var tv_info_ptr heaps 
		make_expr (GTV {tv_info_ptr}) heaps
			= make_type_var tv_info_ptr heaps 
		make_expr TE heaps
			= make_error_type_cons heaps
		make_expr (TFA _ _) heaps
			// error is reported in convertATypeToGenTypeStruct
			= make_error_type_cons heaps
		make_expr (TFAC _ _ _) heaps
			// error is reported in convertATypeToGenTypeStruct
			= make_error_type_cons heaps
		make_expr _ heaps
			= abort "type does not match\n"

		make_apps x [] heaps 
			= (x, heaps)
		make_apps x [y:ys] heaps
			# (z, heaps) = make_app x y heaps	
			= make_apps z ys heaps

		make_type_var tv_info_ptr heaps
			#! type_var_n = case sreadPtr tv_info_ptr heaps.hp_type_heaps.th_vars of
								TVI_GenTypeVarNumber n -> n
			= buildPredefConsApp PD_CGenTypeVar [makeIntExpr type_var_n] predefs heaps									

		make_arrow x y heaps = buildPredefConsApp PD_CGenTypeArrow [x, y] predefs heaps

		make_app x y heaps = buildPredefConsApp PD_CGenTypeApp [x, y] predefs heaps 	 

		make_error_type_cons heaps = make_type_cons "<error>" heaps
	make_type_expr [_:_] {st_vars, st_args, st_result} heaps
		// Error "cannot build a generic representation of an existential type" is reported in buildStructType
		= make_type_cons "<error>" heaps

	make_type_cons name heaps
		# name_expr = makeStringExpr name
		= buildPredefConsApp PD_CGenTypeCons [name_expr] predefs heaps

//	conversions functions

// conversion from type to generic
buildConversionTo ::
		!Index				// type def module
		!CheckedTypeDef 	// the type def
		!Index 				// main module
		!PredefinedSymbolsData
		!FunsAndGroups !*Heaps !*ErrorAdmin
	-> 	(!DefinedSymbol,
		 FunsAndGroups,!*Heaps,!*ErrorAdmin)
buildConversionTo		
		type_def_mod 
		type_def=:{td_rhs, td_ident, td_index, td_pos} 
		main_module_index predefs funs_and_groups heaps error
	# (arg_expr, arg_var, heaps) = buildVarExpr "x" heaps 
	# (body_expr, heaps, error) = 
		build_expr_for_type_rhs type_def_mod td_index td_rhs arg_expr heaps error
	# fun_name = makeIdent ("toGeneric" +++ td_ident.id_name)
	| not error.ea_ok
		# (def_sym, funs_and_groups)
			= (buildFunAndGroup fun_name [] EE No main_module_index td_pos funs_and_groups)
		= (def_sym, funs_and_groups, heaps, error)
		# (def_sym, funs_and_groups) 
			= (buildFunAndGroup fun_name [arg_var] body_expr No main_module_index td_pos funs_and_groups)
		= (def_sym, funs_and_groups, heaps, error)
where
	// build conversion for type rhs
	build_expr_for_type_rhs :: 
			!Int 				// type def module
			!Int 				// type def index 
			!TypeRhs			// type def rhs 
			!Expression			// expression of the function argument variable   
			!*Heaps 
			!*ErrorAdmin
		-> 	( !Expression		// generated expression
			, !*Heaps	// state
			, !*ErrorAdmin)
 	build_expr_for_type_rhs type_def_mod type_def_index (AlgType def_symbols) arg_expr heaps error
		= build_expr_for_conses type_def_mod type_def_index def_symbols arg_expr heaps error
	build_expr_for_type_rhs type_def_mod type_def_index (RecordType {rt_constructor}) arg_expr heaps error		
		= build_expr_for_record type_def_mod type_def_index rt_constructor arg_expr heaps error
 	build_expr_for_type_rhs type_def_mod type_def_index (NewType cons) arg_expr heaps error
		= build_expr_for_newtype type_def_mod type_def_index cons arg_expr heaps error
	build_expr_for_type_rhs type_def_mod type_def_index (AbstractType _) arg_expr  heaps error
		#! error = checkErrorWithIdentPos (newPosition td_ident td_pos) "cannot build isomorphisms for an abstract type" error
		= (EE, heaps, error)
	build_expr_for_type_rhs type_def_mod type_def_index (SynType _) arg_expr  heaps error
		#! error = checkErrorWithIdentPos (newPosition td_ident td_pos) "cannot build isomorphisms for a synonym type" error
		= (EE, heaps, error)

	// build conversion for constructors of a type def 	
	build_expr_for_conses type_def_mod type_def_index cons_def_syms arg_expr heaps error
		# (case_alts, heaps, error)
			= build_exprs_for_conses 0 (length cons_def_syms) type_def_mod cons_def_syms  heaps error
		# case_patterns = AlgebraicPatterns {gi_module = type_def_mod, gi_index = type_def_index} case_alts
		# (case_expr, heaps) = buildCaseExpr arg_expr case_patterns heaps
		= (case_expr, heaps, error)

	// build conversions for constructors 	
	build_exprs_for_conses :: !Int !Int !Int ![DefinedSymbol] !*Heaps !*ErrorAdmin
		-> ([AlgebraicPattern], !*Heaps, !*ErrorAdmin)
	build_exprs_for_conses i n type_def_mod [] heaps error
		= ([], heaps, error)
	build_exprs_for_conses i n type_def_mod [cons_def_sym:cons_def_syms] heaps error
		#! (alt, heaps, error) = build_expr_for_cons i n type_def_mod cons_def_sym heaps error
		#! (alts, heaps, error) =  build_exprs_for_conses (i+1) n type_def_mod cons_def_syms heaps error 		
		= ([alt:alts], heaps, error)

	// build conversion for a constructor	
	build_expr_for_cons :: !Int !Int !Int !DefinedSymbol !*Heaps !*ErrorAdmin 
		-> (AlgebraicPattern, !*Heaps, !*ErrorAdmin)
	build_expr_for_cons i n type_def_mod cons_def_sym=:{ds_ident, ds_arity} heaps error	
		#! names = ["x" +++ toString (i+1) +++ toString k \\ k <- [1..ds_arity]]
		#! (var_exprs, vars, heaps) = buildVarExprs names heaps 

		#! arg_exprs = var_exprs

		#! (expr, heaps) = build_prod arg_exprs predefs heaps
		#! (expr, heaps) = build_cons expr predefs heaps
		#! (expr, heaps) = build_sum i n expr predefs heaps
				
		#! (expr, heaps) = build_object expr predefs heaps
						
		#! alg_pattern = {
			ap_symbol = {glob_module = type_def_mod, glob_object = cons_def_sym},
			ap_vars = vars,
			ap_expr = expr,
			ap_position = NoPos
			}
		= (alg_pattern, heaps, error)	

	build_expr_for_newtype type_def_mod type_def_index cons_def_sym arg_expr heaps error
		# (alt, heaps, error) = build_expr_for_newtype_cons type_def_mod cons_def_sym heaps error
		# case_patterns = NewTypePatterns {gi_module = type_def_mod, gi_index = type_def_index} [alt]
		# (case_expr, heaps) = buildCaseExpr arg_expr case_patterns heaps
		= (case_expr, heaps, error)

	build_expr_for_newtype_cons :: !Int !DefinedSymbol !*Heaps !*ErrorAdmin -> (AlgebraicPattern, !*Heaps, !*ErrorAdmin)
	build_expr_for_newtype_cons type_def_mod cons_def_sym heaps error	
		# (var_expr, var, heaps) = buildVarExpr "x11" heaps

		#! expr = var_expr
		#! (expr, heaps) = build_object expr predefs heaps
						
		#! alg_pattern = {
			ap_symbol = {glob_module = type_def_mod, glob_object = cons_def_sym},
			ap_vars = [var],
			ap_expr = expr,
			ap_position = NoPos
			}
		= (alg_pattern, heaps, error)	

	// build conversion for a record type def
	build_expr_for_record type_def_mod type_def_index cons_def_sym=:{ds_ident, ds_arity} arg_expr heaps error
		#! names = ["x1" +++ toString k \\ k <- [1..ds_arity]]
		#! (var_exprs, vars, heaps) = buildVarExprs names heaps 
		#! (arg_exprs, heaps) = mapSdSt build_field var_exprs predefs heaps
		#! (expr, heaps) = build_prod arg_exprs predefs heaps
		#! (expr, heaps) = build_record expr predefs heaps
		#! alg_pattern = { ap_symbol = {glob_module = type_def_mod, glob_object = cons_def_sym},
						   ap_vars = vars, ap_expr = expr, ap_position = NoPos }
		# case_patterns = AlgebraicPatterns {gi_module = type_def_mod, gi_index = type_def_index} [alg_pattern]
		# (case_expr, heaps) = buildCaseExpr arg_expr case_patterns heaps
		= (case_expr, heaps, error)

// conversion from type to generic
buildBimapConversionTo ::
		!Index				// type def module
		!CheckedTypeDef 	// the type def
		!Index 				// main module
		!PredefinedSymbolsData
		!FunsAndGroups !*Heaps !*ErrorAdmin
	-> 	(!DefinedSymbol,
		 FunsAndGroups,!*Heaps,!*ErrorAdmin)
buildBimapConversionTo		
		type_def_mod 
		type_def=:{td_rhs, td_ident, td_index, td_pos} 
		main_module_index predefs funs_and_groups heaps error
	# (arg_expr, arg_var, heaps) = buildVarExpr "x" heaps 
	# (body_expr, heaps, error) = build_expr_for_type_rhs type_def_mod td_index td_rhs arg_expr heaps error
	# fun_name = makeIdent ("toGeneric-" +++ td_ident.id_name)
	| not error.ea_ok
		# (def_sym, funs_and_groups) = buildFunAndGroup fun_name [] EE No main_module_index td_pos funs_and_groups
		= (def_sym, funs_and_groups, heaps, error)
		# (def_sym, funs_and_groups) = buildFunAndGroup fun_name [arg_var] body_expr No main_module_index td_pos funs_and_groups
		= (def_sym, funs_and_groups, heaps, error)
where
	// build conversion for type rhs
	build_expr_for_type_rhs :: 
			!Int 				// type def module
			!Int 				// type def index 
			!TypeRhs			// type def rhs 
			!Expression			// expression of the function argument variable   
			!*Heaps 
			!*ErrorAdmin
		-> 	( !Expression		// generated expression
			, !*Heaps	// state
			, !*ErrorAdmin)
 	build_expr_for_type_rhs type_def_mod type_def_index (AlgType def_symbols) arg_expr heaps error
		= build_expr_for_conses type_def_mod type_def_index def_symbols arg_expr heaps error
	build_expr_for_type_rhs type_def_mod type_def_index (RecordType {rt_constructor}) arg_expr heaps error		
		= build_expr_for_record type_def_mod type_def_index rt_constructor arg_expr heaps error
 	build_expr_for_type_rhs type_def_mod type_def_index (NewType cons) arg_expr heaps error
		= build_expr_for_newtype type_def_mod type_def_index cons arg_expr heaps error
	build_expr_for_type_rhs type_def_mod type_def_index (AbstractType _) arg_expr  heaps error
		#! error = checkErrorWithIdentPos (newPosition td_ident td_pos) "cannot build isomorphisms for an abstract type" error
		= (EE, heaps, error)
	build_expr_for_type_rhs type_def_mod type_def_index (SynType _) arg_expr  heaps error
		#! error = checkErrorWithIdentPos (newPosition td_ident td_pos) "cannot build isomorphisms for a synonym type" error
		= (EE, heaps, error)

	// build conversion for constructors of a type def 	
	build_expr_for_conses type_def_mod type_def_index cons_def_syms arg_expr heaps error
		# (case_alts, heaps, error)
			= build_exprs_for_conses 0 (length cons_def_syms) type_def_mod cons_def_syms heaps error
		# case_patterns = AlgebraicPatterns {gi_module = type_def_mod, gi_index = type_def_index} case_alts
		# (case_expr, heaps) = buildCaseExpr arg_expr case_patterns heaps
		= (case_expr, heaps, error)

	// build conversions for constructors 	
	build_exprs_for_conses :: !Int !Int !Int ![DefinedSymbol] !*Heaps !*ErrorAdmin
		-> ([AlgebraicPattern], !*Heaps, !*ErrorAdmin)
	build_exprs_for_conses i n type_def_mod [] heaps error
		= ([], heaps, error)
	build_exprs_for_conses i n type_def_mod [cons_def_sym:cons_def_syms] heaps error
		#! (alt, heaps, error) = build_expr_for_cons i n type_def_mod cons_def_sym heaps error
		#! (alts, heaps, error) =  build_exprs_for_conses (i+1) n type_def_mod cons_def_syms heaps error 		
		= ([alt:alts], heaps, error)

	// build conversion for a constructor	
	build_expr_for_cons :: !Int !Int !Int !DefinedSymbol !*Heaps !*ErrorAdmin 
		-> (AlgebraicPattern, !*Heaps, !*ErrorAdmin)
	build_expr_for_cons i n type_def_mod cons_def_sym=:{ds_ident, ds_arity} heaps error	
		#! names = ["x" +++ toString (i+1) +++ toString k \\ k <- [1..ds_arity]]
		#! (var_exprs, vars, heaps) = buildVarExprs names heaps
		#! (expr, heaps) = if (ds_arity==1)
							(build_cons (hd var_exprs) predefs heaps)
							(build_prod var_exprs predefs heaps)
		#! (expr, heaps) = build_sum i n expr predefs heaps
		#! alg_pattern = {ap_symbol={glob_module=type_def_mod,glob_object=cons_def_sym}, ap_vars=vars, ap_expr=expr, ap_position=NoPos}
		= (alg_pattern, heaps, error)	

	build_expr_for_newtype type_def_mod type_def_index cons_def_sym arg_expr heaps error
		# (alt, heaps, error) = build_expr_for_newtype_cons type_def_mod cons_def_sym heaps error
		# case_patterns = NewTypePatterns {gi_module = type_def_mod, gi_index = type_def_index} [alt]
		# (case_expr, heaps) = buildCaseExpr arg_expr case_patterns heaps
		= (case_expr, heaps, error)

	build_expr_for_newtype_cons :: !Int !DefinedSymbol !*Heaps !*ErrorAdmin -> (AlgebraicPattern, !*Heaps, !*ErrorAdmin)
	build_expr_for_newtype_cons type_def_mod cons_def_sym heaps error	
		# (var_expr, var, heaps) = buildVarExpr "x11" heaps
		#! alg_pattern = {ap_symbol={glob_module=type_def_mod,glob_object=cons_def_sym}, ap_vars=[var], ap_expr=var_expr, ap_position=NoPos}
		= (alg_pattern, heaps, error)	

	// build conversion for a record type def
	build_expr_for_record type_def_mod type_def_index cons_def_sym=:{ds_ident, ds_arity} arg_expr heaps error
		#! names = ["x1" +++ toString k \\ k <- [1..ds_arity]]
		#! (var_exprs, vars, heaps) = buildVarExprs names heaps 
		#! (expr, heaps) = if (ds_arity==1)
							(build_record (hd var_exprs) predefs heaps)
							(build_prod var_exprs predefs heaps)
		#! alg_pattern = { ap_symbol = {glob_module = type_def_mod, glob_object = cons_def_sym},
						   ap_vars = vars, ap_expr = expr, ap_position = NoPos }
		# case_patterns = AlgebraicPatterns {gi_module = type_def_mod, gi_index = type_def_index} [alg_pattern]
		# (case_expr, heaps) = buildCaseExpr arg_expr case_patterns heaps
		= (case_expr, heaps, error)

build_prod :: ![Expression] !PredefinedSymbolsData !*Heaps -> (!Expression, !*Heaps)
build_prod [] predefs heaps = build_unit heaps
where
	build_unit heaps = buildPredefConsApp PD_ConsUNIT [] predefs heaps 	
build_prod [expr] predefs heaps = (expr, heaps)
build_prod exprs predefs heaps
	# (lexprs, rexprs) = splitAt ((length exprs)/2) exprs  
	# (lexpr, heaps) = build_prod lexprs predefs heaps
	# (rexpr, heaps) = build_prod rexprs predefs heaps
	= build_pair lexpr rexpr predefs heaps

build_sum :: !Int !Int !Expression !PredefinedSymbolsData !*Heaps -> (!Expression, !*Heaps)
build_sum i n expr predefs heaps
	| n == 0 	= abort "build sum of zero elements\n"
	| i >= n	= abort "error building sum"
	| n == 1 	= (expr, heaps)
	| i < (n/2) 
		# (expr, heaps) = build_sum i (n/2) expr predefs heaps
		= build_left expr predefs heaps
	| otherwise
		# (expr, heaps) = build_sum (i - (n/2)) (n - (n/2)) expr predefs heaps
		= build_right expr predefs heaps

buildConversionFrom	::	
		!Index				// type def module
		!CheckedTypeDef 	// the type def
		!Index 				// main module
		!PredefinedSymbolsData !FunsAndGroups !*Heaps !*ErrorAdmin
	-> (!DefinedSymbol,        !FunsAndGroups,!*Heaps,!*ErrorAdmin)
buildConversionFrom type_def_mod type_def=:{td_rhs, td_ident, td_pos} main_module_index predefs funs_and_groups heaps error
	# (body_expr, arg_var, heaps, error) = build_expr_for_type_rhs type_def_mod td_rhs heaps error
	# fun_name = makeIdent ("fromGeneric" +++ td_ident.id_name)
	| not error.ea_ok
		# (def_sym, funs_and_groups) = buildFunAndGroup fun_name [] EE No main_module_index td_pos funs_and_groups
		= (def_sym, funs_and_groups, heaps, error)
		# (def_sym, funs_and_groups) = buildFunAndGroup fun_name [arg_var] body_expr No main_module_index td_pos funs_and_groups
		= (def_sym, funs_and_groups, heaps, error)
where
	// build expression for type def rhs
	build_expr_for_type_rhs :: 
			!Index				// type def module
			!TypeRhs			// type rhs
			!*Heaps !*ErrorAdmin	
		-> 	( !Expression		// body expresssion
			, !FreeVar, !*Heaps, !*ErrorAdmin)
	build_expr_for_type_rhs type_def_mod (AlgType def_symbols) heaps error
		#! (expr, var, heaps, error) = build_sum type_def_mod def_symbols heaps error
		#! (expr, var, heaps) = build_case_object var expr predefs heaps
		= (expr, var, heaps, error)
	build_expr_for_type_rhs type_def_mod (RecordType {rt_constructor}) heaps error				
		= build_record type_def_mod rt_constructor heaps error
	build_expr_for_type_rhs type_def_mod (NewType cons) heaps error
		#! (expr, var, heaps) = build_newtype_cons_app type_def_mod cons heaps
		#! (expr, var, heaps) = build_case_object var expr predefs heaps
		= (expr, var, heaps, error)
	build_expr_for_type_rhs type_def_mod (AbstractType _) heaps error
		#! error = reportError td_ident.id_name td_pos "cannot build isomorphisms for an abstract type" error
		# dummy_fv = {fv_def_level=(-1), fv_count=0, fv_ident=makeIdent "dummy", fv_info_ptr=nilPtr}
		= (EE, dummy_fv, heaps, error)
	build_expr_for_type_rhs type_def_mod (SynType _) heaps error
		#! error = reportError td_ident.id_name td_pos "cannot build isomorphisms for a synonym type" error
		# dummy_fv = {fv_def_level=(-1), fv_count=0, fv_ident=makeIdent "dummy", fv_info_ptr=nilPtr}
		= (EE, dummy_fv, heaps, error)
	
	// build expression for sums
	build_sum :: !Index ![DefinedSymbol] !*Heaps !*ErrorAdmin -> (!Expression,!FreeVar/*top variable*/,!*Heaps,!*ErrorAdmin)
	build_sum type_def_mod [] heaps error
		= abort "algebraic type with no constructors!\n"
	build_sum type_def_mod [def_symbol] heaps error
		#! (cons_app_expr, cons_arg_vars, heaps) = build_cons_app type_def_mod def_symbol heaps
		#! (prod_expr, var, heaps) = build_case_prod False cons_app_expr cons_arg_vars predefs heaps
		#! (alt_expr, var, heaps) = build_case_cons var prod_expr predefs heaps
		= (alt_expr, var, heaps, error)
	build_sum type_def_mod def_symbols heaps error
		#! (left_def_syms, right_def_syms) = splitAt ((length def_symbols) /2) def_symbols
		#! (left_expr, left_var, heaps, error) = build_sum type_def_mod left_def_syms heaps error
		#! (right_expr, right_var, heaps, error) = build_sum type_def_mod right_def_syms heaps error	
		#! (case_expr, var, heaps) = build_case_either left_var left_expr right_var right_expr predefs heaps
		= (case_expr, var, heaps, error)

	build_record :: !Index !DefinedSymbol !*Heaps !*ErrorAdmin -> (!Expression,!FreeVar/*top variable*/,!*Heaps,!*ErrorAdmin)
	build_record type_def_mod def_symbol heaps error
		#! (cons_app_expr, cons_arg_vars, heaps) = build_cons_app type_def_mod def_symbol heaps
		#! (prod_expr, var, heaps) = build_case_prod True cons_app_expr cons_arg_vars predefs heaps
		#! (alt_expr, var, heaps) = build_case_record var prod_expr predefs heaps
		= (alt_expr, var, heaps, error)

buildBimapConversionFrom	::	
		!Index				// type def module
		!CheckedTypeDef 	// the type def
		!Index 				// main module
		!PredefinedSymbolsData !FunsAndGroups !*Heaps !*ErrorAdmin
	-> (!DefinedSymbol,        !FunsAndGroups,!*Heaps,!*ErrorAdmin)
buildBimapConversionFrom type_def_mod type_def=:{td_rhs, td_ident, td_pos} main_module_index predefs funs_and_groups heaps error
	# (body_expr, arg_var, heaps, error) = build_expr_for_type_rhs type_def_mod td_rhs heaps error
	# fun_name = makeIdent ("fromGeneric-" +++ td_ident.id_name)
	| not error.ea_ok
		# (def_sym, funs_and_groups) = buildFunAndGroup fun_name [] EE No main_module_index td_pos funs_and_groups
		= (def_sym, funs_and_groups, heaps, error)
		# (def_sym, funs_and_groups) = buildFunAndGroup fun_name [arg_var] body_expr No main_module_index td_pos funs_and_groups
		= (def_sym, funs_and_groups, heaps, error)
where
	// build expression for type def rhs
	build_expr_for_type_rhs :: 
			!Index				// type def module
			!TypeRhs			// type rhs
			!*Heaps !*ErrorAdmin	
		-> 	( !Expression		// body expresssion
			, !FreeVar, !*Heaps, !*ErrorAdmin)
	build_expr_for_type_rhs type_def_mod (AlgType def_symbols) heaps error
		#! (expr, var, heaps, error) = build_sum type_def_mod def_symbols heaps error
		= (expr, var, heaps, error)
	build_expr_for_type_rhs type_def_mod (RecordType {rt_constructor}) heaps error				
		= build_record type_def_mod rt_constructor heaps error
	build_expr_for_type_rhs type_def_mod (NewType cons) heaps error
		#! (expr, var, heaps) = build_newtype_cons_app type_def_mod cons heaps
		= (expr, var, heaps, error)
	build_expr_for_type_rhs type_def_mod (AbstractType _) heaps error
		#! error = reportError td_ident.id_name td_pos "cannot build isomorphisms for an abstract type" error
		# dummy_fv = {fv_def_level=(-1), fv_count=0, fv_ident=makeIdent "dummy", fv_info_ptr=nilPtr}
		= (EE, dummy_fv, heaps, error)
	build_expr_for_type_rhs type_def_mod (SynType _) heaps error
		#! error = reportError td_ident.id_name td_pos "cannot build isomorphisms for a synonym type" error
		# dummy_fv = {fv_def_level=(-1), fv_count=0, fv_ident=makeIdent "dummy", fv_info_ptr=nilPtr}
		= (EE, dummy_fv, heaps, error)
	
	// build expression for sums
	build_sum :: !Index ![DefinedSymbol] !*Heaps !*ErrorAdmin -> (!Expression,!FreeVar/*top variable*/,!*Heaps,!*ErrorAdmin)
	build_sum type_def_mod [] heaps error
		= abort "algebraic type with no constructors!\n"
	build_sum type_def_mod [def_symbol] heaps error
		#! (cons_app_expr, cons_arg_vars, heaps) = build_cons_app type_def_mod def_symbol heaps
		#! (alt_expr, var, heaps) = if (def_symbol.ds_arity==1)
										(build_case_cons (hd cons_arg_vars) cons_app_expr predefs heaps)
										(build_case_prod False cons_app_expr cons_arg_vars predefs heaps)
		= (alt_expr, var, heaps, error)
	build_sum type_def_mod def_symbols heaps error
		#! (left_def_syms, right_def_syms) = splitAt ((length def_symbols) /2) def_symbols
		#! (left_expr, left_var, heaps, error) = build_sum type_def_mod left_def_syms heaps error
		#! (right_expr, right_var, heaps, error) = build_sum type_def_mod right_def_syms heaps error	
		#! (case_expr, var, heaps) = build_case_either left_var left_expr right_var right_expr predefs heaps
		= (case_expr, var, heaps, error)

	build_record :: !Index !DefinedSymbol !*Heaps !*ErrorAdmin -> (!Expression,!FreeVar/*top variable*/,!*Heaps,!*ErrorAdmin)
	build_record type_def_mod def_symbol heaps error
		#! (cons_app_expr, cons_arg_vars, heaps) = build_cons_app type_def_mod def_symbol heaps
		#! (alt_expr, var, heaps) = if (def_symbol.ds_arity==1)
										(build_case_record (hd cons_arg_vars) cons_app_expr predefs heaps)
										(build_case_prod False cons_app_expr cons_arg_vars predefs heaps)
		= (alt_expr, var, heaps, error)

// build expression for products
build_case_prod :: 
		!Bool							// is record
		!Expression   					// result of the case on product
		![FreeVar] 						// list of variables of the constructor pattern
		!PredefinedSymbolsData !*Heaps
	-> 	( !Expression					// generated product
		, !FreeVar						// top variable
		, !*Heaps)
build_case_prod add_case_field expr [] predefs heaps
	= build_case_unit expr predefs heaps	
build_case_prod add_case_field expr [cons_arg_var] predefs heaps
	| add_case_field
		= build_case_field cons_arg_var expr predefs heaps
		= (expr, cons_arg_var, heaps)
build_case_prod add_case_field expr cons_arg_vars predefs heaps
	#! (left_vars, right_vars) = splitAt ((length cons_arg_vars) /2) cons_arg_vars		 
	#! (expr, right_var, heaps) = build_case_prod add_case_field expr right_vars predefs heaps
	#! (expr, left_var, heaps) = build_case_prod add_case_field expr left_vars predefs heaps
	#! (case_expr, var, heaps) = build_case_pair left_var right_var expr predefs heaps
	= (case_expr, var, heaps) 

// build constructor application expression
build_cons_app :: !Index !DefinedSymbol !*Heaps -> (!Expression, ![FreeVar], !*Heaps)
build_cons_app cons_mod def_symbol=:{ds_arity} heaps
	#! names = ["x"  +++ toString k \\ k <- [1..ds_arity]]
	#! (var_exprs, vars, heaps) = buildVarExprs names heaps 
	#! (expr, heaps) = buildConsApp cons_mod def_symbol var_exprs heaps
 	= (expr, vars, heaps)

build_newtype_cons_app :: !Index !DefinedSymbol !*Heaps -> (!Expression, !FreeVar, !*Heaps)
build_newtype_cons_app cons_mod def_symbol heaps
	#! (var_expr, var, heaps) = buildVarExpr "x11" heaps 
	#! (expr, heaps) = buildNewTypeConsApp cons_mod def_symbol var_expr heaps
 	= (expr, var, heaps)

build_case_unit body_expr predefs=:{psd_predefs_a} heaps
	# unit_pat = buildPredefConsPattern PD_ConsUNIT [] body_expr predefs
	# {pds_module, pds_def} = psd_predefs_a.[PD_TypeUNIT]
	# case_patterns = AlgebraicPatterns {gi_module = pds_module, gi_index = pds_def} [unit_pat]
	= build_case_expr case_patterns heaps

build_pair x y predefs heaps
	= buildPredefConsApp PD_ConsPAIR [x, y] predefs heaps

build_left x predefs heaps
	= buildPredefConsApp PD_ConsLEFT [x] predefs heaps

build_right x predefs heaps
	= buildPredefConsApp PD_ConsRIGHT [x] predefs heaps

build_object expr predefs heaps
	| predefs.psd_generic_newtypes bitand OBJECT_NewType_Mask<>0
		= buildPredefNewTypeConsApp PD_ConsOBJECT [expr] predefs heaps
		= buildPredefConsApp        PD_ConsOBJECT [expr] predefs heaps

build_cons expr predefs heaps
	| predefs.psd_generic_newtypes bitand CONS_NewType_Mask<>0
		= buildPredefNewTypeConsApp PD_ConsCONS [expr] predefs heaps
		= buildPredefConsApp        PD_ConsCONS [expr] predefs heaps

build_record expr predefs heaps
	| predefs.psd_generic_newtypes bitand RECORD_NewType_Mask<>0
		= buildPredefNewTypeConsApp PD_ConsRECORD [expr] predefs heaps
		= buildPredefConsApp        PD_ConsRECORD [expr] predefs heaps

build_field var_expr predefs heaps
	| predefs.psd_generic_newtypes bitand FIELD_NewType_Mask<>0
		= buildPredefNewTypeConsApp PD_ConsFIELD [var_expr] predefs heaps 
		= buildPredefConsApp        PD_ConsFIELD [var_expr] predefs heaps 

build_case_pair var1 var2 body_expr predefs=:{psd_predefs_a} heaps
	# pair_pat = buildPredefConsPattern PD_ConsPAIR [var1, var2] body_expr predefs
	# {pds_module, pds_def} = psd_predefs_a.[PD_TypePAIR]
	# case_patterns = AlgebraicPatterns {gi_module = pds_module, gi_index = pds_def} [pair_pat]
	= build_case_expr case_patterns heaps

build_case_either left_var left_expr right_var right_expr predefs=:{psd_predefs_a} heaps
	# left_pat = buildPredefConsPattern PD_ConsLEFT [left_var] left_expr predefs
	# right_pat = buildPredefConsPattern PD_ConsRIGHT [right_var] right_expr predefs
	# {pds_module, pds_def} = psd_predefs_a.[PD_TypeEITHER]
	# case_patterns = AlgebraicPatterns {gi_module = pds_module, gi_index = pds_def} [left_pat, right_pat]
	= build_case_expr case_patterns heaps

build_case_object var body_expr predefs=:{psd_predefs_a,psd_generic_newtypes} heaps
	# pat = buildPredefConsPattern PD_ConsOBJECT [var] body_expr predefs
	# {pds_module, pds_def} = psd_predefs_a.[PD_TypeOBJECT]
	| psd_generic_newtypes bitand OBJECT_NewType_Mask<>0
		# case_patterns = NewTypePatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps
		# case_patterns = AlgebraicPatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps

build_case_cons var body_expr predefs=:{psd_predefs_a,psd_generic_newtypes} heaps
	# pat = buildPredefConsPattern PD_ConsCONS [var] body_expr predefs	
	# {pds_module, pds_def} = psd_predefs_a.[PD_TypeCONS]
	| psd_generic_newtypes bitand CONS_NewType_Mask<>0
		# case_patterns = NewTypePatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps 
		# case_patterns = AlgebraicPatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps 

build_case_record var body_expr predefs=:{psd_predefs_a,psd_generic_newtypes} heaps
	# pat = buildPredefConsPattern PD_ConsRECORD [var] body_expr predefs
	# {pds_module, pds_def} = psd_predefs_a.[PD_TypeRECORD]
	| psd_generic_newtypes bitand RECORD_NewType_Mask<>0
		# case_patterns = NewTypePatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps 
		# case_patterns = AlgebraicPatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps 

build_case_field var body_expr predefs=:{psd_predefs_a,psd_generic_newtypes} heaps
	# pat = buildPredefConsPattern PD_ConsFIELD [var] body_expr predefs
	# {pds_module, pds_def} = psd_predefs_a.[PD_TypeFIELD]
	| psd_generic_newtypes bitand FIELD_NewType_Mask<>0
		# case_patterns = NewTypePatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps 
		# case_patterns = AlgebraicPatterns {gi_module = pds_module, gi_index = pds_def} [pat]
		= build_case_expr case_patterns heaps 

// case with a variable as the selector expression
build_case_expr case_patterns heaps
	# (var_expr, var, heaps) = buildVarExpr "c" heaps
	# (case_expr, heaps) = buildCaseExpr var_expr case_patterns heaps
	= (case_expr, var, heaps)

// build kind indexed classes 

buildClasses :: !*GenericState -> *GenericState
buildClasses gs=:{gs_main_module}
	#! ({com_class_defs,com_member_defs},gs) = gs!gs_modules.[gs_main_module]
	#! num_classes = size com_class_defs
	#! num_members = size com_member_defs

	#! ((classes, members, new_num_classes, new_num_members), gs)
		= build_modules 0 ([], [], num_classes, num_members) gs

	# first_new_class_index = size com_class_defs

	// obtain common definitions again because com_gencase_defs are updated 
	#! (common_defs,gs) = gs!gs_modules.[gs_main_module]
	# common_defs = {common_defs & com_class_defs = arrayPlusRevList com_class_defs classes
								 , com_member_defs = arrayPlusRevList com_member_defs members}

	#! (common_defs, gs)
		= build_class_dictionaries first_new_class_index common_defs gs
	
	= {gs & gs_modules.[gs_main_module] = common_defs}  
where
	build_modules :: !Index (![ClassDef], ![MemberDef], !Int, !Int) !*GenericState
		-> ((![ClassDef], ![MemberDef], !Int, !Int), !*GenericState)
	build_modules module_index st gs=:{gs_modules,gs_used_modules}
		| module_index == size gs_modules
			= (st, gs)
		| not (inNumberSet module_index gs_used_modules)
			= build_modules (inc module_index) st gs		
			#! ({com_gencase_defs},gs_modules) = gs_modules![module_index] 
			#! (com_gencase_defs, st, gs) 
				= build_module module_index 0 {x\\x<-:com_gencase_defs} st {gs & gs_modules=gs_modules}
			#! gs = {gs & gs_modules.[module_index].com_gencase_defs = com_gencase_defs}
			= build_modules (inc module_index) st gs

	build_module module_index index com_gencase_defs st gs
		| index == size com_gencase_defs
			= (com_gencase_defs, st, gs)
			#! (gencase, com_gencase_defs) = com_gencase_defs ! [index]
			#! (gencase, st, gs) = on_gencase module_index index gencase st gs
			#! com_gencase_defs = {com_gencase_defs & [index] = gencase} 	
			= build_module module_index (inc index) com_gencase_defs st gs

	on_gencase :: !Index !Index
			!GenericCaseDef (![ClassDef], ![MemberDef], !Index, Index) !*GenericState
		-> (!GenericCaseDef,(![ClassDef], ![MemberDef], !Index, Index),!*GenericState)
	on_gencase module_index index
				gencase=:{gc_gcf=GCF gc_ident gcf=:{gcf_generic,gcf_generic_info,gcf_generic_instance_deps}, gc_type_cons, gc_type, gc_pos}
				st gs=:{gs_modules, gs_td_infos, gs_error}
		#! (gen_def=:{gen_deps}, gs_modules) = gs_modules![gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index]

		#! (kind, gs_td_infos) = get_kind_of_type_cons gc_type_cons gs_td_infos

		# (gcf_generic_instance_deps,gs_error)
			= case gcf_generic_instance_deps of
				GenericInstanceDependencies n_deps deps
					# n_generic_function_arguments = number_of_generic_function_arguments kind gen_deps
					| n_deps == n_generic_function_arguments
						-> (gcf_generic_instance_deps,gs_error)
						# gs_error = reportError gc_ident.id_name gc_pos "incorrect number of dependent generic functions in definition module" gs.gs_error
						| n_deps > n_generic_function_arguments
							# deps = deps bitand ((1<<n_generic_function_arguments)-1)
							-> (GenericInstanceDependencies n_generic_function_arguments deps,gs_error)
							# deps = deps bitor ((-1)<<n_deps)
							# deps = deps bitand ((1<<n_generic_function_arguments)-1)
							-> (GenericInstanceDependencies n_generic_function_arguments deps,gs_error)						
				GenericInstanceUsedArgs n_deps deps
					# n_generic_function_arguments = number_of_generic_function_arguments kind gen_deps
					| n_deps == n_generic_function_arguments
						-> (GenericInstanceDependencies n_deps deps,gs_error)
					| n_deps > n_generic_function_arguments
						# deps = deps bitand ((1<<n_generic_function_arguments)-1)
						-> (GenericInstanceDependencies n_generic_function_arguments deps,gs_error)
						# deps = deps bitor ((-1)<<n_deps)
						# deps = deps bitand ((1<<n_generic_function_arguments)-1)
						-> (GenericInstanceDependencies n_generic_function_arguments deps,gs_error)						
				_
					-> (gcf_generic_instance_deps,gs_error)

		#! type_index = index_gen_cons_with_info_type gencase.gc_type gs.gs_predefs

		// To generate all partially applied shorthand instances we need
		// classes for all partial applications of the gc_kind and for
		// all the argument kinds.
		// Additionally, we always need classes for base cases *, *->* and *->*->* 

		#! gs = {gs & gs_modules = gs_modules, gs_td_infos = gs_td_infos, gs_error = gs_error}
		#! subkinds = determine_subkinds kind 		
		#! kinds =
			[ KindConst
			, KindArrow [KindConst]
			, KindArrow [KindConst, KindConst] 
			: subkinds]
		# (dep_defs, gs_modules) = mapSt lookupDependencyDef gen_deps gs.gs_modules
		# gs = {gs & gs_modules = gs_modules}
		#! (st, gs) = foldSt (\def -> foldSt (build_class_if_needed def) kinds) [gen_def:dep_defs] (st, gs)
		#! gencase = { gencase & gc_gcf = GCF gc_ident {gcf & gcf_kind = kind, gcf_generic_instance_deps = gcf_generic_instance_deps}}
		| type_index>=0
			# (GCF _ {gcf_body = fun_index}) = gencase.gc_gcf
			  gen_info_ptr = gen_def.gen_info_ptr

			  fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons

			  (optional_fun_type,gs)
			  	= case gcf_generic_instance_deps of
					GenericInstanceDependencies n_deps deps
						# (class_var, gs_tvarh) = freshTypeVar (makeIdent "class_var") gs.gs_tvarh
						  gs & gs_tvarh=gs_tvarh
						  unused_class = TCClass {glob_module = -1, glob_object = {ds_index = -1, ds_ident = {id_name="",id_info=nilPtr}, ds_arity = 1}}
						  (member_type, gs) = buildMemberTypeWithPartialDependencies gen_def kind class_var unused_class deps gs

						  ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
				  
						  type_heaps = {th_vars = gs.gs_tvarh, th_attrs = gs.gs_avarh}
						  (fun_type, {th_vars,th_attrs}, var_heap, error)
						  	= determine_type_of_member_instance_from_symbol_type member_type ins_type type_heaps gs.gs_varh gs.gs_error
						  gs & gs_tvarh=th_vars, gs_avarh=th_attrs, gs_varh=var_heap, gs_error=error

						-> (Yes fun_type,gs)
					_
						-> (No,gs)

			  gen_rep_cons = {grc_module=module_index, grc_index=fun_index, grc_local_fun_index = -1, grc_ident=fun_ident,
			  			 	  grc_generic_info=gcf_generic_info, grc_generic_instance_deps=gcf_generic_instance_deps,
			  				  grc_optional_fun_type=optional_fun_type}

			  (gen_info,generic_heap) = readPtr gen_info_ptr gs.gs_genh
			  gen_rep_conses = {gi\\gi<-:gen_info.gen_rep_conses}
			  gen_rep_conses = {gen_rep_conses & [type_index]=gen_rep_cons}
			  gen_info = {gen_info & gen_rep_conses=gen_rep_conses}
			  generic_heap = writePtr gen_info_ptr gen_info generic_heap
			  gs = {gs & gs_genh=generic_heap}
			= (gencase, st, gs)
			= (gencase, st, gs)
	on_gencase module_index index
			gencase=:{gc_gcf=GCFS gcfs,gc_type_cons} st gs=:{gs_td_infos}
		# (kind, gs_td_infos) = get_kind_of_type_cons gc_type_cons gs_td_infos
		#! gs = {gs & gs_td_infos = gs_td_infos}
		# subkinds = determine_subkinds kind 		
		# kinds = 
			[ KindConst
			, KindArrow [KindConst]
			, KindArrow [KindConst, KindConst] 
			: subkinds]
		# (gcfs,st,gs) = build_classes_for_generic_superclasses_if_needed gcfs kind kinds st gs
		#! gencase = {gencase & gc_gcf = GCFS gcfs}
		= (gencase, st, gs)
	where
		build_classes_for_generic_superclasses_if_needed [!gcf=:{gcf_generic}:gcfs!] kind kinds st gs
			#! (gen_def,gs) = gs!gs_modules.[gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index]
			# (st, gs) = build_classes_if_needed gen_def kinds st gs
			# gcf={gcf & gcf_kind = kind}
			# (gcfs,st,gs) = build_classes_for_generic_superclasses_if_needed gcfs kind kinds st gs
			= ([!gcf:gcfs!],st,gs)
		build_classes_for_generic_superclasses_if_needed [!!] kind kinds st gs
			= ([!!],st,gs)

	number_of_generic_function_arguments (KindArrow kinds) gen_deps
		= length kinds * (1 + length gen_deps)
	number_of_generic_function_arguments gcf_kind gen_deps
		= 0

	build_classes_if_needed gen_def kinds st gs
		= foldSt (build_class_if_needed gen_def) kinds (st, gs)

	build_class_if_needed :: !GenericDef !TypeKind ((![ClassDef], ![MemberDef], !Index, Index), *GenericState)
												-> ((![ClassDef], ![MemberDef], !Index, Index), *GenericState)		
	build_class_if_needed gen_def kind ((classes, members, class_index, member_index), gs=:{gs_main_module, gs_genh})
		#! (opt_class_info, gs_genh) = lookup_generic_class_info gen_def kind gs_genh
		#! gs = {gs & gs_genh = gs_genh}
		= case opt_class_info of
			No
				#! (class_def, member_def, gs=:{gs_genh}) 
					= buildClassAndMember gs_main_module class_index member_index kind gen_def gs 
				#! class_info = 
					{	gci_kind = kind
					,	gci_module = gs_main_module
					,	gci_class = class_index
					,	gci_member = member_index
					}
				#! gs_genh = add_generic_class_info gen_def class_info gs_genh
				#! gs = { gs & gs_genh = gs_genh }
				-> (([class_def:classes], [member_def:members], inc class_index, inc member_index), gs)
			Yes class_info	
				-> ((classes, members, class_index, member_index), gs)
	
	determine_subkinds KindConst 
		= [KindConst]
	determine_subkinds (KindArrow kinds) 
		= do_it kinds
	where
		do_it [] = [KindConst]
		do_it all_ks=:[k:ks] 
			#! this_kind = KindArrow all_ks
			#! left_subkinds = determine_subkinds k
			#! right_subkinds = do_it ks
			= [this_kind : left_subkinds ++ right_subkinds] 
				
	get_kind_of_type_cons :: !TypeCons !*TypeDefInfos -> (!TypeKind, !*TypeDefInfos)
	get_kind_of_type_cons (TypeConsBasic _) td_infos 
		= (KindConst, td_infos)
	get_kind_of_type_cons TypeConsArrow td_infos
		= (KindArrow [KindConst,KindConst], td_infos)
	get_kind_of_type_cons (TypeConsSymb {type_ident, type_index}) td_infos
		#! ({tdi_kinds}, td_infos) = td_infos ! [type_index.glob_module,type_index.glob_object]
		= (if (isEmpty tdi_kinds) KindConst (KindArrow tdi_kinds), td_infos)
	get_kind_of_type_cons (TypeConsVar tv) td_infos
		= (KindConst, td_infos)

	lookup_generic_class_info {gen_info_ptr} kind hp_generic_heap
		#! ({gen_classes}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
		= (lookupGenericClassInfo kind gen_classes, hp_generic_heap)

	add_generic_class_info {gen_info_ptr} class_info gs_genh	
		#! (gen_info=:{gen_classes}, gs_genh) = readPtr gen_info_ptr gs_genh
		#! gen_classes = addGenericClassInfo class_info gen_classes
		= writePtr gen_info_ptr {gen_info & gen_classes=gen_classes} gs_genh

	build_class_dictionaries :: !Int !CommonDefs !*GenericState -> (!CommonDefs, !*GenericState) 		
	build_class_dictionaries first_new_class_index common_defs  
			gs=:{gs_varh, gs_tvarh, gs_main_module, gs_symtab, gs_dcl_modules}
		#! class_defs = { x \\ x <-: common_defs.com_class_defs } // make unique copy
		#  type_defs = { x \\ x <-: common_defs.com_type_defs } // make unique copy
		#  cons_defs = { x \\ x <-: common_defs.com_cons_defs } // make unique copy
		#  selector_defs = { x \\ x <-: common_defs.com_selector_defs } // make unique copy
		#  (size_type_defs,type_defs) = usize type_defs 
		#! (new_type_defs, new_selector_defs, new_cons_defs,type_defs,selector_defs,cons_defs,class_defs, gs_dcl_modules, gs_tvarh, gs_varh, gs_symtab)
			= createMoreClassDictionaries
					first_new_class_index
					gs_main_module 
					size_type_defs
					(size common_defs.com_selector_defs) 
					(size common_defs.com_cons_defs) 
					type_defs selector_defs cons_defs class_defs 
					gs_dcl_modules gs_tvarh gs_varh gs_symtab

		#! common_defs = { common_defs & 
			com_class_defs = class_defs, 
			com_type_defs = arrayPlusList type_defs new_type_defs,
			com_selector_defs = arrayPlusList selector_defs new_selector_defs,
			com_cons_defs = arrayPlusList cons_defs new_cons_defs}

		# gs = { gs & gs_tvarh = gs_tvarh
					, gs_varh = gs_varh
					, gs_dcl_modules = gs_dcl_modules
					, gs_symtab = gs_symtab }
		= (common_defs, gs)

instance_vars_from_type_cons (TypeConsVar tv)
	= [tv]
instance_vars_from_type_cons _
	= []

lookupDependencyDef :: GenericDependency !*Modules -> (GenericDef, *Modules)
lookupDependencyDef {gd_index} modules = modules![gd_index.gi_module].com_generic_defs.[gd_index.gi_index]	

// limitations:
// - context restrictions on generic variables are not allowed
buildMemberType :: !GenericDef !TypeKind !TypeVar !TCClass !*GenericState -> (!SymbolType, !*GenericState)
buildMemberType gen_def=:{gen_ident,gen_pos,gen_type,gen_vars,gen_deps} kind class_var tc_class gs=:{gs_varh}
	# (tc_var_ptr, gs_varh) = newPtr VI_Empty gs_varh
	# gs & gs_varh = gs_varh
	#! type_context = {tc_class = tc_class, tc_types = [TV class_var], tc_var = tc_var_ptr}

	#! (gen_type, gs) = add_bimap_contexts gen_def gs 

	#! th = {th_vars = gs.gs_tvarh, th_attrs = gs.gs_avarh}
	#! (kind_indexed_st, gatvs, th, modules, error)
		= buildKindIndexedType gen_type gen_vars gen_deps kind gen_ident gen_pos th gs.gs_modules gs.gs_error

	#! (member_st, th) 
		= replace_generic_vars_with_class_var kind_indexed_st gatvs class_var th

	#! th = assertSymbolType member_st th // just paranoied about cleared variables
	#! th = assertSymbolType gen_type th

	# member_st & st_context = [type_context : member_st.st_context]	
	
	# gs = {gs & gs_avarh = th.th_attrs, gs_tvarh = th.th_vars, gs_modules = modules, gs_error = error }
	= (member_st, gs)

buildMemberTypeWithPartialDependencies :: !GenericDef !TypeKind !TypeVar !TCClass !Int !*GenericState -> (!SymbolType, !*GenericState)
buildMemberTypeWithPartialDependencies gen_def=:{gen_ident,gen_pos,gen_type,gen_vars,gen_deps} kind class_var unused_class deps gs=:{gs_varh}
	# (tc_var_ptr, gs_varh) = newPtr VI_Empty gs_varh
	# gs & gs_varh = gs_varh
	#! type_context = {tc_class = unused_class, tc_types = [TV class_var], tc_var = tc_var_ptr}

	#! (gen_type, gs) = add_bimap_contexts gen_def gs 

	#! th = {th_vars = gs.gs_tvarh, th_attrs = gs.gs_avarh}
	#! (kind_indexed_st, gatvs, th, modules, error)
		= buildKindIndexedTypeWithPartialDependencies gen_type gen_vars gen_deps kind deps gen_ident gen_pos th gs.gs_modules gs.gs_error

	#! (member_st, th) 
		= replace_generic_vars_with_class_var kind_indexed_st gatvs class_var th

	#! th = assertSymbolType member_st th // just paranoied about cleared variables
	#! th = assertSymbolType gen_type th

	# member_st & st_context = [type_context : member_st.st_context]	
	
	# gs = {gs & gs_avarh = th.th_attrs, gs_tvarh = th.th_vars, gs_modules = modules, gs_error = error }
	= (member_st, gs)

add_bimap_contexts :: GenericDef *GenericState -> (!SymbolType,!*GenericState)
add_bimap_contexts 
		{gen_type=gen_type=:{st_vars, st_context}, gen_vars, gen_info_ptr} 
		gs=:{gs_predefs, gs_varh, gs_genh}
	#! ({gen_var_kinds}, gs_genh) = readPtr gen_info_ptr gs_genh	
	#! num_gen_vars = length gen_vars
	#! tvs = st_vars -- gen_vars
	#! kinds = drop num_gen_vars gen_var_kinds
	#! (bimap_contexts, gs_varh) = build_contexts tvs kinds gs_varh 
	
	#! gs = {gs & gs_varh = gs_varh, gs_genh = gs_genh}
	= ({gen_type & st_context = st_context ++ bimap_contexts}, gs)
where
	build_contexts [] [] st 
		= ([], st)
	build_contexts [x:xs] [KindConst:kinds] st
		= build_contexts xs kinds st
	build_contexts [x:xs] [kind:kinds] st
		# (z, st) = build_context x kind st
		# (zs, st) = build_contexts xs kinds st
		= ([z:zs], st) 

	build_context tv kind gs_varh
		#! (var_info_ptr, gs_varh) = newPtr VI_Empty gs_varh
		#! {pds_module, pds_def} = gs_predefs.psd_predefs_a.[PD_GenericBimap]
		#! pds_ident = predefined_idents.[PD_GenericBimap]
		# glob_def_sym = 
			{ glob_module = pds_module
			, glob_object = {ds_ident=pds_ident, ds_index=pds_def, ds_arity = 1}
			}
		# tc_class = TCGeneric 
			{ gtc_generic=glob_def_sym
			, gtc_kind = kind
			, gtc_class = {glob_module=NoIndex,glob_object={ds_ident=makeIdent "<no generic class>", ds_index=NoIndex, ds_arity=1}}
			, gtc_generic_dict = {gi_module=NoIndex, gi_index=NoIndex}
			}
		=({tc_class = tc_class, tc_types = [TV tv], tc_var = var_info_ptr}, gs_varh)	

replace_generic_vars_with_class_var :: SymbolType [ATypeVar] TypeVar *TypeHeaps -> (!SymbolType,!*TypeHeaps)
replace_generic_vars_with_class_var st atvs class_var th
	#! th = subst_gvs atvs th
	= applySubstInSymbolType st th
where
	subst_gvs atvs th=:{th_vars, th_attrs}
		#! tvs = [atv_variable \\ {atv_variable} <- atvs ]
		#! avs = [av \\ {atv_attribute=TA_Var av} <- atvs ]
		
		# th_vars = foldSt subst_tv tvs th_vars

		// all generic vars get the same uniqueness variable
		# th_attrs = case avs of 
			[av:avs]	-> foldSt (subst_av av) avs th_attrs
			[] 			-> th_attrs

		= { th & th_vars = th_vars, th_attrs = th_attrs }
	
	subst_tv {tv_info_ptr} th_vars
		= writePtr tv_info_ptr (TVI_Type (TV class_var)) th_vars

	subst_av av {av_info_ptr} th_attrs
		= writePtr av_info_ptr (AVI_Attr (TA_Var av)) th_attrs

buildClassAndMember :: Int Int Int TypeKind GenericDef *GenericState -> (ClassDef,MemberDef,*GenericState)
buildClassAndMember
		module_index class_index member_index kind
		gen_def=:{gen_ident, gen_pos}
		gs=:{gs_tvarh}
	# (class_var, gs_tvarh) = freshTypeVar (makeIdent "class_var") gs_tvarh
	#! (member_def, gs)
		= build_class_member class_var {gs & gs_tvarh = gs_tvarh}	
	#! class_def = build_class class_var member_def
	= (class_def, member_def, gs)
where
	class_ident = genericIdentToClassIdent gen_def.gen_ident.id_name kind
	member_ident = genericIdentToMemberIdent gen_def.gen_ident.id_name kind
	class_ds = {ds_index = class_index, ds_ident = class_ident, ds_arity = 1}

	build_class_member class_var gs
		#! (member_type, gs) 
			= buildMemberType gen_def kind class_var (TCClass {glob_module = module_index, glob_object=class_ds}) gs
		#! (type_ptr, gs_varh) = newPtr VI_Empty gs.gs_varh 
		#! gs & gs_varh = gs_varh
		#! member_def = {
			me_ident = member_ident, 
			me_class = {glob_module = module_index, glob_object = class_index},
			me_offset = 0,
			me_type = member_type,
			me_type_ptr = type_ptr,				// empty
			me_class_vars = [class_var], 		// the same variable as in the class
			me_pos = gen_pos,
			me_priority = NoPrio,
			me_default_implementation = No
			}
		= (member_def, gs)

	build_class class_var member_def=:{me_type}
		#! class_member = 
			{ ds_ident = member_ident
			, ds_index = member_index
			, ds_arity = me_type.st_arity
			}
		#! class_dictionary = 
			{ ds_ident = class_ident 
			, ds_arity = 0 
			, ds_index = NoIndex/*index in the type def table, filled in later*/ 
			}
		#! class_def = { 
			class_ident = class_ident, 
			class_arity = 1,  
			class_args = [class_var],
		    class_context = [], 
		    class_pos = gen_pos, 
		    class_members = createArray 1 class_member, 
		    class_macro_members = {},
		    class_cons_vars = 0, // dotted class variables
		    class_dictionary = class_dictionary
		    }
		= class_def

// Convert generic cases

:: *SpecializeState = {
		ss_modules :: !*Modules,
		ss_td_infos :: !*TypeDefInfos,
		ss_funs_and_groups :: !FunsAndGroups,
		ss_heaps :: !*Heaps,
		ss_dcl_macros :: !*DclMacros,
		ss_funs :: !*{#FunDef},
		ss_symbol_table :: !*SymbolTable,
		ss_error :: !*ErrorAdmin
	}

convertGenericCases :: !BimapFunctions !*DclMacros !*GenericState -> (!*DclMacros, !*GenericState)
convertGenericCases bimap_functions dcl_macros
		gs=:{gs_main_module, gs_used_modules, gs_predefs, gs_funs, gs_groups, gs_modules, gs_dcl_modules, gs_td_infos, 
			 gs_avarh, gs_tvarh, gs_varh, gs_genh, gs_exprh, gs_symtab, gs_error}

	# heaps = 
		{ hp_expression_heap = gs_exprh
		, hp_var_heap = gs_varh
		, hp_generic_heap = gs_genh
		, hp_type_heaps = { th_vars = gs_tvarh, th_attrs = gs_avarh }
		}	

	#! (first_fun_index, gs_funs) = usize gs_funs
	#! first_group_index = size gs_groups
	#! fun_info = {fg_fun_index=first_fun_index, fg_group_index=first_group_index, fg_funs=[], fg_groups=[], fg_bimap_functions=bimap_functions}

	#! (main_common_defs, gs_modules) = gs_modules ! [gs_main_module] 	
	#! main_module_instances = main_common_defs.com_instance_defs

	#! first_instance_index = size main_module_instances	
	#! instance_info = (first_instance_index, [])

	#! (gs_modules, gs_dcl_modules, (instance_info, heaps, gs_error)) 
		= build_exported_main_instances_in_modules 0 gs_modules gs_dcl_modules (instance_info, heaps, gs_error)

	# st2 = {ss_modules=gs_modules,ss_td_infos=gs_td_infos,ss_funs_and_groups=fun_info,ss_heaps=heaps,ss_dcl_macros=dcl_macros,ss_funs=gs_funs,
			 ss_symbol_table=gs_symtab,ss_error=gs_error}
	#! (gs_dcl_modules, instance_info, st2) 
		= build_main_instances_in_main_module gs_main_module gs_dcl_modules instance_info st2
	# {ss_modules=gs_modules,ss_td_infos=gs_td_infos,ss_funs_and_groups=fun_info,ss_heaps=heaps,ss_dcl_macros=dcl_macros,ss_funs=gs_funs,
	   ss_symbol_table=gs_symtab,ss_error=gs_error} = st2

	#! (gs_modules, gs_dcl_modules, (fun_info, instance_info, heaps, gs_error)) 
		= build_shorthand_instances_in_modules 0 gs_modules gs_dcl_modules (fun_info, instance_info, heaps, gs_error)
	
	#! {fg_fun_index, fg_funs=new_funs, fg_groups=new_groups} = fun_info
	#! gs_funs = arrayPlusRevList gs_funs new_funs
	#! gs_groups = arrayPlusRevList gs_groups new_groups

	#! (instance_index, new_instances) = instance_info
	#! com_instance_defs = arrayPlusRevList main_module_instances new_instances

	#! main_common_defs = {main_common_defs & com_instance_defs = com_instance_defs}	
	#! gs_modules = {gs_modules & [gs_main_module] = main_common_defs}
	
	# {hp_expression_heap, hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}} = heaps
	# gs & gs_modules = gs_modules, gs_dcl_modules = gs_dcl_modules, gs_td_infos = gs_td_infos, gs_funs = gs_funs, gs_groups = gs_groups,
		   gs_avarh = th_attrs, gs_tvarh = th_vars, gs_varh = hp_var_heap, gs_genh = hp_generic_heap, gs_exprh = hp_expression_heap,
		   gs_error = gs_error, gs_symtab = gs_symtab
	= (dcl_macros, gs)
where
	build_exported_main_instances_in_modules :: !Index
			!*{#CommonDefs} !*{#DclModule} !(!(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin)
		-> (!*{#CommonDefs},!*{#DclModule},!(!(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
	build_exported_main_instances_in_modules module_index modules dcl_modules st
		| module_index == size modules
			= (modules, dcl_modules, st)
		| not (inNumberSet module_index gs_used_modules) || module_index==gs_main_module
			= build_exported_main_instances_in_modules (module_index+1) modules dcl_modules st
			#! (com_gencase_defs,modules) = modules![module_index].com_gencase_defs
			| size com_gencase_defs==0
				= build_exported_main_instances_in_modules (module_index+1) modules dcl_modules st
			#! (dcl_functions,dcl_modules) = dcl_modules![module_index].dcl_functions
			#! (dcl_functions, modules, st)
				= build_exported_main_instances_in_module module_index com_gencase_defs {x\\x<-:dcl_functions} modules st  
			#! dcl_modules = {dcl_modules & [module_index].dcl_functions = dcl_functions} 
			= build_exported_main_instances_in_modules (module_index+1) modules dcl_modules st
	where
		build_exported_main_instances_in_module module_index com_gencase_defs dcl_functions modules st
			= foldArraySt (build_exported_main_instance module_index) com_gencase_defs (dcl_functions, modules, st)

		build_exported_main_instance :: !Index !GenericCaseDef
				(!*{#FunType} ,!*Modules, !(!(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
			->	(!*{#FunType} ,!*Modules, !(!(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))					
		build_exported_main_instance module_index
				{gc_gcf=GCF gc_ident {gcf_body,gcf_kind,gcf_generic,gcf_generic_info}, gc_type, gc_type_cons,gc_pos} 
				(dcl_functions, modules, st)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! generic_info_index = index_gen_cons_with_info_type gc_type gs_predefs
			# fun_index
				= case gcf_body of
					GCB_FunIndex fun_index
						-> fun_index
					GCB_FunAndMacroIndex fun_index macro_index
						-> fun_index
			= build_exported_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos generic_info_index gcf_generic_info
									dcl_functions modules st
		build_exported_main_instance module_index
				{gc_gcf=GCFS gcfs,gc_type,gc_type_cons,gc_pos}
				(dcl_functions, modules, st)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! generic_info_index = index_gen_cons_with_info_type gc_type gs_predefs
			= build_exported_main_instances gcfs ins_type module_index gc_type_cons gc_pos generic_info_index
											dcl_functions modules st
		where
			build_exported_main_instances [!{gcf_body = GCB_FunIndex fun_index,gcf_generic,gcf_generic_info,gcf_kind,gcf_gident}:gcfs!] ins_type module_index gc_type_cons gc_pos generic_info_index
									dcl_functions modules st
				# (dcl_functions, modules, st)
					= build_exported_main_instance_ ins_type module_index gcf_gident fun_index gcf_kind gcf_generic gc_type_cons gc_pos generic_info_index gcf_generic_info
									dcl_functions modules st
				= build_exported_main_instances gcfs ins_type module_index gc_type_cons gc_pos generic_info_index
									dcl_functions modules st
			build_exported_main_instances [!!] ins_type module_index gc_type_cons gc_pos generic_info_index
									dcl_functions modules st
				= (dcl_functions, modules, st)

		build_exported_main_instance_ :: InstanceType Int Ident Int TypeKind GlobalIndex TypeCons Position Int Int
				!*{#FunType} !*{#CommonDefs} !(!(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin)
			-> (!*{#FunType},!*{#CommonDefs},!(!(!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
		build_exported_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos generic_info_index generic_info
								dcl_functions modules (ins_info, heaps, error)
			# (gen_info_ptr, modules) = modules![gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index].gen_info_ptr
			  ({gen_classes,gen_rep_conses}, hp_generic_heap) = readPtr gen_info_ptr heaps.hp_generic_heap
			  heaps & hp_generic_heap=hp_generic_heap
			  (Yes class_info) = lookupGenericClassInfo gcf_kind gen_classes

			#! fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons

			| generic_info_index<0
				#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_info.gci_member]
				#! (fun_type, heaps, error)
					= determine_type_of_member_instance member_def ins_type heaps error
				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type dcl_functions heaps
				# class_instance_member = {cim_ident=fun_ident,cim_arity=module_index,cim_index= -1-fun_index}
				#! ins_info = build_class_instance class_info.gci_class gc_ident gc_pos gcf_kind class_instance_member ins_type ins_info
				= (dcl_functions, modules, (ins_info, heaps, error))

				# (fun_type,modules,heaps,error)
					= case gen_rep_conses.[generic_info_index].grc_optional_fun_type of
						Yes fun_type
							-> (fun_type,modules,heaps,error)
						No
							#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_info.gci_member]
							# (fun_type, heaps, error)
								= determine_type_of_member_instance member_def ins_type heaps error
							-> (fun_type,modules,heaps,error)
				# fun_type_with_generic_info
					= if (generic_info<>0)
						(add_generic_info_to_type fun_type generic_info_index generic_info gs_predefs)
						fun_type	
				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type_with_generic_info dcl_functions heaps
				= (dcl_functions, modules, (ins_info, heaps, error))

	build_main_instances_in_main_module :: !Index
			!*{#DclModule} !(!Index, ![ClassInstance])  !*SpecializeState
		-> (!*{#DclModule},!(!Index, ![ClassInstance]), !*SpecializeState)
	build_main_instances_in_main_module gs_main_module dcl_modules st1 st2
		#! (com_gencase_defs,st2) = st2!ss_modules.[gs_main_module].com_gencase_defs
		| size com_gencase_defs==0
			= (dcl_modules,st1,st2)
		#! (dcl_functions,dcl_modules) = dcl_modules![gs_main_module].dcl_functions
		#! (dcl_functions, st1, st2)
			= foldArraySt (build_main_instance gs_main_module) com_gencase_defs ({x\\x<-:dcl_functions}, st1, st2)
		#! dcl_modules = {dcl_modules & [gs_main_module].dcl_functions = dcl_functions}
		= (dcl_modules,st1,st2)
	where
		build_main_instance :: !Index !GenericCaseDef
				(!*{#FunType}, !(!Index, ![ClassInstance]), !*SpecializeState)
			->	(!*{#FunType}, !(!Index, ![ClassInstance]), !*SpecializeState)					
		build_main_instance module_index
				{gc_gcf=GCF gc_ident {gcf_body = GCB_FunIndex fun_index,gcf_kind,gcf_generic,gcf_generic_info}, gc_type, gc_type_cons,gc_pos} 
				(dcl_functions, st1, st2)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! generic_info_index = index_gen_cons_with_info_type gc_type gs_predefs
			= build_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos generic_info_index gcf_generic_info
									dcl_functions st1 st2
		build_main_instance module_index
				{gc_gcf=GCFS gcfs,gc_type,gc_type_cons,gc_pos}
				(dcl_functions, st1, st2)
			#! ins_type = {it_vars = instance_vars_from_type_cons gc_type_cons, it_types = [gc_type], it_attr_vars = [], it_context = []}
			#! generic_info_index = index_gen_cons_with_info_type gc_type gs_predefs
			= build_main_instances gcfs ins_type module_index gc_type_cons gc_pos generic_info_index dcl_functions st1 st2
		where
			build_main_instances [!{gcf_body = GCB_FunIndex fun_index,gcf_generic,gcf_kind,gcf_gident,gcf_generic_info}:gcfs!] ins_type module_index gc_type_cons gc_pos generic_info_index
									dcl_functions st1 st2
				# (dcl_functions, st1, st2)
					= build_main_instance_ ins_type module_index gcf_gident fun_index gcf_kind gcf_generic gc_type_cons gc_pos generic_info_index gcf_generic_info
											dcl_functions st1 st2
				= build_main_instances gcfs ins_type module_index gc_type_cons gc_pos generic_info_index dcl_functions st1 st2
			build_main_instances [!!] ins_type module_index gc_type_cons gc_pos generic_info_index dcl_functions st1 st2
				= (dcl_functions, st1, st2)

		build_main_instance_ :: InstanceType Int Ident Int TypeKind GlobalIndex TypeCons Position Int Int
				!*{#FunType} !(!Index, ![ClassInstance]) !*SpecializeState
			-> (!*{#FunType},!(!Index, ![ClassInstance]),!*SpecializeState)
		build_main_instance_ ins_type module_index gc_ident fun_index gcf_kind gcf_generic gc_type_cons gc_pos generic_info_index generic_info
								dcl_functions ins_info st=:{ss_modules=modules,ss_heaps=heaps,ss_error=error}
			# (gen_info_ptr, modules) = modules![gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index].gen_info_ptr
			  ({gen_classes,gen_rep_conses}, hp_generic_heap) = readPtr gen_info_ptr heaps.hp_generic_heap
			  heaps & hp_generic_heap=hp_generic_heap
			  (Yes class_info) = lookupGenericClassInfo gcf_kind gen_classes

			#! fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons

			| generic_info_index<0
				#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_info.gci_member]
				#! (fun_type, heaps, error)
					= determine_type_of_member_instance member_def ins_type heaps error
				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type dcl_functions heaps
				# st & ss_modules=modules, ss_heaps=heaps, ss_error=error
				#! st = update_icl_function fun_index fun_ident gc_pos gc_type_cons gc_ident gcf_generic
														fun_type generic_info_index -1 AllGenericInstanceDependencies st
				# class_instance_member = {cim_ident=fun_ident,cim_arity=module_index,cim_index= -1-fun_index}
				#! ins_info = build_class_instance class_info.gci_class gc_ident gc_pos gcf_kind class_instance_member ins_type ins_info
				= (dcl_functions, ins_info, st)

				# {grc_optional_fun_type,grc_generic_instance_deps} = gen_rep_conses.[generic_info_index]
				# (fun_type,modules,heaps,error)
					= case grc_optional_fun_type of
						Yes fun_type
							-> (fun_type,modules,heaps,error)
						No
							#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_info.gci_member]
							# (fun_type,heaps,error) = determine_type_of_member_instance member_def ins_type heaps error
							-> (fun_type,modules,heaps,error)
				# fun_type_with_generic_info
					= if (generic_info<>0)
						(add_generic_info_to_type fun_type generic_info_index generic_info gs_predefs)
						fun_type
				#! (dcl_functions, heaps)
					= update_dcl_function fun_index fun_ident fun_type_with_generic_info dcl_functions heaps
				# st & ss_modules=modules,ss_heaps=heaps,ss_error=error
				#! st = update_icl_function fun_index fun_ident gc_pos gc_type_cons gc_ident gcf_generic
											fun_type_with_generic_info generic_info_index generic_info grc_generic_instance_deps st
				= (dcl_functions, ins_info, st)

	build_shorthand_instances_in_modules :: !Index
			!*{#CommonDefs} !*{#DclModule} (FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin)
		-> (!*{#CommonDefs}, *{#DclModule},(FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
	build_shorthand_instances_in_modules module_index modules dcl_modules st
		| module_index == size modules
			= (modules, dcl_modules, st)
		| not (inNumberSet module_index gs_used_modules)
			= build_shorthand_instances_in_modules (module_index+1) modules dcl_modules st
			#! (com_gencase_defs,modules) = modules![module_index].com_gencase_defs
			#! (modules, st)
				= build_shorthand_instances_in_module module_index com_gencase_defs modules st  
			= build_shorthand_instances_in_modules (module_index+1) modules dcl_modules st
	where
		build_shorthand_instances_in_module module_index com_gencase_defs modules st
			= foldArraySt (build_shorthand_instances module_index) com_gencase_defs (modules, st)

	build_shorthand_instances :: !Index !GenericCaseDef
			(!*Modules, (FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))
		->	(!*Modules, (FunsAndGroups, (!Index, ![ClassInstance]), !*Heaps, !*ErrorAdmin))					
	build_shorthand_instances module_index {gc_gcf=GCF gc_ident {gcf_kind=KindConst}} st	
		= st
	build_shorthand_instances module_index {gc_gcf=GCF gc_ident {gcf_kind=KindArrow kinds,gcf_generic,gcf_body},gc_type,gc_type_cons,gc_pos} st
		= build_shorthand_instance_for_kinds gc_ident kinds gcf_generic gcf_body gc_type gc_type_cons gc_pos module_index st
	build_shorthand_instances module_index {gc_gcf=GCFS gcfs,gc_type,gc_type_cons,gc_pos} st
		= build_shorthand_instances_for_generic_superclasses gcfs module_index gc_type gc_type_cons gc_pos st
	where
		build_shorthand_instances_for_generic_superclasses [!{gcf_kind=KindConst}:gcfs!] module_index gc_type gc_type_cons gc_pos st
			= build_shorthand_instances_for_generic_superclasses gcfs module_index gc_type gc_type_cons gc_pos st
		build_shorthand_instances_for_generic_superclasses [!{gcf_kind=KindArrow kinds,gcf_generic,gcf_body,gcf_gident}:gcfs!] module_index gc_type gc_type_cons gc_pos st
			# st = build_shorthand_instance_for_kinds gcf_gident kinds gcf_generic gcf_body gc_type gc_type_cons gc_pos module_index st
			= build_shorthand_instances_for_generic_superclasses gcfs module_index gc_type gc_type_cons gc_pos st
		build_shorthand_instances_for_generic_superclasses [!!] module_index gc_type gc_type_cons gc_pos st
			= st

	build_shorthand_instance_for_kinds gc_ident kinds gcf_generic gcf_body gc_type gc_type_cons gc_pos module_index st
		| is_gen_cons_without_instances gc_type gs_predefs
			// no shorthand instances for OBJECT, RECORD, CONS, FIELD, PAIR and EITHER
			= st
		# fun_index
			= case gcf_body of
				GCB_FunIndex fun_index
					-> fun_index
				GCB_FunAndMacroIndex fun_index macro_index
					-> fun_index
		= foldSt (build_shorthand_instance gc_ident kinds gcf_generic fun_index gc_type gc_type_cons gc_pos module_index) [1 .. length kinds] st
	where
		build_shorthand_instance gc_ident kinds gcf_generic fun_index gc_type gc_type_cons gc_pos module_index num_args
				(modules, (fun_info, ins_info, heaps, error))

			#! (consumed_kinds, rest_kinds) = splitAt num_args kinds 		
			#! this_kind = case rest_kinds of
				[] -> KindConst
				_  -> KindArrow rest_kinds 

			#! (class_info, (modules, heaps)) = get_class_for_kind gcf_generic this_kind (modules, heaps)
			#! (arg_class_infos, (modules, heaps)) 
				= mapSt (get_class_for_kind gcf_generic) consumed_kinds (modules, heaps)
			# (deps, modules) = modules![gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index].gen_deps
			# (dep_class_infoss, (modules, heaps))
				= mapSt (\{gd_index} -> mapSt (get_class_for_kind gd_index) consumed_kinds) deps (modules, heaps)
			# class_idents = [(gcf_generic, gc_ident):[(gd_index, ident) \\ {gd_index, gd_ident=Ident ident} <- deps]]
			# arg_and_dep_class_infoss = map (zip2 class_idents) (transpose [arg_class_infos:dep_class_infoss])

			#! (ins_type, heaps)
				= build_instance_type gc_type num_args (map removeDupByIndex arg_and_dep_class_infoss) heaps
		
			#! (member_def, modules) = modules![class_info.gci_module].com_member_defs.[class_info.gci_member]
			#! (fun_type, heaps, error)
				= determine_type_of_member_instance member_def ins_type heaps error
			# fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons
	
			#! (memfun_ds, fun_info, heaps)
				= build_shorthand_instance_member module_index this_kind fun_index fun_ident gc_pos fun_type (flatten arg_and_dep_class_infoss) fun_info heaps
			#! ins_info 
				= build_shorthand_class_instance this_kind class_info.gci_class gc_ident gc_pos memfun_ds ins_type ins_info

			= (modules, (fun_info, ins_info, heaps, error))
		where
			removeDupByIndex [] = []
			removeDupByIndex [x=:((indexx, _), _):xs] = [x:removeDupByIndex (filter (\((indexy, _), _) -> indexx <> indexy) xs)]

		build_instance_type type arity arg_and_dep_class_infoss heaps=:{hp_type_heaps=th=:{th_vars},hp_var_heap}
			#! type_var_names = [makeIdent ("a" +++ toString i) \\ i <- [1 .. arity]]
			#! (type_vars, th_vars) = mapSt freshTypeVar type_var_names th_vars 
			#! type_var_types = [TV tv \\ tv <- type_vars] 	
			#! new_type_args = [makeAType t TA_Multi \\ t <- type_var_types]
			
			#! type = fill_type_args type new_type_args	
			
			# num_contexts = length (hd arg_and_dep_class_infoss)
			# context_type_vars = flatten (map (repeatn num_contexts) type_vars)
			#! (contexts, hp_var_heap) 
				= zipWithSt build_context (flatten arg_and_dep_class_infoss) context_type_vars hp_var_heap
			
			#! ins_type = 
				{	it_vars	= type_vars
				,	it_types = [type]
				,	it_attr_vars = []
				,	it_context = contexts
				}
			= (ins_type, {heaps & hp_type_heaps = {th & th_vars = th_vars}, hp_var_heap = hp_var_heap})
		where
			fill_type_args (TA type_symb_ident=:{type_arity} type_args) new_type_args
				#! type_arity = type_arity + length new_type_args 
				#! type_args = type_args ++ new_type_args
				= TA {type_symb_ident & type_arity = type_arity} type_args 
			fill_type_args TArrow [arg_type, res_type]
				= arg_type --> res_type
			fill_type_args TArrow [arg_type]
				= TArrow1 arg_type	
			fill_type_args (TArrow1 arg_type) [res_type]
				= arg_type --> res_type	 
			fill_type_args type args
				= abort ("fill_type_args\n"---> ("fill_type_args", type, args)) 

			build_context ((_, ident), {gci_class, gci_module, gci_kind}) tv hp_var_heap
				# (var_info_ptr, hp_var_heap) = newPtr VI_Empty hp_var_heap			
				# type_context =
					{	tc_class = TCClass
							{ glob_module=gci_module // the same as icl module
							, glob_object =
								{ ds_ident = genericIdentToClassIdent ident.id_name gci_kind
								, ds_index = gci_class
								, ds_arity = 1
								}
							}
					,	tc_types = [TV tv]
					,	tc_var	 = var_info_ptr
					}
				= (type_context, hp_var_heap)

		build_shorthand_instance_member :: Int TypeKind Int Ident Position SymbolType [((GlobalIndex, Ident), GenericClassInfo)] !FunsAndGroups !*Heaps
										-> (!DefinedSymbol,!FunsAndGroups,!*Heaps)
		build_shorthand_instance_member module_index this_kind fun_index fun_ident gc_pos st arg_and_dep_class_infos fun_info heaps
			#! arg_var_names = ["x" +++ toString i \\ i <- [1..st.st_arity]]
			#! (arg_var_exprs, arg_vars, heaps) = buildVarExprs arg_var_names heaps
					
			#! (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty heaps.hp_expression_heap
			#! heaps = {heaps & hp_expression_heap = hp_expression_heap}
			#! fun_name = genericIdentToMemberIdent gc_ident.id_name this_kind
	
			# (gen_exprs, heaps) = mapSt build_generic_app arg_and_dep_class_infos heaps
	
			#! arg_exprs = gen_exprs ++ arg_var_exprs
			# (body_expr, heaps)
				= buildFunApp2 module_index fun_index fun_ident arg_exprs heaps
	
			#! (st, heaps) = fresh_symbol_type st heaps

			#! (fun_ds, fun_info) 
				= buildFunAndGroup fun_name arg_vars body_expr (Yes st) gs_main_module gc_pos fun_info

			= (fun_ds, fun_info, heaps)
		where
			build_generic_app (({gi_module, gi_index}, ident), {gci_kind}) heaps
				= buildGenericApp gi_module gi_index ident gci_kind [] heaps

		build_shorthand_class_instance this_kind class_index gc_ident gc_pos {ds_ident,ds_arity,ds_index} ins_type (ins_index, instances)
			#! class_ident = genericIdentToClassIdent gc_ident.id_name this_kind
			#! ins = 
			 	{	ins_class_index = {gi_module=gs_main_module, gi_index=class_index}
			 	,	ins_class_ident = {ci_ident=Ident class_ident, ci_arity=1}
				,	ins_ident 	= class_ident
				,	ins_type 	= ins_type
				,	ins_member_types_and_functions = []
				,	ins_members	= {{cim_ident=ds_ident,cim_arity=ds_arity,cim_index=ds_index}}
				,	ins_specials = SP_None
				,	ins_pos		= gc_pos
				}
			= (ins_index+1, [ins:instances])

	get_class_for_kind :: !GlobalIndex !TypeKind !(!*{#CommonDefs},!*Heaps) -> (!GenericClassInfo,!(!*{#CommonDefs},!*Heaps))
	get_class_for_kind {gi_module, gi_index} kind (modules,heaps=:{hp_generic_heap})
		#! ({gen_info_ptr}, modules) = modules![gi_module].com_generic_defs.[gi_index]
		#! ({gen_classes}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
		# (Yes class_info) = lookupGenericClassInfo kind gen_classes
		= (class_info, (modules, heaps))	

	determine_type_of_member_instance :: !MemberDef !InstanceType !*Heaps !*ErrorAdmin
		-> (!SymbolType, !*Heaps, !*ErrorAdmin)
	determine_type_of_member_instance {me_type, me_class_vars} ins_type heaps=:{hp_type_heaps, hp_var_heap} error
		#! (symbol_type, _, hp_type_heaps, _, error) 
			= determineTypeOfMemberInstance me_type me_class_vars ins_type SP_None hp_type_heaps No error
		#! (st_context, hp_var_heap) = initializeContextVariables symbol_type.st_context hp_var_heap
		#! hp_type_heaps = clearSymbolType me_type hp_type_heaps
		#! symbol_type = {symbol_type & st_context = st_context}
		#! heaps = {heaps & hp_type_heaps = hp_type_heaps, hp_var_heap = hp_var_heap}
		= (symbol_type, heaps, error)

	update_dcl_function :: !Index !Ident !SymbolType !*{#FunType} !*Heaps -> (!*{#FunType}, !*Heaps)
	update_dcl_function fun_index fun_ident symbol_type dcl_functions heaps 
		| fun_index < size dcl_functions
			#! (symbol_type, heaps) = fresh_symbol_type symbol_type heaps			
			#! (fun, dcl_functions) = dcl_functions![fun_index]
			#! fun = {fun	& ft_ident = fun_ident
							, ft_type = symbol_type
							, ft_arity = symbol_type.st_arity}
			#! dcl_functions = {dcl_functions & [fun_index] = fun}
			= (dcl_functions, heaps)
			= (dcl_functions, heaps)

	update_icl_function :: !Index !Ident !Position !TypeCons !Ident !GlobalIndex !SymbolType !Int !Int !GenericInstanceDependencies
							!*SpecializeState -> *SpecializeState
	update_icl_function fun_index fun_ident gc_pos gc_type_cons gc_ident gcf_generic symbol_type generic_info_index generic_info generic_instance_deps
			st
		#! (symbol_type, heaps) = fresh_symbol_type symbol_type st.ss_heaps
		# st & ss_heaps = heaps
		#! (fun=:{fun_body, fun_arity}, st) = st!ss_funs.[fun_index] 		
		= case fun_body of
			TransformedBody {tb_args,tb_rhs}	// user defined case
				| generic_info_index>=0
					# n_unused_dep_args
						= case generic_instance_deps of
							GenericInstanceDependencies n_deps deps
								-> n_deps - add_n_bits deps 0
							_
								-> 0
					| generic_info==0
						// remove generic info argument
						# tb_args = tl tb_args
						  fun_arity = fun_arity-1
						| fun_arity<>symbol_type.st_arity + n_unused_dep_args
							# error = reportError gc_ident.id_name gc_pos
										("incorrect arity "+++toString fun_arity+++", expected "+++toString (symbol_type.st_arity+n_unused_dep_args)) st.ss_error
							-> {st & ss_error=error}
						# (tb_args,fun_arity)
							= case generic_instance_deps of
								GenericInstanceDependencies n_deps deps
									# tb_args = remove_unused_dep_args tb_args 0 n_deps deps
									# fun_arity = fun_arity-n_unused_dep_args
									-> (tb_args,fun_arity)
								_
									-> (tb_args,fun_arity)
						# fun_body = TransformedBody {tb_args = tb_args, tb_rhs = tb_rhs}
						# fun = {fun & fun_ident = fun_ident, fun_type = Yes symbol_type, fun_body = fun_body, fun_arity = fun_arity}
						-> {st & ss_funs.[fun_index] = fun}
					| generic_info<0
						// keep generic info argument
						| fun_arity<>symbol_type.st_arity + n_unused_dep_args
							# error = reportError gc_ident.id_name gc_pos
										("incorrect arity "+++toString (fun_arity-1)+++", expected "+++toString (symbol_type.st_arity+n_unused_dep_args-1)) st.ss_error
							-> {st & ss_error=error}
						# (fun_body,fun_arity)
							= case generic_instance_deps of
								GenericInstanceDependencies n_deps deps
									# [generic_info_arg:args] = tb_args
									# tb_args = [generic_info_arg : remove_unused_dep_args args 0 n_deps deps]
									# fun_arity = fun_arity-n_unused_dep_args
									-> (TransformedBody {tb_args = tb_args, tb_rhs = tb_rhs},fun_arity)
								_
									-> (fun_body,fun_arity)
						# fun = {fun & fun_ident = fun_ident, fun_type = Yes symbol_type, fun_body = fun_body, fun_arity = fun_arity}
						-> {st & ss_funs.[fun_index] = fun}
						// generic info record already replaced by fields
						# n_generic_info_field = add_n_bits generic_info 0
						| fun_arity<>symbol_type.st_arity + n_unused_dep_args
							# error = reportError gc_ident.id_name gc_pos
										("incorrect arity "+++toString (fun_arity-n_generic_info_field)+++", expected "+++toString (symbol_type.st_arity+n_unused_dep_args-n_generic_info_field)) st.ss_error
							-> {st & ss_error=error}
						# (fun_body,fun_arity)
							= case generic_instance_deps of
								GenericInstanceDependencies n_deps deps
									# (generic_info_args,args) = splitAt n_generic_info_field tb_args
									# tb_args = generic_info_args ++ remove_unused_dep_args args 0 n_deps deps
									# fun_arity = fun_arity-n_unused_dep_args
									-> (TransformedBody {tb_args = tb_args, tb_rhs = tb_rhs},fun_arity)
								_
									-> (fun_body,fun_arity)	
						# fun = {fun & fun_ident = fun_ident, fun_type = Yes symbol_type, fun_body = fun_body, fun_arity = fun_arity}
						-> {st & ss_funs.[fun_index] = fun}
					// not a special generic instance, remove generic info argument		
					# tb_args = tl tb_args
					  fun_arity = fun_arity-1
					# fun_body = TransformedBody {tb_args = tb_args, tb_rhs = tb_rhs}
					| fun_arity<>symbol_type.st_arity
						# error = reportError gc_ident.id_name gc_pos
									("incorrect arity "+++toString fun_arity+++", expected "+++toString symbol_type.st_arity) st.ss_error
						-> {st & ss_error=error}
					# fun = {fun & fun_ident = fun_ident, fun_body = fun_body, fun_type = Yes symbol_type, fun_arity=fun_arity}
					-> {st & ss_funs.[fun_index] = fun}
			GeneratedBody		// derived case
				#! (TransformedBody {tb_args, tb_rhs}, st)
					= buildGenericCaseBody gs_main_module gc_pos gc_type_cons gc_ident generic_info_index gcf_generic gs_predefs st
				# funs_and_groups=:{fg_group_index,fg_groups} = st.ss_funs_and_groups
				#! fun = makeFunction fun_ident fg_group_index tb_args tb_rhs (Yes symbol_type) gs_main_module gc_pos
				# group = {group_members=[fun_index]}
				  funs_and_groups & fg_group_index=fg_group_index+1,fg_groups=[group:fg_groups]
				-> {st & ss_funs.[fun_index] = fun, ss_funs_and_groups = funs_and_groups}

	build_class_instance :: Int Ident Position TypeKind ClassInstanceMember InstanceType !(!Int,![ClassInstance]) -> (!Int,![ClassInstance])
	build_class_instance class_index gc_ident gc_pos gc_kind class_instance_member ins_type (ins_index, instances)
		# class_ident = genericIdentToClassIdent gc_ident.id_name gc_kind
		#! ins =
		 	{	ins_class_index = {gi_module=gs_main_module, gi_index=class_index}
		 	,	ins_class_ident = {ci_ident=Ident class_ident, ci_arity=1}
			,	ins_ident 	= class_ident
			,	ins_type 	= ins_type
			,	ins_member_types_and_functions = []
			,	ins_members	= {class_instance_member}
			,	ins_specials = SP_None
			,	ins_pos		= gc_pos
			}
		= (ins_index+1, [ins:instances])

add_n_bits :: !Int !Int -> Int
add_n_bits n c
	| n>1
		= add_n_bits (n>>1) (c+(n bitand 1))
		= c+n

remove_unused_dep_args :: ![FreeVar] !Int !Int !Int -> [FreeVar]
remove_unused_dep_args args=:[arg:r_args] arg_n n_deps deps
	| arg_n>=n_deps
		= args
	| deps bitand (1<<arg_n)<>0
		= [arg : remove_unused_dep_args r_args (arg_n+1) n_deps deps]
		= remove_unused_dep_args r_args (arg_n+1) n_deps deps
remove_unused_dep_args [] arg_n n_deps deps
	= []

determine_type_of_member_instance_from_symbol_type :: !SymbolType !InstanceType !*TypeHeaps !*VarHeap !*ErrorAdmin
	-> (!SymbolType, !*TypeHeaps, !*VarHeap, !*ErrorAdmin)
determine_type_of_member_instance_from_symbol_type me_type=:{st_context=[{tc_types = [TV class_var]}:_]} ins_type hp_type_heaps hp_var_heap error
	#! (symbol_type, _, hp_type_heaps, _, error) 
		= determineTypeOfMemberInstance me_type [class_var] ins_type SP_None hp_type_heaps No error
	#! (st_context, hp_var_heap) = initializeContextVariables symbol_type.st_context hp_var_heap
	#! hp_type_heaps = clearSymbolType me_type hp_type_heaps
	#! symbol_type = {symbol_type & st_context = st_context}
	= (symbol_type, hp_type_heaps, hp_var_heap, error)

// add an argument for generic info at the beginning
add_generic_info_to_type :: !SymbolType !Int !Int !PredefinedSymbolsData -> SymbolType
add_generic_info_to_type st=:{st_arity, st_args, st_args_strictness} generic_info_index generic_info predefs
	# (st_args,n_new_args) = add_generic_info_types generic_info_index generic_info st_args predefs
	= {st & st_args = st_args, st_arity = st_arity + n_new_args, st_args_strictness = insert_n_lazy_values_at_beginning n_new_args st_args_strictness}
where
	add_generic_info_types 0 generic_info args predefs=:{psd_predefs_a}
		| generic_info== -1
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenericTypeDefDescriptor]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenericTypeDefDescriptor] 0
			= ([makeAType (TA type_symb []) TA_Multi : args], 1)
			= add_OBJECT_field_args generic_info args predefs
	add_generic_info_types 1 generic_info args predefs=:{psd_predefs_a}
		| generic_info== -1
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenericConsDescriptor]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenericConsDescriptor] 0
			= ([makeAType (TA type_symb []) TA_Multi : args], 1)
			= add_CONS_field_args generic_info args predefs
	add_generic_info_types 2 generic_info args predefs=:{psd_predefs_a}
		| generic_info== -1
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenericRecordDescriptor]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenericRecordDescriptor] 0
			= ([makeAType (TA type_symb []) TA_Multi : args], 1)
			= add_RECORD_field_args generic_info args predefs
	add_generic_info_types 3 generic_info args predefs=:{psd_predefs_a}
		| generic_info== -1
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenericFieldDescriptor]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenericFieldDescriptor] 0
			= ([makeAType (TA type_symb []) TA_Multi : args], 1)
			= add_FIELD_field_args generic_info args predefs

	add_OBJECT_field_args generic_info args predefs=:{psd_predefs_a}
		| generic_info bitand 1<>0 // gtd_name
			# (args,n_args) = add_OBJECT_field_args (generic_info bitxor 1) args predefs
			= add_String_arg args n_args psd_predefs_a
		| generic_info bitand 2<>0 // gtd_arity
			# (args,n_args) = add_OBJECT_field_args (generic_info bitxor 2) args predefs
			= add_Int_arg args n_args
		| generic_info bitand 4<>0 // gtd_num_conses
			# (args,n_args) = add_OBJECT_field_args (generic_info bitxor 4) args predefs
			= add_Int_arg args n_args
		| generic_info bitand 8<>0 // gtd_conses
			# (args,n_args) = add_RECORD_field_args (generic_info bitxor 8) args predefs
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenericConsDescriptor]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenericConsDescriptor] 0
			# type_GenericConsDescriptor = {at_type= TA type_symb [], at_attribute = TA_Multi}
			# {pds_module,pds_def} = psd_predefs_a.[PD_ListType]
			#! string_type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_ListType] 1
			= ([{at_type = TA string_type_symb [type_GenericConsDescriptor], at_attribute = TA_Multi} : args],n_args+1)
			= (args,0)

	add_CONS_field_args generic_info args predefs=:{psd_predefs_a}
		| generic_info bitand 1<>0 // gcd_name
			# (args,n_args) = add_CONS_field_args (generic_info bitxor 1) args predefs
			= add_String_arg args n_args psd_predefs_a
		| generic_info bitand 2<>0 // gcd_arity
			# (args,n_args) = add_CONS_field_args (generic_info bitxor 2) args predefs
			= add_Int_arg args n_args
		| generic_info bitand 4<>0 // gcd_prio
			# (args,n_args) = add_CONS_field_args (generic_info bitxor 4) args predefs
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenConsPrio]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenConsPrio] 0
			= ([{at_type = TA type_symb [], at_attribute = TA_Multi} : args],n_args+1)
		| generic_info bitand 8<>0 // gcd_type_def
			# (args,n_args) = add_CONS_field_args (generic_info bitxor 8) args predefs
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenericTypeDefDescriptor]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenericTypeDefDescriptor] 0
			= ([{at_type = TA type_symb [], at_attribute = TA_Multi} : args],n_args+1)
		| generic_info bitand 16<>0 // gcd_type
			# (args,n_args) = add_CONS_field_args (generic_info bitxor 16) args predefs
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenType]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenType] 0
			= ([{at_type = TA type_symb [], at_attribute = TA_Multi} : args],n_args+1)
		| generic_info bitand 32<>0 // gcd_index
			# (args,n_args) = add_CONS_field_args (generic_info bitxor 32) args predefs
			= add_Int_arg args n_args
			= (args,0)

	add_RECORD_field_args generic_info args predefs=:{psd_predefs_a}
		| generic_info bitand 1<>0 // grd_name
			# (args,n_args) = add_RECORD_field_args (generic_info bitxor 1) args predefs
			= add_String_arg args n_args psd_predefs_a
		| generic_info bitand 2<>0 // grd_arity
			# (args,n_args) = add_RECORD_field_args (generic_info bitxor 2) args predefs
			= add_Int_arg args n_args
		| generic_info bitand 4<>0 // grd_type_arity
			# (args,n_args) = add_RECORD_field_args (generic_info bitxor 4) args predefs
			= add_Int_arg args n_args
		| generic_info bitand 8<>0 // grd_type
			# (args,n_args) = add_RECORD_field_args (generic_info bitxor 8) args predefs
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenType]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenType] 0
			= ([{at_type = TA type_symb [], at_attribute = TA_Multi} : args],n_args+1)
		| generic_info bitand 16<>0 // grd_fields
			# (args,n_args) = add_RECORD_field_args (generic_info bitxor 16) args predefs
			# {pds_module,pds_def} = psd_predefs_a.[PD_StringType]
			#! string_type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_StringType] 0
			# string_type = {at_type = TA string_type_symb [], at_attribute = TA_Multi}
			# {pds_module,pds_def} = psd_predefs_a.[PD_ListType]
			#! string_type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_ListType] 1
			= ([{at_type = TA string_type_symb [string_type], at_attribute = TA_Multi} : args],n_args+1)
			= (args,0)

	add_FIELD_field_args generic_info args predefs=:{psd_predefs_a}
		| generic_info bitand 1<>0 // gfd_name
			# (args,n_args) = add_FIELD_field_args (generic_info bitxor 1) args predefs
			= add_String_arg args n_args psd_predefs_a
		| generic_info bitand 2<>0 // gfd_index
			# (args,n_args) = add_FIELD_field_args (generic_info bitxor 2) args predefs
			= add_Int_arg args n_args
		| generic_info bitand 4<>0 // gfd_cons
			# (args,n_args) = add_FIELD_field_args (generic_info bitxor 4) args predefs
			# {pds_module, pds_def} = psd_predefs_a.[PD_TGenericRecordDescriptor]
			#! type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_TGenericRecordDescriptor] 0
			= ([{at_type = TA type_symb [], at_attribute = TA_Multi} : args],n_args+1)
			= (args,0)

	add_String_arg args n_args psd_predefs_a
		# {pds_module,pds_def} = psd_predefs_a.[PD_StringType]
		#! string_type_symb = MakeTypeSymbIdent {glob_module = pds_module, glob_object = pds_def} predefined_idents.[PD_StringType] 0
		= ([{at_type = TA string_type_symb [], at_attribute = TA_Multi} : args],n_args+1)

	add_Int_arg args n_args
		= ([{at_type = TB BT_Int, at_attribute = TA_Multi} : args],n_args+1)

index_gen_cons_with_info_type :: !Type !PredefinedSymbolsData -> Int
index_gen_cons_with_info_type (TA {type_index={glob_module,glob_object}} []) {psd_predefs_a}
	| glob_module==psd_predefs_a.[PD_StdGeneric].pds_def
		| glob_object==psd_predefs_a.[PD_TypeOBJECT].pds_def
			= 0
		| glob_object==psd_predefs_a.[PD_TypeCONS].pds_def
			= 1
		| glob_object==psd_predefs_a.[PD_TypeRECORD].pds_def
			= 2
		| glob_object==psd_predefs_a.[PD_TypeFIELD].pds_def
			= 3
		| glob_object==psd_predefs_a.[PD_TypePAIR].pds_def
			= 4
		| glob_object==psd_predefs_a.[PD_TypeEITHER].pds_def
			= 5
		| glob_object==psd_predefs_a.[PD_TypeUNIT].pds_def
			= 6
			= -1
		= -1
index_gen_cons_with_info_type _ predefs
	= -1

is_gen_cons_without_instances :: !Type !PredefinedSymbolsData -> Bool
is_gen_cons_without_instances (TA {type_index={glob_module,glob_object}} []) {psd_predefs_a}
	| glob_module==psd_predefs_a.[PD_StdGeneric].pds_def
		=  glob_object==psd_predefs_a.[PD_TypeOBJECT].pds_def
		|| glob_object==psd_predefs_a.[PD_TypeCONS].pds_def
		|| glob_object==psd_predefs_a.[PD_TypeRECORD].pds_def
		|| glob_object==psd_predefs_a.[PD_TypeFIELD].pds_def
		|| glob_object==psd_predefs_a.[PD_TypePAIR].pds_def
		|| glob_object==psd_predefs_a.[PD_TypeEITHER].pds_def
		|| glob_object==psd_predefs_a.[PD_TypeUNIT].pds_def
		= False
is_gen_cons_without_instances _ predefs
	= False

buildGenericCaseBody :: 
		!Index					// current icl module
		!Position !TypeCons !Ident !Int !GlobalIndex
		!PredefinedSymbolsData
		!*SpecializeState
	-> (!FunctionBody,
		!*SpecializeState)
buildGenericCaseBody main_module_index gc_pos (TypeConsSymb {type_ident,type_index}) gc_ident generic_info_index gcf_generic predefs=:{psd_predefs_a}
					st=:{ss_modules=modules,ss_td_infos=td_infos,ss_heaps=heaps}
	# generic_bimap = psd_predefs_a.[PD_GenericBimap]
	# is_generic_bimap = gcf_generic.gi_module==generic_bimap.pds_module && gcf_generic.gi_index==generic_bimap.pds_def
	#! (gen_def, modules) = modules![gcf_generic.gi_module].com_generic_defs.[gcf_generic.gi_index]
	#! (td_info=:{tdi_gen_rep}, td_infos) = td_infos![type_index.glob_module, type_index.glob_object]
	# (gen_type_rep=:{gtr_type}) = case tdi_gen_rep of
		GenericTypeRepAndBimapTypeRep gen_type_rep bimap_gen_type_rep 
			| is_generic_bimap
				-> bimap_gen_type_rep
				-> gen_type_rep
		GenericTypeRep gen_type_rep
			| not is_generic_bimap
				-> gen_type_rep
		GenericBimapTypeRep bimap_gen_type_rep
			| is_generic_bimap
				-> bimap_gen_type_rep
		_ -> abort "sanity check: no generic representation\n"

	#! (type_def=:{td_args,td_arity,td_rhs}, modules) = modules![type_index.glob_module].com_type_defs.[type_index.glob_object]
	#! (generated_arg_exprss, original_arg_exprs, arg_vars, heaps)
		= build_arg_vars gen_def gcf_generic td_args heaps

	# (arg_vars,heaps)
		= if (generic_info_index>=0)
			(let
				(generic_info_var, heaps_) = build_generic_info_arg heaps
				arg_vars = [generic_info_var:arg_vars]
			 in (arg_vars,heaps_))
			(arg_vars,heaps)

	# (is_simple_bimap,modules,heaps)
		= test_if_simple_bimap gcf_generic td_args td_rhs type_index.glob_module psd_predefs_a modules heaps
	| is_simple_bimap
		# (body_expr,modules,heaps) = build_simple_bimap td_args td_rhs type_index generated_arg_exprss original_arg_exprs modules heaps
		# st & ss_modules=modules,ss_td_infos=td_infos,ss_heaps=heaps
		= (TransformedBody {tb_args=arg_vars, tb_rhs=body_expr}, st)	

	# st & ss_modules=modules,ss_td_infos=td_infos,ss_heaps=heaps
	#! (specialized_expr, st)
		= build_specialized_expr gc_pos gc_ident gcf_generic gen_def.gen_deps gen_def.gen_vars gtr_type td_args generated_arg_exprss gen_def.gen_info_ptr st

	# {ss_modules=modules,ss_td_infos=td_infos,ss_funs_and_groups=funs_and_groups,ss_heaps=heaps,ss_error=error} = st
	#! (body_expr, funs_and_groups, modules, td_infos, heaps, error)
		= adapt_specialized_expr gc_pos gen_def gen_type_rep original_arg_exprs specialized_expr funs_and_groups modules td_infos heaps error
	# st & ss_modules=modules,ss_td_infos=td_infos,ss_funs_and_groups=funs_and_groups,ss_heaps=heaps,ss_error=error

	= (TransformedBody {tb_args=arg_vars, tb_rhs=body_expr}, st)	
where
	build_generic_info_arg heaps=:{hp_var_heap}
		// generic arg is never referenced in the generated body
		#! (fv_info_ptr, hp_var_heap) = newPtr VI_Empty hp_var_heap
		#! fv = {fv_count = 0, fv_ident = makeIdent "geninfo", fv_info_ptr = fv_info_ptr, fv_def_level = NotALevel}	
		= (fv, {heaps & hp_var_heap = hp_var_heap})

	build_arg_vars :: GenericDef GlobalIndex [ATypeVar] *Heaps -> (![[Expression]],![Expression],![FreeVar],!*Heaps)
	build_arg_vars {gen_ident, gen_vars, gen_type, gen_deps} gcf_generic td_args heaps 
		# dep_names = [(gen_ident, gen_vars, gcf_generic) : [(ident, gd_vars, gd_index) \\ {gd_ident=Ident ident, gd_vars, gd_index} <- gen_deps]]
		#! (generated_arg_exprss, generated_arg_vars, heaps) 
			= mapY2St buildVarExprs
				[[mkDepName dep_name atv_variable \\ dep_name <- dep_names] \\ {atv_variable} <- td_args]
				heaps	
		#! (original_arg_exprs, original_arg_vars, heaps) 
			= buildVarExprs 
				[ "x" +++ toString n \\ n <- [1 .. gen_type.st_arity]] 
				heaps	
		= (generated_arg_exprss, original_arg_exprs, flatten generated_arg_vars ++ original_arg_vars, heaps)
		where 
			mkDepName (ident, gvars, index) atv 
				# gvarsName = foldl (\vs v -> vs +++ "_" +++ v.tv_ident.id_name) "" gvars
				# indexName = "_" +++ toString index.gi_module +++ "-" +++ toString index.gi_index
				= ident.id_name +++ gvarsName +++ indexName +++ "_" +++ atv.tv_ident.id_name

	// generic function specialized to the generic representation of the type
	build_specialized_expr gc_pos gc_ident gcf_generic gen_deps gen_vars gtr_type td_args generated_arg_exprss gen_info_ptr st
                // TODO: TvN: bimap_spec_env is hacked to fit the original description of a spec_env, taking the hd of the generated_arg_exprss, change it?
		# generic_bimap = psd_predefs_a.[PD_GenericBimap]
		| gcf_generic.gi_module==generic_bimap.pds_module && gcf_generic.gi_index==generic_bimap.pds_def

			#! bimap_spec_env = [(atv_variable, TVI_BimapExpr False bimap_a_b_expr bimap_b_a_expr) \\ {atv_variable} <- td_args & [bimap_a_b_expr,bimap_b_a_expr] <- generated_arg_exprss]
			// JvG: can probably make special version of simplify_bimap_GenTypeStruct that doesn't simplify if any var occurs, because all vars are passed
			# (gtr_type, heaps) = simplify_bimap_GenTypeStruct [atv_variable \\ {atv_variable} <- td_args] gtr_type st.ss_heaps

			# (expr,funs_and_groups,heaps,error)
				= specialize_generic_bimap gcf_generic gtr_type bimap_spec_env gc_ident gc_pos main_module_index predefs st.ss_funs_and_groups heaps st.ss_error
			# st & ss_funs_and_groups=funs_and_groups,ss_heaps=heaps,ss_error=error
			= (expr,st)

			# g_nums = [i \\ _<-gen_vars & i<-[0..]]
			#! spec_env = [(atv_variable, TVI_Exprs (zip2
							[(gcf_generic,g_nums):[(gd_index,gd_nums) \\ {gd_index,gd_nums} <- gen_deps]] exprs))
							\\ {atv_variable} <- td_args & exprs <- generated_arg_exprss]
			# heaps = st.ss_heaps
			  ({gen_rep_conses},generic_heap) = readPtr gen_info_ptr heaps.hp_generic_heap
			  st & ss_heaps= {heaps & hp_generic_heap=generic_heap}

			= specializeGeneric gcf_generic gtr_type spec_env gc_ident gc_pos gen_deps gen_rep_conses gen_info_ptr g_nums main_module_index predefs st

	// adaptor that converts a function for the generic representation into a 
	// function for the type itself
	adapt_specialized_expr :: Position GenericDef GenericTypeRep [Expression] Expression
						!FunsAndGroups !*Modules !*TypeDefInfos !*Heaps !*ErrorAdmin
		-> (!Expression,!FunsAndGroups,!*Modules,!*TypeDefInfos,!*Heaps,!*ErrorAdmin)
	adapt_specialized_expr gc_pos {gen_type, gen_vars, gen_info_ptr} {gtr_to,gtr_from} original_arg_exprs specialized_expr
			funs_and_groups modules td_infos heaps error
		#! (var_kinds, heaps) = get_var_kinds gen_info_ptr heaps		
		#! non_gen_var_kinds = drop (length gen_vars) var_kinds  
		
		#! non_gen_vars = gen_type.st_vars -- gen_vars	
		#! (gen_env, heaps) 
			= build_gen_env gtr_to gtr_from gen_vars heaps
		#! (non_gen_env, funs_and_groups, heaps)
			= build_non_gen_env non_gen_vars non_gen_var_kinds funs_and_groups heaps
		#! spec_env = gen_env ++ non_gen_env	
		#! curried_gen_type = curry_symbol_type gen_type

		#! (struct_gen_type, (modules, td_infos, heaps, error))
			= convert_bimap_AType_to_GenTypeStruct curried_gen_type gc_pos predefs (modules, td_infos, heaps, error)  

		#! (struct_gen_type, heaps) = simplify_bimap_GenTypeStruct gen_vars struct_gen_type heaps

		# bimap_gi = {gi_module=bimap_module,gi_index=bimap_index}
		#! (body_expr, funs_and_groups, modules, heaps, error)
			= adapt_with_specialized_generic_bimap bimap_gi struct_gen_type spec_env bimap_ident gc_pos original_arg_exprs specialized_expr main_module_index predefs 
						funs_and_groups modules heaps error

		= (body_expr, funs_and_groups, modules, td_infos, heaps, error)
	where
		{pds_module = bimap_module, pds_def=bimap_index} = psd_predefs_a.[PD_GenericBimap]
		bimap_ident = predefined_idents.[PD_GenericBimap]
		
		get_var_kinds gen_info_ptr heaps=:{hp_generic_heap}
			#! ({gen_var_kinds}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap
			= (gen_var_kinds, {heaps & hp_generic_heap = hp_generic_heap})
		
		curry_symbol_type {st_args, st_result}
			= foldr (\x y -> makeAType (x --> y) TA_Multi) st_result st_args 	
	
		build_gen_env :: !DefinedSymbol !DefinedSymbol ![TypeVar] !*Heaps -> (![(!TypeVar, !TypeVarInfo)], !*Heaps)
		build_gen_env gtr_to gtr_from gen_vars heaps 
			= mapSt build_iso_expr gen_vars heaps
		where
			build_iso_expr gen_var heaps 
				= ((gen_var, TVI_Iso gtr_to gtr_from), heaps)

		build_non_gen_env :: ![TypeVar] ![TypeKind] FunsAndGroups !*Heaps -> (![(!TypeVar, !TypeVarInfo)], !FunsAndGroups, !*Heaps)
		build_non_gen_env non_gen_vars kinds funs_and_groups heaps
			= zipWithSt2 build_bimap_expr non_gen_vars kinds funs_and_groups heaps
		where
			// build application of generic bimap for a specific kind
			build_bimap_expr non_gen_var KindConst funs_and_groups heaps
				# (expr, funs_and_groups, heaps)
					= bimap_id_expression main_module_index predefs funs_and_groups heaps
				= ((non_gen_var, TVI_BimapExpr True expr expr), funs_and_groups, heaps)
			build_bimap_expr non_gen_var kind funs_and_groups heaps
				#! (expr, heaps)
					= buildGenericApp bimap_module bimap_index bimap_ident kind [] heaps		
				= ((non_gen_var, TVI_BimapExpr False expr expr), funs_and_groups, heaps)
buildGenericCaseBody main_module_index gc_pos gc_type_cons gc_ident generic_info_index gcf_generic predefs st
	# error = reportError gc_ident.id_name gc_pos "cannot specialize to this type" st.ss_error
	= (TransformedBody {tb_args=[], tb_rhs=EE}, {st & ss_error=error})


test_if_simple_bimap :: GlobalIndex [ATypeVar] TypeRhs Int PredefinedSymbols !*Modules !*Heaps -> (!Bool,!*Modules,!*Heaps)
test_if_simple_bimap gcf_generic td_args (AlgType alts) type_module psd_predefs_a modules heaps
	# generic_bimap = psd_predefs_a.[PD_GenericBimap]
	| gcf_generic.gi_module==generic_bimap.pds_module && gcf_generic.gi_index==generic_bimap.pds_def
		= can_generate_bimap_to_or_from_for_this_type td_args type_module alts modules heaps
		= (False,modules,heaps)
test_if_simple_bimap gcf_generic td_args td_rhs type_module psd_predefs_a modules heaps
	= (False,modules,heaps)

build_simple_bimap :: [ATypeVar] !TypeRhs (Global Index) [[Expression]] [Expression] *Modules *Heaps -> (!Expression,!*Modules,!*Heaps)
build_simple_bimap td_args (AlgType alts) type_index generated_arg_exprss [original_arg_expr] modules heaps
	# {hp_type_heaps} = heaps
	  th_vars = set_arg_exprs td_args generated_arg_exprss hp_type_heaps.th_vars
	  heaps & hp_type_heaps={hp_type_heaps & th_vars=th_vars}
	  (alg_patterns,modules,heaps) = build_bimap_alg_patterns alts type_index.glob_module modules heaps
	  (case_expr,heaps) = build_bimap_case {gi_module=type_index.glob_module,gi_index=type_index.glob_object} original_arg_expr alg_patterns heaps
	  {hp_type_heaps} = heaps
	  th_vars = remove_type_argument_numbers td_args hp_type_heaps.th_vars
	  heaps & hp_type_heaps={hp_type_heaps & th_vars=th_vars}
	= (case_expr,modules,heaps)
where	
	set_arg_exprs :: ![ATypeVar] ![[Expression]] !*TypeVarHeap -> *TypeVarHeap
	set_arg_exprs [{atv_variable={tv_info_ptr}}:atype_vars] [[arg_expr:_]:arg_exprs] th_vars
		# th_vars = writePtr tv_info_ptr (TVI_SimpleBimapArgExpr arg_expr) th_vars
		= set_arg_exprs atype_vars arg_exprs th_vars
	set_arg_exprs [] [] th_vars
		= th_vars
	
	build_bimap_alg_patterns :: [DefinedSymbol] Int !*Modules *Heaps -> (![AlgebraicPattern],!*Modules,!*Heaps)
	build_bimap_alg_patterns [cons_ds=:{ds_ident,ds_index,ds_arity}:alts] type_module_n modules heaps
		# (cons_args,modules) = modules![type_module_n].com_cons_defs.[ds_index].cons_type.st_args
		  arg_names = ["x" +++ toString k \\ k <- [1..ds_arity]]
		  (var_exprs, vars, heaps) = buildVarExprs arg_names heaps
		  {hp_type_heaps} = heaps
		  (args,th_vars) = bimaps_with_arg cons_args var_exprs hp_type_heaps.th_vars
		  heaps & hp_type_heaps={hp_type_heaps & th_vars=th_vars}	  
		  (alg_pattern,heaps) = build_alg_pattern cons_ds vars args type_module_n heaps
		  (alg_patterns,modules,heaps) = build_bimap_alg_patterns alts type_module_n modules heaps
		= ([alg_pattern:alg_patterns],modules,heaps)
	build_bimap_alg_patterns [] type_module_n modules heaps
		= ([],modules,heaps)
	
	bimaps_with_arg :: [AType] [Expression] !*TypeVarHeap -> (![Expression],!*TypeVarHeap)
	bimaps_with_arg [{at_type=TV {tv_info_ptr}}:type_args] [var_expr:var_exprs] th_vars
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars
		= case tv_info of
			TVI_SimpleBimapArgExpr bimap_expr
				# (args,th_vars) = bimaps_with_arg type_args var_exprs th_vars
				= ([bimap_expr @ [var_expr]:args],th_vars)
	bimaps_with_arg [] [] th_vars
		= ([],th_vars)

//  convert generic type contexts into normal type contexts

convertGenericTypeContexts :: !*GenericState -> *GenericState
convertGenericTypeContexts 
		gs=:{gs_main_module, gs_used_modules, gs_predefs, gs_funs, gs_modules, gs_dcl_modules, gs_error,
			gs_avarh, gs_tvarh, gs_exprh, gs_varh, gs_genh}
	# heaps = 
		{ hp_expression_heap = gs_exprh
		, hp_var_heap = gs_varh
		, hp_generic_heap = gs_genh
		, hp_type_heaps = { th_vars = gs_tvarh, th_attrs = gs_avarh }
		}	

	# (gs_funs, (gs_modules, heaps, gs_error)) = convert_functions 0 gs_funs (gs_modules, heaps, gs_error)

	# (gs_modules, gs_dcl_modules, (heaps, gs_error)) = convert_modules 0 gs_modules gs_dcl_modules (heaps, gs_error)

	# {hp_expression_heap, hp_var_heap, hp_generic_heap, hp_type_heaps={th_vars, th_attrs}} = heaps

	 = {gs	& gs_funs = gs_funs
			, gs_modules = gs_modules
			, gs_dcl_modules = gs_dcl_modules
			, gs_error = gs_error
			, gs_avarh = th_attrs
			, gs_tvarh = th_vars
			, gs_varh = hp_var_heap
			, gs_genh = hp_generic_heap
			, gs_exprh = hp_expression_heap
		}
where
	convert_functions fun_index funs st
		| fun_index == size funs 
			= (funs, st)
			# (fun, funs) = funs ! [fun_index]
			# (fun, st) = convert_function fun st
 			# funs = {funs & [fun_index] = fun}
			= convert_functions (inc fun_index) funs st
	where
		convert_function :: !FunDef !(!*Modules, !*Heaps, !*ErrorAdmin) 
						-> (!FunDef,!(!*Modules, !*Heaps, !*ErrorAdmin))
		convert_function fun=:{fun_type=Yes symbol_type, fun_ident, fun_pos} st
			# (has_converted_context, symbol_type, st) = convert_contexts_in_symbol_type fun_ident fun_pos symbol_type st
			| has_converted_context
				# fun = {fun & fun_type = Yes symbol_type}
				= (fun, st)
				= (fun, st)		 
		convert_function fun st
			= (fun, st)

	convert_modules module_index modules dcl_modules st
		| module_index == size modules
			= (modules, dcl_modules, st)
			# (modules, dcl_modules, st) = convert_module module_index modules dcl_modules st
			= convert_modules (inc module_index) modules dcl_modules st
	
	convert_module :: !Index !*Modules !*DclModules (!*Heaps,!*ErrorAdmin)
						 -> (!*Modules,!*DclModules,(!*Heaps,!*ErrorAdmin))
	convert_module module_index modules dcl_modules st
		| inNumberSet module_index gs_used_modules
			#! (common_defs, modules) = modules ! [module_index]
			#! (dcl_module=:{dcl_functions, dcl_common}, dcl_modules) = dcl_modules ! [module_index]
			
			#! (common_defs, modules, st) = convert_common_defs common_defs modules st
			#! (dcl_common, modules, st) = convert_common_defs dcl_common modules st
			#! (dcl_functions, modules, st) = convert_dcl_functions {x\\x<-:dcl_functions} modules st

			# dcl_modules = {dcl_modules & [module_index] = {dcl_module & dcl_functions = dcl_functions, dcl_common = dcl_common}}
			# modules = {modules & [module_index] = common_defs}
			= (modules, dcl_modules, st)
		| otherwise
			= (modules, dcl_modules, st)	
	
	convert_common_defs common_defs=:{com_class_defs,com_member_defs,com_instance_defs,com_cons_defs} modules (heaps, error)
		# (com_class_defs, st) 
			= updateArraySt convert_class {x\\x<-:com_class_defs} (modules, heaps, error)
		# (com_member_defs, st)
			= updateArraySt convert_member {x\\x<-:com_member_defs} st
		# (com_instance_defs, st)
			= updateArraySt convert_instance {x\\x<-:com_instance_defs} st
		# (com_cons_defs, (modules, heaps, error))
			= updateArraySt convert_constructor {x\\x<-:com_cons_defs} st

		# common_defs = { common_defs
			& com_class_defs = com_class_defs
			, com_member_defs = com_member_defs
			, com_instance_defs = com_instance_defs
			, com_cons_defs = com_cons_defs
			}
		= (common_defs, modules, (heaps, error))
	where
		convert_class class_def=:{class_ident, class_pos, class_context} st
			# (ok, class_context, st) = convert_contexts class_ident class_pos class_context st
			| ok 
				# class_def={class_def & class_context = class_context}
				= (class_def, st) 	
				= (class_def, st) 	

		convert_member member_def=:{me_ident, me_pos, me_type} st
			# (ok, me_type, st) = convert_contexts_in_symbol_type me_ident me_pos me_type st
			| ok 
				# member_def={member_def & me_type = me_type}
				= (member_def, st) 	
				= (member_def, st) 	
									
		convert_instance ins=:{ins_type=ins_type=:{it_context}, ins_ident, ins_pos} st
			# (ok, it_context, st) = convert_contexts ins_ident ins_pos it_context st
			| ok 
				# ins={ins & ins_type = {ins_type & it_context = it_context}}
				= (ins, st)
				= (ins, st)

		convert_constructor cons=:{cons_ident,cons_pos,cons_type} st
			# (has_converted_context, cons_type, st) = convert_contexts_in_symbol_type cons_ident cons_pos cons_type st
			| has_converted_context
				= ({cons & cons_type=cons_type}, st)
				= (cons, st)

	convert_dcl_functions dcl_functions modules (heaps, error)
		# (dcl_functions, (modules, heaps, error)) 
			= updateArraySt convert_dcl_function dcl_functions (modules, heaps, error)	
		= (dcl_functions, modules, (heaps, error))
	where
		convert_dcl_function fun=:{ft_type, ft_ident, ft_pos} st
			# (ok, ft_type, st) = convert_contexts_in_symbol_type ft_ident ft_pos ft_type st
			| ok 
				# fun={fun & ft_type = ft_type}
				= (fun, st) 	
				= (fun, st) 	

	convert_contexts_in_symbol_type :: Ident Position !SymbolType !(!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
										 	-> (!Bool,!SymbolType,!(!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	convert_contexts_in_symbol_type fun_ident fun_pos symbol_type=:{st_context,st_args} st
		# (has_converted_context, st_context, st) = convert_contexts fun_ident fun_pos st_context st
		  (has_converted_arg, st_args, st) = convert_contexts_in_args fun_ident fun_pos st_args st
		| has_converted_context || has_converted_arg
			= (True,{symbol_type & st_context=st_context, st_args=st_args}, st)
			= (False,symbol_type, st)

	convert_contexts_in_args :: Ident Position ![AType] !(!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
									 -> (!Bool,![AType],!(!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	convert_contexts_in_args fun_ident fun_pos arg_args=:[arg=:{at_type=TFAC tvs t contexts}:args] st
		# (has_converted_context,contexts,st) = convert_contexts fun_ident fun_pos contexts st
 		# (has_converted_arg,args,st) = convert_contexts_in_args fun_ident fun_pos args st
		| has_converted_context || has_converted_arg
			= (True,[{arg & at_type=TFAC tvs t contexts}:args],st)
			= (False,arg_args,st)
	convert_contexts_in_args fun_ident fun_pos arg_args=:[arg:args] st
		# (has_converted_arg,args,st) = convert_contexts_in_args fun_ident fun_pos args st
		| has_converted_arg
			= (True,[arg:args],st)
			= (False,arg_args,st)
	convert_contexts_in_args fun_ident fun_pos [] st
		= (False,[],st)

	convert_contexts fun_name fun_pos [] st 
		= (False, [], st)
	convert_contexts fun_name fun_pos all_tcs=:[tc:tcs] st
		# (ok1, tc, st) = convert_context fun_name fun_pos tc st
		# (ok2, tcs, st) = convert_contexts fun_name fun_pos tcs st
		| ok1 || ok2
			= (True, [tc:tcs], st) 
			= (False, all_tcs, st)

	convert_context :: !Ident !Position !TypeContext (!*Modules, !*Heaps, !*ErrorAdmin)
		-> (!Bool, !TypeContext, (!*Modules, !*Heaps, !*ErrorAdmin))
	convert_context fun_name fun_pos tc=:{tc_class=TCGeneric gtc=:{gtc_generic, gtc_kind}} (modules, heaps=:{hp_generic_heap}, error)
		# ({gen_info_ptr}, modules) = modules![gtc_generic.glob_module].com_generic_defs.[gtc_generic.glob_object.ds_index]
		# ({gen_classes}, hp_generic_heap) = readPtr gen_info_ptr hp_generic_heap		
		# opt_class_info = lookupGenericClassInfo gtc_kind gen_classes
		# (tc_class, error) = case opt_class_info of 
			No
				# error = reportError fun_name.id_name fun_pos "no generic cases for this kind" error  
				-> (TCGeneric gtc, error)
			Yes class_info
				# clazz =
					{ glob_module = class_info.gci_module
					, glob_object = 
						{ ds_ident = genericIdentToClassIdent gtc_generic.glob_object.ds_ident.id_name gtc_kind 
						, ds_arity = 1
						, ds_index = class_info.gci_class
						}
					}
				// AA HACK: dummy dictionary
				#! {pds_module,pds_def} = gs_predefs.psd_predefs_a.[PD_TypeGenericDict]
				# generic_dict = {gi_module=pds_module, gi_index=pds_def}
				-> (TCGeneric {gtc & gtc_class=clazz, gtc_generic_dict=generic_dict}, error)
		= (True, {tc & tc_class=tc_class}, (modules, {heaps & hp_generic_heap=hp_generic_heap}, error))
	convert_context fun_name fun_pos tc st 
		= (False, tc, st)

//  specialization

specializeGeneric ::
		!GlobalIndex			// generic index
		!GenTypeStruct 			// type to specialize to
		![(TypeVar, TypeVarInfo)] // specialization environment
		!Ident					// generic/generic case
		!Position				// of generic case
		![GenericDependency]
		!{!GenericRepresentationConstructor}
		!GenericInfoPtr
		![Int]
		!Index 					// main_module index
		!PredefinedSymbolsData
		!*SpecializeState
	-> (!Expression,
		!*SpecializeState)
specializeGeneric gen_index type spec_env gen_ident gen_pos gen_deps gen_rep_conses gen_info_ptr initial_g_nums main_module_index predefs st
	#! st & ss_heaps = set_tvs spec_env st.ss_heaps
	#! (expr, st)
		= specialize type gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr initial_g_nums st
	#! st & ss_heaps = clear_tvs spec_env st.ss_heaps
	= (expr, st)
where
	specialize :: GenTypeStruct GlobalIndex Ident [GenericDependency] {!GenericRepresentationConstructor} (Ptr GenericInfo) [Int] *SpecializeState -> *(!Expression,!*SpecializeState)
	specialize (GTSAppCons kind arg_types) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		#! (arg_exprs, st) = specialize_with_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr arg_types g_nums st
		= build_generic_app kind arg_exprs gen_index gen_ident st
	specialize (GTSAppVar tv arg_types) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		#! (arg_exprs, st) = specialize_with_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr arg_types g_nums st
		#! (expr, st) = specialize_type_var tv gen_index g_nums st
		= (expr @ arg_exprs, st)
	specialize (GTSVar tv) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		= specialize_type_var tv gen_index g_nums st
	specialize (GTSArrow x y) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# (arg_exprs, st) = specialize_with_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [x, y] g_nums st
		= build_generic_app (KindArrow [KindConst, KindConst]) arg_exprs gen_index gen_ident st
	specialize (GTSPair x y) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# {grc_ident,grc_generic_instance_deps,grc_index,grc_module,grc_local_fun_index} = gen_rep_conses.[4]
		| grc_module<0
			#! error = reportError gen_ident.id_name gen_pos "cannot specialize because there is no instance of PAIR" st.ss_error
			= (EE, {st & ss_error=error})
		# (fun_module_index,fun_index,gen_rep_conses,st)
			= get_function_or_copied_macro_index grc_index grc_module main_module_index grc_local_fun_index gen_info_ptr 4 gen_rep_conses st
		# (arg_exprs, st)
			= specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [x,y] grc_generic_instance_deps g_nums st
		#! (expr, heaps)
			= buildFunApp2 fun_module_index fun_index grc_ident arg_exprs st.ss_heaps
		= (expr, {st & ss_heaps=heaps})
	specialize (GTSEither x y) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# {grc_ident,grc_generic_instance_deps,grc_index,grc_module,grc_local_fun_index} = gen_rep_conses.[5]
		| grc_module<0
			#! error = reportError gen_ident.id_name gen_pos "cannot specialize because there is no instance of EITHER" st.ss_error
			= (EE, {st & ss_error=error})
		# (fun_module_index,fun_index,gen_rep_conses,st)
			= get_function_or_copied_macro_index grc_index grc_module main_module_index grc_local_fun_index gen_info_ptr 5 gen_rep_conses st
		# (arg_exprs, st)
			= specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [x,y] grc_generic_instance_deps g_nums st
		#! (expr, heaps)
			= buildFunApp2 fun_module_index fun_index grc_ident arg_exprs st.ss_heaps
		= (expr, {st & ss_heaps=heaps})
	specialize (GTSCons cons_info_ds cons_index type_def_info gen_type_ds arg_type) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# {grc_ident,grc_generic_info,grc_generic_instance_deps,grc_index,grc_module,grc_local_fun_index} = gen_rep_conses.[1]
		| grc_module<0
			#! error = reportError gen_ident.id_name gen_pos "cannot specialize because there is no instance of CONS" st.ss_error
			= (EE, {st & ss_error=error})
		# (fun_module_index,fun_index,gen_rep_conses,st)
			= get_function_or_copied_macro_index grc_index grc_module main_module_index grc_local_fun_index gen_info_ptr 1 gen_rep_conses st
		# (arg_exprs, st)
			= specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [arg_type] grc_generic_instance_deps g_nums st
		# (arg_exprs,st)
			= case grc_generic_info of
				0
					-> (arg_exprs,st)
				-1 
					#! (generic_info_expr, heaps) = buildFunApp main_module_index cons_info_ds [] st.ss_heaps
					-> ([generic_info_expr:arg_exprs],{st & ss_heaps=heaps})
				_
					# (cons_def, modules) = (st.ss_modules)![cons_index.gi_module].com_cons_defs.[cons_index.gi_index]
					# (arg_exprs,heaps) = add_CONS_info_args grc_generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs st.ss_heaps
					-> (arg_exprs,{st & ss_modules=modules,ss_heaps=heaps})
		#! (expr, heaps)
			= buildFunApp2 fun_module_index fun_index grc_ident arg_exprs st.ss_heaps
		= (expr, {st & ss_heaps=heaps})
	specialize (GTSRecord record_info_ds type_index gen_type_ds field_list_ds arg_type) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# {grc_ident,grc_generic_info,grc_generic_instance_deps,grc_index,grc_module,grc_local_fun_index} = gen_rep_conses.[2]
		| grc_module<0
			#! error = reportError gen_ident.id_name gen_pos "cannot specialize because there is no instance of RECORD" st.ss_error
			= (EE, {st & ss_error=error})
		# (fun_module_index,fun_index,gen_rep_conses,st)
			= get_function_or_copied_macro_index grc_index grc_module main_module_index grc_local_fun_index gen_info_ptr 2 gen_rep_conses st
		# (arg_exprs, st)
			= specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [arg_type] grc_generic_instance_deps g_nums st
		# (arg_exprs,st)
			= case grc_generic_info of
				0
					-> (arg_exprs,st)
				-1 
					#! (generic_info_expr, heaps) = buildFunApp main_module_index record_info_ds [] st.ss_heaps
					-> ([generic_info_expr:arg_exprs],{st & ss_heaps=heaps})
				_
					# (type_def, modules) = (st.ss_modules)![type_index.gi_module].com_type_defs.[type_index.gi_index]
					# (arg_exprs,modules,heaps) = add_RECORD_info_args grc_generic_info type_def gen_type_ds field_list_ds type_index.gi_module arg_exprs main_module_index modules st.ss_heaps
					-> (arg_exprs,{st & ss_modules=modules,ss_heaps=heaps})
		#! (expr, heaps)
			= buildFunApp2 fun_module_index fun_index grc_ident arg_exprs st.ss_heaps
		= (expr, {st & ss_heaps=heaps})
	specialize (GTSField field_info_ds field_index record_info_ds arg_type) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# {grc_ident,grc_generic_info,grc_generic_instance_deps,grc_index,grc_module,grc_local_fun_index} = gen_rep_conses.[3]
		| grc_module<0
			#! error = reportError gen_ident.id_name gen_pos "cannot specialize because there is no instance of FIELD" st.ss_error
			= (EE, {st & ss_error=error})
		# (fun_module_index,fun_index,gen_rep_conses,st)
			= get_function_or_copied_macro_index grc_index grc_module main_module_index grc_local_fun_index gen_info_ptr 3 gen_rep_conses st
		# (arg_exprs, st)
			= specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [arg_type] grc_generic_instance_deps g_nums st
		# (arg_exprs,st)
			= case grc_generic_info of
				0
					-> (arg_exprs,st)
				-1 
					#! (generic_info_expr, heaps) = buildFunApp main_module_index field_info_ds [] st.ss_heaps
					-> ([generic_info_expr:arg_exprs],{st & ss_heaps=heaps})
				_
					# (field_def, modules) = (st.ss_modules)![field_index.gi_module].com_selector_defs.[field_index.gi_index]
					# (arg_exprs,heaps) = add_FIELD_info_args grc_generic_info field_def record_info_ds arg_exprs main_module_index st.ss_heaps
					-> (arg_exprs,{st & ss_modules=modules,ss_heaps=heaps})
		#! (expr, heaps)
			= buildFunApp2 fun_module_index fun_index grc_ident arg_exprs st.ss_heaps
		= (expr, {st & ss_heaps=heaps})
	specialize (GTSObject type_info_ds type_index cons_desc_list_ds arg_type) gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# {grc_ident,grc_generic_info,grc_generic_instance_deps,grc_index,grc_module,grc_local_fun_index} = gen_rep_conses.[0]
		| grc_module<0
			#! error = reportError gen_ident.id_name gen_pos "cannot specialize because there is no instance of OBJECT" st.ss_error
			= (EE, {st & ss_error=error})
		# (fun_module_index,fun_index,gen_rep_conses,st)
			= get_function_or_copied_macro_index grc_index grc_module main_module_index grc_local_fun_index gen_info_ptr 0 gen_rep_conses st
		# (arg_exprs, st)
			= specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [arg_type] grc_generic_instance_deps g_nums st
		# (arg_exprs,st)
			= case grc_generic_info of
				0
					-> (arg_exprs,st)
				-1 
					#! (generic_info_expr, heaps) = buildFunApp main_module_index type_info_ds [] st.ss_heaps
					-> ([generic_info_expr:arg_exprs],{st & ss_heaps=heaps})
				_
					# (type_def, modules) = (st.ss_modules)![type_index.gi_module].com_type_defs.[type_index.gi_index]
					  (arg_exprs,heaps) = add_OBJECT_info_args grc_generic_info type_def cons_desc_list_ds arg_exprs main_module_index st.ss_heaps
					-> (arg_exprs,{st & ss_modules=modules,ss_heaps=heaps})
		#! (expr, heaps)
			= buildFunApp2 fun_module_index fun_index grc_ident arg_exprs st.ss_heaps
		= (expr, {st & ss_heaps=heaps})
	specialize GTSUnit gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		# {grc_ident,grc_generic_instance_deps,grc_index,grc_module,grc_local_fun_index} = gen_rep_conses.[6]
		| grc_module<0
			#! error = reportError gen_ident.id_name gen_pos "cannot specialize because there is no instance of UNIT" st.ss_error
			= (EE, {st & ss_error=error})
		# (fun_module_index,fun_index,gen_rep_conses,st)
			= get_function_or_copied_macro_index grc_index grc_module main_module_index grc_local_fun_index gen_info_ptr 6 gen_rep_conses st
		# (arg_exprs, st)
			= specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr [] grc_generic_instance_deps g_nums st
		#! (expr, heaps)
			= buildFunApp2 fun_module_index fun_index grc_ident arg_exprs st.ss_heaps
		= (expr, {st & ss_heaps=heaps})
	specialize type gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr g_nums st
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " st.ss_error
		= (EE, {st & ss_error=error})

	specialize_type_var {tv_info_ptr} gen_index g_nums st=:{ss_heaps=heaps=:{hp_type_heaps=th=:{th_vars}}}
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps & hp_type_heaps = {th & th_vars = th_vars}
		= case expr of
			TVI_Exprs exprs
				# (argExpr, error) = lookupArgExpr gen_index g_nums exprs st.ss_error
				-> (argExpr, {st & ss_heaps=heaps,ss_error=error})
	where
		lookupArgExpr x g_nums [((k,gen_var_nums),v):kvs] error
			| k==x && g_nums==gen_var_nums
				= (v, error)
				= lookupArgExpr x g_nums kvs error
		lookupArgExpr _ _ [] error
			= (undef, reportError gen_ident.id_name gen_pos "missing dependencies of its dependencies in the type signature" error)

	specialize_with_deps :: GlobalIndex Ident [GenericDependency] {!GenericRepresentationConstructor} (Ptr GenericInfo) [GenTypeStruct] [Int] *SpecializeState
							-> *(![Expression],!*SpecializeState)
	specialize_with_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr xs g_nums st
		# (info_deps, st) = collect_dependency_infos gen_deps g_nums st
		# info_self = (gen_index, gen_ident, gen_deps, gen_rep_conses, gen_info_ptr, g_nums)
		# arg_and_deps = make_arg_and_deps xs info_self info_deps
		= specialize_arg_and_deps arg_and_deps st

	specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr xs (GenericInstanceDependencies _ deps) g_nums st
		# (info_deps, st) = collect_dependency_infos gen_deps g_nums st
		# info_self = (gen_index, gen_ident, gen_deps, gen_rep_conses, gen_info_ptr, g_nums)
		# arg_and_deps = make_arg_and_deps xs info_self info_deps
		# arg_and_deps = [arg_and_dep \\ arg_and_dep<-arg_and_deps & dep_n<-[0..] | deps bitand (1<<dep_n)<>0]
		= specialize_arg_and_deps arg_and_deps st
	specialize_with_partial_or_all_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr xs _ g_nums st
		= specialize_with_deps gen_index gen_ident gen_deps gen_rep_conses gen_info_ptr xs g_nums st

	make_arg_and_deps xs info_self info_deps
		# info_self_deps = [info_self : info_deps]
		= [(arg,info_self_dep) \\ arg <- xs, info_self_dep <- info_self_deps]

	specialize_arg_and_deps arg_and_deps st
		= mapSt specialize_arg_or_dep arg_and_deps st
	where
		specialize_arg_or_dep (type, (gen_index, gen_ident, deps, gen_rep_conses, gen_info_ptr, dep_g_nums)) st
			= specialize type gen_index gen_ident deps gen_rep_conses gen_info_ptr dep_g_nums st

	collect_dependency_infos gen_deps g_nums st
		= mapSt collect_dependency_info gen_deps st
	where
		collect_dependency_info gen_dep=:{gd_index,gd_nums} st=:{ss_modules,ss_heaps}
			# ({gen_ident, gen_deps, gen_info_ptr}, modules) = lookupDependencyDef gen_dep ss_modules
			# ({gen_rep_conses}, generic_heap) = readPtr gen_info_ptr ss_heaps.hp_generic_heap
			# ss_heaps & hp_generic_heap = generic_heap
			# new_gd_nums = [g_nums!!n\\n<-gd_nums];
			= ((gd_index, gen_ident, gen_deps, gen_rep_conses, gen_info_ptr, new_gd_nums), {st & ss_modules=modules, ss_heaps=ss_heaps})

	build_generic_app kind arg_exprs gen_index gen_ident st=:{ss_heaps}
		#! (expr, heaps)
			= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident kind arg_exprs ss_heaps
		= (expr, {st & ss_heaps=heaps})

	get_function_or_copied_macro_index :: !GenericCaseBody !Int !Int !Int !GenericInfoPtr !Int !{!GenericRepresentationConstructor} !*SpecializeState -> (!Int,!Int,!{!GenericRepresentationConstructor},!*SpecializeState)
	get_function_or_copied_macro_index (GCB_FunIndex fun_index) module_index main_module_index local_fun_index gen_info_ptr gen_cons_index gen_rep_conses st
		= (module_index,fun_index,gen_rep_conses,st)
	get_function_or_copied_macro_index (GCB_FunAndMacroIndex _ macro_index) module_index main_module_index local_fun_index gen_info_ptr gen_cons_index gen_rep_conses st
		| local_fun_index>=0
			= (main_module_index,local_fun_index,gen_rep_conses,st)
		# heaps = st.ss_heaps
		  (gen_info=:{gen_rep_conses}, generic_heap) = readPtr gen_info_ptr heaps.hp_generic_heap
		  {grc_local_fun_index,grc_optional_fun_type,grc_generic_info,grc_generic_instance_deps} = gen_rep_conses.[gen_cons_index]
		  st & ss_heaps = {heaps & hp_generic_heap = generic_heap}
		| grc_local_fun_index>=0
			= (main_module_index,grc_local_fun_index,gen_rep_conses,st)
		# (fun_index,st)
			= copy_generic_case_macro module_index macro_index grc_optional_fun_type gen_cons_index grc_generic_info grc_generic_instance_deps main_module_index st
		  gen_rep_conses = {gen_rep_cons\\gen_rep_cons<-:gen_rep_conses}
		  gen_rep_conses & [gen_cons_index].grc_local_fun_index = fun_index
		  heaps = st.ss_heaps
		  generic_heap = writePtr gen_info_ptr {gen_info & gen_rep_conses=gen_rep_conses} heaps.hp_generic_heap
		  st & ss_heaps = {heaps & hp_generic_heap = generic_heap}
		= (main_module_index,fun_index,gen_rep_conses,st)

	copy_generic_case_macro :: !Int !Int !(Optional SymbolType) !Int !Int !GenericInstanceDependencies !Int !*SpecializeState -> (!Int,!*SpecializeState)
	copy_generic_case_macro macro_module_index macro_index optional_fun_type gen_cons_index generic_info generic_instance_deps main_module_index st
		# {ss_heaps=heaps,ss_funs_and_groups=funs_and_groups,ss_error=error,ss_funs=fun_defs,ss_dcl_macros=dcl_macros,ss_symbol_table=symbol_table} = st
		  {fg_fun_index = fun_index, fg_funs=funs, fg_groups=groups, fg_group_index=group_index} = funs_and_groups

		  fun_defs = case funs of
		  				[] -> fun_defs
		  				_  -> arrayPlusRevList fun_defs funs
		  funs = []

		  {hp_var_heap=var_heap,hp_expression_heap=expression_heap} = heaps
		| size fun_defs<>fun_index
			= abort "copy_generic_case_macro: incorrect function index"

		# (reversed_groups,unexpanded_dcl_macros,fun_defs,dcl_macros,var_heap,expression_heap,symbol_table,error)
			= partitionateAndLiftMacro macro_module_index macro_index main_module_index predefs.psd_predefs_a group_index
												 fun_defs dcl_macros var_heap expression_heap symbol_table error

		  (fun_index,fun_defs) = usize fun_defs

		  (macro,dcl_macros) = dcl_macros![macro_module_index,macro_index]

		  macro
		  	= case generic_instance_deps of
		  		GenericInstanceDependencies n_deps deps
		  			# m = (1<<n_deps)-1
		  			| deps bitand m<>m
		  				# {fun_body=TransformedBody {tb_args,tb_rhs}} = macro
						# n_generic_info_args
							= if (generic_info==0) 0 (if (generic_info<0) 1 (add_n_bits generic_info 0))
						  tb_args = remove_unused_dep_args_after_generic_info_args tb_args n_generic_info_args n_deps deps
		  				-> {macro & fun_body = TransformedBody {tb_args=tb_args,tb_rhs=tb_rhs}}
		  			where
		  				remove_unused_dep_args_after_generic_info_args args 0 n_deps deps
		  					= remove_unused_dep_args args 0 n_deps deps
		  				remove_unused_dep_args_after_generic_info_args [arg:args] n_generic_info_args n_deps deps
		  					= [arg : remove_unused_dep_args_after_generic_info_args args (n_generic_info_args-1) n_deps deps]
		 		_
		  			-> macro

		  (fun_def,local_fun_defs,next_fun_index,fun_defs,dcl_macros,var_heap,expression_heap)
			= copy_macro_and_local_functions macro fun_index fun_defs dcl_macros var_heap expression_heap

		  fun_def & fun_info.fi_properties = fun_def.fun_info.fi_properties bitor FI_GenericFun

		  dcl_macros = restore_unexpanded_dcl_macros unexpanded_dcl_macros dcl_macros

		  heaps & hp_var_heap=var_heap,hp_expression_heap=expression_heap

		  (fun_def,heaps)
		  	= case optional_fun_type of
		  		Yes fun_type
					# (fun_type, heaps) = fresh_symbol_type fun_type heaps
					  fun_type_with_generic_info
						= if (generic_info<>0)
							(add_generic_info_to_type fun_type gen_cons_index generic_info predefs)
							fun_type
					  fun_def & fun_type = Yes fun_type_with_generic_info
		  			-> (fun_def,heaps)
		  		No
		  			-> (fun_def,heaps)

		  funs = [fun_def:funs]
		  (funs,groups,group_index) = add_local_macro_functions local_fun_defs (fun_index+1) funs groups group_index

		  groups = [{group_members = [fun_index]}:groups]
		  group_index = group_index+1

		  funs_and_groups & fg_fun_index=next_fun_index, fg_group_index=group_index, fg_funs=funs, fg_groups=groups
		  st & ss_funs_and_groups=funs_and_groups,ss_dcl_macros=dcl_macros,ss_heaps=heaps,ss_error=error,ss_funs=fun_defs,ss_symbol_table=symbol_table
		= (fun_index,st)

add_local_macro_functions [] fun_index funs groups group_index
	= (funs,groups,group_index)
add_local_macro_functions copied_local_functions fun_index funs groups group_index
	# local_functions_sorted_by_group = sortBy less_than_group_number copied_local_functions
	# (groups,group_index,functions_with_numbers) = add_groups local_functions_sorted_by_group groups group_index []
	# sorted_functions_with_numbers = sortBy (\(function_n1,_) (function_n2,_) -> function_n1<function_n2) functions_with_numbers
	# funs = add_functions sorted_functions_with_numbers fun_index funs
	= (funs,groups,group_index)
where
	less_than_group_number (_,{fun_info={fi_group_index=group_n1}}) (_,{fun_info={fi_group_index=group_n2}})
		= group_n1 < group_n2

	add_functions [(function_n,fun_def):sorted_functions_with_numbers] fun_index funs
		| function_n==fun_index
			# fun_def & fun_info.fi_properties = fun_def.fun_info.fi_properties bitor FI_GenericFun
			= add_functions sorted_functions_with_numbers (fun_index+1) [fun_def:funs]
	add_functions [] fun_index funs
		= funs

	add_groups [] groups group_index functions_with_numbers
		= (groups,group_index,functions_with_numbers)
	add_groups [({new_function_n},function=:{fun_info={fi_group_index}}):local_functions_sorted_by_group] groups group_index functions_with_numbers
		# functions_with_numbers = [(new_function_n,{function & fun_info.fi_group_index=group_index}):functions_with_numbers]
		# (group,local_functions_sorted_by_group,functions_with_numbers)
			= add_functions_to_group local_functions_sorted_by_group [new_function_n] fi_group_index functions_with_numbers
		# groups = [{group_members = group}:groups]
		# group_index = group_index+1
		= add_groups local_functions_sorted_by_group groups group_index functions_with_numbers

	add_functions_to_group local_functions_sorted_by_group=:[({new_function_n},function=:{fun_info={fi_group_index}}):remaining_funs] group group_n functions_with_numbers
		| fi_group_index==group_n
			# functions_with_numbers = [(new_function_n,{function & fun_info.fi_group_index=group_index}):functions_with_numbers]
			= add_functions_to_group remaining_funs [new_function_n:group] group_n functions_with_numbers
			= (group,local_functions_sorted_by_group,functions_with_numbers)
	add_functions_to_group [] group group_n functions_with_numbers
		= (group,[],functions_with_numbers)

fresh_symbol_type :: !SymbolType !*Heaps -> (!SymbolType, !*Heaps)	
fresh_symbol_type st heaps=:{hp_type_heaps}
	# (fresh_st, hp_type_heaps) = freshSymbolType st hp_type_heaps
	= (fresh_st, {heaps & hp_type_heaps = hp_type_heaps})	

add_OBJECT_info_args :: Int CheckedTypeDef DefinedSymbol [Expression] Int *Heaps -> (![Expression],*Heaps)
add_OBJECT_info_args generic_info type_def cons_desc_list_ds arg_exprs main_module_index heaps
	| generic_info==0
		= (arg_exprs,heaps)
	| generic_info bitand 1<>0 // gtd_name
		# generic_info = generic_info bitxor 1
		#! gtd_name_expr = makeStringExpr type_def.td_ident.id_name
		# (arg_exprs,heaps) = add_OBJECT_info_args generic_info type_def cons_desc_list_ds arg_exprs main_module_index heaps
		= ([gtd_name_expr : arg_exprs],heaps)
	| generic_info bitand 2<>0 // gtd_arity
		# generic_info = generic_info bitxor 2
		#! gtd_arity_expr = makeIntExpr type_def.td_arity
		# (arg_exprs,heaps) = add_OBJECT_info_args generic_info type_def cons_desc_list_ds arg_exprs main_module_index heaps
		= ([gtd_arity_expr : arg_exprs],heaps)
	| generic_info bitand 4<>0 // gtd_num_conses
		# generic_info = generic_info bitxor 4
		#! gtd_num_conses_expr = makeIntExpr (case type_def.td_rhs of AlgType alts -> length alts; _ -> 0)
		# (arg_exprs,heaps) = add_OBJECT_info_args generic_info type_def cons_desc_list_ds arg_exprs main_module_index heaps
		= ([gtd_num_conses_expr : arg_exprs],heaps)
	| generic_info bitand 8<>0 // gtd_conses
		# generic_info = generic_info bitxor 8
		# (gtd_conses_expr, heaps) = buildFunApp main_module_index cons_desc_list_ds [] heaps
		# (arg_exprs,heaps) = add_OBJECT_info_args generic_info type_def cons_desc_list_ds arg_exprs main_module_index heaps
		= ([gtd_conses_expr : arg_exprs],heaps)

add_CONS_info_args :: Int ConsDef DefinedSymbol DefinedSymbol [Expression] Int PredefinedSymbolsData *Heaps -> (![Expression],!*Heaps)
add_CONS_info_args generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs heaps
	| generic_info==0
		= (arg_exprs,heaps)
	| generic_info bitand 1<>0 // gcd_name
		# generic_info = generic_info bitxor 1
		#! gcd_name_expr = makeStringExpr cons_def.cons_ident.id_name
		# (arg_exprs,heaps) = add_CONS_info_args generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs heaps
		= ([gcd_name_expr : arg_exprs],heaps)
	| generic_info bitand 2<>0 // gcd_arity
		# generic_info = generic_info bitxor 2
		#! gcd_arity_expr = makeIntExpr cons_def.cons_type.st_arity
		# (arg_exprs,heaps) = add_CONS_info_args generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs heaps
		= ([gcd_arity_expr : arg_exprs],heaps)
	| generic_info bitand 4<>0 // gcd_prio
		# generic_info = generic_info bitxor 4
		# (gcd_prio_expr, heaps) = make_prio_expr cons_def.cons_priority predefs heaps
		# (arg_exprs,heaps) = add_CONS_info_args generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs heaps
		= ([gcd_prio_expr : arg_exprs],heaps)
	| generic_info bitand 8<>0 // gcd_type_def
		# generic_info = generic_info bitxor 8
		# (gcd_type_def_expr, heaps) = buildFunApp main_module_index type_def_info [] heaps
		# (arg_exprs,heaps) = add_CONS_info_args generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs heaps
		= ([gcd_type_def_expr : arg_exprs],heaps)
	| generic_info bitand 16<>0 // gcd_type
		# generic_info = generic_info bitxor 16
		# (gcd_type_expr, heaps) = buildFunApp main_module_index gen_type_ds [] heaps
		# (arg_exprs,heaps) = add_CONS_info_args generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs heaps
		= ([gcd_type_expr : arg_exprs],heaps)
	| generic_info bitand 32<>0 // gcd_index
		# generic_info = generic_info bitxor 32
		#! gcd_index_expr = makeIntExpr cons_def.cons_number
		# (arg_exprs,heaps) = add_CONS_info_args generic_info cons_def type_def_info gen_type_ds arg_exprs main_module_index predefs heaps
		= ([gcd_index_expr : arg_exprs],heaps)

add_RECORD_info_args :: Int CheckedTypeDef DefinedSymbol DefinedSymbol Int [Expression] Int *{#CommonDefs} *Heaps -> (![Expression],!*{#CommonDefs},!*Heaps)
add_RECORD_info_args generic_info type_def gen_type_ds field_list_ds type_module arg_exprs main_module_index modules heaps
	| generic_info==0
		= (arg_exprs,modules,heaps)
	| generic_info bitand 1<>0 // grd_name
		# generic_info = generic_info bitxor 1
		#! grd_name_expr = makeStringExpr type_def.td_ident.id_name
		# (arg_exprs,modules,heaps) = add_RECORD_info_args generic_info type_def gen_type_ds field_list_ds type_module arg_exprs main_module_index modules heaps
		= ([grd_name_expr : arg_exprs],modules,heaps)
	| generic_info bitand 2<>0 // grd_arity
		# generic_info = generic_info bitxor 2
		# (RecordType {rt_constructor}) = type_def.td_rhs
		# ({cons_type}, modules) = modules![type_module].com_cons_defs.[rt_constructor.ds_index]
		#! grd_arity_expr = makeIntExpr cons_type.st_arity
		# (arg_exprs,modules,heaps) = add_RECORD_info_args generic_info type_def gen_type_ds field_list_ds type_module arg_exprs main_module_index modules heaps
		= ([grd_arity_expr : arg_exprs],modules,heaps)
	| generic_info bitand 4<>0 // grd_type_arity
		# generic_info = generic_info bitxor 4
		#! grd_type_arity_expr = makeIntExpr type_def.td_arity
		# (arg_exprs,modules,heaps) = add_RECORD_info_args generic_info type_def gen_type_ds field_list_ds type_module arg_exprs main_module_index modules heaps
		= ([grd_type_arity_expr : arg_exprs],modules,heaps)
	| generic_info bitand 8<>0 // grd_type
		# generic_info = generic_info bitxor 8
		# (gen_type_expr, heaps) = buildFunApp main_module_index gen_type_ds [] heaps
		# (arg_exprs,modules,heaps) = add_RECORD_info_args generic_info type_def gen_type_ds field_list_ds type_module arg_exprs main_module_index modules heaps
		= ([gen_type_expr : arg_exprs],modules,heaps)
	| generic_info bitand 16<>0 // grd_fields
		# generic_info = generic_info bitxor 16
		# (gen_type_expr, heaps) = buildFunApp main_module_index field_list_ds [] heaps
		# (arg_exprs,modules,heaps) = add_RECORD_info_args generic_info type_def gen_type_ds field_list_ds type_module arg_exprs main_module_index modules heaps
		= ([gen_type_expr : arg_exprs],modules,heaps)

add_FIELD_info_args :: Int SelectorDef DefinedSymbol [Expression] Int *Heaps -> (![Expression],!*Heaps)
add_FIELD_info_args generic_info field_def record_info_ds arg_exprs main_module_index heaps
	| generic_info==0
		= (arg_exprs,heaps)
	| generic_info bitand 1<>0 // gfd_name
		# generic_info = generic_info bitxor 1
		#! gcd_name_expr = makeStringExpr field_def.sd_ident.id_name
		# (arg_exprs,heaps) = add_FIELD_info_args generic_info field_def record_info_ds arg_exprs main_module_index heaps
		= ([gcd_name_expr : arg_exprs],heaps)
	| generic_info bitand 2<>0 // gfd_index
		# generic_info = generic_info bitxor 2
		#! gcd_arity_expr = makeIntExpr field_def.sd_field_nr
		# (arg_exprs,heaps) = add_FIELD_info_args generic_info field_def record_info_ds arg_exprs main_module_index heaps
		= ([gcd_arity_expr : arg_exprs],heaps)
	| generic_info bitand 4<>0 // gfd_cons
		# generic_info = generic_info bitxor 4
		# (record_info_expr, heaps) = buildFunApp main_module_index record_info_ds [] heaps
		# (arg_exprs,heaps) = add_FIELD_info_args generic_info field_def record_info_ds arg_exprs main_module_index heaps
		= ([record_info_expr : arg_exprs],heaps)

specialize_generic_bimap ::
		!GlobalIndex			// generic index
		!GenTypeStruct 			// type to specialize to
		![(TypeVar, TypeVarInfo)] // specialization environment
		!Ident					// generic/generic case
		!Position				// of generic case
		!Index 					// main_module index
		!PredefinedSymbolsData
		!FunsAndGroups !*Heaps !*ErrorAdmin
	-> (!Expression,
		!FunsAndGroups,!*Heaps,!*ErrorAdmin)
specialize_generic_bimap gen_index type spec_env gen_ident gen_pos main_module_index predefs funs_and_groups heaps error
	#! heaps = set_tvs spec_env heaps
	#! (expr, (funs_and_groups, heaps, error)) 
		= specialize_f type (funs_and_groups, heaps, error)
	#! heaps = clear_tvs spec_env heaps
	= (expr, funs_and_groups, heaps, error)
where
	specialize_f (GTSAppCons KindConst []) (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSAppCons kind arg_types) st
		#! (arg_exprs, st) = specialize_f_args arg_types st
		= build_generic_app kind arg_exprs gen_index gen_ident st
	specialize_f (GTSAppVar tv arg_types) st
		#! (arg_exprs, st) = specialize_f_args arg_types st
		#! (expr, st) = specialize_f_type_var tv st 
		= (expr @ arg_exprs, st)
	specialize_f (GTSVar tv) st
		= specialize_f_type_var tv st
	specialize_f (GTSArrow x y) st=:(_,heaps,_)
		| is_bimap_id x heaps
			#! (y, st) = specialize_f y st
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_from_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
		| is_bimap_id y heaps
			#! (x, st) = specialize_b x st
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_to_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
			#! (x, st) = specialize_b x st 
			#! (y, st) = specialize_f y st 
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSPair x y) st
		#! (x, st) = specialize_f x st 
		#! (y, st) = specialize_f y st 
		# (funs_and_groups, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_PAIR_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSEither x y) st
		#! (x, st) = specialize_f x st 
		#! (y, st) = specialize_f y st 
		# (funs_and_groups, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_EITHER_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f GTSAppConsBimapKindConst (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, heaps, error))
	specialize_f GTSUnit (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSCons1Bimap arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_f arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_CONS_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSRecord1Bimap arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_f arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_RECORD_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSCons _ _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_f arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_CONS_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSRecord _ _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_f arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_RECORD_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSField _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_f arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_FIELD_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f (GTSObject _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_f arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_OBJECT_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_f type (funs_and_groups, heaps, error)
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " error 
		= (EE, (funs_and_groups, heaps, error))

	specialize_f_args [arg_type:arg_types] st
		# (f_arg_expr,st) = specialize_f arg_type st
		  (b_arg_expr,st) = specialize_b arg_type st
		  (arg_exprs,st) = specialize_f_args arg_types st
		= ([f_arg_expr,b_arg_expr:arg_exprs],st)
	specialize_f_args [] st
		= ([],st)

	specialize_b (GTSAppCons KindConst []) (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSAppCons kind arg_types) st
		#! (arg_exprs, st) = specialize_b_args arg_types st
		= build_generic_app kind arg_exprs gen_index gen_ident st
	specialize_b (GTSAppVar tv arg_types) st
		#! (arg_exprs, st) = specialize_b_args arg_types st
		#! (expr, st) = specialize_b_type_var tv st 
		= (expr @ arg_exprs, st)
	specialize_b (GTSVar tv) st
		= specialize_b_type_var tv st
	specialize_b (GTSArrow x y) st=:(_,heaps,_)
		| is_bimap_id x heaps
			#! (y, st) = specialize_b y st
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_from_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
		| is_bimap_id y heaps
			#! (x, st) = specialize_f x st
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_to_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
			#! (x, st) = specialize_f x st 
			#! (y, st) = specialize_b y st 
			# (funs_and_groups, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSPair x y) st
		#! (x, st) = specialize_b x st 
		#! (y, st) = specialize_b y st 
		# (funs_and_groups, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_PAIR_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSEither x y) st
		#! (x, st) = specialize_b x st 
		#! (y, st) = specialize_b y st 
		# (funs_and_groups, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_EITHER_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b GTSAppConsBimapKindConst (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, heaps, error))
	specialize_b GTSUnit (funs_and_groups, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSCons1Bimap arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_b arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_CONS_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSRecord1Bimap arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_b arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_RECORD_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSCons _ _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_b arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_CONS_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSRecord _ _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_b arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_RECORD_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSField _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_b arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_FIELD_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b (GTSObject _ _ _ arg_type) st
		# (arg_expr, (funs_and_groups, heaps, error)) = specialize_b arg_type st
		  (expr, funs_and_groups, heaps)
			= bimap_OBJECT_expression [arg_expr] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, heaps, error))
	specialize_b type (funs_and_groups, heaps, error)
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " error 
		= (EE, (funs_and_groups, heaps, error))

	specialize_b_args [arg_type:arg_types] st
		# (b_arg_expr,st) = specialize_b arg_type st
		  (f_arg_expr,st) = specialize_f arg_type st
		  (arg_exprs,st) = specialize_b_args arg_types st
		= ([b_arg_expr,f_arg_expr:arg_exprs],st)
	specialize_b_args [] st
		= ([],st)

 	specialize_f_type_var tv=:{tv_info_ptr} (funs_and_groups, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ expr _
				-> (expr, (funs_and_groups, heaps, error))
			TVI_Iso to_ds _
				# (expr,heaps) = buildFunApp main_module_index to_ds [] heaps
				-> (expr, (funs_and_groups, heaps, error))

 	specialize_b_type_var tv=:{tv_info_ptr} (funs_and_groups, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ _ expr
				-> (expr, (funs_and_groups, heaps, error))
			TVI_Iso _ from_ds
				# (expr,heaps) = buildFunApp main_module_index from_ds [] heaps
				-> (expr, (funs_and_groups, heaps, error))

	build_generic_app kind arg_exprs gen_index gen_ident (funs_and_groups, heaps, error)
		#! (expr, heaps)
			= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident kind arg_exprs heaps 
		= (expr, (funs_and_groups, heaps, error))

adapt_with_specialized_generic_bimap ::
		!GlobalIndex			// generic index
		!GenTypeStruct 			// type to specialize to
		![(TypeVar, TypeVarInfo)] // specialization environment
		!Ident					// generic/generic case
		!Position				// of generic case
		![Expression]
		!Expression
		!Index 					// main_module index
		!PredefinedSymbolsData
		!FunsAndGroups !*Modules !*Heaps !*ErrorAdmin
	-> (!Expression,
		!FunsAndGroups,!*Modules,!*Heaps,!*ErrorAdmin)
adapt_with_specialized_generic_bimap gen_index type spec_env gen_ident gen_pos arg_exprs specialized_expr main_module_index predefs
		funs_and_groups modules heaps error
	#! heaps = set_tvs spec_env heaps
	#! (adapted_arg_exprs, arg_exprs, type, st)
		= adapt_args arg_exprs type (funs_and_groups, modules, heaps, error)
	#! (body_expr, (funs_and_groups, modules, heaps, error))
		= adapt_result arg_exprs type specialized_expr adapted_arg_exprs st
	# heaps = clear_tvs spec_env heaps
	= (body_expr, funs_and_groups, modules, heaps, error)
where
	adapt_args [arg_expr:arg_exprs] (GTSArrow arg_type args_type) st
		# (adapted_arg_expr,st)
		  	= adapt_arg arg_type arg_expr st
		  (adapted_arg_exprs,arg_exprs,args_type,st)
			= adapt_args arg_exprs args_type st
		= ([adapted_arg_expr:adapted_arg_exprs],arg_exprs,args_type,st)
	adapt_args arg_exprs args_type st
		= ([],arg_exprs,args_type,st)

	adapt_arg arg_type arg_expr st=:(_,_,heaps,_)
		| is_bimap_id arg_type heaps
			= (arg_expr,st)
			= specialize_to_with_arg arg_type arg_expr st

	adapt_result arg_exprs type specialized_expr adapted_arg_exprs st=:(_,_,heaps,_)
		| is_bimap_id type heaps
			= (build_body_expr specialized_expr adapted_arg_exprs arg_exprs,st)
			with
				build_body_expr specialized_expr [] []
					= specialized_expr
				build_body_expr specialized_expr [] original_arg_exprs
					= specialized_expr @ original_arg_exprs
				build_body_expr specialized_expr adapted_arg_exprs []
					= specialized_expr @ adapted_arg_exprs
				build_body_expr specialized_expr adapted_arg_exprs original_arg_exprs
					= specialized_expr @ (adapted_arg_exprs++original_arg_exprs)

			#! specialized_expr_with_adapted_args
				= case adapted_arg_exprs of
					[] -> specialized_expr
					_  -> specialized_expr @ adapted_arg_exprs
			= case arg_exprs of
				[]
					-> specialize_from_with_arg type specialized_expr_with_adapted_args st
				_
					# (adapted_expr,st)
						= specialize_from_with_arg type specialized_expr_with_adapted_args st
					-> (adapted_expr @ arg_exprs, st)

	specialize_to_with_arg (GTSVar tv=:{tv_info_ptr}) arg (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ expr _
				# expr = expr @ [arg]
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso to_ds _
				# (expr,heaps) = buildFunApp main_module_index to_ds [arg] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_to_with_arg (GTSAppConsSimpleType type_symbol_n kind arg_types) arg st
		= bimap_to_simple_type type_symbol_n kind arg_types arg st
	specialize_to_with_arg type arg st
		# (adaptor_expr,st)
			= specialize_to type st
		= (adaptor_expr @ [arg],st)

	specialize_from_with_arg (GTSVar tv=:{tv_info_ptr}) arg (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ _ expr
				# expr = expr @ [arg]
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso _ from_ds
				# (expr,heaps) = buildFunApp main_module_index from_ds [arg] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_from_with_arg (GTSAppConsSimpleType type_symbol_n kind arg_types) arg st
		= bimap_from_simple_type type_symbol_n kind arg_types arg st
	specialize_from_with_arg type arg st
		# (adaptor_expr,st)
			= specialize_from type st
		= (adaptor_expr @ [arg],st)

	specialize_from (GTSArrow (GTSAppCons KindConst []) y) st
		= specialize_from_arrow_arg_id y st
	specialize_from (GTSArrow GTSAppConsBimapKindConst y) st
		= specialize_from_arrow_arg_id y st
	specialize_from (GTSArrow x (GTSAppCons KindConst [])) st
		= specialize_from_arrow_res_id x st
	specialize_from (GTSArrow x GTSAppConsBimapKindConst) st
		= specialize_from_arrow_res_id x st
	specialize_from (GTSArrow (GTSVar {tv_info_ptr=xp}) (GTSVar {tv_info_ptr=yp})) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (x_expr, th_vars) = readPtr xp th_vars
		  (y_expr, th_vars) = readPtr yp th_vars
		  heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		| is_bimap_id_expression x_expr
			# (y,heaps) = build_map_from_tvi_expr y_expr main_module_index predefs heaps
			  (expr, funs_and_groups, heaps)
				= bimap_from_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
		| is_bimap_id_expression y_expr
			# (x,heaps) = build_map_to_tvi_expr x_expr main_module_index predefs heaps
			  (expr, funs_and_groups, heaps)
				= bimap_to_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
			# (x,heaps) = build_map_to_tvi_expr x_expr main_module_index predefs heaps
			  (y,heaps) = build_map_from_tvi_expr y_expr main_module_index predefs heaps
			  (expr, funs_and_groups, heaps)
				= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSArrow (GTSVar {tv_info_ptr}) y) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		#! (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		| is_bimap_id_expression expr
			# st = (funs_and_groups, modules, heaps, error)
			= specialize_from_arrow_arg_id y st
			# (x,heaps) = build_map_to_tvi_expr expr main_module_index predefs heaps
			  (y, (funs_and_groups, modules, heaps, error))
			  	= specialize_from y (funs_and_groups, modules, heaps, error)
			  (expr, funs_and_groups, heaps)
				= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSArrow x (GTSVar {tv_info_ptr})) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		#! (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		| is_bimap_id_expression expr
			# st = (funs_and_groups, modules, heaps, error)
			= specialize_from_arrow_res_id x st
			# (y,heaps) = build_map_from_tvi_expr expr main_module_index predefs heaps
			  (x, (funs_and_groups, modules, heaps, error))
			  	= specialize_to x (funs_and_groups, modules, heaps, error)
			  (expr, funs_and_groups, heaps)
				= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSArrow x y) st
		#! (x, st) = specialize_to x st
		#! (y, st) = specialize_from y st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize_from (GTSVar tv=:{tv_info_ptr}) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ _ expr
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso _ from_ds
				# (expr,heaps) = buildFunApp main_module_index from_ds [] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_from type (funs_and_groups, modules, heaps, error)
		= specialize_a_b type (funs_and_groups, modules, heaps, error)

	specialize_from_arrow_arg_id y st
		#! (y, st) = specialize_from y st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_from_expression [y] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, modules, heaps, error))

	specialize_from_arrow_res_id x st
		#! (x, st) = specialize_to x st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, funs_and_groups, heaps)
			= bimap_to_expression [x] main_module_index predefs funs_and_groups heaps
		= (expr, (funs_and_groups, modules, heaps, error))

	specialize_to (GTSVar tv=:{tv_info_ptr}) (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ expr _
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso to_ds _
				# (expr,heaps) = buildFunApp main_module_index to_ds [] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))
	specialize_to type (funs_and_groups, modules, heaps, error)
		= specialize_a_f type (funs_and_groups, modules, heaps, error)

	specialize_a_f (GTSAppCons KindConst []) (funs_and_groups, modules, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, modules, heaps, error))
	specialize_a_f (GTSAppCons kind arg_types) st
		#! (arg_exprs, st) = specialize_a_f_args arg_types st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, heaps)
		  	= build_generic_app kind arg_exprs gen_index gen_ident heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize_a_f (GTSAppConsSimpleType _ kind arg_types) st
		#! (arg_exprs, st) = specialize_a_f_args arg_types st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, heaps)
		  	= build_generic_app kind arg_exprs gen_index gen_ident heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize_a_f (GTSAppVar tv arg_types) st
		#! (arg_exprs, st) = specialize_a_f_args arg_types st
		#! (expr, st) = specialize_a_f_type_var tv st 
		= (expr @ arg_exprs, st)
	specialize_a_f (GTSVar tv) st
		= specialize_a_f_type_var tv st
	specialize_a_f (GTSArrow x y) st=:(_,_,heaps,_)
		| is_bimap_id x heaps
			#! (y, st) = specialize_a_f y st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_from_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
		| is_bimap_id y heaps
			#! (x, st) = specialize_a_b x st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_to_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
			#! (x, st) = specialize_a_b x st
			#! (y, st) = specialize_a_f y st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_a_f GTSAppConsBimapKindConst (funs_and_groups, modules, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, modules, heaps, error))
	specialize_a_f type (funs_and_groups, modules, heaps, error)
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " error 
		= (EE, (funs_and_groups, modules, heaps, error))

	specialize_a_f_args [arg_type:arg_types] st
		# (f_expr_arg,st) = specialize_a_f arg_type st
		  (b_expr_arg,st) = specialize_a_b arg_type st
		  (expr_args,st) = specialize_a_f_args arg_types st
		= ([f_expr_arg,b_expr_arg:expr_args],st)
	specialize_a_f_args [] st
		= ([],st)

	specialize_a_b (GTSAppCons KindConst []) (funs_and_groups, modules, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, modules, heaps, error))
	specialize_a_b (GTSAppCons kind arg_types) st
		#! (arg_exprs, st) = specialize_a_b_args arg_types st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, heaps)
		  	= build_generic_app kind arg_exprs gen_index gen_ident heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize_a_b (GTSAppConsSimpleType _ kind arg_types) st
		#! (arg_exprs, st) = specialize_a_b_args arg_types st
		# (funs_and_groups, modules, heaps, error) = st
		  (expr, heaps)
		  	= build_generic_app kind arg_exprs gen_index gen_ident heaps
		= (expr, (funs_and_groups, modules, heaps, error))
	specialize_a_b (GTSAppVar tv arg_types) st
		#! (arg_exprs, st) = specialize_a_b_args arg_types st
		#! (expr, st) = specialize_a_b_type_var tv st 
		= (expr @ arg_exprs, st)
	specialize_a_b (GTSVar tv) st
		= specialize_a_b_type_var tv st
	specialize_a_b (GTSArrow x y) st=:(_,_,heaps,_)
		| is_bimap_id x heaps
			#! (y, st) = specialize_a_b y st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_from_expression [y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
		| is_bimap_id y heaps
			#! (x, st) = specialize_a_f x st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_to_expression [x] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
			#! (x, st) = specialize_a_f x st
			#! (y, st) = specialize_a_b y st
			# (funs_and_groups, modules, heaps, error) = st
			  (expr, funs_and_groups, heaps)
				= bimap_tofrom_expression [x,y] main_module_index predefs funs_and_groups heaps
			= (expr, (funs_and_groups, modules, heaps, error))
	specialize_a_b GTSAppConsBimapKindConst (funs_and_groups, modules, heaps, error)
		# (expr, funs_and_groups, heaps) = bimap_id_expression main_module_index predefs funs_and_groups heaps
		= (expr ,(funs_and_groups, modules, heaps, error))
	specialize_a_b type (funs_and_groups, modules, heaps, error)
		#! error = reportError gen_ident.id_name gen_pos "cannot specialize " error 
		= (EE, (funs_and_groups, modules, heaps, error))

	specialize_a_b_args [arg_type:arg_types] st
		# (b_expr_arg,st) = specialize_a_b arg_type st
		  (f_expr_arg,st) = specialize_a_f arg_type st
		  (expr_args,st) = specialize_a_b_args arg_types st
		= ([b_expr_arg,f_expr_arg:expr_args],st)
	specialize_a_b_args [] st
		= ([],st)

	specialize_a_f_type_var tv=:{tv_info_ptr} (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)		
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ expr _
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso to_ds _
				# (expr,heaps) = buildFunApp main_module_index to_ds [] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))

	specialize_a_b_type_var tv=:{tv_info_ptr} (funs_and_groups, modules, heaps=:{hp_type_heaps=th=:{th_vars}}, error)		
		# (expr, th_vars) = readPtr tv_info_ptr th_vars
		# heaps = {heaps & hp_type_heaps = {th & th_vars = th_vars}}
		= case expr of
			TVI_BimapExpr _ _ expr
				-> (expr, (funs_and_groups, modules, heaps, error))
			TVI_Iso _ from_ds
				# (expr,heaps) = buildFunApp main_module_index from_ds [] heaps
				-> (expr, (funs_and_groups, modules, heaps, error))

	build_generic_app kind arg_exprs gen_index gen_ident heaps
		= buildGenericApp gen_index.gi_module gen_index.gi_index gen_ident kind arg_exprs heaps 

	bimap_to_simple_type :: !GlobalIndex !TypeKind ![GenTypeStruct] !Expression !*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
															   -> *(!Expression,!*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	bimap_to_simple_type global_type_def_index=:{gi_module} (KindArrow kinds) arg_types arg (funs_and_groups,modules,heaps,error)
		# (alts,constructors_arg_types,modules,heaps)
			= determine_constructors_arg_types global_type_def_index arg_types modules heaps
		# (alg_patterns,funs_and_groups,modules,heaps,error)
			= build_to_alg_patterns alts constructors_arg_types gi_module funs_and_groups modules heaps error
/*
		= build_bimap_case global_type_def_index arg alg_patterns funs_and_groups modules heaps error
*/
		# (arg_expr, arg_var, heaps) = buildVarExpr "x" heaps 

		# (case_expr,heaps)
			= build_bimap_case global_type_def_index arg_expr alg_patterns heaps

		# (def_sym, funs_and_groups)
			= buildFunAndGroup (makeIdent "bimapToGeneric") [arg_var] case_expr No main_module_index NoPos funs_and_groups
		# (app_expr, heaps) = buildFunApp main_module_index def_sym [arg] heaps
		= (app_expr,(funs_and_groups,modules,heaps,error))
	where
		build_to_alg_patterns [cons_ds=:{ds_ident,ds_index,ds_arity}:alts] [constructor_arg_types:constructors_arg_types] type_module_n funs_and_groups modules heaps error
			# arg_names = ["x" +++ toString k \\ k <- [1..ds_arity]]
			# (var_exprs, vars, heaps) = buildVarExprs arg_names heaps
			# (args,(funs_and_groups,modules,heaps,error))
				= specialize_to_with_args constructor_arg_types var_exprs (funs_and_groups,modules,heaps,error)
			# (alg_pattern,heaps)
				= build_alg_pattern cons_ds vars args type_module_n heaps
			# (alg_patterns,funs_and_groups,modules,heaps,error)
				= build_to_alg_patterns alts constructors_arg_types type_module_n funs_and_groups modules heaps error
			= ([alg_pattern:alg_patterns],funs_and_groups,modules,heaps,error)
		build_to_alg_patterns [] [] type_module_n funs_and_groups modules heaps error
			= ([],funs_and_groups,modules,heaps,error)
	
		specialize_to_with_args [type:types] [arg:args] st=:(_,_,heaps,_)
			| is_bimap_id type heaps
				# (args,st)
					= specialize_to_with_args types args st
				= ([arg:args],st)
				# (arg,st)
					= specialize_to_with_arg type arg st
				# (args,st)
					= specialize_to_with_args types args st
				= ([arg:args],st)
		specialize_to_with_args [] [] st
			= ([],st)

	bimap_from_simple_type :: !GlobalIndex !TypeKind ![GenTypeStruct] !Expression !*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin)
																 -> *(!Expression,!*(!FunsAndGroups,!*{#CommonDefs},!*Heaps,!*ErrorAdmin))
	bimap_from_simple_type global_type_def_index=:{gi_module} (KindArrow kinds) arg_types arg (funs_and_groups,modules,heaps,error)
		# (alts,constructors_arg_types,modules,heaps)
			= determine_constructors_arg_types global_type_def_index arg_types modules heaps
		# (alg_patterns,funs_and_groups,modules,heaps,error)
			= build_from_alg_patterns alts constructors_arg_types gi_module funs_and_groups modules heaps error
/*
		= build_bimap_case global_type_def_index arg alg_patterns funs_and_groups modules heaps error
*/
		# (arg_expr, arg_var, heaps) = buildVarExpr "x" heaps 

		# (case_expr,heaps)
			= build_bimap_case global_type_def_index arg_expr alg_patterns heaps

		# (def_sym, funs_and_groups)
			= buildFunAndGroup (makeIdent "bimapFromGeneric") [arg_var] case_expr No main_module_index NoPos funs_and_groups
		# (app_expr, heaps) = buildFunApp main_module_index def_sym [arg] heaps
		= (app_expr,(funs_and_groups,modules,heaps,error))
	where
		build_from_alg_patterns [cons_ds=:{ds_ident,ds_index,ds_arity}:alts] [constructor_arg_types:constructors_arg_types] type_module_n funs_and_groups modules heaps error
			# arg_names = ["x" +++ toString k \\ k <- [1..ds_arity]]
			# (var_exprs, vars, heaps) = buildVarExprs arg_names heaps
			# (args,(funs_and_groups,modules,heaps,error))
				= specialize_from_with_args constructor_arg_types var_exprs (funs_and_groups,modules,heaps,error)
			# (alg_pattern,heaps)
				= build_alg_pattern cons_ds vars args type_module_n heaps
			# (alg_patterns,funs_and_groups,modules,heaps,error)
				= build_from_alg_patterns alts constructors_arg_types type_module_n funs_and_groups modules heaps error
			= ([alg_pattern:alg_patterns],funs_and_groups,modules,heaps,error)
		build_from_alg_patterns [] [] type_module_n funs_and_groups modules heaps error
			= ([],funs_and_groups,modules,heaps,error)

		specialize_from_with_args [type:types] [arg:args] st=:(_,_,heaps,_)
			| is_bimap_id type heaps
				# (args,st)
					= specialize_from_with_args types args st
				= ([arg:args],st)
				# (arg,st)
					= specialize_from_with_arg type arg st
				# (args,st)
					= specialize_from_with_args types args st
				= ([arg:args],st)
		specialize_from_with_args [] [] st
			= ([],st)

	determine_constructors_arg_types :: !GlobalIndex ![GenTypeStruct] !*Modules !*Heaps
							  -> (![DefinedSymbol],![[GenTypeStruct]],!*Modules,!*Heaps)
	determine_constructors_arg_types {gi_module,gi_index} arg_types modules heaps
		# ({td_args,td_rhs=AlgType alts},modules) = modules![gi_module].com_type_defs.[gi_index]

		# {hp_type_heaps} = heaps
		# th_vars = number_type_arguments td_args 0 hp_type_heaps.th_vars
		# arg_types_a = {!arg_type\\arg_type<-arg_types}
		# (constructors_arg_types,modules,th_vars)
			= compute_constructors_arg_types alts gi_module arg_types_a modules th_vars
		# th_vars = remove_type_argument_numbers td_args th_vars
		# heaps = {heaps & hp_type_heaps={hp_type_heaps & th_vars=th_vars}}
		= (alts,constructors_arg_types,modules,heaps)
	where
		compute_constructors_arg_types :: ![DefinedSymbol] !Int !{!GenTypeStruct} !*Modules !*TypeVarHeap
														   -> (![[GenTypeStruct]],!*Modules,!*TypeVarHeap)
		compute_constructors_arg_types [cons_ds=:{ds_ident,ds_index}:alts] type_module_n arg_types_a modules th_vars
			# ({cons_type={st_args}},modules) = modules![type_module_n].com_cons_defs.[ds_index]
			# (constructor_arg_numbers,th_vars)
				= compute_constructor_arg_types st_args arg_types_a th_vars
			# (constructors_arg_numbers,modules,th_vars)
				= compute_constructors_arg_types alts type_module_n arg_types_a modules th_vars
			= ([constructor_arg_numbers:constructors_arg_numbers],modules,th_vars)
		compute_constructors_arg_types [] type_module_n arg_types_a modules th_vars
			= ([],modules,th_vars)

		compute_constructor_arg_types :: ![AType] !{!GenTypeStruct} !*TypeVarHeap -> (![GenTypeStruct],!*TypeVarHeap)
		compute_constructor_arg_types [{at_type=TV {tv_info_ptr}}:atypes] arg_types_a th_vars
			# (TVI_GenTypeVarNumber constructor_arg_number,th_vars)
				= readPtr tv_info_ptr th_vars
			#! constructor_arg_types = arg_types_a.[constructor_arg_number]
			# (constructors_arg_types,th_vars)
				= compute_constructor_arg_types atypes arg_types_a th_vars
			= ([constructor_arg_types:constructors_arg_types],th_vars);
		compute_constructor_arg_types [] arg_types_a th_vars
			= ([],th_vars)

build_bimap_case :: !GlobalIndex !.Expression ![AlgebraicPattern] !*Heaps -> (!Expression,!*Heaps)
build_bimap_case global_type_def_index arg alg_patterns heaps
	# case_patterns = AlgebraicPatterns global_type_def_index alg_patterns
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty heaps.hp_expression_heap
	# case_expr = Case {case_expr = arg, case_guards = case_patterns, case_default = No, case_ident = No,
						case_info_ptr = expr_info_ptr, case_explicit = True, case_default_pos = NoPos}
	# heaps = {heaps & hp_expression_heap = hp_expression_heap}
	= (case_expr, heaps)

build_alg_pattern :: !DefinedSymbol ![FreeVar] ![Expression] !Int !*Heaps -> (!AlgebraicPattern,!*Heaps)
build_alg_pattern cons_ds=:{ds_ident,ds_index} vars args type_module_n heaps
	# cons_symbol = {glob_module = type_module_n, glob_object = cons_ds}
	# cons_symb_ident = {symb_ident = ds_ident, symb_kind = SK_Constructor {glob_module = type_module_n,glob_object = ds_index}}

	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty heaps.hp_expression_heap
	# expr = App {app_symb = cons_symb_ident, app_args = args, app_info_ptr = expr_info_ptr} 

	#! alg_pattern = { ap_symbol = cons_symbol, ap_vars = vars, ap_expr = expr, ap_position = NoPos }
	# heaps = {heaps & hp_expression_heap = hp_expression_heap}
	= (alg_pattern,heaps)

is_bimap_id :: !GenTypeStruct !Heaps -> Bool
is_bimap_id (GTSAppCons KindConst []) heaps
	= True
is_bimap_id GTSAppConsBimapKindConst heaps
	= True
is_bimap_id (GTSVar {tv_info_ptr}) heaps
	= case sreadPtr tv_info_ptr heaps.hp_type_heaps.th_vars of
		TVI_BimapExpr is_bimap_id _ _
			-> is_bimap_id
		_
			-> False
is_bimap_id _ heaps
	= False

is_bimap_id_expression (TVI_BimapExpr is_bimap_id _ _)
	= is_bimap_id
is_bimap_id_expression _
	= False

set_tvs spec_env heaps=:{hp_type_heaps=hp_type_heaps=:{th_vars}}
	#! th_vars = foldSt write_tv spec_env th_vars
		with write_tv ({tv_info_ptr}, tvi) th_vars
				= writePtr tv_info_ptr tvi th_vars		
	= {heaps & hp_type_heaps = {hp_type_heaps & th_vars = th_vars }}

clear_tvs spec_env heaps=:{hp_type_heaps=hp_type_heaps=:{th_vars}}
	#! th_vars = foldSt write_tv spec_env th_vars
		with write_tv ({tv_info_ptr}, _) th_vars
				= writePtr tv_info_ptr TVI_Empty th_vars		
	= {heaps & hp_type_heaps = {hp_type_heaps & th_vars = th_vars }}

number_type_arguments :: ![ATypeVar] !Int !*TypeVarHeap -> *TypeVarHeap
number_type_arguments [{atv_variable={tv_info_ptr}}:atype_vars] arg_n th_vars
	# th_vars = writePtr tv_info_ptr (TVI_GenTypeVarNumber arg_n) th_vars
	= number_type_arguments atype_vars (arg_n+1) th_vars
number_type_arguments [] arg_n th_vars
	= th_vars

remove_type_argument_numbers :: ![ATypeVar] !*TypeVarHeap -> *TypeVarHeap
remove_type_argument_numbers [{atv_variable={tv_info_ptr}}:atype_vars] th_vars
	# th_vars = writePtr tv_info_ptr TVI_Empty th_vars
	= remove_type_argument_numbers atype_vars th_vars
remove_type_argument_numbers [] th_vars
	= th_vars

bimap_fromto_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_fromto_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/fromto from to f x = from (f (to x))
		# bimap_fromto_ident = makeIdent "bimap/fromto"
		  (from_expr,from_var,heaps) = buildVarExpr "from" heaps 
		  (to_expr,to_var,heaps) = buildVarExpr "to" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [from_var,to_var,f_var,x_var]
		  rhs_expr = from_expr @ [f_expr @ [to_expr @ [x_expr]]]
		  (bimap_fromto_index,funs_and_groups) = buildFunAndGroup2 bimap_fromto_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_fromto_function={fii_index=bimap_fromto_index,fii_ident=bimap_fromto_ident}}
		= (bimap_fromto_index,bimap_fromto_ident,funs_and_groups,heaps)

bimap_tofrom_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_tofrom_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/tofrom to from f x = from (f (to x))
		# bimap_tofrom_ident = makeIdent "bimap/tofrom"
		  (from_expr,from_var,heaps) = buildVarExpr "from" heaps 
		  (to_expr,to_var,heaps) = buildVarExpr "to" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [to_var,from_var,f_var,x_var]
		  rhs_expr = from_expr @ [f_expr @ [to_expr @ [x_expr]]]
		  (bimap_tofrom_index,funs_and_groups) = buildFunAndGroup2 bimap_tofrom_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_tofrom_function={fii_index=bimap_tofrom_index,fii_ident=bimap_tofrom_ident}}
		= (bimap_tofrom_index,bimap_tofrom_ident,funs_and_groups,heaps)

bimap_to_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_to_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/from to f x = f (to x)
		# bimap_to_ident = makeIdent "bimap/to"
		  (to_expr,to_var,heaps) = buildVarExpr "to" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [to_var,f_var,x_var]
		  rhs_expr = f_expr @ [to_expr @ [x_expr]]
		  (bimap_to_index,funs_and_groups) = buildFunAndGroup2 bimap_to_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_to_function={fii_index=bimap_to_index,fii_ident=bimap_to_ident}}
		= (bimap_to_index,bimap_to_ident,funs_and_groups,heaps)

bimap_from_function main_module_index funs_and_groups=:{fg_bimap_functions={bimap_from_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		= (fii_index,fii_ident,funs_and_groups,heaps)
		// bimap/from from f x = from (f x)
		# bimap_from_ident = makeIdent "bimap/from"
		  (from_expr,from_var,heaps) = buildVarExpr "from" heaps 
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps 

		  args = [from_var,f_var,x_var]
		  rhs_expr = from_expr @ [f_expr @ [x_expr]]
		  (bimap_from_index,funs_and_groups) = buildFunAndGroup2 bimap_from_ident args rhs_expr main_module_index funs_and_groups
		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_from_function={fii_index=bimap_from_index,fii_ident=bimap_from_ident}}
		= (bimap_from_index,bimap_from_ident,funs_and_groups,heaps)

bimap_id_expression main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_id_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident [] heaps
		= (expr,funs_and_groups,heaps)
		// bimap/id x = x
		# bimap_id_ident = makeIdent "bimap/id"
		  (arg_expr, arg_var, heaps) = buildVarExpr "x" heaps 
		  (bimap_id_index,funs_and_groups) = buildFunAndGroup2 bimap_id_ident [arg_var] arg_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_id_function={fii_index=bimap_id_index,fii_ident=bimap_id_ident}}

		  (bimap_c_expr,heaps) = buildFunApp2 main_module_index bimap_id_index bimap_id_ident [] heaps
		= (bimap_c_expr,funs_and_groups,heaps)

bimap_PAIR_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_PAIR_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		// bimap/PAIR fx fy (PAIR x y) = PAIR (fx x) (fy y)
		# map_PAIR_ident = makeIdent "bimap/PAIR"
		  (fx_expr,fx_var,heaps) = buildVarExpr "fx" heaps 
		  (fy_expr,fy_var,heaps) = buildVarExpr "fy" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps
		  (y_expr,y_var,heaps) = buildVarExpr "y" heaps

		  (object_expr,heaps) = build_pair (fx_expr @ [x_expr]) (fy_expr @ [y_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_pair x_var y_var object_expr predefs heaps
		  args = [fx_var,fy_var,c_var]
		  (map_PAIR_index,funs_and_groups) = buildFunAndGroup2 map_PAIR_ident args case_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_PAIR_function={fii_index=map_PAIR_index,fii_ident=map_PAIR_ident}}

		  (bimap_PAIR_expr,heaps) = buildFunApp2 main_module_index map_PAIR_index map_PAIR_ident arg_exprs heaps
		= (bimap_PAIR_expr,funs_and_groups,heaps)

bimap_EITHER_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_EITHER_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		// bimap/EITHER lf rf (LEFT l)  = LEFT  (lf l)
		// bimap/EITHER lf rf (RIGHT r) = RIGHT (rf r)
		# map_EITHER_ident = makeIdent "bimap/EITHER"
		  (lf_expr,lf_var,heaps) = buildVarExpr "lf" heaps 
		  (rf_expr,rf_var,heaps) = buildVarExpr "rf" heaps 
		  (l_expr,l_var,heaps) = buildVarExpr "l" heaps
		  (r_expr,r_var,heaps) = buildVarExpr "r" heaps

		  (left_expr,heaps) = build_left (lf_expr @ [l_expr]) predefs heaps
		  (right_expr,heaps) = build_right (rf_expr @ [r_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_either l_var left_expr r_var right_expr predefs heaps

		  args = [lf_var,rf_var,c_var]
		  (map_EITHER_index,funs_and_groups) = buildFunAndGroup2 map_EITHER_ident args case_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_EITHER_function={fii_index=map_EITHER_index,fii_ident=map_EITHER_ident}}

		  (bimap_EITHER_expr,heaps) = buildFunApp2 main_module_index map_EITHER_index map_EITHER_ident arg_exprs heaps
		= (bimap_EITHER_expr,funs_and_groups,heaps)

bimap_OBJECT_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_OBJECT_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		// bimap/OBJECT f (OBJECT x) = OBJECT (f x)
		# map_OBJECT_ident = makeIdent "bimap/OBJECT"
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps

		  (object_expr,heaps) = build_object (f_expr @ [x_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_object x_var object_expr predefs heaps
		  args = [f_var,c_var]
		  (map_OBJECT_index,funs_and_groups) = buildFunAndGroup2 map_OBJECT_ident args case_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_OBJECT_function={fii_index=map_OBJECT_index,fii_ident=map_OBJECT_ident}}

		  (bimap_OBJECT_expr,heaps) = buildFunApp2 main_module_index map_OBJECT_index map_OBJECT_ident arg_exprs heaps
		= (bimap_OBJECT_expr,funs_and_groups,heaps)

bimap_CONS_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_CONS_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		// bimap/CONS f (CONS x) = CONS (f x)
		# map_CONS_ident = makeIdent "bimap/CONS"
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps

		  (cons_expr,heaps) = build_cons (f_expr @ [x_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_cons x_var cons_expr predefs heaps
		  args = [f_var,c_var]
		  (map_CONS_index,funs_and_groups) = buildFunAndGroup2 map_CONS_ident args case_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_CONS_function={fii_index=map_CONS_index,fii_ident=map_CONS_ident}}

		  (bimap_CONS_expr,heaps) = buildFunApp2 main_module_index map_CONS_index map_CONS_ident arg_exprs heaps
		= (bimap_CONS_expr,funs_and_groups,heaps)

bimap_RECORD_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_RECORD_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		// bimap/RECORD f (RECORD x) = RECORD (f x)
		# map_RECORD_ident = makeIdent "bimap/RECORD"
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps

		  (cons_expr,heaps) = build_record (f_expr @ [x_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_record x_var cons_expr predefs heaps
		  args = [f_var,c_var]
		  (map_RECORD_index,funs_and_groups) = buildFunAndGroup2 map_RECORD_ident args case_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_RECORD_function={fii_index=map_RECORD_index,fii_ident=map_RECORD_ident}}

		  (bimap_RECORD_expr,heaps) = buildFunApp2 main_module_index map_RECORD_index map_RECORD_ident arg_exprs heaps
		= (bimap_RECORD_expr,funs_and_groups,heaps)

bimap_FIELD_expression arg_exprs main_module_index predefs funs_and_groups=:{fg_bimap_functions={bimap_FIELD_function={fii_index,fii_ident}}} heaps
	| fii_index>=0
		# (expr,heaps) = buildFunApp2 main_module_index fii_index fii_ident arg_exprs heaps
		= (expr,funs_and_groups,heaps)
		// bimap/FIELD f (FIELD x) = FIELD (f x)
		# map_FIELD_ident = makeIdent "bimap/FIELD"
		  (f_expr,f_var,heaps) = buildVarExpr "f" heaps 
		  (x_expr,x_var,heaps) = buildVarExpr "x" heaps

		  (field_expr,heaps) = build_field (f_expr @ [x_expr]) predefs heaps
		  (case_expr,c_var,heaps) = build_case_field x_var field_expr predefs heaps
		  args = [f_var,c_var]
		  (map_FIELD_index,funs_and_groups) = buildFunAndGroup2 map_FIELD_ident args case_expr main_module_index funs_and_groups

		  funs_and_groups = {funs_and_groups & fg_bimap_functions.bimap_FIELD_function={fii_index=map_FIELD_index,fii_ident=map_FIELD_ident}}

		  (bimap_FIELD_expr,heaps) = buildFunApp2 main_module_index map_FIELD_index map_FIELD_ident arg_exprs heaps
		= (bimap_FIELD_expr,funs_and_groups,heaps)

bimap_tofrom_expression arg_exprs main_module_index predefs funs_and_groups heaps
	# (bimap_fromto_index,bimap_fromto_ident,funs_and_groups,heaps)
		= bimap_tofrom_function main_module_index funs_and_groups heaps
	# (bimap_from_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_fromto_index bimap_fromto_ident arg_exprs heaps
	= (bimap_from_arrow_expr,funs_and_groups,heaps)

bimap_to_expression arg_exprs main_module_index predefs funs_and_groups heaps
	# (bimap_to_index,bimap_to_ident,funs_and_groups,heaps)
		= bimap_to_function main_module_index funs_and_groups heaps
	# (bimap_from_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_to_index bimap_to_ident arg_exprs heaps
	= (bimap_from_arrow_expr,funs_and_groups,heaps)

bimap_from_expression arg_exprs main_module_index predefs funs_and_groups heaps
	# (bimap_from_index,bimap_from_ident,funs_and_groups,heaps)
		= bimap_from_function main_module_index funs_and_groups heaps
	# (bimap_from_arrow_expr,heaps) = buildFunApp2 main_module_index bimap_from_index bimap_from_ident arg_exprs heaps
	= (bimap_from_arrow_expr,funs_and_groups,heaps)

//	kind indexing of generic types

// kind indexing:
// t_{*} a1 ... an 			= t a1 ... an 
// t_{k->l} a1 ... an		= forall b1...bn.(t_k b1 ... bn) -> (t_l (a1 b1) ... (an bn)) 
buildKindIndexedType ::
		!SymbolType			// symbol type to kind-index
		![TypeVar]			// generic type variables
		![GenericDependency]	// generic dependencies
		!TypeKind			// kind index
		!Ident				// name for debugging
		!Position 			// position for debugging
		!*TypeHeaps !*Modules !*ErrorAdmin
	-> (!SymbolType,		// instantiated type
		![ATypeVar],		// fresh generic type variables
		!*TypeHeaps,!*Modules,!*ErrorAdmin)
buildKindIndexedType st gtvs deps kind ident pos th modules error
	#! (fresh_st, gatvs, th) = fresh_generic_type st gtvs th	

	#! (kind_indexed_st, _, (th, modules, error)) = build_symbol_type fresh_st gatvs deps kind ident pos 1 (th, modules, error)
	 	 
	#! th = clearSymbolType kind_indexed_st th
	#! th = clearSymbolType st th				// paranoja
	= (kind_indexed_st, gatvs, th, modules, error)
where
	build_symbol_type ::
			 !SymbolType 	// generic type, 
			 ![ATypeVar]	// attributed generic variables
			 ![GenericDependency]	// generic dependencies
			 !TypeKind 		// kind to specialize to 
			 !Ident		
	 	  	 !Position 	
			 !Int 			// current order (in the sense of the order of the kind)
			 (!*TypeHeaps, !*Modules, !*ErrorAdmin)
		-> ( !SymbolType	// new generic type
			, ![ATypeVar]	// fresh copies of generic variables created for the 
							// generic arguments
			, (!*TypeHeaps, !*Modules, !*ErrorAdmin))
	build_symbol_type st _ _ KindConst _ _ _ (th, modules, error)	
		= (st, [], (th, modules, error))
	build_symbol_type st gatvs deps (KindArrow kinds) ident pos order (th, modules, error)
		| order > 2
			# error = reportError ident.id_name pos "kinds of order higher than 2 are not supported" error
			= (st, [], (th, modules, error))
		
		# (arg_stss, arg_gatvss, (_, th, modules, error))
			= mapY2St (build_arg st gatvs deps ident pos order) kinds (0, th, modules, error)
		# arg_sts = flatten arg_stss

		# (body_st, th) 
			= build_body st gatvs (transpose arg_gatvss) th 

		# num_added_args = length kinds * (length deps + 1)
		# new_st = 
			{ st_vars = removeDup (
					foldr (++) body_st.st_vars [st_vars \\ {st_vars}<-arg_sts])
			, st_attr_vars = removeDup (
					foldr (++) body_st.st_attr_vars [st_attr_vars \\ {st_attr_vars}<-arg_sts])
			, st_args = [st_result \\ {st_result}<-arg_sts] ++ body_st.st_args
			, st_result = body_st.st_result 
			, st_arity = body_st.st_arity + num_added_args
			, st_context = removeDup(
				foldr (++) body_st.st_context [st_context \\ {st_context} <- arg_sts])
			, st_attr_env = removeDup(
				foldr (++) body_st.st_attr_env [st_attr_env \\ {st_attr_env} <- arg_sts])
			, st_args_strictness = insert_n_lazy_values_at_beginning num_added_args body_st.st_args_strictness	 
			}
		= (new_st, flatten arg_gatvss, (th, modules, error))

	build_arg :: 
			!SymbolType 		// current part of the generic type
			![ATypeVar]			// generic type variables with their attrs
			![GenericDependency]		// generic dependencies
	 	 	!Ident
	 	 	!Position
			!Int 				// order
			!TypeKind			// kind corrseponding to the arg
			( !Int				// the argument number
			, !*TypeHeaps, !*Modules, !*ErrorAdmin)				
		->  ( ![SymbolType], [ATypeVar] // fresh symbol type and generic variables 
			,( !Int			// incremented argument number
			 ,!*TypeHeaps, !*Modules, !*ErrorAdmin))
	build_arg st gatvs deps ident pos order kind (arg_num, th, modules, error)		
		#! th = clearSymbolType st th
		# postfix = toString arg_num
		#! (fresh_gatvs, th) = mapSt (create_fresh_gatv postfix) gatvs th 
		#! (th, error) = fold2St (make_subst_gatv pos_and_ident) gatvs fresh_gatvs (th, error)
		#! (new_st, th) = applySubstInSymbolType st th
		#! (new_st, forall_atvs, (th, modules, error)) 
			= build_symbol_type new_st fresh_gatvs deps kind ident pos (inc order) (th, modules, error)	
		#! (curry_st, th)	
			= curryGenericArgType1 new_st ("cur" +++ toString order +++ postfix) th
		#! curry_st = adjust_forall curry_st forall_atvs

		# (curry_dep_sts, arg_num_th_modules_error) = mapSt (build_dependency_arg fresh_gatvs order kind) deps (arg_num+1, th, modules, error)
		= ([curry_st:curry_dep_sts], fresh_gatvs, arg_num_th_modules_error)
	where
		pos_and_ident = (pos,ident)

		build_dependency_arg fresh_gatvs order kind {gd_index, gd_nums} (arg_num, th, modules, error)
			# ({gen_type, gen_vars, gen_deps, gen_ident, gen_pos}, modules) 
				= modules![gd_index.gi_module].com_generic_defs.[gd_index.gi_index]
			# (fresh_dep_st, fresh_dep_gatvs, th) = fresh_generic_type gen_type gen_vars th
			# to_gatvs = map (\num -> fresh_gatvs !! num) gd_nums
			# (th, error) = fold2St (make_subst_gatv pos_and_ident) fresh_dep_gatvs to_gatvs (th, error)
			# (new_dep_st, th) = applySubstInSymbolType fresh_dep_st th
			# (new_dep_st, forall_dep_atvs, (th, modules, error)) 
				= build_symbol_type new_dep_st to_gatvs gen_deps kind gen_ident gen_pos (inc order) (th, modules, error)
			# (curry_dep_st, th) = curryGenericArgType1 new_dep_st ("cur" +++ toString order +++ toString arg_num) th
			# curry_dep_st = adjust_forall curry_dep_st forall_dep_atvs
			= (curry_dep_st, (arg_num+1, th, modules, error))

buildKindIndexedTypeWithPartialDependencies ::
		!SymbolType			// symbol type to kind-index
		![TypeVar]			// generic type variables
		![GenericDependency]	// generic dependencies
		!TypeKind			// kind index
		!Int
		!Ident				// name for debugging
		!Position 			// position for debugging
		!*TypeHeaps !*Modules !*ErrorAdmin
	-> (!SymbolType,		// instantiated type
		![ATypeVar],		// fresh generic type variables
		!*TypeHeaps,!*Modules,!*ErrorAdmin)
// only for kinds of order<=1
buildKindIndexedTypeWithPartialDependencies st gtvs deps kind used_deps ident pos th modules error
	#! (fresh_st, gatvs, th) = fresh_generic_type st gtvs th	

	#! (kind_indexed_st, (th, modules, error)) = build_symbol_type fresh_st gatvs deps kind ident pos (th, modules, error)
	 	 
	#! th = clearSymbolType kind_indexed_st th
	#! th = clearSymbolType st th				// paranoja
	= (kind_indexed_st, gatvs, th, modules, error)
where
	build_symbol_type ::
			 !SymbolType 	// generic type, 
			 ![ATypeVar]	// attributed generic variables
			 ![GenericDependency]	// generic dependencies
			 !TypeKind 		// kind to specialize to 
			 !Ident
	 	  	 !Position
			 (!*TypeHeaps, !*Modules, !*ErrorAdmin)
		-> ( !SymbolType	// new generic type
			, (!*TypeHeaps, !*Modules, !*ErrorAdmin))
	build_symbol_type st _ _ KindConst _ _ (th, modules, error)	
		= (st, (th, modules, error))
	build_symbol_type st gatvs deps (KindArrow kinds) ident pos (th, modules, error)
		# (arg_stss, arg_gatvss, (_, th, modules, error))
			= mapY2St (build_arg st gatvs deps ident pos) kinds (0, th, modules, error)
		# arg_sts = flatten arg_stss

		# (body_st, th) 
			= build_body st gatvs (transpose arg_gatvss) th 

		# num_added_args = length arg_sts
		# new_st = 
			{ st_vars = removeDup (
					foldr (++) body_st.st_vars [st_vars \\ {st_vars}<-arg_sts])
			, st_attr_vars = removeDup (
					foldr (++) body_st.st_attr_vars [st_attr_vars \\ {st_attr_vars}<-arg_sts])
			, st_args = [st_result \\ {st_result}<-arg_sts] ++ body_st.st_args
			, st_result = body_st.st_result 
			, st_arity = body_st.st_arity + num_added_args
			, st_context = removeDup(
				foldr (++) body_st.st_context [st_context \\ {st_context} <- arg_sts])
			, st_attr_env = removeDup(
				foldr (++) body_st.st_attr_env [st_attr_env \\ {st_attr_env} <- arg_sts])
			, st_args_strictness = insert_n_lazy_values_at_beginning num_added_args body_st.st_args_strictness	 
			}
		= (new_st, (th, modules, error))

	build_arg ::
			!SymbolType 		// current part of the generic type
			![ATypeVar]			// generic type variables with their attrs
			![GenericDependency]	// generic dependencies
	 	 	!Ident
	 	 	!Position
			!TypeKind			// kind corrseponding to the arg
			( !Int				// the argument number
			, !*TypeHeaps, !*Modules, !*ErrorAdmin)				
		->  ( ![SymbolType], [ATypeVar] // fresh symbol type and generic variables 
			,( !Int			// incremented argument number
			 ,!*TypeHeaps, !*Modules, !*ErrorAdmin))
	build_arg st gatvs deps ident pos KindConst (arg_num, th, modules, error)
		# postfix = toString arg_num
		| used_deps bitand (1<<arg_num)<>0
			#! th = clearSymbolType st th
			#! (fresh_gatvs, th) = mapSt (create_fresh_gatv postfix) gatvs th 
			#! (th, error) = fold2St (make_subst_gatv pos_and_ident) gatvs fresh_gatvs (th, error)
			#! (new_st, th) = applySubstInSymbolType st th
			#! (curry_st, th)	
				= curryGenericArgType1 new_st ("cur1" +++ postfix) th
			# (curry_dep_sts, arg_num_th_modules_error) = build_dependency_args fresh_gatvs deps (arg_num+1, th, modules, error)
			= ([curry_st:curry_dep_sts], fresh_gatvs, arg_num_th_modules_error)

			#! (fresh_gatvs, th) = mapSt (create_fresh_gatv postfix) gatvs th 
			#! (th, error) = fold2St (make_subst_gatv pos_and_ident) gatvs fresh_gatvs (th, error)
			# (curry_dep_sts, arg_num_th_modules_error) = build_dependency_args fresh_gatvs deps (arg_num+1, th, modules, error)
			= (curry_dep_sts, fresh_gatvs, arg_num_th_modules_error)
	where
		pos_and_ident = (pos,ident)

		build_dependency_args fresh_gatvs [{gd_index, gd_nums}:deps] (arg_num, th, modules, error)
			| used_deps bitand (1<<arg_num)<>0
				# ({gen_type, gen_vars, gen_deps, gen_ident, gen_pos}, modules) 
					= modules![gd_index.gi_module].com_generic_defs.[gd_index.gi_index]
				# (fresh_dep_st, fresh_dep_gatvs, th) = fresh_generic_type gen_type gen_vars th
				# to_gatvs = map (\num -> fresh_gatvs !! num) gd_nums
				# (th, error) = fold2St (make_subst_gatv pos_and_ident) fresh_dep_gatvs to_gatvs (th, error)
				# (new_dep_st, th) = applySubstInSymbolType fresh_dep_st th
				# (curry_dep_st, th) = curryGenericArgType1 new_dep_st ("cur1" +++ toString arg_num) th
				# (dep_args,(arg_num, th, modules, error)) = build_dependency_args fresh_gatvs deps (arg_num+1, th, modules, error)
				= ([curry_dep_st:dep_args], (arg_num, th, modules, error))
				= build_dependency_args fresh_gatvs deps (arg_num+1, th, modules, error)
		build_dependency_args fresh_gatvs [] (arg_num, th, modules, error)
			= ([],(arg_num, th, modules, error))

fresh_generic_type :: SymbolType [b] *TypeHeaps -> (!SymbolType,![ATypeVar],!*TypeHeaps)
fresh_generic_type st gtvs th
	# th = clearSymbolType st th
	# (fresh_st, th) = freshSymbolType st th
	# fresh_gtvs = take (length gtvs) fresh_st.st_vars
	# (gatvs, th) = collectAttrsOfTypeVarsInSymbolType fresh_gtvs fresh_st th		
	= (fresh_st, gatvs, th)

create_fresh_gatv :: {#Char} ATypeVar *TypeHeaps -> (!ATypeVar, !*TypeHeaps)
create_fresh_gatv postfix atv=:{atv_attribute, atv_variable} th=:{th_attrs, th_vars}
    # (fresh_atv_variable, th_vars) = freshTypeVar (postfixIdent atv_variable.tv_ident.id_name postfix) th_vars   
    # (fresh_atv_attribute, th_attrs)
        = case atv_attribute of
			TA_Var {av_ident}
				# (av, th_attrs) = freshAttrVar (postfixIdent av_ident.id_name postfix) th_attrs
				-> (TA_Var av, th_attrs)
			TA_Multi
				-> (TA_Multi, th_attrs)
			TA_Unique
				-> (TA_Unique, th_attrs)
    # new_atv = {atv_variable = fresh_atv_variable, atv_attribute = fresh_atv_attribute}
    # th = {th & th_vars = th_vars, th_attrs = th_attrs}		    
    = (new_atv, th)

make_subst_gatv :: (Position,Ident) ATypeVar ATypeVar (*TypeHeaps, *ErrorAdmin) -> (!*TypeHeaps, !*ErrorAdmin)
make_subst_gatv pos_and_ident atv=:{atv_attribute, atv_variable} gatv=:{atv_attribute=new_atv_attribute, atv_variable=new_atv_variable} (th=:{th_attrs, th_vars}, error)
	# th_vars = make_subst_gtv atv_variable new_atv_variable th_vars
	# (th_attrs, error) = make_subst_attr atv_attribute new_atv_attribute th_attrs error
	# th & th_vars = th_vars, th_attrs = th_attrs
	= (th, error)
where
	make_subst_gtv :: TypeVar TypeVar *TypeVarHeap -> *TypeVarHeap
	make_subst_gtv {tv_info_ptr} new_atv_variable th_vars
        	= writePtr tv_info_ptr (TVI_Type (TV new_atv_variable)) th_vars

	make_subst_attr :: TypeAttribute TypeAttribute *AttrVarHeap *ErrorAdmin -> (!*AttrVarHeap,!*ErrorAdmin)
	make_subst_attr (TA_Var {av_ident, av_info_ptr}) new_atv_attribute=:(TA_Var _) th_attrs error
		= (writePtr av_info_ptr (AVI_Attr new_atv_attribute) th_attrs, error)
	make_subst_attr TA_Multi TA_Multi th_attrs error
		= (th_attrs, error)
	make_subst_attr TA_Unique TA_Unique th_attrs error
		= (th_attrs, error)
	make_subst_attr _ _ th_attrs error
		# (pos,ident) = pos_and_ident
		= (th_attrs, reportError ident.id_name pos ("inconsistency with attributes of a generic dependency") error) 

adjust_forall curry_st [] = curry_st
adjust_forall curry_st=:{st_result} forall_atvs 
	#! st_result = {st_result & at_type = TFA forall_atvs st_result.at_type}
 	= 	{ curry_st 
		& st_result = st_result
		, st_attr_vars
			= curry_st.st_attr_vars -- [av \\ {atv_attribute=TA_Var av} <- forall_atvs]
		, st_vars 
			= curry_st.st_vars -- [atv_variable \\ {atv_variable} <- forall_atvs]
		}

build_body :: !SymbolType ![ATypeVar] ![[ATypeVar]] !*TypeHeaps -> (!SymbolType, !*TypeHeaps)
build_body st gatvs arg_gatvss  th
	# th = clearSymbolType st th
	# th = fold2St subst_gatv gatvs arg_gatvss th
	# (st, th) = applySubstInSymbolType st th 
	//# st = add_propagating_inequalities st gatvs arg_gatvss 
	= (st, th)
where
	subst_gatv gatv=:{atv_variable} arg_gatvs th=:{th_vars}
		#! type_args = [ makeAType (TV atv_variable) atv_attribute 
						\\ {atv_variable, atv_attribute} <- arg_gatvs]
		#! type = (CV atv_variable) :@: type_args
		#! th_vars = writePtr atv_variable.tv_info_ptr (TVI_Type type) th_vars
		= {th & th_vars = th_vars}
	/*	
	add_propagating_inequalities st gatvs arg_gatvss
		# inequalities = zipWith make_inequalities gatvs arg_gatvss 
		= {st & st_attr_env = st.st_attr_env ++ flatten inequalities}
	where
		make_inequalities gatv arg_gatvs 
			= filterOptionals (map (make_inequality gatv) arg_gatvs)
		make_inequality {atv_attribute=TA_Var x} {atv_attribute=TA_Var y} 
			= Yes {ai_offered = x, ai_demanded = y}	// offered <= demanded = outer<=inner = x<=y
		make_inequality _ _
			= No
	*/

reportError name pos msg error=:{ea_file} 
	# ea_file = ea_file <<< "Error " <<< (stringPosition name pos) <<< ":" <<< msg <<< '\n'
	= { error & ea_file = ea_file , ea_ok = False }

reportWarning name pos msg error=:{ea_file}
	# ea_file = ea_file <<< "Warning " <<< (newPosition name pos) <<< ":" <<< msg <<< '\n'
	= { error & ea_file = ea_file }
	
//	Type Helpers

makeAType :: !Type !TypeAttribute -> AType
makeAType type attr = {	at_attribute = attr, at_type = type }

makeATypeVar :: !TypeVar !TypeAttribute -> ATypeVar
makeATypeVar tv attr = {atv_variable = tv, atv_attribute = attr}

//----------------------------------------------------------------------------------------
// folding of a AType, depth first 
//----------------------------------------------------------------------------------------

class foldType t :: (Type  .st -> .st) (AType  .st -> .st) t .st -> .st

instance foldType [a] | foldType a where
	foldType on_type on_atype types st 
		= foldSt (foldType on_type on_atype) types st

instance foldType (a,b) | foldType a & foldType b where
	foldType on_type on_atype (x,y) st 
		= foldType on_type on_atype y (foldType on_type on_atype x st)

instance foldType Type where
	foldType on_type on_atype type st
		# st = fold_type type st
		= on_type type st 
	where
		fold_type (TA type_symb args) st = foldType on_type on_atype args st
		fold_type (TAS type_symb args _) st = foldType on_type on_atype args st
		fold_type (l --> r) st = foldType on_type on_atype (l,r) st
		fold_type (TArrow) st = st
		fold_type (TArrow1 t) st = foldType on_type on_atype t st
		fold_type (_ :@: args) st = foldType on_type on_atype args st
		fold_type (TB _) st = st
		fold_type (TFA tvs type) st = foldType on_type on_atype type st
		fold_type (GTV _) st = st
		fold_type (TV _) st = st		
		fold_type t st = abort "foldType: does not match\n" ---> ("type", t)

instance foldType AType where
	foldType on_type on_atype atype=:{at_type} st 
		# st = foldType on_type on_atype at_type st
		= on_atype atype st 

instance foldType TypeContext where
	foldType on_type on_atype {tc_types} st
		= foldType on_type on_atype tc_types st 

//----------------------------------------------------------------------------------------
// mapping of a AType, depth first 
//----------------------------------------------------------------------------------------
class mapTypeSt type :: 
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type before recursion
	(AType -> u:(.st -> u:(AType, .st))) 		// called on each attributed type before recursion
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type after recursion
	(AType -> u:(.st -> u:(AType, .st))) 		// called on each attributed type after recursion	
	type .st -> u:(type, .st)

mapTypeBeforeSt :: 
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type before recursion
	(AType  -> u:(.st -> u:(AType, .st))) 		// called on each attributed type before recursion
	type .st -> u:(type, .st) | mapTypeSt type
mapTypeBeforeSt on_type_before on_atype_before type st
	= mapTypeSt on_type_before on_atype_before idSt idSt type st
	
mapTypeAfterSt :: 
	(Type  -> u:(.st -> u:(Type, .st))) 			// called on each type after recursion
	(AType  -> u:(.st -> u:(AType, .st))) 		// called on each attributed type after recursion
	type .st -> u:(type, .st) | mapTypeSt type
mapTypeAfterSt on_type_after on_atype_after type st
	= mapTypeSt idSt idSt on_type_after on_atype_after type st

instance mapTypeSt [a] | mapTypeSt a where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after type st
		= mapSt (mapTypeSt on_type_before on_atype_before on_type_after on_atype_after) type st

instance mapTypeSt (a, b) | mapTypeSt a & mapTypeSt b where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after (x, y) st
		#! (x1, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after x st
		#! (y1, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after y st
		= ((x1,y1), st)

instance mapTypeSt Type where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after type st
		#! (type1, st) = on_type_before type st
		#! (type2, st) = map_type type1 st
		#! (type3, st) = on_type_after type2 st
		= (type3, st)
	where
		map_type (TA type_symb_ident args) st 
			#! (args, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after args st
			= (TA type_symb_ident args, st)
		map_type (TAS type_symb_ident args strictness) st 
			#! (args, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after args st
			= (TAS type_symb_ident args strictness, st)
		map_type (l --> r) st 
			#! ((l,r), st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after (l,r) st
			= (l --> r, st)
		map_type TArrow st 	= (TArrow, st)
		map_type (TArrow1 t) st 
			#! (t, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after t st
			= (TArrow1 t, st)
		map_type (cv :@: args) st 
			#! (args, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after args st
			= (cv :@: args, st)
		map_type t=:(TB _) st = (t, st)	
		map_type (TFA tvs type) st 
			#! (type, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after type st
			= (TFA tvs type, st)
		map_type t=:(GTV _) st = (t, st)	
		map_type t=:(TV _) st = (t, st)	
		map_type t st
			= abort "mapTypeSt: type does not match\n" ---> ("type", t)

instance mapTypeSt AType where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after atype st
		#! (atype, st) = on_atype_before atype st 
		#! (at_type, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after atype.at_type st
		= on_atype_after {atype & at_type = at_type} st

instance mapTypeSt TypeContext where
	mapTypeSt on_type_before on_atype_before on_type_after on_atype_after tc=:{tc_types} st
		#! (tc_types, st) = mapTypeSt on_type_before on_atype_before on_type_after on_atype_after tc_types st
		= ({tc&tc_types=tc_types}, st)

// allocate fresh type variable
freshTypeVar :: !Ident  !*TypeVarHeap -> (!TypeVar, !*TypeVarHeap) 
freshTypeVar name th_vars 
	# (info_ptr, th_vars) = newPtr TVI_Empty th_vars
	= ({tv_ident = name, tv_info_ptr = info_ptr}, th_vars)

// allocate fresh attribute variable
freshAttrVar :: !Ident !*AttrVarHeap -> (!AttributeVar, !*AttrVarHeap)
freshAttrVar name th_attrs
	# (info_ptr, th_attrs) = newPtr AVI_Empty th_attrs
	= ({av_ident = name, av_info_ptr = info_ptr}, th_attrs)

// take a fresh copy of a SymbolType	  
freshSymbolType :: 
		!SymbolType			// symbol type to take fresh 
		!*TypeHeaps			// variable storage
	->  ( !SymbolType		// fresh symbol type
		, !*TypeHeaps		// variable storage
		) 
freshSymbolType st th=:{th_vars, th_attrs}
	#! (fresh_st_vars, th_vars) = mapSt subst_type_var st.st_vars th_vars
	#! (fresh_st_attr_vars, th_attrs) = mapSt subst_attr_var st.st_attr_vars th_attrs
	#! th = {th & th_vars = th_vars, th_attrs = th_attrs}

	#! (fresh_st_args, th) 		= fresh_type st.st_args th
	#! (fresh_st_result, th) 	= fresh_type st.st_result th	
	#! (fresh_st_context, th) 	= fresh_type st.st_context th	
	#! (fresh_st_attr_env, th) 	= mapSt fresh_ineq st.st_attr_env th		
			
	#! fresh_st = 
		{ st
		& st_args = fresh_st_args
		, st_result = fresh_st_result
		, st_context = fresh_st_context
		, st_attr_env = fresh_st_attr_env
		, st_vars = fresh_st_vars
		, st_attr_vars = fresh_st_attr_vars 
		}

	#! th = clearSymbolType fresh_st th
	#! th = clearSymbolType st th

	#! th = assertSymbolType fresh_st th
	#! th = assertSymbolType st th

	= (fresh_st, th)
where
	subst_type_var :: !TypeVar !*TypeVarHeap -> (!TypeVar, !*TypeVarHeap)
	subst_type_var tv=:{tv_info_ptr} th_vars
		# (new_ptr, th_vars) = newPtr TVI_Empty th_vars  
		= ({tv & tv_info_ptr=new_ptr}, writePtr tv_info_ptr (TVI_TypeVar new_ptr) th_vars)

	subst_attr_var :: !AttributeVar !*AttrVarHeap -> (!AttributeVar, !*AttrVarHeap)
	subst_attr_var av=:{av_info_ptr} th_attrs
		# (new_ptr, th_attrs) = newPtr AVI_Empty th_attrs  
		= ({av & av_info_ptr = new_ptr}, writePtr av_info_ptr (AVI_AttrVar new_ptr) th_attrs)			

	fresh_type :: type !*TypeHeaps -> (type, !*TypeHeaps) | mapTypeSt type 
	fresh_type t st = mapTypeBeforeSt on_type on_atype t st
		
	on_type (TV tv) th
		#! (tv, th) = on_type_var tv th	
		= (TV tv, th)		 			 
	on_type (GTV tv) th
		#! (tv, th) = on_type_var tv th	
		= (GTV tv, th)
	on_type (CV tv=:{tv_info_ptr} :@: args) th=:{th_vars}
		#! (tv, th) = on_type_var tv th	
		= (CV tv :@: args, th)
	on_type (TFA atvs type) th
		#! (fresh_atvs, th) = mapSt subst_atv atvs th
		// the variables in the type will be substituted by
		// the recursive call of mapType 
		= (TFA fresh_atvs type, th)
	where
		subst_atv atv=:{atv_variable, atv_attribute}  th=:{th_vars, th_attrs} 
			#! (atv_variable, th_vars) = subst_type_var atv_variable th_vars 
			# (atv_attribute, th_attrs) = subst_attr atv_attribute th_attrs
			=	( {atv & atv_variable = atv_variable, atv_attribute = atv_attribute}
				, {th & th_vars = th_vars, th_attrs = th_attrs})
		subst_attr (TA_Var av=:{av_info_ptr}) th_attrs
			# (av_info, th_attrs) = readPtr av_info_ptr th_attrs
			= case av_info of
				AVI_Empty
					# (av, th_attrs) = subst_attr_var av th_attrs
					-> (TA_Var av, th_attrs)
				AVI_AttrVar av_info_ptr
					-> (TA_Var {av & av_info_ptr = av_info_ptr}, th_attrs)						   
		subst_attr TA_Unique th_attrs 
			= (TA_Unique, th_attrs)
		subst_attr TA_Multi th_attrs 
			= (TA_Multi, th_attrs)
	on_type type th 
		= (type, th)

	on_atype atype=:{at_attribute=TA_Var av} th
		#! (fresh_av, th) = on_attr_var av th 
		= ({atype & at_attribute=TA_Var fresh_av}, th)
	on_atype atype th 
		= (atype, th)

	fresh_ineq :: !AttrInequality !*TypeHeaps -> (!AttrInequality, !*TypeHeaps)
	fresh_ineq 	ai=:{ai_demanded,ai_offered} th
		#! (ai_demanded, th) = on_attr_var ai_demanded th
		#! (ai_offered, th) = on_attr_var ai_offered th
		= ({ai & ai_demanded = ai_demanded, ai_offered = ai_offered}, th)
		
	on_type_var tv=:{tv_info_ptr} th=:{th_vars}
		#! (tv_info, th_vars) = readPtr tv_info_ptr th_vars
		#! tv = case tv_info of
			TVI_TypeVar new_ptr -> {tv & tv_info_ptr = new_ptr} 
			_ 					-> abort ("freshSymbolType, invalid tv_info\n" ---> tv_info)
		= (tv, {th & th_vars = th_vars}) 			 

	on_attr_var av=:{av_info_ptr} th=:{th_attrs}
		#! (av_info, th_attrs) = readPtr av_info_ptr th_attrs 
		#! av = case av_info of
			AVI_AttrVar new_ptr -> {av & av_info_ptr = new_ptr} 			 
					//---> ("fresh attr var", av.av_ident, ptrToInt av_info_ptr, ptrToInt new_ptr)			
			_  -> abort ("freshSymbolType, invalid av_info\n" ---> av_info)
		= ( av, {th & th_attrs = th_attrs}) 			 

assertSymbolType :: !SymbolType !*TypeHeaps -> *TypeHeaps
assertSymbolType {st_args, st_result, st_context} th
	= foldType on_type on_atype ((st_args, st_result), st_context) th	
where
	on_type :: !Type !*TypeHeaps -> *TypeHeaps
	on_type (TV tv) th=:{th_vars}
		#! (tv_info, th_vars) = readPtr tv.tv_info_ptr th_vars
		#! th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Empty 	-> th
			_ 			-> (abort "TV  tv_info not empty\n") --->(tv, tv_info)	
	on_type (CV tv :@: _) th=:{th_vars}
		#! (tv_info, th_vars) = readPtr tv.tv_info_ptr th_vars
		#! th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Empty 	-> th
			_ 			-> (abort "CV tv_info not empty\n") --->(tv, tv_info)			
	on_type (TFA atvs type) th=:{th_attrs, th_vars}		
		#! th_attrs = foldSt on_av [av \\ {atv_attribute=TA_Var av} <- atvs] th_attrs
		#! th_vars = foldSt on_tv [atv_variable\\{atv_variable} <- atvs] th_vars
		= {th & th_attrs = th_attrs, th_vars = th_vars }
	where 		
		on_av av th_attrs 
			#! (av_info, th_attrs) = readPtr av.av_info_ptr th_attrs
			= case av_info of
			AVI_Empty	-> th_attrs
			_ ->  (abort "TFA av_info not empty\n") --->(av, av_info)
		on_tv tv th_vars
			#! (tv_info, th_vars) = readPtr tv.tv_info_ptr th_vars
			= case tv_info of
				TVI_Empty 	-> th_vars
				_ 			-> (abort "TFA tv_info not empty\n") --->(tv, tv_info)					
	on_type _ th = th
		
	on_atype :: !AType !*TypeHeaps -> *TypeHeaps
	on_atype {at_attribute=TA_Var av} th=:{th_attrs}
		#! (av_info, th_attrs) = readPtr av.av_info_ptr th_attrs
		#! th = {th & th_attrs = th_attrs}
		= case av_info of
			AVI_Empty	-> th
			_ ->  (abort "av_info not empty\n") --->(av, av_info)
	on_atype _ th = th

				
// build curried type out of SymbolType
buildCurriedType :: ![AType] !AType !TypeAttribute ![AttrInequality] ![AttributeVar] !String !Int !*AttrVarHeap 
	-> (!AType, ![AttrInequality], ![AttributeVar], !Int, !*AttrVarHeap)
buildCurriedType [] type cum_attr attr_env attr_vars attr_var_name attr_store th_attrs 
	= (type, attr_env, attr_vars, attr_store, th_attrs)
buildCurriedType [at=:{at_attribute}] type cum_attr attr_env attr_vars attr_var_name attr_store th_attrs
	# atype = makeAType (at --> type) cum_attr
	= (atype, attr_env, attr_vars, attr_store, th_attrs)
buildCurriedType [at=:{at_attribute}:ats] type cum_attr attr_env attr_vars attr_var_name attr_store th_attrs
	# (next_cum_attr, new_attr_env, attr_vars, attr_store, th_attrs) = combine_attributes at_attribute cum_attr attr_env attr_vars attr_store th_attrs
	  (res_type, attr_env, attr_vars, attr_store, th_attrs) = buildCurriedType ats type next_cum_attr attr_env attr_vars attr_var_name attr_store th_attrs
	# atype = makeAType (at --> res_type) cum_attr  
	= (atype, attr_env, attr_vars, attr_store, th_attrs)
where
	combine_attributes TA_Unique cum_attr attr_env attr_vars attr_store th_attrs
		= (TA_Unique, attr_env, attr_vars, attr_store, th_attrs)
	combine_attributes (TA_Var attr_var) (TA_Var cum_attr_var) attr_env attr_vars attr_store th_attrs
		#! (new_attr_var, th_attrs) 
			= freshAttrVar (makeIdent (attr_var_name +++ toString attr_store)) th_attrs	
		# attr_env = 
			[	{ ai_demanded = cum_attr_var,ai_offered = new_attr_var }
			, 	{ ai_demanded = attr_var, ai_offered = new_attr_var }
			: 	attr_env
			]
		= (	TA_Var new_attr_var, attr_env, [new_attr_var:attr_vars], inc attr_store, th_attrs)
	combine_attributes (TA_Var _) cum_attr attr_env attr_vars attr_store th_attrs
		= (cum_attr, attr_env, attr_vars, attr_store, th_attrs)
	combine_attributes _ (TA_Var cum_attr_var) attr_env attr_vars attr_store th_attrs
		#! (new_attr_var, th_attrs) 
			= freshAttrVar (makeIdent (attr_var_name +++ toString attr_store)) th_attrs		
		# attr_env = [	{ ai_demanded = cum_attr_var,ai_offered = new_attr_var }: attr_env]
		= (	TA_Var new_attr_var, attr_env, [new_attr_var:attr_vars], inc attr_store, th_attrs)
	combine_attributes _ cum_attr attr_env attr_vars attr_store th_attrs
		= (cum_attr, attr_env, attr_vars, attr_store, th_attrs)

// Build curried type out of symbol type.
// Starts with TA_Multi cumulative attribute.
// This is the weakest requirement,
// since we do not know how the generic argument will be used
// in the instance functions. It depends on the instance type. 
/*
curryGenericArgType :: !SymbolType !String !*TypeHeaps 
	-> (!SymbolType, !*TypeHeaps)
curryGenericArgType  st=:{st_args, st_result, st_attr_env, st_attr_vars} attr_var_name th=:{th_attrs}
	#! (atype, attr_env, attr_vars, attr_store, th_attrs) 
		= buildCurriedType st_args st_result TA_Multi st_attr_env st_attr_vars attr_var_name 1 th_attrs

	# curried_st = 
		{ st 
		& st_args = []
		, st_arity = 0
		, st_result = atype
		, st_attr_env = attr_env
		, st_attr_vars = attr_vars
		}
	= (curried_st, {th & th_attrs = th_attrs})	
*/

curryGenericArgType1 :: !SymbolType !String !*TypeHeaps -> (!SymbolType, !*TypeHeaps)
curryGenericArgType1  st=:{st_args, st_result, st_attr_env, st_attr_vars} attr_var_name th=:{th_attrs}
	# (atype, attr_vars, av_num, th_attrs) = curry st_args st_result 1 th_attrs
	# curried_st = {st & st_args = [], st_arity = 0, st_result = atype, st_attr_vars = attr_vars}
	= (curried_st, {th & th_attrs = th_attrs})	
where
	// outermost closure gets TA_Multi attribute
	curry [] res av_num th_attrs
		= (res, [], av_num, th_attrs)
	curry [arg=:{at_attribute=TA_Multi}:args] res av_num th_attrs
		#! (res, avs, av_num, th_attrs) = curry args res av_num th_attrs
		#! atype = {at_attribute = TA_Multi, at_type = arg --> res}
		= (atype, avs, av_num, th_attrs)
	curry [arg:args] res av_num th_attrs
		#! (res, avs, av_num, th_attrs) = curry1 args res av_num th_attrs
 		#! atype = makeAType (arg --> res) TA_Multi
		= (atype, avs, av_num, th_attrs)
		
	// inner closures get TA_Var attributes	
	curry1 [] res av_num th_attrs
		= (res, [], av_num, th_attrs)	 	
	curry1 [arg:args] res av_num th_attrs
		#! (res, avs, av_num, th_attrs) = curry1 args res av_num th_attrs
		#! (av, th_attrs) = freshAttrVar (makeIdent (attr_var_name +++ toString av_num)) th_attrs
 		#! atype = makeAType (arg --> res) (TA_Var av)
		= (atype, [av:avs], inc av_num, th_attrs)

// write empty value in the variable heaps 

clearType t th 
	= foldType clear_type clear_atype t th
where
	clear_type (TV tv) th = clear_type_var tv th	
	clear_type (GTV tv) th = clear_type_var tv th
	clear_type (CV tv :@: _) th = clear_type_var tv th
	clear_type (TFA atvs type) th
		#! th = foldSt clear_attr [atv_attribute \\ {atv_attribute} <- atvs] th
		#! th = foldSt clear_type_var [atv_variable \\ {atv_variable} <- atvs] th
		= th
	clear_type _ th = th

	clear_atype {at_attribute} th 
		= clear_attr at_attribute th

	clear_attr (TA_Var av) th = clear_attr_var av th
	clear_attr (TA_RootVar av) th = clear_attr_var av th
	clear_attr _ th = th
		
	clear_type_var {tv_info_ptr} th=:{th_vars} 
		= {th & th_vars = writePtr tv_info_ptr TVI_Empty th_vars} 

	clear_attr_var {av_info_ptr} th=:{th_attrs} 
		= {th & th_attrs = writePtr av_info_ptr AVI_Empty th_attrs} 

clearSymbolType st th
	// clears not only st_vars and st_attrs, but also TFA variables
	= clearType ((st.st_result, st.st_args), st.st_context) th

// collect variables

collectTypeVarsAndAttrVars ::
		!type 
		!*TypeHeaps
	-> 	(![TypeVar]
		,![AttributeVar]
		,!*TypeHeaps
		)
	| foldType type	 
collectTypeVarsAndAttrVars type th
	#! th = clearType type th
	#! (tvs, avs, th) = foldType collect_type_var collect_attr type ([], [], th)
	#! th = clearType type th
	= (tvs, avs, th)
where
	collect_type_var (TV tv) st = add_type_var tv st
	collect_type_var (GTV tv) st = add_type_var tv st
	collect_type_var (CV tv :@: _) st = add_type_var tv st
	collect_type_var (TFA forall_atvs type) (tvs, avs, th_vars) 
		#! forall_tvs = [atv_variable\\{atv_variable}<-forall_atvs]
		= (tvs -- forall_tvs, avs, th_vars)
	collect_type_var t st = st

	add_type_var tv (tvs, avs, th=:{th_vars})
		# (was_used, th_vars) = markTypeVarUsed tv th_vars
		# th = {th & th_vars = th_vars}
		| was_used 
			= (tvs, avs, th)
				//---> ("collectTypeVarsAndAttrVars: TV was used", tv)
			= ([tv:tvs], avs, th)
				//---> ("collectTypeVarsAndAttrVars: TV was not used", tv)
	
	collect_attr {at_attribute} st = collect_attr_var at_attribute st
	
	collect_attr_var (TA_Var av) st = add_attr_var av st
	collect_attr_var (TA_RootVar av) st = add_attr_var av st
	collect_attr_var _ st = st
				
	add_attr_var av (atvs, avs, th=:{th_attrs})		
		# (was_used, th_attrs) = markAttrVarUsed av th_attrs
		# th = {th & th_attrs = th_attrs}
		| was_used 
			= (atvs, avs, th)
			= (atvs, [av:avs], th)

collectTypeVars type th
	# (tvs, _, th) = collectTypeVarsAndAttrVars type th
	= (tvs, th)
collectAttrVars type th 
	# (_, avs, th) = collectTypeVarsAndAttrVars type th
	= (avs, th)

collectAttrsOfTypeVars :: ![TypeVar] type !*TypeHeaps -> (![ATypeVar], !*TypeHeaps) | foldType type
collectAttrsOfTypeVars tvs type th
	#! (th=:{th_vars}) = clearType type th
	
	# th_vars = foldSt (\{tv_info_ptr} h->writePtr tv_info_ptr TVI_Empty h) tvs th_vars 
	
	# th_vars = foldType on_type on_atype type th_vars

	# (attrs, th_vars) = mapSt read_attr tvs th_vars
	# atvs = [makeATypeVar tv attr \\ tv <- tvs & attr <- attrs]

	# th_vars = foldSt (\{tv_info_ptr} h->writePtr tv_info_ptr TVI_Empty h) tvs th_vars 

 	#! th = clearType type {th & th_vars= th_vars}
	= (atvs, th)
where
	on_type type st = st

	on_atype {at_type=TV tv, at_attribute} st = on_type_var tv at_attribute st				 	 
	on_atype {at_type=GTV tv, at_attribute} st = on_type_var tv at_attribute st				 	 
	on_atype {at_type=(CV tv :@: _), at_attribute} st = on_type_var tv at_attribute st
	//??? TFA -- seems that it is not needed
 	on_atype _ st = st 	

	on_type_var tv=:{tv_info_ptr} attr th_vars
	 	#! (tvi, th_vars) = readPtr tv_info_ptr th_vars
	 	= case tvi of
	 		TVI_Empty
	 			-> writePtr tv_info_ptr (TVI_Attr attr) th_vars
	 		TVI_Attr _ 
	 			-> th_vars

	read_attr {tv_info_ptr} th_vars
		# (TVI_Attr attr, th_vars) = readPtr tv_info_ptr th_vars
		= (attr, th_vars)

collectAttrsOfTypeVarsInSymbolType tvs {st_args, st_result} th
 	= collectAttrsOfTypeVars tvs [st_result:st_args] th  

// marks empty type vars used,
// returns whether the type var was already used	 	  
markTypeVarUsed tv=:{tv_info_ptr} th_vars
	# (tv_info, th_vars) = readPtr tv_info_ptr th_vars
	= case tv_info of
		TVI_Empty -> (False, writePtr tv_info_ptr TVI_Used th_vars)
		TVI_Used  -> (True, th_vars)
		_ -> (abort "markTypeVarUsed: wrong tv_info ") ---> (tv, tv_info)

// marks empty attr vars  used
// returns whether the attr var was already used		
markAttrVarUsed {av_info_ptr} th_attrs
	# (av_info, th_attrs) = readPtr av_info_ptr th_attrs
	= case av_info of
		AVI_Empty -> (False, writePtr av_info_ptr AVI_Used th_attrs)
		AVI_Used  -> (True, th_attrs)

simplifyTypeApp :: !Type ![AType] -> Type
simplifyTypeApp (TA type_cons=:{type_arity} cons_args) type_args
	= TA { type_cons & type_arity = type_arity + length type_args } (cons_args ++ type_args)
simplifyTypeApp (TAS type_cons=:{type_arity} cons_args strictness) type_args
	= TAS { type_cons & type_arity = type_arity + length type_args } (cons_args ++ type_args) strictness
simplifyTypeApp (CV tv :@: type_args1) type_args2 = CV tv :@: (type_args1 ++ type_args2)
simplifyTypeApp TArrow [type1, type2] = type1 --> type2
simplifyTypeApp TArrow [type] = TArrow1 type
simplifyTypeApp (TArrow1 type1) [type2] = type1 --> type2
simplifyTypeApp (TV tv) type_args = CV tv :@: type_args
simplifyTypeApp (TB _) type_args = TE
simplifyTypeApp (TArrow1 _) type_args = TE
		
// substitutions

// Uninitialized variables are not substituted, but left intact
//
// This behaviour is needed for kind indexing generic types,
// where generic variables are substituted and non-generic variables
// are not
//
applySubst :: !type !*TypeHeaps -> (!type, !*TypeHeaps) | mapTypeSt type 
applySubst type th
	= mapTypeAfterSt on_type on_atype type th
where
	on_type type=:(TV {tv_info_ptr}) th=:{th_vars}
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars 
		# th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Type t -> (t, th)
			TVI_Empty -> (type, th) 
	on_type (GTV _) th 
		= abort "GTV"
	on_type type=:(CV {tv_info_ptr} :@: args) th=:{th_vars}
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars 
		# th = {th & th_vars = th_vars}
		= case tv_info of
			TVI_Type t -> (simplifyTypeApp t args, th)
			TVI_Empty  -> (type, th) 

	//on_type type=:(TFA atvs t) th=:{th_vars}
	//	= abort "applySubst TFA" 

	on_type type th
		= (type, th)

	on_atype atype=:{at_attribute} th=:{th_attrs}	
		# (at_attribute, th_attrs) = subst_attr at_attribute th_attrs
		= ({atype & at_attribute = at_attribute}, {th & th_attrs = th_attrs})

	subst_attr attr=:(TA_Var {av_info_ptr}) th_attrs
		# (av_info, th_attrs) = readPtr av_info_ptr th_attrs 
		= case av_info of
			AVI_Attr a -> (a, th_attrs)
			AVI_Empty -> (attr, th_attrs) 
	subst_attr (TA_RootVar {av_info_ptr}) th_attrs
		# (av_info, th_attrs) = readPtr av_info_ptr th_attrs 
		= case av_info of
			AVI_Attr a -> (a, th_attrs)
	subst_attr TA_Multi th = (TA_Multi, th)
	subst_attr TA_Unique th = (TA_Unique, th)

applySubstInSymbolType st=:{st_args, st_result, st_attr_env, st_context} th
	#! (new_st_args, th) 	= applySubst st.st_args th
	#! (new_st_result, th) 	= applySubst st.st_result th	
	#! (new_st_context, th) 	= applySubst st.st_context th	
	#! (new_st_attr_env, th)	= mapSt subst_ineq st.st_attr_env th		
	
	#! th = clear_type_vars st.st_vars th
	#! th = clear_attr_vars st.st_attr_vars th
		
	#! (new_st_vars, new_st_attr_vars, th) 
		= collectTypeVarsAndAttrVars ((new_st_args,new_st_result), new_st_context) th

	#! new_st = 
		{ st
		& st_args = new_st_args
		, st_result = new_st_result
		, st_context = new_st_context
		, st_attr_env = new_st_attr_env
		, st_vars = new_st_vars
		, st_attr_vars = new_st_attr_vars 
		}
		
	#! th = clearSymbolType st th	

	#! th = assertSymbolType new_st th
	#! th = assertSymbolType st th
		
	= (new_st, th)
		//---> ("applySubstInSymbolType", new_st)
where 
	subst_ineq 	ai=:{ai_demanded,ai_offered} th
		# (ai_demanded, th) = subst_attr_var ai_demanded th
		# (ai_offered, th) = subst_attr_var ai_offered th
		= ({ai & ai_demanded = ai_demanded, ai_offered = ai_offered}, th)
	subst_attr_var  av=:{av_info_ptr} th=:{th_attrs}
		# (av_info, th_attrs) = readPtr av_info_ptr th_attrs
		# th = {th & th_attrs = th_attrs}
		= case av_info of
			AVI_Attr (TA_Var av1) -> (av1, th)
			AVI_Attr _ -> (av, th)
			AVI_Empty -> (av, th)
	clear_type_vars tvs th=:{th_vars}
		#! th_vars = foldSt (\{tv_info_ptr} h->writePtr tv_info_ptr TVI_Empty h) tvs th_vars
		= {th & th_vars = th_vars}
	clear_attr_vars avs th=:{th_attrs}
		#! th_attrs = foldSt (\{av_info_ptr} h->writePtr av_info_ptr AVI_Empty h) avs th_attrs
		= {th & th_attrs = th_attrs}				

expandSynonymType :: !CheckedTypeDef !TypeAttribute ![AType] !*TypeHeaps -> (!Type, !*TypeHeaps)
expandSynonymType {td_rhs=SynType {at_type}, td_args, td_attribute} ta_attr ta_args th
	#! th_attrs = bind_attribute td_attribute ta_attr th.th_attrs
	#! th = fold2St bind_type_and_attr td_args ta_args { th & th_attrs = th_attrs }
	#! (at_type, th) = applySubst at_type th
	#! th_attrs = clear_attribute td_attribute th.th_attrs
	#! th = foldSt clear_type_and_attr td_args { th & th_attrs = th_attrs }
	= (at_type, th)   
where
	bind_type_and_attr {atv_attribute, atv_variable={tv_info_ptr}} {at_type,at_attribute} type_heaps=:{th_vars,th_attrs}
		= { type_heaps &	th_vars = th_vars <:= (tv_info_ptr, TVI_Type at_type),
							th_attrs = bind_attribute atv_attribute at_attribute th_attrs }
		
	bind_attribute (TA_Var {av_info_ptr}) attr th_attrs
		= th_attrs <:= (av_info_ptr, AVI_Attr attr)
	bind_attribute _ _ th_attrs
		= th_attrs

	clear_type_and_attr {atv_attribute, atv_variable={tv_info_ptr}} type_heaps=:{th_vars,th_attrs}
		= { type_heaps & th_vars = th_vars <:= (tv_info_ptr, TVI_Empty), th_attrs = clear_attribute atv_attribute th_attrs }
		
	clear_attribute (TA_Var {av_info_ptr}) th_attrs
		= th_attrs <:= (av_info_ptr, AVI_Empty)
	clear_attribute _ th_attrs
		= th_attrs
expandSynonymType td ta_attr ta_args th = abort "expanding not a synonym type\n" 

//	Function Helpers

makeFunction :: !Ident !Index ![FreeVar] !Expression !(Optional SymbolType) !Index !Position -> FunDef
makeFunction ident group_index arg_vars body_expr opt_sym_type main_dcl_module_n fun_pos	
	#! (arg_vars, local_vars, free_vars) = collectVars body_expr arg_vars	
	| not (isEmpty free_vars)
		= abort "makeFunction: free_vars is not empty\n"
	=	{ fun_ident = ident
		, fun_arity = length arg_vars
		, fun_priority = NoPrio
		, fun_body = TransformedBody {tb_args = arg_vars, tb_rhs = body_expr }
		, fun_type = opt_sym_type
		, fun_pos = fun_pos
		, fun_kind  = FK_Function cNameNotLocationDependent
		, fun_lifted = 0
		, fun_info =
			{ fi_calls = collectCalls main_dcl_module_n body_expr
			, fi_group_index = group_index
			, fi_def_level = NotALevel
			, fi_free_vars =  []
			, fi_local_vars = local_vars
			, fi_dynamics = []
			, fi_properties = FI_GenericFun
			}	
		}

buildFunAndGroup :: !Ident ![FreeVar] !Expression !(Optional SymbolType) !Index !Position !FunsAndGroups -> (!DefinedSymbol, FunsAndGroups)
buildFunAndGroup 
		ident arg_vars body_expr opt_sym_type main_dcl_module_n fun_pos 
		funs_and_groups=:{fg_fun_index,fg_group_index,fg_funs,fg_groups}
	# fun = makeFunction ident fg_group_index arg_vars body_expr opt_sym_type main_dcl_module_n fun_pos
	# group = {group_members = [fg_fun_index]}
	# def_sym = {ds_ident=ident, ds_arity=fun.fun_arity, ds_index=fg_fun_index}
	  funs_and_groups = {funs_and_groups & fg_fun_index=fg_fun_index+1, fg_group_index=fg_group_index+1, fg_funs=[fun:fg_funs], fg_groups=[group:fg_groups]}
	= (def_sym, funs_and_groups)

buildFunAndGroup2 :: !Ident ![FreeVar] !Expression !Index !FunsAndGroups -> (!Index, !FunsAndGroups)
buildFunAndGroup2 ident arg_vars body_expr main_dcl_module_n funs_and_groups=:{fg_fun_index,fg_group_index,fg_funs,fg_groups}
	# fun = makeFunction ident fg_group_index arg_vars body_expr No main_dcl_module_n NoPos
	  group = {group_members = [fg_fun_index]}
	  funs_and_groups = {funs_and_groups & fg_fun_index=fg_fun_index+1, fg_group_index=fg_group_index+1, fg_funs=[fun:fg_funs], fg_groups=[group:fg_groups]}
	= (fg_fun_index, funs_and_groups)
	
//	Expr Helpers

// Primitive expressions

makeIntExpr :: Int -> Expression
makeIntExpr value
	= BasicExpr (BVInt value)

makeStringExpr :: String -> Expression
makeStringExpr str
	=  BasicExpr (BVS (adjust_string str))
where
	adjust_string str
		= { ch \\ ch <- ['\"'] ++ adjust_chars [ch \\ ch <-: str] ++ ['\"'] }
	adjust_chars [] = []
	adjust_chars ['\\':cs] 	= ['\\','\\' : adjust_chars cs]
	adjust_chars [c:cs] 	= [c : adjust_chars cs]
		
makeListExpr :: [Expression] !PredefinedSymbolsData !*Heaps -> (Expression, !*Heaps)
makeListExpr [] predefs heaps
	= buildPredefConsApp PD_NilSymbol [] predefs heaps
makeListExpr [expr:exprs] predefs heaps 
	# (list_expr, heaps) = makeListExpr exprs predefs heaps 
	= buildPredefConsApp PD_ConsSymbol [expr, list_expr] predefs heaps

buildConsApp :: !Index DefinedSymbol ![Expression] !*Heaps 
	-> (!Expression, !*Heaps) 
buildConsApp cons_mod {ds_ident, ds_index, ds_arity} arg_exprs heaps=:{hp_expression_heap}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# cons_glob = {glob_module = cons_mod, glob_object = ds_index}
	# expr = App {
		app_symb = {
			symb_ident = ds_ident, 
			symb_kind = SK_Constructor cons_glob
			}, 
		app_args = arg_exprs, 
		app_info_ptr = expr_info_ptr} 	
	# heaps = { heaps & hp_expression_heap = hp_expression_heap } 
	= (expr, heaps)	

buildNewTypeConsApp :: !Index DefinedSymbol !Expression !*Heaps -> (!Expression, !*Heaps) 
buildNewTypeConsApp cons_mod {ds_ident, ds_index, ds_arity} arg_expr heaps=:{hp_expression_heap}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# expr = App {
		app_symb = {
			symb_ident = ds_ident, 
			symb_kind = SK_NewTypeConstructor {gi_module = cons_mod, gi_index = ds_index}
			},
		app_args = [arg_expr], 
		app_info_ptr = expr_info_ptr} 	
	# heaps = { heaps & hp_expression_heap = hp_expression_heap } 
	= (expr, heaps)	

buildFunApp :: !Index !DefinedSymbol ![Expression] !*Heaps -> (!Expression, !*Heaps) 
buildFunApp fun_mod {ds_ident, ds_index} arg_exprs heaps
	= buildFunApp2 fun_mod ds_index ds_ident arg_exprs heaps

buildFunApp2 :: !Index !Index !Ident ![Expression] !*Heaps -> (!Expression, !*Heaps) 
buildFunApp2 fun_mod ds_index ds_ident arg_exprs heaps=:{hp_expression_heap}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# fun_glob = {glob_module = fun_mod, glob_object = ds_index}
	# expr = App {
		app_symb = {symb_ident = ds_ident, symb_kind = SK_Function fun_glob},
		app_args = arg_exprs, 
		app_info_ptr = expr_info_ptr} 	
	# heaps = {heaps & hp_expression_heap = hp_expression_heap}
	= (expr, heaps)	

buildPredefFunApp :: !Int [Expression] !PredefinedSymbolsData !*Heaps -> (!Expression, !*Heaps)
buildPredefFunApp predef_index args {psd_predefs_a} heaps
	# {pds_module, pds_def} = psd_predefs_a.[predef_index]
 	= buildFunApp2 pds_module pds_def predefined_idents.[predef_index] args heaps

buildGenericApp :: !Index !Index !Ident !TypeKind ![Expression] !*Heaps -> (!Expression, !*Heaps)
buildGenericApp gen_module gen_index gen_ident kind arg_exprs heaps=:{hp_expression_heap}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# glob_index = {glob_module = gen_module, glob_object = gen_index}
	# expr = App {
		app_symb = {symb_ident = gen_ident, symb_kind = SK_Generic glob_index kind}, 
		app_args = arg_exprs, 
		app_info_ptr = expr_info_ptr} 	
	# heaps = {heaps & hp_expression_heap = hp_expression_heap}
	= (expr, heaps)	

buildPredefConsApp :: !Int [Expression] !PredefinedSymbolsData !*Heaps -> (!Expression, !*Heaps)
buildPredefConsApp predef_index args {psd_predefs_a} heaps=:{hp_expression_heap}
	# {pds_module, pds_def} = psd_predefs_a.[predef_index]
	# pds_ident = predefined_idents.[predef_index]
	# global_index = {glob_module = pds_module, glob_object = pds_def}
	# symb_ident = 
		{ symb_ident = pds_ident 
		, symb_kind = SK_Constructor global_index
		}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# app = App {app_symb = symb_ident, app_args = args, app_info_ptr = expr_info_ptr} 
	= (app, {heaps & hp_expression_heap = hp_expression_heap})

buildPredefNewTypeConsApp :: !Int [Expression] !PredefinedSymbolsData !*Heaps -> (!Expression, !*Heaps)
buildPredefNewTypeConsApp predef_index args {psd_predefs_a} heaps=:{hp_expression_heap}
	# {pds_module, pds_def} = psd_predefs_a.[predef_index]
	# pds_ident = predefined_idents.[predef_index]
	# global_index = {gi_module = pds_module, gi_index = pds_def}
	# symb_ident = 
		{ symb_ident = pds_ident 
		, symb_kind = SK_NewTypeConstructor global_index
		}
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# app = App {app_symb = symb_ident, app_args = args, app_info_ptr = expr_info_ptr} 
	= (app, {heaps & hp_expression_heap = hp_expression_heap})

buildPredefConsPattern :: !Int ![FreeVar] !Expression !PredefinedSymbolsData -> AlgebraicPattern
buildPredefConsPattern predef_index vars expr {psd_predefs_a}
	# {pds_module, pds_def} = psd_predefs_a.[predef_index]
	# pds_ident = predefined_idents.[predef_index]
	# cons_def_symbol = {
		ds_ident = pds_ident,
		ds_arity = length vars,
		ds_index = pds_def
		}
	# pattern = {
		ap_symbol = {glob_module = pds_module, glob_object = cons_def_symbol},
		ap_vars = vars,
		ap_expr = expr,
		ap_position = NoPos		
		}
	= pattern

buildCaseExpr :: Expression CasePatterns !*Heaps 
	-> (!Expression, !*Heaps)
buildCaseExpr case_arg case_alts heaps=:{hp_expression_heap}	
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# expr = Case 
		{ case_expr = case_arg
		, case_guards = case_alts
		, case_default = No
		, case_ident = No
		, case_info_ptr = expr_info_ptr
		, case_explicit = False
		, case_default_pos = NoPos 
		}
	# heaps = { heaps & hp_expression_heap = hp_expression_heap}	
	= (expr, heaps)

build_map_from_tvi_expr (TVI_BimapExpr _ _ bimap_expr) main_module_index predefs heaps
	= (bimap_expr, heaps)
build_map_from_tvi_expr (TVI_Iso _ from_ds) main_module_index predefs heaps
	= buildFunApp main_module_index from_ds [] heaps

build_map_to_tvi_expr (TVI_BimapExpr _ bimap_expr _) main_module_index predefs heaps
	= (bimap_expr, heaps)
build_map_to_tvi_expr (TVI_Iso to_ds _) main_module_index predefs heaps
	= buildFunApp main_module_index to_ds [] heaps

// variables

// build a new variable and an expression associated with it
buildVarExpr :: 
		!String 			// variable name
		!*Heaps	
	-> (!Expression 		// variable expression
		, !FreeVar 			// variable
		, !*Heaps
		)
buildVarExpr name heaps=:{hp_var_heap, hp_expression_heap} 
	# (expr_info_ptr, hp_expression_heap) = newPtr EI_Empty hp_expression_heap
	# (var_info_ptr, hp_var_heap) = newPtr VI_Empty hp_var_heap
	# var_ident = makeIdent name
	# var = Var {var_ident = var_ident, var_expr_ptr = expr_info_ptr, var_info_ptr = var_info_ptr } 
	# hp_var_heap = writePtr var_info_ptr (VI_Expression var) hp_var_heap
	# heaps = { heaps & hp_var_heap = hp_var_heap, hp_expression_heap = hp_expression_heap }
	# fv = {fv_count = 1/* if 0, trans crashes*/, fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel} 
	= (var, fv, heaps)

buildVarExprs [] heaps = ([], [], heaps)
buildVarExprs [x:xs] heaps
	# (y, z, heaps) = buildVarExpr x heaps
	# (ys, zs, heaps) = buildVarExprs xs heaps
	= ([y:ys], [z:zs], heaps)

// recursion over expressions

//-----------------------------------------------------------------------------
// fold expression applies a function to each node of an expression
// recursively:
// first apply the function, then recurse
//-----------------------------------------------------------------------------
foldExpr :: 
		(Expression -> .st -> .st)  	// function to apply at each node
		Expression 						// expression to run throuh
		.st 							// state
	-> 
		.st								// updated state 
foldExpr f expr=:(Var _) st 
	= f expr st
foldExpr f expr=:(App {app_args}) st 
	# st = f expr st
	= foldSt (foldExpr f) app_args st
foldExpr f expr=:(expr1 @ exprs) st
	# st = f expr st
	= foldSt (foldExpr f) [expr1:exprs] st	
foldExpr f expr=:(Let {let_lazy_binds, let_strict_binds, let_expr}) st
	# st = f expr st
	# st = foldSt (fold_let_binds f) let_strict_binds st 
	# st = foldSt (fold_let_binds f) let_lazy_binds st 
	= foldExpr f let_expr st 
where
	fold_let_binds f {lb_src} st = foldExpr f lb_src st 
foldExpr f expr=:(Case {case_expr,case_guards,case_default}) st
	# st = f expr st
	# st = foldExpr f case_expr st
	# st = fold_guards f case_guards st 
	# st = foldOptional (foldExpr f) case_default st
	= st
where
	fold_guards f (AlgebraicPatterns gi aps) st = foldSt (foldExpr f) [ap_expr\\{ap_expr}<-aps] st
	fold_guards f (BasicPatterns gi bps) st = foldSt (foldExpr f) [bp_expr\\{bp_expr}<-bps] st
	fold_guards f (DynamicPatterns dps) st = foldSt (foldExpr f) [dp_rhs\\{dp_rhs}<-dps] st
	fold_guards f (NewTypePatterns gi aps) st = foldSt (foldExpr f) [ap_expr\\{ap_expr}<-aps] st
	fold_guards f NoPattern st = st
foldExpr f expr=:(Selection _ expr1 _) st
	# st = f expr st
  	= foldExpr f expr1 st 	
foldExpr f expr=:(Update expr1 sels expr2) st
	# st = f expr st
	# st = foldExpr f expr1 st 
	# st = foldSt (fold_sel f) sels st 
	# st = foldExpr f expr2 st 
	= st
where
	fold_sel f (RecordSelection _ _) st = st
	fold_sel f (ArraySelection _ _ expr) st = foldExpr f expr st
	fold_sel f (DictionarySelection _ _ _ expr) st = foldExpr f expr st
foldExpr f expr=:(RecordUpdate _ expr1 binds) st
	# st = f expr st
	# st = foldExpr f expr1 st 
	# st = foldSt (foldExpr f) [bind_src\\{bind_src}<-binds] st
	= st
foldExpr f expr=:(TupleSelect _ _ expr1) st 
	# st = f expr st
	= foldExpr f expr1 st
foldExpr f expr=:(BasicExpr _) st
	= f expr st	
foldExpr f expr=:(Conditional {if_cond,if_then,if_else}) st
	# st = f expr st
	# st = foldExpr f if_cond st	
	# st = foldExpr f if_then st	
	# st = foldOptional (foldExpr f) if_else st	
	= st
foldExpr f expr=:(MatchExpr _ expr1) st
	# st = f expr st
	= foldExpr f expr1 st
foldExpr f expr=:(IsConstructor expr1 _ _ _ _ _) st 
	# st = f expr st
	= foldExpr f expr1 st
foldExpr f expr=:(DynamicExpr {dyn_expr}) st 
	# st = f expr st
	= foldExpr f dyn_expr st
foldExpr f EE st 
	= st
foldExpr f expr st 
	= abort "generic.icl: foldExpr does not match\n"

// needed for collectCalls
instance == FunCall where (==) (FunCall x _) (FunCall y _) = x == y

// collect function calls made in the expression
collectCalls :: !Index !Expression -> 	[FunCall]
collectCalls current_module expr = removeDup (foldExpr get_call expr [])
where
	get_call (App {app_symb={symb_kind=SK_Function {glob_module,glob_object}, symb_ident}}) indexes
		| glob_module == current_module
			= [FunCall glob_object NotALevel : indexes]
				//---> ("collect call ", symb_ident, glob_object)
			= indexes
				//---> ("do not collect call ", symb_ident, glob_module, glob_object)
	get_call _ indexes = indexes

// collects variables and computes the refernce counts
collectVars :: 
		!Expression 	// expression to collect variables in
		![FreeVar] 		// function argument variables
	-> (  ![FreeVar]	// argument variables (with updated ref count)
		, ![FreeVar]	// local variables
		, ![FreeVar]	// free_variables
		)
collectVars expr arg_vars  
	# arg_vars = [ {v & fv_count = 0} \\ v <- arg_vars]
	= foldExpr collect_vars expr (arg_vars, [], [])
where
	collect_vars (Var {var_ident, var_info_ptr}) (arg_vars, local_vars, free_vars)
		# var = {fv_ident = var_ident, fv_count = 1, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel}
		# (added, arg_vars) = add_var var arg_vars
		| added 
			= (arg_vars, local_vars, free_vars)
		# (added, local_vars) = add_var var local_vars
		| added 
			= (arg_vars, local_vars, free_vars)
		# (added, free_vars) = add_var var free_vars
		| added 
			= (arg_vars, local_vars, free_vars)				
		= (arg_vars, local_vars, [var:free_vars])
	where
		add_var var [] = (False, [])
		add_var var [v=:{fv_count,fv_info_ptr}:vs]
			| var.fv_info_ptr == fv_info_ptr
				= (True, [{v&fv_count = inc fv_count}:vs])
				# (added, vs) = add_var var vs
				= (added, [v:vs])	
	collect_vars (Let {let_lazy_binds, let_strict_binds}) (arg_vars, local_vars, free_vars)
		# vars = [{lb_dst&fv_count=0} \\ {lb_dst} <- (let_lazy_binds ++ let_strict_binds)]
		# (local_vars, free_vars) = foldSt add_local_var vars (local_vars, free_vars) 
		= (arg_vars, local_vars, free_vars)
	collect_vars (Case {case_guards}) (arg_vars, local_vars, free_vars)
		# vars = [{v&fv_count=0} \\ v <- collect case_guards]
		# (local_vars, free_vars) = foldSt add_local_var vars (local_vars, free_vars) 
		= (arg_vars, local_vars, free_vars) 
	where
		collect (AlgebraicPatterns _ aps) = flatten [ap_vars\\{ap_vars}<-aps]
		collect (BasicPatterns _ bps) = []
		collect (DynamicPatterns dps) = [dp_var \\ {dp_var}<-dps]
		collect (NewTypePatterns _ aps) = flatten [ap_vars\\{ap_vars}<-aps]
		collect NoPattern = []
	collect_vars expr st = st		

	add_local_var var (local_vars, []) = ([var:local_vars], [])
	add_local_var var (local_vars, free_vars=:[fv:fvs])
		| var.fv_info_ptr == fv.fv_info_ptr 
			= ([fv:local_vars], fvs)
			# (local_vars, fvs1) = add_local_var var (local_vars, fvs)
			= (local_vars, [fv:fvs1])

// Array helpers

//updateArraySt :: (a .st -> (a, .st)) *{a} .st -> (*{a}, .st) 
updateArraySt f xs st
	:== map_array 0 xs st
where
	map_array n xs st
		| n == size xs
			= (xs, st)
			# (x, xs) = xs![n]	
			# (x, st) = f x st			
			= map_array (inc n) {xs&[n]=x} st

//foldArraySt :: (a .st -> .st) {a} .st -> .st 
foldArraySt f xs st
	:== fold_array 0 xs st
where
	fold_array n xs st
		| n == size xs
			= st	
			# st = f xs.[n] st
			= fold_array (inc n) xs st

//	General Helpers

idSt x st = (x, st)

(--) infixl 5 :: u:[a] .[a] -> u:[a] | Eq a
(--) x y = removeMembers x y 

// should actually be in the standard library
transpose []             = []
transpose [[] : xss]     = transpose xss
transpose [[x:xs] : xss] = 
	[[x : [hd l \\ l <- xss]] : transpose [xs : [ tl l \\  l <- xss]]]

foldOptional f No st = st
foldOptional f (Yes x) st = f x st

filterOptionals [] = []
filterOptionals [No : xs] 	= filterOptionals xs
filterOptionals [Yes x : xs] = [x : filterOptionals xs]

zipWith f [] [] = []
zipWith f [x:xs] [y:ys] = [f x y : zipWith f xs ys]
zipWith f _ _ = abort "zipWith: lists of different length\n"

zipWithSt f l1 l2 st
	:== zipWithSt l1 l2 st
where
	zipWithSt [] [] st
		= ([], st)
	zipWithSt [x:xs] [y:ys] st
		# (z, st) = f x y st
		# (zs, st) = zipWithSt xs ys st
		= ([z:zs], st) 

zipWith3St f l1 l2 l3 st
	:== zipWith3St l1 l2 l3 st
where
	zipWith3St [] [] [] st
		= ([], st)
	zipWith3St [x:xs] [y:ys] [z:zs] st
		# (r, st) = f x y z st
		# (rs, st) = zipWith3St xs ys zs st
		= ([r:rs], st) 

zipWithSt2 f l1 l2 st1 st2
	:== zipWithSt2 l1 l2 st1 st2
where
	zipWithSt2 [] [] st1 st2
		= ([], st1, st2)
	zipWithSt2 [x:xs] [y:ys] st1 st2
		# (z, st1, st2) = f x y st1 st2
		# (zs, st1, st2) = zipWithSt2 xs ys st1 st2
		= ([z:zs], st1, st2)

mapSdSt f l sd s :== map_sd_st l s
where
	map_sd_st [x : xs] s
	 	# (x, s) = f x sd s
		  (xs, s) = map_sd_st xs s
		#! s = s
		= ([x : xs], s)
	map_sd_st [] s
		#! s = s
	 	= ([], s)
