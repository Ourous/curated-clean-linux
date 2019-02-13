implementation module expand_types

import StdEnv
import syntax,predef,containers,utilities

simplifyTypeApplication :: !Type ![AType] -> Type
simplifyTypeApplication type type_args
	# (ok, type)
		=	simplifyAndCheckTypeApplication type type_args
	| not ok
		=	abort "expand_types.simplifyTypeApplication: unexpected error"
	=	type

simplifyAndCheckTypeApplication :: !Type ![AType] -> (!Bool, !Type)
simplifyAndCheckTypeApplication (TA type_cons=:{type_arity} cons_args) type_args
	= (True, TA { type_cons & type_arity = type_arity + length type_args } (cons_args ++ type_args))
simplifyAndCheckTypeApplication (TAS type_cons=:{type_arity} cons_args strictness) type_args
	= (True, TAS { type_cons & type_arity = type_arity + length type_args } (cons_args ++ type_args) strictness)
simplifyAndCheckTypeApplication (CV tv :@: type_args1) type_args2
	= (True, CV tv :@: (type_args1 ++ type_args2))
simplifyAndCheckTypeApplication TArrow [type1, type2] 
	= (True, type1 --> type2)
simplifyAndCheckTypeApplication TArrow [type] 
	= (True, TArrow1 type)
simplifyAndCheckTypeApplication (TArrow1 type1) [type2] 
	= (True, type1 --> type2)
simplifyAndCheckTypeApplication (TV tv) type_args
	= (True, CV tv :@: type_args)
simplifyAndCheckTypeApplication (TempV i) type_args
	= (True, TempCV i :@: type_args)
simplifyAndCheckTypeApplication (TempQV i) type_args
	= (True, TempQCV i :@: type_args)
simplifyAndCheckTypeApplication type type_args
	= (False, type)

readVarInfo :: VarInfoPtr *VarHeap -> (VarInfo, !*VarHeap)
readVarInfo var_info_ptr var_heap
	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_Extended _ original_var_info	-> (original_var_info, var_heap)
		_								-> (var_info, var_heap)

writeVarInfo :: VarInfoPtr VarInfo *VarHeap -> *VarHeap
writeVarInfo var_info_ptr new_var_info var_heap
	# (old_var_info, var_heap) = readPtr var_info_ptr var_heap
	= case old_var_info of
		VI_Extended extensions _	-> writePtr var_info_ptr (VI_Extended extensions new_var_info) var_heap
		_							-> writePtr var_info_ptr new_var_info var_heap

RemoveAnnotationsMask:==1
ExpandAbstractSynTypesMask:==2
DontCollectImportedConstructorsAndRestorePointers:==4

convertSymbolType :: !Bool !{#CommonDefs} !SymbolType !Int !*ImportedTypes !ImportedConstructors !*TypeHeaps !*VarHeap
										  -> (!SymbolType, !*ImportedTypes,!ImportedConstructors,!*TypeHeaps,!*VarHeap)
convertSymbolType rem_annots common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap
	# (st, ets_contains_unexpanded_abs_syn_type,ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)
		= convertSymbolType_  (if rem_annots (RemoveAnnotationsMask bitor ExpandAbstractSynTypesMask) ExpandAbstractSynTypesMask) common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap
	= (st, ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)

convertSymbolTypeWithoutExpandingAbstractSynTypes :: !Bool !{#CommonDefs} !SymbolType !Int
							!*ImportedTypes !ImportedConstructors !*TypeHeaps !*VarHeap
	-> (!SymbolType, !Bool, !*ImportedTypes,!ImportedConstructors,!*TypeHeaps,!*VarHeap)
convertSymbolTypeWithoutExpandingAbstractSynTypes rem_annots common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap
	= convertSymbolType_ (if rem_annots RemoveAnnotationsMask 0) common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap

convertSymbolTypeWithoutCollectingImportedConstructors :: !Bool !{#CommonDefs} !SymbolType !Int !*ImportedTypes !*TypeHeaps !*VarHeap
																				-> (!SymbolType,!*ImportedTypes,!*TypeHeaps,!*VarHeap)
convertSymbolTypeWithoutCollectingImportedConstructors rem_annots common_defs st main_dcl_module_n imported_types type_heaps var_heap
	# rem_annots
		= if rem_annots
			(RemoveAnnotationsMask bitor ExpandAbstractSynTypesMask bitor DontCollectImportedConstructorsAndRestorePointers)
			(ExpandAbstractSynTypesMask bitor DontCollectImportedConstructorsAndRestorePointers)
	# (st, ets_contains_unexpanded_abs_syn_type,ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)
		= convertSymbolType_  rem_annots common_defs st main_dcl_module_n imported_types [] type_heaps var_heap
	= (st, ets_type_defs, ets_type_heaps, ets_var_heap)

convertSymbolType_ :: !Int !{# CommonDefs} !SymbolType !Int !*ImportedTypes !ImportedConstructors !*TypeHeaps !*VarHeap
	-> (!SymbolType, !Bool,!*ImportedTypes, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
convertSymbolType_  rem_annots common_defs st main_dcl_module_n imported_types collected_imports type_heaps var_heap
	# ets	=	{ ets_type_defs			= imported_types
				, ets_collected_conses	= collected_imports
				, ets_type_heaps		= type_heaps
				, ets_var_heap			= var_heap
				, ets_main_dcl_module_n	= main_dcl_module_n 
				, ets_contains_unexpanded_abs_syn_type = False
				}
	# {st_args,st_result,st_context,st_args_strictness} = st
	#! (_,(st_args,st_result), ets)		= expandSynTypes rem_annots common_defs (st_args,st_result) ets
	# new_st_args						= addTypesOfDictionaries common_defs st_context st_args
	  new_st_arity						= length new_st_args
	  st	=	{ st
	  			& st_args				= new_st_args
	  			, st_result				= st_result
	  			, st_arity				= new_st_arity
	  			, st_args_strictness	= insert_n_strictness_values_at_beginning (new_st_arity-length st_args) st_args_strictness
	  			, st_context			= []
	  			}
	# {ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap,ets_contains_unexpanded_abs_syn_type} = ets
	= (st, ets_contains_unexpanded_abs_syn_type, ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)

addTypesOfDictionaries :: !{#CommonDefs} ![TypeContext] ![AType] -> [AType]
addTypesOfDictionaries common_defs type_contexts type_args
	= mapAppend (add_types_of_dictionary common_defs) type_contexts type_args
where
	add_types_of_dictionary common_defs {tc_class = TCGeneric {gtc_generic_dict={gi_module,gi_index}}, tc_types}
		#! generict_dict_ident = predefined_idents.[PD_TypeGenericDict]
		/*
			AA HACK:
			Generic classes are always generated locally, 
			and therefore the their dictionaries are also generated localy. 
			Overloaded functions in DCL modules can have generic context restrictions, i.e. they will 
			get generic class dictionaries. 
			Problem: DCL function types refer to ICL type defs of dictionaries.
			Solution: plug a dummy dictinary type, defined in StdGeneric.
			It is possible because all generic class have one class argument and one member.
		*/
		# dict_type_symb = MakeTypeSymbIdent {glob_object = gi_index, glob_module = gi_module} generict_dict_ident 1
		# type_arg = {at_attribute = TA_Multi, at_type=hd tc_types}
		= {at_attribute = TA_Multi, at_type = TA dict_type_symb [type_arg]}

	add_types_of_dictionary common_defs {tc_class = TCClass {glob_module, glob_object={ds_index,ds_ident}}, tc_types}
		# {class_arity, class_dictionary={ds_ident,ds_index}, class_cons_vars}
				= common_defs.[glob_module].com_class_defs.[ds_index]
		  dict_type_symb
		  		= MakeTypeSymbIdent {glob_object = ds_index, glob_module = glob_module} ds_ident class_arity
		  (dict_args,_) = mapSt (\type class_cons_vars
								-> let at_attribute = if (class_cons_vars bitand 1<>0) TA_MultiOfPropagatingConsVar TA_Multi
							   		in ({at_attribute = at_attribute, at_type = type}, class_cons_vars>>1)
						   	) tc_types class_cons_vars
		= {at_attribute = TA_Multi, /* at_annotation = AN_Strict, */ at_type = TA dict_type_symb dict_args}

::	ExpandTypeState =
	{	ets_type_defs			:: !.{#{#CheckedTypeDef}}
	,	ets_collected_conses	:: !ImportedConstructors
	,	ets_type_heaps			:: !.TypeHeaps
	,	ets_var_heap			:: !.VarHeap
	,	ets_main_dcl_module_n :: !Int
	,	ets_contains_unexpanded_abs_syn_type :: !Bool
	}

class expandSynTypes a :: !Int !{#CommonDefs} !a !*ExpandTypeState -> (!Bool,!a, !*ExpandTypeState)

instance expandSynTypes Type
where
	expandSynTypes rem_annots common_defs type=:(arg_type --> res_type) ets
		# (changed,(arg_type, res_type), ets) = expandSynTypes rem_annots common_defs (arg_type, res_type) ets
		| changed
			= (True,arg_type --> res_type, ets)
			= (False,type, ets)
	expandSynTypes rem_annots common_defs type=:(TB _) ets
		= (False,type, ets)
	expandSynTypes rem_annots common_defs type=:(cons_var :@: types) ets
		# (changed,types, ets) = expandSynTypes rem_annots common_defs types ets
		| changed
			= (True,cons_var :@: types, ets)
			= (False,type, ets)
	expandSynTypes rem_annots common_defs type=:(TA type_symb types) ets
		= expand_syn_types_in_TA rem_annots common_defs type TA_Multi ets
	expandSynTypes rem_annots common_defs type=:(TAS type_symb types _) ets
		= expand_syn_types_in_TA rem_annots common_defs type TA_Multi ets
	expandSynTypes rem_annots common_defs tfa_type=:(TFA vars type) ets
		# (changed,type, ets) = expandSynTypes rem_annots common_defs type ets
		| changed
			= (True,TFA vars type, ets)
			= (False,tfa_type, ets)
	expandSynTypes rem_annots common_defs tfac_type=:(TFAC vars type type_context) ets
		# (changed,type, ets) = expandSynTypes rem_annots common_defs type ets
		| changed
			= (True,TFAC vars type type_context, ets)
			= (False,tfac_type, ets)
	expandSynTypes rem_annots common_defs type ets
		= (False,type, ets)

instance expandSynTypes [a] | expandSynTypes a
where
	expandSynTypes rem_annots common_defs [] ets
		= (False,[],ets)
	expandSynTypes rem_annots common_defs t=:[type:types] ets
		#! (changed_type,type,ets)		= expandSynTypes rem_annots common_defs type ets
		   (changed_types,types,ets)	= expandSynTypes rem_annots common_defs types ets
		| changed_type || changed_types
			= (True,[type:types],ets)
			= (False,t,ets)

instance expandSynTypes (a,b) | expandSynTypes a & expandSynTypes b
where
	expandSynTypes rem_annots common_defs (type1,type2) ets
		#! (changed_type1,type1,ets) = expandSynTypes rem_annots common_defs type1 ets
		   (changed_type2,type2,ets) = expandSynTypes rem_annots common_defs type2 ets
		= (changed_type1 || changed_type2,(type1,type2),ets)

instance expandSynTypes AType
where
	expandSynTypes rem_annots common_defs atype ets
		= expand_syn_types_in_a_type rem_annots common_defs atype ets
	where
		expand_syn_types_in_a_type :: !.Int !{#.CommonDefs} !.AType !*ExpandTypeState -> (!.Bool,!AType,!.ExpandTypeState)
		expand_syn_types_in_a_type rem_annots common_defs atype=:{at_type = at_type=: TA type_symb types,at_attribute} ets
			# (changed,at_type, ets) = expand_syn_types_in_TA rem_annots common_defs at_type at_attribute ets
			| changed
				= (True,{ atype & at_type = at_type }, ets)
				= (False,atype,ets)
		expand_syn_types_in_a_type rem_annots common_defs atype=:{at_type = at_type=: TAS type_symb types _,at_attribute} ets
			# (changed,at_type, ets) = expand_syn_types_in_TA rem_annots common_defs at_type at_attribute ets
			| changed
				= (True,{ atype & at_type = at_type }, ets)
				= (False,atype,ets)
		expand_syn_types_in_a_type rem_annots common_defs atype ets
			# (changed,at_type, ets) = expandSynTypes rem_annots common_defs atype.at_type ets
			| changed
				= (True,{ atype & at_type = at_type }, ets)
				= (False,atype,ets)

expand_syn_types_in_TA :: !.Int !{#.CommonDefs} !.Type !.TypeAttribute !*ExpandTypeState -> (!Bool,!Type,!.ExpandTypeState)
expand_syn_types_in_TA rem_annots common_defs ta_type attribute ets=:{ets_type_defs}
	# (glob_object,glob_module,types)	= case ta_type of
		(TA type_symb=:{type_index={glob_object,glob_module},type_ident} types)				-> (glob_object,glob_module,types)
		(TAS type_symb=:{type_index={glob_object,glob_module},type_ident} types strictness)	-> (glob_object,glob_module,types)
	# ({td_rhs,td_ident,td_args,td_attribute},ets_type_defs) = ets_type_defs![glob_module].[glob_object]
	  ets = { ets & ets_type_defs = ets_type_defs }
	= case td_rhs of
		SynType rhs_type
			-> expand_type types td_args td_attribute rhs_type rem_annots attribute ets
		AbstractSynType _ rhs_type
			| (rem_annots bitand ExpandAbstractSynTypesMask)<>0
				-> expand_type types td_args td_attribute rhs_type rem_annots attribute ets
				# ets = {ets & ets_contains_unexpanded_abs_syn_type=True }
				#! (changed,types, ets) = expandSynTypes rem_annots common_defs types ets
				# ta_type = if changed
								( case ta_type of
									TA  type_symb _				-> TA  type_symb types
									TAS type_symb _ strictness	-> TAS type_symb types strictness
								) ta_type
				| glob_module == ets.ets_main_dcl_module_n
					-> (changed,ta_type, ets)
					-> (changed,ta_type, collect_imported_constructors common_defs glob_module td_rhs ets)
		NewType {ds_index}
			# {cons_type={st_args=[arg_type:_]}} = common_defs.[glob_module].com_cons_defs.[ds_index];
			-> expand_type types td_args td_attribute arg_type rem_annots attribute ets
		_
			#! (changed,types, ets) = expandSynTypes rem_annots common_defs types ets
			# ta_type = if changed
							( case ta_type of
								TA  type_symb _				-> TA  type_symb types
								TAS type_symb _ strictness	-> TAS type_symb types strictness
							) ta_type
			| glob_module == ets.ets_main_dcl_module_n || (rem_annots bitand DontCollectImportedConstructorsAndRestorePointers)<>0
				-> (changed,ta_type, ets)
				-> (changed,ta_type, collect_imported_constructors common_defs glob_module td_rhs ets)
where
	expand_type types td_args td_attribute rhs_type rem_annots attribute ets
		| (rem_annots bitand DontCollectImportedConstructorsAndRestorePointers)==0
			# (type,ets_type_heaps) = bind_and_substitute_before_expand types td_args td_attribute rhs_type rem_annots attribute ets.ets_type_heaps
			  (_,type,ets) = expandSynTypes rem_annots common_defs type {ets & ets_type_heaps = ets_type_heaps}
			= (True,type,ets)
			# (type,rev_tv_infos,ets_type_heaps) = bind_save_and_substitute_before_expand types td_args td_attribute rhs_type rem_annots attribute ets.ets_type_heaps
			  (_,type,ets=:{ets_type_heaps}) = expandSynTypes rem_annots common_defs type {ets & ets_type_heaps = ets_type_heaps}
			  th_vars = fold2St restore_tv_info (reverse rev_tv_infos) td_args ets_type_heaps.th_vars
			= (True,type,{ets & ets_type_heaps = {ets_type_heaps & th_vars=th_vars}})
	where
		bind_and_substitute_before_expand types td_args td_attribute rhs_type rem_annots attribute ets_type_heaps
			# ets_type_heaps = bind_attr td_attribute attribute ets_type_heaps
			  ets_type_heaps = fold2St bind_var_and_attr td_args types ets_type_heaps
			= substitute_rhs rem_annots rhs_type.at_type ets_type_heaps
		where
			bind_var_and_attr {atv_attribute = TA_Var {av_info_ptr}, atv_variable = {tv_info_ptr}} {at_attribute,at_type} type_heaps=:{th_vars,th_attrs}
				= {type_heaps & th_vars = th_vars <:= (tv_info_ptr, TVI_Type at_type), th_attrs = th_attrs <:= (av_info_ptr, AVI_Attr at_attribute)}
			bind_var_and_attr {atv_variable = {tv_info_ptr}} {at_type} type_heaps=:{th_vars}
				= {type_heaps & th_vars = th_vars <:= (tv_info_ptr, TVI_Type at_type)}
	
		bind_save_and_substitute_before_expand types td_args td_attribute rhs_type rem_annots attribute ets_type_heaps
			# ets_type_heaps=:{th_vars,th_attrs} = bind_attr td_attribute attribute ets_type_heaps
			  (rev_tv_infos,th_vars,th_attrs) = fold2St bind_and_save_var_and_attr td_args types ([],th_vars,th_attrs)
			  (type,heaps) = substitute_rhs rem_annots rhs_type.at_type {ets_type_heaps & th_vars=th_vars,th_attrs=th_attrs}
			= (type,rev_tv_infos,heaps)
		where
			bind_and_save_var_and_attr {atv_attribute = TA_Var {av_info_ptr}, atv_variable = {tv_info_ptr}} {at_attribute,at_type} (rev_tv_infos,th_vars,th_attrs)
				# (tv_info,th_vars) = readPtr tv_info_ptr th_vars
				= ([tv_info:rev_tv_infos],th_vars <:= (tv_info_ptr, TVI_Type at_type), th_attrs <:= (av_info_ptr, AVI_Attr at_attribute))
			bind_and_save_var_and_attr {atv_variable = {tv_info_ptr}} {at_type} (rev_tv_infos,th_vars,th_attrs)
				# (tv_info,th_vars) = readPtr tv_info_ptr th_vars
				= ([tv_info:rev_tv_infos],th_vars <:= (tv_info_ptr, TVI_Type at_type),th_attrs)
	
		restore_tv_info tv_info {atv_variable={tv_info_ptr}} th_vars
			= writePtr tv_info_ptr tv_info th_vars

		bind_attr (TA_Var {av_info_ptr}) attribute type_heaps=:{th_attrs}
			= {type_heaps & th_attrs = th_attrs <:= (av_info_ptr, AVI_Attr attribute)}
		bind_attr _ attribute type_heaps
			= type_heaps

		substitute_rhs rem_annots rhs_type type_heaps
			| rem_annots bitand RemoveAnnotationsMask<>0
				# (_, rhs_type) = removeAnnotations rhs_type
			  	# (_,type,heaps) = substitute rhs_type type_heaps
			  	= (type,heaps)
			  	# (_,type,heaps) = substitute rhs_type type_heaps
			  	= (type,heaps)

	collect_imported_constructors :: !{#.CommonDefs} !.Int !.TypeRhs !*ExpandTypeState -> .ExpandTypeState
	collect_imported_constructors common_defs mod_index (RecordType {rt_constructor}) ets=:{ets_collected_conses,ets_var_heap}
		# (ets_collected_conses, ets_var_heap)
				= collect_imported_constructor mod_index common_defs.[mod_index].com_cons_defs rt_constructor (ets_collected_conses, ets_var_heap)
		= { ets & ets_collected_conses = ets_collected_conses, ets_var_heap = ets_var_heap }
	collect_imported_constructors common_defs mod_index (AlgType constructors) ets=:{ets_collected_conses,ets_var_heap}
		# (ets_collected_conses, ets_var_heap) 
				= foldSt (collect_imported_constructor mod_index common_defs.[mod_index].com_cons_defs) constructors (ets_collected_conses, ets_var_heap)
		= { ets & ets_collected_conses = ets_collected_conses, ets_var_heap = ets_var_heap }
	collect_imported_constructors common_defs mod_index _ ets
		= ets
	
	collect_imported_constructor :: !.Int !{#.ConsDef} !.DefinedSymbol !*(!u:[v:(Global .Int)],!*(Heap VarInfo)) -> (!w:[x:(Global Int)],!.(Heap VarInfo)), [u <= w,v <= x]
	collect_imported_constructor mod_index cons_defs {ds_index} (collected_conses, var_heap)
		# {cons_type_ptr} = cons_defs.[ds_index]
		  (type_info, var_heap) = readVarInfo cons_type_ptr var_heap
		| has_been_collected type_info
			= (collected_conses, var_heap)
			= ([{ glob_module = mod_index, glob_object = ds_index } : collected_conses ], writeVarInfo cons_type_ptr VI_Used var_heap)
	where
		has_been_collected VI_Used				= True
		has_been_collected (VI_ExpandedType _)	= True
		has_been_collected _					= False

class substitute a :: !a !*TypeHeaps -> (!Bool, !a, !*TypeHeaps)

instance substitute AType
where
	substitute atype=:{at_attribute,at_type} heaps
		# (changed_attribute, at_attribute_r, heaps) = substitute at_attribute heaps
		# (changed_type, at_type_r, heaps) = substitute at_type heaps
		| changed_attribute
			| changed_type
				= (True, {at_attribute = at_attribute_r, at_type = at_type_r}, heaps)
				= (True, {atype & at_attribute = at_attribute_r}, heaps)
			| changed_type
				= (True, {atype & at_type = at_type_r}, heaps)
				= (False, atype, heaps)

instance substitute TypeAttribute
where
	substitute (TA_Var {av_info_ptr}) heaps=:{th_attrs}
		= case sreadPtr av_info_ptr th_attrs of
			AVI_Attr attr
				-> (True, attr, heaps)
			_
				-> (True, TA_Multi, heaps)
	substitute (TA_RootVar {av_info_ptr}) heaps=:{th_attrs}
		= case sreadPtr av_info_ptr th_attrs of
			AVI_Attr attr
				-> (True, attr, heaps)
			_
				-> (True, TA_Multi, heaps)
	substitute TA_None heaps
		= (True, TA_Multi, heaps)
	substitute attr heaps
		= (False, attr, heaps)

instance substitute Type
where
	substitute tv=:(TV {tv_info_ptr}) heaps=:{th_vars}
		# (tv_info, th_vars) = readPtr tv_info_ptr th_vars
		  heaps & th_vars = th_vars
		= case tv_info of
			TVI_Type type
				-> (True, type, heaps)
			_
				-> (False, tv, heaps)
	substitute type=:(arg_type --> res_type) heaps
		# (changed_arg_type, arg_type_r, heaps) = substitute arg_type heaps
		# (changed_res_type, res_type_r, heaps) = substitute res_type heaps
		| changed_arg_type
			| changed_res_type
				= (True, arg_type_r --> res_type_r, heaps)
				= (True, arg_type_r --> res_type, heaps)
			| changed_res_type
				= (True, arg_type --> res_type_r, heaps)
				= (False, type, heaps)
	substitute type=:(TA cons_id cons_args) heaps
		# (changed, cons_args_r, heaps) = substitute cons_args heaps
		| changed
			= (True, TA cons_id cons_args_r, heaps)
			= (False, type, heaps)
	substitute type=:(TAS cons_id cons_args strictness) heaps
		# (changed, cons_args_r, heaps) = substitute cons_args heaps
		| changed
			= (True, TAS cons_id cons_args_r strictness, heaps)
			= (False, type, heaps)
	substitute type=:(CV type_var :@: types) heaps=:{th_vars}
		# (tv_info, th_vars) = readPtr type_var.tv_info_ptr th_vars
		  heaps & th_vars = th_vars
		  (changed, types_r, heaps) = substitute types heaps
		| changed
			= case tv_info of
				TVI_Type s_type
					# (ok, simplified_type) = simplifyAndCheckTypeApplication s_type types_r
					| ok
						-> (True, simplified_type, heaps)
						// this will lead to a kind check error later on
						-> 	(True, CV type_var :@: types_r, heaps)
				_
					-> 	(True, CV type_var :@: types_r, heaps)
			= case tv_info of
				TVI_Type s_type
					# (ok, simplified_type) = simplifyAndCheckTypeApplication s_type types
					| ok
						-> (True, simplified_type, heaps)
						// this will lead to a kind check error later on
						-> 	(False, type, heaps)
				_
					-> 	(False, type, heaps)
	substitute type=:(TArrow1 arg_type) heaps
		# (changed, arg_type_r, heaps) = substitute arg_type heaps
		| changed
			= (True, TArrow1 arg_type_r, heaps)
			= (False, type, heaps)
	substitute type heaps
		= (False, type, heaps)

instance substitute [a] | substitute a
where
	substitute lt=:[t:ts] heaps
		# (changed_t, t_r, heaps) = substitute t heaps
		  (changed_ts, ts_r, heaps) = substitute ts heaps
		| changed_t
			| changed_ts
				= (True, [t_r:ts_r], heaps)
				= (True, [t_r:ts], heaps)
			| changed_ts
				= (True, [t:ts_r], heaps)
				= (False, lt, heaps)
	substitute [] heaps
		= (False, [], heaps)

instance substitute TypeContext
where
	substitute tc=:{tc_types} heaps
		# (changed_tc_types, tc_types_r, heaps) = substitute tc_types heaps
		| changed_tc_types
			= (True, {tc & tc_types = tc_types_r}, heaps)
			= (False, tc, heaps)

instance substitute AttributeVar
where
	substitute av=:{av_info_ptr} heaps=:{th_attrs}
		= case sreadPtr av_info_ptr th_attrs of
			AVI_Attr (TA_Var attr_var)
				-> (True, attr_var, heaps)
			_
				-> (False, av, heaps)

instance substitute AttrInequality
where
	substitute {ai_demanded,ai_offered} heaps
		# (changed_ai_demanded, ai_demanded_r, heaps) = substitute ai_demanded heaps
		  (changed_ai_offered, ai_offered_r, heaps) = substitute ai_offered heaps
		| changed_ai_demanded
			| changed_ai_offered
				= (True, {ai_demanded = ai_demanded_r, ai_offered = ai_offered_r}, heaps)
				= (True, {ai_demanded = ai_demanded_r, ai_offered = ai_offered}, heaps)
			| changed_ai_offered
				= (True, {ai_demanded = ai_demanded, ai_offered = ai_offered_r}, heaps)
				= (False, {ai_demanded = ai_demanded, ai_offered = ai_offered}, heaps)

instance substitute CaseType
where
	substitute {ct_pattern_type, ct_result_type, ct_cons_types} heaps
		# (changed_pattern_type, pattern_type_r, heaps) = substitute ct_pattern_type heaps
		  (changed_result_type, result_type_r, heaps) = substitute ct_result_type heaps
		  (changed_cons_types, cons_types_r, heaps) = substitute ct_cons_types heaps
		| changed_pattern_type
			| changed_result_type
				| changed_cons_types
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=result_type_r, ct_cons_types=cons_types_r}, heaps)
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=result_type_r, ct_cons_types=ct_cons_types}, heaps)
				| changed_cons_types
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=ct_result_type, ct_cons_types=cons_types_r}, heaps)
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=ct_result_type, ct_cons_types=ct_cons_types}, heaps)
			| changed_result_type
				| changed_cons_types
					= (True, {ct_pattern_type=ct_pattern_type, ct_result_type=result_type_r, ct_cons_types=cons_types_r}, heaps)
					= (True, {ct_pattern_type=ct_pattern_type, ct_result_type=result_type_r, ct_cons_types=ct_cons_types}, heaps)
				| changed_cons_types
					= (True, {ct_pattern_type=ct_pattern_type, ct_result_type=ct_result_type, ct_cons_types=cons_types_r}, heaps)
					= (False, {ct_pattern_type=ct_pattern_type, ct_result_type=ct_result_type, ct_cons_types=ct_cons_types}, heaps)

class removeAnnotations a :: !a  -> (!Bool, !a)

instance removeAnnotations (a,b) | removeAnnotations a & removeAnnotations b
where
	removeAnnotations t=:(x,y)
		# (rem_x, x) = removeAnnotations x
		  (rem_y, y) = removeAnnotations y
		| rem_x || rem_y
			= (True, (x,y))
			= (False, t)
	
instance removeAnnotations [a] | removeAnnotations a
where
	removeAnnotations l=:[x:xs]
		# (rem_x, x) = removeAnnotations x
		  (rem_xs, xs) = removeAnnotations xs
		| rem_x || rem_xs
			= (True, [x:xs])
			= (False, l)
	removeAnnotations el
		= (False, el)

instance removeAnnotations Type
where
	removeAnnotations t=:(arg_type --> res_type)
		# (rem, (arg_type, res_type)) = removeAnnotations (arg_type, res_type)
		| rem 
			= (True, arg_type --> res_type)
			= (False, t)
	removeAnnotations t=:(TA cons_id cons_args)
		# (rem, cons_args) = removeAnnotations cons_args
		| rem 
			= (True, TA cons_id cons_args)
			= (False, t)
	removeAnnotations t=:(TAS cons_id cons_args _)
		# (rem, cons_args) = removeAnnotations cons_args
		| rem 
			= (True, TA cons_id cons_args)
			= (False, t)
	removeAnnotations t=:(TArrow1 arg_type)
		# (rem, arg_type) = removeAnnotations arg_type
		| rem 
			= (True, TArrow1 arg_type)
			= (False, t)
	removeAnnotations t=:(cv :@: types)
		# (rem, types) = removeAnnotations types
		| rem 
			= (True, cv :@: types)
			= (False, t)
	removeAnnotations type
		= (False, type)

instance removeAnnotations AType
where
	removeAnnotations atype=:{at_type}
		# (rem, at_type) = removeAnnotations at_type
		| rem
			= (True, { atype & at_type = at_type })
			= (False, atype)

instance removeAnnotations SymbolType
where
	removeAnnotations st=:{st_args,st_result,st_args_strictness}
		# (rem, (st_args,st_result)) = removeAnnotations (st_args,st_result)
		| rem
			= (True, { st & st_args = st_args, st_args_strictness=NotStrict, st_result = st_result })
		| is_not_strict st_args_strictness
			= (False, st)
			= (True, { st & st_args_strictness=NotStrict })
