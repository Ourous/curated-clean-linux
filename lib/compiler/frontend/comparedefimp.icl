implementation module comparedefimp

from StdFunc import return
import syntax, checksupport, compare_constructor, utilities, compare_types

compare_members_of_exported_classes :: !(Optional {#{#Int}}) !Int !Int !*(CommonDefsR b) !*{#DclModule} !*CheckState -> (!*(CommonDefsR b),!*{#DclModule},!*CheckState)
compare_members_of_exported_classes (Yes conversion_table) n_specified_icl_classes n_specified_icl_members icl_common=:{com_class_defs} dcl_modules cs
	| n_specified_icl_classes==0
		= (icl_common,dcl_modules,cs)
	#! main_dcl_module_n=cs.cs_x.x_main_dcl_module_n
	# class_conversion_table = conversion_table.[cClassDefs]
	  member_conversion_table = conversion_table.[cMemberDefs]
	  ({com_class_defs=dcl_class_defs},dcl_modules) = dcl_modules![main_dcl_module_n].dcl_common
	  (com_class_defs,cs) = check_members_of_exported_classes 0 class_conversion_table member_conversion_table n_specified_icl_classes n_specified_icl_members com_class_defs dcl_class_defs cs
	= (icl_common,dcl_modules,cs)
where
	check_members_of_exported_classes :: !Int !{#Int} !{#Int} !Int !Int !{#ClassDef} !{#ClassDef} !*CheckState -> (!{#ClassDef},!*CheckState)
	check_members_of_exported_classes dcl_class_index class_conversion_table member_conversion_table n_specified_icl_classes n_specified_icl_members com_class_defs dcl_class_defs cs
		| dcl_class_index<size class_conversion_table
			# icl_class_index = class_conversion_table.[dcl_class_index]
			| icl_class_index>=0 && icl_class_index<n_specified_icl_classes
				# cs = check_members_of_exported_class icl_class_index dcl_class_index com_class_defs dcl_class_defs n_specified_icl_members member_conversion_table cs
				= check_members_of_exported_classes (dcl_class_index+1) class_conversion_table member_conversion_table n_specified_icl_classes n_specified_icl_members com_class_defs dcl_class_defs cs
				= check_members_of_exported_classes (dcl_class_index+1) class_conversion_table member_conversion_table n_specified_icl_classes n_specified_icl_members com_class_defs dcl_class_defs cs
		= (com_class_defs,cs)

	check_members_of_exported_class :: !Int !Int !{#ClassDef} !{#ClassDef} !Int !{#Int} !*CheckState -> *CheckState
	check_members_of_exported_class icl_class_index dcl_class_index com_class_defs dcl_class_defs n_specified_icl_members member_conversion_table cs
		# dcl_class = dcl_class_defs.[dcl_class_index]
		# icl_class = com_class_defs.[icl_class_index]
		| size icl_class.class_members<>size dcl_class.class_members
			# cs_error = checkError "different number of members in class definitions in implementation and definition module" "" (setErrorAdmin (newPosition icl_class.class_ident icl_class.class_pos) cs.cs_error)
			= {cs & cs_error=cs_error}
		| size icl_class.class_macro_members<>size dcl_class.class_macro_members
			# cs_error = checkError "different number of macro members in class definitions in implementation and definition module" "" (setErrorAdmin (newPosition icl_class.class_ident icl_class.class_pos) cs.cs_error)
			= {cs & cs_error=cs_error}
			= check_member_names_of_exported_class 0 icl_class.class_members dcl_class.class_members icl_class.class_pos n_specified_icl_members member_conversion_table cs

	check_member_names_of_exported_class :: !Int !{#DefinedSymbol} !{#DefinedSymbol} Position !Int !{#Int} !*CheckState -> *CheckState
	check_member_names_of_exported_class member_n icl_class_members dcl_class_members icl_class_pos n_specified_icl_members member_conversion_table cs
		| member_n<size icl_class_members
			# dcl_index = dcl_class_members.[member_n].ds_index
			| dcl_index<0 || dcl_index>=size member_conversion_table
				= check_member_names_of_exported_class (member_n+1) icl_class_members dcl_class_members icl_class_pos n_specified_icl_members member_conversion_table cs
			# converted_dcl_index = member_conversion_table.[dcl_index];
			| converted_dcl_index<0 || converted_dcl_index>=n_specified_icl_members
				# dcl_ident = dcl_class_members.[member_n].ds_ident
				# cs & cs_error = checkError "member of exported class missing in implementation module" "" (setErrorAdmin (newPosition dcl_ident icl_class_pos) cs.cs_error)
				= check_member_names_of_exported_class (member_n+1) icl_class_members dcl_class_members icl_class_pos n_specified_icl_members member_conversion_table cs
			= check_member_names_of_exported_class (member_n+1) icl_class_members dcl_class_members icl_class_pos n_specified_icl_members member_conversion_table cs
			= cs
compare_members_of_exported_classes No n_specified_icl_classes n_specified_icl_members icl_common dcl_modules cs
	= (icl_common,dcl_modules,cs)

::	CompareState =
	{	comp_type_var_heap	:: !.TypeVarHeap
	,	comp_attr_var_heap	:: !.AttrVarHeap
	,	comp_error			:: !.ErrorAdmin
	}

type_def_error		= "type definition in the impl module conflicts with the def module"
class_def_error		= "class definition in the impl module conflicts with the def module"
instance_def_error	= "instance definition in the impl module conflicts with the def module"
generic_def_error	= "generic definition in the impl module conflicts with the def module"

compareError message pos error_admin
	= popErrorAdmin (checkError "" message (pushErrorAdmin pos error_admin))

compareTypeDefs ::  !{# Int} !{#Bool} !{# CheckedTypeDef} !{# ConsDef} !u:{# CheckedTypeDef} !v:{# ConsDef} !*CompareState
	-> (!u:{# CheckedTypeDef}, !v:{# ConsDef}, !*CompareState) 
compareTypeDefs dcl_sizes copied_from_dcl dcl_type_defs dcl_cons_defs icl_type_defs icl_cons_defs comp_st
	# nr_of_dcl_types = dcl_sizes.[cTypeDefs]
	= iFoldSt (compare_type_defs copied_from_dcl dcl_type_defs dcl_cons_defs) 0 nr_of_dcl_types (icl_type_defs, icl_cons_defs, comp_st)
where
	compare_type_defs :: !{# Bool} !{# CheckedTypeDef} !{# ConsDef} !Index (!u:{# CheckedTypeDef}, !v:{# ConsDef}, !*CompareState)
		-> (!u:{# CheckedTypeDef}, !v:{# ConsDef}, !*CompareState) 
	compare_type_defs copied_from_dcl dcl_type_defs dcl_cons_defs type_index (icl_type_defs, icl_cons_defs, comp_st=:{comp_type_var_heap,comp_attr_var_heap})
		| not copied_from_dcl.[type_index]
			# dcl_type_def = dcl_type_defs.[type_index]
			  (icl_type_def, icl_type_defs) = icl_type_defs![type_index]
			  comp_type_var_heap = initialyseATypeVars dcl_type_def.td_args icl_type_def.td_args comp_type_var_heap
			  comp_attr_var_heap = initialyseAttributeVars dcl_type_def.td_attrs icl_type_def.td_attrs comp_attr_var_heap
			  comp_st = { comp_st & comp_type_var_heap = comp_type_var_heap, comp_attr_var_heap = comp_attr_var_heap }
			  (ok, icl_cons_defs, comp_st) = compare_rhs_of_types dcl_type_def.td_rhs icl_type_def.td_rhs dcl_cons_defs icl_cons_defs comp_st
			| ok && dcl_type_def.td_arity==icl_type_def.td_arity && dcl_type_def.td_attribute==icl_type_def.td_attribute
				= (icl_type_defs, icl_cons_defs, comp_st)
				# comp_error = compareError type_def_error (newPosition icl_type_def.td_ident icl_type_def.td_pos) comp_st.comp_error
				= (icl_type_defs, icl_cons_defs, { comp_st & comp_error = comp_error })
			= (icl_type_defs, icl_cons_defs, comp_st)

	compare_rhs_of_types (AlgType dclConstructors) (AlgType iclConstructors) dcl_cons_defs icl_cons_defs comp_st
		= compare_constructor_lists dclConstructors iclConstructors dcl_cons_defs icl_cons_defs comp_st
	compare_rhs_of_types (SynType dclType) (SynType iclType) dcl_cons_defs icl_cons_defs comp_st
		# (ok, comp_st) = compare dclType iclType comp_st
		= (ok, icl_cons_defs, comp_st)
	compare_rhs_of_types (RecordType dclRecord) (RecordType iclRecord) dcl_cons_defs icl_cons_defs comp_st
		= compare_records dclRecord iclRecord dcl_cons_defs icl_cons_defs comp_st
	where
		compare_records dcl_rec icl_rec dcl_cons_defs icl_cons_defs comp_st
			# nr_of_dcl_fields = size dcl_rec.rt_fields
			| nr_of_dcl_fields == size icl_rec.rt_fields && compare_fields nr_of_dcl_fields dcl_rec.rt_fields icl_rec.rt_fields
				&& icl_rec.rt_is_boxed_record==dcl_rec.rt_is_boxed_record
				= compare_constructors True dcl_rec.rt_constructor.ds_index dcl_cons_defs icl_cons_defs comp_st
				= (False, icl_cons_defs, comp_st)
		
		compare_fields field_nr dcl_fields icl_fields
			| field_nr == 0
				= True
				# field_nr = dec field_nr
				= dcl_fields.[field_nr].fs_index == icl_fields.[field_nr].fs_index && compare_fields field_nr dcl_fields icl_fields
	compare_rhs_of_types (NewType dclConstructor) (NewType iclConstructor) dcl_cons_defs icl_cons_defs comp_st
		| dclConstructor.ds_index<>iclConstructor.ds_index
			= (False, icl_cons_defs, comp_st)
		# dcl_cons_def = dcl_cons_defs.[dclConstructor.ds_index]
		  (icl_cons_def, icl_cons_defs) = icl_cons_defs![iclConstructor.ds_index]
		# (ok, comp_st) = compare_cons_def_types True icl_cons_def dcl_cons_def comp_st
		= (ok, icl_cons_defs, comp_st)
	compare_rhs_of_types (AbstractType _) (NewType _) dcl_cons_defs icl_cons_defs comp_st
		= (False, icl_cons_defs, comp_st)
	compare_rhs_of_types (AbstractType _) icl_type dcl_cons_defs icl_cons_defs comp_st
		= (True, icl_cons_defs, comp_st)
	compare_rhs_of_types (AbstractSynType _ dclType) (SynType iclType) dcl_cons_defs icl_cons_defs comp_st
		# (ok, comp_st) = compare dclType iclType comp_st
		= (ok, icl_cons_defs, comp_st)
	compare_rhs_of_types (ExtensibleAlgType []) (ExtensibleAlgType []) dcl_cons_defs icl_cons_defs comp_st
		= (True, icl_cons_defs, comp_st)
	compare_rhs_of_types (ExtensibleAlgType dclConstructors) (ExtensibleAlgType iclConstructors) dcl_cons_defs icl_cons_defs comp_st
		= compare_constructor_lists dclConstructors iclConstructors dcl_cons_defs icl_cons_defs comp_st
	compare_rhs_of_types (AlgConses dclConstructors dcl_type_index) (AlgConses iclConstructors icl_type_index) dcl_cons_defs icl_cons_defs comp_st
		| dcl_type_index==icl_type_index
			= compare_constructor_lists dclConstructors iclConstructors dcl_cons_defs icl_cons_defs comp_st
			= (False, icl_cons_defs, comp_st)
	compare_rhs_of_types dcl_type icl_type dcl_cons_defs icl_cons_defs comp_st
		= (False, icl_cons_defs, comp_st)

	compare_constructor_lists [dcl_cons : dcl_conses] [icl_cons : icl_conses] dcl_cons_defs icl_cons_defs comp_st
		| dcl_cons.ds_index == icl_cons.ds_index
			# last_cons = isEmpty dcl_conses
			# (ok, icl_cons_defs, comp_st) = compare_constructors last_cons dcl_cons.ds_index dcl_cons_defs icl_cons_defs comp_st
			| ok
				| last_cons
					= (isEmpty icl_conses, icl_cons_defs, comp_st) 
					= compare_constructor_lists dcl_conses icl_conses dcl_cons_defs icl_cons_defs comp_st
				= (False, icl_cons_defs, comp_st)	
			= (False, icl_cons_defs, comp_st)
	compare_constructor_lists _ _ dcl_cons_defs icl_cons_defs comp_st
		= (False, icl_cons_defs, comp_st)	

	compare_constructors do_compare_result_types cons_index dcl_cons_defs icl_cons_defs comp_st
		# dcl_cons_def = dcl_cons_defs.[cons_index]
		  (icl_cons_def, icl_cons_defs) = icl_cons_defs![cons_index]
		  (ok, comp_st) = compare_cons_def_types do_compare_result_types icl_cons_def dcl_cons_def comp_st
		= (ok, icl_cons_defs, comp_st)

	compare_cons_def_types do_compare_result_types icl_cons_def dcl_cons_def comp_st=:{comp_type_var_heap}
		| dcl_cons_def.cons_priority<>icl_cons_def.cons_priority
			= (False,comp_st)
		# dcl_cons_type = dcl_cons_def.cons_type
		  icl_cons_type = icl_cons_def.cons_type
		  comp_type_var_heap = initialyseATypeVars dcl_cons_def.cons_exi_vars icl_cons_def.cons_exi_vars comp_type_var_heap
		  comp_st = { comp_st & comp_type_var_heap = comp_type_var_heap }
		  (ok, comp_st) = compare (dcl_cons_type.st_args,dcl_cons_type.st_args_strictness) (icl_cons_type.st_args,icl_cons_type.st_args_strictness) comp_st
		| not ok
			= (False,comp_st)
		| do_compare_result_types
			# (ok,comp_st) = compare dcl_cons_type.st_result icl_cons_type.st_result comp_st
			| ok
				= compare dcl_cons_type.st_context icl_cons_type.st_context comp_st
				= (False,comp_st)
			= compare dcl_cons_type.st_context icl_cons_type.st_context comp_st

compareClassDefs :: !{#Int} {#Bool} !{# ClassDef} !{# MemberDef} !u:{# ClassDef} !v:{# MemberDef} !*CompareState
	-> (!u:{# ClassDef}, !v:{# MemberDef}, !*CompareState)
compareClassDefs dcl_sizes copied_from_dcl dcl_class_defs dcl_member_defs icl_class_defs icl_member_defs comp_st
	# nr_of_dcl_classes = dcl_sizes.[cClassDefs]
	= iFoldSt (compare_class_defs copied_from_dcl dcl_class_defs dcl_member_defs) 0 nr_of_dcl_classes (icl_class_defs, icl_member_defs, comp_st)
where	  	
	compare_class_defs ::  !{# Bool} {# ClassDef} {# MemberDef} !Index (!u:{# ClassDef}, !v:{# MemberDef}, !*CompareState)
		-> (!u:{# ClassDef}, v:{# MemberDef}, !*CompareState)
	compare_class_defs copied_from_dcl dcl_class_defs dcl_member_defs class_index (icl_class_defs, icl_member_defs, comp_st)
		| not copied_from_dcl.[class_index]
			# dcl_class_def = dcl_class_defs.[class_index]
			  (icl_class_def, icl_class_defs) = icl_class_defs![class_index]
			  (ok, icl_member_defs, comp_st) = compare_classes dcl_class_def dcl_member_defs icl_class_def icl_member_defs comp_st
			| ok
				= (icl_class_defs, icl_member_defs, comp_st)
				# comp_error = compareError class_def_error (newPosition icl_class_def.class_ident icl_class_def.class_pos) comp_st.comp_error
				= (icl_class_defs, icl_member_defs, { comp_st & comp_error = comp_error })
			= (icl_class_defs, icl_member_defs, comp_st)

	compare_classes dcl_class_def dcl_member_defs icl_class_def icl_member_defs comp_st=:{comp_type_var_heap}
		# comp_type_var_heap = initialyseTypeVars dcl_class_def.class_args icl_class_def.class_args comp_type_var_heap
		  comp_st = { comp_st & comp_type_var_heap = comp_type_var_heap }
		# (ok, comp_st) = compare dcl_class_def.class_context icl_class_def.class_context comp_st
		| not ok
			= (False, icl_member_defs, comp_st)
		# nr_of_dcl_members = size dcl_class_def.class_members
		| nr_of_dcl_members <> size icl_class_def.class_members
			= (False, icl_member_defs, comp_st)
		# (ok, icl_member_defs, comp_st) = compare_array_of_class_members nr_of_dcl_members dcl_class_def.class_members icl_class_def.class_members dcl_member_defs icl_member_defs comp_st
		| not ok
			= (False, icl_member_defs, comp_st)
		# n_dcl_class_macro_members = size dcl_class_def.class_macro_members
		| n_dcl_class_macro_members <> size icl_class_def.class_macro_members
			= (False, icl_member_defs, comp_st)
		| sort_clas_macro_members dcl_class_def.class_macro_members <> sort_clas_macro_members icl_class_def.class_macro_members
			= (False, icl_member_defs, comp_st)
			= (True, icl_member_defs, comp_st)

	compare_array_of_class_members loc_member_index dcl_members icl_members dcl_member_defs icl_member_defs comp_st
		| loc_member_index == 0
			= (True, icl_member_defs, comp_st)
		# loc_member_index = dec loc_member_index 
		# dcl_member = dcl_members.[loc_member_index]
		# icl_member = icl_members.[loc_member_index]
		| dcl_member == icl_member
			# glob_member_index = dcl_member.ds_index
			# dcl_member_def = dcl_member_defs.[glob_member_index]
			  (icl_member_def, icl_member_defs) = icl_member_defs![glob_member_index]
			  (ok, comp_st) = compare dcl_member_def.me_type icl_member_def.me_type comp_st
			| ok && dcl_member_def.me_priority == icl_member_def.me_priority
				&& compare_default_implementations dcl_member_def.me_default_implementation icl_member_def.me_default_implementation
				= compare_array_of_class_members loc_member_index dcl_members icl_members dcl_member_defs icl_member_defs comp_st
				= (False, icl_member_defs, comp_st)
			= (False, icl_member_defs, comp_st)

	compare_default_implementations No No = True
	compare_default_implementations (Yes _) (Yes _) = True
	compare_default_implementations _ _ = False

	sort_clas_macro_members class_macro_members
		= sort [id_name \\ {mm_ident={id_name}}<-:class_macro_members]

compareInstanceDefs :: !{# Int} !{# ClassInstance} !u:{# ClassInstance} !*{#FunDef} !*CompareState
											   -> (!u:{# ClassInstance},!*{#FunDef},!*CompareState)
compareInstanceDefs dcl_sizes dcl_instance_defs icl_instance_defs icl_functions comp_st
	# nr_of_dcl_instances = dcl_sizes.[cInstanceDefs]
	= iFoldSt (compare_instance_defs dcl_instance_defs) 0 nr_of_dcl_instances (icl_instance_defs,icl_functions,comp_st)
where
	compare_instance_defs ::  !{# ClassInstance} !Index !(!u:{# ClassInstance},!*{#FunDef},!*CompareState)
													  -> (!u:{# ClassInstance},!*{#FunDef},!*CompareState)
	compare_instance_defs dcl_instance_defs instance_index (icl_instance_defs,icl_functions,comp_st)
		# dcl_instance_def = dcl_instance_defs.[instance_index]
		  (icl_instance_def, icl_instance_defs) = icl_instance_defs![instance_index]
		  (ok, comp_st) = compare dcl_instance_def.ins_type icl_instance_def.ins_type comp_st
		| not ok
			# comp_st = instance_def_conflicts_error icl_instance_def.ins_ident icl_instance_def.ins_pos comp_st
			= (icl_instance_defs,icl_functions, comp_st)
		# (icl_functions,comp_st)
			= member_types_equal dcl_instance_def.ins_member_types_and_functions icl_instance_def.ins_members 0 icl_functions comp_st
		= (icl_instance_defs,icl_functions,comp_st)

	member_types_equal :: [DclInstanceMemberTypeAndFunction] {#ClassInstanceMember} Int *{#FunDef} *CompareState -> (!*{#FunDef},!*CompareState)
	member_types_equal [] icl_instance_members icl_member_n icl_functions comp_st
		| icl_member_n<size icl_instance_members
			# function_index = icl_instance_members.[icl_member_n].cim_index
			| icl_functions.[function_index].fun_info.fi_properties bitand FI_MemberInstanceRequiresTypeInDefMod<>0
			  	# ({fun_ident,fun_pos},icl_functions) = icl_functions![function_index]
				# comp_st = instance_def_conflicts_error fun_ident fun_pos comp_st
				= member_types_equal [] icl_instance_members (icl_member_n+1) icl_functions comp_st
				= member_types_equal [] icl_instance_members (icl_member_n+1) icl_functions comp_st
			= (icl_functions,comp_st)
	member_types_equal [{dim_type=instance_member_type,dim_function_index}:instance_member_types] icl_instance_members icl_member_n icl_functions comp_st
		= member_type_and_types_equal instance_member_type instance_member_types icl_instance_members icl_member_n icl_functions comp_st
	where
		member_type_and_types_equal instance_member_type=:{ft_ident,ft_type,ft_pos} instance_member_types icl_instance_members icl_member_n icl_functions comp_st
			| icl_member_n<size icl_instance_members
				# {cim_ident,cim_index} = icl_instance_members.[icl_member_n]
				| ft_ident.id_name<>cim_ident.id_name
					| icl_functions.[cim_index].fun_info.fi_properties bitand FI_MemberInstanceRequiresTypeInDefMod<>0
					  	# ({fun_ident,fun_pos},icl_functions) = icl_functions![cim_index]
						# comp_st = instance_def_conflicts_error fun_ident fun_pos comp_st
						= member_type_and_types_equal instance_member_type instance_member_types icl_instance_members (icl_member_n+1) icl_functions comp_st
						= member_type_and_types_equal instance_member_type instance_member_types icl_instance_members (icl_member_n+1) icl_functions comp_st

				  	# ({fun_type},icl_functions) = icl_functions![cim_index]
				  	# (Yes icl_instance_member_type) = fun_type

					# tc_state = { tc_type_vars = initial_hwn comp_st.comp_type_var_heap
								 , tc_attr_vars = initial_hwn comp_st.comp_attr_var_heap
								 , tc_strictness_flags = 0
				  				 }
		 			# tc_state = init_symbol_type_vars ft_type icl_instance_member_type tc_state

					# (corresponds, tc_state) = t_corresponds ft_type icl_instance_member_type tc_state
				  	# comp_st = {comp_st &	comp_type_var_heap=tc_state.tc_type_vars.hwn_heap,
				  							comp_attr_var_heap=tc_state.tc_attr_vars.hwn_heap }
					# comp_st = if (not corresponds)
									(instance_def_conflicts_error ft_ident ft_pos comp_st)
									comp_st
					= member_types_equal instance_member_types icl_instance_members (icl_member_n+1) icl_functions comp_st
			# comp_st = instance_def_conflicts_error ft_ident ft_pos comp_st
			= member_types_equal instance_member_types icl_instance_members icl_member_n icl_functions comp_st

	instance_def_conflicts_error ident pos comp_st
		= {comp_st & comp_error = compareError instance_def_error (newPosition ident pos) comp_st.comp_error }

compareGenericDefs :: !{# Int} !{#Bool} !{# GenericDef} !u:{# GenericDef} !*CompareState -> (!u:{# GenericDef}, !*CompareState)
compareGenericDefs dcl_sizes copied_from_dcl dcl_generic_defs icl_generic_defs comp_st
	# nr_of_dcl_generics = dcl_sizes.[cGenericDefs]
	= iFoldSt (compare_generic_defs copied_from_dcl dcl_generic_defs) 0 nr_of_dcl_generics (icl_generic_defs, comp_st)
where
	compare_generic_defs :: !{#Bool} !{# GenericDef} !Index (!u:{# GenericDef}, !*CompareState) -> (!u:{# GenericDef}, !*CompareState)
	compare_generic_defs copied_from_dcl dcl_generic_defs generic_index (icl_generic_defs, comp_st)
		| not copied_from_dcl.[generic_index]
			# dcl_generic_def = dcl_generic_defs.[generic_index]
			  (icl_generic_def, icl_generic_defs) = icl_generic_defs![generic_index]
			  
			# (ok1, comp_st) = compare dcl_generic_def.gen_type icl_generic_def.gen_type comp_st
			# (ok2, comp_st) = compare dcl_generic_def.gen_vars icl_generic_def.gen_vars comp_st
			# (ok3, comp_st) = compare dcl_generic_def.gen_deps icl_generic_def.gen_deps comp_st			
			| ok1 && ok2 && ok3
				= (icl_generic_defs, comp_st)
				# comp_error = compareError generic_def_error (newPosition icl_generic_def.gen_ident icl_generic_def.gen_pos) comp_st.comp_error
				= (icl_generic_defs, { comp_st & comp_error = comp_error })
		| otherwise
			= (icl_generic_defs, comp_st)

collectGenericCaseDefMacros :: !{#GenericCaseDef} -> [(GenericCaseBody,Int)]
collectGenericCaseDefMacros dcl_generic_case_defs
	= [(gcf_body,gcf_generic_info) \\ {gc_gcf=GCF _ {gcf_body=gcf_body=:GCB_FunAndMacroIndex _ _,gcf_generic_info}} <-: dcl_generic_case_defs]

class compare a :: !a !a !*CompareState -> (!Bool, !*CompareState)

instance compare (a,b) | compare a & compare b
where
	compare (x1, y1) (x2, y2) comp_st
		# (ok, comp_st) = compare x1 x2 comp_st
		| ok
			= compare y1 y2 comp_st
			= (False, comp_st)

instance compare (Global a) | == a
where
	compare g1 g2 comp_st
		= (g1.glob_module == g2.glob_module && g1.glob_object == g2.glob_object, comp_st)

instance compare [a] | compare a
where
	compare [x:xs] [y:ys] comp_st
		= compare (x, xs) (y, ys) comp_st
	compare [] [] comp_st
		= (True, comp_st)
	compare _ _ comp_st
		= (False, comp_st)

instance compare Type
where
	compare (TA dclIdent dclArgs) (TA iclIdent iclArgs) comp_st
		= compare (dclIdent.type_index, dclArgs) (iclIdent.type_index, iclArgs) comp_st
	compare (TA dclIdent dclArgs) (TAS iclIdent iclArgs iclStrictness) comp_st
		= compare (dclIdent.type_index, (dclArgs,NotStrict)) (iclIdent.type_index, (iclArgs,iclStrictness)) comp_st
	compare (TAS dclIdent dclArgs dclStrictness) (TA iclIdent iclArgs) comp_st
		= compare (dclIdent.type_index, (dclArgs,dclStrictness)) (iclIdent.type_index, (iclArgs,NotStrict)) comp_st
	compare (TAS dclIdent dclArgs dclStrictness) (TAS iclIdent iclArgs iclStrictness) comp_st
		= compare (dclIdent.type_index, (dclArgs,dclStrictness)) (iclIdent.type_index, (iclArgs,iclStrictness)) comp_st
	compare (dclFun --> dclArg) (iclFun --> iclArg) comp_st
		= compare (dclFun, dclArg) (iclFun, iclArg) comp_st
	compare (TArrow1 dclArg) (TArrow1 iclArg) comp_st
		= compare dclArg iclArg comp_st
	compare TArrow TArrow comp_st
		= (True, comp_st)
	compare (CV dclVar :@: dclArgs) (CV iclVar :@: iclArgs) comp_st
		= compare (dclVar, dclArgs) (iclVar, iclArgs) comp_st
	compare (TB dclDef) (TB iclDef) comp_st
		= (dclDef == iclDef, comp_st)
	compare (GTV dclDef) (GTV iclDef) comp_st
		= compare dclDef iclDef comp_st
	compare (TV dclVar) (TV iclVar) comp_st
		= compare dclVar iclVar comp_st
	compare (TFA dclvars dcltype) (TFA iclvars icltype) comp_st=:{comp_type_var_heap}
		# comp_type_var_heap = initialyseATypeVars dclvars iclvars comp_type_var_heap
		  (ok, comp_st) = compare dcltype icltype {comp_st & comp_type_var_heap = comp_type_var_heap}
		  type_heaps = clear_type_vars dclvars (comp_st.comp_type_var_heap, comp_st.comp_attr_var_heap)
		  (comp_type_var_heap, comp_attr_var_heap) = clear_type_vars iclvars type_heaps
		= (ok, {comp_st & comp_type_var_heap = comp_type_var_heap, comp_attr_var_heap = comp_attr_var_heap})
	compare (TFAC dclvars dcltype dcl_contexts) (TFAC iclvars icltype icl_contexts) comp_st=:{comp_type_var_heap}
		# comp_type_var_heap = initialyseATypeVars dclvars iclvars comp_type_var_heap
		  (ok, comp_st) = compare (dcltype,dcl_contexts) (icltype,icl_contexts) {comp_st & comp_type_var_heap = comp_type_var_heap}
		  type_heaps = clear_type_vars dclvars (comp_st.comp_type_var_heap, comp_st.comp_attr_var_heap)
		  (comp_type_var_heap, comp_attr_var_heap) = clear_type_vars iclvars type_heaps
		= (ok, {comp_st & comp_type_var_heap = comp_type_var_heap, comp_attr_var_heap = comp_attr_var_heap})
	compare _ _ comp_st
		= (False, comp_st)

clear_type_vars vars type_and_attr_var_heaps
	= foldSt clear_type_var vars type_and_attr_var_heaps
where
	clear_type_var {atv_variable={tv_info_ptr}, atv_attribute} (type_var_heap,attr_var_heap) 
		= (type_var_heap <:= (tv_info_ptr, TVI_Empty), clear_attr_var atv_attribute attr_var_heap)
	
	clear_attr_var (TA_Var {av_info_ptr}) attr_var_heap
		= attr_var_heap <:= (av_info_ptr, AVI_Empty)
	clear_attr_var (TA_RootVar {av_info_ptr}) attr_var_heap
		= attr_var_heap <:= (av_info_ptr, AVI_Empty)
	clear_attr_var attr attr_var_heap
		= attr_var_heap

instance compare AType
where
	compare at1 at2 comp_st
		= compare (at1.at_attribute, at1.at_type) (at2.at_attribute, at2.at_type) comp_st

instance compare TypeAttribute
where
	compare ta1 ta2 comp_st
		| equal_constructor ta1 ta2
			= compare_equal_constructor ta1 ta2 comp_st
			= (False, comp_st)
	where
		compare_equal_constructor (TA_Var dclDef) (TA_Var iclDef) comp_st
			= compare dclDef iclDef comp_st
		compare_equal_constructor (TA_RootVar dclDef) (TA_RootVar iclDef) comp_st
			= compare dclDef iclDef comp_st
		compare_equal_constructor _ _ comp_st
			= (True, comp_st)
	
instance compare Annotation
where
	compare an1 an2 comp_st
		= (equal_constructor an1 an2, comp_st)

instance compare StrictnessList
where
	compare strictness1 strictness2 comp_st
		= (equal_strictness_lists strictness1 strictness2,comp_st)

instance compare AttributeVar
where
	compare {av_info_ptr = dcl_info_ptr} {av_info_ptr = icl_info_ptr} comp_st=:{comp_attr_var_heap}
		# (dcl_info, comp_attr_var_heap) = readPtr dcl_info_ptr comp_attr_var_heap
		  (icl_info, comp_attr_var_heap) = readPtr icl_info_ptr comp_attr_var_heap
		  (ok, comp_attr_var_heap) = compare_vars dcl_info icl_info dcl_info_ptr icl_info_ptr comp_attr_var_heap
		= (ok, { comp_st & comp_attr_var_heap = comp_attr_var_heap })
	where
		compare_vars AVI_Empty AVI_Empty dcl_av_info_ptr icl_av_info_ptr comp_attr_var_heap
			= (True, comp_attr_var_heap <:= (dcl_av_info_ptr, AVI_AttrVar icl_av_info_ptr) <:= (icl_av_info_ptr, AVI_AttrVar dcl_av_info_ptr))
		compare_vars (AVI_AttrVar dcl_forward) (AVI_AttrVar icl_forward) dcl_av_info_ptr icl_av_info_ptr comp_attr_var_heap
			= (dcl_forward == icl_av_info_ptr && icl_forward == dcl_av_info_ptr, comp_attr_var_heap)
		compare_vars dcl_info icl_info dcl_av_info_ptr icl_av_info_ptr comp_attr_var_heap
			= (True, comp_attr_var_heap)

instance compare TypeVar
where
	compare {tv_info_ptr = dcl_info_ptr} {tv_info_ptr = icl_info_ptr} comp_st=:{comp_type_var_heap}
		# (dcl_info, comp_type_var_heap) = readPtr dcl_info_ptr comp_type_var_heap
		  (icl_info, comp_type_var_heap) = readPtr icl_info_ptr comp_type_var_heap
		  (ok, comp_type_var_heap) = compare_vars dcl_info icl_info dcl_info_ptr icl_info_ptr comp_type_var_heap
		= (ok, { comp_st & comp_type_var_heap = comp_type_var_heap })
	where
		compare_vars TVI_Empty TVI_Empty dcl_tv_info_ptr icl_tv_info_ptr type_var_heap
			= (True, type_var_heap <:= (dcl_tv_info_ptr, TVI_TypeVar icl_tv_info_ptr) <:= (icl_tv_info_ptr, TVI_TypeVar dcl_tv_info_ptr))
		compare_vars (TVI_TypeVar dcl_forward) (TVI_TypeVar icl_forward) dcl_tv_info_ptr icl_tv_info_ptr type_var_heap
			= (dcl_forward == icl_tv_info_ptr && icl_forward == dcl_tv_info_ptr, type_var_heap)
		compare_vars dcl_info icl_info dcl_tv_info_ptr icl_tv_info_ptr type_var_heap
			= (True, type_var_heap)
		
instance compare AttrInequality
where
	compare dcl_ineq icl_ineq comp_st
		= compare (dcl_ineq.ai_demanded, dcl_ineq.ai_offered) (icl_ineq.ai_demanded, icl_ineq.ai_offered) comp_st

instance compare SymbolType
where
	compare dcl_st icl_st comp_st=:{comp_type_var_heap,comp_attr_var_heap}
		# comp_type_var_heap = initialyseTypeVars dcl_st.st_vars icl_st.st_vars comp_type_var_heap
		  comp_attr_var_heap = initialyseAttributeVars dcl_st.st_attr_vars icl_st.st_attr_vars comp_attr_var_heap
		  comp_st = { comp_st & comp_type_var_heap = comp_type_var_heap, comp_attr_var_heap = comp_attr_var_heap }
		= compare	(dcl_st.st_args, (dcl_st.st_args_strictness, (dcl_st.st_result, (dcl_st.st_context, dcl_st.st_attr_env))))
					(icl_st.st_args, (icl_st.st_args_strictness, (icl_st.st_result, (icl_st.st_context, icl_st.st_attr_env)))) comp_st

instance compare InstanceType
where
	compare dcl_it icl_it comp_st=:{comp_type_var_heap,comp_attr_var_heap}
		# comp_type_var_heap = initialyseTypeVars dcl_it.it_vars icl_it.it_vars comp_type_var_heap
		  comp_attr_var_heap = initialyseAttributeVars dcl_it.it_attr_vars icl_it.it_attr_vars comp_attr_var_heap
		  comp_st = { comp_st & comp_type_var_heap = comp_type_var_heap, comp_attr_var_heap = comp_attr_var_heap }
		= compare (dcl_it.it_types, dcl_it.it_context) (icl_it.it_types, icl_it.it_context) comp_st

instance compare TypeContext
where
	compare dcl_tc icl_tc comp_st
		| dcl_tc.tc_class == icl_tc.tc_class
			= compare dcl_tc.tc_types icl_tc.tc_types comp_st
			= (False, comp_st)

instance compare GenericDependency
where
	compare dcl_gd icl_gd comp_st
		| dcl_gd.gd_index == icl_gd.gd_index = compare dcl_gd.gd_vars icl_gd.gd_vars comp_st
		= (False, comp_st)

initialyseTypeVars [{tv_info_ptr=dcl_tv_info_ptr}:dcl_type_vars] [{tv_info_ptr=icl_tv_info_ptr}:icl_type_vars] type_var_heap
	# type_var_heap = type_var_heap <:= (icl_tv_info_ptr, TVI_TypeVar dcl_tv_info_ptr) <:= (dcl_tv_info_ptr, TVI_TypeVar icl_tv_info_ptr)
	= initialyseTypeVars dcl_type_vars icl_type_vars type_var_heap
initialyseTypeVars [{tv_info_ptr}:dcl_type_vars] [] type_var_heap
	= initialyseTypeVars dcl_type_vars [] (type_var_heap <:= (tv_info_ptr, TVI_Empty));
initialyseTypeVars [] [{tv_info_ptr}:icl_type_vars] type_var_heap
	= initialyseTypeVars [] icl_type_vars (type_var_heap <:= (tv_info_ptr, TVI_Empty));
initialyseTypeVars [] [] type_var_heap
	= type_var_heap

initialyseATypeVars [{atv_variable={tv_info_ptr=dcl_tv_info_ptr}}:dcl_type_vars] [{atv_variable={tv_info_ptr=icl_tv_info_ptr}}:icl_type_vars] type_var_heap
	# type_var_heap = type_var_heap <:= (icl_tv_info_ptr, TVI_TypeVar dcl_tv_info_ptr) <:= (dcl_tv_info_ptr, TVI_TypeVar icl_tv_info_ptr)
	= initialyseATypeVars dcl_type_vars icl_type_vars type_var_heap
initialyseATypeVars [{atv_variable={tv_info_ptr}}:dcl_type_vars] [] type_var_heap
	= initialyseATypeVars dcl_type_vars [] (type_var_heap <:= (tv_info_ptr, TVI_Empty));
initialyseATypeVars [] [{atv_variable={tv_info_ptr}}:icl_type_vars] type_var_heap
	= initialyseATypeVars [] icl_type_vars (type_var_heap <:= (tv_info_ptr, TVI_Empty));
initialyseATypeVars [] [] type_var_heap
	= type_var_heap

initialyseAttributeVars [{av_info_ptr=dcl_av_info_ptr}:dcl_type_vars] [{av_info_ptr=icl_av_info_ptr}:icl_type_vars] type_var_heap
	# type_var_heap = type_var_heap <:= (icl_av_info_ptr, AVI_AttrVar dcl_av_info_ptr) <:= (dcl_av_info_ptr, AVI_AttrVar icl_av_info_ptr)
	= initialyseAttributeVars dcl_type_vars icl_type_vars type_var_heap
initialyseAttributeVars [{av_info_ptr}:dcl_type_vars] [] type_var_heap
	= initialyseAttributeVars dcl_type_vars [] (type_var_heap <:= (av_info_ptr, AVI_Empty));
initialyseAttributeVars [] [{av_info_ptr}:icl_type_vars] type_var_heap
	= initialyseAttributeVars [] icl_type_vars (type_var_heap <:= (av_info_ptr, AVI_Empty));
initialyseAttributeVars [] [] type_var_heap
	= type_var_heap

:: TypesCorrespondState =
		{	tc_type_vars :: !.HeapWithNumber TypeVarInfo
		,	tc_attr_vars :: !.HeapWithNumber AttrVarInfo
		,	tc_strictness_flags :: !Int
		}

AllowFirstMoreStrictness:==1;
FirstHasMoreStrictness:==2;
CompareGenericCaseMacro:==4; // only used from ec_tc_state

:: TypesCorrespondMonad
		:==	*TypesCorrespondState -> *(!Bool, !*TypesCorrespondState)

:: ExpressionsCorrespondState =
		{	ec_icl_correspondences ::	!.{# Int },
			ec_dcl_correspondences ::	!.{# Int }
		,	ec_var_heap ::	!.HeapWithNumber VarInfo
		,	ec_expr_heap ::	!.ExpressionHeap
		,	ec_icl_functions :: !.{#FunDef}
		,	ec_macro_defs :: !.{#.{#FunDef}}
		,	ec_error_admin ::	!.ErrorAdmin
		,	ec_tc_state ::	!.TypesCorrespondState
		,	ec_main_dcl_module_n ::	!Int
		}
		
:: ExpressionsCorrespondMonad
		:== *ExpressionsCorrespondState -> *ExpressionsCorrespondState

:: Conversions :== {#Index}

:: HeapWithNumber a
	=	{	hwn_heap ::	!.Heap a
		,	hwn_number ::	!Int
		}

:: OptionalCorrespondenceNumber = CorrespondenceNumber !Int | Unbound
	
:: ComparisionErrorCode :== Int
// arg n not ok: n
CEC_ResultNotOK :== 0
CEC_Ok :== -1
CEC_NrArgsNotOk :== -2
CEC_StrictnessOfArgsNotOk :== -3
CEC_ContextNotOK :== -4
CEC_AttrEnvNotOK :== -5

class t_corresponds a :: !a !a -> *TypesCorrespondMonad
	// whether two types correspond
class e_corresponds a :: !a !a -> ExpressionsCorrespondMonad
	// check for correspondence of expressions

class getIdentPos a :: a -> IdentPos

class CorrespondenceNumber a where
	toCorrespondenceNumber :: .a -> OptionalCorrespondenceNumber
	fromCorrespondenceNumber :: Int -> .a

initial_hwn hwn_heap = { hwn_heap = hwn_heap, hwn_number = 0 }

compareDefImp :: !Int !DclModule !(Optional {#Index}) !CopiedDefinitions !Int !*IclModule !*{#*{#FunDef}} !*Heaps !*ErrorAdmin 
																		  -> (!.IclModule,!.{#.{#FunDef}},!.Heaps,!.ErrorAdmin)
compareDefImp main_dcl_module_n main_dcl_module No _ n_exported_global_functions icl_module macro_defs heaps error_admin
	= (icl_module, macro_defs,heaps, error_admin)
compareDefImp main_dcl_module_n main_dcl_module (Yes macro_conversion_table) {copied_type_defs,copied_class_defs,copied_generic_defs} n_exported_global_functions icl_module macro_defs heaps error_admin
//	| Trace_array icl_module.icl_functions
//		&& Trace_array macro_defs.[main_dcl_module_n]

	# {dcl_functions,dcl_macros,dcl_common} = main_dcl_module
	  {icl_common, icl_functions} = icl_module
	  {hp_var_heap, hp_expression_heap, hp_type_heaps={th_vars, th_attrs}} = heaps
	  { com_cons_defs=icl_com_cons_defs, com_type_defs = icl_com_type_defs,
		com_selector_defs=icl_com_selector_defs, com_class_defs = icl_com_class_defs,
		com_member_defs=icl_com_member_defs, com_instance_defs = icl_com_instance_defs,
		com_generic_defs=icl_com_generic_defs}
			= icl_common
	  comp_st
	  	=	{	comp_type_var_heap	= th_vars
	  		,	comp_attr_var_heap	= th_attrs
	  		,	comp_error			= error_admin
	  		}

	  (icl_com_type_defs, icl_com_cons_defs, comp_st)
	  		= compareTypeDefs main_dcl_module.dcl_sizes copied_type_defs	dcl_common.com_type_defs dcl_common.com_cons_defs
	  																		icl_com_type_defs icl_com_cons_defs comp_st
	  (icl_com_class_defs, icl_com_member_defs, comp_st)
	  		= compareClassDefs main_dcl_module.dcl_sizes copied_class_defs 	dcl_common.com_class_defs dcl_common.com_member_defs
	  																		icl_com_class_defs icl_com_member_defs comp_st

	  (icl_com_instance_defs, icl_functions, comp_st)
	  		= compareInstanceDefs main_dcl_module.dcl_sizes dcl_common.com_instance_defs icl_com_instance_defs icl_functions comp_st

	  (icl_com_generic_defs, comp_st)
	  		= compareGenericDefs main_dcl_module.dcl_sizes copied_generic_defs dcl_common.com_generic_defs icl_com_generic_defs comp_st

	  generic_case_def_macros = collectGenericCaseDefMacros dcl_common.com_gencase_defs

	  {	comp_type_var_heap = th_vars, comp_attr_var_heap = th_attrs, comp_error = error_admin } = comp_st

	  tc_state
	  		=	{ tc_type_vars = initial_hwn th_vars
				, tc_attr_vars = initial_hwn th_attrs
				, tc_strictness_flags = 0
				}
	  (icl_functions, macro_defs, hp_var_heap, hp_expression_heap, tc_state, error_admin)
			= compareMacrosWithConversion main_dcl_module_n macro_conversion_table dcl_macros generic_case_def_macros icl_functions macro_defs hp_var_heap hp_expression_heap tc_state error_admin
	  (icl_functions, tc_state, error_admin)
	  		= compareFunctionTypes n_exported_global_functions dcl_functions icl_functions tc_state error_admin
	  { tc_type_vars, tc_attr_vars } 
	   		= tc_state
	  icl_common
	  		= { icl_common & com_cons_defs=icl_com_cons_defs, com_type_defs = icl_com_type_defs,
	  			com_selector_defs=icl_com_selector_defs, com_class_defs=icl_com_class_defs,
	  			com_member_defs=icl_com_member_defs, com_instance_defs = icl_com_instance_defs,
	  			com_generic_defs=icl_com_generic_defs }
	  heaps 
	  		= { heaps & hp_var_heap = hp_var_heap, hp_expression_heap = hp_expression_heap,
	  			hp_type_heaps = { th_vars = tc_type_vars.hwn_heap, th_attrs = tc_attr_vars.hwn_heap}}
	= ({ icl_module & icl_common = icl_common, icl_functions = icl_functions },macro_defs,heaps, error_admin )

compareFunctionTypes n_exported_global_functions dcl_fun_types icl_functions tc_state error_admin
	= iFoldSt (compareTwoFunctionTypes dcl_fun_types) 0 n_exported_global_functions (icl_functions, tc_state, error_admin)

compareTwoFunctionTypes :: !{#FunType} !Int !*(!u:{#FunDef},!*TypesCorrespondState,!*ErrorAdmin)
										   -> (!v:{#FunDef},!.TypesCorrespondState,!.ErrorAdmin) , [u <= v]
compareTwoFunctionTypes dcl_fun_types dclIndex (icl_functions, tc_state, error_admin)
	# (fun_def=:{fun_type, fun_priority}, icl_functions) = icl_functions![dclIndex]
	= case fun_type of
		No	-> generate_error "type of exported function is missing" fun_def icl_functions tc_state error_admin
		Yes icl_symbol_type
			# {ft_type=dcl_symbol_type, ft_priority,ft_ident} = dcl_fun_types.[dclIndex]			
			# tc_state = init_symbol_type_vars dcl_symbol_type icl_symbol_type tc_state
			  (corresponds, tc_state)
					= t_corresponds dcl_symbol_type icl_symbol_type tc_state
			| corresponds && fun_priority==ft_priority
				-> (icl_functions, tc_state, error_admin)
			-> generate_error ErrorMessage fun_def icl_functions tc_state error_admin

compare_specified_and_derived_instance_types :: !SymbolType !SymbolType !*TypeHeaps -> (!ComparisionErrorCode, !*TypeHeaps)
compare_specified_and_derived_instance_types specified_instance_type derived_symbol_type type_heaps=:{th_vars, th_attrs}
	| length specified_instance_type.st_args<>length derived_symbol_type.st_args
		= (CEC_NrArgsNotOk, type_heaps)
	# tc_state = { tc_type_vars = initial_hwn th_vars
				 , tc_attr_vars = initial_hwn th_attrs
				 , tc_strictness_flags = AllowFirstMoreStrictness
				 }
	  tc_state = init_symbol_type_vars specified_instance_type derived_symbol_type tc_state
	  (correspond_list, tc_state)
	  		= map2St t_corresponds
	  				[specified_instance_type.st_result:specified_instance_type.st_args]
	  				[derived_symbol_type.st_result:derived_symbol_type.st_args]
	  		        tc_state
	  err_code = firstIndex not correspond_list
	| err_code<>CEC_Ok
		= (err_code, tc_state_to_type_heaps tc_state)
	# (arg_strictness_corresponds, tc_state)
			=  t_corresponds specified_instance_type.st_args_strictness derived_symbol_type.st_args_strictness tc_state
	| not arg_strictness_corresponds
		= (CEC_StrictnessOfArgsNotOk, tc_state_to_type_heaps tc_state)
	# (context_corresponds, tc_state)
			= t_corresponds specified_instance_type.st_context derived_symbol_type.st_context tc_state
	| not context_corresponds
		= (CEC_ContextNotOK, tc_state_to_type_heaps tc_state)
	# (attr_env_corresponds, tc_state)
			= t_corresponds specified_instance_type.st_attr_env derived_symbol_type.st_attr_env tc_state
	| not attr_env_corresponds
		= (CEC_AttrEnvNotOK, tc_state_to_type_heaps tc_state)
	| tc_state.tc_strictness_flags bitand FirstHasMoreStrictness<>0
		= (CEC_OkWithFirstMoreStrictness, tc_state_to_type_heaps tc_state)
		= (CEC_Ok, tc_state_to_type_heaps tc_state)
  where
	tc_state_to_type_heaps {tc_type_vars, tc_attr_vars}
		= { th_vars = tc_type_vars.hwn_heap, th_attrs = tc_attr_vars.hwn_heap}

init_symbol_type_vars symbol_type_1 symbol_type_2 tc_state
	# tc_state = init_attr_vars symbol_type_1.st_attr_vars symbol_type_2.st_attr_vars tc_state
	  tc_state = init_type_vars symbol_type_1.st_vars symbol_type_2.st_vars tc_state
	= tc_state

init_type_vars type_vars1 type_vars2 tc_state=:{tc_type_vars=tc_type_vars=:{hwn_heap}}
	# hwn_heap = foldSt init_type_var type_vars1 hwn_heap
	# hwn_heap = foldSt init_type_var type_vars2 hwn_heap
	= { tc_state & tc_type_vars = {tc_type_vars & hwn_heap = hwn_heap}}
  where
	init_type_var {tv_info_ptr} heap
		= writePtr tv_info_ptr TVI_Empty heap
	
generate_error message iclDef iclDefs tc_state error_admin
	# ident_pos = getIdentPos iclDef
	  error_admin = pushErrorAdmin ident_pos error_admin
	  error_admin = checkError ident_pos.ip_ident message error_admin
	= (iclDefs, tc_state, popErrorAdmin error_admin)

compareMacrosWithConversion main_dcl_module_n conversions macro_range generic_case_def_macros icl_functions macro_defs var_heap expr_heap tc_state error_admin
	#! n_icl_functions = size icl_functions
	#! n_dcl_macros_and_functions = size macro_defs.[main_dcl_module_n]
	# ec_state = {	ec_icl_correspondences = createArray n_icl_functions cNoCorrespondence,
	  				ec_dcl_correspondences = createArray n_dcl_macros_and_functions cNoCorrespondence,
	  				ec_var_heap = initial_hwn var_heap, 
	  				ec_expr_heap = expr_heap, ec_icl_functions = icl_functions,ec_macro_defs=macro_defs,
	  				ec_error_admin = error_admin, ec_tc_state = tc_state,
					ec_main_dcl_module_n = main_dcl_module_n }
	  ec_state = iFoldSt (compareMacroWithConversion conversions macro_range.ir_from) macro_range.ir_from macro_range.ir_to ec_state
	  	with
	  		compareMacroWithConversion conversions ir_from dclIndex ec_state=:{ec_main_dcl_module_n}
				= compareTwoMacroFuns ec_main_dcl_module_n dclIndex conversions.[dclIndex-ir_from] ec_state
	  ec_state = compare_generic_case_def_macros generic_case_def_macros ec_state
	    with
			compare_generic_case_def_macros [(GCB_FunAndMacroIndex fun_index macro_index,generic_info):gcbs] ec_state=:{ec_main_dcl_module_n}
				# ec_state = compare_generic_case_def_macro_and_function macro_index fun_index generic_info ec_state
				= compare_generic_case_def_macros gcbs ec_state
			compare_generic_case_def_macros [] ec_state
				= ec_state
	  {ec_icl_functions,ec_macro_defs,ec_var_heap,ec_expr_heap,ec_error_admin,ec_tc_state} = ec_state
	= (ec_icl_functions,ec_macro_defs,ec_var_heap.hwn_heap,ec_expr_heap,ec_tc_state,ec_error_admin)

compareTwoMacroFuns :: !Int !Int !Int !*ExpressionsCorrespondState -> .ExpressionsCorrespondState;
compareTwoMacroFuns macro_module_index dclIndex iclIndex ec_state=:{ec_icl_functions,ec_macro_defs,ec_main_dcl_module_n}
	| macro_module_index<>ec_main_dcl_module_n
		# (dcl_function,ec_macro_defs) = ec_macro_defs![macro_module_index,dclIndex]
		= { ec_state & ec_macro_defs=ec_macro_defs,ec_error_admin = checkErrorWithIdentPos (getIdentPos dcl_function) ErrorMessage ec_state.ec_error_admin }
	| iclIndex==NoIndex
		= ec_state
	# (dcl_function, ec_macro_defs) = ec_macro_defs![macro_module_index,dclIndex]
	  (icl_function, ec_icl_functions) = ec_icl_functions![iclIndex]
	  ec_state = { ec_state & ec_icl_correspondences.[iclIndex]=dclIndex, ec_dcl_correspondences.[dclIndex]=iclIndex,
	  						  ec_icl_functions = ec_icl_functions,ec_macro_defs=ec_macro_defs }
	  need_to_be_compared
	  		= case (dcl_function.fun_body, icl_function.fun_body) of
	  			(TransformedBody _, CheckedBody _)
					// the macro definition in the icl module is not used, so we don't need to compare
	  				-> False
	  			_	-> True
	| not need_to_be_compared
		= ec_state
	# ident_pos = getIdentPos dcl_function
	  ec_error_admin = pushErrorAdmin ident_pos ec_state.ec_error_admin
	  ec_state = { ec_state & ec_error_admin = ec_error_admin }
	| (dcl_function.fun_info.fi_properties bitand FI_IsMacroFun <> icl_function.fun_info.fi_properties bitand FI_IsMacroFun
		&& (ec_state.ec_tc_state.tc_strictness_flags bitand CompareGenericCaseMacro==0 && dcl_function.fun_info.fi_properties bitand FI_IsMacroFun<>0)) ||
	  dcl_function.fun_priority<>icl_function.fun_priority
		# ec_state = give_error dcl_function.fun_ident ec_state
		= { ec_state & ec_error_admin = popErrorAdmin ec_state.ec_error_admin } 
	# ec_state = e_corresponds dcl_function.fun_body icl_function.fun_body ec_state
	= { ec_state & ec_error_admin = popErrorAdmin ec_state.ec_error_admin }

compare_generic_case_def_macro_and_function :: !Int !Int !Int !*ExpressionsCorrespondState -> .ExpressionsCorrespondState;
compare_generic_case_def_macro_and_function dclIndex iclIndex generic_info ec_state=:{ec_icl_functions,ec_macro_defs,ec_main_dcl_module_n}
	| iclIndex==NoIndex
		= ec_state
	# (dcl_function, ec_macro_defs) = ec_macro_defs![ec_main_dcl_module_n,dclIndex]
	  (icl_function, ec_icl_functions) = ec_icl_functions![iclIndex]
	  ec_state & ec_icl_correspondences.[iclIndex]=dclIndex, ec_dcl_correspondences.[dclIndex]=iclIndex,
				 ec_icl_functions = ec_icl_functions,ec_macro_defs=ec_macro_defs
	  ident_pos = getIdentPos dcl_function
	  ec_state & ec_error_admin = pushErrorAdmin ident_pos ec_state.ec_error_admin

	  dcl_args_and_rhs = from_body dcl_function.fun_body
	  icl_args_and_rhs = from_body icl_function.fun_body

	  icl_args_and_rhs
	  	= if (generic_info==0)
	  		(remove_generic_info_arg icl_args_and_rhs)
	  		icl_args_and_rhs
	  {ec_tc_state} = ec_state
	  ec_state & ec_tc_state = {ec_tc_state & tc_strictness_flags = ec_tc_state.tc_strictness_flags bitor CompareGenericCaseMacro}
	  ec_state = e_corresponds dcl_args_and_rhs icl_args_and_rhs ec_state
	  {ec_tc_state} = ec_state
	  ec_state & ec_tc_state = {ec_tc_state & tc_strictness_flags = ec_tc_state.tc_strictness_flags bitand (bitnot CompareGenericCaseMacro)}
	= {ec_state & ec_error_admin = popErrorAdmin ec_state.ec_error_admin}
where
	remove_generic_info_arg ([generic_info_arg:args],rhs)
		= (args,rhs)
	remove_generic_info_arg args_and_rhs
		= args_and_rhs

instance getIdentPos (TypeDef a) where
	getIdentPos {td_ident, td_pos}
		= newPosition td_ident td_pos

instance getIdentPos ConsDef where
	getIdentPos {cons_ident, cons_pos}
		= newPosition cons_ident cons_pos

instance getIdentPos SelectorDef where
	getIdentPos {sd_ident, sd_pos}
		= newPosition sd_ident sd_pos

instance getIdentPos ClassDef where
	getIdentPos {class_ident, class_pos}
		= newPosition class_ident class_pos

instance getIdentPos MemberDef where
	getIdentPos {me_ident, me_pos}
		= newPosition me_ident me_pos

instance getIdentPos ClassInstance where
	getIdentPos {ins_ident, ins_pos}
		= newPosition ins_ident ins_pos

instance getIdentPos FunDef where
	getIdentPos {fun_ident, fun_pos}
		= newPosition fun_ident fun_pos

instance CorrespondenceNumber VarInfo where
	toCorrespondenceNumber (VI_CorrespondenceNumber number)
		=	CorrespondenceNumber number
	toCorrespondenceNumber _
		// VarInfoPtrs are not initialized in this module. This doesnt harm because VI_CorrespondenceNumber should
		// not be used outside this module 
		=	Unbound
		
	fromCorrespondenceNumber number
		=	VI_CorrespondenceNumber number

instance CorrespondenceNumber TypeVarInfo where
	toCorrespondenceNumber (TVI_CorrespondenceNumber number)
		=	CorrespondenceNumber number
	toCorrespondenceNumber TVI_Empty
		=	Unbound

	fromCorrespondenceNumber number
		=	TVI_CorrespondenceNumber number

instance CorrespondenceNumber AttrVarInfo where
	toCorrespondenceNumber (AVI_CorrespondenceNumber number)
		=	CorrespondenceNumber number
	toCorrespondenceNumber AVI_Empty
		=	Unbound

	fromCorrespondenceNumber number
		=	AVI_CorrespondenceNumber number

assignCorrespondenceNumber ptr1 ptr2 {hwn_heap, hwn_number}
	=	let var_info = fromCorrespondenceNumber hwn_number
		in	{	hwn_heap
					=	writePtr ptr1 var_info (writePtr ptr2 var_info hwn_heap)
			,	hwn_number
					=	hwn_number + 1
			}

tryToUnifyVars ptr1 ptr2 heapWithNumber
	#! info1 = sreadPtr ptr1 heapWithNumber.hwn_heap
	   info2 = sreadPtr ptr2 heapWithNumber.hwn_heap
	=	case (toCorrespondenceNumber info1, toCorrespondenceNumber info2) of
			(CorrespondenceNumber number1, CorrespondenceNumber number2)
				->	(number1==number2, heapWithNumber)
			(Unbound, Unbound)
 				->	(True, assignCorrespondenceNumber ptr1 ptr2 heapWithNumber)
			_	->	(False, heapWithNumber)

instance t_corresponds [a] | t_corresponds a where 
	t_corresponds [] []
		=	return True
	t_corresponds [dclDef:dclDefs] [iclDef:iclDefs]
		=	t_corresponds dclDef iclDef
		&&&	t_corresponds dclDefs iclDefs
	t_corresponds _ _
		=	return False

instance t_corresponds (a, b) | t_corresponds a & t_corresponds b where 
	t_corresponds (a1, b1) (a2, b2)
		=	t_corresponds a1 a2
		&&&	t_corresponds b1 b2

instance t_corresponds {# a} | t_corresponds a & Array {#} a
where 
	t_corresponds dclArray iclArray
		# size_dclArray = size dclArray
		| size_dclArray<>size iclArray
			= return False
			= loop (size_dclArray-1) dclArray iclArray
	  where
		loop :: !Int !{# a} !{# a} -> *TypesCorrespondMonad | t_corresponds a & Array {#} a
		loop i dclArray iclArray
			| i<0
				= return True
				= t_corresponds dclArray.[i] iclArray.[i]
			&&& loop (i-1) dclArray iclArray

instance t_corresponds (Optional a) | t_corresponds a where 
	t_corresponds No No
		= return True
	t_corresponds (Yes dclYes) (Yes iclYes)
		= t_corresponds dclYes iclYes
	t_corresponds _ _
		= return False

instance t_corresponds (Global DefinedSymbol) where
	t_corresponds dclDef iclDef
		=	t_corresponds dclDef.glob_object iclDef.glob_object
		&&&	equal dclDef.glob_module iclDef.glob_module

instance t_corresponds (TypeDef TypeRhs) where
	t_corresponds dclDef iclDef
		= t_corresponds_TypeDef dclDef iclDef
	  where
		t_corresponds_TypeDef dclDef iclDef tc_state
			# tc_state = init_attr_vars dclDef.td_attrs iclDef.td_attrs tc_state 
			  tc_state = init_atype_vars dclDef.td_args iclDef.td_args tc_state
			= t_corresponds (dclDef.td_args, (dclDef.td_rhs, dclDef.td_attribute))
			 				(iclDef.td_args, (iclDef.td_rhs, iclDef.td_attribute)) tc_state

instance t_corresponds TypeContext where
	t_corresponds dclDef iclDef
		=	t_corresponds dclDef.tc_class iclDef.tc_class
		&&& t_corresponds dclDef.tc_types iclDef.tc_types

instance t_corresponds TCClass where
	t_corresponds (TCClass class1) (TCClass class2) 
		= t_corresponds class1 class2
	t_corresponds (TCGeneric {gtc_generic=gen1, gtc_kind=kind1}) (TCGeneric {gtc_generic=gen2, gtc_kind=kind2}) 
		= t_corresponds gen1 gen2 
		&&& equal kind1 kind2  
	t_corresponds _ _ 
		= return False

instance t_corresponds DefinedSymbol where
	t_corresponds dclDef iclDef
		=	equal dclDef.ds_ident iclDef.ds_ident

instance t_corresponds ATypeVar where
	t_corresponds dclDef iclDef
		=	t_corresponds dclDef.atv_attribute iclDef.atv_attribute
		&&&	t_corresponds dclDef.atv_variable iclDef.atv_variable

instance t_corresponds Annotation where
	t_corresponds dcl_annotation icl_annotation 
		= t_corresponds` dcl_annotation icl_annotation
	  where
		t_corresponds` AN_Strict AN_Strict tc_state
			= (True, tc_state)
		t_corresponds` AN_Strict AN_None tc_state=:{tc_strictness_flags}
			| tc_strictness_flags bitand AllowFirstMoreStrictness==0
				= (False,tc_state)
			| tc_strictness_flags bitand FirstHasMoreStrictness<>0
				= (True,tc_state)
				# tc_state = {tc_state & tc_strictness_flags = tc_strictness_flags bitor FirstHasMoreStrictness}
				= (True,tc_state)
		t_corresponds` AN_None AN_None tc_state
			= (True, tc_state)
		t_corresponds` AN_None AN_Strict tc_state
			= (False, tc_state)

instance t_corresponds StrictnessList where
	t_corresponds dcl_strictness icl_strictness 
		= t_corresponds` dcl_strictness icl_strictness
	  where
		t_corresponds` dcl_strictness icl_strictness tc_state=:{tc_strictness_flags}
			| tc_strictness_flags bitand AllowFirstMoreStrictness==0
				= (equal_strictness_lists dcl_strictness icl_strictness, tc_state)
			| tc_strictness_flags bitand FirstHasMoreStrictness<>0
				= (more_or_equal_strictness_lists dcl_strictness icl_strictness, tc_state)
			 | equal_strictness_lists dcl_strictness icl_strictness
				= (True,tc_state)
			| more_or_equal_strictness_lists dcl_strictness icl_strictness
				# tc_state = {tc_state & tc_strictness_flags = tc_strictness_flags bitor FirstHasMoreStrictness}
				= (True,tc_state)
				= (False,tc_state)				

instance t_corresponds AType where
	t_corresponds dclDef iclDef
		=	t_corresponds dclDef.at_attribute iclDef.at_attribute
		&&&	t_corresponds dclDef.at_type iclDef.at_type

instance t_corresponds TypeAttribute where
	t_corresponds TA_Unique TA_Unique
		=	return True
	t_corresponds TA_Multi icl
		= case icl of
			TA_Multi-> return True
			TA_None	-> return True
			_		-> return False
	t_corresponds (TA_Var dclDef) (TA_Var iclDef)
		=	t_corresponds dclDef iclDef
	t_corresponds (TA_RootVar dclDef) (TA_RootVar iclDef)
		= t_corresponds dclDef iclDef
	t_corresponds TA_None icl
		= case icl of
			TA_Multi-> return True
			TA_None	-> return True
			_		-> return False
	t_corresponds _ _
		=	return False

instance t_corresponds AttributeVar where
	t_corresponds dclDef iclDef
		=	corresponds` dclDef iclDef
	  where
		corresponds` dclDef iclDef tc_state=:{tc_attr_vars}
			# (unifiable, tc_attr_vars) = tryToUnifyVars dclDef.av_info_ptr iclDef.av_info_ptr tc_attr_vars
			= (unifiable, { tc_state & tc_attr_vars = tc_attr_vars })

instance t_corresponds Type where
	t_corresponds (TA dclIdent dclArgs) (TA iclIdent iclArgs)
		=	equal dclIdent.type_ident iclIdent.type_ident
		&&& equal dclIdent.type_index.glob_module iclIdent.type_index.glob_module
		&&& t_corresponds dclArgs iclArgs
	t_corresponds (TA dclIdent dclArgs) (TAS iclIdent iclArgs iclStrictness)
		=	equal dclIdent.type_ident iclIdent.type_ident
		&&& equal dclIdent.type_index.glob_module iclIdent.type_index.glob_module
		&&& return (is_not_strict iclStrictness)
		&&& t_corresponds dclArgs iclArgs
	t_corresponds (TAS dclIdent dclArgs dclStrictness) (TA iclIdent iclArgs)
		=	equal dclIdent.type_ident iclIdent.type_ident
		&&& equal dclIdent.type_index.glob_module iclIdent.type_index.glob_module
		&&& compare_strictness dclStrictness
		&&& t_corresponds dclArgs iclArgs
		where
			compare_strictness dclStrictness tc_state=:{tc_strictness_flags}
				| tc_strictness_flags bitand AllowFirstMoreStrictness==0
					= (equal_strictness_lists dclStrictness NotStrict, tc_state)
				| tc_strictness_flags bitand FirstHasMoreStrictness<>0 || equal_strictness_lists dclStrictness NotStrict
					= (True, tc_state)
					# tc_state = {tc_state & tc_strictness_flags = tc_strictness_flags bitor FirstHasMoreStrictness}
					= (True, tc_state)
	t_corresponds (TAS dclIdent dclArgs dclStrictness) (TAS iclIdent iclArgs iclStrictness)
		=	equal dclIdent.type_ident iclIdent.type_ident
		&&& equal dclIdent.type_index.glob_module iclIdent.type_index.glob_module
		&&& compare_strictness dclStrictness iclStrictness
		&&& t_corresponds dclArgs iclArgs
		where
			compare_strictness dclStrictness iclStrictness tc_state=:{tc_strictness_flags}
				| tc_strictness_flags bitand AllowFirstMoreStrictness==0
					= (equal_strictness_lists dclStrictness iclStrictness, tc_state)
				| tc_strictness_flags bitand FirstHasMoreStrictness<>0
					= (more_or_equal_strictness_lists dclStrictness iclStrictness, tc_state)
				| equal_strictness_lists dclStrictness iclStrictness
					= (True, tc_state)
				| more_or_equal_strictness_lists dclStrictness iclStrictness
					# tc_state = {tc_state & tc_strictness_flags = tc_strictness_flags bitor FirstHasMoreStrictness}
					= (True, tc_state)
					= (False, tc_state)
	t_corresponds (dclFun --> dclArg) (iclFun --> iclArg)
		=	t_corresponds dclFun iclFun
		&&&	t_corresponds dclArg iclArg
	t_corresponds (dclVar :@: dclArgs) (iclVar :@: iclArgs)
		=	t_corresponds dclVar iclVar
		&&&	t_corresponds dclArgs iclArgs
	t_corresponds (TB dclDef) (TB iclDef)
		=	equal dclDef iclDef
	t_corresponds (TV dclDef) (TV iclDef)
		=	t_corresponds dclDef iclDef
	t_corresponds (GTV dclDef) (GTV iclDef)
		=	t_corresponds dclDef iclDef
	t_corresponds (TFA dclVars dclType) (TFA iclVars iclType)
		=	do (init_atype_vars dclVars iclVars)
		&&&	t_corresponds dclType iclType
	t_corresponds (TFAC dclVars dclType dclContexts) (TFAC iclVars iclType iclContexts)
		=	do (init_atype_vars dclVars iclVars)
		&&&	t_corresponds dclType iclType
		&&&	t_corresponds dclContexts iclContexts
	t_corresponds _ _
		= return False
		
instance t_corresponds ConsVariable where
	t_corresponds (CV dclVar) (CV iclVar)
		=	t_corresponds dclVar iclVar
		
instance t_corresponds TypeVar where
	t_corresponds dclDef iclDef
		=	corresponds_TypeVar dclDef iclDef
	  where
		corresponds_TypeVar dclDef iclDef tc_state=:{tc_type_vars}
			# (unifiable, tc_type_vars) = tryToUnifyVars dclDef.tv_info_ptr iclDef.tv_info_ptr tc_type_vars
			= (unifiable, { tc_state & tc_type_vars = tc_type_vars })

instance t_corresponds TypeRhs where
	t_corresponds (AlgType dclConstructors) (AlgType iclConstructors)
		=	t_corresponds dclConstructors iclConstructors
	t_corresponds (SynType dclType) (SynType iclType)
		=	t_corresponds dclType iclType
	t_corresponds (RecordType dclRecord) (RecordType iclRecord)
		=	t_corresponds dclRecord iclRecord
	t_corresponds (AbstractType _) (NewType _)
		=	return False
	t_corresponds (AbstractType _) _
		=	return True
	t_corresponds (AbstractSynType _ dclType) (SynType iclType)
		=	t_corresponds dclType iclType
	t_corresponds (NewType dclConstructor) (NewType iclConstructor)
		=	t_corresponds dclConstructor iclConstructor
	t_corresponds (ExtensibleAlgType dclConstructors) (ExtensibleAlgType iclConstructors)
		=	t_corresponds dclConstructors iclConstructors

// sanity check ...
	t_corresponds UnknownType _
		=	undef // <<- "t_corresponds (TypeRhs): dclDef == UnknownType" 
	t_corresponds _ UnknownType
		=	undef // <<- "t_corresponds (TypeRhs): iclDef == UnknownType"
// ... sanity check
	t_corresponds _ _
		=	return False

instance t_corresponds Bool where
	t_corresponds b1 b2 = return (b1==b2)

instance t_corresponds RecordType where
	t_corresponds dclRecord iclRecord
		=	t_corresponds dclRecord.rt_constructor dclRecord.rt_constructor
		&&&	t_corresponds dclRecord.rt_is_boxed_record dclRecord.rt_is_boxed_record
		&&& t_corresponds dclRecord.rt_fields iclRecord.rt_fields

instance t_corresponds FieldSymbol where
	t_corresponds dclField iclField
		=	equal dclField.fs_ident iclField.fs_ident

instance t_corresponds ConsDef where
	t_corresponds dclDef iclDef
		=	do (init_atype_vars dclDef.cons_exi_vars iclDef.cons_exi_vars)
		&&&	t_corresponds dclDef.cons_type iclDef.cons_type
		&&& equal dclDef.cons_ident iclDef.cons_ident
		&&& equal dclDef.cons_priority iclDef.cons_priority

instance t_corresponds SelectorDef where
	t_corresponds dclDef iclDef
		=	do (init_atype_vars dclDef.sd_exi_vars iclDef.sd_exi_vars)
		&&&	t_corresponds dclDef.sd_type iclDef.sd_type
		&&& equal dclDef.sd_field_nr iclDef.sd_field_nr

init_atype_vars atype_vars1 atype_vars2 tc_state=:{tc_type_vars=tc_type_vars=:{hwn_heap}}
	# hwn_heap = foldSt init_type_var atype_vars1 hwn_heap
	# hwn_heap = foldSt init_type_var atype_vars2 hwn_heap
	= {tc_state & tc_type_vars = {tc_type_vars & hwn_heap = hwn_heap}}
  where
	init_type_var {atv_variable} type_heap = writePtr atv_variable.tv_info_ptr TVI_Empty type_heap

instance t_corresponds SymbolType where
	t_corresponds dclDef iclDef
		=	t_corresponds dclDef.st_args iclDef.st_args
		&&&	t_corresponds dclDef.st_args_strictness iclDef.st_args_strictness
		&&&	t_corresponds dclDef.st_result iclDef.st_result
		&&&	t_corresponds dclDef.st_context iclDef.st_context
		&&&	t_corresponds dclDef.st_attr_env iclDef.st_attr_env

instance t_corresponds AttrInequality where
	t_corresponds dclDef iclDef
		=	t_corresponds dclDef.ai_demanded iclDef.ai_demanded
		&&&	t_corresponds dclDef.ai_offered iclDef.ai_offered

instance t_corresponds ClassDef where
	t_corresponds dclDef iclDef
		=	do (init_type_vars dclDef.class_args iclDef.class_args)
		&&&	equal dclDef.class_ident iclDef.class_ident
		&&&	t_corresponds dclDef.class_args iclDef.class_args
		&&&	t_corresponds dclDef.class_context iclDef.class_context
		&&&	t_corresponds dclDef.class_members iclDef.class_members

instance t_corresponds MemberDef where
	t_corresponds dclDef iclDef
		=	do (init_type_vars dclDef.me_type.st_vars iclDef.me_type.st_vars)
		&&&	do (init_attr_vars dclDef.me_type.st_attr_vars iclDef.me_type.st_attr_vars)
		&&& equal dclDef.me_ident iclDef.me_ident
		&&&	equal dclDef.me_offset iclDef.me_offset
		&&&	equal dclDef.me_priority iclDef.me_priority
		&&&	t_corresponds dclDef.me_type iclDef.me_type

instance t_corresponds DynamicType where
	t_corresponds dclDef iclDef
		= t_corresponds dclDef.dt_type iclDef.dt_type

instance e_corresponds (Optional a) | e_corresponds a where 
	e_corresponds No No
		= do_nothing
	e_corresponds (Yes dclYes) (Yes iclYes)
		= e_corresponds dclYes iclYes
	e_corresponds _ _
		= give_error ""

instance e_corresponds (a, b) | e_corresponds a & e_corresponds b where 
	e_corresponds (a1, b1) (a2, b2)
		=	(e_corresponds a1 a2)
		o`	(e_corresponds b1 b2)

instance e_corresponds [a] | e_corresponds a where 
	e_corresponds [] []
		=	do_nothing
	e_corresponds [dclDef:dclDefs] [iclDef:iclDefs]
		=	e_corresponds dclDef iclDef
		o`	e_corresponds dclDefs iclDefs
	e_corresponds _ _
		=	give_error ""

instance e_corresponds (Global a) | e_corresponds a where
	e_corresponds dclDef iclDef
		=	equal2 dclDef.glob_module iclDef.glob_module
		o`	e_corresponds dclDef.glob_object iclDef.glob_object

instance e_corresponds DefinedSymbol where
	e_corresponds dclDef iclDef
		= equal2 dclDef.ds_ident iclDef.ds_ident

instance e_corresponds FunctionBody where
	// both bodies are either CheckedBodies or TransformedBodies
	e_corresponds dclDef iclDef
		=	e_corresponds (from_body dclDef) (from_body iclDef)

from_body (TransformedBody {tb_args, tb_rhs}) = (tb_args, [tb_rhs])
from_body (CheckedBody {cb_args, cb_rhs}) = (cb_args, [ca_rhs \\ {ca_rhs} <- cb_rhs])
		
instance e_corresponds FreeVar where
	e_corresponds dclVar iclVar
		=	e_corresponds_VarInfoPtr iclVar.fv_ident dclVar.fv_info_ptr iclVar.fv_info_ptr

instance e_corresponds Expression where
	// the following alternatives don't occur anymore: Lambda, Conditional, WildCard
	e_corresponds (Var dcl) (Var icl)
		= 	e_corresponds dcl icl
	e_corresponds (App dcl_app) (App icl_app)
		=	e_corresponds_app_symb dcl_app.app_symb icl_app.app_symb
		o`	e_corresponds dcl_app.app_args icl_app.app_args
	e_corresponds (dclFun @ dclArgs) (iclFun @ iclArgs)
		=	e_corresponds dclFun iclFun
		o`	e_corresponds dclArgs iclArgs
	e_corresponds (Let dcl) (Let icl)
		= 	e_corresponds dcl icl
	e_corresponds (Case dcl) (Case icl)
		= 	e_corresponds dcl icl
	e_corresponds (Selection dcl_is_unique dcl_expr dcl_selections) (Selection icl_is_unique icl_expr icl_selections)
		| not (equal_constructor dcl_is_unique icl_is_unique)
			= give_error "" 
		=	e_corresponds dcl_expr icl_expr
		o`	e_corresponds dcl_selections icl_selections
	e_corresponds (Update dcl_expr_1 dcl_selections dcl_expr_2) (Update icl_expr_1 icl_selections icl_expr_2)
		=	e_corresponds dcl_expr_1 icl_expr_1
		o`	e_corresponds dcl_selections icl_selections
		o`	e_corresponds dcl_expr_2 icl_expr_2
	e_corresponds (RecordUpdate dcl_type dcl_expr dcl_selections) (RecordUpdate icl_type icl_expr icl_selections)
		=	e_corresponds dcl_type icl_type
		o`	e_corresponds dcl_expr icl_expr
		o`	e_corresponds dcl_selections icl_selections
	e_corresponds (TupleSelect dcl_ds dcl_field_nr dcl_expr) (TupleSelect icl_ds icl_field_nr icl_expr)
		=	e_corresponds dcl_ds icl_ds
		o`	equal2 dcl_field_nr icl_field_nr
		o`	e_corresponds dcl_expr icl_expr
	e_corresponds (BasicExpr dcl_value) (BasicExpr icl_value)
		=	equal2 dcl_value icl_value
	e_corresponds (AnyCodeExpr dcl_ins dcl_outs dcl_code_sequence) (AnyCodeExpr icl_ins icl_outs icl_code_sequence)
		=	e_corresponds dcl_ins icl_ins
		o`	e_corresponds dcl_outs icl_outs
		o`	equal2 dcl_code_sequence icl_code_sequence
	e_corresponds (ABCCodeExpr dcl_lines dcl_do_inline) (ABCCodeExpr icl_lines icl_do_inline)
		=	equal2 dcl_lines icl_lines
		o`	equal2 dcl_do_inline icl_do_inline
	e_corresponds (MatchExpr dcl_cons_symbol dcl_src_expr)
				 (MatchExpr icl_cons_symbol icl_src_expr)
		=	e_corresponds dcl_cons_symbol icl_cons_symbol
		o`	e_corresponds dcl_src_expr icl_src_expr
	e_corresponds (IsConstructor dcl_src_expr dcl_cons_symbol _ _ _ _)
				 (IsConstructor icl_src_expr icl_cons_symbol _ _ _ _)
		=	e_corresponds dcl_cons_symbol icl_cons_symbol
		o`	e_corresponds dcl_src_expr icl_src_expr
	e_corresponds (FreeVar dcl) (FreeVar icl)
		= e_corresponds dcl icl
	e_corresponds (DynamicExpr dcl) (DynamicExpr icl)
		= e_corresponds dcl icl
	e_corresponds (TypeCodeExpression dcl) (TypeCodeExpression icl)
		= e_corresponds dcl icl
	e_corresponds EE EE
		= do_nothing
	e_corresponds (NoBind _) (NoBind _)
		= do_nothing
	e_corresponds _ _
		= give_error ""

instance e_corresponds Let where
	e_corresponds dclLet iclLet
		=	e_corresponds dclLet.let_strict_binds iclLet.let_strict_binds
		o`	e_corresponds dclLet.let_lazy_binds iclLet.let_lazy_binds
		o`	e_corresponds dclLet.let_expr iclLet.let_expr

instance e_corresponds LetBind where
	e_corresponds dcl icl
		=	e_corresponds dcl.lb_src icl.lb_src
		o`	e_corresponds dcl.lb_dst icl.lb_dst

instance e_corresponds (Bind a b) | e_corresponds a & e_corresponds b where
	e_corresponds dcl icl
		=	e_corresponds dcl.bind_src icl.bind_src
		o`	e_corresponds dcl.bind_dst icl.bind_dst

instance e_corresponds Case where
	e_corresponds dclCase iclCase
		=	e_corresponds dclCase.case_expr iclCase.case_expr
		o`	e_corresponds dclCase.case_guards iclCase.case_guards
		o`	e_corresponds dclCase.case_default iclCase.case_default

instance e_corresponds CasePatterns where
	e_corresponds (AlgebraicPatterns dcl_alg_type dcl_patterns) (AlgebraicPatterns icl_alg_type icl_patterns)
		=	e_corresponds dcl_patterns icl_patterns
	e_corresponds (BasicPatterns dcl_basic_type dcl_patterns) (BasicPatterns icl_basic_type icl_patterns)
		=	equal2 dcl_basic_type icl_basic_type
		o`	e_corresponds dcl_patterns icl_patterns
	e_corresponds (NewTypePatterns _ dcl_patterns) (NewTypePatterns _ icl_patterns)
		=	e_corresponds dcl_patterns icl_patterns
	e_corresponds (OverloadedListPatterns dcl_alg_type _ dcl_patterns) (OverloadedListPatterns icl_alg_type _ icl_patterns)
		=	e_corresponds dcl_patterns icl_patterns
	e_corresponds (DynamicPatterns dcl_patterns) (DynamicPatterns icl_patterns)
		=	e_corresponds dcl_patterns icl_patterns
	e_corresponds NoPattern NoPattern
		=	do_nothing
	e_corresponds _ _
		=	give_error ""

instance e_corresponds AlgebraicPattern where
	e_corresponds dcl icl
		=	e_corresponds dcl.ap_symbol icl.ap_symbol
		o`	e_corresponds dcl.ap_vars icl.ap_vars
		o`	e_corresponds dcl.ap_expr icl.ap_expr

instance e_corresponds BasicPattern where
	e_corresponds dcl icl
		=	equal2 dcl.bp_value icl.bp_value
		o`	e_corresponds dcl.bp_expr icl.bp_expr

instance e_corresponds DynamicPattern where
	e_corresponds dcl icl
		=	e_corresponds dcl.dp_var icl.dp_var
		o`	e_corresponds dcl.dp_rhs icl.dp_rhs
		o`	e_corresponds_dp_type dcl.dp_type icl.dp_type
	  where
		e_corresponds_dp_type dcl_expr_ptr icl_expr_ptr ec_state=:{ec_expr_heap, ec_tc_state}
			#  (dcl_type, ec_expr_heap)
					= readPtr dcl_expr_ptr ec_expr_heap
			   (icl_type, ec_expr_heap) 
			   		= readPtr icl_expr_ptr ec_expr_heap
			# (EI_DynamicTypeWithVars _ dcl_dyn_type _)
					= dcl_type
			  (EI_DynamicTypeWithVars _ icl_dyn_type _)
			  		= icl_type
			  (corresponds, ec_tc_state) 
					= t_corresponds dcl_dyn_type icl_dyn_type ec_tc_state
			  ec_state
				  	= { ec_state & ec_tc_state = ec_tc_state, ec_expr_heap = ec_expr_heap }
			| corresponds
				= ec_state
			= give_error "" ec_state

instance e_corresponds Selection where
	e_corresponds (RecordSelection dcl_selector dcl_field_nr) (RecordSelection icl_selector icl_field_nr)
		=	e_corresponds dcl_selector icl_selector
		o`	equal2 dcl_field_nr icl_field_nr
	e_corresponds (ArraySelection dcl_selector _ dcl_index_expr) (ArraySelection icl_selector _ icl_index_expr)
		=	e_corresponds dcl_selector icl_selector
		o`	e_corresponds dcl_index_expr icl_index_expr
	e_corresponds (DictionarySelection dcl_dict_var dcl_selections _ dcl_index_expr)
				(DictionarySelection icl_dict_var icl_selections _ icl_index_expr)
		=	e_corresponds dcl_dict_var icl_dict_var
		o`	e_corresponds dcl_selections icl_selections
		o`	e_corresponds dcl_index_expr icl_index_expr
		
instance e_corresponds DynamicExpr where
	e_corresponds dcl icl
		=	e_corresponds_dyn_opt_type dcl.dyn_opt_type icl.dyn_opt_type
		o`	e_corresponds dcl.dyn_expr icl.dyn_expr
	  where		
		e_corresponds_dyn_opt_type dcl icl ec_state
			# (corresponds, ec_tc_state) = t_corresponds dcl icl ec_state.ec_tc_state
			  ec_state = { ec_state & ec_tc_state = ec_tc_state }
			| corresponds
				= ec_state
			= give_error "" ec_state

instance e_corresponds TypeCodeExpression where
	e_corresponds TCE_Empty TCE_Empty
		= do_nothing
	e_corresponds _ _
		= abort "comparedefimp:e_corresponds (TypeCodeExpression): currently only TCE_Empty can appear"
	
instance e_corresponds {#Char} where
	e_corresponds s1 s2
		= equal2 s1 s2

instance e_corresponds BoundVar where
	e_corresponds dcl icl
		= e_corresponds_VarInfoPtr icl.var_ident dcl.var_info_ptr icl.var_info_ptr
		
instance e_corresponds FieldSymbol where
	e_corresponds dclField iclField
		= equal2 dclField.fs_ident iclField.fs_ident

e_corresponds_VarInfoPtr ident dclPtr iclPtr ec_state=:{ec_var_heap}
	# (unifiable, ec_var_heap) = tryToUnifyVars dclPtr iclPtr ec_var_heap
	  ec_state = { ec_state & ec_var_heap = ec_var_heap }
	| not unifiable
		= { ec_state & ec_error_admin = checkError ident ErrorMessage ec_state.ec_error_admin }
	= ec_state

/*	e_corresponds_app_symb checks correspondence of the function symbols in an App expression.
	The problem: also different symbols can correspond with each other, because for macros
	all local functions (also lambda functions) will be generated twice.
*/
e_corresponds_app_symb {symb_ident, symb_kind=SK_Function dcl_glob_index} {symb_kind=SK_Function icl_glob_index} ec_state
	#! main_dcl_module_n = ec_state.ec_main_dcl_module_n
	| dcl_glob_index.glob_module==main_dcl_module_n && icl_glob_index.glob_module==main_dcl_module_n
		| dcl_glob_index.glob_object<>icl_glob_index.glob_object
			= give_error symb_ident ec_state
		= ec_state
	| dcl_glob_index<>icl_glob_index
		= give_error symb_ident ec_state
	= ec_state
e_corresponds_app_symb {symb_ident, symb_kind=SK_OverloadedFunction dcl_glob_index} {symb_kind=SK_OverloadedFunction icl_glob_index} ec_state
	| dcl_glob_index<>icl_glob_index
		= give_error symb_ident ec_state
	= ec_state
e_corresponds_app_symb {symb_ident, symb_kind=SK_Generic dcl_glob_index dcl_kind} {symb_kind=SK_Generic icl_glob_index icl_kind} ec_state
	| dcl_glob_index<>icl_glob_index || dcl_kind <> icl_kind
		= give_error symb_ident ec_state
	= ec_state
e_corresponds_app_symb dcl_app_symb=:{symb_kind=SK_DclMacro dcl_glob_index} icl_app_symb=:{symb_kind=SK_IclMacro icl_index} ec_state
	= continuation_for_possibly_twice_defined_macros dcl_app_symb dcl_glob_index icl_app_symb icl_index ec_state
e_corresponds_app_symb {symb_ident,symb_kind=SK_DclMacro dcl_glob_index} {symb_kind=SK_DclMacro icl_glob_index} ec_state
	| dcl_glob_index==icl_glob_index
		= ec_state
		= give_error symb_ident ec_state
e_corresponds_app_symb dcl_app_symb=:{symb_kind=SK_LocalDclMacroFunction dcl_glob_index} icl_app_symb=:{symb_kind=SK_LocalMacroFunction icl_index} ec_state
	= continuation_for_possibly_twice_defined_macros dcl_app_symb dcl_glob_index icl_app_symb icl_index ec_state
e_corresponds_app_symb dcl_app_symb=:{symb_kind=SK_LocalDclMacroFunction dcl_glob_index} icl_app_symb=:{symb_kind=SK_Function {glob_module,glob_object=icl_index}} ec_state
	| glob_module==ec_state.ec_main_dcl_module_n && ec_state.ec_tc_state.tc_strictness_flags bitand CompareGenericCaseMacro<>0
		= continuation_for_possibly_twice_defined_macros dcl_app_symb dcl_glob_index icl_app_symb icl_index ec_state
e_corresponds_app_symb {symb_ident=dcl_symb_name, symb_kind=SK_Constructor dcl_glob_index} {symb_ident=icl_symb_name, symb_kind=SK_Constructor icl_glob_index} ec_state
	| dcl_glob_index.glob_module==icl_glob_index.glob_module && dcl_symb_name.id_name==icl_symb_name.id_name
		= ec_state
		= give_error icl_symb_name ec_state
e_corresponds_app_symb {symb_ident=dcl_symb_name, symb_kind=SK_NewTypeConstructor dcl_glob_index} {symb_ident=icl_symb_name, symb_kind=SK_NewTypeConstructor icl_glob_index} ec_state
	| dcl_glob_index.gi_module==icl_glob_index.gi_module && dcl_symb_name.id_name==icl_symb_name.id_name
		= ec_state
		= give_error icl_symb_name ec_state
e_corresponds_app_symb {symb_ident,symb_kind} {symb_kind=symb_kind2} ec_state
	= give_error symb_ident ec_state

continuation_for_possibly_twice_defined_macros dcl_app_symb {glob_module=dcl_module_index, glob_object=dcl_index} icl_app_symb icl_index ec_state
	| icl_index==NoIndex
		= ec_state
	// two different functions were referenced. In case of macro functions they still could correspond
	| not (names_are_compatible dcl_index icl_index ec_state.ec_icl_functions ec_state.ec_macro_defs)
		= give_error icl_app_symb.symb_ident ec_state
	| dcl_module_index<>ec_state.ec_main_dcl_module_n
		= give_error icl_app_symb.symb_ident ec_state
	| ec_state.ec_dcl_correspondences.[dcl_index]==icl_index && ec_state.ec_icl_correspondences.[icl_index]==dcl_index
		= ec_state
	| ec_state.ec_dcl_correspondences.[dcl_index]==cNoCorrespondence && ec_state.ec_icl_correspondences.[icl_index]==cNoCorrespondence
		// going into recursion is save
		= compareTwoMacroFuns dcl_module_index dcl_index icl_index ec_state
	= give_error icl_app_symb.symb_ident ec_state
  where
	names_are_compatible :: Int Int {#FunDef} {#{#FunDef}} -> Bool;
	names_are_compatible dcl_index icl_index icl_functions macro_defs
		# dcl_function = macro_defs.[dcl_module_index,dcl_index]
		  icl_function = icl_functions.[icl_index]
		  dcl_name_is_loc_dependent = name_is_location_dependent dcl_function.fun_kind
		  icl_name_is_loc_dependent = name_is_location_dependent icl_function.fun_kind
		=	(dcl_name_is_loc_dependent==icl_name_is_loc_dependent)
		 && (implies (not dcl_name_is_loc_dependent) (dcl_function.fun_ident.id_name==icl_function.fun_ident.id_name))
		// functions that originate from e.g. lambda expressions can correspond although their names differ
	  where
		name_is_location_dependent (FK_Function name_is_loc_dependent)
			= name_is_loc_dependent
		name_is_location_dependent _
		 	= False

init_attr_vars attr_vars1 attr_vars2 tc_state=:{tc_attr_vars=tc_attr_vars=:{hwn_heap}}
	# hwn_heap = foldSt init_attr_var attr_vars1 hwn_heap
	# hwn_heap = foldSt init_attr_var attr_vars2 hwn_heap
	= { tc_state & tc_attr_vars = { tc_attr_vars & hwn_heap = hwn_heap } }
  where
	init_attr_var {av_info_ptr} attr_heap
		= writePtr av_info_ptr AVI_Empty attr_heap

ErrorMessage = "definition in the impl module conflicts with the def module"
cNoCorrespondence	:== -1
implies a b 		:== not a || b
	
(==>) infix 0 // :: w:(St .s .a) v:(.a -> .(St .s .b)) -> u:(St .s .b), [u <= v, u <= w]
(==>) f g :== \st0 -> let (r,st1) = f st0 in g r st1

(o`) infixr 0
(o`) f g :== \state -> g (f state)

do f = \state -> (True, f state)

(&&&) infixr
(&&&) m1 m2
	:==	m1 ==> \b
		->	if b
				m2
				(return False)

equal a b
	=	return (a == b)

equal2 a b
	| a<>b
		= give_error ""
	= do_nothing

do_nothing ec_state
	= ec_state

give_error s ec_state
	= { ec_state & ec_error_admin = checkError s ErrorMessage ec_state.ec_error_admin }

