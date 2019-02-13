implementation module overloading

import StdEnv,StdOverloadedList,compare_types

import syntax, type, expand_types, utilities, unitype, predef, checktypes
import genericsupport, type_io_common

::	LocalTypePatternVariable =
	{	ltpv_var			:: !Int
	,	ltpv_new_var		:: !VarInfoPtr
	}

::	ReducedContext = 
	{	rc_class_index		:: !GlobalIndex
	,	rc_types			:: ![Type]
	,	rc_inst_module		:: !Index
	,	rc_inst_members		:: !{#ClassInstanceMember}
	,	rc_red_contexts		:: ![ClassApplication]
	}

::	ReducedContexts = 
	{	rcs_class_context			:: !ReducedContext
	,	rcs_constraints_contexts	:: ![ClassApplication]
	}

::	TypeCodeInstance =
	{	tci_constructor		:: !GlobalTCType
	,	tci_contexts		:: ![ClassApplication]
	}

::	ClassApplication	= CA_Instance !ReducedContexts
						| CA_Context !TypeContext
						| CA_LocalTypeCode !VarInfoPtr			/* for (local) type pattern variables */
						| CA_GlobalTypeCode !TypeCodeInstance	/* for (global) type constructors */

instanceError symbol types err
	# err = errorHeading "Overloading error" err
	  format = { form_properties = cNoProperties, form_attr_position = No }
	= { err & ea_file = err.ea_file <<< " \"" <<< symbol <<< "\" no instance available of type "
									<:: (format, types, Yes initialTypeVarBeautifulizer) <<< '\n' }

uniqueError symbol types err
	# err = errorHeading "Overloading/Uniqueness error" err
	  format = { form_properties = cAnnotated, form_attr_position = No }
	= { err & ea_file = err.ea_file <<< " \"" <<< symbol
			<<< "\" uniqueness specification of instance conflicts with current application "
			<:: (format, types, Yes initialTypeVarBeautifulizer) <<< '\n'}

unboxError class_ident type err
	# err = errorHeading ("Overloading error of "+++class_ident+++" class") err
	  format = { form_properties = cNoProperties, form_attr_position = No }
	= { err & ea_file = err.ea_file <<< ' ' <:: (format, type, Yes initialTypeVarBeautifulizer) <<< " instance cannot be unboxed\n"}

overloadingError op_symb err
	# err = errorHeading "Overloading error" err
	  str = case optBeautifulizeIdent op_symb.id_name of
	  			No
	  				-> op_symb.id_name
	  			Yes (str, line_nr)
	  				-> str+++" [line "+++toString line_nr+++"]"
	= { err & ea_file = err.ea_file <<< " internal overloading of \"" <<< str <<< "\" could not be solved\n" }

sub_class_error op_symb err
	# err = errorHeading "Overloading error" err
	  str = case optBeautifulizeIdent op_symb.id_name of
	  			No
	  				-> op_symb.id_name
	  			Yes (str, line_nr)
	  				-> str+++" [line "+++toString line_nr+++"]"
	= {err & ea_file = err.ea_file <<< " internal overloading could not be solved, because subclass of \"" <<< str <<< "\" used\n"}

abstractTypeInDynamicError td_ident err=:{ea_ok}
	# err = errorHeading "Implementation restriction" err
	= { err & ea_file = err.ea_file <<< (" derived abstract type '" +++ toString td_ident +++ "' not permitted in a dynamic") <<< '\n' }

typeCodeInDynamicError err=:{ea_ok}
	# err = errorHeading "Warning" err
	  err = {err & ea_ok=ea_ok}
	= { err & ea_file = err.ea_file <<< "TC context not allowed in dynamic" <<< '\n' }

cycleAfterRemovingNewTypeConstructorsError ident err
	# err = errorHeading "Error" err
	= { err & ea_file = err.ea_file <<< (" cycle in definition of '" +++ toString ident +++ "' after removing newtype constructors") <<< '\n' }

/*
	As soon as all overloaded variables in an type context are instantiated, context reduction is carried out.
	This reduction yields a type class instance (here represented by a an index) and a list of
	ClassApplications.
*/

containsContext :: !TypeContext ![TypeContext] -> Bool
containsContext new_tc []
	= False
containsContext new_tc [tc : tcs]
	= new_tc == tc || containsContext new_tc tcs

FoundObject object :== object.glob_module <> NotFound
ObjectNotFound 	:== { glob_module = NotFound, glob_object = NotFound }

:: ReduceState =
	{	rs_new_contexts :: ![TypeContext]
	,	rs_special_instances :: !.SpecialInstances
	,	rs_type_pattern_vars :: ![LocalTypePatternVariable]
	,	rs_var_heap :: !.VarHeap
	,	rs_type_heaps :: !.TypeHeaps
	,	rs_coercions :: !.Coercions
	,	rs_predef_symbols :: !.PredefinedSymbols
	,	rs_error :: !.ErrorAdmin
	}

:: ReduceInfo =
	{	ri_defs :: !{# CommonDefs}
	,	ri_instance_info :: !ClassInstanceInfo
	,	ri_main_dcl_module_n :: !Int
	}

:: ReduceTCState =
	{	rtcs_new_contexts :: ![TypeContext]
	,	rtcs_type_pattern_vars :: ![LocalTypePatternVariable]
	,	rtcs_var_heap :: !.VarHeap
	,	rtcs_type_heaps :: !.TypeHeaps
	,	rtcs_error :: !.ErrorAdmin
	}

collect_variable_and_contexts :: [ClassApplication] [(Int,Int)] [TypeContext] -> [(Int,Int)]
collect_variable_and_contexts [CA_Context {tc_class,tc_types=[TempV type_var_n]}:constraints] variables_and_contexts class_context
	# context_index = determine_index_in_class_context tc_class class_context 0
	| context_index<0
		= collect_variable_and_contexts constraints variables_and_contexts class_context
		# variables_and_contexts = add_variable_and_context type_var_n (1<<context_index) variables_and_contexts
		= collect_variable_and_contexts constraints variables_and_contexts class_context
where
	determine_index_in_class_context :: !TCClass ![TypeContext] !Int -> Int
	determine_index_in_class_context tc_class [class_context:class_contexts] class_index
		| class_context.tc_class==tc_class
			= class_index
			= determine_index_in_class_context tc_class class_contexts (class_index+1)
	determine_index_in_class_context tc_class [] class_index
		= -1;

	add_variable_and_context :: !Int !Int ![(Int,Int)] -> [(Int,Int)]
	add_variable_and_context type_var_n tv_context [variable_and_context=:(variable,context):variables_and_contexts]
		| type_var_n==variable
			#! context=context bitor tv_context
			= [(variable,context) : variables_and_contexts]
			= [variable_and_context : add_variable_and_context type_var_n tv_context variables_and_contexts]
	add_variable_and_context type_var_n tv_context []
		= [(type_var_n,tv_context)]
collect_variable_and_contexts [CA_Instance {rcs_class_context={rc_red_contexts},rcs_constraints_contexts}:constraints] variables_and_contexts class_context
	# variables_and_contexts = collect_variable_and_contexts rc_red_contexts variables_and_contexts class_context
	# variables_and_contexts = collect_variable_and_contexts rcs_constraints_contexts variables_and_contexts class_context
	= collect_variable_and_contexts constraints variables_and_contexts class_context
collect_variable_and_contexts [CA_GlobalTypeCode {tci_contexts}:constraints] variables_and_contexts class_context
	# variables_and_contexts = collect_variable_and_contexts tci_contexts variables_and_contexts class_context
	= collect_variable_and_contexts constraints variables_and_contexts class_context
collect_variable_and_contexts [_:constraints] variables_and_contexts class_context
	= collect_variable_and_contexts constraints variables_and_contexts class_context
collect_variable_and_contexts [] variables_and_contexts class_context
	= variables_and_contexts

add_unexpanded_contexts :: ![Int] !TCClass !*ReduceState -> *ReduceState
add_unexpanded_contexts [variable:variables] tc_class rs_state=:{rs_new_contexts,rs_var_heap}
	# tc = {tc_class = tc_class, tc_types = [TempV variable], tc_var = nilPtr}
	| containsContext tc rs_new_contexts
		= add_unexpanded_contexts variables tc_class rs_state
		# (tc_var, rs_var_heap) = newPtr VI_Empty rs_var_heap
		# rs_new_contexts = [{tc & tc_var = tc_var} : rs_new_contexts]
		= add_unexpanded_contexts variables tc_class {rs_state & rs_new_contexts=rs_new_contexts, rs_var_heap=rs_var_heap}
add_unexpanded_contexts [] tc_class rs_state
	= rs_state 

reduceContexts :: !ReduceInfo ![TypeContext] !*ReduceState -> (![ClassApplication], !*ReduceState)
reduceContexts info tcs rs_state
	= mapSt (try_to_reduce_context info) tcs rs_state
where
	try_to_reduce_context :: !ReduceInfo !TypeContext !*ReduceState -> *(!ClassApplication, !*ReduceState)
	try_to_reduce_context info tc rs_state=:{rs_predef_symbols, rs_new_contexts}
		| context_is_reducible tc rs_predef_symbols
			= reduce_any_context info tc rs_state
		| containsContext tc rs_new_contexts
			= (CA_Context tc, rs_state)
			# {rs_var_heap, rs_new_contexts} = rs_state
			# (tc_var, rs_var_heap) = newPtr VI_Empty rs_var_heap
			# rs_new_contexts = [{tc & tc_var = tc_var} : rs_new_contexts]
			= (CA_Context tc, {rs_state & rs_var_heap=rs_var_heap, rs_new_contexts=rs_new_contexts})

	reduce_any_context :: !ReduceInfo !TypeContext !*ReduceState -> *(!ClassApplication, !*ReduceState)
	reduce_any_context info tc=:{tc_class=class_symb=:(TCGeneric {gtc_class})} rs_state
		= reduce_any_context info {tc & tc_class = TCClass gtc_class} rs_state
	reduce_any_context info=:{ri_defs} tc=:{tc_class=class_symb=:(TCClass {glob_object={ds_index},glob_module}),tc_types} rs_state=:{rs_predef_symbols}
		| is_predefined_symbol glob_module ds_index PD_TypeCodeClass rs_predef_symbols
			# {rs_new_contexts, rs_type_pattern_vars,rs_var_heap, rs_type_heaps, rs_error} = rs_state
			# rtcs_state = {rtcs_new_contexts=rs_new_contexts, rtcs_type_pattern_vars=rs_type_pattern_vars,
									rtcs_var_heap=rs_var_heap, rtcs_type_heaps=rs_type_heaps, rtcs_error=rs_error}
			# (red_context, {rtcs_new_contexts, rtcs_type_pattern_vars,rtcs_var_heap, rtcs_type_heaps, rtcs_error})
						= reduce_TC_context ri_defs class_symb (hd tc_types) rtcs_state
			# rs_state = {rs_state & rs_new_contexts=rtcs_new_contexts, rs_type_pattern_vars=rtcs_type_pattern_vars,
									rs_var_heap=rtcs_var_heap, rs_type_heaps=rtcs_type_heaps, rs_error=rtcs_error}
			= (red_context, rs_state)
			# (class_appls, rs_state)
					= reduce_context info tc rs_state
			= (CA_Instance class_appls, rs_state)

	reduce_context :: !ReduceInfo !TypeContext !*ReduceState -> *(!ReducedContexts, !*ReduceState)
	reduce_context info tc=:{tc_class=TCGeneric {gtc_class}} rs_state
		= reduce_context info {tc & tc_class = TCClass gtc_class} rs_state
	reduce_context info=:{ri_defs,ri_instance_info,ri_main_dcl_module_n} {tc_class=tc_class=:TCClass class_symb=:{glob_object={ds_index},glob_module},tc_types}
			rs_state
		# {class_members,class_context,class_args,class_ident} = ri_defs.[glob_module].com_class_defs.[ds_index]
		| size class_members > 0
			# class_instances = ri_instance_info.[glob_module].[ds_index]
			# {rs_coercions, rs_type_heaps} = rs_state
			# ({glob_module,glob_object}, contexts, uni_ok, rs_type_heaps, rs_coercions) = find_instance tc_types class_instances ri_defs rs_type_heaps rs_coercions
			# rs_state = {rs_state & rs_coercions=rs_coercions, rs_type_heaps=rs_type_heaps}
			| (glob_module <> NotFound) && uni_ok
				# {ins_members,ins_class_index,ins_specials} = ri_defs.[glob_module].com_instance_defs.[glob_object]
				| not ins_specials=:SP_GenerateRecordInstances
					# (appls, rs_state)
							= reduceContexts info contexts rs_state
					  (constraints, rs_state)
					  		= reduce_contexts_in_constraints info tc_types class_args class_context rs_state
					= ({ rcs_class_context = { rc_class_index = ins_class_index, rc_inst_module = glob_module, rc_inst_members = ins_members,
								rc_types = tc_types, rc_red_contexts = appls }, rcs_constraints_contexts = constraints }, rs_state)
					# {rs_predef_symbols, rs_error,rs_special_instances, rs_type_heaps} = rs_state
					  (rcs_class_context, rs_special_instances, (rs_predef_symbols, rs_type_heaps), rs_error)
						= check_unboxed_array_or_list_type ri_main_dcl_module_n glob_module ins_class_index ins_members tc_types class_members ri_defs class_instances
															rs_special_instances rs_predef_symbols rs_type_heaps rs_error
					  rs_state & rs_predef_symbols=rs_predef_symbols, rs_special_instances=rs_special_instances,rs_type_heaps=rs_type_heaps, rs_error=rs_error
					= ({ rcs_class_context = rcs_class_context, rcs_constraints_contexts = []}, rs_state)
				# rcs_class_context = { rc_class_index = {gi_module=glob_module,gi_index=ds_index}, rc_inst_module = NoIndex, rc_inst_members = {}, rc_types = tc_types, rc_red_contexts = [] }
				| glob_module <> NotFound
					# rs_state = {rs_state & rs_error = uniqueError class_ident tc_types rs_state.rs_error}
					= ({ rcs_class_context = rcs_class_context, rcs_constraints_contexts = []}, rs_state)
					# rs_state = {rs_state & rs_error = instanceError class_ident tc_types rs_state.rs_error}
					= ({ rcs_class_context = rcs_class_context, rcs_constraints_contexts = []}, rs_state)
			# (constraints, rs_state)
				= reduce_contexts_in_constraints info tc_types class_args class_context rs_state

			| case tc_types of [_] -> False; _ -> True
			|| case class_context of [] -> True; [_] -> True; _ -> False
				// not implemented for multiparameter type classes or fewer than 2 class constraints
				= ({ rcs_class_context = { rc_class_index = {gi_module=glob_module,gi_index=ds_index}, rc_inst_module = NoIndex, rc_inst_members = {}, rc_types = tc_types, rc_red_contexts = [] },
					rcs_constraints_contexts = constraints }, rs_state)

			// if a constraint of a class without members is reduced, and all classes in the constraint of that class appear
			// in the reduced constraints for a variable, add a constraint for the original class for that variable
			// (this causes removal of the other constraints later), to prevent functions with too many constraints
			# n_contexts = length class_context
			  required_used_contexts = (2<<(n_contexts-1))-1 // beware of 1<<32==0 on IA32
			  variables_and_contexts = collect_variable_and_contexts constraints [] class_context
			  variables = [variable \\ (variable,used_contexts)<-variables_and_contexts | used_contexts==required_used_contexts]		

			  rs_state = add_unexpanded_contexts variables tc_class rs_state
			
			= ({ rcs_class_context = { rc_class_index = {gi_module=glob_module,gi_index=ds_index}, rc_inst_module = NoIndex, rc_inst_members = {}, rc_types = tc_types, rc_red_contexts = [] },
				rcs_constraints_contexts = constraints }, rs_state)

	reduce_contexts_in_constraints :: !ReduceInfo ![Type] ![TypeVar] ![TypeContext] *ReduceState
		-> *([ClassApplication],*ReduceState)
	reduce_contexts_in_constraints info types class_args [] rs_state
		= ([],rs_state)
	reduce_contexts_in_constraints info types class_args class_context rs_state=:{rs_type_heaps=rs_type_heaps=:{th_vars}}
		# th_vars = fold2St (\ type {tv_info_ptr} -> writePtr tv_info_ptr (TVI_Type type)) types class_args th_vars
		  (instantiated_context, rs_type_heaps) = fresh_contexts class_context { rs_type_heaps & th_vars = th_vars }
		# rs_state = {rs_state & rs_type_heaps=rs_type_heaps}
		= mapSt (reduce_any_context info) instantiated_context rs_state

	find_instance :: [Type] !InstanceTree {#CommonDefs} *TypeHeaps *Coercions -> *(Global Int,[TypeContext],Bool,*TypeHeaps,*Coercions)
	find_instance co_types (IT_Node this_inst_index=:{glob_object,glob_module} left right) defs type_heaps coercion_env
		# (left_index, types, uni_ok, type_heaps, coercion_env) = find_instance co_types left defs type_heaps coercion_env
		| FoundObject left_index
			= (left_index, types, uni_ok, type_heaps, coercion_env)
			# {ins_type={it_types,it_context}, ins_specials} = defs.[glob_module].com_instance_defs.[glob_object]
			  (matched, type_heaps) = match defs it_types co_types type_heaps
			| matched
				# (subst_context, type_heaps) = fresh_contexts it_context type_heaps
				  (uni_ok, coercion_env, type_heaps) = adjust_type_attributes defs co_types it_types coercion_env type_heaps
				  (spec_inst, type_heaps) = trySpecializedInstances subst_context (get_specials ins_specials) type_heaps
				| FoundObject spec_inst
					= (spec_inst, [], uni_ok, type_heaps, coercion_env)
					= (this_inst_index, subst_context, uni_ok, type_heaps, coercion_env)
				= find_instance co_types right defs type_heaps coercion_env
	find_instance co_types IT_Empty defs heaps coercion_env
		= (ObjectNotFound, [], True, heaps, coercion_env)

	get_specials :: Specials -> [Special]
	get_specials (SP_ContextTypes specials) = specials
	get_specials SP_None = []
	get_specials SP_GenerateRecordInstances = []

	adjust_type_attributes :: !{#CommonDefs} ![Type] ![Type] !*Coercions !*TypeHeaps -> (Bool, !*Coercions, !*TypeHeaps)
	adjust_type_attributes defs act_types form_types coercion_env type_heaps
		= fold2St (adjust_type_attribute defs) act_types form_types (True, coercion_env, type_heaps)

	adjust_type_attribute :: !{#CommonDefs} !Type !Type !(Bool, !*Coercions, !*TypeHeaps) -> (Bool, !*Coercions, !*TypeHeaps)
	adjust_type_attribute _ _ (TV _) state
		= state
	adjust_type_attribute defs type1=:(TA type_cons1 cons_args1) type2=:(TA type_cons2 cons_args2) (ok, coercion_env, type_heaps)
		| type_cons1 == type_cons2
			= adjust_attributes_and_subtypes defs cons_args1 cons_args2 (ok, coercion_env, type_heaps)
			= expand_types_and_adjust_type_attribute type_cons1 cons_args1 type_cons2 cons_args2 defs type1 type2 ok coercion_env type_heaps
	adjust_type_attribute defs type1=:(TA type_cons1 cons_args1) type2=:(TAS type_cons2 cons_args2 _) (ok, coercion_env, type_heaps)
		| type_cons1 == type_cons2
			= adjust_attributes_and_subtypes defs cons_args1 cons_args2 (ok, coercion_env, type_heaps)
			= expand_types_and_adjust_type_attribute type_cons1 cons_args1 type_cons2 cons_args2 defs type1 type2 ok coercion_env type_heaps
	adjust_type_attribute defs type1=:(TA type_cons1 cons_args1) type2 (ok, coercion_env, type_heaps)
		# (expanded, type1, type_heaps) = tryToExpandTypeSyn defs type1 type_cons1 cons_args1 type_heaps
		| expanded
			= adjust_type_attribute defs type1 type2 (ok, coercion_env, type_heaps)
			= (ok, coercion_env, type_heaps)
	adjust_type_attribute defs type1=:(TAS type_cons1 cons_args1 _) type2=:(TA type_cons2 cons_args2) (ok, coercion_env, type_heaps)
		| type_cons1 == type_cons2
			= adjust_attributes_and_subtypes defs cons_args1 cons_args2 (ok, coercion_env, type_heaps)
			= expand_types_and_adjust_type_attribute type_cons1 cons_args1 type_cons2 cons_args2 defs type1 type2 ok coercion_env type_heaps
	adjust_type_attribute defs type1=:(TAS type_cons1 cons_args1 _) type2=:(TAS type_cons2 cons_args2 _) (ok, coercion_env, type_heaps)
		| type_cons1 == type_cons2
			= adjust_attributes_and_subtypes defs cons_args1 cons_args2 (ok, coercion_env, type_heaps)
			= expand_types_and_adjust_type_attribute type_cons1 cons_args1 type_cons2 cons_args2 defs type1 type2 ok coercion_env type_heaps
	adjust_type_attribute defs type1=:(TAS type_cons1 cons_args1 _) type2 (ok, coercion_env, type_heaps)
		# (expanded, type1, type_heaps) = tryToExpandTypeSyn defs type1 type_cons1 cons_args1 type_heaps
		| expanded
			= adjust_type_attribute defs type1 type2 (ok, coercion_env, type_heaps)
			= (ok, coercion_env, type_heaps)
	adjust_type_attribute defs (arg_type1 --> res_type1) (arg_type2 --> res_type2) state
		= adjust_attributes_and_subtypes defs [arg_type1, res_type1] [arg_type2, res_type2] state
	adjust_type_attribute defs (TArrow1 x) (TArrow1 y) state
		= adjust_attributes_and_subtypes defs [x] [y] state
	adjust_type_attribute defs (_ :@: types1) (_ :@: types2) state
		= adjust_attributes_and_subtypes defs types1 types2 state
	adjust_type_attribute defs type1 type2=:(TA type_cons2 cons_args2) (ok, coercion_env, type_heaps)
		# (expanded, type2, type_heaps) = tryToExpandTypeSyn defs type2 type_cons2 cons_args2 type_heaps
		| expanded
			= adjust_type_attribute defs type1 type2 (ok, coercion_env, type_heaps)
			= (ok, coercion_env, type_heaps)
	adjust_type_attribute defs type1 type2=:(TAS type_cons2 cons_args2 _) (ok, coercion_env, type_heaps)
		# (expanded, type2, type_heaps) = tryToExpandTypeSyn defs type2 type_cons2 cons_args2 type_heaps
		| expanded
			= adjust_type_attribute defs type1 type2 (ok, coercion_env, type_heaps)
			= (ok, coercion_env, type_heaps)
	adjust_type_attribute _ _ _ state
		= state
	
	expand_types_and_adjust_type_attribute type_cons1 cons_args1 type_cons2 cons_args2 defs type1 type2 ok coercion_env type_heaps
		# (_, type1, type_heaps) = tryToExpandTypeSyn defs type1 type_cons1 cons_args1 type_heaps
		  (_, type2, type_heaps) = tryToExpandTypeSyn defs type2 type_cons2 cons_args2 type_heaps
		= adjust_type_attribute defs type1 type2 (ok, coercion_env, type_heaps)

	adjust_attributes_and_subtypes :: !{#CommonDefs} ![AType] ![AType] !(Bool, !*Coercions, !*TypeHeaps) -> (Bool, !*Coercions, !*TypeHeaps)
	adjust_attributes_and_subtypes defs types1 types2 state
		= fold2St (adjust_attribute_and_subtypes defs) types1 types2 state
		
	adjust_attribute_and_subtypes :: !{#CommonDefs} !AType !AType !(Bool, !*Coercions, !*TypeHeaps) -> (Bool, !*Coercions, !*TypeHeaps)
	adjust_attribute_and_subtypes defs atype1 atype2 (ok, coercion_env, type_heaps)
		# (ok, coercion_env) = adjust_attribute atype1.at_attribute atype2.at_attribute (ok, coercion_env)
		= adjust_type_attribute defs atype1.at_type atype2.at_type (ok, coercion_env, type_heaps)
	where
		adjust_attribute :: !TypeAttribute !TypeAttribute !(Bool, !*Coercions) -> (Bool, !*Coercions)
		adjust_attribute attr1 (TA_Var _) state
			= state
		adjust_attribute attr1 TA_Unique (ok, coercion_env)
			= case attr1 of
				TA_Unique
					-> (ok, coercion_env)
				TA_TempVar av_number
					# (succ, coercion_env) = tryToMakeUnique av_number coercion_env
					-> (ok && succ, coercion_env)
				_
					-> (False, coercion_env)
	
		adjust_attribute attr1 attr (ok, coercion_env)
			= case attr1 of
				TA_Multi
					-> (ok, coercion_env)
				TA_TempVar av_number
					# (succ, coercion_env) = tryToMakeNonUnique av_number coercion_env
					-> (ok && succ, coercion_env)
				_
					-> (False, coercion_env)

	fresh_contexts :: ![TypeContext] !*TypeHeaps -> ([TypeContext],*TypeHeaps)
	fresh_contexts contexts type_heaps
		= mapSt fresh_context contexts type_heaps
	where
		fresh_context :: !TypeContext !*TypeHeaps -> (TypeContext,*TypeHeaps)
		fresh_context tc=:{tc_types} type_heaps
			# (changed_tc_types, tc_types, type_heaps) = substitute tc_types type_heaps
			| changed_tc_types
				= ({tc & tc_types = tc_types}, type_heaps)
				= (tc, type_heaps)

	disallow_abstract_types_in_dynamics :: {#CommonDefs} (Global Index) *ErrorAdmin ->  *ErrorAdmin
	disallow_abstract_types_in_dynamics defs type_index=:{glob_module,glob_object} error
		| cPredefinedModuleIndex == glob_module
			= error
			
		#! ({td_ident,td_rhs})
			= defs.[glob_module].com_type_defs.[glob_object]
		= case td_rhs of
				AbstractType _			-> abstractTypeInDynamicError td_ident error
				AbstractSynType _ _		-> abstractTypeInDynamicError td_ident error
				_						-> error

	reduce_TC_context :: {#CommonDefs} TCClass Type *ReduceTCState -> (ClassApplication, !*ReduceTCState)
	reduce_TC_context defs type_code_class tc_type rtcs_state
		= reduce_tc_context defs type_code_class tc_type rtcs_state
	where
		reduce_tc_context :: {#CommonDefs} TCClass Type *ReduceTCState -> (ClassApplication, !*ReduceTCState)
		reduce_tc_context defs type_code_class type=:(TA cons_id=:{type_index} cons_args) rtcs_state=:{rtcs_error,rtcs_type_heaps}
			# rtcs_error = disallow_abstract_types_in_dynamics defs type_index rtcs_error
			# (expanded, type, rtcs_type_heaps)
				=	tryToExpandTypeSyn defs type cons_id cons_args rtcs_type_heaps
			# rtcs_state = {rtcs_state & rtcs_error=rtcs_error, rtcs_type_heaps=rtcs_type_heaps}
			| expanded
				=	reduce_tc_context defs type_code_class type rtcs_state
			# type_constructor = toTypeCodeConstructor type_index defs
			  (rc_red_contexts, rtcs_state) = reduce_TC_contexts defs type_code_class cons_args rtcs_state
			= (CA_GlobalTypeCode { tci_constructor = type_constructor, tci_contexts = rc_red_contexts }, rtcs_state)
		reduce_tc_context defs type_code_class (TAS cons_id cons_args _) rtcs_state
			= reduce_tc_context defs type_code_class (TA cons_id cons_args) rtcs_state
		reduce_tc_context defs type_code_class (TB basic_type) rtcs_state
			= (CA_GlobalTypeCode { tci_constructor = GTT_Basic basic_type, tci_contexts = [] }, rtcs_state)
		reduce_tc_context defs type_code_class (arg_type --> result_type) rtcs_state
			#  (rc_red_contexts, rtcs_state) = reduce_TC_contexts defs type_code_class [arg_type, result_type] rtcs_state
			= (CA_GlobalTypeCode { tci_constructor = GTT_Function, tci_contexts = rc_red_contexts }, rtcs_state)
		reduce_tc_context defs type_code_class (TempQV var_number) rtcs_state=:{rtcs_var_heap,rtcs_new_contexts}
			# (tc_var, rtcs_var_heap) = newPtr VI_Empty rtcs_var_heap
			# rtcs_state={rtcs_state & rtcs_var_heap=rtcs_var_heap}
			# tc = { tc_class = type_code_class, tc_types = [TempQV var_number], tc_var = tc_var }
			| containsContext tc rtcs_new_contexts
				= (CA_Context tc, rtcs_state)
				= (CA_Context tc, {rtcs_state & rtcs_new_contexts = [tc : rtcs_new_contexts]})
		reduce_tc_context defs type_code_class (TempQDV var_number) rtcs_state=:{rtcs_type_pattern_vars, rtcs_var_heap}
			# (inst_var, (rtcs_type_pattern_vars, rtcs_var_heap))
				= addLocalTCInstance var_number (rtcs_type_pattern_vars, rtcs_var_heap)
			# rtcs_state = {rtcs_state & rtcs_type_pattern_vars=rtcs_type_pattern_vars, rtcs_var_heap=rtcs_var_heap}
			= (CA_LocalTypeCode inst_var, rtcs_state)
		reduce_tc_context defs type_code_class (TempV var_number) rtcs_state=:{rtcs_var_heap, rtcs_new_contexts}
			# (tc_var, rtcs_var_heap) = newPtr VI_Empty rtcs_var_heap
			# rtcs_state={rtcs_state & rtcs_var_heap=rtcs_var_heap}
			  tc = { tc_class = type_code_class, tc_types = [TempV var_number], tc_var = tc_var }
			| containsContext tc rtcs_new_contexts
				= (CA_Context tc, rtcs_state)
				= (CA_Context tc, {rtcs_state & rtcs_new_contexts = [tc : rtcs_new_contexts]})
		reduce_tc_context defs type_code_class type=:(TempCV _ :@: _) rtcs_state=:{rtcs_var_heap, rtcs_new_contexts}
			# (tc_var, rtcs_var_heap) = newPtr VI_Empty rtcs_var_heap
			# rtcs_state={rtcs_state & rtcs_var_heap=rtcs_var_heap}
			  tc = { tc_class=type_code_class, tc_types=[type], tc_var=tc_var }
			| containsContext tc rtcs_new_contexts
				= (CA_Context tc, rtcs_state)
				= (CA_Context tc, {rtcs_state & rtcs_new_contexts = [tc : rtcs_new_contexts]})

		reduce_TC_contexts :: {#CommonDefs} TCClass [AType] *ReduceTCState -> ([ClassApplication], !*ReduceTCState)
		reduce_TC_contexts defs type_code_class cons_args rtcs_state
			= mapSt (\{at_type} -> reduce_tc_context defs type_code_class at_type) cons_args rtcs_state

check_unboxed_array_or_list_type :: Int Int GlobalIndex {#ClassInstanceMember} ![Type] {#DefinedSymbol} {#CommonDefs} InstanceTree
					   *SpecialInstances  *PredefinedSymbols *TypeHeaps   *ErrorAdmin
	-> (ReducedContext,*SpecialInstances,(*PredefinedSymbols,*TypeHeaps), *ErrorAdmin)
check_unboxed_array_or_list_type ri_main_dcl_module_n glob_module ins_class_index ins_members tc_types class_members ri_defs class_instances
		rs_special_instances rs_predef_symbols rs_type_heaps rs_error
	| is_predefined_global_symbol ins_class_index PD_ArrayClass rs_predef_symbols && is_unboxed_array tc_types rs_predef_symbols
		= check_unboxed_array_type ri_main_dcl_module_n glob_module ins_class_index ins_members tc_types class_members ri_defs class_instances
			rs_special_instances (rs_predef_symbols, rs_type_heaps) rs_error
	| is_predefined_global_symbol ins_class_index PD_UListClass rs_predef_symbols
		= check_unboxed_list_type ri_main_dcl_module_n glob_module ins_class_index ins_members tc_types class_members ri_defs class_instances
			rs_special_instances (rs_predef_symbols, rs_type_heaps) rs_error
	| is_predefined_global_symbol ins_class_index PD_UTSListClass rs_predef_symbols
		= check_unboxed_tail_strict_list_type ri_main_dcl_module_n glob_module ins_class_index ins_members tc_types class_members ri_defs class_instances
			rs_special_instances (rs_predef_symbols, rs_type_heaps) rs_error
where
	is_predefined_global_symbol :: !GlobalIndex !Int !PredefinedSymbols -> Bool
	is_predefined_global_symbol {gi_module,gi_index} predef_index predef_symbols
		# {pds_def,pds_module} = predef_symbols.[predef_index]
		= gi_module == pds_module && gi_index == pds_def

	is_unboxed_array:: [Type] PredefinedSymbols -> Bool
	is_unboxed_array [TA {type_index={glob_module,glob_object},type_arity} _ : _] predef_symbols
		= is_predefined_symbol glob_module glob_object PD_UnboxedArrayType predef_symbols
	is_unboxed_array _ predef_symbols
		= False

	check_unboxed_array_type :: Int Int GlobalIndex {#ClassInstanceMember} ![Type] {#DefinedSymbol} {#CommonDefs} InstanceTree
						   *SpecialInstances *(*PredefinedSymbols,*TypeHeaps) *ErrorAdmin
		-> (ReducedContext,*SpecialInstances,(*PredefinedSymbols,*TypeHeaps), *ErrorAdmin)
	check_unboxed_array_type main_dcl_module_n ins_module ins_class_index ins_members types=:[ _, elem_type :_] class_members defs class_instances
			special_instances predef_symbols_type_heaps error
		# (unboxed_type, opt_record, predef_symbols_type_heaps) = try_to_unbox elem_type defs predef_symbols_type_heaps
		= case opt_record of
			Yes record
				# (ins_members, special_instances) = add_record_to_array_instances record class_members special_instances
				-> ({rc_class_index = ins_class_index, rc_inst_module = main_dcl_module_n, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
						special_instances, predef_symbols_type_heaps, error)
			No
				| not unboxed_type=:TE
					# (predef_symbols,type_heaps) = predef_symbols_type_heaps
					# ({glob_module,glob_object},predef_symbols) = find_unboxed_array_instance unboxed_type class_instances defs predef_symbols
					| glob_module <> NotFound
						# {ins_members,ins_class_index} = defs.[glob_module].com_instance_defs.[glob_object]
						-> ({rc_class_index=ins_class_index, rc_inst_module=glob_module, rc_inst_members=ins_members, rc_types=types, rc_red_contexts=[]},
							special_instances, (predef_symbols,type_heaps), error)
						-> ({rc_class_index = ins_class_index, rc_inst_module = ins_module, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
								special_instances, (predef_symbols,type_heaps), unboxError "Array" elem_type error)
				-> ({rc_class_index = ins_class_index, rc_inst_module = ins_module, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
					special_instances, predef_symbols_type_heaps, unboxError "Array" elem_type error)
	where
		add_record_to_array_instances :: !TypeSymbIdent !{#DefinedSymbol} !*SpecialInstances -> (!{#ClassInstanceMember},!*SpecialInstances)
		add_record_to_array_instances record members special_instances=:{si_next_array_member_index,si_array_instances}
			# may_be_there = look_up_array_or_list_instance record si_array_instances
			= case may_be_there of
				Yes inst
					-> (inst.ai_members, special_instances)
				No
					# inst = new_array_instance record members si_next_array_member_index
					-> (inst.ai_members, { special_instances &  si_next_array_member_index = si_next_array_member_index + size members,
																si_array_instances = [ inst : si_array_instances ] })

	check_unboxed_list_type :: Int Int GlobalIndex {#ClassInstanceMember} ![Type] {#DefinedSymbol} {#CommonDefs} InstanceTree
						   *SpecialInstances *(*PredefinedSymbols,*TypeHeaps) *ErrorAdmin
		-> (ReducedContext,*SpecialInstances,(*PredefinedSymbols,*TypeHeaps), *ErrorAdmin)
	check_unboxed_list_type main_dcl_module_n ins_module ins_class_index ins_members types=:[elem_type:_] class_members defs class_instances
			special_instances predef_symbols_type_heaps error
		# (unboxed_type, opt_record, predef_symbols_type_heaps) = try_to_unbox elem_type defs predef_symbols_type_heaps
		= case opt_record of
			Yes record
				# (ins_members, special_instances) = add_record_to_list_instances record class_members special_instances
				-> ({rc_class_index = ins_class_index, rc_inst_module = main_dcl_module_n, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
						special_instances, predef_symbols_type_heaps, error)
			No
				| not unboxed_type=:TE
					# {glob_module,glob_object} = find_unboxed_list_instance unboxed_type class_instances defs
					| glob_module <> NotFound
						# {ins_members,ins_class_index} = defs.[glob_module].com_instance_defs.[glob_object]
						-> ({rc_class_index=ins_class_index, rc_inst_module=glob_module, rc_inst_members=ins_members, rc_types=types, rc_red_contexts=[]},
							special_instances, predef_symbols_type_heaps, error)
						-> ({rc_class_index = ins_class_index, rc_inst_module = ins_module, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
							special_instances, predef_symbols_type_heaps, unboxError "UList" elem_type error)
				-> ({rc_class_index = ins_class_index, rc_inst_module = ins_module, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
					special_instances, predef_symbols_type_heaps, unboxError "UList" elem_type error)
	where
		add_record_to_list_instances :: !TypeSymbIdent !{# DefinedSymbol} !*SpecialInstances -> (!{#ClassInstanceMember},!*SpecialInstances)
		add_record_to_list_instances record members special_instances=:{si_next_array_member_index,si_list_instances}
			# may_be_there = look_up_array_or_list_instance record si_list_instances
			= case may_be_there of
				Yes inst
					-> (inst.ai_members, special_instances)
				No
					# inst = new_array_instance record members si_next_array_member_index
					-> (inst.ai_members, { special_instances &  si_next_array_member_index = si_next_array_member_index + size members,
																si_list_instances = [ inst : si_list_instances ] })

	check_unboxed_tail_strict_list_type :: Int Int GlobalIndex {#ClassInstanceMember} ![Type] {#DefinedSymbol} {#CommonDefs} InstanceTree
						   *SpecialInstances *(*PredefinedSymbols,*TypeHeaps) *ErrorAdmin
		-> (ReducedContext,*SpecialInstances,(*PredefinedSymbols,*TypeHeaps), *ErrorAdmin)
	check_unboxed_tail_strict_list_type main_dcl_module_n ins_module ins_class_index ins_members types=:[elem_type:_] class_members defs class_instances
			special_instances predef_symbols_type_heaps error
		# (unboxed_type, opt_record, predef_symbols_type_heaps) = try_to_unbox elem_type defs predef_symbols_type_heaps
		= case opt_record of
			Yes record
				# (ins_members, special_instances) = add_record_to_tail_strict_list_instances record class_members special_instances
				-> ({rc_class_index = ins_class_index, rc_inst_module = main_dcl_module_n, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
						special_instances, predef_symbols_type_heaps, error)
			No
				| not unboxed_type=:TE
					# {glob_module,glob_object} = find_unboxed_list_instance unboxed_type class_instances defs
					| glob_module <> NotFound
						# {ins_members,ins_class_index} = defs.[glob_module].com_instance_defs.[glob_object]
						-> ({rc_class_index=ins_class_index, rc_inst_module=glob_module, rc_inst_members=ins_members, rc_types=types, rc_red_contexts=[]},
							special_instances, predef_symbols_type_heaps, error)
						-> ({rc_class_index = ins_class_index, rc_inst_module = ins_module, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
							special_instances, predef_symbols_type_heaps, unboxError "UTSList" elem_type error)
				-> ({rc_class_index = ins_class_index, rc_inst_module = ins_module, rc_inst_members = ins_members, rc_red_contexts = [], rc_types = types},
					special_instances, predef_symbols_type_heaps, unboxError "UTSList" elem_type error)
	where
		add_record_to_tail_strict_list_instances :: !TypeSymbIdent !{#DefinedSymbol} !*SpecialInstances -> (!{#ClassInstanceMember},!*SpecialInstances)
		add_record_to_tail_strict_list_instances record members special_instances=:{si_next_array_member_index,si_tail_strict_list_instances}
			# may_be_there = look_up_array_or_list_instance record si_tail_strict_list_instances
			= case may_be_there of
				Yes inst
					-> (inst.ai_members, special_instances)
				No
					# inst = new_array_instance record members si_next_array_member_index
					-> (inst.ai_members, { special_instances &  si_next_array_member_index = si_next_array_member_index + size members,
																si_tail_strict_list_instances = [ inst : si_tail_strict_list_instances ] })

	try_to_unbox :: Type !{#CommonDefs} (!*PredefinedSymbols, !*TypeHeaps) -> (!Type, !Optional TypeSymbIdent, !(!*PredefinedSymbols, !*TypeHeaps))
	try_to_unbox type=:(TB _) _ predef_symbols_type_heaps
		= (type, No, predef_symbols_type_heaps)
	try_to_unbox type=:(TA type_symb=:{type_index={glob_module,glob_object},type_arity} type_args) defs (predef_symbols, type_heaps)
		# {td_arity,td_rhs,td_args,td_attribute} = defs.[glob_module].com_type_defs.[glob_object]
		= case td_rhs of
			RecordType _
				-> (TE, Yes type_symb, (predef_symbols, type_heaps))
			AbstractType _
				| is_predefined_symbol glob_module glob_object PD_LazyArrayType predef_symbols ||
				  is_predefined_symbol glob_module glob_object PD_StrictArrayType predef_symbols ||
				  is_predefined_symbol glob_module glob_object PD_UnboxedArrayType predef_symbols
					-> (type, No, (predef_symbols, type_heaps))
					-> (TE, No, (predef_symbols, type_heaps))
			SynType {at_type}
				# (expanded_type, type_heaps) = substituteType td_attribute TA_Multi td_args type_args at_type type_heaps
				-> try_to_unbox expanded_type defs (predef_symbols, type_heaps)
			NewType {ds_index}
				# {cons_type={st_args=[arg_type:_]}} = defs.[glob_module].com_cons_defs.[ds_index];
				# (expanded_type, type_heaps) = substituteType td_attribute TA_Multi td_args type_args arg_type.at_type type_heaps
				-> try_to_unbox expanded_type defs (predef_symbols, type_heaps)
			_
				-> (TE, No, (predef_symbols, type_heaps))				
	try_to_unbox type _ predef_symbols_type_heaps
		= (TE, No, predef_symbols_type_heaps)

	find_unboxed_array_instance :: Type !InstanceTree {#CommonDefs} *PredefinedSymbols -> *(!Global Int,!*PredefinedSymbols)
	find_unboxed_array_instance element_type (IT_Node this_inst_index=:{glob_object,glob_module} left right) defs predef_symbols
		# (left_index,predef_symbols) = find_unboxed_array_instance element_type left defs predef_symbols
		| FoundObject left_index
			= (left_index,predef_symbols)
			= case defs.[glob_module].com_instance_defs.[glob_object].ins_type.it_types of
				[TA {type_index={glob_module,glob_object}} _,instance_element_type:_]
					| is_predefined_symbol glob_module glob_object PD_UnboxedArrayType predef_symbols
						-> case (element_type,instance_element_type) of
							(TB bt1,TB bt2)
								| bt1==bt2
									-> (this_inst_index,predef_symbols)
							(TA {type_index=ti1} [_],TA {type_index=ti2} [_]) // for array elements
								| ti1==ti2
									-> (this_inst_index,predef_symbols)
							_
								-> find_unboxed_array_instance element_type right defs predef_symbols
				_
					-> find_unboxed_array_instance element_type right defs predef_symbols
	find_unboxed_array_instance co_types IT_Empty defs predef_symbols
		= (ObjectNotFound,predef_symbols)

	find_unboxed_list_instance :: Type !InstanceTree {#CommonDefs} -> Global Int
	find_unboxed_list_instance element_type (IT_Node this_inst_index=:{glob_object,glob_module} left right) defs
		# left_index = find_unboxed_list_instance element_type left defs
		| FoundObject left_index
			= left_index
			= case defs.[glob_module].com_instance_defs.[glob_object].ins_type.it_types of
				[instance_element_type]
					-> case (element_type,instance_element_type) of
						(TB bt1,TB bt2)
							| bt1==bt2
								-> this_inst_index
						(TA {type_index=ti1} [_],TA {type_index=ti2} [_]) // for array elements
							| ti1==ti2
								-> this_inst_index
						_
							-> find_unboxed_list_instance element_type right defs
				_
					-> find_unboxed_list_instance element_type right defs
	find_unboxed_list_instance co_types IT_Empty defs
		= ObjectNotFound

	look_up_array_or_list_instance :: !TypeSymbIdent ![ArrayInstance] -> Optional ArrayInstance
	look_up_array_or_list_instance record []
		= No
	look_up_array_or_list_instance record [inst : insts]
		| record == inst.ai_record
			= Yes inst
			= look_up_array_or_list_instance record insts
	
	new_array_instance :: !TypeSymbIdent !{#DefinedSymbol} !Index -> ArrayInstance
	new_array_instance record members next_member_index
		= {	ai_members = { {cim_ident=ds_ident,cim_arity=ds_arity,cim_index=next_inst_index} \\ {ds_ident,ds_arity} <-: members & next_inst_index <- [next_member_index .. ]},
			ai_record = record }

context_is_reducible :: TypeContext PredefinedSymbols -> Bool
context_is_reducible {tc_class=TCClass class_symb,tc_types = [type : types]} predef_symbols
	= type_is_reducible type class_symb predef_symbols && types_are_reducible types type class_symb predef_symbols
context_is_reducible tc=:{tc_class=TCGeneric {gtc_class}, tc_types = [type : types]} predef_symbols
	= type_is_reducible type gtc_class predef_symbols && types_are_reducible types type gtc_class predef_symbols

types_are_reducible :: [Type] Type (Global DefinedSymbol) PredefinedSymbols -> Bool
types_are_reducible [] _ _ _
	= True
types_are_reducible [type : types] first_type tc_class predef_symbols
	= case type of
		TempV _
			->	is_lazy_or_strict_array_or_list_context
		_ :@: _
			->	is_lazy_or_strict_array_or_list_context
		TempQV _
			->	is_lazy_or_strict_array_or_list_context
		TempQDV _
			->	is_lazy_or_strict_array_or_list_context
		_
			-> is_reducible types tc_class predef_symbols
where
	is_lazy_or_strict_array_or_list_context
		=>	(is_predefined_symbol tc_class.glob_module tc_class.glob_object.ds_index PD_ArrayClass predef_symbols &&
			is_lazy_or_strict_array_type first_type predef_symbols)
			||
			(is_predefined_symbol tc_class.glob_module tc_class.glob_object.ds_index PD_ListClass predef_symbols &&
			is_lazy_or_strict_list_type first_type predef_symbols)

	is_lazy_or_strict_array_type :: Type PredefinedSymbols -> Bool
	is_lazy_or_strict_array_type (TA {type_index={glob_module,glob_object}} _) predef_symbols
		= is_predefined_symbol glob_module glob_object PD_LazyArrayType predef_symbols ||
		  is_predefined_symbol glob_module glob_object PD_StrictArrayType predef_symbols
	is_lazy_or_strict_array_type _ _
		= False

	is_lazy_or_strict_list_type :: Type PredefinedSymbols -> Bool
	is_lazy_or_strict_list_type (TA {type_index={glob_module,glob_object}} _) predef_symbols
		= is_predefined_symbol glob_module glob_object PD_ListType predef_symbols ||
		  is_predefined_symbol glob_module glob_object PD_TailStrictListType predef_symbols ||
		  is_predefined_symbol glob_module glob_object PD_StrictListType predef_symbols ||
		  is_predefined_symbol glob_module glob_object PD_StrictTailStrictListType predef_symbols ||
		  is_predefined_symbol glob_module glob_object PD_UnboxedListType predef_symbols ||
		  is_predefined_symbol glob_module glob_object PD_UnboxedTailStrictListType predef_symbols
	is_lazy_or_strict_list_type _ _
		= False

	is_reducible :: [Type] (Global DefinedSymbol) PredefinedSymbols -> Bool
	is_reducible [] tc_class predef_symbols
		= True
	is_reducible [type : types] tc_class predef_symbols
		= type_is_reducible type tc_class predef_symbols && is_reducible types tc_class predef_symbols

type_is_reducible :: Type (Global DefinedSymbol) PredefinedSymbols -> Bool
type_is_reducible (TempV _) tc_class predef_symbols
	= False // is_predefined_symbol tc_class.glob_module tc_class.glob_object.ds_index PD_TypeCodeClass predef_symbols
type_is_reducible (_ :@: _) tc_class predef_symbols
	= False
type_is_reducible (TempQV _) tc_class predef_symbols
	= False
type_is_reducible (TempQDV _) {glob_object={ds_index},glob_module} predef_symbols
	= is_predefined_symbol glob_module ds_index PD_TypeCodeClass predef_symbols
type_is_reducible _ tc_class predef_symbols
	= True

is_predefined_symbol :: !Int !Int !Int !PredefinedSymbols -> Bool
is_predefined_symbol mod_index symb_index predef_index predef_symbols
	# {pds_def,pds_module} = predef_symbols.[predef_index]
	= mod_index == pds_module && symb_index == pds_def

addLocalTCInstance :: Int (([LocalTypePatternVariable], *VarHeap)) -> (VarInfoPtr, ([LocalTypePatternVariable], *VarHeap))
addLocalTCInstance var_number (instances=:[inst : insts], ltp_var_heap)
	# cmp = var_number =< inst.ltpv_var
	| cmp == Equal
		= (inst.ltpv_new_var, (instances, ltp_var_heap))
	| cmp == Smaller
		# (ltpv_new_var, ltp_var_heap) = newPtr VI_Empty ltp_var_heap
		= (ltpv_new_var, ( [{ ltpv_new_var = ltpv_new_var, ltpv_var = var_number } : instances ], ltp_var_heap ))
		# (found_var, (insts, ltp_var_heap)) = addLocalTCInstance var_number (insts, ltp_var_heap)
		= (found_var, ([inst : insts ], ltp_var_heap))
addLocalTCInstance var_number ([], ltp_var_heap)
	# (ltpv_new_var, ltp_var_heap) = newPtr VI_Empty ltp_var_heap
	= (ltpv_new_var, ([{ ltpv_new_var = ltpv_new_var, ltpv_var = var_number }], ltp_var_heap))

tryToExpandTypeSyn :: {#CommonDefs} Type TypeSymbIdent [AType] *TypeHeaps -> (Bool, Type, *TypeHeaps)
tryToExpandTypeSyn defs type cons_id=:{type_ident,type_index={glob_object,glob_module}} type_args type_heaps
	# {td_ident,td_rhs,td_args,td_attribute} = defs.[glob_module].com_type_defs.[glob_object]
	= case td_rhs of
		SynType {at_type}
			# (expanded_type, type_heaps) = substituteType td_attribute TA_Multi td_args type_args at_type type_heaps
			-> (True, expanded_type, type_heaps) 
		_
			-> (False, type, type_heaps)

class match type ::  !{# CommonDefs} !type !type !*TypeHeaps -> (!Bool, !*TypeHeaps)

instance match AType
where
	match defs atype1 atype2 type_heaps = match defs atype1.at_type atype2.at_type type_heaps

expand_and_match :: TypeSymbIdent [AType] TypeSymbIdent [AType] {#CommonDefs} Type Type *TypeHeaps -> (Bool, *TypeHeaps)
expand_and_match cons_id1 cons_args1 cons_id2 cons_args2 defs type1 type2 type_heaps
	# (succ1, type1, type_heaps) = tryToExpandTypeSyn defs type1 cons_id1 cons_args1 type_heaps
	# (succ2, type2, type_heaps) = tryToExpandTypeSyn defs type2 cons_id2 cons_args2 type_heaps
	| succ1 || succ2
		= match defs type1 type2 type_heaps
/*
	| succ2
	
		= case type2 of
			TA cons_id2 cons_args2
				| cons_id1 == cons_id2
					-> match defs cons_args1 cons_args2 type_heaps
					-> (False, type_heaps)
			_
					-> (False, type_heaps)
	
*/
		= (False, type_heaps)

instance match Type
where
	match defs (TV {tv_info_ptr}) type type_heaps=:{th_vars}
		= (True, { type_heaps & th_vars = th_vars <:= (tv_info_ptr,TVI_Type type)})
	match defs type1=:(TA cons_id1 cons_args1) type2=:(TA cons_id2 cons_args2) type_heaps
		| cons_id1 == cons_id2
			= match defs cons_args1 cons_args2 type_heaps
			= expand_and_match cons_id1 cons_args1 cons_id2 cons_args2 defs type1 type2 type_heaps
	match defs type1=:(TA cons_id1 cons_args1) type2=:(TAS cons_id2 cons_args2 _) type_heaps
		| cons_id1 == cons_id2
			= match defs cons_args1 cons_args2 type_heaps
			= expand_and_match cons_id1 cons_args1 cons_id2 cons_args2 defs type1 type2 type_heaps
	match defs type1=:(TAS cons_id1 cons_args1 _) type2=:(TA cons_id2 cons_args2) type_heaps
		| cons_id1 == cons_id2
			= match defs cons_args1 cons_args2 type_heaps
			= expand_and_match cons_id1 cons_args1 cons_id2 cons_args2 defs type1 type2 type_heaps
	match defs type1=:(TAS cons_id1 cons_args1 _) type2=:(TAS cons_id2 cons_args2 _) type_heaps
		| cons_id1 == cons_id2
			= match defs cons_args1 cons_args2 type_heaps
			= expand_and_match cons_id1 cons_args1 cons_id2 cons_args2 defs type1 type2 type_heaps
	match defs (arg_type1 --> res_type1) (arg_type2 --> res_type2) type_heaps
		= match defs (arg_type1,res_type1) (arg_type2,res_type2) type_heaps
	match defs (type1 :@: types1) (type2 :@: types2) type_heaps
		= match defs (type1,types1) (type2,types2) type_heaps
	match defs (CV tv :@: types) (TA type_cons cons_args) type_heaps
		# diff = type_cons.type_arity - length types
		| diff >= 0
			= match defs (TV tv, types) (TA { type_cons & type_arity = diff } (take diff cons_args), drop diff cons_args) type_heaps
			= (False, type_heaps)
	match defs (CV tv :@: types) (TAS type_cons cons_args _) type_heaps
		# diff = type_cons.type_arity - length types
		| diff >= 0
			= match defs (TV tv, types) (TA { type_cons & type_arity = diff } (take diff cons_args), drop diff cons_args) type_heaps
			= (False, type_heaps)
	match defs (TB tb1) (TB tb2) type_heaps
		= (tb1 == tb2, type_heaps)
	match defs TArrow TArrow type_heaps
		= (True, type_heaps)
	match defs (TArrow1 t1) (TArrow1 t2) type_heaps
		= match defs t1 t2 type_heaps
	match defs type1=:(TA cons_id cons_args) type2 type_heaps
		# (succ, type1, type_heaps) = tryToExpandTypeSyn defs type1 cons_id cons_args type_heaps
		| succ
			= match defs type1 type2 type_heaps
			= (False, type_heaps)
	match defs type1=:(TAS cons_id cons_args _) type2 type_heaps
		# (succ, type1, type_heaps) = tryToExpandTypeSyn defs type1 cons_id cons_args type_heaps
		| succ
			= match defs type1 type2 type_heaps
			= (False, type_heaps)
	match defs type1 type2=:(TA cons_id cons_args) type_heaps
		# (succ, type2, type_heaps) = tryToExpandTypeSyn defs type2 cons_id cons_args type_heaps
		| succ
			= match defs type1 type2 type_heaps
			= (False, type_heaps)
	match defs type1 type2=:(TAS cons_id cons_args _) type_heaps
		# (succ, type2, type_heaps) = tryToExpandTypeSyn defs type2 cons_id cons_args type_heaps
		| succ
			= match defs type1 type2 type_heaps
			= (False, type_heaps)
	match defs type1 type2 type_heaps
		= (False, type_heaps)

instance match (!a,!b) | match a & match b
where
	match defs (x1,y1) (x2,y2) type_heaps
		# (matched, type_heaps) = match defs x1 x2 type_heaps
		| matched
			= match defs y1 y2 type_heaps
			= (False, type_heaps)
			
instance match [a] | match a
where
	match defs [t1 : ts1] [t2 : ts2] type_heaps
		= match defs (t1,ts1) (t2,ts2) type_heaps
	match defs [] [] type_heaps
		= (True, type_heaps)
	match defs _ _ type_heaps // in case of a kind error
		= (False, type_heaps)

instance match ConsVariable
where
	match defs (CV {tv_info_ptr}) cons_var type_heaps=:{th_vars}
		= (True, { type_heaps & th_vars = th_vars <:= (tv_info_ptr,TVI_Type (consVariableToType cons_var))})

consVariableToType (TempCV temp_var_id)
	= TempV temp_var_id
consVariableToType (TempQCV temp_var_id)
	= TempQV temp_var_id
consVariableToType (TempQCDV temp_var_id)
	= TempQDV temp_var_id

trySpecializedInstances :: [TypeContext] [Special] *TypeHeaps -> (!Global Index,!*TypeHeaps)
trySpecializedInstances type_contexts [] type_heaps
	= (ObjectNotFound, type_heaps)
trySpecializedInstances type_contexts specials type_heaps=:{th_vars}
	# (spec_index, th_vars) = try_specialized_instances (map (\{tc_types} -> tc_types) type_contexts) specials th_vars
	= (spec_index, { type_heaps & th_vars = th_vars })
where
	try_specialized_instances :: [[Type]] [Special] *TypeVarHeap -> (!Global Index,!*TypeVarHeap)
	try_specialized_instances type_contexts_types [{spec_index,spec_vars,spec_types} : specials] type_var_heap
		# type_var_heap = foldSt (\tv -> writePtr tv.tv_info_ptr TVI_Empty) spec_vars type_var_heap
		  (equ, type_var_heap) = specialized_context_matches /*equalTypes*/ spec_types type_contexts_types type_var_heap
		| equ
			= (spec_index, type_var_heap)
			= try_specialized_instances type_contexts_types specials type_var_heap
	try_specialized_instances type_contexts_types [] type_var_heap
		= (ObjectNotFound, type_var_heap)

	specialized_context_matches :: [[Type]] ![[Type]] *TypeVarHeap -> (!.Bool,!.TypeVarHeap);
	specialized_context_matches [spec_context_types:spec_contexts_types] [type_context_types:type_contexts_types] type_var_heap
		# (equal,type_var_heap) = specialized_types_in_context_match spec_context_types type_context_types type_var_heap;
		|  equal
			= specialized_context_matches spec_contexts_types type_contexts_types type_var_heap
			= (False,type_var_heap);
	specialized_context_matches [] [] type_var_heap
		= (True,type_var_heap);
	specialized_context_matches _ _ type_var_heap
		= (False,type_var_heap);

	specialized_types_in_context_match :: [Type] ![Type] *TypeVarHeap -> (!.Bool,!.TypeVarHeap);
	specialized_types_in_context_match [TV _:spec_context_types] [_:type_context_types] type_var_heap
		// special case for type var in lazy or strict Array or List context
		// only these typevars are accepted by function checkAndCollectTypesOfContextsOfSpecials in check
		= specialized_types_in_context_match spec_context_types type_context_types type_var_heap
	specialized_types_in_context_match [spec_context_type:spec_context_types] [type_context_type:type_context_types] type_var_heap
		# (equal,type_var_heap) = equalTypes spec_context_type type_context_type type_var_heap;
		|  equal
			= specialized_types_in_context_match spec_context_types type_context_types type_var_heap
			= (False,type_var_heap);
	specialized_types_in_context_match [] [] type_var_heap
		= (True,type_var_heap);
	specialized_types_in_context_match _ _ type_var_heap
		= (False,type_var_heap);

tryToSolveOverloading :: ![(Optional [TypeContext], [ExprInfoPtr], IdentPos, Index)] !Int !{# CommonDefs } !ClassInstanceInfo !*Coercions !*OverloadingState !{# DclModule}
	-> (![TypeContext], !*Coercions, ![LocalTypePatternVariable], DictionaryTypes, !*OverloadingState)
tryToSolveOverloading ocs main_dcl_module_n defs instance_info coercion_env os dcl_modules
	# (reduced_calls, contexts, coercion_env, type_pattern_vars, os) = foldSt (reduce_contexts_of_applications_in_function defs instance_info) ocs ([], [], coercion_env, [], os)
	| os.os_error.ea_ok
		# (contexts, os_var_heap) = foldSt add_specified_contexts ocs (contexts,os.os_var_heap)
		  (contexts, os_type_heaps) = remove_super_classes contexts os.os_type_heaps
		  ({hp_var_heap, hp_expression_heap, hp_type_heaps,hp_generic_heap}, dict_types, os_error)
			= foldSt (convert_dictionaries defs contexts) reduced_calls
		  					({hp_var_heap = os_var_heap, hp_expression_heap = os.os_symbol_heap, hp_type_heaps = os_type_heaps,hp_generic_heap=os.os_generic_heap}, [], os.os_error)
		= (contexts, coercion_env, type_pattern_vars, dict_types, {os & os_type_heaps = hp_type_heaps, os_symbol_heap = hp_expression_heap, os_var_heap = hp_var_heap, os_generic_heap = hp_generic_heap, os_error = os_error})
		= ([], coercion_env, type_pattern_vars, [], os)
where
	reduce_contexts_of_applications_in_function :: {#CommonDefs} ClassInstanceInfo (.a, [ExprInfoPtr], .b, Index)
		   ([(SymbIdent,Index,ExprInfoPtr,[ClassApplication])], ![TypeContext], !*Coercions, ![LocalTypePatternVariable], !*OverloadingState)
		-> ([(SymbIdent,Index,ExprInfoPtr,[ClassApplication])], ![TypeContext], !*Coercions, ![LocalTypePatternVariable], !*OverloadingState)
	reduce_contexts_of_applications_in_function defs instance_info (opt_spec_contexts, expr_ptrs, pos, index) state
		= foldSt (reduce_contexts_of_application index defs instance_info) expr_ptrs state

	reduce_contexts_of_application :: !Index !{#CommonDefs} !ClassInstanceInfo  !ExprInfoPtr
				([(SymbIdent,Index,ExprInfoPtr,[ClassApplication])], ![TypeContext], !*Coercions, ![LocalTypePatternVariable], !*OverloadingState)
			 -> ([(SymbIdent,Index,ExprInfoPtr,[ClassApplication])], ![TypeContext], !*Coercions, ![LocalTypePatternVariable], !*OverloadingState)
	reduce_contexts_of_application fun_index defs instance_info over_info_ptr (reduced_calls, new_contexts, coercion_env, type_pattern_vars,
			os=:{os_symbol_heap,os_type_heaps,os_var_heap,os_special_instances,os_error,os_predef_symbols})
		= case readPtr over_info_ptr os_symbol_heap of
			(EI_Overloaded {oc_symbol,oc_context,oc_specials},os_symbol_heap)
				# (glob_fun, os_type_heaps) = trySpecializedInstances oc_context oc_specials os_type_heaps
				| FoundObject glob_fun
					# over_info = EI_Instance {glob_module = glob_fun.glob_module, glob_object =
												{ds_ident = oc_symbol.symb_ident, ds_arity = 0, ds_index = glob_fun.glob_object}} []
					# os_symbol_heap = os_symbol_heap <:= (over_info_ptr,over_info)
					-> (reduced_calls,new_contexts,coercion_env,type_pattern_vars,{os & os_type_heaps=os_type_heaps, os_symbol_heap=os_symbol_heap})
				| otherwise
					# rs_state = {rs_new_contexts=new_contexts, rs_special_instances = os_special_instances,
								  rs_type_pattern_vars=type_pattern_vars,rs_var_heap=os_var_heap, 
								  rs_type_heaps=os_type_heaps, rs_coercions=coercion_env,
								  rs_predef_symbols=os_predef_symbols, rs_error=os_error}
					  info = {ri_main_dcl_module_n=main_dcl_module_n, ri_defs=defs, ri_instance_info=instance_info}
					  (class_applications, rs_state) = reduceContexts info oc_context rs_state
					  {rs_new_contexts=new_contexts, rs_special_instances = os_special_instances,
					   rs_type_pattern_vars=type_pattern_vars, rs_var_heap=os_var_heap, rs_type_heaps=os_type_heaps,
					   rs_coercions=coercion_env, rs_predef_symbols=os_predef_symbols, rs_error=os_error}
							= rs_state
					  os = {os & os_type_heaps=os_type_heaps, os_symbol_heap=os_symbol_heap, os_var_heap=os_var_heap,
								 os_special_instances=os_special_instances, os_error=os_error, os_predef_symbols=os_predef_symbols}
					-> ([(oc_symbol,fun_index,over_info_ptr,class_applications):reduced_calls],new_contexts,coercion_env,type_pattern_vars,os)
			(EI_OverloadedWithVarContexts {ocvc_symbol,ocvc_context,ocvc_var_contexts},os_symbol_heap)
				# rs_state 	= { rs_new_contexts=new_contexts, rs_special_instances = os_special_instances,
								rs_type_pattern_vars=type_pattern_vars,rs_var_heap=os_var_heap, 
								rs_type_heaps=os_type_heaps, rs_coercions=coercion_env,
								rs_predef_symbols=os_predef_symbols, rs_error=os_error}
				  info = {ri_main_dcl_module_n=main_dcl_module_n, ri_defs=defs, ri_instance_info=instance_info}
				  (class_applications, rs_state) = reduceContexts info ocvc_context rs_state
				  {rs_new_contexts=new_contexts, rs_special_instances = os_special_instances,
				   rs_type_pattern_vars=type_pattern_vars, rs_var_heap=os_var_heap, rs_type_heaps=os_type_heaps,
				   rs_coercions=coercion_env, rs_predef_symbols=os_predef_symbols, rs_error=os_error}
						= rs_state
				  (new_contexts,os_var_heap) = add_var_contexts ocvc_var_contexts new_contexts os_var_heap
				  os = {os & os_type_heaps=os_type_heaps, os_symbol_heap=os_symbol_heap, os_var_heap=os_var_heap,
							 os_special_instances=os_special_instances, os_error=os_error, os_predef_symbols=os_predef_symbols}
				  ocvc_symbol = {ocvc_symbol & symb_kind = case ocvc_symbol.symb_kind of
															SK_TypeCode
																-> SK_TypeCodeAndContexts ocvc_var_contexts
				  											_
					  											-> SK_VarContexts ocvc_var_contexts
					  			 }
				-> ([(ocvc_symbol,fun_index,over_info_ptr,class_applications):reduced_calls],new_contexts,coercion_env,type_pattern_vars,os)
			(EI_CaseTypeWithContexts case_type constructor_contexts,os_symbol_heap)
				# (new_contexts,constructor_contexts,os_predef_symbols,os_var_heap) = add_constructor_contexts constructor_contexts new_contexts os_predef_symbols os_var_heap
				  os_symbol_heap = writePtr over_info_ptr (EI_CaseTypeWithContexts case_type constructor_contexts) os_symbol_heap
				  os = {os & os_symbol_heap=os_symbol_heap,os_var_heap=os_var_heap,os_predef_symbols=os_predef_symbols}
				-> (reduced_calls,new_contexts,coercion_env,type_pattern_vars,os)
	where
		add_var_contexts NoVarContexts new_contexts var_heap
			= (new_contexts,var_heap)
		add_var_contexts (VarContext arg_n contexts arg_atype var_contexts) new_contexts var_heap
			# (new_contexts,var_heap) = add_contexts contexts new_contexts var_heap
			= add_var_contexts var_contexts new_contexts var_heap

		add_constructor_contexts [(constructor_symbol,constructor_context):constructor_contexts] new_contexts predef_symbols var_heap
			# (new_contexts,constructor_context,predef_symbols,var_heap) = add_contexts_of_constructor constructor_context new_contexts predef_symbols var_heap
			# (new_contexts,constructor_contexts,predef_symbols,var_heap) = add_constructor_contexts constructor_contexts new_contexts predef_symbols var_heap
			= (new_contexts,[(constructor_symbol,constructor_context):constructor_contexts],predef_symbols,var_heap)
		add_constructor_contexts [] new_contexts predef_symbols var_heap
			= (new_contexts,[],predef_symbols,var_heap)

		add_contexts_of_constructor [constructor_context:constructor_contexts] new_contexts predef_symbols var_heap
			| context_is_reducible constructor_context predef_symbols
				# (new_contexts,constructor_contexts,predef_symbols,var_heap)
					= add_contexts_of_constructor constructor_contexts new_contexts predef_symbols var_heap
				= (new_contexts,[constructor_context:constructor_contexts],predef_symbols,var_heap)
			# (found,found_context=:{tc_var}) = lookup_context constructor_context new_contexts
			| found
				# var_heap
					= case readPtr tc_var var_heap of
						(VI_Empty,var_heap)
							-> writePtr tc_var VI_EmptyConstructorClassVar var_heap
						(VI_EmptyConstructorClassVar,var_heap)
							-> var_heap
				  (new_contexts,constructor_contexts,predef_symbols,var_heap)
					= add_contexts_of_constructor constructor_contexts new_contexts predef_symbols var_heap
				  constructor_context = {constructor_context & tc_var=tc_var}
				= (new_contexts,[constructor_context:constructor_contexts],predef_symbols,var_heap)
				# var_heap
					= case readPtr constructor_context.tc_var var_heap of
						(VI_Empty,var_heap)
							-> writePtr constructor_context.tc_var VI_EmptyConstructorClassVar var_heap
						(VI_EmptyConstructorClassVar,var_heap)
							-> var_heap
				  new_contexts = [constructor_context : new_contexts]
				  (new_contexts,constructor_contexts,predef_symbols,var_heap)
					= add_contexts_of_constructor constructor_contexts new_contexts predef_symbols var_heap
				= (new_contexts,[constructor_context:constructor_contexts],predef_symbols,var_heap)
			where
				lookup_context :: !TypeContext ![TypeContext] -> (!Bool,!TypeContext)
				lookup_context new_tc [tc : tcs]
					| new_tc==tc
						= (True,tc)
						= lookup_context new_tc tcs
				lookup_context new_tc []
					= (False,new_tc)
		add_contexts_of_constructor [] new_contexts predef_symbols var_heap
			= (new_contexts,[],predef_symbols,var_heap)

	add_specified_contexts (Yes spec_context, expr_ptrs, pos, index) (contexts,var_heap)
		= add_contexts spec_context contexts var_heap
	add_specified_contexts (No, expr_ptrs, pos, index) (contexts,var_heap)
		= (contexts,var_heap)

	add_contexts contexts all_contexts var_heap
		= foldSt add_spec_context contexts (all_contexts,var_heap)
	where
		add_spec_context tc (contexts, var_heap)
			| containsContext tc contexts
				= (contexts, var_heap)
			  	# (tc_var,var_heap) = newPtr VI_Empty var_heap
				= ([{tc & tc_var = tc_var} : contexts], var_heap)

	remove_super_classes contexts type_heaps
		# (super_classes, type_heaps) = foldSt generate_super_classes contexts ([], type_heaps)
		  sub_classes = foldSt (remove_doubles super_classes) contexts []
		= (sub_classes, type_heaps)

	generate_super_classes tc=:{tc_class=TCGeneric {gtc_class}} st
		= generate_super_classes {tc & tc_class=TCClass gtc_class} st
	generate_super_classes {tc_class=TCClass {glob_object={ds_index},glob_module},tc_types} (super_classes, type_heaps)
		# {class_args,class_members,class_context} = defs.[glob_module].com_class_defs.[ds_index]
		  th_vars = fold2St set_type class_args tc_types type_heaps.th_vars
		= foldSt subst_context_and_generate_super_classes class_context (super_classes, { type_heaps & th_vars = th_vars })
	where
		set_type {tv_info_ptr} type type_var_heap
			= type_var_heap <:= (tv_info_ptr, TVI_Type type)
		  
		subst_context_and_generate_super_classes class_context (super_classes, type_heaps)
			# (_, super_class, type_heaps) = substitute class_context type_heaps
			| containsContext super_class super_classes
				= (super_classes, type_heaps)
				= generate_super_classes super_class ([super_class : super_classes], type_heaps) 

	remove_doubles sub_classes tc context
		| containsContext tc sub_classes
			= context
			= [tc : context]

	convert_dictionaries :: !{#CommonDefs} ![TypeContext] !(!SymbIdent,!Index,!ExprInfoPtr,![ClassApplication]) !(!*Heaps,!DictionaryTypes, !*ErrorAdmin) -> (!*Heaps,!DictionaryTypes, !*ErrorAdmin)
	convert_dictionaries defs contexts (oc_symbol,index,over_info_ptr,class_applications) (heaps, dict_types, error)
		# (heaps, ptrs, error) = convertOverloadedCall defs contexts oc_symbol over_info_ptr class_applications (heaps, [], error)
		| isEmpty ptrs
			= (heaps, dict_types, error)
			= (heaps, add_to_dict_types index ptrs dict_types, error)
	
	add_to_dict_types index ptrs []
		= [(index, ptrs)]
	add_to_dict_types new_index new_ptrs dt=:[(index, ptrs) : dict_types]
		| new_index == index
			= [(index, new_ptrs ++ ptrs) : dict_types]
			= [(new_index, new_ptrs) : dt]
	
selectFromDictionary dict_mod dict_index member_index defs
	# (RecordType {rt_fields}) = defs.[dict_mod].com_type_defs.[dict_index].td_rhs
	  { fs_ident, fs_index } = rt_fields.[member_index]
	= { glob_module = dict_mod, glob_object = { ds_ident = fs_ident, ds_index = fs_index, ds_arity = 1 }}

getDictionaryTypeAndConstructor :: !GlobalIndex !{#CommonDefs} -> (!DefinedSymbol,!DefinedSymbol)
getDictionaryTypeAndConstructor {gi_module,gi_index} defs	  
	# {class_dictionary} = defs.[gi_module].com_class_defs.[gi_index]
	  (RecordType {rt_constructor}) = defs.[gi_module].com_type_defs.[class_dictionary.ds_index].td_rhs
	= (class_dictionary, rt_constructor)

AttributedType type :== { at_attribute = TA_Multi, at_type = type }

convertOverloadedCall :: !{#CommonDefs} ![TypeContext] !SymbIdent !ExprInfoPtr ![ClassApplication] !(!*Heaps, ![ExprInfoPtr],!*ErrorAdmin) -> (!*Heaps, ![ExprInfoPtr],!*ErrorAdmin)
convertOverloadedCall defs contexts {symb_ident,symb_kind = SK_OverloadedFunction {glob_module,glob_object}} expr_ptr [class_appl:class_appls] (heaps,ptrs,error)
	# mem_def = defs.[glob_module].com_member_defs.[glob_object]
	  (class_exprs, heaps_and_ptrs) = convertClassApplsToExpressions defs contexts class_appls (heaps, ptrs)
	  (inst_expr, (heaps, ptrs)) = adjust_member_application defs contexts mem_def class_appl class_exprs heaps_and_ptrs
	= ({heaps & hp_expression_heap = heaps.hp_expression_heap <:= (expr_ptr, inst_expr)}, ptrs, error)
where
	adjust_member_application defs contexts {me_ident,me_offset,me_class} (CA_Instance red_contexts) class_exprs heaps_and_ptrs
		# (glob_module,cim_index,cim_ident,red_contexts_appls) = find_instance_of_member me_class me_offset red_contexts
		#! (exprs, heaps_and_ptrs) = convertClassApplsToExpressions defs contexts red_contexts_appls heaps_and_ptrs
           class_exprs = exprs ++ class_exprs
           n_class_exprs = length class_exprs
        | cim_index>=0
        	= (EI_Instance {glob_module=glob_module, glob_object={ds_ident=me_ident, ds_arity=n_class_exprs, ds_index=cim_index}} class_exprs,
                                heaps_and_ptrs)
            # index = -1 - cim_index
            = (EI_Instance {glob_module=glob_module, glob_object={ds_ident=cim_ident, ds_arity=n_class_exprs, ds_index=index}} class_exprs,
                                heaps_and_ptrs)
	adjust_member_application defs contexts {me_offset,me_class={glob_module,glob_object}} (CA_Context tc) class_exprs (heaps=:{hp_type_heaps}, ptrs)
		# (class_context, address, hp_type_heaps) = determineContextAddress contexts defs tc hp_type_heaps
		# {class_dictionary={ds_index,ds_ident}} = defs.[glob_module].com_class_defs.[glob_object]
		  selector = selectFromDictionary glob_module ds_index me_offset defs
		= (EI_Selection (generateClassSelection address [RecordSelection selector me_offset]) class_context.tc_var class_exprs,
				({ heaps & hp_type_heaps = hp_type_heaps }, ptrs))
	adjust_member_application defs contexts  _ (CA_GlobalTypeCode {tci_constructor,tci_contexts}) _ heaps_and_ptrs
		# (exprs, heaps_and_ptrs) = convertClassApplsToExpressions defs contexts tci_contexts heaps_and_ptrs
		  typeCodeExpressions = expressionsToTypeCodeExpressions exprs
		= case tci_constructor of
			GTT_Constructor _ True
				-> (EI_TypeCode (TCE_UnqType (TCE_Constructor tci_constructor typeCodeExpressions)), heaps_and_ptrs)
			_
				-> (EI_TypeCode (TCE_Constructor tci_constructor typeCodeExpressions), heaps_and_ptrs)
	adjust_member_application defs contexts _ (CA_LocalTypeCode new_var_ptr) _  heaps_and_ptrs
		= (EI_TypeCode (TCE_Var new_var_ptr), heaps_and_ptrs)

	find_instance_of_member :: (Global Int) Int ReducedContexts -> (!Index,!Index,!Ident,[ClassApplication])
	find_instance_of_member me_class me_offset { rcs_class_context = {rc_class_index, rc_inst_module, rc_inst_members, rc_red_contexts}, rcs_constraints_contexts}
    	| rc_class_index.gi_module == me_class.glob_module && rc_class_index.gi_index == me_class.glob_object
        	# {cim_index,cim_arity,cim_ident} = rc_inst_members.[me_offset]
			| cim_index<0
				= (cim_arity, cim_index, cim_ident, rc_red_contexts)
				= (rc_inst_module, cim_index, cim_ident, rc_red_contexts)
			= find_instance_of_member_in_constraints me_class me_offset rcs_constraints_contexts
	where
		find_instance_of_member_in_constraints me_class me_offset [ CA_Instance rcs=:{rcs_constraints_contexts} : rcss ]
			= find_instance_of_member me_class me_offset {rcs & rcs_constraints_contexts = rcs_constraints_contexts ++ rcss}
		find_instance_of_member_in_constraints me_class me_offset [ _ : rcss ]
			= find_instance_of_member_in_constraints me_class me_offset rcss
		find_instance_of_member_in_constraints me_class me_offset []
			= abort "Error in module overloading: find_instance_of_member_in_constraints\n"
convertOverloadedCall defs contexts symbol=:{symb_ident, symb_kind = SK_Generic gen_glob kind} expr_ptr class_appls (heaps, expr_info_ptrs, error)
	#! (opt_member_glob, hp_generic_heap) = getGenericMember gen_glob kind defs heaps.hp_generic_heap
	#! heaps = { heaps & hp_generic_heap = hp_generic_heap }
	= case opt_member_glob of		
		No
			# error = checkError ("no generic instances of " +++ toString symb_ident +++ " for kind") kind error
			-> (heaps, expr_info_ptrs, error)
		Yes member_glob -> convertOverloadedCall defs contexts {symbol & symb_kind = SK_OverloadedFunction member_glob} expr_ptr class_appls (heaps, expr_info_ptrs, error)
convertOverloadedCall defs contexts {symb_ident,symb_kind = SK_TypeCode} expr_info_ptr class_appls (heaps, ptrs, error)
	# (class_expressions, (heaps, ptrs)) = convertClassApplsToExpressions defs contexts class_appls (heaps, ptrs)
	= ({heaps & hp_expression_heap = heaps.hp_expression_heap <:= (expr_info_ptr, EI_TypeCodes (map expressionToTypeCodeExpression class_expressions))}, ptrs, error)
convertOverloadedCall defs contexts {symb_kind=SK_TFACVar var_expr_ptr,symb_ident} expr_info_ptr appls (heaps,ptrs, error)
	# (class_expressions, (heaps, ptrs)) = convertClassApplsToExpressions defs contexts appls (heaps,ptrs)
	= ({heaps & hp_expression_heap = heaps.hp_expression_heap <:= (expr_info_ptr, EI_FPContext class_expressions var_expr_ptr)}, ptrs, error)
convertOverloadedCall defs contexts {symb_kind=SK_VarContexts var_contexts} expr_info_ptr appls (heaps,ptrs, error)
	# (var_contexts,error) = get_var_contexts var_contexts defs contexts error
	  (class_expressions, (heaps, ptrs)) = convertClassApplsToExpressions defs contexts appls (heaps,ptrs)
	  expr_info = EI_ContextWithVarContexts class_expressions var_contexts
	= ({heaps & hp_expression_heap = writePtr expr_info_ptr expr_info heaps.hp_expression_heap}, [expr_info_ptr:ptrs], error)
convertOverloadedCall defs contexts {symb_ident,symb_kind = SK_TypeCodeAndContexts univ_contexts} expr_info_ptr class_appls (heaps, ptrs, error)
	# (univ_contexts,error) = get_var_contexts univ_contexts defs contexts error
	  (class_expressions, (heaps, ptrs)) = convertClassApplsToExpressions defs contexts class_appls (heaps, ptrs)
	  expr_info = EI_TypeCodesWithContexts (expressionsToTypeCodeExpressions class_expressions) univ_contexts
	= ({heaps & hp_expression_heap = writePtr expr_info_ptr expr_info heaps.hp_expression_heap}, ptrs, error)
convertOverloadedCall defs contexts symbol expr_info_ptr appls (heaps,ptrs, error)
	# (class_expressions, (heaps, ptrs)) = convertClassApplsToExpressions defs contexts appls (heaps,ptrs)
	= ({heaps & hp_expression_heap = heaps.hp_expression_heap <:= (expr_info_ptr, EI_Context class_expressions)}, ptrs, error)

expressionsToTypeCodeExpressions class_expressions
	= map expressionToTypeCodeExpression class_expressions

get_var_contexts (VarContext arg_n context arg_atype var_contexts) defs contexts error
	# (cs,error) = get_var_context context contexts error 
	  cs = [convert_TypeContext_to_DictionaryAndClassType c defs \\ c <- cs]
	  (var_contexts,error) = get_var_contexts var_contexts defs contexts error
 	= (VarContext arg_n cs arg_atype var_contexts,error)
where
	get_var_context [] contexts error
		= ([],error)
	get_var_context [var_context:var_contexts] contexts error
		# (var_contexts,error) = get_var_context var_contexts contexts error
		= get_context var_context var_contexts contexts error

	get_context context var_contexts [c:cs] error
		| context==c
			= ([c:var_contexts],error)
			= get_context context var_contexts cs error
	get_context {tc_class=TCClass {glob_object={ds_ident}}} var_contexts [] error
		# error = sub_class_error ds_ident error
		= (var_contexts,error)

	convert_TypeContext_to_DictionaryAndClassType {tc_var,tc_class=TCClass {glob_module,glob_object={ds_ident,ds_index}},tc_types} defs
		# {class_dictionary} = defs.[glob_module].com_class_defs.[ds_index]
		  dict_type_symbol = MakeTypeSymbIdent {glob_module=glob_module,glob_object=class_dictionary.ds_index} class_dictionary.ds_ident class_dictionary.ds_arity
		  class_type = TA dict_type_symbol [AttributedType type \\ type <- tc_types]
		= {dc_var=tc_var,dc_class_type=AttributedType class_type}
get_var_contexts NoVarContexts defs contexts error
	= (NoVarContexts,error)

expressionToTypeCodeExpression (TypeCodeExpression texpr)
	= texpr
expressionToTypeCodeExpression (ClassVariable var_info_ptr)
	= TCE_TypeTerm var_info_ptr
expressionToTypeCodeExpression (Selection NormalSelector (ClassVariable var_info_ptr) selectors)
	= TCE_Selector selectors var_info_ptr
expressionToTypeCodeExpression expr
	= abort "expressionToTypeCodeExpression (overloading.icl)"

generateClassSelection address last_selectors
	= mapAppend (\(off_set,selector) -> RecordSelection selector off_set) address last_selectors

instance toString ClassApplication
where 
	toString (CA_Instance _)		= abort "CA_Instance"
	toString (CA_Context _)			= abort "CA_Context"
	toString (CA_LocalTypeCode _)	= abort "CA_LocalTypeCode"
	toString (CA_GlobalTypeCode _) 	= abort "CA_GlobalTypeCode"

convertClassApplsToExpressions :: {#CommonDefs} [TypeContext] [ClassApplication] *( *Heaps, [ExprInfoPtr])
															-> *(![Expression], !*(!*Heaps,![ExprInfoPtr]))
convertClassApplsToExpressions defs contexts cl_appls heaps_and_ptrs
	= mapSt (convert_class_appl_to_expression defs contexts) cl_appls heaps_and_ptrs
where
	convert_class_appl_to_expression defs contexts (CA_Instance rcs) heaps_and_ptrs
		= convert_reduced_contexts_to_expression defs contexts rcs heaps_and_ptrs
	convert_class_appl_to_expression defs contexts (CA_Context tc) (heaps=:{hp_type_heaps}, ptrs)
		# (class_context, context_address, hp_type_heaps) = determineContextAddress contexts defs tc hp_type_heaps
		| isEmpty context_address
			= (ClassVariable class_context.tc_var, ({heaps & hp_type_heaps=hp_type_heaps}, ptrs))
			= (Selection NormalSelector (ClassVariable class_context.tc_var) (generateClassSelection context_address []), ({heaps & hp_type_heaps = hp_type_heaps}, ptrs))
	convert_class_appl_to_expression defs contexts (CA_LocalTypeCode new_var_ptr) heaps_and_ptrs
		= (TypeCodeExpression (TCE_Var new_var_ptr), heaps_and_ptrs)
	convert_class_appl_to_expression defs contexts (CA_GlobalTypeCode {tci_constructor,tci_contexts}) heaps_and_ptrs
		# (exprs, heaps_and_ptrs) = convertClassApplsToExpressions defs contexts tci_contexts heaps_and_ptrs
		  typeCodeExpressions = expressionsToTypeCodeExpressions exprs
		= case tci_constructor of
			GTT_Constructor _ True
				-> (TypeCodeExpression (TCE_UnqType (TCE_Constructor tci_constructor typeCodeExpressions)), heaps_and_ptrs)
			_
				-> (TypeCodeExpression (TCE_Constructor tci_constructor typeCodeExpressions), heaps_and_ptrs)

	convert_reduced_contexts_to_expression defs contexts {rcs_class_context,rcs_constraints_contexts} heaps_and_ptrs
		# (rcs_exprs, heaps_and_ptrs) = mapSt (convert_class_appl_to_expression defs contexts) rcs_constraints_contexts heaps_and_ptrs
		= convert_reduced_context_to_expression defs contexts rcs_class_context rcs_exprs heaps_and_ptrs
	where
		convert_reduced_context_to_expression :: {#CommonDefs} [TypeContext] ReducedContext [Expression] *(*Heaps,[Ptr ExprInfo]) -> *(Expression,*(*Heaps,[Ptr ExprInfo]))
		convert_reduced_context_to_expression defs contexts {rc_class_index, rc_inst_module, rc_inst_members, rc_red_contexts, rc_types} dictionary_args heaps_and_ptrs
			# (expressions, (heaps, class_ptrs)) = convertClassApplsToExpressions defs contexts rc_red_contexts heaps_and_ptrs
			  context_size = length expressions
			| (size rc_inst_members > 2 && context_size > 0) || (size rc_inst_members==2 && (context_size>1 || not (is_small_context expressions)))
				# (let_binds, let_types, rev_dicts, hp_var_heap, hp_expression_heap)
						= foldSt (bind_shared_dictionary (size rc_inst_members)) expressions ([], [], [], heaps.hp_var_heap, heaps.hp_expression_heap)
				  dictionary_args = build_class_members (size rc_inst_members) rc_inst_members rc_inst_module (reverse rev_dicts) context_size dictionary_args
				  (dict_expr, hp_expression_heap, class_ptrs) = build_dictionary rc_class_index rc_types dictionary_args defs hp_expression_heap class_ptrs
				| isEmpty let_binds
					= (dict_expr, ({ heaps & hp_var_heap = hp_var_heap, hp_expression_heap = hp_expression_heap }, class_ptrs))
					# (let_info_ptr, hp_expression_heap) = newPtr (EI_LetType let_types) hp_expression_heap
					= (Let { let_strict_binds = [], let_lazy_binds = let_binds, let_expr = dict_expr, let_info_ptr = let_info_ptr, let_expr_position = NoPos },
						({ heaps & hp_var_heap = hp_var_heap, hp_expression_heap = hp_expression_heap }, [let_info_ptr : class_ptrs]))
				# dictionary_args = build_class_members (size rc_inst_members) rc_inst_members rc_inst_module expressions context_size dictionary_args
				  (dict_expr, hp_expression_heap, class_ptrs) = build_dictionary rc_class_index rc_types dictionary_args defs heaps.hp_expression_heap class_ptrs
				= (dict_expr, ({ heaps & hp_expression_heap = hp_expression_heap }, class_ptrs))

		is_small_context [] = True;
		is_small_context [App {app_args}] = contains_no_dictionaries app_args;
			where
				contains_no_dictionaries [] = True
				contains_no_dictionaries [App {app_args=[]}:args] = contains_no_dictionaries args
				contains_no_dictionaries [ClassVariable _:args] = contains_no_dictionaries args
				contains_no_dictionaries [Selection _ (ClassVariable _) _:args] = contains_no_dictionaries args
				contains_no_dictionaries l = False // <<- ("contains_no_dictionaries",l);
		is_small_context [ClassVariable _] = True;
		is_small_context l = False // <<- ("is_small_context",l);

		build_class_members mem_offset ins_members mod_index class_arguments arity dictionary_args
			| mem_offset == 0
				= dictionary_args
				# mem_offset = dec mem_offset
				  {cim_ident,cim_index,cim_arity} = ins_members.[mem_offset]
				| cim_index<0
					# mem_expr =  App { app_symb = { symb_ident = cim_ident,
						  							 symb_kind = SK_Function {glob_object = -1 - cim_index, glob_module = cim_arity} },
										app_args = class_arguments,
										app_info_ptr = nilPtr }
					= build_class_members mem_offset ins_members mod_index class_arguments arity [mem_expr : dictionary_args]
					# mem_expr =  App { app_symb = { symb_ident = cim_ident,
						  							 symb_kind = SK_Function {glob_object = cim_index, glob_module = mod_index} },
										app_args = class_arguments,
										app_info_ptr = nilPtr }
					= build_class_members mem_offset ins_members mod_index class_arguments arity [mem_expr : dictionary_args]
		
		build_dictionary class_index instance_types dictionary_args defs expr_heap ptrs
			# (dict_type, dict_cons) = getDictionaryTypeAndConstructor class_index defs
			  record_symbol = { symb_ident = dict_cons.ds_ident,		  
			  					symb_kind = SK_Constructor {glob_module = class_index.gi_module, glob_object = dict_cons.ds_index}
								}
			  dict_type_symbol = MakeTypeSymbIdent {glob_module = class_index.gi_module, glob_object = dict_type.ds_index} dict_type.ds_ident dict_type.ds_arity
			  class_type = TA dict_type_symbol [AttributedType type \\ type <- instance_types]
			  (app_info_ptr, expr_heap) = newPtr (EI_DictionaryType class_type) expr_heap
			  rc_record = App {app_symb = record_symbol, app_args = dictionary_args, app_info_ptr = app_info_ptr}
			= (rc_record, expr_heap, [app_info_ptr : ptrs])

		bind_shared_dictionary nr_of_members dict=:(Let {let_expr=App {app_symb={symb_ident}, app_info_ptr}}) (binds, types, rev_dicts, var_heap, expr_heap)
			# (EI_DictionaryType class_type, expr_heap) = readPtr app_info_ptr expr_heap
		  	  (var_info_ptr, var_heap) = newPtr VI_Empty var_heap
		  	  fv = { fv_ident = symb_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = nr_of_members }
		  	  var = { var_ident = symb_ident, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr }
			= ([{lb_src = dict, lb_dst = fv, lb_position = NoPos } : binds ], [ AttributedType class_type : types ],
				[Var var : rev_dicts], var_heap, expr_heap)
		bind_shared_dictionary nr_of_members dict=:(App {app_symb={symb_ident}, app_info_ptr}) (binds, types, rev_dicts, var_heap, expr_heap)
			# (EI_DictionaryType class_type, expr_heap) = readPtr app_info_ptr expr_heap
		  	  (var_info_ptr, var_heap) = newPtr VI_Empty var_heap
		  	  fv = { fv_ident = symb_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = nr_of_members }
		  	  var = { var_ident = symb_ident, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr }
			= ([{lb_src = dict, lb_dst = fv, lb_position = NoPos} : binds ], [ AttributedType class_type : types ], [Var var : rev_dicts], var_heap, expr_heap)
		bind_shared_dictionary nr_of_members dict (binds, types, rev_dicts, var_heap, expr_heap)
			= (binds, types, [dict : rev_dicts], var_heap, expr_heap)

determineContextAddress :: ![TypeContext] !{#CommonDefs} !TypeContext !*TypeHeaps
	-> (!TypeContext, ![(Int, Global DefinedSymbol)], !*TypeHeaps)
determineContextAddress contexts defs this_context type_heaps
	= look_up_context_and_address this_context contexts defs type_heaps
where
	look_up_context_and_address :: !TypeContext ![TypeContext] !{#CommonDefs} !*TypeHeaps -> (TypeContext, [(Int, Global DefinedSymbol)], !*TypeHeaps)
	look_up_context_and_address this_context [] defs type_heaps
		= abort "look_up_context_and_address (overloading.icl)"
	look_up_context_and_address this_context [tc : tcs] defs type_heaps
		#! (may_be_addres, type_heaps) = determine_address this_context tc [] defs type_heaps
		= case may_be_addres of
			Yes address
				-> (tc, address, type_heaps)
			No
				-> look_up_context_and_address this_context tcs defs type_heaps

	determine_address :: !TypeContext !TypeContext ![(Int, Global DefinedSymbol)] !{#CommonDefs} !*TypeHeaps
		-> (!Optional [(Int, Global DefinedSymbol)],!*TypeHeaps)
	determine_address tc1=:{tc_class=TCGeneric {gtc_class=class1}} tc2=:{tc_class=TCGeneric {gtc_class=class2}} address defs type_heaps
		= determine_address {tc1 & tc_class=TCClass class1} {tc2 & tc_class=TCClass class2} address defs type_heaps
	determine_address tc1=:{tc_class=TCGeneric {gtc_class=class1}} tc2 address defs type_heaps
		= determine_address {tc1 & tc_class=TCClass class1} tc2 address defs type_heaps
	determine_address tc1 tc2=:{tc_class=TCGeneric {gtc_class=class2}} address defs type_heaps
		= determine_address tc1 {tc2 & tc_class=TCClass class2} address defs type_heaps		
	determine_address tc1 tc2 address defs type_heaps=:{th_vars}
		| tc1 == tc2
			= (Yes address, type_heaps)
			# {tc_class=TCClass {glob_object={ds_index},glob_module}} = tc2
			  {class_args,class_members,class_context,class_dictionary} = defs.[glob_module].com_class_defs.[ds_index]
			  th_vars = foldr2 (\{tv_info_ptr} type -> writePtr tv_info_ptr (TVI_Type type)) th_vars class_args tc2.tc_types
			  (_, super_instances, type_heaps) = substitute class_context {type_heaps & th_vars = th_vars} 
			= find_super_instance tc1 super_instances (size class_members) address glob_module class_dictionary.ds_index defs type_heaps
	where
		find_super_instance :: !TypeContext ![TypeContext] !Index ![(Int, Global DefinedSymbol)] !Index !Index !{#CommonDefs} !*TypeHeaps
			-> (!Optional [(Int, Global DefinedSymbol)],!*TypeHeaps)
		find_super_instance context [] tc_index address dict_mod dict_index defs type_heaps
			= (No, type_heaps)
		find_super_instance context [tc : tcs] tc_index address dict_mod dict_index defs type_heaps
			#! (may_be_addres, type_heaps) = determine_address context tc address defs type_heaps
			= case may_be_addres of
				Yes address
					# selector = selectFromDictionary dict_mod dict_index tc_index defs
					-> (Yes [ (tc_index, selector) : address ], type_heaps)
				No
					-> find_super_instance context tcs (inc tc_index) address  dict_mod dict_index defs type_heaps

getClassVariable :: !Ident !VarInfoPtr !*VarHeap !*ErrorAdmin -> (!Ident, !VarInfoPtr, !*VarHeap, !*ErrorAdmin)
getClassVariable symb var_info_ptr var_heap error
	= case (readPtr var_info_ptr var_heap) of
		(VI_ClassVar var_ident new_info_ptr count, var_heap)
			-> (var_ident, new_info_ptr, var_heap <:= (var_info_ptr, VI_ClassVar var_ident new_info_ptr (inc count)), error)
		(_,var_heap)
			# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
			# error = overloadingError symb error
			-> (symb, new_info_ptr, var_heap <:= (var_info_ptr, VI_ClassVar symb new_info_ptr 1), error)

removeOverloadedFunctions :: ![Index] ![LocalTypePatternVariable] !Int !*{#FunDef} !*{! FunctionType} !*ExpressionHeap
	!*TypeCodeInfo !*VarHeap !*ErrorAdmin !*{#PredefinedSymbol}
		-> (!*{#FunDef}, !*{! FunctionType}, !*ExpressionHeap, !*TypeCodeInfo, !*VarHeap, !*ErrorAdmin, !*{#PredefinedSymbol})
removeOverloadedFunctions group type_pattern_vars main_dcl_module_n fun_defs fun_env symbol_heap type_code_info var_heap error predef_symbols
	#! ok = error.ea_ok
	# (_, fun_defs, fun_env, symbol_heap, type_code_info, var_heap, error, predef_symbols)
		= foldSt (remove_overloaded_function type_pattern_vars) group (ok, fun_defs, fun_env, symbol_heap, type_code_info, var_heap, error, predef_symbols)
	= (fun_defs, fun_env, symbol_heap, type_code_info, var_heap, error, predef_symbols)
where
	remove_overloaded_function type_pattern_vars fun_index (ok, fun_defs, fun_env, symbol_heap, type_code_info, var_heap, error, predef_symbols)
		| ok
			# (fun_def, fun_defs) = fun_defs![fun_index]  
			  (CheckedType st=:{st_context,st_args}, fun_env) = fun_env![fun_index]
			  {fun_body = TransformedBody {tb_args,tb_rhs},fun_info,fun_arity,fun_ident,fun_pos} = fun_def
			  var_heap = mark_FPC_arguments st_args tb_args var_heap
			  
			  error = setErrorAdmin (newPosition fun_ident fun_pos) error
			  (rev_variables,var_heap,error) = foldSt determine_class_argument st_context ([],var_heap,error)
			  (type_code_info, symbol_heap, type_pattern_vars, var_heap, error)
			  		= convertDynamicTypes fun_info.fi_dynamics (type_code_info, symbol_heap, type_pattern_vars, var_heap, error)
			 
			  (tb_rhs, ui)
			  		= updateExpression fun_info.fi_group_index tb_rhs {ui_instance_calls = [], ui_local_vars = fun_info.fi_local_vars, ui_symbol_heap = symbol_heap,
			  				ui_var_heap = var_heap, ui_fun_defs = fun_defs, ui_fun_env = fun_env, ui_error = error,
							ui_has_type_codes = False,
						    ui_x = {x_type_code_info=type_code_info, x_predef_symbols=predef_symbols,x_main_dcl_module_n=main_dcl_module_n}}

			#  {ui_instance_calls, ui_local_vars, ui_symbol_heap, ui_var_heap, ui_fun_defs, ui_fun_env, ui_has_type_codes, ui_error, ui_x = {x_type_code_info = type_code_info, x_predef_symbols = predef_symbols}}
				=	ui
			# (tb_args, var_heap) = foldSt retrieve_class_argument rev_variables (tb_args, ui_var_heap)
			  fun_def & fun_body = TransformedBody {tb_args = tb_args, tb_rhs = tb_rhs}, fun_arity = length tb_args,
			 			fun_info = {fun_info & fi_calls = fun_info.fi_calls ++ ui_instance_calls, fi_local_vars = ui_local_vars,
											   fi_properties = if ui_has_type_codes
																	(fun_info.fi_properties bitor FI_HasTypeCodes)
																	fun_info.fi_properties}
			#! ok = ui_error.ea_ok
			= (ok, { ui_fun_defs & [fun_index] = fun_def }, ui_fun_env, ui_symbol_heap, type_code_info, var_heap, ui_error, predef_symbols)
			= (False, fun_defs, fun_env, symbol_heap, type_code_info, var_heap, error, predef_symbols)

	mark_FPC_arguments :: ![AType] ![FreeVar] !*VarHeap -> *VarHeap
	mark_FPC_arguments st_args tb_args var_heap
		| has_TFAC st_args
			= mark_FPC_vars st_args tb_args var_heap
			= var_heap

	determine_class_argument {tc_class, tc_var} (variables,var_heap,error)
		# (var_info, var_heap) = readPtr tc_var var_heap
		= case var_info of
			VI_ForwardClassVar var_info_ptr
				# (var_info, var_heap) = readPtr var_info_ptr var_heap
				-> case var_info of
					VI_Empty
						-> add_class_var var_info_ptr tc_class var_heap error
					VI_EmptyConstructorClassVar
						-> add_class_var var_info_ptr tc_class var_heap error
					VI_ClassVar _ _ _
						# error = errorHeading "Overloading error" error
						  error = {error & ea_file = error.ea_file <<< " a type context occurs multiple times in the specified type\n" }
						-> ([var_info_ptr : variables],var_heap,error)
			VI_Empty
				-> add_class_var tc_var tc_class var_heap error
			VI_EmptyConstructorClassVar
				-> add_class_var tc_var tc_class var_heap error
	where
		add_class_var var tc_class var_heap error
			# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
			  var_heap = writePtr var (VI_ClassVar (build_var_name (toString tc_class)) new_info_ptr 0) var_heap
			= ([var : variables],var_heap,error)

	build_var_name id_name
		= { id_name = "_v" +++ id_name, id_info = nilPtr }

	retrieve_class_argument var_info_ptr (args, var_heap)
		# (VI_ClassVar var_ident new_info_ptr count, var_heap) = readPtr var_info_ptr var_heap
		= ([{fv_ident = var_ident, fv_info_ptr = new_info_ptr, fv_def_level = NotALevel, fv_count = count } : args], var_heap <:= (var_info_ptr, VI_Empty))

has_TFAC [{at_type=TFAC _ _ _}:_] = True
has_TFAC [_:atypes] = has_TFAC atypes
has_TFAC [] = False

mark_FPC_vars [{at_type=TFAC _ _ _}:atypes] [{fv_info_ptr}:args] var_heap
	# var_heap = writePtr fv_info_ptr VI_FPC var_heap
	= mark_FPC_vars atypes args var_heap
mark_FPC_vars [_:atypes] [_:args] var_heap
	= mark_FPC_vars atypes args var_heap
mark_FPC_vars [] [] var_heap
	= var_heap

convertDynamicTypes :: [ExprInfoPtr]
	   *(*TypeCodeInfo,*ExpressionHeap,[LocalTypePatternVariable],*VarHeap,*ErrorAdmin)
	-> *(*TypeCodeInfo,*ExpressionHeap,[LocalTypePatternVariable],*VarHeap,*ErrorAdmin)
convertDynamicTypes dyn_ptrs update_info
	= foldSt update_dynamic dyn_ptrs update_info
where
	update_dynamic dyn_ptr (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
		# (dyn_info, expr_heap) = readPtr dyn_ptr expr_heap 
		= case dyn_info of
			EI_TempDynamicType (Yes {dt_global_vars,dt_uni_vars,dt_type,dt_contexts}) loc_dynamics _ _ _ expr_ptr {symb_ident}
				# (expr_info, expr_heap) = readPtr expr_ptr expr_heap
				-> case expr_info of
					EI_TypeCodes type_codes
						# (type_var_heap, var_heap, error)
								= bind_type_vars_to_type_codes symb_ident dt_global_vars type_codes type_code_info.tci_type_var_heap var_heap error
						  (uni_vars, (type_var_heap, var_heap)) = newTypeVariables dt_uni_vars (type_var_heap, var_heap)
						  (type_code_expr, (type_code_info,var_heap,error)) = toTypeCodeExpression (add_universal_vars_to_type dt_uni_vars dt_type)
						  				({ type_code_info & tci_type_var_heap = type_var_heap }, var_heap, error)
						  expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamic type_code_expr)
						-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
					EI_Empty
						# (uni_vars, (type_var_heap, var_heap)) = newTypeVariables dt_uni_vars (type_code_info.tci_type_var_heap, var_heap)
						  (type_code_expr, (type_code_info,var_heap,error)) = toTypeCodeExpression (add_universal_vars_to_type dt_uni_vars dt_type)
						  			({ type_code_info & tci_type_var_heap = type_var_heap }, var_heap, error)
						  expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamic type_code_expr)
						-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
					EI_TypeCodesWithContexts type_codes univ_contexts=:(VarContext _ dictionaries_and_contexts _ _)
						# (type_var_heap, var_heap, error)
								= bind_type_vars_to_type_codes symb_ident dt_global_vars type_codes type_code_info.tci_type_var_heap var_heap error
						  (uni_vars, (type_var_heap, var_heap)) = newTypeVariables dt_uni_vars (type_var_heap, var_heap)
						  dt_type = add_types_of_dictionaries dt_contexts dt_type type_code_info.tci_common_defs
						  (type_code_expr, (type_code_info,var_heap,error)) = toTypeCodeExpression (add_universal_vars_to_type dt_uni_vars dt_type)
						  				({ type_code_info & tci_type_var_heap = type_var_heap }, var_heap, error)
						  expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamicWithContexts type_code_expr univ_contexts)
						-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
			EI_TempDynamicType No loc_dynamics _ _ _ expr_ptr {symb_ident}
				# (expr_info, expr_heap) = readPtr expr_ptr expr_heap
				-> case expr_info of
					EI_TypeCode type_expr
						# (type_expr, (undefined_tc_class_vars,var_heap)) = try_update_free_vars_of_TCE type_expr ([],var_heap)
						-> case undefined_tc_class_vars of
							[]
								# expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamic type_expr)
								-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
							_
								# expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamicAndTCsToFind type_expr symb_ident undefined_tc_class_vars)
								-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
					EI_Selection selectors record_var _
						# (var_info,var_heap) = readPtr record_var var_heap
						-> case var_info of
							VI_ClassVar var_ident new_info_ptr count
								# var_heap = writePtr record_var (VI_ClassVar var_ident new_info_ptr (inc count)) var_heap
								  expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamic (convert_selectors selectors new_info_ptr))
								-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
							_
								# type_expr = convert_selectors selectors record_var
								  expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamicAndTCsToFind type_expr symb_ident [record_var])
								-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
			EI_TempDynamicPattern type_vars {dt_global_vars,dt_uni_vars,dt_type,dt_contexts} loc_dynamics temp_local_vars _ _ expr_ptr {symb_ident}
				#! no_contexts = isEmpty dt_contexts
				# (expr_info, expr_heap) = readPtr expr_ptr expr_heap
				-> case expr_info of
					EI_TypeCodes type_codes
						# (type_var_heap, var_heap, error)
								= bind_type_vars_to_type_codes symb_ident dt_global_vars type_codes type_code_info.tci_type_var_heap var_heap error
						  (var_ptrs, (type_pattern_vars, var_heap)) = mapSt addLocalTCInstance temp_local_vars (type_pattern_vars, var_heap)
						  type_var_heap = bind_type_vars_to_type_var_codes type_vars var_ptrs type_var_heap
						  dt_type = add_types_of_dictionaries dt_contexts dt_type type_code_info.tci_common_defs
						  type_code_info = {type_code_info & tci_type_var_heap = type_var_heap}
						  (type_code_expr, (type_code_info,var_heap,error))
								= toTypeCodeExpression (add_universal_vars_to_type dt_uni_vars dt_type) (type_code_info, var_heap, error)
						  expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamicPattern var_ptrs type_code_expr no_contexts)
						-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
					EI_Empty
						# (var_ptrs, (type_pattern_vars, var_heap)) = mapSt addLocalTCInstance temp_local_vars (type_pattern_vars, var_heap)
						  type_var_heap = bind_type_vars_to_type_var_codes type_vars var_ptrs type_code_info.tci_type_var_heap
  						  dt_type = add_types_of_dictionaries dt_contexts dt_type type_code_info.tci_common_defs
						  type_code_info = {type_code_info & tci_type_var_heap = type_var_heap}
						  (type_code_expr, (type_code_info,var_heap,error))
								= toTypeCodeExpression (add_universal_vars_to_type dt_uni_vars dt_type) (type_code_info, var_heap, error)
						  expr_heap = expr_heap <:= (dyn_ptr, EI_TypeOfDynamicPattern var_ptrs type_code_expr no_contexts)
						-> convert_local_dynamics loc_dynamics (type_code_info, expr_heap, type_pattern_vars, var_heap, error)
	where
		add_types_of_dictionaries [{tc_var,tc_class=TCClass {glob_module,glob_object={ds_ident,ds_index}},tc_types}:dictionaries_and_contexts] atype common_defs
			# {class_dictionary} = common_defs.[glob_module].com_class_defs.[ds_index]
			  dict_type_symbol = MakeTypeSymbIdent {glob_module=glob_module,glob_object=class_dictionary.ds_index} class_dictionary.ds_ident class_dictionary.ds_arity
			  class_type = AttributedType (TA dict_type_symbol [AttributedType type \\ type <- tc_types])
			= {at_attribute=TA_Multi, at_type=class_type --> add_types_of_dictionaries dictionaries_and_contexts atype common_defs}
		add_types_of_dictionaries [] atype common_defs
			= atype

		bind_type_vars_to_type_codes symb_ident type_vars type_codes type_var_heap var_heap error
			= fold2St (bind_type_var_to_type_code symb_ident) type_vars type_codes (type_var_heap, var_heap, error)
		where
			bind_type_var_to_type_code symb_ident {tv_ident,tv_info_ptr} type_code (type_var_heap, var_heap, error)
				# (type_code, (var_heap, error)) = updateFreeVarsOfTCE symb_ident type_code (var_heap, error)
				= (type_var_heap <:= (tv_info_ptr, TVI_TypeCode type_code), var_heap, error)
	
		bind_type_vars_to_type_var_codes type_vars var_ptrs type_var_heap
			= fold2St bind_type_var_to_type_var_code type_vars var_ptrs type_var_heap
		where
			bind_type_var_to_type_var_code {tv_info_ptr} var_ptr type_var_heap
				= type_var_heap <:= (tv_info_ptr, TVI_TypeCode (TCE_Var var_ptr))

		add_universal_vars_to_type [] at
			= at
		add_universal_vars_to_type uni_vars at=:{at_type}
			= { at & at_type = TFA uni_vars at_type }

		convert_local_dynamics loc_dynamics state
			= foldSt update_dynamic loc_dynamics state

		convert_selectors [type_code_selector] var_info_ptr
			= TCE_TypeTerm var_info_ptr
		convert_selectors selectors var_info_ptr
			= TCE_Selector (init selectors) var_info_ptr

newTypeVariables uni_vars heaps
	= mapSt new_type_variable uni_vars heaps
where			
	new_type_variable {atv_variable = {tv_info_ptr}} (type_var_heap, var_heap)
		# (new_var_ptr, var_heap) = newPtr VI_Empty var_heap
		= (new_var_ptr, (type_var_heap <:= (tv_info_ptr, TVI_TypeCode (TCE_Var new_var_ptr)), var_heap))

updateFreeVarsOfTCE :: !Ident !TypeCodeExpression (!*VarHeap, !*ErrorAdmin) -> (!TypeCodeExpression, !(!*VarHeap, *ErrorAdmin))
updateFreeVarsOfTCE symb_ident (TCE_Constructor type_cons type_args) var_heap_and_error 
	# (type_args, var_heap_and_error) = mapSt (updateFreeVarsOfTCE symb_ident) type_args var_heap_and_error 
	= (TCE_Constructor type_cons type_args, var_heap_and_error)
updateFreeVarsOfTCE symb_ident (TCE_Selector selections var_info_ptr) var_heap_and_error
	# (var_info_ptr, var_heap_and_error) = getTCDictionary symb_ident var_info_ptr var_heap_and_error
	= (TCE_Selector selections var_info_ptr, var_heap_and_error)
updateFreeVarsOfTCE symb_ident (TCE_TypeTerm var_info_ptr) var_heap_and_error 
	# (var_info_ptr, var_heap_and_error) = getTCDictionary symb_ident var_info_ptr var_heap_and_error
	= (TCE_TypeTerm var_info_ptr, var_heap_and_error)
updateFreeVarsOfTCE symb_ident tce var_heap_and_error
	= (tce, var_heap_and_error)

try_update_free_vars_of_TCE :: !TypeCodeExpression !(![VarInfoPtr],!*VarHeap) -> (!TypeCodeExpression, !(![VarInfoPtr],!*VarHeap))
try_update_free_vars_of_TCE (TCE_Constructor type_cons type_args) var_info_ptrs_and_heap 
	# (type_args, var_info_ptrs_and_heap) = mapSt try_update_free_vars_of_TCE type_args var_info_ptrs_and_heap 
	= (TCE_Constructor type_cons type_args, var_info_ptrs_and_heap)
try_update_free_vars_of_TCE (TCE_Selector selections var_info_ptr) var_info_ptrs_and_heap
	# (var_info_ptr, var_info_ptrs_and_heap) = try_getTCDictionary var_info_ptr var_info_ptrs_and_heap
	= (TCE_Selector selections var_info_ptr, var_info_ptrs_and_heap)
try_update_free_vars_of_TCE (TCE_TypeTerm var_info_ptr) var_info_ptrs_and_heap 
	# (var_info_ptr, var_info_ptrs_and_heap) = try_getTCDictionary var_info_ptr var_info_ptrs_and_heap
	= (TCE_TypeTerm var_info_ptr, var_info_ptrs_and_heap)
try_update_free_vars_of_TCE tce var_info_ptrs_and_heap
	= (tce, var_info_ptrs_and_heap)

update_undefined_free_vars_of_TCE :: !Ident ![VarInfoPtr] !TypeCodeExpression !(!*VarHeap,!*ErrorAdmin) -> (!TypeCodeExpression,!(!*VarHeap,!*ErrorAdmin))
update_undefined_free_vars_of_TCE symb_ident undefined_var_info_ptrs (TCE_Constructor type_cons type_args) var_heap_and_error
	# (type_args, var_heap_and_error) = mapSt (update_undefined_free_vars_of_TCE symb_ident undefined_var_info_ptrs) type_args var_heap_and_error 
	= (TCE_Constructor type_cons type_args, var_heap_and_error)
update_undefined_free_vars_of_TCE symb_ident undefined_var_info_ptrs tce=:(TCE_Selector selections var_info_ptr) var_heap_and_error
	| IsMember var_info_ptr undefined_var_info_ptrs
		# (var_info_ptr, var_heap_and_error) = getTCDictionary symb_ident var_info_ptr var_heap_and_error
		= (TCE_Selector selections var_info_ptr, var_heap_and_error)
		= (tce, var_heap_and_error)
update_undefined_free_vars_of_TCE symb_ident undefined_var_info_ptrs tce=:(TCE_TypeTerm var_info_ptr) var_heap_and_error 
	| IsMember var_info_ptr undefined_var_info_ptrs
		# (var_info_ptr, var_heap_and_error) = getTCDictionary symb_ident var_info_ptr var_heap_and_error
		= (TCE_TypeTerm var_info_ptr, var_heap_and_error)
		= (tce, var_heap_and_error)
update_undefined_free_vars_of_TCE symb_ident undefined_var_info_ptrs tce var_heap_and_error
	= (tce, var_heap_and_error)

getTCDictionary symb_ident var_info_ptr (var_heap, error)
	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_ClassVar var_ident new_info_ptr count
			-> (new_info_ptr, (var_heap <:= (var_info_ptr, VI_ClassVar var_ident new_info_ptr (inc count)), error))
		_
			-> (var_info_ptr, (var_heap, overloadingError symb_ident error))

try_getTCDictionary :: !VarInfoPtr !(![VarInfoPtr],!*VarHeap) -> (!VarInfoPtr,!(![VarInfoPtr],!*VarHeap))
try_getTCDictionary var_info_ptr (var_info_ptrs,var_heap)
	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_ClassVar var_ident new_info_ptr count
			-> (new_info_ptr, (var_info_ptrs,writePtr var_info_ptr (VI_ClassVar var_ident new_info_ptr (inc count)) var_heap))
		_
			-> (var_info_ptr, ([var_info_ptr:var_info_ptrs],var_heap))

toTypeCodeConstructor type=:{glob_object=type_index, glob_module=module_index} common_defs
	| module_index == cPredefinedModuleIndex
		= GTT_PredefTypeConstructor type
	// otherwise
		# type = common_defs.[module_index].com_type_defs.[type_index]
		# td_fun_index = type.td_fun_index
		// sanity check ...
		| td_fun_index == NoIndex
			=	fatal "toTypeCodeConstructor" ("no function (" +++ type.td_ident.id_name +++ ")")
		// ... sanity check
		# type_fun
			=	{	symb_ident = {id_name = "TD;"+++type.td_ident.id_name, id_info = nilPtr}
				,	symb_kind = SK_Function {glob_module = module_index, glob_object = td_fun_index}
				}
		# is_unique_type
			= case type.td_attribute of
				TA_Unique -> True
				_ -> False
		= GTT_Constructor type_fun is_unique_type

fatal :: {#Char} {#Char} -> .a
fatal function_name message
	=	abort ("overloading, " +++ function_name +++ ": " +++ message)

class toTypeCodeExpression type :: type !(!*TypeCodeInfo,!*VarHeap,!*ErrorAdmin) -> (!TypeCodeExpression, !(!*TypeCodeInfo,!*VarHeap,!*ErrorAdmin))

instance toTypeCodeExpression Type where
	toTypeCodeExpression type=:(TA cons_id=:{type_index} type_args) (tci=:{tci_dcl_modules,tci_common_defs},var_heap,error)
		# type_heaps = {th_vars = tci.tci_type_var_heap, th_attrs = tci.tci_attr_var_heap}
		# (expanded, type, type_heaps)
			=	tryToExpandTypeSyn tci_common_defs type cons_id type_args type_heaps
		# tci = {tci & tci_type_var_heap = type_heaps.th_vars, tci_attr_var_heap = type_heaps.th_attrs}
		| expanded
			=	toTypeCodeExpression type (tci,var_heap,error)
		# type_constructor = toTypeCodeConstructor type_index tci_common_defs
		  (type_code_args, tci)
		  	=	mapSt toTypeCodeExpression type_args (tci,var_heap,error)
		= (TCE_Constructor type_constructor type_code_args, tci)
	toTypeCodeExpression (TAS cons_id type_args _) state
		=	toTypeCodeExpression (TA cons_id type_args) state
	toTypeCodeExpression (TB basic_type) (tci,var_heap,error)
		= (TCE_Constructor (GTT_Basic basic_type) [], (tci,var_heap,error))
	toTypeCodeExpression (arg_type --> result_type) (tci,var_heap,error)
		# (type_code_args, tci) = mapSt (toTypeCodeExpression) [arg_type, result_type] (tci,var_heap,error)
		= (TCE_Constructor GTT_Function type_code_args, tci)
	toTypeCodeExpression (TV var) st
		= toTypeCodeExpression var st
	toTypeCodeExpression (TFA vars type) (tci=:{tci_type_var_heap}, var_heap, error)
		# (new_vars, (tci_type_var_heap, var_heap)) = newTypeVariables vars (tci_type_var_heap, var_heap)
		  (type_code, tci) = toTypeCodeExpression type ({tci & tci_type_var_heap = tci_type_var_heap}, var_heap, error)
		= (TCE_UniType new_vars type_code, tci)
	toTypeCodeExpression (CV var :@: args) st
		# (type_code_var, st)
			=	toTypeCodeExpression var st
		  (type_code_args, st)
		  	=	mapSt (toTypeCodeExpression) args st
		= (foldl TCE_App type_code_var type_code_args, st)

instance toTypeCodeExpression TypeVar where
	toTypeCodeExpression {tv_ident,tv_info_ptr} (tci=:{tci_type_var_heap}, var_heap, error)
		# (type_info, tci_type_var_heap) = readPtr tv_info_ptr tci_type_var_heap
		  tci = { tci & tci_type_var_heap = tci_type_var_heap }
		= case type_info of
			TVI_TypeCode type_code
				-> (type_code, (tci,var_heap,error))
			_
				-> abort ("toTypeCodeExpression (TypeVar)" ---> ((ptrToInt tv_info_ptr, tv_ident)))

instance toTypeCodeExpression AType
where
	toTypeCodeExpression {at_attribute=TA_Unique, at_type} tci_and_var_heap_and_error
		# (tce, st)
			=	toTypeCodeExpression at_type tci_and_var_heap_and_error
		=	(TCE_UnqType tce, st)
	toTypeCodeExpression {at_type} tci_and_var_heap_and_error
		=	toTypeCodeExpression at_type tci_and_var_heap_and_error

::	UpdateInfo =
	{	ui_instance_calls	:: ![FunCall]
	,	ui_local_vars		:: ![FreeVar]
	,	ui_symbol_heap		:: !.ExpressionHeap
	,	ui_var_heap			:: !.VarHeap
	,	ui_fun_defs			:: !.{# FunDef}
	,	ui_fun_env			:: !.{! FunctionType}
	,	ui_error			:: !.ErrorAdmin
	,	ui_has_type_codes	:: !Bool
	,	ui_x 				:: !.UpdateInfoX
	}

:: UpdateInfoX =
	{	x_type_code_info	:: !.TypeCodeInfo
	,	x_predef_symbols	:: !.{#PredefinedSymbol}
	,	x_main_dcl_module_n :: !Int
	}

class updateExpression e :: !Index !e !*UpdateInfo -> (!e, !*UpdateInfo)

instance updateExpression Expression
where
	updateExpression group_index (App {app_symb={symb_kind=SK_NewTypeConstructor _},app_args=[arg]}) ui
		= updateExpression group_index arg ui
	updateExpression group_index (App app=:{app_symb=symb=:{symb_kind,symb_ident},app_args,app_info_ptr}) ui
		| isNilPtr app_info_ptr
			# (app_args, ui) = updateExpression group_index app_args ui
			= (App {app & app_args = app_args}, ui)
			# (symb_info, ui_symbol_heap) = readPtr app_info_ptr ui.ui_symbol_heap
			  ui = {ui & ui_symbol_heap = ui_symbol_heap}
			= case symb_info of
				EI_Empty
					# (app_args, ui) = updateExpression group_index app_args ui
					#! main_dcl_module_n = ui.ui_x.UpdateInfoX.x_main_dcl_module_n
					#! fun_index = get_recursive_fun_index group_index symb_kind main_dcl_module_n ui.ui_fun_defs
					| fun_index == NoIndex
						-> (App { app & app_args = app_args }, ui)
						# (CheckedType {st_context}, ui) = ui!ui_fun_env.[fun_index]
						  (app_args, (ui_var_heap, ui_error)) = mapAppendSt (build_context_arg symb_ident) st_context app_args (ui.ui_var_heap, ui.ui_error)
						-> (App { app & app_args = app_args }, { ui & ui_var_heap = ui_var_heap, ui_error = ui_error })
				EI_Context context_args
					# (app_args, ui) = updateExpression group_index app_args ui
					# (app_args, ui) = adjustClassExpressions symb_ident context_args app_args ui
					#! main_dcl_module_n = ui.ui_x.UpdateInfoX.x_main_dcl_module_n
					#! fun_index = get_recursive_fun_index group_index symb_kind main_dcl_module_n ui.ui_fun_defs
					| fun_index == NoIndex
						# app = { app & app_args = app_args}
						-> (App app, examine_calls context_args ui)
						# (CheckedType {st_context}, ui) = ui!ui_fun_env.[fun_index]
						  nr_of_context_args = length context_args
						  nr_of_lifted_contexts = length st_context - nr_of_context_args
						  (app_args, (ui_var_heap, ui_error)) = mapAppendSt (build_context_arg symb_ident) (take nr_of_lifted_contexts st_context) app_args (ui.ui_var_heap,ui.ui_error)
						-> (App { app & app_args = app_args }, examine_calls context_args {ui & ui_var_heap = ui_var_heap, ui_error = ui_error })
				EI_Instance inst_symbol context_args
					# (app_args, ui) = updateExpression group_index app_args ui
					# (context_args, ui=:{ui_var_heap, ui_error}) = adjustClassExpressions symb_ident context_args [] ui
					-> (build_application inst_symbol context_args app_args app_info_ptr,
							examine_calls context_args (new_call inst_symbol.glob_module inst_symbol.glob_object.ds_index
								{ ui & ui_var_heap = ui_var_heap, ui_error = ui_error }))
				EI_Selection selectors record_var context_args
					# (app_args, ui) = updateExpression group_index app_args ui
					# (all_args, ui=:{ui_var_heap, ui_error}) = adjustClassExpressions symb_ident context_args app_args ui
					  (var_ident, var_info_ptr, ui_var_heap, ui_error) = getClassVariable symb_ident record_var ui_var_heap ui_error
					  select_expr = Selection NormalSelector (Var { var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr }) selectors
					| isEmpty all_args
						-> (select_expr, { ui & ui_var_heap = ui_var_heap, ui_error = ui_error })
						-> (select_expr @ all_args, examine_calls context_args
								{ ui & ui_var_heap = ui_var_heap, ui_error = ui_error })
				EI_ContextWithVarContexts context_args var_contexts
					# (app_args,ui) = add_class_vars_for_var_contexts_and_update_expressions var_contexts app_args 0 group_index ui				
					# (app_args, ui) = adjustClassExpressions symb_ident context_args app_args ui
					#! main_dcl_module_n = ui.ui_x.UpdateInfoX.x_main_dcl_module_n
					#! fun_index = get_recursive_fun_index group_index symb_kind main_dcl_module_n ui.ui_fun_defs
					| fun_index == NoIndex
						# app = {app & app_args = app_args}
						-> (App app, examine_calls context_args ui)
						# (CheckedType {st_context}, ui) = ui!ui_fun_env.[fun_index]
						  nr_of_context_args = length context_args
						  nr_of_lifted_contexts = length st_context - nr_of_context_args
						  (app_args, (ui_var_heap, ui_error)) = mapAppendSt (build_context_arg symb_ident) (take nr_of_lifted_contexts st_context) app_args (ui.ui_var_heap,ui.ui_error)
						-> (App {app & app_args = app_args}, examine_calls context_args {ui & ui_var_heap = ui_var_heap, ui_error = ui_error })
	where
		build_context_arg symb tc=:{tc_var} (var_heap, error)
			# (var_info, var_heap) = readPtr tc_var var_heap
			= case var_info of
				VI_ForwardClassVar var_info_ptr
					# (var_ident, var_info_ptr, var_heap, error) = getClassVariable symb var_info_ptr var_heap error
					-> (Var { var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr }, (var_heap, error))
				VI_ClassVar var_ident new_info_ptr count
					-> (Var { var_ident = var_ident, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr },
								(var_heap <:= (tc_var, VI_ClassVar var_ident new_info_ptr (inc count)), error))
				_
					# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
					-> (Var { var_ident = symb, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr },
								(var_heap <:= (tc_var, VI_ClassVar symb new_info_ptr 1), overloadingError symb error))

		add_class_vars_for_var_contexts_and_update_expressions var_contexts=:(VarContext arg_n context arg_atype var_contexts_t) [app_arg:app_args] app_arg_n group_index ui
			| app_arg_n<arg_n
				# (app_arg,ui) = updateExpression group_index app_arg ui
				  (app_args,ui) = add_class_vars_for_var_contexts_and_update_expressions var_contexts app_args (app_arg_n+1) group_index ui
				= ([app_arg:app_args],ui)
			| app_arg_n==arg_n
				# (old_var_infos,var_heap) = add_class_vars_for_var_context context ui.ui_var_heap
				  (app_arg,ui) = updateExpression group_index app_arg {ui & ui_var_heap=var_heap}
				  (free_vars_and_types,local_vars,var_heap)
					= restore_old_var_infos_and_retrieve_class_vars context old_var_infos ui.ui_local_vars ui.ui_var_heap
				  ui = {ui & ui_local_vars=local_vars,ui_var_heap=var_heap}
				= case app_arg of
					expr @ args
						| same_args args free_vars_and_types
							# app_arg = expr
							  (app_args,ui) = add_class_vars_for_var_contexts_and_update_expressions var_contexts_t app_args (app_arg_n+1) group_index ui
							-> ([app_arg:app_args],ui)
					_
						# app_arg = DictionariesFunction free_vars_and_types app_arg arg_atype
						  (app_args,ui) = add_class_vars_for_var_contexts_and_update_expressions var_contexts_t app_args (app_arg_n+1) group_index ui
						-> ([app_arg:app_args],ui)
		add_class_vars_for_var_contexts_and_update_expressions NoVarContexts app_args app_arg_n group_index ui
			= updateExpression group_index app_args ui

		same_args [] []
			= True
		same_args [Var {var_info_ptr}:args] [({fv_info_ptr},_):free_vars_and_types]
			= var_info_ptr==fv_info_ptr && same_args args free_vars_and_types
		same_args _ _
			= False

		get_recursive_fun_index :: !Index !SymbKind Int !{# FunDef} -> Index
		get_recursive_fun_index group_index (SK_Function {glob_module,glob_object}) main_dcl_module_n fun_defs
			| glob_module == main_dcl_module_n
				# {fun_info} = fun_defs.[glob_object]
				| fun_info.fi_group_index == group_index
					= glob_object
					= NoIndex
				= NoIndex
		get_recursive_fun_index group_index (SK_LocalMacroFunction glob_object) main_dcl_module_n fun_defs
			# {fun_info} = fun_defs.[glob_object]
			| fun_info.fi_group_index == group_index
				= glob_object
				= NoIndex
		get_recursive_fun_index group_index _ main_dcl_module_n fun_defs
			= NoIndex

		build_application def_symbol=:{glob_object} context_args orig_args app_info_ptr
			= App {app_symb = { symb_ident = glob_object.ds_ident,
									symb_kind = SK_Function { def_symbol & glob_object = glob_object.ds_index } },
				   app_args = context_args ++ orig_args, app_info_ptr = app_info_ptr }

	updateExpression group_index (expr @ exprs) ui
		# ((expr, exprs), ui) = updateExpression group_index (expr, exprs) ui
		= (expr @ exprs, ui)
	updateExpression group_index (Let lad=:{let_lazy_binds, let_strict_binds, let_expr}) ui
		# ui = set_aliases_for_binds_that_will_become_aliases let_lazy_binds ui
		# (let_lazy_binds, ui)		= updateExpression group_index let_lazy_binds ui
		# (let_strict_binds, ui)	= updateExpression group_index let_strict_binds ui
		# (let_expr, ui)			= updateExpression group_index let_expr ui
		= (Let {lad & let_lazy_binds = let_lazy_binds, let_strict_binds = let_strict_binds, let_expr = let_expr}, ui)

	updateExpression group_index (Case kees=:{case_guards=case_guards=:AlgebraicPatterns type patterns,case_expr,case_default,case_info_ptr}) ui
		# (case_info, ui_symbol_heap) = readPtr case_info_ptr ui.ui_symbol_heap
		  ui = {ui & ui_symbol_heap = ui_symbol_heap}
		= case case_info of
			EI_CaseTypeWithContexts case_type=:{ct_cons_types} constructorcontexts
				# (case_expr,ui) = updateExpression group_index case_expr ui
				  (patterns,ct_cons_types,ui) = update_constructors_with_contexts_patterns constructorcontexts patterns ct_cons_types group_index ui
				  case_guards = AlgebraicPatterns type patterns
				  (case_default,ui) = updateExpression group_index case_default ui
				  ui_symbol_heap = writePtr case_info_ptr (EI_CaseType {case_type & ct_cons_types=ct_cons_types}) ui.ui_symbol_heap
				  ui = {ui & ui_symbol_heap = ui_symbol_heap}
				-> (Case {kees & case_expr = case_expr, case_guards = case_guards, case_default = case_default}, ui)
			EI_CaseType {ct_cons_types}
				| Any has_TFAC ct_cons_types
					# (case_expr,ui) = updateExpression group_index case_expr ui
					  (patterns, ui) = update_algebraic_patterns patterns ct_cons_types group_index ui
					  case_guards = AlgebraicPatterns type patterns
					  (case_default, ui) = updateExpression group_index case_default ui
					-> (Case {kees & case_expr = case_expr, case_guards = case_guards, case_default = case_default}, ui)
			_
				# ((case_expr,(case_guards,case_default)), ui) = updateExpression group_index (case_expr,(case_guards,case_default)) ui
				-> (Case {kees & case_expr = case_expr, case_guards = case_guards, case_default = case_default}, ui)

	updateExpression group_index case_expr=:(Case {case_guards=NewTypePatterns _ _}) ui
		= remove_NewTypePatterns_case_and_update_expression case_expr group_index ui
	updateExpression group_index (Case kees=:{case_expr,case_guards,case_default}) ui
		# ((case_expr,(case_guards,case_default)), ui) = updateExpression group_index (case_expr,(case_guards,case_default)) ui
		= (Case { kees & case_expr = case_expr, case_guards = case_guards, case_default = case_default }, ui)
	updateExpression group_index (Selection is_unique expr selectors) ui
		# (expr, ui) = updateExpression group_index expr ui
		  (selectors, ui) = updateExpression group_index selectors ui
		= (Selection is_unique expr selectors, ui)
	updateExpression group_index (Update expr1 selectors expr2) ui
		# (expr1, ui) = updateExpression group_index expr1 ui
		  (selectors, ui) = updateExpression group_index selectors ui
		  (expr2, ui) = updateExpression group_index expr2 ui
		= (Update expr1 selectors expr2, ui)
	updateExpression group_index (RecordUpdate cons_symbol expression expressions) ui
		# (expression, ui) = updateExpression group_index expression ui
		  (expressions, ui) = updateExpression group_index expressions ui
		= (RecordUpdate cons_symbol expression expressions, ui)
	updateExpression group_index (DynamicExpr dyn=:{dyn_expr,dyn_info_ptr}) ui=:{ui_has_type_codes}
		# (dyn_info, ui_symbol_heap) = readPtr dyn_info_ptr ui.ui_symbol_heap
		  ui = {ui & ui_has_type_codes = False, ui_symbol_heap = ui_symbol_heap}
		  (dyn_expr,type_code,ui)
			= case dyn_info of
				EI_TypeOfDynamic type_code
					# (dyn_expr, ui) = updateExpression group_index dyn_expr ui
					-> (dyn_expr,type_code,ui)
				EI_TypeOfDynamicAndTCsToFind type_expr symb_ident undefined_tc_class_vars
					# (type_expr, (var_heap, error))
						= update_undefined_free_vars_of_TCE symb_ident undefined_tc_class_vars type_expr (ui.ui_var_heap, ui.ui_error)
					  ui & ui_var_heap=var_heap, ui_error=error
					  (dyn_expr, ui) = updateExpression group_index dyn_expr ui
					-> (dyn_expr,type_expr,ui)
				EI_TypeOfDynamicWithContexts type_code (VarContext _ context dynamic_expr_type NoVarContexts)
					# (old_var_infos,var_heap) = add_class_vars_for_var_context context ui.ui_var_heap
					  (dyn_expr,ui) = updateExpression group_index dyn_expr {ui & ui_var_heap=var_heap}
					  (free_vars_and_types,local_vars,var_heap)
						= restore_old_var_infos_and_retrieve_class_vars context old_var_infos ui.ui_local_vars ui.ui_var_heap
					  ui = {ui & ui_local_vars=local_vars,ui_var_heap=var_heap}
					  dyn_expr = DictionariesFunction free_vars_and_types dyn_expr dynamic_expr_type
					-> (dyn_expr,type_code,ui)
		  ui = check_type_codes_in_dynamic ui
			with
				check_type_codes_in_dynamic ui=:{ui_has_type_codes, ui_error}
					| ui_has_type_codes
						# ui_error = typeCodeInDynamicError ui_error
						= {ui & ui_error = ui_error}
						= ui
		  ui = {ui & ui_has_type_codes=ui_has_type_codes}
		= (DynamicExpr { dyn & dyn_expr = dyn_expr, dyn_type_code = type_code }, ui)
	updateExpression group_index (TupleSelect symbol argn_nr expr) ui
		# (expr, ui) = updateExpression group_index expr ui
		= (TupleSelect symbol argn_nr expr, ui)
	updateExpression group_index (MatchExpr cons_symbol=:{glob_object={ds_arity}} expr) ui
		| ds_arity <> -2
			# (expr, ui) = updateExpression group_index expr ui
			= (MatchExpr cons_symbol expr, ui)
			// newtype constructor
			= updateExpression group_index expr ui
	updateExpression group_index (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) ui
		# (expr, ui) = updateExpression group_index expr ui
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, ui)
	updateExpression group_index (TypeSignature _ expr) ui
		= updateExpression group_index expr ui
	updateExpression group_index expr=:(Var {var_info_ptr,var_expr_ptr,var_ident}) ui
		# (var_info,var_heap) = readPtr var_info_ptr ui.ui_var_heap
		# ui = {ui & ui_var_heap = var_heap}
		= case var_info of
			VI_Alias var2
				# (var_info2,var_heap) = readPtr var2.var_info_ptr ui.ui_var_heap
				# ui = { ui & ui_var_heap = var_heap }
				-> skip_aliases var_info2 var2 var_info_ptr ui
			VI_FPC
				# (expr_info,ui_symbol_heap) = readPtr var_expr_ptr ui.ui_symbol_heap
				# ui = {ui & ui_symbol_heap=ui_symbol_heap}
				-> case expr_info of
					EI_FPContext context_args var_expr_ptr
						# (app_args, ui) = adjustClassExpressions var_ident context_args [] ui
						# ui = examine_calls context_args ui
						-> (expr @ app_args,ui)
			_
				-> (expr,ui)
	where
		skip_aliases var_info2=:(VI_Alias var3) var2 var_info_ptr1 ui=:{ui_var_heap}
			# ui = set_alias_and_detect_cycle var_info_ptr1 var3 ui
			| var3.var_info_ptr==var_info_ptr1
				= (Var var2,ui)
				# (var_info3,var_heap) = readPtr var3.var_info_ptr ui.ui_var_heap
				# ui = { ui & ui_var_heap = var_heap }
				= skip_aliases var_info3 var3 var2.var_info_ptr ui
		skip_aliases var_info2 var2 var_info ui
			= (Var var2,ui)
	updateExpression group_index expr ui
		= (expr, ui)

update_constructors_with_contexts_patterns [constructor_context:constructor_contexts] patterns cons_types group_index ui
	= update_constructor_with_contexts_patterns constructor_context constructor_contexts patterns cons_types group_index ui
where
	update_constructor_with_contexts_patterns constructor_context=:(constructor_symbol,context) constructor_contexts [pattern:patterns] [cons_type:cons_types] group_index ui
		| constructor_symbol==pattern.ap_symbol.glob_object
			# (old_var_infos,var_heap) = make_class_vars context ui.ui_var_heap
			  ui = {ui & ui_var_heap=var_heap}					  

			  (expr,ui) = updateExpression group_index pattern.ap_expr ui

			  vars = pattern.ap_vars
			  arity = pattern.ap_symbol.glob_object.ds_arity
			  (vars,arity,local_vars,var_heap) = add_class_vars_to_pattern_and_restore_old_var_infos context old_var_infos vars arity ui.ui_local_vars ui.ui_var_heap
			  ui = {ui & ui_local_vars=local_vars,ui_var_heap=var_heap}
			  pattern = {pattern & ap_vars=vars,ap_expr=expr,ap_symbol.glob_object.ds_arity=arity}
			  (patterns,cons_types,ui) = update_constructors_with_contexts_patterns constructor_contexts patterns cons_types group_index ui

			  (common_defs,ui) = ui!ui_x.x_type_code_info.tci_common_defs
			  cons_type = addTypesOfDictionaries common_defs context cons_type

			= ([pattern:patterns],[cons_type:cons_types],ui)

			# (pattern,ui) = updateExpression group_index pattern ui
			  (patterns,cons_types,ui) = update_constructor_with_contexts_patterns constructor_context constructor_contexts patterns cons_types group_index ui
			= ([pattern:patterns],[cons_type:cons_types],ui)

	make_class_vars [tc=:{tc_class,tc_var}:contexts] var_heap
		# (old_var_infos,var_heap) = make_class_vars contexts var_heap
		  (old_var_info,var_heap) = readPtr tc_var var_heap
	  	  (var_info_ptr, var_heap) = newPtr VI_Empty var_heap
		  ident = {id_name = "_v" +++ toString tc_class, id_info = nilPtr}
		  (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
		  var_heap = writePtr tc_var (VI_ClassVar ident new_info_ptr 0) var_heap
		= ([old_var_info:old_var_infos],var_heap)
	make_class_vars [] var_heap
		= ([],var_heap)

	add_class_vars_to_pattern_and_restore_old_var_infos [{tc_var}:contexts] [old_var_info:old_var_infos] vars arity local_vars var_heap
		# (vars,arity,local_vars,var_heap) = add_class_vars_to_pattern_and_restore_old_var_infos contexts old_var_infos vars arity local_vars var_heap
		  (VI_ClassVar var_ident new_info_ptr count, var_heap) = readPtr tc_var var_heap
		  free_var = {fv_ident=var_ident, fv_info_ptr=new_info_ptr, fv_def_level=NotALevel, fv_count=count}
		  var_heap = writePtr tc_var old_var_info var_heap
		= ([free_var:vars],arity+1,[free_var:local_vars],var_heap)
	add_class_vars_to_pattern_and_restore_old_var_infos [] [] vars arity local_vars var_heap
		= (vars,arity,local_vars,var_heap)
update_constructors_with_contexts_patterns [] patterns cons_types group_index ui
	# (patters,ui) = updateExpression group_index patterns ui
	= (patters,cons_types,ui)

update_algebraic_patterns [pattern=:{ap_expr,ap_vars}:patterns] [cons_arg_types:conses_args_types] group_index ui
	# ui & ui_var_heap = mark_FPC_vars cons_arg_types ap_vars ui.ui_var_heap
	# (ap_expr,ui) = updateExpression group_index ap_expr ui
	# (patterns,ui) = update_algebraic_patterns patterns conses_args_types group_index ui
	= ([{pattern & ap_expr=ap_expr}:patterns],ui)
update_algebraic_patterns [] [] group_index ui
	= ([],ui)

add_class_vars_for_var_context [{dc_var}:contexts] var_heap
	# (var_info,var_heap) = readPtr dc_var var_heap
	  symb = {id_name = "_d", id_info = nilPtr}
	  (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
	  var_heap = writePtr dc_var (VI_ClassVar symb new_info_ptr 0) var_heap
	  (old_var_infos,var_heap) = add_class_vars_for_var_context contexts var_heap
	= ([var_info:old_var_infos],var_heap)
add_class_vars_for_var_context [] var_heap
	= ([],var_heap)

restore_old_var_infos_and_retrieve_class_vars [{dc_var,dc_class_type}:contexts] [old_var_info:old_var_infos] local_vars var_heap		
	# (VI_ClassVar var_ident new_info_ptr count, var_heap) = readPtr dc_var var_heap
	  free_var = {fv_ident=var_ident, fv_info_ptr=new_info_ptr, fv_def_level=NotALevel, fv_count=count}
	  var_heap = writePtr dc_var old_var_info var_heap
	  (free_vars_and_types,local_vars,var_heap)
	  	= restore_old_var_infos_and_retrieve_class_vars contexts old_var_infos local_vars var_heap
	= ([(free_var,dc_class_type):free_vars_and_types],[free_var:local_vars],var_heap)
restore_old_var_infos_and_retrieve_class_vars [] [] local_vars var_heap
	= ([],local_vars,var_heap)

examine_calls [expr : exprs] ui
	= examine_calls exprs (examine_calls_in_expr expr ui)
where
	examine_calls_in_expr (App {app_symb = {symb_ident,symb_kind}, app_args}) ui
		= examine_calls app_args (examine_application symb_kind ui)
	examine_calls_in_expr (Let {let_expr,let_lazy_binds}) ui
		# ui = examine_calls_in_expr let_expr ui
		= foldSt (examine_calls_bind) let_lazy_binds (examine_calls_in_expr let_expr ui)
	examine_calls_in_expr _ ui
		= ui

	examine_calls_bind {lb_src,lb_dst} ui=:{ui_local_vars}
		= examine_calls_in_expr lb_src {ui & ui_local_vars = [lb_dst : ui_local_vars]}

	examine_application (SK_Function {glob_module,glob_object}) ui
		= new_call glob_module glob_object ui
	examine_application symb_kind ui
		= ui
examine_calls [] ui
	= ui

new_call mod_index symb_index ui=:{ui_instance_calls,ui_fun_defs}
	| mod_index == ui.ui_x.UpdateInfoX.x_main_dcl_module_n && symb_index < size ui_fun_defs
		# ui_instance_calls = add_call symb_index ui_instance_calls
		= {ui & ui_instance_calls = ui_instance_calls}
		= ui
where
	add_call fun_num []
		= [FunCall fun_num 0]
	add_call fun_num funs=:[call=:(FunCall fc_index _) : ui]
		| fun_num == fc_index
			= funs
		| fun_num < fc_index
			= [FunCall fun_num 0 : funs]
			= [call : add_call fun_num ui]

set_alias_and_detect_cycle info_ptr var ui
	| info_ptr<>var.var_info_ptr
		= { ui & ui_var_heap = writePtr info_ptr (VI_Alias var) ui.ui_var_heap }
	# (var_info,var_heap) = readPtr info_ptr ui.ui_var_heap
	# ui = { ui & ui_var_heap = var_heap }
	= case var_info of
		VI_Alias var
			| var.var_info_ptr==info_ptr // to prevent repeating cycle error
				-> ui
		_
			# ui = { ui & ui_var_heap = writePtr info_ptr (VI_Alias var) ui.ui_var_heap }
			-> {ui & ui_error = cycleAfterRemovingNewTypeConstructorsError var.var_ident ui.ui_error}

remove_NewTypePatterns_case_and_update_expression :: !Expression !Index !*UpdateInfo -> (!Expression,!*UpdateInfo)
remove_NewTypePatterns_case_and_update_expression (Case {case_guards=NewTypePatterns type [{ap_symbol,ap_vars=[ap_var=:{fv_info_ptr}],ap_expr,ap_position}],
													case_expr, case_default, case_explicit, case_info_ptr}) group_index ui
	# ap_expr = add_case_default ap_expr case_default
	# ap_expr = if case_explicit
					(mark_case_explicit ap_expr)
					ap_expr
	# (case_expr,ui) = updateExpression group_index case_expr ui
	= case case_expr of
		Var var
			# ui = set_alias_and_detect_cycle fv_info_ptr var ui
			-> updateExpression group_index ap_expr ui
		case_expr
			# (ap_expr,ui) = updateExpression group_index ap_expr ui
			# let_bind = {lb_dst = ap_var, lb_src = case_expr, lb_position = ap_position}
			# (EI_CaseType {ct_pattern_type}, ui_symbol_heap) = readPtr case_info_ptr ui.ui_symbol_heap
//			# (let_info_ptr, ui_symbol_heap) = newPtr (EI_LetType [ct_pattern_type]) ui_symbol_heap
			# let_info_ptr = case_info_ptr
			# ui_symbol_heap = writePtr case_info_ptr (EI_LetType [ct_pattern_type]) ui_symbol_heap
			# ui = { ui & ui_symbol_heap = ui_symbol_heap }
			# let_expr = Let {	let_strict_binds = [], let_lazy_binds = [let_bind], let_expr = ap_expr,
								let_info_ptr = let_info_ptr, let_expr_position = ap_position }
			-> (let_expr,ui)
	where
		mark_case_explicit (Case case_=:{case_explicit})
			= Case {case_ & case_explicit=True}
		mark_case_explicit (Let let_=:{let_expr})
			= Let {let_ & let_expr=mark_case_explicit let_expr}
		mark_case_explicit expr
			= expr

		add_case_default expr No
			= expr
		add_case_default expr (Yes default_expr)
			= add_default expr default_expr
		where
			add_default (Case kees=:{case_default=No,case_explicit=False}) default_expr
				= Case { kees & case_default = Yes default_expr }
			add_default (Case kees=:{case_default=Yes case_default_expr,case_explicit=False}) default_expr
				= Case { kees & case_default = Yes (add_default case_default_expr default_expr)}
			add_default (Let lad=:{let_expr}) default_expr
				= Let { lad & let_expr = add_default let_expr default_expr }
			add_default expr _
				= expr

instance updateExpression LetBind
where
	updateExpression group_index bind=:{lb_src} ui
		# (lb_src, ui) = updateExpression group_index lb_src ui
		= ({bind & lb_src = lb_src }, ui)

instance updateExpression (Bind a b) | updateExpression a
where
	updateExpression group_index bind=:{bind_src} ui
		# (bind_src, ui) = updateExpression group_index bind_src ui
		= ({bind & bind_src = bind_src }, ui)

instance updateExpression (Optional a) | updateExpression a
where
	updateExpression group_index (Yes x) ui
		# (x, ui) = updateExpression group_index x ui
		= (Yes x, ui)
	updateExpression group_index No ui
		= (No, ui)

instance updateExpression CasePatterns
where
	updateExpression group_index (AlgebraicPatterns type patterns) ui
		# (patterns, ui) = updateExpression group_index patterns ui
		= (AlgebraicPatterns type patterns, ui)
	updateExpression group_index (BasicPatterns type patterns) ui
		# (patterns, ui) = updateExpression group_index patterns ui
		= (BasicPatterns type patterns, ui)
	updateExpression group_index (OverloadedListPatterns type decons_expr patterns) ui
		# (patterns, ui) = updateExpression group_index patterns ui
		# (decons_expr, ui) = updateExpression group_index decons_expr ui
		= (OverloadedListPatterns type decons_expr patterns, ui)
	updateExpression group_index (DynamicPatterns patterns) ui
		# (patterns, ui) = updateExpression group_index patterns ui
		= (DynamicPatterns patterns, ui)
	
instance updateExpression AlgebraicPattern
where
	updateExpression group_index pattern=:{ap_expr} ui
		# (ap_expr, ui) =  updateExpression group_index ap_expr ui
		= ({ pattern & ap_expr = ap_expr }, ui)

instance updateExpression BasicPattern
where
	updateExpression group_index pattern=:{bp_expr} ui
		# (bp_expr, ui) = updateExpression group_index bp_expr ui
		= ({ pattern & bp_expr = bp_expr }, ui)

instance updateExpression Selection
where
  	updateExpression group_index (ArraySelection selector=:{glob_object={ds_ident}} expr_ptr index_expr) ui
		# (index_expr, ui) = updateExpression group_index index_expr ui
		  (symb_info, ui_symbol_heap) = readPtr expr_ptr ui.ui_symbol_heap
		  ui = { ui & ui_symbol_heap = ui_symbol_heap }
		= case symb_info of
			EI_Instance array_select []
				-> (ArraySelection array_select expr_ptr index_expr, ui)
			EI_Selection selectors record_var context_args
				# (var_ident, var_info_ptr, ui_var_heap, ui_error) = getClassVariable ds_ident record_var ui.ui_var_heap ui.ui_error
				-> (DictionarySelection { var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr } selectors expr_ptr index_expr,
							{ ui & ui_var_heap = ui_var_heap, ui_error = ui_error })
  	updateExpression group_index selection ui
		= (selection, ui)

instance updateExpression DynamicPattern
where
	updateExpression group_index dp=:{dp_var,dp_type,dp_rhs} ui
		# (EI_TypeOfDynamicPattern type_pattern_vars type_code no_contexts, ui_symbol_heap) = readPtr dp_type ui.ui_symbol_heap
		  ui = {ui & ui_symbol_heap = ui_symbol_heap}
		| no_contexts
			# (dp_rhs, ui) = updateExpression group_index dp_rhs ui
			= ({dp & dp_rhs = dp_rhs, dp_type_code = type_code}, ui)
			# ui = {ui & ui_var_heap = writePtr dp_var.fv_info_ptr VI_FPC ui.ui_var_heap}
			  (dp_rhs, ui) = updateExpression group_index dp_rhs ui
			= ({dp & dp_rhs = dp_rhs, dp_type_code = type_code}, ui)

instance updateExpression (a,b) | updateExpression a & updateExpression b
where
	updateExpression group_index t ui
		= app2St (updateExpression group_index,updateExpression group_index) t ui

instance updateExpression [e] | updateExpression e
where
	updateExpression group_index l ui
		= mapSt (updateExpression group_index) l ui

set_aliases_for_binds_that_will_become_aliases :: ![LetBind] !*UpdateInfo -> *UpdateInfo
set_aliases_for_binds_that_will_become_aliases [] ui
	= ui
set_aliases_for_binds_that_will_become_aliases [{lb_dst={fv_info_ptr},lb_src}:let_binds] ui
	# ui = make_alias_if_expression_will_become_var lb_src fv_info_ptr ui
	= set_aliases_for_binds_that_will_become_aliases let_binds ui
where
	make_alias_if_expression_will_become_var (Var var) fv_info_ptr ui
		= set_alias_and_detect_cycle fv_info_ptr var ui
	make_alias_if_expression_will_become_var (App {app_symb={symb_kind=SK_NewTypeConstructor _},app_args=[arg]}) fv_info_ptr ui
		= skip_newtypes_and_make_alias_if_var arg fv_info_ptr ui
	make_alias_if_expression_will_become_var (MatchExpr {glob_object={ds_arity = -2}} expr) fv_info_ptr ui
		= skip_newtypes_and_make_alias_if_var expr fv_info_ptr ui
	make_alias_if_expression_will_become_var expr=:(Case {case_guards=NewTypePatterns _ _}) fv_info_ptr ui
		= skip_newtypes_and_make_alias_if_var expr fv_info_ptr ui
	make_alias_if_expression_will_become_var _ fv_info_ptr ui
		= ui

	skip_newtypes_and_make_alias_if_var expr fv_info_ptr ui
		= case skip_newtypes expr of
			Var var
				-> set_alias_and_detect_cycle fv_info_ptr var ui
			_
				-> ui
	where
		skip_newtypes (App {app_symb={symb_kind=SK_NewTypeConstructor _},app_args=[arg]})
			= skip_newtypes arg 
		skip_newtypes (MatchExpr {glob_object={ds_arity = -2}} expr)
			= skip_newtypes expr
		skip_newtypes expr=:(Case {case_guards=NewTypePatterns type [{ap_symbol,ap_vars=[ap_var=:{fv_info_ptr}],ap_expr}],case_expr})
			= case skip_newtypes case_expr of
				Var case_var
					-> case skip_newtypes ap_expr of
						Var rhs_var
							| rhs_var.var_info_ptr==fv_info_ptr
								-> case_expr
								-> ap_expr
						_
							-> expr 
				_
					-> expr
		skip_newtypes expr
			= expr

adjustClassExpressions symb_ident exprs tail_exprs ui
	= mapAppendSt (adjustClassExpression symb_ident) exprs tail_exprs ui
where
	adjustClassExpression symb_ident (App app=:{app_args}) ui
		# (app_args, ui) = adjustClassExpressions symb_ident app_args [] ui
		= (App {app & app_args = app_args}, ui)
	adjustClassExpression symb_ident (ClassVariable var_info_ptr) ui=:{ui_var_heap, ui_error}
		# (var_ident, var_info_ptr, ui_var_heap, ui_error) = getClassVariable symb_ident var_info_ptr ui_var_heap ui_error
		= (Var {var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr}, {ui & ui_var_heap = ui_var_heap, ui_error = ui_error})
	adjustClassExpression symb_ident (Selection opt_type expr selectors) ui
		# (expr, ui) = adjustClassExpression symb_ident expr ui
		= (Selection opt_type expr selectors, ui)
	adjustClassExpression symb_ident tce=:(TypeCodeExpression type_code) ui
		# (type_code, ui) = adjust_type_code type_code ui
		= (TypeCodeExpression type_code, {ui & ui_has_type_codes = True})
	where
		adjust_type_code (TCE_TypeTerm var_info_ptr) ui=:{ui_var_heap,ui_error}
			# (var_info_ptr, (ui_var_heap,ui_error))
				= getTCDictionary symb_ident var_info_ptr (ui_var_heap, ui_error)
			# ui = {ui & ui_var_heap = ui_var_heap, ui_error = ui_error}
			= (TCE_TypeTerm var_info_ptr, ui)
		adjust_type_code (TCE_Selector selectors var_info_ptr) ui=:{ui_var_heap,ui_error}
			# (var_info_ptr, (ui_var_heap,ui_error))
				= getTCDictionary symb_ident var_info_ptr (ui_var_heap, ui_error)
			# ui = {ui & ui_var_heap = ui_var_heap, ui_error = ui_error}
			= (TCE_Selector selectors var_info_ptr, ui)
		adjust_type_code (TCE_Constructor cons typecode_exprs) ui
			# (typecode_exprs, ui) = mapSt adjust_type_code typecode_exprs ui
			= (TCE_Constructor cons typecode_exprs, ui)
		adjust_type_code (TCE_UniType uni_vars type_code) ui
			# (type_code, ui) = adjust_type_code type_code ui
			= (TCE_UniType uni_vars type_code, ui)
		adjust_type_code type_code ui
			= (type_code, ui)

	adjustClassExpression symb_ident (Let this_let=:{let_strict_binds, let_lazy_binds, let_expr }) ui
		# (let_strict_binds, ui) = adjust_let_binds symb_ident let_strict_binds ui
		  (let_lazy_binds, ui) = adjust_let_binds symb_ident let_lazy_binds ui
		  (let_expr, ui) = adjustClassExpression symb_ident let_expr ui
		= (Let { this_let & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr }, ui)
	where
		adjust_let_binds symb_ident let_binds ui
			= mapSt (adjust_let_bind symb_ident) let_binds ui

		adjust_let_bind symb_ident let_bind=:{lb_src} ui
			# (lb_src, ui) = adjustClassExpression symb_ident lb_src ui
			= ({let_bind & lb_src = lb_src}, ui)

	adjustClassExpression symb_ident  expr ui
		= (expr, ui)

class equalTypes a :: !a !a !*TypeVarHeap -> (!Bool, !*TypeVarHeap)

instance equalTypes AType
where
	equalTypes atype1 atype2 type_var_heap
		= equalTypes atype1.at_type atype2.at_type type_var_heap

equalTypeVars {tv_info_ptr}	temp_var_id type_var_heap
	# (tv_info, type_var_heap) = readPtr tv_info_ptr type_var_heap
	= case tv_info of
		TVI_Forward forw_var_number
			-> (forw_var_number == temp_var_id, type_var_heap)
		_
			-> (True, type_var_heap <:= (tv_info_ptr, TVI_Forward temp_var_id))

instance equalTypes Type
where
	equalTypes (TV tv) (TempV var_number) type_var_heap
		= equalTypeVars tv var_number type_var_heap
	equalTypes (arg_type1 --> restype1) (arg_type2 --> restype2) type_var_heap
		= equalTypes (arg_type1,restype1) (arg_type2,restype2) type_var_heap
	equalTypes (TA tc1 types1) (TA tc2 types2) type_var_heap
		| tc1 == tc2
			= equalTypes types1 types2 type_var_heap
			= (False, type_var_heap)
	equalTypes (TA tc1 types1) (TAS tc2 types2 _) type_var_heap
		| tc1 == tc2
			= equalTypes types1 types2 type_var_heap
			= (False, type_var_heap)
	equalTypes (TAS tc1 types1 _) (TA tc2 types2) type_var_heap
		| tc1 == tc2
			= equalTypes types1 types2 type_var_heap
			= (False, type_var_heap)
	equalTypes (TAS tc1 types1 _) (TAS tc2 types2 _) type_var_heap
		| tc1 == tc2
			= equalTypes types1 types2 type_var_heap
			= (False, type_var_heap)
	equalTypes (TB basic1) (TB basic2) type_var_heap
		= (basic1 == basic2, type_var_heap)
	equalTypes TArrow TArrow type_var_heap
		= (True, type_var_heap)
	equalTypes (TArrow1 x) (TArrow1 y) type_var_heap
		= equalTypes x y type_var_heap		
	equalTypes (CV tv :@: types1) (TempCV var_number :@: types2) type_var_heap
		# (eq, type_var_heap) = equalTypeVars tv var_number type_var_heap
		| eq
			= equalTypes types1 types2 type_var_heap
			= (False, type_var_heap)
	equalTypes type1 type2 type_var_heap
		= (False, type_var_heap)

instance equalTypes (a,b) | equalTypes a & equalTypes b
where
	equalTypes (x1,y1) (x2,y2) type_var_heap
		# (eq, type_var_heap) = equalTypes x1 x2 type_var_heap
		| eq
			= equalTypes y1 y2 type_var_heap
			= (False, type_var_heap)

instance equalTypes [a] | equalTypes a
where
	equalTypes [x:xs] [y:ys] type_var_heap
		= equalTypes (x,xs) (y,ys) type_var_heap
	equalTypes [] [] type_var_heap
		= (True, type_var_heap)
	equalTypes _ _ type_var_heap
		= (False, type_var_heap)

instance <<< TypeContext
where
	(<<<) file tc = file <<< toString tc.tc_class <<< ' ' <<< tc.tc_types <<< " <" <<< tc.tc_var <<< '>'

instance <<< Special
where
	(<<<) file {spec_types} = file <<< spec_types

instance <<< (Ptr x)
where
	(<<<) file ptr = file <<< '<' <<< ptrToInt ptr <<< '>'

/*	
instance <<< TypeCodeExpression
where
	(<<<) file _ = file
*/

instance <<< DefinedSymbol
where
	(<<<) file ds = file <<< ds.ds_ident

instance <<< ExprInfo
where
	(<<<) file (EI_Instance symb exprs) = file <<< symb <<< exprs
	(<<<) file (EI_Selection sels var_ptr exprs) = file <<< sels <<< var_ptr <<< exprs
	(<<<) file (EI_Context exprs) = file <<< exprs
	(<<<) file _ = file

instance <<< ClassApplication
where
	(<<<) file (CA_Instance rc) = file <<< "CA_Instance"
	(<<<) file (CA_Context tc) = file <<< "CA_Context " <<< tc
	(<<<) file (CA_LocalTypeCode tc) = file <<< "CA_LocalTypeCode " <<< tc
	(<<<) file (CA_GlobalTypeCode tci) = file <<< "CA_GlobalTypeCode " <<< tci

instance <<< TypeCodeInstance
where
	(<<<) file {tci_contexts} = file <<< ' ' <<< tci_contexts

