implementation module partition

import syntax, transform, utilities

//	PARTITIONING

::	PartitioningInfo =
	{	pi_marks :: 		!.{# Int}
	,	pi_next_num ::		!Int
	,	pi_next_group ::	!Int
	,	pi_groups ::		![ComponentMembers]
	,	pi_deps ::			![Int]
	}

NotChecked :== -1	

partitionateFunctions :: !*{# FunDef} ![IndexRange] -> (!*{!Component}, !*{# FunDef})
partitionateFunctions fun_defs ranges
	#! max_fun_nr = size fun_defs
	# partitioning_info = { pi_marks = createArray max_fun_nr NotChecked, pi_deps = [], pi_next_num = 0, pi_next_group = 0, pi_groups = [] }
	  (fun_defs, {pi_groups,pi_next_group}) = 
	  		foldSt (partitionate_functions max_fun_nr) ranges (fun_defs, partitioning_info)
	  groups = { {component_members = group} \\ group <- reverse pi_groups }
	= (groups, fun_defs)
where
	partitionate_functions :: !Index !IndexRange !(!*{# FunDef}, !*PartitioningInfo) -> (!*{# FunDef}, !*PartitioningInfo)
	partitionate_functions max_fun_nr ir=:{ir_from,ir_to} (fun_defs, pi=:{pi_marks})
		| ir_from == ir_to
			= (fun_defs, pi)
		| pi_marks.[ir_from] == NotChecked
			# (_, fun_defs, pi) = partitionate_function ir_from max_fun_nr fun_defs pi
			= partitionate_functions max_fun_nr { ir & ir_from = inc ir_from } (fun_defs, pi)
			= partitionate_functions max_fun_nr { ir & ir_from = inc ir_from } (fun_defs, pi)

	partitionate_function :: !Int !Int !*{# FunDef} !*PartitioningInfo -> *(!Int, !*{# FunDef}, !*PartitioningInfo)
	partitionate_function fun_index max_fun_nr fun_defs pi=:{pi_next_num}
		# (fd, fun_defs) = fun_defs![fun_index]
		# {fi_calls} = fd.fun_info
		  (min_dep, fun_defs, pi) = visit_functions fi_calls max_fun_nr max_fun_nr fun_defs (push_on_dep_stack fun_index pi)
			with
				visit_functions :: ![FunCall] !Int !Int !*{# FunDef} !*PartitioningInfo -> *(!Int, !*{# FunDef}, !*PartitioningInfo)
				visit_functions [FunCall fc_index _:funs] min_dep max_fun_nr fun_defs pi=:{pi_marks} 
					#! mark = pi_marks.[fc_index]
					| mark == NotChecked
						# (mark, fun_defs, pi) = partitionate_function fc_index max_fun_nr fun_defs  pi
						= visit_functions funs (min min_dep mark) max_fun_nr fun_defs pi
						= visit_functions funs (min min_dep mark) max_fun_nr fun_defs pi
				visit_functions [DclFunCall module_index fc_index:funs] min_dep max_fun_nr fun_defs pi
					= visit_functions funs min_dep max_fun_nr fun_defs pi
				visit_functions [] min_dep max_fun_nr fun_defs pi
					= (min_dep, fun_defs, pi)
		= try_to_close_group fun_index pi_next_num min_dep max_fun_nr fun_defs pi

	push_on_dep_stack :: !Int !*PartitioningInfo -> *PartitioningInfo;
	push_on_dep_stack fun_index pi=:{pi_deps,pi_marks,pi_next_num}
		= { pi & pi_deps = [fun_index : pi_deps], pi_marks = { pi_marks & [fun_index] = pi_next_num}, pi_next_num = inc pi_next_num}

	try_to_close_group :: !Int !Int !Int !Int !*{# FunDef} !*PartitioningInfo -> *(!Int, !*{# FunDef}, !*PartitioningInfo)
	try_to_close_group fun_index fun_nr min_dep max_fun_nr fun_defs pi=:{pi_marks, pi_deps, pi_groups, pi_next_group}
		| fun_nr <= min_dep
			# (pi_deps, pi_marks, group, fun_defs)
				= close_group False False fun_index pi_deps pi_marks NoComponentMembers max_fun_nr pi_next_group fun_defs
			  pi = { pi & pi_deps = pi_deps, pi_marks = pi_marks, pi_next_group = inc pi_next_group,  pi_groups = [group : pi_groups] }
			= (max_fun_nr, fun_defs, pi)
			= (min_dep, fun_defs, pi)
	where
		close_group :: !Bool !Bool !Int ![Int] !*{# Int} !ComponentMembers !Int !Int !*{# FunDef} -> (![Int], !*{# Int}, !ComponentMembers, !*{# FunDef})
		close_group n_r_known non_recursive fun_index [d:ds] marks group max_fun_nr group_number fun_defs
			# marks = { marks & [d] = max_fun_nr }
			# (fd,fun_defs) = fun_defs![d]
			# non_recursive = case n_r_known of
								True	-> non_recursive
								_		-> case fun_index == d of
									True	-> isEmpty [fc \\ fc <- fd.fun_info.fi_calls | case fc of FunCall idx _ -> idx == d; _ -> False]
									_		-> False
			# fd = { fd & fun_info.fi_group_index = group_number, fun_info.fi_properties = set_rec_prop non_recursive fd.fun_info.fi_properties}
			# fun_defs = { fun_defs & [d] = fd}
			| d == fun_index
				= (ds, marks, ComponentMember d group, fun_defs)
				= close_group True non_recursive fun_index ds marks (ComponentMember d group) max_fun_nr group_number fun_defs

::	PartitioningInfo` = 
	{	pi_marks` :: 		!.{# Int}
	,	pi_next_num` ::		!Int
	,	pi_next_group` ::	!Int
	,	pi_groups` ::		![ComponentMembers]
	,	pi_deps` ::			![Int]
	,	pi_collect` ::		!.CollectState
	}

stripStrictLets :: !*{# FunDef} !*PredefinedSymbols !*VarHeap !*ExpressionHeap !*ErrorAdmin -> (!*{# FunDef}, !*PredefinedSymbols, !*VarHeap, !*ExpressionHeap, !*ErrorAdmin)
stripStrictLets fun_defs predef_symbols var_heap sym_heap error_admin
	# (cs_predef,predef_symbols) = get_predef_symbols_for_transform predef_symbols
	# collect_state =
		{ cos_predef_symbols_for_transform	= cs_predef
		, cos_var_heap						= var_heap
		, cos_expression_heap				= sym_heap
		, cos_error							= error_admin
		}
	# (fun_defs,collect_state) = aMapSt determine_ref_counts fun_defs collect_state
	= (fun_defs,predef_symbols,collect_state.cos_var_heap, collect_state.cos_expression_heap, collect_state.cos_error)
where
	aMapSt f a s
		# (l,s)	= mapSt f [e \\ e <-: a] s
		= ({e \\ e <- l},s)

partitionateFunctions` :: !*{# FunDef} ![IndexRange] !Index !Int !Int !*PredefinedSymbols !*VarHeap !*ExpressionHeap !*ErrorAdmin -> (!*{!Component}, !*{# FunDef}, !*PredefinedSymbols, !*VarHeap, !*ExpressionHeap, !*ErrorAdmin)
partitionateFunctions` fun_defs ranges main_dcl_module_n def_min def_max predef_symbols var_heap sym_heap error_admin
	#! max_fun_nr = size fun_defs
	# (cs_predef,predef_symbols) = get_predef_symbols_for_transform predef_symbols
	# collect_state =
		{ cos_predef_symbols_for_transform	= cs_predef
		, cos_var_heap						= var_heap
		, cos_expression_heap				= sym_heap
		, cos_error							= error_admin
		}
	# partitioning_info =
		{ pi_collect` = collect_state
		, pi_marks` = createArray max_fun_nr NotChecked
		, pi_deps` = []
		, pi_next_num` = 0
		, pi_next_group` = 0
		, pi_groups` = [] 
		}
	  (fun_defs, {pi_groups`,pi_next_group`,pi_collect`}) = 
	  		foldSt (partitionate_functions max_fun_nr) ranges (fun_defs, partitioning_info)
	  groups = { {component_members = group} \\ group <- reverse pi_groups` }
	= (groups, fun_defs, predef_symbols, pi_collect`.cos_var_heap, pi_collect`.cos_expression_heap, pi_collect`.cos_error)
where
	partitionate_functions :: !Index !IndexRange !(!*{# FunDef}, !*PartitioningInfo`) -> (!*{# FunDef}, !*PartitioningInfo`)
	partitionate_functions max_fun_nr ir=:{ir_from,ir_to} (fun_defs, pi=:{pi_marks`})
		| ir_from == ir_to
			= (fun_defs, pi)
		| pi_marks`.[ir_from] == NotChecked
			# (_, fun_defs, pi) = partitionate_function ir_from max_fun_nr fun_defs pi
			= partitionate_functions max_fun_nr { ir & ir_from = inc ir_from } (fun_defs, pi)
			= partitionate_functions max_fun_nr { ir & ir_from = inc ir_from } (fun_defs, pi)

	partitionate_function :: !Int !Int !*{# FunDef} !*PartitioningInfo` -> *(!Int, !*{# FunDef}, !*PartitioningInfo`)
	partitionate_function fun_index max_fun_nr fun_defs pi=:{pi_next_num`,pi_collect`}
		# (fd, fun_defs) = fun_defs![fun_index]
		# (fd,pi_collect`) = determine_ref_counts fd pi_collect`
		# pi = {pi & pi_collect` = pi_collect`}
		# fc_state = find_calls
						{ main_dcl_module_n=main_dcl_module_n
						, def_min=def_min
						, def_max=def_max
						, fun_index=fun_index
						} fd.fun_body {fun_calls = []}
		  fi_calls = fc_state.fun_calls
		  fd = {fd & fun_info.fi_calls = fi_calls}
		# fun_defs = {fun_defs & [fun_index] = fd}

		  pi = push_on_dep_stack fun_index pi
		  (min_dep, fun_defs, pi) = visit_functions fi_calls max_fun_nr max_fun_nr fun_defs pi
			with
				visit_functions :: ![FunCall] !Int !Int !*{# FunDef} !*PartitioningInfo` -> *(!Int, !*{# FunDef}, !*PartitioningInfo`)
				visit_functions [FunCall fc_index _:funs] min_dep max_fun_nr fun_defs pi=:{pi_marks`} 
					#! mark = pi_marks`.[fc_index]
					| mark == NotChecked
						# (mark, fun_defs, pi) = partitionate_function fc_index max_fun_nr fun_defs pi
						= visit_functions funs (min min_dep mark) max_fun_nr fun_defs pi
						= visit_functions funs (min min_dep mark) max_fun_nr fun_defs pi
				visit_functions [GeneratedFunCall fc_index _:funs] min_dep max_fun_nr fun_defs pi=:{pi_marks`}
					#! mark = pi_marks`.[fc_index]
					| mark == NotChecked
						# (mark, fun_defs, pi) = partitionate_function fc_index max_fun_nr fun_defs pi
						= visit_functions funs (min min_dep mark) max_fun_nr fun_defs pi
						= visit_functions funs (min min_dep mark) max_fun_nr fun_defs pi
				visit_functions [DclFunCall module_index fc_index:funs] min_dep max_fun_nr fun_defs pi
					= visit_functions funs min_dep max_fun_nr fun_defs pi
				visit_functions [] min_dep max_fun_nr fun_defs pi
					= (min_dep, fun_defs, pi)
		= try_to_close_group fun_index pi_next_num` min_dep max_fun_nr fun_defs pi

	push_on_dep_stack :: !Int !*PartitioningInfo` -> *PartitioningInfo`;
	push_on_dep_stack fun_index pi=:{pi_deps`,pi_marks`,pi_next_num`}
		= { pi & pi_deps` = [fun_index : pi_deps`], pi_marks` = { pi_marks` & [fun_index] = pi_next_num`}, pi_next_num` = inc pi_next_num`}

	try_to_close_group :: !Int !Int !Int !Int !*{# FunDef} !*PartitioningInfo` -> *(!Int, !*{# FunDef}, !*PartitioningInfo`)
	try_to_close_group fun_index fun_nr min_dep max_fun_nr fun_defs pi=:{pi_marks`, pi_deps`, pi_groups`, pi_next_group`}
		| fun_nr <= min_dep
			# (pi_deps`, pi_marks`, group, fun_defs)
				= close_group False False fun_index pi_deps` pi_marks` NoComponentMembers max_fun_nr pi_next_group` fun_defs
			  pi = { pi & pi_deps` = pi_deps`, pi_marks` = pi_marks`, pi_next_group` = inc pi_next_group`,  pi_groups` = [group : pi_groups`] }
			= (max_fun_nr, fun_defs, pi)
			= (min_dep, fun_defs, pi)
	where
		close_group :: !Bool !Bool !Int ![Int] !*{# Int} !ComponentMembers !Int !Int !*{# FunDef} -> (![Int], !*{# Int}, !ComponentMembers, !*{# FunDef})
		close_group n_r_known non_recursive fun_index [d:ds] marks group max_fun_nr group_number fun_defs
			# marks = { marks & [d] = max_fun_nr }
			# (fd,fun_defs) = fun_defs![d]
			# non_recursive = case n_r_known of
								True	-> non_recursive
								_		-> case fun_index == d of
									True	-> isEmpty [fc \\ fc <- fd.fun_info.fi_calls | case fc of FunCall idx _ -> idx == d; _ -> False]
									_		-> False
			# fd = { fd & fun_info.fi_group_index = group_number, fun_info.fi_properties = set_rec_prop non_recursive fd.fun_info.fi_properties}
			# fun_defs = { fun_defs & [d] = fd}
			| d == fun_index
				= (ds, marks, ComponentMember d group, fun_defs)
				= close_group True non_recursive fun_index ds marks (ComponentMember d group) max_fun_nr group_number fun_defs

::	PartitioningInfo`` = 
	{ pi_marks``			:: !.Marks
	, pi_next_num``			:: !Int
	, pi_next_group``		:: !Int
	, pi_groups``			:: ![ComponentMembers]
	, pi_deps``				:: !ComponentMembers
	, pi_collect``			:: !.CollectState
	}

:: Marks	:== {# Mark}
:: Mark		= { m_fun :: !Int, m_mark :: !Int}

create_marks max_fun_nr functions
	= {{m_fun = fun, m_mark = NotChecked} \\ fun <- component_members_to_list functions}

component_members_to_list (ComponentMember member members)
	= [member : component_members_to_list members]
component_members_to_list (GeneratedComponentMember member _ members)
	= [member : component_members_to_list members]
component_members_to_list NoComponentMembers
	= []

get_mark max_fun_nr marks fun
	:== get_mark 0 marks fun max_fun_nr
where
	get_mark :: !Int !{#Mark} !Int !Int -> Int
	get_mark i marks fun max_fun_nr
		| i<size marks
			| marks.[i].m_fun<>fun
				= get_mark (i+1) marks fun max_fun_nr
				= marks.[i].m_mark
			= max_fun_nr

set_mark marks fun val
	:== set_mark 0 marks fun val
where
	set_mark :: !Int !*{#Mark} !Int !Int -> *{#Mark}
	set_mark i marks fun val
//		| i<size marks
		| marks.[i].m_fun<>fun
			= set_mark (i+1) marks fun val
			= {marks & [i].m_mark=val}

partitionateFunctions`` :: !Int !Int !*{#FunDef} !ComponentMembers !Index !Int !Int !*FunctionHeap !*PredefinedSymbols !*VarHeap !*ExpressionHeap !*ErrorAdmin
	-> (!Int, ![Component], !*{#FunDef}, !*FunctionHeap, !*PredefinedSymbols, !*VarHeap, !*ExpressionHeap, !*ErrorAdmin)
partitionateFunctions`` max_fun_nr next_group fun_defs functions main_dcl_module_n def_min def_max fun_heap predef_symbols var_heap sym_heap error_admin
	# marks					= create_marks max_fun_nr functions
	# (cs_predef,predef_symbols) = get_predef_symbols_for_transform predef_symbols
	# collect_state =
		{ cos_predef_symbols_for_transform	= cs_predef
		, cos_var_heap						= var_heap
		, cos_expression_heap				= sym_heap
		, cos_error							= error_admin
		}
	# partitioning_info =
		{ pi_marks``		= marks
		, pi_deps``			= NoComponentMembers
		, pi_next_num``		= 0
		, pi_next_group``	= next_group
		, pi_groups``		= [] 
		, pi_collect``		= collect_state
		}
	  (fun_defs, fun_heap, {pi_groups``,pi_next_group``,pi_collect``})
	  	= partitionate_component functions max_fun_nr (fun_defs, fun_heap, partitioning_info)
	  groups = [ {component_members = group} \\ group <- reverse pi_groups`` ]
	= (pi_next_group``,groups, fun_defs, fun_heap, predef_symbols, pi_collect``.cos_var_heap, pi_collect``.cos_expression_heap, pi_collect``.cos_error)
where
	partitionate_component :: !ComponentMembers !Index !(!*{# FunDef}, !*FunctionHeap, !*PartitioningInfo``) -> (!*{# FunDef}, !*FunctionHeap, !*PartitioningInfo``)
	partitionate_component (ComponentMember member members) max_fun_nr (fun_defs, fun_heap, pi=:{pi_marks``})
		| get_mark max_fun_nr pi_marks`` member == NotChecked
			# (_, fun_defs, fun_heap, pi) = partitionate_function member max_fun_nr fun_defs fun_heap pi
		 	= partitionate_component members max_fun_nr (fun_defs, fun_heap, pi)
		 	= partitionate_component members max_fun_nr (fun_defs, fun_heap, pi)
	partitionate_component (GeneratedComponentMember member fun_ptr members) max_fun_nr (fun_defs, fun_heap, pi=:{pi_marks``})
		| get_mark max_fun_nr pi_marks`` member == NotChecked
			# (_, fun_defs, fun_heap, pi) = partitionate_generated_function member fun_ptr max_fun_nr fun_defs fun_heap pi
			= partitionate_component members max_fun_nr (fun_defs, fun_heap, pi)
			= partitionate_component members max_fun_nr (fun_defs, fun_heap, pi)
	partitionate_component NoComponentMembers max_fun_nr (fun_defs, fun_heap, pi)
		= (fun_defs, fun_heap, pi)

	partitionate_function :: !Int !Int !*{# FunDef} !*FunctionHeap !*PartitioningInfo`` -> *(!Int, !*{# FunDef}, !*FunctionHeap, !*PartitioningInfo``)
	partitionate_function fun_index max_fun_nr fun_defs fun_heap pi=:{pi_next_num``,pi_collect``}
		# (fd,fun_defs) = fun_defs![fun_index]
		  (fd,pi_collect``) = determine_ref_counts fd pi_collect``
		  pi = {pi & pi_collect`` = pi_collect``}
		  fc_state = find_calls {main_dcl_module_n=main_dcl_module_n, def_min=def_min, def_max=def_max, fun_index=fun_index} fd.fun_body {fun_calls = []}
		  fi_calls = fc_state.fun_calls
		  fd = {fd & fun_info.fi_calls = fi_calls}	
		  fun_defs = {fun_defs & [fun_index] = fd}
		  pi = push_on_dep_stack fun_index pi
		= visit_functions_and_try_to_close_group fi_calls fun_index pi_next_num`` max_fun_nr fun_defs fun_heap pi

	partitionate_generated_function :: !Int !FunctionInfoPtr !Int !*{# FunDef} !*FunctionHeap !*PartitioningInfo`` -> *(!Int, !*{# FunDef}, !*FunctionHeap, !*PartitioningInfo``)
	partitionate_generated_function fun_index fun_ptr max_fun_nr fun_defs fun_heap pi=:{pi_next_num``,pi_collect``}
		# (FI_Function gf=:{gf_fun_def=fd}, fun_heap) = readPtr fun_ptr fun_heap
		  (fd,pi_collect``) = determine_ref_counts fd pi_collect``
		  pi = {pi & pi_collect`` = pi_collect``}
		  fc_state = find_calls {main_dcl_module_n=main_dcl_module_n, def_min=def_min, def_max=def_max, fun_index=fun_index} fd.fun_body {fun_calls = []}
		  fi_calls = fc_state.fun_calls
		  fd = {fd & fun_info.fi_calls = fi_calls}
		  fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def = fd}) fun_heap
		  pi = push_generated_function_on_dep_stack fun_index fun_ptr pi
		= visit_functions_and_try_to_close_group fi_calls fun_index pi_next_num`` max_fun_nr fun_defs fun_heap pi

	visit_functions_and_try_to_close_group :: ![FunCall] !Int !Int !Int !*{#FunDef} !*FunctionHeap !*PartitioningInfo`` -> *(!Int,!*{#FunDef},!*FunctionHeap,!*PartitioningInfo``)
	visit_functions_and_try_to_close_group fi_calls fun_index pi_next_num`` max_fun_nr fun_defs fun_heap pi
		# (min_dep, fun_defs, fun_heap, pi) = visit_functions fi_calls max_fun_nr max_fun_nr fun_defs fun_heap pi
		= try_to_close_group fun_index pi_next_num`` min_dep max_fun_nr fun_defs fun_heap pi

	visit_functions :: ![FunCall] !Int !Int !*{# FunDef} !*FunctionHeap !*PartitioningInfo`` -> *(!Int, !*{# FunDef}, !*FunctionHeap, !*PartitioningInfo``)
	visit_functions [FunCall fc_index _:funs] min_dep max_fun_nr fun_defs fun_heap pi=:{pi_marks``} 
		#! mark = get_mark max_fun_nr pi_marks`` fc_index
		| mark == NotChecked
			# (mark, fun_defs, fun_heap, pi) = partitionate_function fc_index max_fun_nr fun_defs fun_heap pi
			= visit_functions funs (min min_dep mark) max_fun_nr fun_defs fun_heap pi
			= visit_functions funs (min min_dep mark) max_fun_nr fun_defs fun_heap pi
	visit_functions [GeneratedFunCall fc_index fun_ptr:funs] min_dep max_fun_nr fun_defs fun_heap pi=:{pi_marks``} 
		#! mark = get_mark max_fun_nr pi_marks`` fc_index
		| mark == NotChecked
			# (mark, fun_defs, fun_heap, pi) = partitionate_generated_function fc_index fun_ptr max_fun_nr fun_defs fun_heap pi
			= visit_functions funs (min min_dep mark) max_fun_nr fun_defs fun_heap pi
			= visit_functions funs (min min_dep mark) max_fun_nr fun_defs fun_heap pi
	visit_functions [DclFunCall module_index fc_index:funs] min_dep max_fun_nr fun_defs fun_heap pi
		= visit_functions funs min_dep max_fun_nr fun_defs fun_heap pi
	visit_functions [] min_dep max_fun_nr fun_defs fun_heap pi
		= (min_dep, fun_defs, fun_heap, pi)

	push_on_dep_stack :: !Int !*PartitioningInfo`` -> *PartitioningInfo``;
	push_on_dep_stack fun_index pi=:{pi_deps``,pi_marks``,pi_next_num``}
		= {pi & pi_deps`` = ComponentMember fun_index pi_deps``
			  , pi_marks`` = set_mark pi_marks`` fun_index pi_next_num``
			  , pi_next_num`` = inc pi_next_num`` }

	push_generated_function_on_dep_stack :: !Int !FunctionInfoPtr !*PartitioningInfo`` -> *PartitioningInfo``;
	push_generated_function_on_dep_stack fun_index fun_ptr pi=:{pi_deps``,pi_marks``,pi_next_num``}
		= {pi & pi_deps`` = GeneratedComponentMember fun_index fun_ptr pi_deps``
			  , pi_marks`` = set_mark pi_marks`` fun_index pi_next_num``
			  , pi_next_num`` = inc pi_next_num`` }

	try_to_close_group :: !Int !Int !Int !Int !*{# FunDef} !*FunctionHeap !*PartitioningInfo`` -> *(!Int, !*{# FunDef}, !*FunctionHeap, !*PartitioningInfo``)
	try_to_close_group fun_index fun_nr min_dep max_fun_nr fun_defs fun_heap pi=:{pi_marks``, pi_deps``, pi_groups``, pi_next_group``}
		| fun_nr <= min_dep
			# (pi_deps``, pi_marks``, group, fun_defs, fun_heap)
				= close_group False False fun_index pi_deps`` pi_marks`` NoComponentMembers max_fun_nr pi_next_group`` fun_defs fun_heap
			  pi = { pi & pi_deps`` = pi_deps``, pi_marks`` = pi_marks``, pi_next_group`` = inc pi_next_group``,  pi_groups`` = [group : pi_groups``] }
			= (max_fun_nr, fun_defs, fun_heap, pi)
			= (min_dep, fun_defs, fun_heap, pi)
	where
		close_group :: !Bool !Bool !Int !ComponentMembers !*Marks !ComponentMembers !Int !Int !*{# FunDef} !*FunctionHeap -> (!ComponentMembers, !*Marks, !ComponentMembers, !*{# FunDef}, !*FunctionHeap)
		close_group n_r_known non_recursive fun_index (ComponentMember d ds) marks group max_fun_nr group_number fun_defs fun_heap
			# marks = set_mark marks d max_fun_nr
			  (fun_info,fun_defs) = fun_defs![d].fun_info
			  non_recursive = determine_if_function_non_recursive n_r_known fun_index d fun_info.fi_calls non_recursive
			  fun_info = {fun_info & fi_group_index = group_number, fi_properties = set_rec_prop non_recursive fun_info.fi_properties}
			  fun_defs = {fun_defs & [d].fun_info = fun_info}
			| d == fun_index
				= (ds, marks, ComponentMember d group, fun_defs, fun_heap)
				= close_group True non_recursive fun_index ds marks (ComponentMember d group) max_fun_nr group_number fun_defs fun_heap
		close_group n_r_known non_recursive fun_index (GeneratedComponentMember d fun_ptr ds) marks group max_fun_nr group_number fun_defs fun_heap
			# marks = set_mark marks d max_fun_nr
			  (FI_Function gf=:{gf_fun_def={fun_info}}, fun_heap) = readPtr fun_ptr fun_heap
			  non_recursive = determine_if_function_non_recursive n_r_known fun_index d fun_info.fi_calls non_recursive
			  fun_info = {fun_info & fi_group_index = group_number, fi_properties = set_rec_prop non_recursive fun_info.fi_properties}
			  fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def.fun_info=fun_info}) fun_heap
			| d == fun_index
				= (ds, marks, GeneratedComponentMember d fun_ptr group, fun_defs, fun_heap)
				= close_group True non_recursive fun_index ds marks (GeneratedComponentMember d fun_ptr group) max_fun_nr group_number fun_defs fun_heap

		determine_if_function_non_recursive :: !Bool !Index !Index ![FunCall] !Bool -> Bool
		determine_if_function_non_recursive n_r_known fun_index d fi_calls non_recursive
			| n_r_known
				= non_recursive
				| fun_index == d
					= isEmpty [fc \\ fc <- fi_calls
									| case fc of FunCall idx _ -> idx == d; GeneratedFunCall idx _ -> idx == d; _ -> False]
					= False

:: FindCallsInfo =
	{ main_dcl_module_n	:: !Index
	, def_min			:: !Int
	, def_max			:: !Int
	, fun_index			:: !Int
	}

:: FindCallsState =
	{ fun_calls			:: ![FunCall]
	}

class find_calls a :: !FindCallsInfo !a !FindCallsState -> FindCallsState

instance find_calls [a] | find_calls a
where
	find_calls fc_info els fc_state = foldSt (find_calls fc_info) els fc_state

instance find_calls (Optional a) | find_calls a
where
	find_calls fc_info (Yes e) fc_state = find_calls fc_info e fc_state
	find_calls fc_info No fc_state = fc_state

instance find_calls FunctionBody
where
	find_calls fc_info (TransformedBody tb) fc_state
		= find_calls fc_info tb fc_state
//	find_calls fc_info NoBody fc_state = fc_state
	find_calls fc_info _ fc_state = abort ("Undefined pattern in FunctionBody: "+++toString fc_info.fun_index+++ "?" +++ toString fc_info.def_min+++ "?" +++ toString fc_info.def_max +++ "\n")

instance find_calls TransformedBody
where
	find_calls fc_info {tb_rhs} fc_state = find_calls fc_info tb_rhs fc_state

instance find_calls Expression
where
	find_calls fc_info (Var _)					fc_state = fc_state
	find_calls fc_info (App app)				fc_state = find_calls fc_info app fc_state
	find_calls fc_info (exp @ exps)				fc_state = find_calls fc_info exps (find_calls fc_info exp fc_state)
	find_calls fc_info (Let lete)				fc_state = find_calls fc_info lete fc_state
	find_calls fc_info (Case kees)				fc_state = find_calls fc_info kees fc_state
	find_calls fc_info (Selection _ exp sells)	fc_state = find_calls fc_info sells (find_calls fc_info exp fc_state)
	find_calls fc_info (Update e1 sl e2)		fc_state
		#! fc_state	= find_calls fc_info e1 fc_state
		   fc_state	= find_calls fc_info sl fc_state
		= find_calls fc_info e2 fc_state
	find_calls fc_info (RecordUpdate _ expr bexps) fc_state
		#! fc_state	= find_calls fc_info expr fc_state
		= find_calls fc_info (map (\{bind_src} -> bind_src) bexps) fc_state
	find_calls fc_info (TupleSelect _ _ expr) fc_state
		= find_calls fc_info expr fc_state
	find_calls fc_info (BasicExpr _) fc_state
		= fc_state
	find_calls fc_info (AnyCodeExpr _ _ _) fc_state
		= fc_state
	find_calls fc_info (ABCCodeExpr _ _) fc_state
		= fc_state
	find_calls fc_info (MatchExpr _ expr) fc_state
		= find_calls fc_info expr fc_state
	find_calls fc_info (IsConstructor expr _ _ _ _ _) fc_state
		= find_calls fc_info expr fc_state
	find_calls fc_info EE fc_state
		= fc_state
	find_calls fc_info (NoBind _) fc_state
		= fc_state
	find_calls fc_info (FailExpr _) fc_state
		= fc_state
	find_calls fc_info (DictionariesFunction dictionaries expr expr_type) fc_state
		= find_calls fc_info expr fc_state
	find_calls fc_info ExprToBeRemoved fc_state
		= fc_state

instance find_calls App
where
	find_calls fc_info {app_symb,app_args} fc_state
		#! fc_state = get_index app_symb.symb_kind fc_state
		= find_calls fc_info app_args fc_state
	where
		get_index (SK_Function {glob_object,glob_module}) fc_state
			| fc_info.main_dcl_module_n == glob_module && (glob_object < fc_info.def_max || glob_object >= fc_info.def_min)
				= {fc_state & fun_calls = [FunCall glob_object 0: fc_state.fun_calls]}
				= {fc_state & fun_calls = [DclFunCall glob_module glob_object: fc_state.fun_calls]}
		get_index (SK_Constructor idx) fc_state
			= fc_state
		get_index (SK_LocalMacroFunction idx) fc_state
			= {fc_state & fun_calls = [FunCall idx 0: fc_state.fun_calls]}
		get_index (SK_GeneratedFunction fun_ptr idx) fc_state
			= {fc_state & fun_calls = [GeneratedFunCall idx fun_ptr : fc_state.fun_calls]}

instance find_calls Let
where
	find_calls fc_info {let_strict_binds,let_lazy_binds,let_expr} fc_state
		= find_calls fc_info (let_strict_binds++let_lazy_binds) (find_calls fc_info let_expr fc_state)

instance find_calls Case
where
	find_calls fc_info {case_expr,case_guards,case_default} fc_state
		#! fc_state	= find_calls fc_info case_expr fc_state
		   fc_state	= find_calls fc_info case_default fc_state
		= find_calls fc_info case_guards fc_state

instance find_calls Selection
where
	find_calls fc_info (RecordSelection _ _) fc_state
		= fc_state
	find_calls fc_info (ArraySelection _ _ expr) fc_state
		= find_calls fc_info expr fc_state
	find_calls fc_info (DictionarySelection _ sells _ expr) fc_state
		= find_calls fc_info expr (find_calls fc_info sells fc_state)
	find_calls _ u _ = abort "Undefined pattern in Selection\n"

instance find_calls LetBind
where
	find_calls fc_info {lb_src} fc_state
		= find_calls fc_info lb_src fc_state

instance find_calls CasePatterns
where
	find_calls fc_info (AlgebraicPatterns _ pats) fc_state
		= find_calls fc_info pats fc_state
	find_calls fc_info (BasicPatterns _ pats) fc_state
		= find_calls fc_info pats fc_state
	find_calls fc_info (DynamicPatterns pats) fc_state
		= find_calls fc_info pats fc_state
	find_calls fc_info (OverloadedListPatterns _ expr pats) fc_state
		= find_calls fc_info pats (find_calls fc_info expr fc_state)
	find_calls fc_info (NoPattern) fc_state
		= fc_state
	find_calls _ u _ = abort "Undefined pattern in CasePatterns\n"

instance find_calls AlgebraicPattern
where
	find_calls fc_info {ap_expr} fc_state
		= find_calls fc_info ap_expr fc_state

instance find_calls BasicPattern
where
	find_calls fc_info {bp_expr} fc_state
		= find_calls fc_info bp_expr fc_state

instance find_calls DynamicPattern
where
	find_calls fc_info {dp_rhs} fc_state
		= find_calls fc_info dp_rhs fc_state

determine_ref_counts fd=:{fun_body=TransformedBody {tb_args,tb_rhs}} pi_collect
	# (new_rhs, new_args, _, _, pi_collect) = determineVariablesAndRefCounts tb_args tb_rhs pi_collect
	# fd = {fd & fun_body=TransformedBody {tb_args=new_args,tb_rhs=new_rhs}}
	= (fd,pi_collect)
determine_ref_counts fd pi_collect
	= (fd, pi_collect)

// from check.icl

get_predef_symbols_for_transform :: *PredefinedSymbols -> (!PredefSymbolsForTransform,!.PredefinedSymbols)
// clean 2.0 does not allow this, clean 1.3 does:
// get_predef_symbols_for_transform cs_predef_symbols=:{[PD_DummyForStrictAliasFun]=predef_alias_dummy,[PD_AndOp]=predef_and,[PD_OrOp]=predef_or}
get_predef_symbols_for_transform cs_predef_symbols
	# (predef_alias_dummy,cs_predef_symbols) = cs_predef_symbols![PD_DummyForStrictAliasFun]
	# (predef_and,cs_predef_symbols) = cs_predef_symbols![PD_AndOp]
	# (predef_or,cs_predef_symbols) = cs_predef_symbols![PD_OrOp]
	= ({predef_alias_dummy=predef_alias_dummy,predef_and=predef_and,predef_or=predef_or},cs_predef_symbols)

dummy_predef_symbol =
	{ pds_module	= 0
	, pds_def		= 0
	}

dummy_predef_symbols =
	{ predef_alias_dummy	= dummy_predef_symbol
	, predef_and			= dummy_predef_symbol
	, predef_or				= dummy_predef_symbol
	}

set_rec_prop non_recursive fi_properties
	| non_recursive
		= fi_properties bitor FI_IsNonRecursive
		= fi_properties bitand (bitnot FI_IsNonRecursive)
