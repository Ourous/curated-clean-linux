implementation module convertcases

import StdStrictLists
import syntax, compare_types, utilities, expand_types, general
from checksupport import ::Component(..),::ComponentMembers(..)

:: VarInfo
	| VI_LetVar !LetVarInfo
	| VI_LetExpression !LetExpressionInfo

:: ExprInfo
	| EI_CaseTypeAndRefCounts !CaseType !RefCountsInCase
	| EI_CaseTypeAndSplits !CaseType !SplitsInCase

// exactZip fails when its arguments are of unequal length
exactZip :: ![.a] ![.b] -> [(.a,.b)]
exactZip [x:xs][y:ys]
	=	[(x,y) : exactZip xs ys]
exactZip [] []
	=	[]

getIdent :: (Optional Ident) Int -> Ident
getIdent (Yes ident) fun_nr
	= ident
getIdent No fun_nr
	= { id_name = "_f" +++ toString fun_nr, id_info = nilPtr }

addLetVars :: [LetBind] [AType] [(FreeVar, AType)] -> [(FreeVar, AType)]
addLetVars [{lb_dst} : binds] [bind_type : bind_types] bound_vars
	= addLetVars binds bind_types [ (lb_dst, bind_type) : bound_vars ]
addLetVars [] [] bound_vars
	= bound_vars

convertCasesOfFunctions :: !*{!Component} !Int !{#{#FunType}} !{#CommonDefs}
				!*{#FunDef} !*{#{#CheckedTypeDef}} !ImportedConstructors !*VarHeap !*TypeHeaps !*ExpressionHeap
			-> (!ImportedFunctions, !*{!Component},
				!*{#FunDef},!*{#{#CheckedTypeDef}},!ImportedConstructors,!*VarHeap,!*TypeHeaps,!*ExpressionHeap)
convertCasesOfFunctions groups main_dcl_module_n dcl_functions common_defs fun_defs imported_types imported_conses var_heap type_heaps expr_heap
	#! nr_of_funs = size fun_defs
	# (groups, (fun_defs, collected_imports, {cs_new_functions, cs_var_heap, cs_expr_heap, cs_fun_heap}))
			= convert_groups 0 groups dcl_functions common_defs main_dcl_module_n
				(fun_defs, [], { cs_new_functions = [], cs_fun_heap = newHeap, cs_var_heap = var_heap, cs_expr_heap = expr_heap, cs_next_fun_nr = nr_of_funs })
	  (groups, new_fun_defs, imported_types, imported_conses, type_heaps, cs_var_heap)
			= addNewFunctionsToGroups common_defs cs_fun_heap cs_new_functions main_dcl_module_n groups imported_types imported_conses type_heaps cs_var_heap
	  (imported_functions, imported_conses) = foldSt split collected_imports ([], imported_conses)
	= (imported_functions, groups, { fundef \\ fundef <- [ fundef \\ fundef <-: fun_defs ] ++ new_fun_defs },
			imported_types, imported_conses, cs_var_heap, type_heaps, cs_expr_heap)
where
	convert_groups group_nr groups dcl_functions common_defs main_dcl_module_n fun_defs_and_ci
		| group_nr == size groups
			= (groups, fun_defs_and_ci)
		// otherwise
			# (group, groups) = groups![group_nr]
			= convert_groups (inc group_nr) groups dcl_functions common_defs main_dcl_module_n
				(convert_functions group.component_members group_nr dcl_functions common_defs main_dcl_module_n fun_defs_and_ci)

	convert_functions (ComponentMember member members) group_nr dcl_functions common_defs main_dcl_module_n fun_defs_and_ci
		# fun_defs_and_ci = convert_function group_nr dcl_functions common_defs main_dcl_module_n member fun_defs_and_ci
		= convert_functions members group_nr dcl_functions common_defs main_dcl_module_n fun_defs_and_ci
	convert_functions (GeneratedComponentMember member _ members) group_nr dcl_functions common_defs main_dcl_module_n fun_defs_and_ci
		# fun_defs_and_ci = convert_function group_nr dcl_functions common_defs main_dcl_module_n member fun_defs_and_ci
		= convert_functions members group_nr dcl_functions common_defs main_dcl_module_n fun_defs_and_ci
	convert_functions NoComponentMembers group_nr dcl_functions common_defs main_dcl_module_n fun_defs_and_ci
		= fun_defs_and_ci

	convert_function :: !Index !{#{#FunType}} !{#CommonDefs} !Int !Int
				!*(!*{#FunDef}, ![SymbKind], !*ConvertState)
			->   *(!*{#FunDef}, ![SymbKind], !*ConvertState)
	convert_function group_index dcl_functions common_defs main_dcl_module_n fun (fun_defs, collected_imports, cs)
		# ({fun_body,fun_type}, fun_defs) = fun_defs![fun]
		  (fun_body, (collected_imports, cs))
				= eliminate_code_sharing_in_function dcl_functions main_dcl_module_n common_defs fun_body (collected_imports, cs)
		  (fun_body, cs) = convertCasesInBody fun_body fun_type group_index common_defs cs
		= ({fun_defs & [fun].fun_body = fun_body}, collected_imports, cs)

	eliminate_code_sharing_in_function dcl_functions main_dcl_module_n common_defs (TransformedBody body=:{tb_rhs}) (collected_imports, cs=:{cs_expr_heap,cs_var_heap})
		# {rcs_var_heap, rcs_expr_heap, rcs_imports} = weightedRefCount {rci_imported = {cii_dcl_functions=dcl_functions, cii_common_defs=common_defs, cii_main_dcl_module_n = main_dcl_module_n}, rci_depth=1} tb_rhs
				{ rcs_var_heap = cs_var_heap, rcs_expr_heap = cs_expr_heap, rcs_free_vars = [],
				  rcs_imports = collected_imports} 
		  ds = { ds_lets = [], ds_var_heap = rcs_var_heap, ds_expr_heap = rcs_expr_heap}
		  (tb_rhs, ds) = distributeLets {di_depth=1,di_explicit_case_depth=0} tb_rhs ds
		  (tb_rhs, {ds_var_heap, ds_expr_heap}) = buildLetExpr tb_rhs ds
		  {ss_expr_heap, ss_var_heap}
		  	= findSplitCases {si_next_alt=No, si_force_next_alt=False} tb_rhs {ss_var_heap=ds_var_heap, ss_expr_heap = ds_expr_heap}
		= (TransformedBody {body & tb_rhs = tb_rhs }, (rcs_imports, {cs & cs_var_heap = ss_var_heap, cs_expr_heap = ss_expr_heap}))

	split :: SymbKind (ImportedFunctions, ImportedConstructors) -> (ImportedFunctions, ImportedConstructors) 
	split (SK_Function fun_ident) (collected_functions, collected_conses)
		= ([fun_ident : collected_functions], collected_conses)
	split (SK_Constructor cons_ident) (collected_functions, collected_conses)
		= (collected_functions, [ cons_ident : collected_conses])

:: CaseLevel = CaseLevelRoot | CaseLevelAfterGuardRoot

:: ConvertInfo =
	{	ci_bound_vars :: ![(FreeVar, AType)]
	,	ci_group_index :: !Index
	,	ci_common_defs :: !{#CommonDefs}
	,	ci_case_level	:: !CaseLevel
	}

convertCasesInBody :: FunctionBody (Optional SymbolType) Int {#CommonDefs} *ConvertState -> (FunctionBody,  *ConvertState)
convertCasesInBody (TransformedBody body) (Yes type) group_index common_defs cs
	# (body, cs) = convertRootCases
						{	ci_bound_vars = exactZip body.tb_args type.st_args
						,	ci_group_index = group_index
						,	ci_common_defs = common_defs
						,	ci_case_level=CaseLevelRoot
						}
						body cs
	= (TransformedBody body, cs)

/*
	weightedRefCount determines the reference counts of variables in an expr. Runtime behaviour
	of constructs is taken into account:  multiple occurrences of variables in different
	alternatives of the same case clause are counted only once. The outcome is used to distribute
	shared exprs (via let declarations) over cases. In this way code sharing is eliminated.
	As a side effect, weightedRefCount returns a list of all imported functions that have been used
	inside the expr.
*/

::	CheckImportedInfo =
	{	cii_dcl_functions	:: !{# {# FunType} }
	,	cii_common_defs		:: !{# CommonDefs}
	,	cii_main_dcl_module_n :: !Int
	}

::	RCInfo =
	{	rci_imported	:: !CheckImportedInfo
	,	rci_depth		:: !Int
	}

::	RCState =
	{	rcs_free_vars	:: ![VarInfoPtr]
	,	rcs_imports		:: ![SymbKind]
	,	rcs_var_heap		:: !.VarHeap
	,	rcs_expr_heap	:: !.ExpressionHeap
	}

::	LetVarInfo =
	{	lvi_count		:: !Int
	,	lvi_depth		:: !Int
	,	lvi_new			:: !Bool
	,	lvi_var			:: !Ident
	,	lvi_expression	:: !Expression	
	,   lvi_previous	:: ![PreviousLetVarInfo]
	}

::	PreviousLetVarInfo =
	{	plvi_count		:: !Int
	,	plvi_depth		:: !Int
	,	plvi_new		:: !Bool
	}

::	RefCountsInCase = 
	{	rcc_all_variables		:: ![CountedVariable]
	,	rcc_default_variables	:: ![CountedVariable]
	,	rcc_pattern_variables	:: ![[CountedVariable]]
	}

::	CountedVariable =
	{	cv_variable	:: !VarInfoPtr
	,	cv_count	:: !Int
	}

checkImportedSymbol :: SymbKind VarInfoPtr ([SymbKind], *VarHeap) -> ([SymbKind], *VarHeap)
checkImportedSymbol symb_kind symb_type_ptr (collected_imports, var_heap)
	# (type_info, var_heap) = readPtr symb_type_ptr var_heap
	= case type_info of
		VI_Used
			-> (collected_imports, var_heap)
		_
			-> ([symb_kind : collected_imports ], var_heap <:= (symb_type_ptr, VI_Used))

weightedRefCountOfVariable depth var_info_ptr lvi=:{lvi_count,lvi_var,lvi_depth,lvi_previous,lvi_new} ref_count new_vars
	| lvi_depth < depth
		= (True, {lvi & lvi_count = ref_count, lvi_depth = depth, lvi_new = True, lvi_previous =
				[{plvi_count = lvi_count, plvi_depth = lvi_depth, plvi_new = lvi_new } : lvi_previous]}, [var_info_ptr : new_vars])
	| lvi_count == 0
		= (True, { lvi & lvi_count = ref_count }, [var_info_ptr : new_vars])
	// otherwise
		= (lvi_new, { lvi & lvi_count = lvi_count + ref_count }, new_vars)

class weightedRefCount e :: RCInfo !e !*RCState -> *RCState

instance weightedRefCount BoundVar
where
	weightedRefCount rci=:{rci_depth} {var_ident,var_info_ptr} rs=:{rcs_var_heap}
		# (var_info, rcs_var_heap) = readPtr var_info_ptr rcs_var_heap
		  rs = {rs & rcs_var_heap = rcs_var_heap}
		= case var_info of
			VI_LetVar lvi
				# (is_new, lvi=:{lvi_expression}, rcs_free_vars) = weightedRefCountOfVariable rci_depth var_info_ptr lvi 1 rs.rcs_free_vars
				| is_new
					# rs = weightedRefCount rci lvi_expression
							{ rs & rcs_free_vars = rcs_free_vars,
							  rcs_var_heap = rs.rcs_var_heap  <:= (var_info_ptr, VI_LetVar {lvi & lvi_expression = EE, lvi_new = False})}
					  (VI_LetVar lvi, rcs_var_heap) = readPtr var_info_ptr rs.rcs_var_heap
					-> { rs & rcs_var_heap = rcs_var_heap <:= (var_info_ptr, VI_LetVar { lvi & lvi_expression = lvi_expression }) }
				// otherwise
					-> { rs & rcs_var_heap = rs.rcs_var_heap <:= (var_info_ptr, VI_LetVar lvi) }
			_
				-> rs

instance weightedRefCount Expression
where
	weightedRefCount rci (Var var) rs
		= weightedRefCount rci var rs
	weightedRefCount rci (App app) rs
		= weightedRefCount rci app rs
	weightedRefCount rci (fun_expr @ exprs) rs
		= weightedRefCount rci (fun_expr, exprs) rs
	weightedRefCount rci=:{rci_depth} (Let {let_strict_binds,let_lazy_binds,let_expr, let_info_ptr}) rs =:{rcs_var_heap}
		# rs = weightedRefCount rci let_strict_binds { rs & rcs_var_heap = foldSt (store_binding rci_depth) let_lazy_binds rcs_var_heap }
		  rs = weightedRefCount rci let_expr rs
		  (let_info, rcs_expr_heap) = readPtr let_info_ptr rs.rcs_expr_heap
		  rs = { rs & rcs_expr_heap = rcs_expr_heap }
		= case let_info of
			EI_LetType let_type
		  		# (ref_counts, rcs_var_heap) = mapSt get_ref_count let_lazy_binds rs.rcs_var_heap
				  (rcs_free_vars, rcs_var_heap) = foldl remove_variable (rs.rcs_free_vars, rcs_var_heap) let_lazy_binds
				-> { rs & rcs_free_vars = rcs_free_vars, rcs_var_heap = rcs_var_heap,
						rcs_expr_heap = rs.rcs_expr_heap <:= (let_info_ptr, EI_LetTypeAndRefCounts let_type ref_counts)}
			_
				# (rcs_free_vars, rcs_var_heap) = foldl remove_variable (rs.rcs_free_vars, rs.rcs_var_heap) let_lazy_binds
				-> { rs & rcs_free_vars = rcs_free_vars, rcs_var_heap = rcs_var_heap }
	where
		remove_variable ([], var_heap) let_bind
			= ([], var_heap)
		remove_variable ([var_ptr : var_ptrs], var_heap) bind=:{lb_dst={fv_ident,fv_info_ptr}}
			| fv_info_ptr == var_ptr
				# (VI_LetVar {lvi_count,lvi_depth}, var_heap) = readPtr fv_info_ptr var_heap
				= (var_ptrs, var_heap) 
			// otherwise
				# (var_ptrs, var_heap) = remove_variable (var_ptrs, var_heap) bind
				= ([var_ptr : var_ptrs], var_heap)

		store_binding depth {lb_dst={fv_ident,fv_info_ptr},lb_src} var_heap
			= var_heap <:= (fv_info_ptr, VI_LetVar {lvi_count = 0, lvi_depth = depth, lvi_previous = [],
													lvi_new = True, lvi_expression = lb_src, lvi_var = fv_ident})

		get_ref_count {lb_dst={fv_ident,fv_info_ptr}} var_heap 
			# (VI_LetVar {lvi_count}, var_heap) = readPtr fv_info_ptr var_heap
		  	= (lvi_count, var_heap)

	weightedRefCount rci (Case case_expr) rs=:{rcs_expr_heap}
		# (case_info, rcs_expr_heap) = readPtr case_expr.case_info_ptr rcs_expr_heap
		= weightedRefCountOfCase rci case_expr case_info {rs & rcs_expr_heap = rcs_expr_heap}
	weightedRefCount rci expr=:(BasicExpr _) rs
		= rs
	weightedRefCount rci (Selection opt_tuple expr selections) rs
		= weightedRefCount rci (expr, selections) rs
	weightedRefCount rci (Update expr1 selections expr2) rs
		= weightedRefCount rci (expr1, (selections, expr2)) rs
	weightedRefCount rci (RecordUpdate cons_symbol expr exprs) rs
		= weightedRefCount rci (expr, exprs) rs
	weightedRefCount rci (TupleSelect tuple_symbol arg_nr expr) rs
		= weightedRefCount rci expr rs
	weightedRefCount rci (MatchExpr constructor expr) rs
		= weightedRefCount rci expr rs
	weightedRefCount rci (IsConstructor expr _ _ _ _ _) rs
		= weightedRefCount rci expr rs
	weightedRefCount rci (AnyCodeExpr _ _ _) rs
		= rs
	weightedRefCount rci (ABCCodeExpr _ _) rs
		= rs
	weightedRefCount rci (TypeCodeExpression type_code_expr) rs
		= weightedRefCount rci type_code_expr rs
	weightedRefCount rci EE rs
		= rs
	weightedRefCount rci (NoBind ptr) rs
		= rs
	weightedRefCount rci (DictionariesFunction _ expr _) rs
		= weightedRefCount rci expr rs
	weightedRefCount rci (FailExpr _) rs
		= rs
	weightedRefCount rci expr rs
		= abort "weightedRefCount [Expression] (convertcases)"

addPatternVariable depth {cv_variable = var_info_ptr, cv_count = ref_count} (free_vars, var_heap)
 	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_LetVar lvi
			# (_, lvi, free_vars) = weightedRefCountOfVariable depth var_info_ptr lvi ref_count free_vars
			-> (free_vars, var_heap <:= (var_info_ptr, VI_LetVar lvi))
		_
			-> (free_vars, var_heap)

weightedRefCountOfCase rci=:{rci_depth} this_case=:{case_expr, case_guards, case_default, case_info_ptr} (EI_CaseType case_type)
			rs=:{ rcs_var_heap, rcs_expr_heap, rcs_imports }
	# (local_vars, vars_and_heaps) = weighted_ref_count_in_case_patterns {rci & rci_depth=rci_depth+1} case_guards rcs_imports rcs_var_heap rcs_expr_heap
	  (default_vars, (all_vars, rcs_imports, var_heap, expr_heap)) = weighted_ref_count_in_default {rci & rci_depth=rci_depth+1} case_default vars_and_heaps
	  rs = { rs & rcs_var_heap = var_heap, rcs_expr_heap = expr_heap, rcs_imports = rcs_imports }
	  rs = weighted_ref_count_of_decons_expr rci case_guards rs
	  rs = weightedRefCount rci case_expr rs
	  (rcs_free_vars, rcs_var_heap) = foldSt (addPatternVariable rci_depth) all_vars (rs.rcs_free_vars, rs.rcs_var_heap)
	  rcs_expr_heap = rs.rcs_expr_heap <:= (case_info_ptr, EI_CaseTypeAndRefCounts case_type 
	  		{ rcc_all_variables = all_vars, rcc_default_variables = default_vars, rcc_pattern_variables = local_vars })
	= {rs & rcs_var_heap = rcs_var_heap, rcs_expr_heap = rcs_expr_heap, rcs_free_vars = rcs_free_vars}
	where
		weighted_ref_count_in_default rci (Yes expr) info
			= weightedRefCountInPatternExpr rci expr info
		weighted_ref_count_in_default rci No info
			= ([], info)

		weighted_ref_count_in_case_patterns rci (AlgebraicPatterns type patterns) collected_imports var_heap expr_heap
			= mapSt (weighted_ref_count_in_algebraic_pattern rci) patterns ([], collected_imports, var_heap, expr_heap)
		weighted_ref_count_in_case_patterns rci (BasicPatterns type patterns) collected_imports var_heap expr_heap
			= mapSt (\{bp_expr} -> weightedRefCountInPatternExpr rci bp_expr) patterns ([], collected_imports, var_heap, expr_heap)
		weighted_ref_count_in_case_patterns rci (OverloadedListPatterns type _ patterns) collected_imports var_heap expr_heap
			= mapSt (weighted_ref_count_in_algebraic_pattern rci) patterns ([], collected_imports, var_heap, expr_heap)
		weighted_ref_count_in_case_patterns rci (DynamicPatterns patterns) collected_imports var_heap expr_heap
			= mapSt (\{dp_rhs} -> weightedRefCountInPatternExpr rci dp_rhs) patterns ([], collected_imports, var_heap, expr_heap)

		weighted_ref_count_in_algebraic_pattern rci=:{rci_imported} {ap_expr,ap_symbol} wrcs_state
			# (free_vars_with_rc, (all_free_vars, collected_imports, var_heap, expr_heap))
				= weightedRefCountInPatternExpr rci ap_expr wrcs_state
			  (collected_imports, var_heap)
				=	check_symbol rci_imported ap_symbol collected_imports var_heap
			=	(free_vars_with_rc, (all_free_vars, collected_imports, var_heap, expr_heap))
			where
				check_symbol {cii_main_dcl_module_n, cii_common_defs} {glob_module, glob_object={ds_index}} collected_imports var_heap
					| glob_module <> cii_main_dcl_module_n
						# {cons_type_ptr} = cii_common_defs.[glob_module].com_cons_defs.[ds_index]
						= checkImportedSymbol (SK_Constructor {glob_module = glob_module, glob_object = ds_index})
												cons_type_ptr (collected_imports, var_heap)
						= (collected_imports, var_heap)

	 	weighted_ref_count_of_decons_expr rci (OverloadedListPatterns _ decons_exp _) rs
	 		= weightedRefCount rci decons_exp rs;
	 	weighted_ref_count_of_decons_expr rci case_guards rs
	 		= rs;

weightedRefCountOfCase rci=:{rci_depth} this_case=:{case_expr, case_guards, case_default, case_info_ptr} (EI_CaseTypeAndRefCounts case_type {rcc_all_variables})
			rs=:{ rcs_var_heap, rcs_expr_heap, rcs_imports }
	# rs = weightedRefCount rci case_expr rs
	  (rcs_free_vars, rcs_var_heap) = foldSt (addPatternVariable rci_depth) rcc_all_variables (rs.rcs_free_vars, rs.rcs_var_heap)
	= {rs & rcs_var_heap = rcs_var_heap, rcs_free_vars = rcs_free_vars}

instance weightedRefCount Selection
where
	weightedRefCount rci=:{rci_imported} (ArraySelection {glob_module,glob_object={ds_index}} _ index_expr) rs
		# rs = weightedRefCount rci index_expr rs
		= checkImportOfDclFunction rci_imported glob_module ds_index rs
	weightedRefCount rci (DictionarySelection _ selectors _ index_expr) rs
		# rs = weightedRefCount rci index_expr rs
		= weightedRefCount rci selectors rs
	weightedRefCount {rci_imported} (RecordSelection selector _) rs
		= checkRecordSelector rci_imported selector rs

weightedRefCountInPatternExpr rci=:{rci_depth} pattern_expr (previous_free_vars, collected_imports, var_heap, expr_heap)
	# {rcs_free_vars,rcs_var_heap,rcs_imports,rcs_expr_heap} = weightedRefCount rci pattern_expr
				{ rcs_var_heap = var_heap, rcs_expr_heap = expr_heap, rcs_free_vars = [], rcs_imports = collected_imports}
	  (free_vars_with_rc, rcs_var_heap) = mapSt get_ref_count rcs_free_vars rcs_var_heap
	  (previous_free_vars, rcs_var_heap) = foldSt (select_unused_free_variable rci_depth) previous_free_vars ([], rcs_var_heap)
	  (all_free_vars, rcs_var_heap) = foldSt (collect_free_variable rci_depth) rcs_free_vars (previous_free_vars, rcs_var_heap)
	= (free_vars_with_rc, (all_free_vars, rcs_imports, rcs_var_heap, rcs_expr_heap))
where
	select_unused_free_variable depth var=:{cv_variable = var_ptr, cv_count = var_count} (collected_vars, var_heap)
		# (VI_LetVar info=:{lvi_count,lvi_depth}, var_heap) = readPtr var_ptr var_heap
		| lvi_depth == depth && lvi_count > 0
			= (collected_vars, var_heap <:= (var_ptr, VI_LetVar {info & lvi_count = max lvi_count var_count}))
		// otherwise
			= ([ var : collected_vars], var_heap) 

	get_ref_count var_ptr var_heap
		# (VI_LetVar {lvi_count}, var_heap) = readPtr var_ptr var_heap
		= ({cv_variable = var_ptr, cv_count = lvi_count}, var_heap)

	collect_free_variable depth var_ptr (collected_vars, var_heap)
		# (VI_LetVar lvi=:{lvi_count,lvi_depth,lvi_previous}, var_heap) = readPtr var_ptr var_heap
		| depth == lvi_depth
			= case lvi_previous of
				[{plvi_depth, plvi_count, plvi_new} : lvi_previous ]
					-> ([ {cv_variable = var_ptr, cv_count = lvi_count} : collected_vars ],
						(var_heap <:= (var_ptr, VI_LetVar {lvi & lvi_count = plvi_count, lvi_depth = plvi_depth,
																 lvi_new = plvi_new, lvi_previous = lvi_previous})))
				[]
					-> (collected_vars, var_heap)
			= ([ {cv_variable = var_ptr, cv_count = lvi_count} : collected_vars ], var_heap)

/*
	Here we examine the appplication to see whether an imported function has been used. If so,
	the 'ft_type_ptr' is examined. Initially this pointer contains VI_Empty. After the first
	occurrence the pointer will be set to 'VI_Used'.
*/
checkImportOfDclFunction :: CheckImportedInfo Int Int *RCState -> *RCState
checkImportOfDclFunction {cii_main_dcl_module_n, cii_dcl_functions} mod_index fun_index rs=:{rcs_imports, rcs_var_heap}
	| mod_index <> cii_main_dcl_module_n
		# {ft_type_ptr} = cii_dcl_functions.[mod_index].[fun_index]
		  (rcs_imports, rcs_var_heap) = checkImportedSymbol (SK_Function {glob_module=mod_index,glob_object=fun_index}) ft_type_ptr (rcs_imports, rcs_var_heap)
		= { rs & rcs_imports = rcs_imports, rcs_var_heap = rcs_var_heap }
	// otherwise
		= rs

checkRecordSelector {cii_main_dcl_module_n, cii_common_defs} {glob_module, glob_object={ds_index}} rs=:{rcs_imports,rcs_var_heap}
	| glob_module <> cii_main_dcl_module_n
		# {com_selector_defs,com_cons_defs,com_type_defs} = cii_common_defs.[glob_module]
		  {sd_type_index} = com_selector_defs.[ds_index]
		  {td_rhs = RecordType {rt_constructor={ds_index=cons_index}}} = com_type_defs.[sd_type_index]
		  {cons_type_ptr} = com_cons_defs.[cons_index]
		  (rcs_imports, rcs_var_heap) = checkImportedSymbol (SK_Constructor {glob_module = glob_module, glob_object = cons_index})
											cons_type_ptr (rcs_imports, rcs_var_heap)
		= { rs & rcs_imports = rcs_imports, rcs_var_heap = rcs_var_heap }
	// otherwise
		= rs

instance weightedRefCount App
where
	weightedRefCount rci=:{rci_imported} {app_symb,app_args} rs
		# rs = weightedRefCount rci app_args rs
		= check_import rci_imported app_symb rs
	where
		check_import cii {symb_kind=SK_Function {glob_module,glob_object}} rs=:{rcs_imports, rcs_var_heap}
			= checkImportOfDclFunction cii glob_module glob_object rs
		check_import {cii_main_dcl_module_n, cii_common_defs} {symb_ident,symb_kind=symb_kind=:(SK_Constructor {glob_module,glob_object})} rs=:{rcs_imports, rcs_var_heap}
			| glob_module <> cii_main_dcl_module_n
				# {cons_type_ptr} = cii_common_defs.[glob_module].com_cons_defs.[glob_object]
				  (rcs_imports, rcs_var_heap) = checkImportedSymbol symb_kind cons_type_ptr (rcs_imports, rcs_var_heap)
				= { rs & rcs_imports = rcs_imports, rcs_var_heap = rcs_var_heap }
				= rs
		check_import _ _ rs
			= rs

instance weightedRefCount TypeCodeExpression
where
	weightedRefCount rci type_code_expr rs
		= rs

instance weightedRefCount [a] | weightedRefCount a
where
	weightedRefCount rci l rs
		=	foldr (weightedRefCount rci) rs l 
		
instance weightedRefCount (a,b) | weightedRefCount a & weightedRefCount b
where
	weightedRefCount rci (x,y) rs
		=	weightedRefCount rci y (weightedRefCount rci x rs) 

instance weightedRefCount LetBind
where
	weightedRefCount rci {lb_src} rs
		=	weightedRefCount rci lb_src rs

instance weightedRefCount (Bind a b) | weightedRefCount a
where
	weightedRefCount rci bind=:{bind_src} rs
		=	weightedRefCount rci bind_src rs

/*
	distributeLets tries to move shared exprs as close as possible to the location at which they are used.
	Case-exprs may require unsharing if the shared expr is used in different alternatives. Of course
	only if the expr is neither used in the pattern nor in a surrounding expr.
*/

::	LetExpressionStatus	= LES_Untouched | LES_Moved | LES_Updated !Expression

::	LetExpressionInfo =
	{	lei_count			:: !Int
	,	lei_depth			:: !Int 
	,	lei_var				:: !FreeVar 
	,   lei_expression		:: !Expression
	,   lei_status			:: !LetExpressionStatus
	,   lei_type			:: !AType
	}

::	DistributeInfo =
	{	di_depth 				:: !Int
	,	di_explicit_case_depth	:: !Int
	}

::	DistributeState =
	{	ds_lets			:: ![VarInfoPtr]
	,	ds_var_heap		:: !.VarHeap
	,	ds_expr_heap	:: !.ExpressionHeap
	}

class distributeLets e :: !DistributeInfo !e !*DistributeState -> (!e, !*DistributeState)

instance distributeLets Expression
where
	distributeLets di=:{di_depth} (Var var=:{var_ident,var_info_ptr}) ds=:{ds_var_heap}
		#! var_info = sreadPtr var_info_ptr ds_var_heap
		= case var_info of
			VI_LetExpression lei
				| lei.lei_depth == di_depth
					| lei.lei_count == 1 && (case lei.lei_status of LES_Updated _ -> False; _ -> True)
						# (lei_updated_expr, ds) = distributeLets di lei.lei_expression ds
						-> (lei_updated_expr, { ds &  ds_var_heap = ds.ds_var_heap <:=
								(var_info_ptr, VI_LetExpression { lei & lei_status = LES_Updated lei_updated_expr }) })
						# ds = distributeLetsInLetExpression di var_info_ptr lei ds
						-> (Var { var & var_info_ptr = lei.lei_var.fv_info_ptr }, ds)
				// otherwise
					-> (Var { var & var_info_ptr = lei.lei_var.fv_info_ptr }, ds)
			VI_CaseOrStrictLetVar var_info_ptr
				-> (Var { var & var_info_ptr = var_info_ptr }, ds)
			_
				-> (Var var, ds)
	distributeLets di (Case kees) ds
		# (kees, ds) = distributeLets di kees ds
		= (Case kees, ds)
	distributeLets di (App app=:{app_args}) ds
		# (app_args, ds) = distributeLets di app_args ds
		= (App {app & app_args = app_args}, ds)
	distributeLets di (fun_expr @ exprs) ds
		# (fun_expr, ds) = distributeLets di fun_expr ds
		  (exprs, ds) = distributeLets di exprs ds
		= (fun_expr @ exprs, ds)
	distributeLets di expr=:(BasicExpr _) ds
		= (expr, ds)
	distributeLets di (Selection opt_tuple expr selectors) ds
		# (expr, ds) = distributeLets di expr ds
		# (selectors, ds) = distributeLets di selectors ds
		= (Selection opt_tuple expr selectors, ds)
	distributeLets di (Update expr1 selectors expr2) ds
		# (expr1, ds) = distributeLets di expr1 ds
		# (selectors, ds) = distributeLets di selectors ds
		# (expr2, ds) = distributeLets di expr2 ds
		= (Update expr1 selectors expr2, ds)
	distributeLets di (RecordUpdate cons_symbol expr exprs) ds
		# (expr, ds) = distributeLets di expr ds
		# (exprs, ds) = distributeLets di exprs ds
		= (RecordUpdate cons_symbol expr exprs, ds)
	distributeLets di (TupleSelect tuple_symbol arg_nr expr) ds
		# (expr, ds) = distributeLets di expr ds
		= (TupleSelect tuple_symbol arg_nr expr, ds)
	distributeLets di=:{di_depth} (Let lad=:{let_strict_binds,let_lazy_binds,let_expr,let_info_ptr}) ds=:{ds_expr_heap,ds_var_heap}
		# (let_info, ds_expr_heap) = readPtr let_info_ptr ds_expr_heap
		# (EI_LetTypeAndRefCounts let_type ref_counts) = let_info
		  nr_of_strict_lets = length let_strict_binds
		  ds_var_heap = set_let_expr_info di_depth let_lazy_binds ref_counts (drop nr_of_strict_lets let_type) ds_var_heap
		  (let_strict_binds,ds_var_heap) = mapSt set_strict_let_expr_info let_strict_binds ds_var_heap
		  (let_expr, ds) = distributeLets di let_expr { ds & ds_var_heap = ds_var_heap, ds_expr_heap = ds_expr_heap }
		  (let_strict_binds, ds) = distributeLets di let_strict_binds ds
		  ds = foldSt (distribute_lets_in_non_distributed_let di) let_lazy_binds ds
		| nr_of_strict_lets == 0
		    = (let_expr, ds)
		// otherwise
		    = case let_expr of
		    	Let inner_let=:{let_info_ptr=inner_let_info_ptr}
					# (EI_LetType strict_inner_types, ds_expr_heap) = readPtr inner_let_info_ptr ds.ds_expr_heap
					# (inner_let_info_ptr, ds_expr_heap)
						=	newPtr (EI_LetType ((take nr_of_strict_lets let_type)++strict_inner_types)) ds_expr_heap
					-> (Let { inner_let & let_strict_binds = let_strict_binds++inner_let.let_strict_binds,
									let_info_ptr = inner_let_info_ptr}, 
						{ds & ds_expr_heap = ds_expr_heap})
				_	# (let_info_ptr, ds_expr_heap)
						=	newPtr (EI_LetType (take nr_of_strict_lets let_type)) ds.ds_expr_heap
					-> (Let { lad & let_strict_binds = let_strict_binds, let_expr = let_expr, let_lazy_binds = [], let_info_ptr = let_info_ptr}, 
						{ds & ds_expr_heap = ds_expr_heap})
	where
		set_let_expr_info depth [{lb_src,lb_dst}:binds] [ref_count:ref_counts] [type:types] var_heap
			# (new_info_ptr, var_heap) = newPtr VI_LocalLetVar var_heap
			  lei = { lei_count = ref_count, lei_depth = depth, lei_var = { lb_dst & fv_info_ptr = new_info_ptr },
			  			lei_expression = lb_src, lei_type = type, lei_status =  LES_Untouched }
			= set_let_expr_info depth binds ref_counts types (var_heap <:= (lb_dst.fv_info_ptr, VI_LetExpression lei))
		set_let_expr_info _ [] _ _ var_heap
			= var_heap

		set_strict_let_expr_info lb=:{lb_dst={fv_info_ptr}} var_heap
			# (new_info_ptr, var_heap) = newPtr VI_StrictLetVar var_heap
			= ({lb & lb_dst.fv_info_ptr = new_info_ptr}, var_heap <:= (fv_info_ptr, VI_CaseOrStrictLetVar new_info_ptr))
	
		distribute_lets_in_non_distributed_let di {lb_dst={fv_ident,fv_info_ptr}} ds=:{ds_var_heap}
			# (VI_LetExpression lei=:{lei_count}, ds_var_heap) = readPtr fv_info_ptr ds_var_heap
			| lei_count > 0
//			| not lei_moved && lei_count > 0
				= distributeLetsInLetExpression di fv_info_ptr lei { ds & ds_var_heap = ds_var_heap }
				= { ds & ds_var_heap = ds_var_heap }

	distributeLets di (MatchExpr constructor expr) ds
		# (expr, ds) = distributeLets di expr ds
		= (MatchExpr constructor expr, ds)
	distributeLets di (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) ds
		# (expr, ds) = distributeLets di expr ds
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, ds)
	distributeLets _ expr=:(TypeCodeExpression _) ds
		= (expr, ds)
	distributeLets _ (AnyCodeExpr in_params out_params code_expr) ds=:{ds_var_heap}
		# (in_params, ds_var_heap) = mapSt determine_input_parameter in_params ds_var_heap
		= (AnyCodeExpr in_params out_params code_expr, { ds & ds_var_heap = ds_var_heap })
		where
			determine_input_parameter bind=:{bind_dst} var_heap
				# (var_info, var_heap) = readPtr bind_dst.var_info_ptr var_heap
				= case var_info of
					VI_CaseOrStrictLetVar new_info_ptr
						-> ({ bind & bind_dst = { bind_dst & var_info_ptr = new_info_ptr }}, var_heap)
					_
						-> (bind, var_heap)
	distributeLets _ expr=:(ABCCodeExpr _ _) ds
		= (expr, ds)
	distributeLets _ EE ds
		= (EE, ds)
	distributeLets _ (NoBind ptr) ds
		= (NoBind ptr, ds)
	distributeLets _ (FailExpr id) ds
		= (FailExpr id, ds)
	distributeLets di (DictionariesFunction dictionaries expr expr_type) ds
		# (expr,ds) = distributeLets di expr ds
		= (DictionariesFunction dictionaries expr expr_type,ds)

instance distributeLets Case
where
	distributeLets di=:{di_depth,di_explicit_case_depth} kees=:{case_info_ptr,case_guards,case_default,case_expr,case_explicit} ds=:{ds_var_heap, ds_expr_heap}
		# (case_old_info, ds_expr_heap) = readPtr case_info_ptr ds_expr_heap
		  (EI_CaseTypeAndRefCounts type
		  	{	rcc_all_variables = tot_ref_counts ,
		  		rcc_default_variables = ref_counts_in_default,
		  		rcc_pattern_variables = ref_counts_in_patterns }) = case_old_info
		  new_depth = di_depth + 1
		  new_di =	{	di
					&	di_depth = new_depth
					,	di_explicit_case_depth = if case_explicit new_depth di_explicit_case_depth
					}
		  (local_lets, ds_var_heap) = mark_local_let_vars new_depth tot_ref_counts ds_var_heap
										  	// -*-> ("ref_counts", case_expr, tot_ref_counts, ref_counts_in_patterns)
		  	with
				mark_local_let_vars new_depth tot_ref_counts var_heap

					| case_explicit
						# (local_vars,local_select_vars,var_heap) = foldSt (mark_local_let_var_of_explicit_case new_depth) tot_ref_counts ([],[],var_heap)
						= foldSt (mark_local_let_select_var_of_explicit_case new_depth) local_select_vars (local_vars,var_heap)

						= foldSt (mark_local_let_var new_depth) tot_ref_counts ([],var_heap)
		  	
	 	  ds = {ds & ds_var_heap=ds_var_heap, ds_expr_heap=ds_expr_heap}
		  (case_guards, ds)  = distribute_lets_in_patterns new_di ref_counts_in_patterns case_guards ds
		  (case_default, ds=:{ds_var_heap}) = distribute_lets_in_default new_di ref_counts_in_default case_default ds
		  (outer_vars, ds_var_heap) = foldSt (is_outer_var new_di) tot_ref_counts (False, ds.ds_var_heap)
		  
		# ds_var_heap = foldSt reset_local_let_var local_lets ds_var_heap ->> ("outer_vars", di_depth, di.di_explicit_case_depth, outer_vars)
		  (case_expr, ds) = distributeLets di case_expr { ds & ds_var_heap = ds_var_heap}
		  kees = { kees & case_guards = case_guards, case_expr = case_expr,
				case_default = case_default}
		  (kind, ds_var_heap) = case_kind outer_vars kees ds.ds_var_heap
		  case_new_info = EI_CaseTypeAndSplits type {sic_splits = [], sic_next_alt = No, sic_case_kind = kind}
		  (case_info_ptr, ds_expr_heap) = newPtr case_new_info ds.ds_expr_heap
		  kees = { kees & case_info_ptr = case_info_ptr } // ->> ("case_kind", di_depth, kind, case_explicit, ptrToInt case_info_ptr)
		= (kees, { ds & ds_expr_heap = ds_expr_heap, ds_var_heap = ds_var_heap})
	where
		case_kind _ {case_guards, case_default, case_explicit, case_expr} var_heap
			| is_guard case_guards case_default case_explicit case_expr
				=	(CaseKindGuard, var_heap)
		case_kind outer_vars {case_expr, case_explicit}  var_heap
			| case_explicit || outer_vars || not (is_lhs_var case_expr var_heap)
				=	(CaseKindTransform, var_heap)
			// otherwise
				=	(CaseKindLeave, var_heap)
			where
				is_lhs_var (Var {var_info_ptr, var_ident}) var_heap
					= 	case sreadPtr var_info_ptr var_heap of
							VI_LocalLetVar
								->	False
							VI_LetExpression _
								->	False
							VI_StrictLetVar
								->	False
							info
								->	True
				is_lhs_var _ _
					=	False
		
		is_guard (BasicPatterns BT_Bool patterns) case_default case_explicit case_expr
			=	is_guard_case patterns case_default case_explicit case_expr
		is_guard _ _ _ _
			=	False

		distribute_lets_in_patterns di ref_counts (AlgebraicPatterns conses patterns) ds
			# (patterns, ds) = mapSt (distribute_lets_in_alg_pattern di) (exactZip ref_counts patterns) ds
			= (AlgebraicPatterns conses patterns, ds)
		distribute_lets_in_patterns di ref_counts (BasicPatterns type patterns) ds
			# (patterns, ds) = mapSt (distribute_lets_in_basic_pattern di) (exactZip ref_counts patterns) ds
			= (BasicPatterns type patterns, ds)
		where
			distribute_lets_in_basic_pattern di (ref_counts,pattern) ds
				# (bp_expr, ds) = distribute_lets_in_pattern_expr di ref_counts pattern.bp_expr ds
				= ({ pattern & bp_expr = bp_expr }, ds)
		distribute_lets_in_patterns di ref_counts (OverloadedListPatterns conses decons_expr patterns) heaps
			# (patterns, heaps) = mapSt (distribute_lets_in_alg_pattern di) (exactZip ref_counts patterns) heaps
			= (OverloadedListPatterns conses decons_expr patterns, heaps)

		distribute_lets_in_alg_pattern di (ref_counts,pattern) ds=:{ds_var_heap}
			# (ap_vars, ds_var_heap) = mapSt refresh_variable pattern.ap_vars ds_var_heap
			  ds = {ds & ds_var_heap = ds_var_heap}
			  (ap_expr, ds) = distribute_lets_in_pattern_expr di ref_counts pattern.ap_expr ds
			= ({ pattern & ap_vars = ap_vars, ap_expr = ap_expr }, ds) 

		distribute_lets_in_default di ref_counts_in_default (Yes expr) ds
			# (expr, ds) = distribute_lets_in_pattern_expr di ref_counts_in_default expr ds
			= (Yes expr, ds)
		distribute_lets_in_default _ ref_counts_in_default No ds
			= (No, ds)

		refresh_variable fv=:{fv_info_ptr} var_heap
			# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
			= ({ fv & fv_info_ptr = new_info_ptr }, var_heap <:= (fv_info_ptr, VI_CaseOrStrictLetVar new_info_ptr))

		mark_local_let_var depth {cv_variable, cv_count} (local_vars, var_heap)
			# (VI_LetExpression lei=:{lei_count,lei_depth,lei_var}, var_heap) = readPtr cv_variable var_heap
			| lei_count == cv_count && lei_depth==depth-1
				= ([(cv_variable, lei_count, lei_depth) : local_vars ], var_heap <:= (cv_variable, VI_LetExpression { lei & lei_depth = depth}))
				= (local_vars, var_heap)

		mark_local_let_var_of_explicit_case depth {cv_variable, cv_count} (local_vars,local_select_vars,var_heap)
			# (VI_LetExpression lei=:{lei_count,lei_depth,lei_expression}, var_heap) = readPtr cv_variable var_heap
			| lei_count == cv_count && lei_depth==depth-1
				= case lei_expression of
					TupleSelect _ _ (Var var=:{var_ident,var_info_ptr})
						# (var_info,var_heap) = readPtr var_info_ptr var_heap
						-> case var_info of
							VI_LetExpression lei2
								-> (local_vars,[(cv_variable,lei_depth):local_select_vars],var_heap <:= (cv_variable, VI_LetExpression { lei & lei_depth = depth}))
							_
								-> ([(cv_variable, lei_count, lei_depth) : local_vars ],local_select_vars,var_heap <:= (cv_variable, VI_LetExpression { lei & lei_depth = depth}))
					Selection NormalSelector (Var var=:{var_ident,var_info_ptr}) [RecordSelection _ _]
						# (var_info,var_heap) = readPtr var_info_ptr var_heap
						-> case var_info of
							VI_LetExpression lei2
								-> (local_vars,[(cv_variable,lei_depth):local_select_vars],var_heap <:= (cv_variable, VI_LetExpression { lei & lei_depth = depth}))
							_
								-> ([(cv_variable, lei_count, lei_depth) : local_vars ],local_select_vars,var_heap <:= (cv_variable, VI_LetExpression { lei & lei_depth = depth}))
					_
						-> ([(cv_variable, lei_count, lei_depth) : local_vars ],local_select_vars,var_heap <:= (cv_variable, VI_LetExpression { lei & lei_depth = depth}))

				= (local_vars,local_select_vars,var_heap)

		mark_local_let_select_var_of_explicit_case depth (cv_variable,old_depth) (local_vars,var_heap)
			# (VI_LetExpression lei=:{lei_count,lei_expression}, var_heap) = readPtr cv_variable var_heap
			= case lei_expression of
				TupleSelect _ _ (Var var=:{var_ident,var_info_ptr})
					# (var_info,var_heap) = readPtr var_info_ptr var_heap
					-> case var_info of
						VI_LetExpression lei2
							| lei2.lei_depth < depth
								-> (local_vars,var_heap <:= (cv_variable, VI_LetExpression {lei & lei_depth = old_depth}))
						_
								-> ([(cv_variable, lei_count, old_depth) : local_vars ],var_heap)
				Selection NormalSelector (Var var=:{var_ident,var_info_ptr}) [RecordSelection _ _]
					# (var_info,var_heap) = readPtr var_info_ptr var_heap
					-> case var_info of
						VI_LetExpression lei2
							| lei2.lei_depth < depth
								-> (local_vars,var_heap <:= (cv_variable, VI_LetExpression {lei & lei_depth = old_depth}))
						_
								-> ([(cv_variable, lei_count, old_depth) : local_vars ],var_heap)

		reset_local_let_var (var_info_ptr, lei_count, lei_depth)  var_heap
			# (VI_LetExpression lei, var_heap) = readPtr var_info_ptr var_heap
			= var_heap <:= (var_info_ptr, VI_LetExpression { lei & lei_depth = lei_depth, lei_count = lei_count, lei_status = LES_Moved })
   
		is_outer_var {di_depth, di_explicit_case_depth} {cv_variable}  (outer, var_heap)
			| outer
				= (True,var_heap)
			# (VI_LetExpression {lei_depth,lei_status}, var_heap) = readPtr cv_variable var_heap
			| di_explicit_case_depth < lei_depth &&
				(lei_depth < di_depth || (lei_depth == di_depth && case lei_status of LES_Moved -> False; _ -> True))
			= (True,var_heap)
			= (False,var_heap);

		distribute_lets_in_pattern_expr di=:{di_depth} local_vars pattern_expr ds=:{ds_var_heap}
			# ds_var_heap = foldSt (mark_local_let_var_of_pattern_expr di_depth) local_vars ds_var_heap
			  (ds=:{ds_lets}) = {ds & ds_var_heap = ds_var_heap}
			  ds = {ds & ds_lets = []}
			  (pattern_expr, ds) = distributeLets di pattern_expr ds
			  (ds_lets2, ds) = ds!ds_lets
			  ds = foldSt (reexamine_local_let_expr di) local_vars ds
			# (letExpr, ds) = buildLetExpr pattern_expr ds
				-*-> ("distribute_lets_in_pattern_expr", ds_lets2)
			  ds = {ds & ds_lets = ds_lets}
			= (letExpr, ds)

		mark_local_let_var_of_pattern_expr depth {cv_variable, cv_count} var_heap
			# (VI_LetExpression lei, var_heap) = readPtr cv_variable var_heap
			| depth == lei.lei_depth
				= (var_heap <:= (cv_variable, VI_LetExpression { lei & lei_count = cv_count, lei_status = LES_Untouched }))
				= var_heap

		reexamine_local_let_expr di=:{di_depth} {cv_variable, cv_count} ds=:{ds_var_heap}
			| cv_count >= 1
				# (VI_LetExpression lei, ds_var_heap) = readPtr cv_variable ds_var_heap
				| di_depth == lei.lei_depth
					= distributeLetsInLetExpression di cv_variable lei { ds & ds_var_heap = ds_var_heap }
					= { ds & ds_var_heap = ds_var_heap }
				= ds

distributeLetsInLetExpression :: DistributeInfo VarInfoPtr LetExpressionInfo *DistributeState -> *DistributeState
distributeLetsInLetExpression _ let_var_info_ptr {lei_status = LES_Moved, lei_var} ds
	= ds
distributeLetsInLetExpression _ let_var_info_ptr {lei_status = LES_Updated _, lei_var} ds
	= ds
distributeLetsInLetExpression di let_var_info_ptr lei=:{lei_expression, lei_status = LES_Untouched, lei_var} ds=:{ds_var_heap}
	# ds_var_heap = ds_var_heap <:= (let_var_info_ptr, VI_LetExpression { lei & lei_status = LES_Updated EE}) /* to prevent doing this expr twice */ -*-> ("distributeLetsInLetExpression, LES_Untouched", lei_var.fv_ident.id_name, let_var_info_ptr)
      (lei_expression, ds) = distributeLets di lei_expression { ds & ds_var_heap = ds_var_heap }
	= { ds & ds_lets = [ let_var_info_ptr : ds.ds_lets ],
		 ds_var_heap = ds.ds_var_heap <:= (let_var_info_ptr, VI_LetExpression { lei & lei_status = LES_Updated lei_expression })}

buildLetExpr :: !Expression !*DistributeState -> (!Expression, !*DistributeState)
buildLetExpr let_expr ds=:{ds_lets=[]}
	=	(let_expr, ds)
buildLetExpr let_expr ds=:{ds_lets, ds_var_heap, ds_expr_heap}
	# (lazy_binds, lazy_binds_types, ds_var_heap) = foldr build_bind ([], [], ds_var_heap) ds_lets
	  ds = {ds & ds_var_heap = ds_var_heap}
	// otherwise
	= case let_expr of
		Let inner_let=:{let_info_ptr }
			# ds_expr_heap = ds.ds_expr_heap
			# (EI_LetType strict_bind_types, ds_expr_heap) = readPtr let_info_ptr ds_expr_heap
			  ds_expr_heap = writePtr let_info_ptr (EI_LetType (strict_bind_types ++ lazy_binds_types)) ds_expr_heap
			-> (Let { inner_let & let_lazy_binds = lazy_binds }, {ds & ds_expr_heap=ds_expr_heap})
		_
			# ds_expr_heap = ds.ds_expr_heap
			# (let_info_ptr, ds_expr_heap) = newPtr (EI_LetType lazy_binds_types) ds_expr_heap
			-> (Let { let_strict_binds = [], let_lazy_binds = lazy_binds, let_expr = let_expr,
					let_info_ptr = let_info_ptr, let_expr_position = NoPos }, {ds & ds_expr_heap = ds_expr_heap})
where
	build_bind :: !VarInfoPtr !(![LetBind], ![AType], !*VarHeap)
		-> (![LetBind], ![AType], !*VarHeap)
	build_bind info_ptr (lazy_binds, lazy_binds_types, var_heap)
		# (let_info, var_heap) = readPtr info_ptr var_heap
		# (VI_LetExpression lei=:{lei_var,lei_status,lei_type}) = let_info
		  (LES_Updated updated_expr) = lei_status
		  (new_info_ptr, var_heap) = newPtr (VI_Labelled_Empty "build_bind") var_heap 
		  var_heap = var_heap <:= (info_ptr, VI_LetExpression { lei & lei_status = LES_Untouched, lei_var = { lei_var & fv_info_ptr = new_info_ptr }})
		= ([{ lb_src = updated_expr, lb_dst = lei_var, lb_position = NoPos } : lazy_binds], [lei_type : lazy_binds_types ], var_heap)

instance distributeLets Selection
where
	distributeLets di (ArraySelection selector expr_ptr expr) cp_info
		# (expr, cp_info) = distributeLets di expr cp_info
		= (ArraySelection selector expr_ptr expr, cp_info)
	distributeLets di (DictionarySelection var selectors expr_ptr expr) cp_info
		# (selectors, cp_info) = distributeLets di selectors cp_info
		# (expr, cp_info) = distributeLets di expr cp_info
		= (DictionarySelection var selectors expr_ptr expr, cp_info)
	distributeLets _ selection cp_info
		= (selection, cp_info)

instance distributeLets [a] | distributeLets a
where
	distributeLets di l cp_info = mapSt (distributeLets di) l cp_info 

instance distributeLets LetBind
where
	distributeLets di bind=:{lb_src} cp_info
		# (lb_src, cp_info) = distributeLets di lb_src cp_info
		= ({ bind & lb_src = lb_src }, cp_info)
	
instance distributeLets (Bind a b) | distributeLets a
where
	distributeLets di bind=:{bind_src} cp_info
		# (bind_src, cp_info) = distributeLets di bind_src cp_info
		= ({ bind & bind_src = bind_src }, cp_info)

/*
	Split case expressions.
	
	Splitting a case expression can be necessary because of limitations in
	the compiler's backend. The backend can only handle case expressions that
	match on left-hand-side variable and are on root positions (right-hand-side
	of functions, the resulting expression of let expressions on a root
	position and the right-hand side or default of case expressions an on a
	root position). The exact test can be found in convertRootCases.

	There's a difference in the semantics of implicit cases (which are written
	as patterns by the programmer) and explicit cases (written as case expres-
	sions by the programmer).

	Implicit cases (denoted as case'):

		fi x y
			=	 case' x of					/					fi 1 2		\
					1	->	case' y of 		| syntax tree for		=	3	|
								2	->	3	|					fi _ _		|
					_	->	4				\						=	4	/

		(fi 1 2) reduces to 2

	Explicit cases:

		fe x y
			=	 case x of
					1	->	case y of 
								2	->	3
					_	->	4

		(fe 1 2) reduces to <<run-time error>>

	The frontend introduces functions for cases expressions that are explicit
	or that the backend can't handle. For the example above:

		fe x y
			=	 _c1 x y
		_c1 x y
			=	case' x of
					1	->	_c2
					_	->	4
		_c2 y
			=	case' y of 
					2	->	3

	This agrees with the semantics: the function _c2 will fail during
	evaluation of (fe 1 2).

	Problems occur when there's an implicit case expression that can't be
	handled by the backend. These case expressions result from transformations
	in the compiler (fusion in transform.icl and the conversion of dynamics).
	For example, in the function

		f
			=	case' 1 of
					1	->	case' 2 of
								3	->	4
					_	->	5

	f should reduce to 5, but in the direct translation

		f
			=	 _c1 1
		_c1 x
			=	case' x of
					1	->	_c2 2
					_	->	5
		_c2 y
			=	case' y of 
					3	->	4

	f erroneously reduces to <<run-time error>>.

	The solution is to split the case in _c1, introduce a function for the
	second part (the default alternative of _c1), and call this function from
	both _c1 and _c2

		f
			=	 _c1 1
		_c1 x
			=	case' x of
					1	->	_c2 2
					_	->	_f
		_c2 y
			=	case' y of 
					3	->	4
					_	->	_f
		_f
			=	5

	This transformation is done in two phases. First findSplitCases determines
	where cases should be split, and to which alternative of an outer case
	a case should pass control if it doesn't have a default. This information
	is recorded in the expression heap (accessed through the case_info_ptr).

	The actual splitting, the introduction of new functions, and the
	introduction of calls to these functions is done in convertRootCases.
*/
:: SplitCase =
	{	sc_alt_nr	:: CaseAltNr			// the number of the alternative, before which
											// the case should be split
	,	sc_call		:: Optional Expression	// call to the function that was introduced for
											// this split case
	}

:: NextAlt =
	{	na_case		:: ExprInfoPtr	// the case_info_ptr of the case
	,	na_alt_nr	:: CaseAltNr	// the number of the alternative
	}

:: CaseAltNr :== Int	// the sequence number of the alternative (zero based), the
						// default alternative is indicated by the number of the last
						// alternative + 1

:: CaseKind
	=	CaseKindUnknown
	|	CaseKindGuard		// a boolean case that can be handled by the backend
	|	CaseKindLeave		// a case that can be handled by the backend
	|	CaseKindTransform	// a case that should be transformed

:: SplitsInCase =
	{	sic_next_alt	:: Optional NextAlt	// the alternative of an outer default, to which
											// control should pass
	,	sic_splits		:: [SplitCase]		// the positions where this case should be split
	,	sic_case_kind	:: CaseKind
	}

::	SplitState =
	{	ss_expr_heap	:: !.ExpressionHeap
	,	ss_var_heap		:: !.VarHeap
	}

::	SplitInfo =
	{	si_next_alt	:: !Optional NextAlt
	,	si_force_next_alt :: !Bool
	}

class findSplitCases e :: !SplitInfo !e !*SplitState -> *SplitState

(:-) infixl
(:-) a f
	:== f a

instance findSplitCases (Optional a) | findSplitCases a  where
	findSplitCases _  No ss
		=	ss
	findSplitCases si (Yes x) ss
		=	findSplitCases si x ss

instance findSplitCases Expression where
	findSplitCases si (Let lad) ss
		=	findSplitCases si lad ss
	findSplitCases si (Case kees) ss
		=	findSplitCases si kees ss
	findSplitCases _ _ ss
		=	ss

instance findSplitCases Case where
	findSplitCases si kees=:{case_info_ptr, case_guards, case_default, case_explicit} ss
		# first_next_alt = Yes {na_case = case_info_ptr, na_alt_nr = 1}
		  use_outer_alt = use_outer_alt_for_last_alt case_default si			
		  ss = split_guards {si & si_next_alt = first_next_alt, si_force_next_alt=False} use_outer_alt case_guards ss
		= nextAlts si kees ss
		where
			split_guards :: SplitInfo (Optional SplitInfo) CasePatterns *SplitState -> *SplitState
			split_guards si use_outer_alt (AlgebraicPatterns _ alts) ss
				= split_alts si use_outer_alt alts ss
			split_guards si use_outer_alt (BasicPatterns _ alts) ss
				= split_alts si use_outer_alt alts ss
			split_guards si use_outer_alt (OverloadedListPatterns _ _ alts) ss
				= split_alts si use_outer_alt alts ss

			split_alts :: SplitInfo (Optional SplitInfo) [a] *SplitState -> *SplitState | findSplitCases a
			split_alts _ _ [] ss
				= ss
			split_alts _ (Yes si) [last] ss
				= findSplitCases si last ss
			split_alts si last_next_alt [pattern : patterns] ss
				# ss = findSplitCases si pattern ss
				= split_alts (incAltNr si) last_next_alt patterns ss

			use_outer_alt_for_last_alt :: (Optional Expression) SplitInfo -> Optional SplitInfo
			use_outer_alt_for_last_alt No si
				//	This case has no default. If the last alternative fails, control is passed to the outer case.
				= Yes si
			use_outer_alt_for_last_alt (Yes _) si
				= No

// debug ...
instance toString (Optional a) | toString a where
	toString No
		=	""
	toString (Yes x)
		=	toString x
// ... debug

class incAltNr a :: a -> a

instance incAltNr Int where
	incAltNr alt_nr
		=	alt_nr + 1

instance incAltNr NextAlt where
	incAltNr next_alt=:{na_alt_nr}
		=	{next_alt & na_alt_nr = incAltNr na_alt_nr}

instance incAltNr (Optional a) | incAltNr a where
	incAltNr No
		=	No
	incAltNr (Yes x)
		=	Yes (incAltNr x)

instance incAltNr SplitInfo where
	incAltNr si=:{si_next_alt}
		=	{si & si_next_alt = incAltNr si_next_alt}

instance findSplitCases AlgebraicPattern where
	findSplitCases si {ap_expr} ss
		=	findSplitCases si ap_expr ss

instance findSplitCases BasicPattern where
	findSplitCases si {bp_expr} ss
		=	findSplitCases si bp_expr ss

instance findSplitCases Let where
	findSplitCases si {let_expr} ss
		=	findSplitCases si let_expr ss

nextAlts :: SplitInfo Case *SplitState -> *SplitState
nextAlts si=:{si_next_alt=Yes next_alt, si_force_next_alt} kees=:{case_info_ptr, case_default} ss
	# (EI_CaseTypeAndSplits type splits, ss_expr_heap)
		=	readPtr case_info_ptr ss.ss_expr_heap
	# ss
		=	{ss & ss_expr_heap = ss_expr_heap}
	# jumps
		=	not kees.case_explicit && (si_force_next_alt || jumps_to_next_alt splits kees)
	# ss
		=	findSplitCases {si & si_force_next_alt=jumps} case_default ss
	| jumps && not (hasOption case_default)
		// update the info for this case
		# ss_expr_heap = ss.ss_expr_heap <:= (case_info_ptr, EI_CaseTypeAndSplits type {splits & sic_next_alt = Yes next_alt})
		// update the info for the outer case
		# (EI_CaseTypeAndSplits type splits, ss_expr_heap) = readPtr next_alt.na_case ss_expr_heap
		  split = {sc_alt_nr = next_alt.na_alt_nr, sc_call = No}
	  	  ss_expr_heap = ss_expr_heap <:= (next_alt.na_case, EI_CaseTypeAndSplits type {splits & sic_splits = [split : splits.sic_splits]})
		=	{ss & ss_expr_heap = ss_expr_heap}
		=	ss
	where
		/* stress test, convert all cases without a default
		jumps_to_next_alt _ {case_default = No} ss
			=	(True, ss)
		*/
		/* stress test, convert all explicit cases (may change semantics for failing programs)
		jumps_to_next_alt _ {case_default = No, case_explicit = True, case_expr}
			=	 (True, ss)	->> (toString (ptrToInt case_info_ptr) +++ " jumps, because explicit")
		*/
		jumps_to_next_alt {sic_splits=[_:_]} {case_explicit = False}
			=	True	->> (toString (ptrToInt case_info_ptr) +++ " jumps, because alt was moved")
		jumps_to_next_alt {sic_case_kind=CaseKindTransform} {case_explicit = False}
			=	True ->> (toString (ptrToInt case_info_ptr) +++ " jumps, because implicit no lhs var")
		jumps_to_next_alt _ _
			=	False 	->> (toString (ptrToInt case_info_ptr) +++ " doesn't jumps " +++ toString kees.case_explicit)
nextAlts si kees=:{case_default} ss
	=	findSplitCases si case_default ss // ->> ("nextAlts no outerdefault" +++ toString kees.case_explicit)

newFunctionWithType :: !(Optional Ident) !FunctionBody ![FreeVar] !SymbolType !Int !(!Int, ![FunctionInfoPtr],!*FunctionHeap)
	-> (! SymbIdent, !(!Int, ![FunctionInfoPtr],!*FunctionHeap))
newFunctionWithType opt_id fun_bodies local_vars fun_type group_index (cs_next_fun_nr, cs_new_functions, cs_fun_heap)
	# (fun_def_ptr, cs_fun_heap) = newPtr FI_Empty cs_fun_heap
	  fun_id = getIdent opt_id cs_next_fun_nr
	  arity = fun_type.st_arity
	  fun_def = 
			{	fun_ident		= fun_id
			,	fun_arity		= arity
			,	fun_priority	= NoPrio
			,	fun_body		= fun_bodies
			,	fun_type		= Yes fun_type
			,	fun_pos			= NoPos
			,	fun_kind		= FK_Function cNameNotLocationDependent
			,	fun_lifted		= 0
			,	fun_info		= { EmptyFunInfo & fi_group_index = group_index, fi_local_vars = local_vars }
			}
	= ({ symb_ident = fun_id, symb_kind = SK_GeneratedFunction fun_def_ptr cs_next_fun_nr },
			(inc cs_next_fun_nr, [fun_def_ptr : cs_new_functions],
				cs_fun_heap <:= (fun_def_ptr,  FI_Function { gf_fun_def = fun_def, gf_instance_info = II_Empty,
	  				  gf_fun_index = cs_next_fun_nr, gf_cons_args = {cc_size=0, cc_args = [], cc_linear_bits = [#!], cc_producer = False} })))

addNewFunctionsToGroups :: !{#.CommonDefs} FunctionHeap ![FunctionInfoPtr] !Int !*{!Component} !*{#{# CheckedTypeDef}} !ImportedFunctions !*TypeHeaps !*VarHeap
	-> (!*{!Component}, ![FunDef], !*{#{# CheckedTypeDef}}, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
addNewFunctionsToGroups common_defs fun_heap new_functions main_dcl_module_n groups imported_types imported_conses type_heaps var_heap
	= foldSt (add_new_function_to_group fun_heap common_defs) new_functions (groups, [], imported_types, imported_conses, type_heaps, var_heap)
where
	add_new_function_to_group :: !FunctionHeap  !{# CommonDefs} !FunctionInfoPtr
				!(!*{!Component}, ![FunDef], !*{#{# CheckedTypeDef}}, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
					-> (!*{!Component}, ![FunDef],  !*{#{# CheckedTypeDef}}, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
	add_new_function_to_group fun_heap common_defs fun_ptr (groups, fun_defs, imported_types, imported_conses, type_heaps, var_heap)
		# (FI_Function {gf_fun_def,gf_fun_index}) = sreadPtr fun_ptr fun_heap
		  {fun_type = Yes ft, fun_info = {fi_group_index, fi_properties}} = gf_fun_def
		  (ft, imported_types, imported_conses, type_heaps, var_heap)
		  		= convertSymbolType (fi_properties bitand FI_HasTypeSpec == 0) common_defs ft main_dcl_module_n
		  		 			imported_types imported_conses type_heaps var_heap
		# (group, groups) = groups![fi_group_index]
		= ({ groups & [fi_group_index] = { group & component_members = ComponentMember gf_fun_index group.component_members} },
				[ { gf_fun_def & fun_type = Yes ft }: fun_defs], imported_types, imported_conses, type_heaps, var_heap)

::	ConvertState =
	{	cs_new_functions 	:: ![FunctionInfoPtr]
	,	cs_fun_heap			:: !.FunctionHeap
	,	cs_var_heap			:: !.VarHeap
	,	cs_expr_heap		:: !.ExpressionHeap
	,	cs_next_fun_nr		:: !Index
	}

is_guard_case [{bp_value=BVB True,bp_expr},{bp_value=BVB False,bp_expr=false_expr}] (Yes _) False case_expr
	= is_then_or_else bp_expr && is_then_or_else false_expr
is_guard_case [{bp_value=BVB True,bp_expr},{bp_value=BVB False,bp_expr=else_expr}] No True case_expr
	= boolean_case_is_if case_expr bp_expr else_expr
is_guard_case [{bp_value=BVB True,bp_expr}:patterns] case_default False case_expr
	= has_no_rooted_case bp_expr
is_guard_case [{bp_value=BVB True,bp_expr=then_expr}] (Yes else_expr) True case_expr
	= boolean_case_is_if case_expr then_expr else_expr
is_guard_case [{bp_value=BVB False,bp_expr},{bp_value=BVB True,bp_expr=true_expr}] (Yes _) False case_expr
	= is_then_or_else bp_expr && is_then_or_else true_expr
is_guard_case [{bp_value=BVB False,bp_expr}:patterns] case_default False case_expr
	= then_part_exists_and_has_no_rooted_case patterns case_default
is_guard_case _ _ _ _
	= False

has_no_rooted_case (Case {case_guards=BasicPatterns BT_Bool patterns, case_default,case_explicit,case_expr})
	= is_nested_guard_case patterns case_default case_explicit case_expr
has_no_rooted_case (Case {case_explicit})
	= case_explicit
has_no_rooted_case (Let {let_expr})
	= has_no_rooted_case let_expr
has_no_rooted_case _
	= True

then_part_exists_and_has_no_rooted_case [ alt=:{bp_value=BVB sign_of_alt,bp_expr} : alts ] case_default
	| sign_of_alt
		= has_no_rooted_case bp_expr
		= then_part_exists_and_has_no_rooted_case alts case_default
then_part_exists_and_has_no_rooted_case [ ] No
	= False
then_part_exists_and_has_no_rooted_case [ ] (Yes then_expr)
	= False // only when the first alt cannot fail use: has_no_rooted_case then_expr

is_nested_guard_case [{bp_value=BVB True,bp_expr},{bp_value=BVB False,bp_expr=false_expr}] (Yes _) False case_expr
	= is_then_or_else bp_expr && is_then_or_else false_expr
is_nested_guard_case [{bp_value=BVB True,bp_expr},{bp_value=BVB False,bp_expr=else_expr}] No True case_expr
	= boolean_case_is_if case_expr bp_expr else_expr

is_nested_guard_case [{bp_value=BVB True,bp_expr}] case_default False case_expr
	= has_no_rooted_case bp_expr && case case_default of Yes _ -> True; No-> False
is_nested_guard_case [{bp_value=BVB True,bp_expr=then_expr},{bp_value=BVB False,bp_expr=else_expr}] No False case_expr
	= has_no_rooted_case then_expr && has_no_rooted_case else_expr 
is_nested_guard_case [{bp_value=BVB True,bp_expr}:patterns] case_default False case_expr
	= has_no_rooted_case bp_expr && is_nested_guard_case patterns case_default False case_expr 

is_nested_guard_case [{bp_value=BVB True,bp_expr=then_expr}] (Yes else_expr) True case_expr
	= boolean_case_is_if case_expr then_expr else_expr
is_nested_guard_case [{bp_value=BVB False,bp_expr},{bp_value=BVB True,bp_expr=true_expr}] (Yes _) False case_expr
	= is_then_or_else bp_expr && is_then_or_else true_expr
is_nested_guard_case [{bp_value=BVB False,bp_expr}:patterns] case_default False case_expr
	= then_part_exists_and_has_no_rooted_case patterns case_default
is_nested_guard_case _ _ _ _
	= False

is_then_or_else (Case {case_expr,case_guards,case_default})
	= is_if_case case_expr case_guards case_default
is_then_or_else (Let {let_expr})
	= is_then_or_else let_expr
is_then_or_else _
	= True

is_if_case case_expr (BasicPatterns BT_Bool [{bp_value=BVB True,bp_expr=then_expr},{bp_value=BVB False,bp_expr=else_expr}]) No
	= boolean_case_is_if case_expr then_expr else_expr
is_if_case case_expr (BasicPatterns BT_Bool [{bp_value=BVB True,bp_expr=then_expr}]) (Yes else_expr)
	= boolean_case_is_if case_expr then_expr else_expr
is_if_case case_expr case_guards case_default
	= False

boolean_case_is_if case_expr then_expr else_expr
	= has_no_rooted_non_if_cases case_expr && is_then_or_else then_expr && is_then_or_else else_expr

has_no_rooted_non_if_cases (Case {case_expr,case_guards,case_default})
	= is_if_case case_expr case_guards case_default
has_no_rooted_non_if_cases (Let _)
	= False
has_no_rooted_non_if_cases _
	= True

convert_let_binds let_strict_binds let_lazy_binds let_info_ptr ci=:{ci_bound_vars} cs=:{cs_expr_heap}
	# (EI_LetType let_type, cs_expr_heap) = readPtr let_info_ptr cs_expr_heap
	  ci_bound_vars = addLetVars (let_strict_binds ++ let_lazy_binds) let_type ci_bound_vars
	  ci = {ci & ci_bound_vars = ci_bound_vars}
	  (let_strict_binds,cs)	= convertCases ci let_strict_binds { cs & cs_expr_heap = cs_expr_heap }
	  (let_lazy_binds,cs) = convertCases ci let_lazy_binds cs
	= (let_strict_binds,let_lazy_binds,ci,cs)

convert_case_to_if case_expr then_expr else_expr ci cs
	# (case_expr,cs)=convert_condition case_expr ci cs
	# (then_expr,cs)=convert_then_or_else then_expr ci cs
	# (else_expr,cs)=convert_then_or_else else_expr ci cs
	= (Conditional { if_cond = case_expr, if_then = then_expr, if_else = Yes else_expr },cs)
where
	convert_then_or_else (Let lad=:{let_strict_binds,let_lazy_binds,let_expr,let_info_ptr}) ci cs=:{cs_expr_heap}
		# (let_strict_binds,let_lazy_binds,ci,cs) = convert_let_binds let_strict_binds let_lazy_binds let_info_ptr ci cs
		  (let_expr,cs) = convert_condition let_expr ci cs
		= (Let { lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr }, cs)
	convert_then_or_else expr ci cs
		= convert_condition expr ci cs

convert_condition (Case {case_expr,case_guards=(BasicPatterns BT_Bool [{bp_value=BVB True,bp_expr=then_expr},{bp_value=BVB False,bp_expr=else_expr}]),case_default=No}) ci cs
	= convert_case_to_if case_expr then_expr else_expr ci cs
convert_condition (Case {case_expr,case_guards=(BasicPatterns BT_Bool [{bp_value=BVB True,bp_expr=then_expr}]),case_default=Yes else_expr}) ci cs
	= convert_case_to_if case_expr then_expr else_expr ci cs
convert_condition expr ci cs
	= convertCases ci expr cs

// optimise the following case: an alternative for each constructor, at least 3, one alternative cannot fail and the other
// alternatives yield the same boolean result. Remove those boolean result alternatives, and add a default with this result.
optimise_case :: !CasePatterns !(Optional Expression) ExprInfoPtr !{#CommonDefs} !*ExpressionHeap
	-> (!CasePatterns,!Optional Expression,!*ExpressionHeap)
optimise_case case_guards=:(AlgebraicPatterns global_type_index algebraic_patterns) case_default=:No case_info_ptr common_defs expr_heap
	= case common_defs.[global_type_index.gi_module].com_type_defs.[global_type_index.gi_index].td_rhs of
		AlgType cons_symbols
			| at_least_three_same_length_lists algebraic_patterns cons_symbols
				# (new_guard_n,new_algebraic_patterns,new_case_default) = try_replace_case_alts_by_default algebraic_patterns
				| new_guard_n<0
					-> (case_guards,case_default,expr_heap)
					-> case readPtr case_info_ptr expr_heap of
						(EI_CaseTypeAndSplits case_type=:{ct_cons_types} splits,expr_heap)
							# case_type & ct_cons_types=[ct_cons_types!!new_guard_n]
							# expr_heap = writePtr case_info_ptr (EI_CaseTypeAndSplits case_type splits) expr_heap
							-> (AlgebraicPatterns global_type_index new_algebraic_patterns,new_case_default,expr_heap)
						(_,expr_heap)
							-> abort "optimise_case No EI_CaseTypeAndSplits"
				-> (case_guards,case_default,expr_heap)
		_
			-> (case_guards,case_default,expr_heap)
where
	at_least_three_same_length_lists :: ![a] ![b] -> Bool
	at_least_three_same_length_lists [_,_,_:l1] [_,_,_:l2] = same_length_lists l1 l2
	at_least_three_same_length_lists _ _ = False

	same_length_lists :: ![a] ![b] -> Bool
	same_length_lists [_:l1] [_:l2] = same_length_lists l1 l2
	same_length_lists [] [] = True
	same_length_lists _ _ = False

	try_replace_case_alts_by_default :: ![AlgebraicPattern] -> (!Int,![AlgebraicPattern],!Optional Expression)
	try_replace_case_alts_by_default [{ap_expr=ap1_expr=:BasicExpr (BVB bool1)},{ap_expr=BasicExpr (BVB bool2)}:algebraic_patterns3]
		| bool1==bool2
			# (algebraic_patterns3,guard_n) = skip_case_alt_results_this_bool algebraic_patterns3 bool1 2
			= case algebraic_patterns3 of
				[algebraic_pattern:algebraic_patterns2]
					| all_case_alt_results_this_bool algebraic_patterns2 bool1 && no_partial_match algebraic_pattern.ap_expr
						-> (guard_n,[algebraic_pattern],Yes ap1_expr)
				_
					-> (-1,[],No)
	try_replace_case_alts_by_default [ap1=:{ap_expr=BasicExpr (BVB bool1)},ap2=:{ap_expr=BasicExpr (BVB bool2)},{ap_expr=BasicExpr (BVB bool3)}:algebraic_patterns4]
		// bool1<>bool2
		| all_case_alt_results_this_bool algebraic_patterns4 bool3
			| bool3<>bool1 // bool3==bool2
				= (0,[ap1],Yes ap2.ap_expr)
			// | bool3==bool1
				= (1,[ap2],Yes ap1.ap_expr)
			= (-1,[],No)
	try_replace_case_alts_by_default [ap1=:{ap_expr=BasicExpr (BVB bool1)},ap2,{ap_expr=BasicExpr (BVB bool3)}:algebraic_patterns4]
		| bool1==bool3 && all_case_alt_results_this_bool algebraic_patterns4 bool1 && no_partial_match ap2.ap_expr
			= (1,[ap2],Yes ap1.ap_expr)
			= (-1,[],No)
	try_replace_case_alts_by_default [ap1,ap2=:{ap_expr=BasicExpr (BVB bool2)},{ap_expr=BasicExpr (BVB bool3)}:algebraic_patterns4]
		| bool2==bool3 && all_case_alt_results_this_bool algebraic_patterns4 bool2 && no_partial_match ap1.ap_expr
			= (0,[ap1],Yes ap2.ap_expr)
			= (-1,[],No)
	try_replace_case_alts_by_default algebraic_patterns
		= (-1,[],No)
	
	skip_case_alt_results_this_bool :: ![AlgebraicPattern] !Bool !Int -> (![AlgebraicPattern],!Int)
	skip_case_alt_results_this_bool [{ap_expr=BasicExpr (BVB bool1)}:algebraic_patterns2] bool guard_n
		| bool1==bool
			= skip_case_alt_results_this_bool algebraic_patterns2 bool (guard_n+1)
	skip_case_alt_results_this_bool algebraic_patterns bool guard_n
		= (algebraic_patterns,guard_n)
	
	all_case_alt_results_this_bool :: ![AlgebraicPattern] !Bool -> Bool
	all_case_alt_results_this_bool [{ap_expr=BasicExpr (BVB bool1)}:algebraic_patterns2] bool
		= bool1==bool && all_case_alt_results_this_bool algebraic_patterns2 bool
	all_case_alt_results_this_bool [_:_] bool
		= False
	all_case_alt_results_this_bool [] bool
		= True
		
	no_partial_match (Case {case_explicit})
		= case_explicit
	no_partial_match (Let {let_expr})
		= no_partial_match let_expr
	no_partial_match _
		= True
optimise_case case_guards case_default case_info_ptr common_defs expr_heap
	= (case_guards,case_default,expr_heap)

class convertRootCases a :: !ConvertInfo !a *ConvertState -> (a, *ConvertState)

instance convertRootCases TransformedBody where
	convertRootCases ci body=:{tb_rhs} cs
		# (tb_rhs, cs) = convertRootCases ci tb_rhs cs
		= ({body & tb_rhs=tb_rhs}, cs)

instance convertRootCases Expression where
	convertRootCases ci (Let lad=:{let_strict_binds,let_lazy_binds,let_expr,let_info_ptr}) cs=:{cs_var_heap}
		# (let_strict_binds,let_lazy_binds,ci,cs) = convert_let_binds let_strict_binds let_lazy_binds let_info_ptr ci cs
		  (let_expr, cs)			= convertRootCases (if (isEmpty let_strict_binds) ci {ci & ci_case_level=CaseLevelAfterGuardRoot}) let_expr cs
		= (Let { lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr }, cs)

	convertRootCases ci caseExpr=:(Case kees=:{case_expr, case_guards, case_default, case_explicit, case_info_ptr}) cs
		# (EI_CaseTypeAndSplits _ {sic_case_kind}, cs_expr_heap)
			= readPtr case_info_ptr cs.cs_expr_heap
		  cs & cs_expr_heap = cs_expr_heap
		= case sic_case_kind of
			CaseKindGuard
				-> case case_guards of
					BasicPatterns BT_Bool patterns
						# ({case_expr, case_guards, case_default, case_explicit, case_info_ptr}, cs)
							=	splitCase ci kees cs
						->	convert_boolean_case_into_guard ci case_expr patterns case_default case_info_ptr cs
					_
						->	abort "convertcases, convertRootCases: bool patterns expected"
			CaseKindLeave
				# (kees=:{case_expr, case_guards, case_default, case_explicit, case_info_ptr}, cs)
						=	splitCase ci kees cs
				  (EI_CaseTypeAndSplits case_type _, cs_expr_heap) = readPtr case_info_ptr cs.cs_expr_heap
		  		  (case_expr, cs)	= convertCases ci case_expr {cs & cs_expr_heap=cs_expr_heap}
				  (case_guards, cs) = convertRootCasesCasePatterns ci case_guards case_type.ct_cons_types cs
				  (case_default, cs)= convertRootCases ci case_default cs
				  (case_guards,case_default,expr_heap) = optimise_case case_guards case_default case_info_ptr ci.ci_common_defs cs.cs_expr_heap
				  cs & cs_expr_heap = expr_heap
				-> (Case {kees & case_expr=case_expr, case_guards=case_guards, case_default=case_default}, cs)
			CaseKindTransform
				->	convertNonRootCase ci kees cs
			CaseKindUnknown
				->	abort "convertRootCases, unknown casekind"
	where
		convert_boolean_case_into_guard ci guard [ alt=:{bp_value=BVB sign_of_then_part,bp_expr} : alts ] case_default case_info_ptr cs
			# (guard, cs) = convert_guard guard ci cs
			# (then_part, cs) = convertRootCases {ci & ci_case_level=CaseLevelAfterGuardRoot} bp_expr cs			
			# (opt_else_part, cs) = convert_to_else_part ci sign_of_then_part alts case_default cs
			= (build_conditional sign_of_then_part guard then_part opt_else_part, cs)
		where
			build_conditional True guard then_expr opt_else_expr
				= Conditional { if_cond = guard, if_then = then_expr, if_else = opt_else_expr }
			build_conditional false guard then_expr (Yes else_expr)
				= Conditional { if_cond = guard, if_then = else_expr, if_else = Yes then_expr }
			build_conditional false guard then_expr No
				= Conditional { if_cond = Conditional { if_cond = guard, if_then = BasicExpr (BVB False), if_else = Yes (BasicExpr (BVB True)) },
									if_then = then_expr, if_else = No }

			convert_to_else_part ci sign_of_then_part [ alt=:{bp_value=BVB sign_of_else_part,bp_expr} : alts ] case_default cs
				# (else_part, cs) = convertRootCases {ci & ci_case_level=CaseLevelAfterGuardRoot} bp_expr cs
				| sign_of_then_part == sign_of_else_part
					= convert_to_else_part ci sign_of_then_part alts case_default cs
					= (Yes else_part, cs)
			convert_to_else_part ci sign_of_then_part [ ] (Yes else_part) cs
				# (else_part, cs) = convertRootCases {ci & ci_case_level=CaseLevelAfterGuardRoot} else_part cs
				= (Yes else_part, cs)
			convert_to_else_part ci sign_of_then_part [ ] No cs
				= (No, cs)
		
			convert_guard guard ci cs
				| has_no_rooted_non_if_cases guard
					= convert_condition guard ci cs
					= convertCases ci guard cs
	convertRootCases ci failExpr=:(FailExpr _) cs
		= (failExpr, cs)
	convertRootCases ci expr cs
		= convertCases ci expr cs

splitCase :: ConvertInfo Case *ConvertState -> (Case, *ConvertState)
splitCase ci kees=:{case_info_ptr} cs=:{cs_expr_heap}
	# (EI_CaseTypeAndSplits case_type splits=:{sic_next_alt, sic_splits}, cs_expr_heap)
		=	readPtr case_info_ptr cs_expr_heap
	# (kees, cs_expr_heap)
		=	addDefault sic_next_alt kees cs_expr_heap
	| isEmpty sic_splits
		// optimisation for the common case
		=	(kees, {cs & cs_expr_heap = cs_expr_heap}) ->> ("split: no", toString kees.case_ident, ptrToInt kees.case_info_ptr)
	# sic_splits
		=	uniq (sortBy (>) sic_splits)

	# cs_expr_heap = cs_expr_heap <:= (case_info_ptr, EI_CaseTypeAndSplits case_type {splits & sic_splits = []})

	# (kees, case_type, cs=:{cs_expr_heap})
		=	split ci sic_splits (kees, case_type, {cs & cs_expr_heap = cs_expr_heap}) // ->> ("split: yes", toString kees.case_ident, ptrToInt kees.case_info_ptr) //, sic_splits)
	=	(kees, {cs & cs_expr_heap = cs_expr_heap})

class split a :: ConvertInfo a (Case, CaseType, *ConvertState) -> (Case, CaseType, *ConvertState)

instance split [a] | split a where
	split ci splits state
		=	foldSt (split ci) splits state

instance split SplitCase where
	split ci split=:{sc_alt_nr} (kees, case_type, cs=:{cs_expr_heap})
		# (kees1, kees2)
			=	splitIt sc_alt_nr kees
		# (case_type1, case_type2)
			=	splitIt sc_alt_nr case_type
		# case_type_and_splits2
			=	EI_CaseTypeAndSplits case_type2 {sic_splits = [], sic_next_alt = No, sic_case_kind = CaseKindUnknown}
		# (case_info_ptr2, cs_expr_heap)
			=	newPtr case_type_and_splits2 cs_expr_heap

		# kees2 = {kees2 & case_info_ptr = case_info_ptr2}
		# (call, cs)
			=	convertNonRootCase ci kees2 {cs & cs_expr_heap = cs_expr_heap}
		# kees1 = {kees1 & case_default = Yes call}
		# (EI_CaseTypeAndSplits _ splits1, cs_expr_heap)
			=	readPtr kees.case_info_ptr cs.cs_expr_heap
		# case_type_and_splits1
			=	EI_CaseTypeAndSplits case_type1 {splits1 & sic_splits = [{split & sc_call = Yes call} : splits1.sic_splits]}
		# cs_expr_heap = cs_expr_heap <:= (kees.case_info_ptr, case_type_and_splits1)
		=	(kees1, case_type1, {cs & cs_expr_heap = cs_expr_heap})

class splitIt a :: CaseAltNr a -> (a, a)

instance splitIt Case where
	splitIt alt_nr kees=:{case_guards}
		# (case_guards1, case_guards2)
			=	splitIt alt_nr case_guards
		# kees1
			=	{kees & case_guards = case_guards1, case_default=No}
		# kees2
			=	{kees & case_guards = case_guards2}
		=	(kees1, kees2)

instance splitIt CaseType where
	splitIt alt_nr case_type=:{ct_cons_types} 
		# (ct_cons_types1, ct_cons_types2)
			=	splitIt alt_nr ct_cons_types
		# case_type1
			=	{case_type & ct_cons_types = ct_cons_types1}
		# case_type2
			=	{case_type & ct_cons_types = ct_cons_types2}
		=	(case_type1, case_type2)

instance splitIt CasePatterns where
	splitIt alt_nr (AlgebraicPatterns type alts)
		# (alts1, alts2)
			=	splitIt alt_nr alts
		=	(AlgebraicPatterns type alts1, AlgebraicPatterns type alts2)
	splitIt alt_nr (BasicPatterns type alts)
		# (alts1, alts2)
			=	splitIt alt_nr alts
		=	(BasicPatterns type alts1, BasicPatterns type alts2)
	splitIt alt_nr (OverloadedListPatterns type decons alts)
		# (alts1, alts2)
			=	splitIt alt_nr alts
		=	(OverloadedListPatterns type decons alts1, OverloadedListPatterns type decons alts2)

instance splitIt [a] where
	splitIt alt_nr l
		=	(take alt_nr l, drop alt_nr l)

instance < SplitCase where
	(<) a b
		=	a.sc_alt_nr < b.sc_alt_nr

instance == SplitCase where
	(==) a b
		=	a.sc_alt_nr == b.sc_alt_nr

uniq :: [a] -> [a] | Eq a
uniq [a : rest =: [b : t]]
    | a == b
        =   uniq rest
    // otherwise
        =   [a : uniq rest]
uniq l
    =   l

class addDefault a :: a Case *ExpressionHeap -> (Case, *ExpressionHeap)

instance addDefault (Optional a) | addDefault a where
	addDefault (Yes next_alt) kees expr_heap
		=	addDefault next_alt kees expr_heap
	addDefault _ kees expr_heap
		=	(kees, expr_heap)

instance addDefault NextAlt where
	addDefault next_alt kees expr_heap
		# (call, expr_heap)
			=	find_call next_alt expr_heap
		=	addDefault call kees expr_heap
		where
			find_call :: NextAlt *ExpressionHeap -> (Expression, *ExpressionHeap)
			find_call {na_case, na_alt_nr} expr_heap
				# (EI_CaseTypeAndSplits case_type {sic_splits}, expr_heap)
					=	readPtr na_case expr_heap
				# call
					=	hd	[	call
							\\	{sc_call=Yes call, sc_alt_nr} <- sic_splits
							|	sc_alt_nr==na_alt_nr
							]
				=	(call,	expr_heap)

instance addDefault Expression where
	addDefault expr kees=:{case_default=No} expr_heap
		=	({kees & case_default=Yes expr}, expr_heap) <<- ("default added to ", ptrToInt kees.case_info_ptr)
	addDefault expr kees expr_heap
		=	abort ("trying to overwrite default of " +++ toString (ptrToInt kees.case_info_ptr) +++ " " +++ toString kees.case_ident)

convertRootCasesCasePatterns :: ConvertInfo CasePatterns [[AType]] *ConvertState -> (CasePatterns, *ConvertState)
convertRootCasesCasePatterns ci (BasicPatterns bt patterns) _ cs
	# (patterns, cs)
		=	convertRootCases ci patterns cs
	=	(BasicPatterns bt patterns, cs)
convertRootCasesCasePatterns ci (AlgebraicPatterns gi patterns) arg_types cs
	# (patterns, cs)
		=	convertRootCasesAlgebraicPatterns ci (exactZip patterns arg_types) cs
	=	(AlgebraicPatterns gi patterns, cs)
convertRootCasesCasePatterns ci (OverloadedListPatterns type decons_expr patterns) arg_types cs
	# (patterns, cs)
		=	convertRootCasesAlgebraicPatterns ci (exactZip patterns arg_types) cs
	=	(OverloadedListPatterns type decons_expr patterns, cs)

convertRootCasesAlgebraicPatterns :: ConvertInfo [(AlgebraicPattern, [AType])] *ConvertState -> ([AlgebraicPattern], *ConvertState)
convertRootCasesAlgebraicPatterns ci l cs
	=	mapSt (convertRootCasesAlgebraicPattern ci) l cs 
where
	convertRootCasesAlgebraicPattern :: ConvertInfo (AlgebraicPattern, [AType]) *ConvertState -> (AlgebraicPattern, *ConvertState)
	convertRootCasesAlgebraicPattern ci (pattern=:{ap_expr, ap_vars}, arg_types) cs
		# ci = {ci & ci_bound_vars= exactZip ap_vars arg_types ++ ci.ci_bound_vars}
		# (ap_expr, cs) = convertRootCases ci ap_expr cs
		=	({pattern & ap_expr=ap_expr}, cs)

instance convertRootCases (Optional a) | convertRootCases a where
	convertRootCases ci (Yes expr) cs
		# (expr, cs) = convertRootCases ci expr cs
		= (Yes expr, cs)
	convertRootCases ci No cs
		= (No, cs)

instance convertRootCases [a] | convertRootCases a where
	convertRootCases ci l cs
		=	mapSt (convertRootCases ci) l cs 

instance convertRootCases BasicPattern where
	convertRootCases ci pattern=:{bp_expr} cs
		# (bp_expr, cs)
			=	convertRootCases ci bp_expr cs
		=	({pattern & bp_expr=bp_expr}, cs)

class convertCases a :: !ConvertInfo !a !*ConvertState -> (!a, !*ConvertState)

instance convertCases [a] | convertCases a
where
	convertCases ci l cs = mapSt (convertCases ci) l cs 

instance convertCases (a,b) | convertCases a & convertCases b
where
	convertCases ci t cs
		= app2St (convertCases ci, convertCases ci) t cs

instance convertCases (Bind a b) | convertCases a
where
	convertCases ci bind=:{bind_src} cs
		# (bind_src, cs) = convertCases ci bind_src cs
		= ({ bind & bind_src = bind_src }, cs)

instance convertCases LetBind
where
	convertCases ci bind=:{lb_src} cs
		# (lb_src, cs) = convertCases ci lb_src cs
		= ({ bind & lb_src = lb_src }, cs)

instance convertCases DynamicExpr
where
	convertCases ci dynamik=:{dyn_expr} cs
		# (dyn_expr, cs) = convertCases ci dyn_expr cs
		= ({ dynamik & dyn_expr = dyn_expr }, cs)

instance convertCases Let
where
	convertCases ci lad=:{let_strict_binds,let_lazy_binds,let_expr,let_info_ptr} cs
		# (let_strict_binds,let_lazy_binds,ci,cs) = convert_let_binds let_strict_binds let_lazy_binds let_info_ptr ci cs
		# (let_expr, cs) = convertCases ci let_expr cs
		= ({ lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr }, cs)

instance convertCases Expression  
where
	convertCases ci (App app=:{app_args}) cs
		# (app_args, cs) = convertCases ci app_args cs
		= (App {app & app_args = app_args}, cs)
	convertCases ci (fun_expr @ exprs) cs
		# ((fun_expr, exprs), cs) = convertCases ci (fun_expr, exprs) cs
		= (fun_expr @ exprs, cs)
	convertCases ci (Let lad) cs
		# (lad, cs) = convertCases ci lad cs
		= (Let lad, cs)
	convertCases ci (Selection is_unique expr selectors) cs
		# (expr, cs) = convertCases ci expr cs
		  (selectors, cs) = convertCases ci selectors cs
		= (Selection is_unique expr selectors, cs)		
	convertCases ci (Update expr1 selectors expr2) cs
		# (expr1, cs) = convertCases ci expr1 cs
		  (selectors, cs) = convertCases ci selectors cs
		  (expr2, cs) = convertCases ci expr2 cs
		= (Update expr1 selectors expr2, cs)		
	convertCases ci (RecordUpdate cons_symbol expr exprs) cs
		# (expr, cs) = convertCases ci expr cs
		  (exprs, cs) = convertCases ci exprs cs
		= (RecordUpdate cons_symbol expr exprs, cs)		
	convertCases ci (TupleSelect tuple_symbol arg_nr expr) cs
		# (expr, cs) = convertCases ci expr cs
		= (TupleSelect tuple_symbol arg_nr expr, cs)
	convertCases ci (Case case_expr) cs
		// this is a case on a non-root position
		# {ss_expr_heap, ss_var_heap}
		  	= findSplitCases {si_next_alt=No, si_force_next_alt=False} case_expr
		  				{ss_var_heap=cs.cs_var_heap,ss_expr_heap = cs.cs_expr_heap}
		  cs = {cs & cs_var_heap=ss_var_heap, cs_expr_heap = ss_expr_heap}
		= convertNonRootCase ci case_expr cs
	convertCases ci (MatchExpr constructor expr) cs
		# (expr, cs) = convertCases ci expr cs
		= (MatchExpr constructor expr, cs)
	convertCases ci=:{ci_common_defs} is_cons_expr=:(IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) cs
		# (expr, cs=:{cs_var_heap,cs_expr_heap}) = convertCases ci expr cs

		  (new_info_ptr, cs_var_heap) = newPtr VI_LocalVar cs_var_heap
		  var_id = {id_name = "_x", id_info = nilPtr}
		  case_var = Var {var_ident = var_id, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr}
		  case_free_var = {	fv_def_level = NotALevel, fv_ident = var_id, fv_info_ptr = new_info_ptr, fv_count = 0}

		  fail_expr = BasicExpr (BVB False)
		  true_expr = BasicExpr (BVB True)
		  (var_args,cs_var_heap) = make_free_vars cons_arity cs_var_heap
		  pattern = {ap_symbol = cons_symbol, ap_vars = var_args, ap_expr = true_expr, ap_position = position}
		  patterns = AlgebraicPatterns global_type_index [pattern]
		  (case_expr_ptr, cs_expr_heap) = newPtr EI_Empty cs_expr_heap
		  case_expr = Case {case_expr = case_var, case_guards = patterns, case_default = Yes fail_expr, case_ident = No,
							case_explicit = False, case_info_ptr = case_expr_ptr, case_default_pos = NoPos}
		  cs & cs_var_heap=cs_var_heap, cs_expr_heap=cs_expr_heap

		  bool_type = {at_attribute = TA_None, at_type = TB BT_Bool}

		  algebraic_type = new_vars_in_algebraic_type ci_common_defs.[cons_symbol.glob_module].com_cons_defs.[cons_symbol.glob_object.ds_index].cons_type.st_result

		  (fun_ident,cs) = new_case_function (Yes case_ident) bool_type case_expr [(case_free_var,algebraic_type)] [] ci.ci_group_index cs
		= (App {app_symb=fun_ident, app_args=[expr], app_info_ptr=nilPtr}, cs)
	where
		make_free_vars :: !Int !*VarHeap -> (![FreeVar],!*VarHeap)
		make_free_vars n_args var_heap
			| n_args>0
				# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
				  (free_vars,var_heap) = make_free_vars (n_args-1) var_heap
				= ([{fv_ident = {id_name = "_x", id_info = nilPtr}, fv_info_ptr = new_info_ptr, fv_def_level = NotALevel, fv_count = 0} : free_vars], var_heap)
				= ([],var_heap)

		new_vars_in_algebraic_type {at_attribute,at_type=TV tv}
			| no_attribute_var at_attribute
				= {at_attribute=at_attribute, at_type=TV {tv & tv_info_ptr=nilPtr}}
				= {at_attribute=new_vars_in_attribute_var at_attribute, at_type=TV {tv & tv_info_ptr=nilPtr}}
		new_vars_in_algebraic_type {at_attribute,at_type=TA type_symbol type_args}
			# type_args = new_vars_in_algebraic_type_args type_args
			| no_attribute_var at_attribute
				= {at_attribute=at_attribute, at_type=TA type_symbol type_args}
				= {at_attribute=new_vars_in_attribute_var at_attribute, at_type=TA type_symbol type_args}

		no_attribute_var TA_Unique = True
		no_attribute_var TA_None = True
		no_attribute_var TA_Multi = True
		no_attribute_var TA_Anonymous = True
		no_attribute_var TA_MultiOfPropagatingConsVar = True
		no_attribute_var _ = False

		new_vars_in_attribute_var (TA_Var attr_var)
			= TA_Anonymous
		new_vars_in_attribute_var (TA_RootVar attr_var)
			= TA_Anonymous

		new_vars_in_algebraic_type_args [type_arg:type_args]
			= [new_vars_in_algebraic_type type_arg:new_vars_in_algebraic_type_args type_args]
		new_vars_in_algebraic_type_args []
			= []
	convertCases ci (FailExpr ident) cs
		# (failExpr, cs)
			=	convertNonRootFail ci ident cs
		= (failExpr, cs)
	convertCases ci (DictionariesFunction dictionaries expr expr_type) cs
		# (expr,cs) = convertRootCases {ci & ci_case_level=CaseLevelRoot,ci_bound_vars=dictionaries++ci.ci_bound_vars} expr cs
	      (old_fv_info_ptr_values,var_heap) = store_VI_FreeVar_in_dictionary_vars_and_save_old_values dictionaries [] cs.cs_var_heap
	      (old_fv_info_ptr_values,var_heap) = store_VI_BoundVar_in_bound_vars_and_save_old_values ci.ci_bound_vars old_fv_info_ptr_values var_heap
		  (expr, {cp_free_vars,cp_var_heap,cp_local_vars}) = copy expr {cp_free_vars=[], cp_var_heap=var_heap, cp_local_vars=[]}
		  (bound_vars, free_typed_vars, var_heap) = retrieve_variables cp_free_vars cp_var_heap
		  (free_typed_dictinary_vars, var_heap) = retrieve_dictionary_variables dictionaries var_heap
		  cs = {cs & cs_var_heap = var_heap}
		  (fun_ident,cs) = new_case_function No expr_type expr (free_typed_vars++free_typed_dictinary_vars) cp_local_vars ci.ci_group_index cs
		  cs_var_heap = restore_old_fv_info_ptr_values old_fv_info_ptr_values (dictionaries++ci.ci_bound_vars) cs.cs_var_heap
		= (App {app_symb=fun_ident, app_args=bound_vars, app_info_ptr=nilPtr}, {cs & cs_var_heap=cs_var_heap})
	where
		store_VI_FreeVar_in_dictionary_vars_and_save_old_values [({fv_info_ptr,fv_ident},type):bound_vars] old_fv_info_ptr_values var_heap
			# (old_fv_info_ptr_value,var_heap) = readPtr fv_info_ptr var_heap
			  (new_info_ptr,var_heap) = newPtr (VI_Labelled_Empty "convertCases [FreeVar]") var_heap
			  var_heap = writePtr fv_info_ptr (VI_FreeVar fv_ident new_info_ptr 0 type) var_heap
			  (old_fv_info_ptr_values,var_heap) = store_VI_FreeVar_in_dictionary_vars_and_save_old_values bound_vars old_fv_info_ptr_values var_heap
			= ([old_fv_info_ptr_value:old_fv_info_ptr_values],var_heap)
		store_VI_FreeVar_in_dictionary_vars_and_save_old_values [] old_fv_info_ptr_values var_heap
			= (old_fv_info_ptr_values,var_heap)

		retrieve_dictionary_variables [({fv_info_ptr}, type):cp_free_vars] var_heap
			# (free_typed_vars, var_heap) = retrieve_dictionary_variables cp_free_vars var_heap
			  (VI_FreeVar name new_ptr count type, var_heap) = readPtr fv_info_ptr var_heap
			= ([({fv_def_level=NotALevel, fv_ident=name, fv_info_ptr=new_ptr, fv_count=count}, type) : free_typed_vars], var_heap)
		retrieve_dictionary_variables [] cp_var_heap
			= ([],cp_var_heap)
	convertCases ci expr cs
		= (expr, cs)

instance convertCases Selection  
where
	convertCases ci (DictionarySelection record selectors expr_ptr index_expr) cs
		# (index_expr, cs) = convertCases ci index_expr cs
		  (selectors, cs) = convertCases ci selectors cs
		= (DictionarySelection record selectors expr_ptr index_expr, cs)
	convertCases ci (ArraySelection selector expr_ptr index_expr) cs
		# (index_expr, cs) = convertCases ci index_expr cs
		= (ArraySelection selector expr_ptr index_expr, cs)
	convertCases ci selector cs
		= (selector, cs)

convertNonRootFail ci=:{ci_group_index, ci_common_defs} ident cs
	# result_type
		=	{	at_attribute = TA_None
			,	at_type = TV {tv_ident = { id_name = "a", id_info = nilPtr }, tv_info_ptr = nilPtr}
			}
	# (fun_ident, cs)
	  	=	new_case_function (Yes ident) result_type (FailExpr ident) [] [] ci_group_index cs
	= (App { app_symb = fun_ident, app_args = [], app_info_ptr = nilPtr }, cs)

convertNonRootCase ci=:{ci_bound_vars, ci_group_index, ci_common_defs} kees=:{ case_expr, case_ident, case_info_ptr} cs
	# (is_degenerate, defoult)
		=	case_is_degenerate kees
	| is_degenerate
		# (EI_CaseTypeAndSplits case_type _, cs_expr_heap) = readPtr case_info_ptr cs.cs_expr_heap
		  cs = { cs & cs_expr_heap = cs_expr_heap }
		  (defoult, cs) = convertRootCases {ci & ci_case_level=CaseLevelRoot} defoult cs
		  (act_vars, form_vars, local_vars, caseExpr, old_fv_info_ptr_values,cs_var_heap)
				= copy_case_expr ci_bound_vars defoult cs.cs_var_heap	
		  cs = { cs & cs_var_heap = cs_var_heap}
	
		  (fun_ident, cs) = new_case_function_and_restore_old_fv_info_ptr_values case_ident case_type.ct_result_type caseExpr
		  							form_vars local_vars
		  							ci_bound_vars old_fv_info_ptr_values ci_group_index ci_common_defs cs
		= (App {app_symb = fun_ident, app_args = act_vars, app_info_ptr = nilPtr}, cs)

	// otherwise

	# (EI_CaseTypeAndSplits case_type splits, cs_expr_heap) = readPtr case_info_ptr cs.cs_expr_heap
	  cs_expr_heap = writePtr case_info_ptr (EI_CaseTypeAndSplits case_type {splits & sic_case_kind=CaseKindLeave}) cs_expr_heap
	  cs = { cs & cs_expr_heap = cs_expr_heap }

	# (new_info_ptr, cs_var_heap) = newPtr VI_Empty cs.cs_var_heap
	  cs = { cs & cs_var_heap = cs_var_heap}
	= case case_expr of
		Var var=:{var_ident,var_info_ptr}
			# var_id = {id_name = var_ident.id_name, id_info = nilPtr}
			  case_var = Var {var_ident = var_id, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr}
			  case_free_var = {	fv_def_level = NotALevel, fv_ident = var_id, fv_info_ptr = new_info_ptr, fv_count = 0}
			  case_bound_var = (case_free_var,case_type.ct_pattern_type)

			# kees = {kees & case_expr=case_var, case_explicit=False}
			  (caseExpr, cs) = convertRootCases {ci & ci_case_level=CaseLevelRoot,ci_bound_vars=[case_bound_var : ci_bound_vars]} (Case kees) cs

			  cs & cs_var_heap = writePtr new_info_ptr VI_LocalVar cs.cs_var_heap

			  (not__x_variable,act_vars, form_vars, local_vars, caseExpr, old_fv_info_ptr_values,cs_var_heap)
				= copy_case_expr_and_use_new_var ci_bound_vars var new_info_ptr caseExpr cs.cs_var_heap
			  cs & cs_var_heap = cs_var_heap
			| not__x_variable
				# (fun_ident, cs) = new_case_function_and_restore_old_fv_info_ptr_values case_ident case_type.ct_result_type caseExpr
				  							form_vars local_vars
				  							ci_bound_vars old_fv_info_ptr_values ci_group_index ci_common_defs cs
				= (App { app_symb = fun_ident, app_args = act_vars, app_info_ptr = nilPtr }, cs)

				# (fun_ident, cs) = new_case_function_and_restore_old_fv_info_ptr_values case_ident case_type.ct_result_type caseExpr
				  							[case_bound_var : form_vars] local_vars
				  							ci_bound_vars old_fv_info_ptr_values ci_group_index ci_common_defs cs
				= (App { app_symb = fun_ident, app_args = [case_expr : act_vars], app_info_ptr = nilPtr }, cs)
		_
			# var_id = {id_name = "_x", id_info = nilPtr}
			  case_var = Var {var_ident = var_id, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr}
			  case_free_var = {	fv_def_level = NotALevel, fv_ident = var_id, fv_info_ptr = new_info_ptr, fv_count = 0}
			  case_bound_var = (case_free_var,case_type.ct_pattern_type)

			# kees = {kees & case_expr=case_var, case_explicit=False}
			  (case_expr, cs) = convertCases ci case_expr cs
		
			  (caseExpr, cs) = convertRootCases {ci & ci_case_level=CaseLevelRoot,ci_bound_vars=[case_bound_var : ci_bound_vars]} (Case kees) cs

			  cs & cs_var_heap = writePtr new_info_ptr VI_LocalVar cs.cs_var_heap

			  (act_vars, form_vars, local_vars, caseExpr, old_fv_info_ptr_values,cs_var_heap)
					= copy_case_expr ci_bound_vars caseExpr cs.cs_var_heap
			  cs & cs_var_heap = cs_var_heap

			# (fun_ident, cs) = new_case_function_and_restore_old_fv_info_ptr_values case_ident case_type.ct_result_type caseExpr
			  							[case_bound_var : form_vars] local_vars
			  							ci_bound_vars old_fv_info_ptr_values ci_group_index ci_common_defs cs
			= (App { app_symb = fun_ident, app_args = [case_expr : act_vars], app_info_ptr = nilPtr }, cs)
where
	case_is_degenerate {case_guards = AlgebraicPatterns _ [], case_default=Yes defoult}
		=	(True, defoult)
	case_is_degenerate {case_guards = BasicPatterns _ [], case_default=Yes defoult}
		=	(True, defoult)
	case_is_degenerate {case_guards = OverloadedListPatterns _ _ [], case_default=Yes defoult}
		=	(True, defoult)
	case_is_degenerate _
		=	(False, undef)

	copy_case_expr :: [(FreeVar,AType)] Expression *VarHeap -> ([Expression],[(FreeVar,AType)],[FreeVar],Expression,[VarInfo],*VarHeap)
	copy_case_expr bound_vars guards_and_default var_heap
	    # (old_fv_info_ptr_values,var_heap) = store_VI_BoundVar_in_bound_vars_and_save_old_values bound_vars [] var_heap
		  (expr, {cp_free_vars, cp_var_heap, cp_local_vars}) = copy guards_and_default { cp_free_vars = [], cp_var_heap = var_heap, cp_local_vars = [] }
		  (bound_vars, free_typed_vars, var_heap) = retrieve_variables cp_free_vars cp_var_heap
		= (bound_vars, free_typed_vars, cp_local_vars, expr, old_fv_info_ptr_values,var_heap)
	
	copy_case_expr_and_use_new_var :: [(FreeVar,AType)] BoundVar VarInfoPtr Expression *VarHeap -> (Bool,[Expression],[(FreeVar,AType)],[FreeVar],Expression,[VarInfo],*VarHeap)
	copy_case_expr_and_use_new_var bound_vars {var_ident,var_info_ptr} new_info_ptr guards_and_default var_heap
	    # (old_fv_info_ptr_values,var_heap) = store_VI_BoundVar_in_bound_vars_and_save_old_values bound_vars [] var_heap
		# (var_info, var_heap) = readPtr var_info_ptr var_heap
		= case var_info of
			VI_BoundVar type
				# var_heap = var_heap <:= (var_info_ptr, VI_FreeVar var_ident new_info_ptr 0 type)
				  (expr, {cp_free_vars, cp_var_heap, cp_local_vars}) = copy guards_and_default {cp_free_vars = [(var_info_ptr, type)], cp_var_heap = var_heap, cp_local_vars = []}
				  (bound_vars, free_typed_vars, var_heap) = retrieve_variables cp_free_vars cp_var_heap
				-> (True,bound_vars, free_typed_vars, cp_local_vars, expr, old_fv_info_ptr_values,var_heap)
			VI_LocalVar
				# (expr, {cp_free_vars, cp_var_heap, cp_local_vars}) = copy guards_and_default {cp_free_vars = [], cp_var_heap = var_heap, cp_local_vars = []}
				  (bound_vars, free_typed_vars, var_heap) = retrieve_variables cp_free_vars cp_var_heap
				-> (False,bound_vars, free_typed_vars, cp_local_vars, expr, old_fv_info_ptr_values,var_heap)

store_VI_BoundVar_in_bound_vars_and_save_old_values [({fv_info_ptr},type):bound_vars] old_fv_info_ptr_values var_heap
	# (old_fv_info_ptr_value,var_heap)=readPtr fv_info_ptr var_heap
	# var_heap=writePtr fv_info_ptr (VI_BoundVar type) var_heap
	# (old_fv_info_ptr_values,var_heap) = store_VI_BoundVar_in_bound_vars_and_save_old_values bound_vars old_fv_info_ptr_values var_heap
	= ([old_fv_info_ptr_value:old_fv_info_ptr_values],var_heap)
store_VI_BoundVar_in_bound_vars_and_save_old_values [] old_fv_info_ptr_values var_heap
	= (old_fv_info_ptr_values,var_heap)

retrieve_variables cp_free_vars cp_var_heap
	= foldSt retrieve_variable cp_free_vars ([], [], cp_var_heap)
where
	retrieve_variable (var_info_ptr, type) (bound_vars, free_typed_vars, var_heap)
		# (VI_FreeVar name new_ptr count type, var_heap) = readPtr var_info_ptr var_heap
		= ( [Var { var_ident = name, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr} : bound_vars],
			[({ fv_def_level = NotALevel, fv_ident = name, fv_info_ptr = new_ptr, fv_count = count }, type) : free_typed_vars], var_heap)

new_case_function_and_restore_old_fv_info_ptr_values opt_id result_type rhs free_vars local_vars
	bound_vars old_fv_info_ptr_values group_index common_defs cs
	# (fun_ident,cs) = new_case_function opt_id result_type rhs free_vars local_vars group_index cs
	# cs_var_heap=restore_old_fv_info_ptr_values old_fv_info_ptr_values bound_vars cs.cs_var_heap
	= (fun_ident,{ cs & cs_var_heap = cs_var_heap});

restore_old_fv_info_ptr_values [old_fv_info_ptr_value:old_fv_info_ptr_values] [({fv_info_ptr},type):bound_vars] var_heap
	# var_heap=writePtr fv_info_ptr old_fv_info_ptr_value var_heap
	= restore_old_fv_info_ptr_values old_fv_info_ptr_values bound_vars var_heap
restore_old_fv_info_ptr_values [] bound_vars var_heap
	= var_heap

new_case_function opt_id result_type rhs free_vars local_vars group_index cs=:{cs_expr_heap}
	# body = TransformedBody {tb_args=[var \\ (var, _) <- free_vars], tb_rhs=rhs}
	  (_,type)
		=	removeAnnotations
			{	st_vars			= []
			,	st_args			= [type \\ (_, type) <- free_vars]
			,	st_args_strictness=NotStrict
			,	st_arity		= length free_vars
			,	st_result		= result_type
			,	st_context		= []
			,	st_attr_vars	= []
			,	st_attr_env		= []
			}
	# (fun_ident,  (cs_next_fun_nr, cs_new_functions, cs_fun_heap))
			= newFunctionWithType opt_id body local_vars type group_index
					(cs.cs_next_fun_nr, cs.cs_new_functions, cs.cs_fun_heap)
	= (fun_ident, { cs & cs_fun_heap = cs_fun_heap, cs_next_fun_nr = cs_next_fun_nr, cs_new_functions = cs_new_functions })

splitGuards :: CasePatterns -> [CasePatterns]
splitGuards (AlgebraicPatterns index patterns)
	=	[AlgebraicPatterns index [pattern] \\ pattern <- patterns]
splitGuards (BasicPatterns basicType patterns)
	=	[BasicPatterns basicType [pattern] \\ pattern <- patterns]
splitGuards (OverloadedListPatterns type decons_expr patterns)
	=	[OverloadedListPatterns type decons_expr [pattern] \\ pattern <- patterns]

::	CopyState =
	{	cp_free_vars	:: ![(VarInfoPtr,AType)]
	,	cp_local_vars	:: ![FreeVar]
	,	cp_var_heap		:: !.VarHeap
	}

class copy e :: !e !*CopyState -> (!e, !*CopyState)

instance copy BoundVar
where
	copy var=:{var_ident,var_info_ptr} cp_info=:{cp_var_heap}
		# (var_info, cp_var_heap) = readPtr var_info_ptr cp_var_heap
		  cp_info & cp_var_heap = cp_var_heap
		= case var_info of
			VI_FreeVar name new_info_ptr count type
				-> ({ var & var_info_ptr = new_info_ptr },
					{ cp_info & cp_var_heap = cp_info.cp_var_heap <:= (var_info_ptr, VI_FreeVar name new_info_ptr (inc count) type)})
			VI_LocalVar
				-> (var, cp_info)
			VI_BoundVar type
				# (new_info_ptr, cp_var_heap) = newPtr (VI_Labelled_Empty "copy [BoundVar]") cp_info.cp_var_heap
				-> ({ var & var_info_ptr = new_info_ptr },
					{ cp_info & cp_free_vars = [(var_info_ptr, type) : cp_info.cp_free_vars],
								cp_var_heap = cp_var_heap <:= (var_info_ptr, VI_FreeVar var_ident new_info_ptr 1 type) })
			_
				-> abort "copy [BoundVar] (convertcases)"

instance copy Expression
where
	copy (Var var) cp_info
		# (var, cp_info) = copy var cp_info
		= (Var var, cp_info)
	copy (App app=:{app_args}) cp_info
		# (app_args, cp_info) = copy app_args cp_info
		= (App {app & app_args = app_args}, cp_info)
	copy (fun_expr @ exprs) cp_info
		# ((fun_expr, exprs), cp_info) = copy (fun_expr, exprs) cp_info
		= (fun_expr @ exprs, cp_info)
 	copy (Let lad=:{let_strict_binds,let_lazy_binds, let_expr}) cp_info=:{cp_var_heap, cp_local_vars}
		# (cp_local_vars, cp_var_heap) = foldSt bind_let_var let_strict_binds (cp_local_vars, cp_var_heap)
		# (cp_local_vars, cp_var_heap) = foldSt bind_let_var let_lazy_binds (cp_local_vars, cp_var_heap)
		# (let_strict_binds, cp_info) = copy let_strict_binds {cp_info & cp_var_heap = cp_var_heap, cp_local_vars = cp_local_vars }
		# (let_lazy_binds, cp_info) = copy let_lazy_binds cp_info
		# (let_expr, cp_info) = copy let_expr cp_info
		= (Let {lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr }, cp_info)
	where
		bind_let_var {lb_dst} (local_vars, var_heap)
			= ([lb_dst : local_vars], var_heap <:= (lb_dst.fv_info_ptr, VI_LocalVar))
	copy (Case case_expr) cp_info
		# (case_expr, cp_info) = copy case_expr cp_info
		= (Case case_expr, cp_info)
	copy (Conditional cond) cp_info
		# (cond, cp_info) = copy cond cp_info
		= (Conditional cond, cp_info)
	copy expr=:(BasicExpr _) cp_info
		= (expr, cp_info)
	copy (Selection is_unique expr selectors) cp_info
		# (expr, cp_info) = copy expr cp_info
		  (selectors, cp_info) = copy selectors cp_info
		= (Selection is_unique expr selectors, cp_info)
	copy (Update expr1 selectors expr2) cp_info
		# (expr1, cp_info) = copy expr1 cp_info
		  (selectors, cp_info) = copy selectors cp_info
		  (expr2, cp_info) = copy expr2 cp_info
		= (Update expr1 selectors expr2, cp_info)
	copy (RecordUpdate cons_symbol expr exprs) cp_info
		# (expr, cp_info) = copy expr cp_info
		  (exprs, cp_info) = copy exprs cp_info
		= (RecordUpdate cons_symbol expr exprs, cp_info)
	copy (TupleSelect tuple_symbol arg_nr expr) cp_info
		# (expr, cp_info) = copy expr cp_info
		= (TupleSelect tuple_symbol arg_nr expr, cp_info)
	copy (MatchExpr constructor expr) cp_info
		# (expr, cp_info) = copy expr cp_info
		= (MatchExpr constructor expr, cp_info)
	copy (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) cp_info
		# (expr, cp_info) = copy expr cp_info
		= ((IsConstructor expr cons_symbol cons_arity global_type_index case_ident position), cp_info)
	copy fail=:(FailExpr _) cp_info
		= (fail, cp_info)
	copy EE cp_info
		= (EE, cp_info)
	copy (NoBind ptr) cp_info
		= (NoBind ptr, cp_info)
	copy expr cp_info
		= abort ("copy (Expression) does not match" ->> expr)

instance copy (Optional a) | copy a
where
	copy (Yes expr) cp_info
		# (expr, cp_info) = copy expr cp_info
		= (Yes expr, cp_info)
	copy No cp_info
		= (No, cp_info)

instance copy Selection  
where
	copy (DictionarySelection record selectors expr_ptr index_expr) cp_info
		# (index_expr, cp_info) = copy index_expr cp_info
		  (selectors, cp_info) = copy selectors cp_info
		  (record, cp_info) = copy record cp_info
		= (DictionarySelection record selectors expr_ptr index_expr, cp_info)
	copy (ArraySelection selector expr_ptr index_expr) cp_info
		# (index_expr, cp_info) = copy index_expr cp_info
		= (ArraySelection selector expr_ptr index_expr, cp_info)
	copy selector cp_info
		= (selector, cp_info)

instance copy Case
where
	copy this_case=:{case_expr, case_guards, case_default, case_info_ptr} cp_info
		# ((case_expr,(case_guards,case_default)), cp_info) = copy (case_expr,(case_guards,case_default)) cp_info
		= ({ this_case & case_expr = case_expr, case_guards = case_guards, case_default = case_default}, cp_info) 

instance copy Conditional
where
	copy cond=:{if_cond, if_then, if_else} cp_info
		# ((if_cond,(if_then, if_else)), cp_info) = copy (if_cond,(if_then, if_else)) cp_info
		= ({ cond & if_cond=if_cond, if_then=if_then, if_else=if_else}, cp_info) 

instance copy CasePatterns
where
	copy (AlgebraicPatterns type patterns) cp_info
		# (patterns, cp_info) = copy patterns cp_info
		= (AlgebraicPatterns type patterns, cp_info) 
	copy (BasicPatterns type patterns) cp_info
		# (patterns, cp_info) = copy patterns cp_info
		= (BasicPatterns type patterns, cp_info) 
	copy (OverloadedListPatterns type decons_expr patterns) cp_info
		# (patterns, cp_info) = copy patterns cp_info
		# (decons_expr, cp_info) = copy decons_expr cp_info
		= (OverloadedListPatterns type decons_expr patterns, cp_info) 

instance copy AlgebraicPattern
where
	copy pattern=:{ap_vars,ap_expr} cp_info=:{cp_local_vars, cp_var_heap}
		# (cp_local_vars, cp_var_heap) = foldSt bind_pattern_var ap_vars (cp_local_vars, cp_var_heap)
		# (ap_expr, cp_info) = copy ap_expr { cp_info & cp_local_vars = cp_local_vars, cp_var_heap = cp_var_heap}
		= ({ pattern & ap_expr = ap_expr }, cp_info) 
	where
		bind_pattern_var pattern_var=:{fv_info_ptr} (local_vars, var_heap)
			= ([pattern_var : local_vars], var_heap <:= (fv_info_ptr, VI_LocalVar))

instance copy BasicPattern
where
	copy pattern=:{bp_expr} cp_info
		# (bp_expr, cp_info) = copy bp_expr cp_info
		= ({ pattern & bp_expr = bp_expr }, cp_info) 

instance copy [a] | copy a
where
	copy l cp_info = mapSt copy l cp_info 

instance copy (a,b) | copy a & copy b
where
	copy t cp_info = app2St (copy, copy) t cp_info 

instance copy LetBind
where
	copy bind=:{lb_src} cp_info
		# (lb_src, cp_info) = copy lb_src cp_info
		= ({ bind & lb_src = lb_src }, cp_info) 

instance copy (Bind a b) | copy a
where
	copy bind=:{bind_src} cp_info
		# (bind_src, cp_info) = copy bind_src cp_info
		= ({ bind & bind_src = bind_src }, cp_info) 

instance <<< ExprInfo
where
	(<<<) file EI_Empty			= file <<< "*Empty*"
	(<<<) file (EI_CaseType _)	= file <<< "CaseType"

instance <<< (Ptr a)
where
	(<<<) file ptr = file <<< ptrToInt ptr  
/*
instance  <<<  FunctionBody
where
	(<<<) file (TransformedBody {tb_rhs}) = file <<<  tb_rhs

instance  <<<  CountedVariable
where
	(<<<) file {cv_variable,cv_count} = file <<< '<' <<< cv_variable <<< ',' <<< cv_count <<< '>'
*/

(-*->) infixl
(-*->) a b :== a // ---> b
(->>) infixl
(->>) a b :== a // ---> b
(<<-) infixl
(<<-) a b :== a // ---> b
