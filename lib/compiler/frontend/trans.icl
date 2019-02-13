implementation module trans

import StdEnv, StdStrictLists

import syntax, transform, checksupport, compare_types, utilities, expand_types, unitype, type
import classify, partition
from StdOverloadedList import RepeatnM,TakeM,++$,Any

SwitchCaseFusion			fuse dont_fuse :== fuse
SwitchGeneratedFusion		fuse dont_fuse :== fuse
SwitchFunctionFusion		fuse dont_fuse :== fuse
SwitchConstructorFusion		fuse fuse_generics dont_fuse :== fuse_generics
SwitchRnfConstructorFusion  rnf  linear	   :== rnf
SwitchCurriedFusion			fuse xtra dont_fuse :== fuse 
SwitchExtraCurriedFusion	fuse macro	   :== fuse//(fuse && macro)//fuse
SwitchTrivialFusion			fuse dont_fuse :== fuse
SwitchUnusedFusion			fuse dont_fuse :== fuse
SwitchTransformConstants	tran dont_tran :== tran
SwitchSpecialFusion			fuse dont_fuse :== fuse
SwitchArityChecks			check dont_check :== check
SwitchAutoFoldCaseInCase	fold dont	   :== fold
SwitchAutoFoldAppInCase		fold dont	   :== fold
SwitchAlwaysIntroduceCaseFunction yes no   :== no
SwitchNonRecFusion			fuse dont_fuse :== dont_fuse
SwitchHOFusion				fuse dont_fuse :== fuse
SwitchHOFusion`				fuse dont_fuse :== fuse
SwitchStrictPossiblyAddLet  strict lazy    :== lazy//strict

/*
(-!->) infix
(-!->) a b :== a  // ---> b
(<-!-) infix
(<-!-) a b :== a  // <--- b
*/

fromYes (Yes x) = x

is_SK_Function_or_SK_LocalMacroFunction (SK_Function _) = True
is_SK_Function_or_SK_LocalMacroFunction (SK_LocalMacroFunction _) = True
is_SK_Function_or_SK_LocalMacroFunction _ = False

undeff :== -1

empty_atype = { at_attribute = TA_Multi, at_type = TE }

get_producer_symbol (PR_Curried symbol arity)
	= (symbol,arity)
get_producer_symbol (PR_Function symbol arity _)
	= (symbol,arity)
get_producer_symbol (PR_GeneratedFunction symbol arity _)
	= (symbol,arity)
get_producer_symbol (PR_Constructor symbol arity _)
	= (symbol,arity)
get_producer_symbol (PR_CurriedFunction symbol arity _)
	= (symbol,arity)

// Extended variable info accessors...

readVarInfo :: VarInfoPtr *VarHeap -> (VarInfo, !*VarHeap)
readVarInfo var_info_ptr var_heap
	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_Extended _ original_var_info	-> (original_var_info, var_heap)
		_								-> (var_info, var_heap)

readExtendedVarInfo :: VarInfoPtr *VarHeap -> (ExtendedVarInfo, !*VarHeap)
readExtendedVarInfo var_info_ptr var_heap
	# (var_info, var_heap) = readPtr var_info_ptr var_heap
	= case var_info of
		VI_Extended extensions _	-> (extensions, var_heap)
		_							-> abort "Error in compiler: 'readExtendedVarInfo' failed in module trans.\n"

writeVarInfo :: VarInfoPtr VarInfo *VarHeap -> *VarHeap
writeVarInfo var_info_ptr new_var_info var_heap
	# (old_var_info, var_heap) = readPtr var_info_ptr var_heap
	= case old_var_info of
		VI_Extended extensions _	-> writePtr var_info_ptr (VI_Extended extensions new_var_info) var_heap
		_							-> writePtr var_info_ptr new_var_info var_heap

setExtendedVarInfo :: !VarInfoPtr !ExtendedVarInfo !*VarHeap -> *VarHeap
setExtendedVarInfo var_info_ptr extension var_heap
	# (old_var_info, var_heap) = readPtr var_info_ptr var_heap
	= case old_var_info of
		VI_Extended _ original_var_info	-> writePtr var_info_ptr (VI_Extended extension original_var_info) var_heap
		_								-> writePtr var_info_ptr (VI_Extended extension old_var_info) var_heap

// Extended expression info accessors...

readExprInfo :: !ExprInfoPtr !*ExpressionHeap -> (!ExprInfo,!*ExpressionHeap)
readExprInfo expr_info_ptr symbol_heap
	# (expr_info, symbol_heap) = readPtr expr_info_ptr symbol_heap
	= case expr_info of
		EI_Extended _ ei	-> (ei, symbol_heap)
		_					-> (expr_info, symbol_heap)

writeExprInfo :: !ExprInfoPtr !ExprInfo !*ExpressionHeap -> *ExpressionHeap
writeExprInfo expr_info_ptr new_expr_info symbol_heap
	# (expr_info, symbol_heap) = readPtr expr_info_ptr symbol_heap
	= case expr_info of
		EI_Extended extensions _	-> writePtr expr_info_ptr (EI_Extended extensions new_expr_info) symbol_heap
		_							-> writePtr expr_info_ptr new_expr_info symbol_heap

app_EEI_ActiveCase transformer expr_info_ptr expr_heap
	# (expr_info, expr_heap) = readPtr expr_info_ptr expr_heap
	= case expr_info of
		(EI_Extended (EEI_ActiveCase aci) original_expr_info)
			-> writePtr expr_info_ptr (EI_Extended (EEI_ActiveCase (transformer aci)) original_expr_info) expr_heap
		_	-> expr_heap

set_aci_free_vars_info_case unbound_variables case_info_ptr expr_heap
	= app_EEI_ActiveCase (\aci -> { aci & aci_free_vars=Yes unbound_variables }) case_info_ptr expr_heap

remove_aci_free_vars_info case_info_ptr expr_heap
	= app_EEI_ActiveCase (\aci->{aci & aci_free_vars = No }) case_info_ptr expr_heap

cleanup_attributes expr_info_ptr symbol_heap
	# (expr_info, symbol_heap) = readPtr expr_info_ptr symbol_heap
	= case expr_info of
		EI_Extended _ expr_info -> writePtr expr_info_ptr expr_info symbol_heap
		_ -> symbol_heap

//	TRANSFORM

::	*TransformInfo =
	{	ti_fun_defs				:: !*{# FunDef}
	,	ti_instances 			:: !*{! InstanceInfo }
	,	ti_cons_args 			:: !*{! ConsClasses}
	,	ti_new_functions 		:: ![FunctionInfoPtr]
	,	ti_fun_heap				:: !*FunctionHeap
	,	ti_var_heap				:: !*VarHeap
	,	ti_symbol_heap			:: !*ExpressionHeap
	,	ti_type_heaps			:: !*TypeHeaps
	,	ti_type_def_infos		:: !*TypeDefInfos
	,	ti_next_fun_nr			:: !Index
	,	ti_cleanup_info			:: !CleanupInfo
	,	ti_recursion_introduced	:: !Optional RI
	,	ti_error_file			:: !*File
	,	ti_predef_symbols		:: !*PredefinedSymbols
	}

:: RI = { ri_fun_index :: !Int, ri_fun_ptr :: !FunctionInfoPtr}

::	ReadOnlyTI = 
	{	ro_imported_funs	:: !{# {# FunType} }
	,	ro_common_defs		:: !{# CommonDefs }
// the following four are used when possibly generating functions for cases...
	,	ro_root_case_mode		:: !RootCaseMode
	,	ro_tfi					:: !TransformFunctionInfo
	,	ro_main_dcl_module_n 	:: !Int
	,	ro_transform_fusion		:: !Int	// fusion switch
	,	ro_StdStrictLists_module_n :: !Int
	,	ro_StdGeneric_module_n	:: !Int
	}

NoFusion:==0
OnlyGenericFusionMask:==1 // transform only generic functions
GenericFullFusionMask:==2 // full fusion for generic functions
FullFusion:==4
NoRecProdFusion:==8	// no recursive producers

::	TransformFunctionInfo =
	{	tfi_root				:: !SymbIdent		// original function
	,	tfi_case				:: !SymbIdent		// original function or possibly generated case
	,	tfi_args				:: ![FreeVar]		// args of above
	,	tfi_vars				:: ![FreeVar]		// strict variables
	,	tfi_orig				:: !SymbIdent		// original consumer
	,	tfi_n_args_before_producer :: !Int
	,	tfi_n_producer_args		:: !Int
	}

::	RootCaseMode = NotRootCase | RootCase | RootCaseOfZombie

::	CopyState = {
		cs_var_heap				:: !.VarHeap,
		cs_symbol_heap			:: !.ExpressionHeap,
		cs_opt_type_heaps		:: !.Optional .TypeHeaps,
		cs_cleanup_info			:: ![ExprInfoPtr]
	}

::	CopyInfo = { ci_handle_aci_free_vars	:: !AciFreeVarsHandleMode }

:: AciFreeVarsHandleMode = LeaveAciFreeVars | RemoveAciFreeVars | SubstituteAciFreeVars

neverMatchingCase (Yes ident)
	= FailExpr ident
neverMatchingCase _ 
	# ident = {id_name = "neverMatchingCase", id_info = nilPtr}
	= FailExpr ident

store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types patterns var_heap
	= fold2St store_type_info_of_alg_pattern ct_cons_types patterns var_heap
	where
		store_type_info_of_alg_pattern var_types {ap_vars} var_heap
			= fold2St store_type_info_of_pattern_var var_types ap_vars var_heap

		store_type_info_of_pattern_var var_type {fv_info_ptr} var_heap
			= setExtendedVarInfo fv_info_ptr (EVI_VarType var_type) var_heap

store_type_info_of_let_bindings_in_heap :: ![LetBind] ![LetBind] !ExprInfoPtr !*TransformInfo -> *TransformInfo
store_type_info_of_let_bindings_in_heap let_strict_binds let_lazy_binds let_info_ptr ti=:{ti_symbol_heap,ti_var_heap}
	# (EI_LetType var_types,symbol_heap) = readExprInfo let_info_ptr ti_symbol_heap
	  (var_types,var_heap) = store_strict_let_type_info let_strict_binds var_types ti_var_heap
	  var_heap = store_lazy_let_type_info let_lazy_binds var_types var_heap
	= {ti & ti_symbol_heap=symbol_heap, ti_var_heap=var_heap}
where
	store_strict_let_type_info [{lb_dst={fv_info_ptr}}:let_strict_binds] [var_type:var_types] var_heap
		# var_heap = setExtendedVarInfo fv_info_ptr (EVI_VarType var_type) var_heap
		= store_strict_let_type_info let_strict_binds var_types var_heap
	store_strict_let_type_info [] var_types var_heap
		= (var_types,var_heap)

	store_lazy_let_type_info [{lb_dst={fv_info_ptr}}:let_lazy_binds] [var_type:var_types] var_heap
		# var_heap = setExtendedVarInfo fv_info_ptr (EVI_VarType var_type) var_heap
		= store_lazy_let_type_info let_lazy_binds var_types var_heap
	store_lazy_let_type_info [] [] var_heap
		= var_heap

class transform a :: !a !ReadOnlyTI !*TransformInfo -> (!a, !*TransformInfo)

instance transform Expression
where
	transform (App app=:{app_args}) ro ti
		# (app_args, ti) = transform app_args ro ti
		= transformApplication {app & app_args = app_args} [] ro ti
	transform appl_expr=:(expr @ exprs) ro ti
		# (expr, ti) = transform expr ro ti
		  (exprs, ti) = transform exprs ro ti
		= case expr of
			App app
				-> transformApplication app exprs ro ti
			_
				-> (expr @ exprs, ti)
	transform (Let lad=:{let_strict_binds,let_lazy_binds,let_info_ptr,let_expr}) ro ti	
		# ti = store_type_info_of_let_bindings_in_heap let_strict_binds let_lazy_binds let_info_ptr ti
		  (let_strict_binds, ti) = transform let_strict_binds ro ti
		  (let_lazy_binds, ti) = transform let_lazy_binds ro ti
		  (let_expr, ti) = transform let_expr ro ti
		  lad = { lad & let_lazy_binds = let_lazy_binds, let_strict_binds = let_strict_binds, let_expr = let_expr}
		= (Let lad, ti)
	transform (Case kees) ro ti
		# ti = store_type_info_of_patterns_in_heap kees ti
		= transformCase kees ro ti
	  where
		store_type_info_of_patterns_in_heap {case_guards,case_info_ptr} ti
			= case case_guards of
				AlgebraicPatterns _ patterns
					# (EI_CaseType {ct_cons_types},ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
					  ti_var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types patterns ti.ti_var_heap
					-> { ti & ti_symbol_heap = ti_symbol_heap, ti_var_heap = ti_var_heap }
				BasicPatterns _ _
					-> ti // no variables occur
				OverloadedListPatterns _ _ patterns
					# (EI_CaseType {ct_cons_types},ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
					  ti_var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types patterns ti.ti_var_heap
					-> { ti & ti_symbol_heap = ti_symbol_heap, ti_var_heap = ti_var_heap }
				NoPattern
					-> ti

	transform (Selection opt_type expr selectors) ro ti
		# (expr, ti) = transform expr ro ti
		= transformSelection opt_type selectors expr ro ti
	transform (Update expr1 selectors expr2) ro ti
		# (expr1,ti) = transform expr1 ro ti
		# (selectors,ti) = transform_expressions_in_selectors selectors ro ti
		# (expr2,ti) = transform expr2 ro ti
		= (Update expr1 selectors expr2,ti)
	transform (RecordUpdate cons_symbol expr exprs) ro ti
		# (expr,ti) = transform expr ro ti
		# (exprs,ti) = transform_fields exprs ro ti
		=(RecordUpdate cons_symbol expr exprs,ti)
	where	
		transform_fields [] ro ti
			= ([],ti)
		transform_fields [bind=:{bind_src} : fields] ro ti
			# (bind_src,ti) = transform bind_src ro ti
			# (fields,ti) = transform_fields fields ro ti
			= ([{bind & bind_src=bind_src} : fields],ti)
	transform (TupleSelect a1 arg_nr expr) ro ti
		# (expr,ti) = transform expr ro ti
		= (TupleSelect a1 arg_nr expr,ti)
	transform (MatchExpr a1 expr) ro ti
		# (expr,ti) = transform expr ro ti
		= (MatchExpr a1 expr,ti)
	transform (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) ro ti
		# (expr,ti) = transform expr ro ti
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, ti)
	transform (DynamicExpr dynamic_expr) ro ti
		# (dynamic_expr, ti) = transform dynamic_expr ro ti
		= (DynamicExpr dynamic_expr, ti)
	transform (DictionariesFunction dictionaries expr expr_type) ro ti
		# ti & ti_var_heap = foldSt store_dictionary_type_info dictionaries ti.ti_var_heap
		# (expr,ti) = transform expr ro ti
		= (DictionariesFunction dictionaries expr expr_type,ti)
	where
		store_dictionary_type_info ({fv_info_ptr},a_type) ti_var_heap
			= setExtendedVarInfo fv_info_ptr (EVI_VarType a_type) ti_var_heap
	transform expr ro ti
		= (expr, ti)

transform_expressions_in_selectors [selection=:RecordSelection _ _ : selections] ro ti
	# (selections,ti) = transform_expressions_in_selectors selections ro ti
	= ([selection:selections],ti)
transform_expressions_in_selectors [ArraySelection ds ep expr : selections] ro ti
	# (expr,ti) = transform expr ro ti
	# (selections,ti) = transform_expressions_in_selectors selections ro ti
	= ([ArraySelection ds ep expr:selections],ti)
transform_expressions_in_selectors [DictionarySelection bv dictionary_selections ep expr : selections] ro ti
	# (expr,ti) = transform expr ro ti
	# (dictionary_selections,ti) = transform_expressions_in_selectors dictionary_selections ro ti
	# (selections,ti) = transform_expressions_in_selectors selections ro ti
	= ([DictionarySelection bv dictionary_selections ep expr:selections],ti)
transform_expressions_in_selectors [] ro ti
	= ([],ti)

instance transform DynamicExpr where
	transform dyn=:{dyn_expr} ro ti
		# (dyn_expr, ti) = transform dyn_expr ro ti
		= ({dyn & dyn_expr = dyn_expr}, ti)

transformCase this_case=:{case_expr,case_guards,case_default,case_ident,case_info_ptr} ro ti
	| SwitchCaseFusion (ro.ro_transform_fusion<FullFusion) True
		= skip_over this_case ro ti
	| isNilPtr case_info_ptr			// encountered neverMatchingCase?!
		= skip_over this_case ro ti
	# (case_info, ti_symbol_heap) = readPtr case_info_ptr ti.ti_symbol_heap
	  ti = { ti & ti_symbol_heap=ti_symbol_heap }
	  (result_expr, ti)	= case case_info of
							EI_Extended (EEI_ActiveCase aci) _
								| is_variable case_expr
									-> skip_over this_case ro ti
								-> case ro.ro_root_case_mode of
									NotRootCase
										-> transform_active_non_root_case this_case aci ro ti
									_
										-> transform_active_root_case aci this_case ro ti
							_
								-> skip_over this_case ro ti
	  ti = { ti & ti_symbol_heap = remove_aci_free_vars_info case_info_ptr ti.ti_symbol_heap }
	# final_expr = removeNeverMatchingSubcases result_expr ro
	= (final_expr, ti) // ---> ("transformCase",result_expr,final_expr)
where
	is_variable (Var _) = True
	is_variable _ 		= False

skip_over this_case=:{case_expr=case_expr=:BasicExpr basic_value,case_guards=case_guards=:BasicPatterns basic_type basicPatterns,case_default} ro ti
	// currently only active cases are matched at runtime (multimatch problem)
	# matching_patterns = [pattern \\ pattern=:{bp_value}<-basicPatterns | bp_value==basic_value]
	= case matching_patterns of
		[]
			-> case case_default of
				Yes default_expr
					-> transform default_expr {ro & ro_root_case_mode = NotRootCase} ti
				No
					# ro_lost_root = {ro & ro_root_case_mode = NotRootCase}
					# (new_case_expr, ti) = transform case_expr ro_lost_root ti
					-> (Case {this_case & case_expr=new_case_expr, case_guards=BasicPatterns basic_type []}, ti)
/*
					// The following does not work, because a FailExpr may only occur as else of an if in the backend */
					# never_ident = case ro.ro_root_case_mode of
										NotRootCase -> this_case.case_ident
										_ -> Yes ro.ro_tfi.tfi_case.symb_ident
					-> (neverMatchingCase never_ident, ti)
*/
		[{bp_expr}]
			| case_alt_matches_always bp_expr ro
				-> transform bp_expr {ro & ro_root_case_mode = NotRootCase} ti
		_
			# ro_lost_root = {ro & ro_root_case_mode = NotRootCase}
			  (new_case_expr, ti) = transform case_expr ro_lost_root ti
			  (new_case_guards, ti) = transform case_guards ro_lost_root ti
			  (new_case_default, ti) = transform case_default ro_lost_root ti
			-> (Case {this_case & case_expr=new_case_expr, case_guards=new_case_guards, case_default=new_case_default}, ti)
skip_over this_case=:{case_expr,case_guards,case_default} ro ti
	# ro_lost_root = { ro & ro_root_case_mode = NotRootCase }
	  (new_case_expr, ti) = transform case_expr ro_lost_root ti
	  (new_case_guards, ti) = transform case_guards ro_lost_root ti
	  (new_case_default, ti) = transform case_default ro_lost_root ti
	= (Case { this_case & case_expr=new_case_expr, case_guards=new_case_guards, case_default=new_case_default }, ti)

case_alt_matches_always (Case {case_default,case_explicit,case_guards}) ro
	| case_explicit
		= True
		= case case_default of
			Yes _
				-> True
			_
				-> case case_guards of
					AlgebraicPatterns {gi_module,gi_index} algebraic_patterns
						-> case ro.ro_common_defs.[gi_module].com_type_defs.[gi_index].td_rhs of
							AlgType constructors
								| same_length constructors algebraic_patterns
									-> algebraic_patterns_match_always algebraic_patterns ro
							RecordType _
								-> algebraic_patterns_match_always algebraic_patterns ro
							_
								-> False
					_
						-> False
case_alt_matches_always (Let {let_expr}) ro
	= case_alt_matches_always let_expr ro
case_alt_matches_always _ ro
	= True

algebraic_patterns_match_always [{ap_expr}:algebraic_patterns] ro
	= case_alt_matches_always ap_expr ro && algebraic_patterns_match_always algebraic_patterns ro
algebraic_patterns_match_always [] ro
	= True

free_vars_to_bound_vars free_vars
	= [Var {var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = nilPtr} \\ {fv_ident,fv_info_ptr} <- free_vars]

transform_active_root_case aci this_case=:{case_expr = Case case_in_case} ro ti
	= lift_case case_in_case this_case ro ti
where
	lift_case nested_case=:{case_guards,case_default} outer_case ro ti
		| isNilPtr nested_case.case_info_ptr	// neverMatchingCase ?!
			= skip_over outer_case ro ti
		# default_exists = case case_default of
							Yes _	-> True
							No		-> False
		  (case_guards, ti) = lift_patterns default_exists case_guards nested_case.case_info_ptr outer_case ro ti
		  (case_default, ti) = lift_default case_default outer_case ro ti
		  (EI_CaseType outer_case_type, ti_symbol_heap) = readExprInfo outer_case.case_info_ptr ti.ti_symbol_heap
		// the result type of the nested case becomes the result type of the outer case
		  ti_symbol_heap = overwrite_result_type nested_case.case_info_ptr outer_case_type.ct_result_type ti_symbol_heap
		// after this transformation the aci_free_vars information doesn't hold anymore
		  ti_symbol_heap = remove_aci_free_vars_info nested_case.case_info_ptr ti_symbol_heap
		  ti = { ti & ti_symbol_heap = ti_symbol_heap }
		= (Case {nested_case & case_guards = case_guards, case_default = case_default}, ti)
	  where
		overwrite_result_type case_info_ptr new_result_type ti_symbol_heap
			#! (EI_CaseType case_type, ti_symbol_heap)	= readExprInfo case_info_ptr ti_symbol_heap
			= writeExprInfo case_info_ptr (EI_CaseType { case_type & ct_result_type = new_result_type}) ti_symbol_heap

	lift_patterns default_exists (AlgebraicPatterns type case_guards) case_info_ptr outer_case ro ti
		# guard_exprs	= [ ap_expr \\ {ap_expr} <- case_guards ]
		  (EI_CaseType {ct_cons_types},symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
		  var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types case_guards ti.ti_var_heap
		  ti = {ti & ti_symbol_heap=symbol_heap,ti_var_heap=var_heap}
		  (guard_exprs_with_case, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= (AlgebraicPatterns type [ { case_guard & ap_expr=guard_expr } \\ case_guard<-case_guards & guard_expr<-guard_exprs_with_case], ti)
	lift_patterns default_exists (BasicPatterns basic_type case_guards) case_info_ptr outer_case ro ti
		# guard_exprs	= [ bp_expr \\ {bp_expr} <- case_guards ]
		  (guard_exprs_with_case, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= (BasicPatterns basic_type [ { case_guard & bp_expr=guard_expr } \\ case_guard<-case_guards & guard_expr<-guard_exprs_with_case], ti)
	lift_patterns default_exists (OverloadedListPatterns type decons_expr case_guards) case_info_ptr outer_case ro ti
		# guard_exprs	= [ ap_expr \\ {ap_expr} <- case_guards ]
		  (EI_CaseType {ct_cons_types},symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
		  var_heap = store_type_info_of_alg_pattern_in_pattern_variables ct_cons_types case_guards ti.ti_var_heap
		  ti = {ti & ti_symbol_heap=symbol_heap,ti_var_heap=var_heap}
		  (guard_exprs_with_case, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= (OverloadedListPatterns type decons_expr [ { case_guard & ap_expr=guard_expr } \\ case_guard<-case_guards & guard_expr<-guard_exprs_with_case], ti)

	lift_patterns_2 False [guard_expr] outer_case ro ti
		// if no default pattern exists, then the outer case expression does not have to be copied for the last pattern
		# (guard_expr, ti)						= possiblyFoldOuterCase True guard_expr outer_case ro ti
		= ([guard_expr], ti)
	lift_patterns_2 default_exists [guard_expr : guard_exprs] outer_case ro ti
		# (guard_expr, ti)						= possiblyFoldOuterCase False guard_expr outer_case ro ti
		  (guard_exprs, ti) = lift_patterns_2 default_exists guard_exprs outer_case ro ti
		= ([guard_expr : guard_exprs], ti)
	lift_patterns_2 _ [] _ _ ti
		= ([], ti)
		
	lift_default (Yes default_expr) outer_case ro ti
		# (default_expr, ti)					= possiblyFoldOuterCase True default_expr outer_case ro ti
		= (Yes default_expr, ti)
	lift_default No _ _ ti
		= (No, ti)

	possiblyFoldOuterCase final guard_expr outer_case ro=:{ro_tfi} ti
		| SwitchAutoFoldCaseInCase (isFoldExpression guard_expr ti.ti_fun_defs ti.ti_cons_args) False // otherwise GOTO next alternative
			| ro_tfi.tfi_n_args_before_producer < 0 || ro_tfi.tfi_n_producer_args < 0
				= possiblyFoldOuterCase` final guard_expr outer_case ro ti	//abort "possiblyFoldOuterCase: unexpected!\n"
			= case aci.aci_opt_unfolder of
				No
					-> possiblyFoldOuterCase` final guard_expr outer_case ro ti
				Yes _
					-> transformApplication (make_consumer_application ro_tfi guard_expr) [] ro ti
		= possiblyFoldOuterCase` final guard_expr outer_case ro ti
	where
		isFoldExpression (App app)	ti_fun_defs ti_cons_args = isFoldSymbol app.app_symb.symb_kind
			where
				isFoldSymbol (SK_Function {glob_module,glob_object})
					| glob_module==ro.ro_StdStrictLists_module_n
						# type_arity = ro.ro_imported_funs.[glob_module].[glob_object].ft_type.st_arity
						| type_arity==0 || (type_arity==2 && case app.app_args of [_:_] -> True; _ -> False)
							= False
							= True
					| glob_module==ro.ro_main_dcl_module_n && glob_object>=size ti_cons_args &&
						(ti_fun_defs.[glob_object].fun_info.fi_properties bitand FI_IsUnboxedListOfRecordsConsOrNil<>0) &&
							(case ti_fun_defs.[glob_object].fun_type of
								Yes type ->(type.st_arity==0 || (type.st_arity==2 && case app.app_args of [_:_] -> True; _ -> False)))
							= False						
						= True
				isFoldSymbol (SK_LocalMacroFunction _)	= True
				isFoldSymbol (SK_GeneratedFunction _ _)	= True
				isFoldSymbol _							= False
		isFoldExpression (Var _)	ti_fun_defs ti_cons_args = True
//		isFoldExpression (Case _)	ti_fun_defs ti_cons_args = True
		isFoldExpression _			ti_fun_defs ti_cons_args = False

	possiblyFoldOuterCase` final guard_expr outer_case ro ti
		| final
			# new_case = {outer_case & case_expr = guard_expr}
			= transformCase new_case ro ti // ---> ("possiblyFoldOuterCase`",Case new_case)
		# cs = {cs_var_heap = ti.ti_var_heap, cs_symbol_heap = ti.ti_symbol_heap, cs_opt_type_heaps = No, cs_cleanup_info=ti.ti_cleanup_info}
		  (outer_guards, cs=:{cs_cleanup_info})	= copyCasePatterns outer_case.case_guards No {ci_handle_aci_free_vars = LeaveAciFreeVars} cs
		  (expr_info, ti_symbol_heap)			= readPtr outer_case.case_info_ptr cs.cs_symbol_heap
		  (new_info_ptr, ti_symbol_heap)		= newPtr expr_info ti_symbol_heap
		  new_cleanup_info 						= case expr_info of
		  		EI_Extended _ _
		  			-> [new_info_ptr:cs_cleanup_info]
		  		_ 	-> cs_cleanup_info
		  ti = { ti & ti_var_heap = cs.cs_var_heap, ti_symbol_heap = ti_symbol_heap, ti_cleanup_info=new_cleanup_info }
		  new_case								= { outer_case & case_expr = guard_expr, case_guards=outer_guards, case_info_ptr=new_info_ptr }
		= transformCase new_case ro ti // ---> ("possiblyFoldOuterCase`",Case new_case)

transform_active_root_case aci this_case=:{case_expr = case_expr=:(App app=:{app_symb,app_args}),case_guards,case_default,case_explicit,case_ident,case_info_ptr} ro ti
	= case app_symb.symb_kind of
		SK_Constructor cons_index
			// currently only active cases are matched at runtime (multimatch problem)
			# aci_linearity_of_patterns = aci.aci_linearity_of_patterns
			  (may_be_match_expr, ti) = match_and_instantiate aci_linearity_of_patterns cons_index app_args case_guards case_info_ptr case_default ro ti
			-> expr_or_never_matching_case may_be_match_expr case_ident ti
		SK_Function {glob_module,glob_object}
			| glob_module==ro.ro_StdStrictLists_module_n &&
				(let type = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
				 in (type.st_arity==0 || (type.st_arity==2 && case app_args of [_:_] -> True; _ -> False)))
				# type = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
				-> trans_case_of_overloaded_nil_or_cons type case_info_ptr ti
			| glob_module==ro.ro_main_dcl_module_n && glob_object>=size ti.ti_cons_args &&
				(ti.ti_fun_defs.[glob_object].fun_info.fi_properties bitand FI_IsUnboxedListOfRecordsConsOrNil)<>0 &&
				(case ti.ti_fun_defs.[glob_object].fun_type of
					Yes type ->(type.st_arity==0 || (type.st_arity==2 && case app_args of [_:_] -> True; _ -> False)))
				# (Yes type,ti) = ti!ti_fun_defs.[glob_object].fun_type
				-> trans_case_of_overloaded_nil_or_cons type case_info_ptr ti
		// otherwise it's a function application
		_
			# {aci_params,aci_opt_unfolder} = aci
			-> case aci_opt_unfolder of
				No
					-> skip_over this_case ro ti									// -!-> ("transform_active_root_case","No opt unfolder")
				Yes unfolder
					| not (equal app_symb.symb_kind unfolder.symb_kind)
						// in this case a third function could be fused in
						-> possiblyFoldOuterCase this_case ro ti					// -!-> ("transform_active_root_case","Diff opt unfolder",unfolder,app_symb)
					# variables = [ Var {var_ident=fv_ident, var_info_ptr=fv_info_ptr, var_expr_ptr=nilPtr}
									\\ {fv_ident, fv_info_ptr} <- ro.ro_tfi.tfi_args ]
					  (app_symb, ti)
						= case ro.ro_root_case_mode /* -!-> ("transform_active_root_case","Yes opt unfolder",unfolder) */ of
							RootCaseOfZombie
								# (recursion_introduced,ti) = ti!ti_recursion_introduced
								  (ro_fun=:{symb_kind=SK_GeneratedFunction fun_info_ptr _}) = ro.ro_tfi.tfi_case
								-> case recursion_introduced of
									No
										# (ti_next_fun_nr, ti) = ti!ti_next_fun_nr
										  ri = {ri_fun_index=ti_next_fun_nr, ri_fun_ptr=fun_info_ptr}
										  ti & ti_next_fun_nr = inc ti_next_fun_nr, ti_recursion_introduced = Yes ri,
										  	   ti_new_functions = [fun_info_ptr:ti.ti_new_functions]
										-> ({ro_fun & symb_kind=SK_GeneratedFunction fun_info_ptr ti_next_fun_nr}, ti)
//											-!-> ("Recursion","RootCaseOfZombie",ti_next_fun_nr,recursion_introduced)
									Yes {ri_fun_index,ri_fun_ptr}
										| ri_fun_ptr==fun_info_ptr
											-> ({ro_fun & symb_kind=SK_GeneratedFunction fun_info_ptr ri_fun_index},ti)
							RootCase
								-> (ro.ro_tfi.tfi_root,{ti & ti_recursion_introduced = No})
//									-!-> ("Recursion","RootCase",ro.ro_tfi.tfi_root)
					  app_args1 = replace_arg [ fv_info_ptr \\ {fv_info_ptr}<-aci_params ] app_args variables
					  (app_args2, ti) = transform app_args1 { ro & ro_root_case_mode = NotRootCase } ti
					-> (App {app_symb=app_symb, app_args=app_args2, app_info_ptr=nilPtr}, ti)
where
	possiblyFoldOuterCase this_case ro=:{ro_tfi} ti
		| SwitchAutoFoldAppInCase True False
			| ro_tfi.tfi_n_args_before_producer < 0 || ro_tfi.tfi_n_producer_args < 0
				= skip_over this_case ro ti	//abort "possiblyFoldOuterCase: unexpected!\n"
			= transformApplication (make_consumer_application ro_tfi case_expr) [] ro ti
		= skip_over this_case ro ti

	equal (SK_Function glob_index1) (SK_Function glob_index2)
		= glob_index1==glob_index2
	equal (SK_LocalMacroFunction glob_index1) (SK_LocalMacroFunction glob_index2)
		= glob_index1==glob_index2
	equal (SK_GeneratedFunction _ index1) (SK_GeneratedFunction _ index2)
		= index1==index2
	equal _ _
		= False

	replace_arg [] _ f
		= f
	replace_arg producer_vars=:[fv_info_ptr:_] act_pars form_pars=:[h_form_pars=:(Var {var_info_ptr}):t_form_pars]
		| fv_info_ptr<>var_info_ptr
			= [h_form_pars:replace_arg producer_vars act_pars t_form_pars]
		= replacement producer_vars act_pars form_pars
	  where
		replacement producer_vars [] form_pars
			= form_pars
		replacement producer_vars _ []
			= []
		replacement producer_vars [h_act_pars:t_act_pars] [form_par=:(Var {var_info_ptr}):form_pars]
			| isMember var_info_ptr producer_vars
				= [h_act_pars:replacement producer_vars t_act_pars form_pars]
			= replacement producer_vars t_act_pars form_pars

	match_and_instantiate linearities cons_index app_args (AlgebraicPatterns _ algebraicPatterns) case_info_ptr case_default ro ti
		# (EI_CaseType {ct_cons_types}, ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
		  ti & ti_symbol_heap=ti_symbol_heap
		= match_and_instantiate_algebraic_type linearities cons_index app_args algebraicPatterns ct_cons_types case_default ro ti
		where
			match_and_instantiate_algebraic_type [!linearity:linearities!] cons_index app_args
												 [{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards]
												 [cons_type:cons_types] case_default ro ti
				| cons_index.glob_module == glob_module && cons_index.glob_object == ds_index
					# args_strictness = ro.ro_common_defs.[glob_module].com_cons_defs.[ds_index].cons_type.st_args_strictness
					= instantiate linearity app_args ap_vars ap_expr args_strictness cons_type ti
				= match_and_instantiate_algebraic_type linearities cons_index app_args guards cons_types case_default ro ti
			match_and_instantiate_algebraic_type _ cons_index app_args [] cons_types case_default ro ti
				= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
	match_and_instantiate linearities cons_index app_args (OverloadedListPatterns (OverloadedList _ _ _ _) _ algebraicPatterns) case_info_ptr case_default ro ti
		# (EI_CaseType {ct_cons_types}, ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
		  ti & ti_symbol_heap=ti_symbol_heap
		= match_and_instantiate_overloaded_list linearities cons_index app_args algebraicPatterns ct_cons_types case_default ro ti
		where
			match_and_instantiate_overloaded_list [!linearity:linearities!] cons_index=:{glob_module=cons_glob_module,glob_object=cons_ds_index} app_args 
									[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards]
									[cons_type:cons_types] case_default ro ti
				| equal_list_contructor glob_module ds_index cons_glob_module cons_ds_index
					# args_strictness = ro.ro_common_defs.[cons_glob_module].com_cons_defs.[cons_ds_index].cons_type.st_args_strictness
					= instantiate linearity app_args ap_vars ap_expr args_strictness cons_type ti
					= match_and_instantiate_overloaded_list linearities cons_index app_args guards cons_types case_default ro ti
					where
						equal_list_contructor glob_module ds_index cons_glob_module cons_ds_index
							| glob_module==cPredefinedModuleIndex && cons_glob_module==cPredefinedModuleIndex
								# index=ds_index+FirstConstructorPredefinedSymbolIndex
								# cons_index=cons_ds_index+FirstConstructorPredefinedSymbolIndex
								| index==PD_OverloadedConsSymbol
									= cons_index==PD_ConsSymbol || cons_index==PD_StrictConsSymbol || cons_index==PD_TailStrictConsSymbol || cons_index==PD_StrictTailStrictConsSymbol
								| index==PD_OverloadedNilSymbol
									= cons_index==PD_NilSymbol || cons_index==PD_StrictNilSymbol || cons_index==PD_TailStrictNilSymbol || cons_index==PD_StrictTailStrictNilSymbol
									= abort "equal_list_contructor"
			match_and_instantiate_overloaded_list _ cons_index app_args [] cons_types case_default ro ti
				= transform case_default { ro & ro_root_case_mode = NotRootCase } ti

	trans_case_of_overloaded_nil_or_cons type case_info_ptr ti
		| type.st_arity==0
			# (may_be_match_expr, ti) = match_and_instantiate_overloaded_nil case_guards case_default ro ti
			= expr_or_never_matching_case may_be_match_expr case_ident ti
			# aci_linearity_of_patterns = aci.aci_linearity_of_patterns
			  (EI_CaseType {ct_cons_types}, ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
			  ti & ti_symbol_heap=ti_symbol_heap
			  (may_be_match_expr, ti) = match_and_instantiate_overloaded_cons type aci_linearity_of_patterns app_args case_guards ct_cons_types case_default ro ti
			= expr_or_never_matching_case may_be_match_expr case_ident ti
	where
		match_and_instantiate_overloaded_nil (OverloadedListPatterns _ _ algebraicPatterns) case_default ro ti
			= match_and_instantiate_nil algebraicPatterns case_default ro ti
		match_and_instantiate_overloaded_nil (AlgebraicPatterns _ algebraicPatterns) case_default ro ti
			= match_and_instantiate_nil algebraicPatterns case_default ro ti
	
		match_and_instantiate_nil [{ap_symbol={glob_module,glob_object={ds_index}},ap_expr} : guards] case_default ro ti
			| glob_module==cPredefinedModuleIndex
				# index=ds_index+FirstConstructorPredefinedSymbolIndex
				| index==PD_NilSymbol || index==PD_StrictNilSymbol || index==PD_TailStrictNilSymbol || index==PD_StrictTailStrictNilSymbol ||
				  index==PD_OverloadedNilSymbol || index==PD_UnboxedNilSymbol || index==PD_UnboxedTailStrictNilSymbol
					= instantiate [] [] [] ap_expr NotStrict [] ti
					= match_and_instantiate_nil guards case_default ro ti
		match_and_instantiate_nil [] case_default ro ti
			= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
	
		match_and_instantiate_overloaded_cons cons_function_type linearities app_args (AlgebraicPatterns _ algebraicPatterns) [cons_type:_] case_default ro ti
			= match_and_instantiate_overloaded_cons_boxed_match linearities app_args algebraicPatterns case_default ro ti
			where
				match_and_instantiate_overloaded_cons_boxed_match [!linearity:linearities!] app_args
										[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] 
										case_default ro ti
					| glob_module==cPredefinedModuleIndex
						# index=ds_index+FirstConstructorPredefinedSymbolIndex
						| index==PD_ConsSymbol || index==PD_StrictConsSymbol || index==PD_TailStrictConsSymbol || index==PD_StrictTailStrictConsSymbol
							# args_strictness = ro.ro_common_defs.[glob_module].com_cons_defs.[ds_index].cons_type.st_args_strictness
							= instantiate linearity app_args ap_vars ap_expr args_strictness cons_type ti
		//				| index==PD_NilSymbol || index==PD_StrictNilSymbol || index==PD_TailStrictNilSymbol || index==PD_StrictTailStrictNilSymbol
							= match_and_instantiate_overloaded_cons_boxed_match linearities app_args guards case_default ro ti
		//					= abort "match_and_instantiate_overloaded_cons_boxed_match"
				match_and_instantiate_overloaded_cons_boxed_match _ app_args [] case_default ro ti
					= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
		match_and_instantiate_overloaded_cons cons_function_type linearities app_args (OverloadedListPatterns _ _ algebraicPatterns) cons_types case_default ro ti
			= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args algebraicPatterns case_default ro ti
			where
				match_and_instantiate_overloaded_cons_overloaded_match [!linearity:linearities!] app_args
										[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] 
										case_default ro ti
					| glob_module==cPredefinedModuleIndex
						# index=ds_index+FirstConstructorPredefinedSymbolIndex
						| index==PD_UnboxedConsSymbol || index==PD_UnboxedTailStrictConsSymbol || index==PD_OverloadedConsSymbol
							= instantiate linearity app_args ap_vars ap_expr cons_function_type.st_args_strictness cons_function_type.st_args ti
		//				| index==PD_UnboxedNilSymbol || index==PD_UnboxedTailStrictNilSymbol || index==PD_OverloadedNilSymbol
							= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args guards case_default ro ti
		//					= abort "match_and_instantiate_overloaded_cons_overloaded_match"
				match_and_instantiate_overloaded_cons_overloaded_match _ app_args [] case_default ro ti
					= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
	
		/*
		match_and_instantiate_overloaded_cons linearities app_args (OverloadedListPatterns _ (App {app_args=[],app_symb={symb_kind=SK_Function {glob_module=decons_module,glob_object=deconsindex}}}) algebraicPatterns) case_default ro ti
			= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args algebraicPatterns case_default ro ti
			where
				match_and_instantiate_overloaded_cons_overloaded_match [linearity:linearities] app_args 
										[{ap_symbol={glob_module,glob_object={ds_index}}, ap_vars, ap_expr} : guards] 
										case_default ro ti
					| glob_module==cPredefinedModuleIndex
						# index=ds_index+FirstConstructorPredefinedSymbolIndex
						| index==PD_UnboxedConsSymbol || index==PD_UnboxedTailStrictConsSymbol || index==PD_OverloadedConsSymbol
							# (argument_types,strictness) = case ro.ro_imported_funs.[decons_module].[deconsindex].ft_type.st_result.at_type of
													TA _ args=:[arg1,arg2] -> (args,NotStrict)
													TAS _ args=:[arg1,arg2] strictness -> (args,strictness)
							= instantiate linearity app_args ap_vars ap_expr strictness argument_types ti
						| index==PD_UnboxedNilSymbol || index==PD_UnboxedTailStrictNilSymbol || index==PD_OverloadedNilSymbol
							= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args guards case_default ro ti
							= abort "match_and_instantiate_overloaded_cons_overloaded_match"
				match_and_instantiate_overloaded_cons_overloaded_match [linearity:linearities] app_args [guard : guards] case_default ro ti
					= match_and_instantiate_overloaded_cons_overloaded_match linearities app_args guards case_default ro ti
				match_and_instantiate_overloaded_cons_overloaded_match _ app_args [] case_default ro ti
					= transform case_default { ro & ro_root_case_mode = NotRootCase } ti
		*/

	instantiate linearity app_args ap_vars ap_expr cons_type_args_strictness cons_type_args ti
		# zipped_ap_vars_and_args = zip2 ap_vars app_args
		  (body_strictness,ti_fun_defs,ti_fun_heap) = body_strict ap_expr ap_vars ro ti.ti_fun_defs ti.ti_fun_heap
		  ti = {ti & ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap}
		  unfoldables = [ (arg_is_strict i body_strictness || ((not (arg_is_strict i cons_type_args_strictness))) && linear) || in_normal_form app_arg
		  				 \\ linear <|- linearity & app_arg <- app_args & i <- [0..]]
		  unfoldable_args = filterWith unfoldables zipped_ap_vars_and_args
		  not_unfoldable = map not unfoldables
		  ti_var_heap = foldSt (\({fv_info_ptr}, arg) -> writeVarInfo fv_info_ptr (VI_Expression arg)) unfoldable_args ti.ti_var_heap
		  (new_expr, ti_symbol_heap) = possibly_add_let zipped_ap_vars_and_args ap_expr not_unfoldable cons_type_args ro ti.ti_symbol_heap cons_type_args_strictness
		  copy_state = { cs_var_heap = ti_var_heap, cs_symbol_heap = ti_symbol_heap, cs_opt_type_heaps = No,cs_cleanup_info=ti.ti_cleanup_info }
		  (unfolded_expr, copy_state) = copy new_expr {ci_handle_aci_free_vars = LeaveAciFreeVars} copy_state
		  ti_var_heap = foldSt (\({fv_info_ptr}, arg) -> writeVarInfo fv_info_ptr VI_Empty) unfoldable_args copy_state.cs_var_heap
		  ti & ti_var_heap = ti_var_heap,ti_symbol_heap = copy_state.cs_symbol_heap,ti_cleanup_info=copy_state.cs_cleanup_info
		  (final_expr, ti) = transform unfolded_expr { ro & ro_root_case_mode = NotRootCase } ti
//		| False ---> ("instantiate",app_args,ap_vars,ap_expr,final_expr,unfoldables) = undef
		= (Yes final_expr, ti)
	where
		body_strict (Var v) ap_vars ro fun_defs fun_heap
			# lazy_args = insert_n_lazy_values_at_beginning (length app_args) NotStrict
			# is = [i \\ i <- [0..] & var <- ap_vars | v.var_info_ptr == var.fv_info_ptr]
			= case is of
				[]		-> (lazy_args,fun_defs,fun_heap)
				[i:_]	-> (add_strictness i lazy_args,fun_defs,fun_heap)
		body_strict (App app) ap_vars ro fun_defs fun_heap
			# (is,fun_defs,fun_heap) = app_indices app ro fun_defs fun_heap
			# lazy_args = insert_n_lazy_values_at_beginning (length app_args) NotStrict
			= (foldSt add_strictness is lazy_args, fun_defs,fun_heap)
		body_strict _ _ ro fun_defs fun_heap
			# lazy_args = insert_n_lazy_values_at_beginning (length app_args) NotStrict
			= (lazy_args,fun_defs,fun_heap)
		
		app_indices {app_symb,app_args} ro fun_defs fun_heap
			# ({st_args_strictness,st_arity},fun_defs,fun_heap)	= get_producer_type app_symb ro fun_defs fun_heap
			| length app_args == st_arity
				= find_indices st_args_strictness 0 app_args ro fun_defs fun_heap
				= ([],fun_defs,fun_heap)
		where
			find_indices st_args_strictness i [] ro fun_defs fun_heap
				= ([],fun_defs,fun_heap)
			find_indices st_args_strictness i [e:es] ro fun_defs fun_heap
				# (is,fun_defs,fun_heap)	= find_index st_args_strictness i e ro fun_defs fun_heap
				# (iss,fun_defs,fun_heap)	= find_indices st_args_strictness (i+1) es ro fun_defs fun_heap
				= (is++iss,fun_defs,fun_heap)

			find_index st_args_strictness i e ro fun_defs fun_heap
				| arg_is_strict i st_args_strictness
					= case e of
						Var v	-> ([i \\ i <- [0..] & var <- ap_vars | v.var_info_ptr == var.fv_info_ptr],fun_defs,fun_heap)
						App	a	-> app_indices a ro fun_defs fun_heap
						_		-> ([],fun_defs,fun_heap)
				= ([],fun_defs,fun_heap)
		
	expr_or_never_matching_case (Yes match_expr) case_ident ti
		= (match_expr, ti)
	expr_or_never_matching_case No case_ident ti
		= (neverMatchingCase never_ident, ti) // <-!- ("transform_active_root_case:App:neverMatchingCase",never_ident)
		where
			never_ident = case ro.ro_root_case_mode of
							NotRootCase -> case_ident
							_ -> Yes ro.ro_tfi.tfi_case.symb_ident

transform_active_root_case aci this_case=:{case_expr=case_expr=:BasicExpr basic_value,case_guards=case_guards=:BasicPatterns _ basicPatterns,case_default} ro ti
	// currently only active cases are matched at runtime (multimatch problem)
	# matching_patterns = [pattern \\ pattern=:{bp_value}<-basicPatterns | bp_value==basic_value]
	= case matching_patterns of
		[]
			-> case case_default of
				Yes default_expr
					-> transform default_expr { ro & ro_root_case_mode = NotRootCase } ti
				No
					-> (neverMatchingCase never_ident, ti)
					with
						never_ident = case ro.ro_root_case_mode of
							NotRootCase -> this_case.case_ident
							_ -> Yes ro.ro_tfi.tfi_case.symb_ident
		[{bp_expr}:_]
			-> transform bp_expr {ro & ro_root_case_mode = NotRootCase} ti

transform_active_root_case aci this_case=:{case_expr = Let lad=:{let_strict_binds,let_lazy_binds,let_info_ptr}} ro ti
	# ro_not_root = { ro & ro_root_case_mode = NotRootCase }
	  ti = store_type_info_of_let_bindings_in_heap let_strict_binds let_lazy_binds let_info_ptr ti
	  (new_let_strict_binds, ti) = transform lad.let_strict_binds ro_not_root ti
	  (new_let_lazy_binds, ti) = transform lad.let_lazy_binds ro_not_root ti
	  (new_let_expr, ti) = transform (Case { this_case & case_expr = lad.let_expr }) ro ti
	= (Let { lad & let_expr = new_let_expr, let_strict_binds = new_let_strict_binds, let_lazy_binds = new_let_lazy_binds }, ti)

transform_active_root_case aci this_case ro ti
	= skip_over this_case ro ti

make_consumer_application {tfi_orig,tfi_args,tfi_n_args_before_producer=bef,tfi_n_producer_args=act} arg_expr
	# args = free_vars_to_bound_vars (take bef tfi_args) ++ [arg_expr : free_vars_to_bound_vars (drop (bef+act) tfi_args)]
	= {app_symb = tfi_orig, app_args = args, app_info_ptr = nilPtr}

in_normal_form (Var _)			= True
in_normal_form (BasicExpr _)	= True
in_normal_form _				= False

filterWith [True:t2] [h1:t1]
	= [h1:filterWith t2 t1]
filterWith [False:t2] [h1:t1]
	= filterWith t2 t1
filterWith _ _
	= []

possibly_add_let [] ap_expr _ _ _ ti_symbol_heap cons_type_args_strictness
	= (ap_expr, ti_symbol_heap)
possibly_add_let zipped_ap_vars_and_args ap_expr not_unfoldable cons_type_args ro ti_symbol_heap cons_type_args_strictness
	= SwitchStrictPossiblyAddLet
		(let
			strict_binds	= [ {lb_src=lb_src, lb_dst=lb_dst, lb_position = NoPos}
									\\ (lb_dst,lb_src)<-zipped_ap_vars_and_args
									& n <- not_unfoldable
									& i <- [0..]
									| n && arg_is_strict i cons_type_args_strictness
									]
			lazy_binds		= [ {lb_src=lb_src, lb_dst=lb_dst, lb_position = NoPos}
									\\ (lb_dst,lb_src)<-zipped_ap_vars_and_args
									& n <- not_unfoldable
									& i <- [0..]
									| n && not (arg_is_strict i cons_type_args_strictness)
									]
		 in
		 	case (strict_binds,lazy_binds) of
		 		([],[])
					-> (ap_expr, ti_symbol_heap)
				_
					# let_type = filterWith not_unfoldable cons_type_args
					  (new_info_ptr, ti_symbol_heap) = newPtr (EI_LetType let_type) ti_symbol_heap
		 			-> (Let
						{	let_strict_binds	= strict_binds
						,	let_lazy_binds		= lazy_binds
						,	let_expr			= ap_expr
						,	let_info_ptr		= new_info_ptr
						,	let_expr_position	= NoPos
						}, ti_symbol_heap)
	  )
	  (let
			lazy_binds		= [ {lb_src=lb_src, lb_dst=lb_dst, lb_position = NoPos}
									\\ (lb_dst,lb_src)<-zipped_ap_vars_and_args
									& n <- not_unfoldable
									| n
									]
	   in
			case lazy_binds of
				[]
					-> (ap_expr, ti_symbol_heap)
				_
					# let_type = filterWith not_unfoldable cons_type_args
					  (new_info_ptr, ti_symbol_heap) = newPtr (EI_LetType let_type) ti_symbol_heap
					-> (Let
		  			 	{	let_strict_binds	= []
						,	let_lazy_binds		= lazy_binds
						,	let_expr			= ap_expr
						,	let_info_ptr		= new_info_ptr
						,	let_expr_position	= NoPos
						}, ti_symbol_heap)
	  )

free_variables_of_expression expr ti
	# ti_var_heap = clearVariables expr ti.ti_var_heap
	  fvi = {fvi_var_heap = ti_var_heap, fvi_expr_heap = ti.ti_symbol_heap, fvi_variables = [], fvi_expr_ptrs = ti.ti_cleanup_info}
	  {fvi_var_heap, fvi_expr_heap, fvi_variables, fvi_expr_ptrs} = freeVariables expr fvi
	  ti = {ti & ti_var_heap = fvi_var_heap, ti_symbol_heap = fvi_expr_heap, ti_cleanup_info = fvi_expr_ptrs}
	 = (fvi_variables,ti)

transform_active_non_root_case :: !Case !ActiveCaseInfo !ReadOnlyTI !*TransformInfo -> *(!Expression, !*TransformInfo)
transform_active_non_root_case kees=:{case_info_ptr,case_expr = App {app_symb}} aci=:{aci_free_vars} ro ti=:{ti_recursion_introduced=old_ti_recursion_introduced}
	| not aci.aci_safe
		= skip_over kees ro ti
	| is_safe_producer app_symb.symb_kind ro ti.ti_fun_heap ti.ti_cons_args
		// determine free variables	
		# (free_vars,ti) = free_variables_of_expression (Case {kees & case_expr=EE}) ti	
		// search function definition and consumer arguments
		  (outer_fun_def, outer_cons_args, ti_cons_args, ti_fun_defs, ti_fun_heap)
				= get_fun_def_and_cons_args ro.ro_tfi.tfi_root.symb_kind ti.ti_cons_args ti.ti_fun_defs ti.ti_fun_heap
		  outer_arguments
		  		= case outer_fun_def.fun_body of
								TransformedBody {tb_args} 	-> tb_args
								Expanding args				-> args
		  outer_info_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-outer_arguments]
		  free_var_info_ptrs = [ var_info_ptr \\ {var_info_ptr}<-free_vars ]
		  used_mask = [isMember fv_info_ptr free_var_info_ptrs \\ {fv_info_ptr}<-outer_arguments]
		  arguments_from_outer_fun = [ outer_argument \\ outer_argument<-outer_arguments & used<-used_mask | used ]
		  lifted_arguments
		  		= [ { fv_def_level = undeff, fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_count = undeff}
								\\ {var_ident, var_info_ptr} <- free_vars | not (isMember var_info_ptr outer_info_ptrs)]
		  all_args = lifted_arguments++arguments_from_outer_fun
		| SwitchArityChecks (1+length all_args > 32) False
			# ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_recursion_introduced = No }
			| ro.ro_transform_fusion>=FullFusion
				# ti = { ti & ti_error_file = ti.ti_error_file <<< "Possibly missed fusion oppurtunity: Case Arity > 32 " <<< ro.ro_tfi.tfi_root.symb_ident.id_name <<< "\n"}
				= skip_over kees ro ti
			= skip_over kees ro ti
		# (fun_info_ptr, ti_fun_heap) = newPtr FI_Empty ti_fun_heap
		  fun_ident = { id_name = ro.ro_tfi.tfi_root.symb_ident.id_name+++"_case", id_info = nilPtr }
		  fun_symb = { symb_ident = fun_ident, symb_kind=SK_GeneratedFunction fun_info_ptr undeff }
		# ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap }
//				---> ("lifted arguments",[fv_ident\\{fv_ident}<-lifted_arguments],outer_arguments,
//					'\n',kees.case_expr,kees.case_guards,kees.case_default)
	  	# fun_index = ti.ti_next_fun_nr
	  	# ti = { ti & ti_next_fun_nr = fun_index + 1 }
		// JvG: why are dictionaries not the first arguments ?
		# new_ro = { ro & ro_root_case_mode = RootCaseOfZombie, ro_tfi.tfi_case = fun_symb, ro_tfi.tfi_args = all_args }
		= generate_case_function_with_pattern_argument fun_index case_info_ptr (Case kees) outer_fun_def outer_cons_args used_mask fun_symb all_args ti

transform_active_non_root_case kees=:{case_info_ptr} aci=:{aci_free_vars} ro ti=:{ti_recursion_introduced=old_ti_recursion_introduced}
	| not aci.aci_safe
		= skip_over kees ro ti
	// determine free variables
	# (free_vars,ti) = free_variables_of_expression (Case kees) ti	
	// search function definition and consumer arguments
	  (outer_fun_def, outer_cons_args, ti_cons_args, ti_fun_defs, ti_fun_heap)
			= get_fun_def_and_cons_args ro.ro_tfi.tfi_root.symb_kind ti.ti_cons_args ti.ti_fun_defs ti.ti_fun_heap
	  outer_arguments
	  		= case outer_fun_def.fun_body of
							TransformedBody {tb_args} 	-> tb_args
							Expanding args				-> args
	  outer_info_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-outer_arguments]
	  free_var_info_ptrs = [ var_info_ptr \\ {var_info_ptr}<-free_vars ]
	  used_mask = [isMember fv_info_ptr free_var_info_ptrs \\ {fv_info_ptr}<-outer_arguments]
	  arguments_from_outer_fun = [ outer_argument \\ outer_argument<-outer_arguments & used<-used_mask | used ]
	  lifted_arguments
	  		= [ { fv_def_level = undeff, fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_count = undeff}
							\\ {var_ident, var_info_ptr} <- free_vars | not (isMember var_info_ptr outer_info_ptrs)]
	  all_args = lifted_arguments++arguments_from_outer_fun
	| SwitchArityChecks (length all_args > 32) False
		# ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_recursion_introduced = No }
		| ro.ro_transform_fusion>=FullFusion
			#  ti	= { ti & ti_error_file = ti.ti_error_file <<< "Possibly missed fusion oppurtunity: Case Arity > 32 " <<< ro.ro_tfi.tfi_root.symb_ident.id_name <<< "\n"}
			= skip_over kees ro ti
		= skip_over kees ro ti
	# (fun_info_ptr, ti_fun_heap) = newPtr FI_Empty ti_fun_heap
	  fun_ident = { id_name = ro.ro_tfi.tfi_root.symb_ident.id_name+++"_case", id_info = nilPtr }
	  fun_symb = { symb_ident = fun_ident, symb_kind=SK_GeneratedFunction fun_info_ptr undeff }
//					<-!- ("<<<transformCaseFunction",fun_symb)
	| SwitchAlwaysIntroduceCaseFunction True False
	  	# fun_index = ti.ti_next_fun_nr
		# ti &	ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap,
				ti_next_fun_nr = fun_index + 1, ti_new_functions = [fun_info_ptr:ti.ti_new_functions]
		# new_ro = { ro & ro_root_case_mode = RootCaseOfZombie , ro_tfi.tfi_case = fun_symb, ro_tfi.tfi_args = all_args }
	  	= generate_case_function fun_index case_info_ptr (Case kees) outer_fun_def outer_cons_args used_mask new_ro ti
	# new_ro = { ro & ro_root_case_mode = RootCaseOfZombie,
				 ro_tfi.tfi_case = fun_symb, ro_tfi.tfi_args = all_args, ro_tfi.tfi_n_args_before_producer = -1,  ro_tfi.tfi_n_producer_args = -1 }
	  ti = { ti & ti_cons_args = ti_cons_args, ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_recursion_introduced = No }
	  (new_expr, ti)
	  		= transformCase kees new_ro ti
	  (ti_recursion_introduced, ti) = ti!ti_recursion_introduced
//	  			<-!- ("transformCaseFunction>>>",fun_symb)
	  ti = { ti & ti_recursion_introduced = old_ti_recursion_introduced }
	= case ti_recursion_introduced of
		Yes {ri_fun_index}
			-> generate_case_function ri_fun_index case_info_ptr new_expr outer_fun_def outer_cons_args used_mask new_ro ti
		No	-> (new_expr, ti)

FI_CopyMask:==63

generate_case_function :: !Int !ExprInfoPtr !Expression FunDef .ConsClasses [.Bool] !.ReadOnlyTI !*TransformInfo -> (!Expression,!*TransformInfo)
generate_case_function fun_index case_info_ptr new_expr outer_fun_def outer_cons_args used_mask
					{ro_tfi={tfi_case=tfi_fun=:{symb_kind=SK_GeneratedFunction fun_info_ptr _},tfi_args}} ti
	# fun_arity								= length tfi_args
	  ti = arity_warning "generate_case_function" tfi_fun.symb_ident fun_index fun_arity ti
	  (Yes {st_args,st_attr_env})			= outer_fun_def.fun_type
	  types_from_outer_fun					= [ st_arg \\ st_arg <- st_args & used <- used_mask | used ]
	  nr_of_lifted_vars						= fun_arity-(length types_from_outer_fun)
	  (lifted_types, ti_var_heap)			= get_types_of_local_vars (take nr_of_lifted_vars tfi_args) ti.ti_var_heap
	  (EI_CaseType {ct_result_type}, ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
	  (form_vars, ti_var_heap)				= mapSt bind_to_fresh_expr_var tfi_args ti_var_heap

	  arg_types								= lifted_types++types_from_outer_fun

	# ti = {ti & ti_var_heap = ti_var_heap, ti_symbol_heap = ti_symbol_heap}
	# (fun_type,type_variables,ti)			= determine_case_function_type fun_arity ct_result_type arg_types st_attr_env ti

	  // unfold...
	  cs =		{ cs_var_heap				= ti.ti_var_heap
	  			, cs_symbol_heap			= ti.ti_symbol_heap
				, cs_opt_type_heaps			= Yes ti.ti_type_heaps
	  			, cs_cleanup_info			= ti.ti_cleanup_info
	  			}
	  (copied_expr, cs)
			= copy new_expr {ci_handle_aci_free_vars = SubstituteAciFreeVars} cs
	  {cs_var_heap=ti_var_heap, cs_symbol_heap=ti_symbol_heap, cs_cleanup_info=ti_cleanup_info, cs_opt_type_heaps = Yes ti_type_heaps} = cs

	  ti_var_heap = remove_VI_Expression_values tfi_args ti_var_heap
	  ti_type_heaps & th_vars = remove_TVI_Type_values type_variables ti_type_heaps.th_vars

	  // generated function...
	  fun_def =	{ fun_ident					= tfi_fun.symb_ident
				, fun_arity					= fun_arity
				, fun_priority				= NoPrio
				, fun_body					= TransformedBody { tb_args = form_vars, tb_rhs = copied_expr}
				, fun_type					= Yes fun_type
				, fun_pos					= NoPos
				, fun_kind					= FK_Function cNameNotLocationDependent
				, fun_lifted				= undeff
				, fun_info = 	{	fi_calls		= []
								,	fi_group_index	= outer_fun_def.fun_info.fi_group_index
								,	fi_def_level	= NotALevel
								,	fi_free_vars	= []
								,	fi_local_vars	= []
								,	fi_dynamics		= []
								,	fi_properties	= outer_fun_def.fun_info.fi_properties bitand FI_CopyMask
								}
				}
	# cc_args_from_outer_fun		= [ cons_arg \\ cons_arg <- outer_cons_args.cc_args & used <- used_mask | used ]
	  cc_linear_bits_from_outer_fun	= [# cons_arg \\ cons_arg <|- outer_cons_args.cc_linear_bits & used <- used_mask | used !]
	  new_cons_args =
				{ cc_size			= fun_arity
				, cc_args			= repeatn nr_of_lifted_vars CPassive ++ cc_args_from_outer_fun
				, cc_linear_bits	= RepeatnAppendM nr_of_lifted_vars False cc_linear_bits_from_outer_fun
				, cc_producer		= False
				}
	  gf =		{ gf_fun_def		= fun_def
				, gf_instance_info	= II_Empty
				, gf_cons_args		= new_cons_args
				, gf_fun_index		= fun_index
				}
	  ti_fun_heap = writePtr fun_info_ptr (FI_Function gf) ti.ti_fun_heap
	  ti = { ti & ti_var_heap		= ti_var_heap
	  			, ti_fun_heap		= ti_fun_heap
	  			, ti_symbol_heap	= ti_symbol_heap
	  			, ti_type_heaps		= ti_type_heaps
	  			, ti_cleanup_info	= ti_cleanup_info 
	  			}
	  app_symb = {tfi_fun & symb_kind = SK_GeneratedFunction fun_info_ptr fun_index}
	  app_args = free_vars_to_bound_vars tfi_args
	= ( App {app_symb = app_symb, app_args = app_args, app_info_ptr = nilPtr}, ti)

generate_case_function_with_pattern_argument :: !Int !ExprInfoPtr !Expression FunDef .ConsClasses [.Bool] !SymbIdent ![FreeVar] !*TransformInfo
	-> (!Expression,!*TransformInfo)
generate_case_function_with_pattern_argument fun_index case_info_ptr 
					case_expr=:(Case kees=:{case_expr=old_case_expr}) outer_fun_def outer_cons_args used_mask
					ro_fun=:{symb_kind=SK_GeneratedFunction fun_info_ptr _} ro_fun_args ti
	# fun_arity								= length ro_fun_args
	  ti = arity_warning "generate_case_function" ro_fun.symb_ident fun_index fun_arity ti
	  (Yes {st_args,st_attr_env})			= outer_fun_def.fun_type
	  types_from_outer_fun					= [ st_arg \\ st_arg <- st_args & used <- used_mask | used ]
	  nr_of_lifted_vars						= fun_arity-(length types_from_outer_fun)
	  (lifted_types, ti_var_heap)			= get_types_of_local_vars (take nr_of_lifted_vars ro_fun_args) ti.ti_var_heap
	  (EI_CaseType {ct_result_type,ct_pattern_type}, ti_symbol_heap) = readExprInfo case_info_ptr ti.ti_symbol_heap
	  (form_vars, ti_var_heap)				= mapSt bind_to_fresh_expr_var ro_fun_args ti_var_heap

	  arg_types								= lifted_types++types_from_outer_fun

	  ti = {ti & ti_var_heap = ti_var_heap, ti_symbol_heap = ti_symbol_heap}
	  (fun_type,type_variables,ti)			= determine_case_function_type fun_arity ct_result_type [ct_pattern_type:arg_types] st_attr_env ti

	  cs =		{ cs_var_heap				= ti.ti_var_heap
	  			, cs_symbol_heap			= ti.ti_symbol_heap
				, cs_opt_type_heaps			= Yes ti.ti_type_heaps
	  			, cs_cleanup_info			= ti.ti_cleanup_info
	  			}
	  (Case copied_kees, cs)
			= copy (Case {kees & case_expr=EE}) {ci_handle_aci_free_vars = SubstituteAciFreeVars} cs
	  {cs_var_heap=ti_var_heap, cs_symbol_heap=ti_symbol_heap, cs_cleanup_info=ti_cleanup_info, cs_opt_type_heaps = Yes ti_type_heaps} = cs

	  ti_var_heap = remove_VI_Expression_values ro_fun_args ti_var_heap
	  ti_type_heaps & th_vars = remove_TVI_Type_values type_variables ti_type_heaps.th_vars

	  (new_info_ptr, ti_var_heap) = newPtr VI_Empty ti_var_heap
	  var_id = {id_name = "_x", id_info = nilPtr}
	  case_free_var = {fv_def_level = NotALevel, fv_ident = var_id, fv_info_ptr = new_info_ptr, fv_count = 0}
	  case_var = Var {var_ident = var_id, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr}
	  copied_expr = Case {copied_kees & case_expr=case_var}
	  form_vars = [case_free_var:form_vars]
	  fun_arity = fun_arity+1
	  // generated function...
	  fun_def =	{ fun_ident					= ro_fun.symb_ident
				, fun_arity					= fun_arity
				, fun_priority				= NoPrio
				, fun_body					= TransformedBody { tb_args = form_vars, tb_rhs = copied_expr}
				, fun_type					= Yes fun_type
				, fun_pos					= NoPos
				, fun_kind					= FK_Function cNameNotLocationDependent
				, fun_lifted				= undeff
				, fun_info = 	{	fi_calls		= []
								,	fi_group_index	= outer_fun_def.fun_info.fi_group_index
								,	fi_def_level	= NotALevel
								,	fi_free_vars	= []
								,	fi_local_vars	= []
								,	fi_dynamics		= []
								,	fi_properties	= outer_fun_def.fun_info.fi_properties bitand FI_CopyMask
								}
				}
	  cc_args_from_outer_fun		= [ cons_arg \\ cons_arg <- outer_cons_args.cc_args & used <- used_mask | used ]
	  cc_linear_bits_from_outer_fun	= [# cons_arg \\ cons_arg <|- outer_cons_args.cc_linear_bits & used <- used_mask | used !]
	  new_cons_args =
	  			{ cc_size			= fun_arity
	  			, cc_args			= [CActive : repeatn nr_of_lifted_vars CPassive ++ cc_args_from_outer_fun]
				, cc_linear_bits	= [#True : RepeatnAppendM nr_of_lifted_vars False cc_linear_bits_from_outer_fun!]
	  			, cc_producer		= False
	  			}
	  gf =		{ gf_fun_def		= fun_def
	  			, gf_instance_info	= II_Empty
	  			, gf_cons_args		= new_cons_args
	  			, gf_fun_index		= fun_index
	  			}
	  ti_fun_heap = writePtr fun_info_ptr (FI_Function gf) ti.ti_fun_heap
	  ti = { ti & ti_new_functions	= [fun_info_ptr:ti.ti_new_functions]
	  			, ti_var_heap		= ti_var_heap
	  			, ti_fun_heap		= ti_fun_heap
	  			, ti_symbol_heap	= ti_symbol_heap
	  			, ti_type_heaps		= ti_type_heaps
	  			, ti_cleanup_info	= ti_cleanup_info 
	  			}
	  app_symb = { ro_fun & symb_kind = SK_GeneratedFunction fun_info_ptr fun_index}
	  app_args = [old_case_expr : free_vars_to_bound_vars ro_fun_args]
	= (App {app_symb = app_symb, app_args = app_args, app_info_ptr = nilPtr}, ti)

get_types_of_local_vars vars var_heap
	= mapSt get_type_of_local_var vars var_heap
where
	get_type_of_local_var {fv_info_ptr} var_heap
		# (EVI_VarType a_type, var_heap)	= readExtendedVarInfo fv_info_ptr var_heap
		= (a_type, var_heap)

determine_case_function_type fun_arity ct_result_type arg_types st_attr_env ti=:{ti_type_heaps}
	# (type_variables, th_vars)				= getTypeVars [ct_result_type:arg_types] ti_type_heaps.th_vars
	  (fresh_type_vars, th_vars)			= bind_to_fresh_type_variables type_variables th_vars
	  ti_type_heaps							= { ti_type_heaps & th_vars = th_vars }
	  (_, fresh_arg_types, ti_type_heaps)	= substitute arg_types ti_type_heaps
	  (_, fresh_result_type, ti_type_heaps)	= substitute ct_result_type ti_type_heaps
	  fun_type =
	  			{ st_vars					= fresh_type_vars
	  			, st_args					= fresh_arg_types
	  			, st_arity					= fun_arity
	  			, st_args_strictness		= NotStrict
	  			, st_result					= fresh_result_type
	  			, st_context				= []
	  			, st_attr_vars				= []
	  			, st_attr_env				= []
	  			}
	  ti									= { ti & ti_type_heaps = ti_type_heaps }
	= (fun_type,type_variables,ti)

removeNeverMatchingSubcases :: Expression !.ReadOnlyTI -> Expression
removeNeverMatchingSubcases keesExpr=:(Case kees) ro
	// remove those case guards whose right hand side is a never matching case
	| is_never_matching_case keesExpr
		= keesExpr
	# {case_guards, case_default} = kees
	  filtered_default = get_filtered_default case_default
	= case case_guards of
		AlgebraicPatterns i alg_patterns
			| not (any (is_never_matching_case o get_alg_rhs) alg_patterns) && not (is_never_matching_default case_default)
				-> keesExpr // frequent case: all subexpressions can't fail
			# filtered_case_guards = filter (not o is_never_matching_case o get_alg_rhs) alg_patterns
			| has_become_never_matching filtered_default filtered_case_guards
				-> neverMatchingCase never_ident
			| is_default_only filtered_default filtered_case_guards
				-> fromYes case_default
			-> Case {kees & case_guards = AlgebraicPatterns i filtered_case_guards, case_default = filtered_default }
		BasicPatterns bt basic_patterns
			| not (any (is_never_matching_case o get_basic_rhs) basic_patterns) && not (is_never_matching_default case_default)
				-> keesExpr // frequent case: all subexpressions can't fail
			# filtered_case_guards = filter (not o is_never_matching_case o get_basic_rhs) basic_patterns
			| has_become_never_matching filtered_default filtered_case_guards
				-> neverMatchingCase never_ident
			| is_default_only filtered_default filtered_case_guards
				-> fromYes case_default
			-> Case {kees & case_guards = BasicPatterns bt filtered_case_guards, case_default = filtered_default }
		OverloadedListPatterns i decons_expr alg_patterns
			| not (any (is_never_matching_case o get_alg_rhs) alg_patterns) && not (is_never_matching_default case_default)
				-> keesExpr // frequent case: all subexpressions can't fail
			# filtered_case_guards = filter (not o is_never_matching_case o get_alg_rhs) alg_patterns
			| has_become_never_matching filtered_default filtered_case_guards
				-> neverMatchingCase never_ident
			| is_default_only filtered_default filtered_case_guards
				-> fromYes case_default
			-> Case {kees & case_guards = OverloadedListPatterns i decons_expr filtered_case_guards, case_default = filtered_default }
		_	-> abort "removeNeverMatchingSubcases does not match"
where
	get_filtered_default y=:(Yes c_default)
		| is_never_matching_case c_default
			= No
		= y
	get_filtered_default no
		= no
	has_become_never_matching No [] = True
	has_become_never_matching _ _ = False
	is_default_only (Yes _) [] = True
	is_default_only _ _ = False
	is_never_matching_case (Case {case_guards = NoPattern, case_default = No })
		= True
	is_never_matching_case _
		= False
	get_alg_rhs {ap_expr} = ap_expr
	get_basic_rhs {bp_expr} = bp_expr
	is_never_matching_default No
		= False
	is_never_matching_default (Yes expr)
		= is_never_matching_case expr
	never_ident = case ro.ro_root_case_mode of
		NotRootCase -> kees.case_ident
		_ -> Yes ro.ro_tfi.tfi_case.symb_ident
removeNeverMatchingSubcases expr ro
	= expr

instance transform LetBind
where
	transform bind=:{lb_src} ro ti
		# (lb_src, ti) = transform lb_src ro ti
		= ({ bind & lb_src = lb_src }, ti)

instance transform BasicPattern
where
	transform pattern=:{bp_expr} ro ti
		# (bp_expr, ti) = transform bp_expr ro ti
		= ({ pattern & bp_expr = bp_expr }, ti)

instance transform AlgebraicPattern
where
	transform pattern=:{ap_expr} ro ti
		# (ap_expr, ti) = transform ap_expr ro ti
		= ({ pattern & ap_expr = ap_expr }, ti)

instance transform CasePatterns
where
	transform (AlgebraicPatterns type patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		= (AlgebraicPatterns type patterns, ti)
	transform (BasicPatterns type patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		= (BasicPatterns type patterns, ti)
	transform (OverloadedListPatterns type=:(OverloadedList _ _ _ _) decons_expr patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		# (decons_expr, ti) = transform decons_expr ro ti
		= (OverloadedListPatterns type decons_expr patterns, ti)
	transform (OverloadedListPatterns type decons_expr patterns) ro ti
		# (patterns, ti) = transform patterns ro ti
		# (decons_expr, ti) = transform decons_expr ro ti
		= (OverloadedListPatterns type decons_expr patterns, ti)
	transform NoPattern ro ti
		= (NoPattern, ti)
	transform _ ro ti
		= abort "transform CasePatterns does not match"

instance transform (Optional a) | transform a
where
	transform (Yes x) ro ti
		# (x, ti) = transform x ro ti
		= (Yes x, ti)
	transform no ro ti
		= (no, ti)

instance transform [a] | transform a
where
	transform [x : xs]  ro ti
		# (x, ti) = transform x ro ti
		  (xs, ti) = transform xs ro ti
		= ([x : xs], ti)
	transform [] ro ti
		= ([], ti)

cIsANewFunction		:== True
cIsNotANewFunction	:== False

tryToFindInstance :: !{! Producer} !InstanceInfo !*FunctionHeap -> *(!Bool, !FunctionInfoPtr, !InstanceInfo, !.FunctionHeap)
tryToFindInstance new_prods II_Empty fun_heap
	# (fun_def_ptr, fun_heap) = newPtr FI_Empty fun_heap
	= (cIsANewFunction, fun_def_ptr, II_Node new_prods fun_def_ptr II_Empty II_Empty, fun_heap)
tryToFindInstance new_prods instances=:(II_Node prods fun_def_ptr left right) fun_heap
	| size new_prods > size prods
		# (is_new, new_fun_def_ptr, right, fun_heap) = tryToFindInstance new_prods right fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)
	| size new_prods < size prods
		# (is_new, new_fun_def_ptr, left, fun_heap) = tryToFindInstance new_prods left fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)
	# cmp = compareProducers new_prods prods
	| cmp == Equal
		= (cIsNotANewFunction, fun_def_ptr, instances, fun_heap)
	| cmp == Greater
		# (is_new, new_fun_def_ptr, right, fun_heap) = tryToFindInstance new_prods right fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)
		# (is_new, new_fun_def_ptr, left, fun_heap) = tryToFindInstance new_prods left fun_heap
		= (is_new, new_fun_def_ptr, II_Node prods fun_def_ptr left right, fun_heap)

compareProducers prods1 prods2
	#! nr_of_prods = size prods1
	= compare_producers 0 nr_of_prods prods1 prods2
where
	compare_producers prod_index nr_of_prods prods1 prods2
		| prod_index == nr_of_prods
			= Equal
		# cmp = prods1.[prod_index] =< prods2.[prod_index]
		| cmp == Equal
			= compare_producers (inc prod_index) nr_of_prods prods1 prods2
		= cmp

instance =< Bool
where
	(=<) True True = Equal
	(=<) True False = Smaller
	(=<) False True = Greater
	(=<) False False = Equal

instance =< Producer
where
	(=<) pr1 pr2
		| equal_constructor pr1 pr2
			= compare_constructor_arguments  pr1 pr2
		| less_constructor pr1 pr2
			= Smaller
			= Greater
	where
		compare_constructor_arguments (PR_Function _ _ index1) (PR_Function _ _ index2)
			= index1 =< index2
		compare_constructor_arguments (PR_GeneratedFunction _ _ index1) (PR_GeneratedFunction _ _ index2)
			= index1 =< index2
		compare_constructor_arguments 	(PR_Class app1 lifted_vars_with_types1 t1)
										(PR_Class app2 lifted_vars_with_types2 t2)
			# cmp = smallerOrEqual t1 t2
			| cmp<>Equal
				= cmp
			= compare_types lifted_vars_with_types1 lifted_vars_with_types2
		compare_constructor_arguments (PR_Curried symb_ident1 _) (PR_Curried symb_ident2 _)
			= symb_ident1 =< symb_ident2
		compare_constructor_arguments PR_Empty PR_Empty
			= Equal
		compare_constructor_arguments PR_Unused PR_Unused
			= Equal
		compare_constructor_arguments (PR_Constructor symb_ident1 _ _) (PR_Constructor symb_ident2 _ _)
			= symb_ident1 =< symb_ident2
		compare_constructor_arguments (PR_CurriedFunction symb_ident1 _ _) (PR_CurriedFunction symb_ident2 _ _)
			= symb_ident1 =< symb_ident2
		compare_constructor_arguments (PR_String s1) (PR_String s2)
			| s1==s2
				= Equal
			| s1<s2
				= Smaller
				= Greater
		compare_constructor_arguments (PR_Int i1) (PR_Int i2)
			| i1==i2
				= Equal
			| i1<i2
				= Smaller
				= Greater
		compare_constructor_arguments PR_Equal PR_Equal
			= Equal
		compare_constructor_arguments (PR_EqualRemove i1) (PR_EqualRemove i2)
			| i1==i2
				= Equal
			| i1<i2
				= Smaller
				= Greater

		compare_types [(_, type1):types1] [(_, type2):types2]
			# cmp = smallerOrEqual type1 type2
			| cmp<>Equal
				= cmp
			= compare_types types1 types2
		compare_types [] [] = Equal
		compare_types [] _ = Smaller
		compare_types _ [] = Greater
		
/*
 *	UNIQUENESS STUFF...
 */

create_fresh_type_vars :: !Int !*TypeVarHeap -> (!{!TypeVar}, !*TypeVarHeap)
create_fresh_type_vars nr_of_all_type_vars th_vars
	# fresh_array = createArray  nr_of_all_type_vars {tv_ident = {id_name="",id_info=nilPtr}, tv_info_ptr=nilPtr}
	= iFoldSt allocate_fresh_type_var 0 nr_of_all_type_vars (fresh_array,th_vars)
where
	allocate_fresh_type_var i (array, th_vars)
		# (new_tv_info_ptr, th_vars) = newPtr TVI_Empty th_vars
		  tv = { tv_ident = { id_name = "a"+++toString i, id_info = nilPtr }, tv_info_ptr=new_tv_info_ptr }
		= ({array & [i] = tv}, th_vars)

create_fresh_attr_vars :: !{!CoercionTree} !Int !*AttrVarHeap -> (!{!TypeAttribute}, !.AttrVarHeap)
create_fresh_attr_vars demanded nr_of_attr_vars th_attrs
	# fresh_array = createArray nr_of_attr_vars TA_None
	= iFoldSt (allocate_fresh_attr_var demanded) 0 nr_of_attr_vars (fresh_array, th_attrs)
where
	allocate_fresh_attr_var demanded i (attr_var_array, th_attrs)
		= case demanded.[i] of
			CT_Unique
				-> ({ attr_var_array & [i] = TA_Unique}, th_attrs)
			CT_NonUnique
				-> ({ attr_var_array & [i] = TA_Multi}, th_attrs)
			_
				# (av, th_attrs) = NewAttrVar i th_attrs
				-> ({attr_var_array & [i] = TA_Var av}, th_attrs)

coercionsToAttrEnv :: !{!TypeAttribute} !Coercions -> [AttrInequality]
coercionsToAttrEnv attr_vars {coer_offered}
	= coercionsToAttrEnv 0 attr_vars coer_offered
where
	coercionsToAttrEnv :: !Int !{!TypeAttribute} !{!CoercionTree} -> [AttrInequality]
	coercionsToAttrEnv demanded_i attr_vars coer_offered
		| demanded_i<size coer_offered
			# (offered,_) = flattenCoercionTree coer_offered.[demanded_i]
			= coercionsToAttrEnvNextTree offered demanded_i attr_vars coer_offered
			= []

	coercionsToAttrEnvNextTree :: ![Int] !Int !{!TypeAttribute} !{!CoercionTree} -> [AttrInequality]
	coercionsToAttrEnvNextTree [offered_i:offered_is] demanded_i attr_vars coer_offered
		#! attr_inequalities = coercionsToAttrEnvNextTree offered_is demanded_i attr_vars coer_offered
		# (TA_Var demanded_attr_var) = attr_vars.[demanded_i]
		#! demanded_attr_var=demanded_attr_var
		# (TA_Var offered_attr_var) = attr_vars.[offered_i]
		#! offered_attr_var=offered_attr_var
		= [{ai_offered = offered_attr_var, ai_demanded = demanded_attr_var} : attr_inequalities]
	coercionsToAttrEnvNextTree [] demanded_i attr_vars coer_offered
		= coercionsToAttrEnv (demanded_i+1) attr_vars coer_offered

substitute_attr_inequality {ai_offered, ai_demanded} th_attrs
	#! ac_offered = pointer_to_int ai_offered th_attrs
	   ac_demanded = pointer_to_int ai_demanded th_attrs
	= ({ ac_offered = ac_offered, ac_demanded = ac_demanded }, th_attrs)
  where
	pointer_to_int {av_info_ptr} th_attrs
		# (AVI_Attr (TA_TempVar i)) = sreadPtr av_info_ptr th_attrs
		= i

new_inequality {ac_offered, ac_demanded} coercions
	= newInequality ac_offered ac_demanded coercions

:: UniquenessRequirement =
	{	ur_offered		:: !AType
	,	ur_demanded		:: !AType
	,	ur_attr_ineqs	:: ![AttrCoercion]
	}

:: ATypesWithStrictness = {ats_types::![AType],ats_strictness::!StrictnessList};

compute_args_strictness new_arg_types_array = compute_args_strictness 0 0 NotStrict 0 new_arg_types_array
  	where
  		compute_args_strictness strictness_index strictness strictness_list array_index new_arg_types_array
  			| array_index==size new_arg_types_array
  				| strictness==0
  					= strictness_list
  					= append_strictness strictness strictness_list
  				# {ats_types,ats_strictness} = new_arg_types_array.[array_index]
  				# (strictness_index,strictness) = add_strictness_for_arguments ats_types 0 strictness_index strictness strictness_list
  					with
  						add_strictness_for_arguments [] ats_strictness_index strictness_index strictness strictness_list
  							= (strictness_index,strictness)
  						add_strictness_for_arguments [_:ats_types] ats_strictness_index strictness_index strictness strictness_list
  							| arg_is_strict ats_strictness_index ats_strictness
  								# (strictness_index,strictness,strictness_list) = add_next_strict strictness_index strictness strictness_list
  								= add_strictness_for_arguments ats_types (ats_strictness_index+1) strictness_index strictness strictness_list
  								# (strictness_index,strictness,strictness_list) = add_next_not_strict strictness_index strictness strictness_list
  								= add_strictness_for_arguments ats_types (ats_strictness_index+1) strictness_index strictness strictness_list
  				= compute_args_strictness strictness_index strictness strictness_list (array_index+1) new_arg_types_array
	
/*
 * GENERATE FUSED FUNCTION
 */

:: OptionalProducerType = ProducerType !SymbolType ![TypeVar] | NoProducerType

:: *DetermineArgsState =
	{ das_vars						:: ![FreeVar]
	, das_arg_types					:: !*{#ATypesWithStrictness}
	, das_next_attr_nr				:: !Int
	, das_new_linear_bits			:: ![#Bool!]
	, das_new_cons_args				:: ![ConsClass]
	, das_uniqueness_requirements	:: ![UniquenessRequirement]
	, das_AVI_Attr_TA_TempVar_info_ptrs	:: ![[AttributeVar]]
	, das_subst						:: !*{!Type}
	, das_type_heaps				:: !*TypeHeaps
	, das_fun_defs					:: !*{#FunDef}
	, das_fun_heap					:: !*FunctionHeap
	, das_var_heap					:: !*VarHeap
	, das_cons_args					:: !*{!ConsClasses}
	, das_predef					:: !*PredefinedSymbols
	, das_removed_equal_info_ptrs	:: ![#VarInfoPtr!]
	}

generateFunction :: !SymbIdent !FunDef ![ConsClass] ![#Bool!] !{! Producer} !FunctionInfoPtr !ReadOnlyTI !Int !*TransformInfo -> (!Index, !Int, !*TransformInfo)
generateFunction app_symb fd=:{fun_body = TransformedBody {tb_args,tb_rhs},fun_info = {fi_group_index,fi_properties},fun_arity}
				 cc_args cc_linear_bits prods fun_def_ptr ro n_extra
				 ti=:{ti_var_heap,ti_next_fun_nr,ti_new_functions,ti_fun_heap,ti_symbol_heap,ti_fun_defs,
				 		ti_type_heaps,ti_cons_args,ti_cleanup_info, ti_type_def_infos}
//	| False--->("generating new function",fd.fun_ident.id_name,"->",ti_next_fun_nr,prods,tb_args)		= undef
/*
	| False-!->("generating new function",fd.fun_ident.id_name,"->",ti_next_fun_nr)		= undef
	| False-!->("with type",fd.fun_type)												= undef
	| False-!->("producers:",II_Node prods nilPtr II_Empty II_Empty,("cc_args",cc_args,("cc_linear_bits",cc_linear_bits)))		= undef
	| False-!->("body:",tb_args, tb_rhs)												= undef
*/
	#!(fi_group_index, ti_cons_args, ti_fun_defs, ti_fun_heap)
			= max_group_index 0 prods ro.ro_main_dcl_module_n fi_group_index ti_fun_defs ti_fun_heap ti_cons_args

	# (Yes consumer_symbol_type) = fd.fun_type
	  consumer_symbol_type		= strip_universal_quantor consumer_symbol_type
	  (sound_consumer_symbol_type, (ti_type_heaps, ti_type_def_infos))
	  		= add_propagation_attributes` ro.ro_common_defs consumer_symbol_type (ti_type_heaps, ti_type_def_infos)
	  (function_producer_types, ti_fun_defs, ti_fun_heap)
	  		= iFoldSt (accum_function_producer_type prods ro) 0 (size prods) 
	  				([], ti_fun_defs, ti_fun_heap)
	  function_producer_types	= mapOpt strip_universal_quantor function_producer_types
	  (opt_sound_function_producer_types, (ti_type_heaps, ti_type_def_infos))
	  		= mapSt (add_propagation_attributes ro.ro_common_defs) function_producer_types (ti_type_heaps, ti_type_def_infos)
	  (opt_sound_function_producer_types, ti_type_heaps)
	  		= mapSt copy_opt_symbol_type opt_sound_function_producer_types ti_type_heaps

	  sound_function_producer_types = [x \\ ProducerType x _ <- opt_sound_function_producer_types]

	# {st_attr_vars,st_args,st_args_strictness,st_result,st_attr_env} = sound_consumer_symbol_type

	  class_types = [{at_attribute = TA_Multi, at_type = class_type} \\ PR_Class _ _ class_type <-:prods]
	  (type_vars_in_class_types, th_vars) = mapSt getTypeVars class_types ti_type_heaps.th_vars

	  all_involved_types
	  		= class_types ++ (flatten (map (\{st_args, st_result}-> [st_result:st_args]) 
					  					[sound_consumer_symbol_type:sound_function_producer_types]))
//	| False ---> ("all_involved_types",app_symb,all_involved_types)	= undef
	# (propagating_cons_vars, th_vars)
	  		= collectPropagatingConsVars all_involved_types th_vars
	  all_type_vars
	  		=   flatten [st_vars \\ {st_vars} <- [sound_consumer_symbol_type:sound_function_producer_types]]
	  		  ++flatten type_vars_in_class_types
//	| False -!-> ("all_type_vars",all_type_vars)	= undef
	# (nr_of_all_type_vars, th_vars) = foldSt bind_to_temp_type_var all_type_vars (0, th_vars)
	  subst = createArray nr_of_all_type_vars TE
	  (next_attr_nr, th_attrs) = bind_to_temp_attr_vars st_attr_vars (FirstAttrVar, ti_type_heaps.th_attrs)
	  // remember the st_attr_vars, because the AVI_Attr (TA_TempVar _)'s must be removed before unfold,
	  // because types in Cases and Lets should not use TA_TempVar's
	  das_AVI_Attr_TA_TempVar_info_ptrs = [st_attr_vars]
	  ti_type_heaps = {ti_type_heaps & th_attrs = th_attrs, th_vars = th_vars}
	  (_, st_args, ti_type_heaps) = substitute st_args ti_type_heaps
	  (_, st_result, ti_type_heaps) = substitute st_result ti_type_heaps
// determine args...
	# das = { das_vars						= []
			, das_arg_types					= st_args_array st_args st_args_strictness
			, das_next_attr_nr				= next_attr_nr
			, das_new_linear_bits			= [#!]
			, das_new_cons_args				= []
			, das_uniqueness_requirements	= []
			, das_AVI_Attr_TA_TempVar_info_ptrs = das_AVI_Attr_TA_TempVar_info_ptrs
			, das_subst						= subst
			, das_type_heaps				= ti_type_heaps
			, das_fun_defs					= ti_fun_defs
			, das_fun_heap					= ti_fun_heap
			, das_var_heap					= ti_var_heap
			, das_cons_args					= ti_cons_args
			, das_predef					= ti.ti_predef_symbols
			, das_removed_equal_info_ptrs = [#!]
			}
	# das		= determine_args cc_linear_bits cc_args 0 prods opt_sound_function_producer_types tb_args ro das
	  uvar		= [arg \\ prod <-: prods & arg <- tb_args | isUnused prod]
	  				with
	  					isUnused PR_Unused = True
//	  					isUnused (PR_EqualRemove _) = True
	  					isUnused _ = False
	  
	  new_fun_args				= das.das_vars
	  new_arg_types_array		= das.das_arg_types
	  next_attr_nr				= das.das_next_attr_nr
	  new_linear_bits			= das.das_new_linear_bits
	  new_cons_args				= das.das_new_cons_args
	  uniqueness_requirements	= das.das_uniqueness_requirements
	  das_AVI_Attr_TA_TempVar_info_ptrs = das.das_AVI_Attr_TA_TempVar_info_ptrs
	  subst						= das.das_subst
	  ti_type_heaps				= das.das_type_heaps
	  ti_fun_defs				= das.das_fun_defs
	  ti_fun_heap				= das.das_fun_heap
	  ti_var_heap				= das.das_var_heap
	  ti_cons_args				= das.das_cons_args
	  ti_predef_symbols			= das.das_predef
	  das_removed_equal_info_ptrs = das.das_removed_equal_info_ptrs

	  new_fun_arity = length new_fun_args
	| SwitchArityChecks (new_fun_arity > 32 && new_fun_arity >= fun_arity) False
		# new_gen_fd =
			{	gf_fun_def			= fd
			,	gf_instance_info	= II_Empty
			,	gf_cons_args		= {cc_args=[], cc_size=0, cc_linear_bits=[#!], cc_producer=False}
			,	gf_fun_index		= -1
			}
		# ti_fun_heap 	= ti_fun_heap <:= (fun_def_ptr, FI_Function new_gen_fd)
		# ti = { ti & ti_type_heaps = ti_type_heaps, ti_symbol_heap = ti_symbol_heap, ti_fun_defs = ti_fun_defs
				, ti_fun_heap = ti_fun_heap, ti_var_heap = ti_var_heap, ti_cons_args = ti_cons_args, ti_type_def_infos = ti_type_def_infos
				, ti_predef_symbols = ti_predef_symbols }
		| ro.ro_transform_fusion>=FullFusion
			#  ti = { ti & ti_error_file = ti.ti_error_file <<< "Possibly missed fusion oppurtunity: Function Arity > 32 " <<< ro.ro_tfi.tfi_root.symb_ident.id_name <<< "\n"}
			= (-1,new_fun_arity,ti)
		= (-1,new_fun_arity,ti)
	# new_arg_types = flatten [ ats_types \\ {ats_types}<-:new_arg_types_array ]
	  new_args_strictness = compute_args_strictness new_arg_types_array

	  cons_vars = createArray (inc (BITINDEX nr_of_all_type_vars)) 0
	  (cons_vars, th_vars)
			= foldSt set_cons_var_bit propagating_cons_vars (cons_vars, ti_type_heaps.th_vars)
//	| False--->("subst before", [el\\el<-:subst], "cons_vars", [el\\el<-:cons_vars])		= undef
	# ti_type_heaps = { ti_type_heaps & th_vars = th_vars }

	# (next_attr_nr, subst, ti_type_def_infos, ti_type_heaps)
		= foldSt (lift_offered_substitutions_for_unification ro.ro_common_defs cons_vars) uniqueness_requirements (next_attr_nr, subst, ti_type_def_infos, ti_type_heaps)
	# (subst, next_attr_nr, ti_type_heaps, ti_type_def_infos)
  		= liftRemainingSubstitutions subst ro.ro_common_defs cons_vars next_attr_nr ti_type_heaps ti_type_def_infos
//	| False--->("subst after lifting", [el\\el<-:subst])		= undef

	# (consumer_attr_inequalities, th_attrs)
			= mapSt substitute_attr_inequality st_attr_env ti_type_heaps.th_attrs
	  ti_type_heaps & th_attrs = th_attrs

	  coercions
	  		= { coer_offered	= {{ CT_Empty \\ i <- [0 .. next_attr_nr - 1] } & [AttrMulti] = CT_NonUnique }
	  		  , coer_demanded	= {{ CT_Empty \\ i <- [0 .. next_attr_nr - 1] } & [AttrUni] = CT_Unique } }
	  coercions
	  		= foldSt new_inequality consumer_attr_inequalities coercions
	  coercions
	  		= foldSt (\{ur_attr_ineqs} coercions -> foldSt new_inequality ur_attr_ineqs coercions) uniqueness_requirements coercions
	  (subst, coercions, ti_type_def_infos, ti_type_heaps)
	  		= foldSt (coerce_types ro.ro_common_defs cons_vars) uniqueness_requirements (subst, coercions, ti_type_def_infos, ti_type_heaps)
	# ([st_result:new_arg_types], (coercions, subst, ti_type_heaps, ti_type_def_infos))
	  		= mapSt (expand_type ro.ro_common_defs cons_vars) [st_result:new_arg_types]
	  				(coercions, subst, ti_type_heaps, ti_type_def_infos)
	# (fresh_type_vars_array,ti_type_heaps)
	  		= accTypeVarHeap (create_fresh_type_vars nr_of_all_type_vars) ti_type_heaps
	  (attr_partition, demanded) 
	  		= partitionateAttributes coercions.coer_offered coercions.coer_demanded
	  		// to eliminate circles in the attribute inequalities graph that was built during "determine_args"
	  (fresh_attr_vars, ti_type_heaps)
	  		= accAttrVarHeap (create_fresh_attr_vars demanded (size demanded)) ti_type_heaps
	  		// the attribute variables stored in the "demanded" graph are represented as integers: 
	  		// prepare to replace them by pointers
	  used_attr_vars = createArray (size demanded) False
	  replace_input = (fresh_type_vars_array, fresh_attr_vars, attr_partition)
	  (_, fresh_arg_types, used_attr_vars) = replaceIntegers new_arg_types replace_input used_attr_vars
	  (_, fresh_result_type, used_attr_vars) = replaceIntegers st_result replace_input used_attr_vars
			// replace the integer-attribute-variables with pointer-attribute-variables or TA_Unique or TA_Multi
	  final_coercions
	  		= removeUnusedAttrVars demanded [i \\ i<-[0..size used_attr_vars-1] | not used_attr_vars.[i]]
			// the attribute inequalities graph may have contained unused attribute variables.

 	  (all_attr_vars2, ti_type_heaps)
 	  		= accAttrVarHeap (getAttrVars (fresh_arg_types, fresh_result_type)) ti_type_heaps
	  all_attr_vars = get_used_attr_vars 0 used_attr_vars fresh_attr_vars
	  (all_fresh_type_vars, ti_type_heaps)
 	  		= accTypeVarHeap (getTypeVars (fresh_arg_types, fresh_result_type)) ti_type_heaps
	  new_fun_type
	  		= Yes
	  			{ st_vars		= all_fresh_type_vars
	  			, st_args		= fresh_arg_types
	  			, st_args_strictness=new_args_strictness
	  			, st_arity		= new_fun_arity
	  			, st_result		= fresh_result_type
	  			, st_context	= []
	  			, st_attr_vars	= all_attr_vars
	  			, st_attr_env	= coercionsToAttrEnv fresh_attr_vars final_coercions 
				}
/* DvA... STRICT_LET
	  // DvA: moet hier rekening houden met strictness dwz alleen safe args expanderen en rest in stricte let genereren...
	  (tb_rhs,ti_symbol_heap,strict_free_vars) = case let_bindings of
	  			([],[],_,_)
	  				-> (tb_rhs,ti_symbol_heap,[])
	  			(s,l,st,lt)
					# let_type = st++lt
					# (new_info_ptr, ti_symbol_heap) = newPtr (EI_LetType let_type) ti_symbol_heap
	  				# new_expr = Let
	  						{ let_strict_binds	= s
	  						, let_lazy_binds	= l
	  						, let_expr			= tb_rhs
	  						, let_info_ptr		= new_info_ptr
	  						, let_expr_position	= NoPos
	  						}
	  				# strict_free_vars = [lb_dst \\ {lb_dst} <- s]
	  				-> (new_expr,ti_symbol_heap,strict_free_vars)
...DvA */
	  new_fd_expanding 
	  		= { fd & fun_body = Expanding new_fun_args, fun_arity = new_fun_arity,fun_type=new_fun_type,
	  				 fun_info.fi_group_index = fi_group_index,
	  				 fun_info.fi_properties = fi_properties bitand FI_CopyMask 
/* DvA... STRICT_LET
					,fun_info.fi_free_vars = strict_free_vars++fd.fun_info.fi_free_vars
...DvA */
			  }
	  new_fd_cons_args
//	  		= {cc_args = new_cons_args, cc_size = length new_cons_args, cc_linear_bits=new_linear_bits, cc_producer = False}
	  		= {cc_args = repeatn (length new_cons_args) CPassive, cc_size = length new_cons_args, cc_linear_bits=new_linear_bits, cc_producer = False}
	  new_gen_fd
	  		= { gf_fun_def = new_fd_expanding,	gf_instance_info = II_Empty, gf_fun_index = ti_next_fun_nr,
				 gf_cons_args = new_fd_cons_args }
	  ti_fun_heap = ti_fun_heap <:= (fun_def_ptr, FI_Function new_gen_fd)
	  (subst, _)
	  		= iFoldSt (replace_integers_in_substitution (fresh_type_vars_array, fresh_attr_vars, attr_partition))
	  				0 nr_of_all_type_vars (subst, createArray (size demanded) False)
	  		// replace the integer-attribute-variables with pointer-attribute-variables or TA_Unique or TA_Multi in subst
	  (_, th_vars)
	  		= foldSt (\{tv_info_ptr} (i, th_vars) 
	  					-> case subst.[i] of
	  						TE
	  							-> (i+1, writePtr tv_info_ptr (TVI_Type (TV fresh_type_vars_array.[i])) th_vars)
	  						_
	  							-> (i+1, writePtr tv_info_ptr (TVI_Type subst.[i]) th_vars))
	  				all_type_vars (0, ti_type_heaps.th_vars)
	  // remove the AVI_Attr (TA_TempVar _)'s before unfold, because types in Cases and Lets should not use TA_TempVar's
	  th_attrs = remove_TA_TempVars_in_info_ptrs das_AVI_Attr_TA_TempVar_info_ptrs ti_type_heaps.th_attrs
	  cs 	=	{ cs_var_heap				= ti_var_heap
	  			, cs_symbol_heap			= ti_symbol_heap
				, cs_opt_type_heaps			= Yes { ti_type_heaps & th_vars=th_vars, th_attrs=th_attrs }
	  			, cs_cleanup_info			= ti_cleanup_info
	  			}
//	| False ---> ("before unfold:", tb_rhs) = undef
	# (tb_rhs, {cs_var_heap=var_heap,cs_symbol_heap,cs_opt_type_heaps=Yes ti_type_heaps, cs_cleanup_info})
			= copy tb_rhs {ci_handle_aci_free_vars = RemoveAciFreeVars} cs
//	| False ---> ("unfolded:", tb_rhs) = undef

	# th_vars = remove_TVI_Type_values all_type_vars ti_type_heaps.th_vars
	  th_attrs = foldSt remove_AVI_Attr_values das_AVI_Attr_TA_TempVar_info_ptrs ti_type_heaps.th_attrs
	  ti_type_heaps & th_vars=th_vars, th_attrs=th_attrs

	# var_heap						= fold2St store_arg_type_info new_fun_args fresh_arg_types var_heap
		with
			store_arg_type_info {fv_info_ptr} a_type ti_var_heap
				= setExtendedVarInfo fv_info_ptr (EVI_VarType a_type) ti_var_heap
	# var_heap = set_removed_equal_info_ptrs_VI_Empty das_removed_equal_info_ptrs var_heap
		with
			set_removed_equal_info_ptrs_VI_Empty [#p:ps!] var_heap
				= set_removed_equal_info_ptrs_VI_Empty ps (writeVarInfo p VI_Empty var_heap)
			set_removed_equal_info_ptrs_VI_Empty [#!] var_heap
				= var_heap
	# ro_fun= { symb_ident = fd.fun_ident, symb_kind = SK_GeneratedFunction fun_def_ptr ti_next_fun_nr }
	# ro_root_case_mode = case tb_rhs of 
	  						Case _
	  							-> RootCase
	  						_	-> NotRootCase

	# (n_args_before_producer,n_producer_args,var_heap)
		= if (more_unused_producers prods)
			(-1,-1,var_heap)
			(n_args_before_producer_and_n_producer_args tb_args new_fun_args var_heap)

	# tfi = {	tfi_root = ro_fun,
				tfi_case = ro_fun,
				tfi_orig = app_symb,
				tfi_args = new_fun_args,
				tfi_vars = uvar ++ [arg \\ arg <- new_fun_args & i <- [0..] | arg_is_strict i new_args_strictness],
										// evt ++ verwijderde stricte arg...
				tfi_n_args_before_producer = n_args_before_producer,
				tfi_n_producer_args = n_producer_args
			 }
	# ro = { ro & ro_root_case_mode = ro_root_case_mode, ro_tfi=tfi}
				// ---> ("genfun uvars",uvar,[arg \\ arg <- new_fun_args & i <- [0..] | arg_is_strict i new_args_strictness])
//	| False ---> ("transform generated function:",ti_next_fun_nr,ro_root_case_mode)		= undef
//	| False ---> ("transforming new function:",ti_next_fun_nr,tb_rhs)		= undef
//	| False -!-> ("transforming new function:",tb_rhs)		= undef
	# ti
			= { ti & ti_var_heap = var_heap, ti_fun_heap = ti_fun_heap, ti_symbol_heap = cs_symbol_heap,
	  			ti_next_fun_nr = inc ti_next_fun_nr, ti_type_def_infos = ti_type_def_infos,
	  			ti_new_functions = [fun_def_ptr : ti_new_functions], ti_fun_defs = ti_fun_defs,
	  			ti_type_heaps = ti_type_heaps, ti_cleanup_info = cs_cleanup_info,
	  			ti_cons_args = ti_cons_args,
	  			ti_predef_symbols = ti_predef_symbols }
	# ti = arity_warning "generateFunction" fd.fun_ident.id_name ti_next_fun_nr new_fun_arity ti

	# (tb_rhs,ti)		= case n_extra of
							0	-> (tb_rhs,ti)
							_
								# act_args = map f2b (reverse (take n_extra (reverse new_fun_args)))
									with
										f2b { fv_ident, fv_info_ptr }
											= Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = nilPtr }
								-> add_args_to_fun_body act_args fresh_result_type tb_rhs ro ti
	  (new_fun_rhs, ti)
			= transform tb_rhs ro ti
	  new_fd
	  		= { new_fd_expanding & fun_body = TransformedBody {tb_args = new_fun_args, tb_rhs = new_fun_rhs} }
//	| False ---> ("generated function", new_fd)		= undef

	# new_gen_fd = { new_gen_fd & gf_fun_def = new_fd, gf_cons_args = new_fd_cons_args}
	# ti = { ti & ti_fun_heap 	= ti.ti_fun_heap <:= (fun_def_ptr, FI_Function new_gen_fd) }
	= (ti_next_fun_nr, new_fun_arity, ti)
where
	st_args_array :: ![AType] !StrictnessList -> .{#ATypesWithStrictness}
	st_args_array st_args args_strictness
		# strict1=Strict 1
		= { {ats_types=[el],ats_strictness=if (arg_is_strict i args_strictness) strict1 NotStrict} \\ i<-[0..] & el <- st_args }

	is_dictionary :: !.AType !{#{#.TypeDefInfo}} -> Bool
	is_dictionary {at_type=TA {type_index} _} es_td_infos
		#! td_infos_of_module=es_td_infos.[type_index.glob_module]
		= type_index.glob_object>=size td_infos_of_module || td_infos_of_module.[type_index.glob_object].tdi_group_nr==(-1)
	is_dictionary _ es_td_infos
		= False

	set_cons_var_bit :: !.TypeVar !*(!*{#.Int},!u:(Heap TypeVarInfo)) -> (!.{#Int},!v:(Heap TypeVarInfo)), [u <= v]
	set_cons_var_bit {tv_info_ptr} (cons_vars, th_vars)
		# (TVI_Type (TempV i), th_vars) = readPtr tv_info_ptr th_vars 
		= (set_bit i cons_vars, th_vars)

	copy_opt_symbol_type :: !(Optional SymbolType) !*TypeHeaps -> (!OptionalProducerType,!*TypeHeaps)
	copy_opt_symbol_type No ti_type_heaps
		= (NoProducerType, ti_type_heaps)
	copy_opt_symbol_type (Yes symbol_type=:{st_vars, st_attr_vars, st_args, st_result, st_attr_env})
				ti_type_heaps=:{th_vars, th_attrs}
		# (fresh_st_vars, th_vars) = bind_to_fresh_type_variables st_vars th_vars
		  (fresh_st_attr_vars, th_attrs)
				= mapSt bind_to_fresh_attr_variable st_attr_vars th_attrs
		  ti_type_heaps & th_vars = th_vars, th_attrs = th_attrs
		  (_, [fresh_st_result:fresh_st_args], ti_type_heaps) = substitute [st_result:st_args] ti_type_heaps
		  (_, fresh_st_attr_env, ti_type_heaps) = substitute st_attr_env ti_type_heaps
		  th_vars = remove_TVI_Type_values st_vars ti_type_heaps.th_vars
		  th_attrs = remove_AVI_Attr_values st_attr_vars ti_type_heaps.th_attrs
		  ti_type_heaps & th_vars=th_vars, th_attrs=th_attrs
		  symbol_type & st_vars = fresh_st_vars, st_attr_vars = fresh_st_attr_vars, st_args = fresh_st_args,
						st_result = fresh_st_result, st_attr_env = fresh_st_attr_env
		= (ProducerType symbol_type st_vars, ti_type_heaps)

	add_propagation_attributes :: !{#.CommonDefs} !(Optional .SymbolType) !*(!*TypeHeaps,!*{#*{#.TypeDefInfo}})
											  -> (!(Optional .SymbolType),! (!.TypeHeaps,! {#.{# TypeDefInfo}}))
	add_propagation_attributes common_defs No state
		= (No, state)
	add_propagation_attributes common_defs (Yes st) state
		# (st, state)	= add_propagation_attributes` common_defs st state
		= (Yes st, state)

	add_propagation_attributes` :: !{#.CommonDefs} !.SymbolType !*(!*TypeHeaps,!*{#*{#.TypeDefInfo}})
											   -> (!.SymbolType,! (!.TypeHeaps,! {#.{# TypeDefInfo}}))
	add_propagation_attributes` common_defs st=:{st_args, st_result, st_attr_env, st_attr_vars}
				(type_heaps, type_def_infos)
		# ps	=	{ prop_type_heaps	= type_heaps
					, prop_td_infos		= type_def_infos
					, prop_attr_vars	= st_attr_vars
					, prop_attr_env		= st_attr_env
					, prop_error		= No 
					}
		# ([sound_st_result:sound_st_args], ps)
			  	= mapSt (add_propagation_attributes_to_atype common_defs) [st_result:st_args] ps
		  sound_symbol_type = {st & st_args	= sound_st_args
								  , st_result = sound_st_result
								  , st_attr_env = ps.prop_attr_env
								  , st_attr_vars = ps.prop_attr_vars
								}
		  state = (ps.prop_type_heaps, ps.prop_td_infos)
		= (sound_symbol_type, state)

	add_propagation_attributes_to_atype :: !{#.CommonDefs} !.AType !*PropState -> (!AType,!.PropState)
	add_propagation_attributes_to_atype modules type ps
		| is_dictionary type ps.prop_td_infos
			= (type, ps)
			= addPropagationAttributesToAType modules type ps

	accum_function_producer_type :: !{!.Producer} !.ReadOnlyTI !.Int !*(!u:[v:(Optional .SymbolType)],!*{#.FunDef},!*(Heap FunctionInfo))
																	 -> (!w:[x:(Optional SymbolType)],!.{# FunDef},!.(Heap FunctionInfo)), [u <= w,v <= x]
	accum_function_producer_type prods ro i (type_accu, ti_fun_defs, ti_fun_heap)
		= case prods.[size prods-i-1] of
			PR_Empty
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			PR_Class _ _ class_type
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			PR_Unused
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			PR_String _
				# string_type = TA (MakeTypeSymbIdent {glob_object = PD_StringTypeIndex, glob_module = cPredefinedModuleIndex} predefined_idents.[PD_StringType] 0) []
				  string_atype = {at_attribute=TA_Multi,at_type=string_type}
				  symbol_type = {st_vars=[],st_args=[],st_args_strictness=NotStrict,st_arity=0,st_result=string_atype,st_context=[],st_attr_vars=[],st_attr_env=[]}
				-> ([Yes symbol_type:type_accu], ti_fun_defs, ti_fun_heap)
			PR_Int _
				# int_atype = {at_attribute=TA_Multi,at_type=TB BT_Int}
				  symbol_type = {st_vars=[],st_args=[],st_args_strictness=NotStrict,st_arity=0,st_result=int_atype,st_context=[],st_attr_vars=[],st_attr_env=[]}
				-> ([Yes symbol_type:type_accu], ti_fun_defs, ti_fun_heap)
			PR_Equal
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			PR_EqualRemove _
				-> ([No:type_accu], ti_fun_defs, ti_fun_heap)
			producer
				# (symbol,_) = get_producer_symbol producer
				  (symbol_type, ti_fun_defs, ti_fun_heap)
						= get_producer_type symbol ro ti_fun_defs ti_fun_heap
				-> ([Yes symbol_type:type_accu], ti_fun_defs, ti_fun_heap)

	collectPropagatingConsVars :: ![AType] !*(Heap TypeVarInfo) -> (!.[TypeVar],!.(Heap TypeVarInfo))
	collectPropagatingConsVars type th_vars
		# th_vars = performOnTypeVars initializeToTVI_Empty type th_vars
		= performOnTypeVars collect_unencountered_cons_var type ([], th_vars)
	  where
		collect_unencountered_cons_var :: !.TypeAttribute !u:TypeVar !*(!v:[w:TypeVar],!*(Heap TypeVarInfo)) -> (!x:[y:TypeVar],!.(Heap TypeVarInfo)), [v <= x,w u <= y]
		collect_unencountered_cons_var TA_MultiOfPropagatingConsVar tv=:{tv_info_ptr} (cons_var_accu, th_vars)
			# (tvi, th_vars) = readPtr tv_info_ptr th_vars
			= case tvi of
				TVI_Empty
					-> ([tv:cons_var_accu], writePtr tv_info_ptr TVI_Used th_vars)
				TVI_Used
					-> (cons_var_accu, th_vars)
		collect_unencountered_cons_var _ _ state
			= state

	replace_integers_in_substitution :: (!{!.TypeVar},!{!.TypeAttribute},!{#.Int}) !.Int !*(!*{!Type},!*{#.Bool}) -> (!.{!Type},!.{#Bool})
	replace_integers_in_substitution replace_input i (subst, used)
		# (subst_i, subst) = subst![i]
		  (_, subst_i, used) = replaceIntegers subst_i replace_input used
		= ({ subst & [i] = subst_i }, used)

	coerce_types common_defs cons_vars {ur_offered, ur_demanded} (subst, coercions, ti_type_def_infos, ti_type_heaps)
		# (opt_error_info, subst, coercions, ti_type_def_infos, ti_type_heaps)
				= determineAttributeCoercions ur_offered ur_demanded True True subst coercions common_defs cons_vars ti_type_def_infos ti_type_heaps
		= case opt_error_info of
			Yes _
				-> abort "Error in compiler: determineAttributeCoercions failed in module trans"
			No
				-> (subst, coercions, ti_type_def_infos, ti_type_heaps)

	lift_offered_substitutions_for_unification common_defs cons_vars {ur_offered, ur_demanded} (next_attr_nr,subst,ti_type_def_infos,ti_type_heaps)
		= liftOfferedSubstitutions ur_offered ur_demanded common_defs cons_vars next_attr_nr subst ti_type_def_infos ti_type_heaps

	expand_type :: !{#CommonDefs} !{#Int} !AType !*(!*Coercions,!*{!Type},!*TypeHeaps,!*TypeDefInfos)
									  -> (!AType,!*(!*Coercions,!*{!Type},!*TypeHeaps,!*TypeDefInfos))
	expand_type ro_common_defs cons_vars atype (coercions, subst, ti_type_heaps, ti_type_def_infos)
		| is_dictionary atype ti_type_def_infos
			# (_, atype, subst) = arraySubst atype subst
			= (atype, (coercions, subst, ti_type_heaps, ti_type_def_infos))
			= expand_and_coerce_type ro_common_defs cons_vars atype (coercions, subst, ti_type_heaps, ti_type_def_infos)

	n_args_before_producer_and_n_producer_args :: [FreeVar] [FreeVar] *VarHeap -> (!Int,!Int,!*VarHeap)
	n_args_before_producer_and_n_producer_args tb_args new_fun_args var_heap
		# (n_args1,resto,restn,var_heap) = take1 tb_args new_fun_args var_heap
			with
				take1 [o:os] [n:ns] var_heap
					# (vi,var_heap) = readVarInfo o.fv_info_ptr var_heap
					= case vi of
						VI_Variable _ fip
							| fip == n.fv_info_ptr
								# (n_args1,os,ns,var_heap) = take1 os ns var_heap
								-> (n_args1+1,os,ns,var_heap)
						_
							-> (0,[o:os],[n:ns],var_heap)
				take1 os ns var_heap
					= (0,os,ns,var_heap)
		# (n_args2n,resto,restn,var_heap) = take2 resto restn var_heap
			with
				take2 [] [] var_heap
					= (0,[],[],var_heap)
				take2 os ns var_heap
					# (os`,var_heap) = extend os var_heap 
					# os`` = map fst os`
					# ns`` = map (\{fv_info_ptr}->fv_info_ptr) ns
					# condO = \(o,_) -> not (isMember o ns``)
					# condN = \{fv_info_ptr} -> not (isMember fv_info_ptr os``)
					# ro` = dropWhile condO os`
					# an = takeWhile condN ns
					# rn = dropWhile condN ns
					# ro = shrink ro`
					= (length an,ro,rn,var_heap)
				where
					extend os uvh = mapSt ext os uvh
					where
						ext o uvh
							# (vi,uvh) = readVarInfo o.fv_info_ptr uvh
							= case vi of
								VI_Variable _ fip	-> ((fip,o),uvh)
								_					-> ((nilPtr,o),uvh)

					shrink as = map snd as

					isMember x [hd:tl]
						| isNilPtr x	= False
						| isNilPtr hd	= isMember x tl
						= hd==x || isMember x tl
					isMember x []	= False
		# var_heap = take3 resto restn var_heap
			with
				take3 [o:os] [n:ns] var_heap
					# (vi,var_heap) = readVarInfo o.fv_info_ptr var_heap
					= case vi of
						VI_Variable _ fip
							| fip == n.fv_info_ptr
								= take3 os ns var_heap
				take3 [] [] var_heap
					= var_heap
		= (n_args1,n_args2n,var_heap)

	get_used_attr_vars :: !Int !{#Bool} !{!TypeAttribute} -> [AttributeVar]
	get_used_attr_vars attr_var_n used_attr_vars fresh_attr_vars
		| attr_var_n<size used_attr_vars
			| used_attr_vars.[attr_var_n]
				# (TA_Var used_attr_var) = fresh_attr_vars.[attr_var_n]
				#! used_attr_var = used_attr_var
				#! used_attr_vars = get_used_attr_vars (attr_var_n+1) used_attr_vars fresh_attr_vars
				= [used_attr_var : used_attr_vars]
				= get_used_attr_vars (attr_var_n+1) used_attr_vars fresh_attr_vars
			= []

more_unused_producers producers
	= more_unused_producers 0 producers
where
	more_unused_producers i producers
		| i<size producers
			= case producers.[i] of
				PR_Empty
					-> more_unused_producers (i+1) producers
				PR_Unused
					-> more_unused_producers2 (i+1) producers
				PR_Equal
					-> more_unused_producers3 (i+1) producers
				_
					-> False
			= False

	more_unused_producers2 i producers
		| i<size producers
			= case producers.[i] of
				PR_Empty
					-> more_unused_producers2 (i+1) producers
				PR_Unused
					-> True
			= False

	more_unused_producers3 i producers
		| i<size producers
			= case producers.[i] of
				PR_Empty
					-> more_unused_producers3 (i+1) producers
				PR_EqualRemove _
					-> more_unused_producers4 (i+1) producers
			= False

	more_unused_producers4 i producers
		| i<size producers
			= case producers.[i] of
				PR_Empty
					-> more_unused_producers4 (i+1) producers
				PR_EqualRemove _
					-> True
			= False

// get_producer_type retrieves the type of symbol
get_producer_type :: !SymbIdent !.ReadOnlyTI !*{#FunDef} !*FunctionHeap -> (!SymbolType,!*{#FunDef},!*FunctionHeap)
get_producer_type {symb_kind=SK_Function {glob_module, glob_object}} ro fun_defs fun_heap
	| glob_module == ro.ro_main_dcl_module_n
		# ({fun_type=Yes symbol_type, fun_info={fi_properties}}, fun_defs) = fun_defs![glob_object]
		|  fi_properties bitand FI_HasTypeSpec <> 0
			# (_, symbol_type) = removeAnnotations symbol_type
			= (symbol_type, fun_defs, fun_heap)
			= (symbol_type, fun_defs, fun_heap)
		# {ft_type} = ro.ro_imported_funs.[glob_module].[glob_object]
		  (_, ft_type=:{st_args,st_args_strictness}) = removeAnnotations ft_type
		  new_st_args = addTypesOfDictionaries ro.ro_common_defs ft_type.st_context st_args
		  new_st_arity = length new_st_args
		  new_st_args_strictness = insert_n_strictness_values_at_beginning (new_st_arity-length st_args) st_args_strictness
		= ({ft_type & st_args = new_st_args, st_args_strictness = new_st_args_strictness, st_arity = new_st_arity, st_context = [] }, fun_defs, fun_heap)
get_producer_type {symb_kind=SK_LocalMacroFunction glob_object} ro fun_defs fun_heap
	# ({fun_type=Yes symbol_type}, fun_defs) = fun_defs![glob_object]
	= (symbol_type, fun_defs, fun_heap)
get_producer_type {symb_kind=SK_GeneratedFunction fun_ptr _} ro fun_defs fun_heap
	# (FI_Function {gf_fun_def={fun_type=Yes symbol_type}}, fun_heap) = readPtr fun_ptr fun_heap
	= (symbol_type, fun_defs, fun_heap)
get_producer_type {symb_kind=SK_Constructor {glob_module, glob_object}} ro fun_defs fun_heap
	# cons_defs = ro.ro_common_defs.[glob_module].com_cons_defs
	# {cons_type} = cons_defs.[glob_object]
	# (_,cons_type) = removeAnnotations cons_type	// necessary???
	= (cons_type, fun_defs, fun_heap)

determine_args
	:: ![#Bool!] ![ConsClass] !Index !{!Producer} ![OptionalProducerType] ![FreeVar] !ReadOnlyTI !*DetermineArgsState
	-> *DetermineArgsState
determine_args _ [] prod_index producers prod_atypes forms _ das=:{das_var_heap}
	# (vars, das_var_heap)	= new_variables forms das_var_heap
	= {das & das_vars = vars, das_var_heap = das_var_heap}
where
	new_variables [] var_heap
		= ([], var_heap)
	new_variables [form=:{fv_ident,fv_info_ptr}:forms] var_heap
		# (vars, var_heap) = new_variables forms var_heap
		  (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
		= ([{ form & fv_info_ptr = new_info_ptr } : vars], writeVarInfo fv_info_ptr (VI_Variable fv_ident new_info_ptr) var_heap)
determine_args [#linear_bit : linear_bits!] [cons_arg : cons_args] prod_index producers [prod_atype:prod_atypes] [form : forms] input das
	# das = determine_args linear_bits cons_args (inc prod_index) producers prod_atypes forms input das
	# producer = producers.[prod_index]
	# producer
		= if (cons_arg==CActive || cons_arg==CUnusedStrict || cons_arg==CUnusedLazy)
			producer
			(case producer of
				PR_String _			-> producer
				PR_Int _			-> producer
				PR_Curried _ 0		-> producer
				PR_Equal			-> producer
				PR_EqualRemove _	-> producer
				_					-> PR_Empty)
	= determine_arg producer prod_atype form prod_index ((linear_bit,cons_arg), input) das

determine_arg
	:: !Producer OptionalProducerType !FreeVar .Int !(!(!Bool,!ConsClass),!ReadOnlyTI) !*DetermineArgsState
	-> *DetermineArgsState

determine_arg PR_Empty _ form=:{fv_ident,fv_info_ptr} _ ((linear_bit,cons_arg), _) das=:{das_var_heap}
	# (new_info_ptr, das_var_heap) = newPtr VI_Empty das_var_heap
	# das_var_heap = writeVarInfo fv_info_ptr (VI_Variable fv_ident new_info_ptr) das_var_heap
	= { das & das_vars				= [{form & fv_info_ptr = new_info_ptr} : das.das_vars]
			, das_new_linear_bits	= [#linear_bit : das.das_new_linear_bits!]
			, das_new_cons_args		= [cons_arg : das.das_new_cons_args]
			, das_var_heap			= das_var_heap }

determine_arg PR_Unused _ {fv_info_ptr} prod_index (_,ro) das=:{das_var_heap}
	# das_var_heap = writeVarInfo fv_info_ptr VI_NotUsed das_var_heap
	# no_arg_type = {ats_types = [], ats_strictness = NotStrict}
	= {das & das_arg_types.[prod_index] = no_arg_type, das_var_heap=das_var_heap}

determine_arg (PR_Class class_app free_vars_and_types class_type) _ {fv_info_ptr} prod_index (_,ro)
			  das=:{das_arg_types, das_subst, das_type_heaps, das_predef}
	# (ws_arg_type, das_arg_types) = das_arg_types![prod_index]
	# {ats_types=[arg_type:_]} = ws_arg_type
	  (_, int_class_type, das_type_heaps) = substitute class_type das_type_heaps
	  class_atype = { empty_atype & at_type = int_class_type }
	  type_input
	  		= { ti_common_defs = ro.ro_common_defs
	  		  , ti_functions = ro.ro_imported_funs
			  ,	ti_main_dcl_module_n = ro.ro_main_dcl_module_n
			  , ti_expand_newtypes = True
			  }
	// AA: Dummy generic dictionary does not unify with corresponding class dictionary.
	// Make it unify
	# ({pds_module,pds_def},das_predef) = das_predef![PD_TypeGenericDict]
	# genericGlobalIndex	= {glob_module = pds_module, glob_object = pds_def}
	# (succ, das_subst, das_type_heaps)
		= unify_dict class_atype arg_type type_input das_subst das_type_heaps
		with
			unify_dict class_atype=:{at_type=TA type_symb1 args1} arg_type=:{at_type=TA type_symb2 args2} 
				| type_symb1 == type_symb2 
					= unify class_atype arg_type
				| type_symb1.type_index == genericGlobalIndex
					= unify {class_atype & at_type = TA type_symb2 args1} arg_type	
				| type_symb2.type_index == genericGlobalIndex
					= unify class_atype {arg_type & at_type = TA type_symb1 args2} 	  				
			unify_dict class_atype arg_type 
				= unify class_atype arg_type
	| not succ
		= abort ("sanity check nr 93 in module trans failed\n"--->(class_atype,"\n", arg_type))
	# (free_vars_and_types,das_type_heaps) = mapSt subFVT free_vars_and_types das_type_heaps
		with
			subFVT (fv,ty) type_heaps
				# (_, ty`,type_heaps) = substitute ty type_heaps
				= ((fv,ty`),type_heaps)

	# ws_ats_types = [ { empty_atype & at_type = at_type } \\ (_, at_type) <- free_vars_and_types]
	# ws_arg_type` = {ats_types= ws_ats_types, ats_strictness = first_n_strict (length free_vars_and_types) }

	= {das
		& das_vars = mapAppend (\({var_info_ptr,var_ident}, _)
						-> { fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = 0 })
				  			  free_vars_and_types das.das_vars
		, das_arg_types			= {das_arg_types & [prod_index] = ws_arg_type` }
		, das_new_linear_bits	= MapAppend (\_ -> True) free_vars_and_types das.das_new_linear_bits
		, das_new_cons_args		= mapAppend (\_ -> CActive) free_vars_and_types das.das_new_cons_args
		, das_subst				= das_subst
		, das_type_heaps		= das_type_heaps
		, das_var_heap			= writeVarInfo fv_info_ptr (VI_Dictionary class_app.app_symb class_app.app_args class_type) das.das_var_heap
		, das_predef			= das_predef
		}

determine_arg (PR_String s) _ {fv_info_ptr} prod_index (_,ro) das=:{das_var_heap,das_arg_types}
	# no_arg_type = {ats_types = [], ats_strictness = NotStrict}
	  das_arg_types & [prod_index] = no_arg_type
	  das_var_heap = writeVarInfo fv_info_ptr (VI_Expression (BasicExpr (BVS s))) das_var_heap
	= {das & das_arg_types=das_arg_types, das_var_heap=das_var_heap}

determine_arg (PR_Int i) _ {fv_info_ptr} prod_index (_,ro) das=:{das_var_heap,das_arg_types}
	# no_arg_type = {ats_types = [], ats_strictness = NotStrict}
	  das_arg_types & [prod_index] = no_arg_type
	  das_var_heap = writeVarInfo fv_info_ptr (VI_Expression (BasicExpr (BVInt i))) das_var_heap
	= {das & das_arg_types=das_arg_types, das_var_heap=das_var_heap}

determine_arg PR_Equal _ form=:{fv_ident,fv_info_ptr} prod_index ((linear_bit,cons_arg), ro) das=:{das_var_heap,das_removed_equal_info_ptrs}
	# (new_info_ptr, das_var_heap) = newPtr VI_Empty das_var_heap
	  var_info = VI_Variable fv_ident new_info_ptr
	  das_var_heap = writeVarInfo fv_info_ptr var_info das_var_heap
	  das_var_heap = set_removed_equal_info_ptrs_var_info das_removed_equal_info_ptrs das_var_heap
		with
			set_removed_equal_info_ptrs_var_info [#p:ps!] var_heap
				= set_removed_equal_info_ptrs_var_info ps (writeVarInfo p var_info var_heap)
			set_removed_equal_info_ptrs_var_info [#!] var_heap
				= var_heap
	= {das & das_vars				= [{form & fv_info_ptr = new_info_ptr} : das.das_vars]
		   , das_new_linear_bits	= [#False/*linear_bit*//*?*/ : das.das_new_linear_bits!]
		   , das_new_cons_args		= [CPassive/*cons_arg*//*?*/ : das.das_new_cons_args]
		   , das_var_heap = das_var_heap}

determine_arg (PR_EqualRemove arg_index) _ form=:{fv_info_ptr} prod_index (_,ro)
		das=:{das_subst,das_arg_types,das_type_heaps,das_removed_equal_info_ptrs}
	# ([prod_type:_], das_arg_types) = das_arg_types![prod_index].ats_types
	# ([arg_type:_], das_arg_types) = das_arg_types![arg_index].ats_types
	# type_input = {ti_common_defs = ro.ro_common_defs, ti_functions = ro.ro_imported_funs, ti_main_dcl_module_n = ro.ro_main_dcl_module_n, ti_expand_newtypes = True}
	# (succ, das_subst, das_type_heaps)
	  	= unify prod_type arg_type type_input das_subst das_type_heaps
	| not succ
		| False ---> ("prod_type",prod_type,"\narg_type",arg_type) = undef
		= abort "Error in compiler: unification in module trans failed\n"
	# no_arg_type = {ats_types = [], ats_strictness = NotStrict}
	  das_arg_types & [prod_index] = no_arg_type
	  das_removed_equal_info_ptrs = [#fv_info_ptr:das_removed_equal_info_ptrs!]
	= {das & das_arg_types = das_arg_types, das_subst = das_subst, das_type_heaps = das_type_heaps, das_removed_equal_info_ptrs = das_removed_equal_info_ptrs}

determine_arg producer (ProducerType {st_args, st_args_strictness, st_result, st_attr_vars, st_context, st_attr_env, st_arity, st_vars} original_type_vars)
				{fv_info_ptr,fv_ident} prod_index ((linear_bit, _),ro)
				das=:{das_subst,das_type_heaps,das_fun_defs,das_fun_heap,das_var_heap,das_cons_args,das_arg_types,das_next_attr_nr,das_AVI_Attr_TA_TempVar_info_ptrs}

	# {th_vars, th_attrs}		= das_type_heaps
	# (symbol,symbol_arity)		= get_producer_symbol producer
	  curried					= case producer of
	  									PR_Curried _ _ -> True
	  									PR_CurriedFunction _ _ _  -> True
	  									_ -> False;
	#! size_fun_defs			= size das_fun_defs

	# ({cc_args, cc_linear_bits}, das_fun_heap, das_cons_args)
			= calc_cons_args curried symbol.symb_kind symbol_arity das_cons_args linear_bit size_fun_defs das_fun_heap
	  ({ats_types=[arg_type:_],ats_strictness}, das_arg_types) = das_arg_types![prod_index]

	  (das_next_attr_nr, th_attrs)
	  		= bind_to_temp_attr_vars st_attr_vars (das_next_attr_nr, th_attrs)
	  // remember the st_attr_vars, because the AVI_Attr (TA_TempVar _)'s must be removed before unfold,
	  // because types in Cases and Lets should not use TA_TempVar's
	  das_AVI_Attr_TA_TempVar_info_ptrs = [st_attr_vars:das_AVI_Attr_TA_TempVar_info_ptrs]
	  		// prepare for substitute calls
	  (_, st_args, das_type_heaps) = substitute st_args {das_type_heaps & th_vars = th_vars, th_attrs = th_attrs}
	  (_, st_result, das_type_heaps) = substitute st_result das_type_heaps
	  nr_of_applied_args = symbol_arity
	  (application_type, attr_env, das_next_attr_nr)
	  		= build_application_type st_arity (length st_context) st_result st_args nr_of_applied_args [] das_next_attr_nr
	  type_input
	  		= { ti_common_defs			= ro.ro_common_defs
	  		  , ti_functions			= ro.ro_imported_funs
			  ,	ti_main_dcl_module_n	= ro.ro_main_dcl_module_n
			  , ti_expand_newtypes = True }
	# (succ, das_subst, das_type_heaps)
	  		= unify application_type arg_type type_input das_subst das_type_heaps
	| not succ
		= abort "Error in compiler: unification in module trans failed\n"
	# (attr_inequalities, das_type_heaps)
			= accAttrVarHeap (mapSt substitute_attr_inequality st_attr_env) das_type_heaps
	  new_uniqueness_requirement
			= { ur_offered		= application_type
			  , ur_demanded		= arg_type
			  , ur_attr_ineqs	= attr_inequalities ++ attr_env
			  }
	  (expr_to_unfold,form_vars,das_fun_defs,das_fun_heap,das_var_heap)
		= make_producer_expression_and_args producer original_type_vars st_vars das.das_vars das_fun_defs das_fun_heap das_var_heap
/* DvA... STRICT_LET
	  (expr_to_unfold, das_var_heap, let_bindings)
			= case arg_type.at_annotation of
				AN_Strict
					# (new_info_ptr_l, das_var_heap) = newPtr VI_Empty das_var_heap
	  				# free_var_l = { fv_ident = { id_name = "free_l", id_info = nilPtr }, fv_info_ptr = new_info_ptr_l, fv_count = 0, fv_def_level = NotALevel }
			  		# act_var_l = Var { var_ident = { id_name = "act_l", id_info = nilPtr }, var_info_ptr = new_info_ptr_l, var_expr_ptr = nilPtr }

					# bind = {lb_dst = fv, lb_src = act_var_l, lb_position = NoPos}

					# das_var_heap = writeVarInfo new_info_ptr_l expr_to_unfold das_var_heap

					# let_bindings = case let_bindings of
									(s,l,st,lt) -> ([bind:s],l,[arg_type:st],lt)
					-> (VI_Empty, das_var_heap, let_bindings)
				_	-> (expr_to_unfold,das_var_heap,let_bindings)
...DvA */
	# das_arg_types = { das_arg_types & [prod_index] = {ats_types=take nr_of_applied_args st_args,ats_strictness=st_args_strictness} }
	=	{ das 
		& das_vars						= form_vars
		, das_arg_types					= das_arg_types 
		, das_next_attr_nr				= das_next_attr_nr
		, das_new_linear_bits			= cc_linear_bits ++$ das.das_new_linear_bits
		, das_new_cons_args				= cc_args ++ das.das_new_cons_args
		, das_uniqueness_requirements	= [new_uniqueness_requirement:das.das_uniqueness_requirements]
		, das_AVI_Attr_TA_TempVar_info_ptrs = das_AVI_Attr_TA_TempVar_info_ptrs
		, das_subst						= das_subst
		, das_type_heaps				= das_type_heaps
		, das_fun_defs					= das_fun_defs
		, das_fun_heap					= das_fun_heap
		, das_var_heap					= writeVarInfo fv_info_ptr expr_to_unfold das_var_heap
		, das_cons_args					= das_cons_args
		}
where
	make_producer_expression_and_args (PR_Constructor symbol=:{symb_kind=SK_Constructor {glob_module}} arity _) _ _ das_vars das_fun_defs das_fun_heap das_var_heap
		# (form_vars, act_vars, das_var_heap) = build_n_anonymous_var_args arity das_vars das_var_heap
		= (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_Curried symbol=:{symb_kind=SK_Function {glob_module}} arity) _ _ das_vars das_fun_defs das_fun_heap das_var_heap
		| glob_module <> ro.ro_main_dcl_module_n
			# (form_vars, act_vars, das_var_heap) = build_n_anonymous_var_args arity das_vars das_var_heap
			= (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_Curried symbol=:{symb_kind} arity) _ _ das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				-> (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
			_
				# (form_vars, act_vars, das_var_heap) = build_n_anonymous_var_args arity das_vars das_var_heap
				-> (VI_Expression (App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}),form_vars,das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_Function symbol=:{symb_kind} arity _) original_type_vars new_type_vars das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				-> (VI_Body symbol tb (take arity form_vars) original_type_vars new_type_vars, form_vars, das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_GeneratedFunction symbol=:{symb_kind} arity _) original_type_vars new_type_vars das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				-> (VI_Body symbol tb (take arity form_vars) original_type_vars new_type_vars, form_vars, das_fun_defs,das_fun_heap,das_var_heap)
	make_producer_expression_and_args (PR_CurriedFunction symbol=:{symb_kind} arity _) original_type_vars new_type_vars das_vars das_fun_defs das_fun_heap das_var_heap
		# ({fun_body}, das_fun_defs, das_fun_heap)
			= get_fun_def symb_kind ro.ro_main_dcl_module_n das_fun_defs das_fun_heap
		= case fun_body of
			TransformedBody tb=:{tb_args}
				# (form_vars, act_vars, das_var_heap)
					= build_n_named_var_args arity tb_args das_vars das_var_heap
				  expr = App {app_symb = symbol, app_args = act_vars, app_info_ptr = nilPtr}
				-> (VI_ExpressionOrBody expr symbol tb (take arity form_vars) original_type_vars new_type_vars, form_vars, das_fun_defs,das_fun_heap,das_var_heap)

	build_n_anonymous_var_args arity das_vars das_var_heap
 		# var_names = repeatn arity {id_name = "_x", id_info = nilPtr}
		= build_var_args (/*reverse*/ var_names) das_vars [] das_var_heap

	build_n_named_var_args arity tb_args das_vars das_var_heap
		# var_names = take arity [fv_ident \\ {fv_ident}<-tb_args]
		= build_var_args (reverse var_names) das_vars [] das_var_heap

	build_var_args [] form_vars act_vars var_heap
		= (form_vars, act_vars, var_heap)
	build_var_args [new_name:new_names] form_vars act_vars var_heap
		# (info_ptr, var_heap) = newPtr VI_Empty var_heap
		  form_var = { fv_ident = new_name, fv_info_ptr = info_ptr, fv_count = 0, fv_def_level = NotALevel }
		  act_var = { var_ident = new_name, var_info_ptr = info_ptr, var_expr_ptr = nilPtr }
		= build_var_args new_names [form_var : form_vars] [Var act_var : act_vars] var_heap

	calc_cons_args curried symb_kind symbol_arity ti_cons_args linear_bit size_fun_defs fun_heap
		# (cons_size, ti_cons_args) = usize ti_cons_args
		# (opt_cons_classes, fun_heap, ti_cons_args)
				= case symb_kind of
					SK_Function {glob_module, glob_object}
						| glob_module == ro.ro_main_dcl_module_n && glob_object < cons_size
							# (cons_args, ti_cons_args) = ti_cons_args![glob_object]
							-> (Yes cons_args, fun_heap, ti_cons_args)
						-> (No, fun_heap, ti_cons_args)
					SK_LocalMacroFunction glob_object
						| glob_object < cons_size
							# (cons_args, ti_cons_args) = ti_cons_args![glob_object]
							-> (Yes cons_args, fun_heap, ti_cons_args)
						-> (No, fun_heap, ti_cons_args)
					SK_GeneratedFunction fun_ptr fun_index
						| fun_index < cons_size
							# (cons_args, ti_cons_args) = ti_cons_args![fun_index]
							-> (Yes cons_args, fun_heap, ti_cons_args)
						| fun_index < size_fun_defs
							-> abort "sanity check failed in module trans"
						# (FI_Function {gf_cons_args}, fun_heap) = readPtr fun_ptr fun_heap
						-> (Yes gf_cons_args, fun_heap, ti_cons_args)
					SK_Constructor _
						-> (No, fun_heap, ti_cons_args)
		= case opt_cons_classes of
			Yes cons_classes
				# cc_args = copy_classes symbol_arity cons_classes.cc_args
				-> ({ cc_size			= symbol_arity
					, cc_args			= cc_args
					, cc_linear_bits	= if curried
											(RepeatnM symbol_arity linear_bit)
											(TakeM symbol_arity cons_classes.cc_linear_bits)
					, cc_producer		= False
					}
		  			, fun_heap, ti_cons_args)
			No
				-> ({ cc_size			= symbol_arity
					, cc_args			= repeatn symbol_arity CPassive
					, cc_linear_bits	= RepeatnM symbol_arity linear_bit
					, cc_producer		= False
					}
					, fun_heap, ti_cons_args)

	copy_classes 0 _ = []
	copy_classes n [cc:ccs]
		= case cc of
			CUnusedStrict	-> [CActive:copy_classes (dec n) ccs]
			CUnusedLazy		-> [CActive:copy_classes (dec n) ccs]
			cc				-> [cc:copy_classes (dec n) ccs]

/*
	build_application_type st_arity nr_context_args st_result st_args nr_of_applied_args
		| st_arity+nr_context_args==nr_of_applied_args
			= st_result
		| nr_of_applied_args<nr_context_args
			= abort "sanity check nr 234 failed in module trans"
		# (applied_args, unapplied_args) = splitAt (nr_of_applied_args-nr_context_args) st_args
		  attr_approx = if (any has_unique_attribute applied_args) TA_Unique TA_Multi
		= foldr (\atype1 atype2->{at_attribute=attr_approx, at_type=atype1-->atype2})
				st_result unapplied_args
	  where
		has_unique_attribute {at_attribute=TA_Unique} = True
		has_unique_attribute _ = False
*/
	build_application_type st_arity nr_context_args st_result st_args nr_of_applied_args attr_env attr_store
		| st_arity+nr_context_args==nr_of_applied_args
			= (st_result, attr_env, attr_store)
		| nr_of_applied_args<nr_context_args
			= abort "sanity check nr 234 failed in module trans"
		# req_arity	= nr_of_applied_args - nr_context_args

		= currySymbolType st_args st_arity st_result attr_env req_arity attr_store
/*
		# (type`,attr_env`,attr_store`)
			= currySymbolType st_args st_arity st_result attr_env req_arity attr_store
		# (applied_args, unapplied_args) = splitAt req_arity st_args
		  attr_approx = if (any has_unique_attribute applied_args) TA_Unique TA_Multi			// DvA: should be var instead of multi...
		# type = foldr (\atype1 atype2->{at_attribute=attr_approx, at_type=atype1-->atype2})
				st_result unapplied_args
		| False ---> ("build",type,type`) = undef
//		= (type, attr_env, attr_store)
		= (type`, attr_env`, attr_store`)
	  where
		has_unique_attribute {at_attribute=TA_Unique} = True
		has_unique_attribute _ = False
*/

// DvA: from type.icl...
currySymbolType tst_args tst_arity tst_result tst_attr_env req_arity ts_attr_store
	| tst_arity == req_arity
		= (tst_result, tst_attr_env, ts_attr_store)
	# (tst_args, rest_args, is_unique)			= split_args req_arity tst_args 
	| is_unique
		# (type, _, _)							= buildCurriedType rest_args tst_result TA_Unique [] 0
		= (type, tst_attr_env, ts_attr_store)
		# tst_attr_env							= build_attr_env ts_attr_store tst_args tst_attr_env
		# (type, tst_attr_env, ts_attr_store)	= buildCurriedType rest_args tst_result (TA_TempVar ts_attr_store)
		  												tst_attr_env (inc ts_attr_store)
		= (type, tst_attr_env, ts_attr_store)
where
	split_args 0 args = ([], args, False)
	split_args n [atype=:{at_attribute} : args]
		# (left, right, is_unique) = split_args (dec n) args
		= ([ atype : left ], right, is_unique || attr_is_unique at_attribute)
	
	attr_is_unique TA_Unique = True
	attr_is_unique _ = False
	
	build_attr_env cum_attr_var [] attr_env
		= attr_env
	build_attr_env cum_attr_var [{at_attribute=(TA_TempVar attr_var)} : args] attr_env
		# attr_env = [{ ac_demanded = attr_var, ac_offered = cum_attr_var } : attr_env]
		= build_attr_env cum_attr_var args attr_env
	build_attr_env cum_attr_var [_ : args] attr_env
		= build_attr_env cum_attr_var args attr_env

buildCurriedType [] type cum_attr attr_env attr_store
	= (type, attr_env, attr_store)
buildCurriedType [at=:{at_attribute}:ats] type cum_attr attr_env attr_store
	# (next_cum_attr, attr_env, attr_store) = combine_attributes at_attribute cum_attr attr_env attr_store
	  (res_type, attr_env, attr_store) = buildCurriedType ats type next_cum_attr attr_env attr_store
	= ({at_attribute = cum_attr , at_type = at --> res_type }, attr_env, attr_store)
where
	combine_attributes TA_Unique cum_attr attr_env attr_store
		= (TA_Unique, attr_env, attr_store)
	combine_attributes (TA_TempVar attr_var) (TA_TempVar cum_attr_var) attr_env attr_store
		# attr_env =
			[{ ac_demanded = cum_attr_var,ac_offered = attr_store }
			,{ ac_demanded = attr_var,ac_offered = attr_store }
			:attr_env]
		= (TA_TempVar attr_store, attr_env, inc attr_store)
	combine_attributes (TA_TempVar _) cum_attr attr_env attr_store
		= (cum_attr, attr_env, attr_store)
	combine_attributes _ (TA_TempVar cum_attr_var) attr_env attr_store
		# attr_env = [{ ac_demanded = cum_attr_var,ac_offered = attr_store }:attr_env]
		= (TA_TempVar attr_store, attr_env, inc attr_store)
	combine_attributes _ cum_attr attr_env attr_store
		= (cum_attr, attr_env, attr_store)

freshAttrVar attr_var th_attrs
	:== NewAttrVar attr_var th_attrs

RepeatnAppendM n a l :== repeatn_append_ n a l
	where
		repeatn_append_ 0 _ l = l
		repeatn_append_ n a l = [|a:repeatn_append_ (dec n) a l]

MapAppend f [|x : xs] tail
	#  x = f x
	   xs = MapAppend f xs tail
	= [|x : xs]
MapAppend f [|] tail
	= tail

//@ max_group_index

max_group_index
	:: !Int !{!Producer} Index Index *{#FunDef} *FunctionHeap *{!ConsClasses}
	-> (Index,*{!ConsClasses},*{#FunDef},*FunctionHeap)
max_group_index prod_index producers ro_main_dcl_module_n current_max fun_defs fun_heap cons_args
	| prod_index == size producers
		= (current_max, cons_args, fun_defs, fun_heap)
		# (current_max, cons_args, fun_defs, fun_heap)
			= max_group_index_of_producer producers.[prod_index] current_max fun_defs fun_heap cons_args
		= max_group_index (inc prod_index) producers ro_main_dcl_module_n current_max fun_defs fun_heap cons_args
where
	max_group_index_of_producer PR_Empty current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer PR_Unused current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Class {app_args} _ _) current_max fun_defs fun_heap cons_args
		= foldSt (foldrExprSt max_group_index_of_member) app_args (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Curried {symb_kind=SK_Function {glob_object=fun_index, glob_module}} _) current_max fun_defs fun_heap cons_args
		| glob_module<>ro_main_dcl_module_n
			= (current_max, cons_args, fun_defs, fun_heap)
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Curried {symb_kind=SK_LocalMacroFunction fun_index} _) current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Curried { symb_kind = SK_GeneratedFunction fun_ptr fun_index} _) current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs, fun_heap) = max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Function _ _ fun_index) current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_GeneratedFunction { symb_kind = SK_GeneratedFunction fun_ptr fun_index} _ _)
								current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs, fun_heap) = max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Constructor symb _ args) current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_CurriedFunction {symb_kind = SK_GeneratedFunction fun_ptr fun_index} _ _)
								current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs, fun_heap) = max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_CurriedFunction _ _ fun_index)
								current_max fun_defs fun_heap cons_args
		# (current_max, fun_defs) = max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_String _) current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_Int _) current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer PR_Equal current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_producer (PR_EqualRemove _) current_max fun_defs fun_heap cons_args
		= (current_max, cons_args, fun_defs, fun_heap)

	max_group_index_of_member
				(App {app_symb = {symb_ident, symb_kind = SK_Function { glob_object = fun_index, glob_module = mod_index}}}) 
				(current_max, cons_args, fun_defs, fun_heap)
		| mod_index == ro_main_dcl_module_n
			# (size_args, cons_args) = usize cons_args
			| fun_index < size_args
				# ({fun_info = {fi_group_index}},fun_defs) = fun_defs![fun_index]
				= (max fi_group_index current_max, cons_args, fun_defs, fun_heap)
			= (current_max, cons_args, fun_defs, fun_heap)
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_member
				(App {app_symb = {symb_ident, symb_kind = SK_LocalMacroFunction fun_index}})
				(current_max, cons_args, fun_defs, fun_heap)
		# (size_args, cons_args) = usize cons_args
		| fun_index < size_args
			# ({fun_info = {fi_group_index}}, fun_defs) = fun_defs![fun_index]
			= (max fi_group_index current_max, cons_args, fun_defs, fun_heap)
		= (current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_member
				(App {app_symb = {symb_kind = SK_GeneratedFunction fun_ptr _}})
				(current_max, cons_args, fun_defs, fun_heap)
		# (FI_Function {gf_fun_def={fun_info = {fi_group_index}}}, fun_heap) = readPtr fun_ptr fun_heap
		= (max fi_group_index current_max, cons_args, fun_defs, fun_heap)
	max_group_index_of_member _ (current_max, cons_args, fun_defs, fun_heap)
		= (current_max, cons_args, fun_defs, fun_heap)

	max_group_index_of_fun_with_fun_index fun_index current_max fun_defs
		# (fun_def,fun_defs) = fun_defs![fun_index]
		= (max fun_def.fun_info.fi_group_index current_max, fun_defs)

	max_group_index_of_fun_with_fun_index_and_ptr fun_ptr fun_index current_max fun_defs fun_heap
		# (fun_size, fun_defs)	= usize fun_defs
		| fun_index < fun_size
			# ({fun_info},fun_defs) = fun_defs![fun_index] 
			= (max fun_info.fi_group_index current_max, fun_defs, fun_heap)
			# (FI_Function generated_function, fun_heap) = readPtr fun_ptr fun_heap
			= (max generated_function.gf_fun_def.fun_info.fi_group_index current_max, fun_defs, fun_heap)

class replaceIntegers a :: !a !({!TypeVar}, !{!TypeAttribute}, !AttributePartition) !*{#Bool} -> (!Bool, !a, !*{#Bool})
	// get rid of all those TempV and TA_Var things

instance replaceIntegers [a] | replaceIntegers a where
	replaceIntegers l=:[h:t] input used
		# (h_m, h_r, used) = replaceIntegers h input used
		  (t_m, t_r, used) = replaceIntegers t input used
		| h_m
			| t_m
				= (True, [h_r:t_r], used)
				= (True, [h_r:t], used)
			| t_m
				= (True, [h:t_r], used)
				= (False, l, used)
	replaceIntegers [] input used
		= (False, [], used)

instance replaceIntegers TypeAttribute where
	replaceIntegers (TA_TempVar i) (_, attributes, attr_partition) used
		# index = attr_partition.[i]
		  attribute = attributes.[index]
		= case attribute of
			TA_Var _
				-> (True, attribute, {used & [index] = True})
			_
				-> (True, attribute, used)
	replaceIntegers ta _ used
		= (False, ta, used)

instance replaceIntegers Type where
	replaceIntegers type=:(TA type_symb_ident args) input used
		# (args_m, args_r, used) = replaceIntegers args input used
		| args_m
			= (True, TA type_symb_ident args_r, used)
			= (False, type, used)
	replaceIntegers type=:(TAS type_symb_ident args strictness) input used
		# (args_m, args_r, used) = replaceIntegers args input used
		| args_m
			= (True, TAS type_symb_ident args_r strictness, used)
			= (False, type, used)
	replaceIntegers type=:(a --> b) input used
		# (a_m, a_r, used) = replaceIntegers a input used
		  (b_m, b_r, used) = replaceIntegers b input used
		| a_m
			| b_m
				= (True, a_r --> b_r, used)
				= (True, a_r --> b, used)
			| b_m
				= (True, a --> b_r, used)
				= (False, type, used)
	replaceIntegers (consvar :@: args) input=:(fresh_type_vars, _, _) used
		# (TempCV i) = consvar
		  (_, args, used) = replaceIntegers args input used
		= (True, CV fresh_type_vars.[i] :@: args, used)
	replaceIntegers (TempV i) (fresh_type_vars, _, _) used
		= (True, TV fresh_type_vars.[i], used)
	replaceIntegers type input used
		= (False, type, used)

instance replaceIntegers AType where
	replaceIntegers atype=:{at_attribute, at_type} input used
		# (at_attribute_m, at_attribute_r, used) = replaceIntegers at_attribute input used
		  (at_type_m, at_type_r, used) = replaceIntegers at_type input used
		| at_attribute_m
			| at_type_m
				= (True, {atype & at_attribute = at_attribute_r, at_type = at_type_r}, used)
				= (True, {atype & at_attribute = at_attribute_r}, used)
			| at_type_m
				= (True, {atype & at_type = at_type_r}, used)
				= (False, atype, used)

// Variable binding...

bind_to_fresh_expr_var {fv_ident, fv_info_ptr} var_heap
	# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
	  form_var = { fv_ident = fv_ident, fv_info_ptr = new_info_ptr, fv_count = undeff, fv_def_level = NotALevel }
	  act_var = { var_ident = fv_ident, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr }
	= (form_var, writeVarInfo fv_info_ptr (VI_Expression (Var act_var)) var_heap)

remove_VI_Expression_values [{fv_info_ptr}:args] var_heap
	= remove_VI_Expression_values args (writeVarInfo fv_info_ptr VI_Empty var_heap)
remove_VI_Expression_values [] var_heap
	= var_heap 

bind_to_fresh_type_variables type_variables th_vars
	= mapSt bind_to_fresh_type_variable type_variables th_vars
where
	bind_to_fresh_type_variable {tv_ident, tv_info_ptr} th_vars
		# (new_tv_info_ptr, th_vars) = newPtr TVI_Empty th_vars
		  tv = {tv_ident=tv_ident, tv_info_ptr=new_tv_info_ptr}
		= (tv, writePtr tv_info_ptr (TVI_Type (TV tv)) th_vars)

remove_TVI_Type_values [{tv_info_ptr}:type_vars] type_var_heap
	= remove_TVI_Type_values type_vars (writePtr tv_info_ptr TVI_Empty type_var_heap)
remove_TVI_Type_values [] type_var_heap
	= type_var_heap

bind_to_fresh_attr_variable {av_ident, av_info_ptr} th_attrs
	# (new_av_info_ptr, th_attrs) = newPtr AVI_Empty th_attrs
	  av = { av_ident=av_ident, av_info_ptr=new_av_info_ptr }
	= (av, writePtr av_info_ptr (AVI_Attr (TA_Var av)) th_attrs)

remove_AVI_Attr_values [{av_info_ptr}:st_attr_vars] th_attrs
	# th_attrs = writePtr av_info_ptr AVI_Empty th_attrs
	= remove_AVI_Attr_values st_attr_vars th_attrs
remove_AVI_Attr_values [] th_attrs
	= th_attrs

bind_to_temp_type_var {tv_info_ptr} (next_type_var_nr, th_vars)
	= (next_type_var_nr+1, writePtr tv_info_ptr (TVI_Type (TempV next_type_var_nr)) th_vars)

bind_to_temp_attr_vars :: [AttributeVar] *(Int,*AttrVarHeap) -> (!Int,!*AttrVarHeap)
bind_to_temp_attr_vars attr_vars next_attr_var_n_and_attrs
	= foldSt bind_to_temp_attr_var attr_vars next_attr_var_n_and_attrs
where
	bind_to_temp_attr_var {av_info_ptr} (next_attr_var_nr, th_attrs)
		= (next_attr_var_nr+1, writePtr av_info_ptr (AVI_Attr (TA_TempVar next_attr_var_nr)) th_attrs)

remove_TA_TempVars_in_info_ptrs [hAVI_Attr_TA_TempVar_info_ptrs:tAVI_Attr_TA_TempVar_info_ptrs] attrs
	# attrs = remove_TA_TempVars_in_info_ptr_list hAVI_Attr_TA_TempVar_info_ptrs attrs
	= remove_TA_TempVars_in_info_ptrs tAVI_Attr_TA_TempVar_info_ptrs attrs
where
	remove_TA_TempVars_in_info_ptr_list [{av_info_ptr}:tAVI_Attr_TA_TempVar_info_ptrs] attrs
		= case readPtr av_info_ptr attrs of
			(AVI_Attr (TA_TempVar _),attrs)
				// use TA_Multi as in cleanUpTypeAttribute
				# attrs = writePtr av_info_ptr (AVI_Attr TA_Multi) attrs
				-> remove_TA_TempVars_in_info_ptr_list tAVI_Attr_TA_TempVar_info_ptrs attrs
			(_,attrs)
				-> remove_TA_TempVars_in_info_ptr_list tAVI_Attr_TA_TempVar_info_ptrs attrs
	remove_TA_TempVars_in_info_ptr_list [] attrs
		= attrs
remove_TA_TempVars_in_info_ptrs [] attrs
	= attrs

transformFunctionApplication :: !FunDef !InstanceInfo !ConsClasses !App ![Expression] !ReadOnlyTI !*TransformInfo -> *(!Expression,!*TransformInfo)
transformFunctionApplication fun_def instances cc=:{cc_size, cc_args, cc_linear_bits} app=:{app_symb,app_args} extra_args ro ti
	# (app_args, extra_args) = complete_application fun_def.fun_arity app_args extra_args
//	| False ---> ("transformFunctionApplication",app_symb,app_args,extra_args,fun_def.fun_arity,cc_size) = undef
	| expanding_consumer
	 	= (build_application { app & app_args = app_args } extra_args, ti)
	# {fun_body=TransformedBody {tb_rhs}, fun_kind} = fun_def
	| cc_size == 0
		| SwitchTransformConstants (ro.ro_transform_fusion>=FullFusion && is_not_caf fun_kind && is_sexy_body tb_rhs) False
			= transform_trivial_function app app_args extra_args ro ti
		= (build_application { app & app_args = app_args } extra_args, ti)
	# (opt_expr,ti) = is_trivial_function app_symb app_args fun_kind tb_rhs ro ti
	| case opt_expr of No -> False; Yes _ -> True
		= case opt_expr of
			Yes (App app)
				-> transformApplication app extra_args ro ti
			Yes rhs
				| isEmpty extra_args
					-> (rhs, ti)
					-> (rhs @ extra_args, ti)
	| cc_size >= 0
		# consumer_properties = fun_def.fun_info.fi_properties
	  	# consumer_is_curried = cc_size <> length app_args
		# non_rec_consumer = consumer_properties bitand FI_IsNonRecursive <> 0
		# safe_args
			= isEmpty [arg \\ arg <- app_args & cc_arg <- cc_args | unsafe cc_arg && non_var arg]
						with
							unsafe CAccumulating			= True
							unsafe CVarOfMultimatchCase		= True
							unsafe _						= False
							
							non_var (Var _)					= False
							non_var _						= True
	  	# ok_non_rec_consumer	= non_rec_consumer && safe_args
	  	#! (producers, new_args, strict_let_binds, ti)
			= determineProducers consumer_properties consumer_is_curried ok_non_rec_consumer fun_def.fun_type cc_linear_bits cc_args app_args 0 (createArray cc_size PR_Empty) ro ti
		#! (arity_changed,new_args,extra_args,producers,cc_args,cc_linear_bits,fun_def,n_extra,ti)
			= determineCurriedProducersInExtraArgs new_args extra_args consumer_properties producers cc_args cc_linear_bits fun_def ro ti
		| containsProducer cc_size producers || arity_changed
	  		# (is_new, fun_def_ptr, instances, ti_fun_heap) = tryToFindInstance producers instances ti.ti_fun_heap
	  		| is_new
				# ti							= update_instance_info app_symb.symb_kind instances { ti & ti_fun_heap = ti_fun_heap }
	  			# (fun_index, fun_arity, ti)	= generateFunction app_symb fun_def cc_args cc_linear_bits producers fun_def_ptr ro n_extra ti
				| fun_index == (-1)
					= (build_application { app & app_args = app_args } extra_args, ti) // ---> ("failed instance")
	  			# app_symb = { app_symb & symb_kind = SK_GeneratedFunction fun_def_ptr fun_index }
				# (app_args, extra_args) = complete_application fun_arity new_args extra_args
	  			
//	  			# (FI_Function {gf_fun_def},ti_fun_heap) = readPtr fun_def_ptr ti.ti_fun_heap
//	  			# ti = {ti & ti_fun_heap = ti_fun_heap} ---> ("generated",fun_def_ptr,gf_fun_def)
	  			
	  			# (expr,ti) = transformApplication { app & app_symb = app_symb, app_args = app_args } extra_args ro ti
	  			= possiblyAddStrictLetBinds expr strict_let_binds ti
			# (FI_Function gf=:{gf_fun_index, gf_fun_def}, ti_fun_heap) = readPtr fun_def_ptr ti_fun_heap
			# ti = {ti & ti_fun_heap = ti_fun_heap}
			| gf_fun_index == (-1)
				= (build_application { app & app_args = app_args } extra_args, ti) // ---> ("known failed instance")
			# app_symb` = { app_symb & symb_kind = SK_GeneratedFunction fun_def_ptr gf_fun_index }
			  (app_args, extra_args) = complete_application gf_fun_def.fun_arity new_args extra_args
			| gf_fun_def.fun_info.fi_properties bitand FI_Unused<>0
				# {fi_properties,fi_calls} = gf_fun_def.fun_info
				  gf & gf_fun_def.fun_info.fi_properties = (fi_properties bitxor FI_Unused) bitor FI_UnusedUsed
				  ti & ti_fun_heap = writePtr fun_def_ptr (FI_Function gf) ti.ti_fun_heap,
					   ti_new_functions = [fun_def_ptr : ti.ti_new_functions]
				  ti = add_unused_calls fi_calls ti
				  (expr,ti) = transformApplication {app & app_symb = app_symb`, app_args = app_args} extra_args ro ti
				= possiblyAddStrictLetBinds expr strict_let_binds ti
				# (expr,ti) = transformApplication {app & app_symb = app_symb`, app_args = app_args} extra_args ro ti
				= possiblyAddStrictLetBinds expr strict_let_binds ti
		| SwitchTrivialFusion (ro.ro_transform_fusion>=FullFusion) False
			= transform_trivial_function app app_args extra_args ro ti
		= (build_application { app & app_args = app_args } extra_args, ti)
	= (build_application { app & app_args = app_args } extra_args, ti)
where
	expanding_consumer = case fun_def.fun_body of
								Expanding _	-> True
								_			-> False

	is_not_caf FK_Caf	= False
	is_not_caf _		= True

	possiblyAddStrictLetBinds expr strict_lets ti
		# (strict_let_binds,let_type) = unzip strict_lets
		= case strict_let_binds of
			[]	-> (expr,ti)
			_
				# (new_info_ptr, ti_symbol_heap) = newPtr (EI_LetType let_type) ti.ti_symbol_heap
				  ti = {ti & ti_symbol_heap = ti_symbol_heap}
				-> (Let 	{	let_strict_binds	= strict_let_binds
							,	let_lazy_binds		= []
							,	let_expr			= expr
							,	let_info_ptr		= new_info_ptr
							,	let_expr_position	= NoPos
							},ti) // ---> "added strict_let_binds"

	transform_trivial_function :: !.App ![.Expression] ![.Expression] !.ReadOnlyTI !*TransformInfo -> *(!Expression,!*TransformInfo)
	transform_trivial_function app=:{app_symb} app_args extra_args ro ti
		# (opt_expr,ti) = is_trivial_function_call app_symb.symb_kind app_args ro ti
		= case opt_expr of
			No
				-> (build_application {app & app_symb = app_symb, app_args = app_args} extra_args, ti)
			Yes tb_rhs=:(App app)
				# (is_cycle,ti) = is_cycle_of_trivial_function_calls app.app_symb.symb_kind app_args [app_symb.symb_kind] ro ti
				| not is_cycle
					-> transformApplication app extra_args ro ti
					| isEmpty extra_args
						-> (tb_rhs, ti)
						-> (tb_rhs @ extra_args, ti)
			Yes tb_rhs
				| isEmpty extra_args
					-> (tb_rhs, ti)
					-> (tb_rhs @ extra_args, ti)

	is_cycle_of_trivial_function_calls :: !SymbKind ![Expression] ![SymbKind] !ReadOnlyTI !*TransformInfo -> *(!Bool,!*TransformInfo)
	is_cycle_of_trivial_function_calls symb_kind app_args previous_function_symb_kinds ro ti
		| not (is_main_module_function_symbol symb_kind ro.ro_main_dcl_module_n)
			= (False,ti)
		| Any (equal_function symb_kind) previous_function_symb_kinds
			= (True,ti)
		# (opt_expr,ti) = is_trivial_function_call symb_kind app_args ro ti
		= case opt_expr of
			Yes (App {app_symb,app_args})
				-> is_cycle_of_trivial_function_calls app_symb.symb_kind app_args [symb_kind:previous_function_symb_kinds] ro ti
			_
				-> (False,ti)
	where
		is_main_module_function_symbol (SK_Function {glob_module})	main_dcl_module_n = glob_module == main_dcl_module_n
		is_main_module_function_symbol (SK_LocalMacroFunction _)	main_dcl_module_n = True
		is_main_module_function_symbol (SK_GeneratedFunction _ _)	main_dcl_module_n = True
		is_main_module_function_symbol _							main_dcl_module_n = False

		equal_function (SK_Function i1) (SK_Function i2) = i1==i2
		equal_function (SK_LocalMacroFunction i1) (SK_LocalMacroFunction i2) = i1==i2
		equal_function (SK_GeneratedFunction _ i1) (SK_GeneratedFunction _ i2) = i1==i2
		equal_function _ _ = False

	is_trivial_function :: !SymbIdent ![Expression] !FunKind !Expression !ReadOnlyTI !*TransformInfo -> *(!Optional Expression,!*TransformInfo)
	is_trivial_function app_symb app_args fun_kind rhs ro ti
		| SwitchTransformConstants (ro.ro_transform_fusion>=FullFusion && is_not_caf fun_kind && is_sexy_body rhs) False
			= is_trivial_function_call app_symb.symb_kind app_args ro ti
			= (No, ti)

	is_trivial_function_call :: !SymbKind ![Expression] !ReadOnlyTI !*TransformInfo -> *(!Optional Expression,!*TransformInfo)
	is_trivial_function_call symb_kind app_args ro ti
		# (fun_def,ti_fun_defs,ti_fun_heap) = get_fun_def symb_kind ro.ro_main_dcl_module_n ti.ti_fun_defs ti.ti_fun_heap
		# {fun_body=fun_body=:TransformedBody {tb_args,tb_rhs},fun_type} = fun_def
		# (opt_expr, ti_fun_defs, ti_fun_heap, ti_type_heaps, ti_cons_args)
			= is_trivial_body tb_args tb_rhs app_args fun_type ro ti_fun_defs ti_fun_heap ti.ti_type_heaps ti.ti_cons_args
		# ti & ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap, ti_type_heaps = ti_type_heaps, ti_cons_args = ti_cons_args
		= (opt_expr, ti)

	update_instance_info :: !.SymbKind !.InstanceInfo !*TransformInfo -> *TransformInfo
	update_instance_info (SK_Function {glob_object}) instances ti=:{ti_instances}
		 = { ti & ti_instances = { ti_instances & [glob_object] = instances } }
	update_instance_info (SK_LocalMacroFunction glob_object) instances ti=:{ti_instances}
		 = { ti & ti_instances = { ti_instances & [glob_object] = instances } }
	update_instance_info (SK_GeneratedFunction fun_def_ptr fun_index) instances ti=:{ti_fun_heap, ti_instances}
		| fun_index < size ti_instances
			= { ti & ti_instances = { ti_instances & [fun_index] = instances } }
		# (FI_Function fun_info, ti_fun_heap) = readPtr fun_def_ptr ti_fun_heap
		= { ti & ti_fun_heap = ti_fun_heap <:= (fun_def_ptr, FI_Function { fun_info & gf_instance_info = instances })}

	complete_application :: !.Int !.[Expression] !.[Expression] -> (!.[Expression],![Expression])
	complete_application form_arity args extra_args
		= (take form_arity all_args,drop form_arity all_args)
	where
		all_args = args ++ extra_args

	build_application :: !.App ![.Expression] -> Expression
	build_application app []
		= App app
	build_application app extra_args
		= App app @ extra_args

	add_unused_calls [GeneratedFunCall _ fun_def_ptr:calls] ti=:{ti_fun_heap}
		# (FI_Function gf, ti_fun_heap) = readPtr fun_def_ptr ti_fun_heap
		  ti & ti_fun_heap=ti_fun_heap
		  {fi_properties,fi_calls} = gf.gf_fun_def.fun_info
		| fi_properties bitand FI_Unused<>0
			# gf & gf_fun_def.fun_info.fi_properties = (fi_properties bitxor FI_Unused) bitor FI_UnusedUsed
			  ti & ti_fun_heap = writePtr fun_def_ptr (FI_Function gf) ti.ti_fun_heap,
				   ti_new_functions = [fun_def_ptr : ti.ti_new_functions]
			  ti = add_unused_calls fi_calls ti
			= add_unused_calls calls ti
			= add_unused_calls calls ti
	add_unused_calls [_:calls] ti
		= add_unused_calls calls ti
	add_unused_calls [] ti
		= ti

is_cons_or_decons_of_UList_or_UTSList glob_object glob_module imported_funs
	:== let  type = imported_funs.[glob_module].[glob_object].ft_type;
		  in type.st_arity>0 && not (isEmpty type.st_context);

determineCurriedProducersInExtraArgs :: ![Expression] ![Expression] !BITVECT !{!.Producer} ![Int] ![#Bool!] !FunDef !ReadOnlyTI !*TransformInfo
	-> *(!Bool,![Expression],![Expression],!{!Producer},![Int],![#Bool!],!FunDef,!Int,!*TransformInfo)
determineCurriedProducersInExtraArgs new_args [] consumer_properties producers cc_args cc_linear_bits fun_def ro ti
	= (False,new_args,[],producers,cc_args,cc_linear_bits,fun_def,0,ti)
determineCurriedProducersInExtraArgs new_args extra_args consumer_properties producers cc_args cc_linear_bits fun_def ro ti
	| not (SwitchExtraCurriedFusion (ro.ro_transform_fusion>=FullFusion) consumer_properties)
		= (False,new_args,extra_args,producers,cc_args,cc_linear_bits,fun_def,0,ti)
	# n_extra_args													= length extra_args
	# {fun_type = Yes symbol_type=:{st_args,st_result,st_arity}}	= fun_def
	# (ok,new_args_types,new_result_type)							= get_new_args_types_from_result_type st_result n_extra_args
	| not ok
		= (False,new_args,extra_args,producers,cc_args,cc_linear_bits,fun_def,0,ti)
	# symbol_type	= {symbol_type & st_result=new_result_type,st_args=st_args++new_args_types,st_arity=st_arity+n_extra_args}
	# fun_def							= {fun_def & fun_type=Yes symbol_type}
	# (form_args,var_heap)				= create_new_args n_extra_args ti.ti_var_heap
	# ti								= {ti & ti_var_heap=var_heap}
	# fun_def							= case fun_def.fun_body of
											TransformedBody tb
												-> {fun_def & fun_body=TransformedBody 
													{tb & tb_args = add_args_to_fun_args form_args tb.tb_args
													}}
	# new_producers						= arrayPlusList producers [PR_Empty \\ i<-[0..n_extra_args-1]]
	# new_cc_args						= cc_args ++ [CPassive \\ i<-[0..n_extra_args-1]]
	# new_cc_linear_bits				= cc_linear_bits ++$ [#True \\ i<-[0..n_extra_args-1]!]
	= (True,new_args++extra_args,[],new_producers,new_cc_args,new_cc_linear_bits,fun_def,n_extra_args,ti)
where
	get_new_args_types_from_result_type type 0
		= (True,[],type)
	get_new_args_types_from_result_type {at_type=a-->b} n
		# (ok,args_types,result_type) = get_new_args_types_from_result_type b (n-1)
		= (ok,[a:args_types],result_type)
	get_new_args_types_from_result_type type _
		= (False,[],type)

	create_new_args n_new_args var_heap
		| n_new_args==0
			= ([], var_heap)            
		# new_name				= { id_name = "_a", id_info = nilPtr }
		  (info_ptr, var_heap)	= newPtr VI_Empty var_heap
		  form_var				= { fv_ident = new_name, fv_info_ptr = info_ptr, fv_count = 0, fv_def_level = NotALevel }
		  (form_vars,var_heap)	= create_new_args (n_new_args-1) var_heap
		= ([form_var : form_vars],var_heap)

add_args_to_fun_args form_args tb_args
	= tb_args ++ form_args

add_args_to_fun_body act_args new_result_type tb_rhs ro ti
		= add_arguments tb_rhs act_args new_result_type ro ti
where
	add_arguments (App app=:{app_symb,app_args}) extra_args new_result_type ro ti
		# (form_arity,fun_defs,fun_heap) = get_arity app_symb ro ti.ti_fun_defs ti.ti_fun_heap
		# ti = {ti & ti_fun_defs=fun_defs,ti_fun_heap=fun_heap}
		# ar_diff = form_arity - length app_args
		| length extra_args <= ar_diff
			= (App {app & app_args = app_args ++ extra_args }, ti)
			= (App {app & app_args = app_args ++ take ar_diff extra_args } @ drop ar_diff extra_args, ti)
	add_arguments (Case kees=:{case_guards,case_default,case_info_ptr}) extra_args new_result_type ro ti
		# (case_default, ti)	= add_arguments_opt case_default extra_args new_result_type ro ti
		# (case_guards, ti)		= add_arguments_guards case_guards extra_args new_result_type ro ti
		# ti_symbol_heap		= overwrite_result_type case_info_ptr new_result_type ti.ti_symbol_heap
		# ti					= {ti & ti_symbol_heap = ti_symbol_heap}
		= (Case {kees & case_guards = case_guards, case_default = case_default}, ti)
	where
		overwrite_result_type case_info_ptr new_result_type ti_symbol_heap
			#! (EI_CaseType case_type, ti_symbol_heap)	= readExprInfo case_info_ptr ti_symbol_heap
			= writeExprInfo case_info_ptr (EI_CaseType { case_type & ct_result_type = new_result_type}) ti_symbol_heap
	add_arguments (Let lad=:{let_expr}) extra_args new_result_type ro ti
		# (let_expr, ti)		= add_arguments let_expr extra_args new_result_type ro ti
		= (Let {lad & let_expr = let_expr}, ti)
	add_arguments (expr1 @ expr2) extra_args _ ro ti
		= (expr1 @ (expr2++extra_args),ti)
	add_arguments expr extra_args _ ro ti
		= (expr @ extra_args,ti) // ---> ("????",expr)
	
	add_arguments_opt No _ _ ro ti = (No,ti)
	add_arguments_opt (Yes expr) extra_args new_result_type ro ti
		# (expr, ti)	= add_arguments expr extra_args new_result_type ro ti
		= (Yes expr,ti)
	
	add_arguments_guards (AlgebraicPatterns gindex apats) extra_args new_result_type ro ti
		# (apats, ti)	= add_arguments_apats apats extra_args new_result_type ro ti
		= (AlgebraicPatterns gindex apats, ti)
	add_arguments_guards (BasicPatterns btype bpats) extra_args new_result_type ro ti
		# (bpats, ti)	= add_arguments_bpats bpats extra_args new_result_type ro ti
		= (BasicPatterns btype bpats, ti)
	add_arguments_guards (DynamicPatterns dpats) extra_args new_result_type ro ti
		# (dpats, ti)	= add_arguments_dpats dpats extra_args new_result_type ro ti
		= (DynamicPatterns dpats, ti)
	add_arguments_guards (OverloadedListPatterns type decons_expr apats) extra_args new_result_type ro ti
		# (apats, ti)	= add_arguments_apats apats extra_args new_result_type ro ti
		= (OverloadedListPatterns type decons_expr apats, ti)
	add_arguments_guards NoPattern extra_args _ ro ti
		= (NoPattern, ti)
	
	add_arguments_apats [] extra_args _ ro ti = ([],ti)
	add_arguments_apats [ap=:{ap_expr}:aps] extra_args new_result_type ro ti
		# (ap_expr, ti)	= add_arguments ap_expr extra_args new_result_type ro ti
		# (aps, ti)		= add_arguments_apats aps extra_args new_result_type ro ti
		= ([{ap & ap_expr = ap_expr}:aps],ti)

	add_arguments_bpats [] extra_args _ ro ti = ([],ti)
	add_arguments_bpats [bp=:{bp_expr}:bps] extra_args new_result_type ro ti
		# (bp_expr, ti)	= add_arguments bp_expr extra_args new_result_type ro ti
		# (bps, ti)		= add_arguments_bpats bps extra_args new_result_type ro ti
		= ([{bp & bp_expr = bp_expr}:bps],ti)

	add_arguments_dpats [] extra_args _ ro ti = ([],ti)
	add_arguments_dpats [dp=:{dp_rhs}:dps] extra_args new_result_type ro ti
		# (dp_rhs, ti)	= add_arguments dp_rhs extra_args new_result_type ro ti
		# (dps, ti)	= add_arguments_dpats dps extra_args new_result_type ro ti
		= ([{dp & dp_rhs = dp_rhs}:dps],ti)

	get_arity {symb_kind=SK_Function {glob_module, glob_object}} ro fun_defs fun_heap
		| glob_module == ro.ro_main_dcl_module_n
			# (fun_arity, fun_defs) = fun_defs![glob_object].fun_arity
			= (fun_arity, fun_defs, fun_heap)
		# {ft_arity,ft_type} = ro.ro_imported_funs.[glob_module].[glob_object]
		= (ft_arity + length ft_type.st_context, fun_defs, fun_heap)
	get_arity {symb_kind=SK_LocalMacroFunction glob_object} ro fun_defs fun_heap
		# (fun_arity, fun_defs) = fun_defs![glob_object].fun_arity
		= (fun_arity, fun_defs, fun_heap)
	get_arity {symb_kind=SK_GeneratedFunction fun_ptr _} ro fun_defs fun_heap
		# (FI_Function {gf_fun_def={fun_arity}}, fun_heap) = readPtr fun_ptr fun_heap
		= (fun_arity, fun_defs, fun_heap)
	get_arity {symb_kind=SK_Constructor {glob_module, glob_object}} ro fun_defs fun_heap
		# arity = ro.ro_common_defs.[glob_module].com_cons_defs.[glob_object].cons_type.st_arity
		= (arity, fun_defs, fun_heap)

is_trivial_body :: ![FreeVar] !Expression ![Expression] !(Optional SymbolType) !.ReadOnlyTI
							 !*{#FunDef} !*FunctionHeap !*TypeHeaps !*{!ConsClasses}
	-> (!Optional Expression,!*{#FunDef},!*FunctionHeap,!*TypeHeaps,!*{!ConsClasses})
is_trivial_body [fv] (Var bv) [arg] type ro fun_defs fun_heap type_heaps cons_args
	| fv.fv_info_ptr == bv.var_info_ptr
		= (Yes arg, fun_defs, fun_heap, type_heaps, cons_args)
		= (No, fun_defs, fun_heap, type_heaps, cons_args)
is_trivial_body lhs_args (App app) f_args type ro fun_defs fun_heap type_heaps cons_args
	| not (is_safe_producer app.app_symb.symb_kind ro fun_heap cons_args)
		= (No,fun_defs,fun_heap,type_heaps,cons_args)
	# (type`,fun_defs,fun_heap)	= get_producer_type app.app_symb ro fun_defs fun_heap
	  lhs_args_var_ptrs = {!fv_info_ptr \\ {fv_info_ptr} <- lhs_args}
	  n_f_args = length f_args
	  optional_perm = match_args lhs_args app.app_args 0 n_f_args lhs_args_var_ptrs []
	= case optional_perm of
		Yes perm
			# (match, type_heaps) = match_types type type` perm ro.ro_common_defs type_heaps
			| match
				# f_args = permute_args f_args perm n_f_args
				-> (Yes (App {app & app_args = f_args}),fun_defs,fun_heap,type_heaps,cons_args)
				-> (No,fun_defs,fun_heap,type_heaps,cons_args)
		_
			-> (No,fun_defs,fun_heap,type_heaps,cons_args)
where
	match_args :: ![FreeVar] ![Expression] !Int !Int !*{!VarInfoPtr} ![Int] -> Optional [Int]
	match_args [fv:fvs] [Var bv:bvs] arg_n n_f_args lhs_args_var_ptrs reversed_perm
		| arg_n<n_f_args
			# (index,lhs_args_var_ptrs) = lookup_lhs_arg_n 0 bv.var_info_ptr lhs_args_var_ptrs
			| index<n_f_args
				= match_args fvs bvs (arg_n+1) n_f_args lhs_args_var_ptrs [index:reversed_perm]
				= No
		| fv.fv_info_ptr==bv.var_info_ptr 
			= match_args fvs bvs (arg_n+1) n_f_args lhs_args_var_ptrs [arg_n:reversed_perm]
			= No
	match_args [] [] arg_n n_f_args _ reversed_perm
		| arg_n==n_f_args
			= Yes (reverse reversed_perm)
	match_args _ _ _ _ _ _
		= No

	lookup_lhs_arg_n :: !Int !VarInfoPtr !*{!VarInfoPtr} -> (!Int,!*{!VarInfoPtr})
	lookup_lhs_arg_n i x a
		| i<size a
			# ai=a.[i]
			| isNilPtr ai || x<>ai
				= lookup_lhs_arg_n (i+1) x a
				# a & [i] = nilPtr
				= (i,a)
			= (i,a)

	// check if strict values in type are also strict in type`
	match_types :: !(Optional SymbolType) SymbolType ![Int] !{#CommonDefs} !*TypeHeaps -> (!Bool,!*TypeHeaps)
	match_types No type` perm common_defs type_heaps
		= (True,type_heaps)
	match_types (Yes type) type` perm common_defs type_heaps
		| is_not_strict type.st_args_strictness 
			= match_tuple_strictness type.st_result type`.st_result common_defs type_heaps
		| type.st_arity<>type`.st_arity
			= (False,type_heaps)
		# (args_strictness_ok,type_heaps)
			= match_args_strictness 0 type.st_arity type.st_args_strictness type`.st_args_strictness perm type.st_args type`.st_args common_defs type_heaps
		| not args_strictness_ok
			= (False,type_heaps)
			= match_tuple_strictness type.st_result type`.st_result common_defs type_heaps
	where
		match_args_strictness :: !Int !Int !StrictnessList !StrictnessList ![Int] ![AType] ![AType] !{#CommonDefs} !*TypeHeaps -> (!Bool,!*TypeHeaps)
		match_args_strictness arg_n arity s1 s2 perm arg_types1 arg_types2 common_defs type_heaps
			| arg_n<arity
				# lhs_arg_n = perm!!arg_n
				| not (arg_is_strict lhs_arg_n s1)
					= match_args_strictness (arg_n+1) arity s1 s2 perm arg_types1 arg_types2 common_defs type_heaps
				| not (arg_is_strict arg_n s2)
					= (False,type_heaps)
				# (tuple_strictness_ok,type_heaps) = match_tuple_strictness (arg_types1!!lhs_arg_n) (arg_types2!!arg_n) common_defs type_heaps
				| not tuple_strictness_ok
					= (False,type_heaps)
					= match_args_strictness (arg_n+1) arity s1 s2 perm arg_types1 arg_types2 common_defs type_heaps
				= (True,type_heaps)

		match_tuple_strictness :: !AType AType !{#CommonDefs} !*TypeHeaps -> (!Bool,!*TypeHeaps)
		match_tuple_strictness {at_type=TAS _ args1 strictness1} {at_type=TAS _ args2 strictness2} common_defs type_heaps
			| not (more_or_equal_strictness_lists strictness2 strictness1)
				= (False,type_heaps)
				= match_tuple_args_strictness 0 args1 args2 strictness1 strictness2 common_defs type_heaps
		match_tuple_strictness atype1=:{at_type=TAS _ args1 strictness1} {at_attribute,at_type=type2=:TA _ _} common_defs type_heaps
			| is_not_strict strictness1
				= (True,type_heaps)
			# (ok,type2,type_heaps)	= tryToExpand type2 at_attribute common_defs type_heaps
			|  ok
				= match_tuple_strictness atype1 {at_attribute=at_attribute,at_type=type2} common_defs type_heaps
				= (False,type_heaps)
		match_tuple_strictness {at_attribute,at_type=type1=:TA _ _} atype2=:{at_type=TAS _ _ _} common_defs type_heaps
			# (ok,type1,type_heaps)	= tryToExpand type1 at_attribute common_defs type_heaps
			| ok
				= match_tuple_strictness {at_attribute=at_attribute,at_type=type1} atype2 common_defs type_heaps
				= (True,type_heaps)
		match_tuple_strictness {at_attribute,at_type=type1=:TA _ _} atype2=:{at_type=TA _ _} common_defs type_heaps
			# (ok,type1,type_heaps)	= tryToExpand type1 at_attribute common_defs type_heaps
			| ok
				= match_tuple_strictness {at_attribute=at_attribute,at_type=type1} atype2 common_defs type_heaps
				= (True,type_heaps)
		match_tuple_strictness arg_type1 arg_type2 common_defs type_heaps
			= (True,type_heaps)

		match_tuple_args_strictness :: !Int ![AType] ![AType] !StrictnessList !StrictnessList !{#CommonDefs} !*TypeHeaps -> (!Bool,!*TypeHeaps)
		match_tuple_args_strictness arg_n [arg1:args1] [arg2:args2] strictness1 strictness2 common_defs type_heaps
			| not (arg_is_strict arg_n strictness1)
				= match_tuple_args_strictness (arg_n+1) args1 args2 strictness1 strictness2 common_defs type_heaps
			| not (arg_is_strict arg_n strictness2)
				= (False,type_heaps)
			# (tuple_strictness_ok,type_heaps) = match_tuple_strictness arg1 arg2 common_defs type_heaps
			| not tuple_strictness_ok
				= (False,type_heaps)
				= match_tuple_args_strictness (arg_n+1) args1 args2 strictness1 strictness2 common_defs type_heaps
		match_tuple_args_strictness arg_n [] [] strictness1 strictness2 common_defs type_heaps
			= (True,type_heaps)

	permute_args args perm n_f_args
		= [args!!p \\ p <- perm & arg_n<-[0..n_f_args-1]]
is_trivial_body args rhs_expr=:(BasicExpr (BVB _)) f_args type ro fun_defs fun_heap type_heaps cons_args
	| both_nil args f_args || (same_length args f_args && no_strict_args type)
		= (Yes rhs_expr,fun_defs,fun_heap,type_heaps,cons_args)
where
	no_strict_args (Yes type)
		= is_not_strict type.st_args_strictness 
	no_strict_args No
		= True
is_trivial_body args rhs f_args type ro fun_defs fun_heap type_heaps cons_args
	= (No,fun_defs,fun_heap,type_heaps,cons_args)
	
same_length [_:l1] [_:l2] = same_length l1 l2
same_length l1 l2 = both_nil l1 l2

both_nil [] [] = True
both_nil _ _ = False

is_safe_producer (SK_GeneratedFunction fun_ptr _) ro fun_heap cons_args
	# (FI_Function {gf_cons_args={cc_producer}}) = sreadPtr fun_ptr fun_heap
	= cc_producer
is_safe_producer (SK_LocalMacroFunction glob_object) ro fun_heap cons_args
	= cons_args.[glob_object].cc_producer
is_safe_producer (SK_Function {glob_module, glob_object}) ro fun_heap cons_args
	# max_index = size cons_args
	| glob_module <> ro.ro_main_dcl_module_n || glob_object >= max_index
		= False
		= cons_args.[glob_object].cc_producer
is_safe_producer (SK_Constructor {glob_module}) ro fun_heap cons_args
	= SwitchConstructorFusion True True/*(glob_module==ro.ro_StdGeneric_module_n)*/ False

transformApplication :: !App ![Expression] !ReadOnlyTI !*TransformInfo -> *(!Expression,!*TransformInfo)
transformApplication app=:{app_symb=symb=:{symb_kind}, app_args} extra_args
			ro ti=:{ti_cons_args,ti_instances,ti_fun_defs}
	| is_SK_Function_or_SK_LocalMacroFunction symb_kind // otherwise GOTO next alternative	
		# gi
			= case symb_kind of
				SK_Function global_index -> global_index
				SK_LocalMacroFunction index -> { glob_module = ro.ro_main_dcl_module_n, glob_object = index }
		# { glob_module, glob_object } = gi
		| glob_module == ro.ro_main_dcl_module_n
			| glob_object < size ti_cons_args
				#  (cons_class,ti_cons_args) = ti_cons_args![glob_object]
				   (instances, ti_instances) = ti_instances![glob_object]
				   (fun_def, ti_fun_defs) = ti_fun_defs![glob_object]
				   ti = { ti & ti_instances = ti_instances, ti_fun_defs = ti_fun_defs, ti_cons_args = ti_cons_args }
				= transformFunctionApplication fun_def instances cons_class app extra_args ro ti
			// It seems as if we have an array function 
				| isEmpty extra_args
					= (App app, ti)
					= (App { app & app_args = app_args ++ extra_args}, ti)

		| glob_module==ro.ro_StdStrictLists_module_n && is_cons_or_decons_of_UList_or_UTSList glob_object glob_module ro.ro_imported_funs && (not (isEmpty app_args))
//			&& True ---> ("transformApplication "+++toString symb.symb_ident)
			# {ft_type} = ro.ro_imported_funs.[glob_module].[glob_object] // type of cons instance of instance List [#] a | U(TS)List a
			# [{tc_class=TCClass {glob_module,glob_object={ds_index}}}:_] = ft_type.st_context			
			# member_n=find_member_n 0 symb.symb_ident.id_name ro.ro_common_defs.[glob_module].com_class_defs.[ds_index].class_members
			# cons_u_member_index=ro.ro_common_defs.[glob_module].com_class_defs.[ds_index].class_members.[member_n].ds_index
			# {me_ident,me_offset}=ro.ro_common_defs.[glob_module].com_member_defs.[cons_u_member_index]
			# select_symb= {glob_module=glob_module,glob_object={ds_ident=me_ident,ds_index=cons_u_member_index,ds_arity=1}}
			# [first_arg:other_app_args] = app_args;
			# args=other_app_args++extra_args
			| isEmpty args
				= select_member first_arg select_symb me_offset ti
				# (expr,ti) = select_member first_arg select_symb me_offset ti
				= case expr of
					App app
						-> transformApplication app args ro ti
					_
						-> (expr @ args,ti)
		// This function is imported
		| SwitchSpecialFusion
				(not (isEmpty app_args) )
				False
			// Check imported overloaded function application for specials...
			# {ft_specials}						= ro.ro_imported_funs.[glob_module].[glob_object]
			# specials							= case ft_specials of
													FSP_ContextTypes s	-> s
													_ -> []
			| not (isEmpty specials)
				# (ei,ti_symbol_heap)			= mapSt readAppInfo app_args ti.ti_symbol_heap
					with
						readAppInfo :: !Expression !*ExpressionHeap -> (!ExprInfo,!*ExpressionHeap)
						readAppInfo (App {app_info_ptr}) heap
							| isNilPtr app_info_ptr
								= (EI_Empty,heap)
							= readPtr app_info_ptr heap
						readAppInfo _ heap = (EI_Empty,heap)
				# ti							= {ti & ti_symbol_heap = ti_symbol_heap}
				# context						= ro.ro_imported_funs.[glob_module].[glob_object].ft_type.st_context
				# insts							= resolveContext context ei ro.ro_common_defs
				# (num_special_args,special_gi)	= findInstInSpecials insts specials
				| foundSpecial special_gi
					= build_application {app & app_symb.symb_kind = SK_Function special_gi} (drop num_special_args app_args) extra_args special_gi ti
				= build_application app app_args extra_args gi ti
			= build_application app app_args extra_args gi ti
		= build_application app app_args extra_args gi ti
	where
		build_application :: !.App ![.Expression] ![.Expression] !(Global .Int) !*TransformInfo -> (!Expression,!*TransformInfo)
		build_application app app_args extra_args {glob_module,glob_object} ti
			| isEmpty extra_args
				= (App {app & app_args = app_args}, ti)
			# {ft_arity,ft_type}	= ro.ro_imported_funs.[glob_module].[glob_object]
			  form_arity			= ft_arity + length ft_type.st_context
			  ar_diff				= form_arity - length app_args
			  nr_of_extra_args		= length extra_args
			| nr_of_extra_args <= ar_diff
				= (App {app  &  app_args = app_args ++ extra_args }, ti)
				= (App {app  &  app_args = app_args ++ take ar_diff extra_args } @ drop ar_diff extra_args, ti)
/*		
		build_special_application app app_args extra_args {glob_module,glob_object} ro ti
			| isEmpty extra_args
				= (App {app & app_args = app_args}, ti)
			# {ft_arity,ft_type} = ro.ro_imported_funs.[glob_module].[glob_object]
			  form_arity = ft_arity + length ft_type.st_context
			  ar_diff = form_arity - length app_args
			  nr_of_extra_args = length extra_args
			| nr_of_extra_args <= ar_diff
				= (App {app  &  app_args = app_args ++ extra_args }, ti)
				= (App {app  &  app_args = app_args ++ take ar_diff extra_args } @ drop ar_diff extra_args, ti)
*/
		find_member_n :: !Int !String !{#.DefinedSymbol} -> Int
		find_member_n i member_string a
			| i<size a
				| a.[i].ds_ident.id_name % (0,size member_string-1)==member_string
					= i
					= find_member_n (i+1) member_string a

		select_member :: !.Expression !(Global .DefinedSymbol) !.Int !*TransformInfo -> *(!Expression,!*TransformInfo)
		select_member exp=:(App {app_symb={symb_kind=SK_Constructor _},app_args,app_info_ptr}) select_symb me_offset ti=:{ti_symbol_heap}
			| not (isNilPtr app_info_ptr)
				# (ei,ti_symbol_heap)	= readPtr app_info_ptr ti_symbol_heap
				# ti = {ti & ti_symbol_heap = ti_symbol_heap}
				= case ei of
					(EI_DictionaryType _)	-> (app_args !! me_offset,ti)
					_						-> (Selection NormalSelector exp [RecordSelection select_symb me_offset],ti)
		select_member exp select_symb me_offset ti
			= (Selection NormalSelector exp [RecordSelection select_symb me_offset],ti)

// XXX linear_bits field has to be added for generated functions
transformApplication app=:{app_symb={symb_ident,symb_kind = SK_GeneratedFunction fun_def_ptr fun_index}} extra_args
			ro ti=:{ti_cons_args,ti_instances,ti_fun_defs,ti_fun_heap}
	| fun_index < size ti_cons_args
		#  (cons_class, ti_cons_args) = ti_cons_args![fun_index]
		   (instances, ti_instances) = ti_instances![fun_index]
		   (fun_def, ti_fun_defs) = ti_fun_defs![fun_index]
		   ti = { ti & ti_instances = ti_instances, ti_fun_defs = ti_fun_defs, ti_cons_args = ti_cons_args }
		= transformFunctionApplication fun_def instances cons_class app extra_args ro ti
	# (FI_Function {gf_fun_def,gf_instance_info,gf_cons_args}, ti_fun_heap) = readPtr fun_def_ptr ti_fun_heap
	  ti = { ti & ti_fun_heap = ti_fun_heap }
	= transformFunctionApplication gf_fun_def gf_instance_info gf_cons_args app extra_args ro ti
transformApplication app [] ro ti
	= (App app, ti)
transformApplication app=:{app_symb={symb_ident,symb_kind = SK_Constructor cons_index},app_args} extra_args
			ro ti=:{ti_cons_args,ti_instances,ti_fun_defs,ti_fun_heap}
	# {cons_type}			= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
	# (app_args,extra_args)	= complete_application cons_type.st_arity app_args extra_args
	= (build_application { app & app_args = app_args } extra_args, ti)
where
	complete_application :: !.Int ![Expression] ![Expression] -> (![Expression],![Expression])
	complete_application form_arity args []
		= (args, [])
	complete_application form_arity args extra_args
		# arity_diff = min (form_arity - length args) (length extra_args)
		= (args ++ take arity_diff extra_args, drop arity_diff extra_args)

	build_application :: !.App ![.Expression] -> Expression
	build_application app []
		= App app
	build_application app extra_args
		= App app @ extra_args
transformApplication app extra_args ro ti
	= (App app @ extra_args, ti)

transformSelection :: SelectorKind [Selection] Expression ReadOnlyTI *TransformInfo -> (!Expression,!*TransformInfo)
transformSelection NormalSelector s=:[RecordSelection _ field_index : selectors] 
					app=:(App appi=:{app_symb={symb_kind= SK_Constructor _ }, app_args, app_info_ptr})
					ro ti=:{ti_symbol_heap}
	| isNilPtr app_info_ptr
		// urgh: now reevaluates cnf for each nested strict selector :-(
		| cnf_app_args appi ro
			= transformSelection NormalSelector selectors (app_args !! field_index) ro ti
			= transform_remaining_selectors_of_normal_record_selector s app ro ti
	# (app_info, ti_symbol_heap) = readPtr app_info_ptr ti_symbol_heap
	  ti = { ti & ti_symbol_heap = ti_symbol_heap }
	= case app_info of
		EI_DictionaryType _
			-> transformSelection NormalSelector selectors (app_args !! field_index) ro ti
		_
			// urgh: now reevaluates cnf for each nested strict selector :-(
			| cnf_app_args appi ro
				-> transformSelection NormalSelector selectors (app_args !! field_index) ro ti
			-> transform_remaining_selectors_of_normal_record_selector s app ro ti
where
	cnf_args [] index strictness ro = True
	cnf_args [arg:args] index strictness ro
		| arg_is_strict index strictness
			= case arg of
				BasicExpr _	-> cnf_args args (inc index) strictness ro
				App app		-> cnf_app_args app ro
				_			-> False
		= cnf_args args (inc index) strictness ro
	
	cnf_app_args {app_symb=symb=:{symb_kind = SK_Constructor cons_index, symb_ident}, app_args} ro
		# {cons_type}		= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
		= cnf_args app_args 0 cons_type.st_args_strictness ro
	cnf_app_args {app_symb=symb=:{symb_kind}, app_args} ro
		= False
transformSelection NormalSelector s=:[RecordSelection _ field_index : selectors] 
					app=:(App appi=:{app_symb=app_symb=:{symb_kind}, app_args, app_info_ptr})
					ro ti
	| isOKSymbol symb_kind && isEmpty app_args
		# (fun_def,ti_fun_defs,ti_fun_heap)		= get_fun_def symb_kind ro.ro_main_dcl_module_n ti.ti_fun_defs ti.ti_fun_heap
		# ti = {ti & ti_fun_defs = ti_fun_defs, ti_fun_heap = ti_fun_heap}
		# {fun_body,fun_type,fun_kind}			= fun_def
		| is_not_caf fun_kind
			= case fun_body of
				TransformedBody {tb_rhs}	-> case tb_rhs of
					App app						-> transformSelection NormalSelector s tb_rhs ro ti
					_							-> transform_remaining_selectors_of_normal_record_selector s app ro ti
			= transform_remaining_selectors_of_normal_record_selector s app ro ti
where
	isOKSymbol (SK_Function {glob_module})	= glob_module == ro.ro_main_dcl_module_n
	isOKSymbol (SK_LocalMacroFunction _)	= True
	isOKSymbol (SK_GeneratedFunction _ _)	= True
	isOKSymbol _							= False
	
	is_not_caf FK_Caf	= False
	is_not_caf _		= True
transformSelection NormalSelector [] expr ro ti
	= (expr, ti)
transformSelection selector_kind selectors expr ro ti
	# (selectors,ti) = transform_expressions_in_selectors selectors ro ti
	= (Selection selector_kind expr selectors, ti)

transform_remaining_selectors_of_normal_record_selector :: ![Selection] !Expression ReadOnlyTI *TransformInfo -> (!Expression,!*TransformInfo)
transform_remaining_selectors_of_normal_record_selector selectors=:[record_selector] app ro ti
	= (Selection NormalSelector app selectors, ti)
transform_remaining_selectors_of_normal_record_selector [record_selector:remaining_selectors] app ro ti
	# (remaining_selectors,ti) = transform_expressions_in_selectors remaining_selectors ro ti
	= (Selection NormalSelector app [record_selector:remaining_selectors], ti)

//@	determineProducers: finds all legal producers in the argument list.
// This version finds FIRST legal producer in argument list...

// XXX store linear_bits and cc_args together ?

determineProducers :: !BITVECT !Bool !Bool !(Optional SymbolType) ![#Bool!] ![Int] ![Expression] !Int *{!Producer} !ReadOnlyTI !*TransformInfo
	-> *(!*{!Producer},![Expression],![(LetBind,AType)],!*TransformInfo)
determineProducers _ _ _ _ _ _ [] _ producers _ ti
	= (producers, [], [], ti)
determineProducers consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type [#linear_bit : linear_bits!] [cons_arg : cons_args] [arg : args] prod_index producers ro ti
 	| cons_arg == CActive
 		# (equal_arg_ns,ti) = find_equal_arguments arg args consumer_properties consumer_type cons_arg prod_index ro ti
		| not (case equal_arg_ns of [#!] -> True; _ -> False)
			= equal_producers equal_arg_ns arg args prod_index producers ti
		# (producers, new_arg, ti) = determine_producer consumer_properties consumer_is_curried ok_non_rec_consumer linear_bit arg [] prod_index producers ro ti
		| isProducer producers.[prod_index]
			= (producers, new_arg++args, [], ti)
		| ro.ro_transform_fusion<FullFusion || consumer_properties bitand FI_GenericFun==0
			#! (producers, new_args, lb, ti)
				= determineProducers consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args args (inc prod_index) producers ro ti
			= (producers, new_arg++new_args, lb, ti)
			= case arg of
				BasicExpr (BVS s)
					# producers & [prod_index] = PR_String s
					-> (producers, args, [], ti)
				BasicExpr (BVInt i)
					# producers & [prod_index] = PR_Int i
					-> (producers, args, [], ti)
				_
					#! (producers, new_args, lb, ti) = determineProducers consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args args (inc prod_index) producers ro ti
					-> (producers, new_arg++new_args, lb, ti)
	| ro.ro_transform_fusion<FullFusion
		#! (producers, new_args, lb, ti)
			= determineProducers consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args args (inc prod_index) producers ro ti
		= (producers, [arg : new_args], lb, ti)
	| SwitchUnusedFusion (cons_arg == CUnusedStrict && isStrictArg consumer_type prod_index) False
		# producers = { producers & [prod_index] = PR_Unused }
		# (is_strict_var_or_simple_expression,ti) = isStrictVarOrSimpleExpression arg ti
		# (lb,ti) = case is_strict_var_or_simple_expression of
						True
							-> ([],ti)
						_
							# arg_type = getArgType consumer_type prod_index
							  (info_ptr, ti_var_heap) = newPtr (VI_Extended (EVI_VarType arg_type) VI_Empty) ti.ti_var_heap
							  ti = {ti & ti_var_heap = ti_var_heap}
							  lb = {lb_dst=
							  			{ fv_ident = {id_name = "dummy_for_strict_unused", id_info = nilPtr}
							  			, fv_info_ptr = info_ptr
							  			, fv_count = 0
							  			, fv_def_level = NotALevel 
							  			}
							  		,lb_src=arg
							  		,lb_position=NoPos
							  		}
							-> ([(lb,arg_type)],ti)
		= (producers, args, lb, ti)	 // ---> ("UnusedStrict",lb,arg,fun_type)
	| SwitchUnusedFusion (cons_arg == CUnusedStrict && not (isStrictArg consumer_type prod_index) && isStrictVar arg) False
		# producers = { producers & [prod_index] = PR_Unused }
		= determineUnusedProducersInNextArgs cons_args args (prod_index+1) producers ro ti
	| SwitchUnusedFusion (cons_arg == CUnusedLazy) False
		# producers = { producers & [prod_index] = PR_Unused }
		= determineUnusedProducersInNextArgs cons_args args (prod_index+1) producers ro ti
		= case arg of
			App {app_symb=symb=:{symb_kind=SK_Function {glob_module,glob_object}},app_args=[]}
				| glob_module==ro.ro_main_dcl_module_n
					# ({fun_arity,fun_info,fun_type},ti) = ti!ti_fun_defs.[glob_object]
					| fun_arity>0
						# arg_ns = find_same_SK_Function_args args glob_module glob_object (prod_index+1)
						| not (case arg_ns of [#!] -> True; _ -> False) && is_monomorphic_symbol_type fun_type
							-> equal_producers arg_ns arg args prod_index producers ti
						| fun_info.fi_properties bitand FI_IsNonRecursive<>0 && consumer_properties bitand FI_GenericFun<>0
							# producers & [prod_index] = PR_Curried symb 0
							-> (producers, args, [], ti)
							-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
						-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
				# {st_arity,st_context} = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
				| (st_arity>0 || not (isEmpty st_context)) && consumer_properties bitand FI_GenericFun<>0
					# producers & [prod_index] = PR_Curried symb 0
					-> (producers, args, [], ti)
					-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
			App {app_symb=symb1=:{symb_kind=SK_Function {glob_module=module_1,glob_object=object_1}},app_args=
					[App {app_symb=symb2=:{symb_kind=SK_Function {glob_module=module_2,glob_object=object_2}},app_args=[]}]}
				| module_1==ro.ro_main_dcl_module_n && module_2==ro.ro_main_dcl_module_n && consumer_properties bitand FI_GenericFun<>0
					# (fun_def1,ti) = ti!ti_fun_defs.[object_1]
					| fun_def1.fun_arity>1 && fun_def1.fun_info.fi_properties bitand FI_IsNonRecursive==0
						# (fun_def2,ti) = ti!ti_fun_defs.[object_2]
						| fun_def2.fun_arity>0 // && fun_def2.fun_info.fi_properties bitand FI_IsNonRecursive<>0
							# arg_ns = find_same_SK_Functions_args args ro.ro_main_dcl_module_n object_1 object_2 (prod_index+1)
							| not (case arg_ns of [#!] -> True; _ -> False)
							&& is_monomorphic_symbol_type fun_def2.fun_type
							&& is_monomorphic_symbol_type_for_monomorphic_arg fun_def1.fun_type
								-> equal_producers arg_ns arg args prod_index producers ti
								-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
							-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
						-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
					-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
			App {app_symb=symb=:{symb_kind=SK_LocalMacroFunction fun_index},app_args=[]}
				# ({fun_arity,fun_info,fun_type},ti) = ti!ti_fun_defs.[fun_index]
				| fun_arity>0
					# arg_ns = find_same_SK_LocalMacroFunction_args args fun_index (prod_index+1)
					| not (case arg_ns of [#!] -> True; _ -> False) && is_monomorphic_symbol_type fun_type
						-> equal_producers arg_ns arg args prod_index producers ti
					| fun_info.fi_properties bitand FI_IsNonRecursive<>0 && consumer_properties bitand FI_GenericFun<>0
						# producers & [prod_index] = PR_Curried symb 0
						-> (producers, args, [], ti)
					-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
			App {app_symb=symb=:{symb_kind=SK_GeneratedFunction fun_ptr fun_index},app_args=[]}
				# (FI_Function {gf_fun_def={fun_arity,fun_info,fun_type}},fun_heap) = readPtr fun_ptr ti.ti_fun_heap
				  ti & ti_fun_heap = fun_heap
				| fun_arity>0
					# arg_ns = find_same_SK_GeneratedFunction_args args fun_index (prod_index+1)
					| not (case arg_ns of [#!] -> True; _ -> False) && is_monomorphic_symbol_type fun_type
						-> equal_producers arg_ns arg args prod_index producers ti
					| fun_info.fi_properties bitand FI_IsNonRecursive<>0 && consumer_properties bitand FI_GenericFun<>0
						# producers & [prod_index] = PR_Curried symb 0
						-> (producers, args, [], ti)
						-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
					-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
			Var {var_info_ptr}
				| not (cons_arg==CUnusedStrict || cons_arg==CUnusedLazy)
					# arg_ns = find_same_Vars args var_info_ptr (prod_index+1)
					| not (case arg_ns of [#!] -> True; _ -> False)
						# arg_ns = filter_same_types consumer_type prod_index arg_ns
						| not (case arg_ns of [#!] -> True; _ -> False)
							-> equal_producers arg_ns arg args prod_index producers ti
							-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
						-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
			BasicExpr (BVS s)
				| consumer_properties bitand FI_GenericFun<>0
					# producers & [prod_index] = PR_String s
					-> (producers, args, [], ti)
			BasicExpr (BVInt i)
				| consumer_properties bitand FI_GenericFun<>0
					# producers & [prod_index] = PR_Int i
					-> (producers, args, [], ti)
			_
				-> determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
where
	determineProducersInNextArgs consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args arg args prod_index producers ro ti
		#! (producers, new_args, lb, ti)
			= determineProducers consumer_properties consumer_is_curried ok_non_rec_consumer consumer_type linear_bits cons_args args (inc prod_index) producers ro ti
		= (producers, [arg : new_args], lb, ti)

	determineUnusedProducersInNextArgs [cons_arg : cons_args] arg_and_args=:[arg : args] prod_index producers ro ti
		| SwitchUnusedFusion (cons_arg == CUnusedStrict && isStrictArg consumer_type prod_index) False
			# producers & [prod_index] = PR_Unused
			# (is_strict_var_or_simple_expression,ti) = isStrictVarOrSimpleExpression arg ti
			# (lb,ti) = case is_strict_var_or_simple_expression of
							True
								-> ([],ti)
							_
								# arg_type = getArgType consumer_type prod_index
								  (info_ptr, ti_var_heap) = newPtr (VI_Extended (EVI_VarType arg_type) VI_Empty) ti.ti_var_heap
								  ti & ti_var_heap = ti_var_heap
								  lb =	{lb_dst=
								  			{ fv_ident = {id_name = "dummy_for_strict_unused", id_info = nilPtr}
								  			, fv_info_ptr = info_ptr, fv_count = 0, fv_def_level = NotALevel }
								  		,lb_src=arg, lb_position=NoPos }
								-> ([(lb,arg_type)],ti)
			= (producers, args, lb, ti)	 // ---> ("UnusedStrict",lb,arg,fun_type)
		| SwitchUnusedFusion (cons_arg == CUnusedStrict && not (isStrictArg consumer_type prod_index) && isStrictVar arg) False
			# producers & [prod_index] = PR_Unused
			= determineUnusedProducersInNextArgs cons_args args (prod_index+1) producers ro ti
		| SwitchUnusedFusion (cons_arg == CUnusedLazy) False
			# producers & [prod_index] = PR_Unused
			= determineUnusedProducersInNextArgs cons_args args (prod_index+1) producers ro ti
			= (producers, arg_and_args, [], ti)
	determineUnusedProducersInNextArgs _ [] prod_index producers ro ti
		= (producers, [], [], ti)

	isProducer PR_Empty	= False
	isProducer _		= True
	
	isStrictArg No _ = False
	isStrictArg (Yes {st_args_strictness}) index = arg_is_strict index st_args_strictness

	getArgType (Yes {st_args}) index = st_args!!index

	isStrictVar (Var bv) = not (isEmpty [fv \\ fv <- ro.ro_tfi.tfi_vars | fv.fv_info_ptr == bv.var_info_ptr])
	isStrictVar _ = False

	isStrictVarOrSimpleExpression :: !Expression *TransformInfo -> *(!Bool,*TransformInfo)
	isStrictVarOrSimpleExpression (Var bv) ti
		= (not (isEmpty [fv \\ fv <- ro.ro_tfi.tfi_vars | fv.fv_info_ptr == bv.var_info_ptr]),ti)
	isStrictVarOrSimpleExpression (App {app_symb={symb_kind=SK_Constructor _},app_args=[]}) ti
		= (True,ti)
	isStrictVarOrSimpleExpression (App {app_symb={symb_kind=SK_Constructor cons_index},app_args}) ti
		# cons_arity = ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object].cons_type.st_arity
		# n_args = length app_args
		| n_args==cons_arity
			= all_isStrictVarOrSimpleExpression app_args ti
			= (n_args<cons_arity,ti)
		where
			all_isStrictVarOrSimpleExpression [a:as] ti
				# (b,ti) = isStrictVarOrSimpleExpression a ti
				| b
					= all_isStrictVarOrSimpleExpression as ti
					= (False,ti)
			all_isStrictVarOrSimpleExpression [] ti
				= (True,ti)
	isStrictVarOrSimpleExpression (App {app_symb={symb_kind=SK_Function {glob_module, glob_object}},app_args}) ti
		| glob_module == ro.ro_main_dcl_module_n
			#! fun_arity = ti.ti_fun_defs.[glob_object].fun_arity
			= (length app_args < fun_arity,ti)
			#! fun_arity = ro.ro_imported_funs.[glob_module].[glob_object].ft_arity
			= (length app_args < fun_arity,ti)
	isStrictVarOrSimpleExpression (App {app_symb={symb_kind=SK_LocalMacroFunction glob_object},app_args}) ti
		#! fun_arity = ti.ti_fun_defs.[glob_object].fun_arity
		= (length app_args < fun_arity,ti)
	isStrictVarOrSimpleExpression (App {app_symb={symb_kind=SK_GeneratedFunction fun_ptr fun_index},app_args}) ti
		# (FI_Function {gf_fun_def={fun_arity}},fun_heap) = readPtr fun_ptr ti.ti_fun_heap
		  ti & ti_fun_heap = fun_heap
		= (length app_args<fun_arity,ti)
	isStrictVarOrSimpleExpression (BasicExpr _) ti
		= (True,ti)
	isStrictVarOrSimpleExpression ExprToBeRemoved ti
		= (True,ti)
	isStrictVarOrSimpleExpression _ ti
		= (False,ti)

	determine_producer consumer_properties consumer_is_curried ok_non_rec_consumer linear_bit arg=:(App app=:{app_info_ptr}) new_args prod_index producers ro ti
		| isNilPtr app_info_ptr
			= determineProducer app EI_Empty consumer_properties consumer_is_curried ok_non_rec_consumer linear_bit new_args prod_index producers ro ti
		# (app_info, ti_symbol_heap) = readPtr app_info_ptr ti.ti_symbol_heap
		# ti = { ti & ti_symbol_heap = ti_symbol_heap }
		= determineProducer app app_info consumer_properties consumer_is_curried ok_non_rec_consumer linear_bit new_args prod_index producers ro ti
	determine_producer _ _ _ _ arg new_args _ producers _ ti
		= (producers, [arg : new_args], ti)

find_equal_arguments :: !Expression ![Expression] !BITVECT !(Optional SymbolType) !Int !Int !ReadOnlyTI !*TransformInfo -> *(![#Int!],!*TransformInfo)
find_equal_arguments (App {app_symb=symb=:{symb_kind=SK_Function {glob_module,glob_object}},app_args=[]}) args consumer_properties consumer_type cons_arg prod_index ro ti
	| glob_module==ro.ro_main_dcl_module_n
		# ({fun_arity,fun_type},ti) = ti!ti_fun_defs.[glob_object]
		| fun_arity>0
			# arg_ns = find_same_SK_Function_args args glob_module glob_object (prod_index+1)
			| not (case arg_ns of [#!] -> True; _ -> False) && is_monomorphic_symbol_type fun_type
				= (arg_ns,ti)
				= ([#!],ti)
			= ([#!],ti)
find_equal_arguments (App {app_symb=symb1=:{symb_kind=SK_Function {glob_module=module_1,glob_object=object_1}},app_args=
					 [App {app_symb=symb2=:{symb_kind=SK_Function {glob_module=module_2,glob_object=object_2}},app_args=[]}]}) args consumer_properties consumer_type cons_arg prod_index ro ti
	| module_1==ro.ro_main_dcl_module_n && module_2==ro.ro_main_dcl_module_n && consumer_properties bitand FI_GenericFun<>0
		# (fun_def1,ti) = ti!ti_fun_defs.[object_1]
		| fun_def1.fun_arity>1 && fun_def1.fun_info.fi_properties bitand FI_IsNonRecursive==0
			# (fun_def2,ti) = ti!ti_fun_defs.[object_2]
			| fun_def2.fun_arity>0 // && fun_def2.fun_info.fi_properties bitand FI_IsNonRecursive<>0				
				# arg_ns = find_same_SK_Functions_args args ro.ro_main_dcl_module_n object_1 object_2 (prod_index+1)
				| not (case arg_ns of [#!] -> True; _ -> False)
				&& is_monomorphic_symbol_type fun_def2.fun_type
				&& is_monomorphic_symbol_type_for_monomorphic_arg fun_def1.fun_type
					= (arg_ns,ti)
					= ([#!],ti)
				= ([#!],ti)
			= ([#!],ti)
		= ([#!],ti)
find_equal_arguments (App {app_symb=symb=:{symb_kind=SK_LocalMacroFunction fun_index},app_args=[]}) args consumer_properties consumer_type cons_arg prod_index ro ti
	# ({fun_arity,fun_type},ti) = ti!ti_fun_defs.[fun_index]
	| fun_arity>0
		# arg_ns = find_same_SK_LocalMacroFunction_args args fun_index (prod_index+1)
		| not (case arg_ns of [#!] -> True; _ -> False) && is_monomorphic_symbol_type fun_type
			= (arg_ns,ti)
			= ([#!],ti)
		= ([#!],ti)
find_equal_arguments (App {app_symb=symb=:{symb_kind=SK_GeneratedFunction fun_ptr fun_index},app_args=[]}) args consumer_properties consumer_type cons_arg prod_index ro ti
	# (FI_Function {gf_fun_def={fun_arity,fun_type}},fun_heap) = readPtr fun_ptr ti.ti_fun_heap
	  ti & ti_fun_heap = fun_heap
	| fun_arity>0
		# arg_ns = find_same_SK_GeneratedFunction_args args fun_index (prod_index+1)
		| not (case arg_ns of [#!] -> True; _ -> False) && is_monomorphic_symbol_type fun_type
			= (arg_ns,ti)
			= ([#!],ti)
		= ([#!],ti)
find_equal_arguments (Var {var_info_ptr}) args consumer_properties consumer_type cons_arg prod_index ro ti
	| not (cons_arg==CUnusedStrict || cons_arg==CUnusedLazy)
		# arg_ns = find_same_Vars args var_info_ptr (prod_index+1)
		| not (case arg_ns of [#!] -> True; _ -> False)
			# arg_ns = filter_same_types consumer_type prod_index arg_ns
			| not (case arg_ns of [#!] -> True; _ -> False)
				= (arg_ns,ti)
				= ([#!],ti)
			= ([#!],ti)
		= ([#!],ti)
find_equal_arguments _ args consumer_properties consumer_type cons_arg prod_index ro ti
	= ([#!],ti)

equal_producers :: ![#Int!] !Expression ![Expression] !Int !*{!Producer} !*TransformInfo -> (!*{!Producer},![Expression],![(LetBind,AType)],!*TransformInfo)
equal_producers arg_ns arg args prod_index producers ti
	# producers & [prod_index] = PR_Equal
	  pr_equal_remove = PR_EqualRemove prod_index
	  producers = set_args_pr_equal_remove arg_ns producers
		with
			set_args_pr_equal_remove [#arg_n:arg_ns!] producers
				= set_args_pr_equal_remove arg_ns {producers & [arg_n]=pr_equal_remove}
			set_args_pr_equal_remove [#!] producers
				= producers
	= (producers, [arg:remove_arg_ns arg_ns (prod_index+1) args], [], ti)
where
	remove_arg_ns arg_ns0=:[#arg_n0:arg_ns1!] arg_n [arg:args]
		| arg_n0<>arg_n
			= [arg:remove_arg_ns arg_ns0 (arg_n+1) args]
			= remove_arg_ns arg_ns1 (arg_n+1) args
	remove_arg_ns [#!] _ args = args

find_same_SK_Function_args :: ![Expression] !Int !Int !Int -> [#Int!]
find_same_SK_Function_args [App {app_symb={symb_kind=SK_Function {glob_module,glob_object}},app_args=[]}:args] fun_module fun_index arg_n
	| glob_module==fun_module && glob_object==fun_index
		= [#arg_n:find_same_SK_Function_args args fun_module fun_index (arg_n+1)!]
		= find_same_SK_Function_args args fun_module fun_index (arg_n+1)
find_same_SK_Function_args [arg:args] fun_module fun_index arg_n
	= find_same_SK_Function_args args fun_module fun_index (arg_n+1)
find_same_SK_Function_args [] fun_module fun_index arg_n
	= [#!]

find_same_SK_Functions_args :: ![Expression] !Int !Int !Int !Int -> [#Int!]
find_same_SK_Functions_args [App {app_symb={symb_kind=SK_Function app_fun1},app_args=
								[App {app_symb={symb_kind=SK_Function app_fun2},app_args=[]}]}:args]
							main_module_n fun_index1 fun_index2 arg_n
	| app_fun1.glob_module==main_module_n && app_fun1.glob_object==fun_index1
	&& app_fun2.glob_module==main_module_n && app_fun2.glob_object==fun_index2
		= [#arg_n:find_same_SK_Functions_args args main_module_n fun_index1 fun_index2 (arg_n+1)!]
		= find_same_SK_Functions_args args main_module_n fun_index1 fun_index2 (arg_n+1)
find_same_SK_Functions_args [arg:args] main_module_n fun_index1 fun_index2 arg_n
	= find_same_SK_Functions_args args main_module_n fun_index1 fun_index2 (arg_n+1)
find_same_SK_Functions_args [] main_module_n fun_index1 fun_index2 arg_n
	= [#!]

find_same_SK_LocalMacroFunction_args :: ![Expression] !Int !Int -> [#Int!]
find_same_SK_LocalMacroFunction_args [App {app_symb={symb_kind=SK_LocalMacroFunction arg_fun_index},app_args=[]}:args] fun_index arg_n
	| arg_fun_index==fun_index
		= [#arg_n:find_same_SK_LocalMacroFunction_args args fun_index (arg_n+1)!]
		= find_same_SK_LocalMacroFunction_args args fun_index (arg_n+1)
find_same_SK_LocalMacroFunction_args [arg:args] fun_index arg_n
	= find_same_SK_LocalMacroFunction_args args fun_index (arg_n+1)
find_same_SK_LocalMacroFunction_args [] fun_index arg_n
	= [#!]

find_same_SK_GeneratedFunction_args :: ![Expression] !Int !Int -> [#Int!]
find_same_SK_GeneratedFunction_args [App {app_symb={symb_kind=SK_GeneratedFunction fun_ptr arg_fun_index},app_args=[]}:args] fun_index arg_n
	| arg_fun_index==fun_index
		= [#arg_n:find_same_SK_GeneratedFunction_args args fun_index (arg_n+1)!]
		= find_same_SK_GeneratedFunction_args args fun_index (arg_n+1)
find_same_SK_GeneratedFunction_args [arg:args] fun_index arg_n
	= find_same_SK_GeneratedFunction_args args fun_index (arg_n+1)
find_same_SK_GeneratedFunction_args [] fun_index arg_n
	= [#!]

is_monomorphic_symbol_type :: !(Optional SymbolType) -> Bool
is_monomorphic_symbol_type (Yes {st_vars=[],st_attr_vars=[]})
	= True
is_monomorphic_symbol_type _
	= False

is_monomorphic_symbol_type_for_monomorphic_arg :: !(Optional SymbolType) -> Bool
is_monomorphic_symbol_type_for_monomorphic_arg (Yes {st_vars=vars,st_attr_vars=[],st_args=[arg:_]})
	= all_vars_occur vars arg
where
	all_vars_occur [type_var:type_vars] type = var_occurs_in_atype type_var.tv_info_ptr type && all_vars_occur type_vars type
	all_vars_occur [] type = True

	var_occurs_in_atype var_info_ptr {at_type}
		= var_occurs_in_type var_info_ptr at_type

	var_occurs_in_type var_info_ptr (TV {tv_info_ptr})
		= var_info_ptr==tv_info_ptr
	var_occurs_in_type var_info_ptr (TA _ atypes)
		= var_occurs_in_atypes var_info_ptr atypes
	var_occurs_in_type var_info_ptr (atype1-->atype2)
		= var_occurs_in_atype var_info_ptr atype1 || var_occurs_in_atype var_info_ptr atype2
	var_occurs_in_type var_info_ptr _
		= False

	var_occurs_in_atypes var_info_ptr [{at_type}:atypes]
		= var_occurs_in_type var_info_ptr at_type || var_occurs_in_atypes var_info_ptr atypes
	var_occurs_in_atypes _ []
		= False
is_monomorphic_symbol_type_for_monomorphic_arg _
	= False

find_same_Vars :: ![Expression] !VarInfoPtr !Int -> [#Int!]
find_same_Vars [Var var:args] var_info_ptr arg_n
	| var.var_info_ptr==var_info_ptr
		= [#arg_n:find_same_Vars args var_info_ptr (arg_n+1)!]
		= find_same_Vars args var_info_ptr (arg_n+1)
find_same_Vars [arg:args] var_info_ptr arg_n
	= find_same_Vars args var_info_ptr (arg_n+1)
find_same_Vars [] var_info_ptr arg_n
	= [#!]

filter_same_types :: !(Optional SymbolType) !Int ![#Int!] -> [#Int!]
filter_same_types (Yes {st_args}) arg_n1 arg_ns
	# (arg1_type,arg_types) = get_arg_type 0 arg_n1 st_args
	= filter_same_arg_types arg_ns (arg_n1+1) arg_types arg1_type
where
	filter_same_arg_types [#arg_n:arg_ns!] arg_types_i arg_types arg1_type
		# (arg_type,arg_types) = get_arg_type arg_n arg_types_i arg_types
		| equal_non_unique_atype arg1_type arg_type
			= [#arg_n:filter_same_arg_types arg_ns (arg_n+1) arg_types arg1_type!]
			= filter_same_arg_types arg_ns (arg_n+1) arg_types arg1_type
	filter_same_arg_types [#!] _ _ _
		= [#!]

	get_arg_type arg_i arg_n [arg_type:arg_types]
		| arg_i<arg_n
			= get_arg_type (arg_i+1) arg_n arg_types
			= (arg_type,arg_types)

	equal_non_unique_atype :: !AType !AType -> Bool 
	equal_non_unique_atype {at_attribute=TA_Multi,at_type=type1} {at_attribute=TA_Multi,at_type=type2}
		= equal_non_unique_type type1 type2
	equal_non_unique_atype {at_attribute=TA_None,at_type=type1} {at_attribute=TA_None,at_type=type2}
		= equal_non_unique_type type1 type2
	equal_non_unique_atype {at_attribute=TA_Var {av_info_ptr=av_info_ptr1},at_type=type1} {at_attribute=TA_Var {av_info_ptr=av_info_ptr2},at_type=type2}
		= av_info_ptr1==av_info_ptr2 && equal_non_unique_type type1 type2
	equal_non_unique_atype type1 type2
		= False

	equal_non_unique_type :: !Type !Type -> Bool 
	equal_non_unique_type (TA {type_index=type_index1} atypes1) (TA {type_index=type_index2} atypes2)
		= type_index1==type_index2 && equal_non_unique_atypes atypes1 atypes2
	equal_non_unique_type (TAS {type_index=type_index1} atypes1 strictness1) (TAS {type_index=type_index2} atypes2 strictness2)
		= type_index1==type_index2 && equal_strictness_lists strictness1 strictness2 && equal_non_unique_atypes atypes1 atypes2
	equal_non_unique_type (a_atype1-->r_atype1) (a_atype2-->r_atype2)
		= equal_non_unique_atype a_atype1 a_atype2 && equal_non_unique_atype r_atype1 r_atype2
	equal_non_unique_type (TB BT_Int) (TB BT_Int)
		= True
	equal_non_unique_type (TB BT_Char) (TB BT_Char)
		= True
	equal_non_unique_type (TB BT_Bool) (TB BT_Bool)
		= True
	equal_non_unique_type (TB BT_Real) (TB BT_Real)
		= True
	equal_non_unique_type (TV {tv_info_ptr=tv_info_ptr1}) (TV {tv_info_ptr=tv_info_ptr2})
		= tv_info_ptr1==tv_info_ptr2
	equal_non_unique_type _ _
		= False

	equal_non_unique_atypes [atype1:atypes1] [atype2:atypes2] = equal_non_unique_atype atype1 atype2 && equal_non_unique_atypes atypes1 atypes2
	equal_non_unique_atypes [] [] = True
	equal_non_unique_atypes _ _ = False

determineProducer :: App ExprInfo BITVECT Bool Bool Bool [Expression] Int *{!Producer} ReadOnlyTI *TransformInfo
	-> *(!*{!Producer},![Expression],!*TransformInfo)
determineProducer app=:{app_symb = symb=:{symb_kind = SK_Constructor _}, app_args} (EI_DictionaryType type) _ _ _ _
				  new_args prod_index producers _ ti=:{ti_var_heap,ti_predef_symbols}
	# (normalise_symbol,ti_predef_symbols) = ti_predef_symbols![PD_Dyn_normalise]
	# (app_args, (new_vars_and_types, free_vars, ti_var_heap))
			= renewVariables app_args normalise_symbol ti_var_heap
	# prod	= PR_Class { app & app_args = app_args } new_vars_and_types type
	= ( {producers & [prod_index] = prod}
	  , free_vars++new_args
	  , {ti & ti_var_heap=ti_var_heap, ti_predef_symbols=ti_predef_symbols}
	  )
determineProducer app=:{app_symb = symb=:{symb_kind = SK_Constructor cons_index, symb_ident}, app_args} _ consumer_properties _ _ linear_bit
				  new_args prod_index producers ro ti
	# {cons_type}								= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
	  rnf										= rnf_args app_args 0 cons_type.st_args_strictness ro
	| SwitchConstructorFusion
		(ro.ro_transform_fusion>=FullFusion && SwitchRnfConstructorFusion (linear_bit || rnf) linear_bit)
		(ro.ro_transform_fusion>=FullFusion && (cons_index.glob_module==ro.ro_StdGeneric_module_n || consumer_properties bitand FI_GenericFun<>0)
											&& (linear_bit || rnf))
		False
		# producers = {producers & [prod_index] = PR_Constructor symb (length app_args) app_args }
		= (producers, app_args ++ new_args, ti)
	= ( producers, [App app : new_args ], ti)
where
	rnf_args [] index strictness ro
		= True
	rnf_args [arg:args] index strictness ro
		| arg_is_strict index strictness
			= case arg of
				BasicExpr _	-> rnf_args args (inc index) strictness ro
				App app		-> rnf_app_args app args index strictness ro
				_			-> False
			= rnf_args args (inc index) strictness ro

	rnf_app_args {app_symb=symb=:{symb_kind = SK_Constructor cons_index, symb_ident}, app_args} args index strictness ro
		# {cons_type}		= ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object]
		| rnf_args app_args 0 cons_type.st_args_strictness ro
			= rnf_args args (inc index) strictness ro
			= False
	// what else is rnf => curried apps
	rnf_app_args {app_symb=symb=:{symb_kind}, app_args} args index strictness ro
		= False
determineProducer app=:{app_symb = symb=:{ symb_kind = SK_GeneratedFunction fun_ptr fun_index}, app_args} _ consumer_properties consumer_is_curried ok_non_rec_consumer linear_bit
				  new_args prod_index producers ro ti
	# (FI_Function {gf_cons_args={cc_producer},gf_fun_def={fun_body, fun_arity, fun_type, fun_info}}, ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
	  ti & ti_fun_heap=ti_fun_heap
	# rec_producer = fun_info.fi_properties bitand FI_IsNonRecursive == 0
	| rec_producer && ro.ro_transform_fusion>=NoRecProdFusion
		= (producers, [App app : new_args], ti)
	# n_app_args = length app_args
	| SwitchArityChecks (n_app_args>1 && size producers + n_app_args - 1 > 32) False
		# ti & ti_error_file = ti.ti_error_file <<< "Possibly missed fusion oppurtunity: Function Arity > 32\n"
		= (producers, [App app : new_args], ti)
	| n_app_args<>fun_arity
		| SwitchCurriedFusion (ro.ro_transform_fusion>=FullFusion) cc_producer False
			# (is_good_producer,ti)
				= SwitchGeneratedFusion
					(function_is_good_producer fun_body fun_type linear_bit ro ti)
					(False,ti)
			| cc_producer && is_good_producer
				= ({producers & [prod_index] = PR_CurriedFunction symb n_app_args fun_index}, app_args ++ new_args, ti)
			= ({producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
		| consumer_properties bitand FI_IsMacroFun <> 0
			= ({producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
		= (producers, [App app : new_args], ti)
	# (is_good_producer,ti)
		= SwitchGeneratedFusion
			(function_is_good_producer fun_body fun_type linear_bit ro ti)
			(False,ti)
	| cc_producer && is_good_producer
		= ({producers & [prod_index] = PR_GeneratedFunction symb n_app_args fun_index}, app_args ++ new_args, ti)
	# not_expanding_producer
		= case fun_body of
			Expanding _
				-> False
			_
				-> True //cc_producer
	| SwitchHOFusion
    	((not consumer_is_curried && not_expanding_producer) && consumer_properties bitand FI_IsMacroFun <> 0 && linear_bit && is_higher_order_function fun_type)
    	False
		= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
    | SwitchHOFusion`
    	((not consumer_is_curried && not_expanding_producer) && ok_non_rec_consumer && linear_bit && is_higher_order_function fun_type)
    	False
		= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
	# non_rec_producer = (fun_info.fi_properties bitand FI_IsNonRecursive) <> 0
	# ok_non_rec
		= case fun_body of
			Expanding _
				-> False
			(TransformedBody {tb_rhs})
				-> ro.ro_transform_fusion>=FullFusion && not_expanding_producer && is_sexy_body tb_rhs && ok_non_rec_consumer && non_rec_producer//is_good_producer
	| SwitchNonRecFusion ok_non_rec False
		= ({producers & [prod_index] = PR_GeneratedFunction symb n_app_args fun_index}, app_args ++ new_args, ti)
	= (producers, [App app : new_args ], ti)
determineProducer app=:{app_symb = symb=:{symb_kind}, app_args} _ consumer_properties consumer_is_curried ok_non_rec_consumer linear_bit
				  new_args prod_index producers ro ti
	| is_SK_Function_or_SK_LocalMacroFunction symb_kind
		# { glob_module, glob_object }
			= case symb_kind of
				SK_Function global_index -> global_index
				SK_LocalMacroFunction index -> { glob_module = ro.ro_main_dcl_module_n, glob_object = index }
		| glob_module==ro.ro_main_dcl_module_n && glob_object<size ti.ti_cons_args &&
			ti.ti_fun_defs.[glob_object].fun_info.fi_properties bitand FI_IsNonRecursive == 0 && ro.ro_transform_fusion>=NoRecProdFusion
			= (producers, [App app : new_args], ti)
		# (fun_arity, ti) = get_fun_arity glob_module glob_object ro ti
		  n_app_args = length app_args
		| n_app_args<>fun_arity
			| consumer_properties bitand FI_IsMacroFun <> 0
				= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
			# ({cc_producer},ti) = ti!ti_cons_args.[glob_object]
			| SwitchCurriedFusion (ro.ro_transform_fusion>=FullFusion) cc_producer False
				# ({fun_body,fun_type,fun_info}, ti) = ti!ti_fun_defs.[glob_object]
				# (is_good_producer,ti)
					= SwitchFunctionFusion
						(function_is_good_producer fun_body fun_type linear_bit ro ti)
						(False,ti)
				#! max_index = size ti.ti_cons_args
				| glob_module==ro.ro_main_dcl_module_n && glob_object < max_index &&
				  is_good_producer && cc_producer && not consumer_is_curried
					= ({producers & [prod_index] = PR_CurriedFunction symb n_app_args glob_object}, app_args ++ new_args, ti)
				= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
			= (producers, [App app : new_args], ti)
		#! max_index = size ti.ti_cons_args
		| glob_module <> ro.ro_main_dcl_module_n || glob_object >= max_index /* Sjaak, to skip array functions */
			= (producers, [App app : new_args ], ti)
//					-!-> ("Produce2cc_array",symb.symb_ident,if (glob_module <> ro.ro_main_dcl_module_n) "foreign" "array")
		# ({fun_body,fun_type,fun_info}, ti) = ti!ti_fun_defs.[glob_object]
		# (is_good_producer,ti)
			= SwitchFunctionFusion
				(function_is_good_producer fun_body fun_type linear_bit ro ti)
				(False,ti)
		  {cc_producer} = ti.ti_cons_args.[glob_object]
		| is_good_producer && cc_producer && not consumer_is_curried
			= ({ producers & [prod_index] = PR_Function symb n_app_args glob_object}, app_args ++ new_args, ti)
	    # not_expanding_producer
			= case fun_body of
				Expanding _
					-> False
				_
					-> True // cc_producer
		| (not consumer_is_curried && not_expanding_producer) && consumer_properties bitand FI_IsMacroFun <> 0 && linear_bit && is_higher_order_function fun_type
			= ({ producers & [prod_index] = PR_Curried symb n_app_args}, app_args ++ new_args, ti)
		# non_rec_producer = (fun_info.fi_properties bitand FI_IsNonRecursive) <> 0
		# ok_non_rec
			= case fun_body of
				Expanding _
					-> False
				(TransformedBody {tb_rhs})
					-> ro.ro_transform_fusion>=FullFusion && not_expanding_producer && is_sexy_body tb_rhs && ok_non_rec_consumer && non_rec_producer//&& is_good_producer
		| SwitchNonRecFusion ok_non_rec False
			= ({producers & [prod_index] = PR_Function symb n_app_args glob_object}, app_args ++ new_args, ti)
		= (producers, [App app : new_args], ti)
	= (producers, [App app : new_args], ti)
where
	get_max_index ti=:{ti_cons_args}
		#! (max_index, ti_cons_args)	= usize ti_cons_args
		= (max_index, {ti & ti_cons_args = ti_cons_args})

	get_fun_arity glob_module glob_object ro ti
		| glob_module <> ro.ro_main_dcl_module_n
			# {st_arity, st_context} = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
			= (st_arity+length st_context, ti)
		// for imported functions you have to add ft_arity and length st_context, but for unimported
		// functions fun_arity alone is sufficient
		= ti!ti_fun_defs.[glob_object].fun_arity

function_is_good_producer (Expanding _) fun_type linear_bit ro ti
	= (False,ti)
function_is_good_producer (TransformedBody {tb_rhs}) fun_type linear_bit ro ti
	| ro.ro_transform_fusion>=FullFusion
		| linear_bit && is_sexy_body tb_rhs
			= (True,ti)
			= function_may_be_copied fun_type tb_rhs ro ti
		= (False,ti)
where
	function_may_be_copied (Yes {st_args_strictness}) rhs ro ti
		| is_not_strict st_args_strictness
			= expression_may_be_copied rhs ro ti
			= (False,ti)
	function_may_be_copied No rhs ro ti
		= expression_may_be_copied rhs ro ti

	// to optimize bimap
	expression_may_be_copied (Var _) ro ti
		= (True,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_Constructor cons_index}, app_args}) ro ti
		# cons_type = ro.ro_common_defs.[cons_index.glob_module].com_cons_defs.[cons_index.glob_object].cons_type
		| cons_index.glob_module==ro.ro_StdGeneric_module_n && is_not_strict cons_type.st_args_strictness
			= expressions_may_be_copied app_args ro ti
			= (False,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_Function {glob_object,glob_module}}, app_args}) ro ti
		| glob_module <> ro.ro_main_dcl_module_n
			# fun_type = ro.ro_imported_funs.[glob_module].[glob_object].ft_type
			| length app_args < fun_type.st_arity+length fun_type.st_context
				= expressions_may_be_copied app_args ro ti
				= (False,ti)
			# (fun_arity,ti) = ti!ti_fun_defs.[glob_object].fun_arity
			| length app_args < fun_arity
				= expressions_may_be_copied app_args ro ti
				= (False,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_LocalMacroFunction glob_object}, app_args}) ro ti
		# (fun_arity,ti) = ti!ti_fun_defs.[glob_object].fun_arity
		| length app_args < fun_arity
			= expressions_may_be_copied app_args ro ti
			= (False,ti)
	expression_may_be_copied (App {app_symb={symb_kind = SK_GeneratedFunction fun_ptr _}, app_args}) ro ti
		# (FI_Function {gf_fun_def={fun_arity}}) = sreadPtr fun_ptr ti.ti_fun_heap
		| length app_args < fun_arity
			= expressions_may_be_copied app_args ro ti
			= (False,ti)
	expression_may_be_copied (Selection NormalSelector (Var _) [RecordSelection {glob_module,glob_object={ds_index}} _]) ro ti
		# selector_type = ro.ro_common_defs.[glob_module].com_selector_defs.[ds_index].sd_type
		| glob_module==ro.ro_StdGeneric_module_n && is_not_strict selector_type.st_args_strictness
			= (True,ti)
			= (False,ti)
	expression_may_be_copied _ ro ti
		= (False,ti)
	
	expressions_may_be_copied [expr:exprs] ro ti
		# (ok,ti) = expression_may_be_copied expr ro ti
		| ok
			= expressions_may_be_copied exprs ro ti
			= (False,ti)
	expressions_may_be_copied [] ro ti
		= (True,ti)

// when two function bodies have fusion with each other this only leads into satisfaction if one body
// fulfills the following sexyness property
// DvA: now that we have producer requirements we can integrate this condition there...
is_sexy_body (AnyCodeExpr _ _ _) = False	
is_sexy_body (ABCCodeExpr _ _) = False	
is_sexy_body (Let {let_strict_binds}) = isEmpty let_strict_binds	
	// currently a producer's body must not be a let with strict bindings. The code sharing elimination algorithm assumes that
	// all strict let bindings are on the top level expression (see "convertCasesOfFunctionsIntoPatterns"). This assumption
	// could otherwise be violated during fusion.
	// -> Here is place for optimisation: Either the fusion algorithm or the code sharing elimination algorithm could be
	// extended to generate new functions when a strict let ends up during fusion in a non top level position (MW)
is_sexy_body _ = True	

is_higher_order_function (Yes {st_result={at_type=_ --> _}})
        = True
is_higher_order_function _
        = False

containsProducer prod_index producers
	| prod_index == 0
		= False
		#! prod_index = dec prod_index
		= is_a_producer producers.[prod_index] || containsProducer prod_index producers
where
	is_a_producer PR_Empty	= False
	is_a_producer _ 		= True

:: *RenewState :== (![(BoundVar, Type)], ![Expression], !*VarHeap)

renewVariables :: ![Expression] !PredefinedSymbol !*VarHeap -> (![Expression], !RenewState)
renewVariables exprs normalise_symbol var_heap
	# (exprs, (new_vars, free_vars, var_heap))
			= mapSt map_expr_st exprs ([], [], var_heap)
	  var_heap = foldSt (\ expr var_heap
	  						-> case expr of
	  							Var {var_info_ptr}	-> writeVarInfo var_info_ptr VI_Empty var_heap
	  							_					-> var_heap
	  					) free_vars var_heap
	= (exprs, (new_vars, free_vars, var_heap))
  where
	map_expr_st (Var var=:{var_info_ptr, var_ident}) (new_vars_accu, free_vars_accu, var_heap)
		# (var_info, var_heap) = readPtr var_info_ptr var_heap
		= case var_info of
			VI_Extended _ (VI_Forward new_var)
				-> (Var new_var, (new_vars_accu, free_vars_accu, var_heap))
			VI_Extended evi=:(EVI_VarType var_type) _
				# (new_var, var_heap)
					= allocate_and_bind_new_var var_ident var_info_ptr evi var_heap
				-> (Var new_var, ([(new_var, var_type.at_type) : new_vars_accu], [Var var:free_vars_accu], var_heap))
	map_expr_st expr=:(App app=:{app_symb={symb_kind=SK_Function {glob_object,glob_module},symb_ident},app_args}) (new_vars_accu, free_vars_accu, var_heap)
		| glob_module==normalise_symbol.pds_module && glob_object==normalise_symbol.pds_def
			# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
			  new_var = { var_ident = symb_ident, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr }
			= (Var new_var, ([(new_var, TE) : new_vars_accu], [expr:free_vars_accu], var_heap))
	map_expr_st expr=:(App app=:{app_args}) st
		# (app_args, st) = mapSt map_expr_st app_args st
		= (App { app & app_args = app_args }, st)
	map_expr_st (Let lad=:{let_lazy_binds, let_strict_binds, let_expr}) st
		# (lazy_free_vars, st)
				= mapSt (\{lb_dst} st -> preprocess_local_var lb_dst st) let_lazy_binds st
		  (strict_free_vars, st)
				= mapSt (\{lb_dst} st -> preprocess_local_var lb_dst st) let_strict_binds st
		  (lazy_rhss, st)
		  		= mapSt (\{lb_src} st -> map_expr_st lb_src st) let_lazy_binds st
		  (strict_rhss, st)
		  		= mapSt (\{lb_src} st -> map_expr_st lb_src st) let_strict_binds st
		  (let_expr, st)
		  		= map_expr_st let_expr st
		  st	= foldSt (\{lb_dst} st -> postprocess_local_var lb_dst st) let_lazy_binds st
		  st	= foldSt (\{lb_dst} st -> postprocess_local_var lb_dst st) let_strict_binds st
		  expr	= Let { lad
		  			& let_lazy_binds	= add_let_binds lazy_free_vars lazy_rhss let_lazy_binds
		  			, let_strict_binds	= add_let_binds strict_free_vars strict_rhss let_strict_binds
		  			, let_expr			= let_expr
					}
		= (expr, st)
	map_expr_st (Selection a expr b) st
		# (expr, st) = map_expr_st expr st
		= (Selection a expr b, st)
	map_expr_st expr=:(BasicExpr _) st 
		= (expr, st)

	preprocess_local_var :: !FreeVar !RenewState -> (!FreeVar, !RenewState)
	preprocess_local_var fv=:{fv_ident, fv_info_ptr} (new_vars_accu, free_vars_accu, var_heap)
		# (evi, var_heap) = readExtendedVarInfo fv_info_ptr var_heap
		  (new_var, var_heap) = allocate_and_bind_new_var fv_ident fv_info_ptr evi var_heap
		= ( { fv & fv_info_ptr = new_var.var_info_ptr }
		  , (new_vars_accu, free_vars_accu, var_heap))

	allocate_and_bind_new_var var_ident var_info_ptr evi var_heap
		# (new_info_ptr, var_heap) = newPtr (VI_Extended evi VI_Empty) var_heap
		  new_var = { var_ident = var_ident, var_info_ptr = new_info_ptr, var_expr_ptr = nilPtr }
		  var_heap = writeVarInfo var_info_ptr (VI_Forward new_var) var_heap
		= (new_var, var_heap)

	postprocess_local_var :: !FreeVar !RenewState -> RenewState
	postprocess_local_var {fv_info_ptr} (a, b, var_heap)
		= (a, b, writeVarInfo fv_info_ptr VI_Empty var_heap)

foldrExprSt f expr st :== foldr_expr_st expr st
  where
	foldr_expr_st expr=:(Var _) st
		= f expr st
	foldr_expr_st app=:(App {app_args}) st
		= f app (foldSt foldr_expr_st app_args st)
	foldr_expr_st lad=:(Let {let_lazy_binds, let_strict_binds, let_expr}) st
		# st
		  		= foldSt (\{lb_src} st -> foldr_expr_st lb_src st) let_lazy_binds st
		  st
		  		= foldSt (\{lb_src} st -> foldr_expr_st lb_src st) let_strict_binds st
		  st
		  		= f let_expr st
		= f lad st
	foldr_expr_st sel=:(Selection a expr b) st
		= f sel (foldr_expr_st expr st)
	foldr_expr_st expr=:(BasicExpr _) st 
		= f expr st

add_let_binds :: [FreeVar] [Expression] [LetBind] -> [LetBind]
add_let_binds free_vars rhss original_binds
	= [{ original_bind & lb_dst = lb_dst, lb_src = lb_src}
		\\ lb_dst <- free_vars & lb_src <- rhss & original_bind <- original_binds]

remove_groups_not_used_by_original_component_members :: ComponentMembers [Component] [Component] -> (![Component],![Component])
remove_groups_not_used_by_original_component_members original_component_members new_groups removed_groups
	# last_component = last new_groups
	| contains_function_in_component last_component.component_members original_component_members
		= (new_groups,removed_groups)
		= remove_groups_not_used_by_original_component_members original_component_members (init new_groups) [last_component:removed_groups]
where
	contains_function_in_component (GeneratedComponentMember function_n _ component_members) original_component_members
		| component_contains_generated_function_n original_component_members function_n
			= True
			= contains_function_in_component component_members original_component_members
	contains_function_in_component (ComponentMember function_n component_members) original_component_members
		| component_contains_function_n original_component_members function_n
			= True
			= contains_function_in_component component_members original_component_members
	contains_function_in_component NoComponentMembers original_component_members
		= False
	
	component_contains_function_n (ComponentMember function_n2 component_members) function_n
		= function_n==function_n2 || component_contains_function_n component_members function_n
	component_contains_function_n (GeneratedComponentMember _ _ component_members) function_n
		= component_contains_function_n component_members function_n
	component_contains_function_n NoComponentMembers function_n
		= False

	component_contains_generated_function_n (GeneratedComponentMember function_n2 _ component_members) function_n
		= function_n==function_n2 || component_contains_generated_function_n component_members function_n
	component_contains_generated_function_n (ComponentMember _ component_members) function_n
		= component_contains_generated_function_n component_members function_n
	component_contains_generated_function_n NoComponentMembers function_n
		= False

remove_unused_used_functions :: ![FunctionInfoPtr] !*FunctionHeap -> (![FunctionInfoPtr],!*FunctionHeap)
remove_unused_used_functions [fun_ptr:fun_ptrs] fun_heap
	# (FI_Function gf, fun_heap) = readPtr fun_ptr fun_heap
	| gf.gf_fun_def.fun_info.fi_properties bitand FI_UnusedUsed<>0
		= remove_unused_used_functions fun_ptrs fun_heap
		# (fun_ptrs, fun_heap) = remove_unused_used_functions fun_ptrs fun_heap
		= ([fun_ptr:fun_ptrs], fun_heap)
remove_unused_used_functions [] fun_heap
	= ([],fun_heap)

mark_unused_functions_in_components :: [Component] *TransformInfo -> *TransformInfo
mark_unused_functions_in_components [removed_group:removed_groups] ti
	# ti = mark_unused_functions removed_group.component_members ti
	= mark_unused_functions_in_components removed_groups ti
where
	mark_unused_functions (ComponentMember member members) ti
		# (fun_info,ti) = ti!ti_fun_defs.[member].fun_info
		  fun_info & fi_properties = fun_info.fi_properties bitor FI_Unused
		  ti & ti_fun_defs.[member].fun_info = fun_info
		= mark_unused_functions members ti
	mark_unused_functions (GeneratedComponentMember member fun_ptr members) ti=:{ti_fun_heap}	
		# (FI_Function gf=:{gf_fun_def=fd=:{fun_info},gf_fun_index}, ti_fun_heap) = readPtr fun_ptr ti_fun_heap
		  fun_info & fi_properties = fun_info.fi_properties bitor FI_Unused
		  fd & fun_info=fun_info
		  ti & ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def=fd}) ti_fun_heap
		= mark_unused_functions members ti
	mark_unused_functions NoComponentMembers ti
		= ti
mark_unused_functions_in_components [] ti
	= ti

mark_fused_members_of_instance :: !ClassInstance !Int !{!InstanceInfo} !{#CommonDefs} !FunctionHeap !*{#FunDef} -> *{#FunDef}
mark_fused_members_of_instance instance_def=:{ins_members,ins_class_index} main_dcl_module_n instances common_defs fun_heap fun_defs
	# {class_ident,class_members} = common_defs.[ins_class_index.gi_module].com_class_defs.[ins_class_index.gi_index]
	# clas_module_member_defs = common_defs.[ins_class_index.gi_module].com_member_defs
	= mark_fused_members_of_instance_members 0 ins_members instances class_members clas_module_member_defs fun_defs
where
	mark_fused_members_of_instance_members :: !Int !{#ClassInstanceMember} !{!InstanceInfo} !{#DefinedSymbol} !{#MemberDef} !*{#FunDef} -> *{#FunDef}
	mark_fused_members_of_instance_members ins_i ins_members instances class_members clas_module_member_defs fun_defs
		| ins_i<size ins_members
			# member_i = class_members.[ins_i].ds_index
			  member_arity = clas_module_member_defs.[member_i].me_type.st_arity
			  {cim_arity,cim_index} = ins_members.[ins_i]
			| cim_index<0
				| cim_arity==main_dcl_module_n
					# cim_index = -1-cim_index
					| cim_index<size instances
						#! fun_defs = mark_fused_members_of_function cim_index instances.[cim_index] cim_index member_arity fun_defs
						= mark_fused_members_of_instance_members (ins_i+1) ins_members instances class_members clas_module_member_defs fun_defs
						= mark_fused_members_of_instance_members (ins_i+1) ins_members instances class_members clas_module_member_defs fun_defs
					= mark_fused_members_of_instance_members (ins_i+1) ins_members instances class_members clas_module_member_defs fun_defs
			#! fun_defs = mark_fused_members_of_function cim_index instances.[cim_index] cim_index member_arity fun_defs
			= mark_fused_members_of_instance_members (ins_i+1) ins_members instances class_members clas_module_member_defs fun_defs
			= fun_defs

	mark_fused_members_of_function :: !Int !InstanceInfo !Int !Int !*{#FunDef} -> *{#FunDef}
	mark_fused_members_of_function fun_index instance_info instance_function_index member_arity fun_defs
		# ({fun_arity},fun_defs) = fun_defs![fun_index]
		= mark_fused_members instance_info fun_arity instance_function_index member_arity fun_defs
	where
		mark_fused_members :: !InstanceInfo !Int !Int !Int !*{#FunDef} -> *{#FunDef}
		mark_fused_members II_Empty fun_arity instance_function_index member_arity fun_defs
			= fun_defs
		mark_fused_members (II_Node producers function_info_ptr l r) fun_arity instance_function_index member_arity fun_defs
			| size producers==fun_arity && last_n_producers_are_empty member_arity (size producers) producers 
				= case sreadPtr function_info_ptr fun_heap of
					FI_Function {gf_fun_index,gf_instance_info}
						# (fun_info,fun_defs) = fun_defs![gf_fun_index].fun_info
						  fun_info & fi_properties = fun_info.fi_properties bitor FI_FusedMember, fi_def_level = instance_function_index
						  fun_defs & [gf_fun_index].fun_info = fun_info
						  fun_defs = mark_fused_members l fun_arity instance_function_index member_arity fun_defs
						  fun_defs = mark_fused_members r fun_arity instance_function_index member_arity fun_defs
						-> mark_fused_members_of_function gf_fun_index gf_instance_info instance_function_index member_arity fun_defs
					FI_Empty
						# fun_defs = mark_fused_members l fun_arity instance_function_index member_arity fun_defs
						-> mark_fused_members r fun_arity instance_function_index member_arity fun_defs
				= fun_defs

		last_n_producers_are_empty :: !Int !Int !{!Producer} -> Bool
		last_n_producers_are_empty n i producers
			| n==0
				= True
				# i=i-1
				= i>=0 && producers.[i]=:PR_Empty && last_n_producers_are_empty (n-1) i producers

mark_fused_members_of_implementation_module :: !Int !Int !{#ClassInstance} !{!InstanceInfo} !{#CommonDefs} !FunctionHeap !*{#FunDef} -> *{#FunDef}
mark_fused_members_of_implementation_module instance_i main_dcl_module_n instance_defs instances common_defs fun_heap fun_defs
	| instance_i<size instance_defs
		#! fun_defs = mark_fused_members_of_instance instance_defs.[instance_i] main_dcl_module_n instances common_defs fun_heap fun_defs
		= mark_fused_members_of_implementation_module (instance_i+1) main_dcl_module_n instance_defs instances common_defs fun_heap fun_defs
		= fun_defs

mark_fused_members_of_specialized_instances :: !DclModule !Int !{!InstanceInfo} !{#CommonDefs} !FunctionHeap !*{#.FunDef} -> *{#FunDef}
mark_fused_members_of_specialized_instances {dcl_module_kind=MK_None} main_dcl_module_n instances common_defs fun_heap fun_defs
	= fun_defs
mark_fused_members_of_specialized_instances {dcl_common={com_instance_defs},dcl_sizes} main_dcl_module_n instances common_defs fun_heap fun_defs
	= mark_fused_members_of_instances dcl_sizes.[cInstanceDefs] com_instance_defs instances common_defs fun_heap fun_defs
where
	mark_fused_members_of_instances :: !Int !{#ClassInstanceR .DclInstanceMemberTypeAndFunction} !{!InstanceInfo} !{#CommonDefs} !FunctionHeap !*{#FunDef} -> *{#FunDef}
	mark_fused_members_of_instances instance_i instance_defs instances common_defs fun_heap fun_defs
		| instance_i<size instance_defs
			# instance_def=:{ins_specials} = instance_defs.[instance_i]
			| ins_specials=:SP_TypeOffset _
				#! fun_defs = mark_fused_members_of_instance instance_def main_dcl_module_n instances common_defs fun_heap fun_defs
				= mark_fused_members_of_instances (instance_i+1) instance_defs instances common_defs fun_heap fun_defs
				= mark_fused_members_of_instances (instance_i+1) instance_defs instances common_defs fun_heap fun_defs
			= fun_defs

transformGroups :: !CleanupInfo !Int !Int !Int !Int !*{!Component} !*{!ConsClasses}
					!{#CommonDefs} !{#{#FunType}} !*TypeDefInfos !{#DclModule} !FusionOptions
											 !*{#FunDef} !*ImportedTypes !*VarHeap !*TypeHeaps !*ExpressionHeap !*File !*PredefinedSymbols
	-> (!*{!Component},!ImportedConstructors,!*{#FunDef},!*ImportedTypes,!*VarHeap,!*TypeHeaps,!*ExpressionHeap,!*File,!*PredefinedSymbols)
transformGroups cleanup_info main_dcl_module_n ro_StdStrictLists_module_n def_min def_max groups cons_args
		common_defs imported_funs type_def_infos dcl_mods {compile_with_fusion,generic_fusion}
		fun_defs imported_types var_heap type_heaps symbol_heap error predef_symbols
	#! nr_of_funs = size fun_defs
	# initial_ti =	{ ti_fun_defs		= fun_defs
					, ti_instances		= createArray nr_of_funs II_Empty
					, ti_cons_args		= cons_args
					, ti_new_functions	= []
					, ti_fun_heap		= newHeap
					, ti_var_heap		= var_heap
					, ti_symbol_heap	= symbol_heap
					, ti_type_heaps		= type_heaps
					, ti_type_def_infos	= type_def_infos
					, ti_next_fun_nr	= nr_of_funs
					, ti_cleanup_info	= cleanup_info
					, ti_recursion_introduced	= No
					, ti_error_file		= error
					, ti_predef_symbols	= predef_symbols }

	# groups = [group \\ group <-: groups]
	  transform_fusion1 = if generic_fusion (NoRecProdFusion bitor OnlyGenericFusionMask) (if compile_with_fusion FullFusion NoFusion)
	# (groups, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti)
		= transform_groups 0 groups [] transform_fusion1 common_defs imported_funs imported_types [] [] initial_ti
	# (groups,ti)
		= case generic_fusion of
			True
				# groups = reverse groups
				# transform_fusion2 = if compile_with_fusion FullFusion GenericFullFusionMask
				-> transform_groups_again 0 groups [] transform_fusion2 common_defs imported_funs ti
			False
				-> (groups,ti)
	# groups = {group \\ group <- reverse groups}
	  {ti_fun_defs,ti_new_functions,ti_var_heap,ti_symbol_heap,ti_fun_heap,ti_type_heaps,ti_cleanup_info,ti_instances,ti_predef_symbols,ti_error_file} = ti
	  (fun_defs, imported_types, collected_imports, type_heaps, var_heap)
			= foldSt (expand_abstract_syn_types_in_function_type common_defs) (reverse fun_indices_with_abs_syn_types)
					(ti_fun_defs, imported_types, collected_imports, ti_type_heaps, ti_var_heap)

	  (groups, new_fun_defs, imported_types, collected_imports, type_heaps, var_heap) 
	  		= foldSt (add_new_function_to_group common_defs ti_fun_heap) ti_new_functions
	  				(groups, [], imported_types, collected_imports, type_heaps, var_heap)
	  symbol_heap = foldSt cleanup_attributes ti_cleanup_info ti_symbol_heap
	  fun_defs = { fundef \\ fundef <- [ fundef \\ fundef <-: fun_defs ] ++ new_fun_defs }

	  fun_defs = mark_fused_members_of_implementation_module 0 main_dcl_module_n common_defs.[main_dcl_module_n].com_instance_defs ti_instances common_defs ti_fun_heap fun_defs
	  fun_defs = mark_fused_members_of_specialized_instances dcl_mods.[main_dcl_module_n] main_dcl_module_n ti_instances common_defs ti_fun_heap fun_defs

	= (groups, collected_imports, fun_defs, imported_types, var_heap, type_heaps, symbol_heap, ti_error_file, ti_predef_symbols)
where
	transform_groups :: !Int ![Component] ![Component] !Int !{#CommonDefs} !{#{#FunType}}
						  !*{#{#CheckedTypeDef}} ![Global Int] ![Int] !*TransformInfo
		-> *(![Component],!.{#{#CheckedTypeDef}},![Global Int],![Int],!*TransformInfo)
	transform_groups group_nr [group:groups] acc_groups transform_fusion common_defs imported_funs imported_types collected_imports fun_indices_with_abs_syn_types ti
		# {component_members} = group
		# (ti_fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti_type_heaps, ti_var_heap) 
				= convert_function_types component_members common_defs
						(ti.ti_fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti.ti_type_heaps, ti.ti_var_heap)
		# ti = { ti & ti_fun_defs = ti_fun_defs, ti_type_heaps = ti_type_heaps, ti_var_heap = ti_var_heap }
		# (group_nr,acc_groups,ti) = transform_group common_defs imported_funs group_nr component_members acc_groups transform_fusion ti
		= transform_groups group_nr groups acc_groups transform_fusion common_defs imported_funs imported_types collected_imports fun_indices_with_abs_syn_types ti
	transform_groups group_nr [] acc_groups transform_fusion common_defs imported_funs imported_types collected_imports fun_indices_with_abs_syn_types ti
		= (acc_groups, imported_types, collected_imports, fun_indices_with_abs_syn_types, ti)

	convert_function_types (ComponentMember member members) common_defs s
		# s = convert_function_type common_defs member s
		= convert_function_types members common_defs s
	convert_function_types NoComponentMembers common_defs s
		= s

	transform_groups_again :: !Int ![Component] ![Component] !Int !{#CommonDefs} !{#{#FunType}} !*TransformInfo -> *(![Component],!*TransformInfo)
	transform_groups_again group_nr [group:groups] acc_groups transform_fusion common_defs imported_funs ti
		# {component_members} = group
		# (group_nr,acc_groups,ti) = transform_group common_defs imported_funs group_nr component_members acc_groups transform_fusion ti
		= transform_groups_again group_nr groups acc_groups transform_fusion common_defs imported_funs ti
	transform_groups_again group_nr [] acc_groups transform_fusion common_defs imported_funs ti
		= (acc_groups, ti)

	transform_group :: !{#CommonDefs} !{#{#FunType}} !Int !ComponentMembers !u:[Component] !Int !*TransformInfo -> *(!Int,!u:[Component],!*TransformInfo)
	transform_group common_defs imported_funs group_nr component_members acc_groups transform_fusion ti
		// assign group_nr to component_members
		# ti = assign_groups component_members group_nr ti

		# (skip_group,ti)
			= if (transform_fusion bitand OnlyGenericFusionMask<>0)
					(group_has_no_generic_function component_members ti)
					(False,ti)
		# (previous_new_functions,ti) = ti!ti_new_functions
		# ti & ti_new_functions=[]
		// transform component_members
		# ti = if skip_group
					ti
					(transform_functions component_members common_defs imported_funs ti)
		// partitionate group: need to know added functions for this...
		# (new_functions,ti) = ti!ti_new_functions

		# (new_generated_functions,ti_fun_heap) = remove_unused_used_functions new_functions ti.ti_fun_heap
		# ti & ti_fun_heap = ti_fun_heap,
			   ti_new_functions = new_generated_functions ++ previous_new_functions

		| not (compile_with_fusion || (generic_fusion && not skip_group) || not (isEmpty new_functions))
			= (inc group_nr,[{component_members=component_members}:acc_groups],ti)

		# (new_functions_in_component,ti_fun_heap)
			= determine_new_functions_in_component new_functions ti.ti_fun_heap
		# ti = {ti & ti_fun_heap=ti_fun_heap}
		# (new_groups,ti) = partition_group group_nr (append_ComponentMembers component_members new_functions_in_component) ti
		// reanalyse consumers
		# (cleanup,ti_fun_defs,ti_var_heap,ti_symbol_heap,ti_fun_heap,ti_cons_args,same)
				= reanalyseGroups common_defs imported_funs main_dcl_module_n ro_StdStrictLists_module_n
					new_groups
					ti.ti_fun_defs ti.ti_var_heap ti.ti_symbol_heap ti.ti_fun_heap ti.ti_cons_args
		# ti = {ti 
				& ti_cleanup_info = cleanup ++ ti.ti_cleanup_info
				, ti_fun_defs = ti_fun_defs
				, ti_var_heap = ti_var_heap
				, ti_symbol_heap = ti_symbol_heap
				, ti_fun_heap = ti_fun_heap
				, ti_cons_args = ti_cons_args
				}
		// if wanted reapply transform_group to all found groups
		| not (isEmpty new_functions) || length new_groups > 1 || not same
			# (new_groups,removed_groups) = remove_groups_not_used_by_original_component_members component_members new_groups []
			  ti = mark_unused_functions_in_components removed_groups ti
			= transform_groups` common_defs imported_funs group_nr new_groups acc_groups ti
		// producer annotation for finished components!
		# ti = reannotate_producers group_nr component_members ti
		= (inc group_nr,(reverse new_groups)++acc_groups,ti)
	where
		transform_groups` :: !{#CommonDefs} !{#{#FunType}} !Int ![Component] !u:[Component] !*TransformInfo -> *(!Int,!u:[Component],!*TransformInfo)
		transform_groups` common_defs imported_funs group_nr [] acc_groups ti
			= (group_nr, acc_groups, ti)
		transform_groups` common_defs imported_funs group_nr [{component_members}:groups] acc_groups ti
			# (group_nr,acc_groups,ti) = transform_group common_defs imported_funs group_nr component_members acc_groups transform_fusion ti
			= transform_groups` common_defs imported_funs group_nr groups acc_groups ti

		assign_groups :: !ComponentMembers !Int !*TransformInfo -> *TransformInfo
		assign_groups (ComponentMember member members) group_nr ti
			# ti = {ti & ti_fun_defs.[member].fun_info.fi_group_index = group_nr}
			= assign_groups members group_nr ti
		assign_groups (GeneratedComponentMember member fun_ptr members) group_nr ti=:{ti_fun_heap}
			# (FI_Function gf=:{gf_fun_def=fd}, ti_fun_heap) = readPtr fun_ptr ti_fun_heap
			# fd = {fd & fun_info.fi_group_index = group_nr}
			# ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def=fd}) ti_fun_heap
			# ti = {ti & ti_fun_heap=ti_fun_heap}
			= assign_groups members group_nr ti
		assign_groups NoComponentMembers group_nr ti
			= ti
		
		partition_group :: !.Int !ComponentMembers !*TransformInfo -> *(![Component],!*TransformInfo)
		partition_group group_nr component_members ti
			# {ti_fun_defs=fun_defs, ti_fun_heap=fun_heap, ti_next_fun_nr=max_fun_nr,
			   ti_predef_symbols=predef_symbols, ti_var_heap=var_heap, ti_symbol_heap=expression_heap, ti_error_file} = ti
			# next_group = group_nr
			# error_admin = {ea_file = ti_error_file, ea_loc = [], ea_ok = True }
			# (_,groups,fun_defs,fun_heap,predef_symbols,var_heap,expression_heap,error_admin)
				= partitionateFunctions`` max_fun_nr next_group fun_defs component_members main_dcl_module_n def_min def_max fun_heap predef_symbols var_heap expression_heap error_admin
			# ti = { ti	& ti_fun_defs		= fun_defs
						, ti_fun_heap		= fun_heap
						, ti_predef_symbols	= predef_symbols
						, ti_var_heap		= var_heap
						, ti_symbol_heap	= expression_heap
						, ti_error_file		= error_admin.ea_file }
			= (groups,ti)

		group_has_no_generic_function :: !ComponentMembers !*TransformInfo -> (!Bool,!*TransformInfo)
		group_has_no_generic_function (ComponentMember member members) ti
			| ti.ti_fun_defs.[member].fun_info.fi_properties bitand FI_GenericFun<>0
				= (False,ti)
				= group_has_no_generic_function members ti
		group_has_no_generic_function (GeneratedComponentMember member fun_ptr members) ti
			# (FI_Function {gf_fun_def},ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
			  ti & ti_fun_heap = ti_fun_heap
			| gf_fun_def.fun_info.fi_properties bitand FI_GenericFun<>0
				= (False,ti)
				= group_has_no_generic_function members ti
		group_has_no_generic_function NoComponentMembers ti
			= (True,ti)

		transform_functions :: !ComponentMembers !{#CommonDefs} !{#{#FunType}} !*TransformInfo -> *TransformInfo
		transform_functions (ComponentMember member members) common_defs imported_funs ti
			# (fun_def, ti) = ti!ti_fun_defs.[member]
			  fun_symb = {symb_ident=fun_def.fun_ident, symb_kind=SK_Function {glob_object=member, glob_module=main_dcl_module_n}}
			| transform_fusion bitand GenericFullFusionMask<>0 && fun_def.fun_info.fi_properties bitand FI_GenericFun<>0
				# (fun_body,ti)
					= transform_function fun_def.fun_type fun_def.fun_body fun_symb FullFusion common_defs imported_funs ti
				  ti & ti_fun_defs.[member] = {fun_def & fun_body=fun_body}
				= transform_functions members common_defs imported_funs ti
				# (fun_body,ti)
					= transform_function fun_def.fun_type fun_def.fun_body fun_symb transform_fusion common_defs imported_funs ti
				  ti & ti_fun_defs.[member] = {fun_def & fun_body=fun_body}
				= transform_functions members common_defs imported_funs ti
		transform_functions (GeneratedComponentMember member fun_ptr members) common_defs imported_funs ti
			# (FI_Function gf=:{gf_fun_def},ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
			  fun_symb = {symb_ident=gf_fun_def.fun_ident, symb_kind=SK_GeneratedFunction fun_ptr member}
			  ti & ti_fun_heap = ti_fun_heap
			| transform_fusion bitand GenericFullFusionMask<>0 && gf_fun_def.fun_info.fi_properties bitand FI_GenericFun<>0
				# (fun_body,ti)
					= transform_function gf_fun_def.fun_type gf_fun_def.fun_body fun_symb FullFusion common_defs imported_funs ti
				  ti & ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def={gf_fun_def & fun_body=fun_body}}) ti.ti_fun_heap
				= transform_functions members common_defs imported_funs ti
				# (fun_body,ti)
					= transform_function gf_fun_def.fun_type gf_fun_def.fun_body fun_symb transform_fusion common_defs imported_funs ti
				  ti & ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_fun_def={gf_fun_def & fun_body=fun_body}}) ti.ti_fun_heap
				= transform_functions members common_defs imported_funs ti
		transform_functions NoComponentMembers common_defs imported_funs ti
			= ti

		transform_function :: !(Optional SymbolType) !FunctionBody !SymbIdent !Int !{#CommonDefs} !{#{#FunType}} !*TransformInfo
								-> (!FunctionBody,!*TransformInfo)
		transform_function (Yes {st_args,st_args_strictness}) (TransformedBody tb) fun_symb transform_fusion common_defs imported_funs ti
			# (ro_StdGeneric_module_n,ti) = ti!ti_predef_symbols.[PD_StdGeneric].pds_def
			  ti_var_heap					= fold2St store_arg_type_info tb.tb_args st_args ti.ti_var_heap
			  tfi =	{ tfi_root				= fun_symb
					, tfi_case				= fun_symb
					, tfi_orig				= fun_symb
					, tfi_args				= tb.tb_args
					, tfi_vars				= [arg \\ arg <- tb.tb_args & i <- [0..] | arg_is_strict i st_args_strictness]
					, tfi_n_args_before_producer = -1
					, tfi_n_producer_args	= -1
					}
			  ro =	{ ro_imported_funs				= imported_funs
					, ro_common_defs 				= common_defs
					, ro_root_case_mode				= get_root_case_mode tb
					, ro_tfi						= tfi
					, ro_main_dcl_module_n			= main_dcl_module_n
					, ro_transform_fusion			= transform_fusion
					, ro_StdStrictLists_module_n	= ro_StdStrictLists_module_n
					, ro_StdGeneric_module_n		= ro_StdGeneric_module_n
					}
			  ti = {ti & ti_var_heap = ti_var_heap}
		  
			  (fun_rhs, ti)						= transform tb.tb_rhs ro ti
			= (TransformedBody {tb & tb_rhs = fun_rhs},ti)
		  where
			store_arg_type_info {fv_info_ptr} a_type ti_var_heap
				= setExtendedVarInfo fv_info_ptr (EVI_VarType a_type) ti_var_heap
			
			fun_def_to_symb_ident fun_index fsize {fun_ident}
				| fun_index < fsize
				= { symb_ident=fun_ident, symb_kind=SK_Function {glob_object=fun_index, glob_module=main_dcl_module_n } }

			get_root_case_mode {tb_rhs=Case _}	= RootCase
			get_root_case_mode _ 				= NotRootCase

	reannotate_producers group_nr component_members ti
		// determine if safe group
		# (safe,ti) = safe_producers group_nr component_members component_members main_dcl_module_n ti
		| safe
			// if safe mark all members as safe
			= mark_producers_safe component_members ti
		= ti

	safe_producers :: Int ComponentMembers ComponentMembers Int *TransformInfo -> *(!Bool,!*TransformInfo)
	safe_producers group_nr component_members (ComponentMember fun funs) main_dcl_module_n ti
		// look for occurrence of component_members in safe argument position of fun RHS
		// i.e. linearity ok && ...
		# (fun_def,fun_defs) = (ti.ti_fun_defs)![fun]
		  {fun_body = TransformedBody {tb_rhs}} = fun_def
		  prs =	{ prs_group				= component_members
				, prs_cons_args 		= ti.ti_cons_args
				, prs_main_dcl_module_n	= main_dcl_module_n
				, prs_fun_heap			= ti.ti_fun_heap
				, prs_fun_defs			= fun_defs
				, prs_group_index		= group_nr }
		# (safe,prs) = producerRequirements tb_rhs prs
		#! ti = {ti & ti_fun_defs = prs.prs_fun_defs, ti_fun_heap = prs.prs_fun_heap, ti_cons_args = prs.prs_cons_args}
		// put back prs info into ti?
		| safe
			= safe_producers group_nr component_members funs main_dcl_module_n ti
			= (False,ti)
	safe_producers group_nr component_members (GeneratedComponentMember fun fun_ptr funs) main_dcl_module_n ti
		# (FI_Function {gf_fun_def}, ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
		  ti = {ti & ti_fun_heap=ti_fun_heap}
		  {fun_body = TransformedBody {tb_rhs}} = gf_fun_def
		  prs =	{ prs_group				= component_members
				, prs_cons_args 		= ti.ti_cons_args
				, prs_main_dcl_module_n	= main_dcl_module_n
				, prs_fun_heap			= ti.ti_fun_heap
				, prs_fun_defs			= ti.ti_fun_defs
				, prs_group_index		= group_nr }
		  (safe,prs) = producerRequirements tb_rhs prs
		#! ti = {ti & ti_fun_defs = prs.prs_fun_defs, ti_fun_heap = prs.prs_fun_heap, ti_cons_args = prs.prs_cons_args}
		| safe
			= safe_producers group_nr component_members funs main_dcl_module_n ti
			= (False,ti)
	safe_producers group_nr component_members NoComponentMembers main_dcl_module_n ti
		= (True,ti)

	mark_producers_safe (ComponentMember member members) ti
		# ti = {ti & ti_cons_args.[member].cc_producer = pIsSafe}
		= mark_producers_safe members ti
	mark_producers_safe (GeneratedComponentMember member fun_ptr members) ti
		# (FI_Function gf,ti_fun_heap) = readPtr fun_ptr ti.ti_fun_heap
		  ti_fun_heap = writePtr fun_ptr (FI_Function {gf & gf_cons_args.cc_producer = pIsSafe}) ti_fun_heap
		  ti = {ti & ti_fun_heap = ti_fun_heap}
		= mark_producers_safe members ti
	mark_producers_safe NoComponentMembers ti
		= ti

	add_new_function_to_group ::  !{# CommonDefs} !FunctionHeap  !FunctionInfoPtr
				  !(!*{!Component}, ![FunDef], !*ImportedTypes, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
				-> (!*{!Component}, ![FunDef], !*ImportedTypes, !ImportedConstructors, !*TypeHeaps, !*VarHeap)
	add_new_function_to_group common_defs fun_heap fun_ptr (groups, fun_defs, imported_types, collected_imports, type_heaps, var_heap)
		# (FI_Function {gf_fun_def,gf_fun_index}) = sreadPtr fun_ptr fun_heap
		  {fun_type = Yes ft=:{st_args,st_result}, fun_info = {fi_group_index,fi_properties}} = gf_fun_def
		  ets =
		  	{ ets_type_defs							= imported_types
		  	, ets_collected_conses					= collected_imports
		  	, ets_type_heaps						= type_heaps
		  	, ets_var_heap							= var_heap
		  	, ets_main_dcl_module_n					= main_dcl_module_n
		  	, ets_contains_unexpanded_abs_syn_type	= False
		  	}
		#! (_,(st_args,st_result), {ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap})
				= expandSynTypes (if (fi_properties bitand FI_HasTypeSpec == 0) (RemoveAnnotationsMask bitor ExpandAbstractSynTypesMask) ExpandAbstractSynTypesMask) common_defs (st_args,st_result) ets
		# ft = { ft &  st_result = st_result, st_args = st_args }

		| fi_properties bitand FI_Unused<>0
			# gf_fun_def = {gf_fun_def & fun_type = Yes ft}
			= (groups, [gf_fun_def : fun_defs], ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)

		| fi_group_index >= size groups
			= abort ("add_new_function_to_group "+++ toString fi_group_index+++ "," +++ toString (size groups) +++ "," +++ toString gf_fun_index)

		| not (isComponentMember gf_fun_index groups.[fi_group_index].component_members)
			= abort ("add_new_function_to_group INSANE!\n" +++ toString gf_fun_index +++ "," +++ toString fi_group_index)
		# gf_fun_def = {gf_fun_def & fun_type = Yes ft}
		= (groups, [gf_fun_def : fun_defs], ets_type_defs, ets_collected_conses, ets_type_heaps, ets_var_heap)
	where
		isComponentMember index (ComponentMember member members)
			= index==member || isComponentMember index members
		isComponentMember index (GeneratedComponentMember member _ members)
			= index==member || isComponentMember index members
		isComponentMember index NoComponentMembers
			= False

	convert_function_type common_defs fun_index (fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, type_heaps, var_heap)
		# (fun_def=:{fun_type = Yes fun_type, fun_info = {fi_properties}}, fun_defs)
					= fun_defs![fun_index]
		  rem_annot	= fi_properties bitand FI_HasTypeSpec == 0
		  (fun_type,contains_unexpanded_abs_syn_type,imported_types, collected_imports, type_heaps, var_heap)
		  		= convertSymbolTypeWithoutExpandingAbstractSynTypes rem_annot common_defs fun_type main_dcl_module_n imported_types collected_imports type_heaps var_heap
		# fun_def	= { fun_def & fun_type = Yes fun_type }
		  fun_defs	= { fun_defs & [fun_index] = fun_def }
		| contains_unexpanded_abs_syn_type
			= (fun_defs, imported_types, collected_imports, [fun_index : fun_indices_with_abs_syn_types], type_heaps, var_heap)
			= (fun_defs, imported_types, collected_imports, fun_indices_with_abs_syn_types, type_heaps, var_heap)

	expand_abstract_syn_types_in_function_type :: !{#.CommonDefs} !.Int !*(!*{#FunDef},!*{#{#CheckedTypeDef}},![(Global .Int)],!*TypeHeaps,!*(Heap VarInfo)) -> (!*{#FunDef},!.{#{#CheckedTypeDef}},![(Global Int)],!.TypeHeaps,!.(Heap VarInfo))
	expand_abstract_syn_types_in_function_type common_defs fun_index (fun_defs, imported_types, collected_imports, type_heaps, var_heap)
		# (fun_def=:{fun_type = Yes fun_type, fun_info = {fi_properties}}, fun_defs)
					= fun_defs![fun_index]
		  rem_annot	= fi_properties bitand FI_HasTypeSpec == 0
		  (fun_type,imported_types, collected_imports, type_heaps, var_heap)
	  		= convertSymbolType rem_annot common_defs fun_type main_dcl_module_n imported_types collected_imports type_heaps var_heap
	  	  fun_def = { fun_def & fun_type = Yes fun_type}
	  	  fun_defs = { fun_defs & [fun_index] = fun_def }
		= (fun_defs, imported_types, collected_imports, type_heaps, var_heap)

	append_ComponentMembers :: !ComponentMembers !ComponentMembers -> ComponentMembers
	append_ComponentMembers (ComponentMember member members) component_members_to_append
		= ComponentMember member (append_ComponentMembers members component_members_to_append)
	append_ComponentMembers (GeneratedComponentMember member fun_ptr members) component_members_to_append
		= GeneratedComponentMember member fun_ptr (append_ComponentMembers members component_members_to_append)
	append_ComponentMembers NoComponentMembers component_members_to_append
		= component_members_to_append

	determine_new_functions_in_component :: ![FunctionInfoPtr] !*FunctionHeap -> (ComponentMembers,!*FunctionHeap)
	determine_new_functions_in_component [fun_ptr:new_functions] fun_heap
		# (FI_Function {gf_fun_index},fun_heap) = readPtr fun_ptr fun_heap
		# (members,fun_heap) = determine_new_functions_in_component new_functions fun_heap
		= (GeneratedComponentMember gf_fun_index fun_ptr members,fun_heap)	
	determine_new_functions_in_component [] fun_heap
		= (NoComponentMembers,fun_heap)

//@	freeVariables

class clearVariables expr :: !expr !*VarHeap -> *VarHeap

instance clearVariables [a] | clearVariables a
where
	clearVariables list fvi
		= foldSt clearVariables list fvi

instance clearVariables LetBind
where
	clearVariables {lb_src} fvi
		= clearVariables lb_src fvi

instance clearVariables (Bind a b) | clearVariables a
where
	clearVariables {bind_src} fvi
		= clearVariables bind_src fvi

instance clearVariables (Optional a) | clearVariables a
where
	clearVariables (Yes x) fvi
		= clearVariables x fvi
	clearVariables No fvi
		= fvi

instance clearVariables BoundVar
where
	clearVariables bound_var=:{var_info_ptr} var_heap
		# (var_info, var_heap) = readVarInfo var_info_ptr var_heap
		= case var_info of
			VI_UsedVar _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_LocalVar				-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_Empty				-> var_heap
			VI_Expression _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_Dictionary _ _ _		-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_Variable _ _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_AccVar _ _			-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_ExpressionOrBody _ _	_ _ _ _
				-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_Body _ _ _ _ _
				-> writeVarInfo var_info_ptr VI_Empty var_heap
			VI_NotUsed
				-> var_heap

instance clearVariables Expression
where
	clearVariables (Var var) fvi
		= clearVariables var fvi
	clearVariables (App {app_args}) fvi
		= clearVariables app_args fvi
	clearVariables (fun @ args) fvi
		= clearVariables args (clearVariables fun fvi)
	clearVariables (Let {let_strict_binds,let_lazy_binds,let_expr}) fvi
		# fvi = clearVariables let_strict_binds fvi
		  fvi = clearVariables let_lazy_binds fvi
		  fvi = clearVariables let_expr fvi
		= fvi
	clearVariables (Case {case_expr,case_guards,case_default}) fvi
		# fvi = clearVariables case_expr fvi
		  fvi = clearVariables case_guards fvi
		  fvi = clearVariables case_default fvi
		= fvi
	clearVariables (Selection _ expr selectors) fvi
		= clearVariables expr (clearVariables selectors fvi)
	clearVariables (Update expr1 selectors expr2) fvi
		= clearVariables expr2 (clearVariables selectors (clearVariables expr1 fvi))
	clearVariables (RecordUpdate cons_symbol expression expressions) fvi
		= clearVariables expression (clearVariables expressions fvi)
	clearVariables (TupleSelect _ arg_nr expr) fvi
		= clearVariables expr fvi
	clearVariables (MatchExpr _ expr) fvi
		= clearVariables expr fvi
	clearVariables (IsConstructor expr _ _ _ _ _) fvi
		= clearVariables expr fvi
	clearVariables EE fvi
		= fvi
	clearVariables _ fvi
		= fvi

instance clearVariables CasePatterns
where
	clearVariables (AlgebraicPatterns _ alg_patterns) fvi
		= foldSt clearVariables alg_patterns fvi
	clearVariables (BasicPatterns _ basic_patterns) fvi
		= foldSt clearVariables basic_patterns fvi
	clearVariables (OverloadedListPatterns _ _ alg_patterns) fvi
		= foldSt clearVariables alg_patterns fvi

instance clearVariables BasicPattern
where
	clearVariables {bp_expr} fvi
		= clearVariables bp_expr fvi

instance clearVariables AlgebraicPattern
where
	clearVariables {ap_vars, ap_expr} fvi
		= clearVariables ap_expr fvi
		
instance clearVariables Selection
where
	clearVariables (RecordSelection _ _) fvi
		= fvi
	clearVariables (ArraySelection _ _ expr) fvi
		= clearVariables expr fvi
	clearVariables (DictionarySelection dict_var selections _ expr) fvi
		= clearVariables dict_var (clearVariables selections (clearVariables expr fvi))
	
////////////////

::	FreeVarInfo =
	{	fvi_var_heap	:: !.VarHeap
	,	fvi_expr_heap	:: !.ExpressionHeap
	,	fvi_variables	:: ![BoundVar]
	,	fvi_expr_ptrs	:: ![ExprInfoPtr]
	}

class freeVariables expr ::  !expr !*FreeVarInfo -> *FreeVarInfo

instance freeVariables [a] | freeVariables a
where
	freeVariables list fvi
		= foldSt freeVariables list fvi

instance freeVariables LetBind
where
	freeVariables {lb_src} fvi
		= freeVariables lb_src fvi

instance freeVariables (Bind a b) | freeVariables a
where
	freeVariables {bind_src} fvi
		= freeVariables bind_src fvi

instance freeVariables (Optional a) | freeVariables a
where
	freeVariables (Yes x) fvi
		= freeVariables x fvi
	freeVariables No fvi
		= fvi

instance freeVariables BoundVar
where
	freeVariables bound_var=:{var_info_ptr} fvi=:{fvi_var_heap, fvi_variables}
		# (var_info, fvi_var_heap) = readVarInfo var_info_ptr fvi_var_heap
		  (fvi_variables, fvi_var_heap) = adjust_var_info bound_var var_info fvi_variables fvi_var_heap
		= {fvi & fvi_variables = fvi_variables, fvi_var_heap = fvi_var_heap }
	where
		adjust_var_info _ (VI_UsedVar _) fvi_variables fvi_var_heap
			= (fvi_variables, fvi_var_heap)
		adjust_var_info bound_var=:{var_ident} _ fvi_variables fvi_var_heap
			= ([bound_var : fvi_variables], writeVarInfo var_info_ptr (VI_UsedVar var_ident) fvi_var_heap)

instance freeVariables Expression
where
	freeVariables (Var var) fvi
		= freeVariables var fvi
	freeVariables (App {app_args}) fvi
		= freeVariables app_args fvi
	freeVariables (fun @ args) fvi
		= freeVariables args (freeVariables fun fvi)
	freeVariables (Let {let_strict_binds,let_lazy_binds,let_expr,let_info_ptr}) fvi=:{fvi_variables = global_variables}
		# let_binds = let_strict_binds ++ let_lazy_binds
		  (removed_variables, fvi_var_heap) = removeVariables global_variables fvi.fvi_var_heap
		  fvi = freeVariables let_binds { fvi & fvi_variables = [], fvi_var_heap = fvi_var_heap }
		  {fvi_expr_heap, fvi_variables, fvi_var_heap, fvi_expr_ptrs} = freeVariables let_expr fvi
		  (fvi_variables, fvi_var_heap) = removeLocalVariables [lb_dst \\ {lb_dst} <- let_binds] fvi_variables [] fvi_var_heap		
		  (unbound_variables, fvi_var_heap) = determineGlobalVariables fvi_variables fvi_var_heap
		  (fvi_variables, fvi_var_heap) = restoreVariables removed_variables fvi_variables fvi_var_heap
		  (let_info, fvi_expr_heap) = readPtr let_info_ptr fvi_expr_heap
		= { fvi & fvi_variables = fvi_variables
		  , fvi_var_heap = fvi_var_heap
		  , fvi_expr_heap = fvi_expr_heap
		  , fvi_expr_ptrs = [let_info_ptr : fvi_expr_ptrs]
		  }
	freeVariables (Case kees) fvi
		= freeVariablesOfCase kees fvi
	where
		freeVariablesOfCase {case_expr,case_guards,case_default, case_info_ptr} fvi=:{fvi_variables, fvi_var_heap}
			# (removed_variables, fvi_var_heap) = removeVariables fvi_variables fvi_var_heap
			  fvi = free_variables_of_guards case_guards { fvi & fvi_variables = [], fvi_var_heap = fvi_var_heap }
			  {fvi_expr_heap, fvi_variables, fvi_var_heap, fvi_expr_ptrs} = freeVariables case_default fvi
			  (unbound_variables, fvi_var_heap) = determineGlobalVariables fvi_variables fvi_var_heap
			  (fvi_variables, fvi_var_heap) = restoreVariables removed_variables fvi_variables fvi_var_heap
			  (case_info, fvi_expr_heap) = readPtr case_info_ptr fvi_expr_heap
			= freeVariables case_expr { fvi & fvi_variables = fvi_variables, fvi_var_heap = fvi_var_heap,
					fvi_expr_heap = set_aci_free_vars_info_case unbound_variables case_info_ptr fvi_expr_heap,
					fvi_expr_ptrs = [case_info_ptr : fvi_expr_ptrs] }
		where
			free_variables_of_guards (AlgebraicPatterns _ alg_patterns) fvi
				= foldSt free_variables_of_alg_pattern alg_patterns fvi
			free_variables_of_guards (BasicPatterns _ basic_patterns) fvi
				= foldSt free_variables_of_basic_pattern basic_patterns fvi
			where
				free_variables_of_basic_pattern {bp_expr} fvi
					= freeVariables bp_expr fvi
			free_variables_of_guards (OverloadedListPatterns _ _ alg_patterns) fvi
				= foldSt free_variables_of_alg_pattern alg_patterns fvi
		
			free_variables_of_alg_pattern {ap_vars, ap_expr} fvi=:{fvi_variables}
				# fvi = freeVariables ap_expr { fvi & fvi_variables = [] }
				  (fvi_variables, fvi_var_heap) = removeLocalVariables ap_vars fvi.fvi_variables fvi_variables fvi.fvi_var_heap
				= { fvi & fvi_var_heap = fvi_var_heap, fvi_variables = fvi_variables }
		
	freeVariables (Selection _ expr selectors) fvi
		= freeVariables selectors (freeVariables expr fvi)
	freeVariables (Update expr1 selectors expr2) fvi
		= freeVariables expr2 (freeVariables selectors (freeVariables expr1 fvi))
	freeVariables (RecordUpdate cons_symbol expression expressions) fvi
		= freeVariables expressions (freeVariables expression fvi)
	freeVariables (TupleSelect _ arg_nr expr) fvi
		= freeVariables expr fvi
	freeVariables (MatchExpr _ expr) fvi
		= freeVariables expr fvi
	freeVariables (IsConstructor expr _ _ _ _ _) fvi
		= freeVariables expr fvi
	freeVariables EE fvi
		= fvi
	freeVariables _ fvi
		= fvi

instance freeVariables Selection
where
	freeVariables (RecordSelection _ _) fvi
		= fvi
	freeVariables (ArraySelection _ _ expr) fvi
		= freeVariables expr fvi
	freeVariables (DictionarySelection dict_var selections _ expr) fvi
		= freeVariables dict_var (freeVariables selections (freeVariables expr fvi))
	
removeVariables global_variables var_heap
	= foldSt remove_variable global_variables ([], var_heap)
where
	remove_variable v=:{var_info_ptr} (removed_variables, var_heap)
		# (VI_UsedVar used_var, var_heap) = readVarInfo var_info_ptr var_heap
		= ([(v, used_var) : removed_variables], writeVarInfo var_info_ptr VI_Empty var_heap)

restoreVariables removed_variables global_variables var_heap
	= foldSt restore_variable removed_variables (global_variables, var_heap)
where
	restore_variable (v=:{var_info_ptr}, var_id) (restored_variables, var_heap)
		# (var_info, var_heap) = readVarInfo var_info_ptr var_heap
		= case var_info of
			VI_UsedVar _
				-> (restored_variables, var_heap)
			_
				-> ([ v : restored_variables ], writeVarInfo var_info_ptr (VI_UsedVar var_id) var_heap)

determineGlobalVariables global_variables var_heap
	= foldSt determine_global_variable global_variables ([], var_heap)
where		
	determine_global_variable {var_info_ptr} (global_variables, var_heap)
		# (VI_UsedVar v_name, var_heap) = readVarInfo var_info_ptr var_heap
		= ([{var_ident = v_name, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr} : global_variables], var_heap)

removeLocalVariables local_variables all_variables global_variables var_heap
	# var_heap = foldSt mark_local_var local_variables var_heap
	= foldSt filter_local_var all_variables (global_variables, var_heap)
where
	mark_local_var {fv_info_ptr} var_heap
		= writeVarInfo fv_info_ptr VI_LocalVar var_heap

	filter_local_var v=:{var_info_ptr} (global_vars, var_heap)
		# (var_info, var_heap) = readVarInfo var_info_ptr var_heap
		= case var_info of
			VI_LocalVar
				-> (global_vars, var_heap)
			_
				-> ([ v : global_vars ], var_heap)

//@ fun_def & cons_arg getters...

get_fun_def :: !SymbKind !Int !u:{#FunDef} !*FunctionHeap -> (!FunDef, !u:{#FunDef}, !*FunctionHeap)
get_fun_def (SK_Function {glob_module, glob_object}) main_dcl_module_n fun_defs fun_heap
	| glob_module<>main_dcl_module_n
		= abort "sanity check 2 failed in module trans"
	# (fun_def, fun_defs) = fun_defs![glob_object]
	= (fun_def, fun_defs, fun_heap)
get_fun_def (SK_LocalMacroFunction glob_object) main_dcl_module_n fun_defs fun_heap
	# (fun_def, fun_defs) = fun_defs![glob_object]
	= (fun_def, fun_defs, fun_heap)
get_fun_def (SK_GeneratedFunction fun_ptr _) main_dcl_module_n fun_defs fun_heap
	# (FI_Function {gf_fun_def}, fun_heap) = readPtr fun_ptr fun_heap
	= (gf_fun_def, fun_defs, fun_heap)

get_fun_def_and_cons_args :: !SymbKind !v:{!ConsClasses} !u:{#FunDef} !*FunctionHeap
		    -> (!FunDef, !ConsClasses, !v:{!ConsClasses},!u:{#FunDef},!*FunctionHeap)
get_fun_def_and_cons_args (SK_Function {glob_object}) cons_args fun_defs fun_heap
	# (fun_def, fun_defs) = fun_defs![glob_object]
	# (fun_args, cons_args) = cons_args![glob_object]
	= (fun_def, fun_args, cons_args, fun_defs, fun_heap)
get_fun_def_and_cons_args (SK_LocalMacroFunction glob_object) cons_args fun_defs fun_heap
	# (fun_def, fun_defs) = fun_defs![glob_object]
	# (fun_args, cons_args) = cons_args![glob_object]
	= (fun_def, fun_args, cons_args, fun_defs, fun_heap)
get_fun_def_and_cons_args (SK_GeneratedFunction fun_info_ptr fun_index) cons_args fun_defs fun_heap
	| fun_index < size fun_defs
		# (fun_def, fun_defs) = fun_defs![fun_index]
		# (fun_args, cons_args) = cons_args![fun_index]
		= (fun_def, fun_args, cons_args, fun_defs, fun_heap)
	# (FI_Function {gf_fun_def, gf_cons_args}, fun_heap) = readPtr fun_info_ptr fun_heap
	= (gf_fun_def, gf_cons_args, cons_args, fun_defs, fun_heap)

//@ <<<

instance <<< RootCaseMode where
	(<<<) file mode = case mode of NotRootCase -> file <<< "NotRootCase"; RootCase -> file <<< "RootCase"; RootCaseOfZombie -> file <<< "RootCaseOfZombie";

/*
instance <<< InstanceInfo
where
	(<<<) file (II_Node prods _ left right) = file <<< left <<< prods <<< right 
	(<<<) file II_Empty = file 

instance <<< Producer
where
	(<<<) file (PR_Function symbol _ index)
		= file <<< "(F)" <<< symbol.symb_ident
	(<<<) file (PR_GeneratedFunction symbol _ index)
		= file <<< "(G)" <<< symbol.symb_ident <<< index
	(<<<) file PR_Empty = file <<< 'E'
	(<<<) file (PR_Class app vars type) = file <<< "(Class(" <<< App app<<<","<<< type <<< "))"
	(<<<) file (PR_Curried {symb_ident, symb_kind} _) = file <<< "(Curried)" <<< symb_ident <<< symb_kind
	(<<<) file _ = file
*/
instance <<< Producer where
	(<<<) file PR_Empty
		= file <<< "(E)"
	(<<<) file PR_Unused
		= file <<< "(U)"
	(<<<) file (PR_Function ident int index)
		= file <<< "(F:" <<< ident <<< ")"
	(<<<) file (PR_Class app binds type)
		= file <<< "(O::" <<< app.app_symb <<< ")"
	(<<<) file (PR_Constructor ident int exprl)
		= file <<< "(C:" <<< ident <<< ")"
	(<<<) file (PR_GeneratedFunction ident arity index)
		= file <<< "(G:" <<< ident <<< ' ' <<< arity <<< ")"
	(<<<) file (PR_Curried ident arity)
		= file <<< "(P:" <<< ident <<< ' ' <<< arity <<< ")"
	(<<<) file (PR_CurriedFunction ident arity index)
		= file <<< "(CF:" <<< ident <<< ' ' <<< arity <<< ")"
	(<<<) file (PR_String _)
		= file <<< "(S)"
	(<<<) file (PR_Int _)
		= file <<< "(I)"
	(<<<) file PR_Equal
		= file <<< "(=)"
	(<<<) file (PR_EqualRemove i)
		= file <<< "(=R" <<< i <<< ')'

instance <<< {!a} | <<< a
where
	(<<<) file array
		# file = file <<< "{"
		= showBody 0 (size array) array file
	where
		showBody i m a f
			| i >= m	= f <<< "}"
						= showBody (inc i) m a (f <<< a.[i] <<< ", ")

instance <<< SymbKind
where
	(<<<) file SK_Unknown = file <<< "(SK_Unknown)"
	(<<<) file (SK_Function gi) = file <<< "(SK_Function)" <<< gi
	(<<<) file (SK_IclMacro gi) = file <<< "(SK_IclMacro)" <<< gi
	(<<<) file (SK_LocalMacroFunction gi) = file <<< "(SK_LocalMacroFunction)" <<< gi
	(<<<) file (SK_DclMacro gi) = file <<< "(SK_DclMacro)" <<< gi
	(<<<) file (SK_LocalDclMacroFunction gi) = file <<< "(SK_LocalDclMacroFunction)" <<< gi
	(<<<) file (SK_OverloadedFunction gi) = file <<< "(SK_OverloadedFunction)" <<< gi
	(<<<) file (SK_GeneratedFunction _ gi) = file <<< "(SK_GeneratedFunction)" <<< gi
	(<<<) file (SK_Constructor gi) = file <<< "(SK_Constructor)" <<< gi
	(<<<) file (SK_Generic gi tk) = file <<< "(SK_Constructor)" <<< gi
	(<<<) file SK_TypeCode = file <<< "(SK_TypeCode)"
	(<<<) file _ = file <<< "(SK_UNKNOWN)"
	
instance <<< ConsClasses
where
	(<<<) file {cc_args,cc_linear_bits,cc_producer} = file <<< cc_args <<< cc_linear_bits <<< cc_producer

instance <<< [#a!] | UTSList,<<< a
where
	(<<<) file [|] = file <<< "[]"
	(<<<) file l  = showTail (file <<< "[") l
	where
		showTail f [|x]   = f <<< x <<< "] "
		showTail f [|a:x] = showTail (f <<< a <<< ", ") x
		showTail f [|]    = f <<< "] "

instance <<< InstanceInfo
  where
	(<<<) file ii = (write_ii ii (file <<< "[")) <<< "]"
	  where
		write_ii II_Empty file
			= file
		write_ii (II_Node producers _ l r) file
			# file = write_ii l file <<< "("
			  file = foldSt (\pr file -> file<<<pr<<<",") [el \\ el<-:producers] file
			= write_ii r (file<<<")")

instance <<< (Ptr a)
where
	(<<<) file p = file <<< ptrToInt p

instance <<< SymbIdent
where
	(<<<) file symb=:{symb_kind = SK_Function symb_index }
		= file <<< symb.symb_ident <<<  '@' <<< symb_index
	(<<<) file symb=:{symb_kind = SK_LocalMacroFunction symb_index }
		= file <<< symb.symb_ident <<<  '@' <<< symb_index
	(<<<) file symb=:{symb_kind = SK_GeneratedFunction _ symb_index }
		= file <<< symb.symb_ident <<<  '@' <<< symb_index
	(<<<) file symb=:{symb_kind = SK_OverloadedFunction symb_index }
		= file <<< symb.symb_ident <<<  "[o]@" <<< symb_index
	(<<<) file symb
		= file <<< symb.symb_ident 
/*
instance <<< {!Type}
where
	(<<<) file subst
		= file <<< "{"<<<[s\\s<-:subst] <<< "}\n"
*/
// SPECIAL...
instance <<< Specials
where
	(<<<) file spec = case spec of
		SP_None							-> file <<< "SP_None"
		(SP_ParsedSubstitutions 	_)	-> file <<< "SP_ParsedSubstitutions"
		(SP_Substitutions 		 	_)	-> file <<< "SP_Substitutions"
		(SP_ContextTypes			l)	-> file <<< "(SP_ContextTypes: " <<< l <<< ")"
		(SP_TypeOffset				_)	-> file <<< "SP_TypeOffset"
		SP_GenerateRecordInstances		-> file <<< "SP_GenerateRecordInstances"

instance <<< Special
where
	(<<<) file {spec_index,spec_types,spec_vars,spec_attrs}
		= file <<< "spec_index" <<< spec_index <<< "spec_types" <<< spec_types <<< "spec_vars" <<< spec_vars <<< "spec_attrs" <<< spec_attrs

instance <<< ExprInfo
where
	(<<<) file EI_Empty = file <<< "EI_Empty"
	(<<<) file (EI_DictionaryType t) = file <<< "<EI_DictionaryType: " <<< t <<< ">"
//	(<<<) file (EI_Instance symb exprs) = file <<< symb <<< exprs
//	(<<<) file (EI_Selection sels var_ptr exprs) = file <<< sels <<< var_ptr <<< exprs
//	(<<<) file (EI_Context exprs) = file <<< exprs
	(<<<) file _ = file <<< "EI_Other"

instance <<< TypeContext
where
	(<<<) file co = file <<< co.tc_class <<< " " <<< co.tc_types <<< " <" <<< co.tc_var <<< '>'

resolveContext :: ![TypeContext] ![ExprInfo] !{#CommonDefs} -> [[Type]]
resolveContext [tc:tcs] [EI_DictionaryType t:eis] common_defs
	= minimiseContext tc t common_defs ++ resolveContext tcs eis common_defs
resolveContext _ _ common_defs
	= []

minimiseContext :: !TypeContext !Type !{#CommonDefs} -> [[Type]]
minimiseContext {tc_class = TCClass gds} (TA ti=:{type_index} ts) common_defs
	# class_module_index = gds.glob_module
	# dictionary_index = common_defs.[class_module_index].com_class_defs.[gds.glob_object.ds_index].class_dictionary.ds_index
	| type_index.glob_module==class_module_index && type_index.glob_object==dictionary_index
		= [[at_type \\ {at_type} <- ts]]
		= []
minimiseContext _ _ common_defs
	= []

findInstInSpecials :: ![[Type]] ![Special] -> (!Int,!Global Int)
findInstInSpecials insts []
	= (0,{glob_object= -1,glob_module = -1})
findInstInSpecials insts [{spec_types,spec_index}:specials]
	| matchTypes insts spec_types
		= (length spec_types, spec_index)
	= findInstInSpecials insts specials

matchTypes :: ![[Type]] ![[Type]] -> Bool
matchTypes [] [] = True
matchTypes [[TA ltsi [],_]:ls] [[TA rtsi [],TV _]:rs]
	// second arg is contexts of special, a TV can only occur as lazy or boxed element of an array or list
	= rtsi==ltsi && matchTypes ls rs
matchTypes [l:ls] [r:rs]
	= l==r && matchTypes ls rs
matchTypes _ _ = False

foundSpecial {glob_object= -1,glob_module = -1}	= False
foundSpecial _ = True	

// ...SPECIAL

arity_warning msg symb_ident fun_index fun_arity ti
	| fun_arity <= 32
		= ti
	= {ti & ti_error_file = ti.ti_error_file <<< "Warning: Arity > 32 " <<< msg <<< " " <<< fun_arity <<< " " <<< symb_ident <<< "@" <<< fun_index <<< "\n"}

strip_universal_quantor :: SymbolType -> SymbolType
strip_universal_quantor st=:{st_vars,st_args,st_result}
	# (st_result,st_vars)	= strip st_result st_vars
	# (st_args,st_vars)		= mapSt strip st_args st_vars
	= {st & st_vars = st_vars, st_args = st_args, st_result = st_result}
where
	strip :: AType [TypeVar] -> (AType,[TypeVar])
	strip atype=:{at_type = TFA vars type} tvs
		= ({atype & at_type = type}, map (\{atv_variable}->atv_variable) vars ++ tvs)
	strip atype=:{at_type = TFAC vars type contexts} tvs
		= ({atype & at_type = type}, map (\{atv_variable}->atv_variable) vars ++ tvs)
	strip atype tvs
		= (atype,tvs)

mapOpt f [Yes a:x]	= [Yes (f a):mapOpt f x]
mapOpt f [No:x]		= [No:mapOpt f x]
mapOpt f [] 		= []

class copy a :: !a !CopyInfo !*CopyState -> (!a, !*CopyState)

instance copy Expression
where
	copy (Var var) ci cs
		= copyVariable var ci cs
	copy (App app) ci cs
		# (app, cs) = copy app ci cs
		= (App app, cs)
	copy (expr @ exprs) ci cs
		# ((expr,exprs), cs) = copy (expr,exprs) ci cs
		= (expr @ exprs, cs)
	copy (Let lad) ci cs
		# (lad, cs) = copyLet lad No ci cs
		= (Let lad, cs)
	copy (Case case_expr) ci cs
		# (case_expr, cs) = copyCase case_expr No ci cs
		= (Case case_expr, cs)
	copy (Selection selector_kind=:NormalSelector (Var var) selectors=:[RecordSelection _ field_n]) ci cs	
		# (var_info,var_heap) = readVarInfo var.var_info_ptr cs.cs_var_heap
		  cs = {cs & cs_var_heap=var_heap}
		= case var_info of
			VI_Expression expr
				-> (Selection selector_kind expr selectors, cs)
			VI_Variable var_ident var_info_ptr
			 	# (var_expr_ptr, cs_symbol_heap) = newPtr EI_Empty cs.cs_symbol_heap
				  expr = Var {var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr}
				-> (Selection selector_kind expr selectors, {cs & cs_symbol_heap = cs_symbol_heap})
			VI_Dictionary app_symb app_args class_type
				# (expr,cs) = copy_dictionary_variable app_symb app_args class_type ci cs
				-> (Selection selector_kind expr selectors, cs)
			VI_Body fun_ident {tb_args, tb_rhs} new_aci_params original_type_vars new_type_vars
				# tb_args_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-tb_args ] 
				  (original_bindings, cs_var_heap) = mapSt readPtr tb_args_ptrs cs.cs_var_heap
				  cs_var_heap = bind_vars tb_args_ptrs new_aci_params cs_var_heap
				  cs = { cs & cs_var_heap = cs_var_heap }
				-> case tb_rhs of
					App {app_symb={symb_kind=SK_Constructor _},app_args}
						# (expr,cs) = copy (app_args!!field_n) ci cs
						  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
						-> (expr, {cs & cs_var_heap = cs_var_heap})
					_
						# (expr,cs) = copy tb_rhs ci cs
						  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
						-> (Selection selector_kind expr selectors, {cs & cs_var_heap = cs_var_heap})
			VI_ExpressionOrBody expr _ _ _ _ _
				-> (Selection selector_kind expr selectors, cs)
			_
				-> (Selection selector_kind (Var var) selectors, cs)
	copy (Selection selector_kind expr selectors) ci cs
		# ((expr, selectors), cs) = copy (expr, selectors) ci cs
		= (Selection selector_kind expr selectors, cs)
	copy (Update expr1 selectors expr2) ci cs
		# (((expr1, expr2), selectors), cs) = copy ((expr1, expr2), selectors) ci cs
		= (Update expr1 selectors expr2, cs)
	copy (RecordUpdate cons_symbol expression expressions) ci cs
		# ((expression, expressions), cs) = copy (expression, expressions) ci cs
		= (RecordUpdate cons_symbol expression expressions, cs)
	copy (TupleSelect symbol argn_nr expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (TupleSelect symbol argn_nr expr, cs)
	copy (MatchExpr cons_ident expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (MatchExpr cons_ident expr, cs)
	copy (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) ci cs
		# (expr, cs) = copy expr ci cs
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, cs)
	copy (DynamicExpr expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (DynamicExpr expr, cs)
	copy (TypeSignature type_function expr) ci cs
		# (expr, cs) = copy expr ci cs
		= (TypeSignature type_function expr, cs)
	copy (DictionariesFunction dictionaries expr expr_type) ci cs
		// the variables in dictionaries are not copied
		# (expr, cs) = copy expr ci cs
		= (DictionariesFunction dictionaries expr expr_type,cs)
	copy expr ci cs
		= (expr, cs)

copyVariable :: !BoundVar CopyInfo !*CopyState -> (!Expression, !*CopyState)
copyVariable var=:{var_info_ptr} ci cs
	# (var_info,var_heap) = readVarInfo var_info_ptr cs.cs_var_heap
	  cs = {cs & cs_var_heap=var_heap}
	= case var_info of
		VI_Expression expr
			-> (expr, cs)
		VI_Variable var_ident var_info_ptr
			# (var_expr_ptr, cs_symbol_heap) = newPtr EI_Empty cs.cs_symbol_heap
			-> (Var {var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr}, { cs & cs_symbol_heap = cs_symbol_heap})
		VI_Body fun_ident _ vars _ _
			-> (App {	app_symb = fun_ident,
						app_args = [ Var { var_ident=fv_ident, var_info_ptr=fv_info_ptr, var_expr_ptr=nilPtr }
									\\ {fv_ident,fv_info_ptr}<-vars],
						app_info_ptr = nilPtr }, cs)
		VI_Dictionary app_symb app_args class_type
			-> copy_dictionary_variable app_symb app_args class_type ci cs
		VI_ExpressionOrBody expr _ _ _ _ _
			-> (expr, cs)
		VI_NotUsed
			-> (ExprToBeRemoved, cs)
		_
			-> (Var var, cs)

copy_dictionary_variable app_symb app_args class_type ci cs
	# (new_class_type, cs_opt_type_heaps) = substitute_class_types class_type cs.cs_opt_type_heaps
	  (new_info_ptr, cs_symbol_heap) = newPtr (EI_DictionaryType new_class_type) cs.cs_symbol_heap
	  app = App { app_symb = app_symb, app_args = app_args, app_info_ptr = new_info_ptr }
	  cs = { cs & cs_opt_type_heaps = cs_opt_type_heaps, cs_symbol_heap = cs_symbol_heap }
	= copy app ci cs
  where
	substitute_class_types class_types No
		= (class_types, No)
	substitute_class_types class_types (Yes type_heaps)
		# (_, new_class_types, type_heaps) = substitute class_types type_heaps
		= (new_class_types, Yes type_heaps)

instance copy DynamicExpr
where
	copy expr=:{dyn_expr, dyn_info_ptr} ci cs=:{cs_symbol_heap}
		# (dyn_info, cs_symbol_heap) = readPtr dyn_info_ptr cs_symbol_heap
		# (new_dyn_info_ptr, cs_symbol_heap) = newPtr dyn_info cs_symbol_heap
		# (dyn_expr, cs) = copy dyn_expr ci {cs & cs_symbol_heap=cs_symbol_heap}
		= ({ expr & dyn_expr = dyn_expr, dyn_info_ptr = new_dyn_info_ptr }, cs)

instance copy Selection
where
	copy (ArraySelection array_select expr_ptr index_expr) ci cs=:{cs_symbol_heap}
		# (new_ptr, cs_symbol_heap) = newPtr EI_Empty cs_symbol_heap
		  (index_expr, cs) = copy index_expr ci { cs & cs_symbol_heap = cs_symbol_heap}
		= (ArraySelection array_select new_ptr index_expr, cs)
	copy (DictionarySelection var selectors expr_ptr index_expr) ci cs=:{cs_symbol_heap}
		# (new_ptr, cs_symbol_heap) = newPtr EI_Empty cs_symbol_heap
		  (index_expr, cs) = copy index_expr ci { cs & cs_symbol_heap = cs_symbol_heap}
		  (var_expr, cs) = copyVariable var ci cs
		= case var_expr of 
			App {app_symb={symb_kind= SK_Constructor _ }, app_args}
				# [RecordSelection _ field_index:_] = selectors
				  (App { app_symb = {symb_ident, symb_kind = SK_Function array_select}}) =  app_args !! field_index
				-> (ArraySelection { array_select & glob_object = { ds_ident = symb_ident, ds_arity = 2, ds_index = array_select.glob_object}}
							new_ptr index_expr, cs)
			Var var
				-> (DictionarySelection var selectors new_ptr index_expr, cs)
	copy record_selection ci cs
		= (record_selection, cs)

instance copy FreeVar
where
	copy fv=:{fv_info_ptr,fv_ident} ci cs=:{cs_var_heap}
		# (new_info_ptr, cs_var_heap) = newPtr VI_Empty cs_var_heap
		= ({fv & fv_info_ptr = new_info_ptr}, {cs & cs_var_heap = writeVarInfo fv_info_ptr (VI_Variable fv_ident new_info_ptr) cs_var_heap})

instance copy App
where
	copy app=:{app_symb={symb_kind}, app_args, app_info_ptr} ci cs
		= case symb_kind of
			SK_Function {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_IclMacro macro_index
				-> copy_function_app app ci cs
			SK_DclMacro {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_OverloadedFunction {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_Generic {glob_module,glob_object} kind
				-> copy_function_app app ci cs
			SK_LocalMacroFunction local_macro_function_n
				-> copy_function_app app ci cs
			SK_LocalDclMacroFunction {glob_module,glob_object}
				-> copy_function_app app ci cs
			SK_Constructor _
				| not (isNilPtr app_info_ptr)
					# (app_info, cs_symbol_heap) = readPtr app_info_ptr cs.cs_symbol_heap
					  (new_app_info, cs_opt_type_heaps) = substitute_EI_DictionaryType app_info cs.cs_opt_type_heaps
					  (new_info_ptr, cs_symbol_heap) = newPtr new_app_info cs_symbol_heap
					  cs={ cs & cs_symbol_heap = cs_symbol_heap, cs_opt_type_heaps = cs_opt_type_heaps }
					  (app_args, cs) = copy app_args ci cs
					-> ({ app & app_args = app_args, app_info_ptr = new_info_ptr}, cs) 
					# (app_args, cs) = copy app_args ci cs
					-> ({ app & app_args = app_args}, cs)
			_
				# (app_args, cs) = copy app_args ci cs
				-> ({ app & app_args = app_args, app_info_ptr = nilPtr}, cs) 
	where
		copy_function_app app=:{app_args, app_info_ptr} ci cs
			# (new_info_ptr, cs_symbol_heap) = newPtr EI_Empty cs.cs_symbol_heap
			# cs={ cs & cs_symbol_heap = cs_symbol_heap }
			# (app_args, cs) = copy app_args ci cs
			= ({ app & app_args = app_args, app_info_ptr = new_info_ptr}, cs) 

		substitute_EI_DictionaryType (EI_DictionaryType class_type) (Yes type_heaps)
			# (_, new_class_type, type_heaps) = substitute class_type type_heaps
			= (EI_DictionaryType new_class_type, Yes type_heaps)
		substitute_EI_DictionaryType x opt_type_heaps
			= (x, opt_type_heaps)

instance copy LetBind
where
	copy bind=:{lb_src} ci cs
		# (lb_src, cs) = copy lb_src ci cs
		= ({ bind & lb_src = lb_src }, cs)

instance copy (Bind a b) | copy a
where
	copy bind=:{bind_src} ci cs
		# (bind_src, cs) = copy bind_src ci cs
		= ({ bind & bind_src = bind_src }, cs)

copyCaseAlt (Let lad) opt_result_type ci cs
	# (lad, cs) = copyLet lad opt_result_type ci cs
	= (Let lad, cs)
copyCaseAlt (Case case_expr) opt_result_type ci cs
	# (case_expr, cs) = copyCase case_expr opt_result_type ci cs
	= (Case case_expr, cs)
copyCaseAlt expr opt_result_type ci cs
	= copy expr ci cs

copyOptCaseAlt (Yes expr) opt_result_type ci cs
	# (expr,cs) = copyCaseAlt expr opt_result_type ci cs
	= (Yes expr, cs)
copyOptCaseAlt No opt_result_type ci cs
	= (No, cs)

copyCase :: !Case !(Optional AType) !CopyInfo !*CopyState -> (!Case, !*CopyState)
copyCase kees=:{case_expr,case_guards,case_default,case_info_ptr} opt_result_type ci cs=:{cs_cleanup_info}
	# (old_case_info, cs_symbol_heap) = readPtr case_info_ptr cs.cs_symbol_heap
	  (new_case_info, opt_result_type, cs_opt_type_heaps) = substitute_case_type old_case_info opt_result_type cs.cs_opt_type_heaps
	  (new_info_ptr, cs_symbol_heap) = newPtr new_case_info cs_symbol_heap
	  cs_cleanup_info = case old_case_info of
							EI_Extended _ _	-> [new_info_ptr:cs_cleanup_info]
							_				-> cs_cleanup_info
	  cs = { cs & cs_symbol_heap = cs_symbol_heap, cs_opt_type_heaps = cs_opt_type_heaps, cs_cleanup_info=cs_cleanup_info }
	  (case_guards, cs) = copyCasePatterns case_guards opt_result_type ci cs
	  (case_default, cs) = copyOptCaseAlt case_default opt_result_type ci cs
	  (case_expr, cs) = update_active_case_info_and_copy case_expr new_info_ptr cs
	= ({ kees & case_expr = case_expr,case_guards = case_guards, case_default = case_default, case_info_ptr = new_info_ptr}, cs)
where
	update_active_case_info_and_copy case_expr=:(Var {var_info_ptr}) case_info_ptr cs
		# (case_info, cs_symbol_heap) = readPtr case_info_ptr cs.cs_symbol_heap
		  cs = { cs & cs_symbol_heap = cs_symbol_heap }
		= case case_info of
			EI_Extended (EEI_ActiveCase aci=:{aci_free_vars}) ei
				# (new_aci_free_vars, cs) = case ci.ci_handle_aci_free_vars of
												LeaveAciFreeVars
													-> (aci_free_vars, cs)
												RemoveAciFreeVars
													-> (No, cs)
												SubstituteAciFreeVars
													-> case aci_free_vars of
															No		-> (No, cs)
															Yes fvs	# (fvs_subst, cs) = mapSt copyBoundVar fvs cs
																	-> (Yes fvs_subst, cs)
				  (var_info,var_heap) = readVarInfo var_info_ptr cs.cs_var_heap
				  cs = {cs & cs_var_heap=var_heap}
				-> case var_info of
					VI_Body fun_ident {tb_args, tb_rhs} new_aci_params original_type_vars new_type_vars
						# (old_original_type_vars_values,cs_opt_type_heaps)
							= forward_old_type_vars_to_new_type_vars original_type_vars new_type_vars cs.cs_opt_type_heaps
						  // replacing the type variables is only necessary if the consumer is the same function as the producer
						  tb_args_ptrs = [ fv_info_ptr \\ {fv_info_ptr}<-tb_args ] 
						  (original_bindings, cs_var_heap) = mapSt readPtr tb_args_ptrs cs.cs_var_heap
						  cs_var_heap = bind_vars tb_args_ptrs new_aci_params cs_var_heap
						  cs & cs_var_heap = cs_var_heap, cs_opt_type_heaps = cs_opt_type_heaps
						  (tb_rhs, cs) = copy tb_rhs ci cs
						  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
						  new_aci = { aci & aci_params = new_aci_params, aci_opt_unfolder = Yes fun_ident, aci_free_vars = new_aci_free_vars }
						  new_eei = (EI_Extended (EEI_ActiveCase new_aci) ei)
						  cs_symbol_heap = writePtr case_info_ptr new_eei cs.cs_symbol_heap
						  cs_opt_type_heaps = restore_old_type_vars_values original_type_vars old_original_type_vars_values cs.cs_opt_type_heaps
						-> (tb_rhs, {cs & cs_var_heap = cs_var_heap, cs_symbol_heap = cs_symbol_heap, cs_opt_type_heaps = cs_opt_type_heaps})
					_	# new_eei = EI_Extended (EEI_ActiveCase { aci & aci_free_vars = new_aci_free_vars }) ei
						  cs_symbol_heap = writePtr case_info_ptr new_eei cs.cs_symbol_heap
						-> copy case_expr ci { cs & cs_symbol_heap = cs_symbol_heap }
			_	-> copy case_expr ci cs
	update_active_case_info_and_copy (Var var=:{var_info_ptr} @ exprs) case_info_ptr cs
		# (exprs,cs) = copy exprs ci cs
		| is_var_list exprs
			# (var_info,var_heap) = readVarInfo var_info_ptr cs.cs_var_heap
			  cs & cs_var_heap=var_heap
			= case var_info of
				VI_ExpressionOrBody _ fun_ident {tb_args, tb_rhs} new_aci_params original_type_vars new_type_vars
					# (old_original_type_vars_values,cs_opt_type_heaps)
						= forward_old_type_vars_to_new_type_vars original_type_vars new_type_vars cs.cs_opt_type_heaps
					  // replacing the type variables is only necessary if the consumer is the same function as the producer
					  tb_args_ptrs = [fv_info_ptr \\ {fv_info_ptr}<-tb_args] 
					  (original_bindings, cs_var_heap) = mapSt readPtr tb_args_ptrs cs.cs_var_heap
					  (extra_exprs,cs_var_heap) = bind_variables tb_args_ptrs new_aci_params exprs cs_var_heap
					  cs & cs_var_heap = cs_var_heap, cs_opt_type_heaps = cs_opt_type_heaps
					  (expr,cs) = copy tb_rhs ci cs

					  (case_info, cs_symbol_heap) = readPtr case_info_ptr cs.cs_symbol_heap
					  cs & cs_symbol_heap
						= case case_info of
							EI_Extended (EEI_ActiveCase aci) ei
								# aci & aci_opt_unfolder = No
								-> writePtr case_info_ptr (EI_Extended (EEI_ActiveCase aci) ei) cs_symbol_heap
							_
								-> cs_symbol_heap

					  cs_var_heap = fold2St writePtr tb_args_ptrs original_bindings cs.cs_var_heap
					  cs_opt_type_heaps = restore_old_type_vars_values original_type_vars old_original_type_vars_values cs.cs_opt_type_heaps
					  cs & cs_var_heap = cs_var_heap, cs_opt_type_heaps = cs_opt_type_heaps
					-> case extra_exprs of
						[]
							-> (expr,cs)
						extra_exprs
							-> (expr @ extra_exprs, cs)
					where
						bind_variables :: [VarInfoPtr] [FreeVar] [Expression] *VarHeap -> (![Expression],!*VarHeap)
						bind_variables [fv_info_ptr:arg_ptrs] [{fv_ident=name, fv_info_ptr=info_ptr}:new_aci_params] exprs var_heap
							# (exprs,var_heap) = bind_variables arg_ptrs new_aci_params exprs var_heap
							# var_heap = writeVarInfo fv_info_ptr (VI_Expression (Var {var_ident=name, var_info_ptr=info_ptr, var_expr_ptr = nilPtr})) var_heap
							= (exprs,var_heap)
						bind_variables arg_ptrs=:[_:_] [] exprs var_heap
							= bind_variables_for_exprs arg_ptrs exprs var_heap
						bind_variables [] [] exprs var_heap
							= (exprs,var_heap)

						bind_variables_for_exprs :: [VarInfoPtr] [Expression] *VarHeap -> (![Expression],!*VarHeap)
						bind_variables_for_exprs [fv_info_ptr:arg_ptrs] [Var {var_ident=name, var_info_ptr=info_ptr}:exprs] var_heap
							# (exprs,var_heap) = bind_variables_for_exprs arg_ptrs exprs var_heap
							# var_heap = writeVarInfo fv_info_ptr (VI_Expression (Var {var_ident=name, var_info_ptr=info_ptr, var_expr_ptr = nilPtr})) var_heap
							= (exprs,var_heap)
						bind_variables_for_exprs [] exprs var_heap
							= (exprs,var_heap)
				_
					# (expr,cs) = copyVariable var ci cs
					-> (expr @ exprs, cs)
			# (expr,cs) = copyVariable var ci cs
			= (expr @ exprs, cs)
		where
			is_var_list [Var _:exprs] = is_var_list exprs
			is_var_list [_ : _] = False
			is_var_list [] = True
	update_active_case_info_and_copy case_expr _ cs
		= copy case_expr ci cs

	copyBoundVar {var_info_ptr} cs
		# (VI_Expression (Var act_var), cs_var_heap) = readPtr var_info_ptr cs.cs_var_heap
		= (act_var, { cs & cs_var_heap = cs_var_heap })

bind_vars dest_info_ptrs src_free_vars var_heap
	= fold2St bind dest_info_ptrs src_free_vars var_heap
where
	bind fv_info_ptr {fv_ident=name, fv_info_ptr=info_ptr} var_heap
		= writeVarInfo fv_info_ptr (VI_Expression (Var {var_ident=name, var_info_ptr=info_ptr, var_expr_ptr = nilPtr})) var_heap

forward_old_type_vars_to_new_type_vars :: ![TypeVar] ![TypeVar] !*(Optional *TypeHeaps) -> (![TypeVarInfo],!*Optional *TypeHeaps)
forward_old_type_vars_to_new_type_vars original_type_vars new_type_vars No
	= ([],No)
forward_old_type_vars_to_new_type_vars original_type_vars new_type_vars (Yes type_heaps)
	# (old_type_vars_values,th_vars) = forward_old_type_vars_to_new_type_vars original_type_vars new_type_vars type_heaps.th_vars
	= (old_type_vars_values,Yes {type_heaps & th_vars=th_vars})
where
	forward_old_type_vars_to_new_type_vars :: ![TypeVar] ![TypeVar] !*TypeVarHeap -> (![TypeVarInfo],!*TypeVarHeap)
	forward_old_type_vars_to_new_type_vars [original_type_var:original_type_vars] [new_type_var:new_type_vars] type_var_heap
		# (old_type_vars_values,type_var_heap) = forward_old_type_vars_to_new_type_vars original_type_vars new_type_vars type_var_heap
		# (old_type_var_value,type_var_heap) = readPtr original_type_var.tv_info_ptr type_var_heap
		# (new_type_var_value,type_var_heap) = readPtr new_type_var.tv_info_ptr type_var_heap
		= case new_type_var_value of
			TVI_Type type
				# type_var_heap = writePtr original_type_var.tv_info_ptr new_type_var_value type_var_heap
				-> ([old_type_var_value:old_type_vars_values],type_var_heap)
			_
				# type_var_heap = writePtr original_type_var.tv_info_ptr (TVI_Type (TV new_type_var)) type_var_heap
				-> ([old_type_var_value:old_type_vars_values],type_var_heap)
	forward_old_type_vars_to_new_type_vars [] [] type_var_heap
		= ([],type_var_heap)

restore_old_type_vars_values :: ![TypeVar] ![TypeVarInfo] !*(Optional *TypeHeaps) -> *Optional *TypeHeaps
restore_old_type_vars_values original_type_vars old_original_type_vars_values No
	= No
restore_old_type_vars_values original_type_vars old_original_type_vars_values (Yes type_heaps)
	# type_heaps & th_vars = write_old_type_vars_values original_type_vars old_original_type_vars_values type_heaps.th_vars
	= Yes type_heaps
where
	write_old_type_vars_values [{tv_info_ptr}:type_vars] [type_var_value:type_var_values] type_var_heap
		# type_var_heap = writePtr tv_info_ptr type_var_value type_var_heap
		= write_old_type_vars_values type_vars type_var_values type_var_heap
	write_old_type_vars_values [] [] type_var_heap
		= type_var_heap

copyLet :: !Let !(Optional AType) !CopyInfo !*CopyState -> (!Let, !*CopyState)
copyLet lad=:{let_strict_binds, let_lazy_binds, let_expr, let_info_ptr} optional_result_type ci cs
	# (let_strict_binds, cs) = copy_bound_vars let_strict_binds cs
	# (let_lazy_binds, cs) = copy_bound_vars let_lazy_binds cs
	# (let_strict_binds, cs) = copy let_strict_binds ci cs
	# (let_lazy_binds, cs) = copy let_lazy_binds ci cs
	# (let_expr, cs) = copyCaseAlt let_expr optional_result_type ci cs
	  (old_let_info, cs_symbol_heap) = readPtr let_info_ptr cs.cs_symbol_heap
	  (new_let_info, cs_opt_type_heaps) = substitute_let_type old_let_info cs.cs_opt_type_heaps
	  (new_info_ptr, cs_symbol_heap) = newPtr new_let_info cs_symbol_heap
	= ({lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr, let_info_ptr = new_info_ptr},
		{ cs & cs_symbol_heap = cs_symbol_heap, cs_opt_type_heaps = cs_opt_type_heaps })
	where
		copy_bound_vars [bind=:{lb_dst} : binds] cs
			# (lb_dst, cs) = copy lb_dst ci cs
			  (binds, cs) = copy_bound_vars binds cs
			= ([ {bind & lb_dst = lb_dst} : binds ], cs)
		copy_bound_vars [] cs
			= ([], cs)

substitute_let_type expr_info No
	= (expr_info, No)
substitute_let_type (EI_Extended extensions expr_info) yes_type_heaps
	# (new_expr_info, yes_type_heaps) = substitute_let_type expr_info yes_type_heaps
	= (EI_Extended extensions new_expr_info, yes_type_heaps)
substitute_let_type expr_info=:(EI_LetType let_type) (Yes type_heaps)
	# (changed, new_let_type, type_heaps) = substitute let_type type_heaps
	| changed
		= (EI_LetType new_let_type, Yes type_heaps)
		= (expr_info, Yes type_heaps)

substitute_case_type expr_info parent_opt_result_type No
	= (expr_info, No, No)
substitute_case_type (EI_Extended extensions expr_info) parent_opt_result_type yes_type_heaps
	# (new_expr_info, opt_result_type, yes_type_heaps)
		= substitute_case_type expr_info parent_opt_result_type yes_type_heaps
	= (EI_Extended extensions new_expr_info, opt_result_type, yes_type_heaps)
substitute_case_type expr_info=:(EI_CaseType case_type) parent_opt_result_type (Yes type_heaps)
	# (changed, new_case_type, type_heaps) = substituteCaseType case_type parent_opt_result_type type_heaps
	| changed
		= (EI_CaseType new_case_type, Yes new_case_type.ct_result_type, Yes type_heaps)
		= (expr_info, Yes new_case_type.ct_result_type, Yes type_heaps)
where
	substituteCaseType {ct_pattern_type, ct_result_type, ct_cons_types} parent_opt_result_type heaps
		# (changed_pattern_type, pattern_type_r, heaps) = substitute ct_pattern_type heaps
		  (changed_result_type, result_type_r, heaps) = substitute ct_result_type heaps
		  (changed_cons_types, cons_types_r, heaps) = substitute ct_cons_types heaps
		| changed_pattern_type
			| changed_result_type
				# result_type_r = use_parent_result_type_if_equal parent_opt_result_type result_type_r
				| changed_cons_types
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=result_type_r, ct_cons_types=cons_types_r}, heaps)
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=result_type_r, ct_cons_types=ct_cons_types}, heaps)
				| changed_cons_types
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=ct_result_type, ct_cons_types=cons_types_r}, heaps)
					= (True, {ct_pattern_type=pattern_type_r, ct_result_type=ct_result_type, ct_cons_types=ct_cons_types}, heaps)
			| changed_result_type
				# result_type_r = use_parent_result_type_if_equal parent_opt_result_type result_type_r
				| changed_cons_types
					= (True, {ct_pattern_type=ct_pattern_type, ct_result_type=result_type_r, ct_cons_types=cons_types_r}, heaps)
					= (True, {ct_pattern_type=ct_pattern_type, ct_result_type=result_type_r, ct_cons_types=ct_cons_types}, heaps)
				| changed_cons_types
					= (True, {ct_pattern_type=ct_pattern_type, ct_result_type=ct_result_type, ct_cons_types=cons_types_r}, heaps)
					= (False, {ct_pattern_type=ct_pattern_type, ct_result_type=ct_result_type, ct_cons_types=ct_cons_types}, heaps)

	use_parent_result_type_if_equal (Yes parent_result_type) result_type_r
		| equal_atype result_type_r parent_result_type
			= parent_result_type
			= result_type_r
	use_parent_result_type_if_equal No result_type_r
		= result_type_r

copyCasePatterns :: !CasePatterns !(Optional AType) !CopyInfo !*CopyState -> *(!CasePatterns,!*CopyState)
copyCasePatterns (AlgebraicPatterns type patterns) opt_result_type ci cs
	# (patterns, cs) = copyAlgebraicPatterns patterns opt_result_type ci cs
	= (AlgebraicPatterns type patterns, cs)
copyCasePatterns (BasicPatterns type patterns) opt_result_type ci cs
	# (patterns, cs) = copyBasicPatterns patterns opt_result_type ci cs
	= (BasicPatterns type patterns, cs)
copyCasePatterns (OverloadedListPatterns type decons_expr patterns) opt_result_type ci cs
	# (patterns, cs) = copyAlgebraicPatterns patterns opt_result_type ci cs
	# (decons_expr, cs) = copy decons_expr ci cs
	= (OverloadedListPatterns type decons_expr patterns, cs)
copyCasePatterns (NewTypePatterns type patterns) opt_result_type ci cs
	# (patterns, cs) = copyAlgebraicPatterns patterns opt_result_type ci cs
	= (NewTypePatterns type patterns, cs)
copyCasePatterns (DynamicPatterns patterns) opt_result_type ci cs
	# (patterns, cs) = copy patterns ci cs
	= (DynamicPatterns patterns, cs)

copyAlgebraicPatterns [guard=:{ap_vars,ap_expr} : guards] opt_result_type ci cs
	# (ap_vars, cs) = copy ap_vars ci cs
	# (ap_expr, cs) = copyCaseAlt ap_expr opt_result_type ci cs
	#! guard & ap_vars = ap_vars, ap_expr = ap_expr
	# (guards, cs) = copyAlgebraicPatterns guards opt_result_type ci cs
	#! cs = cs
	= ([guard : guards], cs)
copyAlgebraicPatterns [] opt_result_type ci cs
 	= ([], cs)

copyBasicPatterns [guard=:{bp_expr} : guards] opt_result_type ci cs
	# (bp_expr, cs) = copyCaseAlt bp_expr opt_result_type ci cs
	#! guard & bp_expr = bp_expr
	# (guards, cs) = copyBasicPatterns guards opt_result_type ci cs
	#! cs = cs
	= ([guard : guards], cs)
copyBasicPatterns [] opt_result_type ci cs
 	= ([], cs)

instance copy DynamicPattern
where
	copy guard=:{dp_var,dp_rhs} ci cs
		# (dp_var, cs) = copy dp_var ci cs
		  (dp_rhs, cs) = copy dp_rhs ci cs
		= ({ guard & dp_var = dp_var, dp_rhs = dp_rhs }, cs)

instance copy [a] | copy a
where
	copy l ci cs
		= map_st l cs
		where
			map_st [x : xs] s
			 	# (x, s) = copy x ci s
				  (xs, s) = map_st xs s
				#! s = s
				= ([x : xs], s)
			map_st [] s
			 	= ([], s)

instance copy (a,b) | copy a & copy b
where
	copy (a,b) ci cs
		# (a,cs) = copy a ci cs
		# (b,cs) = copy b ci cs
		= ((a,b),cs)

instance copy (Optional a) | copy a
where
	copy (Yes x) ci cs
		# (x, cs) = copy x ci cs
		= (Yes x, cs)
	copy no ci cs
		= (no, cs)

equal_atype :: !AType !AType -> Bool
equal_atype {at_attribute=TA_Multi,at_type=type1} {at_attribute=TA_Multi,at_type=type2}
	= equal_type type1 type2
equal_atype {at_attribute=TA_Unique,at_type=type1} {at_attribute=TA_Unique,at_type=type2}
	= equal_type type1 type2
equal_atype {at_attribute=TA_Var {av_info_ptr=av_info_ptr1},at_type=type1} {at_attribute=TA_Var {av_info_ptr=av_info_ptr2},at_type=type2}
	= av_info_ptr1==av_info_ptr2 && equal_type type1 type2
equal_atype new_type old_type
	= False

equal_type :: !Type !Type -> Bool
equal_type (TA {type_index=type_index1} types1) (TA {type_index=type_index2} types2)
	= type_index1==type_index2 && equal_atypes types1 types2
equal_type (TAS {type_index=type_index1} types1 strictness1) (TAS {type_index=type_index2} types2 strictness2)
	= type_index1==type_index2 && equal_strictness_lists strictness1 strictness2 && equal_atypes types1 types2
equal_type (TB bt1) (TB bt2)
	= equal_basic_type bt1 bt2
equal_type (TV {tv_info_ptr=tv_info_ptr1}) (TV {tv_info_ptr=tv_info_ptr2})
	= tv_info_ptr1==tv_info_ptr2
equal_type (CV {tv_info_ptr=tv_info_ptr1} :@: types1) (CV {tv_info_ptr=tv_info_ptr2} :@: types2)
	= tv_info_ptr1==tv_info_ptr2 && equal_atypes types1 types2
equal_type (a_atype1 --> r_atype1) (a_atype2 --> r_atype2)
	= equal_atype a_atype1 a_atype2 && equal_atype r_atype1 r_atype2
equal_type new_type old_type
	= False

equal_basic_type BT_Int BT_Int = True
equal_basic_type BT_Char BT_Char = True
equal_basic_type BT_Bool BT_Bool = True
equal_basic_type BT_Real BT_Real = True
equal_basic_type BT_Dynamic BT_Dynamic = True
equal_basic_type BT_File BT_File = True
equal_basic_type BT_World BT_World = True
equal_basic_type _ _ = False

equal_atypes [] []
	= True
equal_atypes [atype1:atypes1] [atype2:atypes2]
	= equal_atype atype1 atype2 && equal_atypes atypes1 atypes2
equal_atypes new_types old_types
	= False
