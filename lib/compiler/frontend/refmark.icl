implementation module refmark

import StdEnv
import syntax, Heap, typesupport, overloading, unitype, utilities

(===>) infix 1
(===>) a b :== a // --->  b

:: VarInfo
	| VI_Occurrence !Occurrence

::	PatternVar =
	{	pv_var		:: !FreeVar
	,	pv_arg_nr	:: !Int
	}

:: ReferenceCountList
	= ReferenceCounts !ReferenceCount !ReferenceCountList
	| ReferenceCountsUnused !Int !ReferenceCountList
	| ReferenceCountsAllUnused
	| EndReferenceCounts

::	Occurrence =
	{	occ_ref_count		:: !ReferenceCount
	,	occ_bind			:: !OccurrenceBinding
	,	occ_pattern_vars	:: ![[PatternVar]]
	,	occ_observing		:: (Bool, Ptr ExprInfo)
	,	occ_previous 		:: !ReferenceCountList
	}

::	ReferenceCount = RC_Used !RC_Used | RC_Unused 

::	SelectiveUse =
	{	su_field	:: !Int
	,	su_multiply :: ![ExprInfoPtr]
	,	su_uniquely :: ![ExprInfoPtr]
	}

::	RC_Used =
	{ 	rcu_multiply	:: ![ExprInfoPtr]
	,	rcu_selectively :: ![SelectiveUse]
	,	rcu_uniquely	:: ![ExprInfoPtr]
	}

::	CountedFreeVar =
	{	cfv_var		:: !FreeVar
	,	cfv_is_let	:: !Bool
	,	cfv_count	:: !ReferenceCount
	}

::	OccurrenceBinding	= OB_Empty 
						| OB_OpenLet	!FreeVar !(Optional RefMarkResult)
						| OB_LockedLet	!OccurrenceBinding
						| OB_MarkedLet	!OccurrenceBinding

::	RefMarkResult :== ([CountedFreeVar], [FreeVar])

NotASelector :== -1

::	RMState =
	{	rms_var_heap :: !.VarHeap
	,	rms_let_vars :: ![FreeVar]
	,	rms_counted_let_vars :: ![FreeVar]
	}

class refMark expr :: ![[FreeVar]] !Int !(Optional [CountedFreeVar]) !expr !*RMState -> *RMState

fullRefMarkOfRootOrLetExpr :: ![[FreeVar]] !Int !(Optional [CountedFreeVar]) !Expression [FreeVar] !*VarHeap -> *RMState
fullRefMarkOfRootOrLetExpr free_vars sel def expr rms_counted_let_vars var_heap
	# {rms_let_vars,rms_counted_let_vars,rms_var_heap}
		= refMark free_vars sel def expr {rms_var_heap=var_heap, rms_let_vars=[], rms_counted_let_vars=rms_counted_let_vars}
	  rms_var_heap = openLetVars rms_let_vars rms_var_heap
	  (closed_let_vars,rms) = addParRefMarksOfLets rms_let_vars ([],{rms_let_vars=[],rms_counted_let_vars=rms_counted_let_vars,rms_var_heap=rms_var_heap})
	= {rms & rms_counted_let_vars=closed_let_vars++rms.rms_counted_let_vars}

fullRefMarkOfAlternative :: ![[FreeVar]] !Int !(Optional [CountedFreeVar]) !expr !*VarHeap -> (!*[FreeVar],!*RMState) | refMark expr
fullRefMarkOfAlternative free_vars sel def expr var_heap
	# {rms_let_vars,rms_counted_let_vars,rms_var_heap}
		= refMark free_vars sel def expr {rms_var_heap=var_heap, rms_let_vars=[], rms_counted_let_vars=[]}
	  rms_var_heap = openLetVars rms_let_vars rms_var_heap
	= addParRefMarksOfLets rms_let_vars ([], {rms_let_vars=[], rms_counted_let_vars=rms_counted_let_vars, rms_var_heap=rms_var_heap})

fullRefMarkOfCaseExpr :: ![[FreeVar]] !Int !(Optional [CountedFreeVar]) !Expression [FreeVar] !*VarHeap -> (!*[FreeVar],!*RMState)
fullRefMarkOfCaseExpr free_vars sel def expr rms_counted_let_vars var_heap
	# {rms_let_vars,rms_counted_let_vars,rms_var_heap}
		= refMark free_vars sel def expr {rms_var_heap=var_heap, rms_let_vars=[], rms_counted_let_vars=rms_counted_let_vars}
	  rms_var_heap = openLetVars rms_let_vars rms_var_heap
	= addParRefMarksOfLets rms_let_vars ([], {rms_let_vars=[], rms_counted_let_vars=rms_counted_let_vars, rms_var_heap=rms_var_heap})

ref_mark_of_lets free_vars let_binds rms_counted_let_vars rms_var_heap
	= foldSt (ref_mark_of_let free_vars) let_binds (rms_counted_let_vars,rms_var_heap)
where
	ref_mark_of_let free_vars let_bind=:{lb_src, lb_dst=fv=:{fv_info_ptr}} (rms_counted_let_vars,rms_var_heap)
		# (VI_Occurrence occ, rms_var_heap) = readPtr fv_info_ptr rms_var_heap
		  rms_var_heap = rms_var_heap <:= (fv_info_ptr, VI_Occurrence {occ & occ_bind = OB_LockedLet occ.occ_bind})
		  (res,rms_counted_let_vars,rms_var_heap) = partialRefMark free_vars lb_src rms_counted_let_vars rms_var_heap
		#! rms_var_heap = rms_var_heap <:= (fv_info_ptr, VI_Occurrence {occ & occ_bind = OB_OpenLet fv (Yes res)})
		= (rms_counted_let_vars,rms_var_heap) ===> ("ref_mark_of_let", fv, res)

	partialRefMark :: ![[FreeVar]] !Expression [FreeVar] !*VarHeap -> (!([CountedFreeVar],[FreeVar]),![FreeVar],!*VarHeap)
	partialRefMark free_vars expr rms_counted_let_vars var_heap
		# var_heap = saveOccurrences free_vars var_heap
		  {rms_var_heap,rms_counted_let_vars,rms_let_vars}
		  	= refMark free_vars NotASelector No expr {rms_var_heap=var_heap, rms_let_vars=[], rms_counted_let_vars=rms_counted_let_vars}
		  rms_var_heap = openLetVars rms_let_vars rms_var_heap
		  (occurrences, rms_var_heap) = restoreOccurrences free_vars rms_var_heap
		= ((occurrences, rms_let_vars),rms_counted_let_vars,rms_var_heap)

remove_local_let_vars_from_counted_let_vars :: ![FreeVar] ![FreeVar] !*VarHeap -> (![FreeVar],!*VarHeap)
remove_local_let_vars_from_counted_let_vars local_let_vars counted_let_vars var_heap
	# var_heap = foldSt (\ {fv_info_ptr} -> mark_bind fv_info_ptr) local_let_vars var_heap
	  (counted_let_vars,var_heap) = remove_marked_local_let_vars counted_let_vars var_heap
	  var_heap = foldSt (\ {fv_info_ptr} -> unmark_bind fv_info_ptr) local_let_vars var_heap
	= (counted_let_vars,var_heap)
where
	mark_bind fv_info_ptr var_heap
		# (VI_Occurrence occ,var_heap) = readPtr fv_info_ptr var_heap
		= writePtr fv_info_ptr (VI_Occurrence {occ & occ_bind=OB_MarkedLet occ.occ_bind}) var_heap

	remove_marked_local_let_vars [closed_let_var:closed_let_vars] var_heap
		# (VI_Occurrence occ) = sreadPtr closed_let_var.fv_info_ptr var_heap
		= case occ.occ_bind of
			OB_MarkedLet _
				-> remove_marked_local_let_vars closed_let_vars var_heap
			_
				# (closed_let_vars,var_heap) = remove_marked_local_let_vars closed_let_vars var_heap
				-> ([closed_let_var:closed_let_vars],var_heap)
	remove_marked_local_let_vars [] var_heap
		= ([],var_heap)

	unmark_bind fv_info_ptr var_heap
		# (VI_Occurrence occ=:{occ_bind=OB_MarkedLet occ_bind},var_heap) = readPtr fv_info_ptr var_heap
		= writePtr fv_info_ptr (VI_Occurrence {occ & occ_bind=occ_bind}) var_heap

instance refMark [a] | refMark a
where
	refMark free_vars sel _ list rms 
		= foldSt (refMark free_vars sel No) list rms 

collectAllSelections [] cum_sels
	= cum_sels
collectAllSelections [{su_multiply,su_uniquely} : sels ] cum_sels
	= collectAllSelections sels (su_uniquely ++ su_multiply ++ cum_sels)

saveOccurrences free_vars var_heap
	= foldSt (foldSt save_occurrence) free_vars var_heap // (free_vars ===> ("saveOccurrences", free_vars)) var_heap
where
	save_occurrence {fv_ident,fv_info_ptr} var_heap
		# (VI_Occurrence old_occ=:{occ_ref_count,occ_previous}, var_heap) = readPtr fv_info_ptr var_heap
		= case occ_ref_count of
			RC_Unused
				-> case occ_previous of
					ReferenceCountsUnused n ref_counts
						#! n=n+1
						-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = ReferenceCountsUnused n ref_counts})
					ReferenceCountsAllUnused
						-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = ReferenceCountsAllUnused})
					EndReferenceCounts
						-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = ReferenceCountsAllUnused})
					_
						-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = ReferenceCountsUnused 1 occ_previous})
			_
				-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = RC_Unused, occ_previous = ReferenceCounts occ_ref_count occ_previous } )
					===> ("save_occurrence", fv_ident, fv_info_ptr, occ_ref_count, length occ_previous)

restoreOccurrences free_vars var_heap
	= foldSt (foldSt restore_occurrence) (free_vars ===> ("restoreOccurrences", free_vars)) ([], var_heap)
where
	restore_occurrence fv=:{fv_ident,fv_info_ptr} (occurrences, var_heap)
		# (VI_Occurrence old_occ=:{occ_ref_count,occ_previous,occ_bind}, var_heap) = readPtr fv_info_ptr var_heap
		= case occ_ref_count of
			RC_Unused
				# var_heap
					= case occ_previous of
						ReferenceCounts prev_ref_count occ_previous
							-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = prev_ref_count, occ_previous = occ_previous })
								===> ("restore_occurrence", fv_ident, fv_info_ptr, (occ_ref_count, prev_ref_count, occ_previous))
						ReferenceCountsAllUnused
							-> var_heap
						ReferenceCountsUnused 1 xs
							-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = xs})
						ReferenceCountsUnused n xs
							#! n=n-1
							-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = ReferenceCountsUnused n xs})
						_
							-> abort ("restoreOccurrences" /* ---> (fv_ident, fv_info_ptr) */)
				-> (occurrences, var_heap)
			_
				# var_heap
					= case occ_previous of
						ReferenceCounts prev_ref_count occ_previous
							-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = prev_ref_count, occ_previous = occ_previous })
								===> ("restore_occurrence", fv_ident, fv_info_ptr, (occ_ref_count, prev_ref_count, occ_previous))
						ReferenceCountsAllUnused
							-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = RC_Unused})
						ReferenceCountsUnused 1 xs
							-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = RC_Unused, occ_previous = xs})
						ReferenceCountsUnused n xs
							#! n=n-1
							-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = RC_Unused, occ_previous = ReferenceCountsUnused n xs})
						_
							-> abort ("restoreOccurrences" /* ---> (fv_ident, fv_info_ptr) */)
				-> case occ_bind of
					OB_OpenLet _ _
						-> ([{cfv_var = fv, cfv_count = occ_ref_count, cfv_is_let = True} : occurrences ], var_heap)
					_
						-> ([{cfv_var = fv, cfv_count = occ_ref_count, cfv_is_let = False} : occurrences ], var_heap)

markPatternVariables sel list_of_used_pattern_vars var_heap
	| sel == NotASelector
		= markPatternVariables list_of_used_pattern_vars var_heap
		= foldSt (mark_selected_variable sel) list_of_used_pattern_vars var_heap
where
	markPatternVariables list_of_used_pattern_vars var_heap
		= foldSt mark_pattern_variables list_of_used_pattern_vars var_heap

	mark_pattern_variables used_pattern_vars var_heap
		= foldSt mark_variable used_pattern_vars var_heap

	mark_selected_variable sel [] var_heap
		= var_heap
	mark_selected_variable sel [pv=:{pv_var, pv_arg_nr} : pvs ] var_heap
		| sel == pv_arg_nr
			= mark_variable pv var_heap
			= mark_selected_variable sel pvs var_heap

	mark_variable {pv_var={fv_ident,fv_info_ptr}} var_heap
		# (VI_Occurrence old_occ=:{occ_ref_count,occ_observing = (_, expr_ptr),occ_pattern_vars}, var_heap) = readPtr fv_info_ptr var_heap
		= case occ_ref_count ===> ("mark_variable", fv_ident) of
			RC_Unused
				# occ_ref_count = RC_Used {rcu_multiply = [], rcu_selectively = [], rcu_uniquely = [expr_ptr]}
				# var_heap= var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = occ_ref_count } )
				-> markPatternVariables occ_pattern_vars var_heap
			RC_Used {rcu_multiply,rcu_uniquely,rcu_selectively}
				# occ_ref_count = RC_Used { rcu_multiply = collectAllSelections rcu_selectively (rcu_uniquely ++ [expr_ptr : rcu_multiply]),
							 rcu_selectively = [], rcu_uniquely = [] }
				# var_heap = var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = occ_ref_count } )
				-> markPatternVariables occ_pattern_vars var_heap

refMarkOfVariable sel (VI_Occurrence var_occ) var=:{var_ident, var_info_ptr, var_expr_ptr} rms=:{rms_var_heap}
	# occ_ref_count = adjust_ref_count sel var_occ.occ_ref_count var_expr_ptr
	  rms_var_heap = markPatternVariables sel var_occ.occ_pattern_vars rms_var_heap
	= ref_count_of_bindings var_occ var_ident var_info_ptr occ_ref_count { rms & rms_var_heap = rms_var_heap }
		===> ("refMarkOfVariable", var_ident, var_occ.occ_ref_count, occ_ref_count, var_occ.occ_pattern_vars)
where
	adjust_ref_count sel RC_Unused var_expr_ptr
		| sel == NotASelector
			= RC_Used {rcu_multiply = [], rcu_uniquely = [var_expr_ptr], rcu_selectively = []}
			= RC_Used {rcu_multiply = [], rcu_uniquely = [],
					   rcu_selectively = [{ su_field = sel, su_multiply = [], su_uniquely = [var_expr_ptr] }]}
	adjust_ref_count sel use=:(RC_Used {rcu_multiply,rcu_uniquely,rcu_selectively}) var_expr_ptr
		| sel == NotASelector
			# rcu_multiply = collectAllSelections rcu_selectively (rcu_uniquely ++ [var_expr_ptr : rcu_multiply])
			= RC_Used {rcu_multiply = rcu_multiply, rcu_uniquely = [], rcu_selectively = []}
			# rcu_multiply = rcu_uniquely ++ rcu_multiply
			| isEmpty rcu_multiply
				# rcu_selectively = add_selection var_expr_ptr sel rcu_selectively
				= RC_Used {rcu_multiply = [], rcu_uniquely = [], rcu_selectively = rcu_selectively}
				# rcu_multiply = collectAllSelections rcu_selectively [var_expr_ptr : rcu_multiply]
				= RC_Used {rcu_multiply = rcu_multiply, rcu_uniquely = [], rcu_selectively = []}

	add_selection var_expr_ptr sel []
		= [ { su_field = sel, su_multiply = [], su_uniquely = [var_expr_ptr]  } ]
	add_selection var_expr_ptr sel sels=:[selection=:{ su_field,su_multiply,su_uniquely } : selections]
		| sel == su_field
			= [ { selection & su_multiply = su_multiply ++ [var_expr_ptr : su_uniquely], su_uniquely = [] } : selections ]
		| sel < su_field
			= [ { su_field = sel, su_multiply = [], su_uniquely = [var_expr_ptr]  } : sels ]
			= [ selection : add_selection var_expr_ptr sel selections ]

	ref_count_of_bindings var_occ=:{occ_bind = OB_OpenLet fv _} var_ident var_info_ptr occ_ref_count rms=:{rms_var_heap,rms_let_vars}
		# rms_var_heap = rms_var_heap <:= (var_info_ptr, VI_Occurrence { var_occ & occ_ref_count = occ_ref_count, occ_bind = OB_LockedLet var_occ.occ_bind })
		= { rms & rms_var_heap = rms_var_heap, rms_let_vars = [ fv : rms_let_vars ]}
				===> ("ref_count_of_bindings (OB_OpenLet)", var_ident)
	ref_count_of_bindings var_occ=:{occ_bind = OB_LockedLet _} var_ident var_info_ptr occ_ref_count rms=:{rms_var_heap} 
		= { rms & rms_var_heap = rms_var_heap <:= (var_info_ptr, VI_Occurrence { var_occ & occ_ref_count = occ_ref_count })}
//				===> ("ref_count_of_bindings (OB_LockedLet)", var_ident)
	ref_count_of_bindings var_occ var_ident var_info_ptr occ_ref_count rms=:{rms_var_heap}
		= { rms & rms_var_heap = rms_var_heap <:= (var_info_ptr, VI_Occurrence { var_occ & occ_ref_count = occ_ref_count })}

addParRefMarksOfLets let_vars closed_vars_and_rms
	= foldSt ref_mark_of_let let_vars closed_vars_and_rms
where
	ref_mark_of_let fv=:{fv_ident,fv_info_ptr} (closed_let_vars, rms=:{rms_var_heap})
		# (VI_Occurrence var_occ, rms_var_heap) = readPtr fv_info_ptr rms_var_heap
		  rms = {rms & rms_var_heap = rms_var_heap}
		= case var_occ.occ_bind of
			OB_OpenLet _ (Yes (ref_counts, let_vars))
				# rms_var_heap = rms.rms_var_heap <:= (fv_info_ptr, VI_Occurrence {var_occ & occ_bind = OB_LockedLet var_occ.occ_bind})
				  rms_var_heap = addParRefCounts ref_counts rms_var_heap
				-> addParRefMarksOfLets let_vars ([fv : closed_let_vars], {rms & rms_var_heap = rms_var_heap})
						 ===> ("addParRefMarksOfLets (OB_OpenLet Yes)", fv_ident) 
			OB_OpenLet _ No
				# rms_var_heap = rms.rms_var_heap <:= (fv_info_ptr, VI_Occurrence {var_occ & occ_bind = OB_LockedLet var_occ.occ_bind})
				-> (closed_let_vars, { rms  & rms_var_heap = rms_var_heap, rms_let_vars = [fv : rms.rms_let_vars]})
						 ===> ("addParRefMarksOfLets (OB_OpenLet No)", fv_ident) 
			OB_LockedLet _
				-> (closed_let_vars, rms)
						 ===> ("addParRefMarksOfLets (OB_LockedLet)", fv_ident) 

addParRefCounts ref_counts var_heap
	= foldSt set_occurrence ref_counts var_heap
where
	set_occurrence {cfv_var = {fv_ident,fv_info_ptr}, cfv_count} var_heap
		# (VI_Occurrence occ=:{occ_ref_count}, var_heap) = readPtr fv_info_ptr var_heap
		  comb_ref_count = parCombineRefCount occ_ref_count cfv_count
		= var_heap <:= (fv_info_ptr, VI_Occurrence { occ & occ_ref_count = comb_ref_count})
			===>  ("addParRefCounts", fv_ident, fv_info_ptr, (cfv_count, occ_ref_count, comb_ref_count))

addSeqRefCounts ref_counts var_heap
	= foldSt set_occurrence ref_counts var_heap
where
	set_occurrence {cfv_var = {fv_ident,fv_info_ptr}, cfv_count} var_heap
		# (VI_Occurrence occ=:{occ_ref_count}, var_heap) = readPtr fv_info_ptr var_heap
		  comb_ref_count = seqCombineRefCount occ_ref_count cfv_count
		= var_heap <:= (fv_info_ptr, VI_Occurrence { occ & occ_ref_count = comb_ref_count})
			===>  ("addSeqRefCounts", fv_ident, cfv_count, occ_ref_count, comb_ref_count)

instance refMark BoundVar
where
	refMark free_vars sel _ var rms=:{rms_var_heap}
		# (var_occ, rms_var_heap) = readPtr var.var_info_ptr rms_var_heap
		= refMarkOfVariable sel var_occ var { rms & rms_var_heap = rms_var_heap }

instance refMark Expression
where
	refMark free_vars sel _ (Var var) rms 
		= refMark free_vars sel No var rms 
	refMark free_vars sel _ (App {app_args}) rms 
		= refMark free_vars NotASelector No app_args rms 
	refMark free_vars sel _ (fun @ args) rms 
		= refMark free_vars NotASelector No args (refMark free_vars NotASelector No fun rms)

	refMark free_vars sel def (Let {let_strict_binds,let_lazy_binds,let_expr}) rms=:{rms_counted_let_vars,rms_var_heap}
		| isEmpty let_lazy_binds
			# local_let_vars = [lb_dst \\ {lb_dst} <- let_strict_binds]
			# new_free_vars = [local_let_vars : free_vars]
			| binds_are_observing let_strict_binds rms_var_heap
				# rms_var_heap = saveOccurrences free_vars rms_var_heap
				  rms = refMark new_free_vars NotASelector No let_strict_binds {rms & rms_var_heap = rms_var_heap}
				  rms_var_heap = saveOccurrences new_free_vars rms.rms_var_heap
				  {rms_var_heap,rms_let_vars,rms_counted_let_vars}
					= fullRefMarkOfRootOrLetExpr new_free_vars sel def let_expr rms_counted_let_vars rms_var_heap
				  rms_var_heap = let_combine free_vars rms_var_heap
				  (rms_counted_let_vars,rms_var_heap) = remove_local_let_vars_from_counted_let_vars local_let_vars rms_counted_let_vars rms_var_heap
				= {rms_var_heap=rms_var_heap, rms_let_vars=rms_let_vars ++ rms.rms_let_vars, rms_counted_let_vars=rms_counted_let_vars}
					===> ("refMark (Let (observing))", hd new_free_vars)
				= refMark new_free_vars sel def let_expr (refMark new_free_vars NotASelector No let_strict_binds {rms & rms_var_heap = rms_var_heap})
			# all_binds								= let_strict_binds ++ let_lazy_binds
			  local_let_vars						= [lb_dst \\ {lb_dst} <- all_binds]
			  new_free_vars							= [local_let_vars : free_vars]
			  rms_var_heap							= init_let_binds local_let_vars rms_var_heap
			  (rms_counted_let_vars,rms_var_heap)	= ref_mark_of_lets new_free_vars all_binds rms_counted_let_vars rms_var_heap
			  {rms_var_heap,rms_let_vars,rms_counted_let_vars}
				= fullRefMarkOfRootOrLetExpr new_free_vars sel def let_expr rms_counted_let_vars rms_var_heap
			  (rms_counted_let_vars,rms_var_heap) = remove_local_let_vars_from_counted_let_vars local_let_vars rms_counted_let_vars rms_var_heap
			= {rms_var_heap=rms_var_heap, rms_let_vars=rms_let_vars ++ rms.rms_let_vars, rms_counted_let_vars=rms_counted_let_vars}
		where
			binds_are_observing [{lb_dst={fv_info_ptr}}:binds] var_heap
				# (VI_Occurrence {occ_observing=(is_observing,_)}) = sreadPtr fv_info_ptr var_heap
				= is_observing && binds_are_observing binds var_heap
		    binds_are_observing [] var_heap
		    	= True
			
			let_combine free_vars var_heap
				= foldSt (foldSt let_combine_ref_count) free_vars var_heap
			where
				let_combine_ref_count {fv_ident,fv_info_ptr} var_heap
					# (VI_Occurrence old_occ=:{occ_ref_count,occ_previous}, var_heap) = readPtr fv_info_ptr var_heap
					= case occ_previous of
						ReferenceCounts prev_ref_count (ReferenceCounts pre_pref_recount occ_previouses)
							# seq_comb_ref_count = seqCombineRefCount occ_ref_count prev_ref_count
							  comb_ref_count = parCombineRefCount seq_comb_ref_count pre_pref_recount
							-> (var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = comb_ref_count, occ_previous = occ_previouses}))
								===> ("let_combine_ref_count", fv_ident, (pre_pref_recount, prev_ref_count, occ_ref_count, seq_comb_ref_count, comb_ref_count))
						ReferenceCounts prev_ref_count (ReferenceCountsUnused 1 occ_previouses)
							# comb_ref_count = seqCombineRefCount occ_ref_count prev_ref_count
							-> (var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = comb_ref_count, occ_previous = occ_previouses}))
						ReferenceCounts prev_ref_count ReferenceCountsAllUnused
							# comb_ref_count = seqCombineRefCount occ_ref_count prev_ref_count
							-> (var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = comb_ref_count, occ_previous = ReferenceCountsAllUnused}))
						ReferenceCounts prev_ref_count (ReferenceCountsUnused n occ_previouses)
							#! n=n-1
							# comb_ref_count = seqCombineRefCount occ_ref_count prev_ref_count
							-> (var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = comb_ref_count, occ_previous = ReferenceCountsUnused n occ_previouses}))
						ReferenceCountsUnused 1 (ReferenceCounts pre_pref_recount occ_previouses)
							# comb_ref_count = parCombineRefCount occ_ref_count pre_pref_recount
							-> (var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = comb_ref_count, occ_previous = occ_previouses}))
						ReferenceCountsUnused 2 occ_previouses
							-> (var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = occ_previouses}))
						ReferenceCountsUnused n occ_previouses
							#! n=n-2
							-> (var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = ReferenceCountsUnused n occ_previouses}))
						ReferenceCountsAllUnused
							-> var_heap

			init_let_binds let_binds var_heap
				= foldSt bind_variable let_binds var_heap
			where
				bind_variable fv=:{fv_info_ptr} var_heap
					# (VI_Occurrence occ, var_heap) = readPtr fv_info_ptr var_heap
					= var_heap <:= (fv_info_ptr, VI_Occurrence { occ & occ_ref_count = RC_Unused, occ_bind = OB_OpenLet fv No })

	refMark free_vars sel def (Case ca) rms
		= refMarkOfCase free_vars sel def ca rms 
	refMark free_vars sel _ (Selection selkind expr selectors) rms
		= case selkind of
			UniqueSelector
				-> refMark free_vars NotASelector No expr rms
			UniqueSelectorUniqueElementResult
				-> refMark free_vars NotASelector No expr rms
			_
				-> refMark free_vars (field_number selectors) No expr rms 
	where
		field_number [ RecordSelection _ field_nr : _ ]
			= field_nr	
		field_number _
			= NotASelector	
	refMark free_vars sel _ (Update expr1 selectors expr2) rms 
		# rms  = refMark free_vars NotASelector No expr1 rms 
		  rms  = refMark free_vars NotASelector No selectors rms 
		= refMark free_vars NotASelector No expr2 rms 
	refMark free_vars sel _ (RecordUpdate cons_symbol expression expressions) rms 
		= ref_mark_of_record_expression free_vars expression expressions rms 
	where
		ref_mark_of_record_expression free_vars (Var var) fields rms 
			= ref_mark_of_fields 0 free_vars fields var rms 
		ref_mark_of_record_expression free_vars expression fields rms 
			# rms  = refMark free_vars NotASelector No expression rms 
			= foldSt (ref_mark_of_field free_vars) fields rms 
	
		ref_mark_of_fields field_nr free_vars [] var rms 
			= rms 
		ref_mark_of_fields field_nr free_vars [{bind_src = NoBind expr_ptr} : fields] var=:{var_info_ptr} rms=:{rms_var_heap}
			# (var_occ, rms_var_heap) = readPtr var_info_ptr rms_var_heap
			  rms  = refMarkOfVariable field_nr var_occ { var & var_expr_ptr = expr_ptr } { rms & rms_var_heap = rms_var_heap }
			= ref_mark_of_fields (inc field_nr) free_vars fields var rms 
		ref_mark_of_fields field_nr free_vars [{bind_src} : fields] var rms 
			# rms  = refMark free_vars NotASelector No bind_src rms 
			= ref_mark_of_fields (inc field_nr) free_vars fields var rms 

		ref_mark_of_field free_vars {bind_src} rms 
			= refMark free_vars NotASelector No bind_src rms 

	refMark free_vars sel _ (TupleSelect _ arg_nr expr) rms 
		= refMark free_vars arg_nr No expr rms 
	refMark free_vars sel _ (MatchExpr _ expr) rms 
		= refMark free_vars sel No expr rms 
	refMark free_vars sel _ (IsConstructor expr _ _ _ _ _) rms 
		= refMark free_vars sel No expr rms 
	refMark free_vars sel _ EE rms 
		= rms 
	refMark _ _ _ _ rms 
		= rms 

instance refMark LetBind
where
	refMark free_vars sel _ {lb_src} rms
		= refMark free_vars NotASelector No lb_src rms 

instance refMark Selection
where
	refMark free_vars _ _ (ArraySelection _ _ index_expr) rms 
		= refMark free_vars NotASelector No index_expr rms 
	refMark free_vars _ _ _ rms 
		= rms 

collectPatternsVariables pattern_vars
	= collect_used_vars pattern_vars 0 []
where
	collect_used_vars [ fv=:{fv_count} : pattern_vars ] arg_nr collected_vars
		| fv_count > 0
			= collect_used_vars pattern_vars (inc arg_nr) [{pv_var = fv, pv_arg_nr = arg_nr} : collected_vars]
			= collect_used_vars pattern_vars (inc arg_nr) collected_vars
	collect_used_vars [] arg_nr collected_vars
		= collected_vars

openLetVars let_vars var_heap
	= foldSt open_let_vars let_vars var_heap
where
	open_let_vars {fv_ident,fv_info_ptr} var_heap
		# (VI_Occurrence var_occ, var_heap) = readPtr fv_info_ptr var_heap
		= case var_occ.occ_bind of
			OB_LockedLet occ_bind
				-> var_heap <:= (fv_info_ptr, VI_Occurrence {var_occ & occ_bind = occ_bind})
//					 ===> ("openLetVars (OB_LockedLet)", fv_ident)
			_
				-> abort "open_let_vars (refmark.icl))"

setUsedLetVars used_vars counted_let_vars var_heap
	= foldSt (foldSt set_used_let_var) used_vars (counted_let_vars,var_heap)
where
	set_used_let_var fv=:{fv_info_ptr,fv_ident} (counted_let_vars,var_heap)
		# (VI_Occurrence var_occ, var_heap) = readPtr fv_info_ptr var_heap
		= case var_occ.occ_bind of
			OB_OpenLet _ _
				# var_heap = writePtr fv_info_ptr (VI_Occurrence {var_occ & occ_bind = OB_LockedLet var_occ.occ_bind}) var_heap
				  counted_let_vars = [fv:counted_let_vars]
				-> (counted_let_vars,var_heap)
			_
				-> (counted_let_vars,var_heap)

refMarkOfCase free_vars sel def {case_expr, case_guards=AlgebraicPatterns type patterns, case_explicit, case_default} rms 
	= refMarkOfAlgebraicOrOverloadedListCase free_vars sel def case_expr patterns case_explicit case_default rms 

refMarkOfCase free_vars sel def {case_expr, case_guards=BasicPatterns type patterns,case_default,case_explicit} rms=:{rms_counted_let_vars}
	# (def, all_closed_let_vars, rms) = refMarkOfDefault case_explicit free_vars sel def case_expr case_default [] rms
	  (pattern_depth, all_closed_let_vars, rms) = foldSt (ref_mark_of_basic_pattern free_vars sel def case_expr) patterns (0, all_closed_let_vars, rms)
	  (let_vars_in_default, rms_var_heap) = addRefMarkOfDefault pattern_depth free_vars def rms.rms_var_heap
	  (rms_counted_let_vars,rms_var_heap) = setUsedLetVars [let_vars_in_default : all_closed_let_vars] rms_counted_let_vars rms_var_heap
	  rms_var_heap = parCombine free_vars rms_var_heap
	= {rms & rms_var_heap=rms_var_heap, rms_counted_let_vars=rms_counted_let_vars}
where
	ref_mark_of_basic_pattern free_vars sel def case_expr {bp_expr} (pattern_depth, all_closed_let_vars, rms)
		# (all_closed_let_vars, rms) = refMarkOfAlternative free_vars [] sel def case_expr bp_expr all_closed_let_vars rms
		= (inc pattern_depth, all_closed_let_vars, rms)

refMarkOfCase free_vars sel def {case_expr, case_guards=OverloadedListPatterns type _ patterns, case_explicit, case_default} rms
	= refMarkOfAlgebraicOrOverloadedListCase free_vars sel def case_expr patterns case_explicit case_default rms 

refMarkOfCase free_vars sel def {case_expr, case_guards=NewTypePatterns type patterns, case_explicit, case_default} rms 
	= refMarkOfAlgebraicOrOverloadedListCase free_vars sel def case_expr patterns case_explicit case_default rms 

refMarkOfCase free_vars sel def {case_expr, case_guards=DynamicPatterns patterns,case_default,case_explicit} rms=:{rms_counted_let_vars}
	# (def, all_closed_let_vars, rms) = refMarkOfDefault case_explicit free_vars sel def case_expr case_default [] rms
	  (pattern_depth, all_closed_let_vars, rms) = foldSt (ref_mark_of_dynamic_pattern free_vars sel def case_expr) patterns (0, all_closed_let_vars, rms)
	  (let_vars_in_default, rms_var_heap) = addRefMarkOfDefault pattern_depth free_vars def rms.rms_var_heap
	  (rms_counted_let_vars,rms_var_heap) = setUsedLetVars [let_vars_in_default : all_closed_let_vars] rms_counted_let_vars rms_var_heap
	  rms_var_heap = parCombine free_vars rms_var_heap
	= {rms & rms_var_heap=rms_var_heap, rms_counted_let_vars=rms_counted_let_vars}
where
	ref_mark_of_dynamic_pattern free_vars sel def case_expr {dp_var, dp_rhs} (pattern_depth, all_closed_let_vars, rms=:{rms_var_heap})
		# used_pattern_vars = collectPatternsVariables [dp_var]
		  new_free_vars = [pv_var \\ {pv_var} <- used_pattern_vars]
		  (all_closed_let_vars, rms) = refMarkOfAlternative free_vars new_free_vars sel def case_expr dp_rhs all_closed_let_vars rms
		= (inc pattern_depth, all_closed_let_vars, rms)	

refMarkOfAlgebraicOrOverloadedListCase free_vars sel def (Var var) alternatives case_explicit case_default rms=:{rms_counted_let_vars}
	# (def, all_closed_let_vars, rms) = ref_mark_of_default case_explicit free_vars sel def var case_default rms
	  (pattern_depth, all_closed_let_vars, rms) = foldSt (ref_mark_of_algebraic_pattern free_vars sel var def) alternatives (0, all_closed_let_vars, rms)		
	  (let_vars_in_default, rms_var_heap) = addRefMarkOfDefault pattern_depth free_vars def rms.rms_var_heap
	  (rms_counted_let_vars,rms_var_heap) = setUsedLetVars [let_vars_in_default : all_closed_let_vars] rms_counted_let_vars rms_var_heap
	  rms_var_heap = parCombine free_vars rms_var_heap
	= {rms & rms_var_heap=rms_var_heap, rms_counted_let_vars=rms_counted_let_vars}
where
	ref_mark_of_default case_explicit free_vars sel def var (Yes expr) rms=:{rms_var_heap, rms_let_vars}
		# rms_var_heap = saveOccurrences free_vars rms_var_heap
		  (closed_lets, rms) = fullRefMarkOfAlternative free_vars sel No expr rms_var_heap 
		  (closed_lets, rms) = ref_mark_of_variable_pattern True var (closed_lets, rms)
		  rms_var_heap = openLetVars closed_lets rms.rms_var_heap
		  rms_var_heap = openLetVars rms.rms_counted_let_vars rms_var_heap
		  all_closed_let_vars = [closed_lets,rms.rms_counted_let_vars]
		  (occurrences, rms_var_heap) = restoreOccurrences free_vars rms_var_heap
		= (Yes occurrences, all_closed_let_vars, {rms & rms_var_heap = rms_var_heap, rms_let_vars = rms.rms_let_vars ++ rms_let_vars})
			===>  ("ref_mark_of_default", occurrences, closed_lets)
	ref_mark_of_default case_explicit free_vars sel def var No rms
		| case_explicit
			= (No,	[], rms)
			= (def, [], rms)

	ref_mark_of_algebraic_pattern free_vars sel var def {ap_vars,ap_expr} (pattern_depth, all_closed_let_vars, {rms_var_heap}) 
		# rms_var_heap = saveOccurrences free_vars rms_var_heap
		  used_pattern_vars = collectPatternsVariables ap_vars
		  rms_var_heap = bind_pattern_variable var used_pattern_vars rms_var_heap
		  free_vars = [[pv_var \\ {pv_var} <- used_pattern_vars] : free_vars]
		  (closed_let_vars, rms) = fullRefMarkOfAlternative free_vars sel def ap_expr rms_var_heap
		  rms_var_heap = restore_binding_of_pattern_variable var used_pattern_vars rms.rms_var_heap
		  (closed_let_vars, rms) = ref_mark_of_variable_pattern (isEmpty used_pattern_vars) var (closed_let_vars, {rms & rms_var_heap = rms_var_heap})
		  rms_var_heap = openLetVars closed_let_vars rms.rms_var_heap
		  rms_var_heap = openLetVars rms.rms_counted_let_vars rms_var_heap
		  all_closed_let_vars = [rms.rms_counted_let_vars:all_closed_let_vars]
		= (inc pattern_depth, [closed_let_vars:all_closed_let_vars], {rms & rms_var_heap = rms_var_heap})

	bind_pattern_variable _ [] var_heap
		= var_heap
	bind_pattern_variable {var_info_ptr} used_pattern_vars var_heap
		# (VI_Occurrence var_occ, var_heap) = readPtr var_info_ptr var_heap
		= var_heap <:= (var_info_ptr, VI_Occurrence {var_occ & occ_pattern_vars = [used_pattern_vars : var_occ.occ_pattern_vars]})

	restore_binding_of_pattern_variable _ [] var_heap
		= var_heap
	restore_binding_of_pattern_variable {var_info_ptr} used_pattern_vars var_heap
		# (VI_Occurrence var_occ, var_heap) = readPtr var_info_ptr var_heap
		= var_heap <:= (var_info_ptr, VI_Occurrence {var_occ & occ_pattern_vars = tl var_occ.occ_pattern_vars})

	ref_mark_of_variable_pattern do_seq_combine {var_ident,var_info_ptr,var_expr_ptr} (closed_lets, rms=:{rms_var_heap})
		# (VI_Occurrence var_occ_in_alts, rms_var_heap) = readPtr var_info_ptr rms_var_heap
		  (var_occ_in_alts, rms_var_heap) = adjust_ref_count_of_variable_pattern var_occ_in_alts var_info_ptr var_expr_ptr rms_var_heap
		= add_let_variable do_seq_combine var_info_ptr var_occ_in_alts (closed_lets, {rms & rms_var_heap = rms_var_heap})
	where
		adjust_ref_count_of_variable_pattern var_occ_in_alts=:{occ_ref_count = RC_Unused} var_info_ptr var_expr_ptr var_heap
			# var_occ_in_alts = { var_occ_in_alts & occ_ref_count = RC_Used { rcu_multiply = [], rcu_uniquely = [var_expr_ptr], rcu_selectively = []}}
			= (var_occ_in_alts, var_heap <:= (var_info_ptr, VI_Occurrence var_occ_in_alts))
		adjust_ref_count_of_variable_pattern var_occ_in_alts=:{occ_ref_count = RC_Used rcu} var_info_ptr var_expr_ptr var_heap
			# var_occ_in_alts = { var_occ_in_alts & occ_ref_count = RC_Used { rcu & rcu_uniquely = [var_expr_ptr : rcu.rcu_uniquely] }}
			= (var_occ_in_alts, var_heap <:= (var_info_ptr, VI_Occurrence var_occ_in_alts))

		add_let_variable do_seq_combine var_info_ptr var_occ=:{occ_bind = ob =: OB_OpenLet fv (Yes (ref_counts,let_vars))} (closed_lets, rms=:{rms_var_heap})
			# rms_var_heap = rms_var_heap <:= (var_info_ptr, VI_Occurrence {var_occ & occ_bind = OB_LockedLet ob}) 
			| do_seq_combine
				# rms_var_heap = addSeqRefCounts ref_counts rms_var_heap
				= addSeqRefMarksOfLets let_vars ([fv : closed_lets], { rms & rms_var_heap = rms_var_heap })
				# rms_var_heap = addParRefCounts ref_counts rms_var_heap
				= addParRefMarksOfLets let_vars ([fv : closed_lets], { rms & rms_var_heap = rms_var_heap })
		add_let_variable do_seq_combine var_info_ptr var_occ=:{occ_bind = ob =: OB_OpenLet fv No} (closed_lets, rms=:{rms_var_heap,rms_let_vars})
			# rms_var_heap = rms_var_heap <:= (var_info_ptr, VI_Occurrence {var_occ & occ_bind = OB_LockedLet ob}) 
			= (closed_lets, {rms & rms_var_heap = rms_var_heap, rms_let_vars = [fv : rms_let_vars]})
		add_let_variable do_seq_combine var_info_ptr v_ closed_lets_and_rms
			= closed_lets_and_rms

refMarkOfAlgebraicOrOverloadedListCase free_vars sel def case_expr alternatives case_explicit case_default rms=:{rms_counted_let_vars}
	# (def, all_closed_let_vars, rms) = refMarkOfDefault case_explicit free_vars sel def case_expr case_default [] rms
	  (pattern_depth, all_closed_let_vars, rms) = foldSt (ref_mark_of_algebraic_pattern free_vars sel def case_expr) alternatives (0, all_closed_let_vars, rms)
	  (let_vars_in_default, rms_var_heap) = addRefMarkOfDefault pattern_depth free_vars def rms.rms_var_heap
	  (rms_counted_let_vars,rms_var_heap) = setUsedLetVars [let_vars_in_default : all_closed_let_vars] rms_counted_let_vars rms_var_heap
	  rms_var_heap = parCombine free_vars rms_var_heap
	= {rms & rms_var_heap=rms_var_heap, rms_counted_let_vars=rms_counted_let_vars}
where
	ref_mark_of_algebraic_pattern free_vars sel def case_expr {ap_vars,ap_expr} (pattern_depth, all_closed_let_vars, rms) 
		# used_pattern_vars = collectPatternsVariables ap_vars
		  new_free_vars = [pv_var \\ {pv_var} <- used_pattern_vars]
		  (all_closed_let_vars, rms) = refMarkOfAlternative free_vars new_free_vars sel def case_expr ap_expr all_closed_let_vars rms
		= (inc pattern_depth, all_closed_let_vars, rms)	

refMarkOfDefault case_explicit free_vars sel def case_expr (Yes expr) all_closed_let_vars rms
	# (all_closed_let_vars, rms) = refMarkOfAlternative free_vars [] sel def case_expr expr all_closed_let_vars rms
	  (occurrences, rms_var_heap) = restoreOccurrences free_vars rms.rms_var_heap
	= (Yes occurrences, all_closed_let_vars, { rms & rms_var_heap = rms_var_heap })
		===>  ("refMarkOfDefault", occurrences)
refMarkOfDefault case_explicit free_vars sel def case_expr No all_closed_let_vars rms 
	| case_explicit
		= (No,	all_closed_let_vars, rms)
		= (def, all_closed_let_vars, rms)

refMarkOfAlternative free_vars [] sel def case_expr alt_expr all_closed_let_vars {rms_let_vars,rms_var_heap}
	# rms_var_heap = saveOccurrences free_vars rms_var_heap
	  (closed_let_vars_in_alt, alt_rms) = fullRefMarkOfAlternative free_vars sel def alt_expr rms_var_heap
	  rms_var_heap = saveOccurrences free_vars alt_rms.rms_var_heap
	  (closed_let_vars_in_expr, case_rms) = fullRefMarkOfCaseExpr free_vars sel def case_expr alt_rms.rms_counted_let_vars rms_var_heap
	  rms_var_heap = seqCombine free_vars case_rms.rms_var_heap
	  rms_var_heap = openLetVars closed_let_vars_in_alt rms_var_heap
	  rms_var_heap = openLetVars closed_let_vars_in_expr rms_var_heap
	  rms_var_heap = openLetVars case_rms.rms_counted_let_vars rms_var_heap
	  all_closed_let_vars = [case_rms.rms_counted_let_vars:all_closed_let_vars]
	= ([closed_let_vars_in_alt,closed_let_vars_in_expr:all_closed_let_vars],
			{case_rms & rms_var_heap = rms_var_heap, rms_let_vars = case_rms.rms_let_vars ++ alt_rms.rms_let_vars ++ rms_let_vars})
refMarkOfAlternative free_vars pattern_vars sel def case_expr alt_expr all_closed_let_vars {rms_let_vars,rms_var_heap}
	# rms_var_heap = saveOccurrences [pattern_vars : free_vars] rms_var_heap
	  (closed_let_vars_in_alt_and_expr, alt_and_case_rms) = fullRefMarkOfAlternative [pattern_vars : free_vars] sel def [alt_expr,case_expr] rms_var_heap
	  rms_var_heap = openLetVars closed_let_vars_in_alt_and_expr alt_and_case_rms.rms_var_heap
	  rms_var_heap = openLetVars alt_and_case_rms.rms_counted_let_vars rms_var_heap
	  all_closed_let_vars = [alt_and_case_rms.rms_counted_let_vars:all_closed_let_vars]
	= ([closed_let_vars_in_alt_and_expr:all_closed_let_vars],
			{alt_and_case_rms & rms_var_heap = rms_var_heap, rms_let_vars = alt_and_case_rms.rms_let_vars ++ rms_let_vars})

addSeqRefMarksOfLets let_vars closed_vars_and_rms
	= foldSt ref_mark_of_let let_vars closed_vars_and_rms
where
	ref_mark_of_let fv=:{fv_ident,fv_info_ptr} (closed_let_vars, rms=:{rms_var_heap})
		# (VI_Occurrence var_occ, rms_var_heap) = readPtr fv_info_ptr rms_var_heap
		  rms = { rms & rms_var_heap = rms_var_heap }
		= case var_occ.occ_bind of
			OB_OpenLet _ (Yes (ref_counts, let_vars))
				# rms_var_heap = rms.rms_var_heap <:= (fv_info_ptr, VI_Occurrence {var_occ & occ_bind = OB_LockedLet var_occ.occ_bind})
				  rms_var_heap = addSeqRefCounts ref_counts rms_var_heap
				-> addSeqRefMarksOfLets let_vars ([fv : closed_let_vars], {rms & rms_var_heap = rms_var_heap})
//					  ===> ("addSeqRefMarksOfLets (OB_OpenLet Yes)", fv_ident) 
			OB_OpenLet fv No
				# rms_var_heap = rms.rms_var_heap <:= (fv_info_ptr, VI_Occurrence {var_occ & occ_bind = OB_LockedLet var_occ.occ_bind})
				-> (closed_let_vars, { rms  & rms_var_heap = rms_var_heap, rms_let_vars = [fv : rms.rms_let_vars]})
//					  ===> ("addSeqRefMarksOfLets (OB_OpenLet No)", fv_ident) 
			OB_LockedLet _
				-> (closed_let_vars, rms)
//					  ===> ("addSeqRefMarksOfLets (OB_LockedLet)", fv_ident) 

addRefMarkOfDefault :: !Int ![[FreeVar]] !(Optional [CountedFreeVar]) !*VarHeap -> *(![FreeVar], !*VarHeap)
addRefMarkOfDefault pattern_depth free_vars (Yes occurrences) var_heap
	# var_heap = saveOccurrences free_vars var_heap
	# (open_let_vars, var_heap)  = foldSt set_occurrence occurrences ([], var_heap)
	= (open_let_vars, altCombine (inc pattern_depth) free_vars var_heap)
where
	set_occurrence {cfv_var=fv=:{fv_ident,fv_info_ptr}, cfv_count, cfv_is_let} (open_let_vars, var_heap)
		# (VI_Occurrence old_occ, var_heap) = readPtr fv_info_ptr var_heap
		= (cond_add cfv_is_let fv open_let_vars, var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_ref_count = cfv_count}))
			===>  ("set_occurrence", fv_ident, cfv_count)
	where
		cond_add cond var vars
			| cond
				= [var : vars]
				= vars
addRefMarkOfDefault pattern_depth free_vars No var_heap
	= ([], altCombine pattern_depth free_vars var_heap)

parCombine free_vars var_heap
	= foldSt (foldSt par_combine) free_vars (var_heap===> ("parCombine", free_vars))
where
	par_combine {fv_ident,fv_info_ptr} var_heap
		# (VI_Occurrence old_occ, var_heap) = readPtr fv_info_ptr var_heap
		= case old_occ.occ_previous of
			ReferenceCounts glob_ref_count occ_previous
				# comb_ref_count = parCombineRefCount old_occ.occ_ref_count glob_ref_count
				-> var_heap <:= (fv_info_ptr, VI_Occurrence { old_occ & occ_ref_count = comb_ref_count , occ_previous = occ_previous })
						===> ("par_combine", fv_ident, old_occ.occ_ref_count, glob_ref_count, comb_ref_count)
			ReferenceCountsUnused 1 occ_previous
				-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = occ_previous})
			ReferenceCountsUnused n occ_previous
				#! n=n-1
				-> var_heap <:= (fv_info_ptr, VI_Occurrence {old_occ & occ_previous = ReferenceCountsUnused n occ_previous})
			ReferenceCountsAllUnused
				-> var_heap
			_
				-> abort ("inconsistent reference count administration" ===> fv_ident)

seqCombine free_vars var_heap
	= foldSt (foldSt seq_combine) free_vars (var_heap===> ("seqCombine", free_vars))
where
	seq_combine {fv_ident,fv_info_ptr} var_heap
		# (VI_Occurrence pattern_occ, var_heap) = readPtr fv_info_ptr var_heap
		= case pattern_occ.occ_previous of
			ReferenceCounts alt_ref_count occ_previous
				# comb_ref_count = seqCombineRefCount alt_ref_count pattern_occ.occ_ref_count
				-> var_heap <:= (fv_info_ptr, VI_Occurrence { pattern_occ & occ_ref_count = comb_ref_count , occ_previous = occ_previous })
						===> ("seq_combine", fv_ident, pattern_occ.occ_ref_count, alt_ref_count, comb_ref_count)
			ReferenceCountsUnused 1 occ_previous
				-> var_heap <:= (fv_info_ptr, VI_Occurrence {pattern_occ & occ_previous = occ_previous})
			ReferenceCountsUnused n xs
				#! n=n-1
				-> var_heap <:= (fv_info_ptr, VI_Occurrence {pattern_occ & occ_previous = ReferenceCountsUnused n xs})
			ReferenceCountsAllUnused
				-> var_heap
			_
				-> abort ("inconsistent reference count administration" ===> fv_ident)

altCombine depth free_vars var_heap
	= foldSt (foldSt (alt_combine depth)) free_vars var_heap
where
	alt_combine depth {fv_ident,fv_info_ptr} var_heap
		# (VI_Occurrence old_occ=:{occ_ref_count,occ_previous}, var_heap) = readPtr fv_info_ptr var_heap
		  (occ_ref_count, occ_previous) = alt_combine_ref_counts occ_ref_count occ_previous (dec depth)
											===> ("alt_combine", fv_ident, occ_ref_count, length occ_previous, depth)
		= var_heap <:= (fv_info_ptr, VI_Occurrence { old_occ & occ_ref_count = occ_ref_count , occ_previous = occ_previous })
				
	alt_combine_ref_counts comb_ref_count ref_counts 0
		= (comb_ref_count, ref_counts)
	alt_combine_ref_counts comb_ref_count (ReferenceCounts occ_ref_count occ_previous) depth
		# new_comb_ref_count = alt_combine_ref_count comb_ref_count occ_ref_count
		= alt_combine_ref_counts new_comb_ref_count occ_previous (dec depth)
				===> ("alt_combine_ref_count", comb_ref_count, occ_ref_count, new_comb_ref_count)
	alt_combine_ref_counts comb_ref_count (ReferenceCountsUnused n_unused_ref_count occ_previous) depth
		| n_unused_ref_count<depth
			= alt_combine_ref_counts comb_ref_count occ_previous (depth-n_unused_ref_count)
		| n_unused_ref_count==depth
			= (comb_ref_count, occ_previous)
			#! n_unused_ref_count=n_unused_ref_count-depth
			= (comb_ref_count, ReferenceCountsUnused n_unused_ref_count occ_previous)
	alt_combine_ref_counts comb_ref_count ReferenceCountsAllUnused depth
		= (comb_ref_count, ReferenceCountsAllUnused)

	alt_combine_ref_count RC_Unused ref_count
		= ref_count
	alt_combine_ref_count ref_count RC_Unused
		= ref_count
	alt_combine_ref_count (RC_Used {rcu_multiply,rcu_selectively,rcu_uniquely}) (RC_Used ref_count2)
		= RC_Used { rcu_uniquely = rcu_uniquely ++ ref_count2.rcu_uniquely, rcu_multiply = rcu_multiply ++ ref_count2.rcu_multiply,
					rcu_selectively = alt_combine_of_selections rcu_selectively ref_count2.rcu_selectively}
	where
		alt_combine_of_selections [] sels
			= sels
		alt_combine_of_selections sels []
			= sels
		alt_combine_of_selections sl1=:[sel1=:{ su_field, su_multiply, su_uniquely } : sels1] sl2=:[sel2 : sels2]
			| su_field == sel2.su_field
				# sel1 = { sel1 & su_multiply = sel2.su_multiply ++ su_multiply, su_uniquely =  sel2.su_uniquely ++ su_uniquely }
				= [ sel1 : alt_combine_of_selections sels1 sels2 ]
			| su_field < sel2.su_field
				= [sel1 : alt_combine_of_selections sels1 sl2 ]
				= [sel2 : alt_combine_of_selections sl1 sels2 ]

parCombineRefCount RC_Unused ref_count
	= ref_count
parCombineRefCount ref_count RC_Unused
	= ref_count
parCombineRefCount (RC_Used {rcu_multiply,rcu_selectively,rcu_uniquely}) (RC_Used ref_count2)
	# rcu_multiply = ref_count2.rcu_uniquely ++ ref_count2.rcu_multiply ++ rcu_uniquely ++ rcu_multiply
	| isEmpty rcu_multiply
		= RC_Used { rcu_multiply = [], rcu_uniquely = [], rcu_selectively = par_combine_selections rcu_selectively ref_count2.rcu_selectively }
		# rcu_multiply = collectAllSelections ref_count2.rcu_selectively (collectAllSelections rcu_selectively rcu_multiply)
		= RC_Used { rcu_multiply = rcu_multiply, rcu_uniquely = [], rcu_selectively = [] }
where
	par_combine_selections [] sels
		= sels
	par_combine_selections sels []
		= sels
	par_combine_selections sl1=:[sel1=:{ su_field, su_multiply, su_uniquely } : sels1] sl2=:[sel2 : sels2]
		| su_field == sel2.su_field
			# sel1 = { sel1 & su_multiply = sel2.su_multiply ++ su_multiply ++ sel2.su_uniquely ++ su_uniquely, su_uniquely = [] }
			= [sel1 : par_combine_selections sels1 sels2]
		| su_field < sel2.su_field
			= [sel1 : par_combine_selections sels1 sl2]
			= [sel2 : par_combine_selections sl1 sels2]

seqCombineRefCount RC_Unused ref_count
	= ref_count
seqCombineRefCount ref_count RC_Unused
	= ref_count
seqCombineRefCount (RC_Used sec_ref) (RC_Used prim_ref)
	# rcu_multiply = prim_ref.rcu_uniquely ++ prim_ref.rcu_multiply ++ sec_ref.rcu_multiply
	| isEmpty rcu_multiply
		| isEmpty sec_ref.rcu_uniquely /* so sec_ref contains selections only */
			# rcu_selectively = seq_combine_selections sec_ref.rcu_selectively prim_ref.rcu_selectively /* rcu_selectively can't be empty */
			= RC_Used { rcu_uniquely = [], rcu_multiply = [], rcu_selectively = rcu_selectively }
			# prim_selections = make_primary_selections_non_unique prim_ref.rcu_selectively
			  rcu_selectively = seq_combine_selections sec_ref.rcu_selectively prim_selections
			= RC_Used { sec_ref & rcu_selectively = rcu_selectively }
		= RC_Used { sec_ref & rcu_multiply = collectAllSelections prim_ref.rcu_selectively rcu_multiply }
	where
		seq_combine_selections [] sels
			= sels
		seq_combine_selections sels []
			= sels
		seq_combine_selections sl1=:[sel1=:{ su_field, su_multiply, su_uniquely } : sels1] sl2=:[sel2 : sels2]
			| su_field == sel2.su_field
				# sel1 = { sel1 & su_multiply = sel2.su_multiply ++ sel2.su_uniquely ++ su_multiply }
				= [ sel1 : seq_combine_selections sels1 sels2 ]
			| su_field < sel2.su_field
				= [sel1 : seq_combine_selections sels1 sl2 ]
				= [sel2 : seq_combine_selections sl1 sels2 ]

		make_primary_selections_non_unique [sel=:{su_multiply, su_uniquely } : sels]
			= [ { sel & su_multiply = su_uniquely ++ su_multiply, su_uniquely = [] } : make_primary_selections_non_unique sels ]
		make_primary_selections_non_unique []
			= []

emptyOccurrence type_info =
		{	occ_ref_count		= RC_Unused
		,	occ_previous		= EndReferenceCounts
		,	occ_observing		= type_info
		,	occ_bind			= OB_Empty
		, 	occ_pattern_vars	= []
		}

makeSharedReferencesNonUnique :: ![Int] !u:{# FunDef} !*Coercions !w:{! Type} !v:TypeDefInfos !*VarHeap !*ExpressionHeap !*ErrorAdmin
	-> (!u:{# FunDef}, !*Coercions, !w:{! Type},  !v:TypeDefInfos, !*VarHeap, !*ExpressionHeap, !*ErrorAdmin)
makeSharedReferencesNonUnique [] fun_defs coercion_env subst type_def_infos var_heap expr_heap  error
	= (fun_defs, coercion_env, subst, type_def_infos, var_heap, expr_heap, error)
makeSharedReferencesNonUnique [fun : funs] fun_defs coercion_env subst type_def_infos var_heap expr_heap error
	# (fun_def, fun_defs) = fun_defs![fun] 
	# (coercion_env, subst, type_def_infos, var_heap, expr_heap, error)
		= make_shared_references_of_function_non_unique fun_def coercion_env subst type_def_infos var_heap expr_heap error
	= makeSharedReferencesNonUnique funs fun_defs coercion_env subst type_def_infos var_heap expr_heap error
where
	make_shared_references_of_function_non_unique {fun_ident, fun_pos, fun_body = fun_body =: TransformedBody {tb_args,tb_rhs},fun_info={fi_local_vars}}
			coercion_env subst type_def_infos var_heap expr_heap error
	# variables = tb_args ++ fi_local_vars
	  (subst, type_def_infos, var_heap, expr_heap) = clear_occurrences variables subst type_def_infos var_heap expr_heap
	  {rms_var_heap} = fullRefMarkOfRootOrLetExpr [tb_args] NotASelector No (tb_rhs ===> ("makeSharedReferencesNonUnique", fun_ident, tb_rhs)) [] var_heap
	  position = newPosition fun_ident fun_pos
	  (coercion_env, var_heap, expr_heap, error)
		= make_shared_vars_non_unique variables fun_body coercion_env rms_var_heap expr_heap (setErrorAdmin position error)
	#! var_heap = empty_occurrences variables var_heap
	= (coercion_env, subst, type_def_infos, var_heap, expr_heap, error)
	where
		clear_occurrences vars subst type_def_infos var_heap expr_heap
			= foldSt initial_occurrence vars (subst, type_def_infos, var_heap, expr_heap)
		where
			initial_occurrence {fv_ident,fv_info_ptr} (subst, type_def_infos, var_heap, expr_heap) 
				# (var_info, var_heap) = readPtr fv_info_ptr var_heap
				  {at_type, at_attribute} 	= get_type var_info
				  (expr_ptr, expr_heap)		= newPtr (EI_Attribute (toInt at_attribute)) expr_heap
				| has_observing_type at_type type_def_infos subst
					= (subst, type_def_infos, var_heap <:= (fv_info_ptr, VI_Occurrence (emptyOccurrence (True, expr_ptr))), expr_heap)
					= (subst, type_def_infos, var_heap <:= (fv_info_ptr, VI_Occurrence (emptyOccurrence (False, expr_ptr))), expr_heap)

		empty_occurrences vars var_heap
			= foldSt empty_occurrence vars var_heap
		where
			empty_occurrence {fv_info_ptr} var_heap 
				= var_heap <:= (fv_info_ptr, VI_Empty)

		get_type (VI_Type atype _)			= atype
		get_type (VI_FAType _ atype _)		= atype
		get_type (VI_FATypeC _ atype _ _)	= atype

		make_shared_vars_non_unique vars fun_body coercion_env var_heap expr_heap error
			= foldl make_shared_var_non_unique (coercion_env, var_heap, expr_heap, error) vars
		where
			make_shared_var_non_unique (coercion_env, var_heap, expr_heap, error)  fv=:{fv_ident,fv_info_ptr}
				# (VI_Occurrence occ, var_heap) = readPtr fv_info_ptr var_heap
				= case occ.occ_ref_count of
					RC_Used {rcu_multiply,rcu_selectively}
						# (coercion_env, expr_heap, error) = make_shared_occurrences_non_unique fv rcu_multiply (coercion_env, expr_heap, error)
						  (coercion_env, expr_heap, error) = foldSt (make_selection_non_unique fv) rcu_selectively (coercion_env, expr_heap, error)  
						-> (coercion_env, var_heap, expr_heap, error)
					_
						-> (coercion_env, var_heap, expr_heap, error)
	//						===> ("make_shared_var_non_unique", fv_ident)
	
			make_shared_occurrences_non_unique fv multiply (coercion_env, expr_heap, error)
				= foldSt (make_shared_occurrence_non_unique fv) multiply (coercion_env, expr_heap, error) 
			
			make_shared_occurrence_non_unique free_var var_expr_ptr (coercion_env, expr_heap, error) 
				| isNilPtr var_expr_ptr
					= (coercion_env, expr_heap, error)
					# (expr_info, expr_heap) = readPtr var_expr_ptr expr_heap
					= case expr_info of
						EI_Attribute sa_attr_nr
							# (succ, coercion_env) = tryToMakeNonUnique sa_attr_nr coercion_env
							| succ
									 ===> ("make_shared_occurrence_non_unique", free_var, var_expr_ptr, sa_attr_nr)
								-> (coercion_env, expr_heap, error)
								-> (coercion_env, expr_heap, uniquenessErrorVar free_var fun_body " demanded attribute cannot be offered by shared object" error)
						EI_FPContext _ var_expr_ptr
							-> make_shared_occurrence_non_unique free_var var_expr_ptr (coercion_env, expr_heap, error)
						_
							-> abort ("make_shared_occurrence_non_unique" ===> ((free_var, var_expr_ptr) )) // <<- expr_info))
	
			make_selection_non_unique fv {su_multiply} cee
				= make_shared_occurrences_non_unique fv su_multiply cee

	has_observing_type (TB basic_type) type_def_infos subst
		= True
	has_observing_type (TempV var_number) type_def_infos subst
		= case subst.[var_number] of
			TE
				-> True
			subst_type
				-> 	has_observing_type subst_type type_def_infos subst
	has_observing_type (TA {type_index = {glob_object,glob_module}} type_args) type_def_infos subst
		# {tdi_properties} = type_def_infos.[glob_module].[glob_object]
		= tdi_properties bitand cIsHyperStrict <> 0 && args_have_observing_type type_args type_def_infos subst
	has_observing_type (TAS {type_index = {glob_object,glob_module}} type_args _) type_def_infos subst
		# {tdi_properties} = type_def_infos.[glob_module].[glob_object]
		= tdi_properties bitand cIsHyperStrict <> 0 && args_have_observing_type type_args type_def_infos subst
	has_observing_type type type_def_infos subst
		= False

	args_have_observing_type [{at_type}:type_args] type_def_infos subst
		= has_observing_type at_type type_def_infos subst && args_have_observing_type type_args type_def_infos subst
	args_have_observing_type [] type_def_infos subst
		= True

instance <<< ReferenceCount
where
	(<<<) file RC_Unused = file
	(<<<) file (RC_Used {rcu_multiply,rcu_uniquely,rcu_selectively}) = file <<< '\n' <<< "M:" <<< rcu_multiply <<< " U:" <<< rcu_uniquely <<< " S:" <<< rcu_selectively

instance <<< SelectiveUse
where
	(<<<) file {su_field,su_multiply,su_uniquely} = file <<< su_field <<< " M:" <<< su_multiply <<< " U:" <<< su_uniquely


instance <<< (Ptr v)
where
	(<<<) file ptr = file <<< '[' <<< ptrToInt ptr <<< ']'


instance <<< CountedFreeVar
where
	(<<<) file {cfv_var,cfv_count} = file <<< cfv_var <<< ':' <<< cfv_count

instance <<< PatternVar
where
	(<<<) file {pv_var} = file <<< pv_var

