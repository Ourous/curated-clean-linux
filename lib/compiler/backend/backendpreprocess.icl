/*
	module owner: Ronny Wichers Schreur
*/
implementation module backendpreprocess

// assign sequence numbers to all variables in the syntax tree

import checksupport
import Heap
import backendsupport
// import RWSDebug

backEndPreprocess :: !Ident ![Index] !IclModule !*VarHeap -> *VarHeap
backEndPreprocess aliasDummyId functionIndices iclModule varHeap
	=	preprocess aliasDummyId
					[iclModule.icl_functions.[i] \\ i <- functionIndices] varHeap

class preprocess a :: !Ident a !*PreprocessState -> *PreprocessState
:: PreprocessState
	:==	VarHeap

instance preprocess {#a} | preprocess a & Array {#} a where
	preprocess aliasDummyId array pst
		=	foldStateA (preprocess aliasDummyId) array pst

instance preprocess [a] | preprocess a where
	preprocess aliasDummyId list pst
		=	foldState (preprocess aliasDummyId) list pst

// +++ this assigns sequence numbers per function, should be per alternative and move to backendconvert
instance preprocess FunDef where
	preprocess aliasDummyId funDef pst
		=	fromSequencerToPreprocessor aliasDummyId (sequence funDef.fun_body) pst

class sequence a :: a -> Sequencer
:: Sequencer
	:== *SequenceState -> *SequenceState
:: SequenceState
	=	{ss_sequenceNumber :: !Int, ss_varHeap :: .VarHeap, ss_aliasDummyId :: !Ident}

toSequenceState aliasDummyId varHeap
	:==	{ss_sequenceNumber = 0, ss_varHeap = varHeap, ss_aliasDummyId = aliasDummyId}
fromSequenceState sequenceState
	:==	sequenceState.ss_varHeap

fromSequencerToPreprocessor aliasDummyId sequencer
	:==	toSequenceState aliasDummyId
	o`	sequencer
	o`	fromSequenceState

assignSequenceNumber :: VarInfoPtr *SequenceState -> *SequenceState
assignSequenceNumber varInfoPtr sequenceState
	# (varInfo, ss_varHeap) = readPtr varInfoPtr sequenceState.ss_varHeap
	| alreadySequenced varInfo
		=	sequenceState
	// otherwise
		=	{	sequenceState
			&	ss_varHeap = writePtr varInfoPtr (VI_SequenceNumber sequenceState.ss_sequenceNumber) sequenceState.ss_varHeap
			,	ss_sequenceNumber = sequenceState.ss_sequenceNumber + 1
			}
	where
		alreadySequenced :: VarInfo -> Bool
		alreadySequenced (VI_SequenceNumber _)
			=	True
		alreadySequenced (VI_AliasSequenceNumber _)
			=	True
		alreadySequenced _
			=	False

instance sequence [a] | sequence a where
	sequence list
		=	foldState sequence list

instance sequence (Optional a) | sequence a where
	sequence (Yes x)
		=	sequence x
	sequence No
		=	identity

// +++ this assigns sequence numbers per function, should be per alternative and moved to backendconvert
instance sequence FunctionBody where
	sequence (TransformedBody transformedBody)
		=	sequence transformedBody
	sequence body
		=	abort "preprocess (FunctionBody): unknown body"// <<- body

// case test ...
instance sequence TransformedBody where
	sequence body
		=	sequence body.tb_args
		o`	sequence body.tb_rhs
// ... case test

instance sequence FreeVar where
	sequence freeVar
		=	sequence freeVar.fv_info_ptr

instance sequence Expression where
	sequence (Let {let_strict_binds, let_lazy_binds, let_expr})
		=	sequence let_strict_binds
		o`	sequence let_lazy_binds
		o`	sequence let_expr
	sequence (Conditional {if_cond, if_then, if_else})
		=	sequence if_cond
		o`	sequence if_then
		o`	sequence if_else
	sequence (App {app_args})
		=	sequence app_args
	sequence (f @ arg)
		=	sequence f
		o`	sequence arg
	sequence (Selection _ exp selections)
		=	sequence exp
		o`	sequence selections
	sequence (AnyCodeExpr _ outParams _)
		=	foldState (\{bind_dst}->sequence bind_dst) outParams
	sequence (Case caseExpr)
		=	sequence caseExpr 
	sequence _
		=	identity

instance sequence Case where
	sequence {case_expr, case_guards, case_default}
		=	sequence case_expr
		o`	sequence case_guards
		o`	sequence case_default

instance sequence CasePatterns where
	sequence (AlgebraicPatterns _ patterns)
		=	sequence patterns
	sequence (BasicPatterns _ patterns)
		=	sequence patterns
	sequence (OverloadedListPatterns _ decons_expr patterns)
		=	sequence patterns

instance sequence AlgebraicPattern where
	sequence {ap_vars, ap_expr}
		=	sequence ap_vars
		o`	sequence ap_expr

instance sequence BasicPattern where
	sequence {bp_expr}
		=	sequence bp_expr

instance sequence Selection where
	sequence (RecordSelection _ _)
		=	identity
	sequence (ArraySelection _ _ index)
		=	sequence index
	sequence (DictionarySelection dictionaryVar dictionarySelections _ index)
		=	sequence index

instance sequence LetBind where
	sequence {lb_src=App app , lb_dst}
		= sequence` app lb_dst
	  where
	  	sequence` {app_symb, app_args} lb_dst sequenceState=:{ss_aliasDummyId}
			| not (isNilPtr app_symb.symb_ident.id_info) // nilPtr's are generated for Case's with case_ident=No in convertcases
				&& app_symb.symb_ident==ss_aliasDummyId
				// the compiled source was a strict alias like "#! x = y"
				= case hd app_args of
					Var bound_var=:{var_info_ptr}
						# sequenceState = assignSequenceNumber var_info_ptr sequenceState
						  (vi, ss_varHeap) = readPtr var_info_ptr sequenceState.ss_varHeap
						  non_alias_bound_var = case vi of
													VI_SequenceNumber _		-> bound_var
													VI_AliasSequenceNumber alias_bound_var-> alias_bound_var
						  ss_varHeap = writePtr lb_dst.fv_info_ptr (VI_AliasSequenceNumber non_alias_bound_var) ss_varHeap
						-> { sequenceState & ss_varHeap = ss_varHeap }
					_
						-> sequence lb_dst sequenceState
		= sequence lb_dst sequenceState
	sequence bind
		=	sequence bind.lb_dst

instance sequence (Ptr VarInfo) where
	sequence varInfoPtr
		=	assignSequenceNumber varInfoPtr
