implementation module coredump

import classify
from CoclSystemDependent import DirectorySeparator
from filesystem import ensureDirectoryExists

dumpCore :: !*{! Group} Int [IndexRange] !.IclModule !.DclModule !*{# FunDef} {!ConsClasses} .Int .Int *Files -> *(!*{! Group},!*{# FunDef},!*Files)
dumpCore components start_rule_index exported_global_functions 
	{icl_name,icl_global_functions,icl_instances,icl_specials,icl_gencases} 
	{dcl_instances,dcl_specials,dcl_gencases} 
	fun_defs acc_args def_min def_max files
	# dump_folder = "Dumps"
	# (ok,files) = ensureDirectoryExists "Dumps" files
	| not ok
		= abort ("can't create folder \"" +++ dump_folder +++"\"\n")
	# (ok1,f,files) = fopen (dump_folder+++{DirectorySeparator}+++icl_name.id_name+++".dump") FWriteText files
	# [icl_exported_global_functions,icl_not_exported_global_functions:_] = icl_global_functions
	# f = showRanges "icl_global_functions" icl_global_functions f
	# f = showRanges "icl_instances" icl_instances f
	# f = showRanges "icl_specials" [icl_specials] f
	# f = showRanges "icl_gencases" icl_gencases f
	# f = showRanges "icl_exported_global_functions" icl_exported_global_functions f
	# f = showRanges "start_rule_index" start_rule_index f
	# f = showRanges "exported_global_functions" exported_global_functions f
	# f = showRanges "dcl_instances" dcl_instances f
	# f = showRanges "dcl_specials" dcl_specials f
	# f = showRanges "dcl_gencases" dcl_gencases f
	# (components, fun_defs, acc_args, f) = showComponents2 components 0 fun_defs acc_args def_min def_max f
	  (components, fun_defs, f) = showComponents3 components 0 True fun_defs f
	  (ok2,files) = fclose f files
	= (components,fun_defs,files)
where
	showRanges name range file
		# file = file <<< name <<< ":\n"
		# file = file <<< range
		# file = file <<< '\n'
		= file

	showComponents2 :: !*{! Group} !Int !*{# FunDef} !u:{! ConsClasses} !Int !Int !*File  -> (!*{! Group},!*{# FunDef},!u:{! ConsClasses},!*File)
	showComponents2 comps comp_index fun_defs acc_args acc_min acc_max file
		| comp_index >= (size comps)
			= (comps, fun_defs, acc_args, file)
		# (comp, comps) = comps![comp_index]
		# (fun_defs, acc_args, file) = show_component comp.group_members fun_defs acc_args file
		= showComponents2 comps (inc comp_index) fun_defs acc_args acc_min acc_max file
	where
		show_component :: ![Int] !*{# FunDef} !u:{! ConsClasses} !*File -> (!*{# FunDef}, !u:{! ConsClasses}, !*File)
		show_component [] fun_defs acc_args file
			= (fun_defs, acc_args, file <<< '\n')
		show_component [fun:funs] fun_defs acc_args file
			# (fd, fun_defs) = fun_defs![fun]
			| fun >= acc_max && fun < acc_min
				# file = file <<< fd.fun_ident <<< '@' <<< fun <<< " ???"
				= show_component funs fun_defs acc_args file
			# file = file <<< fd.fun_ident <<< '@' <<< fun <<< " ("
			# (acc_arg,acc_args)
				 = case fun < acc_min of
							True	-> acc_args![fun]
							_		-> acc_args![fun-acc_min+acc_max]
			# file = show_producer_status acc_arg.cc_producer file
			# file = show_accumulating_arguments acc_arg.cc_args file
			# file = show_linear_arguments acc_arg.cc_linear_bits file
			= show_component funs fun_defs acc_args (file <<< ") ")
	
	show_producer_status pc file
		| pc == True
			= file <<< "+:"
			= file <<< "-:"
	
	show_accumulating_arguments [ cc : ccs] file
		| cc == CPassive
			= show_accumulating_arguments ccs (file <<< 'p')
		| cc == CActive
			= show_accumulating_arguments ccs (file <<< 'c')
		| cc == CAccumulating
			= show_accumulating_arguments ccs (file <<< 'a')
		| cc == CVarOfMultimatchCase
			= show_accumulating_arguments ccs (file <<< 'm')
		| cc == CUnusedLazy
			= show_accumulating_arguments ccs (file <<< 'u')
		| cc == CUnusedStrict
			= show_accumulating_arguments ccs (file <<< 's')
			= show_accumulating_arguments ccs (file <<< '?')
	show_accumulating_arguments [] file
		= file

	show_linear_arguments [ cc : ccs] file
		| cc == True
			= show_linear_arguments ccs (file <<< 'l')
			= show_linear_arguments ccs (file <<< 'n')
	show_linear_arguments [] file
		= file

// -----------------------------------------------------------------------------------------------------------------------		

showComponents3 :: !*{! Group} !Int !Bool !*{# FunDef} !*File  -> (!*{! Group}, !*{# FunDef},!*File)
showComponents3 comps comp_index show_types fun_defs file
	# pp_state
		= InitPPState file
	# (comps,fun_defs,{file})
		= showComponents4 comps comp_index show_types fun_defs pp_state
	= (comps,fun_defs,file)
where
	showComponents4 comps comp_index show_types fun_defs pp_state
		| comp_index >= size comps
			= (comps, fun_defs, pp_state)
			
			# (comp, comps) = comps![comp_index]
			# (fun_defs, pp_state) = show_component comp.group_members show_types fun_defs (pp_state <#< "\ncomponent " <#< comp_index <#< '\n')
			= showComponents4 comps (inc comp_index) show_types fun_defs pp_state
	where
		show_component [] show_types fun_defs pp_state
			= (fun_defs, pp_state <#< '\n')

		show_component [fun:funs] show_types fun_defs pp_state
			# (fun_def, fun_defs) = fun_defs![fun]
			# pp_state = { pp_state & function_index = fun}
			| show_types
				= show_component funs show_types fun_defs (pp_state <#< fun_def.fun_type <#< '\n' <#< fun_def)
				= show_component funs show_types fun_defs (pp_state <#< fun_def)

:: PPState = {
		file							:: !.File
	,	indent_level					:: !Int
	,	last_character_written_was_nl	:: !Bool
	,	write_indent					:: !Bool
	,	function_index					:: !Int
	}
	
InitPPState :: !*File -> *PPState
InitPPState file
	= { PPState |
		file							 = file
	,	indent_level					 = 0
	,	last_character_written_was_nl	 = False
	, 	write_indent					 = True
	,	function_index					 = 0
	}
	
deltaIndent	:== 4

IndentLevelForward pp_state=:{indent_level}
	# pp_state
		= { pp_state & indent_level = indent_level + deltaIndent }
	# pp_state
		= pp_state <#< '\n'
	= pp_state

IndentLevelBackward pp_state=:{indent_level}
	# pp_state
		= { pp_state & indent_level = indent_level - deltaIndent }
	# pp_state
		= pp_state <#< '\n'
	= pp_state


PPState_writes :: !{#.Char} !*PPState -> .PPState	
PPState_writes s pp_state
	#! (pp_state=:{file})
		= write_indent_if_necessary pp_state
	
	#! file
		= fwrites s file
	= {pp_state & file = file, last_character_written_was_nl = False}
where 
	write_indent 0 file
		= file
	write_indent i file
		= write_indent (dec i) (fwritec ' ' file)
	
	
	
class (<#<) infixl a :: !*PPState !a -> *PPState

instance <#< {#Char}
where
	(<#<) pp_state s
		#! (pp_state=:{file})
			= write_indent_if_necessary pp_state
		# file
			= fwrites s file
		= { pp_state & file = file, last_character_written_was_nl = False }
		
instance <#< Int
where
	(<#<) pp_state i
		#! (pp_state=:{file})
			= write_indent_if_necessary pp_state

		# file
			= fwrites (toString i) file
		= { pp_state & file = file, last_character_written_was_nl = False }
		
	
instance <#< CheckedAlternative
where
	(<#<) pp_state ca=:{ca_rhs,ca_position}
		= pp_state <#< ("CheckedAlternative",ca_rhs);	

write_indent_if_necessary pp_state=:{file,indent_level,write_indent=True}
	# file
		= write_indent indent_level file
	= {pp_state & file = file, write_indent = False}
where
		write_indent 0 file
			= file
		write_indent i file
			= write_indent (dec i) (fwritec ' ' file)
write_indent_if_necessary pp_state
	= pp_state
	
instance <#< Char
where
	(<#<) pp_state=:{file,indent_level,last_character_written_was_nl} '\n'
		#! file
			= case last_character_written_was_nl of
				True
					-> file
				_	
					# file
						= fwritec '\n' file
					-> file
		= { pp_state & file = file, last_character_written_was_nl = True, write_indent = True }
	where 
		write_indent 0 file
			= file
		write_indent i file
			= write_indent (dec i) (fwritec ' ' file)

	(<#<) pp_state c
		#! (pp_state=:{file})
			= write_indent_if_necessary pp_state
		# file
			= fwritec c file
		= { pp_state & file = file, last_character_written_was_nl = False }

	
instance <#< Bool
where
	(<#<) pp_state bool 
		#! (pp_state=:{file})
			= write_indent_if_necessary pp_state

		# file
			= fwrites (toString bool) file
		= { pp_state & file = file, last_character_written_was_nl = False }

instance <#< (a,b) | <#< a & <#< b
where
	(<#<) pp_state (x,y) = pp_state <#< '(' <#< x <#< ", " <#< y <#< ") "

instance <#< (a,b,c) | <#< a & <#< b & <#< c
where
	(<#<) pp_state (x,y,z) = pp_state <#< '(' <#< x <#< ", " <#< y <#< ", " <#< z <#< ") "

instance <#< (a,b,c,d) | <#< a & <#< b & <#< c & <#< d
where
	(<#<) pp_state (w,x,y,z) = pp_state <#< '(' <#< w <#< ", " <#< x <#< ", " <#< y <#< ", " <#< z <#< ") "

instance <#< (a,b,c,d,e) | <#< a & <#< b & <#< c & <#< d & <#< e
where
	(<#<) pp_state (v,w,x,y,z) = pp_state <#< '(' <#< v <#< ", " <#< w <#< ", " <#< x <#< ", " <#< y <#< ", " <#< z <#< ") "


instance <#< (a,b,c,d,e,f,g) | <#< a & <#< b & <#< c & <#< d & <#< e & <#< f & <#< g
where
	(<#<) pp_state (v,w,x,y,z,f,g) = pp_state <#< '(' <#< v <#< ", " <#< w <#< ", " <#< x <#< ", " <#< y <#< ", " <#< z <#< ", " <#< f <#< ", " <#< g <#< ") "

instance <#< [a] | <#< a
where
	(<#<) pp_state [] = pp_state
	(<#<) pp_state l  = showTail (pp_state <#< "[") l
	where
		showTail f [x]   = f <#< x  <#< "] "
		showTail f [a:x] = showTail (f <#< a <#< ", ") x
		showTail f []    = f <#< "] "
		
		
// COMPILER


instance <#< BasicType
where
	(<#<) pp_state BT_Int			= pp_state <#< "Int"
	(<#<) pp_state BT_Char			= pp_state <#< "Char"
	(<#<) pp_state BT_Real			= pp_state <#< "Real"
	(<#<) pp_state BT_Bool			= pp_state <#< "Bool"
	(<#<) pp_state (BT_String _)	= pp_state <#< "String" 
	(<#<) pp_state BT_Dynamic		= pp_state <#< "Dynamic"
	(<#<) pp_state BT_File			= pp_state <#< "File"
	(<#<) pp_state BT_World			= pp_state <#< "World"

instance <#< TypeVar
where
	(<#<) pp_state varid = pp_state <#< varid.tv_ident 

instance <#< AttributeVar
where
	(<#<) pp_state {av_ident,av_info_ptr} = pp_state <#< av_ident 

instance <#< AType
where
	(<#<) pp_state {at_attribute,at_type}
		= pp_state <#< at_attribute <#< at_type

instance <#< TypeAttribute
where
	(<#<) pp_state ta
		= pp_state <#< toString ta

instance <#< Annotation
where
	(<#<) pp_state an = pp_state <#< toString an

instance <#< ATypeVar
where
	(<#<) pp_state {atv_attribute,atv_variable}
		= pp_state <#< atv_attribute <#< atv_variable

instance <#< ConsVariable
where
	(<#<) pp_state (CV tv)
		= pp_state <#< tv
	(<#<) pp_state (TempCV tv)
		= pp_state <#<  "v" <#< tv <#< ' ' 
	(<#<) pp_state (TempQCV tv)
		= pp_state <#<  "q" <#< tv <#< ' ' 

instance <#< Type
where
	(<#<) pp_state (TA consid types)
		= pp_state  <#< consid <#< " " <#< types
	(<#<) pp_state (TAS consid types strictness)
		= pp_state  <#< consid <#< " [" <#< strictness <#< "] "<#< types
	(<#<) pp_state (arg_type --> res_type)
		= pp_state <#< arg_type <#< " -> " <#< res_type
	(<#<) pp_state (TArrow)
		= pp_state <#<  " (->) "
	(<#<) pp_state (TArrow1 arg_type)
		= pp_state <#<  " ((->) " <#< arg_type <#<") "
	(<#<) pp_state (type :@: types)
		= pp_state <#< type <#< " @" <#< types
	(<#<) pp_state (TB tb)
		= pp_state <#< tb
	(<#<) pp_state (TFA vars types)
		= pp_state <#< "A." <#< vars <#< ':' <#< types
	(<#<) pp_state (GTV varid)
		= pp_state <#< varid
	(<#<) pp_state (TV varid)
		= pp_state <#< varid
	(<#<) pp_state (TempV tv_number)
		= pp_state  <#< "TempV "  <#< tv_number <#< ' ' 
	(<#<) pp_state (TQV varid)
		= pp_state <#< "E." <#< varid
	(<#<) pp_state (TempQV tv_number)
		= pp_state  <#< "E." <#< tv_number <#< ' ' 
	(<#<) pp_state (TLifted varid)
		= pp_state <#< "TLifted " <#< varid
	(<#<) pp_state TE
		= pp_state <#< "TE"

instance <#< StrictnessList where
	(<#<) pp_state NotStrict		= pp_state <#< '.'
	(<#<) pp_state (Strict i)		= pp_state <#< '!' <#< i
	(<#<) pp_state (StrictList i l)	= pp_state <#< '!' <#< i <#< l

instance <#< SymbolType
where
	(<#<) pp_state st=:{st_vars,st_attr_vars,st_args_strictness}
		| st.st_arity == 0
			= write_inequalities st.st_attr_env (write_contexts st.st_context (pp_state <#< '[' <#< st_vars <#< ',' <#< st_attr_vars <#< ']' <#< st.st_result))
			= write_inequalities st.st_attr_env (write_contexts st.st_context (pp_state <#< '[' <#< st_vars <#< ',' <#< st_attr_vars <#< ']' <#< st.st_args <#< " -> " <#< st.st_result))

write_contexts [] pp_state
	= pp_state
write_contexts [tc : tcs] pp_state
	= write_contexts2 tcs (pp_state <#< " | " <#< tc) 
where
	write_contexts2 [] pp_state
		= pp_state
	write_contexts2 [tc : tcs] pp_state
		= write_contexts2 tcs (pp_state <#< " & " <#< tc)

instance <#< AttrInequality
where
	(<#<) pp_state {ai_demanded,ai_offered}
		= pp_state <#< ai_offered <#< " <= " <#< ai_demanded
	
write_inequalities [] pp_state
	= pp_state
write_inequalities [ineq:ineqs] pp_state
	= write_remaining_inequalities ineqs (pp_state <#< ',' <#< ineq)
where
	write_remaining_inequalities [] pp_state
		= pp_state
	write_remaining_inequalities [ineq:ineqs] pp_state
		= write_remaining_inequalities ineqs (pp_state <#< ' ' <#< ineq)

instance <#< TypeContext
where
	(<#<) pp_state co=:{tc_class,tc_types,tc_var}
		# pp_state
			= pp_state <#< "TC\n"
		# pp_state
			= pp_state <#< "tc_class: " <#< tc_class <#< "\n"
		# pp_state
			= pp_state <#< "tc_types: " <#< tc_types <#< "\n"
		# pp_state
			= pp_state <#< "tc_var: " <#< ptrToInt tc_var <#< "\n"
		= pp_state
	
instance <#< SymbIdent
where
	(<#<) pp_state symb=:{symb_kind = SK_Function symb_index } = pp_state <#< symb.symb_ident <#<  '@' <#< symb_index
	(<#<) pp_state symb=:{symb_kind = SK_GeneratedFunction _ symb_index } = pp_state <#< symb.symb_ident <#<  '@' <#< symb_index
	(<#<) pp_state symb=:{symb_kind = SK_OverloadedFunction symb_index } = pp_state  <#< symb.symb_ident <#<  "[o]@" <#< symb_index
	(<#<) pp_state symb=:{symb_kind = SK_LocalMacroFunction symb_index } = pp_state <#< symb.symb_ident <#<  '@' <#< symb_index
	(<#<) pp_state symb = pp_state <#< symb.symb_ident <#< '?'

instance <#< TypeSymbIdent
where
	(<#<) pp_state symb	= pp_state <#< symb.type_ident <#< '.' <#< symb.type_index

instance <#< BoundVar
where
	(<#<) pp_state {var_ident,var_info_ptr,var_expr_ptr}
		= pp_state <#< var_ident <#< '<' <#< ptrToInt var_info_ptr <#< '>'

instance <#< (Bind a b) | <#< a & <#< b 
where
	(<#<) pp_state {bind_src,bind_dst} = pp_state <#< bind_dst <#<  " = " <#< bind_src 

instance <#< LetBind
where
	(<#<) pp_state {lb_dst,lb_src} = pp_state <#< lb_dst <#<  " = " <#< lb_src 

instance <#< AlgebraicPattern
where
	(<#<) pp_state g = pp_state <#< g.ap_symbol <#< g.ap_vars <#< " -> " <#< g.ap_expr

instance <#< BasicPattern
where
	(<#<) pp_state g 
		# pp_state
			= pp_state <#< g.bp_value
		# pp_state
			= IndentLevelForward pp_state
		# pp_state
			= pp_state <#< "-> " <#< g.bp_expr
		# pp_state
			= IndentLevelBackward pp_state
		= pp_state
	
instance <#< CasePatterns
where
	(<#<) pp_state (AlgebraicPatterns type patterns) = pp_state <#< patterns
	(<#<) pp_state (BasicPatterns type patterns) = pp_state <#<patterns
	(<#<) pp_state (DynamicPatterns patterns) = pp_state <#< patterns
	(<#<) pp_state (OverloadedListPatterns olt expr patterns) = pp_state <#< "( " <#< olt <#< " : " <#< expr <#< ")" <#< patterns
	(<#<) pp_state NoPattern = pp_state 

instance <#< OverloadedListType
where
	(<#<) pp_state (UnboxedList list_type_symbol strict_lists_module_index decons_u_index nil_u_index)
		= pp_state <#< "UnboxedList (" <#< list_type_symbol <#< "," <#< strict_lists_module_index 
			<#< "," <#< decons_u_index <#< "," <#< nil_u_index <#< ")"
	(<#<) pp_state (UnboxedTailStrictList list_type_symbol strict_lists_module_index decons_uts_index nil_uts_index)
		= pp_state <#< "UnboxedTailStrictList (" <#< list_type_symbol <#< "," <#< strict_lists_module_index 
			<#< "," <#< decons_uts_index <#< "," <#< nil_uts_index <#< ")"
	(<#<) pp_state (OverloadedList list_type_symbol strict_lists_module_index decons_index nil_index)
		= pp_state <#< "OverloadedList (" <#< list_type_symbol <#< "," <#< strict_lists_module_index 
			<#< "," <#< decons_index <#< "," <#< nil_index <#< ")"

instance <#< BasicValue
where
	(<#<) pp_state (BVI int)	= pp_state <#< int
	(<#<) pp_state (BVInt int)	= pp_state <#< int
	(<#<) pp_state (BVC char)	= pp_state <#< char
	(<#<) pp_state (BVB bool)	= pp_state <#< bool
	(<#<) pp_state (BVR real)	= pp_state <#< real
	(<#<) pp_state (BVS string)	= pp_state <#< string

instance <#< Expression
where
	(<#<) pp_state (Var ident) = pp_state <#< ident
	(<#<) pp_state (App {app_symb, app_args, app_info_ptr})
		= pp_state <#< app_symb <#< ' ' <#< app_args
	(<#<) pp_state (f_exp @ a_exp) = pp_state <#< '(' <#< f_exp <#< " @ " <#< a_exp <#< ')'
	(<#<) pp_state (Let {let_info_ptr, let_strict_binds, let_lazy_binds, let_expr}) 
			#! pp_state
				= pp_state <#< "let" 

			#! pp_state
				= IndentLevelForward pp_state
			#! pp_state
				= write_binds "!" pp_state let_strict_binds

							
			#! pp_state
				= write_binds "" pp_state let_lazy_binds

			#! pp_state
				= IndentLevelBackward pp_state
	
			// in
			#! pp_state
				= pp_state <#< "in"
			#! pp_state
				= IndentLevelForward pp_state
			#! pp_state 
				= pp_state <#< let_expr
			#! pp_state
				= IndentLevelBackward pp_state
			= pp_state
	where
		write_binds x pp_state []
			= pp_state
		write_binds x pp_state [bind : binds]
			# s
				= if (isEmpty binds) ' ' '\n'
			= write_binds x (pp_state <#< x <#< " " <#< bind <#< s <#< '\n') binds
			
			
 	(<#<) pp_state (Case {case_expr,case_guards,case_default=No,case_explicit})
			#! pp_state
				= pp_state <#< "case" <#< (if case_explicit '!' '?') <#< " " <#< case_expr <#< " of"

			#! pp_state
				= IndentLevelForward pp_state
			#! pp_state
				= pp_state <#< case_guards
				
			#! pp_state
				= IndentLevelBackward pp_state
			= pp_state

	(<#<) pp_state (Case {case_expr,case_guards,case_default= Yes def_expr,case_explicit})
			#! pp_state
				= pp_state <#< "case" <#< (if case_explicit '!' '?') <#< " " <#< case_expr <#< " of"

			#! pp_state
				= IndentLevelForward pp_state
			#! pp_state
				= pp_state <#< case_guards
				
			// default		
			#! pp_state
				= pp_state <#< "_DEF_" 
			#! pp_state
				= IndentLevelForward pp_state
			#! pp_state	
				= pp_state <#< "-> " <#< def_expr
			#! pp_state
				= IndentLevelBackward pp_state
			#! pp_state
				= IndentLevelBackward pp_state
			= pp_state

	(<#<) pp_state (BasicExpr basic_value) = pp_state <#< basic_value
	(<#<) pp_state (Conditional {if_cond,if_then,if_else}) =
			else_part (pp_state <#< "IF " <#< if_cond <#< '\n' <#< "THEN\n" <#< if_then) if_else
	where
		else_part pp_state No = pp_state <#< '\n'
		else_part pp_state (Yes else) = pp_state <#< '\n' <#< "ELSE\n" <#< else <#< '\n'

 	(<#<) pp_state (Selection selector_kind expr selectors) = pp_state <#< expr <#< selector_kind <#< selectors
	(<#<) pp_state (Update expr1 selections expr2) =  pp_state <#< '{' <#< expr1  <#< " & " <#<  selections <#< " = " <#< expr2 <#< '}'
	(<#<) pp_state (RecordUpdate cons_symbol expression expressions) = pp_state <#< '{' <#< cons_symbol <#< ' ' <#< expression <#< " & " <#< expressions <#< '}'
	(<#<) pp_state (TupleSelect field field_nr expr) = pp_state <#< expr <#<'.' <#< field_nr
	(<#<) pp_state WildCard = pp_state <#< '_'
	(<#<) pp_state (MatchExpr cons expr) = pp_state <#< cons <#< " =: " <#< expr
	(<#<) pp_state EE = pp_state <#< "** E **"
	(<#<) pp_state (NoBind _) = pp_state <#< "** NB **"
	(<#<) pp_state (FailExpr id) = pp_state <#< "** FAIL " <#< id <#< " **"
	(<#<) pp_state (DynamicExpr {dyn_expr,dyn_type_code})     = pp_state <#< "**dynamic " <#< dyn_expr /*<#< " :: dyn_uni_vars"*/ /*) dyn_uni_vars*/ <#< '\n' //<#< "dyn_type_code=" <#< dyn_type_code 
	(<#<) pp_state (TypeCodeExpression type_code)      = pp_state <#< type_code
	(<#<) pp_state (Constant symb _ _ _)         = pp_state <#<  "** Constant **" <#< symb
	(<#<) pp_state (ABCCodeExpr code_sequence do_inline)      = pp_state <#< (if do_inline "code inline\n" "code\n") <#< code_sequence
	(<#<) pp_state (AnyCodeExpr input output code_sequence)   = pp_state <#< "code\n" <#< input <#< '\n' <#< "" <#< output <#< '\n' <#< "" <#< code_sequence
	(<#<) pp_state (FreeVar {fv_ident})         	= pp_state <#< fv_ident
	(<#<) pp_state (ClassVariable info_ptr)     = pp_state <#< "ClassVariable " <#< ptrToInt info_ptr
	(<#<) pp_state expr         				= abort ("<#< (Expression) [line 1290]" )
	
instance <#< SelectorKind where
	(<#<) pp_state NormalSelector						= pp_state <#< '.'
	(<#<) pp_state NormalSelectorUniqueElementResult	= pp_state <#< '?'
	(<#<) pp_state UniqueSelector						= pp_state <#< '!'

instance <#< ParsedSelectorKind where
	(<#<) pp_state ParsedNormalSelector					= pp_state <#< '.'
	(<#<) pp_state (ParsedUniqueSelector True)			= pp_state <#< '?'
	(<#<) pp_state (ParsedUniqueSelector False)			= pp_state <#< '!'

instance <#< TypeCase
where
	(<#<) pp_state {type_case_dynamic,type_case_patterns,type_case_default}
			= pp_state <#< "typecase " <#< type_case_dynamic <#< "of\n" <#<
				type_case_patterns <#< type_case_default

instance <#< DynamicPattern
where
	(<#<) pp_state {dp_type_patterns_vars,dp_var,dp_rhs,dp_type_code}
			= writeVarPtrs (pp_state <#< dp_var <#< " :: ")  dp_type_patterns_vars <#<  dp_type_code <#< " = " <#< dp_rhs

writeVarPtrs pp_state []
	= pp_state
writeVarPtrs pp_state vars
	= write_var_ptrs (pp_state <#< '<') vars <#< '>'
	where
		write_var_ptrs pp_state [var]
			= pp_state <#< ptrToInt var
		write_var_ptrs pp_state [var : vars]
			= write_var_ptrs (pp_state <#< ptrToInt var <#< '.') vars
		
		
instance <#< TypeCodeExpression
where
	(<#<) pp_state TCE_Empty
		= pp_state
	(<#<) pp_state (TCE_Var info_ptr)
		= pp_state <#< "TCE_Var " <#< ptrToInt info_ptr
	(<#<) pp_state (TCE_TypeTerm info_ptr)
		= pp_state <#< "TCE_TypeTerm " <#< ptrToInt info_ptr
	(<#<) pp_state (TCE_Constructor index _ exprs)
		= pp_state <#< "TCE_Constructor " <#< index <#< ' ' <#< exprs
	(<#<) pp_state (TCE_Selector selectors info_ptr)
		= pp_state <#< "TCE_Selector " <#< selectors <#< "VAR " <#< ptrToInt info_ptr
	(<#<) pp_state (TCE_UniType info_ptrs expr)
		= pp_state <#< "TCE_UniType "

instance <#< Selection
where
	(<#<) pp_state (RecordSelection selector _) = pp_state <#< selector
	(<#<) pp_state (ArraySelection _ _ index_expr) = pp_state <#< '[' <#< index_expr <#< ']'
	(<#<) pp_state (DictionarySelection var selections _ index_expr) = pp_state <#< '(' <#< var <#< '.' <#< selections <#< ')' <#< '[' <#< index_expr <#< ']'

instance <#< FunKind
where
	(<#<) pp_state (FK_Function False) = pp_state <#< "FK_Function"
	(<#<) pp_state (FK_Function True) = pp_state <#< "Lambda"
	(<#<) pp_state FK_Macro = pp_state <#< "FK_Macro"
	(<#<) pp_state FK_Caf = pp_state <#< "FK_Caf"
	(<#<) pp_state FK_Unknown = pp_state <#< "FK_Unknown"

instance <#< FunDef
where
	(<#<) pp_state=:{function_index} {fun_ident,fun_body=TransformedBody {tb_args,tb_rhs},fun_info={fi_free_vars,fi_def_level,fi_calls}} 
		# pp_state
			=  pp_state <#< "\nFunction: " <#< fun_ident <#< '@' <#< function_index <#< '\n' <#< tb_args <#< '[' <#< fi_calls <#< ']' <#< " = "
		# pp_state
			= IndentLevelForward pp_state
		# pp_state
			= pp_state <#< '\n' <#< tb_rhs <#< '\n'
		# pp_state
			= IndentLevelBackward pp_state
		= pp_state
	(<#<) pp_state=:{function_index} {fun_ident,fun_body=NoBody,fun_type=Yes type}
		= pp_state <#< type <#< '\n' <#< fun_ident <#< '.'	<#< function_index <#< "Array function\n"
	(<#<) pp_state=:{function_index} {fun_ident,fun_body=Expanding vars,fun_type=Yes type}
		= pp_state <#< type <#< '\n' <#< fun_ident <#< '.'	<#< function_index <#< "Expanding function\n"

instance <#< FunCall
where
	(<#<) pp_state (FunCall fc_index fc_level)
			= pp_state <#< fc_index <#< '.' <#< fc_level
	(<#<) pp_state (DclFunCall fc_module fc_index)
			= pp_state <#< fc_index <#< ':' <#< fc_module
	(<#<) pp_state (MacroCall index1 index2 level)
			= pp_state <#< index1 <#< '.' <#< index2 <#< '.' <#< level

instance <#< FreeVar
where
	(<#<) pp_state {fv_ident,fv_info_ptr,fv_count}
			= pp_state <#< fv_ident <#< '.' <#< fv_count <#< '<' <#< ptrToInt fv_info_ptr <#< '>'

instance <#< DynamicType
where
	(<#<) pp_state {dt_uni_vars,dt_type}
		| isEmpty dt_uni_vars
			= pp_state <#< "DynamicType" <#< dt_type
			= pp_state <#< "DynamicType" <#< "A." <#< dt_uni_vars <#< ":" <#< dt_type
			

instance <#< SignClassification
where
	(<#<) pp_state {sc_pos_vect,sc_neg_vect} = write_signs pp_state sc_pos_vect sc_neg_vect 0
	where
		write_signs pp_state sc_pos_vect sc_neg_vect index
			| sc_pos_vect == 0 && sc_neg_vect == 0
				= pp_state
			#	index_bit = (1 << index)
			| sc_pos_vect bitand index_bit == 0
				| sc_neg_vect bitand index_bit == 0
					= write_signs (pp_state <#< 'O') sc_pos_vect sc_neg_vect (inc index)
					= write_signs (pp_state <#< '-') sc_pos_vect (sc_neg_vect bitand (bitnot index_bit)) (inc index)
				| sc_neg_vect bitand index_bit == 0
					= write_signs (pp_state <#< '+') (sc_pos_vect bitand (bitnot index_bit)) sc_neg_vect (inc index)
					= write_signs (pp_state <#< 'T') (sc_pos_vect bitand (bitnot index_bit)) (sc_neg_vect bitand (bitnot index_bit)) (inc index)
				
instance <#< TypeKind
where
	(<#<) pp_state (KindVar _) = pp_state <#< "**"
	(<#<) pp_state KindConst
		= pp_state <#< '*'
	(<#<) pp_state (KindArrow arity)
		= pp_state <#< "TypeKind" //write_kinds pp_state arity
	where
		write_kinds pp_state 1
			= pp_state <#< "* -> *"
		write_kinds pp_state n
			= write_kinds (pp_state <#< "* -> ") (dec n)
		
instance <#< GlobalIndex
where
	(<#<) pp_state {gi_module,gi_index} = pp_state <#< gi_module <#< ':' <#< gi_index

instance <#< TypeDefInfo
where
	(<#<) pp_state {tdi_group,tdi_group_vars,tdi_cons_vars}
		= pp_state <#< '[' <#< tdi_group <#< '=' <#< tdi_group_vars <#< '=' <#< tdi_cons_vars <#< ']'

instance <#< DefinedSymbol
where
	(<#<) pp_state {ds_ident,ds_index}
		= pp_state <#< ds_ident

instance <#< (TypeDef a) | <#< a
where
	(<#<) pp_state {td_ident, td_args, td_rhs}
		= pp_state <#< ":: " <#< td_ident <#< ' ' <#< td_args <#< td_rhs

instance <#< TypeRhs
where
	(<#<) pp_state (SynType type)
		= pp_state <#< " :== " <#< type 
	(<#<) pp_state (AlgType data)
		= pp_state <#< " = " <#< data 
	(<#<) pp_state (RecordType record)
		= pp_state <#< " = " <#< '{' <#< record <#< '}'
	(<#<) pp_state _
		= pp_state 

instance <#< RecordType
where
	(<#<) pp_state {rt_fields} = iFoldSt (\index pp_state -> pp_state <#< rt_fields.[index]) 0 (size rt_fields) pp_state

instance <#< FieldSymbol
where
	(<#<) pp_state {fs_ident} = pp_state <#< fs_ident

instance <#< InstanceType
where
	(<#<) pp_state it = write_contexts it.it_context (pp_state <#< it.it_types) 

instance <#< ModuleKind
where
	(<#<) pp_state kind 		= pp_state

instance <#< ConsDef
where
	(<#<) pp_state {cons_ident,cons_type} = pp_state <#< cons_ident <#< " :: " <#< cons_type

instance <#< SelectorDef
where
	(<#<) pp_state {sd__ident} = pp_state <#< sd__ident

instance <#< ClassDef
where
	(<#<) pp_state {class_ident} = pp_state <#< class_ident

instance <#< ClassInstance
where
	(<#<) pp_state {ins_class,ins_type} = pp_state <#< ins_class <#< " :: " <#< ins_type

instance <#< (Optional a) | <#< a
where
	(<#<) pp_state (Yes x) = pp_state <#< x
	(<#<) pp_state No = pp_state
	
instance <#< (Module a) | <#< a
where
	(<#<) pp_state {mod_ident,mod_type,mod_defs} = pp_state <#< mod_type <#< mod_ident <#< mod_defs

instance <#< (CollectedDefinitions a b) | <#< a & <#< b
where
	(<#<) pp_state {def_types,def_constructors,def_selectors,def_macros,def_classes,def_members,def_instances}
		= pp_state

instance <#< IndexRange
where
	(<#<) pp_state {ir_from,ir_to}
		| ir_from == ir_to
			= pp_state
			= pp_state <#< ir_from <#< "---" <#< ir_to

instance <#< Ident
where
//	(<#<) pp_state {id_name,id_index} = pp_state <#< id_name <#< '@' <#< id_index
	(<#<) pp_state {id_name,id_info} = pp_state <#< id_name

instance <#< (Global a) | <#< a
where
	(<#<) pp_state {glob_object,glob_module} = pp_state <#< glob_object

instance <#< Position
where
	(<#<) pp_state (FunPos pp_state_name line func) = pp_state <#< '[' <#< pp_state_name <#< ',' <#< line <#< ',' <#< func <#< ']'
	(<#<) pp_state (LinePos pp_state_name line) = pp_state <#< '[' <#< pp_state_name <#< ',' <#< line <#< ']'
	(<#<) pp_state _ = pp_state

instance <#< TypeVarInfo
where
	(<#<) pp_state TVI_Empty				= pp_state <#< "TVI_Empty"
	(<#<) pp_state (TVI_Type _)				= pp_state <#< "TVI_Type"
	(<#<) pp_state (TVI_Forward	_) 			= pp_state <#< "TVI_Forward"
	(<#<) pp_state (TVI_TypeKind _)			= pp_state <#< "TVI_TypeKind"
	(<#<) pp_state (TVI_SignClass _ _ _) 	= pp_state <#< "TVI_SignClass"
	(<#<) pp_state (TVI_PropClass _ _ _) 	= pp_state <#< "TVI_PropClass"

instance <#< (Import from_symbol) | <#< from_symbol
where
	(<#<) pp_state {import_module, import_symbols}
		= pp_state <#< "import " <#< import_module <#< import_symbols

instance <#< ImportDeclaration
where
	(<#<) pp_state (ID_Function ident)			= pp_state <#< ident
	(<#<) pp_state (ID_Class ident optIdents)	= pp_state <#< "class " <#< ident <#< optIdents
	(<#<) pp_state (ID_Type ident optIdents)	= pp_state <#< ":: " <#< ident <#< optIdents
	(<#<) pp_state (ID_Record ident optIdents)	= pp_state <#< ident <#< " { " <#< optIdents <#< " } "
	(<#<) pp_state (ID_Instance i1 i2 tup)		= pp_state <#< "instance " <#< i1 <#< i2 <#< tup // !ImportedIdent !Ident !(![Type],![TypeContext])

instance <#< ImportedIdent
where
	(<#<) pp_state {ii_ident, ii_extended}	= pp_state <#< ii_ident <#< ' ' <#< ii_extended

instance <#< TCClass
where
	(<#<) pp_state (TCClass class_sym)
		= pp_state <#< "C: " <#< class_sym
	(<#<) pp_state (TCGeneric gen_sym)
		= pp_state <#< "TCGeneric ..."

instance <#< VarInfo
where
	(<#<) pp_state VI_Empty
		= pp_state <#< "VI_Empty"
	(<#<) pp_state l
		= pp_state <#< "VarInfo (unimplemented <#< VarInfo)"
		
		
instance <#< {!a} | <#< a
/* // */
						& select_u, size_u, <#< a	/* Clean 1.3 only */
/* // /*
*/ */
where
	(<#<) pp_state array
		# list
			= [0..dec (size array)]
		=  (foldSt f list (pp_state <#< "{")) <#< "}"
	where
		f index pp_state
			# pp_state 
				= pp_state <#< array.[index]
			# pp_state
				= case (index < dec (size array)) of
					True	-> pp_state <#< ",";
					False 	-> pp_state
			= pp_state 
