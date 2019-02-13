implementation module postparse

import StdEnv
import syntax, parse, utilities, containers, compare_types
import genericsupport

:: *CollectAdmin =
	{	ca_error		:: !*ParseErrorAdmin
	,	ca_fun_count	:: !Int
	,	ca_rev_fun_defs	:: ![FunDef]
	,	ca_hash_table	:: !*HashTable
	}

cIsAGlobalDef		:== True
cIsNotAGlobalDef	:== False

predef_ident_expr :: Int -> ParsedExpr
predef_ident_expr index
	=	PE_Ident predefined_idents.[index]

optGuardedAltToRhs :: OptGuardedAlts -> Rhs
optGuardedAltToRhs optGuardedAlt
	=	{	rhs_alts	= optGuardedAlt
		,	rhs_locals	= LocalParsedDefs []
		}

exprToRhs expr 
	:==	{	rhs_alts	= UnGuardedExpr (exprToExprWithLocalDefs expr)
		,	rhs_locals	= LocalParsedDefs []
		}

exprToExprWithLocalDefs expr
	:== {	ewl_nodes	= []
		,	ewl_expr	= expr
		,	ewl_locals = LocalParsedDefs []
		,	ewl_position= NoPos
		}

prefixAndPositionToIdent :: !String !LineAndColumn !*CollectAdmin -> (!Ident, !*CollectAdmin)
prefixAndPositionToIdent prefix {lc_line, lc_column} ca=:{ca_hash_table}
	# ({boxed_ident=ident}, ca_hash_table) = putIdentInHashTable (prefix +++ ";" +++ toString lc_line +++ ";" +++ toString lc_column) IC_Expression ca_hash_table
	= (ident, { ca & ca_hash_table = ca_hash_table } )

prefixAndPositionToIdentExp :: !String !LineAndColumn !*CollectAdmin -> (!ParsedExpr, !*CollectAdmin)
prefixAndPositionToIdentExp prefix {lc_line, lc_column} ca=:{ca_hash_table}
	# ({boxed_ident=ident}, ca_hash_table) = putIdentInHashTable (prefix +++ ";" +++ toString lc_line +++ ";" +++ toString lc_column) IC_Expression ca_hash_table
	= (PE_Ident ident, { ca & ca_hash_table = ca_hash_table } )

(`) infixl 9
(`) f a
	:== apply f (toParsedExpr a)

// apply :: ParsedExpr ParsedExpr -> ParsedExpr

apply :: ParsedExpr ParsedExpr -> ParsedExpr
apply (PE_List application) a
	=	PE_List (application ++ [a])
apply f a
	=	PE_List [f, a]

class toParsedExpr a :: !a -> ParsedExpr

instance toParsedExpr [a] | toParsedExpr a where
	toParsedExpr []
		=	predef_ident_expr PD_NilSymbol
	toParsedExpr [hd:tl]
		=	predef_ident_expr PD_ConsSymbol ` hd ` tl

instance toParsedExpr ParsedExpr where
	toParsedExpr x
		=	x

instance toParsedExpr Int where
	toParsedExpr x
		=	PE_Basic (BVInt x)

postParseError :: Position {#Char} *CollectAdmin -> *CollectAdmin
postParseError pos msg ps=:{ca_error={pea_file}}
	# (filename, line, funname) = get_file_and_line_nr pos
	  pea_file	= pea_file <<< "Error [" <<< filename <<< "," <<< line
	  pea_file	= case funname of
	  				Yes name	-> pea_file <<< "," <<< name
	  				No			-> pea_file
	  pea_file	= pea_file <<< "]: " <<< msg <<< ".\n"
	= {ps & ca_error = { pea_file = pea_file, pea_ok = False }}
where
	get_file_and_line_nr :: Position -> (FileName, LineNr, Optional FunctName)
	get_file_and_line_nr (FunPos filename linenr funname)
		= (filename, linenr, Yes funname)
	get_file_and_line_nr (LinePos filename linenr)
		= (filename, linenr, No)

addFunctionsRange :: [FunDef] *CollectAdmin -> (IndexRange, *CollectAdmin)
addFunctionsRange fun_defs ca
	# (frm, ca) = ca!ca_fun_count
	  ca = foldSt add_function fun_defs ca
	  (to, ca) = ca!ca_fun_count
	= ({ir_from = frm, ir_to = to}, ca)
	where
		add_function :: FunDef !*CollectAdmin -> *CollectAdmin
		add_function fun_def ca=:{ca_fun_count, ca_rev_fun_defs}
			=	{ca & ca_fun_count = ca.ca_fun_count + 1
					, ca_rev_fun_defs = [fun_def : ca.ca_rev_fun_defs]
				}

MakeNewImpOrDefFunction name arity body kind prio opt_type pos
	:== { fun_ident = name, fun_arity = arity, fun_priority = prio, fun_type = opt_type, fun_kind = kind,
		  fun_body = ParsedBody body, fun_pos = pos, fun_lifted = 0, fun_info = EmptyFunInfo }

class collectFunctions a :: a Bool !*CollectAdmin -> (a, !*CollectAdmin)

instance collectFunctions ParsedExpr
where
	collectFunctions (PE_List exprs) icl_module ca
		# (exprs, ca) = collectFunctions exprs icl_module ca
		= (PE_List exprs, ca)
	collectFunctions (PE_Bound bound_expr) icl_module ca
		# (bound_expr, ca) = collectFunctions bound_expr icl_module ca
		= (PE_Bound bound_expr, ca)
	collectFunctions (PE_Lambda lam_ident args rhs pos) icl_module ca
		# ((args,rhs), ca) = collectFunctions (args,rhs) icl_module ca
		# (range, ca) = addFunctionsRange [transformLambda lam_ident args rhs pos] ca
		= (PE_Let (CollectedLocalDefs { loc_functions = range, loc_nodes = [], loc_in_icl_module=icl_module })
				(PE_Ident lam_ident), ca)
	collectFunctions (PE_Record rec_expr type_ident fields) icl_module ca
		# ((rec_expr,fields), ca) = collectFunctions (rec_expr,fields) icl_module ca
		= (PE_Record rec_expr type_ident fields, ca)
	collectFunctions (PE_Tuple exprs) icl_module ca
		# (exprs, ca) = collectFunctions exprs icl_module ca
		= (PE_Tuple exprs, ca)
	collectFunctions (PE_Selection is_unique expr selectors) icl_module ca
		# ((expr, selectors), ca) = collectFunctions (expr, selectors) icl_module ca
		= (PE_Selection is_unique expr selectors, ca)
	collectFunctions (PE_Update expr1 updates expr2) icl_module ca
		# ((expr1, (updates, expr2)), ca) = collectFunctions (expr1, (updates, expr2)) icl_module ca
		= (PE_Update expr1 updates expr2, ca)
	collectFunctions (PE_Case case_ident pattern_expr case_alts) icl_module ca
		# ((pattern_expr,case_alts), ca) = collectFunctions (pattern_expr,case_alts) icl_module ca
		= (PE_Case case_ident pattern_expr case_alts, ca)
	collectFunctions (PE_If if_ident c t e) icl_module ca
		# case_alts			= [ {calt_pattern = PE_Basic (BVB True), calt_rhs = exprToRhs t, calt_position=NoPos}
							  , {calt_pattern = PE_WildCard        , calt_rhs = exprToRhs e, calt_position=NoPos}]
		  (c, ca) = collectFunctions c icl_module ca
		  (case_alts, ca) = collectFunctions case_alts icl_module ca
		= (PE_Case if_ident c case_alts, ca)
	collectFunctions (PE_Let locals in_expr) icl_module ca
		# ((node_defs,in_expr), ca) = collectFunctions (locals,in_expr) icl_module ca
		= (PE_Let node_defs in_expr, ca)
	collectFunctions (PE_ListCompr predef_cons_index predef_nil_index expr qualifiers) icl_module ca
		# (compr, ca) = transformListComprehension predef_cons_index predef_nil_index expr qualifiers ca
		= collectFunctions compr icl_module ca
	collectFunctions (PE_ArrayCompr array_kind expr qualifiers) icl_module ca
		# (compr, ca) = transformArrayComprehension array_kind expr qualifiers ca
		=	collectFunctions compr icl_module ca
	collectFunctions (PE_UpdateComprehension expr updateExpr identExpr qualifiers) icl_module ca
		# (compr, ca) = transformUpdateComprehension [expr] [updateExpr] [identExpr] identExpr qualifiers ca
		= collectFunctions compr icl_module ca
	collectFunctions (PE_Sequ sequence) icl_module ca
		= collectFunctions (transformSequence sequence) icl_module ca
	collectFunctions (PE_ArrayDenot array_kind exprs) icl_module ca
		= collectFunctions (transformArrayDenot array_kind exprs) icl_module ca
	collectFunctions (PE_Matches case_ident expr pattern position) icl_module ca
		# (expr, ca) = collectFunctions expr icl_module ca
		= (PE_Matches case_ident expr pattern position, ca)
	collectFunctions (PE_Dynamic exprs opt_dyn_type) icl_module ca
		# (exprs, ca) = collectFunctions exprs icl_module ca
		= (PE_Dynamic exprs opt_dyn_type, ca)
	collectFunctions (PE_TypeSignature array_kind expr) icl_module ca
		# (expr, ca) = collectFunctions expr icl_module ca
		= (PE_TypeSignature array_kind expr,ca)
	collectFunctions expr icl_module ca
		= (expr, ca)

instance collectFunctions [a] | collectFunctions a
where
	collectFunctions l icl_module ca
//		=	mapSt collectFunctions l icl_module ca
		= map_st l ca
		where
			map_st [x : xs] s
			 	# (x, s) = collectFunctions x icl_module s
				  (xs, s) = map_st xs s
				#! s = s
				= ([x : xs], s)
			map_st [] s
			 	= ([], s)

instance collectFunctions (a,b) | collectFunctions a & collectFunctions b
where
	collectFunctions (x,y) icl_module ca
		# (x, ca) = collectFunctions x icl_module ca
		  (y, ca) = collectFunctions y icl_module ca
		= ((x,y), ca)

instance collectFunctions Qualifier
where
	collectFunctions qual=:{qual_generators,qual_let_defs,qual_filter} icl_module ca
		# (qual_let_defs, ca) = collectFunctions qual_let_defs icl_module ca
		# ((qual_generators,qual_filter), ca) = collectFunctions (qual_generators,qual_filter) icl_module ca
		= ({ qual & qual_generators = qual_generators, qual_filter = qual_filter }, ca) 

instance collectFunctions Generator
where
	collectFunctions gen=:{gen_pattern,gen_expr} icl_module ca
		# ((gen_pattern,gen_expr), ca) = collectFunctions (gen_pattern,gen_expr) icl_module ca
		= ({gen & gen_pattern = gen_pattern, gen_expr = gen_expr}, ca)

instance collectFunctions (Optional a) | collectFunctions a
where
	collectFunctions (Yes expr) icl_module ca
		# (expr, ca) = collectFunctions expr icl_module ca
		= (Yes expr, ca) 
	collectFunctions No icl_module ca
		= (No, ca) 

instance collectFunctions ParsedSelection
where
	collectFunctions (PS_Array index_expr) icl_module ca
		# (index_expr, ca) = collectFunctions index_expr icl_module ca
		= (PS_Array index_expr, ca)
	collectFunctions expr icl_module ca
		= (expr, ca)

instance collectFunctions CaseAlt
where
	collectFunctions calt=:{calt_pattern,calt_rhs} icl_module ca
		# ((calt_pattern,calt_rhs), ca) = collectFunctions (calt_pattern,calt_rhs) icl_module ca
		= ({calt & calt_pattern = calt_pattern, calt_rhs = calt_rhs}, ca) 

instance collectFunctions (Bind a b) | collectFunctions a & collectFunctions b
where
	collectFunctions bind=:{bind_src,bind_dst} icl_module ca
		# ((bind_src,bind_dst), ca) = collectFunctions (bind_src,bind_dst) icl_module ca
		= ({bind & bind_src = bind_src, bind_dst = bind_dst }, ca)

instance collectFunctions OptGuardedAlts
where
	collectFunctions (GuardedAlts guarded_exprs (Yes def_expr)) icl_module ca
		# ((guarded_exprs, def_expr), ca) = collectFunctions (guarded_exprs, def_expr) icl_module ca
		= (GuardedAlts guarded_exprs (Yes def_expr), ca)
	collectFunctions (GuardedAlts guarded_exprs No) icl_module ca
		# (guarded_exprs, ca) = collectFunctions guarded_exprs icl_module ca
		= (GuardedAlts guarded_exprs No, ca)
	collectFunctions (UnGuardedExpr unguarded_expr) icl_module ca
		# (unguarded_expr, ca) = collectFunctions unguarded_expr icl_module ca
		= (UnGuardedExpr unguarded_expr, ca)

instance collectFunctions GuardedExpr
where
	collectFunctions alt=:{alt_nodes,alt_guard,alt_expr} icl_module ca
		# ((alt_nodes, (alt_guard, alt_expr)), ca) =
				collectFunctions (alt_nodes, (alt_guard, alt_expr)) icl_module ca
		= ({alt & alt_nodes = alt_nodes, alt_guard = alt_guard, alt_expr = alt_expr}, ca)

instance collectFunctions ExprWithLocalDefs
where
	collectFunctions expr=:{ewl_nodes, ewl_expr,ewl_locals} icl_module ca
		# ((ewl_nodes, (ewl_expr, ewl_locals)), ca) = collectFunctions (ewl_nodes, (ewl_expr, ewl_locals)) icl_module ca
		= ({expr & ewl_nodes = ewl_nodes, ewl_expr = ewl_expr, ewl_locals = ewl_locals}, ca)

instance collectFunctions NodeDefWithLocals
where
	collectFunctions node_def=:{ndwl_def, ndwl_locals} icl_module ca
		# (( ndwl_def, ndwl_locals), ca) = collectFunctions (ndwl_def, ndwl_locals) icl_module ca
		= ({node_def & ndwl_def = ndwl_def, ndwl_locals = ndwl_locals}, ca)

instance collectFunctions Rhs
where
	collectFunctions {rhs_alts, rhs_locals} icl_module ca
		# ((rhs_alts, rhs_locals), ca) = collectFunctions (rhs_alts, rhs_locals) icl_module ca
		= ({rhs_alts = rhs_alts, rhs_locals = rhs_locals}, ca)

instance collectFunctions LocalDefs
where
	collectFunctions (LocalParsedDefs locals) icl_module ca
		# (fun_defs, node_defs, ca) = reorganiseLocalDefinitions locals ca
		  (node_defs, ca) = collect_functions_in_node_defs node_defs ca
		  (fun_defs, ca) = collectFunctions fun_defs icl_module ca
		  (range, ca) = addFunctionsRange fun_defs ca
		= (CollectedLocalDefs { loc_functions = range, loc_nodes = node_defs, loc_in_icl_module=icl_module }, ca)
	where
		reorganiseLocalDefinitions :: [ParsedDefinition] *CollectAdmin -> ([FunDef],[NodeDef ParsedExpr],*CollectAdmin)
		reorganiseLocalDefinitions [PD_NodeDef pos pattern {rhs_alts,rhs_locals} : defs] ca
			# (fun_defs, node_defs, ca) = reorganiseLocalDefinitions defs ca
			= (fun_defs, [{ nd_dst = pattern, nd_alts = rhs_alts, nd_locals = rhs_locals, nd_position = pos } : node_defs], ca)

		reorganiseLocalDefinitions [PD_Function pos name is_infix [] {rhs_alts, rhs_locals} FK_NodeDefOrFunction : defs] ca
			# (fun_defs, node_defs, ca) = reorganiseLocalDefinitions defs ca
			= (fun_defs, [{ nd_dst = PE_Ident name, nd_alts = rhs_alts, nd_locals = rhs_locals, nd_position = pos } : node_defs], ca)

		reorganiseLocalDefinitions [PD_Function pos name is_infix args rhs fun_kind : defs] ca
			# prio = if is_infix DefaultPriority NoPrio
			  fun_arity = length args
			  (bodies, fun_kind, defs, ca) = collectFunctionBodies name fun_arity prio fun_kind defs ca
			  (fun_defs, node_defs, ca) = reorganiseLocalDefinitions defs ca
			  fun = MakeNewImpOrDefFunction name fun_arity [{ pb_args = args, pb_rhs = rhs, pb_position = pos } : bodies ] fun_kind prio No pos
			= ([ fun : fun_defs ], node_defs, ca)
		reorganiseLocalDefinitions [PD_TypeSpec pos1 name1 prio type specials : defs] ca
			= case defs of
				[PD_Function pos name is_infix args rhs fun_kind : othe] // PK ..
					| fun_kind == FK_Caf 
						# ca = postParseError pos "No typespecification for local graph definitions allowed" ca // .. PK
						-> reorganiseLocalDefinitions (tl defs) ca
					| belongsToTypeSpec name1 prio name is_infix
						# fun_arity = determineArity args type
						# (bodies, fun_kind, defs, ca) = collectFunctionBodies name1 fun_arity prio fun_kind defs ca
						  (fun_defs, node_defs, ca) = reorganiseLocalDefinitions defs ca
						  fun = MakeNewImpOrDefFunction name fun_arity bodies fun_kind prio type pos1
						-> ([fun : fun_defs], node_defs, ca)
						-> reorganiseLocalDefinitions defs (postParseError pos "function body expected" ca)
				[PD_NodeDef pos pattern=:(PE_Ident id) rhs : defs]
					| not (belongsToTypeSpec name1 prio id False)
						-> reorganiseLocalDefinitions defs (postParseError pos "function body expected" ca)
					| arity type<>0
						-> reorganiseLocalDefinitions defs (postParseError pos "this alternative has not enough arguments" ca)
					# (fun_defs, node_defs, ca) = reorganiseLocalDefinitions defs ca
					  fun = MakeNewImpOrDefFunction id 0 
					  				[{ pb_args = [], pb_rhs = rhs, pb_position = pos }]
					  				(FK_Function cNameNotLocationDependent) prio type pos1
					-> ([fun : fun_defs], node_defs, ca)
				_
					-> reorganiseLocalDefinitions defs (postParseError pos1 "function body expected" ca)
		  where
			arity (Yes {st_arity}) = st_arity
			arity No = 2 // it was specified as infix
		reorganiseLocalDefinitions [] ca
			= ([], [], ca)

		collect_functions_in_node_defs :: [NodeDef ParsedExpr] *CollectAdmin -> ([NodeDef ParsedExpr],*CollectAdmin)
		collect_functions_in_node_defs [ bind : node_defs ] ca
			# (bind, ca) = collectFunctions bind icl_module ca
			  (node_defs, ca) = collect_functions_in_node_defs node_defs ca
			= ([bind:node_defs], ca)
		collect_functions_in_node_defs [] ca
			= ([], ca)
// RWS ... +++ remove recollection
	collectFunctions locals icl_module ca
		=	(locals, ca)
// ... RWS

instance collectFunctions (NodeDef a) | collectFunctions a
where
	collectFunctions node_def=:{nd_dst,nd_alts,nd_locals} icl_module ca
		# ((nd_dst,(nd_alts,nd_locals)), ca) = collectFunctions (nd_dst,(nd_alts,nd_locals)) icl_module ca
		= ({ node_def & nd_dst = nd_dst, nd_alts = nd_alts, nd_locals = nd_locals }, ca)

instance collectFunctions Ident
where
	collectFunctions e icl_module ca
		= (e, ca)

instance collectFunctions FieldNameOrQualifiedFieldName
where
	collectFunctions e icl_module ca
		= (e, ca)

instance collectFunctions (ScannedInstanceAndMembersR FunDef) where
	collectFunctions inst=:{sim_members} icl_module ca
		# (sim_members, ca) = collectFunctions sim_members icl_module ca
		= ({inst & sim_members = sim_members }, ca)

instance collectFunctions GenericCaseDef where
	collectFunctions gc=:{gc_gcf=GCF gc_ident gcf=:{gcf_body=GCB_FunDef fun_def}} icl_module ca
		# (fun_def, ca) = collectFunctions fun_def icl_module ca 
		= ({gc & gc_gcf = GCF gc_ident {gcf & gcf_body=GCB_FunDef fun_def}}, ca)
	collectFunctions gc=:{gc_gcf=GCF _ {gcf_body=GCB_None}} icl_module ca
		= (gc, ca)
	collectFunctions gc=:{gc_gcf=GCFC _ _} icl_module ca
		= (gc, ca)

instance collectFunctions FunDef where
	collectFunctions fun_def=:{fun_body = ParsedBody bodies} icl_module ca
		# (bodies, ca) = collectFunctions bodies icl_module ca
		= ({fun_def & fun_body = ParsedBody bodies}, ca)

instance collectFunctions ParsedBody where
	collectFunctions pb=:{pb_rhs} icl_module ca
		# (pb_rhs, ca) = collectFunctions pb_rhs icl_module ca
		= ({ pb & pb_rhs = pb_rhs }, ca)

NoCollectedLocalDefs :== CollectedLocalDefs { loc_functions = { ir_from = 0, ir_to = 0 }, loc_nodes = [], loc_in_icl_module=True }

transformLambda :: Ident [ParsedExpr] Rhs Position -> FunDef
transformLambda lam_ident args rhs pos
	# lam_body = [{pb_args = args, pb_rhs = rhs, pb_position = pos }]
	= MakeNewImpOrDefFunction lam_ident (length args) lam_body (FK_Function cNameLocationDependent) NoPrio No pos

makeConsExpressionForGenerator :: GeneratorKind ParsedExpr ParsedExpr -> ParsedExpr
makeConsExpressionForGenerator gen_kind a1 a2
	#! cons_id = predefined_idents.[case gen_kind of IsListGenerator -> PD_ConsSymbol ; _ -> PD_OverloadedConsSymbol]
	= PE_List [PE_Ident cons_id, a1, a2]

makeNilExpression :: Int -> ParsedExpr
makeNilExpression predef_nil_index
	#! nil_id = predefined_idents.[predef_nil_index]
	= PE_Ident nil_id

makeConsExpression :: Int ParsedExpr ParsedExpr -> ParsedExpr
makeConsExpression predef_cons_index a1 a2
	#! cons_id = predefined_idents.[predef_cons_index]
	= PE_List [PE_Ident cons_id, a1, a2]

//get_predef_id :: Int -> Ident
get_predef_id predef_index :== predefined_idents.[predef_index]

// +++ change to accessor functions
:: TransformedGenerator =
	{	tg_expr :: ([ParsedDefinition],[ParsedExpr])
	,	tg_lhs_arg :: [ParsedExpr]
	,	tg_case_end_expr :: ParsedExpr
	,	tg_case_end_pattern :: ParsedExpr
	,	tg_element :: ParsedExpr
	,	tg_element_is_uselect :: !Bool
	,	tg_pattern :: ParsedExpr
	,	tg_rhs_continuation :: [ParsedExpr]
	,	tg_case1 :: Ident
	,	tg_case2 :: Ident
	}

:: IndexGenerator :== Optional (ParsedExpr,[([ParsedDefinition],ParsedExpr,ParsedExpr)])

is_zero_expression (PE_Basic (BVI "0")) = True
is_zero_expression (PE_Basic (BVInt 0)) = True
is_zero_expression _ = False

is_overloaded_list_generator IsOverloadedListGenerator = True
is_overloaded_list_generator _ = False

transformGenerator :: Generator String IndexGenerator *CollectAdmin -> (!TransformedGenerator,!IndexGenerator,!Int,!*CollectAdmin)
transformGenerator {gen_kind=IsArrayGenerator, gen_expr, gen_pattern, gen_position} qual_filename index_generator ca
	# (array, ca) = prefixAndPositionToIdentExp "g_a" gen_position ca
	  (n, ca) = prefixAndPositionToIdentExp "g_s" gen_position ca
	  (n2, ca) = prefixAndPositionToIdentExp "g_s2" gen_position ca
	  (a2, ca) = prefixAndPositionToIdentExp "g_a2" gen_position ca
	  (gen_var_case1, ca) = prefixAndPositionToIdent "g_c1" gen_position ca
	  (gen_var_case2, ca) = prefixAndPositionToIdent "g_c2" gen_position ca
	  less_or_equal = get_predef_id PD_LessOrEqualFun
	  sub = get_predef_id PD_SubFun
	  usize = get_predef_id PD_UnqArraySizeFun
	  pattern = PE_Tuple [gen_pattern, array]
	= case index_generator of
		No
			# (i, ca) = prefixAndPositionToIdentExp "g_i" gen_position ca
			  inc = get_predef_id PD_IncFun
			# dec_n = PE_List [n,PE_Ident sub,PE_Basic (BVInt 1)]
			# transformed_generator
			  	=	{	tg_expr =	([PD_NodeDef (LinePos qual_filename gen_position.lc_line) (PE_Tuple [n,a2]) (exprToRhs (PE_List [PE_Ident usize, gen_expr]))],
				  					[PE_Basic (BVInt 0),dec_n,a2])
			  		,	tg_lhs_arg = [i, n, array]
			  		,	tg_case_end_expr = PE_List [i,PE_Ident less_or_equal, n]
			  		,	tg_case_end_pattern = PE_Basic (BVB True)
					,	tg_element = PE_Selection (ParsedUniqueSelector True) array [PS_Array i]
					,	tg_element_is_uselect=True
					,	tg_pattern = pattern
					,	tg_rhs_continuation = [PE_List [PE_Ident inc, i], n, array]
					,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
					}
			-> (transformed_generator,Yes (i,[([],dec_n,n2)]),2,ca)
		Yes (i,[])
			# inc = get_predef_id PD_IncFun
			# dec_n = PE_List [n,PE_Ident sub,PE_Basic (BVInt 1)]
			# transformed_generator
			  	=	{	tg_expr =	([PD_NodeDef (LinePos qual_filename gen_position.lc_line) (PE_Tuple [n,a2]) (exprToRhs (PE_List [PE_Ident usize, gen_expr]))],
				  					[dec_n,a2])
			  		,	tg_lhs_arg = [n,array]
			  		,	tg_case_end_expr = PE_List [i,PE_Ident less_or_equal, n]
			  		,	tg_case_end_pattern = PE_Basic (BVB True)
					,	tg_element = PE_Selection (ParsedUniqueSelector True) array [PS_Array i]
					,	tg_element_is_uselect=True
					,	tg_pattern = pattern
					,	tg_rhs_continuation = [n,array]
					,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
					}
			-> (transformed_generator,Yes (i,[([],dec_n,n2)]),1,ca)
		Yes (i,size_expressions)
			# transformed_generator
			  	=	{	tg_expr = ([],[a2])
					,	tg_lhs_arg = [array]
			  		,	tg_case_end_expr = PE_Empty
			  		,	tg_case_end_pattern = PE_Empty
					,	tg_element = PE_Selection (ParsedUniqueSelector True) array [PS_Array i]
					,	tg_element_is_uselect=True
					,	tg_pattern = pattern
					,	tg_rhs_continuation = [array]
					,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
					}
			# size_expression
				=([PD_NodeDef (LinePos qual_filename gen_position.lc_line) (PE_Tuple [n,a2]) (exprToRhs (PE_List [PE_Ident usize, gen_expr]))],
				  					(PE_List [n,PE_Ident sub,PE_Basic (BVInt 1)]),n2)
			-> (transformed_generator,Yes (i,[size_expression:size_expressions]),0,ca)
transformGenerator {gen_kind, gen_expr=PE_Sequ (SQ_FromTo pd_from_to_index from_exp to_exp), gen_pattern, gen_position} qual_filename index_generator ca
	| is_overloaded_list_generator gen_kind || pd_from_to_index==PD_FromTo
		# (n, ca) = prefixAndPositionToIdentExp "g_s" gen_position ca
		  (gen_var_case1, ca) = prefixAndPositionToIdent "g_c1" gen_position ca
	 	  (gen_var_case2, ca) = prefixAndPositionToIdent "g_c2" gen_position ca
		| is_zero_expression from_exp
			= case index_generator of
				No
					# (i, ca) = prefixAndPositionToIdentExp "g_i" gen_position ca
					# inc = get_predef_id PD_IncFun
					  less_or_equal = get_predef_id PD_LessOrEqualFun
					# transformed_generator
					  	=	{	tg_expr = ([],[from_exp,to_exp])
					  		,	tg_lhs_arg = [i,n]
					  		,	tg_case_end_expr = PE_List [i,PE_Ident less_or_equal,n]
					  		,	tg_case_end_pattern = PE_Basic (BVB True)
							,	tg_element = i
							,	tg_element_is_uselect=False
							,	tg_pattern = gen_pattern
							,	tg_rhs_continuation = [PE_List [PE_Ident inc, i], n]
							,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
							}
					-> (transformed_generator,Yes (i,[([],to_exp,n)]),2,ca)
				Yes (i,[])
					# inc = get_predef_id PD_IncFun
					  less_or_equal = get_predef_id PD_LessOrEqualFun
					# transformed_generator
					  	=	{	tg_expr = ([],[to_exp])
					  		,	tg_lhs_arg = [n]
					  		,	tg_case_end_expr = PE_List [i,PE_Ident less_or_equal,n]
					  		,	tg_case_end_pattern = PE_Basic (BVB True)
							,	tg_element = i
							,	tg_element_is_uselect=False
							,	tg_pattern = gen_pattern
							,	tg_rhs_continuation = [n]
							,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
							}
					-> (transformed_generator,Yes (i,[([],to_exp,n)]),1,ca)
				Yes (i,size_expressions)
					# transformed_generator
					  	=	{	tg_expr = ([],[])
					  		,	tg_lhs_arg = []
					  		,	tg_case_end_expr = PE_Empty
					  		,	tg_case_end_pattern = PE_Empty
							,	tg_element = i
							,	tg_element_is_uselect=False
							,	tg_pattern = gen_pattern
							,	tg_rhs_continuation = []
							,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
							}
					-> (transformed_generator,Yes (i,[([],to_exp,n):size_expressions]),0,ca)
			# (i, ca) = prefixAndPositionToIdentExp "g_i" gen_position ca
			# inc = get_predef_id PD_IncFun
			  less_or_equal = get_predef_id PD_LessOrEqualFun
			# transformed_generator
			  	=	{	tg_expr = ([],[from_exp,to_exp])
			  		,	tg_lhs_arg = [i,n]
			  		,	tg_case_end_expr = PE_List [i,PE_Ident less_or_equal,n]
			  		,	tg_case_end_pattern = PE_Basic (BVB True)
					,	tg_element = i
					,	tg_element_is_uselect=False
					,	tg_pattern = gen_pattern
					,	tg_rhs_continuation = [PE_List [PE_Ident inc, i], n]
					,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
					}
			= (transformed_generator,index_generator,0,ca)
transformGenerator {gen_kind, gen_expr=PE_Sequ (SQ_From pd_from_index from_exp), gen_pattern, gen_position} qual_filename index_generator ca
	| is_overloaded_list_generator gen_kind || pd_from_index==PD_From
		# (gen_var_case1, ca) = prefixAndPositionToIdent "g_c1" gen_position ca
		  (gen_var_case2, ca) = prefixAndPositionToIdent "g_c2" gen_position ca
		| is_zero_expression from_exp
			= case index_generator of
				No
					# (i, ca) = prefixAndPositionToIdentExp "g_i" gen_position ca
					# inc = get_predef_id PD_IncFun
					# transformed_generator
					  	=	{	tg_expr = ([],[from_exp])
					  		,	tg_lhs_arg = [i]
					  		,	tg_case_end_expr = PE_Empty
					  		,	tg_case_end_pattern = PE_Empty
							,	tg_element = i
							,	tg_element_is_uselect=False
							,	tg_pattern = gen_pattern
							,	tg_rhs_continuation = [PE_List [PE_Ident inc, i]]
							,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
							}
					-> (transformed_generator,Yes (i,[]),0,ca)
				Yes (i,size_expressions)
					# transformed_generator
					  	=	{	tg_expr = ([],[])
					  		,	tg_lhs_arg = []
					  		,	tg_case_end_expr = PE_Empty
					  		,	tg_case_end_pattern = PE_Empty
							,	tg_element = i
							,	tg_element_is_uselect=False
							,	tg_pattern = gen_pattern
							,	tg_rhs_continuation = []
							,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
							}
					-> (transformed_generator,index_generator,0,ca)
			# (i, ca) = prefixAndPositionToIdentExp "g_i" gen_position ca
			# inc = get_predef_id PD_IncFun
			# transformed_generator
			  	=	{	tg_expr = ([],[from_exp])
			  		,	tg_lhs_arg = [i]
			  		,	tg_case_end_expr = PE_Empty
			  		,	tg_case_end_pattern = PE_Empty
					,	tg_element = i
					,	tg_element_is_uselect=False
					,	tg_pattern = gen_pattern
					,	tg_rhs_continuation = [PE_List [PE_Ident inc, i]]
					,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
					}
			= (transformed_generator,index_generator,0,ca)
transformGenerator {gen_kind, gen_expr, gen_pattern, gen_position} qual_filename index_generator ca
	# (list, ca) = prefixAndPositionToIdentExp "g_l" gen_position ca
	  (hd, ca) = prefixAndPositionToIdentExp "g_h" gen_position ca
	  (tl, ca) = prefixAndPositionToIdentExp "g_t" gen_position ca
	  (gen_var_case1, ca) = prefixAndPositionToIdent "g_c1" gen_position ca
	  (gen_var_case2, ca) = prefixAndPositionToIdent "g_c2" gen_position ca
	  cons = makeConsExpressionForGenerator gen_kind hd tl
	# transformed_generator
	  	=	{	tg_expr = ([],[gen_expr])
	  		,	tg_lhs_arg = [list]
	  		,	tg_case_end_expr = list
	  		,	tg_case_end_pattern = cons
			,	tg_element = hd
			,	tg_element_is_uselect=False
			,	tg_pattern = gen_pattern
			,	tg_rhs_continuation = [tl]
			,	tg_case1 = gen_var_case1, tg_case2 = gen_var_case2
			}
	= (transformed_generator,index_generator,0,ca)

transformGenerators :: [Generator] String IndexGenerator *CollectAdmin -> (![TransformedGenerator],!IndexGenerator,!*CollectAdmin)
transformGenerators [generator:generators] qual_filename index_generator ca
	# (transformed_generator,index_generator,index_argument_n,ca) = transformGenerator generator qual_filename index_generator ca
	| index_argument_n>0
		# (transformed_generators,index_generator,ca) = transformGenerators generators qual_filename index_generator ca		
		# (node_defs,size_exp,_,ca) = compute_minimum_of_sizes index_generator generator.gen_position qual_filename ca
		# transformed_generator = store_minimum_of_sizes_in_generator node_defs size_exp index_argument_n transformed_generator
		= ([transformed_generator:transformed_generators],index_generator,ca)
		# (transformed_generators,index_generator,ca) = transformGenerators generators qual_filename index_generator ca
		= ([transformed_generator:transformed_generators],index_generator,ca)
transformGenerators [] qual_filename index_generator ca
	= ([],index_generator,ca)

transformGeneratorsAndReturnSize :: [Generator] String IndexGenerator ParsedExpr *CollectAdmin -> (![TransformedGenerator],!IndexGenerator,!ParsedExpr,!*CollectAdmin)
transformGeneratorsAndReturnSize [generator:generators] qual_filename index_generator size_exp ca
	# (transformed_generator,index_generator,index_argument_n,ca) = transformGenerator generator qual_filename index_generator ca
	| index_argument_n>0
		# (transformed_generators,index_generator,_,ca) = transformGeneratorsAndReturnSize generators qual_filename index_generator size_exp ca		

		# (node_defs,size_exp,ident,ca) = compute_minimum_of_sizes index_generator generator.gen_position qual_filename ca
		# (node_defs,size_exp) = case size_exp of
									PE_Ident _
										-> (node_defs,size_exp)
									_
										-> (node_defs++[PD_NodeDef (LinePos qual_filename generator.gen_position.lc_line) ident (exprToRhs size_exp)],ident)
		# transformed_generator = store_minimum_of_sizes_in_generator node_defs size_exp index_argument_n transformed_generator

		# inc = get_predef_id PD_IncFun
		# size_exp = PE_List [PE_Ident inc,size_exp]
		
		= ([transformed_generator:transformed_generators],index_generator,size_exp,ca)

		# (transformed_generators,index_generator,size_exp,ca) = transformGeneratorsAndReturnSize generators qual_filename index_generator size_exp ca
		= ([transformed_generator:transformed_generators],index_generator,size_exp,ca)
transformGeneratorsAndReturnSize [] qual_filename index_generator size_exp ca
	= ([],index_generator,size_exp,ca)

compute_minimum_of_sizes :: IndexGenerator LineAndColumn String *CollectAdmin -> *(![ParsedDefinition],!ParsedExpr,!ParsedExpr,!*CollectAdmin);
compute_minimum_of_sizes (Yes (i,sizes)) gen_position qual_filename ca
	= compute_minimum (reverse sizes) 1 ca
where
	compute_minimum [(node_defs,exp,ident)] n ca
		= (node_defs,exp,ident,ca)
	compute_minimum [(node_defs1,exp1,ident1):sizes] n ca
		# (node_defs2,exp2,ident2,ca) = compute_minimum sizes (n+1) ca
		# node_defs=node_defs1++node_defs2
		# (to_exp,ca)=minimum exp1 exp2 ca
			with
				minimum ident1=:(PE_Ident _) ident2=:(PE_Ident _) ca
					= minimum_of_idents ident1 ident2 ca
				minimum exp1 ident2=:(PE_Ident _) ca
					# node_def1 = PD_NodeDef (LinePos qual_filename gen_position.lc_line) ident1 (exprToRhs exp1)
					# (min_exp,ca) = minimum_of_idents ident1 ident2 ca
					= (PE_Let (LocalParsedDefs [node_def1]) min_exp,ca)
				minimum ident1=:(PE_Ident _) exp2 ca
					# node_def2 = PD_NodeDef (LinePos qual_filename gen_position.lc_line) ident2 (exprToRhs exp2)
					# (min_exp,ca) = minimum_of_idents ident1 ident2 ca
					= (PE_Let (LocalParsedDefs [node_def2]) min_exp,ca)
				minimum exp1 exp2 ca
					# pos = LinePos qual_filename gen_position.lc_line
					# node_def1 = PD_NodeDef pos ident1 (exprToRhs exp1)
					# node_def2 = PD_NodeDef pos ident2 (exprToRhs exp2)
					# (min_exp,ca) = minimum_of_idents ident1 ident2 ca
					= (PE_Let (LocalParsedDefs [node_def1,node_def2]) min_exp,ca)
				
				minimum_of_idents ident1 ident2 ca
					# smaller_fun = get_predef_id PD_SmallerFun
					# (case_ident,ca) = prefixAndPositionToIdent ("g_s"+++toString n) gen_position ca
					# pos = LinePos qual_filename gen_position.lc_line
					= (PE_Case case_ident (PE_List [ident1,PE_Ident smaller_fun,ident2])
						[{calt_pattern = PE_Basic (BVB True), calt_rhs = exprToRhs ident1, calt_position=pos},
						 {calt_pattern = PE_WildCard, calt_rhs = exprToRhs ident2, calt_position=pos}],ca)
		= (node_defs,to_exp,ident1,ca)

store_minimum_of_sizes_in_generator :: [ParsedDefinition] ParsedExpr Int TransformedGenerator -> TransformedGenerator;
store_minimum_of_sizes_in_generator node_defs size_exp index_argument_n generator=:{tg_expr=(exp_node_defs,exps)}
	# exps=replace_exp_n index_argument_n exps
		with
			replace_exp_n 1 [e:l] = [size_exp:l]
			replace_exp_n n [e:l] = [e: replace_exp_n (n-1) l]
	= {generator & tg_expr=(node_defs++exp_node_defs,exps)}

::	TransformedQualifier =
	{	tq_generators :: [TransformedGenerator]
	,	tq_call :: ParsedExpr
	,	tq_lhs_args :: [ParsedExpr]
	,	tq_let_defs :: LocalDefs
	,	tq_filter :: Optional ParsedExpr
	,	tq_continue :: ParsedExpr
	,	tq_success :: ParsedExpr
	,	tq_end :: ParsedExpr
	,	tq_fun_id :: Ident
	,	tq_fun_pos :: !Position
	}

rhs_continuation_args_from_generators generators
	= [arg \\ generator<-generators, arg<-generator.tg_rhs_continuation]

expr_args_from_generators generators
	= [arg \\ {tg_expr=(_,args)}<-generators, arg<-args]

lhs_args_from_generators generators
	= [arg \\ generator<-generators, arg<-generator.tg_lhs_arg]

add_node_defs_to_exp [] exp
	= exp
add_node_defs_to_exp [{tg_expr=([],_)}:generators] exp
	= add_node_defs_to_exp generators exp
add_node_defs_to_exp [{tg_expr=(node_defs,_)}:generators] exp
	= PE_Let (LocalParsedDefs node_defs) (add_node_defs_to_exp generators exp)

transformQualifier :: Qualifier *CollectAdmin -> (TransformedQualifier, *CollectAdmin) 
transformQualifier {qual_generators,qual_let_defs,qual_filter, qual_position, qual_filename} ca
	# (transformedGenerators,index_generator,ca) = transformGenerators qual_generators qual_filename No ca
	# (qual_fun_id, ca) = prefixAndPositionToIdent "c" qual_position ca
	=	({	tq_generators = transformedGenerators
		,	tq_call = add_node_defs_to_exp transformedGenerators (PE_List [PE_Ident qual_fun_id : expr_args_from_generators transformedGenerators])
		,	tq_lhs_args = lhs_args_from_generators transformedGenerators
		,	tq_let_defs = qual_let_defs
		,	tq_filter = qual_filter
		,	tq_continue = PE_List [PE_Ident qual_fun_id : rhs_continuation_args_from_generators transformedGenerators]
		,	tq_success = PE_Empty
		,	tq_end = PE_Empty
		,	tq_fun_id = qual_fun_id
		,	tq_fun_pos = LinePos qual_filename qual_position.lc_line
		}, ca)

// =array&callArray are misnomers (can also be records)
transformUpdateQualifier :: [ParsedExpr] [ParsedExpr] Qualifier *CollectAdmin -> (TransformedQualifier, *CollectAdmin) 
transformUpdateQualifier array callArray {qual_generators,qual_let_defs,qual_filter, qual_position, qual_filename} ca
	# (transformedGenerators,index_generator,ca) = transformGenerators qual_generators qual_filename No ca
	= CreateTransformedQualifierFromTransformedGenerators transformedGenerators array callArray qual_let_defs qual_filter qual_position qual_filename ca

CreateTransformedQualifierFromTransformedGenerators transformedGenerators array callArray qual_let_defs qual_filter qual_position qual_filename ca
	# (qual_fun_id, ca) = prefixAndPositionToIdent "cu" qual_position ca
	=	({	tq_generators = transformedGenerators
		,	tq_call = add_node_defs_to_exp transformedGenerators (PE_List [PE_Ident qual_fun_id : callArray ++ expr_args_from_generators transformedGenerators])
		,	tq_lhs_args = array ++ lhs_args_from_generators transformedGenerators
		,	tq_let_defs=qual_let_defs
		,	tq_filter = qual_filter
		,	tq_continue = PE_List [PE_Ident qual_fun_id : array ++ rhs_continuation_args_from_generators transformedGenerators]
		,	tq_success = PE_Empty
		,	tq_end = PE_Empty
		,	tq_fun_id = qual_fun_id
		,	tq_fun_pos = LinePos qual_filename qual_position.lc_line
		}, ca)

transformListComprehension :: Int Int ParsedExpr [Qualifier] *CollectAdmin -> (ParsedExpr, *CollectAdmin)
transformListComprehension predef_cons_index predef_nil_index expr qualifiers ca
	# (transformed_qualifiers, ca) = mapSt transformQualifier qualifiers ca
	  success = makeConsExpression predef_cons_index expr (last transformed_qualifiers).tq_continue
	  nil = makeNilExpression predef_nil_index
	  transformed_qualifiers
	  	=	[	{qual & tq_success = success, tq_end = end}
	  		\\	qual <- transformed_qualifiers
	  		&	success <- [qual.tq_call \\ qual <- tl transformed_qualifiers] ++ [success]
	  		&	end <- [nil : [qual.tq_continue \\ qual <- transformed_qualifiers]]
	  		]
	= (makeComprehensions transformed_qualifiers success [],ca)

transformArrayComprehension :: ArrayKind ParsedExpr [Qualifier] *CollectAdmin -> (ParsedExpr, *CollectAdmin)
transformArrayComprehension array_kind expr qualifiers ca
	# [hd_qualifier:_] = qualifiers
	  qual_position = hd_qualifier.qual_position
	  (c_i_ident_exp, ca) = prefixAndPositionToIdentExp "c_i" qual_position ca
	  (c_a_ident_exp, ca) = prefixAndPositionToIdentExp "c_a" qual_position ca
	  create_array_expr = predef_ident_expr PD__CreateArrayFun
	| same_index_for_update_and_array_generators qualifiers
		# index_generator = {gen_kind=IsListGenerator, gen_pattern=c_i_ident_exp, gen_expr=PE_Sequ (SQ_From PD_From (PE_Basic (BVInt 0))), gen_position=qual_position}
		# update = PE_Update c_a_ident_exp [PS_Array  c_i_ident_exp] expr
		| size_of_generators_can_be_computed_quickly qualifiers
			# {qual_generators,qual_let_defs,qual_filter,qual_position,qual_filename} = hd_qualifier
			# qual_generators = [index_generator : qual_generators]
			# (transformedGenerators,index_generator,size_exp,ca) = transformGeneratorsAndReturnSize qual_generators qual_filename No PE_Empty ca
			# new_array = PE_List [create_array_expr,size_exp]
			  new_array = cast_array_kind array_kind new_array
			# (transformed_qualifier,ca) = CreateTransformedQualifierFromTransformedGenerators transformedGenerators [c_a_ident_exp] [new_array] qual_let_defs qual_filter qual_position qual_filename ca
			= (makeUpdateComprehensionFromTransFormedQualifiers [update] [c_a_ident_exp] c_a_ident_exp [transformed_qualifier],ca)

			# (length, ca) = computeSize qualifiers qual_position hd_qualifier.qual_filename ca
			# new_array = PE_List [create_array_expr,length]
			  new_array = cast_array_kind array_kind new_array
			  qualifiers = [{hd_qualifier & qual_generators = [index_generator : hd_qualifier.qual_generators] }]
			= transformUpdateComprehension [new_array] [update] [c_a_ident_exp] c_a_ident_exp qualifiers ca

		# (length, ca) = computeSize qualifiers qual_position hd_qualifier.qual_filename ca
		# new_array = PE_List [create_array_expr,length]
		  new_array = cast_array_kind array_kind new_array
		# inc = get_predef_id PD_IncFun
		  new_array_and_index =	[new_array,PE_Basic (BVInt 0)]
		  update = [PE_Update c_a_ident_exp [PS_Array  c_i_ident_exp] expr,PE_List [PE_Ident inc,c_i_ident_exp]]
		= transformUpdateComprehension new_array_and_index update [c_a_ident_exp,c_i_ident_exp] c_a_ident_exp qualifiers ca

All p l :== all l
where
	all [] = True
	all [b : tl] = p b && all tl

pattern_will_always_match (PE_Ident _)
	= True;
pattern_will_always_match (PE_Tuple tuple_args)
	= All pattern_will_always_match tuple_args
pattern_will_always_match _
	= False

patterns_in_generator_will_always_match {gen_pattern}
	= pattern_will_always_match gen_pattern

same_index_for_update_and_array_generators [{qual_generators,qual_filter=No}]
	= All patterns_in_generator_will_always_match qual_generators
same_index_for_update_and_array_generators _
	= False

transformUpdateQualifiers :: [ParsedExpr] [ParsedExpr] [Qualifier] *CollectAdmin -> *(![TransformedQualifier],!*CollectAdmin);
transformUpdateQualifiers identExprs exprs [first_qualifier:rest_qualifiers] ca
	# (transformed_first_qualifier,ca) = transformUpdateQualifier identExprs exprs first_qualifier ca
	  (transformed_rest_qualifiers,ca) = mapSt (transformUpdateQualifier identExprs identExprs) rest_qualifiers ca
	= ([transformed_first_qualifier : transformed_rest_qualifiers],ca)

makeUpdateOrSizeComprehension transformed_qualifiers success identExprs result_expr
	# transformed_qualifiers
	  	=	[	{qual & tq_success = success, tq_end = end}
	  		\\	qual <- transformed_qualifiers
	  		&	success <- [qual.tq_call \\ qual <- tl transformed_qualifiers] ++ [success]
	  		&	end <- [result_expr : [qual.tq_continue \\ qual <- transformed_qualifiers]]
	  		]
 	= makeComprehensions transformed_qualifiers success identExprs

size_of_generator_can_be_computed_quickly {gen_pattern,gen_kind=IsArrayGenerator}
	= pattern_will_always_match gen_pattern
size_of_generator_can_be_computed_quickly {gen_pattern,gen_kind=IsListGenerator,gen_expr=PE_Sequ (SQ_FromTo PD_FromTo (PE_Basic (BVInt 0)) to_exp)}
	= pattern_will_always_match gen_pattern	
size_of_generator_can_be_computed_quickly {gen_pattern,gen_kind=IsListGenerator,gen_expr=PE_Sequ (SQ_From PD_From from_exp)}
	= pattern_will_always_match gen_pattern	
size_of_generator_can_be_computed_quickly {gen_pattern,gen_kind=IsOverloadedListGenerator,gen_expr=PE_Sequ (SQ_FromTo _ (PE_Basic (BVInt 0)) to_exp)}
	= pattern_will_always_match gen_pattern	
size_of_generator_can_be_computed_quickly {gen_pattern,gen_kind=IsOverloadedListGenerator,gen_expr=PE_Sequ (SQ_From _ from_exp)}
	= pattern_will_always_match gen_pattern	
size_of_generator_can_be_computed_quickly _
	= False

size_of_generators_can_be_computed_quickly qualifiers=:[qualifier=:{qual_generators,qual_filter=No}]
	= All size_of_generator_can_be_computed_quickly qual_generators && not (All is_from_generator qual_generators)
	where
		is_from_generator {gen_pattern,gen_kind=IsListGenerator,gen_expr=PE_Sequ (SQ_From _ from_exp)}
			= True
		is_from_generator {gen_pattern,gen_kind=IsOverloadedListGenerator,gen_expr=PE_Sequ (SQ_From _ from_exp)}
			= True
		is_from_generator _
			= False
size_of_generators_can_be_computed_quickly _
	= False

computeSize :: [Qualifier] LineAndColumn FileName *CollectAdmin -> (!ParsedExpr,!*CollectAdmin)
computeSize qualifiers qual_position qual_filename ca
	# (counter_ident_exp, ca) = prefixAndPositionToIdentExp "c_l_i" qual_position ca
	  (transformed_qualifiers,ca) = transformUpdateQualifiers [counter_ident_exp] [PE_Basic (BVInt 0)] qualifiers ca
	  inc = get_predef_id PD_IncFun
	  success = insert_inc_in_inner_loop (last transformed_qualifiers).tq_continue
				with
					insert_inc_in_inner_loop (PE_List [f, a : args])
						= PE_List [f, PE_List  [PE_Ident inc,a] : args]
	= (makeUpdateOrSizeComprehension transformed_qualifiers success [counter_ident_exp] counter_ident_exp,ca)

transformUpdateComprehension :: [ParsedExpr] [ParsedExpr] [ParsedExpr] ParsedExpr [Qualifier] *CollectAdmin -> (!ParsedExpr,!*CollectAdmin)
transformUpdateComprehension exprs updateExprs identExprs result_expr qualifiers ca
	# (transformed_qualifiers,ca) = transformUpdateQualifiers identExprs exprs qualifiers ca
	= (makeUpdateComprehensionFromTransFormedQualifiers updateExprs identExprs result_expr transformed_qualifiers,ca)

makeUpdateComprehensionFromTransFormedQualifiers :: [ParsedExpr] [ParsedExpr] ParsedExpr [TransformedQualifier] -> ParsedExpr
makeUpdateComprehensionFromTransFormedQualifiers updateExprs identExprs result_expr transformed_qualifiers
	# success
	  	// +++ remove hack
	  	=	this_is_definitely_a_hack (last transformed_qualifiers).tq_continue updateExprs
			with
				this_is_definitely_a_hack (PE_List [f : args]) updateExprs
					=	PE_List [f : replace_args updateExprs args]
					with
						replace_args [] args = args
						replace_args [e:l] [a:args] = [e:replace_args l args]
	= makeUpdateOrSizeComprehension transformed_qualifiers success identExprs result_expr

makeComprehensions :: [TransformedQualifier] ParsedExpr [ParsedExpr] -> ParsedExpr
makeComprehensions [] success _
	= success
makeComprehensions [{tq_generators,tq_let_defs,tq_filter, tq_end, tq_call, tq_lhs_args, tq_fun_id, tq_fun_pos} : qualifiers] success threading
	# success		= makeComprehensions qualifiers success threading
	  failure		= PE_List [PE_Ident tq_fun_id : threading ++ rhs_continuation_args_from_generators tq_generators]
	  rhs	 		= build_rhs tq_generators success tq_let_defs tq_filter failure tq_end tq_fun_pos
	  parsed_def 	= MakeNewParsedDef tq_fun_id tq_lhs_args rhs tq_fun_pos
	= PE_Let (LocalParsedDefs [parsed_def]) tq_call
	where
		build_rhs :: [TransformedGenerator] ParsedExpr LocalDefs (Optional ParsedExpr) ParsedExpr ParsedExpr Position -> Rhs
		build_rhs [generator : generators] success let_defs optional_filter failure end fun_pos
			#	rhs2 = foldr (case_end end)
						(case_with_default generator.tg_case2 generator.tg_element generator.tg_element_is_uselect generator.tg_pattern
							(foldr (case_pattern failure) rhs generators)
							failure)
						generators
			=	case_with_default generator.tg_case1 generator.tg_case_end_expr False generator.tg_case_end_pattern rhs2 end
			where
				rhs
					=	case optional_filter of
							Yes filter
								-> {rhs_alts = GuardedAlts [
										{alt_nodes = [], alt_guard = filter, alt_expr = UnGuardedExpr
												{ewl_nodes = [], ewl_expr = success, ewl_locals	= LocalParsedDefs [], ewl_position = NoPos },
											alt_ident = { id_name ="_f;" +++ toString line_nr +++ ";", id_info = nilPtr },
										 alt_position = NoPos}] No
									,	rhs_locals	= let_defs}
							No
								-> {rhs_alts=UnGuardedExpr {ewl_nodes=[],ewl_expr=success,ewl_locals=LocalParsedDefs [],ewl_position=NoPos},rhs_locals=let_defs}
				(LinePos _ line_nr) = fun_pos

		case_end :: ParsedExpr TransformedGenerator Rhs -> Rhs
		case_end end {tg_case1, tg_case_end_expr, tg_case_end_pattern} rhs
			=	case_with_default tg_case1 tg_case_end_expr False tg_case_end_pattern rhs end
	
		case_pattern :: ParsedExpr TransformedGenerator Rhs -> Rhs
		case_pattern failure {tg_case2, tg_element,tg_element_is_uselect, tg_pattern} rhs
			=	case_with_default tg_case2 tg_element tg_element_is_uselect tg_pattern rhs failure

		case_with_default :: Ident ParsedExpr Bool ParsedExpr Rhs ParsedExpr -> Rhs
		case_with_default case_ident expr expr_is_uselect pattern=:(PE_Ident ident) rhs=:{rhs_alts=UnGuardedExpr ung_exp=:{ewl_nodes,ewl_expr,ewl_locals=LocalParsedDefs [],ewl_position},rhs_locals=LocalParsedDefs []} default_rhs
			| isLowerCaseName ident.id_name
				# new_node={ndwl_strict=False,ndwl_def={bind_src=expr,bind_dst=pattern},ndwl_locals=LocalParsedDefs [],ndwl_position=ewl_position}
				= {rhs & rhs_alts=UnGuardedExpr {ung_exp & ewl_nodes=[new_node:ewl_nodes]}}
		case_with_default case_ident expr True pattern=:(PE_Tuple [PE_Ident ident1,ident2_exp=:PE_Ident ident2]) rhs=:{rhs_alts=UnGuardedExpr ung_exp=:{ewl_nodes,ewl_expr,ewl_locals=LocalParsedDefs [],ewl_position},rhs_locals=LocalParsedDefs []} default_rhs
			# new_node1={ndwl_strict=False,ndwl_def={bind_src=expr,bind_dst=pattern},ndwl_locals=LocalParsedDefs [],ndwl_position=ewl_position}
			# new_node2={ndwl_strict=True,ndwl_def={bind_src=ident2_exp,bind_dst=ident2_exp},ndwl_locals=LocalParsedDefs [],ndwl_position=ewl_position}
			= {rhs & rhs_alts=UnGuardedExpr {ung_exp & ewl_nodes=[new_node1,new_node2:ewl_nodes]}}
		case_with_default case_ident expr expr_is_uselect PE_Empty rhs default_rhs
			= rhs
		case_with_default case_ident expr expr_is_uselect pattern rhs default_rhs
			=	exprToRhs (PE_Case case_ident expr
					[	{calt_pattern = pattern, calt_rhs = rhs, calt_position=NoPos}
					,	{calt_pattern = PE_WildCard, calt_rhs = exprToRhs default_rhs, calt_position=NoPos}
					])

transformSequence :: Sequence -> ParsedExpr
transformSequence (SQ_FromThen pd_from_then frm then)
	=	predef_ident_expr pd_from_then ` frm ` then
transformSequence (SQ_FromThenTo pd_from_then_to frm then to)
	=	predef_ident_expr pd_from_then_to ` frm ` then ` to
transformSequence (SQ_From pd_from frm)
	=	predef_ident_expr pd_from ` frm
transformSequence (SQ_FromTo pd_from_to frm to)
	=	predef_ident_expr pd_from_to ` frm ` to

transformArrayUpdate :: ParsedExpr [Bind ParsedExpr ParsedExpr] -> ParsedExpr
transformArrayUpdate expr updates
	=	foldr (update (predef_ident_expr PD_ArrayUpdateFun)) expr updates
	where
		update :: ParsedExpr (Bind ParsedExpr ParsedExpr) ParsedExpr -> ParsedExpr
		update updateIdent {bind_src=value, bind_dst=index} expr
			=	updateIdent ` expr ` index ` value

transformArrayDenot :: ArrayKind [ParsedExpr] -> ParsedExpr
transformArrayDenot array_kind exprs
	# create_array_call=cast_array_kind array_kind (predef_ident_expr PD__CreateArrayFun ` length exprs)
	=	transformArrayUpdate
			create_array_call
			[{bind_dst=toParsedExpr i, bind_src=expr} \\ expr <- exprs & i <- [0..]]

cast_array_kind OverloadedArray array_expr = array_expr
cast_array_kind array_kind array_expr = PE_TypeSignature array_kind array_expr

scanModules :: [ParsedImport] [ScannedModule] [Ident] SearchPaths Bool Bool (ModTimeFunction *Files) *Files *CollectAdmin -> (Bool, [ScannedModule],*Files, *CollectAdmin)
scanModules [] parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
	= (True, parsed_modules,files, ca)
scanModules [{import_module,import_file_position} : mods] parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
	| in_cache import_module cached_modules
		= scanModules mods parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
	# (found_module,mod_type) = try_to_find import_module parsed_modules
	| found_module
		= case mod_type of
			MK_NoMainDcl
				# ca = postParseError import_file_position ("main module \'"+++import_module.id_name+++"\' does not have a definition module") ca
				# (_,parsed_modules,files,ca) = scanModules mods parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
				-> (False,parsed_modules,files,ca)
			_
				-> scanModules mods parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
		# (succ, parsed_modules,files, ca)
				= parseAndScanDclModule import_module import_file_position parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
		  (mods_succ, parsed_modules,files, ca)
		  		= scanModules mods parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
		= (succ && mods_succ, parsed_modules,files, ca)
where
	in_cache mod_id []
		= False
	in_cache mod_id [cached_module_ident : pmods]
		| mod_id==cached_module_ident
			=True
			= in_cache mod_id pmods

 	try_to_find :: Ident [ScannedModule] -> (Bool,ModuleKind)
	try_to_find mod_id []
		= (False,MK_None)
	try_to_find mod_id [pmod : pmods]
		| mod_id == pmod.mod_ident
			= (True,pmod.mod_type)
			= try_to_find mod_id pmods

MakeEmptyModule name mod_type
	:== { mod_ident = name, mod_modification_time = "", mod_type = mod_type, mod_imports = [], mod_imported_objects = [],mod_foreign_exports=[],mod_defs =
				{	def_types = [], def_constructors = [], def_selectors = [], def_classes = [], def_macro_indices={ir_from=0,ir_to=0},
					def_macros=[],def_members = [], def_funtypes = [], def_instances = [], 
					def_generics = [], def_generic_cases = []} }

parseAndScanDclModule :: !Ident !Position ![ScannedModule] ![Ident] !SearchPaths !Bool Bool (ModTimeFunction *Files) !*Files !*CollectAdmin
	-> *(!Bool, ![ScannedModule],!*Files, !*CollectAdmin)
parseAndScanDclModule dcl_module import_file_position parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
	# {ca_error, ca_hash_table} = ca
	# (opt_file_dir_time,files) = fopenInSearchPaths dcl_module.id_name ".dcl" searchPaths FReadData modtimefunction files
	| case opt_file_dir_time of No -> True; _ -> False
		# err_file = moduleCouldNotBeImportedError False dcl_module import_file_position ca_error.pea_file
		# ca & ca_error={pea_file=err_file,pea_ok=True}
		= (False, [MakeEmptyModule dcl_module MK_None: parsed_modules],files, ca)
	# (Yes (mod_file,mod_dir,mod_time)) = opt_file_dir_time
	# (parse_ok,dynamic_type_used,mod, ca_hash_table, err_file, files)
		= wantModule mod_file mod_time cWantDclFile dcl_module import_file_position support_generics ca_hash_table ca_error.pea_file files
	# ca = {ca & ca_hash_table=ca_hash_table, ca_error={pea_file=err_file,pea_ok=True} }
	| parse_ok
		= scan_dcl_module dcl_module mod parsed_modules searchPaths modtimefunction files ca
		= (False, [MakeEmptyModule mod.mod_ident MK_None: parsed_modules],files, ca)
where
	scan_dcl_module :: Ident ParsedModule [ScannedModule] !SearchPaths (ModTimeFunction *Files) *Files *CollectAdmin -> (Bool, [ScannedModule],*Files, *CollectAdmin)
	scan_dcl_module dcl_module mod=:{mod_defs = pdefs} parsed_modules searchPaths modtimefunction files ca
		# (_, defs, imports, imported_objects,foreign_exports,ca)
			= reorganiseDefinitionsAndAddTypes dcl_module support_dynamics False pdefs ca
		  n_macros = length defs.def_macros
  	  	  (def_macros, ca) = collectFunctions defs.def_macros False {ca & ca_fun_count=n_macros,ca_rev_fun_defs=[]}
		  range = {ir_from=0,ir_to=n_macros}
		  (rev_fun_defs,ca) = ca!ca_rev_fun_defs
		  def_macros = def_macros++reverse rev_fun_defs
		  (pea_ok,ca) = ca!ca_error.pea_ok
		  ca = {ca & ca_rev_fun_defs=[]}
		  mod = { mod & mod_imports = imports, mod_imported_objects = imported_objects, mod_defs = { defs & def_macros=def_macros,def_macro_indices = range }}
		  (import_ok, parsed_modules,files, ca)
			= scanModules imports [mod : parsed_modules] cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
		= (pea_ok && import_ok, parsed_modules,files, ca)

scanModule :: !ParsedModule ![Ident] !Bool !Bool !*HashTable !*File !SearchPaths (ModTimeFunction *Files) !*Files
	-> (!Bool, !ScannedModule, !IndexRange, ![FunDef], !Optional ScannedModule, ![ScannedModule],!Int,!*HashTable, !*File, !*Files)
scanModule mod=:{mod_ident,mod_type,mod_defs = pdefs} cached_modules support_generics support_dynamics hash_table err_file searchPaths /*predefs*/ modtimefunction files
	# predefIdents = predefined_idents
	# ca =	{	ca_error		= {pea_file = err_file, pea_ok = True}
			,	ca_fun_count	= 0
			,	ca_rev_fun_defs	= []
			,	ca_hash_table	= hash_table
			}
	  (fun_defs, defs, imports, imported_objects,foreign_exports,ca) = reorganiseDefinitionsAndAddTypes mod_ident support_dynamics True pdefs ca

	  (reorganise_icl_ok, ca) = ca!ca_error.pea_ok

	  (import_dcl_ok, optional_parsed_dcl_mod,dcl_module_n,parsed_modules, cached_modules,files, ca)
	  		= scan_main_dcl_module mod_ident mod_type modtimefunction files ca
	  (import_dcls_ok, parsed_modules, files, ca)
	  		= scanModules imports parsed_modules cached_modules searchPaths support_generics support_dynamics modtimefunction files ca

	  (pea_dcl_ok,optional_dcl_mod,ca) = collect_main_dcl_module optional_parsed_dcl_mod dcl_module_n ca

	  modules = case (reverse parsed_modules) of
						  	[{mod_type=MK_NoMainDcl}:modules]
						  		-> modules
						  	modules
						  		-> modules

	  import_dcl_ok = import_dcl_ok && pea_dcl_ok;

	  ca = {ca & ca_hash_table=set_hte_mark 1 ca.ca_hash_table}

	  n_global_functions = length fun_defs

	  (fun_defs, ca) = collectFunctions fun_defs True {ca & ca_fun_count=n_global_functions,ca_rev_fun_defs=[]}
	  (macro_defs, ca) = collectFunctions defs.def_macros True ca
	  (def_instances, ca) = collectFunctions defs.def_instances True ca
	  (def_generic_cases, ca) = collectFunctions defs.def_generic_cases True ca  
	  (macro_range, ca) = addFunctionsRange macro_defs ca

	  {ca_error = {pea_file = err_file,pea_ok}, ca_rev_fun_defs, ca_hash_table} = ca
	  mod = { mod & mod_imports = imports, mod_imported_objects = imported_objects, mod_foreign_exports = foreign_exports,
	  				mod_defs = { defs & def_instances = def_instances,
	  								def_generic_cases = def_generic_cases,
	  								def_macro_indices = macro_range }}
	  
	  hash_table = set_hte_mark 0 ca_hash_table

	  fun_defs = fun_defs++reverse ca_rev_fun_defs
	  fun_range = {ir_from=0,ir_to=n_global_functions}

	= (reorganise_icl_ok && pea_ok && import_dcl_ok && import_dcls_ok, mod, fun_range, fun_defs, optional_dcl_mod, modules, dcl_module_n,hash_table, err_file, files)
where
	scan_main_dcl_module :: Ident ModuleKind (ModTimeFunction *Files) *Files *CollectAdmin -> (!Bool,!Optional ScannedModule,!Int,![ScannedModule],![Ident],!*Files,!*CollectAdmin)
	scan_main_dcl_module mod_ident MK_Main _ files ca
		= (True, No,NoIndex,[MakeEmptyModule mod_ident MK_NoMainDcl], cached_modules,files, ca)
	scan_main_dcl_module mod_ident MK_None _ files ca
		= (True, No,NoIndex,[], cached_modules,files, ca)
	scan_main_dcl_module mod_ident kind modtimefunction files ca
		# module_n_in_cache = in_cache 0 cached_modules;
			with
			in_cache module_n []
				= NoIndex
			in_cache module_n [cached_module_ident : pmods]
				| mod_ident==cached_module_ident
					= module_n
					= in_cache (module_n+1) pmods
		| module_n_in_cache<>NoIndex
			= (True,No,module_n_in_cache,[],cached_modules,files,ca)
		# {ca_error, ca_hash_table} = ca
		# (opt_file_dir_time,files) = fopenInSearchPaths mod_ident.id_name ".dcl" searchPaths FReadData modtimefunction files
		| case opt_file_dir_time of No -> True; _ -> False
			# err_file = moduleCouldNotBeImportedError False mod_ident NoPos ca_error.pea_file
			# ca & ca_error={pea_file=err_file,pea_ok=True}
			= (False, No,NoIndex, [],cached_modules, files, ca)
		# (Yes (mod_file,mod_dir,mod_time)) = opt_file_dir_time
		# (parse_ok,dynamic_type_used,mod, hash_table, err_file, files)
			= wantModule mod_file mod_time cWantDclFile mod_ident NoPos support_generics ca_hash_table ca_error.pea_file files
		# ca = {ca & ca_hash_table=hash_table, ca_error={pea_file=err_file,pea_ok=True}}
		| not parse_ok
			= (False, No,NoIndex, [],cached_modules, files, ca)
			# pdefs = mod.mod_defs
			# (_, defs, imports, imported_objects,foreign_exports,ca) = reorganiseDefinitionsAndAddTypes mod_ident support_dynamics False pdefs ca
			# mod = { mod & mod_imports = imports, mod_imported_objects = imported_objects, mod_defs = defs}
			# cached_modules = [mod.mod_ident:cached_modules]
			# (import_ok, parsed_modules,files, ca) = scanModules imports [] cached_modules searchPaths support_generics support_dynamics modtimefunction files ca
			= (import_ok, Yes mod, NoIndex,parsed_modules, cached_modules,files, ca)

	collect_main_dcl_module (Yes mod=:{mod_defs=defs}) dcl_module_n ca
	 #	n_macros = length defs.def_macros
	 	(def_macros, ca) = collectFunctions defs.def_macros False {ca & ca_fun_count=n_macros,ca_rev_fun_defs=[]}
		range = {ir_from=0,ir_to=n_macros}
		(rev_fun_defs,ca) = ca!ca_rev_fun_defs
		def_macros = def_macros++reverse rev_fun_defs
		ca = {ca & ca_rev_fun_defs=[]}
		(pea_ok,ca) = ca!ca_error.pea_ok
		mod  = { mod & mod_defs = { defs & def_macros=def_macros,def_macro_indices = range }}
	 = (pea_ok,Yes mod,ca)
	collect_main_dcl_module No dcl_module_n ca
		| dcl_module_n==NoIndex
			 =	(True,Yes (MakeEmptyModule mod_ident MK_None),ca)
			 =	(True,No,ca)

MakeNewParsedDef ident args rhs pos
	:==	PD_Function pos ident False args rhs (FK_Function cNameLocationDependent)

collectFunctionBodies :: !Ident !Int !Priority !FunKind ![ParsedDefinition] !*CollectAdmin
	-> (![ParsedBody], !FunKind, ![ParsedDefinition], !*CollectAdmin)
collectFunctionBodies fun_name fun_arity fun_prio fun_kind all_defs=:[PD_Function pos name is_infix args rhs new_fun_kind : defs] ca
	| belongsToTypeSpec fun_name fun_prio name is_infix
		# (new_fun_kind, ca) = combine_fun_kinds pos fun_kind new_fun_kind ca
		  (bodies, new_fun_kind, rest_defs, ca) = collectFunctionBodies fun_name fun_arity fun_prio new_fun_kind defs ca
		  act_arity	= length args
		| fun_arity == act_arity
			= ([{ pb_args = args, pb_rhs = rhs, pb_position = pos } : bodies ], new_fun_kind, rest_defs, ca)
			= ([{ pb_args = args, pb_rhs = rhs, pb_position = pos } : bodies ], new_fun_kind, rest_defs, 
			    postParseError pos	("This alternative has " + toString act_arity +
			  						 (if (act_arity == 1)" argument instead of " " arguments instead of ") + toString fun_arity
			  						) ca
			  )
		= ([], fun_kind, all_defs, ca)
	where
		combine_fun_kinds :: Position FunKind FunKind *CollectAdmin -> (FunKind, *CollectAdmin)
		combine_fun_kinds pos FK_Unknown fun_kind ca
			= (fun_kind, ca)
		combine_fun_kinds pos fun_kind new_fun_kind ca
			| fun_kind == new_fun_kind
				= (fun_kind, ca)
				= (fun_kind, postParseError pos "illegal combination of function alternatives" ca)
collectFunctionBodies fun_name fun_arity fun_prio fun_kind defs ca
	= ([], fun_kind, defs, ca)

collectGenericBodies :: ![ParsedDefinition] !Ident !Int !TypeCons !*CollectAdmin -> (![ParsedBody], !Int, ![ParsedDefinition],!*CollectAdmin)
collectGenericBodies all_defs=:[PD_GenericCase gc=:{gc_gcf=GCF gc_ident2 gcf} _ : defs] gc_ident1 gcf_arity1 gc_type_cons1 ca
	| gc_ident2==gc_ident1 && gc.gc_type_cons==gc_type_cons1
		#! (bodies, generic_info, rest_defs, ca) = collectGenericBodies defs gc_ident1 gcf_arity1 gc_type_cons1 ca
		# (GCF _ {gcf_body=GCB_ParsedBody args rhs,gcf_arity,gcf_generic_info}) = gc.gc_gcf
		# generic_info = generic_info bitor gcf_generic_info
		#! body = {pb_args = args, pb_rhs = rhs, pb_position = gc.gc_pos}
		| gcf_arity==gcf_arity1
			= ([body : bodies], generic_info, rest_defs, ca)
			#! msg = "This generic alternative has " +++ toString gcf_arity +++ " argument" 
					 +++ (if (gcf_arity <> 1) "s" "")+++" instead of " +++ toString gcf_arity1
			#! ca = postParseError gc.gc_pos msg ca	
			= ([body : bodies], generic_info, rest_defs, ca)
		= ([], 0, all_defs, ca)
collectGenericBodies defs gc_ident1 gcf_arity1 gc_type_cons1 ca
	= ([], 0, defs, ca)

replace_generic_info_record_by_arguments :: !Int ![ParsedBody] !Int !TypeCons !*CollectAdmin -> (![ParsedBody],!Int,!GenericInstanceDependencies,!*CollectAdmin)
replace_generic_info_record_by_arguments generic_info bodies arity (TypeConsSymb {type_ident={id_name}}) ca
	# arity = add_n_bits generic_info (arity-1)
	# (bodies,n_deps,deps,ca) = replace_generic_info_record_by_arguments_in_bodies bodies generic_info -1 0 ca
	| n_deps>=0
		# deps = deps bitand ((1<<n_deps)-1)
		= (bodies,arity,GenericInstanceUsedArgs n_deps deps,ca)
		= (bodies,arity,GenericInstanceUsedArgs 0 0,ca)
  where
	generic_cons_index
		= case id_name of
			"OBJECT" -> 0
			"CONS" -> 1
			"RECORD" -> 2
			"FIELD" -> 3

	replace_generic_info_record_by_arguments_in_bodies [body:bodies] generic_info n_deps deps ca
		# (body,n_deps,deps,ca) = replace_generic_info_record_by_arguments_in_body body generic_info n_deps deps ca
		# (bodies,n_deps,deps,ca) = replace_generic_info_record_by_arguments_in_bodies bodies generic_info n_deps deps ca
		= ([body : bodies],n_deps,deps,ca)
	replace_generic_info_record_by_arguments_in_bodies [] generic_info n_deps deps ca
		= ([],n_deps,deps,ca)

	replace_generic_info_record_by_arguments_in_body body=:{pb_args=[PE_Record PE_Empty NoRecordName field_assignments:args]} generic_info n_deps deps ca
		# (n_deps,deps) = mark_deps_in_args args 0 n_deps deps
		# (args,ca) = add_fields generic_info field_assignments args ca
		= ({body & pb_args = args},n_deps,deps,ca)
	replace_generic_info_record_by_arguments_in_body body=:{pb_args=[PE_WildCard:args]} generic_info n_deps deps ca
		# (n_deps,deps) = mark_deps_in_args args 0 n_deps deps
		# args = add_wild_cards generic_info args
		= ({body & pb_args = args},n_deps,deps,ca)

	add_fields :: !Int [FieldAssignment] [ParsedExpr] *CollectAdmin -> (![ParsedExpr],!*CollectAdmin)
	add_fields generic_info field_assignments args ca
		| generic_info==0
			= (args,ca)
		| generic_info bitand 1<>0
			= add_field (field_0_name_of_generic_info generic_cons_index) (generic_info bitxor 1) field_assignments args ca
		| generic_info bitand 2<>0
			= add_field (field_1_name_of_generic_info generic_cons_index) (generic_info bitxor 2) field_assignments args ca
		| generic_info bitand 4<>0
			= add_field (field_2_name_of_generic_info generic_cons_index) (generic_info bitxor 4) field_assignments args ca
		| generic_info bitand 8<>0
			= add_field (field_3_name_of_generic_info generic_cons_index) (generic_info bitxor 8) field_assignments args ca
		| generic_info bitand 16<>0
			= add_field (field_4_name_of_generic_info generic_cons_index) (generic_info bitxor 16) field_assignments args ca
		| generic_info bitand 32<>0
			= add_field (field_5_name_of_generic_info generic_cons_index) (generic_info bitxor 32) field_assignments args ca

	add_field :: !{#Char} !Int [FieldAssignment] [ParsedExpr] *CollectAdmin -> (![ParsedExpr],!*CollectAdmin)
	add_field field_name generic_info field_assignments args ca
		# (arg,ca) = field_or_wild_card field_name field_assignments ca
		# (args,ca) = add_fields generic_info field_assignments args ca
		= ([arg : args],ca)

	add_wild_cards 0 args
		= args
	add_wild_cards generic_info args
		| generic_info bitand 1<>0
			= [PE_WildCard : add_wild_cards (generic_info>>1) args]
			= add_wild_cards (generic_info>>1) args

	field_or_wild_card field_name [{bind_dst=FieldName {id_name},bind_src}:field_assignments] ca
		| id_name==field_name
			= case bind_src of
				PE_Empty
					# ({boxed_ident=ident}, ca_hash_table) = putIdentInHashTable id_name IC_Expression ca.ca_hash_table
					-> (PE_Ident ident, {ca & ca_hash_table = ca_hash_table})
				_ 
					-> (bind_src,ca)
			= field_or_wild_card field_name field_assignments ca
	field_or_wild_card field_name field_assignments ca
		= (PE_WildCard,ca)

	add_n_bits n c
		| n>1
			= add_n_bits (n>>1) (c+(n bitand 1))
			= c+n

determine_generic_instance_deps :: ![ParsedBody] !Int !TypeCons !*CollectAdmin -> (![ParsedBody],!Int,!GenericInstanceDependencies,!*CollectAdmin)
determine_generic_instance_deps bodies arity type_cons ca
	= case type_cons of
		TypeConsSymb {type_ident={id_name}}
			| id_name=="OBJECT" || id_name=="CONS" || id_name=="RECORD" || id_name=="FIELD" || id_name=="PAIR" || id_name=="EITHER" || id_name=="UNIT"
				# (n_deps,deps) = determine_generic_instance_deps_in_bodies bodies -1 0
				| n_deps>=0
					# deps = deps bitand ((1<<n_deps)-1)
					-> (bodies,arity,GenericInstanceUsedArgs n_deps deps,ca)
					-> (bodies,arity,GenericInstanceUsedArgs 0 0,ca)
		_
			-> (bodies,arity,AllGenericInstanceDependencies,ca)
  where
	determine_generic_instance_deps_in_bodies [body:bodies] n_deps deps
		# (n_deps,deps) = determine_generic_instance_deps_in_body body n_deps deps
		= determine_generic_instance_deps_in_bodies bodies n_deps deps
	determine_generic_instance_deps_in_bodies [] n_deps deps
		= (n_deps,deps)

	determine_generic_instance_deps_in_body {pb_args=[_:args]} n_deps deps
		= mark_deps_in_args args 0 n_deps deps
	determine_generic_instance_deps_in_body body n_deps deps
		= (n_deps,deps)

remove_generic_info_and_determine_generic_instance_deps :: ![ParsedBody] !Int !TypeCons !*CollectAdmin -> (![ParsedBody],!Int,!GenericInstanceDependencies,!*CollectAdmin)
remove_generic_info_and_determine_generic_instance_deps bodies arity type_cons ca
	= case type_cons of
		TypeConsSymb {type_ident={id_name}}
			| id_name=="OBJECT" || id_name=="CONS" || id_name=="RECORD" || id_name=="FIELD" || id_name=="PAIR" || id_name=="EITHER" || id_name=="UNIT"
				# (bodies,n_deps,deps) = remove_generic_info_and_determine_generic_instance_deps_in_bodies bodies -1 0
				| n_deps>=0
					# deps = deps bitand ((1<<n_deps)-1)
					-> (bodies,arity-1,GenericInstanceUsedArgs n_deps deps,ca)
					-> (bodies,arity-1,GenericInstanceUsedArgs 0 0,ca)
		_
			-> (map remove_generic_info_in_body bodies,arity-1,AllGenericInstanceDependencies,ca)
  where
	remove_generic_info_and_determine_generic_instance_deps_in_bodies [body:bodies] n_deps deps
		# (body,n_deps,deps) = remove_generic_info_and_determine_generic_instance_deps_in_body body n_deps deps
		# (bodies,n_deps,deps) = remove_generic_info_and_determine_generic_instance_deps_in_bodies bodies n_deps deps
		= ([body:bodies],n_deps,deps)
	remove_generic_info_and_determine_generic_instance_deps_in_bodies [] n_deps deps
		= ([],n_deps,deps)

	remove_generic_info_and_determine_generic_instance_deps_in_body body=:{pb_args=[_:args]} n_deps deps
		# (n_deps,deps) = mark_deps_in_args args 0 n_deps deps
		= ({body & pb_args=args},n_deps,deps)
	remove_generic_info_and_determine_generic_instance_deps_in_body body n_deps deps
		= (body,n_deps,deps)

	remove_generic_info_in_body body=:{pb_args=[_:args]}
		= {body & pb_args=args}
	remove_generic_info_in_body body
		= body

mark_deps_in_args :: [ParsedExpr] Int Int Int -> (!Int,!Int)
mark_deps_in_args [PE_WildCard:args] arg_n n_deps deps
	= mark_deps_in_args args (arg_n+1) n_deps deps
mark_deps_in_args [_:args] arg_n n_deps deps
	# deps = deps bitor (1<<arg_n)
	= mark_deps_in_args args (arg_n+1) n_deps deps
mark_deps_in_args [] arg_n n_deps deps
	| n_deps>=0
		| arg_n<n_deps
			= (arg_n,deps)
			= (n_deps,deps)
		= (arg_n,deps)

strictness_from_fields :: ![ParsedSelector] -> StrictnessList
strictness_from_fields fields
	= add_strictness_for_arguments fields 0 0 NotStrict
where 
	add_strictness_for_arguments :: ![ParsedSelector] !Int !Int !StrictnessList -> StrictnessList
	add_strictness_for_arguments [] strictness_index strictness strictness_list
		| strictness==0
			= strictness_list
			= append_strictness strictness strictness_list
	add_strictness_for_arguments [{ps_field_annotation=AN_Strict}:fields] strictness_index strictness strictness_list
		# (strictness_index,strictness,strictness_list) = add_next_strict strictness_index strictness strictness_list
		= add_strictness_for_arguments fields strictness_index strictness strictness_list
	add_strictness_for_arguments [{ps_field_annotation=AN_None}:fields] strictness_index strictness strictness_list
		# (strictness_index,strictness,strictness_list) = add_next_not_strict strictness_index strictness strictness_list
		= add_strictness_for_arguments fields strictness_index strictness strictness_list

:: *DefCounts = !{
	cons_count	:: !Int,
	sel_count	:: !Int,
	mem_count	:: !Int,
	type_count	:: !Int,
	macro_count	:: !Int
   }

reorganiseDefinitions :: Bool [ParsedDefinition] !DefCounts *CollectAdmin -> (![FunDef],!CollectedDefinitions (ScannedInstanceAndMembersR FunDef), ![ParsedImport], ![ImportedObject],![ParsedForeignExport],!*CollectAdmin)
reorganiseDefinitions icl_module [PD_Function pos name is_infix args rhs fun_kind : defs] def_counts=:{macro_count} ca
	# prio = if is_infix (Prio NoAssoc 9) NoPrio
	  fun_arity = length args
	  (bodies, fun_kind, defs, ca) = collectFunctionBodies name fun_arity prio fun_kind defs ca
	  fun = MakeNewImpOrDefFunction name fun_arity [{ pb_args = args, pb_rhs = rhs, pb_position = pos } : bodies] fun_kind prio No pos
	| fun_kind == FK_Macro
		# def_counts & macro_count=macro_count+1
		  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
		= (fun_defs, { c_defs & def_macros = [ fun : c_defs.def_macros ]}, imports, imported_objects,foreign_exports, ca)
		# (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
		= ([ fun : fun_defs ], c_defs, imports, imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_TypeSpec fun_pos fun_name prio No specials : defs] def_counts=:{macro_count} ca
	= case defs of
		[PD_Function pos name is_infix args rhs fun_kind : defs]
			| fun_name <> name
				-> reorganiseDefinitions icl_module defs def_counts (postParseError fun_pos ("function alternative for "+++fun_name.id_name+++" expected") ca)
			| not (sameFixity prio is_infix)
				-> reorganiseDefinitions icl_module defs def_counts (postParseError fun_pos "infix of type specification and alternative should match" ca)
			//	| belongsToTypeSpec fun_name prio name is_infix
  				# fun_arity = length args
				  (bodies, fun_kind, defs, ca) = collectFunctionBodies name fun_arity prio fun_kind defs ca
				  fun = MakeNewImpOrDefFunction name fun_arity [{ pb_args = args, pb_rhs = rhs, pb_position = pos } : bodies ] fun_kind prio No fun_pos
				| fun_kind == FK_Macro
					# def_counts & macro_count=macro_count+1
	  				  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
					-> (fun_defs, { c_defs & def_macros = [ fun : c_defs.def_macros]}, imports, imported_objects,foreign_exports, ca)
				  	# (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
					-> ([ fun : fun_defs ], c_defs, imports, imported_objects,foreign_exports, ca)
			//	-> reorganiseDefinitions icl_module defs cons_count sel_count mem_count (postParseError fun_pos "function body expected (1)" ca)
		_
			-> reorganiseDefinitions icl_module defs def_counts (postParseError fun_pos "function alternative expected (2)" ca)
reorganiseDefinitions icl_module [PD_TypeSpec pos name prio (Yes fun_type=:{st_arity}) specials : defs] def_counts ca
	# (bodies, fun_kind, defs, ca) = collectFunctionBodies name st_arity prio FK_Unknown defs ca
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	| isEmpty bodies
		# fun_type = MakeNewFunctionType name st_arity prio fun_type pos specials nilPtr
		  c_defs = { c_defs & def_funtypes = [ fun_type : c_defs.def_funtypes ]}
		| icl_module
			= (fun_defs, c_defs, imports, imported_objects,foreign_exports, postParseError pos "function body expected" ca)
			= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)
		# fun = MakeNewImpOrDefFunction name fun_type.st_arity bodies fun_kind prio (Yes fun_type) pos
		| icl_module
			| case fun_kind of FK_Macro -> True; _ -> False
				= ([fun : fun_defs], c_defs, imports, imported_objects,foreign_exports, postParseError pos "macro with function type not allowed" ca)		  
				= ([fun : fun_defs], c_defs, imports, imported_objects,foreign_exports, ca)		  
			= ([fun : fun_defs], c_defs, imports, imported_objects,foreign_exports, postParseError pos "function body not allowed in definition module" ca)
reorganiseDefinitions icl_module [PD_Type type_def=:{td_rhs = ConsList cons_defs} : defs] def_counts=:{cons_count,type_count} ca
	# (cons_symbs, cons_count) = determine_symbols_of_conses cons_defs cons_count
	  def_counts & cons_count=cons_count, type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  type_def = { type_def & td_rhs = AlgType cons_symbs }
	  c_defs = { c_defs & def_types = [type_def : c_defs.def_types], def_constructors = mapAppend ParsedConstructorToConsDef cons_defs c_defs.def_constructors }
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)  
reorganiseDefinitions icl_module [PD_Type type_def=:{td_rhs = NewTypeCons cons_def=:{pc_cons_ident,pc_cons_arity}} : defs] def_counts=:{cons_count,type_count} ca
	# cons_symb = { ds_ident = pc_cons_ident, ds_arity = pc_cons_arity, ds_index = cons_count }
	  def_counts & cons_count=cons_count+1, type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  type_def = { type_def & td_rhs = NewType cons_symb }
	  c_defs = { c_defs & def_types = [type_def : c_defs.def_types], def_constructors = [ParsedConstructorToConsDef cons_def : c_defs.def_constructors] }
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)  
reorganiseDefinitions icl_module [PD_Type type_def=:{td_ident, td_rhs = SelectorList rec_cons_id exivars is_boxed_record sel_defs, td_pos } : defs] def_counts=:{cons_count,sel_count,type_count} ca
	# (sel_syms, new_count) = determine_symbols_of_selectors sel_defs sel_count
	  def_counts & cons_count=cons_count+1, sel_count=new_count, type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  cons_arity = new_count - sel_count
	  pc_arg_types = [ ps_field_type \\ {ps_field_type} <- sel_defs ]
	  cons_def = {	pc_cons_ident = rec_cons_id, pc_cons_prio = NoPrio, pc_cons_arity = cons_arity, pc_cons_pos = td_pos,
	  				pc_arg_types = pc_arg_types, pc_args_strictness=strictness_from_fields sel_defs,pc_context=[], pc_exi_vars = exivars }
	  type_def = { type_def & td_rhs = RecordType {rt_constructor = { ds_ident = rec_cons_id, ds_arity = cons_arity, ds_index = cons_count },
	  							rt_fields =  { sel \\ sel <- sel_syms }, rt_is_boxed_record = is_boxed_record}}
	  c_defs = { c_defs & def_types = [type_def : c_defs.def_types], def_constructors = [ParsedConstructorToConsDef cons_def : c_defs.def_constructors],
	  				def_selectors = mapAppend (ParsedSelectorToSelectorDef type_count) sel_defs c_defs.def_selectors }
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)  
where
	determine_symbols_of_selectors :: [ParsedSelector] Index -> ([FieldSymbol], Index)
	determine_symbols_of_selectors [{ps_field_ident,ps_field_var} : sels] next_selector_index
		# field = { fs_ident = ps_field_ident, fs_var = ps_field_var, fs_index = next_selector_index }
		  (fields, next_selector_index) = determine_symbols_of_selectors sels (inc next_selector_index)
		= ([field : fields], next_selector_index)
	determine_symbols_of_selectors [] next_selector_index
		= ([], next_selector_index)
reorganiseDefinitions icl_module [PD_Type type_def=:{td_rhs = TypeSpec type} : defs] def_counts=:{type_count} ca
	# def_counts & type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  type_def = { type_def & td_rhs = SynType type }
	  c_defs = { c_defs & def_types = [type_def : c_defs.def_types] }
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)  
reorganiseDefinitions icl_module [PD_Type type_def=:{td_rhs = EmptyRhs properties} : defs] def_counts=:{type_count} ca
	# def_counts & type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  type_def = { type_def & td_rhs = AbstractType properties }
	  c_defs = { c_defs & def_types = [type_def : c_defs.def_types] }
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_Type type_def=:{td_rhs = AbstractTypeSpec properties type} : defs] def_counts=:{type_count} ca
	# def_counts & type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  type_def = { type_def & td_rhs = AbstractSynType properties type }
	  c_defs = { c_defs & def_types = [type_def : c_defs.def_types] }
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_Type type_def=:{td_rhs = ExtensibleConses cons_defs} : defs] def_counts=:{cons_count,type_count} ca
	# (cons_symbs, cons_count) = determine_symbols_of_conses cons_defs cons_count
	  def_counts & cons_count=cons_count, type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  type_def & td_rhs = ExtensibleAlgType cons_symbs
	  c_defs & def_types = [type_def : c_defs.def_types], def_constructors = mapAppend ParsedConstructorToConsDef cons_defs c_defs.def_constructors
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)  
reorganiseDefinitions icl_module [PD_Type type_def=:{td_rhs = MoreConses type_ext_ident cons_defs} : defs] def_counts=:{cons_count,type_count} ca
	# (cons_symbs, cons_count) = determine_symbols_of_conses cons_defs cons_count
	  def_counts & cons_count=cons_count, type_count=type_count+1
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  type_def & td_rhs = UncheckedAlgConses type_ext_ident cons_symbs
	  c_defs & def_types = [type_def : c_defs.def_types], def_constructors = mapAppend ParsedConstructorToConsDef cons_defs c_defs.def_constructors
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)  
reorganiseDefinitions icl_module [PD_Class class_def=:{class_ident,class_arity,class_args} members : defs] def_counts=:{mem_count,macro_count} ca
	# type_context = { tc_class = TCClass {glob_module = NoIndex, glob_object = {ds_ident = class_ident, ds_arity = class_arity, ds_index = NoIndex }},
					   tc_types = [ TV tv \\ tv <- class_args ], tc_var = nilPtr}
	  (mem_defs,mem_macros,default_members_without_type,macro_members,macro_count,ca)
			= check_symbols_of_class_members members type_context macro_count ca
	  (mem_defs,ca) = add_default_members_without_type default_members_without_type mem_defs ca
	  (mem_symbs, mem_defs, class_size) = reorganise_member_defs mem_defs mem_count
	  def_counts & mem_count=mem_count + class_size, macro_count=macro_count
	  (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	  class_def = { class_def & class_members = { member \\ member <- mem_symbs}
	  						  , class_macro_members = {macro_member \\ macro_member<|-macro_members}
				  }
	  c_defs = { c_defs & def_classes = [class_def : c_defs.def_classes], def_macros = mem_macros ++ c_defs.def_macros,
	  			 def_members = mem_defs ++ c_defs.def_members }
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)  
where
	check_symbols_of_class_members :: ![ParsedDefinition] !TypeContext !Int !*CollectAdmin
									-> (![MemberDef],![FunDef],![(Ident,MacroMember,Position)],[!MacroMember!],!Int,!*CollectAdmin)
	check_symbols_of_class_members [PD_TypeSpec pos name prio opt_type=:(Yes type=:{st_context,st_arity}) specials : defs] type_context macro_count ca
		# (bodies, fun_kind, defs, ca) = collectFunctionBodies name st_arity prio FK_Unknown defs ca
		| isEmpty bodies
			# mem_def = {	me_ident = name, me_type = { type & st_context = [type_context : st_context ]}, me_pos = pos, me_priority = prio,
							me_default_implementation = No,
							me_offset = NoIndex, me_class_vars = [], me_class = { glob_module = NoIndex, glob_object = NoIndex}, me_type_ptr = nilPtr }
			  (mem_defs,mem_macros,default_members_without_type,macro_members,new_macro_count,ca)
					= check_symbols_of_class_members defs type_context macro_count ca
			= ([mem_def : mem_defs],mem_macros,default_members_without_type,macro_members,new_macro_count,ca)
		= case fun_kind of
		   FK_Macro
			# macro = MakeNewImpOrDefFunction name st_arity bodies FK_Macro prio opt_type pos
			  (mem_defs,mem_macros,default_members_without_type,macro_members,new_macro_count,ca)
					= check_symbols_of_class_members defs type_context (macro_count+1) ca
			  macro_member = {mm_ident=name,mm_index=macro_count}
			= (mem_defs,[macro : mem_macros],default_members_without_type,[|macro_member : macro_members],new_macro_count,ca)
		   FK_Function _
		    # macro_name = class_ident.id_name+++"_"+++name.id_name
			# ({boxed_ident=macro_ident}, ca_hash_table) = putIdentInHashTable macro_name IC_Expression ca.ca_hash_table
			# ca = { ca & ca_hash_table = ca_hash_table }
			# macro = MakeNewImpOrDefFunction macro_ident st_arity bodies FK_Macro prio opt_type pos
			# mem_def = {	me_ident = name, me_type = { type & st_context = [type_context : st_context ]}, me_pos = pos, me_priority = prio,
							me_offset = NoIndex, me_class_vars = [], me_class = { glob_module = NoIndex, glob_object = NoIndex},
			 				me_default_implementation = Yes {mm_ident=macro_ident,mm_index=macro_count}, me_type_ptr = nilPtr }
			  (mem_defs,mem_macros,default_members_without_type,macro_members,macro_count,ca)
					= check_symbols_of_class_members defs type_context (macro_count+1) ca
			= ([mem_def : mem_defs],[macro : mem_macros],default_members_without_type,macro_members,macro_count,ca)
	check_symbols_of_class_members [PD_TypeSpec fun_pos fun_name prio No specials : defs] type_context macro_count ca
		= case defs of
			[PD_Function pos name is_infix args rhs fun_kind : defs]
				| belongsToTypeSpec fun_name prio name is_infix
  					# fun_arity = length args
  					  (bodies, fun_kind, defs, ca) = collectFunctionBodies name fun_arity prio fun_kind defs ca
					  (mem_defs,mem_macros,default_members_without_type,macro_members,new_macro_count,ca)
							= check_symbols_of_class_members defs type_context macro_count ca
					  macro = MakeNewImpOrDefFunction name fun_arity bodies FK_Macro prio No fun_pos
					-> (mem_defs,[macro : mem_macros],default_members_without_type,macro_members,new_macro_count,ca)
					-> check_symbols_of_class_members defs type_context macro_count (postParseError fun_pos "macro body expected" ca)
			_
				-> check_symbols_of_class_members defs type_context macro_count (postParseError fun_pos "macro body expected" ca)
	check_symbols_of_class_members [PD_Function fun_pos name is_infix args rhs fun_kind : defs] type_context macro_count ca
		# prio = if is_infix (Prio NoAssoc 9) NoPrio
		  fun_arity = length args
		  (bodies, fun_kind, defs, ca) = collectFunctionBodies name fun_arity prio fun_kind defs ca
		  bodies = [{ pb_args = args, pb_rhs = rhs, pb_position = fun_pos } : bodies]
		  (mem_defs,mem_macros,default_members_without_type,macro_members,new_macro_count,ca)
				= check_symbols_of_class_members defs type_context (macro_count+1) ca
		= case fun_kind of
			FK_Macro
				# macro = MakeNewImpOrDefFunction name fun_arity bodies FK_Macro prio No fun_pos
				  macro_member = {mm_ident=name,mm_index=macro_count}
				-> (mem_defs,[macro : mem_macros],default_members_without_type,[|macro_member : macro_members],new_macro_count,ca)
			FK_Function _
				# macro_name = class_ident.id_name+++"_"+++name.id_name
				  ({boxed_ident=macro_ident}, ca_hash_table) = putIdentInHashTable macro_name IC_Expression ca.ca_hash_table
				  ca = { ca & ca_hash_table = ca_hash_table }
				  macro = MakeNewImpOrDefFunction macro_ident fun_arity bodies FK_Macro prio No fun_pos
				  macro_member = {mm_ident=macro_ident,mm_index=macro_count}
				-> (mem_defs,[macro : mem_macros],[(name,macro_member,fun_pos) : default_members_without_type],macro_members,new_macro_count,ca)
	check_symbols_of_class_members [def : _] type_context macro_count ca
		= abort "postparse.check_symbols_of_class_members: unknown def"  // <<- def
	check_symbols_of_class_members [] type_context macro_count ca
		= ([],[],[],[!!],macro_count,ca)

	add_default_members_without_type :: ![(Ident,MacroMember,Position)] [MemberDef] *CollectAdmin -> *(![MemberDef],!*CollectAdmin)
	add_default_members_without_type [(name,macro_member,fun_pos):default_members_without_type] mem_defs ca
		# (mem_defs,ca) = add_default_member mem_defs name ca
		= add_default_members_without_type default_members_without_type mem_defs ca
	where
		add_default_member [mem_def:mem_defs] name ca
			| mem_def.me_ident==name && case mem_def.me_default_implementation of No -> True; _ -> False
				# mem_def = {mem_def & me_default_implementation = Yes macro_member}
				= ([mem_def:mem_defs],ca)
				# (mem_defs,ca) = add_default_member mem_defs name ca
				= ([mem_def:mem_defs],ca)
		add_default_member [] name ca
			# ca = postParseError fun_pos "type missing of default implementation" ca
			= ([],ca)
	add_default_members_without_type [] mem_defs ca
		= (mem_defs,ca)

	reorganise_member_defs :: [MemberDef] Index -> ([DefinedSymbol], [MemberDef], Index)
	reorganise_member_defs mem_defs first_mem_index
		# mem_defs = sort mem_defs
		= determine_indexes_of_class_members mem_defs first_mem_index 0
		
	determine_indexes_of_class_members :: [MemberDef] Index Index -> ([DefinedSymbol], [MemberDef], Index)
	determine_indexes_of_class_members [member=:{me_ident,me_type}:members] first_mem_index mem_offset
		#! (member_symbols, member_defs, last_mem_offset) = determine_indexes_of_class_members members first_mem_index (inc mem_offset)
		= ([{ds_ident = me_ident, ds_index = first_mem_index + mem_offset, ds_arity = me_type.st_arity } : member_symbols],
			[ { member & me_offset = mem_offset } : member_defs], last_mem_offset)
	determine_indexes_of_class_members [] first_mem_index last_mem_offset
		= ([], [], last_mem_offset)

reorganiseDefinitions icl_module [PD_Instance class_instance=:{pim_members,pim_pi} : defs] def_counts ca
	# (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	| icl_module || isEmpty pim_members
		# (mem_defs, ca) = collect_member_instances pim_members ca
		= (fun_defs, { c_defs & def_instances = [{sim_pi=class_instance.pim_pi, sim_members = mem_defs, sim_member_types=[]} : c_defs.def_instances] }, imports, imported_objects,foreign_exports, ca)
		# (mem_types, ca) = collect_member_instance_types pim_members ca
		= (fun_defs, { c_defs & def_instances = [{sim_pi=class_instance.pim_pi, sim_members = [], sim_member_types=mem_types} : c_defs.def_instances] }, imports, imported_objects,foreign_exports, ca)
where	  
	collect_member_instances :: [ParsedDefinition] *CollectAdmin -> ([FunDef], *CollectAdmin)
	collect_member_instances [PD_Function pos name is_infix args rhs fun_kind : defs] ca
		# fun_arity = length args
		  prio = if is_infix (Prio NoAssoc 9) NoPrio
		  (bodies, fun_kind, defs, ca) = collectFunctionBodies name fun_arity prio fun_kind defs ca
		  (fun_defs, ca) = collect_member_instances defs ca
		  fun = MakeNewImpOrDefFunction name fun_arity [{ pb_args = args, pb_rhs = rhs, pb_position = pos } : bodies ] fun_kind prio No pos
		= ([ fun : fun_defs ], ca)
	collect_member_instances [PD_TypeSpec fun_pos fun_name prio type specials : defs] ca
		= case defs of
			[PD_Function pos name is_infix args rhs fun_kind : _]
				| belongsToTypeSpec fun_name prio name is_infix
 					# fun_arity = determineArity args type
  					  (bodies, fun_kind, defs, ca) = collectFunctionBodies name fun_arity prio fun_kind defs ca
		  			  (fun_defs, ca) = collect_member_instances defs ca
					  fun = MakeNewImpOrDefFunction name fun_arity bodies fun_kind prio type fun_pos
					-> ([ fun : fun_defs ], ca)
			_
				-> collect_member_instances defs (postParseError fun_pos "function body expected" ca)
	collect_member_instances [] ca
	    = ([], ca)	

	collect_member_instance_types :: [ParsedDefinition] *CollectAdmin -> (![FunType], !*CollectAdmin)
	collect_member_instance_types [PD_TypeSpec fun_pos fun_name prio type specials : defs] ca
		= case type of
			Yes fun_type=:{st_arity}
				# fun_type = MakeNewFunctionType fun_name st_arity prio fun_type fun_pos specials nilPtr
  				  (fun_types, ca) = collect_member_instance_types defs ca
				-> ([fun_type : fun_types], ca)
			No
				-> collect_member_instance_types defs (postParseError fun_pos "function body expected" ca)
	collect_member_instance_types [] ca
	    = ([], ca)
reorganiseDefinitions icl_module [PD_Instances class_instances : defs] def_counts ca
	= reorganiseDefinitions icl_module ([PD_Instance class_instance \\ class_instance <- class_instances] ++ defs) def_counts ca
reorganiseDefinitions icl_module [PD_Generic gen : defs] def_counts ca
	# 	(fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
		c_defs = {c_defs & def_generics = [gen : c_defs.def_generics]}
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_GenericCase gc=:{gc_type_cons} generic_fun_ident : defs] def_counts=:{macro_count} ca
	# (GCF gc_ident gcf=:{gcf_body=GCB_ParsedBody args rhs,gcf_arity,gcf_generic_info}) = gc.gc_gcf
	#! (bodies, generic_info, defs, ca) = collectGenericBodies defs gc_ident gcf_arity gc_type_cons ca
	# generic_info = generic_info bitor gcf_generic_info
	#! body = { pb_args = args, pb_rhs = rhs, pb_position = gc.gc_pos }
	# bodies = [body : bodies]
	# fun_name = genericIdentToFunIdent gc_ident.id_name /*gcf.gcf_ident.id_name*/ gc.gc_type_cons
	| icl_module
		# (bodies,gcf_arity,generic_instance_deps,ca)
			= if (generic_info > 0)
				(replace_generic_info_record_by_arguments generic_info bodies gcf_arity gc_type_cons ca)
				(determine_generic_instance_deps bodies gcf_arity gc_type_cons ca)
		#! (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) 
			= reorganiseDefinitions icl_module defs def_counts ca
		#! fun = MakeNewImpOrDefFunction fun_name gcf_arity bodies (FK_Function cNameNotLocationDependent) NoPrio No gc.gc_pos
		# gcf & gcf_body=GCB_FunDef fun, gcf_arity=gcf_arity, gcf_generic_info=generic_info, gcf_generic_instance_deps=generic_instance_deps
		#! inst = {gc & gc_gcf = GCF gc_ident gcf}
		#! c_defs & def_generic_cases = [inst : c_defs.def_generic_cases]
		= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)

		# (bodies,gcf_arity,generic_instance_deps,ca)
			= if (generic_info > 0)
				(replace_generic_info_record_by_arguments generic_info bodies gcf_arity gc_type_cons ca)
				(if (generic_info < 0)
					(determine_generic_instance_deps bodies gcf_arity gc_type_cons ca)
					(remove_generic_info_and_determine_generic_instance_deps bodies gcf_arity gc_type_cons ca))
		# def_counts & macro_count=macro_count+1
		#! (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) 
			= reorganiseDefinitions icl_module defs def_counts ca
		#! macro = MakeNewImpOrDefFunction generic_fun_ident gcf_arity bodies (FK_Function False) NoPrio No gc.gc_pos
		# gcf & gcf_body=GCB_MacroIndex macro_count, gcf_arity=gcf_arity, gcf_generic_info=generic_info, gcf_generic_instance_deps=generic_instance_deps
		#! inst = {gc & gc_gcf = GCF gc_ident gcf}
		#! c_defs & def_generic_cases = [inst : c_defs.def_generic_cases], def_macros = [macro : c_defs.def_macros]
		= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_Derive derive_defs : defs] def_counts=:{type_count} ca
	# def_counts & type_count=type_count+1
	#! (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	#! c_defs = { c_defs & def_generic_cases = derive_defs ++ c_defs.def_generic_cases}
	= (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_Import new_imports : defs] def_counts ca
	# (new_imports,hash_table) = make_implicit_qualified_imports_explicit new_imports ca.ca_hash_table
	# ca = {ca & ca_hash_table=hash_table}
	# (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	= (fun_defs, c_defs, new_imports ++ imports, imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_ImportedObjects new_imported_objects : defs] def_counts ca
	# (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	= (fun_defs, c_defs, imports, new_imported_objects ++ imported_objects,foreign_exports, ca)
reorganiseDefinitions icl_module [PD_ForeignExport new_foreign_export file_name line_n stdcall : defs] def_counts ca
	# (fun_defs, c_defs, imports, imported_objects,foreign_exports, ca) = reorganiseDefinitions icl_module defs def_counts ca
	= (fun_defs, c_defs, imports, imported_objects,[{pfe_ident=new_foreign_export,pfe_file=file_name,pfe_line=line_n,pfe_stdcall=stdcall}:foreign_exports], ca)
reorganiseDefinitions icl_module [def:defs] _ ca
	= abort "reorganiseDefinitions does not match"
reorganiseDefinitions icl_module [] _ ca
	= ([], { def_types = [], def_constructors = [], def_selectors = [], def_macros = [],def_macro_indices={ir_from=0,ir_to=0},def_classes = [], def_members = [],
			def_instances = [], def_funtypes = [], 
			def_generics = [], def_generic_cases = []}, [], [], [], ca)

determine_symbols_of_conses :: [ParsedConstructor] Index -> ([DefinedSymbol], Index)
determine_symbols_of_conses [{pc_cons_ident,pc_cons_arity} : conses] next_cons_index
	# cons = { ds_ident = pc_cons_ident, ds_arity = pc_cons_arity, ds_index = next_cons_index }
	  (conses, next_cons_index) = determine_symbols_of_conses conses (inc next_cons_index)
	= ([cons : conses], next_cons_index)
determine_symbols_of_conses [] next_cons_index
	= ([], next_cons_index)

make_implicit_qualified_imports_explicit [import_=:{import_qualified=Qualified,import_symbols=ImportSymbolsAll,import_module,import_file_position}:imports] hash_table
	# (qualified_idents,hash_table) = get_qualified_idents_from_hash_table import_module hash_table
	# import_declarations = qualified_idents_to_import_declarations qualified_idents
	# (imports,hash_table) = make_implicit_qualified_imports_explicit imports hash_table
	= ([{import_ & import_symbols=ImportSymbolsOnly import_declarations}:imports],hash_table)
make_implicit_qualified_imports_explicit [import_:imports] hash_table
	# (imports,hash_table) = make_implicit_qualified_imports_explicit imports hash_table
	= ([import_:imports],hash_table)
make_implicit_qualified_imports_explicit [] hash_table
	= ([],hash_table)

qualified_idents_to_import_declarations (QualifiedIdents ident ident_class qualified_idents)
	= [qualified_ident_to_import_declaration ident_class ident : qualified_idents_to_import_declarations qualified_idents]
qualified_idents_to_import_declarations NoQualifiedIdents
	= []

qualified_ident_to_import_declaration IC_Expression ident
	= ID_Function ident
qualified_ident_to_import_declaration IC_Type ident
	= ID_Type ident No
qualified_ident_to_import_declaration IC_Class ident
	= ID_Class ident No
qualified_ident_to_import_declaration IC_Selector ident
	= abort "qualified_ident_to_import_declaration IC_Selector not yet implemented"

reorganiseDefinitionsAndAddTypes mod_ident support_dynamics icl_module defs ca
	# def_counts = {cons_count=0, sel_count=0, mem_count=0, type_count=0, macro_count=0}
	| support_dynamics
		# clean_types_module_ident
			=	predefined_idents.[PD_StdDynamic]
		# clean_types_module =
			{	import_module = clean_types_module_ident
			,	import_symbols = ImportSymbolsAll
			,	import_file_position = NoPos
			,	import_qualified = NotQualified
			}
		# imports = if (mod_ident == clean_types_module_ident) [] [clean_types_module]
		= reorganiseDefinitions icl_module [PD_Import imports : defs] def_counts ca
	// otherwise
		= reorganiseDefinitions icl_module defs def_counts ca

belongsToTypeSpec name prio new_name is_infix :==
	name == new_name && sameFixity prio is_infix

determineArity :: [ParsedExpr] (Optional SymbolType) -> Int
determineArity args (Yes {st_arity})
	=	st_arity
determineArity args No
	=	length args

sameFixity :: Priority Bool -> Bool
sameFixity (Prio _ _) is_infix
	=	is_infix
sameFixity NoPrio is_infix
	=	not is_infix
