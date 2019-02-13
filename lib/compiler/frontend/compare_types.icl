implementation module compare_types

import StdEnv, compare_constructor
import syntax

instance == TypeVar
where
	(==) varid1 varid2 = varid1.tv_info_ptr == varid2.tv_info_ptr

instance == AttributeVar 
where
	(==) varid1 varid2 = varid1.av_info_ptr == varid2.av_info_ptr

instance == AttrInequality
where
	(==) ai1 ai2 = ai1.ai_demanded == ai2.ai_demanded && ai1.ai_offered == ai2.ai_offered

instance == FunKind
where
	(==) fk1 fk2 = equal_constructor fk1 fk2

instance == (Global a) | == a
where
	(==) g1 g2
		= g1.glob_module == g2.glob_module && g1.glob_object == g2.glob_object


instance == TypeSymbIdent
where
	(==) tsymb_id1 tsymb_id2
		= tsymb_id1.type_index == tsymb_id2.type_index

instance == AType
where
	(==) atype1 atype2 = atype1.at_type == atype2.at_type

instance == ConsVariable
where
	(==) (CV tv1) (CV tv2)			= tv1 == tv2
	(==) (TempCV tv1) (TempCV tv2)	= tv1 == tv2
	(==) (TempQCV tv1) (TempQCV tv2)= tv1 == tv2
	(==) (TempQCDV tv1) (TempQCDV tv2)= tv1 == tv2
	(==) _ _ = False

instance == TypeContext
where
 (==) tc1 tc2 = tc1.tc_class == tc2.tc_class && tc1.tc_types == tc2.tc_types

instance == TCClass
where
	(==) (TCClass x) (TCClass y) 				= x == y
	(==) (TCGeneric {gtc_class}) (TCClass y) 	= gtc_class == y
	(==) (TCClass x) (TCGeneric {gtc_class}) 	= x == gtc_class
	(==) (TCGeneric {gtc_generic=g1,gtc_kind=k1}) (TCGeneric {gtc_generic=g2,gtc_kind=k2}) 	
		= g1 == g2 && k1 == k2

instance == BasicType
where
	(==) bt1 bt2 = equal_constructor bt1 bt2

instance == BasicValue
where
	(==) (BVI int1) (BVI int2) = int1 == int2
	(==) (BVI int1) (BVInt int2) = int1 == toString int2
	(==) (BVInt int1) (BVI int2) = toString int1 == int2
	(==) (BVInt int1) (BVInt int2) = int1 == int2
	(==) (BVC char1) (BVC char2)		= char1 == char2
	(==) (BVB bool1) (BVB bool2)		= bool1 == bool2
	(==) (BVR real1) (BVR real2)		= real1 == real2
	(==) (BVS string1) (BVS string2)	= string1 == string2
	(==) _ _							= False
			
instance == DefinedSymbol
where
	(==) ds1 ds2
		= ds1.ds_index == ds2.ds_index //&& ds1.ds_ident == ds2.ds_ident 

instance == Type
where
	(==) (TA tc1 types1) (TA tc2 types2)
		= tc1 == tc2 && types1 == types2
	(==) (TA tc1 types1) (TAS tc2 types2 _)
		= tc1 == tc2 && types1 == types2
	(==) (TA tc1 types1) _
		= False
	(==) (TAS tc1 types1 _) (TA tc2 types2)
		= tc1 == tc2 && types1 == types2
	(==) (TAS tc1 types1 _) (TAS tc2 types2 _)
		= tc1 == tc2 && types1 == types2
	(==) (TAS tc1 types1 _) _
		= False
	(==) t1 t2
		= equal_constructor t1 t2 && equal_constructor_args t1 t2
	where
		equal_constructor_args (TV varid1) (TV varid2)
			= varid1 == varid2
		equal_constructor_args (TempV varid1) (TempV varid2)
			= varid1 == varid2
		equal_constructor_args (arg_type1 --> restype1) (arg_type2 --> restype2)
			= arg_type1 == arg_type2 && restype1 == restype2
		equal_constructor_args (TB tb1) (TB tb2)
			= tb1 == tb2
		equal_constructor_args (type1 :@: types1) (type2 :@: types2)
			= type1 == type2 && types1 == types2
		equal_constructor_args (GTV varid1) (GTV varid2)
			= varid1 == varid2
		equal_constructor_args (TempQV varid1) (TempQV varid2)
			= varid1 == varid2
		equal_constructor_args (TempQDV varid1) (TempQDV varid2)
			= varid1 == varid2
		equal_constructor_args (TLifted varid1) (TLifted varid2)
			= varid1 == varid2
		equal_constructor_args type1 type2
			= True

instance == Priority
where
	(==) NoPrio NoPrio = True
	(==) (Prio assoc1 prio1) (Prio assoc2 prio2) = assoc1==assoc2 && prio1==prio2
	(==) _ _ = False
	
instance == Assoc
where
	(==) a1 a2 = equal_constructor a1 a2

instance == SignClassification where
	(==) sc1 sc2 = sc1.sc_pos_vect == sc2.sc_pos_vect && sc1.sc_neg_vect == sc2.sc_neg_vect

instance == TypeCons where
	(==) (TypeConsSymb x) (TypeConsSymb y) = x == y 
	(==) (TypeConsBasic x) (TypeConsBasic y) = x == y 
	(==) TypeConsArrow TypeConsArrow = True
	(==) (TypeConsVar x) (TypeConsVar y) = x == y
	(==) _ _ = False

::	CompareValue :== Int
Smaller :== -1
Greater	:== 1
Equal	:== 0

class (=<) infix 4 a :: !a !a -> CompareValue

instance =< Int
where
	(=<) i1 i2
		| i1 == i2
			= Equal
		| i1 < i2
			= Smaller
			= Greater

instance =< SymbKind
where
	(=<) symb1 symb2
		| equal_constructor symb1 symb2
			= compare_indexes  symb1 symb2
		with
			compare_indexes (SK_Function i1) (SK_Function i2)						= i1 =< i2
			compare_indexes (SK_LocalMacroFunction i1) (SK_LocalMacroFunction i2) = i1 =< i2
			compare_indexes (SK_Constructor i1) (SK_Constructor i2)					= i1 =< i2
			compare_indexes (SK_OverloadedFunction i1) (SK_OverloadedFunction i2)	= i1 =< i2
			compare_indexes (SK_GeneratedFunction _ i1) (SK_GeneratedFunction _ i2)	= i1 =< i2
			compare_indexes (SK_LocalDclMacroFunction i1) (SK_LocalDclMacroFunction i2) = i1 =< i2

		| less_constructor symb1 symb2
			= Smaller
			= Greater

instance =< SymbIdent
where
	(=<) {symb_kind=symb_kind1} {symb_kind=symb_kind2} = symb_kind1 =< symb_kind2
			

instance =< App
where
	(=<) app1 app2
		# cmp = app1.app_symb =< app2.app_symb
		| cmp == Equal
			= app1.app_args =< app2.app_args
			= cmp

instance =< (a,b) | =< a & =< b
where
	(=<) (x1,y1) (x2,y2)
		# cmp = x1 =< x2
		| cmp == Equal
			= y1 =< y2
			= cmp
	
instance =< [a] | =< a
where
	(=<) [x:xs] [y:ys]	= (x,xs) =< (y,ys)
	(=<) [] []			= Equal
	(=<) [] _			= Smaller
	(=<) _ _			= Greater

instance =< {# Char}
where
	(=<) s1 s2
		| s1 == s2
			= Equal
		| s1 < s2
			= Smaller
			= Greater
	
instance =< Expression
where
	(=<) expr1 expr2
		| equal_constructor expr1 expr2
			= compare_arguments  expr1 expr2
		with
			compare_arguments (App app1) (App app2)						= app1 =< app2
			compare_arguments (Var v1) (Var v2)							= v1 =< v2
			compare_arguments (fun1 @ args1) (fun2 @ args2)				= (fun1,args1) =< (fun2,args2) 
			compare_arguments EE EE										= Equal		
			compare_arguments _ _										= Greater		
		| less_constructor expr1 expr2
			= Smaller
			= Greater

instance =< BoundVar
where
	(=<) bv1 bv2
		= bv1.var_ident =< bv2.var_ident
	
instance =< FreeVar
where
	(=<) fv1 fv2
		= fv1.fv_ident =< fv2.fv_ident
	
instance =< Ident
where
	(=<) id1 id2
		= id1.id_name =< id2.id_name

instance =< (Global a) | =< a
where
	(=<) g1 g2
		= (g1.glob_module,g1.glob_object) =< (g2.glob_module,g2.glob_object)

instance =< TypeSymbIdent
where
	(=<) s1 s2
		= s1.type_ident =< s2.type_ident

instance =< Type
where
	(=<) (TA tc1 args1) (TA tc2 args2)
		# cmp_app_symb = tc1 =< tc2
		| cmp_app_symb==Equal
			= length args1 =< length args2
		= cmp_app_symb
	(=<) (TA tc1 args1) (TAS tc2 args2 _)
		# cmp_app_symb = tc1 =< tc2
		| cmp_app_symb==Equal
			= length args1 =< length args2
		= cmp_app_symb
	(=<) (TAS tc1 args1 _) (TA tc2 args2)
		# cmp_app_symb = tc1 =< tc2
		| cmp_app_symb==Equal
			= length args1 =< length args2
		= cmp_app_symb
	(=<) (TAS tc1 args1 _) (TAS tc2 args2 _)
		# cmp_app_symb = tc1 =< tc2
		| cmp_app_symb==Equal
			= length args1 =< length args2
		= cmp_app_symb
	(=<) t1 t2
		| equal_constructor t1 t2
			= compare_arguments t1 t2
		| less_constructor t1 t2
			= Smaller
			= Greater
	where
		compare_arguments (TB tb1) (TB tb2)
			= tb1 =< tb2
		compare_arguments (TQualifiedIdent module_id1 name1 _) (TQualifiedIdent module_id2 name2 _)
			| module_id1.id_name<>module_id2.id_name
				| module_id1.id_name<module_id2.id_name
					= Smaller
					= Greater
			| name1<>name2
				| name1<name2
					= Smaller
					= Greater
				= Equal
		compare_arguments _ _
			= Equal

smallerOrEqual :: !Type !Type -> CompareValue
smallerOrEqual (TA tc1 args1) (TA tc2 args2)
	# cmp_app_symb = tc1 =< tc2
	| cmp_app_symb==Equal
		= args1 =< args2
	= cmp_app_symb
smallerOrEqual (TA tc1 args1) (TAS tc2 args2 _)
	# cmp_app_symb = tc1 =< tc2
	| cmp_app_symb==Equal
		= args1 =< args2
	= cmp_app_symb
smallerOrEqual (TAS tc1 args1 _) (TA tc2 args2)
	# cmp_app_symb = tc1 =< tc2
	| cmp_app_symb==Equal
		= args1 =< args2
	= cmp_app_symb
smallerOrEqual (TAS tc1 args1 _) (TAS tc2 args2 _)
	# cmp_app_symb = tc1 =< tc2
	| cmp_app_symb==Equal
		= args1 =< args2
	= cmp_app_symb
smallerOrEqual t1 t2
	| equal_constructor t1 t2
		= compare_arguments t1 t2
	| less_constructor t1 t2
		= Smaller
		= Greater
	where
		compare_arguments (l1 --> r1) (l2 --> r2)
			# cmp_app_symb = l1 =< l2
			| cmp_app_symb==Equal
				= r1 =< r2
			= cmp_app_symb
		compare_arguments (_ :@: args1) (_ :@: args2)
			= args1 =< args2
		compare_arguments (TB tb1) (TB tb2)
			= tb1 =< tb2
		compare_arguments (TQualifiedIdent module_id1 name1 type1) (TQualifiedIdent module_id2 name2 type2)
			| module_id1.id_name<>module_id2.id_name
				| module_id1.id_name<module_id2.id_name
					= Smaller
					= Greater
			| name1<>name2
				| name1<name2
					= Smaller
					= Greater
				= type1 =< type2
		compare_arguments _ _
			= Equal

instance =< AType
where
	(=<) {at_type=at_type_1} {at_type=at_type_2}
		= smallerOrEqual at_type_1 at_type_2

instance =< BasicType
where
	(=<) bt1 bt2
		| equal_constructor bt1 bt2
			= Equal
		| less_constructor bt1 bt2
			= Smaller
			= Greater

instance < MemberDef
where
	(<) md1 md2 = md1.me_ident.id_name < md2.me_ident.id_name
