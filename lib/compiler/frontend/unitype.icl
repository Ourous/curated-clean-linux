implementation module unitype

import StdEnv, StdStrictLists, compare_types
from StdOverloadedList import Any
import syntax, analunitypes, type, utilities, checktypes

AttrUni			:== 0
AttrMulti		:== 1
/*
FirstAttrVar	:== 2
*/
AttrExi			:== 2
FirstAttrVar	:== 3

::	CoercionTree	= CT_Node !Int !CoercionTree !CoercionTree | CT_Empty | CT_Unique | CT_NonUnique | CT_Existential

::	Coercions		= { coer_demanded :: !.{! .CoercionTree}, coer_offered :: !.{! .CoercionTree }}

::	AttributePartition :== {# Int}

::	PartitioningInfo = 
	{	pi_marks :: 		!.AttributePartition
	,	pi_next_num ::		!Int
	,	pi_groups ::		!.[[Int]]
	,	pi_deps ::			![Int]
	}

::	BOOLVECT :== Int

BITINDEX temp_var_id :== temp_var_id >> 5
BITNUMBER temp_var_id :== temp_var_id bitand 31

isPositive :: !TempVarId !{# BOOLVECT } -> Bool
isPositive var_id cons_vars
	= cons_vars.[BITINDEX var_id] bitand (1 << BITNUMBER var_id) <> 0

:: CoerceInfo = ! { ci_common_defs :: !{#CommonDefs}, ci_cons_vars :: !{#BOOLVECT}, ci_expand_newtypes :: !Bool }

determineAttributeCoercions :: !AType !AType !Bool !Bool !u:{!Type} !*Coercions !{#CommonDefs} !{#BOOLVECT} !*TypeDefInfos !*TypeHeaps
												 -> (!Optional (TypePosition, AType),!u:{!Type},!*Coercions,!*TypeDefInfos,!*TypeHeaps) 
determineAttributeCoercions off_type dem_type coercible expand_newtypes subst coercions defs cons_vars td_infos type_heaps
	# (_, exp_off_type, es) = expandType defs cons_vars off_type (subst, { es_type_heaps = type_heaps, es_td_infos = td_infos})
	  (_, exp_dem_type, (subst, {es_td_infos,es_type_heaps})) = expandType defs cons_vars dem_type es
	  ci = {ci_common_defs=defs, ci_cons_vars=cons_vars, ci_expand_newtypes=expand_newtypes}
	  (result, {crc_type_heaps, crc_coercions, crc_td_infos}) = coerce (if coercible PositiveSign TopSign) ci [#!] exp_off_type exp_dem_type
	  		 { crc_type_heaps = es_type_heaps, crc_coercions = coercions, crc_td_infos = es_td_infos}
	= case result of
		No
	  		-> (No, subst, crc_coercions, crc_td_infos, crc_type_heaps)
	  	Yes pos
			-> (Yes (pos, exp_off_type), subst, crc_coercions, crc_td_infos, crc_type_heaps)

/*
	= case result of
		No
	  		# (crc_coercions, copy_crc_coercions) = copyCoercions crc_coercions
			  format = { form_properties = cMarkAttribute, form_attr_position = Yes ([], copy_crc_coercions) }			
			| file_to_true (stderr <:: (format, exp_off_type, No) <:: (format, exp_dem_type, No) <<< '\n')
					---> ("determineAttributeCoercions (OK)", off_type, exp_off_type, ('\n', dem_type, exp_dem_type))
				-> (No, subst, crc_coercions, crc_td_infos, crc_type_heaps)
				-> undef
//	  		-> (No, subst, crc_coercions, crc_td_infos, crc_type_heaps)
	  	Yes pos
	  		# (crc_coercions, copy_crc_coercions) = copyCoercions crc_coercions
			  format = { form_properties = cMarkAttribute, form_attr_position = Yes ([], copy_crc_coercions) }			
			| file_to_true (stderr <:: (format, exp_off_type,No) <:: (format, exp_dem_type,No) <<< '\n')
					---> ("determineAttributeCoercions (NOK)", off_type, exp_off_type, ('\n',  dem_type, exp_dem_type))
				-> (Yes (pos, exp_off_type), subst, crc_coercions, crc_td_infos, crc_type_heaps)
				-> undef

file_to_true :: !File -> Bool
file_to_true file = True
*/

NotChecked :== -1	
DummyAttrNumber :== -1

::	AttributeGroups	:== {! [Int]}

partitionateAttributes :: !{! CoercionTree} !{! *CoercionTree} -> (!AttributePartition, !*{! CoercionTree})
partitionateAttributes coer_offered coer_demanded
	#! max_attr_nr = size coer_offered
	# partitioning_info = { pi_marks = createArray max_attr_nr NotChecked, pi_deps = [], pi_next_num = 0, pi_groups = [] }
	# {pi_marks,pi_groups} = partitionate_attributes FirstAttrVar max_attr_nr coer_offered partitioning_info
	  (nr_of_groups, groups) = reverse_and_length pi_groups 0 []
	  partition = build_partition 0 groups pi_marks
	# demanded = { CT_Empty \\ i <- [0 .. nr_of_groups - 1] }
	= (partition, adjust_coercions 0 groups partition coer_offered coer_demanded demanded)
where
	visit_attributes :: !CoercionTree !Int !Int !{! CoercionTree} !*PartitioningInfo -> *(!Int, !*PartitioningInfo)
	visit_attributes (CT_Node attr left right) max_attr_nr min_dep coer_offered pi=:{pi_marks}
		#! mark = pi_marks.[attr]
		| mark == NotChecked
			# (mark, pi) = partitionate_attribute attr max_attr_nr coer_offered pi
			  (min_dep, pi) = visit_attributes left max_attr_nr (min min_dep mark)  coer_offered pi
			= visit_attributes right max_attr_nr min_dep coer_offered pi
		# (min_dep, pi) = visit_attributes left max_attr_nr (min min_dep mark) coer_offered pi
		= visit_attributes right max_attr_nr min_dep coer_offered pi
	visit_attributes tree max_attr_nr min_dep coer_offered pi
		= (min_dep, pi)

	reverse_and_length :: !*[a] !Int ![a] -> (!Int, ![a])
	reverse_and_length [] length list = (length, list)
	reverse_and_length [ x : xs ] length list = reverse_and_length xs (inc length) [x : list]
	
 	partitionate_attributes :: !Int !Int !{!CoercionTree} !*PartitioningInfo -> *PartitioningInfo
	partitionate_attributes from_index max_attr_nr coer_offered pi=:{pi_marks}
		| from_index == max_attr_nr
			= pi
		| pi_marks.[from_index] == NotChecked
			# (_, pi) = partitionate_attribute from_index max_attr_nr coer_offered pi
			= partitionate_attributes (inc from_index) max_attr_nr coer_offered pi
		= partitionate_attributes (inc from_index) max_attr_nr coer_offered pi

	partitionate_attribute :: !Int !Int !{!CoercionTree} !*PartitioningInfo -> *(!Int, !*PartitioningInfo)
	partitionate_attribute attr max_attr_nr coer_offered=:{ [attr] = off_attributes } pi=:{pi_next_num}
		# (min_dep, pi) = visit_attributes off_attributes max_attr_nr max_attr_nr coer_offered (push_on_dep_stack attr pi)
		= try_to_close_group attr pi_next_num min_dep max_attr_nr pi
	where		
		push_on_dep_stack attr pi=:{pi_deps,pi_marks,pi_next_num}
			= { pi & pi_deps = [attr : pi_deps], pi_marks = { pi_marks & [attr] = pi_next_num }, pi_next_num = inc pi_next_num}

		try_to_close_group :: !Int !Int !Int !Int !*PartitioningInfo -> (!Int,!*PartitioningInfo)
		try_to_close_group attr attr_nr min_dep max_attr_nr pi=:{pi_marks, pi_deps, pi_groups}
			| attr_nr <= min_dep
				# (pi_deps, pi_marks, group) = close_group attr pi_deps pi_marks [] max_attr_nr
				= (max_attr_nr, { pi & pi_deps = pi_deps, pi_marks = pi_marks, pi_groups = [group : pi_groups] })
				= (min_dep, pi)
		where
			close_group :: !Int ![Int] !*{#Int} ![Int] Int -> (![Int],!*{#Int},![Int])
			close_group attr [d:ds] marks group max_attr_nr
				# marks = { marks & [d] = max_attr_nr }
				| d == attr
					= (ds, marks, [d : group])
					= close_group attr ds marks [d : group] max_attr_nr

	build_partition group_nr [] partition
		= partition
	build_partition group_nr [group : groups] partition
		= build_partition (inc group_nr) groups (build_partition_of_group group_nr group partition)
	where
		build_partition_of_group group_nr [attr : attrs] partition
			= build_partition_of_group group_nr attrs { partition & [attr] = group_nr }
		build_partition_of_group group_nr [] partition
			= partition

	adjust_coercions group_index [group : groups] partition coer_offered coer_demanded demanded
		# (combined_tree, coer_demanded) = combine_coercion_trees group_index group partition CT_Empty coer_offered coer_demanded
		= adjust_coercions (inc group_index) groups partition coer_offered coer_demanded { demanded & [ group_index ] = combined_tree }
	adjust_coercions group_index [] partition coer_offered coer_demanded demanded
		= demanded

	combine_coercion_trees group_index [ attr : attrs ] partition merged_tree coer_offered coer_demanded
		| isNonUnique coer_offered.[attr]
			= (CT_NonUnique, coer_demanded)
		# (next_tree, coer_demanded) = replace coer_demanded attr CT_Empty
		| isUnique next_tree
			= (CT_Unique, coer_demanded)
			# merged_tree = rebuild_tree group_index partition next_tree merged_tree
			= combine_coercion_trees group_index attrs partition merged_tree coer_offered coer_demanded
	combine_coercion_trees group_index [ ] partition merged_tree coer_offered coer_demanded
		= (merged_tree, coer_demanded)

	rebuild_tree :: !Index !AttributePartition !*CoercionTree !*CoercionTree -> *CoercionTree
	rebuild_tree group_index partition (CT_Node attr left right) tree
		# tree = rebuild_tree group_index partition left tree
		  tree = rebuild_tree group_index partition right tree
		#! attr_nr = partition.[attr]
		| attr_nr == group_index
			= tree
		# { tree } = insert partition.[attr] tree
		= tree	
	where
		insert ::  !Int !*CoercionTree -> *CoercionTreeRecord
		insert new_attr CT_Empty
			=  { tree = CT_Node new_attr CT_Empty CT_Empty }
		insert new_attr (CT_Node this_attr ct_less ct_greater)
			| new_attr < this_attr
				# { tree } = insert new_attr ct_less
				= { tree = CT_Node this_attr tree ct_greater }
			| new_attr > this_attr
				# { tree } = insert new_attr ct_greater
				= { tree = CT_Node this_attr ct_less tree }
				= { tree = CT_Node this_attr ct_less ct_greater }
	rebuild_tree group_index partition empty_tree tree
		= tree

::	CoercionTreeRecord = { tree :: !.CoercionTree }

liftSubstitution :: !*{! Type} !{# CommonDefs } !{# BOOLVECT }  !Int !*TypeHeaps !*TypeDefInfos -> (*{! Type}, !Int, !*TypeHeaps, !*TypeDefInfos)
liftSubstitution subst modules cons_vars attr_store type_heaps td_infos 
	# ls = { ls_next_attr = attr_store, ls_td_infos = td_infos, ls_type_heaps = type_heaps}
	= lift_substitution 0 modules cons_vars subst ls
where
	lift_substitution var_index modules cons_vars subst ls
		| var_index < size subst
			# (type, subst) = subst![var_index]
			# (_, type, subst, ls) = lift modules cons_vars type subst ls
			= lift_substitution (inc var_index) modules cons_vars { subst & [var_index] = type } ls
			= (subst, ls.ls_next_attr, ls.ls_type_heaps, ls.ls_td_infos)

adjustSignClass :: !SignClassification !Int -> SignClassification
adjustSignClass {sc_pos_vect,sc_neg_vect} arity
	= { sc_pos_vect = sc_pos_vect >> arity, sc_neg_vect = sc_neg_vect >> arity }

//adjustPropClass :: !PropClassification !Int -> PropClassification
adjustPropClass prop_class arity :== prop_class >> arity

::	LiftState = 
	{	ls_next_attr		:: !Int
	,	ls_type_heaps		:: !.TypeHeaps
	,	ls_td_infos			:: !.TypeDefInfos
	}

liftTempTypeVariable :: !{# CommonDefs } !{# BOOLVECT } !TempVarId !*{! Type} !*LiftState
	-> (!Bool, !Type, !*{! Type}, !*LiftState)
liftTempTypeVariable modules cons_vars tv_number subst ls
	# (type, subst) = subst![tv_number]
	= case type of
		TE
			-> (False, TempV tv_number, subst, ls)
		TLiftedSubst type
			# (_, type, subst, ls) = lift modules cons_vars type subst ls
			-> (True, type, subst, ls)
		_
			# (_, type, subst, ls) = lift modules cons_vars type subst ls
			-> (True, type, subst, ls)

typeIsNonCoercible _ (TempV _)
	= True
typeIsNonCoercible _ (TempQV _)
	= True
typeIsNonCoercible _ (TempQDV _)
	= True
typeIsNonCoercible _ (_ --> _)
	= True
typeIsNonCoercible _ TArrow
	= True
typeIsNonCoercible _ (TArrow1 t)
	= True	
typeIsNonCoercible cons_vars (TempCV tmp_var_id :@: _)
	= not (isPositive tmp_var_id cons_vars)
typeIsNonCoercible cons_vars (_ :@: _)
	= True
typeIsNonCoercible _ _
	= False

class lift a :: !{#CommonDefs} !{#BOOLVECT} !a !*{!Type} !*LiftState -> (!Bool,!a, !*{! Type}, !*LiftState)

liftTypeApplication modules cons_vars t0=:(TA cons_id=:{type_ident,type_index={glob_object,glob_module},type_arity,type_prop=type_prop0} cons_args) subst ls
	# ({tdi_kinds}, ls) = ls!ls_td_infos.[glob_module].[glob_object]
	# (changed,cons_args, sign_classes, prop_classes, subst, ls=:{ls_type_heaps}) = lift_list modules cons_vars cons_args tdi_kinds subst ls
	| changed
		# (type_prop, th_vars, ls_td_infos) = typeProperties glob_object glob_module sign_classes prop_classes modules ls_type_heaps.th_vars ls.ls_td_infos
		  ls = { ls & ls_td_infos = ls_td_infos, ls_type_heaps = {ls_type_heaps & th_vars = th_vars}}
		| equal_type_prop type_prop type_prop0
			= (True, TA cons_id cons_args, subst, ls)
			= (True, TA { cons_id & type_prop = type_prop } cons_args, subst, ls)
		# (type_prop, th_vars, ls_td_infos) = typeProperties glob_object glob_module sign_classes prop_classes modules ls_type_heaps.th_vars ls.ls_td_infos
		  ls = { ls & ls_td_infos = ls_td_infos, ls_type_heaps = {ls_type_heaps & th_vars = th_vars}}
		| equal_type_prop type_prop type_prop0
			= (False, t0, subst, ls)
			= (True, TA { cons_id & type_prop = type_prop } cons_args, subst, ls)
liftTypeApplication modules cons_vars t0=:(TAS cons_id=:{type_ident,type_index={glob_object,glob_module},type_arity,type_prop=type_prop0} cons_args strictness) subst ls
	# ({tdi_kinds}, ls) = ls!ls_td_infos.[glob_module].[glob_object]
	# (changed,cons_args, sign_classes, prop_classes, subst, ls=:{ls_type_heaps}) = lift_list modules cons_vars cons_args tdi_kinds subst ls
	| changed
		# (type_prop, th_vars, ls_td_infos) = typeProperties glob_object glob_module sign_classes prop_classes modules ls_type_heaps.th_vars ls.ls_td_infos
		  ls = { ls & ls_td_infos = ls_td_infos, ls_type_heaps = {ls_type_heaps & th_vars = th_vars}}
		| equal_type_prop type_prop type_prop0
			= (True, TAS cons_id cons_args strictness, subst, ls)
			= (True, TAS { cons_id & type_prop = type_prop } cons_args strictness, subst, ls)
		# (type_prop, th_vars, ls_td_infos) = typeProperties glob_object glob_module sign_classes prop_classes modules ls_type_heaps.th_vars ls.ls_td_infos
		  ls = { ls & ls_td_infos = ls_td_infos, ls_type_heaps = {ls_type_heaps & th_vars = th_vars}}
		| equal_type_prop type_prop type_prop0
			= (False, t0, subst, ls)
			= (True, TAS { cons_id & type_prop = type_prop } cons_args strictness, subst, ls)
liftTypeApplication modules cons_vars type subst ls
	= lift modules cons_vars type subst ls

lift_list :: !{#CommonDefs} !{# BOOLVECT } ![AType] ![TypeKind] !*{!Type} !*LiftState
	-> (!Bool,![AType], ![SignClassification], ![PropClassification], !*{!Type}, !*LiftState)
lift_list modules cons_vars [] _ subst ls
	= (False, [], [], [], subst, ls)
lift_list modules cons_vars ts0=:[t0:ts] [tk : tks] subst ls
	# (changed, t, subst, ls) = lift modules cons_vars t0 subst ls
	| changed
		# (_, ts, sign_classes, prop_classes, subst, ls) = lift_list modules cons_vars ts tks subst ls
		| IsArrowKind tk
			# (sign_classes,prop_classes) = add_sign_and_prop_of_arrow_kind_in_lift t.at_type cons_vars sign_classes prop_classes
			= (True,[t:ts],sign_classes,prop_classes,subst,ls)
			= (True,[t:ts],sign_classes,prop_classes,subst,ls)
		# (changed, ts, sign_classes, prop_classes, subst, ls) = lift_list modules cons_vars ts tks subst ls
		| changed
			| IsArrowKind tk
				# (sign_classes,prop_classes) = add_sign_and_prop_of_arrow_kind_in_lift t.at_type cons_vars sign_classes prop_classes
				= (True, [t0:ts], sign_classes,prop_classes, subst, ls)
				= (True, [t:ts], sign_classes, prop_classes, subst, ls)
			| IsArrowKind tk
				# (sign_classes,prop_classes) = add_sign_and_prop_of_arrow_kind_in_lift t.at_type cons_vars sign_classes prop_classes
				= (False, ts0, sign_classes, prop_classes, subst, ls)
				= (False, ts0, sign_classes, prop_classes, subst, ls)

add_sign_and_prop_of_arrow_kind_in_lift (TA {type_arity,type_prop} _) cons_vars sign_classes prop_classes
	= ([adjustSignClass type_prop.tsp_sign type_arity : sign_classes], [adjustPropClass type_prop.tsp_propagation type_arity : prop_classes])
add_sign_and_prop_of_arrow_kind_in_lift (TAS {type_arity,type_prop} _ _) cons_vars sign_classes prop_classes
	= ([adjustSignClass type_prop.tsp_sign type_arity : sign_classes], [adjustPropClass type_prop.tsp_propagation type_arity : prop_classes])
add_sign_and_prop_of_arrow_kind_in_lift (TempV tmp_var_id) cons_vars sign_classes prop_classes
	| isPositive tmp_var_id cons_vars
		= ([PostiveSignClass : sign_classes], [PropClass : prop_classes])
		= ([TopSignClass : sign_classes], [NoPropClass : prop_classes])
add_sign_and_prop_of_arrow_kind_in_lift _ cons_vars sign_classes prop_classes
	= ([TopSignClass : sign_classes], [PropClass : prop_classes])

instance lift Type
where
	lift modules cons_vars (TempV temp_var) subst ls
		= liftTempTypeVariable modules cons_vars temp_var subst ls
	lift modules cons_vars type=:(arg_type0 --> res_type0) subst ls
		# (changed, arg_type, subst, ls) = lift modules cons_vars arg_type0 subst ls
		| changed
			# (changed, res_type, subst, ls) = lift modules cons_vars res_type0 subst ls
			| changed
				= (True, arg_type --> res_type, subst, ls)
				= (True, arg_type --> res_type0, subst, ls)
			# (changed, res_type, subst, ls) = lift modules cons_vars res_type0 subst ls
			| changed
				= (True, arg_type0 --> res_type, subst, ls)
				= (False, type, subst, ls)
	lift modules cons_vars type=:(TA cons_id cons_args) subst ls=:{ls_type_heaps}
		# (_, type, ls_type_heaps) = tryToExpand type TA_Multi modules ls_type_heaps
		= liftTypeApplication modules cons_vars type subst {ls & ls_type_heaps = ls_type_heaps}
	lift modules cons_vars type=:(TAS cons_id cons_args _) subst ls=:{ls_type_heaps}
		# (_, type, ls_type_heaps) = tryToExpand type TA_Multi modules ls_type_heaps
		= liftTypeApplication modules cons_vars type subst {ls & ls_type_heaps = ls_type_heaps}			
	lift modules cons_vars type=:(TArrow1 arg_type) subst ls
		# (changed, arg_type, subst, ls) = lift modules cons_vars arg_type subst ls
		| changed 
			= (True, TArrow1 arg_type, subst, ls)
			= (False, type, subst, ls)
	lift modules cons_vars type=:(TempCV temp_var :@: types) subst ls
		# (changed, var_type, subst, ls) = liftTempTypeVariable modules cons_vars temp_var subst ls
		  (changed_types, types, subst, ls) = lift_list modules cons_vars types subst ls
		| changed || changed_types
			= case var_type of
				TA type_cons cons_args
					-> (True, TA { type_cons & type_arity = type_cons.type_arity + length types } (cons_args ++ types), subst, ls)
				TAS type_cons cons_args strictness
					-> (True, TAS { type_cons & type_arity = type_cons.type_arity + length types } (cons_args ++ types) strictness, subst, ls)
				TempV tv_number
					-> (True, TempCV tv_number :@: types, subst, ls)
				TempQV tv_number
					-> (True, TempQCV tv_number :@: types, subst, ls)
				TempQDV tv_number
					-> (True, TempQCDV tv_number :@: types, subst, ls)
				cons_var :@: cv_types
					-> (True, cons_var :@: (cv_types ++ types), subst, ls)
				TArrow -> case types of
					[t1, t2] 	-> (True, t1 --> t2, subst, ls)
					[t1]		-> (True, TArrow1 t1, subst, ls)
					_			-> (False, type, subst, ls)	
				(TArrow1 t1) -> case types of
					[t2]		-> (True, t1 --> t2, subst, ls)
					_			-> (False, type, subst, ls)				
			= (False, type, subst, ls)
		where
			lift_list :: !{#CommonDefs} !{# BOOLVECT } ![a] !*{!Type} !*LiftState -> (!Bool,![a], !*{!Type}, !*LiftState) | lift a
			lift_list modules cons_vars [] subst ls
				= (False, [], subst, ls)
			lift_list modules cons_vars ts0=:[t0:ts] subst ls
				# (changed,t, subst, ls) = lift modules cons_vars t0 subst ls
				| changed
					# (_, ts, subst, ls) = lift_list modules cons_vars ts subst ls
					= (True,[t:ts], subst, ls)
					# (changed, ts, subst, ls) = lift_list modules cons_vars ts subst ls
					| changed
						= (True, [t0:ts], subst, ls)
						= (False, ts0, subst, ls)
	lift modules cons_vars (TFA vars type) subst ls
		= abort "lift (TFA) (unitype.icl)"
	lift modules cons_vars type subst ls
		= (False, type, subst, ls)

instance lift AType
where
	lift modules cons_vars attr_type=:{at_attribute,at_type} subst ls
		# (changed, at_type, subst, ls) = lift modules cons_vars at_type subst ls
		| changed
			| typeIsNonCoercible cons_vars at_type
				= (True, {attr_type & at_type = at_type },subst, ls)
				= (True, {attr_type & at_attribute = TA_TempVar ls.ls_next_attr, at_type = at_type}, subst, {ls & ls_next_attr = inc ls.ls_next_attr})
			| typeIsNonCoercible cons_vars at_type
				= (False, attr_type,subst, ls)
				= (True, {attr_type & at_attribute = TA_TempVar ls.ls_next_attr}, subst, {ls & ls_next_attr = inc ls.ls_next_attr})

::	ExpansionState = 
	{	es_type_heaps	:: !.TypeHeaps
	,	es_td_infos		:: !.TypeDefInfos
	}

class expandType a :: !{# CommonDefs } !{# BOOLVECT } !a !*(!u:{! Type}, !*ExpansionState) -> (!Bool,!a, !*(!u:{! Type}, !*ExpansionState))

instance expandType AType
where
	expandType modules cons_vars attr_type=:{at_type, at_attribute} (subst, es=:{es_type_heaps})
		# (changed, at_attribute, th_attrs) = expand_attribute at_attribute es_type_heaps.th_attrs
		| changed
			# (_, at_type, subst_and_es) = expandType modules cons_vars at_type (subst, {es & es_type_heaps = { es_type_heaps & th_attrs = th_attrs }})
			= (True, { attr_type & at_type = at_type, at_attribute = at_attribute }, subst_and_es)
			# (changed, at_type, subst_and_es) = expandType modules cons_vars at_type (subst, {es & es_type_heaps = { es_type_heaps & th_attrs = th_attrs }})
			| changed
				= (True, { attr_type & at_type = at_type }, subst_and_es)
				= (False, attr_type, subst_and_es)
	where
		expand_attribute :: TypeAttribute *(Heap AttrVarInfo) -> (!.Bool,TypeAttribute,!.Heap AttrVarInfo);
		expand_attribute (TA_Var {av_ident,av_info_ptr}) attr_var_heap
			= case (readPtr av_info_ptr attr_var_heap) of
				(AVI_Attr attr, attr_var_heap)
					-> (True, attr, attr_var_heap)
				(info, attr_var_heap)
					-> abort ("expand_attribute (unitype.icl)" )//---> (av_ident <<- info ))
		expand_attribute attr attr_var_heap
			= (False, attr, attr_var_heap)

expandTempTypeVariable :: !TempVarId !*(!u:{! Type}, !*ExpansionState) -> (!Bool, !Type, !*(!u:{! Type}, !*ExpansionState))
expandTempTypeVariable tv_number (subst, es)
	# (type, subst) = subst![tv_number]
	= case type of
		TE
			-> (False, TempV tv_number, (subst, es))
		TLiftedSubst type
			-> (True, type, (subst, es))
		_
			-> (True, type, (subst, es))

IsArrowKind (KindArrow _) = True
IsArrowKind _ = False

equal_type_prop {tsp_sign=sign0,tsp_propagation=prop0,tsp_coercible=coerc0}
				{tsp_sign=sign1,tsp_propagation=prop1,tsp_coercible=coerc1}
	= prop0==prop1 && coerc0==coerc1 && sign0.sc_pos_vect==sign1.sc_pos_vect && sign0.sc_neg_vect==sign1.sc_neg_vect

instance expandType Type
where
	expandType modules cons_vars (TempV tv_number) est
		= expandTempTypeVariable tv_number est
	expandType modules cons_vars (TV {tv_info_ptr}) (subst, es=:{es_type_heaps})
		# (TVI_Type type, th_vars) = readPtr tv_info_ptr es_type_heaps.th_vars
		= (True,type, (subst, {es & es_type_heaps = {es_type_heaps & th_vars = th_vars}}))
	expandType modules cons_vars t0=:(arg_type0 --> res_type0) es
		# (changed,arg_type, es) = expandType modules cons_vars arg_type0 es
		| changed
			# (changed,res_type, es) = expandType modules cons_vars res_type0 es
			| changed
				= (True,arg_type --> res_type, es)
				= (True,arg_type --> res_type0, es)
			# (changed,res_type, es) = expandType modules cons_vars res_type0 es
			| changed
				= (True,arg_type0 --> res_type, es)
				= (False,t0, es)
	expandType modules cons_vars t0=:(TA cons_id=:{type_ident, type_index={glob_object,glob_module},type_prop=type_prop0} cons_args) (subst, es)
		# ({tdi_kinds}, es) = es!es_td_infos.[glob_module].[glob_object]
		  (changed,cons_args, hio_signs, hio_props, (subst,es=:{es_td_infos,es_type_heaps})) = expand_type_list modules cons_vars cons_args tdi_kinds (subst, es)
		| changed
		  	# (type_prop, th_vars, es_td_infos) = typeProperties glob_object glob_module hio_signs hio_props modules es_type_heaps.th_vars es_td_infos
			| equal_type_prop type_prop type_prop0
			  	# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (True,TA cons_id cons_args, (subst, es))
			  	# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (True,TA { cons_id & type_prop = type_prop } cons_args, (subst, es))
		  	# (type_prop, th_vars, es_td_infos) = typeProperties glob_object glob_module hio_signs hio_props modules es_type_heaps.th_vars es_td_infos
			| equal_type_prop type_prop type_prop0
			  	# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (False,t0, (subst, es))
			  	# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (True,TA { cons_id & type_prop = type_prop } cons_args, (subst, es))
	expandType modules cons_vars t0=:(TAS cons_id=:{type_ident, type_index={glob_object,glob_module},type_prop=type_prop0} cons_args strictness) (subst, es)
		# ({tdi_kinds}, es) = es!es_td_infos.[glob_module].[glob_object]
		  (changed,cons_args, hio_signs, hio_props, (subst,es=:{es_td_infos,es_type_heaps})) = expand_type_list modules cons_vars cons_args tdi_kinds (subst, es)
		| changed
		 	# (type_prop, th_vars, es_td_infos) = typeProperties glob_object glob_module hio_signs hio_props modules es_type_heaps.th_vars es_td_infos
			| equal_type_prop type_prop type_prop0
			  	# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (True,TAS cons_id cons_args strictness, (subst, es))
			  	# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (True,TAS { cons_id & type_prop = type_prop } cons_args strictness, (subst, es))
			# (type_prop, th_vars, es_td_infos) = typeProperties glob_object glob_module hio_signs hio_props modules es_type_heaps.th_vars es_td_infos
			| equal_type_prop type_prop type_prop0
		 		# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (False,t0, (subst, es))
				# es = { es & es_td_infos = es_td_infos, es_type_heaps = { es_type_heaps & th_vars = th_vars }}
				= (True,TAS { cons_id & type_prop = type_prop } cons_args strictness, (subst, es))
	expandType modules cons_vars type=:(TArrow1 arg_type) es
		# (changed,arg_type, es) = expandType modules cons_vars arg_type es
		| changed
			= (True, TArrow1 arg_type, es)
			= (False, type, es)	
	expandType modules cons_vars type=:(TempCV temp_var :@: types) es
		# (changed_type, var_type, es) = expandTempTypeVariable temp_var es
		  (changed_types, types, es) = expandType modules cons_vars types es
		| changed_type || changed_types
			= case var_type of
				TA type_cons=:{type_arity} cons_args
					# nr_of_new_args = length types
					-> (True, TA { type_cons & type_arity = type_arity + nr_of_new_args } (cons_args ++ types), es)
				TAS type_cons=:{type_arity} cons_args strictness
					# nr_of_new_args = length types
					-> (True, TAS { type_cons & type_arity = type_arity + nr_of_new_args } (cons_args ++ types) strictness, es)
				TempV tv_number
					-> (True, TempCV tv_number :@: types, es)
				TempQV tv_number
					-> (True, TempQCV tv_number :@: types, es)
				TempQDV tv_number
					-> (True, TempQCDV tv_number :@: types, es)
				cons_var :@: cv_types
					-> (True, cons_var :@: (cv_types ++ types), es)
				TArrow -> case types of
					[t1, t2] 	-> (True, t1 --> t2, es)
					[t1]		-> (True, TArrow1 t1, es)
					_			-> (False, type, es)	
				(TArrow1 t1) -> case types of
					[t2]		-> (True, t1 --> t2, es)
					_			-> (False, type, es)				
			= (False, type, es)
	expandType modules cons_vars type es
		= (False, type, es)

expand_type_list ::  !{#CommonDefs} !{# BOOLVECT } ![AType] ![TypeKind] !(!u:{!Type}, !*ExpansionState)
	-> (!Bool,![AType], ![SignClassification], ![PropClassification], !(!u:{!Type}, !*ExpansionState))
expand_type_list modules cons_vars [] _ es
	= (False,[], [], [], es)
expand_type_list modules cons_vars ts0=:[t0:ts] [tk : tks] es
	# (changed,t, es) = expandType modules cons_vars t0 es
	| changed
		# (_,ts, sign_classes, prop_classes, es) = expand_type_list modules cons_vars ts tks es
		| IsArrowKind tk
			# (sign_classes,prop_classes) = add_sign_and_prop_of_arrow_kind_in_expand t.at_type cons_vars sign_classes prop_classes;
			= (True,[t:ts], sign_classes, prop_classes, es)
			= (True,[t:ts], sign_classes, prop_classes, es)
		# (changed,ts, sign_classes, prop_classes, es) = expand_type_list modules cons_vars ts tks es
		| changed
			| IsArrowKind tk
				# (sign_classes,prop_classes) = add_sign_and_prop_of_arrow_kind_in_expand t.at_type cons_vars sign_classes prop_classes;
				= (True,[t0:ts], sign_classes, prop_classes, es)
				= (True,[t0:ts], sign_classes, prop_classes, es)
			| IsArrowKind tk
				# (sign_classes,prop_classes) = add_sign_and_prop_of_arrow_kind_in_expand t.at_type cons_vars sign_classes prop_classes;
				= (False,ts0, sign_classes, prop_classes, es)
				= (False,ts0, sign_classes, prop_classes, es)

add_sign_and_prop_of_arrow_kind_in_expand (TA {type_arity,type_prop} _) cons_vars sign_classes prop_classes
	=([adjustSignClass type_prop.tsp_sign type_arity : sign_classes],[adjustPropClass type_prop.tsp_propagation type_arity : prop_classes])
add_sign_and_prop_of_arrow_kind_in_expand (TAS {type_arity,type_prop} _ _) cons_vars sign_classes prop_classes
	=([adjustSignClass type_prop.tsp_sign type_arity : sign_classes],[adjustPropClass type_prop.tsp_propagation type_arity : prop_classes])
add_sign_and_prop_of_arrow_kind_in_expand (	TempV tmp_var_id) cons_vars sign_classes prop_classes
	| isPositive tmp_var_id cons_vars
		= ([PostiveSignClass : sign_classes], [PropClass : prop_classes])
		= ([TopSignClass : sign_classes], [NoPropClass : prop_classes])
add_sign_and_prop_of_arrow_kind_in_expand _ cons_vars sign_classes prop_classes
	= ([TopSignClass : sign_classes], [PropClass : prop_classes])

instance expandType [a] | expandType a
where
	expandType modules cons_vars [] es
		= (False,[],es)
	expandType modules cons_vars types0=:[type0:types] es
		# (changed, type, es) = expandType modules cons_vars type0 es
		| changed
			# (_, types, es) = expandType modules cons_vars types es
			= (True, [type:types], es)
			# (changed, types, es) = expandType modules cons_vars types es
			| changed
				= (True, [type0:types], es)
				= (False, types0, es)

instance toInt TypeAttribute
where
	 toInt TA_Unique 				= AttrUni
	 toInt (TA_TempVar av_number)	= av_number
	 toInt TA_Multi 				= AttrMulti
	 toInt TA_None 					= AttrMulti

expand_and_coerce_type :: !{#CommonDefs} !{#Int} !AType !*(!*Coercions,!*{!Type},!*TypeHeaps,!*TypeDefInfos)
											 -> (!AType,!*(!*Coercions,!*{!Type},!*TypeHeaps,!*TypeDefInfos))
expand_and_coerce_type common_defs cons_vars atype (coercions, subst, ti_type_heaps, ti_type_def_infos)
	# es = {es_type_heaps=ti_type_heaps, es_td_infos=ti_type_def_infos}
	  (_, btype, (subst, {es_type_heaps, es_td_infos}))
		= expandType common_defs cons_vars atype (subst, es)
	  cs = {crc_type_heaps=es_type_heaps, crc_coercions=coercions, crc_td_infos=es_td_infos}
	  ci = {ci_common_defs=common_defs, ci_cons_vars=cons_vars, ci_expand_newtypes=True}
	  (_, {crc_type_heaps,crc_coercions,crc_td_infos})
		= coerce PositiveSign ci [#!] btype btype cs
	= (btype, (crc_coercions, subst, crc_type_heaps, crc_td_infos))

::	CoercionState =
	{	crc_type_heaps	:: !.TypeHeaps
	,	crc_coercions	:: !.Coercions
	,	crc_td_infos	:: !.TypeDefInfos
	}

::	TypePosition :== [#Int!]

/*
'coerceAttributes offered_attribute offered_attribute sign coercions' coerce offered_attribute to
offered_attribute according to sign. Failure is indicated by returning False as a result.
*/

coerceAttributes :: !.TypeAttribute !.TypeAttribute !.Sign *Coercions -> (!Bool,.Coercions);
coerceAttributes TA_Unique dem_attr {neg_sign} coercions
	| not neg_sign
		= (True, coercions)
coerceAttributes off_attr TA_Unique {pos_sign} coercions
	| not pos_sign
		= (True, coercions)
coerceAttributes off_attr TA_Multi {neg_sign} coercions
	| not neg_sign
		= (True, coercions)
coerceAttributes TA_Multi dem_attr {pos_sign} coercions
	| not pos_sign
		= (True, coercions)
coerceAttributes (TA_TempVar av_number) dem_attr {neg_sign} coercions=:{coer_demanded}
	| not neg_sign && isUnique coer_demanded.[av_number]
		= (True, coercions)
coerceAttributes off_attr (TA_TempVar av_number) {pos_sign} coercions=:{coer_demanded}
	| not pos_sign && isUnique coer_demanded.[av_number]
		= (True, coercions)
coerceAttributes (TA_TempVar av_number1) (TA_TempVar av_number2) {pos_sign,neg_sign} coercions
	| av_number1 == av_number2
		= (True, coercions)
	| pos_sign
		| neg_sign
			# (ok, coercions) = new_inequality av_number1 av_number2 coercions
			| ok
				= new_inequality av_number2 av_number1 coercions
				= (False, coercions)
			= new_inequality av_number1 av_number2 coercions
	| neg_sign
		= new_inequality av_number2 av_number1 coercions
		= (True, coercions)
where
	new_inequality :: !Int !Int !*Coercions  -> (!Bool, !*Coercions)
	new_inequality off_attr dem_attr coercions=:{coer_demanded, coer_offered}
		| isNonUnique coer_offered.[off_attr]
			| isUnique coer_demanded.[dem_attr]
				= (False, coercions)
				= (True, makeNonUnique dem_attr coercions)
		| isUnique coer_demanded.[dem_attr]
			= (True, makeUnique off_attr coercions)
		| isNonUnique coer_offered.[dem_attr] || isUnique coer_demanded.[off_attr]
			= (True, coercions)
			= (True, newInequality off_attr dem_attr coercions)
coerceAttributes TA_Unique (TA_TempVar av_number) {neg_sign} coercions=:{coer_offered}
	| isNonUnique coer_offered.[av_number]
		= (False, coercions)
		= (True, makeUnique av_number coercions)// ---> "*** 1 ***"
coerceAttributes (TA_TempVar av_number) TA_Unique {pos_sign} coercions=:{coer_offered}
	| isNonUnique coer_offered.[av_number]
		= (False, coercions)
		= (True, makeUnique av_number coercions)// ---> "*** 2 ***"
coerceAttributes TA_Multi (TA_TempVar av_number) {pos_sign} coercions=:{coer_demanded}
	| isUnique coer_demanded.[av_number]
		= (False, coercions)
		= (True, makeNonUnique av_number coercions)
coerceAttributes (TA_TempVar av_number) TA_Multi {neg_sign} coercions=:{coer_demanded}
	| isUnique coer_demanded.[av_number]
		= (False, coercions)
		= (True, makeNonUnique av_number coercions)
coerceAttributes TA_Unique TA_Multi _ coercions
	= (False, coercions)
coerceAttributes TA_Multi TA_Unique _ coercions
	= (False, coercions)
coerceAttributes off_attr dem_attr _ coercions
	= (True, coercions)

newInequality :: !Int !Int !*Coercions -> *Coercions 
newInequality off_attr dem_attr coercions=:{coer_demanded, coer_offered}
	# (dem_coercions, coer_demanded) = coer_demanded![off_attr]
	  (succ, dem_coercions) = insert dem_attr dem_coercions
	  coer_demanded = {coer_demanded & [off_attr] = dem_coercions}
	| succ
		# (off_coercions, coer_offered) = coer_offered![dem_attr]
	  	  (succ, off_coercions) = insert off_attr off_coercions
	 	  coer_offered = {coer_offered & [dem_attr] = off_coercions}
		= {coer_demanded = coer_demanded, coer_offered = coer_offered}
		= {coer_demanded = coer_demanded, coer_offered = coer_offered}
where
	insert ::  !Int !*CoercionTree -> (!Bool, !*CoercionTree)
	insert new_attr CT_Empty
		=  (True, CT_Node new_attr CT_Empty CT_Empty)
	insert new_attr (CT_Node this_attr ct_less ct_greater)
		| new_attr < this_attr
			# (succ, ct_less) = insert new_attr ct_less
			= (succ, CT_Node this_attr ct_less ct_greater)
		| new_attr > this_attr
			# (succ, ct_greater) = insert new_attr ct_greater
			= (succ, CT_Node this_attr ct_less ct_greater)
			= (False, CT_Node this_attr ct_less ct_greater)

isNonUnique :: !CoercionTree -> Bool
isNonUnique CT_NonUnique	= True
isNonUnique _ 				= False

isUnique  :: !CoercionTree -> Bool
isUnique CT_Unique			= True
isUnique _					= False

isExistential :: !CoercionTree -> Bool
isExistential CT_Existential	= True
isExistential _					= False

isUniqueAttribute :: !Int !Coercions -> Bool
isUniqueAttribute attr_number {coer_demanded}
	= isUnique coer_demanded.[attr_number]

isNonUniqueAttribute :: !Int !Coercions -> Bool
isNonUniqueAttribute attr_number {coer_offered}
	= isNonUnique coer_offered.[attr_number]
		
makeUnique :: !Int !*Coercions -> *Coercions
makeUnique attr {coer_demanded, coer_offered}
	# (off_coercions, coer_offered) = replace coer_offered attr CT_Empty
	  coer_demanded = { coer_demanded & [attr] = CT_Unique }
	= make_unique off_coercions {coer_offered = coer_offered, coer_demanded = coer_demanded}
where
	make_unique :: !CoercionTree !*Coercions -> *Coercions;
	make_unique (CT_Node this_attr ct_less ct_greater) coercions
		# coercions = makeUnique this_attr coercions
		  coercions = make_unique ct_less coercions
		  coercions = make_unique ct_greater coercions
		= coercions
	make_unique tree coercions
		= coercions

tryToMakeUnique :: !Int !*Coercions -> (!Bool, !*Coercions)
tryToMakeUnique attr coercions=:{coer_offered}
	| isNonUnique coer_offered.[attr]
		= (False, coercions)
		= (True, makeUnique attr coercions)

makeNonUnique :: !Int !*Coercions -> *Coercions
makeNonUnique attr {coer_demanded, coer_offered}
	# (dem_coercions, coer_demanded) = replace coer_demanded attr CT_Empty
	  coer_offered = { coer_offered & [attr] = CT_NonUnique }
	= make_non_unique dem_coercions {coer_offered = coer_offered, coer_demanded = coer_demanded}
where
	make_non_unique :: !CoercionTree !*Coercions -> *Coercions;
	make_non_unique (CT_Node this_attr ct_less ct_greater) coercions
		# coercions = makeNonUnique this_attr coercions
		  coercions = make_non_unique ct_less coercions
		  coercions = make_non_unique ct_greater coercions
		= coercions
	make_non_unique tree coercions
		= coercions

tryToMakeNonUnique :: !Int !*Coercions -> (!Bool, !*Coercions)
tryToMakeNonUnique attr coercions=:{coer_demanded}
	| isUnique coer_demanded.[attr]
//		-?-> (s <= attr, ("tryToMakeNonUnique", s, attr))]
		= (False, coercions)
		= (True, makeNonUnique attr coercions)
//				---> ("tryToMakeNonUnique", attr)

Success suc :== suc=:No

coerce :: !Sign !CoerceInfo !TypePosition !AType !AType !*CoercionState -> (!Optional TypePosition, !*CoercionState)
coerce sign ci=:{ci_cons_vars} tpos at1=:{at_attribute=attr1, at_type = type1} at2=:{at_attribute=attr2} cs=:{crc_coercions}
	#!attr_sign = adjust_sign sign type1 ci_cons_vars
	  (succ, crc_coercions) = coerceAttributes attr1 attr2 attr_sign crc_coercions
	| succ
		# (succ, cs) = coerceTypes sign ci tpos at1 at2 { cs & crc_coercions = crc_coercions }
		| Success succ
			# (succ1, crc_coercions) = add_propagation_inequalities ci_cons_vars attr1 type1 cs.crc_coercions
			  (succ2, crc_coercions) = add_propagation_inequalities ci_cons_vars attr2 at2.at_type crc_coercions
			= (if (succ1 && succ2) No (Yes tpos), { cs & crc_coercions = crc_coercions })
			= (succ, cs)
		= (Yes tpos, { cs & crc_coercions = crc_coercions })
where
	adjust_sign :: !Sign !Type {# BOOLVECT} -> Sign
	adjust_sign sign (TempV _) cons_vars
		= TopSign
	adjust_sign sign (TempQV _) cons_vars
		= TopSign
	adjust_sign sign (TempQDV _) cons_vars
		= TopSign
	adjust_sign sign (_ --> _) cons_vars
		= TopSign
	adjust_sign sign (TA {type_ident, type_prop={tsp_coercible}} _) cons_vars
		| tsp_coercible
			= sign
			= TopSign
	adjust_sign sign (TAS {type_ident, type_prop={tsp_coercible}} _ _) cons_vars
		| tsp_coercible
			= sign
			= TopSign
	adjust_sign sign TArrow cons_vars
		= TopSign
	adjust_sign sign (TArrow1 _) cons_vars
		= TopSign					
	adjust_sign sign (TempCV tmp_var_id :@: _) cons_vars
		| isPositive tmp_var_id cons_vars
			= sign
			= TopSign
	adjust_sign sign (_ :@: _) cons_vars
		= TopSign
	adjust_sign sign _ cons_vars
		= sign

	add_propagation_inequalities cons_vars attr (TA {type_prop={tsp_propagation}} cons_args) coercions
		= add_inequalities_for_TA tsp_propagation attr cons_args coercions
	add_propagation_inequalities cons_vars attr (TAS {type_prop={tsp_propagation}} cons_args _) coercions
		= add_inequalities_for_TA tsp_propagation attr cons_args coercions
	add_propagation_inequalities cons_vars attr (TempCV tmp_var_id :@: types) coercions
		| isPositive tmp_var_id cons_vars
			= add_inequalities attr types coercions
			= (True, coercions)
	where
		add_inequalities attr [] coercions
			= (True, coercions)
		add_inequalities attr [{at_attribute} : args] coercions
			# (succ, coercions) = coerceAttributes attr at_attribute PositiveSign coercions
			| succ
				= add_inequalities attr args coercions
				= (False, coercions)
	add_propagation_inequalities cons_vars attr type coercions
			= (True, coercions)

	add_inequalities_for_TA prop_class attr [] coercions
		= (True, coercions)
	add_inequalities_for_TA prop_class attr [{at_attribute} : args] coercions
		| (prop_class bitand 1) == 0
			= add_inequalities_for_TA (prop_class >> 1) attr args coercions
		# (succ, coercions) = coerceAttributes attr at_attribute PositiveSign coercions
		| succ
			= add_inequalities_for_TA (prop_class >> 1) attr args coercions
			= (False, coercions)

coerceTypes :: !Sign !CoerceInfo !TypePosition !AType !AType !*CoercionState -> (!Optional TypePosition, !*CoercionState)		
coerceTypes sign ci tpos dem_type=:{at_type=type1=:TA dem_cons dem_args} off_type=:{at_type=type2=:TA off_cons off_args} cs=:{crc_type_heaps, crc_td_infos}
	| dem_cons == off_cons
		= coercions_of_arg_types sign ci tpos dem_args off_args dem_cons.type_prop.tsp_sign 0 cs
		# (_, exp_dem_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type1 dem_cons dem_args dem_type.at_attribute crc_type_heaps crc_td_infos
		  (_, exp_off_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type2 off_cons off_args off_type.at_attribute crc_type_heaps crc_td_infos
		= coerceTypes sign ci tpos { dem_type & at_type = exp_dem_type } { off_type & at_type = exp_off_type }
							{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
coerceTypes sign ci tpos dem_type=:{at_type=type1=:TA dem_cons dem_args} off_type=:{at_type=type2=:TAS off_cons off_args _} cs=:{crc_type_heaps, crc_td_infos}
	| dem_cons == off_cons
		= coercions_of_arg_types sign ci tpos dem_args off_args dem_cons.type_prop.tsp_sign 0 cs
		# (_, exp_dem_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type1 dem_cons dem_args dem_type.at_attribute crc_type_heaps crc_td_infos
		  (_, exp_off_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type2 off_cons off_args off_type.at_attribute crc_type_heaps crc_td_infos
		= coerceTypes sign ci tpos { dem_type & at_type = exp_dem_type } { off_type & at_type = exp_off_type }
							{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
coerceTypes sign ci tpos dem_type=:{at_type=type1=:TAS dem_cons dem_args _} off_type=:{at_type=type2=:TA off_cons off_args} cs=:{crc_type_heaps, crc_td_infos}
	| dem_cons == off_cons
		= coercions_of_arg_types sign ci tpos dem_args off_args dem_cons.type_prop.tsp_sign 0 cs
		# (_, exp_dem_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type1 dem_cons dem_args dem_type.at_attribute crc_type_heaps crc_td_infos
		  (_, exp_off_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type2 off_cons off_args off_type.at_attribute crc_type_heaps crc_td_infos
		= coerceTypes sign ci tpos { dem_type & at_type = exp_dem_type } { off_type & at_type = exp_off_type }
							{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
coerceTypes sign ci tpos dem_type=:{at_type=type1=:TAS dem_cons dem_args _} off_type=:{at_type=type2=:TAS off_cons off_args _} cs=:{crc_type_heaps, crc_td_infos}
	| dem_cons == off_cons
		= coercions_of_arg_types sign ci tpos dem_args off_args dem_cons.type_prop.tsp_sign 0 cs
		# (_, exp_dem_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type1 dem_cons dem_args dem_type.at_attribute crc_type_heaps crc_td_infos
		  (_, exp_off_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type2 off_cons off_args off_type.at_attribute crc_type_heaps crc_td_infos
		= coerceTypes sign ci tpos { dem_type & at_type = exp_dem_type } { off_type & at_type = exp_off_type }
							{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
coerceTypes sign ci tpos dem_type=:{at_type=type=:TA dem_cons dem_args} off_type cs=:{crc_type_heaps, crc_td_infos}
	# (succ, exp_dem_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type dem_cons dem_args dem_type.at_attribute crc_type_heaps crc_td_infos
	| succ
		= coerceTypes sign ci tpos { dem_type & at_type = exp_dem_type } off_type
					{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
		= (No, { cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos })
coerceTypes sign ci tpos dem_type=:{at_type=type=:TAS dem_cons dem_args _} off_type cs=:{crc_type_heaps, crc_td_infos}
	# (succ, exp_dem_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type dem_cons dem_args dem_type.at_attribute crc_type_heaps crc_td_infos
	| succ
		= coerceTypes sign ci tpos { dem_type & at_type = exp_dem_type } off_type
					{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
		= (No, { cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos })
coerceTypes sign ci tpos dem_type off_type=:{at_type=type=:TAS off_cons off_args _} cs=:{crc_type_heaps, crc_td_infos}
	# (succ, exp_off_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type off_cons off_args off_type.at_attribute crc_type_heaps crc_td_infos
	| succ
		= coerceTypes sign ci tpos dem_type { off_type & at_type = exp_off_type }
					{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
		= (No, { cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos })
coerceTypes sign ci tpos dem_type off_type=:{at_type=type=:TA off_cons off_args} cs=:{crc_type_heaps, crc_td_infos}
	# (succ, exp_off_type, crc_type_heaps, crc_td_infos) = tryToExpandTypeSyn ci type off_cons off_args off_type.at_attribute crc_type_heaps crc_td_infos
	| succ
		= coerceTypes sign ci tpos dem_type { off_type & at_type = exp_off_type }
					{ cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos }
		= (No, { cs & crc_type_heaps = crc_type_heaps, crc_td_infos = crc_td_infos })
coerceTypes sign ci tpos {at_type = arg_type1 --> res_type1} {at_type = arg_type2 --> res_type2} cs
	# arg_sign = NegativeSign * sign
	# (succ, cs) = coerce arg_sign ci [#0 : tpos!] arg_type1 arg_type2  cs
	| Success succ
		= coerce sign ci [#1 : tpos!] res_type1 res_type2 cs
		= (succ, cs)
coerceTypes sign ci tpos {at_type = TArrow} {at_type = TArrow} cs
	= (No, cs) // ???
coerceTypes sign ci tpos {at_type = TArrow1 arg_type1} {at_type = TArrow1 arg_type2} cs
	# arg_sign = NegativeSign * sign
	= coerce arg_sign ci [#0 : tpos!] arg_type1 arg_type2 cs
coerceTypes sign ci tpos {at_type = cons_var :@: types1} {at_type = _ :@: types2}  cs
	# sign = determine_sign_of_arg_types sign cons_var ci
	= coercions_of_type_list sign ci tpos 0 types1 types2 cs
where
	determine_sign_of_arg_types sign (TempCV tmp_var_id) {ci_cons_vars}
		| isPositive tmp_var_id ci_cons_vars
			= sign
			= TopSign
	determine_sign_of_arg_types _ _ ci
			= TopSign
		
	coercions_of_type_list sign ci tpos arg_number [t1 : ts1] [t2 : ts2] cs
		# (succ, cs) = coerce sign ci [#arg_number : tpos!] t1 t2 cs
		| Success succ
			= coercions_of_type_list sign ci tpos (inc arg_number) ts1 ts2 cs
			= (succ, cs)
	coercions_of_type_list sign ci tpos arg_number [] [] cs
		= (No, cs)
coerceTypes sign ci tpos _ _  cs
	= (No, cs)

coercions_of_arg_types :: Sign CoerceInfo !TypePosition [AType] [AType] SignClassification !Int *CoercionState -> (Optional TypePosition,*CoercionState)
coercions_of_arg_types sign ci tpos [t1 : ts1] [t2 : ts2] sign_class arg_number cs
	# arg_sign = sign * signClassToSign sign_class arg_number
	  (succ, cs) = coerce arg_sign ci [#arg_number : tpos!] t1 t2 cs
	| Success succ
		= coercions_of_arg_types sign ci tpos ts1 ts2 sign_class (inc arg_number) cs
		= (succ, cs)
coercions_of_arg_types sign ci tpos [] [] _ _ cs
	= (No, cs)

tryToExpandTypeSyn :: !CoerceInfo !Type !TypeSymbIdent ![AType] !TypeAttribute !*TypeHeaps !*TypeDefInfos
	-> (!Bool, !Type, !*TypeHeaps, !*TypeDefInfos)
tryToExpandTypeSyn {ci_common_defs,ci_cons_vars,ci_expand_newtypes} type {type_index={glob_object,glob_module}} type_args attribute type_heaps td_infos
	# {td_rhs,td_args,td_attribute,td_ident} = ci_common_defs.[glob_module].com_type_defs.[glob_object]
	= case td_rhs of
		SynType {at_type}
			# type_heaps = bindTypeVarsAndAttributes td_attribute attribute td_args type_args type_heaps
			  (_, expanded_type, (_, {es_type_heaps, es_td_infos})) = expandType ci_common_defs ci_cons_vars at_type
			  		({}, { es_type_heaps = type_heaps, es_td_infos = td_infos })
			-> (True, expanded_type, clearBindingsOfTypeVarsAndAttributes attribute td_args es_type_heaps, es_td_infos)
		NewType {ds_index}
			| ci_expand_newtypes
				# {cons_type={st_args=[{at_type}:_]}} = ci_common_defs.[glob_module].com_cons_defs.[ds_index];
				  type_heaps = bindTypeVarsAndAttributes td_attribute attribute td_args type_args type_heaps
				  (_, expanded_type, (_, {es_type_heaps, es_td_infos})) = expandType ci_common_defs ci_cons_vars at_type
				  		({}, { es_type_heaps = type_heaps, es_td_infos = td_infos })
				-> (True, expanded_type, clearBindingsOfTypeVarsAndAttributes attribute td_args es_type_heaps, es_td_infos)
		_
			-> (False, type, type_heaps, td_infos)

AttrRestricted :== 0

instance <<< CoercionTree
where
	(<<<) file (CT_Node attr left right) = file <<< left <<< ' ' <<< attr <<< ' ' <<< right
	(<<<) file CT_Unique = file <<< "CT_Unique"
	(<<<) file CT_NonUnique = file <<< "CT_NonUnique"
	(<<<) file CT_Empty = file <<< "##"

set_bit :: !Int !*{# BOOLVECT} -> .{# BOOLVECT}
set_bit var_number bitvects
	# bit_index = BITINDEX var_number
	  (prev_vect, bitvects) = bitvects![bit_index]
	= { bitvects & [bit_index] = prev_vect bitor (1 << BITNUMBER var_number) }

checkExistentionalAttributeVars :: [TempAttrId] !AttributePartition !*{! CoercionTree} -> (!Bool,!*{! CoercionTree})
checkExistentionalAttributeVars tmp_attr_vars partition coercions
	= foldSt (check_existentional_attribute_var partition) tmp_attr_vars (True, coercions)
where
	check_existentional_attribute_var partition tmp_attr (ok, coercions)
		# av_group_nr = partition.[tmp_attr]
		  (coercion_tree,coercions) = coercions![av_group_nr]
		= check_demanded_attribute_vars av_group_nr coercion_tree partition (ok, coercions)
	
	check_demanded_attribute_vars av_group_nr (CT_Node dem_attr left right) partition (ok, coercions)
		# (ok, coercions) = check_existentional_attribute_var partition dem_attr (ok, { coercions & [av_group_nr] = CT_Existential })
		| ok
			# ok_coercions = check_demanded_attribute_vars av_group_nr left partition (True, coercions)
			= check_demanded_attribute_vars av_group_nr right partition ok_coercions
			= (False, coercions)
	check_demanded_attribute_vars av_group_nr CT_Empty partition ok_coercions
		= ok_coercions
	check_demanded_attribute_vars av_group_nr _ partition (ok, coercions)
		= (False, coercions)

copyCoercions :: *Coercions -> (*Coercions, *Coercions)
copyCoercions coercions=:{coer_demanded, coer_offered}
	# (coer_demanded_copy, coer_demanded) = copy_coercion_trees coer_demanded
	# (coer_offered_copy, coer_offered) = copy_coercion_trees coer_offered
	= ({coercions & coer_demanded = coer_demanded, coer_offered = coer_offered}, {coercions & coer_demanded = coer_demanded_copy, coer_offered = coer_offered_copy})
where
	copy_coercion_trees trees
		= arrayAndElementsCopy copy_coercion_tree trees

	copy_coercion_tree (CT_Node attr left right)
		# (copy_left, left) = copy_coercion_tree left
		# (copy_right, right) = copy_coercion_tree right
		= (CT_Node attr copy_left copy_right, CT_Node attr left right)
	copy_coercion_tree tree=:CT_Empty
		= (CT_Empty, tree)
	copy_coercion_tree tree=:CT_Unique
		= (CT_Unique, tree)
	copy_coercion_tree tree=:CT_NonUnique
		= (CT_NonUnique, tree)
	copy_coercion_tree tree=:CT_Existential
		= (CT_Existential, tree)

uniquenessErrorVar :: !FreeVar !FunctionBody !String !*ErrorAdmin -> *ErrorAdmin
uniquenessErrorVar free_var=:{fv_info_ptr} (TransformedBody {tb_args,tb_rhs}) mess err
	| var_in_free_vars fv_info_ptr tb_args
		= uniquenessError (CP_Expression (FreeVar free_var)) mess err
	# position = find_var_position_in_expression fv_info_ptr tb_rhs
	= case position of
		LinePos file_name line_n
			# ea_file = err.ea_file <<< "Uniqueness error " <<< {ip_file=file_name,ip_line=line_n,ip_ident=free_var.fv_ident} <<< '\"' <<< mess <<< '\n'
			-> { err & ea_file = ea_file, ea_ok = False}
		FunPos file_name line_n fun_name
			# ea_file = err.ea_file <<< "Uniqueness error " <<< {ip_file=file_name,ip_line=line_n,ip_ident=free_var.fv_ident} <<< '\"' <<< mess <<< '\n'
			-> { err & ea_file = ea_file, ea_ok = False}			
		_
			-> uniquenessError (CP_Expression (FreeVar free_var)) mess err

uniquenessError :: !CoercionPosition !String !*ErrorAdmin -> *ErrorAdmin
uniquenessError position mess err=:{ea_file,ea_loc}
	# ea_file = ea_file <<< "Uniqueness error " <<< hd ea_loc <<< ": \"" <<<  position <<< '\"' <<< mess <<< '\n'
	= { err & ea_file = ea_file, ea_ok = False}

var_in_free_vars var_ptr []
	= False
var_in_free_vars var_ptr [{fv_info_ptr}:vars]
	= var_ptr==fv_info_ptr || var_in_free_vars var_ptr vars

find_var_position_in_expression var_ptr expr
	# (found,pos) = find_var_position_in_expression expr
	| found
		= pos
		= NoPos
where
	find_var_position_in_expression (App {app_args})
		= find_var_position_in_expressions app_args
	find_var_position_in_expression (f @ a)
		# (found,pos) = find_var_position_in_expression f
		| found
			= (True,pos)
		= find_var_position_in_expressions a
	find_var_position_in_expression (Selection _ expr selections)
		# (found,pos) = find_var_position_in_selections selections
		| found
			= (True,pos)
		= find_var_position_in_expression expr
	find_var_position_in_expression (TupleSelect _ _ expr)
		= find_var_position_in_expression expr
	find_var_position_in_expression (Update expr1 selections expr2)
		# (found,pos) = find_var_position_in_expression expr1
		| found
			= (True,pos)
		# (found,pos) = find_var_position_in_selections selections
		| found
			= (True,pos)
		= find_var_position_in_expression expr2
	find_var_position_in_expression (RecordUpdate _ expr updated_fields)
		# (found,pos) = find_var_position_in_updated_fields updated_fields
		| found
			= (True,pos)
		= find_var_position_in_expression expr
	where
		find_var_position_in_updated_fields [{bind_src}:updated_fields]
			# (found,pos) = find_var_position_in_expression bind_src
			| found
				= (True,pos)
			= find_var_position_in_updated_fields updated_fields
		find_var_position_in_updated_fields []
			= (False,NoPos)
	find_var_position_in_expression (MatchExpr _ expr)
		= find_var_position_in_expression expr
	find_var_position_in_expression (IsConstructor expr _ _ _ _ _)
		= find_var_position_in_expression expr
	find_var_position_in_expression (Let {let_strict_binds,let_lazy_binds,let_expr})
		# (found,pos) = find_var_position_in_let_binds let_strict_binds
		| found
			= (True,pos)
		# (found,pos) = find_var_position_in_let_binds let_lazy_binds
		| found
			= (True,pos)
		= find_var_position_in_expression let_expr
	where
		find_var_position_in_let_binds [{lb_dst={fv_info_ptr},lb_position}:let_binds]
			| var_ptr==fv_info_ptr
				= (True,lb_position)
				= find_var_position_in_let_binds let_binds
		find_var_position_in_let_binds []
			= (False,NoPos)
	find_var_position_in_expression (Case {case_expr,case_guards,case_default})
		# (found,pos) = find_var_position_in_expression	case_expr
		| found
			= (True,pos);
		# (found,pos) = find_var_position_in_case_guards case_guards
		| found
			= (True,pos);
		= find_var_position_in_case_default case_default
	where
		find_var_position_in_case_guards (AlgebraicPatterns _ algebraic_patterns)
			= find_var_position_in_algebraic_patterns algebraic_patterns
		find_var_position_in_case_guards (BasicPatterns _ basic_patterns)
			= find_var_position_in_basic_patterns basic_patterns
		where
			find_var_position_in_basic_patterns [{bp_expr}:basic_patterns]
				# (found,pos) = find_var_position_in_expression bp_expr
				| found
					= (True,pos)
				= find_var_position_in_basic_patterns basic_patterns
			find_var_position_in_basic_patterns []
				= (False,NoPos)
		find_var_position_in_case_guards (OverloadedListPatterns _ _ algebraic_patterns)
			= find_var_position_in_algebraic_patterns algebraic_patterns
		find_var_position_in_case_guards (NewTypePatterns _ algebraic_patterns)
			= find_var_position_in_algebraic_patterns algebraic_patterns
		find_var_position_in_case_guards (DynamicPatterns dynamic_patterns)
			= find_var_position_in_dynamic_patterns dynamic_patterns
		where
			find_var_position_in_dynamic_patterns [{dp_var,dp_rhs,dp_position}:dynamic_patterns]
				| var_ptr==dp_var.fv_info_ptr
					= (True,dp_position)
				# (found,pos) = find_var_position_in_expression dp_rhs
				| found
					= (True,pos)
				= find_var_position_in_dynamic_patterns dynamic_patterns
			find_var_position_in_dynamic_patterns []
				= (False,NoPos)
		find_var_position_in_case_guards NoPattern
			= (False,NoPos)

		find_var_position_in_algebraic_patterns [{ap_vars,ap_expr,ap_position}:algebraic_patterns]
			| var_in_free_vars var_ptr ap_vars
				= (True,ap_position);
			# (found,pos) = find_var_position_in_expression ap_expr
			| found
				= (True,pos)
			= find_var_position_in_algebraic_patterns algebraic_patterns
		find_var_position_in_algebraic_patterns []
			= (False,NoPos)

		find_var_position_in_case_default (Yes expr)
			= find_var_position_in_expression expr
		find_var_position_in_case_default No
			= (False,NoPos)
	find_var_position_in_expression (DynamicExpr {dyn_expr})
		= find_var_position_in_expression dyn_expr
	find_var_position_in_expression expr
		= (False,NoPos)

	find_var_position_in_expressions [expr:exprs]
		# (found,pos) = find_var_position_in_expression expr
		| found
			= (True,pos)
		= find_var_position_in_expressions exprs
	find_var_position_in_expressions []
		= (False,NoPos)

	find_var_position_in_selections [RecordSelection _ _:selections]
		= find_var_position_in_selections selections
	find_var_position_in_selections [ArraySelection _ _ expr:selections]
		# (found,pos) = find_var_position_in_expression expr
		| found
			= (True,pos)
		= find_var_position_in_selections selections
	find_var_position_in_selections [DictionarySelection _ d_selections _ expr:selections]
		# (found,pos) = find_var_position_in_expression expr
		| found
			= (True,pos)
		# (found,pos) = find_var_position_in_selections d_selections
		| found
			= (True,pos)
		= find_var_position_in_selections selections
	find_var_position_in_selections []
		= (False,NoPos)

liftOfferedSubstitutions :: !AType !AType !{#CommonDefs} !{#BOOLVECT} !Int !*{!Type} !*TypeDefInfos !*TypeHeaps
																  -> (!Int,!*{!Type},!*TypeDefInfos,!*TypeHeaps)
liftOfferedSubstitutions off_type dem_type common_defs cons_vars next_attr_n subst td_infos type_heaps
	# ls = {ls_next_attr = next_attr_n, ls_td_infos = td_infos, ls_type_heaps = type_heaps}
	# (subst,ls) = lift_offered_substitutions off_type dem_type common_defs cons_vars subst ls
	= (ls.ls_next_attr, subst, ls.ls_td_infos, ls.ls_type_heaps)

lift_offered_substitutions :: !AType !AType !{#CommonDefs} !{#BOOLVECT} !*{!Type} !*LiftState
																	-> (!*{!Type},!*LiftState)
lift_offered_substitutions {at_type=TA off_cons off_args} {at_type=TA {type_index,type_arity} dem_args} common_defs cons_vars subst ls
	= lift_offered_substitutions_type_application off_cons.type_index off_args type_index dem_args type_arity common_defs cons_vars subst ls
lift_offered_substitutions {at_type=TA off_cons off_args} {at_type=TAS {type_index,type_arity} dem_args _} common_defs cons_vars subst ls
	= lift_offered_substitutions_type_application off_cons.type_index off_args type_index dem_args type_arity common_defs cons_vars subst ls
lift_offered_substitutions {at_type=TAS off_cons off_args _} {at_type=TA {type_index,type_arity} dem_args} common_defs cons_vars subst ls
	= lift_offered_substitutions_type_application off_cons.type_index off_args type_index dem_args type_arity common_defs cons_vars subst ls
lift_offered_substitutions {at_type=TAS off_cons off_args _} {at_type=TAS {type_index,type_arity} dem_args _} common_defs cons_vars subst ls
	= lift_offered_substitutions_type_application off_cons.type_index off_args type_index dem_args type_arity common_defs cons_vars subst ls
lift_offered_substitutions {at_type=_ --> atype1} {at_type=_ --> atype2} common_defs cons_vars subst ls
	= lift_offered_substitutions atype1 atype2 common_defs cons_vars subst ls
lift_offered_substitutions {at_type=TempV _} {at_type=TempV _} common_defs cons_vars subst ls
	= (subst,ls)
lift_offered_substitutions {at_type=off_type} {at_type=TempV tv_number} common_defs cons_vars subst ls
	# (subst_type,subst) = subst![tv_number]
	= case subst_type of
		TLiftedSubst _
			-> (subst,ls)
		_
			# (changed, lifted_subst_type, subst, ls) = lift_pos_type_with_offered_type common_defs cons_vars off_type subst_type subst ls
			#! lifted_subst_type = lifted_subst_type
			# subst & [tv_number] = TLiftedSubst lifted_subst_type
			-> (subst,ls)
lift_offered_substitutions {at_type=TempV _} _ common_defs cons_vars subst ls
	= (subst,ls)
lift_offered_substitutions {at_type=TV {tv_info_ptr},at_attribute} dem_type common_defs cons_vars subst ls=:{ls_type_heaps}
	# (TVI_Type type, th_vars) = readPtr tv_info_ptr ls_type_heaps.th_vars
	  ls & ls_type_heaps = {ls_type_heaps & th_vars = th_vars}
	= lift_offered_substitutions {at_type=type,at_attribute=at_attribute} dem_type common_defs cons_vars subst ls
lift_offered_substitutions off_type {at_type=TV {tv_info_ptr},at_attribute} common_defs cons_vars subst ls=:{ls_type_heaps}
	# (TVI_Type type, th_vars) = readPtr tv_info_ptr ls_type_heaps.th_vars
	  ls & ls_type_heaps = {ls_type_heaps & th_vars = th_vars}
	= lift_offered_substitutions off_type {at_type=type,at_attribute=at_attribute} common_defs cons_vars subst ls
lift_offered_substitutions {at_type=TB _} {at_type=TB _} common_defs cons_vars subst ls
	= (subst,ls)
lift_offered_substitutions off_type dem_type common_defs cons_vars subst ls
	= (subst,ls)

has_no_ArrowKind :: ![TypeKind] -> Bool
has_no_ArrowKind kinds
	= not (Any IsArrowKind kinds)

lift_offered_substitutions_type_application :: (Global Int) [AType] (Global Int) [AType] Int !{#CommonDefs} !{#Int} !*{!Type} !*LiftState
																											   -> *(!*{!Type},!*LiftState)
lift_offered_substitutions_type_application off_type_index off_args dem_type_index=:{glob_module,glob_object} dem_args type_arity common_defs cons_vars subst ls
	| off_type_index==dem_type_index
		| has_no_ArrowKind ls.ls_td_infos.[glob_module].[glob_object].tdi_kinds
			# {ls_type_heaps,ls_td_infos} = ls
			  ({tsp_sign},th_vars,ls_td_infos) = typeProperties glob_object glob_module [] [] common_defs ls_type_heaps.th_vars ls_td_infos
			  ls & ls_type_heaps = {ls_type_heaps & th_vars=th_vars}, ls_td_infos=ls_td_infos
			| is_positive_sign tsp_sign type_arity
				= lift_offered_substitutions_args off_args dem_args common_defs cons_vars subst ls
				= (subst,ls)		
			= (subst,ls)		
		= (subst,ls)
where
	lift_offered_substitutions_args [off_arg:off_args] [dem_arg:dem_args] common_defs cons_vars subst ls
		# (subst,ls) = lift_offered_substitutions_args off_args dem_args common_defs cons_vars subst ls
		= lift_offered_substitutions off_arg dem_arg common_defs cons_vars subst ls
	lift_offered_substitutions_args [] [] common_defs cons_vars subst ls
		= (subst,ls)

	is_positive_sign {sc_pos_vect,sc_neg_vect} arity
		| arity==0
			= True
	 	| arity<32
			# m = (1<<arity)-1
			= (sc_pos_vect bitand m) == m && (sc_neg_vect bitand m) == 0
			= sc_pos_vect == -1 && sc_neg_vect == 0

lift_pos_atype_with_offered_type :: !{#CommonDefs} !{#BOOLVECT} !AType !AType !*{!Type} !*LiftState -> (!Bool,!AType,!*{!Type},!*LiftState)
lift_pos_atype_with_offered_type modules cons_vars {at_attribute=TA_Multi,at_type=off_type} attr_type=:{at_attribute=TA_Multi,at_type} subst ls
	// no new type attribute
	# (changed, at_type, subst, ls) = lift_pos_type_with_offered_type modules cons_vars off_type at_type subst ls
	| changed
		= (True, {attr_type & at_type = at_type},subst, ls)
		= (False, attr_type,subst, ls)
lift_pos_atype_with_offered_type modules cons_vars {at_type=off_type} attr_type=:{at_type} subst ls
	# (changed, at_type, subst, ls) = lift_pos_type_with_offered_type modules cons_vars off_type at_type subst ls
	| changed
		| typeIsNonCoercible cons_vars at_type
			= (True, {attr_type & at_type = at_type},subst, ls)
			= (True, {attr_type & at_attribute = TA_TempVar ls.ls_next_attr, at_type = at_type}, subst, {ls & ls_next_attr = inc ls.ls_next_attr})
		| typeIsNonCoercible cons_vars at_type
			= (False, attr_type,subst, ls)
			= (True, {attr_type & at_attribute = TA_TempVar ls.ls_next_attr}, subst, {ls & ls_next_attr = inc ls.ls_next_attr})

lift_pos_type_with_offered_type :: !{#CommonDefs} !{#BOOLVECT} !Type !Type !*{!Type} !*LiftState -> (!Bool,!Type,!*{!Type},!*LiftState)
lift_pos_type_with_offered_type modules cons_vars (TempV _) type subst ls
	= lift modules cons_vars type subst ls
lift_pos_type_with_offered_type modules cons_vars (_ :@: _) type subst ls
	= lift modules cons_vars type subst ls
lift_pos_type_with_offered_type modules cons_vars off_type type=:(TempV tv_number) subst ls
	# (type, subst) = subst![tv_number]
	= case type of
		TE
			-> (False, type, subst, ls)
		TLiftedSubst type
			# (_, type, subst, ls) = lift_pos_type_with_offered_type modules cons_vars off_type type subst ls
			-> (True, type, subst, ls)
		_
			# (_, type, subst, ls) = lift_pos_type_with_offered_type modules cons_vars off_type type subst ls
			-> (True, type, subst, ls)
lift_pos_type_with_offered_type modules cons_vars (_ --> off_res_type) type=:(arg_type0 --> res_type0) subst ls
	# (changed, arg_type, subst, ls) = lift modules cons_vars arg_type0 subst ls
	| changed
		# (changed, res_type, subst, ls) = lift_pos_atype_with_offered_type modules cons_vars off_res_type res_type0 subst ls
		| changed
			= (True, arg_type --> res_type, subst, ls)
			= (True, arg_type --> res_type0, subst, ls)
		# (changed, res_type, subst, ls) = lift_pos_atype_with_offered_type modules cons_vars off_res_type res_type0 subst ls
		| changed
			= (True, arg_type0 --> res_type, subst, ls)
			= (False, type, subst, ls)
lift_pos_type_with_offered_type modules cons_vars off_type=:(TA _ _) type=:(TA _ _) subst ls=:{ls_type_heaps}
	# (_, off_type, ls_type_heaps) = tryToExpand off_type TA_Multi modules ls_type_heaps
	# (_, type, ls_type_heaps) = tryToExpand type TA_Multi modules ls_type_heaps
	= lift_pos_type_application_with_offered_type modules cons_vars off_type type subst {ls & ls_type_heaps = ls_type_heaps}
lift_pos_type_with_offered_type modules cons_vars off_type=:(TA _ _) type=:(TAS _ _ _) subst ls=:{ls_type_heaps}
	# (_, off_type, ls_type_heaps) = tryToExpand off_type TA_Multi modules ls_type_heaps
	# (_, type, ls_type_heaps) = tryToExpand type TA_Multi modules ls_type_heaps
	= lift_pos_type_application_with_offered_type modules cons_vars off_type type subst {ls & ls_type_heaps = ls_type_heaps}
lift_pos_type_with_offered_type modules cons_vars off_type=:(TAS _ _ _) type=:(TA _ _) subst ls=:{ls_type_heaps}
	# (_, off_type, ls_type_heaps) = tryToExpand off_type TA_Multi modules ls_type_heaps
	# (_, type, ls_type_heaps) = tryToExpand type TA_Multi modules ls_type_heaps
	= lift_pos_type_application_with_offered_type modules cons_vars off_type type subst {ls & ls_type_heaps = ls_type_heaps}			
lift_pos_type_with_offered_type modules cons_vars off_type=:(TAS _ _ _) type=:(TAS _ _ _) subst ls=:{ls_type_heaps}
	# (_, off_type, ls_type_heaps) = tryToExpand off_type TA_Multi modules ls_type_heaps
	# (_, type, ls_type_heaps) = tryToExpand type TA_Multi modules ls_type_heaps
	= lift_pos_type_application_with_offered_type modules cons_vars off_type type subst {ls & ls_type_heaps = ls_type_heaps}			
lift_pos_type_with_offered_type modules cons_vars off_type type=:(TArrow1 arg_type) subst ls
	# (changed, arg_type, subst, ls) = lift modules cons_vars arg_type subst ls
	| changed
		= (True, TArrow1 arg_type, subst, ls)
		= (False, type, subst, ls)	
lift_pos_type_with_offered_type modules cons_vars off_type type=:(TempCV temp_var :@: types) subst ls
	= lift modules cons_vars type subst ls
lift_pos_type_with_offered_type modules cons_vars off_type (TFA vars type) subst ls
	= abort "lift_pos_type_with_offered_type TFA"
lift_pos_type_with_offered_type modules cons_vars off_type type subst ls
	= (False, type, subst, ls)

lift_pos_type_application_with_offered_type :: !{#CommonDefs} !{#Int} !Type !Type !*{!Type} !*LiftState -> *(!Bool,!Type,!*{!Type},!*LiftState)
lift_pos_type_application_with_offered_type modules cons_vars (TA _ off_type_args) type=:(TA _ _) subst ls
	= lift_pos_TA_application_with_offered_type modules cons_vars off_type_args type subst ls
lift_pos_type_application_with_offered_type modules cons_vars (TAS _ off_type_args _) type=:(TA _ _) subst ls
	= lift_pos_TA_application_with_offered_type modules cons_vars off_type_args type subst ls
lift_pos_type_application_with_offered_type modules cons_vars (TA _ off_type_args) type=:(TAS _ _ _) subst ls
	= lift_pos_TAS_application_with_offered_type modules cons_vars off_type_args type subst ls
lift_pos_type_application_with_offered_type modules cons_vars (TAS _ off_type_args _) type=:(TAS _ _ _) subst ls
	= lift_pos_TAS_application_with_offered_type modules cons_vars off_type_args type subst ls
lift_pos_type_application_with_offered_type modules cons_vars off_type type subst ls
	= lift_pos_type_with_offered_type modules cons_vars off_type type subst ls

lift_pos_TA_application_with_offered_type :: !{#CommonDefs} !{#Int} ![AType] !Type !*{!Type} !*LiftState -> *(!Bool,!Type,!*{!Type},!*LiftState)
lift_pos_TA_application_with_offered_type modules cons_vars off_type_args t0=:(TA cons_id=:{type_ident,type_index={glob_object,glob_module},type_arity,type_prop=type_prop0} cons_args) subst ls=:{ls_type_heaps}
	| has_no_ArrowKind ls.ls_td_infos.[glob_module].[glob_object].tdi_kinds
		# (type_prop, th_vars, ls_td_infos) = typeProperties glob_object glob_module [] [] modules ls_type_heaps.th_vars ls.ls_td_infos
		  ls & ls_td_infos = ls_td_infos, ls_type_heaps = {ls_type_heaps & th_vars = th_vars}
		# (changed,cons_args, subst, ls=:{ls_type_heaps}) = lift_pos_list_with_offered_type modules cons_vars off_type_args cons_args type_prop.tsp_sign subst ls
		| changed
			| equal_type_prop type_prop type_prop0
				= (True, TA cons_id cons_args, subst, ls)
				= (True, TA {cons_id & type_prop = type_prop} cons_args, subst, ls)
			| equal_type_prop type_prop type_prop0
				= (False, t0, subst, ls)
				= (True, TA {cons_id & type_prop = type_prop} cons_args, subst, ls)
		= liftTypeApplication modules cons_vars t0 subst ls

lift_pos_TAS_application_with_offered_type :: !{#CommonDefs} !{#Int} ![AType] !Type !*{!Type} !*LiftState -> *(!Bool,!Type,!*{!Type},!*LiftState)
lift_pos_TAS_application_with_offered_type modules cons_vars off_type t0=:(TAS cons_id=:{type_ident,type_index={glob_object,glob_module},type_arity,type_prop=type_prop0} cons_args strictness) subst ls=:{ls_type_heaps}
	| has_no_ArrowKind ls.ls_td_infos.[glob_module].[glob_object].tdi_kinds
		# (type_prop, th_vars, ls_td_infos) = typeProperties glob_object glob_module [] [] modules ls_type_heaps.th_vars ls.ls_td_infos
		  ls & ls_td_infos = ls_td_infos, ls_type_heaps = {ls_type_heaps & th_vars = th_vars}
		# (changed,cons_args, subst, ls) = lift_pos_list_with_offered_type modules cons_vars off_type cons_args type_prop.tsp_sign subst ls
		| changed
			| equal_type_prop type_prop type_prop0
				= (True, TAS cons_id cons_args strictness, subst, ls)
				= (True, TAS {cons_id & type_prop = type_prop} cons_args strictness, subst, ls)
			| equal_type_prop type_prop type_prop0
				= (False, t0, subst, ls)
				= (True, TAS {cons_id & type_prop = type_prop} cons_args strictness, subst, ls)
		= liftTypeApplication modules cons_vars t0 subst ls

lift_pos_list_with_offered_type :: !{#CommonDefs} !{#BOOLVECT} ![AType] ![AType] !SignClassification !*{!Type} !*LiftState -> (!Bool,![AType], !*{!Type}, !*LiftState)
lift_pos_list_with_offered_type modules cons_vars [off_type:off_types] ts0=:[t0:ts] {sc_pos_vect,sc_neg_vect} subst ls
	# next_sc = {sc_pos_vect=sc_pos_vect>>1,sc_neg_vect=sc_neg_vect>>1}
	| sc_pos_vect bitand 1 > sc_neg_vect bitand 1
		# (changed, t, subst, ls) = lift_pos_atype_with_offered_type modules cons_vars off_type t0 subst ls
		| changed
			# (_, ts, subst, ls) = lift_pos_list_with_offered_type modules cons_vars off_types ts next_sc subst ls
			= (True,[t:ts],subst,ls)
			# (changed, ts, subst, ls) = lift_pos_list_with_offered_type modules cons_vars off_types ts next_sc subst ls
			| changed
				= (True, [t:ts], subst, ls)
				= (False, ts0, subst, ls)
		# (changed, t, subst, ls) = lift modules cons_vars t0 subst ls
		| changed
			# (_, ts, subst, ls) = lift_pos_list_with_offered_type modules cons_vars ts off_types next_sc subst ls
			= (True,[t:ts],subst,ls)
			# (changed, ts, subst, ls) = lift_pos_list_with_offered_type modules cons_vars ts off_types next_sc subst ls
			| changed
				= (True, [t:ts], subst, ls)
				= (False, ts0, subst, ls)
lift_pos_list_with_offered_type modules cons_vars [] [] sign_class subst ls
	= (False, [], subst, ls)


liftRemainingSubstitutions :: !*{!Type} !{#CommonDefs} !{#BOOLVECT} !Int !*TypeHeaps !*TypeDefInfos -> (*{! Type}, !Int, !*TypeHeaps, !*TypeDefInfos)
liftRemainingSubstitutions subst modules cons_vars attr_store type_heaps td_infos 
	# ls = {ls_next_attr = attr_store, ls_td_infos = td_infos, ls_type_heaps = type_heaps}
	= lift_substitution 0 modules cons_vars subst ls
where
	lift_substitution var_index modules cons_vars subst ls
		| var_index < size subst
			# (type, subst) = subst![var_index]
			= case type of
				TLiftedSubst type
					-> lift_substitution (inc var_index) modules cons_vars {subst & [var_index] = type} ls
				_
					# (_, type, subst, ls) = lift modules cons_vars type subst ls
					-> lift_substitution (inc var_index) modules cons_vars {subst & [var_index] = type} ls
			= (subst, ls.ls_next_attr, ls.ls_type_heaps, ls.ls_td_infos)
