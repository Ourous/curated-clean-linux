implementation module analunitypes

import StdEnv, compare_types
import syntax, checksupport, analtypes, checktypes, utilities

instance + SignClassification
where
	(+) {sc_pos_vect=sc_pos_vect1,sc_neg_vect=sc_neg_vect1} {sc_pos_vect=sc_pos_vect2,sc_neg_vect=sc_neg_vect2}
		= {	sc_pos_vect = sc_pos_vect1 bitor sc_pos_vect2, sc_neg_vect = sc_neg_vect1 bitor sc_neg_vect2 }

(*+)  infixl 7 :: !Sign !SignClassification -> SignClassification
(*+) {pos_sign,neg_sign} {sc_pos_vect,sc_neg_vect}
	= {	sc_pos_vect = (if pos_sign sc_pos_vect 0) bitor (if neg_sign sc_neg_vect 0),
		sc_neg_vect = (if neg_sign sc_pos_vect 0) bitor (if pos_sign sc_neg_vect 0) }

sign_class_to_sign :: !SignClassification !Int -> Sign
sign_class_to_sign {sc_pos_vect,sc_neg_vect} index
	= { pos_sign = sc_pos_vect bitand (1 << index) <> 0, neg_sign = sc_neg_vect bitand (1 << index) <> 0}

set_sign_in_sign_class :: !Sign !Int !SignClassification -> SignClassification
set_sign_in_sign_class {pos_sign,neg_sign} index {sc_pos_vect,sc_neg_vect}
	= { sc_pos_vect = sc_pos_vect bitor (if pos_sign (1 << index) 0), sc_neg_vect = sc_neg_vect bitor (if neg_sign (1 << index) 0) }

typeProperties :: !Index !Index ![SignClassification] ![PropClassification] !{# CommonDefs } !*TypeVarHeap !*TypeDefInfos
	-> (!TypeSymbProperties, !*TypeVarHeap, !*TypeDefInfos)
typeProperties type_index module_index hio_signs hio_props defs type_var_heap td_infos
	# (td_info, td_infos) = td_infos![module_index].[type_index]
	  (tsp_sign, type_var_heap, td_infos) = determineSignClassOfTypeDef type_index module_index td_info hio_signs defs type_var_heap td_infos
	  (tsp_propagation, type_var_heap, td_infos) = determinePropClassOfTypeDef type_index module_index td_info hio_props defs type_var_heap td_infos
	  tsp_coercible = (td_info.tdi_properties bitand cIsNonCoercible) == 0
	= ({tsp_sign = tsp_sign, tsp_propagation = tsp_propagation, tsp_coercible = tsp_coercible }, type_var_heap, td_infos)

signClassification :: !Index !Index ![SignClassification] !{# CommonDefs } !*TypeVarHeap !*TypeDefInfos
	-> (!SignClassification, !*TypeVarHeap, !*TypeDefInfos)
signClassification type_index module_index hio_signs defs type_var_heap td_infos
	# (td_info, td_infos) = td_infos![module_index].[type_index]
	# (tsp_sign, type_var_heap, td_infos)
		= determineSignClassOfTypeDef type_index module_index td_info hio_signs defs type_var_heap td_infos
	= (tsp_sign, type_var_heap, td_infos)

removeTopClasses [cv : cvs] [tc : tcs] 
	| isATopConsVar cv
		= removeTopClasses cvs tcs
		= [tc : removeTopClasses cvs tcs] 
removeTopClasses _ _
	= []

::	RecTypeApplication classification =
	{	rta_index			:: !Int
	,	rta_classification	:: !classification
	}

::	SignClassState =
	{	scs_type_var_heap	:: !.TypeVarHeap
	,	scs_type_def_infos	:: !.TypeDefInfos
	,	scs_rec_appls		:: ![RecTypeApplication (Sign, [SignClassification])]
	}

determineSignClassOfTypeDef :: !Int !Int !TypeDefInfo ![SignClassification] !{# CommonDefs} !*TypeVarHeap !*TypeDefInfos
	-> (!SignClassification, !*TypeVarHeap,!*TypeDefInfos)
determineSignClassOfTypeDef type_index module_index {tdi_classification,tdi_cons_vars,tdi_group_vars,tdi_group,tdi_group_nr}
			hio_signs ci type_var_heap td_infos
	# hio_signs = removeTopClasses tdi_cons_vars hio_signs
	  result = retrieveSignClassification hio_signs tdi_classification
	= case result of
		Yes {ts_type_sign}
			-> (ts_type_sign, type_var_heap, td_infos)
		No
			# signs_of_group_vars = foldSt (determine_signs_of_group_var tdi_cons_vars hio_signs) tdi_group_vars []
			-> newSignClassOfTypeDefGroup tdi_group_nr { gi_module = module_index, gi_index = type_index}
					tdi_group signs_of_group_vars ci type_var_heap td_infos
where
	determine_signs_of_group_var cons_vars cons_var_signs gv signs_of_group_vars
		| sign_determined gv signs_of_group_vars
			= signs_of_group_vars
			# sign = determine_classification gv cons_vars cons_var_signs BottomSignClass
			= [(gv, sign) : signs_of_group_vars]
	where
		sign_determined this_gv []
			= False
		sign_determined this_gv [(gv,_) : signs]
			= this_gv == gv || sign_determined this_gv signs
			
		determine_classification gv [cv : cvs] sigs=:[tc : tcs] cumm_sign_class
			| isATopConsVar cv
				| gv == decodeTopConsVar cv
					= TopSignClass
					= determine_classification gv cvs sigs cumm_sign_class
			| gv == cv
				= determine_classification gv cvs tcs (tc + cumm_sign_class)
				= determine_classification gv cvs tcs cumm_sign_class
		determine_classification gv cons_vars [] cumm_sign_class
			= cumm_sign_class
		
::	SignRequirements =
	{	sr_classification		:: !SignClassification
	,	sr_hio_signs			:: ![SignClassification]
	,	sr_type_applications	:: ![RecTypeApplication (Sign, [SignClassification])]
	}

newGroupSigns :: !Int -> *{# SignRequirements}
newGroupSigns group_size = createArray group_size { sr_hio_signs = [], sr_classification = BottomSignClass, sr_type_applications = [] }

newSignClassOfTypeDefGroup :: !Int !GlobalIndex ![GlobalIndex] ![(Int, SignClassification)] !{#CommonDefs} !*TypeVarHeap !*TypeDefInfos
			-> *(!SignClassification, !*TypeVarHeap, !*TypeDefInfos)
newSignClassOfTypeDefGroup group_nr {gi_module,gi_index} group signs_of_group_vars ci type_var_heap td_infos
	# (group_signs, type_var_heap, td_infos) = collect_sign_class_of_type_defs group_nr group signs_of_group_vars ci
	  					(newGroupSigns (length group)) type_var_heap td_infos
	  group_signs = determine_fixed_point group_signs
	  td_infos = update_sign_class_of_group group group_signs td_infos
	  (tdi=:{tdi_index_in_group},td_infos) = td_infos![gi_module].[gi_index]
	= (group_signs.[tdi_index_in_group].sr_classification, type_var_heap, td_infos)
where
	update_sign_class_of_group group group_signs td_infos
		= foldSt (update_sign_class_of_type_def group_signs) group td_infos
	where
		update_sign_class_of_type_def group_signs {gi_module,gi_index} td_infos
			# (tdi=:{tdi_classification,tdi_index_in_group},td_infos) = td_infos![gi_module].[gi_index]
			  {sr_hio_signs, sr_classification} = group_signs.[tdi_index_in_group]
			  tdi_classification = addSignClassification sr_hio_signs sr_classification tdi_classification
			= { td_infos & [gi_module].[gi_index] = { tdi & tdi_classification = tdi_classification }}

	collect_sign_class_of_type_defs group_nr group signs_of_group_vars ci sign_requirements type_var_heap td_infos
		= foldSt (collect_sign_class_of_type_def group_nr signs_of_group_vars ci) group (sign_requirements, type_var_heap, td_infos)
	where			
		collect_sign_class_of_type_def group_nr signs_of_group_vars ci {gi_module,gi_index} (sign_requirements, type_var_heap, td_infos)
			# ({tdi_group_vars,tdi_kinds,tdi_index_in_group},td_infos) = td_infos![gi_module].[gi_index]
			  {td_ident,td_args,td_rhs} = ci.[gi_module].com_type_defs.[gi_index]
			  (rev_hio_signs, type_var_heap) = bind_type_vars_to_signs td_args tdi_group_vars tdi_kinds signs_of_group_vars ([], type_var_heap)
			  (sign_env, scs) = sign_class_of_type_def gi_module td_rhs group_nr ci 
		  								{scs_type_var_heap = type_var_heap, scs_type_def_infos = td_infos, scs_rec_appls = [] }
			  type_var_heap = foldSt restore_binds_of_type_var td_args scs.scs_type_var_heap
			= ({sign_requirements & [tdi_index_in_group] = { sr_hio_signs = reverse rev_hio_signs, sr_classification = sign_env,
					sr_type_applications = scs.scs_rec_appls }}, type_var_heap, scs.scs_type_def_infos)

	determine_fixed_point sign_requirements
		#! group_size = size sign_requirements
		# (go_on, sign_requirements) = iFoldSt next_sign_classification 0 group_size (False, sign_requirements)
		| go_on
			= determine_fixed_point sign_requirements
			= sign_requirements
	
	next_sign_classification type_index (changed, sign_requirements)
		# ({sr_classification,sr_type_applications}, sign_requirements) = sign_requirements![type_index]
		  (new_sr_classification, sign_requirements) = foldSt examine_type_application sr_type_applications (sr_classification, sign_requirements)
		| sr_classification == new_sr_classification
			= (changed, sign_requirements)
			= (True, { sign_requirements & [type_index].sr_classification = new_sr_classification })
	
	examine_type_application {rta_index, rta_classification = (sign, arg_classes)} (cumm_class, sign_requirements)
		# (sr_classification, sign_requirements) = sign_requirements![rta_index].sr_classification
		  cumm_class = determine_cummulative_sign sign sr_classification arg_classes 0 cumm_class
		= (cumm_class, sign_requirements)
	where
		determine_cummulative_sign sign sign_class [arg_class : arg_classes] type_index cumm_class
			# this_sign = sign_class_to_sign sign_class type_index
			= determine_cummulative_sign sign sign_class arg_classes (inc type_index) ((sign * this_sign) *+ arg_class + cumm_class)
		determine_cummulative_sign sign sign_class [] type_index cumm_class
			= cumm_class
	
	bind_type_vars_to_signs [] group_vars kinds signs_of_group_vars (rev_hio_signs, type_var_heap)
		= (rev_hio_signs, type_var_heap)
	bind_type_vars_to_signs [{atv_variable={tv_info_ptr}}: tvs] [gv : gvs] [tk : tks] signs_of_group_vars (rev_hio_signs, type_var_heap)
		# sign = retrieve_sign gv signs_of_group_vars
		  (var_info, type_var_heap) = readPtr tv_info_ptr type_var_heap
		| IsArrowKind tk
			= bind_type_vars_to_signs tvs gvs tks signs_of_group_vars ([sign:rev_hio_signs], type_var_heap <:= (tv_info_ptr, TVI_SignClass gv sign var_info))
			= bind_type_vars_to_signs tvs gvs tks signs_of_group_vars (rev_hio_signs, type_var_heap <:= (tv_info_ptr, TVI_SignClass gv sign var_info))
	where
		retrieve_sign this_gv [(gv,sign) : signs ]
			| this_gv == gv
				= sign
				= retrieve_sign this_gv signs
		retrieve_sign this_gv [ ]
			= TopSignClass

	restore_binds_of_type_var {atv_variable={tv_info_ptr}} type_var_heap
		# (TVI_SignClass _ _ old_info, type_var_heap) = readPtr tv_info_ptr type_var_heap
		= type_var_heap <:= (tv_info_ptr, old_info)

	sign_class_of_type_def :: !Int !TypeRhs !Int !{#CommonDefs} !*SignClassState -> (!SignClassification,!*SignClassState)
	sign_class_of_type_def module_index (AlgType conses) group_nr ci scs
		= sign_class_of_type_conses module_index conses group_nr ci BottomSignClass scs
	sign_class_of_type_def _ (SynType {at_type}) group_nr ci scs
		# (sign_class, _, scs) = signClassOfType at_type PositiveSign DontUSeTopSign group_nr ci scs
		= (sign_class, scs)
	sign_class_of_type_def module_index (RecordType {rt_constructor}) group_nr ci scs
		= sign_class_of_type_cons module_index rt_constructor group_nr ci BottomSignClass scs
	sign_class_of_type_def module_index (NewType constructor) group_nr ci scs
		= sign_class_of_type_cons module_index constructor group_nr ci BottomSignClass scs
	sign_class_of_type_def _ (AbstractType properties) _ _ scs
		| properties bitand cIsNonCoercible == 0
			= (PostiveSignClass, scs)
			= (TopSignClass, scs)
	sign_class_of_type_def _ (AbstractSynType properties _) _ _ scs
		| properties bitand cIsNonCoercible == 0
			= (PostiveSignClass, scs)
			= (TopSignClass, scs)
	sign_class_of_type_def module_index (ExtensibleAlgType conses) group_nr ci scs
		= (TopSignClass, scs)
	sign_class_of_type_def module_index (AlgConses conses _) group_nr ci scs
		= (TopSignClass, scs)

	sign_class_of_type_conses module_index [{ds_index}:conses] group_nr ci cumm_sign_class scs
		#! cons_def = ci.[module_index].com_cons_defs.[ds_index]
		#  (cumm_sign_class, scs) = sign_class_of_type_of_list cons_def.cons_type.st_args group_nr ci cumm_sign_class scs
		= sign_class_of_type_conses module_index conses group_nr ci cumm_sign_class scs
	sign_class_of_type_conses module_index [] _ _ cumm_sign_class scs
		= (cumm_sign_class, scs)

	sign_class_of_type_cons module_index {ds_index} group_nr ci cumm_sign_class scs
		#! cons_def = ci.[module_index].com_cons_defs.[ds_index]
		= sign_class_of_type_of_list cons_def.cons_type.st_args group_nr ci cumm_sign_class scs

	sign_class_of_type_of_list [] _ _ cumm_sign_class scs
		= (cumm_sign_class, scs)
	sign_class_of_type_of_list [{at_type} : types] group_nr ci cumm_sign_class scs
		# (sign_class, _, scs) = signClassOfType at_type PositiveSign DontUSeTopSign group_nr ci scs
		= sign_class_of_type_of_list types group_nr ci (cumm_sign_class + sign_class) scs
			
IsAHioType		:== True
IsNotAHioType	:== False

IsArrowKind (KindArrow _) = True
IsArrowKind _ = False

signClassOfTypeVariable :: !TypeVar !{#CommonDefs} !*SignClassState -> (!SignClassification,!SignClassification,!*SignClassState)
signClassOfTypeVariable {tv_ident,tv_info_ptr} ci scs=:{scs_type_var_heap}
	# (var_info, scs_type_var_heap) = readPtr tv_info_ptr scs_type_var_heap
	  scs = { scs & scs_type_var_heap = scs_type_var_heap }
	= case var_info of
		TVI_SignClass group_var_index var_class _ 
			-> (var_index_to_sign_class group_var_index, var_class, scs)
		_
			-> (BottomSignClass, TopSignClass, scs)
where
	var_index_to_sign_class :: !Int -> SignClassification
	var_index_to_sign_class var_index 
		= { sc_pos_vect = 1 << var_index, sc_neg_vect = 0}

UseTopSign		:== True
DontUSeTopSign	:== False

signClassOfType_for_TA :: Int Int [AType] !Sign !Bool !Int !{#CommonDefs} !*SignClassState -> (!SignClassification,!SignClassification,!*SignClassState)
signClassOfType_for_TA glob_module glob_object types sign use_top_sign group_nr ci scs
	# (td_info=:{tdi_group_nr,tdi_index_in_group,tdi_kinds}, scs) = scs!scs_type_def_infos.[glob_module].[glob_object]
	| tdi_group_nr == group_nr
		= sign_class_of_type_list_of_rec_type types sign use_top_sign tdi_index_in_group ci [] scs 
		# {td_arity,td_ident} = ci.[glob_module].com_type_defs.[glob_object]
		  (sign_classes, hio_signs, scs) = collect_sign_classes_of_type_list types tdi_kinds group_nr ci scs 
		  (type_class, scs_type_var_heap, scs_type_def_infos)
		  		= determineSignClassOfTypeDef glob_object glob_module td_info hio_signs ci scs.scs_type_var_heap scs.scs_type_def_infos
		  (sign_class, scs) = determine_cummulative_sign types tdi_kinds sign use_top_sign type_class sign_classes 0 ci BottomSignClass
		  							{ scs & scs_type_var_heap = scs_type_var_heap, scs_type_def_infos = scs_type_def_infos } 
		= (sign_class, adjust_sign_class type_class td_arity, scs)
where
	sign_class_of_type_list_of_rec_type [t : ts] sign use_top_sign tmp_type_index ci rev_sign_classes scs
		# (sign_class, type_class, scs) = signClassOfType t.at_type PositiveSign UseTopSign group_nr ci scs
		= sign_class_of_type_list_of_rec_type ts sign use_top_sign tmp_type_index ci [ sign_class : rev_sign_classes ] scs
	sign_class_of_type_list_of_rec_type [] sign use_top_sign tmp_type_index ci rev_sign_classes scs=:{scs_rec_appls}
		# rta = { rta_index = tmp_type_index, rta_classification = (if use_top_sign TopSign sign, reverse rev_sign_classes) }
		= (BottomSignClass, TopSignClass, { scs & scs_rec_appls = [ rta : scs_rec_appls ] })

	collect_sign_classes_of_type_list [t : ts] [tk : tks] group_nr ci scs
		| IsArrowKind tk
			# (sign_class, type_class, scs) = signClassOfType t.at_type PositiveSign UseTopSign group_nr ci scs
			  (sign_classes, hio_signs, scs) = collect_sign_classes_of_type_list ts tks group_nr ci scs
			= ([sign_class : sign_classes], [type_class:hio_signs], scs)
			= collect_sign_classes_of_type_list ts tks group_nr ci scs
	collect_sign_classes_of_type_list [] _ _ ci scs
		= ([], [], scs)
	collect_sign_classes_of_type_list _ _ _ ci scs
		= abort "collect_sign_classes_of_type_list (analunitypes)"

	determine_cummulative_sign [t : ts] [tk : tks] sign use_top_sign sign_class sign_classes type_index ci cumm_class scs
		| IsArrowKind tk
			# [sc : sign_classes] = sign_classes
			= determine_cummulative_sign ts tks sign use_top_sign sign_class sign_classes (inc type_index) ci (sc + cumm_class) scs
			# this_sign = sign_class_to_sign sign_class type_index
			  (sign_class, type_class, scs) = signClassOfType t.at_type this_sign use_top_sign group_nr ci scs
			= determine_cummulative_sign ts tks sign use_top_sign  sign_class sign_classes (inc type_index) ci (sign *+ sign_class + cumm_class) scs
	determine_cummulative_sign [] _ sign use_top_sign sign_class sign_classes type_index ci cumm_class scs
		= (cumm_class, scs)

	adjust_sign_class {sc_pos_vect,sc_neg_vect} arity
		= { sc_pos_vect = sc_pos_vect >> arity, sc_neg_vect = sc_neg_vect >> arity }

signClassOfType :: !Type !Sign !Bool !Int !{#CommonDefs} !*SignClassState -> (!SignClassification,!SignClassification,!*SignClassState)
signClassOfType (TV tv) sign use_top_sign group_nr ci scs
	# (sign_class, type_class, scs) = signClassOfTypeVariable tv ci scs
	= (sign *+ sign_class, type_class, scs)

signClassOfType (TA {type_index = {glob_module, glob_object}} types) sign use_top_sign group_nr ci scs
	= signClassOfType_for_TA glob_module glob_object types sign use_top_sign group_nr ci scs

signClassOfType (TAS {type_index = {glob_module, glob_object}} types _) sign use_top_sign group_nr ci scs
	= signClassOfType_for_TA glob_module glob_object types sign use_top_sign group_nr ci scs

signClassOfType (CV tv :@: types) sign use_top_sign group_nr ci scs
	# (sign_class, type_class, scs) = signClassOfTypeVariable tv ci scs
	  (sign_class, scs) = sign_class_of_type_list types sign use_top_sign group_nr type_class 0 sign_class ci scs
	= (sign_class, BottomSignClass, scs)
where	  	
	sign_class_of_type_list [{at_type} : ts] sign use_top_sign group_nr cv_sign_class type_index cumm_class ci scs
		# (sign_class, _, scs) = signClassOfType at_type (sign_class_to_sign cv_sign_class type_index) use_top_sign group_nr ci scs
		  cumm_class = (sign *+ sign_class) + cumm_class
		= sign_class_of_type_list ts sign use_top_sign group_nr sign_class (inc type_index) cumm_class ci scs
	sign_class_of_type_list [] sign use_top_sign group_nr cv_sign_class type_index cumm_class ci scs
		= (cumm_class, scs)

signClassOfType (arg_type --> res_type) sign use_top_sign group_nr ci scs
	# (arg_class, _, scs) = signClassOfType arg_type.at_type NegativeSign use_top_sign group_nr ci scs
	  (res_class, _, scs) = signClassOfType res_type.at_type PositiveSign use_top_sign group_nr ci scs
	= (sign *+ (arg_class + res_class), BottomSignClass, scs)

signClassOfType (TFA vars type) sign use_top_sign group_nr ci scs
	= signClassOfType type sign use_top_sign group_nr ci scs
signClassOfType (TFAC vars type _) sign use_top_sign group_nr ci scs
	= signClassOfType type sign use_top_sign group_nr ci scs

signClassOfType type _ _ _ _ scs
	= (BottomSignClass, BottomSignClass, scs)

propClassification :: !Index !Index ![PropClassification] !{# CommonDefs } !*TypeVarHeap !*TypeDefInfos
	-> (!PropClassification, !*TypeVarHeap, !*TypeDefInfos)
propClassification type_index module_index hio_props defs type_var_heap td_infos
	| type_index >= size td_infos.[module_index]
		= (0, type_var_heap, td_infos)
		# (td_info, td_infos) = td_infos![module_index].[type_index]
		| td_info.tdi_group_nr== (-1) // is an exported dictionary ?
			= (0, type_var_heap, td_infos)
			# (tsp_prop, type_var_heap, td_infos)
				= determinePropClassOfTypeDef type_index module_index td_info hio_props defs type_var_heap td_infos
			= (tsp_prop, type_var_heap, td_infos)
//				---> ("propClassification", defs.[module_index].com_type_defs.[type_index].td_ident, tsp_prop)

determinePropClassOfTypeDef :: !Int !Int !TypeDefInfo ![PropClassification] !{# CommonDefs} !*TypeVarHeap !*TypeDefInfos
	-> (!PropClassification,!*TypeVarHeap, !*TypeDefInfos)
determinePropClassOfTypeDef type_index module_index {tdi_classification, tdi_kinds, tdi_group, tdi_group_vars, tdi_cons_vars, tdi_group_nr}
			hio_props ci type_var_heap td_infos
	# hio_props = removeTopClasses tdi_cons_vars hio_props
	  result = retrievePropClassification hio_props tdi_classification
	= case result of
		Yes {ts_type_prop}
			-> (ts_type_prop, type_var_heap, td_infos)

		No
			# props_of_group_vars = foldSt (determine_props_of_group_var tdi_cons_vars hio_props) tdi_group_vars []
			-> newPropClassOfTypeDefGroup tdi_group_nr { gi_module = module_index, gi_index = type_index}
					tdi_group props_of_group_vars ci type_var_heap td_infos

where
	determine_props_of_group_var cons_vars cons_var_signs gv props_of_group_vars
		| prop_determined gv props_of_group_vars
			= props_of_group_vars
			# prop = determine_classification gv cons_vars cons_var_signs NoPropClass
			= [(gv, prop) : props_of_group_vars]
	where
		prop_determined this_gv []
			= False
		prop_determined this_gv [(gv,_) : props]
			= this_gv == gv || prop_determined this_gv props
			
		determine_classification gv [cv : cvs] hio_props=:[tc : tcs] cumm_prop_class
			| isATopConsVar cv
				| gv == decodeTopConsVar cv
					= PropClass
					= determine_classification gv cvs tcs cumm_prop_class
			| gv == cv
				= determine_classification gv cvs tcs (tc bitor cumm_prop_class)
				= determine_classification gv cvs tcs cumm_prop_class
		determine_classification gv cons_vars [] cumm_prop_class
			= cumm_prop_class

::	PropRequirements =
	{	pr_classification		:: !PropClassification
	,	pr_hio_signs			:: ![PropClassification]
	,	pr_type_applications	:: ![RecTypeApplication [PropClassification]]
	}

::	PropClassState =
	{	pcs_type_var_heap	:: !.TypeVarHeap
	,	pcs_type_def_infos	:: !.TypeDefInfos
	,	pcs_rec_appls		:: ![RecTypeApplication [PropClassification]]
	}

newGroupProps :: !Int -> *{# PropRequirements}
newGroupProps group_size = createArray group_size { pr_hio_signs = [], pr_classification = NoPropClass, pr_type_applications = [] }

newPropClassOfTypeDefGroup :: !Int !GlobalIndex ![GlobalIndex] ![(Int, PropClassification)] !{#CommonDefs} !*TypeVarHeap !*TypeDefInfos
			-> *(!PropClassification, !*TypeVarHeap, !*TypeDefInfos)
newPropClassOfTypeDefGroup group_nr {gi_module,gi_index} group props_of_group_vars ci type_var_heap td_infos
	# (group_props, type_var_heap, td_infos) = collect_prop_class_of_type_defs group_nr group props_of_group_vars ci
														(newGroupProps (length group)) type_var_heap td_infos
	  group_props = determine_fixed_point group_props
	  td_infos = update_prop_class_of_group group group_props td_infos
	  (tdi=:{tdi_index_in_group},td_infos) = td_infos![gi_module].[gi_index]
	= (group_props.[tdi_index_in_group].pr_classification, type_var_heap, td_infos)
where
	update_prop_class_of_group group group_props td_infos
		= foldSt (update_prop_class_of_type_def group_props) group td_infos
	where
		update_prop_class_of_type_def group_props {gi_module,gi_index} td_infos
			# (tdi=:{tdi_classification,tdi_index_in_group},td_infos) = td_infos![gi_module].[gi_index]
			  {pr_hio_signs, pr_classification} = group_props.[tdi_index_in_group]
			  tdi_classification = addPropClassification pr_hio_signs pr_classification tdi_classification
			= { td_infos & [gi_module].[gi_index] = { tdi & tdi_classification = tdi_classification }}
		
	collect_prop_class_of_type_defs group_nr group props_of_group_vars ci prop_requirements type_var_heap td_infos
		= foldSt (collect_sign_class_of_type_def group_nr props_of_group_vars ci) group (prop_requirements, type_var_heap, td_infos)
	where			
		collect_sign_class_of_type_def group_nr props_of_group_vars ci {gi_module,gi_index} (prop_requirements, type_var_heap, td_infos)
			# ({tdi_group_vars,tdi_kinds,tdi_index_in_group},td_infos) = td_infos![gi_module].[gi_index]
			  {td_ident,td_args,td_rhs} = ci.[gi_module].com_type_defs.[gi_index]
			  (rev_hio_props, type_var_heap) = bind_type_vars_to_props td_args tdi_group_vars tdi_kinds props_of_group_vars ([], type_var_heap)
			  (prop_env, pcs) = prop_class_of_type_def gi_module td_rhs group_nr ci 
		  								{pcs_type_var_heap = type_var_heap, pcs_type_def_infos = td_infos, pcs_rec_appls = [] }
			  type_var_heap = foldSt restore_binds_of_type_var td_args pcs.pcs_type_var_heap
			= ({prop_requirements & [tdi_index_in_group] = { pr_hio_signs = reverse rev_hio_props, pr_classification = prop_env,
					pr_type_applications = pcs.pcs_rec_appls }}, type_var_heap, pcs.pcs_type_def_infos)

	determine_fixed_point sign_requirements
		#! group_size = size sign_requirements
		# (go_on, sign_requirements) = iFoldSt next_prop_classification 0 group_size (False, sign_requirements)
		| go_on
			= determine_fixed_point sign_requirements
			= sign_requirements
	
	next_prop_classification type_index (changed, prop_requirements)
		# ({pr_classification,pr_type_applications}, prop_requirements) = prop_requirements![type_index]
		  (new_pr_classification, prop_requirements) = foldSt examine_type_application pr_type_applications (pr_classification, prop_requirements)
		| pr_classification == new_pr_classification
			= (changed, prop_requirements)
			= (True, { prop_requirements & [type_index].pr_classification = new_pr_classification })
	
	examine_type_application {rta_index, rta_classification = arg_classes} (cumm_class, prop_requirements)
		# (pr_classification, prop_requirements) = prop_requirements![rta_index].pr_classification
		  cumm_class = determine_cummulative_prop pr_classification arg_classes 0 cumm_class
		= (cumm_class, prop_requirements)
	where
		determine_cummulative_prop prop_class [arg_class : arg_classes] type_index cumm_class
			| IsPropagating prop_class type_index
				= determine_cummulative_prop prop_class arg_classes (inc type_index) (arg_class bitor cumm_class)
				= determine_cummulative_prop prop_class arg_classes (inc type_index) cumm_class
		determine_cummulative_prop prop_class [] type_index cumm_class
			= cumm_class
	
	bind_type_vars_to_props [] group_vars kinds props_of_group_vars (rev_hio_props, type_var_heap)
		= (rev_hio_props, type_var_heap)
	bind_type_vars_to_props [{atv_variable={tv_info_ptr}}: tvs] [gv : gvs] [tk : tks] props_of_group_vars (rev_hio_props, type_var_heap)
		# prop = retrieve_prop gv props_of_group_vars
		  (var_info, type_var_heap) = readPtr tv_info_ptr type_var_heap
		| IsArrowKind tk
			= bind_type_vars_to_props tvs gvs tks props_of_group_vars ([prop:rev_hio_props], type_var_heap <:= (tv_info_ptr, TVI_PropClass gv prop var_info))
			= bind_type_vars_to_props tvs gvs tks props_of_group_vars (rev_hio_props, type_var_heap <:= (tv_info_ptr, TVI_PropClass gv prop var_info))
	where
		retrieve_prop this_gv [(gv,prop) : props ]
			| this_gv == gv
				= prop
				= retrieve_prop this_gv props
		retrieve_prop this_gv [ ]
			= PropClass

	restore_binds_of_type_var {atv_variable={tv_info_ptr}} type_var_heap
		# (TVI_PropClass _ _ old_info, type_var_heap) = readPtr tv_info_ptr type_var_heap
		= type_var_heap <:= (tv_info_ptr, old_info)

	prop_class_of_type_def :: !Int !TypeRhs !Int !{#CommonDefs} !*PropClassState -> (!PropClassification,!*PropClassState)
	prop_class_of_type_def module_index (AlgType conses) group_nr ci pcs
		= prop_class_of_type_conses module_index conses group_nr ci NoPropClass pcs
	prop_class_of_type_def _ (SynType {at_type}) group_nr ci pcs
		# (prop_class, _, pcs) = propClassOfType at_type group_nr ci pcs
		= (prop_class, pcs)
	prop_class_of_type_def module_index (RecordType {rt_constructor}) group_nr ci pcs
		= prop_class_of_type_cons module_index rt_constructor group_nr ci NoPropClass pcs
	prop_class_of_type_def module_index (NewType constructor) group_nr ci pcs
		= prop_class_of_type_cons module_index constructor group_nr ci NoPropClass pcs
	prop_class_of_type_def _ (AbstractType properties) _ _ pcs
		= (PropClass, pcs)
	prop_class_of_type_def _ (AbstractSynType properties _) _ _ pcs
		= (PropClass, pcs)
	prop_class_of_type_def module_index (ExtensibleAlgType conses) group_nr ci pcs
		= (PropClass, pcs)
	prop_class_of_type_def module_index (AlgConses conses _) group_nr ci pcs
		= (PropClass, pcs)

	prop_class_of_type_conses module_index [{ds_index}:conses] group_nr ci cumm_prop_class pcs
		#! cons_def = ci.[module_index].com_cons_defs.[ds_index]
		#  (cumm_prop_class, pcs) = prop_class_of_type_of_list cons_def.cons_type.st_args group_nr ci cumm_prop_class pcs
		= prop_class_of_type_conses module_index conses group_nr ci cumm_prop_class pcs
	prop_class_of_type_conses module_index [] _ _ cumm_prop_class pcs
		= (cumm_prop_class, pcs)

	prop_class_of_type_cons module_index {ds_index} group_nr ci cumm_prop_class pcs
		#! cons_def = ci.[module_index].com_cons_defs.[ds_index]
		= prop_class_of_type_of_list cons_def.cons_type.st_args group_nr ci cumm_prop_class pcs

	prop_class_of_type_of_list [] _ _ cumm_prop_class pcs
		= (cumm_prop_class, pcs)
	prop_class_of_type_of_list [{at_type} : types] group_nr ci cumm_prop_class pcs
		# (prop_class, _, pcs) = propClassOfType at_type group_nr ci pcs
		= prop_class_of_type_of_list types group_nr ci (cumm_prop_class bitor prop_class) pcs


IndexToPropClass index :== 1 << index
IsPropagating prop_class_of_type type_index :== prop_class_of_type == (prop_class_of_type bitor IndexToPropClass type_index)
AdjustPropClass prop_class arity :== prop_class >> arity

propClassOfTypeVariable :: !TypeVar !{#CommonDefs} !*PropClassState -> (!PropClassification, !PropClassification, !*PropClassState)
propClassOfTypeVariable {tv_info_ptr} ci pcs=:{pcs_type_var_heap}
	# (var_info, pcs_type_var_heap) = readPtr tv_info_ptr pcs_type_var_heap
	  pcs = { pcs & pcs_type_var_heap = pcs_type_var_heap }
	= case var_info of
		TVI_PropClass group_var_index var_class _
			-> (IndexToPropClass group_var_index, var_class, pcs)
		_
			-> (NoPropClass, PropClass, pcs)

propClassOfType_for_TA :: Int Int [AType] !Int !{#CommonDefs} !*PropClassState -> (!PropClassification, !PropClassification, !*PropClassState)
propClassOfType_for_TA glob_module glob_object types group_nr ci pcs
	# (td_info=:{tdi_group_nr,tdi_index_in_group,tdi_kinds}, pcs) = pcs!pcs_type_def_infos.[glob_module].[glob_object]
	| tdi_group_nr == group_nr
		= prop_class_of_type_list_of_rec_type types tdi_index_in_group ci [] pcs 
		# {td_arity} = ci.[glob_module].com_type_defs.[glob_object]
		  (prop_classes, hio_props, pcs) = collect_prop_classes_of_type_list types tdi_kinds group_nr ci pcs 
		  (type_class, pcs_type_var_heap, pcs_type_def_infos)
		  		= determinePropClassOfTypeDef glob_object glob_module td_info hio_props ci pcs.pcs_type_var_heap pcs.pcs_type_def_infos
		  (prop_class, pcs) = determine_cummulative_prop types tdi_kinds type_class prop_classes 0 group_nr ci NoPropClass
		  							{ pcs & pcs_type_var_heap = pcs_type_var_heap, pcs_type_def_infos = pcs_type_def_infos } 
		= (prop_class, AdjustPropClass type_class td_arity, pcs)
where

	prop_class_of_type_list_of_rec_type [t : ts] tmp_type_index ci rev_prop_classes pcs
		# (prop_class, type_class, pcs) = propClassOfType t.at_type group_nr ci pcs
		= prop_class_of_type_list_of_rec_type ts tmp_type_index ci [ prop_class : rev_prop_classes ] pcs
	prop_class_of_type_list_of_rec_type [] tmp_type_index ci rev_prop_classes pcs=:{pcs_rec_appls}
		# rta = { rta_index = tmp_type_index, rta_classification = reverse rev_prop_classes }
		= (NoPropClass, PropClass, { pcs & pcs_rec_appls = [ rta : pcs_rec_appls ] })

	collect_prop_classes_of_type_list [t : ts] [tk : tks] group_nr ci pcs
		| IsArrowKind tk
			# (prop_class, type_class, pcs) = propClassOfType t.at_type group_nr ci pcs
			  (prop_classes, hio_props, pcs) = collect_prop_classes_of_type_list ts tks group_nr ci pcs
			= ([prop_class : prop_classes], [type_class : hio_props], pcs)
			= collect_prop_classes_of_type_list ts tks group_nr ci pcs
	collect_prop_classes_of_type_list [] _ _ ci pcs
		= ([], [], pcs)

	determine_cummulative_prop [t : ts] [tk : tks] prop_class hio_prop_classes type_index group_nr ci cumm_class pcs
		| IsArrowKind tk
			# [pc : hio_prop_classes] = hio_prop_classes
			= determine_cummulative_prop ts tks prop_class hio_prop_classes (inc type_index) group_nr ci (pc bitor cumm_class) pcs
		| IsPropagating prop_class type_index 
			# (pc, _, pcs) = propClassOfType t.at_type group_nr ci pcs
			= determine_cummulative_prop ts tks prop_class hio_prop_classes (inc type_index) group_nr ci (pc bitor cumm_class) pcs
			= determine_cummulative_prop ts tks prop_class hio_prop_classes (inc type_index) group_nr ci cumm_class pcs
	determine_cummulative_prop [] _ prop_class hio_prop_classes type_index group_nr ci cumm_class pcs
		= (cumm_class, pcs)

propClassOfType :: !Type !Int !{#CommonDefs} !*PropClassState -> (!PropClassification, !PropClassification, !*PropClassState)
propClassOfType (TV tv) _ ci pcs
	= propClassOfTypeVariable tv ci pcs

propClassOfType (TA {type_index = {glob_module, glob_object}} types) group_nr ci pcs
	= propClassOfType_for_TA glob_module glob_object types group_nr ci pcs

propClassOfType (TAS {type_index = {glob_module, glob_object}} types _) group_nr ci pcs
	= propClassOfType_for_TA glob_module glob_object types group_nr ci pcs

propClassOfType (CV tv :@: types) group_nr ci pcs
	# (prop_class, type_class, pcs) = propClassOfTypeVariable tv ci pcs
	  (prop_class, pcs) = prop_class_of_type_list types type_class 0 group_nr ci prop_class pcs
	= (prop_class, NoPropClass, pcs)
where	  	
	prop_class_of_type_list [{at_type} : types] cv_prop_class type_index group_nr ci cumm_class pcs
		| IsPropagating cv_prop_class type_index 
			# (pc, _, pcs) = propClassOfType at_type group_nr ci pcs
			= prop_class_of_type_list types cv_prop_class (inc type_index) group_nr ci (cumm_class bitor pc) pcs
			= prop_class_of_type_list types cv_prop_class (inc type_index) group_nr ci cumm_class pcs
	prop_class_of_type_list [] _ _ _ _ cumm_class pcs
		= (cumm_class, pcs)

propClassOfType (TFA vars type) group_nr ci pcs
	= propClassOfType type group_nr ci pcs
propClassOfType (TFAC vars type _) group_nr ci pcs
	= propClassOfType type group_nr ci pcs

propClassOfType _ _ _ pcs
	= (NoPropClass, NoPropClass, pcs)

