implementation module typeproperties

import StdEnv

import general, compare_types

::	TypeClassification = 
	{	tc_signs	:: TypeSignTree
	,	tc_props	:: TypePropTree
	}

::	SignClassification	=
	{	sc_pos_vect	:: !BITVECT
	,	sc_neg_vect	:: !BITVECT
	}
/*
IsPositive sign_class index :== sign_class.sc_pos_vect bitand (1 << index) <> 0
IsNegative sign_class index :== sign_class.sc_neg_vect bitand (1 << index) <> 0
*/
::	PropClassification	:== BITVECT

TopSignClass	:== { sc_pos_vect = bitnot 0, sc_neg_vect = bitnot 0 }
ArrowSignClass	:== { sc_pos_vect = 2, sc_neg_vect = 1 }
PosSignClass	:== { sc_pos_vect = bitnot 0, sc_neg_vect = 0 }

:: Sign =
	{	pos_sign :: !Bool
	,	neg_sign :: !Bool
	}


TopSign			:== { pos_sign = True,	neg_sign = True }
BottomSign		:== { pos_sign = False,	neg_sign = False }
PositiveSign	:== { pos_sign = True,	neg_sign = False }
NegativeSign	:== { pos_sign = False,	neg_sign = True }

::	TypeSign key =
	{	ts_cons_var_signs	:: !key
	,	ts_type_sign		:: !SignClassification
	}

::	TypeProp key =
	{	ts_cons_var_props	:: !key
	,	ts_type_prop		:: !PropClassification
	}

::	VarBind a key =
	{	vb_number			:: !key
	,	vb_value			:: !a
	}

::	TypeSignTree 	:== BinTree (TypeSign [SignClassification])
::	TypePropTree 	:== BinTree (TypeProp [PropClassification])
::	EnvTree	a 		:== BinTree (VarBind a Int)

::	BinTree a = BT_Node !a !(BinTree a) !(BinTree a) | BT_Empty

class key m :: (m a) -> a

instance key TypeSign
where
	key {ts_cons_var_signs} = ts_cons_var_signs

instance key TypeProp
where
	key {ts_cons_var_props} = ts_cons_var_props

instance key (VarBind a)
where
	key {vb_number} = vb_number

EmptyTypeClassification :: TypeClassification
EmptyTypeClassification = { tc_signs = BT_Empty, tc_props = BT_Empty }

treeInsert :: !k !(m k) !(BinTree (m k)) -> BinTree (m k) | =< k & key m
treeInsert new_key el BT_Empty
	= BT_Node el BT_Empty BT_Empty
treeInsert new_key new_el tree=:(BT_Node el left right)
	# cmp = new_key =< key el
	| cmp == Smaller
		= BT_Node el (treeInsert new_key new_el left) right
		= BT_Node el left (treeInsert new_key new_el right)
	
treeRetrieve :: !k !(BinTree (m k)) -> Optional (m k) | =< k & key m
treeRetrieve search_key BT_Empty
	= No
treeRetrieve search_key tree=:(BT_Node el left right)
	# cmp = search_key =< key el
	| cmp == Equal
		= Yes el
	| cmp == Smaller
		= treeRetrieve search_key left
		= treeRetrieve search_key right

signClassToSign :: !SignClassification !Int -> Sign
signClassToSign {sc_pos_vect,sc_neg_vect} index
	= { pos_sign = sc_pos_vect bitand (1 << index) <> 0, neg_sign = sc_neg_vect bitand (1 << index) <> 0}

instance <<< Sign
where
	(<<<) file {pos_sign,neg_sign}
		| pos_sign
			| neg_sign
				= file <<< "T"
				= file <<< "+"
		| neg_sign
			= file <<< "-"
			= file <<< "L"
		
instance =< SignClassification
where
	=< sc1 sc2
		| sc1.sc_pos_vect == sc2.sc_pos_vect
			| sc1.sc_neg_vect == sc2.sc_neg_vect
				= Equal
			| sc1.sc_neg_vect < sc2.sc_neg_vect
				= Smaller
				= Greater
		| sc1.sc_pos_vect < sc2.sc_pos_vect
			= Smaller
			= Greater

retrieveSignClassification :: ![SignClassification] !TypeClassification -> Optional (TypeSign [SignClassification])
retrieveSignClassification cons_classes {tc_signs}
	= treeRetrieve cons_classes tc_signs

addSignClassification :: ![SignClassification] !SignClassification !TypeClassification -> TypeClassification
addSignClassification hio_signs sign_class tc=:{tc_signs}
	= { tc & tc_signs = treeInsert hio_signs { ts_cons_var_signs = hio_signs, ts_type_sign = sign_class } tc_signs }

retrievePropClassification :: ![PropClassification] !TypeClassification -> Optional (TypeProp [PropClassification])
retrievePropClassification cons_classes {tc_props}
	= treeRetrieve cons_classes tc_props

addPropClassification :: ![PropClassification] !PropClassification !TypeClassification -> TypeClassification
addPropClassification hio_props prop_class tc=:{tc_props}
	= { tc & tc_props = treeInsert hio_props { ts_cons_var_props = hio_props, ts_type_prop = prop_class } tc_props }

instance * Sign
where
	(*) sign1 sign2
		| sign1.pos_sign
			| sign1.neg_sign
				= sign1
				= sign2
		| sign1.neg_sign
			= { pos_sign = sign2.neg_sign, neg_sign = sign2.pos_sign }
			= sign1

/*			
		= {	pos_sign = sign1.pos_sign * sign2.pos_sign || sign1.neg_sign * sign2.neg_sign,
			neg_sign = sign1.pos_sign * sign2.neg_sign || sign1.neg_sign * sign2.pos_sign }
		
instance * Bool
where
	(*) b1 b2 = b1 && b2 || not b1 && not b2

*/
