definition module typeproperties

import StdInt, StdClass

import general

::	TypeClassification

EmptyTypeClassification :: TypeClassification

::	SignClassification	=
	{	sc_pos_vect	:: !BITVECT
	,	sc_neg_vect	:: !BITVECT
	}

::	PropClassification	:== BITVECT

TopSignClass	:== { sc_pos_vect = bitnot 0, sc_neg_vect = bitnot 0 }
ArrowSignClass	:== { sc_pos_vect = 2, sc_neg_vect = 1 }

:: Sign =
	{	pos_sign :: !Bool
	,	neg_sign :: !Bool
	}


TopSign			:== { pos_sign = True,	neg_sign = True }
BottomSign		:== { pos_sign = False,	neg_sign = False }
PositiveSign	:== { pos_sign = True,	neg_sign = False }
NegativeSign	:== { pos_sign = False,	neg_sign = True }

signClassToSign :: !SignClassification !Int -> Sign

/*
IsPositive sign_class index :== sign_class.sc_pos_vect bitand (1 << index) <> 0
IsNegative sign_class index :== sign_class.sc_neg_vect bitand (1 << index) <> 0
*/

instance <<< Sign
instance * Sign

::	TypeSign key =
	{	ts_cons_var_signs	:: !key
	,	ts_type_sign		:: !SignClassification
	}

::	TypeProp key =
	{	ts_cons_var_props	:: !key
	,	ts_type_prop		:: !PropClassification
	}

retrieveSignClassification :: ![SignClassification] !TypeClassification -> Optional (TypeSign [SignClassification])
retrievePropClassification :: ![PropClassification] !TypeClassification -> Optional (TypeProp [PropClassification])

addSignClassification :: ![SignClassification] !SignClassification !TypeClassification -> TypeClassification
addPropClassification :: ![PropClassification] !PropClassification !TypeClassification -> TypeClassification
