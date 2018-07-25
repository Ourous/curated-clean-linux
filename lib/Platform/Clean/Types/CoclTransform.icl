implementation module Clean.Types.CoclTransform

from StdList import map

from Clean.Types import class toType, class toTypeVar, class toTypeDef,
	class toTypeDefRhs, class toConstructor, class toRecordField,
	::TypeRestriction
import qualified Clean.Types as T
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor
from Data.Maybe import :: Maybe (..), instance Functor Maybe,
	instance Applicative Maybe, instance Monad Maybe
import qualified Data.Map as M

import syntax
import qualified syntax

instance 'T'.toTypeContext ['syntax'.TypeContext]
where
	toTypeContext context
		= ['T'.Instance gds.glob_object.ds_ident.id_name (map 'T'.toType tc_types)
		     \\ {tc_class=(TCClass gds),tc_types} <- context] ++
		  ['T'.Derivation gtc_generic.glob_object.ds_ident.id_name ('T'.toType t)
		     \\ {tc_class=(TCGeneric {gtc_generic}),tc_types=[t]} <- context]

instance 'T'.toTypeContext 'syntax'.TypeContext where toTypeContext tc = 'T'.toTypeContext [tc]

instance toType 'syntax'.ATypeVar
where
	toType {atv_attribute=TA_Unique,atv_variable}
		= 'T'.Uniq ('T'.Var ('T'.toTypeVar atv_variable))
	toType {atv_variable} = 'T'.Var ('T'.toTypeVar atv_variable)

instance toType 'syntax'.AType
where
	toType {at_type,at_attribute}
		| at_attribute == TA_Unique = 'T'.Uniq ('T'.toType at_type)
		| otherwise = 'T'.toType at_type

instance toType 'syntax'.Type
where
	toType (TA tsi ats) = case tsi.type_ident.id_name of
		"_String" = 'T'.Type "String" []
		type_name = 'T'.Type tsi.type_ident.id_name (map 'T'.toType ats)
	toType (TAS tsi ats _) = 'T'.Type tsi.type_ident.id_name (map 'T'.toType ats)
	toType (TB bt) = 'T'.Type (toString bt) []
	toType (TV tv) = 'T'.Var tv.tv_ident.id_name
	toType (GTV tv) = 'T'.Var tv.tv_ident.id_name
	toType (t1 --> t2) = 'T'.Func ['T'.toType t1] ('T'.toType t2) []
	toType ((CV cv) :@: ats) = 'T'.Cons cv.tv_ident.id_name (map 'T'.toType ats)
	toType (TFAC tvas t tc) = 'T'.Forall (map 'T'.toType tvas) ('T'.toType t) ('T'.toTypeContext tc)
	toType TArrow = 'T'.Arrow Nothing
	toType (TArrow1 t) = 'T'.Arrow (Just ('T'.toType t))
	toType (TQualifiedIdent _ s ts) = 'T'.Type s (map 'T'.toType ts)
	toType _ = abort "CoclUtils: unimplemented Type\n"

instance toType 'syntax'.SymbolType
where
	toType {st_args,st_result,st_context}
		= 'T'.Func (map 'T'.toType st_args) ('T'.toType st_result) ('T'.toTypeContext st_context)

instance toTypeVar 'syntax'.TypeVar where toTypeVar {tv_ident} = tv_ident.id_name

instance toTypeDef 'syntax'.ParsedTypeDef
where
	toTypeDef {td_ident,td_attribute,td_args,td_rhs}
		= 'T'.typedef td_ident.id_name
			(td_attribute == TA_Unique)
			(map 'T'.toType td_args)
			('T'.toTypeDefRhs td_rhs)

instance toTypeDefRhs 'syntax'.RhsDefsOfType
where
	toTypeDefRhs (ConsList pcs)
		= 'T'.TDRCons False (map 'T'.toConstructor pcs)
	toTypeDefRhs (SelectorList id exi_vars _ pss)
		= 'T'.TDRRecord id.id_name
			(map (\t -> 'T'.toTypeVar t.atv_variable) exi_vars)
			(map 'T'.toRecordField pss)
	toTypeDefRhs (TypeSpec atype)
		= 'T'.TDRSynonym ('T'.toType atype)
	toTypeDefRhs (NewTypeCons cons)
		= 'T'.TDRNewType ('T'.toConstructor cons)
	toTypeDefRhs (EmptyRhs _)
		= 'T'.TDRAbstract Nothing
	toTypeDefRhs (AbstractTypeSpec _ atype)
		= 'T'.TDRAbstractSynonym ('T'.toType atype)
	toTypeDefRhs (ExtensibleConses pcs)
		= 'T'.TDRCons True (map 'T'.toConstructor pcs)
	toTypeDefRhs (MoreConses id pcs)
		= 'T'.TDRMoreConses (map 'T'.toConstructor pcs)

instance toConstructor 'syntax'.ParsedConstructor
where
	toConstructor {pc_cons_ident,pc_arg_types,pc_exi_vars,pc_context,pc_cons_prio}
		= 'T'.constructor pc_cons_ident.id_name
			(map 'T'.toType pc_arg_types)
			(map (\t -> 'T'.toTypeVar t.atv_variable) pc_exi_vars)
			('T'.toTypeContext pc_context)
			('T'.toMaybePriority pc_cons_prio)

instance 'T'.toMaybePriority 'syntax'.Priority
where
	toMaybePriority NoPrio              = Nothing
	toMaybePriority (Prio LeftAssoc i)  = Just ('T'.LeftAssoc i)
	toMaybePriority (Prio RightAssoc i) = Just ('T'.RightAssoc i)
	toMaybePriority (Prio NoAssoc i)    = Just ('T'.NoAssoc i)

instance toRecordField 'syntax'.ParsedSelector
where
	toRecordField {ps_selector_ident,ps_field_type}
		= 'T'.recordfield ps_selector_ident.id_name ('T'.toType ps_field_type)

:: TypeDerivState =
	{ tds_var_index         :: Int
	, tds_allows_new_idents :: Bool
	, tds_map               :: 'M'.Map String 'T'.Type
	}
tds_var_index         tds = tds.tds_var_index
tds_allows_new_idents tds = tds.tds_allows_new_idents
tds_map               tds = tds.tds_map

class coclType a :: !a -> StateT TypeDerivState Maybe 'T'.Type

store :: !String !'T'.Type -> StateT TypeDerivState Maybe 'T'.Type
store id t = modify (\tds -> {tds & tds_map='M'.put id t tds.tds_map}) $> t

allowNewIdents :: !Bool -> StateT TypeDerivState Maybe ()
allowNewIdents b = modify \tds -> {tds & tds_allows_new_idents=b}

fail :: StateT a Maybe b
fail = StateT \_ -> Nothing

pdType :: !'syntax'.ParsedDefinition -> Maybe 'T'.Type
pdType pd = evalStateT (coclType pd)
	{ tds_var_index         = 0
	, tds_allows_new_idents = True
	, tds_map               = 'M'.newMap
	}

instance coclType 'syntax'.ParsedDefinition
where
	coclType (PD_Function _ {id_name=id} _ args {rhs_alts=UnGuardedExpr {ewl_expr}} _)
		= allowNewIdents True >>|
			mapM coclType args >>= \argts ->
			allowNewIdents False >>|
			coclType ewl_expr >>= \rt ->
			store id ('T'.Func argts rt [])
	coclType _
		= fail

instance coclType 'syntax'.ParsedExpr
where
	coclType (PE_Basic b) = coclType b
	coclType (PE_Ident id) = gets tds_map >>= \m -> case 'M'.get id.id_name m of
		Nothing -> gets tds_allows_new_idents >>= \allowed -> if allowed
			(gets tds_var_index >>= \i ->
				modify (\tds -> {tds & tds_var_index=i+1}) >>|
				let t = var i in store id.id_name t)
			fail
		Just t  -> pure t
	where
		var :: Int -> 'T'.Type
		var n = 'T'.Var (if (n < 26) {toChar n + 'a'} ("v" +++ toString n))

	coclType _ = fail

instance coclType 'syntax'.BasicValue
where
	coclType (BVI _)   = pure ('T'.Type "Int" [])
	coclType (BVInt _) = pure ('T'.Type "Int" [])
	coclType (BVC _)   = pure ('T'.Type "Char" [])
	coclType (BVB _)   = pure ('T'.Type "Bool" [])
	coclType (BVR _)   = pure ('T'.Type "Real" [])
	coclType (BVS _)   = pure ('T'.Type "String" [])
