implementation module Clean.Types.CoclTransform

from StdList import map

from Clean.Types import class toType, class toTypeVar, class toTypeDef,
	class toTypeDefRhs, class toConstructor, class toRecordField,
	::TypeRestriction
import qualified Clean.Types
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Functor
from Data.Maybe import :: Maybe (..), instance Functor Maybe,
	instance pure Maybe, instance <*> Maybe, instance Monad Maybe
import qualified Data.Map as M

import syntax
import qualified syntax

instance 'Clean.Types'.toTypeContext ['syntax'.TypeContext]
where
	toTypeContext context
		= ['Clean.Types'.Instance gds.glob_object.ds_ident.id_name (map 'Clean.Types'.toType tc_types)
		     \\ {tc_class=(TCClass gds),tc_types} <- context] ++
		  ['Clean.Types'.Derivation gtc_generic.glob_object.ds_ident.id_name ('Clean.Types'.toType t)
		     \\ {tc_class=(TCGeneric {gtc_generic}),tc_types=[t]} <- context]

instance 'Clean.Types'.toTypeContext 'syntax'.TypeContext where toTypeContext tc = 'Clean.Types'.toTypeContext [tc]

instance toType 'syntax'.ATypeVar
where
	toType {atv_attribute=TA_Unique,atv_variable}
		= 'Clean.Types'.Uniq ('Clean.Types'.Var ('Clean.Types'.toTypeVar atv_variable))
	toType {atv_variable} = 'Clean.Types'.Var ('Clean.Types'.toTypeVar atv_variable)

instance toType 'syntax'.AType
where
	toType {at_type,at_attribute}
		| at_attribute == TA_Unique = 'Clean.Types'.Uniq ('Clean.Types'.toType at_type)
		| otherwise = 'Clean.Types'.toType at_type

instance toType 'syntax'.Type
where
	toType (TA tsi ats) = case tsi.type_ident.id_name of
		"_String" = 'Clean.Types'.Type "String" []
		type_name = 'Clean.Types'.Type tsi.type_ident.id_name (map 'Clean.Types'.toType ats)
	toType (TAS tsi ats ss) = 'Clean.Types'.Type tsi.type_ident.id_name
		[if s 'Clean.Types'.Strict id ('Clean.Types'.toType t) \\ t <- ats & s <- strictnessListToBools ss]
	toType (TB bt) = 'Clean.Types'.Type (toString bt) []
	toType (TV tv) = 'Clean.Types'.Var tv.tv_ident.id_name
	toType (GTV tv) = 'Clean.Types'.Var tv.tv_ident.id_name
	toType (t1 --> t2) = 'Clean.Types'.Func ['Clean.Types'.toType t1] ('Clean.Types'.toType t2) []
	toType ((CV cv) :@: ats) = 'Clean.Types'.Cons cv.tv_ident.id_name (map 'Clean.Types'.toType ats)
	toType (TFAC tvas t tc) = 'Clean.Types'.Forall (map 'Clean.Types'.toType tvas) ('Clean.Types'.toType t) ('Clean.Types'.toTypeContext tc)
	toType TArrow = 'Clean.Types'.Arrow Nothing
	toType (TArrow1 t) = 'Clean.Types'.Arrow (Just ('Clean.Types'.toType t))
	toType (TQualifiedIdent _ s ts) = 'Clean.Types'.Type s (map 'Clean.Types'.toType ts)
	toType _ = abort "CoclUtils: unimplemented Type\n"

instance toType 'syntax'.SymbolType
where
	toType {st_args,st_result,st_context,st_args_strictness}
		= 'Clean.Types'.Func [if s 'Clean.Types'.Strict id ('Clean.Types'.toType t) \\ t <- st_args & s <- strictnessListToBools st_args_strictness]
			('Clean.Types'.toType st_result) ('Clean.Types'.toTypeContext st_context)

instance toTypeVar 'syntax'.TypeVar where toTypeVar {tv_ident} = tv_ident.id_name

instance toTypeDef 'syntax'.ParsedTypeDef
where
	toTypeDef {td_ident,td_attribute,td_args,td_rhs}
		= 'Clean.Types'.typedef td_ident.id_name
			(td_attribute == TA_Unique)
			(map 'Clean.Types'.toType td_args)
			('Clean.Types'.toTypeDefRhs td_rhs)

instance toTypeDefRhs 'syntax'.RhsDefsOfType
where
	toTypeDefRhs (ConsList pcs)
		= 'Clean.Types'.TDRCons False (map 'Clean.Types'.toConstructor pcs)
	toTypeDefRhs (SelectorList id exi_vars _ pss)
		= 'Clean.Types'.TDRRecord id.id_name
			(map (\t -> 'Clean.Types'.toTypeVar t.atv_variable) exi_vars)
			(map 'Clean.Types'.toRecordField pss)
	toTypeDefRhs (TypeSpec atype)
		= 'Clean.Types'.TDRSynonym ('Clean.Types'.toType atype)
	toTypeDefRhs (NewTypeCons cons)
		= 'Clean.Types'.TDRNewType ('Clean.Types'.toConstructor cons)
	toTypeDefRhs (EmptyRhs _)
		= 'Clean.Types'.TDRAbstract Nothing
	toTypeDefRhs (AbstractTypeSpec _ atype)
		= 'Clean.Types'.TDRAbstractSynonym ('Clean.Types'.toType atype)
	toTypeDefRhs (ExtensibleConses pcs)
		= 'Clean.Types'.TDRCons True (map 'Clean.Types'.toConstructor pcs)
	toTypeDefRhs (MoreConses id pcs)
		= 'Clean.Types'.TDRMoreConses (map 'Clean.Types'.toConstructor pcs)

instance toConstructor 'syntax'.ParsedConstructor
where
	toConstructor {pc_cons_ident,pc_arg_types,pc_args_strictness,pc_exi_vars,pc_context,pc_cons_prio}
		= 'Clean.Types'.constructor pc_cons_ident.id_name
			[if s 'Clean.Types'.Strict id ('Clean.Types'.toType t) \\ t <- pc_arg_types & s <- strictnessListToBools pc_args_strictness]
			(map (\t -> 'Clean.Types'.toTypeVar t.atv_variable) pc_exi_vars)
			('Clean.Types'.toTypeContext pc_context)
			('Clean.Types'.toMaybePriority pc_cons_prio)

instance 'Clean.Types'.toMaybePriority 'syntax'.Priority
where
	toMaybePriority NoPrio              = Nothing
	toMaybePriority (Prio LeftAssoc i)  = Just ('Clean.Types'.LeftAssoc i)
	toMaybePriority (Prio RightAssoc i) = Just ('Clean.Types'.RightAssoc i)
	toMaybePriority (Prio NoAssoc i)    = Just ('Clean.Types'.NoAssoc i)

instance toRecordField 'syntax'.ParsedSelector
where
	toRecordField {ps_selector_ident,ps_field_type,ps_field_annotation}
		= 'Clean.Types'.recordfield ps_selector_ident.id_name (if ps_field_annotation=:AN_Strict 'Clean.Types'.Strict id ('Clean.Types'.toType ps_field_type))

strictnessListToBools :: !StrictnessList -> [Bool]
strictnessListToBools NotStrict        = repeat False
strictnessListToBools (Strict i)       = [i bitand (1 << e) <> 0 \\ e <- [0..31]]
strictnessListToBools (StrictList i l) = strictnessListToBools (Strict i) ++ strictnessListToBools l

:: TypeDerivState =
	{ tds_var_index         :: Int
	, tds_allows_new_idents :: Bool
	, tds_map               :: 'M'.Map String 'Clean.Types'.Type
	}
tds_var_index         tds = tds.tds_var_index
tds_allows_new_idents tds = tds.tds_allows_new_idents
tds_map               tds = tds.tds_map

class coclType a :: !a -> StateT TypeDerivState Maybe 'Clean.Types'.Type

store :: !String !'Clean.Types'.Type -> StateT TypeDerivState Maybe 'Clean.Types'.Type
store id t = modify (\tds -> {tds & tds_map='M'.put id t tds.tds_map}) $> t

allowNewIdents :: !Bool -> StateT TypeDerivState Maybe ()
allowNewIdents b = modify \tds -> {tds & tds_allows_new_idents=b}

fail :: StateT a Maybe b
fail = StateT \_ -> Nothing

pdType :: !'syntax'.ParsedDefinition -> Maybe 'Clean.Types'.Type
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
			store id ('Clean.Types'.Func argts rt [])
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
		var :: Int -> 'Clean.Types'.Type
		var n = 'Clean.Types'.Var (if (n < 26) {toChar n + 'a'} ("v" +++ toString n))

	coclType _ = fail

instance coclType 'syntax'.BasicValue
where
	coclType (BVI _)   = pure ('Clean.Types'.Type "Int" [])
	coclType (BVInt _) = pure ('Clean.Types'.Type "Int" [])
	coclType (BVC _)   = pure ('Clean.Types'.Type "Char" [])
	coclType (BVB _)   = pure ('Clean.Types'.Type "Bool" [])
	coclType (BVR _)   = pure ('Clean.Types'.Type "Real" [])
	coclType (BVS _)   = pure ('Clean.Types'.Type "String" [])
