implementation module Clean.Types

from StdOverloaded import class ==(..), class length(..)
from StdClass import class Eq
import StdList
import StdMisc
import StdTuple
from StdString import instance == {#Char}
import StdBool
from StdFunc import o, id

from Data.Func import $
import Data.Functor
import Data.GenEq
import Data.List
import Data.Maybe

derive gEq Type, TypeRestriction, Kind

instance == Type where == a b = a === b
instance == TypeRestriction where == a b = a === b

subtypes :: !Type -> [Type]
subtypes t=:(Type s ts) = removeDup [t : flatten (map subtypes ts)]
subtypes t=:(Func is r tc) = removeDup [t : flatten (map subtypes [r:is])]
subtypes t=:(Cons c ts) = removeDup [t : flatten (map subtypes ts)]
subtypes t=:(Uniq t`) = removeDup [t : subtypes t`]
subtypes t=:(Forall vs t` tc) = removeDup [t : flatten (map subtypes [t`:vs])]
subtypes t=:(Var _) = [t]
subtypes t=:(Arrow mt) = [t:flatten (map subtypes (maybeToList mt))]
subtypes t=:(Strict t`) = [t:subtypes t`]

allRestrictions :: !Type -> [TypeRestriction]
allRestrictions (Type _ ts) = concatMap allRestrictions ts
allRestrictions (Func is r tc) = tc ++ concatMap allRestrictions [r:is]
allRestrictions (Cons _ ts) = concatMap allRestrictions ts
allRestrictions (Uniq t) = allRestrictions t
allRestrictions (Forall _ t tc) = tc ++ allRestrictions t
allRestrictions (Var _) = []
allRestrictions (Arrow t) = fromMaybe [] (allRestrictions <$> t)
allRestrictions (Strict t) = allRestrictions t

allVars :: (Type -> [TypeVar])
allVars = removeDup o map name o filter (\t -> isCons t || isVar t) o subtypes
where
	name :: !Type -> TypeVar
	name (Cons v _) = v
	name (Var v) = v
	name _ = abort "error in allVars\n"

allUniversalVars :: !Type -> [TypeVar]
allUniversalVars (Forall vs t tc) = removeDup (flatten (map allVars vs) ++ allUniversalVars t)
allUniversalVars (Type _ ts) = removeDup (flatten (map allUniversalVars ts))
allUniversalVars (Func is r _) = removeDup (flatten (map allUniversalVars [r:is]))
allUniversalVars (Cons _ ts) = removeDup (flatten (map allUniversalVars ts))
allUniversalVars (Uniq t) = allUniversalVars t
allUniversalVars (Var _) = []
allUniversalVars (Arrow (Just t)) = allUniversalVars t
allUniversalVars (Arrow Nothing)  = []
allUniversalVars (Strict t) = allUniversalVars t

isVar :: !Type -> Bool
isVar t = t=:(Var _)

fromVar :: !Type -> TypeVar
fromVar t = case t of
	Var v -> v
	_     -> abort "error in fromVar\n"

fromVarLenient :: !Type -> TypeVar
fromVarLenient t = case t of
	Var v    -> v
	Cons v _ -> v
	Uniq t   -> fromVarLenient t
	Strict t -> fromVarLenient t
	_        -> abort "missing case in fromVarLenient\n"


isCons :: !Type -> Bool
isCons t = t=:(Cons _ _)

isCons` :: TypeVar !Type -> Bool
isCons` v t = case t of
	Cons v` _ -> v == v`
	_         -> False

isVarOrCons` :: TypeVar !Type -> Bool
isVarOrCons` v t = case t of
	Var v`    -> v == v`
	Cons v` _ -> v == v`
	_         -> False

isType :: !Type -> Bool
isType t = t=:(Type _ _)

isFunc :: !Type -> Bool
isFunc t = t=:(Func _ _ _)

isUniq :: !Type -> Bool
isUniq t = t=:(Uniq _)

isForall :: !Type -> Bool
isForall t = t=:(Forall _ _ _)

fromForall :: !Type -> Type
fromForall t = case t of
	Forall _ t _ -> t
	_            -> abort "fromForall called on non-Forall\n"

isArrow :: !Type -> Bool
isArrow t = t=:(Arrow _)

fromArrow :: !Type -> Maybe Type
fromArrow t = case t of
	Arrow t -> t
	_       -> abort "fromArrow called on non-Arrow\n"

fromUnifyingAssignment :: !UnifyingAssignment -> TVAssignment
fromUnifyingAssignment (LeftToRight x) = x
fromUnifyingAssignment (RightToLeft x) = x

arity :: !Type -> Int
arity (Type _ ts) = length ts
arity (Func is _ _) = length is
arity (Var _) = 0
arity (Cons _ ts) = length ts
arity (Strict t) = arity t
arity (Uniq _) = abort "what is the arity of Uniq?\n" // TODO
arity (Forall _ _ _) = abort "what is the arity of Forall?\n" // TODO
arity (Arrow _) = abort "what is the arity of Arrow?\n" // TODO

removeTypeContexts :: !Type -> Type
removeTypeContexts (Type s ts) = Type s $ map removeTypeContexts ts
removeTypeContexts (Func is r _) = Func (map removeTypeContexts is) (removeTypeContexts r) []
removeTypeContexts (Var v) = Var v
removeTypeContexts (Cons v ts) = Cons v $ map removeTypeContexts ts
removeTypeContexts (Uniq t) = Uniq $ removeTypeContexts t
removeTypeContexts (Forall ts t _) = Forall (map removeTypeContexts ts) (removeTypeContexts t) []
removeTypeContexts (Arrow t) = Arrow (removeTypeContexts <$> t)
removeTypeContexts (Strict t) = Strict (removeTypeContexts t)

constructorsToFunctions :: !TypeDef -> [(String,Type,Maybe Priority)]
constructorsToFunctions {td_name,td_uniq,td_args,td_rhs} = case td_rhs of
	TDRCons _ conses     -> map consfun conses
	TDRMoreConses conses -> map consfun conses
	TDRNewType cons      -> [consfun cons]
	_                    -> []
where
	consfun :: !Constructor -> (String, Type, Maybe Priority)
	consfun c = (c.cons_name, Func c.cons_args ret c.cons_context, c.cons_priority)
	where ret = if td_uniq Uniq id $ Type td_name td_args

recordsToFunctions :: !TypeDef -> [(String,Type)]
recordsToFunctions {td_name,td_uniq,td_args,td_rhs=TDRRecord _ _ fields}
	= [(f.rf_name, Func [arg] f.rf_type []) \\ f <- fields]
where arg = if td_uniq Uniq id $ Type td_name td_args
recordsToFunctions _ = []

td_name :: !TypeDef -> String
td_name {td_name} = td_name

td_uniq :: !TypeDef -> Bool
td_uniq {td_uniq} = td_uniq

td_rhs :: !TypeDef -> TypeDefRhs
td_rhs {td_rhs} = td_rhs

typedef :: !String !Bool ![Type] !TypeDefRhs -> TypeDef
typedef name uniq args rhs
	= {td_name=name, td_uniq=uniq, td_args=args, td_rhs=rhs}

constructor :: !String ![Type] ![TypeVar] !TypeContext !(Maybe Priority) -> Constructor
constructor name args exi_vars tc pri
	= {cons_name=name, cons_args=args, cons_exi_vars=exi_vars, cons_context=tc, cons_priority=pri}

recordfield :: !String !Type -> RecordField
recordfield selector type = {rf_name=selector, rf_type=type}

removeDupTypedefs :: ![TypeDef] -> [TypeDef]
removeDupTypedefs [] = []
removeDupTypedefs [td:tds]
	= [td:removeDupTypedefs $ filter (\d -> d.td_name <> td.td_name) tds]

typeRhsRestrictions :: !TypeDefRhs -> [TypeRestriction]
typeRhsRestrictions (TDRCons _ cs) = flatten [c.cons_context \\ c <- cs]
typeRhsRestrictions (TDRNewType c) = c.cons_context
typeRhsRestrictions (TDRMoreConses cs) = flatten [c.cons_context \\ c <- cs]
typeRhsRestrictions (TDRRecord _ _ _) = []
typeRhsRestrictions (TDRSynonym _) = []
typeRhsRestrictions (TDRAbstract _) = []
typeRhsRestrictions (TDRAbstractSynonym _) = []
