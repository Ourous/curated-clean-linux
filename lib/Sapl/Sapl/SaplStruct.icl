implementation module Sapl.SaplStruct

import StdEnv
import Data.Error, Data.Maybe
import Sapl.SaplTokenizer

ltVarByName :: !SaplVar !SaplVar -> Bool
ltVarByName a b = unpackVar a < unpackVar b

eqVarByName :: !SaplVar !SaplVar -> Bool
eqVarByName a b = unpackVar a == unpackVar b

eqVarByNameLevel :: !SaplVar !SaplVar -> Bool
eqVarByNameLevel a b = unpackVar a == unpackVar b && unpackLevel a == unpackLevel b

ltVarByNameLevel :: !SaplVar !SaplVar -> Bool
ltVarByNameLevel a b = unpackVar a < unpackVar b || (unpackVar a == unpackVar b && unpackLevel a < unpackLevel b)

unpackLevel (NormalVar _ level) = level 
unpackLevel (StrictVar _ level) = level
unpackLevel (GlobalVar _) = 0

instance toString SaplVar
where
	toString (NormalVar name 0) = name
	toString (NormalVar name level) = name +++ "_" +++ toString level
	toString (StrictVar name 0) = "!" +++ name
	toString (StrictVar name level) = "!" +++ name +++ "_" +++ toString level
    toString (GlobalVar name) = name

removeTypeInfo :: !SaplTypedVar -> SaplVar
removeTypeInfo (TypedVar var _) = var

instance eqStrictVar SaplVar
where
	eqStrictVar :: !String !SaplVar -> Bool
	eqStrictVar name1 (StrictVar name2 _) = name1 == name2 
	eqStrictVar _ _ = False

instance eqStrictVar SaplTypedVar
where
	eqStrictVar :: !String !SaplTypedVar -> Bool
	eqStrictVar name (TypedVar var _) = eqStrictVar name var

instance isStrictVar SaplVar
where
	isStrictVar :: !SaplVar -> Bool
	isStrictVar (StrictVar _ _) = True
	isStrictVar _ = False

instance isStrictVar SaplTypedVar
where
	isStrictVar :: !SaplTypedVar -> Bool
	isStrictVar (TypedVar var _) = isStrictVar var

instance toNormalVar SaplVar
where
	toNormalVar :: !SaplVar -> SaplVar
	toNormalVar (StrictVar name level) = (NormalVar name level)
	toNormalVar v = v

instance toNormalVar SaplTypedVar
where
	toNormalVar :: !SaplTypedVar -> SaplTypedVar
	toNormalVar (TypedVar var type) = TypedVar (toNormalVar var) type

instance toStrictVar SaplVar
where
	toStrictVar :: !SaplVar -> SaplVar
	toStrictVar (NormalVar name level) = (StrictVar name level)
	toStrictVar v = v

instance toStrictVar SaplTypedVar
where
	toStrictVar :: !SaplTypedVar -> SaplTypedVar
	toStrictVar (TypedVar var type) = TypedVar (toStrictVar var) type

instance unpackVar SaplVar
where
	unpackVar :: !SaplVar -> String
	unpackVar (NormalVar name _) = name
	unpackVar (StrictVar name _) = name
    unpackVar (GlobalVar name) = name

instance unpackVar SaplTypedVar
where
	unpackVar :: !SaplTypedVar -> String
	unpackVar (TypedVar var _) = unpackVar var

unpackBindVar :: !SaplLetDef -> SaplTypedVar
unpackBindVar (SaplLetDef typedVar _) = typedVar

unpackBindExpr :: !SaplLetDef -> SaplTerm
unpackBindExpr (SaplLetDef _ expr) = expr

unpackConsName :: !SaplPattern -> Maybe String
unpackConsName (PCons cons _) = Just cons
unpackConsName _ = Nothing

toStrictBind :: !SaplLetDef -> SaplLetDef
toStrictBind (SaplLetDef (TypedVar var type) body) = SaplLetDef (TypedVar (toStrictVar var) type) body

isConsPattern :: !SaplPattern -> Bool
isConsPattern (PCons _ _) = True
isConsPattern _ = False

isDefaultPattern :: !SaplPattern -> Bool
isDefaultPattern PDefault = True
isDefaultPattern _ = False
