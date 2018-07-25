implementation module Clean.Types.Util

import StdArray
import StdBool
from StdFunc import flip, id, o
import StdOrdList
import StdString
import StdTuple

import Clean.Types
import Control.Applicative
import Control.Monad
from Data.Func import $
import Data.Functor
import Data.GenEq
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Tuple
from Text import class Text (concat), instance Text String

(--) infixr 1 :: !a !b -> [String] | print a & print b
(--) a b = print False a ++ print False b
(+-) infixr 1 :: !a !b -> [String] | print a & print b
(+-) a b = print True a ++ print False b
(-+) infixr 1 :: !a !b -> [String] | print a & print b
(-+) a b = print False a ++ print True b
(+-+) infixr 1 :: !a !b -> [String] | print a & print b
(+-+) a b = print True a ++ print True b

printersperse :: !Bool !a ![b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

instance toInt Bool where toInt True = 1; toInt False = 0

instance print String where print _ s = [s]
instance print Int where print _ i = [toString i]
instance print Char where print _ c = [{c}]
instance print [a] | print a
where print _ xs = [concat e \\ e <- map (print False) xs]

instance print (Maybe a) | print a
where print _ Nothing = []; print b (Just x) = print b x

instance print Kind
where
	print _ KStar = ["*"]
	print b (k1 KArrow k2) = parlft -- print True k1 ++ ["->"] ++ print b k2 -- parrgt
	where (parlft,parrgt) = if b ("(",")") ("","")

instance print TypeRestriction
where
	print _ (Instance c ts)  = "instance " -- c -- " " -- printersperse True " " ts
	print _ (Derivation g t) = "derive " -- g -- " " -+ t

instance print TypeContext
where
	print _ [] = []
	print _ crs = printersperse False " & "
		[printersperse False ", " (map corg gr) -- " " -- printersperse False " " (types $ hd gr) \\ gr <- grps]
	where
		grps = groupBy (\a b -> types a == types b && length (types a) == 1) crs
		types (Instance _ ts) = ts; types (Derivation _ t) = [t]
		corg (Instance c _) = c; corg (Derivation g _) = g +++ "{|*|}"

instance print Type
where
	print ia (Type s vs) = typeConstructorName True ia s vs
	print _ (Var v) = [v]
	print ia (Func [] r []) = print ia r
	print _ (Func [] r tc) = r -- " | " -- tc
	print ia (Func ts r []) = parens ia (printersperse True " " ts -- " -> " -- r)
	print _ (Func ts r tc) = (Func ts r []) -- " | " -- tc
	print ia (Cons tv [])  = print ia tv
	print ia (Cons tv ats) = parens ia (tv -- " " -- printersperse True " " ats)
	print _ (Uniq t)       = case t of
		Type _ _ -> "*" -- t
		_        -> "*" -+ t
	print _ (Forall tvs t []) = "(A." -- printersperse True " " tvs -- ": " -- t -- ")"
	print _ (Forall tvs t tc) = "(A." -- printersperse True " " tvs -- ": " -- t -- " | " -- tc -- ")"
	print _ (Arrow Nothing)  = ["(->)"]
	print _ (Arrow (Just t)) = "((->) " -+ t +- ")"

instance toString Type where toString t = concat $ print False t

parens :: !Bool ![String] -> [String]
parens False ss = ss
parens True ss  = ["(":ss] ++ [")"]

instance print TypeDef
where
	print _ {td_name,td_uniq,td_args,td_rhs}
		= ":: " -- if td_uniq "*" "" -- typeConstructorName False False td_name td_args -- td_rhs

instance print TypeDefRhs
where
	print _ (TDRCons ext cs)         = "\n\t= " -- makeADT ext cs
	where
		makeADT :: !Bool ![Constructor] -> String
		makeADT exten [] = if exten " .." ""
		makeADT False [c1:cs]
			= concat (c1 -- "\n" --
				concat [concat ("\t| " -- c -- "\n") \\ c <- cs])
		makeADT True cs = concat (makeADT False cs -- "\t| ..")
	print _ (TDRNewType c) = " =: " -- c
	print _ (TDRRecord _ exi fields) = " =" --
		if (isEmpty exi) [] (" E." -- printersperse False " " exi -- ":") --
		"\n\t" -- makeRecord exi fields
	where
		makeRecord :: ![TypeVar] ![RecordField] -> String
		makeRecord _ [] = "{}"
		makeRecord exi [f1:fs]
			= concat ("{ " -- printRf f1 -- "\n" --
				concat [concat ("\t, " -- printRf f -- "\n")
				        \\ f <- fs] -- "\t}")
		where
			padLen = maxList (map (\f -> size f.rf_name) [f1:fs])
			pad i s = s +++ toString (repeatn (i - size s) ' ')

			printRf {rf_name,rf_type} = pad padLen rf_name -- " :: " -- rf_type
	print _ (TDRSynonym t)           = " :== " -- t
	print _ (TDRAbstract Nothing)    = []
	print _ (TDRAbstract (Just rhs)) = " /*" -- rhs -- " */"
	print _ (TDRAbstractSynonym t)   = " (:== " -- t -- ")"

typeConstructorName :: !Bool !Bool !String ![Type] -> [String]
typeConstructorName isInfix isArg t as
# isInfix = isInfix && not (isEmpty as)
// Lists
| t == "_List"   = if isInfix ("["  -- as --  "]") ("[]"   -- as`)
| t == "_!List"  = if isInfix ("[!" -- as --  "]") ("[! ]" -- as`)
| t == "_List!"  = if isInfix ("["  -- as -- "!]") ("[ !]" -- as`)
| t == "_!List!" = if isInfix ("[!" -- as -- "!]") ("[!!]" -- as`)
| t == "_#List"  = if isInfix ("[#" -- as --  "]") ("[#]"  -- as`)
| t == "_#List!" = if isInfix ("[#" -- as -- "!]") ("[#!]" -- as`)
| t == "_|List"  = if isInfix ("[|" -- as --  "]") ("[|]"  -- as`)
// Arrays
| t == "_#Array" = if isInfix ("{#" -- as --  "}") ("{#}"  -- as`)
| t == "_Array"  = if isInfix ("{"  -- as --  "}") ("{}"   -- as`)
| t == "_!Array" = if isInfix ("{!" -- as --  "}") ("{!}"  -- as`)
// Tuples
| t % (0,5) == "_Tuple"
	# n = toInt (t % (6, size t - 1))
	| isEmpty as                   = "(" -- repeatn (n-1) ',' -- ")"
	| n > length as || not isInfix = parens isArg ("(" -- repeatn (n-1) ',' -- ") " -- printersperse True " " as)
	| otherwise                    = "(" -- printersperse False ", " as -- ")"
// Other predefined types
| t == "_Unit"   = ["()"]
| t.[0] == '_'   = [t % (1, size t - 1)]
// Other types
| isEmpty as     = print isArg t
| otherwise      = parens isArg (t -- " " -- printersperse True " " as)
where
	as` = if (isEmpty as) [] (" " -- as)

instance print Constructor
where
	print _ {cons_name,cons_args,cons_exi_vars=evars,cons_context,cons_priority}
		= if (isEmpty evars) [] ("E." -- printersperse False " " evars -- ": ") --
			name -- " " -- prio -- printersperse True " " cons_args --
			if (isEmpty cons_context) [] (" & " -- cons_context)
	where
		(name,prio) = case cons_priority of
			Nothing -> ([cons_name],             [])
			Just p  -> ("(" -- cons_name -- ")", p -- " ")

instance print Priority
where
	print _ (LeftAssoc i)  = "infixl " -- i
	print _ (RightAssoc i) = "infixr " -- i
	print _ (NoAssoc i)    = "infix " -- i

propagate_uniqueness :: (String -> Bool) !Type -> Type
propagate_uniqueness p (Type t ts)
	# ts = map (propagate_uniqueness p) ts
	= if (p t || any isUniq ts) Uniq id (Type t ts)
propagate_uniqueness p (Func is r tc)
	= Func (map (propagate_uniqueness p) is) (propagate_uniqueness p r) tc
propagate_uniqueness p (Cons v ts)
	# ts = map (propagate_uniqueness p) ts
	= if (any isUniq ts) Uniq id (Cons v ts)
propagate_uniqueness p (Forall vs t tc)
	= Forall vs (propagate_uniqueness p t) tc
propagate_uniqueness p t
	= t

resolve_synonyms :: ('M'.Map String [TypeDef]) !Type -> ([TypeDef], Type)
resolve_synonyms tds (Type t ts)
	# (syns, ts) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) ts
	= case candidates of
		[] = (syns, Type t ts)
		[syn=:{td_args, td_rhs=TDRSynonym synt}:_]
			# newargs = map ((+++) "__" o fromVar) td_args
			# (Just t)
				= assignAll [(fromVar a, Var n) \\ a <- td_args & n <- newargs] synt
				>>= assignAll [(a,r) \\ a <- newargs & r <- ts]
			| length td_args <> length ts
				# (Type r rs) = t
				# t = Type r $ rs ++ drop (length td_args) ts
				= appFst ((++) [syn:syns]) $ resolve_synonyms tds t
			= appFst ((++) [syn:syns]) $ resolve_synonyms tds t
where
	candidates = [td \\ td=:{td_rhs=TDRSynonym syn} <- fromMaybe [] $ 'M'.get t tds
		| length td.td_args <= tslen && (isType syn || length td.td_args == tslen)]
	where tslen = length ts
resolve_synonyms tds (Func is r tc)
	# (syns, [r:is]) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) [r:is]
	= (syns, Func is r tc)
resolve_synonyms _ (Var v)
	= ([], Var v)
resolve_synonyms tds (Cons v ts)
	# (syns, ts) = appFst (removeDupTypedefs o flatten) $ unzip $ map (resolve_synonyms tds) ts
	= (syns, Cons v ts)
resolve_synonyms tds (Forall vs t tc)
	# (syns, t) = resolve_synonyms tds t
	= (syns, Forall vs t tc)
resolve_synonyms tds (Arrow (Just t))
	= Arrow o Just <$> resolve_synonyms tds t
resolve_synonyms tds (Arrow Nothing)
	= ([], Arrow Nothing)
resolve_synonyms tds (Uniq t)
	= Uniq <$> resolve_synonyms tds t

// Apply a TVAssignment to a Type
assign :: !TVAssignment !Type -> Maybe Type
assign va (Type s ts) = Type s <$> mapM (assign va) ts
assign va (Func ts r tc)
	= liftM3 Func (mapM (assign va) ts) (assign va r) (pure tc) // TODO tc
assign (v,a) (Var v`) = pure $ if (v == v`) a (Var v`)
assign va=:(v,Type s ts) (Cons v` ts`)
	| v == v`   = Type s <$> mapM (assign va) (ts ++ ts`)
	| otherwise = Cons v` <$> mapM (assign va) ts`
assign va=:(v,Cons c ts) (Cons v` ts`)
	| v == v`   = Cons c <$> mapM (assign va) (ts ++ ts`)
	| otherwise = Cons v` <$> mapM (assign va) ts`
assign va=:(v,Var v`) (Cons v`` ts)
	| v == v``  = Cons v` <$> mapM (assign va) ts
	| otherwise = Cons v`` <$> mapM (assign va) ts
assign va=:(v,_) (Cons v` ts)
	| v == v` = empty
	| otherwise = Cons v` <$> mapM (assign va) ts
assign va (Uniq t) = Uniq <$> assign va t
assign va=:(v,Var v`) (Forall tvs t tc)
	= liftM3 Forall (mapM (assign va) tvs) (assign va t) (pure tc) // TODO tc
assign va=:(v,_) (Forall tvs t tc)
	| isMember (Var v) tvs = empty
	| otherwise = flip (Forall tvs) tc <$> assign va t
assign va (Arrow (Just t)) = Arrow o Just <$> assign va t
assign va (Arrow Nothing) = Just $ Arrow Nothing

reduceArities :: !Type -> Type
reduceArities (Func [] r []) = r
reduceArities (Func ts r tc)
| length ts > 1 = Func [reduceArities $ hd ts] (reduceArities $ Func (tl ts) r tc) tc
| otherwise = Func (map reduceArities ts) (reduceArities r) tc
reduceArities (Type s ts) = Type s $ map reduceArities ts
reduceArities (Cons v ts) = Cons v $ map reduceArities ts
reduceArities (Uniq t) = Uniq $ reduceArities t
reduceArities (Var v) = Var v
reduceArities (Forall [] t []) = reduceArities t
reduceArities (Forall tvs t tc) = Forall tvs (reduceArities t) tc
reduceArities (Arrow mt) = Arrow (reduceArities <$> mt)

normalise_type :: (String -> Bool) !('M'.Map String [TypeDef]) !Type -> (!Type, ![TypeDef], ![TypeVar])
normalise_type alwaysUnique tds t
# t        = reduceArities t
# (syns,t) = resolve_synonyms tds t
# t        = propagate_uniqueness alwaysUnique t
# t        = optConses t
# (t,vars) = rename t
= (t,syns,vars)
where
	rename :: !Type -> (!Type, ![TypeVar])
	rename t = (renameVars t, map fst renames)
	where
		renames :: [(TypeVar, TypeVar)]
		renames = [(o, "v" +++ toString n) \\ o <- removeDup $ allVars t & n <- [1..]]

		renameVars :: !Type -> Type
		renameVars (Type s ts)      = Type s $ map renameVars ts
		renameVars (Func is r tc)   = Func (map renameVars is) (renameVars r) $ map renameVarsInTC tc
		renameVars (Var tv)         = Var $ fromJust $ lookup tv renames
		renameVars (Cons cv ts)     = Cons (fromJust $ lookup cv renames) $ map renameVars ts
		renameVars (Uniq t)         = Uniq $ renameVars t
		renameVars (Forall vs t tc) = Forall (map renameVars vs) (renameVars t) $ map renameVarsInTC tc
		renameVars (Arrow t)        = Arrow $ renameVars <$> t

		renameVarsInTC :: !TypeRestriction -> TypeRestriction
		renameVarsInTC (Instance c ts)  = Instance c $ map renameVars ts
		renameVarsInTC (Derivation g t) = Derivation g $ renameVars t

		allVars :: !Type -> [TypeVar]
		allVars (Type _ ts)      = allVars` ts
		allVars (Func is r _)    = allVars` is ++ allVars r
		allVars (Var tv)         = [tv]
		allVars (Cons cv ts)     = [cv:allVars` ts]
		allVars (Uniq t)         = allVars t
		allVars (Forall vs t _)  = allVars` vs ++ allVars t
		allVars (Arrow (Just t)) = allVars t
		allVars (Arrow Nothing)  = []

		allVars` :: ([Type] -> [TypeVar])
		allVars` = concatMap allVars

	optConses :: !Type -> Type
	optConses (Type s ts)      = Type s $ map optConses ts
	optConses (Func is r tc)   = Func (map optConses is) (optConses r) $ map optConsesInTR tc
	optConses (Var v)          = Var v
	optConses (Cons c [])      = Var c
	optConses (Cons c ts)      = Cons c $ map optConses ts
	optConses (Uniq t)         = Uniq $ optConses t
	optConses (Forall vs t tc) = Forall (map optConses vs) (optConses t) $ map optConsesInTR tc
	optConses (Arrow t)        = Arrow $ optConses <$> t

	optConsesInTR :: !TypeRestriction -> TypeRestriction
	optConsesInTR (Instance c ts)  = Instance c $ map optConses ts
	optConsesInTR (Derivation g t) = Derivation g $ optConses t
