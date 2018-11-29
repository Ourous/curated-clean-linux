implementation module Clean.Types.Unify

import StdArray
import StdBool
from StdFunctions import o, const
import StdList
import StdOrdList
import StdTuple
import StdString

import Clean.Types
import Clean.Types.Util
import Control.Applicative
import Control.Monad
import Control.Monad.State
from Data.Func import $
import Data.Functor
import Data.GenEq
import Data.List
from Data.Map import :: Map, newMap
import Data.Maybe

derive gEq Type, TypeRestriction, Kind

isGeneralisingUnifier :: ![TVAssignment] -> Bool
isGeneralisingUnifier tvas = all isOk $ groupVars tvas []
where
	isOk :: ![Type] -> Bool
	isOk ts
	| length [v \\ Var v <- ts | v.[0] == 'r'] >= 2 = False
	| any (not o isVar) ts && any (\t -> isVar t && isMember (fromVar t).[0] ['_r']) ts = False
	| otherwise = True

isIsomorphicUnifier :: ![TVAssignment] -> Bool
isIsomorphicUnifier tvas = all isOk $ groupVars tvas []
where
	isOk :: ![Type] -> Bool
	isOk ts = all isVar ts && length ts == 2

groupVars :: ![TVAssignment] ![[Type]] -> [[Type]]
groupVars []           groups = map removeDup groups
groupVars [(v,t):rest] groups = case partition (\g -> isMember (Var v) g || isMember t g) groups of
	([], gs) -> groupVars rest [[Var v,t]:gs]
	(yes,no) -> groupVars rest $ [[Var v,t:flatten yes]:no]

(generalises) infix 4 :: !Type !Type -> Bool
(generalises) a b = case unify a` b` of
	Nothing  -> False
	Just tvs -> isGeneralisingUnifier tvs
where
	(_, a`) = prepare_unification True  (const False) newMap a
	(_, b`) = prepare_unification False (const False) newMap b

(specialises) infix 4 :: !Type !Type -> Bool
(specialises) a b = b generalises a

(isomorphic_to) infix 4 :: !Type !Type -> Bool
(isomorphic_to) a b = fromMaybe False (isIsomorphicUnifier <$> unify a` b`)
where
	(_, a`) = prepare_unification True  (const False) newMap a
	(_, b`) = prepare_unification False (const False) newMap b

prepare_unification :: !Bool (String -> Bool) (Map String [TypeDef]) !Type -> ([TypeDef], Type)
prepare_unification b alwaysUnique db (Func [] t _) = prepare_unification b alwaysUnique db t
prepare_unification isleft alwaysUnique db t
# (syns, t) = resolve_synonyms db t
# t = propagate_uniqueness alwaysUnique t
# t = reduceArities t
# t = renameAndRemoveStrictness t
= (syns, t)
where
	prep = if isleft "l" "r"
	renameAndRemoveStrictness :: !Type -> Type
	renameAndRemoveStrictness (Var v) = Var (prep +++ v)
	renameAndRemoveStrictness (Cons c ts) = Cons (prep +++ c) $ map renameAndRemoveStrictness ts
	renameAndRemoveStrictness (Type t ts) = Type t $ map renameAndRemoveStrictness ts
	renameAndRemoveStrictness (Func is r tc) = Func (map renameAndRemoveStrictness is) (renameAndRemoveStrictness r) (map (inTC renameAndRemoveStrictness) tc)
	renameAndRemoveStrictness (Uniq t) = Uniq $ renameAndRemoveStrictness t
	renameAndRemoveStrictness (Arrow t) = Arrow (renameAndRemoveStrictness <$> t)
	renameAndRemoveStrictness (Forall vs t tc) = fromJust $
		assignAll [(prep+++v,Var ("_"+++prep+++v)) \\ v <- map fromVarLenient vs] $
		renameAndRemoveStrictness t
	renameAndRemoveStrictness (Strict t) = renameAndRemoveStrictness t

	inTC f (Derivation g t) = Derivation g (f t)
	inTC f (Instance c ts)  = Instance c (map f ts)

finish_unification :: ![TypeDef] ![TVAssignment] -> Unifier
finish_unification syns tvs
# (tvs1, tvs2) = (filter (startsWith 'l') tvs, filter (startsWith 'r') tvs)
# (tvs1, tvs2) = (map removePrefixes tvs1, map removePrefixes tvs2)
= {assignments=sortBy order (map LeftToRight tvs1 ++ map RightToLeft tvs2), used_synonyms=removeDupTypedefs syns}
where
	startsWith :: !Char !TVAssignment -> Bool
	startsWith c (h,_) = h.[0] == c || h.[0] == '_' && h.[1] == c

	removePrefixes :: !TVAssignment -> TVAssignment
	removePrefixes (v,t) = (rm v, fromJust $ assignAll (map (\v->(v,Var (rm v))) $ allVars t) t)
	where rm s = s % (if (s.[0] == '_') 2 1, size s - 1)

	order :: !UnifyingAssignment !UnifyingAssignment -> Bool
	order ua1 ua2
	| isMember v1 (allVars t2) = False
	| isMember v2 (allVars t1) = True
	| otherwise                = True // don't care
	where
		(v1,t1) = fromUnifyingAssignment ua1
		(v2,t2) = fromUnifyingAssignment ua2

:: UnificationState =
	{ assignments         :: ![TVAssignment]
	, goals               :: ![(!Type, !Type)]
	, used_universal_vars :: ![TypeVar]
	}
assignments         s :== s.UnificationState.assignments
goals               s :== s.goals
used_universal_vars s :== s.used_universal_vars

:: UnifyM t :== StateT UnificationState Maybe t

fail :: UnifyM a
fail = StateT \_ -> Nothing

succeed :: UnifyM ()
succeed = pure ()

applyAssignment :: !TypeVar !Type -> UnifyM ()
applyAssignment v (Var w) | v.[0] <> '_' && w.[0] == '_' =
	applyAssignment w (Var v) // the below assumes a universal variable is always the TypeVar
applyAssignment v t =
	checkUniversalisedVariables v t >>= \t ->
	checkCircularAssignment v t >>|
	gets goals >>= mapM (assign` (v,t)) >>= \goals ->
	modify \s ->
	{ s
	& assignments = [(v,t):s.UnificationState.assignments]
	, goals = goals
	}
where
	checkUniversalisedVariables :: !TypeVar !Type -> UnifyM Type
	checkUniversalisedVariables v t
	| v.[0] <> '_' = case t of
		Var v -> if (v.[0] == '_') fail (pure t)
		_     -> pure t
	| otherwise = case t of
		Var v -> gets used_universal_vars >>= \used
			| isMember v  used = fail
			| isMember v` used = fail
			| otherwise =
				modify (\s -> {s & used_universal_vars=[v,v`:used]}) >>|
				applyInGoals (v,Var v`) >>|
				pure (Var v`)
			with v` = if (v.[0] == '_') v ("_" +++ v)
		_     -> fail
	where
		applyInGoals :: !TVAssignment -> UnifyM ()
		applyInGoals tva =
			gets goals >>=
			mapM (\(t,u) -> case (assign tva t, assign tva u) of
				(Just t`, Just u`) -> pure (t`,u`)
				_                  -> fail) >>= \gs ->
			modify (\s -> {s & goals=gs})

	checkCircularAssignment :: !TypeVar !Type -> UnifyM ()
	checkCircularAssignment v t
	| isMember v (allVars t) = fail
	| otherwise              = succeed

	assign` :: !TVAssignment !(!Type,!Type) -> UnifyM (!Type,!Type)
	assign` a=:(v,_) (t,u) = case (assign a t, assign a u) of
		(Just t, Just u) -> pure (t,u)
		_                -> fail

unify :: !Type !Type -> Maybe [TVAssignment]
unify t u = evalStateT loopUntilDone {assignments=[], goals=[(t,u)], used_universal_vars=[]}
where
	loopUntilDone :: UnifyM [TVAssignment]
	loopUntilDone = gets goals >>= \goals -> case goals of
		[(t1,t2):_] -> modify (\s -> {s & goals=tl s.goals}) >>| uni t1 t2 >>| loopUntilDone
		[]          -> gets assignments

uni :: !Type !Type -> UnifyM ()
uni (Var v) t = if (t == Var v) succeed (applyAssignment v t)
uni t (Var v) = applyAssignment v t
uni (Type t tas) (Type u uas) = if (t==u) (addGoals tas uas) fail
uni (Cons c cas) (Type t tas)
| lc <= lt = addGoals cas end >>| applyAssignment c (Type t begin)
where
	(lc,lt) = (length cas, length tas)
	(begin,end) = splitAt (lt - lc) tas
uni t=:(Type _ _) c=:(Cons _ _) = uni c t
uni (Cons c1 as1) (Cons c2 as2)
| l1 == l2  = addGoals as1 as2 >>| if (c1 == c2) succeed (applyAssignment c1 (Var c2))
| l1 <  l2  = addGoals as1 end >>| applyAssignment c1 (Cons c2 begin) with (begin,end) = splitAt (l2-l1) as2
| otherwise = addGoals end as2 >>| applyAssignment c2 (Cons c1 begin) with (begin,end) = splitAt (l1-l2) as1
where (l1,l2) = (length as1, length as2)
uni (Func [i1] r1 _) (Func [i2] r2 _) = addGoal i1 i2 >>| addGoal r1 r2
uni (Uniq a) (Uniq b) = addGoal a b
uni (Arrow Nothing) (Arrow Nothing) = succeed
uni (Arrow (Just t)) (Arrow (Just u)) = addGoal t u
uni _ _ = fail

addGoal :: !Type !Type -> UnifyM ()
addGoal t u = modify (\s -> {s & goals=[(t,u):s.goals]})

addGoals :: ![Type] ![Type] -> UnifyM ()
addGoals [t:ts] [u:us] = addGoal t u >>| addGoals ts us
addGoals []     []     = succeed
addGoals _      _      = fail
