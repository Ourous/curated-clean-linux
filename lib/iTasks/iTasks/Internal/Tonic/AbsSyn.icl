implementation module iTasks.Internal.Tonic.AbsSyn

import Text.GenJSON, Data.Maybe
from Data.GenEq import generic gEq
from Data.Map import :: Map
import qualified Data.Map as DM
import StdBool, StdList, StdTuple
from StdOverloaded import class == (..)

derive JSONEncode TonicModule, TonicFunc, TExpr, TPriority, TAssoc, TLit

derive JSONDecode TonicModule, TonicFunc, TExpr, TPriority, TAssoc, TLit

derive gEq TonicModule, TonicFunc, TExpr, TPriority, TAssoc, TLit

instance == TonicModule where
  (==) tm1 tm2 =  tm1.tm_name              == tm2.tm_name
               && 'DM'.toList tm1.tm_funcs == 'DM'.toList tm2.tm_funcs

instance == TonicFunc where
  (==) tt1 tt2 =  tt1.tf_module    == tt2.tf_module
               && tt1.tf_name      == tt2.tf_name
               && tt1.tf_iclLineNo == tt2.tf_iclLineNo
               && tt1.tf_resty     == tt2.tf_resty
               && tt1.tf_args      == tt2.tf_args
               && tt1.tf_body      == tt2.tf_body

instance == TExpr where
  (==) (TVar  _ _ p1) (TVar _ _ p2) = p1 == p2
  (==) (TLit  ppe1) (TLit ppe2) = ppe1 == ppe2
  (==) (TMApp eid1 tn1 mn1 vn1 as1 p1 ptr1) (TMApp eid2 tn2 mn2 vn2 as2 p2 ptr2) = eid1 == eid2 && tn1 == tn2 && mn1 == mn2 && vn1 == vn2 && as1 == as2 && p1 == p2 && ptr1 == ptr2
  (==) (TFApp eid1 vn1 es1 p1) (TFApp eid2 vn2 es2 p2) = eid1 == eid2 && vn1 == vn2 && es1 == es2 && p1 == p2
  (==) (TLam  es1 e1) (TLam es2 e2) = es1 == es2 && e1 == e2
  (==) (TSel  e1 es1) (TSel e2 es2) = e1 == e2 && es1 == es2
  (==) (TRecUpd vn1 te1 es1) (TRecUpd vn2 te2 es2) = vn1 == vn2 && te1 == te2 && es1 == es2
  (==) TNoBind TNoBind = True
  (==) (TLet bs1 e1) (TLet bs2 e2) = bs1 == bs2 && e1 == e2
  (==) (TIf eid1 e1 e2 e3) (TIf eid2 e1` e2` e3`) = eid1 == eid2 && e1 == e1` && e2 == e2` && e3 == e3`
  (==) (TCase eid1 e1 ps1) (TCase eid2 e2 ps2) = eid1 == eid2 && e1 == e2 && ps1 == ps2
  (==) (TExpand es1 tt1) (TExpand es2 tt2) = es1 == es2 && tt1 == tt2
  (==) (TAugment e1 _) (TAugment e2 _) = e1 == e2
  (==) _ _ = False

instance == TAssoc where
  (==) TLeftAssoc  TLeftAssoc  = True
  (==) TRightAssoc TRightAssoc = True
  (==) TNoAssoc    TNoAssoc    = True
  (==) _           _           = False

instance == TPriority where
  (==) (TPrio a1 n1) (TPrio a2 n2) = a1 == a2 && n1 == n2
  (==) TNoPrio       TNoPrio       = True
  (==) _             _             = False

instance == TLit where
  (==) (TBool   l) (TBool   r) = l == r
  (==) (TInt    l) (TInt    r) = l == r
  (==) (TReal   l) (TReal   r) = l == r
  (==) (TString l) (TString r) = l == r
  (==) _ _ = False

