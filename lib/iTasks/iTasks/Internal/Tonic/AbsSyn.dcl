definition module iTasks.Internal.Tonic.AbsSyn

from Data.Map import :: Map
from Data.Maybe import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenEq import generic gEq
from StdOverloaded import class ==

derive JSONEncode TonicModule, TonicFunc, TExpr, TPriority, TAssoc, TLit

derive JSONDecode TonicModule, TonicFunc, TExpr, TPriority, TAssoc, TLit

derive gEq TonicModule, TonicFunc, TExpr, TPriority, TAssoc, TLit

instance == TonicModule
instance == TonicFunc
instance == TExpr
instance == TAssoc
instance == TPriority
instance == TLit

:: ModuleName :== String
:: FuncName   :== String
:: Pattern    :== TExpr
:: TypeName   :== String
:: PPExpr     :== String
:: ExprId     :== [Int]
:: VarName    :== String
:: VarPtr     :== Int

:: TonicModule =
  { tm_name  :: ModuleName
  , tm_funcs :: Map FuncName TonicFunc
  }

:: TonicFunc =
  { tf_comments  :: !String
  , tf_module    :: !ModuleName
  , tf_name      :: !FuncName
  , tf_iclLineNo :: !Int
  , tf_resty     :: !TExpr
  , tf_args      :: ![(TExpr, TExpr)]
  , tf_body      :: !TExpr
  }

:: TExpr
  = TVar     !ExprId !PPExpr !VarPtr
  | TLit     !TLit
  | TPPExpr  !PPExpr
  | TMApp    !ExprId !(Maybe TypeName) !ModuleName
             !FuncName ![TExpr] !TPriority !(Maybe VarPtr)
  | TFApp    !ExprId !FuncName ![TExpr] !TPriority
  | TLam     ![TExpr] !TExpr
  | TSel     !TExpr ![TExpr]
  | TRecUpd  !VarName !TExpr ![TExpr]
  | TNoBind
  | TLet     ![(!Pattern, !TExpr)] !TExpr
  | TIf      !ExprId !TExpr !TExpr !TExpr
  | TCase    !ExprId !TExpr ![(!Pattern, !TExpr)]
  | TExpand  ![TExpr] !TonicFunc
  | TAugment !TExpr !TExpr

:: TLit
  = TBool   Bool
  | TInt    Int
  | TReal   Real
  | TString String

:: TAssoc
  = TLeftAssoc
  | TRightAssoc
  | TNoAssoc

:: TPriority
  = TPrio TAssoc Int
  | TNoPrio
