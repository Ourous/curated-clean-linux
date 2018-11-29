definition module iTasks.Internal.Tonic

from iTasks.Internal.SDS import :: Shared, :: ReadWriteShared, :: RWShared
from iTasks.Internal.IWorld import :: IWorld
from iTasks.Internal.Task import :: TaskEvalOpts, :: TaskResult
from iTasks.WF.Definition import :: Task, :: InstanceNo, class iTask
from iTasks.WF.Combinators.Tune import class tune
from iTasks.SDS.Definition import :: SDS

import iTasks.Internal.Tonic.AbsSyn
import iTasks.Internal.Tonic.Images
from System.Time import :: Timestamp
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Either import :: Either
from Graphics.Scalable.Image import :: Image, :: TagSource, :: TagRef, :: ImageTag
from Graphics.Scalable.Internal.Image` import :: Image`
from iTasks.WF.Combinators.Overloaded import class TMonad, class TApplicative
import Data.Functor

from System.IO import :: IO

// For all of these classes goes that the iTask context restriction shouldn't
// be there. Ideally, we would have something like associated type families
// and constraintkinds to determine the context restriction per monad.
class TonicTopLevelBlueprint m | TonicBlueprintPart m where
  tonicWrapBody :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)     ] (m a) -> m a | iTask a
  tonicWrapArg  :: !VarName !Int a -> m () | iTask a

class TonicBlueprintPart m | TMonad m where
  tonicWrapApp  :: !ModuleName !FuncName !ExprId                [(ExprId, a -> Int)] (m a) -> m a | iTask a

instance TonicTopLevelBlueprint Task
instance TonicBlueprintPart Task

instance TonicTopLevelBlueprint Maybe
instance TonicBlueprintPart Maybe

instance TonicTopLevelBlueprint (Either e)
instance TonicBlueprintPart (Either e)

instance TonicTopLevelBlueprint IO
instance TonicBlueprintPart IO

instance TApplicative IO
instance TMonad IO

tonicExtWrapArg       :: !VarName !Int !a -> m () | iTask a & TonicTopLevelBlueprint m

tonicExtWrapBody      :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (         m a)          -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam1  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam2  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapBodyLam3  :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a

tonicExtWrapApp       :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] (          m a)          -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam1   :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b     -> m a) -> b     -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam2   :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c   -> m a) -> b c   -> m a | TonicBlueprintPart m & iTask a

tonicExtWrapAppLam3   :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a
