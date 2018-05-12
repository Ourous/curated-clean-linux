definition module iTasks.Internal.Tonic.Images

from Data.IntMap.Strict import :: IntMap
from Data.Map import :: Map
from Data.Set import :: Set
from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Graphics.Scalable.Image import :: Image, :: TagSource, :: TagRef, :: ImageTag
from Graphics.Scalable.Internal.Image` import :: Image`
from iTasks.Internal.Tonic.AbsSyn import :: TonicFunc, :: ExprId, :: FuncName, :: ModuleName, :: TExpr
from iTasks.Internal.Tonic.Types import :: TStability, :: BlueprintIdent, :: BlueprintInstance, :: GenBlueprintInstance, :: ComputationId, :: TClickAction, :: ClickMeta, :: TonicImageState

from iTasks.UI.Definition import :: UI
from iTasks.WF.Definition import :: TaskId, :: TaskValue, class iTask(..)

from iTasks.UI.Editor import :: Editor
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode
from Data.GenEq import generic gEq


:: ActionState a s = { state :: s, action :: Maybe a }
doAction :: !(a (ActionState a s) -> b) !(TaskValue (ActionState a s)) -> Maybe b

:: ModelTy :== ActionState (TClickAction, ClickMeta) TonicImageState

:: TaskAppRenderer :== Bool Bool Bool Bool Bool Bool Bool ExprId ModuleName FuncName
                       ModuleName FuncName [Image ModelTy] [Image ModelTy] *TagSource
                    -> *(!Maybe (Image ModelTy), !*TagSource)

mkStaticImage   :: ![TaskAppRenderer] !BlueprintIdent !Bool !ModelTy *TagSource
                -> Image ModelTy

mkTaskInstanceImage :: ![TaskAppRenderer] !BlueprintInstance
                       !(Map ExprId TStability) !(Map ExprId [UI])
                       !(Maybe (Either ClickMeta (!ModuleName, !FuncName, !ComputationId, !Int)))
                       !Bool !ModelTy *TagSource
                    -> Image ModelTy

mkGenInstanceImage :: ![TaskAppRenderer] !GenBlueprintInstance
                      !(Maybe (Either ClickMeta (!ModuleName, !FuncName, !ComputationId, !Int)))
                      !Bool !ModelTy *TagSource
                   -> Image ModelTy

tDefaultMApp :: !Bool !Bool !Bool !Bool !Bool !Bool !Bool !ExprId !ModuleName !FuncName
                !ModuleName !FuncName ![TExpr] ![Image ModelTy] ![Image ModelTy] !*TagSource
             -> *(!Image ModelTy, !*TagSource)

derive class iTask ActionState
