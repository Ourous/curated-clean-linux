definition module iTasks.Extensions.Admin.TonicAdmin

import iTasks
from iTasks.Internal.Tonic.Images import :: TaskAppRenderer, :: ModelTy, :: ClickMeta, :: TonicImageState, :: ActionState, :: TClickAction
from iTasks.Internal.Tonic.Types import :: AllBlueprints, :: TonicModule, :: TonicFunc, :: FuncName, :: ModuleName, :: NavStack, :: BlueprintIdent, :: ExprId
from Graphics.Scalable.Image import :: TagSource, :: TagRef, :: Image, :: ImageTag
from Graphics.Scalable.Internal.Image` import :: Image`

tonicDashboard :: [TaskAppRenderer] -> Task ()

tonic :: Task ()

tonicStaticBrowser    :: [TaskAppRenderer] -> Task ()

tonicBrowseWithModule :: AllBlueprints [TaskAppRenderer] (Shared NavStack) TonicModule -> Task ()

tonicStaticWorkflow   :: [TaskAppRenderer] -> Workflow

tonicDynamicBrowser   :: [TaskAppRenderer] -> Task ()

tonicDynamicWorkflow  :: [TaskAppRenderer] -> Workflow

viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared NavStack) !BlueprintIdent !TonicModule !TonicFunc !Int !Bool -> Task ()
