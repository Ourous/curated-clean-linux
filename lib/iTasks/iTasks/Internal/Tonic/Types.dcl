definition module iTasks.Internal.Tonic.Types

import StdString
import StdOverloaded
from Data.IntMap.Strict import :: IntMap
from iTasks.Internal.Tonic.AbsSyn import :: TonicModule, :: TAssoc, :: TLit, :: TExpr, :: FuncName, :: TPriority, :: ModuleName, :: TonicFunc
from iTasks.Internal.Tonic.AbsSyn import :: ExprId
from Data.CircularStack import :: CircularStack
import iTasks.Extensions.User
from iTasks.Extensions.User import :: User

:: ListId :== TaskId

:: StaticDisplaySettings
  = { unfold_depth    :: !Int
    , display_compact :: !Bool
    , show_comments   :: !Bool
    }

:: DynamicDisplaySettings
  = { unfold_depth             :: !Int
    , display_compact          :: !Bool
    , show_finished_blueprints :: !Bool
    , show_task_value          :: !Bool
    , show_comments            :: !Bool
    , show_all_child_tasks     :: !Bool
    }

:: NavStack :== [ClickMeta]

:: DynamicView =
  { taskName    :: !String
  , startTime   :: !String
  , lastUpdate  :: !String
  , endTime     :: !String
  , user        :: !String
  }

:: BlueprintQuery
  = FuncName String
  | UserInvolved String
  | IsActiveTask
  | HasInstanceNo Int
  | AndQuery BlueprintQuery BlueprintQuery
  | OrQuery BlueprintQuery BlueprintQuery

:: AllBlueprints :== Map ModuleName (Map FuncName TonicFunc)

:: BlueprintIdent =
  { bpr_moduleName :: !ModuleName
  , bpr_taskName   :: !FuncName
  }

:: BlueprintInstance =
  { bpi_taskId           :: !TaskId
  , bpi_startTime        :: !DateTime
  , bpi_lastUpdated      :: !DateTime
  , bpi_endTime          :: !Maybe DateTime
  , bpi_activeNodes      :: !Map ListId (IntMap (TaskId, ExprId))
  , bpi_previouslyActive :: !Map ExprId TaskId
  , bpi_parentTaskId     :: !TaskId
  , bpi_currentUser      :: !Maybe User
  , bpi_blueprint        :: !TonicFunc
  , bpi_case_branches    :: !Map ExprId Int
  , bpi_index            :: !Int
  , bpi_bpref            :: !BlueprintIdent
  }

:: TonicRTMap :== Map TaskId [((ModuleName, FuncName), BlueprintInstance)]

:: Calltrace :== CircularStack TaskId

:: TStability = TNoVal | TStable | TUnstable

derive class iTask TStability, BlueprintIdent, BlueprintInstance

:: ComputationId :== [Int]
:: NodeId        :== [Int]
:: FunctionName  :== String

:: TonicMessage
  = TMNewTopLevel TMNewTopLevel
  | TMApply TMApply

:: TMNewTopLevel =
  { tmn_computationId  :: ComputationId // Abstraction from TaskId
  , tmn_bpModuleName   :: ModuleName
  , tmn_bpFunctionName :: FunctionName
  }

:: TMApply =
  { tma_computationId  :: ComputationId // Abstraction from TaskId
  , tma_nodeId         :: NodeId
  , tma_bpModuleName   :: ModuleName
  , tma_bpFunctionName :: FunctionName
  , tma_appModuleName  :: ModuleName
  , tma_appFunName     :: FunctionName
  }

:: GenBlueprintInstance =
  { gbpi_computationId    :: !ComputationId
  , gbpi_activeNode       :: !(ComputationId, ExprId)
  , gbpi_previouslyActive :: !Map ExprId ComputationId
  , gbpi_parentId         :: !ComputationId
  , gbpi_blueprint        :: !TonicFunc
  , gbpi_case_branches    :: !Map ExprId Int
  , gbpi_bpref            :: !BlueprintIdent
  }

class BlueprintLike a where
  getComputationId    :: a -> ComputationId
  getIdent            :: a -> BlueprintIdent
  getBranches         :: a -> Map ExprId Int
  getActiveCompIds    :: a -> [ComputationId]
  getActiveCompId     :: ExprId a -> Maybe ComputationId
  getPreviouslyActive :: a -> Map ExprId ComputationId
  getCurrentUser      :: a -> Maybe String

instance BlueprintLike BlueprintInstance

toComp :: !TaskId -> ComputationId
comp2TaskId :: !ComputationId -> TaskId

instance BlueprintLike GenBlueprintInstance

:: TClickAction = TNavAction | TDetailAction | TSelectArg Int

:: TonicImageState
  = { tis_task    :: TonicFunc
    , tis_depth   :: Int
    , tis_compact :: Bool
    }

:: ClickMeta =
  { click_origin_mbbpident :: !Maybe BlueprintRef
  , click_origin_mbnodeId  :: !Maybe ExprId
  , click_target_bpident   :: !BlueprintRef
  }

:: BlueprintRef =
  { bpident_moduleName :: !ModuleName
  , bpident_compName   :: !FuncName
  , bpident_compId     :: !Maybe ComputationId
  }

derive class iTask TonicImageState, TClickAction, ClickMeta, BlueprintRef


derive class iTask GenBlueprintInstance, TonicMessage, TMNewTopLevel, TMApply

derive gEditor
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gDefault
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gText
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit
