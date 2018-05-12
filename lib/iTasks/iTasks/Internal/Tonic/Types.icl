implementation module iTasks.Internal.Tonic.Types

import StdString
import StdOverloaded
import iTasks.Internal.Tonic.AbsSyn
import qualified Data.Map as DM
from Data.Map import instance Functor (Map k)
import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap
import Data.Functor
import Data.List
import Data.Maybe
import Data.CircularStack
import iTasks.Extensions.User

derive class iTask TonicImageState, TClickAction, ClickMeta, BlueprintRef

instance BlueprintLike BlueprintInstance where
  getComputationId bpi = toComp bpi.bpi_taskId
  getIdent bpi         = bpi.bpi_bpref
  getBranches bpi      = bpi.bpi_case_branches
  getActiveCompIds bpi
    = [toComp tid \\ (tid, _) <- concatMap 'DIS'.elems ('DM'.elems bpi.bpi_activeNodes)]
  getActiveCompId eid {bpi_activeNodes}
    = case [tid \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems bpi_activeNodes) | eid == nid] of
        [tid : _] -> Just (toComp tid)
        _         -> Nothing

  getPreviouslyActive bpi = fmap toComp bpi.bpi_previouslyActive
  getCurrentUser bpi   = fmap toString bpi.bpi_currentUser

toComp :: !TaskId -> ComputationId
toComp (TaskId i t) = [i, t]

comp2TaskId :: !ComputationId -> TaskId
comp2TaskId [i, t] = TaskId i t

instance BlueprintLike GenBlueprintInstance where
  getComputationId gbpi = gbpi.gbpi_computationId
  getIdent gbpi         = gbpi.gbpi_bpref
  getBranches gbpi      = gbpi.gbpi_case_branches
  getActiveCompIds {gbpi_activeNode = (cid, _)} = [cid]
  getActiveCompId eid {gbpi_activeNode = (cid, eid`)}
    | eid == eid` = Just cid
    | otherwise   = Nothing

  getPreviouslyActive gbpi = gbpi.gbpi_previouslyActive
  getCurrentUser gbpi   = Nothing

derive class iTask TStability, BlueprintIdent, BlueprintInstance
derive class iTask GenBlueprintInstance, TonicMessage, TMNewTopLevel, TMApply

derive gEditor
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gDefault
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit

derive gText
  TonicModule, TonicFunc, TExpr, TPriority, TAssoc, IntMap, TLit
