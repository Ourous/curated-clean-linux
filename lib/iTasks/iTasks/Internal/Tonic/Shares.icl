implementation module iTasks.Internal.Tonic.Shares

import qualified Data.Map as DM
import qualified iTasks.Internal.SDS as DSDS
import Data.Error
import iTasks.Internal.Tonic.AbsSyn
import iTasks.Internal.Tonic.Types
import iTasks.Internal.Store
import StdMisc, Data.Maybe

NS_TONIC_INSTANCES :== "tonic-instances"

sdsUnsafeRead :: (sds () a b) *IWorld -> *(a, *IWorld) | TC a & RWShared sds & TC b
sdsUnsafeRead focus iworld
  # (res, iworld) = 'DSDS'.read focus 'DSDS'.EmptyContext iworld
  = case res of
      Ok ('DSDS'.ReadingDone x) -> (x, iworld)

selectedBlueprint :: SimpleSDSLens (Maybe ClickMeta)
selectedBlueprint = sdsFocus "selectedBlueprint" (removeMaybe (Just Nothing) memoryShare)

selectedDetail :: SimpleSDSLens (Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int)))
selectedDetail = sdsFocus "selectedDetail" (removeMaybe (Just Nothing) memoryShare)

storedOutputEditors :: SimpleSDSLens (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability))
storedOutputEditors = sdsTranslate "storedOutputEditors" (\t -> t +++> "-storedOutputEditors")
                                  (removeMaybe (Just 'DM'.newMap) memoryShare)

outputForTaskId :: SDSLens (TaskId, ExprId) (TaskId, Int, Task (), TStability) (TaskId, Int, Task (), TStability)
outputForTaskId = sdsLens "outputForTaskId" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) (Just reducer) storedOutputEditors
  where

  reducer p ws = read p  ws

  read :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability))
       -> MaybeError TaskException (TaskId, Int, Task (), TStability)
  read oid=:(tid, _) trtMap = maybe (Ok (TaskId 0 0, 0, viewInformation (Title "Notice") [] ("No task value for the selected task. Try entering or updating a value in its editor.") @! (), TNoVal))
                          Ok ('DM'.get oid trtMap)

  write :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (TaskId, Int, Task (), TStability)
        -> MaybeError TaskException (Maybe (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)))
  write tid trtMap bpref = Ok (Just ('DM'.put tid bpref trtMap))

  notify :: (TaskId, ExprId) (Map (TaskId, ExprId) (TaskId, Int, Task (), TStability)) (TaskId, Int, Task (), TStability)
         -> SDSNotifyPred (TaskId, ExprId)
  notify tid oldmap (_, n, _, st) = \_ tid` -> case (tid == tid`, 'DM'.get tid oldmap) of
    (True, Just (_, n`, _, st`)) -> n <> n` || st =!= st`
    _                            -> False

tonicSharedRT :: SimpleSDSLens TonicRTMap
tonicSharedRT = sdsTranslate "tonicSharedRT" (\t -> t +++> "-tonicSharedRT")
                             (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

allTonicInstances :: SDSLens TaskId [((ModuleName, FuncName), BlueprintInstance)] ()
allTonicInstances = sdsLens "allTonicInstances" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) (Just \_ _ -> Ok ()) tonicSharedRT
where
  //read :: (TaskId, ModuleName, FuncName) TonicRTMap -> MaybeError TaskException  (Maybe BlueprintInstance) BlueprintInstance
  read tid trtMap = Ok (fromMaybe [] ('DM'.get tid trtMap))

  //write :: (TaskId, ModuleName, FuncName) TonicRTMap  (Maybe BlueprintInstance) BlueprintInstance -> MaybeError TaskException (Maybe TonicRTMap)
  write tid trtMap bpref = abort "allTonicInstances" // Ok ()

  //notify :: (TaskId, ModuleName, FuncName) TonicRTMap BlueprintInstance -> SDSNotifyPred (TaskId, ModuleName, FuncName)
  notify tid oldmap inst = \_ tid` -> False

tonicInstances :: SDSLens (TaskId, ModuleName, FuncName) (Maybe BlueprintInstance) BlueprintInstance
tonicInstances = sdsLens "tonicInstances" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) (Just reducer) tonicSharedRT
where
  read :: (TaskId, ModuleName, FuncName) TonicRTMap -> MaybeError TaskException (Maybe BlueprintInstance)
  read (tid, mn, fn) trtMap = Ok ('DM'.get tid trtMap >>= 'DM'.get (mn, fn) o 'DM'.fromList)

  write :: (TaskId, ModuleName, FuncName) TonicRTMap BlueprintInstance -> MaybeError TaskException (Maybe TonicRTMap)
  write (tid, mn, fn) trtMap bpref = Ok (Just (case 'DM'.get tid trtMap of
                                                 Just im -> let xs    = [if (mn == mn` && fn == fn`) (True, ((mn`, fn`), {bpref & bpi_index = i})) (False, ((mn`, fn`), {bpref` & bpi_index = i})) \\ (i, ((mn`, fn`), bpref`)) <- zip2 [0..] im]
                                                                elems = map snd xs
                                                             in 'DM'.put tid (if (or (map fst xs))
                                                                                elems
                                                                                (elems ++ [((mn, fn), {bpref & bpi_index = length elems})])) trtMap
                                                 _       -> 'DM'.put tid [((mn, fn), bpref)] trtMap))

  reducer p ws = case read p ws of
    (Error e) -> (Error e)
    Ok Nothing -> Error (exception "No Blueprint avaialbe?")
    Ok (Just v) -> (Ok v)

  notify :: (TaskId, ModuleName, FuncName) TonicRTMap BlueprintInstance -> SDSNotifyPred (TaskId, ModuleName, FuncName)
  notify tid oldmap inst = \_ tid` -> case (tid == tid`, read tid oldmap) of
                                      (True, Ok (Just oldinst)) -> oldinst =!= inst
                                      _                    -> False


tonicEnabledSteps :: SimpleSDSLens (Map TaskId (Map ExprId [UI]))
tonicEnabledSteps = sdsTranslate "tonicEnabledSteps" (\t -> t +++> "-tonicEnabledSteps")
                                 (memoryStore NS_TONIC_INSTANCES (Just 'DM'.newMap))

tonicActionsForTaskID :: SDSLens TaskId (Map ExprId [UI]) (Map ExprId [UI])
tonicActionsForTaskID = sdsLens "tonicActionsForTaskID" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) (Just reducer) tonicEnabledSteps
  where
  read :: TaskId (Map TaskId (Map ExprId [UI])) -> MaybeError TaskException (Map ExprId [UI])
  read tid acts
    = case 'DM'.get tid acts of
        Just acts` -> Ok acts`
        _          -> Ok 'DM'.newMap

  write :: TaskId (Map TaskId (Map ExprId [UI])) (Map ExprId [UI]) -> MaybeError TaskException (Maybe (Map TaskId (Map ExprId [UI])))
  write tid oldmap acts
    = Ok (Just ('DM'.put tid acts oldmap))

  notify :: TaskId (Map TaskId (Map ExprId [UI])) (Map ExprId [UI]) -> SDSNotifyPred TaskId
  notify tid oldmap acts = \_ tid` -> case read tid oldmap of
                                      Ok oldacts -> oldacts =!= acts
                                      _          -> False

  reducer p ws = read p ws

tonicActionsForTaskIDAndExpr :: SDSLens (TaskId, ExprId) [UI] [UI]
tonicActionsForTaskIDAndExpr = sdsLens "tonicActionsForTaskIDAndExpr" (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) (Just reducer) tonicEnabledSteps
  where
  read :: (TaskId, ExprId) (Map TaskId (Map ExprId [UI])) -> MaybeError TaskException [UI]
  read (tid, eid) acts
    = case 'DM'.get tid acts of
        Just acts` -> case 'DM'.get eid acts` of
                        Just xs -> Ok xs
                        _       -> Ok []
        _          -> Ok []

  write :: (TaskId, ExprId) (Map TaskId (Map ExprId [UI])) [UI] -> MaybeError TaskException (Maybe (Map TaskId (Map ExprId [UI])))
  write (tid, eid) oldmap acts
    # m = case 'DM'.get tid oldmap of
            Just acts` -> acts`
            _          -> 'DM'.newMap
    # m = 'DM'.put eid acts m
    = Ok (Just ('DM'.put tid m oldmap))

  notify :: (TaskId, ExprId) (Map TaskId (Map ExprId [UI])) [UI] -> SDSNotifyPred (TaskId, ExprId)
  notify tid oldmap acts = \_ tid` -> case read tid oldmap of
                                      Ok oldacts -> oldacts =!= acts
                                      _          -> False

  reducer p ws = read p ws

staticDisplaySettings :: SimpleSDSLens StaticDisplaySettings
staticDisplaySettings = sdsFocus "staticDisplaySettings" (removeMaybe (Just
                                     { StaticDisplaySettings
                                     | unfold_depth    = 0
                                     , display_compact = False
                                     , show_comments   = True
                                     }) memoryShare)

queryShare :: SimpleSDSLens (Maybe BlueprintQuery)
queryShare = sdsFocus "queryShare" (removeMaybe (Just Nothing) memoryShare)

dynamicDisplaySettings :: SimpleSDSLens DynamicDisplaySettings
dynamicDisplaySettings = sdsFocus "dynamicDisplaySettings" (removeMaybe (Just
                                     { DynamicDisplaySettings
                                     | unfold_depth    = 0
                                     , display_compact = False
                                     , show_finished_blueprints = False
                                     , show_task_value = False
                                     , show_comments = False
                                     , show_all_child_tasks = False
                                     }) memoryShare)


paramsForTaskInstance :: SDSLens (ModuleName, FuncName, TaskId) [(VarName, Int, Task ())] [(VarName, Int, Task ())]
paramsForTaskInstance = sdsTranslate "paramsForTaskInstance" (\t -> t +++> "-paramsForTaskInstance")
                             (memoryStore NS_TONIC_INSTANCES Nothing)

storeTaskOutputViewer :: !(TaskResult a) !ExprId !TaskId !TaskId !*IWorld -> *IWorld | iTask a
storeTaskOutputViewer tr nid parentTaskId childTaskId iworld // = iworld // TODO FIXME
  | nid <> [] && parentTaskId <> TaskId 0 0
    # childFocus             = sdsFocus (parentTaskId, nid) outputForTaskId
    # ((_, n, _, _), iworld) = sdsUnsafeRead childFocus iworld
    = snd ('DSDS'.write (resultToOutput (n + 1) childTaskId tr) childFocus 'DSDS'.EmptyContext iworld)
  | otherwise = iworld

resultToOutput :: !Int !TaskId !(TaskResult a) -> (!TaskId, !Int, !Task (), !TStability) | iTask a
resultToOutput newN tid (ValueResult (Value v s) _ _ _) = (tid, newN, viewInformation (Title ("Value for task " +++ toString tid)) [] v @! (), if s TStable TUnstable)
resultToOutput newN tid (ValueResult NoValue _ _ _)     = (tid, newN, viewInformation (Title ("Value for task " +++ toString tid)) [] "No value" @! (), TNoVal)
resultToOutput newN tid _                               = (tid, newN, viewInformation (Title "Error") [] ("No task value for task " +++ toString tid) @! (), TNoVal)

