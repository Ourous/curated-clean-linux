implementation module iTasks.Extensions.Admin.TonicAdmin

import iTasks
import StdMisc, Data.Tuple, Text, Data.Either, Data.Functor
import iTasks.Internal.SDS
import iTasks.Internal.Tonic.Blueprints
import iTasks.Internal.Tonic.Shares
import iTasks.Internal.Tonic.Types
import iTasks.Internal.Tonic.AbsSyn
import iTasks.Internal.Tonic.Pretty
import iTasks.Internal.Tonic.Images
import iTasks.Internal.Task
import iTasks.UI.Definition
import iTasks.Extensions.DateTime
from StdFunc import seq
import qualified Data.Map as DM
from Data.Map import instance Functor (Map a)
from Control.Monad import `b`, class Monad(bind)
import qualified iTasks.Internal.SDS as DSDS
import Data.List
from iTasks.Extensions.SVG.SVGEditor import fromSVGEditor, :: SVGEditor {..}
import iTasks.UI.JS.Encoding
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import Data.Maybe
import qualified Control.Applicative as CA
from Control.Applicative import class Applicative

derive class iTask StaticDisplaySettings, DynamicDisplaySettings,
                   DynamicView, BlueprintQuery, CircularStack
derive gEditor Set
derive gText Set
derive gDefault Set
derive JSONEncode Set
derive JSONDecode Set

derive JSEncode ActionState, TClickAction, ClickMeta, TonicImageState, BlueprintRef, TonicFunc, TExpr, TPriority, TLit, TAssoc
derive JSDecode ActionState, TClickAction, ClickMeta, TonicImageState, BlueprintRef, TonicFunc, TExpr, TPriority, TLit, TAssoc

tonic :: Task ()
tonic = tonicDashboard []

tonicDashboard :: [TaskAppRenderer] -> Task ()
tonicDashboard rs = ((tonicStaticBrowser rs <<@ Title "Static Blueprints")
               -||- (tonicDynamicBrowser rs <<@ Title "Dynamic Blueprints")) <<@ ArrangeWithTabs False

tonicStaticWorkflow :: [TaskAppRenderer] -> Workflow
tonicStaticWorkflow rs = workflow "Tonic Static Browser" "Tonic Static Browser" (tonicStaticBrowser rs)

tonicDynamicWorkflow :: [TaskAppRenderer] -> Workflow
tonicDynamicWorkflow rs = workflow "Tonic Dynamic Browser" "Tonic Dynamic Browser" (tonicDynamicBrowser rs)

(>>~) infixl 1 :: !(Task a) !(a -> Task b) -> Task b | iTask a & iTask b
(>>~) taska taskbf = step taska (const Nothing) [OnValue (hasValue taskbf)]

tonicStaticBrowser :: [TaskAppRenderer] -> Task ()
tonicStaticBrowser rs
  =                withShared [] (
      \navstack -> (updateSharedInformation "Display settings" [] staticDisplaySettings
              -&&- (allBlueprints
  >>- \allbps   -> (selectModule
               >&> withSelection noModuleSelection (
      \mn       -> getModule mn
  >>- \tm       -> tonicBrowseWithModule allbps rs navstack tm
         )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
         )) ) @! ()
  where
  selectModule      = getTonicModules >>- enterChoice "Select a module" [ChooseFromDropdown id]
  noModuleSelection = viewInformation () [] "Select module..."

tonicBrowseWithModule :: AllBlueprints [TaskAppRenderer] (Shared sds NavStack) TonicModule -> Task () | RWShared sds
tonicBrowseWithModule allbps rs navstack tm
  =           (selectTask tm
           >&> withSelection noTaskSelection (
  \tn       -> maybe (return ()) (
  \tt       ->   whileUnchanged staticDisplaySettings (
  \sett     ->   (if (sett.StaticDisplaySettings.show_comments && tt.tf_comments <> "")
                    (viewInformation "Task comments" [] tt.tf_comments @! ())
                    (return ()))
                 -&&-
                 viewStaticTask allbps rs navstack { BlueprintIdent
                                                   | bpr_moduleName = tm.tm_name
                                                   , bpr_taskName   = tt.tf_name
                                                   } tm tt sett.StaticDisplaySettings.unfold_depth sett.StaticDisplaySettings.display_compact @! ()))
               (getTonicFunc tm tn)
     )) <<@ ArrangeWithSideBar 0 LeftSide 200 True
        @! ()
  where
  selectTask tm   = enterChoice "Select task" [ChooseFromDropdown id] (getTasks tm)
  noTaskSelection = viewInformation () [] "Select task..."


viewStaticTask :: !AllBlueprints ![TaskAppRenderer] !(Shared sds NavStack) !BlueprintIdent !TonicModule !TonicFunc !Int !Bool -> Task () | RWShared sds
viewStaticTask allbps rs navstack bpref tm tt depth compact
  =          get navstack
  >>~ \ns -> (showStaticBlueprint rs bpref (expandTask allbps depth tt) compact depth
         >>* [ OnValue (doAction (handleClicks tm tt))
             , OnAction (Action "Back") (navigateBackwards tm tt ns)
             ] @! ()) <<@ ApplyLayout (layoutSubUIs (SelectByType UIAction) (setActionIcon ('DM'.fromList [("Back","Previous")])))
  where

  navigateBackwards :: TonicModule TonicFunc NavStack a -> Maybe (Task ())
  navigateBackwards _  _  []           _ = Nothing
  navigateBackwards tm tt [prev:stack] _ = navigateBackwards` prev
    where
    navigateBackwards` :: ClickMeta -> Maybe (Task ())
    navigateBackwards` meta`=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_compName, bpident_compId = Just tid}}
      =                 Just (upd pop navstack
      >>|               get dynamicDisplaySettings
      >>~ \sett ->      get selectedDetail
      >>~ \selDetail -> get (sdsFocus (comp2TaskId tid, bpident_moduleName, bpident_compName) tonicInstances)
      >>~ \mbpref ->    case mbpref of
                          Just bpref` -> viewInstance rs navstack sett bpref` selDetail meta`
                          _           -> return ())
    navigateBackwards` meta=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_compName}}
      =   Just (upd pop navstack
      >>| getModule bpident_moduleName
      >>* [ OnValue (onNavVal bpident_compName)
          , OnAllExceptions (const (viewInformation "Error" [] "Something went wrong with navigating backwards" @! ()))
          ] @! ())
      where
      onNavVal bpident_compName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack {bpr_moduleName = bpident_moduleName, bpr_taskName = bpident_compName} tm` tt` depth compact @! ()) (getTonicFunc tm` bpident_compName)
      onNavVal _                _             = Nothing
    navigateBackwards` _ = Nothing
    pop [] = []
    pop [_:xs] = xs

  handleClicks :: TonicModule TonicFunc (TClickAction, ClickMeta) a -> Task ()
  handleClicks tm tt (TNavAction, meta) _ = navigate (\ns -> [meta : ns]) tm tt meta
  handleClicks tm tt _                  _ = viewStaticTask allbps rs navstack bpref tm tt depth compact

  navigate :: (NavStack -> NavStack) TonicModule TonicFunc ClickMeta -> Task ()
  navigate mkNavStack _ _ meta`=:{click_target_bpident = {bpident_moduleName, bpident_compName, bpident_compId = Just tid}}
    =              get (sdsFocus (comp2TaskId tid, bpident_moduleName, bpident_compName) tonicInstances)
    >>~ \mbpref -> case mbpref of
                     Just bpref`
                       =                   upd mkNavStack navstack
                         >>|               get dynamicDisplaySettings
                         >>~ \sett ->      get selectedDetail
                         >>~ \selDetail -> viewInstance rs navstack sett bpref` selDetail meta`
                     _ = return ()
  navigate mkNavStack tm tt meta=:{click_target_bpident = {bpident_moduleName, bpident_compName}}
    =   upd mkNavStack navstack
    >>| getModule bpident_moduleName
    >>* [ OnValue (onNavVal bpident_compName)
        , OnAllExceptions (const (viewStaticTask allbps rs navstack bpref tm tt depth compact))
        ] @! ()
    where
    onNavVal bpident_compName (Value tm` _) = fmap (\tt` -> viewStaticTask allbps rs navstack {bpr_moduleName = bpident_moduleName, bpr_taskName = bpident_compName} tm` tt` depth compact @! ()) (getTonicFunc tm` bpident_compName)
    onNavVal _                _             = Nothing

showBlueprintInstance :: ![TaskAppRenderer] !BlueprintInstance
                         !(Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int)))
                         !(Map ExprId [UI]) !Bool !Int
                      -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showBlueprintInstance rs bpi selDetail enabledSteps compact depth
  =               get (mapRead (fmap (\(_, _, _, x) -> x)) storedOutputEditors)
  >>~ \outputs -> let outputs` = 'DM'.foldlWithKey (\m (tid, eid) v -> if (tid == bpi.bpi_taskId)
                                                                         ('DM'.put eid v m)
                                                                         m) 'DM'.newMap outputs
                   in updateInformation ()
                        [UpdateUsing id (const id) (editor outputs`)]
                        { ActionState
                        | state  = { tis_task    = bpi.bpi_blueprint
                                   , tis_depth   = depth
                                   , tis_compact = compact }
                        , action = Nothing}
  where
  editor outputs` = fromSVGEditor
    { initView    = id
    , renderImage = \_ -> mkTaskInstanceImage rs bpi outputs` enabledSteps selDetail compact
    , updModel    = \x _ -> x
    }

showStaticBlueprint :: ![TaskAppRenderer] !BlueprintIdent !TonicFunc !Bool !Int
                    -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showStaticBlueprint rs bpref task compact depth
  = updateInformation ()
      [UpdateUsing id (const id) editor]
      { ActionState
      | state  = { tis_task    = task
                 , tis_depth   = depth
                 , tis_compact = compact }
      , action = Nothing}
  where
  editor = fromSVGEditor
    { initView    = id
    , renderImage = \_ -> mkStaticImage rs bpref compact
    , updModel    = \x _ -> x
    }

enterQuery :: Task (Maybe BlueprintQuery)
enterQuery = enterInformation "Enter filter query" []

tonicDynamicBrowser :: [TaskAppRenderer] -> Task ()
tonicDynamicBrowser rs
  =            withShared [] (
  \navstack -> (parallel [ (Embedded, \_ -> tonicDynamicBrowser` rs navstack)
                         , (Embedded, \_ -> settingsViewer)
                         , (Embedded, \_ -> filterQuery)
                         , (Embedded, \_ -> activeUsers)
                         , (Embedded, \_ -> taskViewer)
                         ] [] /*<<@ ArrangeCustom layout*/
               )) @! ()
  where
/*
  layout [mainTask, settingsTask, filterTask, usersTask : _] actions
    = arrangeWithSideBar 0 RightSide 250 True [supportArea, mainTask] actions
    where
    supportArea = arrangeWithSideBar 0 TopSide 200 False [settingsTask, filterTask, usersTask] []
*/

  filterQuery = updateSharedInformation (Title "Filter query") [] queryShare @! ()

  taskViewer = whileUnchanged dynamicDisplaySettings (
            \{show_task_value} -> if show_task_value
                                    (whileUnchanged selectedDetail viewDetail /*<<@ InWindow*/)
                                    (viewInformation () [] ())
               ) @! ()
    where
    viewDetail (Just (Left { click_origin_mbbpident = Just {bpident_compId = Just tid}
                           , click_origin_mbnodeId  = Just nid })) = whileUnchanged (sdsFocus (comp2TaskId tid, nid) outputForTaskId) (\(_, _, x, _) -> x)
    viewDetail (Just (Left {click_target_bpident = {bpident_compId = Nothing}}))  = viewInformation (Title "Notice") [] "No data available for selected task. " @! ()
    viewDetail (Just (Right (mn, tn, tid, argIdx))) =                get (sdsFocus (mn, tn, comp2TaskId tid) paramsForTaskInstance)
                                                      >>~ \params -> case getN params argIdx of
                                                                       Just (_, _, vi) -> viewInformation (Title ("Selected argument (" +++ toString tid +++ ")")) [] () ||- vi
                                                                       _               -> viewInformation (Title "Notice") [] "Argument value not found" @! ()
      where
      getN []     _ = Nothing
      getN [x:_]  0 = Just x
      getN [_:xs] n
        | n < 0     = Nothing
        | otherwise = getN xs (n - 1)
    viewDetail _ = viewInformation (Title "Task viewer") [] "Select dynamic task" @! ()

  settingsViewer :: Task ()
  settingsViewer
    =   updateSharedInformation (Title "Settings") [] dynamicDisplaySettings @! ()

  windowIf True t = t <<@ InWindow
  windowIf _    _ = return ()

  activeUsers :: Task ()
  activeUsers = return ()
    //=           get currentTaskInstanceNo
    //>>- \ino -> let filter = { onlyInstanceNo    = Nothing
                             //, notInstanceNo     = Just [ino]
                             //, onlySession       = Nothing
                             //, includeConstants  = False
                             //, includeProgress   = True
                             //, includeAttributes = True
                             //}
                //in whileUnchanged (sdsFocus filter filteredInstanceIndex) (
        //\idatas -> let userData = mergeSortBy (\(l, _) (r, _) -> l <= r) (nub [(usr, dt) \\ (Ok usr, dt) <- map (\(_,_, Just {InstanceProgress | lastIO}, Just attributes) -> (userFromAttr () attributes, lastIO)) idatas | usr <> SystemUser])
                   //in  get currentDateTime >>= \currDT -> enterChoice (Title "Active users") [ChooseWith (ChooseFromGrid (mkUsersView currDT))] userData
      //) @! ()

//:: UsersView = { username :: User, inactivity :: String }
//derive class iTask UsersView

//mkUsersView :: DateTime (User, Maybe DateTime) -> UsersView
//mkUsersView currDT (usr, Just mLastIO)
  //# (DateTime _ dt) = currDT - mLastIO
  //# st = if (dt.Time.min > 0) "> 1m" (if (dt.Time.sec > 30) "> 30s" "")
  //= { username = usr, inactivity = st }
//mkUsersView currDT (usr, _) = { username = usr, inactivity = ""}

//merge _ []         ys = ys
//merge _ xs         [] = xs
//merge f xs=:[x:xt] ys=:[y:yt]
  //| f x y    = [x : merge f xt ys]
  //| otherwise = [y : merge f xs yt]

//split [x:y:zs] = let (xs,ys) = split zs in ([x:xs], [y:ys])
//split [x]      = ([x],[])
//split []       = ([],[])

//mergeSortBy _ []  = []
//mergeSortBy _ [x] = [x]
//mergeSortBy f xs
  //# (as,bs) = split xs
  //= merge f (mergeSortBy f as) (mergeSortBy f bs)

tonicDynamicBrowser` :: [TaskAppRenderer] (Shared sds NavStack) -> Task () | RWShared sds
tonicDynamicBrowser` rs navstack =
  ((activeBlueprintInstances -&&- blueprintViewer) /* <<@ ArrangeVertical */) @! ()
where
  activeBlueprintInstances = editSharedChoiceWithSharedAs
                               (Title "Active blueprint instances")
                               [ChooseFromGrid customView]
                               (mapRead (\(trt, q) -> filterActiveTasks q (flattenRTMap trt)) (tonicSharedRT |*| queryShare))
                               setTaskId selectedBlueprint <<@ ArrangeWithSideBar 0 TopSide 175 True
  where
    setTaskId x = { click_origin_mbbpident  = Nothing
                  , click_origin_mbnodeId   = Nothing
                  , click_target_bpident    = { bpident_moduleName = x.bpi_bpref.bpr_moduleName
                                              , bpident_compName   = x.bpi_bpref.bpr_taskName
                                              , bpident_compId     = Just (toComp x.bpi_taskId)
                                              }
                  }

    flattenRTMap :: TonicRTMap -> [BlueprintInstance]
    flattenRTMap trt = 'DM'.elems ('DM'.foldrWithKey f 'DM'.newMap trt)
      where
      f :: TaskId [((ModuleName, FuncName), BlueprintInstance)] (Map (TaskId, ModuleName, FuncName) BlueprintInstance) -> Map (TaskId, ModuleName, FuncName) BlueprintInstance
      f tid m acc = foldr (g tid) acc m
      g :: TaskId ((ModuleName, FuncName), BlueprintInstance) (Map (TaskId, ModuleName, FuncName) BlueprintInstance) -> Map (TaskId, ModuleName, FuncName) BlueprintInstance
      g tid ((mn, fn), bpi) acc = 'DM'.put (tid, mn, fn) bpi acc

  blueprintViewer
    = whileUnchanged (selectedBlueprint |*| navstack) (
        \(bpmeta, ns) -> case bpmeta of
                           Just meta=:{click_target_bpident = {bpident_compId = Just tid, bpident_moduleName, bpident_compName}}
                             # focus = (sdsFocus (comp2TaskId tid, bpident_moduleName, bpident_compName) tonicInstances)
                             =                 get focus
                             >>~ \mbprnt ->    get selectedDetail
                             >>~ \selDetail -> whileUnchanged (focus |*| dynamicDisplaySettings) (
                                                 \shareData ->
                                                    case shareData of
                                                       (Just bpinst, dynSett) ->     viewInstance rs navstack dynSett bpinst selDetail meta
                                                                                 >>*   [ OnAction (Action "Back") (navigateBackwards dynSett selDetail ns)
                                                                                       //, OnAction (Action "Parent task")     (\_ -> navToParent bpinst dynSett selDetail tid rs mbprnt)
                                                                                       ]
                                                       _                      -> return ()
                                               )

                           _ = viewInformation () [] "Please select a blueprint" @! ()
      )<<@ ApplyLayout (layoutSubUIs (SelectByType UIAction) (setActionIcon ('DM'.fromList [("Back","previous"),("Parent task","open")])))
     where

     navigateBackwards :: !DynamicDisplaySettings !(Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int))) NavStack a -> Maybe (Task ())
     navigateBackwards _ _ [] _ = Nothing
     navigateBackwards dynSett selDetail [prev:stack] _ = navigateBackwards` prev
       where
       navigateBackwards` :: ClickMeta -> Maybe (Task ())
       navigateBackwards` meta`=:{click_origin_mbbpident = Just {bpident_compId = Just tid, bpident_moduleName, bpident_compName}}
         =                 Just (upd pop navstack
         >>|               get (sdsFocus (comp2TaskId tid, bpident_moduleName, bpident_compName) tonicInstances)
         >>~ \mbpref ->    case mbpref of
                             Just bpref` -> viewInstance rs navstack dynSett bpref` selDetail meta`
                             _           -> return ())
       navigateBackwards` meta=:{click_origin_mbbpident = Just {bpident_moduleName, bpident_compName}}
         =   Just (upd pop navstack
         >>| getModule bpident_moduleName
         >>* [ OnValue (onNavVal bpident_compName)
             , OnAllExceptions (const (viewInformation "Error" [] "Something went wrong with navigating backwards" @! ()))
             ] @! ())
         where
         onNavVal bpident_compName (Value tm` _) = fmap (\tt` -> allBlueprints >>- \allbps -> viewStaticTask allbps rs navstack {bpr_moduleName = bpident_moduleName, bpr_taskName = bpident_compName} tm` tt` dynSett.DynamicDisplaySettings.unfold_depth dynSett.DynamicDisplaySettings.display_compact @! ()) (getTonicFunc tm` bpident_compName)
         onNavVal _                _             = Nothing
       navigateBackwards` _ = Nothing
       pop [] = []
       pop [_:xs] = xs

  filterActiveTasks Nothing instances = instances
  filterActiveTasks (Just q) instances
    = [bpi \\ bpi <- instances | not (startsWith "iTasks" bpi.bpi_bpref.bpr_moduleName) && isNothing bpi.bpi_endTime && doFilter bpi q]
    where
    doFilter {bpi_bpref = {bpr_taskName}}  (FuncName tn)     = tn == "" || indexOf tn bpr_taskName >= 0
    doFilter {bpi_currentUser = Just u}    (UserInvolved un) = un == "" || indexOf un (toString u) >= 0
    doFilter {bpi_endTime}                 IsActiveTask      = isNothing bpi_endTime
    doFilter {bpi_taskId = TaskId tinst _} (HasInstanceNo n) = tinst == n
    doFilter bpi                           (AndQuery l r)    = doFilter bpi l && doFilter bpi r
    doFilter bpi                           (OrQuery l r)     = doFilter bpi l || doFilter bpi r
    doFilter _                             _                 = True
  customView bpi=:{bpi_bpref = {bpr_moduleName, bpr_taskName}}
    = { DynamicView
      | taskName    = bpr_moduleName +++ "." +++ bpr_taskName +++ " (" +++ toString bpi.bpi_taskId +++ ")"
      , startTime   = toString bpi.bpi_startTime
      , lastUpdate  = toString bpi.bpi_lastUpdated
      , endTime     = maybe "" toString bpi.bpi_endTime
      , user        = maybe "" toString bpi.bpi_currentUser
      }

getModuleAndTask :: !AllBlueprints !ModuleName !FuncName -> Task (TonicModule, TonicFunc)
getModuleAndTask allbps mn tn
  =           getModule mn
  >>~ \mod -> case 'DM'.get mn allbps `b` 'DM'.get tn of
                Just tt -> return (mod, tt)
                _       -> throw "Can't get module and task"

viewInstance :: ![TaskAppRenderer] !(Shared sds NavStack) !DynamicDisplaySettings !BlueprintInstance
                !(Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int))) !ClickMeta
             -> Task () | RWShared sds
viewInstance rs navstack dynSett bpinst=:{bpi_bpref = {bpr_moduleName, bpr_taskName}} selDetail meta=:{click_target_bpident = {bpident_compId = Just tid}}
  = (if (dynSett.DynamicDisplaySettings.show_comments && bpinst.bpi_blueprint.tf_comments <> "")
       (viewInformation "Task comments" [] bpinst.bpi_blueprint.tf_comments @! ())
       (return ()))
    -&&-
    ((whileUnchanged (sdsFocus bpinst.bpi_taskId tonicActionsForTaskID) (
        \steps -> showBlueprintInstance rs bpinst selDetail steps False 0)
    -|| showChildTasks dynSett bpinst)
    >>* [OnValue (doAction (handleClicks bpr_moduleName bpr_taskName))]) @! ()
  where
  showChildTasks :: DynamicDisplaySettings BlueprintInstance -> Task ()
  showChildTasks {DynamicDisplaySettings | show_all_child_tasks = False, unfold_depth = 0} bpinst = return ()
  showChildTasks {DynamicDisplaySettings | show_all_child_tasks, unfold_depth = d, show_finished_blueprints } bpinst
    # childIds  = getActiveCompIds bpinst // TODO: Should this be retrieved from the runtime map share instead?
    # childIds  = if show_finished_blueprints
                    ('DM'.elems (getPreviouslyActive bpinst) ++ childIds)
                    childIds
    # viewTasks = map (\childId -> whileUnchanged (sdsFocus (comp2TaskId childId) allTonicInstances) (
                       \mbpref ->  case [bpi \\ ((bpiMn, bpiTn), bpi=:{bpi_taskId, bpi_index}) <- mbpref
                                              | (bpi_taskId > bpinst.bpi_taskId || (bpi_taskId == bpinst.bpi_taskId && bpi_index > bpinst.bpi_index))
                                                && not (bpi_taskId == bpinst.bpi_taskId && bpiMn == bpr_moduleName && bpiTn == bpr_taskName)
                                        ] of
                                     [bpref` : _]
                                       # dynSett = if show_all_child_tasks dynSett
                                                     {DynamicDisplaySettings | dynSett & unfold_depth = d - 1}
                                       = viewInstance rs navstack dynSett bpref` selDetail (mkClickMeta childId)
                                     _ = return ())) childIds
    = allTasks viewTasks @! ()
    where
    mkClickMeta childId = {meta & click_origin_mbbpident = Nothing
                                , click_origin_mbnodeId  = Nothing
                                , click_target_bpident   = { bpident_compId     = Just childId
                                                           , bpident_moduleName = ""
                                                           , bpident_compName   = ""
                                                           }
                          }

  handleClicks :: !ModuleName !FuncName !(TClickAction, ClickMeta) (ActionState (TClickAction, ClickMeta) TonicImageState) -> Task ()
  handleClicks _ _ (TNavAction, meta`) _
    =   upd (\xs -> [meta` : xs]) navstack
    >>| viewInstance rs navstack dynSett bpinst selDetail meta`
  handleClicks _ _ (TDetailAction, meta) _
    =   set (Just (Left meta)) selectedDetail
    >>| viewInstance rs navstack dynSett bpinst (Just (Left meta)) meta
  handleClicks mn tn (TSelectArg i, meta) _
    =   set (Just (Right (mn, tn, tid, i))) selectedDetail
    >>| viewInstance rs navstack dynSett bpinst (Just (Right (mn, tn, tid, i))) meta
  handleClicks _ _ _ _ = viewInstance rs navstack dynSett bpinst selDetail meta

  noSelection :: Task String
  noSelection = viewInformation () [] "Select argument..."

  collectArgs :: !BlueprintIdent !BlueprintInstance !TonicFunc -> Task [(String, Task ())]
  collectArgs bpref bpinst graph = mkInstantTask f
    where
    f _ iworld
      # (mparams, iworld) = 'DSDS'.read (sdsFocus (bpref.bpr_moduleName, bpref.bpr_taskName, bpinst.bpi_taskId) paramsForTaskInstance) iworld
      = case mparams of
          Ok params -> (Ok (zipWith (\(argnm, argty) (_, vi) -> (ppTExpr argnm +++ " :: " +++ ppTExpr argty, vi)) graph.tf_args params), iworld)
          _         -> (Ok [], iworld)

viewInstance rs navstack dynSett bpinst selDetail {click_target_bpident = {bpident_moduleName, bpident_compName}}
  =                allBlueprints
  >>- \allbps   -> getModuleAndTask allbps bpident_moduleName bpident_compName
  >>- \(tm, tt) -> viewStaticTask allbps rs navstack bpinst.bpi_bpref tm tt 0 False

pp3 (x, y, ns) = toString x +++ " " +++ toString y +++ " " +++ toString ns

expandTask :: !AllBlueprints !Int !TonicFunc -> TonicFunc
expandTask allbps n tt
  | n > 0     = {tt & tf_body = expandTExpr allbps n tt.tf_body}
  | otherwise = tt

expandTExpr :: !AllBlueprints !Int !TExpr -> TExpr
expandTExpr _      0 texpr = texpr
expandTExpr allbps n (TFApp eid vn args assoc)
  = TFApp eid vn (map (expandTExpr allbps n) args) assoc
expandTExpr allbps n texpr=:(TMApp eid mtn mn tn args assoc ptr)
  = case 'DM'.get mn allbps >>= 'DM'.get tn of
      Just tt
        = TExpand args (expandTask allbps (n - 1) tt)
      _ = TMApp eid mtn mn tn (map (expandTExpr allbps n) args) assoc ptr
expandTExpr allbps n (TLet pats bdy)
  = TLet (map f pats) (expandTExpr allbps n bdy)
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
expandTExpr allbps n (TIf cs c t e)
  = TIf cs c (expandTExpr allbps n t) (expandTExpr allbps n e)
expandTExpr allbps n (TCase cs e pats)
  = TCase cs (expandTExpr allbps n e)
              (map f pats)
  where
  f (pat, rhs) = (pat, expandTExpr allbps n rhs)
expandTExpr allbps n (TExpand vars tt)
  = TExpand vars (expandTask allbps n tt)
expandTExpr allbps n (TSel e es)
  = TSel (expandTExpr allbps n e) (map (expandTExpr allbps n) es)
expandTExpr allbps n (TRecUpd vn e es)
  = TRecUpd vn (expandTExpr allbps n e) (map (expandTExpr allbps n) es)
expandTExpr allbps n (TLam vars e)
  = TLam vars (expandTExpr allbps n e)
expandTExpr _ _ texpr = texpr

