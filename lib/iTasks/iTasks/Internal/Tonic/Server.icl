implementation module iTasks.Internal.Tonic.Server

import iTasks
from Text import class Text, instance Text String
import qualified Text as T
import qualified Data.Map as DM
import Data.Map.GenJSON
from Data.Map import :: Map
import qualified Data.IntMap.Strict as DIS
import qualified Data.List as DL
import Data.Error 
from Data.IntMap.Strict import :: IntMap
import iTasks.Internal.Tonic.Blueprints
import iTasks.Extensions.Admin.TonicAdmin
import iTasks.Extensions.SVG.SVGEditor
import iTasks.UI.JS.Encoding
import iTasks.Extensions.DateTime
import iTasks.Internal.Tonic.AbsSyn
import iTasks.Internal.Tonic.Types
import iTasks.Internal.Tonic.Images
from iTasks.Internal.IWorld import :: ConnectionId

:: ViewerSettings =
  { recording         :: Bool
  , selectedBlueprint :: Maybe TMNewTopLevel
  }

derive class iTask ViewerSettings

shViewerSettings :: SimpleSDSLens ViewerSettings
shViewerSettings = sharedStore "shViewerSettings" { recording = True
                                                  , selectedBlueprint = Nothing
                                                  }

foldT_ :: (a -> Task ()) [a] -> Task ()
foldT_ f []       = return ()
foldT_ f [x : xs] = f x >>| foldT_ f xs

liveRunStateShare :: SimpleSDSLens TonicGenRTMap
liveRunStateShare = sharedStore "liveRunStateShare" 'DM'.newMap

recordingsShare :: SimpleSDSLens (Map DateTime [TonicMessage])
recordingsShare = sharedStore "recordingsShare" 'DM'.newMap

recordingForDateTimeShare :: SDSLens DateTime [TonicMessage] ()
recordingForDateTimeShare = toReadOnly (mapLens "recordingForDateTimeShare" recordingsShare (Just []))

newRTMapFromMessages :: [TonicMessage] -> Task TonicGenRTMap
newRTMapFromMessages xs = updRTMapFromMessages xs 'DM'.newMap

updRTMapFromMessages :: [TonicMessage] TonicGenRTMap -> Task TonicGenRTMap
updRTMapFromMessages []         rtMap = return rtMap
updRTMapFromMessages [msg : xs] rtMap = processMessage msg rtMap >>= updRTMapFromMessages xs

// Partial function :(
mkParentId :: ComputationId -> ComputationId
mkParentId [x : xs] = xs

processMessage :: TonicMessage TonicGenRTMap -> Task TonicGenRTMap
processMessage (TMNewTopLevel tmn) rtMap
  =           getModule tmn.tmn_bpModuleName
  >>= \mod -> case getTonicFunc mod tmn.tmn_bpFunctionName of
                Just func
                  # bpinst = { GenBlueprintInstance
                             | gbpi_computationId    = tmn.tmn_computationId
                             , gbpi_activeNode       = ([], []) // TODO Better representation? Or better default?
                             , gbpi_previouslyActive = 'DM'.newMap
                             , gbpi_parentId         = mkParentId tmn.tmn_computationId
                             , gbpi_blueprint        = func
                             , gbpi_case_branches    = 'DM'.newMap
                             , gbpi_bpref            = { BlueprintIdent
                                                       | bpr_moduleName = tmn.tmn_bpModuleName
                                                       , bpr_taskName   = tmn.tmn_bpFunctionName
                                                       }
                             }
                  = return (insertIntoRTMap bpinst rtMap)
                _ = return rtMap
  where
  insertIntoRTMap bpinst rtMap
    # comps = case 'DM'.get tmn.tmn_computationId rtMap of
                Just xs -> xs
                _       -> []
    # comps = comps ++ [((tmn.tmn_bpModuleName, tmn.tmn_bpFunctionName), bpinst)]
    = 'DM'.put tmn.tmn_computationId comps rtMap
processMessage (TMApply tma) rtMap
  # mParentBP = readRTMap (mkParentId tma.tma_computationId) tma.tma_bpModuleName tma.tma_bpFunctionName rtMap
  = case mParentBP of
      Just parentBPInst
        = return (updateRTMap tma parentBPInst rtMap)
      _ = return rtMap
  where
  readRTMap :: ComputationId ModuleName FuncName TonicGenRTMap -> Maybe GenBlueprintInstance
  readRTMap bpId mn tn rtMap
    = case 'DM'.get bpId rtMap of
        Just xs -> case [bp \\ ((mn`, fn), bp) <- xs | mn == mn` && tn == fn] of
                     [x : _] -> Just x
                     _       -> Nothing
        _ -> Nothing
  updateRTMap :: TMApply GenBlueprintInstance TonicGenRTMap -> TonicGenRTMap
  updateRTMap tma parentBPInst rtMap
    # oldActiveNodes = 'DM'.put (snd parentBPInst.gbpi_activeNode) (fst parentBPInst.gbpi_activeNode) parentBPInst.gbpi_previouslyActive
    # newParent      = { parentBPInst
                       & gbpi_activeNode       = (tma.tma_computationId, tma.tma_nodeId)
                       , gbpi_previouslyActive = oldActiveNodes}
    = case 'DM'.get tma.tma_computationId rtMap of
        Just [(x, _) : xs] // TODO Really? How do we determine which one to write to?
          = 'DM'.put tma.tma_computationId [(x, newParent) : xs] rtMap
        _ = rtMap

import StdMisc
showGenBlueprintInstance :: ![TaskAppRenderer] !GenBlueprintInstance
                            !(Maybe (Either ClickMeta (ModuleName, FuncName, ComputationId, Int)))
                            !Bool !Int
                         -> Task (ActionState (TClickAction, ClickMeta) TonicImageState)
showGenBlueprintInstance rs bpi selDetail compact depth
  = updateInformation ()
      [abort "huehue" /*imageUpdate id (\_ -> mkGenInstanceImage rs bpi selDetail compact) (const id) (const id)  (\_ _ -> Nothing) (const id) */]
      { ActionState
      | state  = { tis_task    = bpi.gbpi_blueprint
                 , tis_depth   = depth
                 , tis_compact = compact }
      , action = Nothing
      }

// TODO FIXME:
// - Indices might be calculated incorrectly
// - Flatten the list of instances
archivedStandAloneViewer :: Task ()
archivedStandAloneViewer
  = archivedStandAloneViewer` 0
  where
  archivedStandAloneViewer` curIdx
    =            enterChoiceWithShared "Select recording" [] (mapRead 'DM'.keys recordingsShare)
    >&>          withSelection noSel1
    (\dt ->      get (sdsFocus dt recordingForDateTimeShare)
    >>~ \recs -> showRecs curIdx recs)
  showRecs curIdx recs
    # numMsgs  = length recs
    # lastIdx  = numMsgs - 1
    # notFirst = curIdx > 0
    # notLast  = curIdx < numMsgs - 1
    =                newRTMapFromMessages (take (curIdx + 1) recs)
    >>~ \newRTMap -> archivedStandAloneViewer`` curIdx newRTMap
    >>* [ OnAction (Action "First")    (ifCond notFirst (showRecs 0 recs))
        , OnAction (Action "Previous") (ifCond notFirst (showRecs (curIdx - 1) recs))
        , OnAction (Action "Next")     (ifCond notLast  (showRecs (curIdx + 1) recs))
        , OnAction (Action "Last")     (ifCond notLast  (showRecs lastIdx recs))
        ]
  archivedStandAloneViewer`` curIdx newRTMap
    =   enterChoice "Select blueprint" [ChooseFromGrid (\(x, y, z, _) -> (x, y, z))] (flattenRTMap newRTMap)
    >&> withSelection noSel2 viewBP
  noSel1 = viewInformation "Notice" [] "No recording selected"
  noSel2 = viewInformation "Notice" [] "No blueprint"
  viewBP :: (ComputationId, ModuleName, FuncName, GenBlueprintInstance) -> Task ()
  viewBP (cid, _, _, gbpi) = showGenBlueprintInstance [] gbpi Nothing False 0 @! () // TODO Enable controls

flattenRTMap :: TonicGenRTMap -> [(ComputationId, ModuleName, FuncName, GenBlueprintInstance)]
flattenRTMap m = flatten (flattenRTMap` ('DM'.toList m))
  where
  flattenRTMap` [] = []
  flattenRTMap` [(cid, ys) : xs] = [map (\((mn, fn), gbpi) -> (cid, mn, fn, gbpi)) ys : flattenRTMap` xs]

:: TonicGenRTMap :== Map ComputationId [((ModuleName, FuncName), GenBlueprintInstance)]

saSelectedBlueprint :: SimpleSDSLens (Maybe (ComputationId, BlueprintIdent))
saSelectedBlueprint = sharedStore "saSelectedBlueprint" Nothing

liveStandAloneViewer :: Task ()
liveStandAloneViewer = allTasks [ updateSharedInformation "Viewer settings" [] shViewerSettings @! ()
             , startViewer @! ()
             ] @! ()
where
  startViewer
    =   enterChoiceWithShared "Select blueprint" [] (mapRead (\ts -> 'DL'.concatMap f ts.ts_allMsgs) tonicServerShare)
    >&> withSelection noSel (
    (\bp -> whileUnchanged (tonicServerShare |*| shViewerSettings)
    (\x=:(tms, _) -> (runViewer x -|| forever (viewInformation () [] () >>* [ startAction tms
                                                                            , pauseAction tms
                                                                            , continueAction tms
                                                                            , stopAction tms
                                                                            ])))))
  where
    startAction :: TMessageStore -> TaskCont a (Task ())
    startAction {ts_recording} = OnAction (Action "Start new recording") (ifCond (not ts_recording) startTask)
    where
      startTask
        =   upd (\ts -> {ts & ts_recording = True, ts_recordingBuffer = []}) tonicServerShare @! ()
    
    pauseAction :: TMessageStore -> TaskCont a (Task ())
    pauseAction {ts_recording} = OnAction (Action "Pause recording") (ifCond ts_recording stopTask)
    where
      stopTask
        =   upd (\ts -> {ts & ts_recording = False}) tonicServerShare @! ()
    
    continueAction :: TMessageStore -> TaskCont a (Task ())
    continueAction {ts_recording} = OnAction (Action "Continue recording") (ifCond (not ts_recording) stopTask)
    where
      stopTask
        =   upd (\ts -> {ts & ts_recording = True}) tonicServerShare @! ()
    
    stopAction :: TMessageStore -> TaskCont a (Task ())
    stopAction {ts_recording} = OnAction (Action "Pause and save recording") (ifCond ts_recording stopTask)
    where
      stopTask
        =           get tonicServerShare
        >>- \ts  -> get currentDateTime
        >>- \cdt -> upd ('DM'.put cdt ts.ts_recordingBuffer) recordingsShare
        >-|         upd (\ts -> {ts & ts_recording = False}) tonicServerShare @! ()
    
    refreshAction :: TaskCont a (Task ())
    refreshAction = OnAction (Action "Refresh") (always startViewer)

    noSel :: Task ()
    noSel = viewInformation "Notice" [] "No blueprint selected" @! ()
    f (TMNewTopLevel tl) = [tl]
    f _                  = []
  runViewer :: (TMessageStore, ViewerSettings) -> Task ()
  runViewer ({ts_allMsgs}, {selectedBlueprint = Just tmn})
    =                newRTMapFromMessages ts_allMsgs
    >>~ \newRTMap -> case 'DM'.get tmn.tmn_computationId newRTMap of
                       Just [(_, selBPI) : _]
                         = showGenBlueprintInstance [] selBPI Nothing False 0 @! () // TODO Enable controls
                       _ = startViewer
  runViewer x = viewInformation "Notice" [] "No blueprint selected" >>| runViewer x

viewMessage :: TonicMessage [TonicMessage] -> Task ()
viewMessage (TMNewTopLevel msg) prevMsgs
  = viewInformation () [] "Not implemented!" @! ()
viewMessage (TMApply msg) prevMsgs
  =           getModule msg.tma_bpModuleName
  >>= \mod -> case getTonicFunc mod msg.tma_bpFunctionName of
                Just func
                  # numPrev                     = length prevMsgs
                  # inst                        = mkInstance msg.tma_nodeId func
                  # inst & bpi_previouslyActive = 'DM'.fromList [(msg.tma_nodeId, TaskId 1 i) \\ TMApply msg <- prevMsgs & i <- reverse [0..numPrev]]
                  # currActive                  = [(eid, tid) \\ (_, m) <- 'DM'.toList inst.bpi_activeNodes, (_, (tid, eid)) <- 'DIS'.toList m]
                  # inst & bpi_previouslyActive = 'DM'.union ('DM'.fromList currActive) inst.bpi_previouslyActive
                  # inst & bpi_activeNodes      = case currActive of
                                                    [(_, TaskId ino tid) : _] -> 'DM'.put (TaskId 1 0) ('DIS'.singleton 0 (TaskId ino numPrev, msg.tma_nodeId)) inst.bpi_activeNodes
                  = viewInstance inst
                _ = viewInformation () [] "No blueprint found!" @! ()

viewInstance :: !BlueprintInstance -> Task ()
viewInstance bpi=:{bpi_blueprint, bpi_bpref = {bpr_moduleName, bpr_taskName}} = return ()
/*
  = updateInformation ()
      [imageUpdate id (\_ -> mkTaskInstanceImage [] bpi 'DM'.newMap 'DM'.newMap Nothing False) (const id) (const id) (\_ _ -> Nothing) (const id)]
      { ActionState
      | state  = { tis_task    = bpi.bpi_blueprint
                 , tis_depth   = 0
                 , tis_compact = False }
      , action = Nothing}
      @! ()
*/

nulDT = toDateTime { Date | day = 0, mon = 0, year = 0 } { Time | hour = 0, min = 0, sec = 0 }

mkInstance :: NodeId TonicFunc -> BlueprintInstance
mkInstance nid tf =
  { BlueprintInstance
  | bpi_taskId           = TaskId 1 0
  , bpi_startTime        = nulDT
  , bpi_lastUpdated      = nulDT
  , bpi_endTime          = Nothing
  , bpi_activeNodes      = 'DM'.singleton (TaskId 1 0) ('DIS'.singleton 0 (TaskId 1 1, nid))
  , bpi_previouslyActive = 'DM'.newMap
  , bpi_parentTaskId     = TaskId 0 0
  , bpi_currentUser      = Nothing
  , bpi_blueprint        = tf
  , bpi_case_branches    = 'DM'.newMap
  , bpi_index            = 0
  , bpi_bpref            = { BlueprintIdent
                           | bpr_moduleName = tf.tf_module
                           , bpr_taskName   = tf.tf_name }
  }

messageArchive :: SimpleSDSLens [TonicMessage]
messageArchive = sharedStore "messageArchive" []


:: TMessageStore =
  { ts_recording       :: !Bool
  , ts_allMsgs         :: ![TonicMessage]
  , ts_recordingBuffer :: ![TonicMessage]
  }

derive class iTask TMessageStore

tonicServerShare :: SimpleSDSLens TMessageStore
tonicServerShare = sharedStore "tonicServerShare" { TMessageStore
                                                  | ts_recording       = True
                                                  , ts_allMsgs         = []
                                                  , ts_recordingBuffer = []
                                                  }

:: ServerState =
  { oldData  :: String
  , clientIp :: String
  }

derive class iTask ServerState

acceptAndViewTonicTraces :: Task ()
acceptAndViewTonicTraces
  = acceptTonicTraces tonicServerShare
      ||-
    viewSharedInformation "Logged traces" [] tonicServerShare @! ()

acceptTonicTraces :: !(Shared sds TMessageStore) -> Task [ServerState] | RWShared sds
acceptTonicTraces tonicShare
  = tcplisten 9000 True tonicShare { ConnectionHandlers
                                   | onConnect     = onConnect
                                   , onData        = onData
                                   , onShareChange = onShareChange
                                   , onDisconnect  = onDisconnect
                                   , onDestroy     = onDestroy
                                   }
  where
    onConnect :: ConnectionId String TMessageStore
              -> (MaybeErrorString ServerState, Maybe TMessageStore, [String], Bool)
    onConnect connId host olderMessages
    = ( Ok { oldData = ""
           , clientIp = host}
      , Just olderMessages
      , ["Welcome!"]
      , False)

    onData :: String ServerState TMessageStore
           -> (MaybeErrorString ServerState, Maybe TMessageStore, [String], Bool)
    onData newData st=:{oldData} tstate
        # collectedData       = oldData +++ 'T'.trim newData
        # (strmsgs, leftover) = partitionMessages ('T'.split "TONIC_EOL" collectedData)
        # tmsgs               = [msg \\ Just msg <- map strToMessage strmsgs]
        # tstate & ts_allMsgs = tmsgs ++ tstate.ts_allMsgs
        # tstate              = if tstate.ts_recording
                                  {tstate & ts_recordingBuffer = tmsgs ++ tstate.ts_recordingBuffer}
                                  tstate
        = (Ok {st & oldData = leftover}, Just tstate, [], False)
    where
        strToMessage :: !String -> Maybe TonicMessage
        strToMessage str = fromJSON (fromString str)

        partitionMessages :: [String] -> ([String], String)
        partitionMessages []  = ([], "")
        partitionMessages [x] = ([], x)
        partitionMessages [x:y:xs]
            # (msgs, leftover) = partitionMessages [y:xs]
            = ([x:msgs], leftover)

    onShareChange st _  = (Ok st, Nothing, [], False)

    onDisconnect :: ServerState TMessageStore
                 -> (MaybeErrorString ServerState, Maybe TMessageStore)
    onDisconnect st lines
        = (Ok st, Just lines)

	onDestroy st = (Ok st, [])
