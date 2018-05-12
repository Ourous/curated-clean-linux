implementation module iTasks.SDS.Combinators.Common

import StdTuple, StdList
import iTasks.SDS.Definition, iTasks.SDS.Combinators.Core, iTasks.SDS.Sources.Core
import iTasks.WF.Combinators.Core
from StdFunc import o, const, flip, id
from iTasks.Internal.Task import exception, :: TaskException
import qualified Data.Map as DM
import qualified Data.IntMap.Strict as DIS
from Data.IntMap.Strict import :: IntMap
from Data.Map import :: Map
import Data.Maybe, Data.Error, Data.Either, StdString
import Text.GenJSON
import System.FilePath

sdsFocus :: !p !(RWShared p r w) -> (RWShared p` r w) | iTask p & TC r & TC w
sdsFocus p sds = sdsTranslate ("("+++ toString (toJSON p)+++")/") (const p) sds

sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(RWShared p rs ws) -> RWShared p r w | iTask p & TC rs & TC ws
sdsProject read write sds = sdsLens "()" param` read` write` (SDSNotifyConst notify`) sds
where
    param` p = p
    read` = case read of
        SDSLensRead f = SDSRead (\p rs -> f rs)
        SDSConstRead f = SDSReadConst (\_ -> f)

    write` = case write of
        SDSLensWrite f = SDSWrite (\p rs w -> f rs w)
        SDSBlindWrite f = SDSWriteConst (\p w -> f w)
        SDSNoWrite      = SDSWriteConst (\p w -> Ok Nothing)
    notify` p w = const (const True)

sdsTranslate :: !String !(p -> ps) !(RWShared ps r w) -> RWShared p r w | iTask ps & TC r & TC w
sdsTranslate name param sds = sdsLens name param (SDSRead (\_ rs -> Ok rs)) (SDSWriteConst (\_ w -> Ok (Just w))) (SDSNotifyConst (\_ _ _ _-> True)) sds

sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(RWShared ps rs ws) -> RWShared p r w | iTask ps & iTask pn & TC rs  & TC ws
sdsSplit name param read write sds = sdsLens name param` (SDSRead read`) (SDSWrite write`) (SDSNotify notify`) sds
where
    param` p             = fst (param p)
    read` p rs           = Ok (read (snd (param p)) rs)
    write` p rs w        = Ok (Just (fst (write (snd (param p)) rs w)))
    notify` p rs w ts pq = (snd (write (snd (param p)) rs w)) ts (snd (param pq))

removeMaybe :: !(Maybe a) !(SDS p (Maybe a) (Maybe a)) -> SDS p a a | iTask p & TC a
removeMaybe defaultValue sds = sdsLens "removeMaybe" id (SDSRead read) (SDSWriteConst write) (SDSNotifyConst (\_ _ _ _-> True)) sds
where
	read p (Just r) = Ok r
	read p Nothing = maybe (Error (exception "Required value not available in shared data source")) Ok defaultValue
	
	write p w = Ok (Just (Just w))

mapRead :: !(r -> r`) !(RWShared p r w) -> RWShared p r` w | iTask p & TC r & TC w
mapRead read sds = mapReadError (\r -> Ok (read r)) sds

mapWrite :: !(w` r -> Maybe w) !(RWShared p r w) -> RWShared p r w` | iTask p & TC r & TC w
mapWrite write sds = mapWriteError (\r w -> Ok (write r w)) sds

mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(RWShared p r w) -> RWShared p r` w` | iTask p & TC r & TC w
mapReadWrite (read,write) sds = mapReadWriteError (\r -> Ok (read r), (\r w -> Ok (write r w))) sds

mapReadError :: !(r -> MaybeError TaskException r`) !(RWShared p r w) -> RWShared p r` w | iTask p & TC r & TC w
mapReadError read sds = sdsProject (SDSLensRead read) (SDSBlindWrite (Ok o Just)) sds

mapWriteError :: !(w` r -> MaybeError TaskException  (Maybe w)) !(RWShared p r w) -> RWShared p r w` | iTask p & TC r & TC w
mapWriteError write sds = sdsProject (SDSLensRead Ok) (SDSLensWrite (flip write)) sds
	
mapReadWriteError :: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w)) !(RWShared p r w) -> RWShared p r` w` | iTask p & TC r & TC w
mapReadWriteError (read,write) sds = sdsProject (SDSLensRead read) (SDSLensWrite (flip write)) sds

mapSingle :: !(RWShared p [r] [w]) -> (RWShared p r w) | iTask p & TC r & TC w
mapSingle sds = sdsProject (SDSLensRead read) (SDSBlindWrite write) sds
where
    read [x]    = Ok x
    read []     = Error (exception "List element not found")
    read _      = Error (exception "Multiple list elements found, expected only one")

    write x     = Ok (Just [x])

toReadOnly :: !(RWShared p r w) -> ROShared p r | iTask p & TC r & TC w
toReadOnly sds = sdsProject (SDSLensRead Ok) SDSNoWrite sds

toDynamic :: !(RWShared p r w) -> (RWShared p Dynamic Dynamic) | iTask p & TC r & TC w //FIXME: Use 1 lens directly
toDynamic sds = mapRead (\r -> (dynamic r :: r^)) (mapWrite (\(w :: w^) _ -> Just w) sds)

// START DEPRECATED
(>+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy) | iTask p & TC rx & TC ry & TC wx & TC wy
(>+<) sds1 sds2 = sdsParallel ">+<" (\p -> (p,p)) id (SDSWriteConst write1) (SDSWriteConst write2) sds1 sds2
where
    write1 _ w = Ok (Just (fst w))
    write2 _ w = Ok (Just (snd w))

(>+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wx | iTask p & TC rx & TC ry & TC wx & TC wy
(>+|) srcX srcY = mapWrite (\wx _ -> Just (wx, ())) (srcX >+< toReadOnly srcY)

(|+<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wy | iTask p & TC rx & TC ry & TC wx & TC wy
(|+<) srcX srcY = mapWrite (\wy _ -> Just ((), wy)) (toReadOnly srcX >+< srcY)

(|+|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) () | iTask p & TC rx & TC ry & TC wx & TC wy
(|+|) srcX srcY = toReadOnly (srcX >+< srcY)
// END DEPRECATED

(>*<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) (wx,wy)     | iTask p & TC rx & TC ry & TC wx & TC wy
(>*<) l r = l >+< r

(>*|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wx          | iTask p & TC rx & TC ry & TC wx & TC wy
(>*|) l r = l >+| r

(|*<) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) wy          | iTask p & TC rx & TC ry & TC wx & TC wy
(|*<) l r = l |+< r

(|*|) infixl 6 :: !(RWShared p rx wx) !(RWShared p ry wy) -> RWShared p (rx,ry) ()          | iTask p & TC rx & TC ry & TC wx & TC wy
(|*|) l r = l |+| r

symmetricLens :: !(a b -> b) !(b a -> a) !(RWShared p a a) !(RWShared p b b) -> (!RWShared p a a, !RWShared p b b) | iTask p & TC a & TC b
symmetricLens putr putl sharedA sharedB = (newSharedA,newSharedB)
where
	sharedAll = sharedA >+< sharedB
	newSharedA = mapReadWrite (fst,\a (_,b) -> Just (a,putr a b)) sharedAll
	newSharedB = mapReadWrite (snd,\b (a,_) -> Just (putl b a,b)) sharedAll

//mapToJSONString :: !(RWShared p a a) -> (RWShared p String String) | JSONDecode{|*|} a & JSONEncode{|*|} a

//mapToDynamicString :: !(RWShared p a a) -> (RWShared p String String) | TC a


//Derived shares of tasklists
taskListState :: !(SharedTaskList a) -> ReadOnlyShared [TaskValue a] | TC a
taskListState tasklist = mapRead (\(_,items) -> [value \\ {TaskListItem|value} <- items]) (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}

taskListMeta :: !(SharedTaskList a) -> ReadWriteShared [TaskListItem a] [(TaskId,TaskAttributes)] | TC a
taskListMeta tasklist = mapRead (\(_,items) -> items) (sdsFocus listFilter tasklist)
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True}

taskListIds :: !(SharedTaskList a) -> ROShared () [TaskId] | TC a
taskListIds tasklist = mapRead prj (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=False}
    prj (_,items) = [taskId \\ {TaskListItem|taskId} <- items]

taskListEntryMeta :: !(SharedTaskList a) -> RWShared TaskId (TaskListItem a) TaskAttributes | TC a
taskListEntryMeta tasklist = mapSingle (sdsSplit "taskListEntryMeta" param read write tasklist)
where
    param p = ({onlyIndex=Nothing,onlyTaskId=Just [p],onlySelf=False,includeValue=True,includeAttributes=True,includeProgress=True},p)
    read p (_,items) = [i \\ i=:{TaskListItem|taskId} <- items | taskId == p]
    write p _ attributes    = ([(p,a) \\ a <- attributes], const ((==) p))

taskListSelfId :: !(SharedTaskList a) -> ReadOnlyShared TaskId | TC a
taskListSelfId tasklist = mapRead (\(_,items) -> hd [taskId \\ {TaskListItem|taskId,self} <- items | self]) (toReadOnly (sdsFocus listFilter tasklist))
where
    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=True,includeValue=False,includeAttributes=False,includeProgress=False}

taskListSelfManagement :: !(SharedTaskList a) -> Shared TaskAttributes | TC a
taskListSelfManagement tasklist = mapReadWriteError (toPrj,fromPrj) (sdsFocus listFilter tasklist)
where
    toPrj (_,items) = case [m \\ m=:{TaskListItem|taskId,self} <- items | self] of
        []                              = Error (exception "Task id not found in self management share")
        [{TaskListItem|attributes}:_]   = Ok attributes

    fromPrj attributes (_,[{TaskListItem|taskId}])
        = Ok (Just [(taskId,attributes)])

    listFilter = {onlyIndex=Nothing,onlyTaskId=Nothing,onlySelf=True,includeValue=False,includeAttributes=True,includeProgress=False}

taskListItemValue :: !(SharedTaskList a) -> ROShared (Either Int TaskId) (TaskValue a) | TC a
taskListItemValue tasklist = mapReadError read (toReadOnly (sdsTranslate "taskListItemValue" listFilter tasklist))
where
    listFilter (Left index) = {onlyIndex=Just [index],onlyTaskId=Nothing,onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}
    listFilter (Right taskId) = {onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=True,includeAttributes=False,includeProgress=False}

    read (_,items) = case [value \\ {TaskListItem|value} <- items] of
        [v:_]   = Ok v
        _       = Error (exception "taskListItemValue: item not found")

taskListItemProgress :: !(SharedTaskList a) -> ROShared (Either Int TaskId) InstanceProgress | TC a
taskListItemProgress tasklist = mapReadError read (toReadOnly (sdsTranslate "taskListItemProgress" listFilter tasklist))
where
    listFilter (Left index) = {onlyIndex=Just [index],onlyTaskId=Nothing,onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=True}
    listFilter (Right taskId) = {onlyIndex=Nothing,onlyTaskId=Just [taskId],onlySelf=False,includeValue=False,includeAttributes=False,includeProgress=True}

    read (_,items) = case [p \\ {TaskListItem|progress=Just p} <- items] of
        [p:_]   = Ok p
        _       = Error (exception "taskListItemProgress: item not found")

mapMaybeLens :: !String !(RWShared () (Map a b) (Map a b)) -> RWShared a (Maybe b) b | < a & == a & TC a & TC b
mapMaybeLens name origShare = sdsLens name (const ()) (SDSRead read) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: !a !(Map a b) -> MaybeError TaskException (Maybe b) | < a & == a
  read idx m = Ok ('DM'.get idx m)

  write :: !a !(Map a b) !b -> MaybeError TaskException (Maybe (Map a b)) | < a & == a
  write idx oldmap newval = Ok (Just ('DM'.put idx newval oldmap))

  notify :: !a !(Map a b) !b -> SDSNotifyPred a | < a & == a
  notify idx oldmap newval = \_ idx` -> idx == idx`

mapLens :: !String !(RWShared () (Map a b) (Map a b)) !(Maybe b) -> RWShared a b b | < a & == a & TC a & TC b
mapLens name origShare mdef = sdsLens name (const ()) (SDSRead (read mdef)) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: !(Maybe b) !a !(Map a b) -> MaybeError TaskException b | < a & == a
  read mdef idx m
    = case 'DM'.get idx m of
        Just x -> Ok x
        _      -> case mdef of
                    Just def -> Ok def
                    _        -> Error (exception (name +++ " (mapLens): Index not found"))

  write :: !a !(Map a b) !b -> MaybeError TaskException (Maybe (Map a b)) | < a & == a
  write idx oldmap newval = Ok (Just ('DM'.put idx newval oldmap))

  notify :: !a !(Map a b) !b -> SDSNotifyPred a | < a & == a
  notify idx oldmap newval = \_ idx` -> idx == idx`

intMapLens :: !String !(RWShared () (IntMap a) (IntMap a)) !(Maybe a) -> RWShared Int a a | TC a
intMapLens name origShare mdef = sdsLens name (const ()) (SDSRead (read mdef)) (SDSWrite write) (SDSNotify notify) origShare
  where
  read :: !(Maybe a) !Int !(IntMap a) -> MaybeError TaskException a
  read mdef idx intmap
    = case 'DIS'.get idx intmap of
        Just x -> Ok x
        _      -> case mdef of
                    Just def -> Ok def
                    _        -> Error (exception (name +++ " (intMapLens): Index " +++ toString idx +++ " not found"))

  write :: !Int !(IntMap a) !a -> MaybeError TaskException (Maybe (IntMap a))
  write idx oldmap newval = Ok (Just ('DIS'.put idx newval oldmap))

  notify :: !Int !(IntMap a) !a -> SDSNotifyPred Int
  notify idx oldmap newval = \_ idx` -> idx == idx`
