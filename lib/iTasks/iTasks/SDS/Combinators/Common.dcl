definition module iTasks.SDS.Combinators.Common
/**
* This module provides common patterns for composing shared sources defined on top of the core set
*/
import iTasks.SDS.Definition
from iTasks.WF.Definition import :: TaskException, :: TaskValue, :: TaskId, :: TaskAttributes, :: InstanceProgress, class iTask
from iTasks.WF.Definition import generic gEditor, generic gEq, generic gText, generic JSONEncode, generic JSONDecode
from iTasks.WF.Combinators.Core import :: TaskList, :: TaskListFilter, :: TaskListItem, :: SharedTaskList
from iTasks.Internal.Generic.Visualization import :: TextFormat
from iTasks.UI.Editor import :: Editor

from Data.Maybe import :: Maybe
from Data.Either import :: Either
from Data.Error import :: MaybeError, :: MaybeErrorString
from Data.Map import :: Map
from Data.IntMap.Strict import :: IntMap
from StdOverloaded import class <
from System.FilePath import :: FilePath
from System.Time import :: Timespec

from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode

:: SDSReadProjection rs rt
    = SDSLensRead      (rs -> MaybeError TaskException rt) //Read lens-like
    | SDSConstRead     rt                                  //No need to read the original source

:: SDSWriteProjection rs ws wt
    = SDSLensWrite     (rs wt   -> MaybeError TaskException (Maybe ws)) //Write lens-like
    | SDSBlindWrite    (wt      -> MaybeError TaskException (Maybe ws)) //No-need to read the original source

// Fix a focus parameter
sdsFocus :: !p !(sds p r w) -> (SDSLens p` r w) | gText{|*|} p & JSONEncode{|*|} p & TC p & TC r & TC w & RWShared sds

// Projection of the domain with a lens
sdsProject :: !(SDSReadProjection rs r) !(SDSWriteProjection rs ws w) !(Maybe (SDSReducer p ws w)) !(sds p rs ws) -> SDSLens p r w | gText{|*|} p & TC p & TC rs & TC ws & RWShared sds

// Translate the parameter space
sdsTranslate :: !String !(p -> ps) !(sds ps r w) -> SDSLens p r w |  gText{|*|} ps & TC ps & TC r & TC w & RWShared sds

// Introduce a new parameter
sdsSplit :: !String !(p -> (ps,pn)) !(pn rs -> r) !(pn rs w -> (ws,SDSNotifyPred pn)) !(Maybe (SDSReducer p ws w)) !(sds ps rs ws) -> SDSLens p r w | gText{|*|} ps & TC ps & gText{|*|} pn & TC pn & TC rs  & TC ws & RWShared sds

// Treat symmetric sources with optional values as if they always have a value.
// You can provide a default value, if you don't it will trigger a read error
removeMaybe :: !(Maybe a) !(sds p (Maybe a) (Maybe a)) -> SDSLens p a a | gText{|*|} p & TC p & TC a & RWShared sds

/**
* Maps the read type, the write type or both of a shared reference to another one using a functional mapping.
* The function for mapping the write type also gets the current read-value as input
* making it possible to change only parts of the datastructure.
*
* @param A functional mapping
* @param A reference to shared data
* @return A reference to shared data of another type
*/
mapRead :: !(r -> r`) !(sds p r w) -> SDSLens p r` w | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapWrite :: !(w` r -> Maybe w) !(Maybe (SDSReducer p w w`)) !(sds p r w) -> SDSLens p r w` | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapReadWrite :: !(!r -> r`,!w` r -> Maybe w) !(Maybe (SDSReducer p w w`)) !(sds p r w) -> SDSLens p r` w` | gText{|*|} p & TC p & TC r & TC w & RWShared sds

mapReadError :: !(r -> MaybeError TaskException r`) !(sds p r w) -> SDSLens p r` w | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapWriteError :: !(w` r -> MaybeError TaskException (Maybe w)) !(Maybe (SDSReducer p w w`)) !(sds p r w) -> SDSLens p r w` | gText{|*|} p & TC p & TC r & TC w & RWShared sds
mapReadWriteError :: !(!r -> MaybeError TaskException r`,!w` r -> MaybeError TaskException (Maybe w)) !(Maybe (SDSReducer p w w`)) !(sds p r w) -> SDSLens p r` w` | gText{|*|} p & TC p & TC r & TC w & RWShared sds

toReadOnly :: !(sds p r w) -> SDSLens p r () | gText{|*|} p & TC p & TC r & TC w & RWShared sds

toDynamic :: !(sds p r w) -> (SDSLens p Dynamic Dynamic) | gText{|*|} p & TC p & TC r & TC w & RWShared sds


//Map a list SDS of one element to the element itself
mapSingle :: !(sds p [r] [w]) -> (SDSLens p r w) | gText{|*|} p & TC p & TC r & TC w & RWShared sds

// Composition of two shared references.
// The read type is a tuple of both types.
// The write type can either be a tuple of both write types, only one of them or it is written to none of them (result is a read-only shared).
(>*<) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSParallel p (rx,ry) (wx,wy)	| gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & RWShared sds1 & RWShared sds2
(>*|) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSParallel p (rx,ry) wx           | gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & RWShared sds1 & Registrable sds2
(|*<) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSParallel p (rx,ry) wy           | gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & Registrable sds1 & RWShared sds2
(|*|) infixl 6 :: !(sds1 p rx wx) !(sds2 p ry wy) -> SDSParallel p (rx,ry) ()			| gText{|*|} p & TC p & TC rx & TC ry & TC wx & TC wy & Registrable sds1 & Registrable sds2

/**
* Puts a symmetric lens between two symmetric shared data sources.
* Changes of one also affects the other one.
*
* @param putr: used to map changes of shared a to shared b
* @param putl: used to map changes of shared b to shared a
* @param SymmetricShared a
* @param SymmetricShared b
* @param RWShared references of the same type with symmetric lens between them
*/
symmetricLens :: !(a b -> b) !(b a -> a) !(sds1 p a a) !(sds2 p b b) -> (!SDSLens p a a, !SDSLens p b b) | gText{|*|} p & TC p & TC a & TC b & RWShared sds1 & RWShared sds2

//Derived versions of tasks lists
/**
* Get the shared state of a task list
*/
taskListState :: !(SharedTaskList a) -> SDSLens () [TaskValue a] () | TC a
/**
* Get the meta data sds of a task list
*/
taskListMeta :: !(SharedTaskList a) -> SDSLens () [TaskListItem a] [(TaskId,TaskAttributes)] | TC a
/**
* Get the list of task id's in a task list
*/
taskListIds :: !(SharedTaskList a) -> SDSLens () [TaskId] () | TC a
/**
* Get the meta data sds for a specific entry in a task list
*/
taskListEntryMeta :: !(SharedTaskList a) -> SDSLens TaskId (TaskListItem a) TaskAttributes | TC a
/*
* Get the id of the entry in the list the current task is part of
*/
taskListSelfId :: !(SharedTaskList a) -> SDSLens () TaskId () | TC a
/**
* Get the current tasks management meta data share
*/
taskListSelfManagement :: !(SharedTaskList a) -> SimpleSDSLens TaskAttributes | TC a
/**
* Get the value of a specific task in the list
* The paramater is either the index in the list or a specific task id
*/
taskListItemValue :: !(SharedTaskList a) -> SDSLens (Either Int TaskId) (TaskValue a) () | TC a
/**
* Get the progress of a specific task in the list
* The paramater is either the index in the list or a specific task id
* Note that there is only progress information for detached tasks
*/
taskListItemProgress :: !(SharedTaskList a) -> SDSLens (Either Int TaskId) InstanceProgress () | TC a

/**
 * Convenience lens for lookups in Maps. Returns Nothing on a missing key.
 */
mapMaybeLens :: !String !(Shared sds (Map a b)) -> SDSLens a (Maybe b) b | < a & == a & TC a & TC b & RWShared sds

/**
 * Convenience lens for lookups in Maps. Can use a default value on a missing key, gives an error if no default is supplied.
 */
mapLens :: !String !(Shared sds (Map a b)) !(Maybe b) -> SDSLens a b b | < a & == a & TC a & TC b & RWShared sds

/**
 * Convenience lens for lookups in IntMaps. Can use a default value on a missing key, gives an error if no default is supplied.
 */
intMapLens :: !String !(Shared sds (IntMap a)) !(Maybe a) -> SDSLens Int a a | TC a & RWShared sds
