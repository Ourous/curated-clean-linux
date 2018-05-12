definition module iTasks.SDS.Combinators.Core
/**
* This module provides the core builtin combinators for composing shared data sources.
*/
from iTasks.SDS.Definition import :: SDS, :: SDSLensRead, :: SDSLensWrite, :: SDSLensNotify, :: SDSNotifyPred, :: SDSCacheWrite
from iTasks.Internal.IWorld import :: IWorld
from Data.Either import :: Either
from Data.Maybe import :: Maybe
from Text.GenJSON import :: JSONNode
from System.Time import :: Timespec

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import generic gEditor, generic gEq, generic gDefault, generic gText, generic JSONEncode, generic JSONDecode

from iTasks.UI.Editor import :: Editor
from iTasks.Internal.Generic.Visualization import :: TextFormat

// Apply a parametric lens
sdsLens :: !String (p -> ps) (SDSLensRead p r rs) (SDSLensWrite p w rs ws) (SDSLensNotify p p w rs) !(SDS ps rs ws) -> SDS p r w | iTask ps & TC rs & TC ws

// Choose between two SDS's based on the parameter.
// Because there may be overlap in the parameter spaces of the two SDS's
// a write to the merged SDS can invalidate both SDS's even though only one is chosen to write to.
sdsSelect :: !String (p -> Either p1 p2) (SDSLensNotify p1 p2 w r) (SDSLensNotify p2 p1 w r) !(SDS p1 r w) !(SDS p2 r w) -> SDS p r w | iTask p1 & iTask p2 & TC r & TC w

// Create a new SDS by simultaneous access to two independent SDS's
sdsParallel :: !String !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(SDS p1 r1 w1) !(SDS p2 r2 w2) -> SDS p r w | iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2

// Create a new SDS by sequential access to two dependent SDS's
sdsSequence :: !String !(p -> p1) !(p r1 -> p2) (p r1 -> Either r ((r1,r2) -> r)) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(SDS p1 r1 w1) !(SDS p2 r2 w2) -> SDS p r w | iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2

// Create a cached version of an SDS
// This is only allowed on source SDSs, otherwise notification can break.
// If used on other SDSs errors will be generated on read/write.
sdsCache :: (p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)) (SDS p r w) -> SDS p r w | iTask p & TC r & TC w


