definition module iTasks.SDS.Definition
/**
* This module provides the types that define a shared data source
*/
from iTasks.WF.Definition import :: TaskException, class iTask
from iTasks.Internal.IWorld import :: IWorld

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import Data.GenEq

from Data.Either import :: Either
from Data.Error import :: MaybeError
from Data.Maybe import :: Maybe

//This is the definition of a shared data source

:: SDS p r w
	= 			            SDSSource		!(SDSSource p r w) 
    | E.ps rs ws:           SDSLens         !(SDS ps rs ws)                   (SDSLens p r w ps rs ws) & iTask ps & TC rs & TC ws
    | E.p1 p2:              SDSSelect       !(SDS p1 r w)   !(SDS p2 r w)     (SDSSelect p p1 p2 r w) & iTask p1 & iTask p2 & TC r & TC w
    | E.p1 r1 w1 p2 r2 w2:  SDSParallel     !(SDS p1 r1 w1) !(SDS p2 r2 w2)   (SDSParallel p1 r1 w1 p2 r2 w2 p r w) & iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2
    | E.p1 r1 w1 p2 r2 w2:  SDSSequence     !(SDS p1 r1 w1) !(SDS p2 r2 w2)   (SDSSequence p1 r1 w1 p2 r2 w2 p r w) & iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2
	|                       SDSCache        !(SDSSource p r w)                (SDSCache p r w) & iTask p & TC r & TC w
							// USE IT CAREFULLY, IT CAN BREAK NOTIFICATION!
    |						SDSDynamic		!(p *IWorld -> *(MaybeError TaskException (RWShared p r w), *IWorld)) //TODO: Remove


// Common aliases
:: RWShared p r w       :== SDS p r w
:: ROShared p a         :== SDS p a ()
:: WOShared p a         :== SDS p () a

:: ReadWriteShared r w  :== SDS () r w
:: ReadOnlyShared a     :== SDS () a ()
:: WriteOnlyShared a    :== SDS () () a
:: Shared a             :== SDS () a a

//For notification we need a predicate that can determine whether
//some registered parameter of type p needs to be notified.
:: SDSNotifyPred p          :== Timespec p -> Bool

//Sources provide direct access to a data source
:: SDSSource p r w =
	{ name          :: String
    , read			:: p *IWorld -> *(!MaybeError TaskException r, !*IWorld)
	, write			:: p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld)
	}

//Lenses select and transform data
:: SDSLens p r w ps rs ws =
    { name         :: String
    , param        :: p -> ps
    , read         :: SDSLensRead p r rs
    , write        :: SDSLensWrite p w rs ws
    , notify       :: SDSLensNotify p p w rs
    }

:: SDSLensRead p r rs
    = SDSRead       (p rs -> MaybeError TaskException r)  //Read original source and transform
    | SDSReadConst  (p -> r)                              //No need to read the original source

:: SDSLensWrite p w rs ws
    = SDSWrite      (p rs w  -> MaybeError TaskException (Maybe ws)) //Read original source, and write updated version
    | SDSWriteConst (p w     -> MaybeError TaskException (Maybe ws)) //No need to read the original source

:: SDSLensNotify pw pq w rs
    = SDSNotify         (pw rs w -> SDSNotifyPred pq)
    | SDSNotifyConst    (pw w    -> SDSNotifyPred pq)

//Merge two sources by selecting one based on the parameter
:: SDSSelect p p1 p2 r w =
    { name          :: String
    , select        :: p -> Either p1 p2
    , notifyl       :: SDSLensNotify p1 p2 w r
    , notifyr       :: SDSLensNotify p2 p1 w r
    }

//Read from and write to two independent SDS's
:: SDSParallel p1 r1 w1 p2 r2 w2 p r w =
    { name          :: String
    , param         :: p -> (p1,p2)
    , read          :: (r1,r2) -> r
    , writel        :: SDSLensWrite p w r1 w1
    , writer        :: SDSLensWrite p w r2 w2
    }

//Read from and write to two dependent SDS's
//The read value from the first is used to compute the parameter for the second
:: SDSSequence p1 r1 w1 p2 r2 w2 p r w =
    { name          :: String
	, paraml        :: p -> p1
	, paramr        :: p r1 -> p2
	, read          :: p r1 -> Either r ((r1,r2) -> r)
    , writel        :: SDSLensWrite p w r1 w1
    , writer        :: SDSLensWrite p w r2 w2
    }

:: SDSCache p r w =
	{ write        :: p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)
	}

:: SDSCacheWrite = WriteNow | WriteDelayed | NoWrite

