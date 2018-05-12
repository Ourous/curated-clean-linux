implementation module iTasks.SDS.Combinators.Core

import iTasks.SDS.Definition, iTasks.Internal.SDS
import Data.Either

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import iTasks.WF.Definition
import Data.GenEq, StdString

sdsLens :: !String (p -> ps) (SDSLensRead p r rs) (SDSLensWrite p w rs ws) (SDSLensNotify p p w rs) !(SDS ps rs ws) -> SDS p r w | iTask ps & TC rs & TC ws
sdsLens name param read write notify sds = SDSLens sds {SDSLens|name=name,param=param,read=read,write=write,notify=notify}

sdsSelect :: !String (p -> Either p1 p2) (SDSLensNotify p1 p2 w r) (SDSLensNotify p2 p1 w r) !(SDS p1 r w) !(SDS p2 r w) -> SDS p r w | iTask p1 & iTask p2 & TC r & TC w
sdsSelect name select notifyl notifyr sds1 sds2 = SDSSelect sds1 sds2 {SDSSelect|name=name,select=select,notifyl=notifyl,notifyr=notifyr}

sdsParallel :: !String !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(SDS p1 r1 w1) !(SDS p2 r2 w2) -> SDS p r w | iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2
sdsParallel name param read write1 write2 sds1 sds2 = SDSParallel sds1 sds2 {SDSParallel|name=name,param=param,read=read,writel=write1,writer=write2}

sdsSequence :: !String !(p -> p1) !(p r1 -> p2) (p r1 -> Either r ((r1,r2) -> r)) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(SDS p1 r1 w1) !(SDS p2 r2 w2) -> SDS p r w | iTask p1 & iTask p2 & TC r1 & TC r2 & TC w1 & TC w2
sdsSequence name paraml paramr read write1 write2 sds1 sds2 = SDSSequence sds1 sds2 {SDSSequence|name=name,paraml=paraml,paramr=paramr,read=read,writel=write1,writer=write2}

sdsCache:: (p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)) (SDS p r w) -> SDS p r w | iTask p & TC r & TC w
sdsCache write (SDSSource sds) = SDSCache sds {SDSCache|write=write}
sdsCache _ _ = createReadWriteSDS invalidCacheName
                                  invalidCacheName
                                  (\_   iworld -> (Error invalidCacheError, iworld))
                                  (\_ _ iworld -> (Error invalidCacheError, iworld))
where
    invalidCacheName  = "invalid SDS cache"
    invalidCacheError = exception "SDS cache only allowed on source SDSs."
