implementation module iTasks.SDS.Combinators.Core

import iTasks.SDS.Definition, iTasks.Internal.SDS
import Data.Either

import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.UI.Editor.Generic
import iTasks.WF.Definition
import Data.GenEq, StdString

sdsLens :: !String (p -> ps) (SDSLensRead p r rs) (SDSLensWrite p w rs ws) (SDSLensNotify p p w rs) (Maybe (SDSReducer p ws w)) !(sds ps rs ws) -> SDSLens p r w | gText{|*|} ps & TC ps & TC rs & TC ws & RWShared sds
sdsLens name param read write notify reducer sds = SDSLens sds {SDSLensOptions|name=name,param=param,read=read,write=write,notify=notify,reducer=reducer}

sdsSelect :: !String (p -> Either p1 p2) (SDSLensNotify p1 p2 w r) (SDSLensNotify p2 p1 w r) !(sds1 p1 r w) !(sds2 p2 r w) -> SDSSelect p r w | gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r & TC w & RWShared sds1 & RWShared sds2
sdsSelect name select notifyl notifyr sds1 sds2 = SDSSelect sds1 sds2 {SDSSelectOptions|name=name,select=select,notifyl=notifyl,notifyr=notifyr}

sdsParallel :: !String !(p -> (p1,p2)) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(sds1 p1 r1 w1) !(sds2 p2 r2 w2) -> SDSParallel p r w | gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w2 & RWShared sds1 & RWShared sds2
sdsParallel name param read write1 write2 sds1 sds2 = SDSParallel sds1 sds2 {SDSParallelOptions|name=name,param=param,read=read,writel=write1,writer=write2}

sdsSequence :: !String !(p -> p1) !(p r1 -> p2) (p r1 -> Either r ((r1,r2) -> r)) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(sds1 p1 r1 w1) !(sds2 p2 r2 w2) -> SDSSequence p r w | gText{|*|} p1 & TC p1 & gText{|*|} p2 & TC p2 & TC r1 & TC r2 & TC w1 & TC w2 & RWShared sds1 & RWShared sds2
sdsSequence name paraml paramr read write1 write2 sds1 sds2 = SDSSequence sds1 sds2 {SDSSequenceOptions|name=name,paraml=paraml,paramr=paramr,read=read,writel=write1,writer=write2}

sdsCache:: (p (Maybe r) (Maybe w) w -> (Maybe r, SDSCacheWrite)) (SDSSource p r w) -> SDSCache p r w | iTask p & TC r & TC w
sdsCache write sds = SDSCache sds {SDSCacheOptions|write=write}
