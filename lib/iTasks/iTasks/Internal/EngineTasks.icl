implementation module iTasks.Internal.EngineTasks

import StdBool, StdOverloaded, StdList, StdOrdList
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.List
import Data.Functor, Data.Func
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.WF.Definition
import iTasks.Internal.Util
import iTasks.Internal.SDS
import iTasks.Internal.TaskStore
import iTasks.SDS.Definition
import iTasks.SDS.Combinators.Common

from iTasks.Extensions.DateTime import toDate, toTime, instance == Date, instance == Time
from System.Time import time

from TCPIP import :: Timeout

import Data.Queue
import Text

timeout :: !(Maybe Timeout) !*IWorld -> (!Maybe Timeout,!*IWorld)
timeout mt iworld = case read taskEvents iworld of
	//No events
	(Ok (Queue [] []),iworld=:{sdsNotifyRequests,world})
		# (ts, world) = nsTime world
		= ( minListBy lesser [mt:flatten $ map (getTimeoutFromClock ts) $ 'DM'.elems sdsNotifyRequests]
		  , {iworld & world = world})
	(Ok _,iworld)               = (Just 0,iworld)   //There are still events, don't wait
	(Error _,iworld)            = (Just 500,iworld) //Keep retrying, but not too fast
where
	lesser (Just x) (Just y) = x < y
	lesser (Just _) Nothing = True
	lesser Nothing Nothing = False
	
	getTimeoutFromClock :: Timespec (Map SDSNotifyRequest Timespec) -> [Maybe Timeout]
	getTimeoutFromClock now requests = getTimeoutFromClock` <$> 'DM'.toList requests
	where
		getTimeoutFromClock` :: (!SDSNotifyRequest, !Timespec) -> Maybe Timeout
		getTimeoutFromClock` (snr=:{cmpParam=(ts :: ClockParameter Timespec)}, reqTimespec)
			| startsWith "$IWorld:timespec$" snr.reqSDSId && ts.interval <> zero
				# fire = iworldTimespecNextFire now reqTimespec ts
				= Just (max 0 (toMs fire - toMs now))
			= mt
		getTimeoutFromClock` _ = mt

	toMs x = x.tv_sec * 1000 + x.tv_nsec / 1000000

updateClock :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
updateClock iworld=:{IWorld|clock,world}
    //Determine current date and time
	# (timespec,world) 	= nsTime world
    # iworld = {iworld & world = world}
    //Write SDS if necessary
    # (mbe,iworld) = write timespec (sdsFocus {start=zero,interval=zero} iworldTimespec) iworld
	| mbe =:(Error _) = (mbe,iworld)
    = (Ok (),iworld)

//When we run the built-in HTTP server we need to do active garbage collection of instances that were created for sessions
removeOutdatedSessions :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
removeOutdatedSessions iworld=:{IWorld|options}
    # (mbIndex,iworld) = read (sdsFocus {InstanceFilter|defaultValue & onlySession=Just True} filteredInstanceIndex) iworld
    = case mbIndex of
        Ok index    = checkAll removeIfOutdated index iworld 
        Error e     = (Error e, iworld)
where
	checkAll f [] iworld = (Ok (),iworld)
	checkAll f [x:xs] iworld = case f x iworld of
		(Ok (),iworld) = checkAll f xs iworld
		(Error e,iworld) = (Error e,iworld)

    removeIfOutdated (instanceNo,_,_,_) iworld=:{options={appVersion},clock=tNow}
		# (remove,iworld) = case read (sdsFocus instanceNo taskInstanceIO) iworld of
			//If there is I/O information, we check that age first
			(Ok (Just (client,tInstance)),iworld) //No IO for too long, clean up
				= (Ok ((tNow - tInstance) > options.EngineOptions.sessionTime),iworld)
			//If there is no I/O information, get meta-data and check builtId and creation date
			(Ok Nothing,iworld)
				= case read (sdsFocus instanceNo taskInstanceConstants) iworld of
					(Ok {InstanceConstants|build,issuedAt=tInstance},iworld)
						| build <> appVersion = (Ok True,iworld)
						| (tNow - tInstance) > options.EngineOptions.sessionTime = (Ok True,iworld)
						= (Ok False,iworld)
					(Error e,iworld)
						= (Error e,iworld)
			(Error e,iworld) 
				= (Error e,iworld)
		= case remove of
			(Ok True)
				# (e,iworld) = deleteTaskInstance instanceNo iworld
				| e=:(Error _) = (e,iworld)
				# (e,iworld) = write Nothing (sdsFocus instanceNo taskInstanceIO) iworld
				| e=:(Error _) = (e,iworld)
				= (Ok (),iworld)
			(Ok False)
				= (Ok (), iworld)
			(Error e)
				= (Error e,iworld)

//When the event queue is empty, write deferred SDS's
flushWritesWhenIdle:: !*IWorld -> (!MaybeError TaskException (), !*IWorld)
flushWritesWhenIdle iworld = case read taskEvents iworld of
		(Error e,iworld)          = (Error e,iworld)
		(Ok (Queue [] []),iworld) = flushDeferredSDSWrites iworld
		(Ok _,iworld)             = (Ok (),iworld)

//When we don't run the built-in HTTP server we don't want to loop forever so we stop the loop
//once all tasks are stable
stopOnStable :: !*IWorld -> *(!MaybeError TaskException (), !*IWorld)
stopOnStable iworld=:{IWorld|shutdown}
    # (mbIndex,iworld) = read (sdsFocus {InstanceFilter|defaultValue & includeProgress=True} filteredInstanceIndex) iworld
	= case mbIndex of 
		Ok index 
			# shutdown = case shutdown of
				Nothing = if (allStable index) (Just (if (exceptionOccurred index) 1 0)) Nothing
				_       = shutdown
			= (Ok (), {IWorld|iworld & shutdown = shutdown})
		Error e  = (Error e, iworld)
where
	allStable instances = all (\v -> v =: Stable || v =: (Exception _)) (values instances) 
	exceptionOccurred instances = any (\v -> v =: (Exception _)) (values instances)
	values instances = [value \\ (_,_,Just {InstanceProgress|value},_) <- instances]


