implementation module iTasks.Internal.EngineTasks

import Data.Error
import Data.Queue
import StdEnv
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskServer
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.SDS.Combinators.Common
import iTasks.UI.Definition
import iTasks.WF.Definition
import Text

from Data.Map import newMap, member

everyTick :: (*IWorld -> *(!MaybeError TaskException (), !*IWorld)) -> Task ()
everyTick f = Task eval
where
	eval DestroyEvent evalOpts tree iworld
		= (DestroyedResult, iworld)
	eval event evalOpts tree=:(TCInit taskId ts) iworld
		# (merr, iworld) = f iworld
		| isError merr = (ExceptionResult (fromError merr), iworld)
		# (merr, iworld) = readRegister taskId tick iworld
		| isError merr = (ExceptionResult (fromError merr), iworld)
		= (ValueResult
				NoValue
				{TaskEvalInfo|lastEvent=ts,removedTasks=[],attributes=newMap}
				NoChange
				(TCInit taskId ts)
			, iworld)
	
//When we run the built-in HTTP server we need to do active garbage collection of instances that were created for sessions
removeOutdatedSessions :: Task ()
removeOutdatedSessions = everyTick \iworld=:{IWorld|options}->
	case read (sdsFocus {InstanceFilter|defaultValue & includeSessions=True} filteredInstanceIndex) EmptyContext iworld of
		(Ok (ReadingDone index), iworld) = checkAll (removeIfOutdated options) index iworld
		(Error e, iworld)                = (Error e, iworld)
where
	checkAll f [] iworld = (Ok (),iworld)
	checkAll f [x:xs] iworld = case f x iworld of
		(Ok (),iworld) = checkAll f xs iworld
		(Error e,iworld) = (Error e,iworld)

    removeIfOutdated options (instanceNo,_,_,_) iworld=:{options={appVersion},clock=tNow}
		# (remove,iworld) = case read (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
			//If there is I/O information, we check that age first
			(Ok (ReadingDone (Just (client,tInstance))),iworld) //No IO for too long, clean up
				= (Ok ((tNow - tInstance) > options.EngineOptions.sessionTime),iworld)
			//If there is no I/O information, get meta-data and check builtId and creation date
			(Ok (ReadingDone Nothing),iworld)
				= case read (sdsFocus instanceNo taskInstanceConstants) EmptyContext iworld of
					(Ok (ReadingDone {InstanceConstants|build,issuedAt=tInstance}),iworld)
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
				= case write Nothing (sdsFocus instanceNo taskInstanceIO) EmptyContext iworld of
					(Error e, iworld) = (Error e, iworld)
					(Ok WritingDone, iworld) = (Ok (), iworld)
			(Ok False)
				= (Ok (), iworld)
			(Error e)
				= (Error e,iworld)

//When the event queue is empty, write deferred SDS's
flushWritesWhenIdle:: Task ()
flushWritesWhenIdle = everyTick \iworld->case read taskEvents EmptyContext iworld of
		(Error e,iworld)          = (Error e,iworld)
		(Ok (ReadingDone (Queue [] [])),iworld) = flushDeferredSDSWrites iworld
		(Ok _,iworld)             = (Ok (),iworld)

//When we don't run the built-in HTTP server we don't want to loop forever so we stop the loop
//once all tasks are stable
stopOnStable :: Task ()
stopOnStable = everyTick \iworld->case read (sdsFocus {InstanceFilter|defaultValue & includeProgress=True, includeStartup=True, includeAttributes=True} filteredInstanceIndex) EmptyContext iworld of
		(Ok (ReadingDone index), iworld)
			# iworld = case exceptions index of
				[] = iworld
				excs
					# (_, world) = fclose (stderr <<< join "\n" excs <<< "\n") iworld.world
					= {IWorld | iworld & world=world, shutdown=Just 1}
			# iworld = if (isNothing iworld.shutdown && all isStable (filter (not o isSystem) index))
				{IWorld | iworld & shutdown=Just 0}
				iworld
			= (Ok (), iworld)
		(Ok _,iworld)
			= (Error (exception "Unexpeced SDS state"),iworld)
		(Error e, iworld)  = (Error e, iworld)
where
	isStable (_, _, Nothing, _) = False
	isStable (_, _, Just {InstanceProgress|value}, attributes) = value =: Stable

	isSystem (_, _, Just {InstanceProgress|value}, attributes) = member "system" (fromMaybe newMap attributes)
	isSystem _ = False
	
	exceptions instances = [e\\(_, _, Just {InstanceProgress|value=Exception e}, _)<-instances]
