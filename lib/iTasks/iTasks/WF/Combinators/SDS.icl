implementation module iTasks.WF.Combinators.SDS

import iTasks.WF.Derives
import iTasks.WF.Definition
import iTasks.SDS.Definition
from iTasks.SDS.Combinators.Common import sdsFocus

import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.Util
from iTasks.Internal.SDS import write, read, readRegister

from Data.Func import mapSt

import StdTuple, StdArray, StdList, StdString
import Text, Text.GenJSON
import Data.Maybe, Data.Error, Data.Functor
import System.Directory, System.File, System.FilePath, Data.Error, System.OSError
import qualified Data.Map as DM

withShared :: !b !((SimpleSDSLens b) -> Task a) -> Task a | iTask a & iTask b
withShared initial stask = Task evalinit
where
	//Initialization
	evalinit DestroyEvent _ iworld = (DestroyedResult,iworld)

	evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld
		# (taskIda, iworld) = getNextTaskId iworld
		# (e, iworld)       = write initial (sdsFocus taskId localShare) EmptyContext iworld
		| isError e
			= (ExceptionResult (fromError e),iworld)
		= eval taskIda (stask (sdsFocus taskId localShare)) event evalOpts iworld

	//Running
	eval innerTaskId (Task inner) DestroyEvent opts iworld
		= case inner DestroyEvent {TaskEvalOpts|opts&taskId=innerTaskId} iworld of
			(ValueResult _ _ _ _, iworld)
				= (ExceptionResult (exception "Failed to destroy withShared child"), iworld)
			e = e

	eval innerTaskId (Task inner) event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld
		= case inner event {TaskEvalOpts|evalOpts&taskId=innerTaskId} iworld of
			(ValueResult val info rep newinner, iworld)
				# info = {TaskEvalInfo|info & lastEvent = max lastEval info.TaskEvalInfo.lastEvent}
				= (ValueResult val info rep (Task (eval innerTaskId newinner)), iworld)
			e = e

withTaskId :: (Task a) -> Task (a, TaskId)
withTaskId (Task task) = Task eval
where
	eval event evalOpts=:{TaskEvalOpts|taskId} iworld
		= case task event evalOpts iworld of
			(ValueResult (Value x st) info rep newtask, iworld)
				= (ValueResult (Value (x, taskId) st) info rep (withTaskId newtask), iworld)
			(ExceptionResult te, iworld) = (ExceptionResult te, iworld)
			(DestroyedResult, iworld) = (DestroyedResult, iworld)

withTemporaryDirectory :: (FilePath -> Task a) -> Task a | iTask a
withTemporaryDirectory taskfun = Task evalinit
where
	//Initialization
	evalinit DestroyEvent _ iworld
		= (DestroyedResult, iworld)
	evalinit event eo=:{TaskEvalOpts|taskId} iworld=:{options={appVersion,tempDirPath}}
		# tmpDir                    = tempDirPath </> (appVersion +++ "-" +++ toString taskId +++ "-tmpdir")
		# (taskIda,iworld=:{world}) = getNextTaskId iworld
		# (ok ,world)               = ensureDirectoryExists tmpDir world
		# iworld & world            = world
		= case ok of
			Ok _    = eval tmpDir taskIda (taskfun tmpDir) event eo iworld
			Error e = (ExceptionResult (exception ("Could not create temporary directory: " +++ tmpDir +++ " (" +++ toString e +++ ")")) , iworld)
	//Actual task execution
	//First destroy the inner task, then delete the tmp dir
	eval tmpDir innerTaskId (Task inner) DestroyEvent evalOpts iworld
		# (resa,iworld)  = inner DestroyEvent {TaskEvalOpts|evalOpts&taskId=innerTaskId} iworld
		# (merr, world)  = recursiveDelete tmpDir iworld.world
		# iworld & world = world
		| isError merr = (ExceptionResult (exception (fromError merr)), iworld)
		= case resa of
			ValueResult _ _ _ _ = (ExceptionResult (exception "Failed to destroy inner task withTempdir"), iworld)
			e = (e, iworld)

	//During execution, set the cwd to the tmp dir
	eval tmpDir innerTaskId (Task inner) event evalOpts=:{TaskEvalOpts|lastEval} iworld
		# (oldcurdir, iworld)= liftIWorld getCurrentDirectory iworld
		| isError oldcurdir  = (ExceptionResult (exception (fromError oldcurdir)), iworld)
		# (Ok oldcurdir)     = oldcurdir
		# (mbErr, iworld)    = liftIWorld (setCurrentDirectory tmpDir) iworld
		| isError mbErr      = (ExceptionResult (exception (fromError mbErr)), iworld)
		# (resa, iworld)     = inner event {TaskEvalOpts|evalOpts&taskId=innerTaskId} iworld
		# (mbErr,iworld)     = setCurrentDirectory oldcurdir iworld
		| isError mbErr      = (ExceptionResult (exception (fromError mbErr)), iworld)
		= case resa of
			ValueResult value info rep newinner
				# info = {TaskEvalInfo|info & lastEvent = max lastEval info.TaskEvalInfo.lastEvent}
				= (ValueResult value info rep (Task (eval tmpDir innerTaskId newinner)), iworld)
			ExceptionResult e = (ExceptionResult e, iworld)

instance toString (OSErrorCode,String) where toString x = snd x
