implementation module iTasks.WF.Tasks.IO

import iTasks.Internal.SDS
import iTasks.Internal.Util
import iTasks.SDS.Combinators.Common
import iTasks.SDS.Definition
import iTasks.WF.Definition
import iTasks.WF.Derives
import iTasks.UI.Definition
import iTasks.UI.Editor

import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskState
import iTasks.Internal.TaskEval
import iTasks.Internal.TaskServer
import iTasks.Internal.Generic.Visualization
import iTasks.Internal.Generic.Defaults
import iTasks.WF.Tasks.Core

import System.Process
import Text, Text.GenJSON, StdString, StdInt, StdBool, StdList, StdTuple, Data.Tuple, Data.Func, StdFunc
import qualified Data.Map as DM
import qualified Data.Set as DS

:: ExitCode = ExitCode !Int
:: ExternalProcessHandlers l r w =
    { onStartup     :: !(           r -> (MaybeErrorString l, Maybe w, [String], Bool))
    , onOutData     :: !(String   l r -> (MaybeErrorString l, Maybe w, [String], Bool))
    , onErrData     :: !(String   l r -> (MaybeErrorString l, Maybe w, [String], Bool))
    , onShareChange :: !(         l r -> (MaybeErrorString l, Maybe w, [String], Bool))
    , onExit        :: !(ExitCode l r -> (MaybeErrorString l, Maybe w                ))
    }

derive JSONEncode ProcessHandle, ProcessIO
derive JSONDecode ProcessHandle, ProcessIO

liftOSErr f iw = case (liftIWorld f) iw of
	(Error (_, e), iw) = (Error (exception e), iw)
	(Ok a, iw) = (Ok a, iw)

externalProcess :: !Timespec !FilePath ![String] !(Maybe FilePath) !(Maybe ProcessPtyOptions) !(Shared sds1 [String]) !(Shared sds2 ([String], [String])) -> Task Int | RWShared sds1 & RWShared sds2
externalProcess poll cmd args dir mopts sdsin sdsout = Task evalinit
where
	evalinit DestroyEvent _ iworld
		= (DestroyedResult, iworld)
	evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld
		= case liftOSErr (maybe (runProcessIO cmd args dir) (runProcessPty cmd args dir) mopts) iworld of
			(Error e, iworld)  = (ExceptionResult e, iworld)
			(Ok phpio, iworld) = eval phpio event evalOpts iworld

	eval (ph, pio) DestroyEvent {TaskEvalOpts|taskId} iworld
		# iworld = clearTaskSDSRegistrations ('DS'.singleton taskId) iworld
		= apIWTransformer iworld
		$       liftOSErr (terminateProcess ph)
		>-= \_->liftOSErr (closeProcessIO pio)
		>-= \_->tuple (Ok DestroyedResult)
	//TODO: Support async sdss
	eval (ph, pio) event {taskId,lastEval} iworld
		| not (isRefreshForTask event taskId)
			= (ValueResult NoValue (mkTaskEvalInfo lastEval) (mkUIIfReset event rep) (Task (eval (ph, pio))), iworld)
		= apIWTransformer iworld $
			read sdsout EmptyContext                    >-= \(ReadingDone (stdoutq, stderrq))->
			liftOSErr (readPipeNonBlocking pio.stdOut)  >-= \stdoutData->
			liftOSErr (readPipeNonBlocking pio.stdErr)  >-= \stderrData->
			(if (stdoutData == "" && stderrData == "")
				(tuple (Ok WritingDone))
				(write (stdoutq ++ filter ((<>)"") [stdoutData]
				       ,stderrq ++ filter ((<>)"") [stderrData]
				       ) sdsout EmptyContext))          >-= \WritingDone->
			liftOSErr (checkProcess ph)                 >-= \mexitcode->case mexitcode of
				(Just i) = tuple (Ok (ValueResult (Value i True) (mkTaskEvalInfo lastEval) (mkUIIfReset event rep) (treturn i)))
				Nothing =
					readRegister taskId clock                            >-= \_->
					readRegister taskId sdsin                            >-= \(ReadingDone stdinq)->
					liftOSErr (writePipe (concat stdinq) pio.stdIn)      >-= \_->
					(if (stdinq =: []) (tuple (Ok WritingDone)) (write [] sdsin EmptyContext)) >-= \WritingDone ->
					tuple (Ok (ValueResult NoValue (mkTaskEvalInfo lastEval) (mkUIIfReset event rep)
						(Task (eval (ph, pio)))))

	rep = stringDisplay ("External process: " <+++ cmd)
	clock = sdsFocus {start=zero,interval=poll} iworldTimespec

tcplisten :: !Int !Bool !(sds () r w) (ConnectionHandlers l r w) -> Task [l] | iTask l & iTask r & iTask w & RWShared sds
tcplisten port removeClosed sds handlers = Task eval
where
	evalinit DestroyEvent _ iworld = (DestroyedResult, iworld)
	evalinit event evalOpts=:{TaskEvalOpts|taskId} iworld
		= case addListener taskId port removeClosed (wrapConnectionTask handlers sds) iworld of
			(Error e, iworld) = (ExceptionResult e, iworld)
			(Ok _, iworld) = eval event evalOpts iworld

	eval DestroyEvent {TaskEvalOpts|taskId} iworld=:{ioStates}
		# ioStates = case 'DM'.get taskId ioStates of
			Just (IOActive values)  = 'DM'.put taskId (IODestroyed values) ioStates
			_                       = ioStates
		= (DestroyedResult,{iworld & ioStates = ioStates})
	eval event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld=:{ioStates}
		= case 'DM'.get taskId ioStates of
			Just (IOException e) = (ExceptionResult (exception e), iworld)
			Just (IOActive values)
				# value = Value [l \\ (_,(l :: l^,_)) <- 'DM'.toList values] False
				= (ValueResult value (mkTaskEvalInfo lastEval) (mkUIIfReset event (rep port)) (Task eval), iworld)
			Nothing = (ValueResult (Value [] False) (mkTaskEvalInfo lastEval) (mkUIIfReset event (rep port)) (Task eval), iworld)

	rep port = stringDisplay ("Listening for connections on port "<+++ port)

tcpconnect :: !String !Int !(sds () r w) (ConnectionHandlers l r w) -> Task l | iTask l & iTask r & iTask w & RWShared sds
tcpconnect host port sds handlers = Task evalinit
where
	//We cannot make ioStates local since the engine uses it
	evalinit DestroyEvent _ iworld
		= (DestroyedResult, iworld)
	evalinit event eo=:{TaskEvalOpts|taskId} iworld
		= case addConnection taskId host port (wrapConnectionTask handlers sds) iworld of
			(Error e,iworld) = (ExceptionResult e, iworld)
			(Ok _,iworld) = eval event eo iworld

	eval DestroyEvent evalOpts=:{TaskEvalOpts|taskId} iworld=:{ioStates}
		# ioStates = case 'DM'.get taskId ioStates of
			Just (IOActive values)  = 'DM'.put taskId (IODestroyed values) ioStates
			_                       = ioStates
		= (DestroyedResult, {iworld & ioStates = ioStates})

	eval event evalOpts=:{TaskEvalOpts|taskId,lastEval} iworld=:{ioStates}
		= case 'DM'.get taskId ioStates of
			Nothing = (ValueResult NoValue (mkTaskEvalInfo lastEval) (mkUIIfReset event rep) (Task eval), iworld)
			Just (IOActive values)
				= case 'DM'.get 0 values of
					Just (l :: l^, s)
						= (ValueResult (Value l s) (mkTaskEvalInfo lastEval) (mkUIIfReset event rep) (Task eval), iworld)
					_
						= (ExceptionResult (exception "Corrupt IO task result"),iworld)
			Just (IOException e)
				= (ExceptionResult (exception e),iworld)

	rep = stringDisplay ("TCP client " <+++ host <+++ ":" <+++ port)
