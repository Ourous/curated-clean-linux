implementation module iTasks.Internal.Tonic

import iTasks.Engine
import iTasks.Internal.SDS
import qualified iTasks.Internal.SDS as DSDS
import iTasks.Internal.AsyncSDS
import iTasks.Internal.IWorld
import iTasks.Internal.Tonic.AbsSyn
import iTasks.Internal.Tonic.Blueprints
import iTasks.Internal.Tonic.Images
import iTasks.Internal.Tonic.Types
import iTasks.Internal.Tonic.Pretty
import iTasks.Internal.Tonic.Shares
import iTasks.Internal.Tonic.Server
import iTasks.Internal.TaskState
import iTasks.Internal.TaskStore
import iTasks.Internal.TaskEval
import iTasks.Internal.Task
import iTasks.Internal.Serialization
import iTasks.WF.Tasks.Interaction
import iTasks.WF.Combinators.Common
import iTasks.Extensions.Admin.UserAdmin
import iTasks.Extensions.DateTime
import System.File
from StdFunc import o
from System.FilePath import </>
from StdMisc import abort
from StdFile import instance FileSystem World
import StdArray
import System.Directory, System.FilePath, Data.Func, Data.Functor, Data.List
import qualified Data.Map as DM
from Data.Map import instance Functor (Map a)
from Data.Set import :: Set
import qualified Data.Set as DS
import qualified Data.Foldable as DF
from Data.Foldable import class Foldable
from Data.IntMap.Strict import :: IntMap
import qualified Data.IntMap.Strict as DIS
import Text
import Data.GenLexOrd
from Control.Monad import `b`, class Monad(bind)
import qualified Control.Applicative as CA
from Control.Applicative import class Applicative
import Data.CircularStack
import qualified TCPIP as TCP
from TCPChannelClass import :: DuplexChannel {..}, instance ChannelEnv World, class ChannelEnv
from TCPChannels import instance Send TCP_SChannel_, class Send, instance closeRChannel	TCP_RChannel_, class closeRChannel
from TCPEvent import instance accSChannel TCP_SChannel_, class accSChannel
import System.IO
import StdFile

//-----------------------------------------------------------------------------
// INSTANCES
//-----------------------------------------------------------------------------

instance TonicTopLevelBlueprint Task where
  tonicWrapBody mn tn args cases t = tonicWrapTaskBody` mn tn args cases t
  tonicWrapArg d ptr v = viewInformation d [] v @! ()

instance TonicBlueprintPart Task where
  tonicWrapApp mn fn nid cases t = tonicWrapApp` mn fn nid cases t

instance TonicTopLevelBlueprint Maybe where
  tonicWrapBody mn tn args _ t = tonicIOTopLevel mn tn t
  tonicWrapArg _ _ _ = return ()

instance TonicBlueprintPart Maybe where
  tonicWrapApp mn fn nid _ mb = tonicIOBlueprintPart mn fn nid mb

instance TonicTopLevelBlueprint (Either e) where
  tonicWrapBody mn tn args _ t = tonicIOTopLevel mn tn t
  tonicWrapArg _ _ _ = return ()

instance TonicBlueprintPart (Either e) where
  tonicWrapApp mn fn nid _ mb = tonicIOBlueprintPart mn fn nid mb

import StdDebug
tcpsend :: TonicMessage *World -> *World
tcpsend msg world
  = case tcpsend` "localhost" 9000 [toString (toJSON msg) +++ "TONIC_EOL"] world of
	  (Ok _, world) = world
	  (Error str, world)
		| trace_tn ("Failed to connect to Tonic server: " +++ str) = world

tcpsend` :: String Int [String] *World -> *(MaybeError String (), *World)
tcpsend` host port out world
  # (mbIP, world) = 'TCP'.lookupIPAddress host world
  = case mbIP of
	  Nothing = (mkError, world)
	  Just ip
		# (_, mbConn, world) = 'TCP'.connectTCP_MT Nothing (ip, port) world
		= case mbConn of
			Nothing
			  = (mkError, world)
			Just {DuplexChannel | rChannel, sChannel}
			  # (sChannel, world) = case out of
									  []   -> (sChannel, world)
									  data -> foldl (\(s, w) d -> 'TCP'.send ('TCP'.toByteSeq d) s w) (sChannel, world) data
			  # world = 'TCP'.closeRChannel rChannel world
			  # world = 'TCP'.closeChannel sChannel world
			  = (Ok (), world)
  where
  mkError = Error ("Failed to connect to host " +++ host)

instance TonicTopLevelBlueprint IO where
  tonicWrapBody mn tn args _ t = tonicIOTopLevel mn tn t
  tonicWrapArg _ _ _ = return ()

instance TonicBlueprintPart IO where
  tonicWrapApp mn fn nid _ mb = tonicIOBlueprintPart mn fn nid mb

TonicBookkeepingFile =: "TonicBookkeepingFile"

:: TonicBookkeeping =
  { computations    :: Map [Int] TonicComputation
  , bkComputationId :: [Int]
  }

:: TonicComputation =
  { computationId :: [Int]
  , moduleName    :: String
  , funcName      :: String
  }

derive class iTask TonicBookkeeping, TonicComputation

openBookkeeping :: *World -> *(TonicBookkeeping, *World)
openBookkeeping world
  # (ok, file, world) = fopen TonicBookkeepingFile FReadText world
  | ok
	# (str, file) = freads file 16777216
	# (_, world)  = fclose file world
	= case fromJSON (fromString str) of
		Just bk = (bk, world)
		_       = (newbk, world)
  | otherwise = (newbk, world)
  where
  newbk = { TonicBookkeeping
		  | computations    = 'DM'.newMap
		  , bkComputationId = [0]
		  }

writeBookkeeping :: TonicBookkeeping *World -> *World
writeBookkeeping bk world
  # (ok, file, world) = fopen TonicBookkeepingFile FWriteText world
  | ok
	# file       = fwrites (toString (toJSON bk)) file
	# (_, world) = fclose file world
	= world
  | otherwise = world

tonicIOTopLevel :: ModuleName FuncName (m a) -> m a | TMonad m & iTask a
tonicIOTopLevel mn tn t
  | isLambda tn = t
  | unsafePerformIOTrue createTopLevel = t
  where
  createTopLevel :: *World -> *((), *World)
  createTopLevel world
	# (bk, world)       = openBookkeeping world
	# bk & computations = 'DM'.put bk.TonicBookkeeping.bkComputationId (mkComputation bk.TonicBookkeeping.bkComputationId) bk.computations
	# world             = writeBookkeeping bk world
	# msg               = { TMNewTopLevel
						  | tmn_computationId  = bk.TonicBookkeeping.bkComputationId
						  , tmn_bpModuleName   = mn
						  , tmn_bpFunctionName = tn
						  }
	# world             = tcpsend (TMNewTopLevel msg) world
	= ((), world)
	where
	mkComputation :: [Int] -> TonicComputation
	mkComputation n = { TonicComputation
					  | computationId = n
					  , moduleName    = mn
					  , funcName      = tn
					  }

tonicIOBlueprintPart :: ModuleName FuncName ExprId (m a) -> m a | TMonad m & iTask a
tonicIOBlueprintPart mn fn nid mb
  | isLambda  fn = mb
  | isBind mn fn = mb
  | unsafePerformIOTrue persistTonicState1
	=          mb
	>>= \x ->  return (unsafePerformIO persistTonicState2)
	>>= \() -> return x
  where
  persistTonicState1 :: *World -> *((), *World)
  persistTonicState1 world
	# (bk, world)          = openBookkeeping world
	# curId                = bk.TonicBookkeeping.bkComputationId
	# bk & bkComputationId = case curId of
							   [x : xs] -> [x + 1 : x : xs]
							   xs       -> xs
	# world                = writeBookkeeping bk world
	# world                = case 'DM'.get (tl curId) bk.TonicBookkeeping.computations of
							   Just comp
								 # msg = { TMApply
										 | tma_computationId  = curId
										 , tma_nodeId         = nid
										 , tma_bpModuleName   = comp.TonicComputation.moduleName
										 , tma_bpFunctionName = comp.TonicComputation.funcName
										 , tma_appModuleName  = mn
										 , tma_appFunName     = fn
										 }
								 = tcpsend (TMApply msg) world
							   _ = world
	= ((), world)
  persistTonicState2 :: *World -> *((), *World)
  persistTonicState2 world
	# (bk, world)          = openBookkeeping world
	# bk & bkComputationId = case bk.TonicBookkeeping.bkComputationId of
							   [x : y : xs] -> [x : xs]
							   [x : xs]     -> xs
							   xs           -> xs
	# world                = writeBookkeeping bk world
	= ((), world)

instance TApplicative IO where
  return x   = IO (\s -> (x, s))
  (<#>) f g  = liftA2 id f g

instance TMonad IO where
  (>>=) (IO f) a2mb = IO run
	where
	  run world
		# (x, world) = f world
		# (IO g)     = a2mb x
		= g world
  (>>|) l r = l >>= \_ -> r

ppnid nid = "[" +++ ppnid` nid +++ "]"
  where
  ppnid` [] = ""
  ppnid` [x] = toString x
  ppnid` [x:xs] = toString x +++ ", " +++ ppnid` xs

liftA2 f a b = f <$> a <#> b

derive class iTask StaticDisplaySettings, DynamicDisplaySettings,
				   DynamicView, BlueprintQuery, CircularStack
derive gEditor Set
derive gText Set
derive gDefault Set

//-----------------------------------------------------------------------------
// REST
//-----------------------------------------------------------------------------

tonicExtWrapArg :: !VarName !Int !a -> m () | iTask a & TonicTopLevelBlueprint m
tonicExtWrapArg d n v = tonicWrapArg d n v

tonicExtWrapBody :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (         m a) -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBody mn tn args cases t = tonicWrapBody mn tn args cases t

tonicExtWrapBodyLam1 :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b     -> m a) -> b     -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam1 mn tn args cases f = \x -> tonicWrapBody mn tn args cases (f x)

tonicExtWrapBodyLam2 :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c   -> m a) -> b c   -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam2 mn tn args cases f = \x y -> tonicWrapBody mn tn args cases (f x y)

tonicExtWrapBodyLam3 :: !ModuleName !FuncName [(VarName, Int, m ())] [(ExprId, Int)] (b c d -> m a) -> b c d -> m a | TonicTopLevelBlueprint m & iTask a
tonicExtWrapBodyLam3 mn tn args cases f = \x y z -> tonicWrapBody mn tn args cases (f x y z)

tonicWrapTaskBody` :: !ModuleName !FuncName [(VarName, Int, Task ())] [(ExprId, Int)] (Task a) -> Task a | iTask a
tonicWrapTaskBody` mn tn args cases t=:(Task eval)
  | isLambda tn = Task updCases
  | otherwise   = Task preEval
  where
  setBlueprintInfo :: !TaskEvalOpts -> TaskEvalOpts
  setBlueprintInfo evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintModuleName = mn
																  , currBlueprintFuncName   = tn})

  setBPTaskId :: !TaskId !TaskEvalOpts -> TaskEvalOpts
  setBPTaskId tid evalOpts = modTonicOpts evalOpts (\teo -> {teo & currBlueprintTaskId = tid})

  resetInhOpts :: !TaskEvalOpts -> TaskEvalOpts
  resetInhOpts evalOpts = modTonicOpts evalOpts (\teo -> {teo & inParallel   = Nothing
															  , inAssignNode = Nothing })

  updCases event evalOpts taskTree iworld
	# iworld = addCases evalOpts cases iworld
	= eval event evalOpts taskTree iworld

  preEval event evalOpts taskTree iworld
	# (mmn, iworld) = getModule` mn iworld
	= case mmn of
		Ok mod -> eval` mod event evalOpts taskTree iworld
		_      -> eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld

  eval` mod event evalOpts=:{tonicOpts={callTrace, currBlueprintTaskId}} taskTree=:(TCInit currTaskId=:(TaskId instanceNo _) _) iworld
	# iworld = updateInstance iworld
	= eval event (resetInhOpts (setBPTaskId currTaskId (setBlueprintInfo evalOpts))) taskTree iworld
	where
	updateInstance iworld =
	  case getTonicFunc mod tn of
		Just bprep
		  # (curr,   iworld) = iworld!current
		  # (currDateTime, iworld) = iworldLocalDateTime` iworld
		  # (muser, iworld)  = 'DSDS'.read (sdsFocus instanceNo taskInstanceUser) EmptyContext iworld
		  # bpinst           = { BlueprintInstance
							   | bpi_taskId           = currTaskId
							   , bpi_startTime        = currDateTime
							   , bpi_lastUpdated      = currDateTime
							   , bpi_endTime          = Nothing
							   , bpi_activeNodes      = 'DM'.newMap
							   , bpi_previouslyActive = 'DM'.newMap
							   , bpi_parentTaskId     = currBlueprintTaskId
							   , bpi_blueprint        = bprep
							   , bpi_currentUser      = case error2mb muser of
								  Nothing = Nothing
								  Just (ReadingDone v) = Just v
							   , bpi_case_branches    = 'DM'.newMap
							   , bpi_index            = 0
							   , bpi_bpref            = { BlueprintIdent
														| bpr_moduleName = mn
														, bpr_taskName   = tn
														}
							   }
		  # bpinst           = addCases` bpinst evalOpts cases
		  # (_, iworld)      = 'DSDS'.write bpinst (sdsFocus (currTaskId, mn, tn) tonicInstances) EmptyContext iworld
		  # (_, iworld)      = 'DSDS'.write args (sdsFocus (mn, tn, currTaskId) paramsForTaskInstance) EmptyContext iworld
		  = iworld
		_ = iworld

  eval` _ event evalOpts=:{tonicOpts = tonicOpts=:{currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName}} taskTree=:(TCDestroy _) iworld
	# (tr, iworld) = eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld
	= (tr, okSt iworld logTaskEnd (taskIdFromTaskTree taskTree))
	where
	logTaskEnd currTaskId iworld
	  # (mbpref, iworld) = 'DSDS'.read (sdsFocus (currTaskId, mn, tn) tonicInstances) EmptyContext iworld
	  = case mbpref of
		  Ok (ReadingDone (Just bpi))
			 # (currDateTime, iworld) = iworldLocalDateTime` iworld
			 # oldActive        = 'DM'.union ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems bpi.bpi_activeNodes)])
											 bpi.bpi_previouslyActive
			 # (_, iworld)      = 'DSDS'.write { bpi
											   & bpi_endTime          = Just currDateTime
											   , bpi_previouslyActive = oldActive
											   , bpi_activeNodes      = 'DM'.newMap
											   } (sdsFocus (currTaskId, mn, tn) tonicInstances) EmptyContext iworld
			 = iworld
		  _  = iworld

  eval` _ event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
	# (tr, iworld) = eval event (resetInhOpts (setBPTaskId currTaskId (setBlueprintInfo evalOpts))) taskTree iworld
	# iworld       = markStable currTaskId mn tn iworld
	# iworld       = storeTaskOutputViewer tr evalOpts.tonicOpts.currBlueprintExprId evalOpts.tonicOpts.currBlueprintTaskId currTaskId iworld
	= (tr, iworld)

  eval` _ event evalOpts taskTree=:TCNop iworld
	= eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld

  eval` _ event evalOpts taskTree iworld
	# evalOpts     = case taskIdFromTaskTree taskTree of
					   Ok tid -> setBPTaskId tid evalOpts
					   _      -> evalOpts
	# (tr, iworld) = eval event (resetInhOpts (setBlueprintInfo evalOpts)) taskTree iworld
	# iworld       = case (taskIdFromTaskTree taskTree, tr) of
					   (Ok tid, ValueResult (Value _ True) _ _ _)
						 # iworld = markStable tid mn tn iworld
						 = storeTaskOutputViewer tr evalOpts.tonicOpts.currBlueprintExprId evalOpts.tonicOpts.currBlueprintTaskId tid iworld
					   _ = iworld
	= (tr, iworld)

modTonicOpts :: !TaskEvalOpts !(TonicOpts -> TonicOpts) -> TaskEvalOpts
modTonicOpts teo f = {teo & tonicOpts = f teo.tonicOpts}

markStable :: !TaskId !ModuleName !FuncName !*IWorld -> *IWorld
markStable currTaskId currBlueprintModuleName currBlueprintFuncName iworld
  # focus            = sdsFocus (currTaskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances
  # (mbpref, iworld) = 'DSDS'.read focus EmptyContext iworld
  = case mbpref of
	  Ok (ReadingDone (Just {bpi_endTime = Just _})) // Already marked as stable, don't do extra work
		= iworld
	  Ok (ReadingDone (Just bpi))
		# (curr, iworld)         = iworld!current
		# (currDateTime, iworld) = iworldLocalDateTime` iworld
		# oldActive        = 'DM'.union ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems bpi.bpi_activeNodes)])
										bpi.bpi_previouslyActive
		# (_, iworld)      = 'DSDS'.write { bpi
										  & bpi_previouslyActive = oldActive
										  , bpi_activeNodes      = 'DM'.newMap
										  , bpi_lastUpdated      = currDateTime
										  , bpi_endTime          = Just currDateTime
										  } focus EmptyContext iworld
		= iworld
	  _ = iworld

tonicExtWrapApp :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] (m a) -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapApp mn tn nid cases mapp = tonicWrapApp mn tn nid cases mapp

isBind :: !String !String -> Bool
isBind "iTasks.API.Core.Types" ">>=" = True
isBind "iTasks.API.Core.Types" ">>|" = True
isBind _                       _     = False

isStep :: !String !String -> Bool
isStep "iTasks.API.Core.TaskCombinators"   "step" = True
isStep "iTasks.API.Common.TaskCombinators" ">>*"  = True
isStep _                                   _      = False

isParallel :: !String !String -> Bool
isParallel "iTasks.API.Core.TaskCombinators"   "parallel" = True
isParallel "iTasks.API.Common.TaskCombinators" "-&&-"     = True
isParallel "iTasks.API.Common.TaskCombinators" "-||-"     = True
isParallel "iTasks.API.Common.TaskCombinators" "||-"      = True
isParallel "iTasks.API.Common.TaskCombinators" "-||"      = True
isParallel "iTasks.API.Common.TaskCombinators" "anyTask"  = True
isParallel "iTasks.API.Common.TaskCombinators" "allTasks" = True
isParallel _                                   _          = False

isAssign :: !String !String -> Bool
isAssign "iTasks.API.Extensions.User" "@:" = True
isAssign _                            _    = False

isLambda :: !FuncName -> Bool
isLambda str = startsWith "\;" str

stepEval cases eval nid event evalOpts taskTree=:(TCInit childTaskId _) iworld
  = stepEval` cases nid childTaskId eval event evalOpts taskTree iworld
stepEval cases eval nid event evalOpts taskTree=:(TCStep childTaskId _ (Left _)) iworld
  = stepEval` cases nid childTaskId eval event evalOpts taskTree iworld
stepEval cases eval nid event evalOpts taskTree iworld
  # (tr, iworld) = eval event evalOpts taskTree iworld
  # iworld = case tr of
			   ValueResult (Value x _) _ _ _ -> addCases evalOpts (map (\(eid, f) -> (eid, f x)) cases) iworld
			   _ -> iworld
  = (tr, iworld)

stepEval` cases nid childTaskId=:(TaskId ino tno) eval event evalOpts=:{TaskEvalOpts|tonicOpts} taskTree iworld
  # (taskResult, iworld) = eval event evalOpts taskTree iworld
  # iworld               = case taskResult of
							 ValueResult (Value x _) _ change _
								# iworld = addCases evalOpts (map (\(eid, f) -> (eid, f x)) cases) iworld
								# iworld = case change of
									ReplaceUI ui = storeActions ui iworld
									_ 			 = iworld
							   = iworld
							 ValueResult _ _ change _
								# iworld = case change of
									ReplaceUI ui = storeActions ui iworld
									_ 			 = iworld
							   = iworld
							 _ = iworld
  = (taskResult, iworld)
  where
  storeActions uiDef iworld
	// TODO
	// This LC filters out the actions for the current task. For some reason, we sometimes
	// get actions for the _next_ step here. Why is this? Ideally, we should remove this LC here.
	= case [a \\ a <- [] /*uiDefActions uiDef*/ | ("TODO" /*a.UIAction.taskId*/) == toString ino +++ "-" +++ toString tno] of
		[] = iworld
		xs
		  # focus         = sdsFocus (tonicOpts.currBlueprintTaskId, nid) tonicActionsForTaskIDAndExpr
		  # (mas, iworld) = 'DSDS'.read focus EmptyContext iworld
		  # iworld        = case mas of
							  Ok (ReadingDone as) | as === xs -> iworld
							  _                 -> snd ('DSDS'.write xs focus EmptyContext iworld)
		  = iworld

derive class iTask TonicOpts

ppeid xs = foldr (\x xs -> toString x +++ "," +++ xs) "" xs

addCases evalOpts [] iworld = iworld
addCases evalOpts=:{TaskEvalOpts|tonicOpts={currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName}} cases iworld
  # focus               = sdsFocus (currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances
  # (mParentBP, iworld) = 'DSDS'.read focus EmptyContext iworld
  = case mParentBP of
	  Ok (ReadingDone (Just parentBPInst))
		# bpi = addCases` parentBPInst evalOpts cases
		= snd ('DSDS'.write bpi focus EmptyContext iworld)
	  _ = iworld

addCases` parentBPInst evalOpts=:{TaskEvalOpts|tonicOpts} cases
  = {parentBPInst & bpi_case_branches = 'DM'.union ('DM'.fromList cases) parentBPInst.bpi_case_branches}

isVar :: !String -> Bool
isVar "(Var)"      = True
isVar "(Var @ es)" = True
isVar _            = False

/**
 * ModuleName and FuncName identify the blueprint, of which we need to
 * highlight nodes.
 */
tonicWrapApp` :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] (Task a) -> Task a | iTask a
tonicWrapApp` mn fn nid cases t=:(Task eval)
  | isStep mn fn             = Task (stepEval cases eval nid)
  | isLambda fn              = t
  | isBind mn fn             = t
  | otherwise                = Task eval`
  where
  updateAssignStatus evalOpts
	= { evalOpts
	  & tonicOpts = { evalOpts.tonicOpts
					& inAssignNode = if (isJust evalOpts.tonicOpts.inAssignNode)
									   Nothing
									   (if (isAssign mn fn)
										  (Just nid)
										  evalOpts.tonicOpts.inAssignNode)
					}
	  }

  // TODO Double check focusses (foci?)
  eval` event evalOpts=:{TaskEvalOpts|tonicOpts = tonicOpts=:{currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName}} taskTree=:(TCInit childTaskId=:(TaskId childInstanceNo _) _) iworld
	# (mParentBP, iworld) = 'DSDS'.read (sdsFocus (currBlueprintTaskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances) EmptyContext iworld
	= case mParentBP of
		Ok (ReadingDone (Just parentBPInst))
		  # (parentBPInst, iworld)
			  = case tonicOpts.inAssignNode of
				  Just assignNode
					# (muser, iworld)     = 'DSDS'.read (sdsFocus childInstanceNo taskInstanceUser) EmptyContext iworld
					# (parent_body, _, _) = case muser of
											  Ok (ReadingDone usr)
												= updateNode assignNode (\x -> case x of
																				 TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp eid` "_Tuple2" [_, descr] prio : as] assoc ptr
																				   | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TFApp eid` "_Tuple2" [TLit (TString (toString usr)), descr] prio : as] assoc ptr
																				   | otherwise         = x
																				 TMApp eid mtn "iTasks.API.Extensions.User" "@:" [_ : as] assoc ptr
																				   | eid == assignNode = TMApp eid mtn "iTasks.API.Extensions.User" "@:" [TLit (TString (toString usr)) : as] assoc ptr
																				   | otherwise         = x
																				 e = e
																		) parentBPInst.bpi_blueprint.tf_body
											  _ = (parentBPInst.bpi_blueprint.tf_body, False, Nothing)
					= ({parentBPInst & bpi_blueprint = {parentBPInst.bpi_blueprint & tf_body = parent_body}}, iworld)
				  _ = (parentBPInst, iworld)
		  # evalOpts     = if (isParallel mn fn)
							 {evalOpts & tonicOpts = {tonicOpts & inParallel = Just childTaskId}}
							 evalOpts
		  # evalOpts     = {evalOpts & tonicOpts = {tonicOpts & currBlueprintExprId = nid}}
		  # iworld       = updRTMap tonicOpts nid childTaskId parentBPInst iworld
		  # (tr, iworld) = eval event (updateAssignStatus evalOpts) taskTree iworld
		  // These reads need to be done here, because:
		  // - The parent blueprint may have been altered while evaluating the continuation
		  // - The childTaskId blueprint won't be instantiated before the continuation is evaluated
		  # (mparent_bpr, iworld) = 'DSDS'.read (sdsFocus (parentBPInst.bpi_taskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances) EmptyContext iworld
		  # iworld = case (tr, mparent_bpr) of
					   (ValueResult _ _ _ (TCParallel childTaskId _ parallelChildren _), Ok (ReadingDone (Just new_parent_instance)))
						 = evalParallel new_parent_instance tr evalOpts childTaskId parallelChildren iworld
					   (_, Ok (ReadingDone (Just new_parent_instance)))
						 # (new_parent_instance, chng)          = case (tr, cases) of
																	(ValueResult (Value x _) _ _ _, [_ : _]) -> (addCases` new_parent_instance evalOpts (map (\(eid, f) -> (eid, f x)) cases), True)
																	_                                        -> (new_parent_instance, False)
						 # iworld                               = storeTaskOutputViewer tr nid parentBPInst.bpi_taskId childTaskId iworld
						 # (mchild_bpr, iworld)                 = if (isVar fn)
																	(case 'DSDS'.read (sdsFocus childTaskId allTonicInstances) EmptyContext iworld of
																		(Error e, iworld) = (Nothing, iworld)
																		(Ok (ReadingDone r), iworld) = (Just (r), iworld)
																	)
																	(Just [], iworld)
						 # (new_parent_instance, chng`, iworld) = case mchild_bpr of
																	Just bprefs=:[_ : _]
																	  = case [bpi \\ (_, bpi=:{bpi_taskId, bpi_index}) <- bprefs | bpi_taskId > parentBPInst.bpi_taskId || (bpi_taskId == parentBPInst.bpi_taskId && bpi_index > parentBPInst.bpi_index)] of
																		  [{bpi_bpref} : _]
																			# (parent_body, chng, mvid) = updateNode nid (\x -> case x of
																																  TVar eid _ _ -> TMApp eid Nothing bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName [] TNoPrio Nothing
																																  TMApp _ _ _ _ _ _ (Just _) -> TAugment x (TLit (TString (bpi_bpref.bpr_moduleName +++ "." +++ bpi_bpref.bpr_taskName)))
																																  e -> e
																														 ) new_parent_instance.bpi_blueprint.tf_body
																			| chng
																				# parent_body = case mvid of
																								  Just (vid, expr) -> replaceNode vid expr parent_body
																								  _                -> parent_body
																				# parent_bpr  = {new_parent_instance & bpi_blueprint = {new_parent_instance.bpi_blueprint & tf_body = parent_body}}
																				= (parent_bpr, True, iworld)
																			| otherwise = (new_parent_instance, False, iworld)
																		  _ = (new_parent_instance, False, iworld)
																	Nothing = (new_parent_instance, False, iworld)
						 | chng || chng` = snd ('DSDS'.write new_parent_instance (sdsFocus (new_parent_instance.bpi_taskId, currBlueprintModuleName, currBlueprintFuncName) tonicInstances) EmptyContext iworld)
						 | otherwise     = iworld
					   _ = iworld
		  = (tr, iworld)
		_ = eval event (updateAssignStatus evalOpts) taskTree iworld

  eval` event evalOpts taskTree=:(TCStable currTaskId _ _) iworld
	# evalOpts             = {evalOpts & tonicOpts = {evalOpts.tonicOpts & currBlueprintExprId = nid}}
	# (tr, iworld)         = eval event evalOpts taskTree iworld
	# (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus currTaskId allTonicInstances) EmptyContext iworld
	# iworld               = case mchild_bpr of
							   Ok (ReadingDone xs)
								 = snd (mapSt (\(_, {bpi_bpref}) iworld -> ((), markStable currTaskId bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName iworld)) xs iworld)
							   _ = iworld
	# iworld       = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId currTaskId iworld
	= (tr, iworld)

  eval` event evalOpts taskTree=:TCNop iworld
	= eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree=:(TCDestroy _) iworld
	= eval event evalOpts taskTree iworld

  eval` event evalOpts taskTree iworld
	= case taskIdFromTaskTree taskTree of
		Ok tid
		  # evalOpts     = {evalOpts & tonicOpts = {evalOpts.tonicOpts & currBlueprintExprId = nid}}
		  # (tr, iworld) = eval event (updateAssignStatus evalOpts) taskTree iworld
		  # iworld       = case tr of
							 (ValueResult (Value x stable) _ _ _)
							   # iworld = addCases evalOpts (map (\(eid, f) -> (eid, f x)) cases) iworld
							   | stable
								 # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus tid allTonicInstances) EmptyContext iworld
								 = case mchild_bpr of
									 Ok (ReadingDone xs)
									   = snd (mapSt (\(_, {bpi_bpref}) iworld -> ((), markStable tid bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName iworld)) xs iworld)
									 _ = iworld
							   | otherwise = iworld
							 _ = iworld
		  # iworld       = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId tid iworld
		  = (tr, iworld)
		_ = eval event (updateAssignStatus evalOpts) taskTree iworld

  updRTMap tonicOpts nid childTaskId parentBPInst iworld
	# (newActiveNodes, iworld) = setActiveNodes tonicOpts parentBPInst childTaskId nid iworld
	# newActiveNodeMap         = 'DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems newActiveNodes)]
	# oldActiveNodes           = 'DM'.difference ('DM'.union parentBPInst.bpi_previouslyActive
															 ('DM'.fromList [(nid, tid) \\ (tid, nid) <- concatMap 'DIS'.elems ('DM'.elems parentBPInst.bpi_activeNodes)]))
												 newActiveNodeMap // This difference is required, because currently active nodes may up in the old set due to the iteration over parallel branches
	//#! iworld = trace_n ("newActiveNodes = " +++ toString (toJSON newActiveNodes) +++ " oldActiveNodes = " +++ toString (toJSON oldActiveNodes)) iworld
	# newParent   = { parentBPInst
					& bpi_activeNodes      = newActiveNodes
					, bpi_previouslyActive = oldActiveNodes}
	# (_, iworld) = 'DSDS'.write newParent (sdsFocus (parentBPInst.bpi_taskId, tonicOpts.currBlueprintModuleName, tonicOpts.currBlueprintFuncName) tonicInstances) EmptyContext iworld
	= iworld

  evalParallel :: BlueprintInstance (TaskResult a) TaskEvalOpts TaskId [(TaskId, TaskTree)] *IWorld
			   -> *IWorld | iTask a
  evalParallel pinst tr evalOpts childTaskId parallelChildren iworld
	# currActive = case 'DM'.get childTaskId pinst.bpi_activeNodes of
					 Just ns -> ns
					 _       -> 'DIS'.newMap
	# (childNodes, currActive, iworld) = foldr (registerTask pinst.bpi_taskId childTaskId) ([], currActive, iworld) (zip2 [0..] parallelChildren)
	# (tf_body, _, _) = updateNode nid (\x -> case x of
												e=:(TMApp _ _ _ _ [TMApp _ _ _ _ _ _ _ : _] _ _) -> e
												e=:(TMApp _ _ _ _ [TFApp _ "_Cons" _ _ : _] _ _) -> e // TODO This is probably insufficient. It will capture things like [t1:someOtherTasks], where we would like to expand someOtherTasks at runtime
												TMApp eid mtn mn tn _ pr ptr -> TMApp eid mtn mn tn [list2TExpr childNodes] pr ptr // FIXME This conflicts with another example (I forgot which one)
												e -> e
									   ) pinst.bpi_blueprint.tf_body
	# pinst  = { pinst
			   & bpi_blueprint = { pinst.bpi_blueprint & tf_body = tf_body}
			   , bpi_activeNodes = 'DM'.put childTaskId currActive pinst.bpi_activeNodes}
	# iworld = snd ('DSDS'.write pinst (sdsFocus (pinst.bpi_taskId, pinst.bpi_bpref.bpr_moduleName, pinst.bpi_bpref.bpr_taskName) tonicInstances) EmptyContext iworld)
	# iworld = storeTaskOutputViewer tr nid evalOpts.tonicOpts.currBlueprintTaskId childTaskId iworld
	= iworld
	where
	registerTask :: TaskId TaskId (Int, (TaskId, TaskTree)) *([TExpr], IntMap (TaskId, [Int]), *IWorld)
				 -> *([TExpr], IntMap (TaskId, [Int]), *IWorld)
	registerTask (TaskId parentInstanceNo parentTaskNo) (TaskId listInstanceNo listTaskNo) (n, (tid, _)) (acc, currActive, iworld)
	  # (mchild_bpr, iworld) = 'DSDS'.read (sdsFocus tid allTonicInstances) EmptyContext iworld
	  = case mchild_bpr of
		  (Ok (ReadingDone [(_, {bpi_bpref}) : _]))
			# newNodeId  = nid ++ [n]
			# childApp   = TMApp newNodeId Nothing bpi_bpref.bpr_moduleName bpi_bpref.bpr_taskName [] TNoPrio Nothing
			# currActive = 'DIS'.put n (tid, newNodeId) currActive
			= ([childApp:acc], currActive, iworld)
		  _ = (acc, currActive, iworld)

getNode :: !ExprId !TExpr -> Maybe TExpr
getNode eid expr=:(TVar eid` _ _)
  | eid == eid` = Just expr
getNode eid expr=:(TMApp eid` _ _ _ es _ _)
  | eid == eid` = Just expr
  | otherwise
	  = case [e \\ Just e <- map (getNode eid) es] of
		  [x : _] -> Just x
		  _       -> Nothing
getNode eid (TFApp _ _ es _)
  = case [e \\ Just e <- map (getNode eid) es] of
	  [x : _] -> Just x
	  _       -> Nothing
getNode eid (TLam _ e)
  = getNode eid e
getNode eid (TLet pats e) = getNode eid e
getNode eid (TIf _ c t e)
  = case [e \\ Just e <- [getNode eid t, getNode eid e]] of
	  [x : _] -> Just x
	  _       -> Nothing
getNode eid (TCase _ e pats)
  = case [e \\ Just e <- map (getNode eid o snd) pats] of
	  [x : _] -> Just x
	  _       -> Nothing
getNode _ e = Nothing

dump x = toString (toJSON x)

list2TExpr :: [TExpr] -> TExpr
list2TExpr []     = TFApp [] "_Nil"  [] TNoPrio
list2TExpr [x:xs] = TFApp [] "_Cons" [x, list2TExpr xs] TNoPrio

setActiveNodes :: !TonicOpts !BlueprintInstance !TaskId !ExprId !*IWorld -> *(!Map ListId (IntMap (TaskId, ExprId)), !*IWorld)
setActiveNodes tonicOpts {bpi_taskId = parentTaskId, bpi_activeNodes = parentActiveNodes} childTaskId nid iworld
  = case tonicOpts.inParallel of
	  Just currentListId
		| currentListId < parentTaskId = (defVal parentTaskId, iworld)
		| otherwise
			# taskListFilter      = { TaskListFilter | onlyIndex = Nothing, onlyTaskId = Nothing, onlySelf = False, includeValue = False, includeAttributes = False, includeProgress = False}
			# (mTaskList, iworld) = 'DSDS'.read (sdsFocus (currentListId, taskListFilter) taskInstanceParallelTaskList) EmptyContext iworld
			= case mTaskList of
			  (Ok (ReadingDone l)) = case getTaskState tonicOpts.callTrace l of
				Just parallelTaskState
				  # parentCallTrace = dropFirstInstances tonicOpts.callTrace
				  # parentCtx       = getParentContext parentTaskId parentCallTrace
				  # activeTasks     = 'DM'.del parentCtx parentActiveNodes
				  # activeTasks     = 'DM'.filterWithKey (\k _ -> k >= parentCtx) activeTasks
				  # activeSubTasks  = fromMaybe 'DIS'.newMap ('DM'.get currentListId activeTasks)
				  # activeSubTasks  = 'DIS'.put parallelTaskState.index (childTaskId, nid) activeSubTasks
				  = ('DM'.put currentListId activeSubTasks activeTasks, iworld)
				_ = (defVal currentListId, iworld)
			  _ = (defVal currentListId, iworld)
	  _ = (defVal parentTaskId, iworld)
  where
  defVal :: !TaskId -> Map ListId (IntMap (!TaskId, !ExprId))
  defVal tid = 'DM'.singleton tid ('DIS'.singleton 0 (childTaskId, nid))

  getTaskState :: !Calltrace ![ParallelTaskState] -> Maybe ParallelTaskState
  getTaskState trace ss
	 = case pop trace of
		 (Just ct, trace)
		   = case [ts \\ ts=:{ParallelTaskState | taskId} <- ss | ct == taskId] of
			   [ts : _] -> Just ts
			   _        -> getTaskState trace ss
		 _ = Nothing

  getParentContext :: !TaskId !Calltrace -> TaskId
  getParentContext parentTaskId trace
	= case pop trace of
		(Just (TaskId ino _), trace)
		  # parentTraceId = TaskId ino 0
		  | parentTraceId < parentTaskId = parentTaskId
		  | otherwise
			  = case findNext ino trace of
				  Just parentContextId
					| parentContextId < parentTaskId = parentTaskId
					| otherwise                      = parentContextId
				  _ = parentTaskId
		_ = parentTaskId
	where
	findNext :: !InstanceNo !Calltrace -> Maybe TaskId
	findNext ino trace
	  = case pop trace of
		  (Just tid=:(TaskId ino` _), trace)
			| ino <> ino` = Just tid
			| otherwise   = findNext ino trace
		  _ = Nothing

  dropFirstInstances :: !Calltrace -> Calltrace
  dropFirstInstances trace
	= case pop trace of
		(Just (TaskId ino _), trace) = dropFirstInstances` ino trace
		_                            = trace
	where
	dropFirstInstances` :: !InstanceNo !Calltrace -> Calltrace
	dropFirstInstances` ino trace
	  = case pop trace of
		  (Just (TaskId ino` _), trace)
			| ino == ino` = dropFirstInstances` ino trace
		  _ = trace

tonicExtWrapAppLam1 :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b -> m a)     -> b     -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam1 mn fn nid cases f = \x -> tonicWrapApp mn fn nid cases (f x)

tonicExtWrapAppLam2 :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c -> m a)   -> b c   -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam2 mn fn nid cases f = \x y -> tonicWrapApp mn fn nid cases (f x y)

tonicExtWrapAppLam3 :: !ModuleName !FuncName !ExprId [(ExprId, a -> Int)] !(b c d -> m a) -> b c d -> m a | TonicBlueprintPart m & iTask a
tonicExtWrapAppLam3 mn fn nid cases f = \x y z -> tonicWrapApp mn fn nid cases (f x y z)

anyTrue :: ![Bool] -> Bool
anyTrue [True : _] = True
anyTrue [_ : xs]   = anyTrue xs
anyTrue _          = False

replaceNode :: !Int !TExpr !TExpr -> TExpr
replaceNode varid newExpr expr=:(TVar eid _ varid`)
  | varid == varid` = case newExpr of
						TMApp _ mtn mn tn es p ptr -> TMApp eid mtn mn tn es p ptr
						TVar _ x vid               -> TVar eid x vid
						_                          -> newExpr
  | otherwise       = expr
replaceNode varid newExpr (TMApp eid` mtn mn tn es p ptr)
  #! es` = map (replaceNode varid newExpr) es
  = TMApp eid` mtn mn tn es` p ptr
replaceNode varid newExpr (TFApp eid fn es p)
  #! es` = map (replaceNode varid newExpr) es
  = TFApp eid fn es` p
replaceNode varid newExpr (TLam es e)
  #! e`  = replaceNode varid newExpr e
  #! es` = map (replaceNode varid newExpr) es
  = TLam es` e`
replaceNode varid newExpr (TSel e es)
  #! e`  = replaceNode varid newExpr e
  #! es` = map (replaceNode varid newExpr) es
  = TSel e` es`
replaceNode varid newExpr (TRecUpd vn e es)
  #! e`  = replaceNode varid newExpr e
  #! es` = map (replaceNode varid newExpr) es
  = TRecUpd vn e` es`
replaceNode varid newExpr (TLet pats e)
  #! e`   = replaceNode varid newExpr e
  #! pats = replaceNodePats varid newExpr pats
  = TLet pats e`
replaceNode varid newExpr (TIf cs c t e)
  #! c` = replaceNode varid newExpr c
  #! t` = replaceNode varid newExpr t
  #! e` = replaceNode varid newExpr e
  = TIf cs c` t` e`
replaceNode varid newExpr (TCase cs e pats)
  #! e`   = replaceNode varid newExpr e
  #! pats = replaceNodePats varid newExpr pats
  = TCase cs e` pats
replaceNode _ _ e = e

replaceNodePats _ _ [] = []
replaceNodePats varid newExpr [(pat, e) : xs]
  #! pat` = replaceNode varid newExpr pat
  #! e`   = replaceNode varid newExpr e
  #! pats = replaceNodePats varid newExpr xs
  = [(pat`, e`) : pats]

fst3 (x, _, _) = x
snd3 (_, x, _) = x
thrd (_, _, x) = x

getMVid :: ![Maybe a] -> Maybe a
getMVid xs = case [x \\ Just x <- xs] of
			   [x : _] -> Just x
			   _       -> Nothing

// TODO This can be made faster by using the ExprId's structure
updateNode :: !ExprId !(TExpr -> TExpr) !TExpr -> (!TExpr, !Bool, !Maybe (!Int, !TExpr))
updateNode eid f expr=:(TVar eid` _ varid)
  # expr` = f expr
  | eid == eid` = (expr`, True, Just (varid, expr`))
updateNode eid f expr=:(TMApp eid` mtn mn tn es p ptr)
  | eid == eid` = (f expr, True, Nothing)
  | otherwise
	  #! es` = map (updateNode eid f) es
	  = (TMApp eid` mtn mn tn (map fst3 es`) p ptr, anyTrue (map snd3 es`), getMVid (map thrd es`))
updateNode eid f (TFApp eid` fn es p)
  #! es` = map (updateNode eid f) es
  = (TFApp eid` fn (map fst3 es`) p, anyTrue (map snd3 es`), getMVid (map thrd es`))
updateNode eid f (TLam es e)
  #! (e`, eb, mvid) = updateNode eid f e
  #! es`            = map (updateNode eid f) es
  = (TLam (map fst3 es`) e`, anyTrue [eb : map snd3 es`], getMVid [mvid : (map thrd es`)])
updateNode eid f (TSel e es)
  #! (e`, eb, mvid) = updateNode eid f e
  #! es`            = map (updateNode eid f) es
  = (TSel e` (map fst3 es`), anyTrue [eb : map snd3 es`], getMVid [mvid : (map thrd es`)])
updateNode eid f (TRecUpd vn e es)
  #! (e`, eb, mvid) = updateNode eid f e
  #! es`            = map (updateNode eid f) es
  = (TRecUpd vn e` (map fst3 es`), anyTrue [eb : map snd3 es`], getMVid [mvid : (map thrd es`)])
updateNode eid f (TLet pats e)
  #! (e`, eb, mvid)   = updateNode eid f e
  #! (pats, b, mvids) = updatePats eid f pats
  = (TLet pats e`, b || eb, getMVid [mvid : mvids])
updateNode eid f (TIf cs c t e )
  #! (c`, cb, mvidc) = updateNode eid f c
  #! (t`, tb, mvidt) = updateNode eid f t
  #! (e`, eb, mvide) = updateNode eid f e
  = (TIf cs c` t` e`, cb || tb || eb, getMVid [mvidc, mvidt, mvide])
updateNode eid f (TCase cs e pats)
  #! (e`, eb, mvid)   = updateNode eid f e
  #! (pats, b, mvids) = updatePats eid f pats
  = (TCase cs e` pats, b || eb, getMVid [mvid : mvids])
updateNode _ _ e = (e, False, Nothing)

updatePats _ _ [] = ([], False, [])
updatePats eid f [(pat, e) : xs]
  #! (pat`, pb, mvid1) = updateNode eid f pat
  #! (e`, eb, mvid2)   = updateNode eid f e
  #! (pats, b, mvids)  = updatePats eid f xs
  = ([(pat`, e`) : pats], pb || eb || b, [mvid1 : mvid2 : mvids])


