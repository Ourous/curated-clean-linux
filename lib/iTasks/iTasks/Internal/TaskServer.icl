implementation module iTasks.Internal.TaskServer

import StdFile, StdBool, StdInt, StdClass, StdList, StdMisc, StdArray, StdTuple, StdOrdList
import Data.Maybe, Data.Functor, Data.Func, Data.Error, System.Time, Text, Data.Tuple
from StdFunc import seq, o
from Data.Map import :: Map (..)
import System.CommandLine
import qualified Data.List as DL
import qualified Data.Map as DM
import qualified iTasks.Internal.SDS as SDS
import TCPChannelClass, TCPChannels, TCPEvent, TCPStringChannels, TCPDef, tcp

import iTasks.Engine, iTasks.Internal.IWorld, iTasks.Internal.TaskEval, iTasks.Internal.TaskStore
import iTasks.Internal.IWorld
import iTasks.Internal.Task
import iTasks.Internal.TaskEval
import iTasks.Internal.Util
from iTasks.Internal.TaskStore import queueRefresh
import iTasks.WF.Tasks.IO
import iTasks.SDS.Combinators.Common

from Data.Foldable import maximum
from Data.List import instance Foldable []

//Helper type that holds the mainloop instances during a select call
//in these mainloop instances the unique listeners and read channels
//have been temporarily removed.
:: *IOTaskInstanceDuringSelect
	= ListenerInstanceDS !ListenerInstanceOpts
	| ConnectionInstanceDS !ConnectionInstanceOpts !*TCP_SChannel
	| BackgroundInstanceDS !BackgroundInstanceOpts !BackgroundTask

serve :: ![StartupTask] ![(!Int,!ConnectionTask)] ![BackgroundTask] (*IWorld -> (!Maybe Timeout,!*IWorld)) *IWorld -> *IWorld
serve its cts bts determineTimeout iworld
	= loop determineTimeout (init its cts bts iworld)

init :: ![StartupTask] ![(!Int,!ConnectionTask)] ![BackgroundTask] !*IWorld -> *IWorld
init its cts bts iworld
	// Check if the initial tasks have been added already
	# iworld = createInitialInstances its iworld
	// All persistent task instances should receive a reset event to continue their work
	# iworld=:{IWorld|ioTasks,world} = queueAll iworld
	# (listeners,world) = connectAll cts world
	# ioStates = 'DM'.fromList [(TaskId 0 0, IOActive 'DM'.newMap)]
	= {iworld & ioTasks = {done=[],todo=listeners ++ map (BackgroundInstance {bgInstId=0}) bts}, ioStates = ioStates,  world = world}
where
	createInitialInstances :: [StartupTask] !*IWorld -> *IWorld
	createInitialInstances [] iworld = iworld
	createInitialInstances [{StartupTask|task=TaskWrapper task,attributes}:ts] iworld
		= case createStartupTaskInstance task attributes iworld of
			(Ok _,iworld) = createInitialInstances ts iworld
			(Error (_,e),iworld) = abort e

	queueAll :: !*IWorld -> *IWorld
	queueAll iworld
		# (mbIndex,iworld) = read (sdsFocus defaultValue filteredInstanceIndex) EmptyContext iworld
		= case mbIndex of
			Ok (ReadingDone index)    = foldl (\w (instanceNo,_,_,_) -> queueEvent instanceNo ResetEvent w) iworld index
			_           = iworld

	connectAll :: ![(!Int,!ConnectionTask)] !*World -> *(![*IOTaskInstance],!*World)
	connectAll [] world = ([],world)
	connectAll [(port,ct):cts] world
		# (l,world) = connect port ct world
		# (ls,world) = connectAll cts world
		= ([l:ls],world)

	connect :: !Int !ConnectionTask !*World -> *(!*IOTaskInstance,!*World)
	connect port ct world
		# (success, mbListener, world) = openTCP_Listener port world
		| not success = abort ("Error: port "+++ toString port +++ " already in use.\n")
		# opts = {ListenerInstanceOpts|taskId=TaskId 0 0, port=port, connectionTask=ct, removeOnClose = True}
		= (ListenerInstance opts (fromJust mbListener),world)

loop :: !(*IWorld -> (!Maybe Timeout,!*IWorld)) !*IWorld -> *IWorld
loop determineTimeout iworld=:{ioTasks,sdsNotifyRequests}
	// Also put all done tasks at the end of the todo list, as the previous event handling may have yielded new tasks.
	# (mbTimeout,iworld=:{IWorld|ioTasks={todo},world}) = determineTimeout {iworld & ioTasks = {done=[], todo = ioTasks.todo ++ (reverse ioTasks.done)}}
	//Check which mainloop tasks have data available
	# (todo,chList,world) = select mbTimeout todo world
	//Process the select result
	# iworld =:{shutdown,ioTasks={done}} = process 0 chList {iworld & ioTasks = {done=[],todo=todo}, world = world}
	//Move everything from the done list  back to the todo list
	# iworld = {iworld & ioTasks={todo = reverse done,done=[]}}
	//Everything needs to be re-evaluated
	= case shutdown of
		(Just exitCode) = halt exitCode iworld
		_               = loop determineTimeout iworld

select :: (Maybe Timeout) *[IOTaskInstance] *World -> (!*[IOTaskInstance],![(Int,SelectResult)],!*World)
select mbTimeout mlInstances world
	# (empty,listeners,rChannels,mlInstances) = toSelectSet mlInstances
	| empty //selectChannel_MT aborts if it is called with an empty list, so we must make sure that never happens
		# (mlInstances, chList) = fromSelectSet listeners rChannels mlInstances []
		= (mlInstances, chList, world)
	| otherwise
		# (chList,(TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)),_,world)
		   = selectChannel_MT mbTimeout (TCP_Pair (TCP_Listeners listeners) (TCP_RChannels rChannels)) TCP_Void world
		# (mlInstances, chList)
		   = fromSelectSet listeners rChannels mlInstances chList
		= (mlInstances, chList, world)

toSelectSet :: !*[IOTaskInstance] -> *(!Bool,!*[*TCP_Listener],!*[*TCP_RChannel],!*[*IOTaskInstanceDuringSelect])
toSelectSet [] = (True,[],[],[])
toSelectSet [i:is]
	# (e,ls,rs,is) = toSelectSet is
	= case i of
		ListenerInstance opts l = (False,[l:ls],rs,[ListenerInstanceDS opts:is])
		ConnectionInstance opts {rChannel,sChannel} = (False,ls,[rChannel:rs],[ConnectionInstanceDS opts sChannel:is])
		BackgroundInstance opts bt = (e,ls,rs,[BackgroundInstanceDS opts bt:is])

/* Restore the list of main loop instances.
	In the same pass also update the indices in the select result to match the
	correct indices of the main loop instance list.
*/
fromSelectSet :: !*[*TCP_Listener] !*[*TCP_RChannel] !*[*IOTaskInstanceDuringSelect] ![(!Int,!SelectResult)] -> *(![*IOTaskInstance],![(!Int,!SelectResult)])
fromSelectSet ls rs is chList
	# (numListeners,ls) = ulength ls
	# sortedChList      = sortBy (\(x,_) (y,_) -> (x < y)) chList //The single-pass algorithm expects a sorted select result
	= fromSelectSet` 0 numListeners 0 0 ls rs sortedChList is
where
	fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls rs _ [] = ([],[])
	//Listeners
	fromSelectSet` i numListeners numSeenListeners numSeenReceivers [l:ls] rs [] [ListenerInstanceDS opts:is]
		# (is,_) = fromSelectSet` (i+1) numListeners (numSeenListeners+1) numSeenReceivers ls rs [] is
		= ([ListenerInstance opts l:is],[])
	fromSelectSet` i numListeners numSeenListeners numSeenReceivers [l:ls] rs [(c,what):ch] [ListenerInstanceDS opts:is]
		| c == numSeenListeners //Check select result
			# (is,ch) = fromSelectSet` (i+1) numListeners (numSeenListeners+1) numSeenReceivers ls rs ch is
			= ([ListenerInstance opts l:is],[(i,what):ch])
		| otherwise
			# (is,ch) = fromSelectSet` (i+1) numListeners (numSeenListeners+1) numSeenReceivers ls rs [(c,what):ch] is
			= ([ListenerInstance opts l:is],ch)
	//Receivers
	fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls [rChannel:rs] [] [ConnectionInstanceDS opts sChannel:is]
		# (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners (numSeenReceivers+1) ls rs [] is
		= ([ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}:is],[])
	fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls [rChannel:rs] [(c,what):ch] [ConnectionInstanceDS opts sChannel:is]
		| c == numListeners + numSeenReceivers
			# (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners (numSeenReceivers+1) ls rs ch is
			= ([ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}:is],[(i,what):ch])
		| otherwise
			# (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners (numSeenReceivers+1) ls rs [(c,what):ch] is
			= ([ConnectionInstance opts {rChannel=rChannel,sChannel=sChannel}:is],ch)
	//Background tasks
	fromSelectSet` i numListeners numSeenListeners numSeenReceivers ls rs ch [BackgroundInstanceDS opts bt:is]
		# (is,ch) = fromSelectSet` (i+1) numListeners numSeenListeners numSeenReceivers ls rs ch is
		= ([BackgroundInstance opts bt:is],ch)

	ulength [] = (0,[])
	ulength [x:xs]
		# (n,xs) = ulength xs
		= (n + 1,[x:xs])

//TODO: Use share notification to trigger task re-evaluation based on io events
process :: !Int [(!Int,!SelectResult)] !*IWorld -> !*IWorld
process i chList iworld=:{ioTasks={done,todo=[]}} = iworld
process i chList iworld=:{ioTasks={done,todo=[ListenerInstance lopts listener:todo]},ioStates,world}
	# taskId=:(TaskId instanceNo _) = lopts.ListenerInstanceOpts.taskId
	= case 'DM'.get lopts.ListenerInstanceOpts.taskId ioStates of
		//Active listener:
		Just (IOActive conStates)
			# (mbSelect,chList) = checkSelect i chList
			| mbSelect =:(Just _)
				# (tReport, mbNewConn, listener, world)   = receive_MT (Just 0) listener world
				| tReport == TR_Success
					# (ip,{rChannel,sChannel}) = fromJust mbNewConn
					# (ConnectionTask handlers sds) = lopts.ListenerInstanceOpts.connectionTask
					# (mbr,iworld) = 'SDS'.read sds EmptyContext {iworld & ioTasks={done=done,todo=todo},world=world}
					| mbr =:(Error _)
						# iworld=:{ioTasks={done,todo},world} = if (instanceNo > 0) (queueRefresh [(taskId,"IO Exception for instance "<+++instanceNo)] iworld) iworld
						# ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (snd (fromError mbr))) ioStates
						# world = closeRChannel listener world
						= process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
					# (mbConState,mbw,out,close,iworld) = handlers.ConnectionHandlersIWorld.onConnect (nextConnId ('DM'.keys conStates)) (toString ip) (directResult (fromOk mbr)) iworld
					# iworld = if (instanceNo > 0) (queueRefresh [(taskId,"New TCP connection for instance "<+++taskId)] iworld) iworld
					# (mbSdsErr, iworld=:{ioTasks={done,todo},world}) = writeShareIfNeeded sds mbw iworld
					| mbConState =:(Error _)
						# ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (fromError mbConState)) ioStates
						= process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, ioStates = ioStates, world=world}
					| isError mbSdsErr
						# ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOException (snd (fromError mbSdsErr))) ioStates
						= process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, ioStates = ioStates, world=world}
					# connId = nextConnId ('DM'.keys conStates)
					# conStates = 'DM'.put connId (fromOk mbConState,close) conStates
					# (sChannel,world) = case out of
						[]          = (sChannel,world)
						data        = foldl (\(s,w) d -> send (toByteSeq d) s w) (sChannel,world) data
					| close
					//Close the connection immediately
						# world = closeRChannel rChannel world
						# world = closeChannel sChannel world
						//Remove the connection state if configured in the connection listener options
						# conStates = if lopts.ListenerInstanceOpts.removeOnClose
							('DM'.del connId conStates)
							conStates
						# ioStates  = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOActive conStates) ioStates
						= process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, ioStates = ioStates, world=world}
					| otherwise
					//Persist the connection
						# copts = {ConnectionInstanceOpts|taskId = lopts.ListenerInstanceOpts.taskId
								  ,connectionId = connId
								  ,remoteHost = ip, connectionTask = lopts.ListenerInstanceOpts.connectionTask
								  ,removeOnClose = lopts.ListenerInstanceOpts.removeOnClose}
						# todo = todo ++ [ConnectionInstance copts {rChannel=rChannel,sChannel=sChannel}]
						= process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}
							, ioStates = 'DM'.put lopts.ListenerInstanceOpts.taskId (IOActive conStates) ioStates
							, world=world}
				//We did not properly accept a connection
				| otherwise
					= process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, world=world}
			//Nothing to do
			| otherwise
				= process (i+1) chList {iworld & ioTasks={done=[ListenerInstance lopts listener:done],todo=todo}, world=world}
		//Destroyed listener:
		Just (IODestroyed conStates)
			# world = closeRChannel listener world
			//If there are no connections belonging to this listener we can clean up, if there are the last connection will cleanup
			# ioStates = if ('DM'.mapSize conStates == 0) ('DM'.del lopts.ListenerInstanceOpts.taskId ioStates) ioStates
			= process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}
		//There was an exception or the state has already been removed
		_
			# world = closeRChannel listener world
			= process (i+1) chList {iworld & ioTasks={done=done,todo=todo}, ioStates = ioStates, world=world}

process i chList iworld=:{ioTasks={done, todo=[ConnectionInstance opts duplexChannel:todo]}}
	# iworld = {iworld & ioTasks = {done = done, todo = todo}}
	# iworld = processIOTask
		i chList opts.ConnectionInstanceOpts.taskId opts.ConnectionInstanceOpts.connectionId
		opts.ConnectionInstanceOpts.removeOnClose sds tcpConnectionIOOps
		(\_ -> handlers.ConnectionHandlersIWorld.onDisconnect) handlers.ConnectionHandlersIWorld.onData
		handlers.ConnectionHandlersIWorld.onShareChange handlers.ConnectionHandlersIWorld.onTick (ConnectionInstance opts) duplexChannel iworld
	= process (i+1) chList iworld
where
	(ConnectionTask handlers sds) = opts.ConnectionInstanceOpts.connectionTask


process i chList iworld=:{ioTasks={done,todo=[BackgroundInstance opts bt=:(BackgroundTask eval):todo]}}
	# (mbe,iworld=:{ioTasks={done,todo}}) = eval {iworld & ioTasks = {done=done,todo=todo}}
	| mbe =: (Error _) = abort (snd (fromError mbe)) //TODO Handle the error without an abort
	= process (i+1) chList {iworld & ioTasks={done=[BackgroundInstance opts bt:done],todo=todo}}
process i chList iworld=:{ioTasks={done,todo=[t:todo]}}
	= (process (i+1) chList {iworld & ioTasks={done=[t:done],todo=todo}})

// Definitions of IO tasks (tcp connections)

:: IOTaskOperations ioChannels readData closeInfo =
	{ readData  :: !(Int [(Int, SelectResult)] *(!ioChannels, !*IWorld) -> *(!IOData readData closeInfo, !ioChannels, !*IWorld))
	, writeData :: !(String                    *(!ioChannels, !*IWorld) -> *(!ioChannels, !*IWorld))
	, closeIO   :: !(                          *(!ioChannels, !*IWorld) -> *IWorld)
	}
:: IOData data closeInfo = IODClosed closeInfo
						 | IODNoData
						 | IODData !data & TC data

tcpConnectionIOOps :: IOTaskOperations *TCP_DuplexChannel String ()
tcpConnectionIOOps = {readData = readData, writeData = writeData, closeIO = closeIO}
where
	readData :: !Int
				![(Int, SelectResult)]
				!(!*TCP_DuplexChannel, !*IWorld)
			 -> (!IOData String (), !*TCP_DuplexChannel, !*IWorld)
	readData i chList (channel, iworld)
		# (mbSelect, chList) = checkSelect i chList
		| mbSelect =: (Just SR_Disconnected) || mbSelect=:(Just SR_EOM)
			= (IODClosed (), channel, iworld)
		| mbSelect =: (Just SR_Available)
			# (data, rChannel, world) = receive channel.rChannel iworld.world
			= (IODData (toString data), {channel & rChannel = rChannel}, {iworld & world = world})
		| otherwise
			= (IODNoData, channel, iworld)

	writeData :: !String !(!*TCP_DuplexChannel, !*IWorld) -> (!*TCP_DuplexChannel, !*IWorld)
	writeData data (channel, iworld)
		# (sChannel, world) = send (toByteSeq data) channel.sChannel iworld.world
		= ({channel & sChannel = sChannel}, {iworld & world = world})

	closeIO :: !(!*TCP_DuplexChannel, !*IWorld) -> *IWorld
	closeIO ({rChannel, sChannel}, iworld=:{world})
		# world = closeRChannel rChannel world
		# world = closeChannel  sChannel world
		= {iworld & world = world}

processIOTask :: !Int
				 ![(Int, SelectResult)]
				 !TaskId
				 !Int
				 !Bool
				 !(SimpleSDSLens Dynamic)
				 !(IOTaskOperations .ioChannels readData closeInfo)
				 !(closeInfo Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, !*IWorld))
				 !(readData Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
				 !(Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
				 !(Dynamic Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
				 !(.ioChannels -> *IOTaskInstance)
				 !.ioChannels
				 !*IWorld
			  -> *IWorld
processIOTask i chList taskId connectionId removeOnClose sds ioOps onCloseHandler onDataHandler
			  onShareChangeHandler onTickHandler mkIOTaskInstance ioChannels iworld=:{ioStates}
	= case 'DM'.get taskId ioStates of
		Just (IOActive taskStates)
			# (TaskId instanceNo _) = taskId
			// get task state
			# mbTaskState = 'DM'.get connectionId taskStates
			| isNothing mbTaskState
				# iworld   = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
				# ioStates = 'DM'.put taskId (IOException "Missing IO task state for task ") ioStates
				= ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
			# taskState = fst (fromJust mbTaskState)

			// *** onTick handler ***
			// read sds
			# (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds EmptyContext iworld
			| mbr =: (Error _) = sdsException mbr instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			# r = directResult (fromOk mbr)
			// call handler
			# (mbTaskState, mbw, out, close, iworld) = onTickHandler taskState r iworld
			# (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
			// write data
			# (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
			| mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			| isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			| close = closeConnection taskStates ioStates ioOps.closeIO (ioChannels, iworld)

			// *** onShareChange handler ***
			// read sds
			# (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds EmptyContext iworld
			| mbr =: (Error _) = sdsException mbr instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			# r = directResult (fromOk mbr)
			// call handler
			# (mbTaskState, mbw, out, close, iworld) = onShareChangeHandler taskState r iworld
			# (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
			| mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			| isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			# ioStates = 'DM'.put taskId (IOActive ('DM'.put connectionId (fromOk mbTaskState, close) taskStates)) ioStates
			// write data
			# (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
			| mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			| isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			| close = closeConnection taskStates ioStates ioOps.closeIO (ioChannels, iworld)

			// ** onData handler ***
			// read sds
			# (mbr,iworld=:{ioTasks={done,todo},world}) = 'SDS'.read sds EmptyContext iworld
			| mbr =: (Error _) = sdsException mbr instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
			# r = directResult (fromOk mbr)
			# taskState = fromOk mbTaskState
			// try to read data
			# (mbData, ioChannels, iworld) = ioOps.readData i chList (ioChannels, iworld)
			= case mbData of
				IODClosed closeInfo
					# (mbTaskState, mbw, iworld) = onCloseHandler closeInfo taskState r iworld
					# ioStates = case mbTaskState of
						Ok state
							= 'DM'.put taskId (IOActive ('DM'.put connectionId (state, True) taskStates)) ioStates
						Error e
							= 'DM'.put taskId (IOException e) ioStates
					# (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
					| isError mbSdsErr
						# iworld = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
						# ioStates = 'DM'.put taskId (IOException (snd (fromError mbSdsErr))) ioStates
						= ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
					# iworld = if (instanceNo > 0) (queueRefresh [(taskId, "IO closed for " <+++ instanceNo)] iworld) iworld
					= ioOps.closeIO (ioChannels, {iworld & ioStates = ioStates})
				IODNoData
					// persist connection
					# {done, todo} = iworld.ioTasks
					= {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance ioChannels : done], todo = todo}}
				IODData data
					# (mbTaskState, mbw, out, close, iworld) = onDataHandler data taskState r iworld
					# iworld = if (instanceNo > 0) (queueRefresh [(taskId, "New data for "<+++ instanceNo)] iworld) iworld
					# (mbSdsErr, iworld) = writeShareIfNeeded sds mbw iworld
					// write data
					# (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
					| mbTaskState =: (Error _) = taskStateException mbTaskState instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
					| isError mbSdsErr         = sdsException       mbSdsErr    instanceNo ioStates ioOps.closeIO (ioChannels, iworld)
					# (connectionMap, iworld) = appFst (\map. 'DM'.put connectionId (fromOk mbTaskState, close) map) (connMapForTask taskId iworld)
					# ioStates = 'DM'.put taskId (IOActive connectionMap) ioStates
					| close = closeConnection connectionMap ioStates ioOps.closeIO (ioChannels, iworld)
					| otherwise
						// persist connection
						# {done, todo} = iworld.ioTasks
						= {iworld & ioStates = ioStates, ioTasks = {done = [mkIOTaskInstance ioChannels : done], todo = todo}}
		Just (IODestroyed taskStates)
			# iworld = ioOps.closeIO (ioChannels, iworld)
			//Remove the state for this connection
			//If this is the last connection for this task, we can clean up.
			# ioStates = if ('DM'.mapSize taskStates == 0) ('DM'.del taskId ioStates) ioStates
			= {iworld & ioStates = ioStates}
		_ = ioOps.closeIO (ioChannels, iworld)
where
	connMapForTask :: !TaskId !*IWorld -> (!Map ConnectionId (!Dynamic,!Bool), !*IWorld)
	connMapForTask taskId iworld=:{ioStates} = case 'DM'.get taskId ioStates of
		(Just (IOActive connectionMap)) = (connectionMap, iworld)

	taskStateException :: (MaybeError String Dynamic)
						  InstanceNo
						  (Map TaskId IOState)
						  (*(!.ioChannels, !*IWorld) -> *IWorld)
						  *(!.ioChannels, !*IWorld)
					   -> *IWorld
	taskStateException mbTaskState instanceNo ioStates closeIO (ioChannels, iworld)
		# iworld = iShow ["Exception in TaskServer: taskStateException: " +++ fromError mbTaskState] iworld
		# iworld = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
		# ioStates = 'DM'.put taskId (IOException (fromError mbTaskState)) ioStates
		= closeIO (ioChannels, {iworld & ioStates = ioStates})

	sdsException :: (MaybeError TaskException a)
					InstanceNo
					(Map TaskId IOState)
					(*(!.ioChannels, !*IWorld) -> *IWorld)
					*(!.ioChannels, !*IWorld)
					-> *IWorld
	sdsException mbSdsErr instanceNo ioStates closeIO (ioChannels, iworld)
		# iworld = iShow ["Exception in TaskServer: sdsException: " +++ snd (fromError mbSdsErr)] iworld
		# iworld = if (instanceNo > 0) (queueRefresh [(taskId, "Exception for " <+++ instanceNo)] iworld) iworld
		# ioStates = 'DM'.put taskId (IOException (snd (fromError mbSdsErr))) ioStates
		= closeIO (ioChannels, {iworld & ioStates = ioStates})

	closeConnection :: (Map ConnectionId (Dynamic,Bool))
					   (Map TaskId IOState)
					   (*(!.ioChannels, !*IWorld) -> *IWorld)
					   *(!.ioChannels, !*IWorld)
					-> *IWorld
	closeConnection taskStates ioStates closeIO (ioChannels, iworld)
		//Remove the connection state if configured in the connection listener options
		# taskStates = if removeOnClose
			('DM'.del connectionId taskStates)
			taskStates
		# ioStates = 'DM'.put taskId (IOActive taskStates) ioStates
		= closeIO (ioChannels, {iworld & ioStates = ioStates})

writeShareIfNeeded :: !(sds () r w) !(Maybe w) !*IWorld -> (!MaybeError TaskException (), !*IWorld) | TC r & TC w & Writeable sds
writeShareIfNeeded sds Nothing iworld  = (Ok (), iworld)
writeShareIfNeeded sds (Just w) iworld = case 'SDS'.write w sds EmptyContext iworld of
	(Error e, iworld) = (Error e, iworld)
	(Ok WritingDone, iworld) = (Ok (), iworld)

addListener :: !TaskId !Int !Bool !ConnectionTask !*IWorld -> (!MaybeError TaskException (),!*IWorld)
addListener taskId port removeOnClose connectionTask iworld=:{ioTasks={todo,done}, ioStates, world}
	//Open listener
	# (success, mbListener, world) = openTCP_Listener port world
	| not success
		= (Error (exception ("Error: port "+++ toString port +++ " already in use.")), {iworld & ioTasks = {done=done,todo=todo},world = world})
	# opts = {ListenerInstanceOpts|taskId = taskId, port = port, connectionTask= connectionTask, removeOnClose = removeOnClose}
	# todo = todo ++ [ListenerInstance opts (fromJust mbListener)]
	# ioStates = 'DM'.put taskId (IOActive 'DM'.newMap) ioStates
	= (Ok (),{iworld & ioTasks = {done=done,todo=todo}, ioStates = ioStates, world = world})

addConnection :: !TaskId !String !Int !ConnectionTask !*IWorld -> (!MaybeError TaskException (ConnectionId, Dynamic),!*IWorld)
addConnection taskId host port connectionTask=:(ConnectionTask handlers sds) iworld
	= addIOTask taskId sds init tcpConnectionIOOps onInitHandler mkIOTaskInstance iworld
where
	init :: !*IWorld -> (!MaybeErrorString (!IPAddress, !*TCP_DuplexChannel), !*IWorld)
	init iworld
		# (mbIP, world) = lookupIPAddress host iworld.world
		= case mbIP of
			Nothing = (Error ("Failed to connect to host " +++ host +++ ": lookup failed"), {iworld & world = world})
			Just ip
				# (tReport, mbConn, world) = connectTCP_MT Nothing (fromJust mbIP,port) world
				= case mbConn of
					Nothing = (Error ("Failed to connect to host " +++ host +++ ":" +++ toString port), {iworld & world = world})
					Just channel = (Ok (ip, channel), {iworld & world = world})

	onInitHandler :: ConnectionId !IPAddress !Dynamic !*IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld)
	onInitHandler connId ip r iworld = handlers.ConnectionHandlersIWorld.onConnect connId (toString ip) r iworld

	mkIOTaskInstance :: ConnectionId !IPAddress !*TCP_DuplexChannel -> *IOTaskInstance
	mkIOTaskInstance connectionId ip channel
		# opts = {ConnectionInstanceOpts|taskId = taskId, connectionId = connectionId, remoteHost = ip, connectionTask = connectionTask, removeOnClose = False}
		= ConnectionInstance opts channel

addIOTask :: !TaskId
			 !(Shared sds Dynamic)
			 !(*IWorld -> (!MaybeErrorString (!initInfo, !.ioChannels), !*IWorld))
			 !(IOTaskOperations .ioChannels readData closeInfo)
			 !(ConnectionId initInfo Dynamic *IWorld -> (!MaybeErrorString Dynamic, !Maybe Dynamic, ![String], !Bool, !*IWorld))
			 !(ConnectionId initInfo .ioChannels -> *IOTaskInstance)
			 !*IWorld
		  -> (!MaybeError TaskException (ConnectionId, Dynamic), !*IWorld) | Readable sds
addIOTask taskId sds init ioOps onInitHandler mkIOTaskInstance iworld=:{ioStates}
	# (mbInitRes, iworld) = init iworld
	= case mbInitRes of
		Error e = (Error (exception e), iworld)
		Ok (initInfo, ioChannels)
			// Read share
			# (mbr, iworld) = 'SDS'.read sds EmptyContext iworld
			| isError mbr = (liftError mbr, iworld)
			# newConnectionId = connId taskId ioStates
			// Evaluate onInit handler
			# (mbl, mbw, out, close, iworld) = onInitHandler newConnectionId initInfo (directResult (fromOk mbr)) iworld
			// Check initialization of local state
			= case mbl of
				Error e = (Error (exception e), iworld)
				Ok l
					// write output
					# (ioChannels, iworld) = seq [ioOps.writeData o \\ o <- out] (ioChannels, iworld)
					//Close or add to queue
					| close
						# iworld = ioOps.closeIO (ioChannels, iworld)
						= (Ok (0, dynamic l), iworld)
					| otherwise
						# ioStates = iworld.ioStates

						# (connectionId, connectionMap) = case 'DM'.get taskId ioStates of
							Nothing                             = (0, IOActive ('DM'.fromList [(0,(l, False))]))
							(Just (IOActive connectionMap))     = (newConnectionId, IOActive ('DM'.put newConnectionId (l, False) connectionMap))
						# ioStates = 'DM'.put taskId connectionMap ioStates
						# iworld = {iworld & ioStates = ioStates}
						# {done, todo} = iworld.ioTasks
						# iworld = {iworld & ioTasks = {done = [mkIOTaskInstance connectionId initInfo ioChannels : done], todo = todo}}
						= (Ok (connectionId, l), iworld)
where
	connId taskId ioStates = case 'DM'.get taskId ioStates of
		Nothing = 0
		(Just (IOActive connectionMap)) = maxList ('DM'.keys connectionMap)
		(Just (IOException s)) = 0
		(Just (IODestroyed connectionMap)) = maxList ('DM'.keys connectionMap)

	maxList [] = 0
	maxList list = inc (maximum list)

//Dynamically add a background task
addBackgroundTask :: !BackgroundTask !*IWorld -> (!MaybeError TaskException BackgroundTaskId,!*IWorld)
addBackgroundTask bt iworld=:{ioTasks={done,todo}}
# (todo, i) = appSnd (\is->1 + maxList is) (unzip (map transform todo))
# todo = todo ++ [BackgroundInstance {BackgroundInstanceOpts|bgInstId=i} bt]
= (Ok i, {iworld & ioTasks={done=done, todo=todo}})
	where
		transform a=:(BackgroundInstance {bgInstId} _) = (a, bgInstId)
		transform a = (a, 1)

//Dynamically remove a background task
removeBackgroundTask :: !BackgroundTaskId !*IWorld -> (!MaybeError TaskException (),!*IWorld)
removeBackgroundTask btid iworld=:{ioTasks={done,todo}}
//We filter the tasks and use the boolean state to hold whether a task was dropped
# (r, todo) = foldr (\e (b, l)->let (b`, e`)=drop e in (b` || b, if b` l [e`:l])) (False, []) todo
# iworld = {iworld & ioTasks={done=done, todo=todo}}
| not r = (Error (exception "No backgroundtask with that id"), iworld)
= (Ok (), iworld)
	where
		drop a=:(BackgroundInstance {bgInstId} _) = (bgInstId == btid, a)
		drop a = (False, a)

checkSelect :: !Int ![(!Int,!SelectResult)] -> (!Maybe SelectResult,![(!Int,!SelectResult)])
checkSelect i chList =:[(who,what):ws] | (i == who) = (Just what,ws)
checkSelect i chList = (Nothing,chList)

halt :: !Int !*IWorld -> *IWorld
halt exitCode iworld=:{ioTasks={todo=[],done},world}
	# world = setReturnCode exitCode world
	= {IWorld|iworld & world = world}
halt exitCode iworld=:{ioTasks={todo=[ListenerInstance _ listener:todo],done},world}
	# world = closeRChannel listener world
	= halt exitCode {iworld & ioTasks = {todo=todo,done=done}}
halt exitCode iworld=:{ioTasks={todo=[ConnectionInstance _ {rChannel,sChannel}:todo],done},world}
	# world = closeRChannel rChannel world
	# world = closeChannel sChannel world
	= halt exitCode {iworld & ioTasks = {todo=todo,done=done}}
halt exitCode iworld=:{ioTasks={todo=[BackgroundInstance _ _ :todo],done},world}
	= halt exitCode {iworld & ioTasks= {todo=todo,done=done}}

nextConnId :: [ConnectionId] -> ConnectionId
nextConnId [] = 0
nextConnId m = inc (maximum m)

ioStateString :: !IOStates -> String
ioStateString ioStates
# list =  'DM'.toList ioStates
# l = map (appFst toString) list
# l = map (appSnd cMapString) l
= concat (map (\(taskIdS, connectionsS). taskIdS +++ ": " +++ connectionsS) l)
where
	cMapString (IOActive mapje) = concat (map ((\s. s +++ " ") o toString o fst) ('DM'.toList mapje))
	cMapString (IOException str) = "Exception: " +++ str
	cMapString _ = "Destroyed"
