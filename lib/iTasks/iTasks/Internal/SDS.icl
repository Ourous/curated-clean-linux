implementation module iTasks.Internal.SDS

from StdFunc import const
import StdString, StdTuple, StdMisc, StdList, StdBool
from Data.Map import :: Map
import qualified Data.Map as DM
import Data.Error, Data.Func, Data.Tuple, System.OS, System.Time, Text, Text.GenJSON
import qualified Data.Set as Set
import iTasks.Engine
import iTasks.Internal.IWorld
import iTasks.Internal.Task, iTasks.Internal.TaskStore, iTasks.Internal.TaskEval

createReadWriteSDS ::
	!String
	!String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createReadWriteSDS ns id read write
	= createSDS ns id read write

createReadOnlySDS ::
	!(p *IWorld -> *(!r, !*IWorld))
	->
	ROShared p r
createReadOnlySDS read
	= createReadOnlySDSError (\p env -> appFst Ok (read p env))
	
createReadOnlySDSError ::
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	->
	ROShared p r
createReadOnlySDSError read
	= createSDS "readonly" "readonly" read (\_ _ env -> (Ok (const (const True)), env))

createSDS ::
	!String
    !String
	!(p *IWorld -> *(!MaybeError TaskException r, !*IWorld))
	!(p w *IWorld -> *(!MaybeError TaskException (SDSNotifyPred p), !*IWorld))
	->
	RWShared p r w
createSDS ns id read write = SDSSource
	{ SDSSource
	| name = ns +++ ":" +++ id
    , read = read
	, write = write
	}

//Construct the identity of an sds
sdsIdentity :: !(RWShared p r w) -> SDSIdentity
sdsIdentity s = concat (sdsIdentity` s [])
where
	sdsIdentity` :: !(RWShared p r w) [String] -> [String]
	sdsIdentity` (SDSSource {SDSSource|name}) acc = ["$", name, "$":acc]
	sdsIdentity` (SDSLens sds {SDSLens|name}) acc = sdsIdentity` sds ["/[", name, "]":acc]
	sdsIdentity` (SDSSelect sds1 sds2 {SDSSelect|name}) acc
		= ["{", name:sdsIdentity` sds1 [",":sdsIdentity` sds2 ["}":acc]]]
	sdsIdentity` (SDSParallel sds1 sds2 {SDSParallel|name}) acc
		= ["|",name:sdsIdentity` sds1 [",":sdsIdentity` sds2 ["|":acc]]]
	sdsIdentity` (SDSSequence sds1 sds2 {SDSSequence|name}) acc
		= ["<",name:sdsIdentity` sds1 [",":sdsIdentity` sds2 [">":acc]]]
	sdsIdentity` (SDSCache {SDSSource|name} _) acc = ["$", name, "$":acc]
	sdsIdentity` (SDSDynamic f) acc = ["SDSDYNAMIC":acc] //TODO: Figure out how to determine the identity of the wrapped sds

iworldNotifyPred :: !(p -> Bool) !p !*IWorld -> (!Bool,!*IWorld)
iworldNotifyPred npred p env = (npred p, env)

read :: !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r
read sds env = read` () Nothing (sdsIdentity sds) sds env

readRegister :: !TaskId !(RWShared () r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | TC r
readRegister taskId sds env = read` () (Just taskId) (sdsIdentity sds) sds env

mbRegister :: !p !(RWShared p r w) !(Maybe TaskId) !SDSIdentity !*IWorld -> *IWorld | iTask p
mbRegister p sds Nothing reqSDSId iworld = iworld
mbRegister p sds (Just taskId) reqSDSId iworld=:{IWorld|sdsNotifyRequests,world}
	# (ts, world) = nsTime world
    # req = {SDSNotifyRequest|reqTimespec=ts,reqTaskId=taskId,reqSDSId=reqSDSId,cmpSDSId=sdsIdentity sds,cmpParam=dynamic p,cmpParamText=toSingleLineText p}
    = {iworld & world=world, sdsNotifyRequests = [req:sdsNotifyRequests]}

read` :: !p !(Maybe TaskId) !SDSIdentity !(RWShared p r w) !*IWorld -> (!MaybeError TaskException r, !*IWorld) | iTask p & TC r
read` p mbNotify reqSDSId sds=:(SDSSource {SDSSource|read}) env
    //New registration
    # env = mbRegister p sds mbNotify reqSDSId env
    = read p env

read` p mbNotify reqSDSId sds=:(SDSLens sds1 {SDSLens|param,read}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    = case read of
        SDSRead f = case (read` (param p) mbNotify reqSDSId sds1 env) of
            (Error e, env)  = (Error e, env)
            (Ok r, env)     = (f p r, env)
        SDSReadConst f
            = (Ok (f p), env)

read` p mbNotify reqSDSId sds=:(SDSSelect sds1 sds2 {SDSSelect|select}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    = case select p of
        Left p1     = read` p1 mbNotify reqSDSId sds1 env
        Right p2    = read` p2 mbNotify reqSDSId sds2 env

read` p mbNotify reqSDSId sds=:(SDSParallel sds1 sds2 {SDSParallel|param,read}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    # (p1,p2) = param p
    # (res1, env) = read` p1 mbNotify reqSDSId sds1 env
    | res1 =:(Error _)
        = (liftError res1, env)
    # (res2, env) = read` p2 mbNotify reqSDSId sds2 env
    | res2 =:(Error _)
        = (liftError res2, env)
    = (Ok (read (fromOk res1, fromOk res2)), env)

read` p mbNotify reqSDSId sds=:(SDSSequence sds1 sds2 {SDSSequence|paraml,paramr,read}) env
    # env = mbRegister p sds mbNotify reqSDSId env
    # (res1,env) = read` (paraml p) mbNotify reqSDSId sds1 env
    | res1 =:(Error _)
        = (liftError res1,env)
    # r1 = fromOk res1
	= case read p r1 of
		Left r = (Ok r,env)
		Right read2
    		# (res2,env) = read` (paramr p r1) mbNotify reqSDSId sds2 env
    		| res2 =:(Error _)
        		= (liftError res2,env)
			= (Ok (read2 (r1,fromOk res2)),env)

read` p mbNotify reqSDSId sds=:(SDSCache sds1 _) env=:{IWorld|readCache}
    # env = mbRegister p sds mbNotify reqSDSId env
	# key = (sdsIdentity sds,toSingleLineText p)
	//First check cache
	= case 'DM'.get key readCache of
		Just (val :: r^) = (Ok val,env)
		Just _           = (Error (exception "Cached value of wrong type"), env)
		Nothing = case read` p mbNotify reqSDSId (SDSSource sds1) env of
			(Error e,env) = (Error e, env)
			//Read and add to cache
			(Ok val,env)  = (Ok val, {env & readCache = 'DM'.put key (dynamic val :: r^) env.readCache})

read` p mbNotify reqSDSId sds=:(SDSDynamic f) env
    # env = mbRegister p sds mbNotify reqSDSId env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, env)
		(Ok sds)  = read` p mbNotify reqSDSId sds env

write :: !w !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld) | TC r & TC w
write w sds iworld
    = case write` () w sds iworld of
		(Ok notify, iworld) = (Ok (), queueNotifyEvents (sdsIdentity sds) notify iworld)
        (Error e,iworld)    = (Error e,iworld)

write` :: !p !w !(RWShared p r w) !*IWorld -> (!MaybeError TaskException (Set TaskId), !*IWorld) | iTask p & TC r & TC w
write` p w sds=:(SDSSource {SDSSource|name,write}) env
    = case write p w env of
        (Error e, env)   = (Error e, env)
        (Ok npred, env)  
			# (match,nomatch, env) = checkRegistrations (sdsIdentity sds) npred env
			= (Ok match, env)

write` p w sds=:(SDSLens sds1 {SDSLens|param,write,notify}) env
	//Determine the parameter for writing the underlying SDS
    # ps = param p
    = case (write,notify) of
        //Special case: we don't need to read the base SDS
        (SDSWriteConst writef,SDSNotifyConst notifyf)
			//Check which registrations the current parameter matches
			# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) (notifyf p w) env 
            = case writef p w of
                (Error e) = (Error e, env)
                (Ok Nothing)
					//We need to decide based on the current parameter if we need to notify or not
					= (Ok match, env)
                (Ok (Just ws)) = case write` ps ws sds1 env of
                    (Error e, env) = (Error e, env)
                    (Ok notify, env) 
						//Remove the registrations that we can eliminate based on the current parameter
						# notify = 'Set'.difference notify ('Set'.difference nomatch match)
						= (Ok notify, env)
        //General case: read base SDS before writing
        _
            = case read` ps Nothing (sdsIdentity sds1) sds1 env of
                (Error e, env) = (Error e, env)
                (Ok rs, env)
                    # ws = case write of
                        SDSWrite writef = writef p rs w
                        SDSWriteConst writef = writef p w
                    # notifyf = case notify of
                        SDSNotify notifyf = notifyf p rs w
                        SDSNotifyConst notifyf = notifyf p w
					//Check which registrations the current parameter matches
					# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) notifyf env 
                    = case ws of
                        (Error e) = (Error e, env)
                        (Ok Nothing)
                            = (Ok match, env)
                        (Ok (Just ws)) = case write` ps ws sds1 env of
                            (Error e, env) = (Error e, env)
                            (Ok notify, env)
								//Remove the registrations that we can eliminate based on the current parameter
								# notify = 'Set'.difference notify ('Set'.difference nomatch match)
                                = (Ok notify, env)

write` p w sds=:(SDSSelect sds1 sds2 {SDSSelect|select,notifyl,notifyr}) env
    = case select p of
        Left p1 = case notifyl of
			(SDSNotify f)  = case read` p1 Nothing (sdsIdentity sds1) sds1 env of
            	(Error e, env)  = (Error e, env)
            	(Ok r1, env)    = case write` p1 w sds1 env of
               		(Error e, env) = (Error e, env)
	                (Ok notify, env)
   		                # npred = (\ts pq -> case select pq of Right p2 = f p1 r1 w ts p2; _ = False)
						# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
   		                = (Ok notify, env)
			(SDSNotifyConst f) = case write` p1 w sds1 env of
				(Error e, env) = (Error e, env)
				(Ok notify, env)
   		        	# npred = (\ts pq -> case select pq of Right p2 = f p1 w ts p2; _ = False)
					# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
					# notify = 'Set'.union notify match
   		            = (Ok notify, env)
        Right p2 = case notifyr of
			(SDSNotify f) = case read` p2 Nothing (sdsIdentity sds2) sds2 env of
            	(Error e, env)  = (Error e, env)
            	(Ok r2, env)    = case write` p2 w sds2 env of
               		(Error e, env) = (Error e,env)
                	(Ok notify, env)
                    	# npred = (\ts pq -> case select pq of Left p1 = f p2 r2 w ts p1 ; _ = False)
						# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
						//Add the matching registrations for the 'other' SDS
						# notify = 'Set'.union notify match
                    	= (Ok notify, env)

			(SDSNotifyConst f) = case write` p2 w sds2 env of
				(Error e, env) = (Error e,env)
               	(Ok notify, env)
                	# npred = (\ts pq -> case select pq of Left p1 = f p2 w ts p1 ; _ = False)
					# (match,nomatch,env) = checkRegistrations (sdsIdentity sds) npred env
					//Add the matching registrations for the 'other' SDS
					# notify = 'Set'.union notify match
                    = (Ok notify, env)

write` p w sds=:(SDSParallel sds1 sds2 {SDSParallel|param,writel,writer}) env
    # (p1,p2) = param p
    //Read/write sds1
    # (npreds1,env) = case writel of
        (SDSWrite f) = case read` p1 Nothing (sdsIdentity sds1) sds1 env of
            (Error e, env)  = (Error e, env)
            (Ok r1,env)     = case f p r1 w of
                Error e         = (Error e, env)
                Ok (Nothing)    = (Ok 'Set'.newSet, env)
                Ok (Just w1)    = write` p1 w1 sds1 env
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,env)
                Ok (Nothing)        = (Ok 'Set'.newSet,env)
                Ok (Just w1)        = write` p1 w1 sds1 env
    | npreds1 =:(Error _) = (liftError npreds1, env)
    //Read/write sds2
    # (npreds2,env) = case writer of
        (SDSWrite f) = case read` p2 Nothing (sdsIdentity sds2) sds2 env of
            (Error e, env)  = (Error e, env)
            (Ok r2,env)     = case f p r2 w of
                Error e         = (Error e, env)
                Ok (Nothing)    = (Ok 'Set'.newSet, env)
                Ok (Just w2)    = write` p2 w2 sds2 env
        (SDSWriteConst f) = case f p w of
                Error e             = (Error e,env)
                Ok (Nothing)        = (Ok 'Set'.newSet, env)
                Ok (Just w2)        = write` p2 w2 sds2 env
    | npreds2 =:(Error _) = (liftError npreds2, env)
    = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), env)

write` p w sds=:(SDSSequence sds1 sds2 {SDSSequence|paraml,paramr,writel,writer}) env
    = case read` (paraml p) Nothing (sdsIdentity sds1) sds1 env of
        (Error e, env)  = (Error e, env)
        (Ok r1, env)
            //Write sds1 if necessary
            # (npreds1,env) = case writel of
                (SDSWrite f)  = case f p r1 w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok 'Set'.newSet, env)
                    Ok (Just w1)     = write` (paraml p) w1 sds1 env
                (SDSWriteConst f) = case f p w of
                    Error e          = (Error e, env)
                    Ok (Nothing)     = (Ok 'Set'.newSet, env)
                    Ok (Just w1)     = write` (paraml p) w1 sds1 env
            | npreds1 =:(Error _) = (liftError npreds1, env)
            //Read/write sds2 if necessary
            # (npreds2,env) = case writer of
                (SDSWrite f) = case read` (paramr p r1) Nothing (sdsIdentity sds2) sds2 env of //Also read sds2
                    (Error e, env)  = (Error e, env)
                    (Ok r2,env)     = case f p r2 w of
                        Error e         = (Error e, env)
                        Ok (Nothing)    = (Ok 'Set'.newSet, env)
                        Ok (Just w2)    = write` (paramr p r1) w2 sds2 env
                (SDSWriteConst f) = case f p w of
                    Error e             = (Error e, env)
                    Ok (Nothing)        = (Ok 'Set'.newSet, env)
                    Ok (Just w2)        = write` (paramr p r1) w2 sds2 env
            | npreds2 =:(Error _) = (liftError npreds2, env)
            = (Ok ('Set'.union (fromOk npreds1) (fromOk npreds2)), env)

write` p w sds=:(SDSCache sds1 {SDSCache|write}) env=:{IWorld|readCache,writeCache}
	# key = (sdsIdentity sds,toSingleLineText p)
	//Check cache
	# mbr = case 'DM'.get key readCache of
		Just (val :: r^) = Just val
		_                = Nothing
	# mbw = case 'DM'.get key writeCache of
		Just (val :: w^,_) = Just val
		_                  = Nothing
	//Determine what to do
	# (mbr,policy) = write p mbr mbw w
	//Update read cache
	# readCache = case mbr of
		Just r = 'DM'.put key (dynamic r :: r^) readCache
		Nothing  = 'DM'.del key readCache
	= case policy of
		NoWrite = (Ok 'Set'.newSet, {env & readCache = readCache})
		WriteNow = write` p w (SDSSource sds1) {env & readCache = readCache}
		WriteDelayed
			# writeCache = 'DM'.put key (dynamic w :: w^, DeferredWrite p w (SDSSource sds1)) writeCache
			= (Ok 'Set'.newSet, {env & readCache = readCache, writeCache = writeCache})

write` p w sds=:(SDSDynamic f) env
	# (mbsds, env) = f p env
	= case mbsds of
		(Error e) = (Error e, env)
		(Ok dsds) = write` p w dsds env

//Check the registrations and find the set of id's for which the current predicate holds
//and for which id's it doesn't
checkRegistrations :: !SDSIdentity (SDSNotifyPred p) !*IWorld -> (Set TaskId, Set TaskId,!*IWorld) | TC p
checkRegistrations sdsId pred iworld
	# (registrations, iworld) 	= lookupRegistrations sdsId iworld
	# (match,nomatch) 			= matchRegistrations pred registrations
	= (match,nomatch,iworld)
where
	//Find all notify requests for the given share id
	lookupRegistrations sdsId iworld=:{sdsNotifyRequests}
        = ([reg \\ reg=:{SDSNotifyRequest|cmpSDSId} <- sdsNotifyRequests | cmpSDSId == sdsId],iworld)

	//Match the notify requests against the predicate to determine two sets:
	//The registrations that matched the predicate, and those that did not match the predicate
	matchRegistrations pred [] = ('Set'.newSet,'Set'.newSet)
	matchRegistrations pred [{SDSNotifyRequest|reqTimespec,reqTaskId,cmpParam}:regs]
		# (match,nomatch) = matchRegistrations pred regs
    	= case cmpParam of
            (p :: p^) = if (pred reqTimespec p)
							('Set'.insert reqTaskId match,nomatch)
							(match, 'Set'.insert reqTaskId nomatch)
			//In case of a type mismatch, just ignore (should not happen)
            _                        = (match,nomatch)

modify :: !(r -> (!a,!w)) !(RWShared () r w) !*IWorld -> (!MaybeError TaskException a, !*IWorld) | TC r & TC w
modify f sds iworld = case read sds iworld of
    (Ok r,iworld)      = let (a,w) = f r in case write w sds iworld of
		(Ok (),iworld)    = (Ok a,iworld)	
		(Error e,iworld)  = (Error e, iworld)
    (Error e,iworld)   = (Error e,iworld)

notify :: !(RWShared () r w) !*IWorld -> (!MaybeError TaskException (), !*IWorld)
notify sds iworld = (Ok (), iworld) //TODO

queueNotifyEvents :: !String !(Set TaskId) *IWorld -> *IWorld
queueNotifyEvents sdsId notify iworld
	= queueRefresh [(t,"Notification for write of " +++ sdsId) \\ t <- 'Set'.toList notify] iworld

clearTaskSDSRegistrations :: !(Set TaskId) !*IWorld -> *IWorld
clearTaskSDSRegistrations taskIds iworld=:{IWorld|sdsNotifyRequests}
    = {iworld & sdsNotifyRequests = [r \\ r=:{SDSNotifyRequest|reqTaskId} <- sdsNotifyRequests | not ('Set'.member reqTaskId taskIds)]}

listAllSDSRegistrations :: *IWorld -> (![(InstanceNo,[(TaskId,SDSIdentity)])],!*IWorld)
listAllSDSRegistrations iworld=:{IWorld|sdsNotifyRequests} = ('DM'.toList (foldr addReg 'DM'.newMap sdsNotifyRequests),iworld)
where
    addReg {SDSNotifyRequest|reqTaskId=reqTaskId=:(TaskId taskInstance _),cmpSDSId} list
        = 'DM'.put taskInstance [(reqTaskId,cmpSDSId):fromMaybe [] ('DM'.get taskInstance list)] list

formatSDSRegistrationsList :: [(InstanceNo,[(TaskId,SDSIdentity)])] -> String
formatSDSRegistrationsList list
    = join "\n" (flatten [["Task instance " +++ toString i +++ ":"
                          :["\t"+++toString taskId +++ "->"+++sdsId\\(taskId,sdsId) <- regs]] \\ (i,regs) <- list])

flushDeferredSDSWrites :: !*IWorld -> (!MaybeError TaskException (), !*IWorld)
flushDeferredSDSWrites iworld=:{writeCache}
	# (errors,iworld) = flushAll ('DM'.toList writeCache) iworld
	| errors =: [] = (Ok (), {iworld & writeCache = 'DM'.newMap})
	# msg = join OS_NEWLINE ["Could not flush all deferred SDS writes, some data may be lost":map snd errors]
	= (Error (exception msg),{iworld & writeCache = 'DM'.newMap})
where
	flushAll [] iworld = ([],iworld)
	flushAll [(_,(_,DeferredWrite p w sds)):rest] iworld
		= case write` p w sds iworld of
			(Ok notify,iworld)
				# iworld = queueNotifyEvents (sdsIdentity sds) notify iworld
				= flushAll rest iworld
			(Error e,iworld)
				# (errors,iworld) = flushAll rest iworld
				= ([e:errors],iworld)

toJSONShared :: (RWShared p r w) -> JSONShared | JSONDecode{|*|} p & JSONEncode{|*|} r & JSONDecode{|*|} w & iTask p & TC r & TC w
toJSONShared sds = SDSLens sds {SDSLens|name="toJSONShared",param=param,read=SDSRead read,write=SDSWriteConst write,notify=SDSNotifyConst notify}
where
	param p = fromJust (fromJSON p)
    read p rs = Ok (toJSON rs)
    write _ w = case fromJSON w of
        (Just ws)   = (Ok (Just ws))
        Nothing     = Error (exception "Shared type mismatch in toJSONShared")
    notify _ _      = const (const True)

fromJSONShared :: JSONShared -> RWShared p r w | JSONEncode{|*|} p & JSONDecode{|*|} r & JSONEncode{|*|} w
fromJSONShared sds = SDSLens sds {SDSLens|name="fromJSONShared",param=param,read=SDSRead read,write=SDSWriteConst write,notify=SDSNotifyConst notify}
where
    param p = toJSON p
    read _ rs = case fromJSON rs of
        (Just r)    = Ok r
        Nothing     = Error (exception "Shared type mismatch in fromJSONShared")
    write _ w       = Ok (Just (toJSON w))
    notify _ _      = const (const True)

newSDSId :: !*IWorld -> (!String, !*IWorld)
newSDSId iworld=:{IWorld|random}
	= (toString (take 32 [toChar (97 +  abs (i rem 26)) \\ i <- random]) , {IWorld|iworld&random = drop 32 random})

newURL :: !*IWorld -> (!String, !*IWorld)
newURL iworld=:{IWorld|options={serverUrl},random}
	# (sdsId, iworld) = newSDSId iworld
	= getURLbyId sdsId iworld

// TODO: different URL for clients
getURLbyId :: !String !*IWorld -> (!String, !*IWorld)
getURLbyId sdsId iworld=:{IWorld|options={serverUrl},random}
	= ("sds:" +++ serverUrl +++ "/" +++ sdsId, iworld)	

