implementation module iTasks.Extensions.Distributed._Attributes

import iTasks
import qualified Data.Map as DM
from Data.Maybe import fromMaybe, isNothing, fromJust, maybe, instance Functor Maybe, isJust

derive class iTask InstanceReference

instance toString InstanceReference
where
	toString (Local instanceNo) = toString instanceNo
	toString (DistributedInstancePool instanceNo _) = toString instanceNo

distributedInstanceIdAttribuut :: String
distributedInstanceIdAttribuut = "distributedInstanceId"

currentDistributedId :: Task InstanceReference
currentDistributedId
	= get currentTaskInstanceNo
	>>- \instanceNo -> getInstancePoolId instanceNo
	>>- \poolId -> getServerId instanceNo
	>>- \serverId -> return (maybe (Local instanceNo) (\x -> (DistributedInstancePool x (fromMaybe 0 serverId))) poolId)
where
	find key attributes = maybe Nothing (\v -> (Just (toInt v))) ('DM'.get key attributes)

	getInstancePoolId instanceNo = get (mapRead (find distributedInstanceIdAttribuut) (sdsFocus instanceNo taskInstanceAttributesByNo))

	getServerId instanceNo = get (mapRead (find "distributedInstanceServerId") (sdsFocus instanceNo taskInstanceAttributesByNo))

isLocal :: InstanceReference -> Bool
isLocal (Local _) 	= True
isLocal _		= False
