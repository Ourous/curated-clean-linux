definition module iTasks.Extensions.Distributed._Attributes

import iTasks

:: InstanceReference = Local InstanceNo
		     | DistributedInstancePool InstanceNo Int

derive class iTask InstanceReference

instance toString	InstanceReference

distributedInstanceIdAttribuut :: String

currentDistributedId :: Task InstanceReference

isLocal :: InstanceReference -> Bool
