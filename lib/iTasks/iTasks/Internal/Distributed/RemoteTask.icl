implementation module iTasks.Internal.Distributed.RemoteTask

import iTasks
import iTasks.Internal.Distributed.Instance
from iTasks.Internal.Distributed.Domain import :: Domain(..)

remoteAssignTask :: !TaskAttributes (Task a) Domain -> Task a | iTask a
remoteAssignTask attributes task domain
	= get currentTaskInstanceNo
	>>= \taskid -> sendDistributedInstance taskid task attributes domain

