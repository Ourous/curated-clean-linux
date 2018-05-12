definition module iTasks.Internal.Client.RunOnClient

import iTasks

runOnClient :: !(Task m) -> Task m | iTask m
createClientIWorld :: !String !InstanceNo -> *IWorld
getUIUpdates :: !*IWorld -> (!Maybe [(InstanceNo, [String])], *IWorld)
