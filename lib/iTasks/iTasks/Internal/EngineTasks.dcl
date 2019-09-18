definition module iTasks.Internal.EngineTasks
/**
* This module defines the separate system tasks that the iTasks engine performs
*/
from iTasks.WF.Definition import :: Task
from iTasks.Internal.Generic.Visualization import generic gText, :: TextFormat
from iTasks.Internal.IWorld import :: IWorld
from Data.Maybe import :: Maybe

removeOutdatedSessions :: Task ()

flushWritesWhenIdle:: Task ()

stopOnStable :: Task ()

printStdErr :: v !*IWorld -> *IWorld | gText{|*|} v
