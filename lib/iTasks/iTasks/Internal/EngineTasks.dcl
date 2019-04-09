definition module iTasks.Internal.EngineTasks
/**
* This module defines the separate system tasks that the iTasks engine performs
*/
from iTasks.WF.Definition import :: Task

removeOutdatedSessions :: Task ()

flushWritesWhenIdle:: Task ()

stopOnStable :: Task ()
