implementation module iTasks.WF.Tasks.System

import iTasks.WF.Definition
import iTasks.Internal.Task
import iTasks.Internal.IWorld
import iTasks.Internal.Generic.Visualization
import Data.Error, Data.Maybe
import StdFile
import iTasks.WF.Tasks.Core

traceValue :: a -> Task a | iTask a
traceValue v = accWorld printStdErr
where
	printStdErr w
	# (_, w) = fclose (stderr <<< toSingleLineText v <<< "\n") w
	= (v, w)

shutDown :: Int -> Task ()
shutDown exitCode = mkInstantTask (\taskId iworld -> (Ok (), {IWorld|iworld & shutdown = Just exitCode}))

