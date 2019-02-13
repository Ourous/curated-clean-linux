definition module iTasks.Extensions.Distributed._Types

from iTasks.WF.Definition import :: TaskAttributes, :: Task, class iTask, :: TaskValue
import iTasks.SDS.Definition

:: Remote_Task = E. a: Remote_Task (Task a) TaskAttributes Int & iTask a

:: Remote_TaskValue = E. a: Remote_TaskValue (TaskValue a) & iTask a
