definition module iTasks.Internal.Distributed.Instance

from iTasks.WF.Definition import :: InstanceNo
from iTasks.UI.Editor import :: Editor
from iTasks.Internal.Generic.Visualization import :: TextFormat
from Data.Maybe import :: Maybe
from Text.GenJSON import :: JSONNode
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.Internal.Generic.Visualization import generic gText
from Text.GenJSON import generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Defaults import generic gDefault
from Data.GenEq import generic gEq
from Data.Map import :: Map
from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task
from iTasks.WF.Definition import :: TaskAttributes
from iTasks.Internal.Distributed.Domain import :: Domain
from iTasks.Extensions.Distributed.Task import :: Domain

instanceServer :: Int Domain -> Task ()

instanceClient :: String Int Domain -> Task ()

instanceFilter :: (TaskAttributes -> Bool) Domain -> Task ()

instanceClameFilter :: (TaskAttributes -> Bool) Domain -> Task ()

sendDistributedInstance :: InstanceNo (Task a) TaskAttributes Domain -> Task a | iTask a

sendRequestToInstanceServer :: Int String -> Task ()
