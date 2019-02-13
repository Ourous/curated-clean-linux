definition module iTasks.Extensions.Distributed.InteractionTasks

from iTasks.WF.Definition import class iTask
import iTasks.Internal.SDS
from iTasks.WF.Definition	import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Data.Maybe import :: Maybe 
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization	import :: TextFormat(..)
from iTasks.WF.Tasks.Interaction         import :: ViewOption(..)

viewSharedInformation :: String [ViewOption r] !(sds () r w) -> Task r | iTask r & iTask w & RWShared sds
