definition module iTasks.Internal.Distributed.Domain

from Text.GenJSON import :: JSONNode
from iTasks.Internal.Generic.Visualization import :: TextFormat
from Data.Maybe import :: Maybe
from iTasks.UI.Editor import :: Editor
from Data.GenEq import generic gEq
from iTasks.Internal.Generic.Defaults import generic gDefault
from Text.GenJSON import generic JSONDecode
from Text.GenJSON import generic JSONEncode
from iTasks.Internal.Generic.Visualization import generic gText
from iTasks.UI.Editor.Generic import generic gEditor
from iTasks.WF.Definition import class iTask


derive class iTask Domain
:: Domain = Domain String
