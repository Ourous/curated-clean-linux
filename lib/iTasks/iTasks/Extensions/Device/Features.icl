implementation module iTasks.Extensions.Device.Features

import iTasks.SDS.Definition
import iTasks.SDS.Sources.Store
import iTasks.Internal.SDS
from iTasks.WF.Tasks.SDS import get, set
from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition      import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskId
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.Maybe import :: Maybe
from iTasks.WF.Tasks.Interaction import :: UpdateOption, updateInformation
from iTasks.UI.Prompt import class toPrompt, instance toPrompt String
from iTasks.WF.Combinators.Common import >>-
from iTasks.WF.Combinators.Overloaded import instance Functor Task, instance TMonad Task, class TMonad(..), class TApplicative, instance TApplicative Task
from Data.Functor import class Functor 

import StdString

derive class iTask DeviceFeatures

hasCamera :: DeviceFeatures -> Bool
hasCamera {DeviceFeatures|camera} = camera

device :: SimpleSDSLens DeviceFeatures
device = sharedStore "deviceFeaturs" {DeviceFeatures| camera = False }

manageDeviceFeaturs :: Task DeviceFeatures
manageDeviceFeaturs
	=              get device
	>>- \info -> updateInformation "Manage device features" [] info
	>>= \info -> set info device
