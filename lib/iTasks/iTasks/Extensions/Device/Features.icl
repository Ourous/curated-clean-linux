implementation module iTasks.Extensions.Device.Features

import iTasks.SDS.Definition
import iTasks.SDS.Sources.Store
import iTasks.Internal.SDS
from iTasks.WF.Tasks.SDS import get, set
from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition      import :: Task, generic gEq, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskId
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from Data.Maybe import :: Maybe
from iTasks.WF.Tasks.Interaction import :: UpdateOption, updateInformation
from iTasks.WF.Combinators.Common import >>-
from iTasks.WF.Combinators.Overloaded import instance Functor Task, instance TMonad Task, class TMonad(..), class TApplicative, instance TApplicative Task
from iTasks.UI.Definition import :: Hint(..)
from iTasks.UI.Tune import class tune(..), @>>, instance tune Hint Task
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
	>>- \info -> Hint "Manage device features" @>> updateInformation [] info
	>>= \info -> set info device
