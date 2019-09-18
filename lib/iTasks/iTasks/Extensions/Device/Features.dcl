definition module iTasks.Extensions.Device.Features

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from iTasks.SDS.Definition import :: SDSLens, :: SimpleSDSLens
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from Data.Maybe import :: Maybe

:: DeviceFeatures = { camera :: Bool }

derive class iTask DeviceFeatures

hasCamera :: DeviceFeatures -> Bool

device :: SimpleSDSLens DeviceFeatures

manageDeviceFeaturs :: Task DeviceFeatures
