definition module iTasks.Extensions.Device.Location

from iTasks.WF.Definition import class iTask
from iTasks.WF.Definition import :: Task, generic gEq, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from Data.Maybe import :: Maybe
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode

:: Coordinates = LatLon Real Real

derive class iTask Coordinates

getLocation :: Task (Maybe Coordinates)
