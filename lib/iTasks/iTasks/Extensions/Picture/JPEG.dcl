definition module iTasks.Extensions.Picture.JPEG

from iTasks.WF.Definition import :: Task, generic gEq, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from Data.Maybe import :: Maybe
from Data.Map import :: Map

:: JPEGPicture = JPEGPicture String

derive gText JPEGPicture
derive JSONEncode JPEGPicture
derive JSONDecode JPEGPicture
derive gEq JPEGPicture

derive gEditor JPEGPicture
