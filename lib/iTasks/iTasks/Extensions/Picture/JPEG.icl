implementation module iTasks.Extensions.Picture.JPEG

import StdString

from iTasks.Internal.IWorld import :: IWorld
from Data.Maybe import :: Maybe(..)
from iTasks.WF.Definition      import :: Task, generic gEq, generic gDefault, generic JSONDecode, generic JSONEncode, generic gText, generic gEditor, :: Editor, :: TaskId
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization    import :: TextFormat(..)
from Data.Maybe import :: Maybe
from Data.Map import :: Map, newMap

from iTasks.UI.Editor.Modifiers import comapEditorValue, instance tune UIAttributes Editor
from iTasks.UI.Editor.Controls import htmlView
from iTasks.UI.Definition import :: UIAttributes
from Text.HTML import :: HtmlTag(ImgTag), :: HtmlAttr(SrcAttr,StyleAttr,AltAttr)
from iTasks.WF.Combinators.Tune import <<@, class tune

derive gText JPEGPicture
derive JSONEncode JPEGPicture
derive JSONDecode JPEGPicture
derive gDefault JPEGPicture
derive gEq JPEGPicture

gEditor{|JPEGPicture|} = comapEditorValue (\(JPEGPicture val) -> ImgTag [SrcAttr val, AltAttr "no photo", StyleAttr ("max-width: 200px; max-height: 200px;")])
                                 		htmlView
