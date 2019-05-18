implementation module iTasks.Extensions.Image
import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor.Controls, iTasks.UI.Editor.Modifiers
import Text.HTML

gEditor{|WebImage|} = comapEditorValue (\{WebImage|src,alt,width,height} -> ImgTag [SrcAttr src,AltAttr alt,WidthAttr (toString width), HeightAttr (toString height)])
                                 		htmlView

derive gText	        WebImage
derive JSONEncode		WebImage
derive JSONDecode		WebImage
derive gDefault			WebImage
derive gEq				WebImage
