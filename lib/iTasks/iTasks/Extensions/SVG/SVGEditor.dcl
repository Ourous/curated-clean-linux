definition module iTasks.Extensions.SVG.SVGEditor

from Data.GenEq import generic gEq
import Graphics.Scalable.Image
from Text.GenJSON import :: JSONNode, generic JSONEncode, generic JSONDecode
from iTasks.Internal.Generic.Visualization import :: TextFormat, generic gText
from   iTasks.UI.Editor import :: Editor

// An SVGEditor let's you specify an editor as an interactive SVG image (Graphics.Scalable.Image)
:: SVGEditor m v =
	{ initView    :: m -> v                      // Generate the view value from the current model value
	, renderImage :: m v *TagSource -> Image v   // Render an interactive image from the current model and view value
	, updModel    :: m v -> m                    // When the view is updated (using the interactive image), update the model
	}

fromSVGEditor :: !(SVGEditor s v) -> Editor s | gEq{|*|}, gText{|*|}, JSONEncode{|*|}, JSONDecode{|*|} s
