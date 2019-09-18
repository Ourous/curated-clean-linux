definition module Graphics.Scalable.Types

import Data.GenEq
from Data.Maybe import :: Maybe (..)
from Data.Set   import :: Set
from Text.HTML  import :: SVGColor
from StdOverloaded import class zero (..), class + (..), class -  (..), class ~ (..), class sign (..), 
                          class abs  (..), class < (..), class == (..), class toReal (..), class / (..), class * (..), class toString (..)
from Graphics.Scalable.Internal.Types import
  :: Span, class *. (..), class /. (..), instance zero Span, instance + Span, instance -  Span, instance * Span,  instance / Span,
                                         instance abs  Span, instance ~ Span, instance *. Span, instance *. Real, instance *. Int,
                                                                              instance /. Span, instance /. Real, instance /. Int, 
  px, textxspan, imagexspan, imageyspan, columnspan, rowspan, minSpan, maxSpan,
  :: ImageTag, instance == ImageTag, instance < ImageTag,
  :: FontDef`

:: ImageSpan   :== (!Span, !Span)
:: ImageOffset :== (!Span, !Span)

:: FontDef     :== FontDef`
normalFontDef     :: !String !Real    -> FontDef  // (normalFontDef family size) sets all other fields to "normal"
setfontfamily     :: !String !FontDef -> FontDef
setfontysize      :: !Real   !FontDef -> FontDef
setfontstretch    :: !String !FontDef -> FontDef
setfontstyle      :: !String !FontDef -> FontDef
setfontvariant    :: !String !FontDef -> FontDef
setfontweight     :: !String !FontDef -> FontDef
getfontfamily     ::         !FontDef -> String
getfontysize      ::         !FontDef -> Real
getfontstretch    ::         !FontDef -> String
getfontstyle      ::         !FontDef -> String
getfontvariant    ::         !FontDef -> String
getfontweight     ::         !FontDef -> String

instance ==       FontDef
instance <        FontDef
instance toString FontDef

:: LineMarkerPos
  = LineMarkerEnd
  | LineMarkerMid
  | LineMarkerStart
instance == LineMarkerPos

:: *TagSource      :== *[TagRef]
:: *TagRef         :== *(!ImageTag, !*ImageTag)

:: GridDimension     = Rows !Int | Columns !Int
:: GridLayout      :== (!GridMajor, !GridXLayout, !GridYLayout)
:: GridMajor         = ColumnMajor | RowMajor
:: GridXLayout       = LeftToRight | RightToLeft
:: GridYLayout       = TopToBottom | BottomToTop
:: XAlign            = AtLeft | AtMiddleX | AtRight
:: YAlign            = AtTop  | AtMiddleY | AtBottom
:: XYAlign         :== (!XAlign, !YAlign)

:: OnClickAttr     m = { onclick     :: !(m -> m),     local :: !Bool }   // handle single mouse click, without delay
:: OnNClickAttr    m = { onNclick    :: !(Int m -> m), local :: !Bool }   // handle multiple mouse clicks, this comes with a brief (client dependent) delay
:: OnMouseDownAttr m = { onmousedown :: !(m -> m),     local :: !Bool }   // handle mouse down event
:: OnMouseUpAttr   m = { onmouseup   :: !(m -> m),     local :: !Bool }   // handle mouse up event
:: OnMouseOverAttr m = { onmouseover :: !(m -> m),     local :: !Bool }   // handle mouse over event (move into associated image)
:: OnMouseMoveAttr m = { onmousemove :: !(m -> m),     local :: !Bool }   // handle mouse move event (move inside associated image)
:: OnMouseOutAttr  m = { onmouseout  :: !(m -> m),     local :: !Bool }   // handle mouse out event (move away from associated image)
:: DraggableAttr   m = { draggable   :: !(SVGDragFun m) }
:: SVGDragFun    m :== (Set ImageTag) (Real,Real) m -> m     // \tags (x,y) model: tags is the set of ImageTag-s associated with the 'dragged-on' image, (x,y) the location within that image
:: RGB               = { r :: !Int, g :: !Int, b :: !Int }

class toSVGColor a :: !a -> SVGColor
instance toSVGColor String, RGB
instance zero RGB
