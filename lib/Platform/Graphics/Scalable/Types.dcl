definition module Graphics.Scalable.Types

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
  :: ImageTag, instance == ImageTag, instance < ImageTag

:: ImageSpan   :== (!Span, !Span)
:: ImageOffset :== (!Span, !Span)

:: FontDef
  = { fontfamily  :: !String
    , fontysize   :: !Real
    , fontstretch :: !String
    , fontstyle   :: !String
    , fontvariant :: !String
    , fontweight  :: !String
    }
normalFontDef     :: !String !Real -> FontDef  // (normalFontDef family size) sets all other fields to "normal"
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

:: OnClickAttr     m = { onclick     :: !(Int m -> m), local :: !Bool }
:: OnMouseDownAttr m = { onmousedown :: !(m -> m), local :: !Bool }
:: OnMouseUpAttr   m = { onmouseup   :: !(m -> m), local :: !Bool }
:: OnMouseOverAttr m = { onmouseover :: !(m -> m), local :: !Bool }
:: OnMouseMoveAttr m = { onmousemove :: !(m -> m), local :: !Bool }
:: OnMouseOutAttr  m = { onmouseout  :: !(m -> m), local :: !Bool }
:: DraggableAttr   m = { draggable   :: !(SVGDragFun m) }
:: SVGDragFun    m :== (Set ImageTag) (Real,Real) m -> m     // \tags (x,y) model: tags is the set of ImageTag-s associated with the 'dragged-on' image, (x,y) the location within that image
:: RGB               = { r :: !Int, g :: !Int, b :: !Int }

class toSVGColor a :: !a -> SVGColor
instance toSVGColor String, RGB
instance zero RGB
