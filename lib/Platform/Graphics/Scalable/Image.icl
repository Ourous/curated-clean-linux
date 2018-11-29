implementation module Graphics.Scalable.Image

from StdList      import repeat
import StdMisc
from Data.Maybe   import :: Maybe (..), instance Functor Maybe, maybeToList
from Data.Functor import class Functor (..)
import Graphics.Scalable.Internal.Image`

:: Image m :== Image` m

empty :: !Span !Span -> Image m
empty xspan yspan = Empty` xspan yspan
	 
text :: !FontDef !String -> Image m
text font str = Text` font str

circle :: !Span -> Image m
circle diameter = Circle` diameter

ellipse :: !Span !Span -> Image m
ellipse diax diay = Ellipse` diax diay

square :: !Span -> Image m
square edge = Square` edge

rect :: !Span !Span -> Image m
rect xspan yspan = Rect` xspan yspan

xline :: !Span -> Image m
xline xspan = Polyline` [(zero,zero),(xspan,zero)]

yline :: !Span -> Image m
yline yspan = Polyline` [(zero,zero),(zero,yspan)]

line :: !Span !Span -> Image m
line xspan yspan = Polyline` [(zero,zero),(xspan,yspan)]

polyline :: ![ImageOffset] -> Image m
polyline offsets = Polyline` offsets

polygon :: ![ImageOffset] -> Image m
polygon offsets = Polygon` offsets

rotate :: !Angle !(Image m) -> Image m
rotate a image = Rotate` a image

flipx :: !(Image m) -> Image m
flipx image = Flipx` image

flipy :: !(Image m) -> Image m
flipy image = Flipy` image

fit :: !Span !Span !(Image m) -> Image m
fit xspan yspan image = Fit` xspan yspan image

fitx :: !Span !(Image m) -> Image m
fitx xspan image = Fitx` xspan image

fity :: !Span !(Image m) -> Image m
fity yspan image = Fity` yspan image

scale :: !Real !Real !(Image m) -> Image m
scale fx fy image = Scale` fx fy image

scalex :: !Real !(Image m) -> Image m
scalex fx image = Scale` fx 1.0 image

scaley :: !Real !(Image m) -> Image m
scaley fy image = Scale` 1.0 fy image

skewx :: !Angle !(Image m) -> Image m
skewx a image = Skewx` a image

skewy :: !Angle !(Image m) -> Image m
skewy a image = Skewy` a image

overlay :: ![XYAlign] ![ImageOffset] ![Image m] !(Host m) -> Image m
overlay aligns offsets images host = Overlay` aligns offsets images (toHost` host)

collage :: ![ImageOffset] ![Image m] !(Host m) -> Image m
collage offsets images host = Overlay` (repeat (AtLeft,AtTop)) offsets images (toHost` host)

beside :: ![YAlign] ![Span] !(Maybe Span) ![ImageOffset] ![Image m] !(Host m) -> Image m
beside ylayouts column_widths row_height offsets imgs host
  = Grid` (Rows 1) (RowMajor, LeftToRight, TopToBottom) [(AtLeft, ylayout) \\ ylayout <- ylayouts] column_widths (maybeToList row_height) offsets imgs (toHost` host)

above :: ![XAlign] ![Span] !(Maybe Span) ![ImageOffset] ![Image m] !(Host m) -> Image m
above xlayouts row_heights column_width offsets imgs host
  = Grid` (Columns 1) (ColumnMajor, LeftToRight, TopToBottom) [(xlayout, AtTop) \\ xlayout <- xlayouts] (maybeToList column_width) row_heights offsets imgs (toHost` host)

grid :: !GridDimension !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image m] !(Host m) -> Image m
grid dimension layout aligns column_widths row_heights offsets images host
  = Grid` dimension layout aligns column_widths row_heights offsets images (toHost` host)

:: Host  m = NoHost | Host (Image m)

toHost` :: !(Host m) -> Host` m
toHost` NoHost     = NoHost`
toHost` (Host img) = Host` img

class tuneImage attr :: !(Image m) !(attr m) -> Image m

(<@<) infixl 2 :: !(Image m) !(attr m) -> Image m | tuneImage attr
(<@<) image attr = tuneImage image attr

(>@>) infixr 2 :: !(attr m) !(Image m) -> Image m | tuneImage attr
(>@>) attr image = tuneImage image attr

tuneIf :: !Bool !(Image m) !(attr m) -> Image m | tuneImage attr
tuneIf True img t = tuneImage img t
tuneIf _    img _ = img

:: NoAttr          m = NoAttr
:: DashAttr        m = { dash        :: ![Int]    }
:: FillAttr        m = { fill        :: !SVGColor }
:: LineEndMarker   m = { endmarker   :: !Image m  }
:: LineMidMarker   m = { midmarker   :: !Image m  }
:: LineStartMarker m = { startmarker :: !Image m  }
:: MaskAttr        m = { mask        :: !Image m  }
:: OpacityAttr     m = { opacity     :: !Real     }
:: StrokeAttr      m = { stroke      :: !SVGColor }
:: StrokeWidthAttr m = { strokewidth :: !Span     }
:: XRadiusAttr     m = { xradius     :: !Span     }
:: YRadiusAttr     m = { yradius     :: !Span     }


instance tuneImage NoAttr          where tuneImage image _    = image
instance tuneImage DashAttr        where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgDashAttr attr.DashAttr.dash)) image
instance tuneImage FillAttr        where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgFillAttr attr.FillAttr.fill)) image
instance tuneImage LineEndMarker   where tuneImage image attr = Attr` (LineMarkerAttr` {LineMarkerAttr | markerImg = attr.LineEndMarker.endmarker, markerPos = LineMarkerEnd}) image
instance tuneImage LineMidMarker   where tuneImage image attr = Attr` (LineMarkerAttr` {LineMarkerAttr | markerImg = attr.LineMidMarker.midmarker, markerPos = LineMarkerMid}) image
instance tuneImage LineStartMarker where tuneImage image attr = Attr` (LineMarkerAttr` {LineMarkerAttr | markerImg = attr.LineStartMarker.startmarker, markerPos = LineMarkerStart}) image
instance tuneImage MaskAttr        where tuneImage image attr = Attr` (MaskAttr` attr.MaskAttr.mask) image
instance tuneImage OpacityAttr     where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgFillOpacityAttr attr.OpacityAttr.opacity)) image
instance tuneImage StrokeAttr      where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgStrokeAttr      attr.StrokeAttr.stroke)) image
instance tuneImage StrokeWidthAttr where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgStrokeWidthAttr attr.StrokeWidthAttr.strokewidth)) image
instance tuneImage XRadiusAttr     where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgXRadiusAttr     attr.XRadiusAttr.xradius)) image
instance tuneImage YRadiusAttr     where tuneImage image attr = Attr` (BasicImageAttr` (BasicImgYRadiusAttr     attr.YRadiusAttr.yradius)) image

instance tuneImage DraggableAttr   where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerDraggableAttr   attr)) image
instance tuneImage OnClickAttr     where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnClickAttr     attr)) image
instance tuneImage OnMouseDownAttr where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseDownAttr attr)) image
instance tuneImage OnMouseMoveAttr where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseMoveAttr attr)) image
instance tuneImage OnMouseOutAttr  where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseOutAttr  attr)) image
instance tuneImage OnMouseOverAttr where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseOverAttr attr)) image
instance tuneImage OnMouseUpAttr   where tuneImage image attr = Attr` (HandlerAttr` (ImgEventhandlerOnMouseUpAttr   attr)) image

class margin a where
  margin :: !a !(Image m) -> Image m

instance margin Span where
  margin sp im = margin (sp, sp, sp, sp) im

instance margin (!Span, !Span) where
  margin (sp1, sp2) im = margin (sp1, sp2, sp1, sp2) im

instance margin (!Span, !Span, !Span) where
  margin (sp1, sp2, sp3) im = margin (sp1, sp2, sp3, sp2) im

instance margin (!Span, !Span, !Span, !Span) where
  margin (sp1, sp2, sp3, sp4) image = Margin` {Margins` | n=sp1, e=sp2, s=sp3, w=sp4} image

tag :: !*ImageTag !(Image m) -> Image m
tag t image = Tag` t image

tagWithSrc :: !*TagSource !(Image m) -> *(!(!Image m, !ImageTag), !*TagSource)
tagWithSrc [(nut, t) : tsrc] image
  = ((Tag` t image, nut), tsrc)
tagWithSrc _ _ = abort "empty TagSource in tagWithSrc\n"
