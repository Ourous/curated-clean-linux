definition module Graphics.Scalable.Internal.Image`

from Data.Maybe    import :: Maybe
from Data.Set      import :: Set
from Data.Map      import :: Map
from Data.Error    import :: MaybeError (..)
from Math.Geometry import :: Angle
from StdOverloaded import class zero (..), class + (..), class -  (..), class ~ (..), class sign (..), 
                          class abs  (..), class < (..), class == (..), class toReal (..), class / (..), class * (..)
import Graphics.Scalable.Types
import Graphics.Scalable.Internal.Types

:: Image` m
  = Empty`    !Span !Span
  | Circle`   !Span
  | Ellipse`  !Span !Span
  | Square`   !Span
  | Rect`     !Span !Span
  | Polyline` ![ImageSpan]
  | Polygon`  ![ImageSpan]
  | Text`     !FontDef !String
  | Rotate`   !Angle          !(Image` m)
  | Flipx`                    !(Image` m)
  | Flipy`                    !(Image` m)
  | Fit`      !Span !Span     !(Image` m)
  | Fitx`     !Span           !(Image` m)
  | Fity`     !Span           !(Image` m)
  | Scale`    !Real !Real     !(Image` m)
  | Skewx`    !Angle          !(Image` m)
  | Skewy`    !Angle          !(Image` m)
  | Attr`     !(ImageAttr` m) !(Image` m)
  | Margin`   !Margins`       !(Image` m)
  | Tag`      !ImageTag       !(Image` m)
  | Overlay`                             ![XYAlign]                 ![ImageOffset] ![Image` m] !(Host` m)
  | Grid`     !GridDimension !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image` m] !(Host` m)
:: ImageAttr` m
  = BasicImageAttr` !BasicImgAttr
  | LineMarkerAttr` !(LineMarkerAttr m)
  | MaskAttr`       !(Image` m)
  | HandlerAttr`    !(ImgEventhandler m)
:: ImgTables m
  = { imgEventhandlers :: !ImgEventhandlers m
    , imgNewFonts      :: !ImgFonts
    , imgNewTexts      :: !ImgTexts
    , imgMasks         :: !ImgMasks
    , imgLineMarkers   :: !ImgLineMarkers
    , imgPaths         :: !ImgPaths
    , imgSpans         :: !ImgSpans
    , imgGrids         :: !GridSpans
    , imgTags          :: !ImgTags
    , imgUniqIds       :: !ImgTagNo
    }
:: FontSpans          :== Map FontDef FontDescent              // of each font, the font descent
:: ImgFonts           :== Set FontDef                          // the collection of fonts used in the image for which no metrics are available
:: TextSpans          :== Map FontDef (Map String TextSpan)    // of each font, of each text of that font, the width
:: ImgEventhandlers m :== Map ImgTagNo [ImgEventhandler m]     // the registered event handlers of the image identified by the id (once registered, an event handler can not be overruled)
:: ImgTexts           :== Map FontDef (Set String)             // of each font, the collection of texts
:: ImgMasks           :== Map ImgTagNo Img                     // of each mask, the mask-image (associate the id with (MaskImg id))
:: ImgLineMarkers     :== Map ImgTagNo LineMarkers             // of each poly(gon/line) with markers, its markers
:: ImgPaths           :== Map ImgTagNo ImgPath                 // of each poly(gon/line), initially its connecting points, and secondly, its span
:: ImgSpans           :== Map ImgTagNo ImageSpan               // of each image, its (width,height)
:: GridSpans          :== Map ImgTagNo GridSpan                // of each grid, the spans of its columns and the spans of its rows
:: ImgTags            :== Map ImageTag ImgTagNo                // map user-tag to system number
:: FontDescent        :== Real
:: TextSpan           :== Real
:: ImgEventhandler m
  = ImgEventhandlerOnClickAttr     !(OnClickAttr     m)
  | ImgEventhandlerOnMouseDownAttr !(OnMouseDownAttr m)
  | ImgEventhandlerOnMouseUpAttr   !(OnMouseUpAttr   m)
  | ImgEventhandlerOnMouseOverAttr !(OnMouseOverAttr m)
  | ImgEventhandlerOnMouseMoveAttr !(OnMouseMoveAttr m)
  | ImgEventhandlerOnMouseOutAttr  !(OnMouseOutAttr  m)
  | ImgEventhandlerDraggableAttr   !(DraggableAttr   m)
:: ImgPath
  = { pathPoints   :: ![ImageOffset]                           // the connecting points of the path
    , pathSpan     :: !ImageSpan                               // the span of the path (also stored in imgSpans after resolving span-expressions)
    }
:: LineMarkers
  = { lineStart    :: !Maybe Img
    , lineMid      :: !Maybe Img
    , lineEnd      :: !Maybe Img
    }
:: GridSpan
  = { col_spans    :: ![Span]
    , row_spans    :: ![Span]
    }
:: Img
  = { uniqId       :: !ImgTagNo                                // the unique system identification within the entire image
    , host         :: !HostImg                                 // the host of this image
    , transform    :: !Maybe ImgTransform                      // the optional transform of the basic/composite image
    , overlays     :: ![Img]                                   // the back-to-front ordering of images 'on top of' host
    , offsets      :: ![ImageOffset]                           // the offsets matching one-by-one with .overlays
    }
:: HostImg
  = BasicHostImg !BasicImg !(Set BasicImgAttr)
  | RawHostImg   !String
  | CompositeImg !Img
:: BasicImg
  = EmptyImg
  | TextImg      !FontDef !String
  | CircleImg
  | RectImg
  | EllipseImg
  | PolylineImg
  | PolygonImg
:: BasicImgAttr                                                // attributes that are applicable only on basic images
  = BasicImgDashAttr          ![Int]
  | BasicImgFillAttr          !SVGColor
  | BasicImgFillOpacityAttr   !Real
  | BasicImgStrokeAttr        !SVGColor
  | BasicImgStrokeOpacityAttr !Real
  | BasicImgStrokeWidthAttr   !Span
  | BasicImgXRadiusAttr       !Span
  | BasicImgYRadiusAttr       !Span
:: LineMarkerAttr m
  = { markerImg :: !Image` m
    , markerPos :: !LineMarkerPos
    }
:: ImgTransform
  = RotateImg !Angle
  | SkewXImg  !Angle
  | SkewYImg  !Angle
  | FitImg    !Span !Span
  | FitXImg   !Span
  | FitYImg   !Span
  | ScaleImg  !Real !Real
  | FlipXImg
  | FlipYImg
  | MaskImg   !ImgTagNo                                        // the id-img pair is stored in the ImgMasks table
:: Host` m
  = NoHost`
  | Host` (Image` m)

:: Markers` m
  = { markerStart` :: !Maybe (Image` m)
    , markerMid`   :: !Maybe (Image` m)
    , markerEnd`   :: !Maybe (Image` m)
    }
defaultMarkers`    :: Markers` m
defaultLineMarkers :: LineMarkers

:: Margins`
  = { n :: !Span, e :: !Span, s :: !Span, w :: !Span }
defaultMargins`     :: Margins`

toImg :: !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)

:: SpanResolveError :== String

resolve_all_spans :: !ImgTags !FontSpans !TextSpans !Img !ImgMasks !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans
                    -> MaybeError SpanResolveError (!Img,!ImgMasks,!ImgLineMarkers,!ImgPaths,!ImgSpans,!GridSpans)
