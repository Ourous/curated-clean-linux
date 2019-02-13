implementation module Graphics.Scalable.Internal.Image`

import StdMisc
import StdBool
import StdFunc
import StdClass
import StdTuple
import StdOverloaded
import StdString
import StdInt
import StdReal
import StdOrdList

import Data.List
import Data.GenEq
import Data.Maybe
import Data.Error
import Data.Functor
import Data.Monoid
import Data.MapCollection
import Math.Geometry
import Graphics.Scalable.Types
import Graphics.Scalable.Internal.Types

import qualified Data.Foldable

import qualified Data.Set
from Data.Set import instance Foldable Set
import qualified Data.Map

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
:: Margins`
  = { n            :: !Span
    , e            :: !Span
    , s            :: !Span
    , w            :: !Span
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

derive gEq ImgTransform, Span, LookupSpan, BasicImg, FontDef, BasicImgAttr, Angle, ImageTag
instance == ImgTransform where == a b = a === b

equivImg :: !Img !Img -> Bool
equivImg {Img | transform = tfs,  offsets = offs,  host = h,  overlays = overs }
         {Img | transform = tfs`, offsets = offs`, host = h`, overlays = overs`}
	= tfs == tfs` && offs === offs` && equivHostImg h h` && gEq{|*->*|} equivImg overs overs`

equivHostImg :: !HostImg !HostImg -> Bool
equivHostImg (BasicHostImg basic attrs) (BasicHostImg basic` attrs`) = basic === basic` && attrs === attrs`
equivHostImg (RawHostImg   txt)         (RawHostImg   txt`)          = txt == txt`
equivHostImg (CompositeImg img)         (CompositeImg img`)          = equivImg img img`
equivHostImg _                          _                            = False

toImg :: !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
toImg (Empty`    w h)                                font_spans text_spans tables = empty`      w h                              font_spans text_spans tables
toImg (Text`     fontDef txt)                        font_spans text_spans tables = text`       fontDef txt                      font_spans text_spans tables
toImg (Circle`     r)                                font_spans text_spans tables = circle`       r                              font_spans text_spans tables
toImg (Ellipse`  w h)                                font_spans text_spans tables = ellipse`    w h                              font_spans text_spans tables
toImg (Square`     e)                                font_spans text_spans tables = square`       e                              font_spans text_spans tables
toImg (Rect`     w h)                                font_spans text_spans tables = rect`       w h                              font_spans text_spans tables
toImg (Polyline` ps)                                 font_spans text_spans tables = polyline`   ps                               font_spans text_spans tables
toImg (Polygon`  ps)                                 font_spans text_spans tables = polygon`    ps                               font_spans text_spans tables
toImg (Rotate`     a img)                            font_spans text_spans tables = rotate`     a img                            font_spans text_spans tables
toImg (Flipx`        img)                            font_spans text_spans tables = flipx`        img                            font_spans text_spans tables
toImg (Flipy`        img)                            font_spans text_spans tables = flipy`        img                            font_spans text_spans tables
toImg (Fit`      w h img)                            font_spans text_spans tables = fit`      w h img                            font_spans text_spans tables
toImg (Fitx`     w   img)                            font_spans text_spans tables = fitx`       w img                            font_spans text_spans tables
toImg (Fity`       h img)                            font_spans text_spans tables = fity`       h img                            font_spans text_spans tables
toImg (Scale`  rw rh img)                            font_spans text_spans tables = scale`  rw rh img                            font_spans text_spans tables
toImg (Skewx`      a img)                            font_spans text_spans tables = skewx`      a img                            font_spans text_spans tables
toImg (Skewy`      a img)                            font_spans text_spans tables = skewy`      a img                            font_spans text_spans tables
toImg (Overlay` aligns offsets imgs h)               font_spans text_spans tables = overlay` aligns offsets imgs h               font_spans text_spans tables
toImg (Grid` dim layout aligns ws hs offsets imgs h) font_spans text_spans tables = grid` dim layout aligns ws hs offsets imgs h font_spans text_spans tables
toImg (Attr`    attr img)                            font_spans text_spans tables = attr`    attr img                            font_spans text_spans tables
toImg (Margin`    ms img)                            font_spans text_spans tables = margin`    ms img                            font_spans text_spans tables
toImg (Tag`        t img)                            font_spans text_spans tables = tag`        t img                            font_spans text_spans tables

toImgs :: ![Image` m] !FontSpans !TextSpans !(ImgTables m) -> (![Img],!ImgTables m)
toImgs images font_spans text_spans imgTables
	= strictTRMapSt (\img tables -> toImg img font_spans text_spans tables) images imgTables

defaultFilledImgAttributes :: Set BasicImgAttr
defaultFilledImgAttributes
	= 'Data.Set'.fromList [ BasicImgStrokeAttr      (toSVGColor "black")
                    , BasicImgStrokeWidthAttr (PxSpan 1.0)
                    , BasicImgFillAttr        (toSVGColor "black")
                    , BasicImgFillOpacityAttr 1.0
                    ]

defaultOutlineImgAttributes :: Set BasicImgAttr
defaultOutlineImgAttributes
	= 'Data.Set'.fromList [ BasicImgFillAttr        (toSVGColor "none")
                    , BasicImgStrokeAttr      (toSVGColor "black")
                    , BasicImgStrokeWidthAttr (PxSpan 1.0)
                    ]

defaultMargins` :: Margins`
defaultMargins` = {Margins` | n=zero, e=zero, s=zero, w=zero}

defaultMarkers` :: Markers` m
defaultMarkers` = {Markers` | markerStart` = Nothing, markerMid` = Nothing, markerEnd` = Nothing}

mkBasicHostImg :: !ImgTagNo !BasicImg !(Set BasicImgAttr) -> Img
mkBasicHostImg no basicImg atts = {Img | uniqId    = no
                                       , host      = BasicHostImg basicImg atts
                                       , transform = Nothing
                                       , overlays  = []
                                       , offsets   = []
                                  }

mkTransformImg :: !ImgTagNo !Img !ImgTransform -> Img
mkTransformImg no img tf = {Img | uniqId    = no
                                , host      = CompositeImg img
                                , transform = Just tf
                                , overlays  = []
                                , offsets   = []
                           }

isPathHostImg :: !HostImg -> Bool
isPathHostImg (BasicHostImg img _)
	= case img of
	    PolylineImg = True
	    PolygonImg  = True
	    _           = False
isPathHostImg _ = False

normalizePolyPoints :: ![ImageOffset] -> [ImageOffset]
normalizePolyPoints offsets
  #! minX = minSpan (strictTRMap fst offsets)
  #! minY = minSpan (strictTRMap snd offsets)
  = strictTRMap (\(x, y) -> (x - minX, y - minY)) offsets

getLineMarkers :: !(Markers` m) !FontSpans !TextSpans !(ImgTables m) -> (!LineMarkers,!ImgTables m)
getLineMarkers {Markers` | markerStart`, markerMid`, markerEnd`} font_spans text_spans imgTables
  #! (startm,imgTables) = getMarkerImg markerStart` font_spans text_spans imgTables
  #! (midm,  imgTables) = getMarkerImg markerMid`   font_spans text_spans imgTables
  #! (endm,  imgTables) = getMarkerImg markerEnd`   font_spans text_spans imgTables
  = ({LineMarkers | lineStart = startm, lineMid = midm, lineEnd = endm},imgTables)
where
	getMarkerImg :: !(Maybe (Image` m)) !FontSpans !TextSpans !(ImgTables m) -> (!Maybe Img,!ImgTables m)
	getMarkerImg Nothing _ _ imgTables
	  = (Nothing,imgTables)
	getMarkerImg (Just image) font_spans text_spans imgTables
	  #! (marker,imgTables) = toImg image font_spans text_spans imgTables
	  = (Just marker,imgTables)

grid_dimension :: !GridDimension !Int -> (!Int,!Int)
grid_dimension (Rows    no) no_of_elts = let no` = max 1 no in (no_of_elts / no` + sign (no_of_elts rem no`), no`)
grid_dimension (Columns no) no_of_elts = let no` = max 1 no in (no`, no_of_elts / no` + sign (no_of_elts rem no`))

grid_layout :: !(!Int,!Int) !GridLayout ![a] -> [[a]]
grid_layout (no_of_cols,no_of_rows) (major,xlayout,ylayout) cells
  #! cells = case major of
               RowMajor = chop no_of_cols cells
               column   = transpose (chop no_of_rows cells)
  = case (xlayout,ylayout) of
      (LeftToRight, TopToBottom) = cells
      (RightToLeft, TopToBottom) = strictTRMap reverseTR cells
      (LeftToRight, BottomToTop) = reverseTR cells
      (RightToLeft, BottomToTop) = strictTRMapRev reverseTR cells

perhaps_look_up_span :: !Span !ImgTagNo !(ImageTag -> LookupSpan) -> Span
perhaps_look_up_span span no spanf
| isPxSpan span = span
| otherwise     = LookupSpan (spanf (ImageTagSystem no))

bounding_box_of_spans :: ![(ImgTagNo,ImageSpan)] -> ImageSpan
bounding_box_of_spans []
	= (zero,zero)
bounding_box_of_spans spans
	= ( if (isPxSpan immediate_w) immediate_w (maxSpan [perhaps_look_up_span w no ImageXSpan \\ w <- ws & no <- img_nos])
	  , if (isPxSpan immediate_h) immediate_h (maxSpan [perhaps_look_up_span h no ImageYSpan \\ h <- hs & no <- img_nos])
	  )
where
	(img_nos,img_spans) = unzip spans
	(ws, hs)            = unzip img_spans
	immediate_w         = maxSpan ws
	immediate_h         = maxSpan hs

positive_span :: !Span -> Span
positive_span       (PxSpan a)     = PxSpan (max zero a)
positive_span span=:(LookupSpan _) = span
positive_span span=:(AbsSpan _)    = span
positive_span span                 = maxSpan [zero,span]

empty` :: !Span !Span !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
empty` xspan yspan font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts,imgSpans = curSpans, imgUniqIds = no}
  #! (xspan`,txts) = spanImgTexts text_spans xspan txts
  #! (yspan`,txts) = spanImgTexts text_spans yspan txts
  #! dx            = positive_span xspan`
  #! dy            = positive_span yspan`
  = ( mkBasicHostImg no EmptyImg 'Data.Set'.newSet
	, {ImgTables | imgTables & imgNewTexts = txts
	                         , imgSpans    = 'Data.Map'.put no (dx,dy) curSpans
	                         , imgUniqIds  = no-1
	  }
	)

text` :: !FontDef !String !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
text` font str font_spans text_spans imgTables=:{ImgTables | imgNewFonts = curFonts, imgNewTexts = txts, imgSpans = curSpans, imgUniqIds = no}
  #! (w,txts) = spanImgTexts text_spans (LookupSpan (TextXSpan font` str)) txts
  #! curFonts = if ('Data.Map'.member font` font_spans) curFonts ('Data.Set'.insert font` curFonts)
  = ( mkBasicHostImg no (TextImg font` str) 'Data.Set'.newSet
    , {ImgTables | imgTables & imgNewFonts = curFonts
                             , imgNewTexts = txts
                             , imgSpans    = 'Data.Map'.put no (w,PxSpan h`) curSpans
                             , imgUniqIds  = no-1
      }
    )
where
	h`      = max zero font.FontDef.fontysize
	font`   = {FontDef | font & fontysize = h`}

circle` :: !Span !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
circle` diameter font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgUniqIds = no}
  #! (diameter`,txts) = spanImgTexts text_spans diameter txts
  #! d                = positive_span diameter`
  = ( mkBasicHostImg no CircleImg defaultFilledImgAttributes
    , {ImgTables | imgTables & imgNewTexts = txts
                             , imgSpans    = 'Data.Map'.put no (d,perhaps_look_up_span d no ImageXSpan) curSpans
                             , imgUniqIds  = no-1
      }
    )

ellipse` :: !Span !Span !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
ellipse` diax diay font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgUniqIds = no}
  #! (diax`,txts) = spanImgTexts  text_spans diax txts
  #! (diay`,txts) = spanImgTexts  text_spans diay txts
  #! dx           = positive_span diax`
  #! dy           = positive_span diay`
  = ( mkBasicHostImg no EllipseImg defaultFilledImgAttributes
    , {ImgTables | imgTables & imgNewTexts = txts
                             , imgSpans    = 'Data.Map'.put no (dx,dy) curSpans
                             , imgUniqIds  = no-1
      }
    )

square` :: !Span !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
square` edge font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgUniqIds = no}
  #! (edge`,txts) = spanImgTexts text_spans edge txts
  #! dx           = positive_span edge`
  = ( mkBasicHostImg no RectImg defaultFilledImgAttributes
    , {ImgTables | imgTables & imgNewTexts = txts
                             , imgSpans    = 'Data.Map'.put no (dx,perhaps_look_up_span dx no ImageXSpan) curSpans
                             , imgUniqIds  = no-1
      }
    )

rect` :: !Span !Span !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
rect` xspan yspan font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgUniqIds = no}
  #! (xspan`,txts) = spanImgTexts  text_spans xspan txts
  #! (yspan`,txts) = spanImgTexts  text_spans yspan txts
  #! dx            = positive_span xspan`
  #! dy            = positive_span yspan`
  = ( mkBasicHostImg no RectImg defaultFilledImgAttributes
    , {ImgTables | imgTables & imgNewTexts = txts
                             , imgSpans    = 'Data.Map'.put no (dx,dy) curSpans
                             , imgUniqIds  = no-1
      }
    )

raw` :: !Span !Span !String !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
raw` xspan yspan svgStr font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgUniqIds = no}
  #! (xspan`,txts) = spanImgTexts text_spans xspan txts
  #! (yspan`,txts) = spanImgTexts text_spans yspan txts
  #! dx            = positive_span xspan`
  #! dy            = positive_span yspan`
  = ( {Img | uniqId    = no
           , host      = RawHostImg svgStr
           , transform = Nothing
           , overlays  = []
           , offsets   = []
      }
    , {ImgTables | imgTables & imgNewTexts = txts
                             , imgSpans    = 'Data.Map'.put no (dx,dy) curSpans
                             , imgUniqIds  = no-1
      }
    )

polyline` :: ![ImageOffset] !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
polyline` offsets font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgPaths = curPaths, imgUniqIds = no}
  | no_of_offsets < 2     = abort ("Graphics.Scalable.Image: polyline must be applied to at least 2 ImageOffset values instead of " +++ toString no_of_offsets)
  #! (offsets`,txts)      = offsetsImgTexts text_spans offsets txts
  #! offsets``            = normalizePolyPoints offsets`
  #! dx                   = maxSpan (strictTRMap fst offsets``)
  #! dy                   = maxSpan (strictTRMap snd offsets``)
  = ( mkBasicHostImg no PolylineImg defaultOutlineImgAttributes
    , {ImgTables | imgTables & imgPaths    = 'Data.Map'.put no {ImgPath | pathPoints = offsets``, pathSpan = (dx,dy)} curPaths
                             , imgSpans    = 'Data.Map'.put no (perhaps_look_up_span dx no PathXSpan,perhaps_look_up_span dy no PathYSpan) curSpans
                             , imgNewTexts = txts
                             , imgUniqIds  = no-1
      }
    )
where
	no_of_offsets = length offsets

polygon` :: ![ImageOffset] !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
polygon` offsets font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgPaths = curPaths, imgUniqIds = no}
| no_of_offsets < 3       = abort ("Graphics.Scalable.Image: polygon must be applied to at least 3 ImageOffset values instead of " +++ toString no_of_offsets)
  #! (offsets`,txts)      = offsetsImgTexts text_spans offsets txts
  #! offsets``            = normalizePolyPoints offsets`
  #! dx                   = maxSpan (strictTRMap fst offsets``)
  #! dy                   = maxSpan (strictTRMap snd offsets``)
  = ( mkBasicHostImg no PolygonImg defaultFilledImgAttributes
    , {ImgTables | imgTables & imgPaths    = 'Data.Map'.put no {ImgPath | pathPoints = offsets``, pathSpan = (dx,dy)} curPaths
                             , imgSpans    = 'Data.Map'.put no (perhaps_look_up_span dx no PathXSpan,perhaps_look_up_span dy no PathYSpan) curSpans
                             , imgNewTexts = txts
                             , imgUniqIds  = no-1
      }
    )
where
	no_of_offsets = length offsets

rotate` :: !Angle !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
rotate` a image font_spans text_spans imgTables=:{ImgTables | imgUniqIds = no}
  #! (img,imgTables`=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgUniqIds = no-1}
  = ( mkTransformImg no img (RotateImg (normalize a))
    , {ImgTables | imgTables` & imgSpans = 'Data.Map'.put no ('Data.Map'.find img.Img.uniqId curSpans) curSpans}    // span of (rotate img) = span of img
    )

flipx` :: !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
flipx` image font_spans text_spans imgTables=:{ImgTables | imgUniqIds = no}
  #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgUniqIds = no-1}
  = ( mkTransformImg no img FlipXImg
    , {ImgTables | imgTables & imgSpans = 'Data.Map'.put no ('Data.Map'.find img.Img.uniqId curSpans) curSpans}    // span of (flipx img) = span of img
    )

flipy` :: !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
flipy` image font_spans text_spans imgTables=:{ImgTables | imgUniqIds = no}
  #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgUniqIds = no-1}
  = ( mkTransformImg no img FlipYImg
    , {ImgTables | imgTables & imgSpans = 'Data.Map'.put no ('Data.Map'.find img.Img.uniqId curSpans) curSpans}    // span of (flipy img) = span of img
    )

fit` :: !Span !Span !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
fit` xspan yspan image font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgSpans = curSpans, imgUniqIds = no}
  #! (xspan`,txts)   = spanImgTexts text_spans xspan txts
  #! (yspan`,txts)   = spanImgTexts text_spans yspan txts
  #! dx              = positive_span xspan`
  #! dy              = positive_span yspan`
  #! (img,imgTables) = toImg image font_spans text_spans {ImgTables | imgTables & imgNewTexts = txts
                                                                                , imgSpans    = 'Data.Map'.put no (dx,dy) curSpans
                                                                                , imgUniqIds  = no-1
                                                         }
  = ( mkTransformImg no img (FitImg dx dy), imgTables )

fitx` :: !Span !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
fitx` xspan image font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgUniqIds = no}
  #! (xspan`,txts)   = spanImgTexts text_spans xspan txts
  #! dx              = positive_span xspan`
  #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgNewTexts = txts, imgUniqIds = no-1}
  #! (oldx,oldy) = 'Data.Map'.find img.Img.uniqId curSpans
  = ( mkTransformImg no img (FitXImg dx)
    , {ImgTables | imgTables & imgSpans = 'Data.Map'.put no (dx,oldy * (dx / oldx)) curSpans}
    )

fity` :: !Span !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
fity` yspan image font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgUniqIds = no}
  #! (yspan`,txts)   = spanImgTexts text_spans yspan txts
  #! dy              = positive_span yspan`
  #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgNewTexts = txts, imgUniqIds = no-1}
  #! (oldx,oldy) = 'Data.Map'.find img.Img.uniqId curSpans
  = ( mkTransformImg no img (FitYImg dy)
    , {ImgTables | imgTables & imgSpans = 'Data.Map'.put no (oldx * (dy / oldy),dy) curSpans}
    )

scale` :: !Real !Real !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
scale` fx fy image font_spans text_spans imgTables=:{ImgTables | imgNewTexts = curTexts, imgUniqIds = no}
  #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgUniqIds = no-1}
  #! (dx,dy) = 'Data.Map'.find img.Img.uniqId curSpans
  = ( mkTransformImg no img (ScaleImg fx` fy`)
    , {ImgTables | imgTables & imgSpans = 'Data.Map'.put no (dx *. fx`, dy *. fy`) curSpans}
    )
where
	fx` = max zero fx
	fy` = max zero fy

skewx` :: !Angle !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
skewx` a image font_spans text_spans imgTables=:{ImgTables | imgNewTexts = curTexts, imgUniqIds = no}
  #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgUniqIds = no-1}
  = ( mkTransformImg no img (SkewXImg (normalize a))
    , {ImgTables | imgTables & imgSpans = 'Data.Map'.put no ('Data.Map'.find img.Img.uniqId curSpans) curSpans}
    )

skewy` :: !Angle !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
skewy` a image font_spans text_spans imgTables=:{ImgTables | imgNewTexts = curTexts, imgUniqIds = no}
  #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgUniqIds = no-1}
  = ( mkTransformImg no img (SkewYImg (normalize a))
    , {ImgTables | imgTables & imgSpans = 'Data.Map'.put no ('Data.Map'.find img.Img.uniqId curSpans) curSpans}
    )

margin` :: !Margins` !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
margin` {Margins` | n,e,s,w} image font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgUniqIds = no}
  #! (nesw,txts)      = strictTRMapSt (spanImgTexts text_spans) [n,e,s,w] txts
  = case nesw of
    [n,e,s,w:_]
      #! (img,imgTables=:{ImgTables | imgSpans = curSpans}) = toImg image font_spans text_spans {ImgTables | imgTables & imgNewTexts = txts,imgUniqIds = no-1}
      #! (img_w,img_h)    = 'Data.Map'.find img.Img.uniqId curSpans
      #! span_host        = (w + img_w + e, n + img_h + s)
      = ({Img | uniqId    = no
              , host      = BasicHostImg EmptyImg 'Data.Set'.newSet
              , transform = Nothing
              , overlays  = [img]
              , offsets   = [(w,n)]
         }
        ,{ImgTables | imgTables & imgSpans = 'Data.Map'.put no span_host curSpans}
        )
    _ = abort "error in margin`\n"

overlay` :: ![XYAlign] ![ImageOffset] ![Image` m] !(Host` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
overlay` aligns offsets images host font_spans text_spans imgTables
  #! l        = length images
  #! aligns`  = take l (aligns ++ repeat (AtLeft,AtTop))
  #! offsets` = take l (offsets ++ repeat (zero, zero))
  = overlay aligns` offsets` images host font_spans text_spans imgTables
where
	overlay :: ![XYAlign] ![ImageOffset] ![Image` m] !(Host` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
	overlay aligns offsets images NoHost` font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgUniqIds = no}
	  #! (offsets,txts)   = offsetsImgTexts text_spans offsets txts
	  #! (imgs,imgTables=:{ImgTables | imgSpans = curSpans})
	                      = toImgs images font_spans text_spans {ImgTables | imgTables & imgNewTexts = txts, imgUniqIds = no-1}
	  #! span_imgs        = [(uniqId,'Data.Map'.find uniqId curSpans) \\ {Img | uniqId} <- imgs]
	  #! span_host        = bounding_box_of_spans span_imgs
	  = ({Img | uniqId    = no
	          , host      = BasicHostImg EmptyImg 'Data.Set'.newSet
	          , transform = Nothing
	          , overlays  = imgs
	          , offsets   = [ offset_within_host span_img align offset span_host
	                        \\ span_img <- span_imgs & align <- aligns & offset <- offsets
	                        ]
	     }
	    ,{ImgTables | imgTables & imgSpans = 'Data.Map'.put no span_host curSpans}
	    )
	overlay aligns offsets images (Host` image) font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgUniqIds = no}
	  #! (offsets,txts)   = offsetsImgTexts text_spans offsets txts
	  #! (imgs,imgTables) = toImgs images font_spans text_spans {ImgTables | imgTables & imgNewTexts = txts, imgUniqIds = no-1}
	  #! (host,imgTables=:{ImgTables | imgSpans = curSpans})
	                      = toImg image font_spans text_spans imgTables
	  #! span_imgs        = [(uniqId,'Data.Map'.find uniqId curSpans) \\ {Img | uniqId} <- imgs]
	  #! span_host        = 'Data.Map'.find host.Img.uniqId curSpans
	  = ({Img | uniqId    = no
	          , host      = CompositeImg host
	          , transform = Nothing
	          , overlays  = imgs
	          , offsets   = [ offset_within_host span_img align offset span_host
	                        \\ span_img <- span_imgs & align <- aligns & offset <- offsets
	                        ]
	     }
	    ,{ImgTables | imgTables & imgSpans = 'Data.Map'.put no span_host curSpans}
	    )
	
	offset_within_host :: !(!ImgTagNo,!ImageSpan) !XYAlign !ImageOffset !ImageSpan -> ImageOffset
	offset_within_host (no,(x_img,y_img)) (x_align,y_align) (x_offset,y_offset) (x_host,y_host)
		= (x_offset + x_offset_within_host x_img x_align x_host
		  ,y_offset + y_offset_within_host y_img y_align y_host
		  )
	where
		x_offset_within_host :: !Span !XAlign !Span -> Span
		x_offset_within_host x_img AtLeft    width = zero
		x_offset_within_host x_img AtMiddleX width = (width - x_img) /. 2
		x_offset_within_host x_img AtRight   width = width - x_img
		
		y_offset_within_host :: !Span !YAlign !Span -> Span
		y_offset_within_host y_img AtTop     height = zero
		y_offset_within_host y_img AtMiddleY height = (height - y_img) /. 2
		y_offset_within_host y_img AtBottom  height = height - y_img

grid` :: !GridDimension !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image` m] !(Host` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
grid` dimension layout aligns column_widths row_heights offsets images host font_spans text_spans imgTables
  #! l                       = length images
  #! (no_of_cols,no_of_rows) = grid_dimension dimension l
  #! no_of_cells             = no_of_cols * no_of_rows
  #! aligns`                 = take no_of_cells (aligns  ++ repeat (AtLeft,AtTop))
  #! offsets`                = take no_of_cells (offsets ++ repeat (zero,zero))
  #! images`                 = take no_of_cells (images  ++ repeat (Empty` zero zero))
  = grid (no_of_cols,no_of_rows) layout aligns` column_widths row_heights offsets` images` host font_spans text_spans imgTables
where
	grid :: !(!Int,!Int) !GridLayout ![XYAlign] ![Span] ![Span] ![ImageOffset] ![Image` m] !(Host` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
	grid (no_of_cols,no_of_rows) layout aligns column_widths row_heights offsets images h font_spans text_spans imgTables=:{ImgTables | imgNewTexts = txts, imgUniqIds = no}
	  #! (offsets,      txts)          = offsetsImgTexts text_spans offsets txts
	  #! (column_widths,txts)          = strictTRMapSt (spanImgTexts text_spans) column_widths txts
	  #! (row_heights,  txts)          = strictTRMapSt (spanImgTexts text_spans) row_heights   txts
	  #! (imgs,imgTables=:{ImgTables | imgSpans = curSpans})
	                                   = toImgs images font_spans text_spans {ImgTables | imgTables & imgNewTexts = txts, imgUniqIds = no-1}
	  #! imgid_span_align_offsets      = [(uniqId,'Data.Map'.find uniqId curSpans,align,offset) \\ {Img | uniqId} <- imgs & align <- aligns & offset <- offsets]
	  #! imgid_span_align_offsets_grid = grid_layout (no_of_cols,no_of_rows) layout imgid_span_align_offsets
	  #! cell_spans                    = [[span \\ (_,span,_,_) <- row] \\ row <- imgid_span_align_offsets_grid]
	  #! grid_widths                   = constrain column_widths (cols_widths  cell_spans)
	  #! grid_heights                  = constrain row_heights   (rows_heights cell_spans)
	  #! grid_info                     = {col_spans = grid_widths, row_spans = grid_heights}
	  #! (grid_width,grid_height)      = (sum grid_widths, sum grid_heights)
	  #! (host,imgTables=:{ImgTables | imgSpans = newSpans, imgGrids = newGrids})
	                                   = case h of
	                                       Host` image = toImg image font_spans text_spans imgTables
	                                       no_host     = empty` grid_width grid_height font_spans text_spans imgTables
	  #! span_host                     = case h of
	                                       Host` _     = 'Data.Map'.find host.Img.uniqId newSpans
	                                       no_host     = (grid_width,grid_height)
	  #! imgid_offsets                 = offsets_within_grid grid_widths grid_heights imgid_span_align_offsets_grid
	  #! offsets                       = associate_offset_with_img imgid_offsets imgs
	  = ({Img | uniqId    = no
	          , host      = CompositeImg host
	          , transform = Nothing
	          , overlays  = imgs
	          , offsets   = offsets
	     }
	    ,{ImgTables | imgTables & imgSpans = 'Data.Map'.put no span_host newSpans
	                            , imgGrids = 'Data.Map'.put no grid_info newGrids
	     }
	    )
	
	cols_widths :: ![[ImageSpan]] -> [Span]
	cols_widths cells = [maxSpan (map fst column) \\ column <- transpose cells]
	
	rows_heights :: ![[ImageSpan]] -> [Span]
	rows_heights cells = [maxSpan (map snd row) \\ row <- cells]
	
	offsets_within_grid :: ![Span] ![Span] ![[(ImgTagNo,ImageSpan,XYAlign,ImageOffset)]] -> [[(ImgTagNo,ImageOffset)]]
	offsets_within_grid grid_widths grid_heights cells
		= [ [  offset_within_grid col_offset col_width row_offset row_height cell 
		    \\ cell       <- row 
		     & col_offset <- scan (+) zero grid_widths
		     & col_width  <- grid_widths
		    ] 
		  \\ row          <- cells 
		   & row_offset   <- scan (+) zero grid_heights
		   & row_height   <- grid_heights
		  ]
	where
		offset_within_grid :: !Span !Span !Span !Span !(!ImgTagNo,!ImageSpan,!XYAlign,!ImageOffset) -> (!ImgTagNo,!ImageOffset)
		offset_within_grid column_offset column_width row_offset row_height (img_id,(img_width,img_height),(x_align,y_align),(dx,dy))
			= (img_id, (column_offset + dx + x_align_offset, row_offset + dy + y_align_offset))
		where
			x_align_offset = case x_align of
			                   AtLeft    = zero
			                   AtMiddleX = (column_width - img_width) /. 2.0
			                   AtRight   =  column_width - img_width
			y_align_offset = case y_align of
			                   AtTop     = zero
			                   AtMiddleY = (row_height - img_height) /. 2.0
			                   AtBottom  =  row_height - img_height
	
	associate_offset_with_img :: ![[(ImgTagNo,ImageOffset)]] ![Img] -> [ImageOffset]
	associate_offset_with_img cells imgs = [fromJust (lookup uniqId offsets) \\ {Img | uniqId} <- imgs]
	where
		offsets = flatten cells

attr` :: !(ImageAttr` m) !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
attr` (BasicImageAttr` attr) image font_spans text_spans imgTables
  #! (img,imgTables=:{ImgTables | imgNewTexts = txts}) = toImg image font_spans text_spans imgTables
  #! (attr`,txts) = imgAttrTexts text_spans attr txts
  #! img`         = {Img | img & host = add_basic_attribute attr` img.Img.host}
  = (img`,{ImgTables | imgTables & imgNewTexts = txts})
where	
	add_basic_attribute :: !BasicImgAttr !HostImg -> HostImg
	add_basic_attribute attr (BasicHostImg img attrs) = BasicHostImg img ('Data.Set'.insert attr attrs)
	add_basic_attribute _    host = host
attr` (LineMarkerAttr` {LineMarkerAttr | markerImg,markerPos}) image font_spans text_spans imgTables
  #! (mark,imgTables)                             = toImg markerImg font_spans text_spans imgTables
  #! (img, imgTables=:{imgLineMarkers = markers}) = toImg image     font_spans text_spans imgTables
  | isPathHostImg img.Img.host                    = (img,{ImgTables | imgTables & imgLineMarkers = 'Data.Map'.alter (add_line_marker mark markerPos) img.Img.uniqId markers})
  | otherwise                                     = (img,imgTables)
where
	add_line_marker :: !Img !LineMarkerPos !(Maybe LineMarkers) -> Maybe LineMarkers
	add_line_marker mark pos Nothing              = Just (setLineMarker pos mark defaultLineMarkers)
	add_line_marker mark pos (Just markers)       = Just (setLineMarker pos mark markers)
attr` (MaskAttr` mask) image font_spans text_spans imgTables=:{ImgTables | imgUniqIds = no}
  #! (img,imgTables) = toImg image font_spans text_spans {ImgTables | imgTables & imgUniqIds = no-1}
  #! (m`, imgTables=:{ImgTables | imgMasks = curMasks, imgSpans = curSpans}) = toImg mask font_spans text_spans imgTables
  #! (mask_key,masks) = case 'Data.Map'.findKeyWith (equivImg m`) curMasks of
                           Just k  = (k, curMasks)                                                  // similar mask already present, so use it's identification
                           nothing = (m`.Img.uniqId, 'Data.Map'.put m`.Img.uniqId m` curMasks)            // similar mask not yet present, so add it to mask collection
  = ( mkTransformImg no img (MaskImg mask_key)                                                      // this *must* be the id of the mask image, because for that an svg-definition is generated
    , {ImgTables | imgTables & imgMasks = masks
                             , imgSpans = 'Data.Map'.put no ('Data.Map'.find img.Img.uniqId curSpans) curSpans  // span of (mask m img) = span of img
      }
    )
attr` (HandlerAttr` attr) image font_spans text_spans imgTables
  #! (img,imgTables=:{imgEventhandlers = es}) = toImg image font_spans text_spans imgTables
  = (img,{ImgTables | imgTables & imgEventhandlers = 'Data.Map'.alter (add_new_eventhandler attr) img.Img.uniqId es})
where
	add_new_eventhandler :: !(ImgEventhandler m) !(Maybe [ImgEventhandler m]) -> Maybe [ImgEventhandler m]
	add_new_eventhandler h Nothing  = Just [h]
	add_new_eventhandler h (Just hs)
	| any (match_eventhandler h) hs = Just hs
	| otherwise                     = Just [h:hs]

defaultLineMarkers :: LineMarkers
defaultLineMarkers = {LineMarkers | lineStart = Nothing, lineMid = Nothing, lineEnd = Nothing}

setLineMarker :: !LineMarkerPos !Img !LineMarkers -> LineMarkers
setLineMarker LineMarkerEnd   img markers = {LineMarkers | markers & lineEnd   = Just img}
setLineMarker LineMarkerMid   img markers = {LineMarkers | markers & lineMid   = Just img}
setLineMarker LineMarkerStart img markers = {LineMarkers | markers & lineStart = Just img}

match_eventhandler :: !(ImgEventhandler m) !(ImgEventhandler m) -> Bool
match_eventhandler (ImgEventhandlerOnClickAttr     _) (ImgEventhandlerOnClickAttr     _) = True
match_eventhandler (ImgEventhandlerOnMouseDownAttr _) (ImgEventhandlerOnMouseDownAttr _) = True
match_eventhandler (ImgEventhandlerOnMouseUpAttr   _) (ImgEventhandlerOnMouseUpAttr   _) = True
match_eventhandler (ImgEventhandlerOnMouseOverAttr _) (ImgEventhandlerOnMouseOverAttr _) = True
match_eventhandler (ImgEventhandlerOnMouseMoveAttr _) (ImgEventhandlerOnMouseMoveAttr _) = True
match_eventhandler (ImgEventhandlerOnMouseOutAttr  _) (ImgEventhandlerOnMouseOutAttr  _) = True
match_eventhandler (ImgEventhandlerDraggableAttr   _) (ImgEventhandlerDraggableAttr   _) = True
match_eventhandler _ _ = False

ImgEventhandlerConsName :: !(ImgEventhandler m) -> String
ImgEventhandlerConsName (ImgEventhandlerOnClickAttr     _) = "ImgEventhandlerOnClickAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseDownAttr _) = "ImgEventhandlerOnMouseDownAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseUpAttr   _) = "ImgEventhandlerOnMouseUpAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseOverAttr _) = "ImgEventhandlerOnMouseOverAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseMoveAttr _) = "ImgEventhandlerOnMouseMoveAttr"
ImgEventhandlerConsName (ImgEventhandlerOnMouseOutAttr  _) = "ImgEventhandlerOnMouseOutAttr"
ImgEventhandlerConsName (ImgEventhandlerDraggableAttr   _) = "ImgEventhandlerDraggableAttr"

instance <  (ImgEventhandler m) where <  a b = ImgEventhandlerConsName a <  ImgEventhandlerConsName b
instance == (ImgEventhandler m) where == a b = ImgEventhandlerConsName a == ImgEventhandlerConsName b

ImgAttrConsName :: !BasicImgAttr -> String
ImgAttrConsName (BasicImgStrokeAttr        _) = "BasicImgStrokeAttr"
ImgAttrConsName (BasicImgStrokeWidthAttr   _) = "BasicImgStrokeWidthAttr"
ImgAttrConsName (BasicImgXRadiusAttr       _) = "BasicImgXRadiusAttr"
ImgAttrConsName (BasicImgYRadiusAttr       _) = "BasicImgYRadiusAttr"
ImgAttrConsName (BasicImgStrokeOpacityAttr _) = "BasicImgStrokeOpacityAttr"
ImgAttrConsName (BasicImgFillOpacityAttr   _) = "BasicImgFillOpacityAttr"
ImgAttrConsName (BasicImgFillAttr          _) = "BasicImgFillAttr"
ImgAttrConsName (BasicImgDashAttr          _) = "BasicImgDashAttr"

instance <  BasicImgAttr where <  a b = ImgAttrConsName a <  ImgAttrConsName b
instance == BasicImgAttr where == a b = ImgAttrConsName a == ImgAttrConsName b

tag` :: !ImageTag !(Image` m) !FontSpans !TextSpans !(ImgTables m) -> (!Img,!ImgTables m)
tag` t=:(ImageTagUser no label) image font_spans txt_spans imgTables
  #! (img,imgTables=:{ImgTables | imgTags = curTags}) = toImg image font_spans txt_spans imgTables
  =  (img,{ImgTables | imgTables & imgTags = 'Data.Map'.put t img.Img.uniqId curTags})
tag` (ImageTagSystem no) _ _ _ _
  = abort "Graphics.Scalable.Image: tag applied to unexpected ImageTag"


/** chop n xs = xss:
      @xss consists of the subsequent sub-lists of @xs of length @n.
      The length of the last element of @xss can be less than @n.
*/
chop :: !Int ![a] -> [[a]]
chop n [] = []
chop n xs
  #! (firstN, withoutN) = splitAt n xs
  = [firstN : chop n withoutN]

/** constrain as bs = cs:
      replace the first elements of @bs by @as.
*/
constrain :: ![a] ![a] -> [a]
constrain [a:as] [_:bs] = [a:constrain as bs]
constrain _      bs     = bs

spanImgTexts :: !TextSpans !Span !ImgTexts -> (!Span,!ImgTexts)
spanImgTexts text_spans span txts
  = case span of
      LookupSpan (TextXSpan font str) = case lookupTextSpan font str text_spans of
                                          Just w  = (PxSpan w,txts)
                                          no_info = (span,    addToMapSet font str txts)
      AddSpan sp1 sp2                 = spanImgTexts` text_spans ('Data.Foldable'.foldl1 (+)) [sp1,sp2] txts
      SubSpan sp1 sp2                 = spanImgTexts` text_spans ('Data.Foldable'.foldl1 (-)) [sp1,sp2] txts
      MulSpan sp1 sp2                 = spanImgTexts` text_spans ('Data.Foldable'.foldl1 (*)) [sp1,sp2] txts
      DivSpan sp1 sp2                 = spanImgTexts` text_spans ('Data.Foldable'.foldl1 (/)) [sp1,sp2] txts
      AbsSpan sp                      = spanImgTexts` text_spans (abs o hd)                   [sp]      txts
      MinSpan sps                     = spanImgTexts` text_spans minSpan                      sps       txts
      MaxSpan sps                     = spanImgTexts` text_spans maxSpan                      sps       txts
      span                            = (span,txts)
where
	spanImgTexts` :: !TextSpans !([Span] -> Span) ![Span] !ImgTexts -> (!Span,!ImgTexts)
	spanImgTexts` text_spans combine spans txts
		#! (spans,txts) = strictTRMapSt (spanImgTexts text_spans) spans txts
		= (combine spans,txts)

offsetsImgTexts :: !TextSpans ![ImageOffset] !ImgTexts -> (![ImageOffset],!ImgTexts)
offsetsImgTexts text_spans offsets txts
 #! (offset_elts,txts) = strictTRMapSt (spanImgTexts text_spans) (flatten (strictTRMap (\(a,b) -> [a,b]) offsets)) txts
 = ([(dx,dy) \\ [dx,dy:_] <- chop 2 offset_elts],txts)

imgAttrTexts :: !TextSpans !BasicImgAttr !ImgTexts -> (!BasicImgAttr,!ImgTexts)
imgAttrTexts text_spans (BasicImgStrokeWidthAttr span) txts = let (span`,txts`) = spanImgTexts text_spans span txts in (BasicImgStrokeWidthAttr span`,txts`)
imgAttrTexts text_spans (BasicImgXRadiusAttr     span) txts = let (span`,txts`) = spanImgTexts text_spans span txts in (BasicImgXRadiusAttr     span`,txts`)
imgAttrTexts text_spans (BasicImgYRadiusAttr     span) txts = let (span`,txts`) = spanImgTexts text_spans span txts in (BasicImgYRadiusAttr     span`,txts`)
imgAttrTexts _          attr                           txts = (attr,txts)

lookupTextSpan :: !FontDef !String !TextSpans -> Maybe Real
lookupTextSpan font str text_spans
	= case 'Data.Map'.get font text_spans of
	    Just ws = 'Data.Map'.get str ws
	    nothing = Nothing


:: SpanResolveError :== String

user_error f msg = Error (       "Error in " +++ f +++ ": " +++ msg)
sys_error  f msg = Error ("System error in " +++ f +++ ": " +++ msg)

get_col_spans :: !GridSpan -> [Span]
get_col_spans {GridSpan | col_spans} = col_spans

get_row_spans :: !GridSpan -> [Span]
get_row_spans {GridSpan | row_spans} = row_spans

set_col_spans :: ![Span] !GridSpan -> GridSpan
set_col_spans cols grid = {GridSpan | grid & col_spans = cols}

set_row_spans :: ![Span] !GridSpan -> GridSpan
set_row_spans rows grid = {GridSpan | grid & row_spans = rows}

upd_fst :: a !(a,b) -> (a,b)
upd_fst x (_,y) = (x,y)

upd_snd :: b !(a,b) -> (a,b)
upd_snd y (x,_) = (x,y)

resolve_all_spans :: !ImgTags !FontSpans !TextSpans !Img !ImgMasks !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans
                    -> MaybeError SpanResolveError (!Img,!ImgMasks,!ImgLineMarkers,!ImgPaths,!ImgSpans,!GridSpans)
resolve_all_spans user_tags font_spans text_spans img masks markers paths imgs_spans grids_spans
  #! (e1,spans) = resolveImgPaths  user_tags font_spans text_spans (paths,imgs_spans,grids_spans)
  #! (e2,spans) = resolveImgSpans  user_tags font_spans text_spans spans
  #! (e3,spans) = resolveGridSpans user_tags font_spans text_spans spans
  = case [e \\ Just e <- [e1,e2,e3]] of
      [e : _] = Error e
      _       = case resolveImgMasks user_tags font_spans text_spans masks spans of
                  (Error e,  spans) = Error e
                  (Ok masks`,spans) = case resolveImgLineMarkers user_tags font_spans text_spans markers spans of
                                        (Error e,_)         = Error e
                                        (Ok markers`,spans) = case resolveImg user_tags font_spans text_spans img spans of
                                                                (Error e,_)                                 = Error e
                                                                (Ok img`,(paths`,imgs_spans`,grids_spans`)) = Ok (img`,masks`,markers`,paths`,imgs_spans`,grids_spans`)

resolveImgPaths :: !ImgTags !FontSpans !TextSpans !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!Maybe SpanResolveError,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolveImgPaths user_tags font_spans text_spans (paths,imgs_spans,grids_spans)
  #! (rs,spans) = strictTRMapSt (resolve_span user_tags font_spans text_spans) unresolved_path_spans (paths,imgs_spans,grids_spans)
  = case [e \\ Error e <- rs] of
      [e : _] = (Just e, spans)
      _       = (Nothing,spans)
where
	unresolved_path_spans = flatten [  if (isPxSpan w) [] [LookupSpan (PathXSpan (ImageTagSystem no))] ++
	                                   if (isPxSpan h) [] [LookupSpan (PathYSpan (ImageTagSystem no))]
	                                \\ (no,{ImgPath | pathSpan=(w,h)}) <- 'Data.Map'.toList paths 
	                                ]

resolveImgSpans :: !ImgTags !FontSpans !TextSpans !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!Maybe SpanResolveError,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolveImgSpans user_tags font_spans text_spans (paths,imgs_spans,grids_spans)
  #! (rs,spans) = strictTRMapSt (resolve_span user_tags font_spans text_spans) unresolved_img_spans (paths,imgs_spans,grids_spans)
  = case [e \\ Error e <- rs] of
      [e : _] = (Just e, spans)
      _       = (Nothing,spans)
where
	unresolved_img_spans = flatten [ if (isPxSpan w) [] [imagexspan (ImageTagSystem no)] ++
	                                 if (isPxSpan h) [] [imageyspan (ImageTagSystem no)]
	                               \\ (no,(w,h)) <- 'Data.Map'.toList imgs_spans
	                               ]

resolveGridSpans :: !ImgTags !FontSpans !TextSpans !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!Maybe SpanResolveError,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolveGridSpans user_tags font_spans text_spans (paths,imgs_spans,grids_spans)
  #! (rs,spans) = strictTRMapSt (resolve_span user_tags font_spans text_spans) unresolved_grid_spans (paths,imgs_spans,grids_spans)
  = case [e \\ Error e <- rs] of
      [e : _] = (Just e, spans)
      _       = (Nothing,spans)
where
	unresolved_grid_spans = flatten [ flatten [if (isPxSpan w) [] [columnspan (ImageTagSystem no) col] \\ w <- col_spans & col <- [0..]] ++
	                                  flatten [if (isPxSpan h) [] [rowspan    (ImageTagSystem no) row] \\ h <- row_spans & row <- [0..]]
	                                \\ (no,{GridSpan | col_spans,row_spans}) <- 'Data.Map'.toList grids_spans
	                                ]

resolveImageOffset :: !ImgTags !FontSpans !TextSpans !ImageOffset !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError ImageOffset,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolveImageOffset user_tags font_spans text_spans (w,h) spans
  = case resolve_span user_tags font_spans text_spans w spans of
      (Ok w`,  spans) = case resolve_span user_tags font_spans text_spans h spans of
                          (Ok h`,  spans) = (Ok (PxSpan w`,PxSpan h`),spans)
                          (Error e,spans) = (Error e,                 spans)
      (Error e,spans) = (Error e,spans)

resolveImgMasks :: !ImgTags !FontSpans !TextSpans !ImgMasks !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError ImgMasks,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolveImgMasks user_tags font_spans text_spans masks spans
  #! (m_imgs`,spans) = strictTRMapSt (resolveImg user_tags font_spans text_spans) imgs spans
  = case [e \\ Error e <- m_imgs`] of
      [e : _]      = (Error e,spans)
      no_error     = (Ok ('Data.Map'.fromList (zip2 img_nos (map fromOk m_imgs`))),spans)
where
	(img_nos,imgs) = unzip ('Data.Map'.toList masks)

resolveImgLineMarkers :: !ImgTags !FontSpans !TextSpans !ImgLineMarkers !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError ImgLineMarkers,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolveImgLineMarkers user_tags font_spans text_spans markers spans
  #! (m_line_markers`,spans) = strictTRMapSt (resolveLineMarkers user_tags font_spans text_spans) line_markers spans
  = case [e \\ Error e <- m_line_markers`] of
      [e : _]      = (Error e, spans)
      no_error     = (Ok ('Data.Map'.fromList (zip2 m_nos (map fromOk m_line_markers`))),spans)
where
	(m_nos,line_markers) = unzip ('Data.Map'.toList markers)
		
	resolveLineMarkers :: !ImgTags !FontSpans !TextSpans !LineMarkers !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError LineMarkers,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolveLineMarkers user_tags font_spans text_spans {LineMarkers | lineStart, lineMid, lineEnd} spans
	  #! (m_lineStart`,spans) = liftMaybe (resolveImg user_tags font_spans text_spans) lineStart spans
	  #! (m_lineMid`,  spans) = liftMaybe (resolveImg user_tags font_spans text_spans) lineMid   spans
	  #! (m_lineEnd`,  spans) = liftMaybe (resolveImg user_tags font_spans text_spans) lineEnd   spans
	  = case [e \\ Just (Error e) <- [m_lineStart`,m_lineMid`,m_lineEnd`]] of
	      [e : _]  = (Error e,spans)
	      no_error = (Ok {LineMarkers | lineStart = fmap fromOk m_lineStart`, lineMid = fmap fromOk m_lineMid`, lineEnd = fmap fromOk m_lineEnd`},spans)

liftMaybe :: !(a .st -> *(b,.st)) !(Maybe a) .st -> (!Maybe b,!.st)
liftMaybe f Nothing st = (Nothing,st)
liftMaybe f (Just a) st
  #! (b,st) = f a st
  = (Just b,st)

resolveImg :: !ImgTags !FontSpans !TextSpans !Img !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError Img,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolveImg user_tags font_spans text_spans img=:{Img | host,overlays,offsets,transform} spans
	= case resolveHostImg user_tags font_spans text_spans host spans of
	    (Error e, spans)
	        = (Error e,spans)
	    (Ok host`,spans)
	        #! (m_overlays`,spans) = strictTRMapSt (resolveImg user_tags font_spans text_spans) overlays spans
	        = case [e \\ Error e <- m_overlays`] of
	            [e : _] = (Error e,spans)
	            no_error
	              #! (m_offsets`,spans) = strictTRMapSt (resolveImageOffset user_tags font_spans text_spans) offsets spans
	              = case [e \\ Error e <- m_offsets`] of
	                  [e : _] = (Error e,spans)
	                  no_error
	                    #! (m_transform`,spans)  = liftMaybe (resolveImgTransform user_tags font_spans text_spans) transform spans
	                    = case m_transform` of
	                        Just (Error e)       = (Error e,spans)
	                        Just (Ok transform`) = (Ok {Img | img & host = host`, overlays = map fromOk m_overlays`, offsets = map fromOk m_offsets`, transform = Just transform`},spans)
	                        nothing              = (Ok {Img | img & host = host`, overlays = map fromOk m_overlays`, offsets = map fromOk m_offsets`},spans)

where
	resolveHostImg :: !ImgTags !FontSpans !TextSpans !HostImg !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError HostImg,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolveHostImg user_tags font_spans text_spans (BasicHostImg img attrs) spans
	  #! (m_attrs`,spans) = resolveImgAttrs user_tags font_spans text_spans attrs spans
	  = case m_attrs` of
	      Error e   = (Error e,spans)
	      Ok attrs` = (Ok (BasicHostImg img attrs`),spans)
	resolveHostImg user_tags font_spans text_spans (CompositeImg img) spans
	  #! (m_img`,spans) = resolveImg user_tags font_spans text_spans img spans
	  = case m_img` of
	      Error e   = (Error e,spans)
	      Ok img`   = (Ok (CompositeImg img`),spans)
	resolveHostImg user_tags font_spans text_spans host spans
	  = (Ok host,spans)
	
	resolveImgAttrs :: !ImgTags !FontSpans !TextSpans !(Set BasicImgAttr) !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError (Set BasicImgAttr),!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolveImgAttrs user_tags font_spans text_spans attrs spans
	  #! (m_attrs`,spans) = strictTRMapSt (resolveImgAttr user_tags font_spans text_spans) ('Data.Foldable'.foldr` (\a as -> [a:as]) [] attrs) spans // USING (toList attrs) INSTEAD OF (fold (\a as -> [a:as]) [] attrs) CRASHES THE COMPILER: Run Time Error: index out of range
	  = case [e \\ Error e <- m_attrs`] of
	      [e : _] = (Error e,spans)
	      _       = (Ok ('Data.Set'.fromList (map fromOk m_attrs`)),spans)
	
	resolveImgAttr :: !ImgTags !FontSpans !TextSpans !BasicImgAttr !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError BasicImgAttr,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolveImgAttr user_tags font_spans text_spans (BasicImgStrokeWidthAttr span) spans
	  #! (m_span`,spans) = resolve_span user_tags font_spans text_spans span spans
	  = case m_span` of
	      Error e = (Error e,spans)
	      Ok r    = (Ok (BasicImgStrokeWidthAttr (PxSpan r)),spans)
	resolveImgAttr user_tags font_spans text_spans (BasicImgXRadiusAttr span) spans
	  #! (m_span`,spans) = resolve_span user_tags font_spans text_spans span spans
	  = case m_span` of
	      Error e = (Error e,spans)
	      Ok r    = (Ok (BasicImgXRadiusAttr (PxSpan r)),spans)
	resolveImgAttr user_tags font_spans text_spans (BasicImgYRadiusAttr span) spans
	  #! (m_span`,spans) = resolve_span user_tags font_spans text_spans span spans
	  = case m_span` of
	      Error e = (Error e,spans)
	      Ok r    = (Ok (BasicImgYRadiusAttr (PxSpan r)),spans)
	resolveImgAttr user_tags font_spans text_spans attr spans
	  = (Ok attr,spans)
	
	resolveImgTransform :: !ImgTags !FontSpans !TextSpans !ImgTransform !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError ImgTransform,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolveImgTransform user_tags font_spans text_spans (FitImg w h) spans
	  #! (m_w`,spans) = resolve_span user_tags font_spans text_spans w spans
	  #! (m_h`,spans) = resolve_span user_tags font_spans text_spans h spans
	  = case (m_w`,m_h`) of
	      (Ok w`,Ok h`) = (Ok (FitImg (PxSpan w`) (PxSpan h`)),spans)
	      (Error e,_)   = (Error e,spans)
	      (_,Error e)   = (Error e,spans)
	resolveImgTransform user_tags font_spans text_spans (FitXImg w) spans
	  #! (m_w`,spans) = resolve_span user_tags font_spans text_spans w spans
	  = case m_w` of
	      Ok w`         = (Ok (FitXImg (PxSpan w`)),spans)
	      Error e       = (Error e,spans)
	resolveImgTransform user_tags font_spans text_spans (FitYImg h) spans
	  #! (m_h`,spans) = resolve_span user_tags font_spans text_spans h spans
	  = case m_h` of
	      Ok h`         = (Ok (FitYImg (PxSpan h`)),spans)
	      Error e       = (Error e,spans)
	resolveImgTransform user_tags font_spans text_spans transform spans
	  = (Ok transform,spans)

//	span resolve algorithm memoizes path, image-span, and grid-span dimensions
resolve_span :: !ImgTags !FontSpans !TextSpans !Span !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError Real,!*(!ImgPaths,!ImgSpans,!GridSpans))
resolve_span user_tags font_spans text_spans span (paths,imgs_spans,grids_spans)
	= resolve_span` 'Data.Set'.newSet user_tags font_spans text_spans span (paths,imgs_spans,grids_spans)
where
	resolve_span` :: !(Set ImgTagNo) !ImgTags !FontSpans !TextSpans !Span !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError Real,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolve_span` visited user_tags font_spans text_spans (PxSpan r) spans
		= (Ok r,spans)
	resolve_span` visited user_tags font_spans text_spans (LookupSpan l) spans
		= case l of
		    ColumnXSpan tag column_no
		      = case tag of
			      ImageTagUser no label = case 'Data.Map'.get (ImageTagUser no label) user_tags of
	                                        Nothing  = (user_error "columnspan" "unassigned ImageTag",spans)
	                                        Just no` = resolve_from_grid_span "column" "ImageTag" get_col_spans set_col_spans column_no (user_error "columnspan") no` visited user_tags font_spans text_spans spans
	              ImageTagSystem no`    = resolve_from_grid_span "column" "system tag" get_col_spans set_col_spans column_no (sys_error "columnspan") no` visited user_tags font_spans text_spans spans
		    RowYSpan tag row_no
		      = case tag of
	              ImageTagUser no label = case 'Data.Map'.get (ImageTagUser no label) user_tags of
	                                        Nothing  = (user_error "rowspan" "unassigned ImageTag",spans)
	                                        Just no` = resolve_from_grid_span "row" "ImageTag" get_row_spans set_row_spans row_no (user_error "rowspan") no` visited user_tags font_spans text_spans spans
	              ImageTagSystem no`    = resolve_from_grid_span "row" "system tag" get_row_spans set_row_spans row_no (sys_error "rowspan") no` visited user_tags font_spans text_spans spans
		    ImageXSpan tag
		      = case tag of
	              ImageTagUser no label = case 'Data.Map'.get (ImageTagUser no label) user_tags of
	                                        Nothing  = (user_error "imagexspan" "unassigned ImageTag",spans)
	                                        Just no` = resolve_from_image_span "x" "ImageTag" fst upd_fst (user_error "imagexspan") no` visited user_tags font_spans text_spans spans
		          ImageTagSystem no     = resolve_from_image_span "x" "system tag" fst upd_fst (sys_error "imagexspan") no visited user_tags font_spans text_spans spans
		    ImageYSpan tag
		      = case tag of
	              ImageTagUser no label = case 'Data.Map'.get (ImageTagUser no label) user_tags of
	                                        Nothing  = (user_error "imageyspan" "unassigned ImageTag",spans)
	                                        Just no` = resolve_from_image_span "y" "ImageTag" snd upd_snd (user_error "imageyspan") no` visited user_tags font_spans text_spans spans
	              ImageTagSystem no`    = resolve_from_image_span "y" "system tag" snd upd_snd (sys_error "imageyspan") no` visited user_tags font_spans text_spans spans
		    TextXSpan font txt
		      = case 'Data.Map'.get font text_spans of
	              Nothing               = (sys_error "textxspan" ("missing FontDef entry (" +++ toString font +++ ")"),spans)
	              Just ws               = case 'Data.Map'.get txt ws of
	                                        Nothing = (sys_error "textxspan" ("missing text entry \"" +++ txt +++ "\""),spans)
	                                        Just w  = (Ok w,spans)
	        PathXSpan tag
	          = case tag of
	              ImageTagUser no label = (sys_error "resolve_span" ("unexpected value (PathXSpan (ImageTagUser " +++ toString no +++ "))"),spans)
	              ImageTagSystem no`    = resolve_from_path "x" "PathXSpan" fst (\a (_,b) -> (a,b)) (user_error "x span of path") no` visited user_tags font_spans text_spans spans
	        PathYSpan tag
	          = case tag of
	              ImageTagUser no label = (sys_error "resolve_span" ("unexpected value (PathYSpan (ImageTagUser " +++ toString no +++ "))"),spans)
	              ImageTagSystem no`    = resolve_from_path "y" "PathYSpan" snd (\b (a,_) -> (a,b)) (user_error "y span of path") no` visited user_tags font_spans text_spans spans


	resolve_span` visited user_tags font_spans text_spans (AddSpan a b) spans
	  = resolve_span_exprs [a,b] combine visited user_tags font_spans text_spans spans
	where
	  combine [a,b] = Ok (a+b)
	  combine _     = abort "error in resolve_span\n"
	resolve_span` visited user_tags font_spans text_spans (SubSpan a b) spans
	  = resolve_span_exprs [a,b] combine visited user_tags font_spans text_spans spans
	where
	  combine [a,b] = Ok (a-b)
	  combine _     = abort "error in resolve_span\n"
	resolve_span` visited user_tags font_spans text_spans (MulSpan a b) spans
	  = resolve_span_exprs [a,b] combine visited user_tags font_spans text_spans spans
	where
	  combine [a,b] = Ok (a*b)
	  combine _     = abort "error in resolve_span\n"
	resolve_span` visited user_tags font_spans text_spans (DivSpan a b) spans
	  = resolve_span_exprs [a,b] combine visited user_tags font_spans text_spans spans
	where
	  combine [a,b]
	  | b == 0.0  = user_error "(/.)" "division by zero"
	  | otherwise = Ok (a/b)
	  combine _   = abort "error in resolve_span\n"
	resolve_span` visited user_tags font_spans text_spans (MinSpan as) spans
	  = resolve_span_exprs as combine visited user_tags font_spans text_spans spans    where combine rs = Ok (minList rs)
	resolve_span` visited user_tags font_spans text_spans (MaxSpan as) spans
	  = resolve_span_exprs as combine visited user_tags font_spans text_spans spans    where combine rs = Ok (maxList rs)
	resolve_span` visited user_tags font_spans text_spans (AbsSpan a) spans
	  = case resolve_span` visited user_tags font_spans text_spans a spans of
	      (Ok    r,spans) = (Ok (abs r),spans)
	      (Error e,spans) = (Error e,   spans)
	
	resolve_span_exprs :: ![Span] !([Real] -> MaybeError SpanResolveError Real) 
	                      !(Set ImgTagNo) !ImgTags !FontSpans !TextSpans !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError Real,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolve_span_exprs span_exprs combine visited user_tags font_spans text_spans spans
	  = case foldl (resolve_span_expr visited user_tags font_spans text_spans) (Ok [],spans) span_exprs of
	      (Ok   rs,spans) = (combine (reverse rs),spans)
	      (Error e,spans) = (Error   e,           spans)
	where
		resolve_span_expr :: !(Set ImgTagNo) !ImgTags !FontSpans !TextSpans !(!MaybeError SpanResolveError [Real],!*(!ImgPaths,!ImgSpans,!GridSpans)) !Span -> (!MaybeError SpanResolveError [Real],!*(!ImgPaths,!ImgSpans,!GridSpans))
		resolve_span_expr visited user_tags font_spans text_spans (Ok resolved_spans,spans) span_expr
			= case resolve_span` visited user_tags font_spans text_spans span_expr spans of
			    (Ok r,   spans) = (Ok [r:resolved_spans],spans)
			    (Error e,spans) = (Error e,              spans)
		resolve_span_expr visited user_tags font_spans text_spans error span_expr
			= error
	
	resolve_from_grid_span :: !String !String !(GridSpan -> [Span]) !([Span] GridSpan -> GridSpan) !Int !(String -> MaybeError SpanResolveError Real) !Int 
	                          !(Set ImgTagNo) !ImgTags !FontSpans !TextSpans !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError Real,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolve_from_grid_span dim tag_type select replace elem_no error no visited user_tags font_spans text_spans spans=:(paths,imgs_spans,grids_spans)
		= case 'Data.Map'.get no grids_spans of
            Nothing   = (error (tag_type +++ " does not refer to grid"),spans)
            Just grid = let dim_spans = select grid
                         in if (elem_no < 0 || elem_no >= length dim_spans)
                               (error ("incorrect number (" +++ toString elem_no +++ ")"),spans)
                               (if ('Data.Set'.member no visited)
                                   (error "cyclic dependency of system tags",spans)
                                   (case dim_spans !! elem_no of
                                      PxSpan r
                                         = (Ok r,spans)
                                      unresolved
                                         = case resolve_span` ('Data.Set'.insert no visited) user_tags font_spans text_spans unresolved spans of
                                             (Ok r,(paths,imgs_spans,grids_spans))
                                                              = (Ok r,(paths,imgs_spans,'Data.Map'.put no (replace (updateAt elem_no (PxSpan r) dim_spans) grid) grids_spans))
                                             unresolved_error = unresolved_error
                                   )
                               )
	
	resolve_from_image_span :: !String !String !(ImageSpan -> Span) !(Span ImageSpan -> ImageSpan) !(String -> MaybeError SpanResolveError Real) !Int 
	                           !(Set ImgTagNo) !ImgTags !FontSpans !TextSpans !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError Real,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolve_from_image_span dim tag_type select replace error no visited user_tags font_spans text_spans spans=:(paths,imgs_spans,grids_spans)
        = case 'Data.Map'.get no imgs_spans of
            Nothing    = (error (tag_type +++ " is not associated with an image"),spans)
            Just span  = if ('Data.Set'.member no visited)
                            (error ("cyclic dependency of " +++ tag_type +++ "s"),spans)
                            (case select span of
                               PxSpan r
                                  = (Ok r,spans)
                               unresolved
                                  = case resolve_span` ('Data.Set'.insert no visited) user_tags font_spans text_spans unresolved spans of
                                      (Ok r,(paths,imgs_spans,grids_spans))
                                                       = (Ok r,(paths,'Data.Map'.put no (replace (PxSpan r) span) imgs_spans,grids_spans))
                                      unresolved_error = unresolved_error
                            )
	
	resolve_from_path :: !String !String !(ImageSpan -> Span) !(Span ImageSpan -> ImageSpan) !(String -> MaybeError SpanResolveError Real) !Int
	                     !(Set ImgTagNo) !ImgTags !FontSpans !TextSpans !*(!ImgPaths,!ImgSpans,!GridSpans) -> (!MaybeError SpanResolveError Real,!*(!ImgPaths,!ImgSpans,!GridSpans))
	resolve_from_path dim span_type select replace error no visited user_tags font_spans text_spans spans=:(paths,imgs_spans,grids_spans)
		= case 'Data.Map'.get no paths of
		    Nothing    = (sys_error "resolve_span" ("unassigned system tag for " +++ span_type),spans)
		    Just path  = if ('Data.Set'.member no visited)
		                    (error ("cyclic dependency of " +++ span_type +++ "s"),spans)
		                    (case select path.ImgPath.pathSpan of
		                       PxSpan r
		                          = (Ok r,spans)
		                       unresolved
		                         #! (path,spans) = strictTRMapSt (resolveImageOffset user_tags font_spans text_spans) path.ImgPath.pathPoints spans
		                         = case [e \\ Error e <- path] of
		                             [e : _] = (Error e,spans)
		                             _       = case resolve_span` ('Data.Set'.insert no visited) user_tags font_spans text_spans unresolved spans of
		                                         (Ok r,(paths,imgs_spans,grids_spans))
		                                                          = (Ok r,('Data.Map'.alter (replace_in_path (map fromOk path) r replace) no paths,'Data.Map'.alter (replace_in_span r replace) no imgs_spans,grids_spans))
		                                         unresolved_error = unresolved_error
		                    )
	where
		replace_in_span :: !Real !(Span ImageSpan -> ImageSpan) !(Maybe ImageSpan) -> Maybe ImageSpan
		replace_in_span r f (Just pair)   = Just (f (PxSpan r) pair)
		replace_in_span _ _ nothing       = nothing
		
		replace_in_path :: ![ImageOffset] !Real !(Span ImageSpan -> ImageSpan) !(Maybe ImgPath) -> Maybe ImgPath
		replace_in_path p r f (Just path) = Just {ImgPath | path & pathPoints = p, pathSpan = f (PxSpan r) path.ImgPath.pathSpan}
		replace_in_path _ _ _ nothing     = nothing
