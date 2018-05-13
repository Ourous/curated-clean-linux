definition module Text.HTML
/**
* This module provides data types for easy construction of Html documents.
* All tags and attributes of Xhtml 1.0 transitional are captured
* in the HtmlTag and HtmlAttribute type. This library does not control
* **how** you assemble these tags into a document. It only discerns
* between tags that contain other tags and tags that are empty.
*
* For information on how to construct valid html pages with these types,
* see the document definition at:
*  http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd
*/

import StdString, Data.Maybe


/**
* This type provides an enumeration of all html tags.
*/
:: HtmlTag	= Text				!String					//Text, in which special characters should be automatically escaped.
			| Html				!String					//Text, which should be used without any conversions.
			| ATag				![HtmlAttr] ![HtmlTag]
			| AbbrTag			![HtmlAttr] ![HtmlTag]
			| AcronymTag		![HtmlAttr] ![HtmlTag]
			| AddressTag		![HtmlAttr] ![HtmlTag]
			| AppletTag			![HtmlAttr] ![HtmlTag]
			| AreaTag			![HtmlAttr]
			| BTag				![HtmlAttr] ![HtmlTag]
			| BaseTag			![HtmlAttr] ![HtmlTag]
			| BasefontTag		![HtmlAttr]
			| BdoTag			![HtmlAttr] ![HtmlTag]
			| BigTag			![HtmlAttr] ![HtmlTag]
			| BlockquoteTag		![HtmlAttr] ![HtmlTag]
			| BodyTag			![HtmlAttr] ![HtmlTag]
			| BrTag				![HtmlAttr]
			| ButtonTag			![HtmlAttr] ![HtmlTag]
			| CaptionTag		![HtmlAttr] ![HtmlTag]
			| CanvasTag 		![HtmlAttr] ![HtmlTag]
			| CenterTag			![HtmlAttr] ![HtmlTag]
			| CircleTag			![HtmlAttr]
			| CiteTag			![HtmlAttr] ![HtmlTag]
			| CodeTag			![HtmlAttr] ![HtmlTag]
			| ColTag			![HtmlAttr] ![HtmlTag]
			| ColgroupTag		![HtmlAttr] ![HtmlTag]
			| DdTag				![HtmlAttr] ![HtmlTag]
			| DelTag			![HtmlAttr] ![HtmlTag]
			| DfnTag			![HtmlAttr] ![HtmlTag]
			| DirTag			![HtmlAttr] ![HtmlTag]
			| DivTag			![HtmlAttr] ![HtmlTag]
			| DlTag				![HtmlAttr] ![HtmlTag]
			| DtTag				![HtmlAttr] ![HtmlTag]
			| EllipseTag		![HtmlAttr]
			| EmTag				![HtmlAttr] ![HtmlTag]
			| FieldsetTag		![HtmlAttr] ![HtmlTag]
			| FontTag			![HtmlAttr] ![HtmlTag]
			| FormTag			![HtmlAttr] ![HtmlTag]
			| H1Tag				![HtmlAttr] ![HtmlTag]
			| H2Tag				![HtmlAttr] ![HtmlTag]
			| H3Tag				![HtmlAttr] ![HtmlTag]
			| H4Tag				![HtmlAttr] ![HtmlTag]
			| H5Tag				![HtmlAttr] ![HtmlTag]
			| H6Tag				![HtmlAttr] ![HtmlTag]
			| HeadTag			![HtmlAttr] ![HtmlTag]
			| HrTag				![HtmlAttr]
			| HtmlTag			![HtmlAttr] ![HtmlTag]
			| ITag				![HtmlAttr] ![HtmlTag]
			| IframeTag			![HtmlAttr] ![HtmlTag]
			| ImgTag			![HtmlAttr]
			| InputTag			![HtmlAttr]
			| InsTag			![HtmlAttr] ![HtmlTag]
			| IsindexTag		![HtmlAttr]
			| KdbTag			![HtmlAttr] ![HtmlTag]
			| LabelTag			![HtmlAttr] ![HtmlTag]
			| LegendTag			![HtmlAttr] ![HtmlTag]
			| LiTag				![HtmlAttr] ![HtmlTag]
			| LinkTag			![HtmlAttr] ![HtmlTag]
			| MapTag			![HtmlAttr] ![HtmlTag]
			| MenuTag			![HtmlAttr] ![HtmlTag]
			| MetaTag			![HtmlAttr] ![HtmlTag]
			| NoframesTag		![HtmlAttr] ![HtmlTag]
			| NoscriptTag		![HtmlAttr] ![HtmlTag]
			| ObjectTag			![HtmlAttr] ![HtmlTag]
			| OlTag				![HtmlAttr] ![HtmlTag]
			| OptgroupTag		![HtmlAttr] ![HtmlTag]
			| OptionTag			![HtmlAttr] ![HtmlTag]
			| PTag				![HtmlAttr] ![HtmlTag]
			| ParamTag			![HtmlAttr] ![HtmlTag]
			| PreTag			![HtmlAttr] ![HtmlTag]
			| QTag				![HtmlAttr] ![HtmlTag]
			| RectTag			![HtmlAttr]
			| STag				![HtmlAttr] ![HtmlTag]
			| SampTag			![HtmlAttr] ![HtmlTag]
			| ScriptTag			![HtmlAttr] ![HtmlTag]
			| SelectTag			![HtmlAttr] ![HtmlTag]
			| SmallTag			![HtmlAttr] ![HtmlTag]
			| SpanTag			![HtmlAttr] ![HtmlTag]
			| StrikeTag			![HtmlAttr] ![HtmlTag]
			| StrongTag			![HtmlAttr] ![HtmlTag]
			| StyleTag			![HtmlAttr] ![HtmlTag]
			| SubTag			![HtmlAttr] ![HtmlTag]
			| SupTag			![HtmlAttr] ![HtmlTag]
			| SvgTag			![HtmlAttr] ![SVGAttr]  ![SVGElt]
			| TableTag			![HtmlAttr] ![HtmlTag]
			| TbodyTag			![HtmlAttr] ![HtmlTag]
			| TdTag				![HtmlAttr] ![HtmlTag]
			| TextTag			![HtmlAttr] ![HtmlTag]
			| TextareaTag		![HtmlAttr] ![HtmlTag]
			| TfootTag			![HtmlAttr] ![HtmlTag]
			| ThTag				![HtmlAttr] ![HtmlTag]
			| TheadTag			![HtmlAttr] ![HtmlTag]
			| TitleTag			![HtmlAttr] ![HtmlTag]
			| TrTag				![HtmlAttr] ![HtmlTag]
			| TspanTag			![HtmlAttr] ![HtmlTag]
			| TtTag				![HtmlAttr] ![HtmlTag]
			| UTag				![HtmlAttr] ![HtmlTag]
			| UlTag	 			![HtmlAttr] ![HtmlTag]
			| VarTag			![HtmlAttr] ![HtmlTag]
            | DetailsTag        ![HtmlAttr] ![HtmlTag]
            | SummaryTag        ![HtmlAttr] ![HtmlTag]

/**
* This type provides an enumeration of all attributes that can occur in html tags.
*/
:: HtmlAttr	= AbbrAttr			!String
			| AcceptAttr		!String
			| AcceptcharsetAttr	!String
			| AccesskeyAttr		!String
			| ActionAttr		!String
			| AlignAttr			!String
			| AlinkAttr			!String
			| AltAttr			!String
			| ArchiveAttr		!String
			| AxisAttr			!String
			| BackgroundAttr	!String
			| BgcolorAttr		!String
			| BorderAttr		!String
			| CellspacingAttr	!String
			| CellpaddingAttr	!String
			| CharAttr			!String
			| CharoffAttr		!String
			| CharsetAttr		!String
			| CheckedAttr
			| CiteAttr			!String
			| ClassAttr			!String
			| ClassidAttr		!String
			| ColorAttr			!String
			| ColsAttr			!String
			| ColspanAttr		!String
			| CodebaseAttr		!String
			| CodetypeAttr		!String
			| ContentAttr		!String
			| CompactAttr
			| CoordsAttr		!String
			| DataAttr			!String
			| DatetimeAttr		!String
			| DeclareAttr
			| DeferAttr			!String
			| DirAttr			!String
			| DisabledAttr
			| DXAttr			!String
			| DYAttr			!String
			| EnctypeAttr		!String
			| FaceAttr			!String
			| ForAttr			!String
			| FrameAttr			!String
			| FrameborderAttr	!String
			| HeadersAttr		!String
			| HeightAttr		!String
			| HrefAttr			!String
			| HreflangAttr		!String
			| HttpequivAttr		!String
			| HspaceAttr		!String
			| IdAttr			!String
			| IsmapAttr
			| LabelAttr			!String
			| LangAttr			!String
			| LanguageAttr		!String
			| LinkAttr			!String
			| LongdescAttr		!String
			| MarginheightAttr	!String
			| MarginwidthAttr	!String
			| MaxlengthAttr		!String
			| MediaAttr			!String
			| MethodAttr		!String
			| MultipleAttr
			| NameAttr			!String
			| NohrefAttr
			| NoshadeAttr
			| NowrapAttr
			| OnblurAttr		!String
			| OnchangeAttr		!String
			| OnclickAttr		!String
			| OndblclickAttr	!String
			| OnfocusAttr		!String
			| OnloadAttr		!String
			| OnmousedownAttr	!String
			| OnmousemoveAttr	!String
			| OnmouseoutAttr	!String
			| OnmouseoverAttr	!String
			| OnmouseupAttr		!String
			| OnkeydownAttr		!String
			| OnkeypressAttr	!String
			| OnkeyupAttr		!String
			| OnresetAttr		!String
			| OnselectAttr		!String
			| OnsubmitAttr		!String
			| OnunloadAttr		!String
			| ProfileAttr		!String
			| PromptAttr		!String
			| ReadonlyAttr
			| RelAttr			!String
			| RevAttr			!String
			| RowsAttr			!String
			| RowspanAttr		!String
			| RulesAttr			!String
			| RXAttr 			!String
			| RYAttr 			!String
			| SchemeAttr		!String
			| ScopeAttr			!String
			| ScrollingAttr		!String
			| SelectedAttr
			| ShapeAttr			!String
			| SizeAttr			!String
			| SpanAttr			!String
			| SrcAttr			!String
			| StandbyAttr		!String
			| StartAttr			!String
			| StyleAttr			!String
			| SummaryAttr		!String
			| TabindexAttr		!String
			| TargetAttr		!String
			| TextAttr			!String
			| TitleAttr			!String
			| TypeAttr			!String
			| UsemapAttr		!String
			| ValignAttr		!String
			| ValueAttr			!String
			| ValuetypeAttr		!String
			| VlinkAttr			!String
			| VspaceAttr		!String
			| WidthAttr			!String
			| XmllangAttr		!String
			| XmlspaceAttr		!String
			| XmlnsAttr			!String
			| XmlnsXlinkAttr	!String

/**
* This type provides an enumeration of (not yet) all SVG elements.
*/
:: SVGElt				= SVGElt            ![HtmlAttr] ![SVGAttr] ![SVGElt]
                        | CircleElt         ![HtmlAttr] ![SVGAttr]
                        | ClipPathElt       ![HtmlAttr] ![SVGAttr] ![SVGElt]
                        | DefsElt           ![HtmlAttr] ![SVGAttr] ![SVGElt]
                        | EllipseElt        ![HtmlAttr] ![SVGAttr]
						| GElt              ![HtmlAttr] ![SVGAttr] ![SVGElt]
						| ImageElt          ![HtmlAttr] ![SVGAttr] ![SVGElt]
                        | LinearGradientElt ![HtmlAttr] ![SVGAttr] ![SVGElt]
                        | LineElt           ![HtmlAttr] ![SVGAttr]
                        | MarkerElt         ![HtmlAttr] ![SVGAttr] ![SVGElt]
                        | MaskElt           ![HtmlAttr] ![SVGAttr] ![SVGElt]
                        | PathElt           ![HtmlAttr] ![SVGAttr]
                        | PolygonElt        ![HtmlAttr] ![SVGAttr]
                        | PolylineElt       ![HtmlAttr] ![SVGAttr]
						| RectElt           ![HtmlAttr] ![SVGAttr]
                        | RadialGradientElt ![HtmlAttr] ![SVGAttr] ![SVGElt]
						| StopElt           ![HtmlAttr] ![SVGAttr]
						| TextElt           ![HtmlAttr] ![SVGAttr] !String                      // currently only a String as content
                        | RawElt            !String


/*
* This type provides an enumeration of (not yet) all SVG element attributes.
*/
:: SVGAttr				= AlignmentBaselineAttr   !String
                        | BaseProfileAttr         !String										// necessary?
						| ContentScriptTypeAttr   !String
						| ClipPathAttr            !String
//						| ContentStyleTypeAttr    !String										// deprecated in SVG1.1
						| CxAttr                  !SVGCoordinate
						| CyAttr                  !SVGCoordinate
                        | DominantBaselineAttr    !String
						| ExternalResourcesRequiredAttr !Bool
						| FillAttr                !SVGPaint
						| FillOpacityAttr         !SVGFillOpacity
						| FillRuleAttr            !SVGFillRule
						| FontFamilyAttr          !String
						| FontSizeAttr            !String										// {<absolute-size>,<relative-size>,<length>,<percentage>,inherit}
						| FontStyleAttr           !String										// {normal,italic,oblique,inherit}
						| FontStretchAttr         !String										// {normal,wider,narrower,ultra-condensed,extra-condensed,condensed,semi-condensed,semi-expanded,expanded,extra-expanded,ultra-expanded,inherit}
                        | FontVariantAttr         !String                                       // {normal,small-caps,inherit}
                        | FontWeightAttr          !String                                       // {normal,bold,bolder,lighter,100,200,300,400,500,600,700,800,900,inherit}
						| LengthAdjustAttr        !SVGLengthAdjust
                        | MarkerStartAttr         !String
                        | MarkerMidAttr           !String
                        | MarkerEndAttr           !String
                        | MarkerHeightAttr        !SVGLength
                        | MarkerWidthAttr         !SVGLength
                        | MaskAttr                !String
                        | OffsetAttr              !String
                        | OrientAttr              !String
						| PointsAttr              ![(String, String)]
						| PreserveAspectRatioAttr !(Maybe SVGDefer) !(Maybe SVGAlign) !(Maybe SVGMeetOrSlice)
                        | RAttr                   !SVGLength
                        | RefXAttr                !SVGLength
                        | RefYAttr                !SVGLength
						| RxAttr                  !SVGLength									// negative value is an error
						| RyAttr                  !SVGLength									// negative value is an error
                        | StopColorAttr           !String
                        | StopOpacityAttr         !String
						| StrokeAttr              !SVGPaint
						| StrokeDashArrayAttr     !SVGStrokeDashArray
						| StrokeDashOffsetAttr    !SVGStrokeDashOffset
						| StrokeLineCapAttr       !SVGLineCap
						| StrokeLineJoinAttr      !SVGLineJoin
						| StrokeMiterLimitAttr    !SVGStrokeMiterLimit
						| StrokeOpacityAttr       !String//SVGOpacityValue
						| StrokeWidthAttr         !SVGStrokeWidth
						| TextAnchorAttr          !String										// {start,middle,end,inherit}
						| TextLengthAttr          !SVGLength
						| TextRenderingAttr       !String
						| TransformAttr			  ![SVGTransform]
						| VersionAttr             !String										// "1.1" for SVG1.1
						| ViewBoxAttr             !SVGNumber !SVGNumber !SVGNumber !SVGNumber	// <min-x> <min-y> <width> <height>
						| XAttr                   !SVGCoordinate
						| X1Attr                  !SVGLength									// negative value is an error
						| X2Attr                  !SVGLength									// negative value is an error
						| XLinkHRefAttr           !String										// <iri>
						| YAttr                   !SVGCoordinate
						| Y1Attr                  !SVGLength									// negative value is an error
						| Y2Attr                  !SVGLength									// negative value is an error
						| ZoomAndPanAttr          !SVGZoomAndPan

:: SVGAlign				= XMinYMin								// preserve aspect-ratio, align <min-x> of element's viewBox with smallest x-value of viewport,          align <min-y> of element's viewBox with smallest y-value of viewport
						| XMidYMin								// preserve aspect-ratio, align midpoint x-value of element's viewBox with midpoint x-value of viewport, align <min-y> of element's viewBox with smallest y-value of viewport
						| XMaxYMin								// preserve aspect-ratio, align <min-x>+<width> of element's viewBox with maximum x-value of viewport,   align <min-y> of element's viewBox with smallest y-value of viewport
						| XMinYMid								// preserve aspect-ratio, align <min-x> of element's viewBox with smallest x-value of viewport,          align midpoint y-value of element's viewBox with midpoint y-value of viewport
						| XMidYMid								// preserve aspect-ratio, align midpoint x-value of element's viewBox with midpoint x-value of viewport, align midpoint y-value of element's viewBox with midpoint y-value of viewport
						| XMaxYMid								// preserve aspect-ratio, align <min-x>+<width> of element's viewBox with maximum x-value of viewport,   align midpoint y-value of element's viewBox with midpoint y-value of viewport
						| XMinYMax								// preserve aspect-ratio, align <min-x> of element's viewBox with smallest x-value of viewport,          align <min-y>+<height> of element's viewBox with maximum y-value of viewport
						| XMidYMax								// preserve aspect-ratio, align midpoint x-value of element's viewBox with midpoint x-value of viewport, align <min-y>+<height> of element's viewBox with maximum y-value of viewport
						| XMaxYMax								// preserve aspect-ratio, align <min-x>+<width> of element's viewBox with maximum x-value of viewport,   align <min-y>+<height> of element's viewBox with maximum y-value of viewport
:: SVGColor				= SVGRGB !Int !Int !Int					// <r><g><b> component values, each between 0 and 255 (inclusive)
						| SVGColorText !String					// one of the recognized color keywords names
:: SVGCoordinate	  :== SVGLength								// coordinate  ::= length
:: SVGDefer				= SVGDefer
:: SVGFillOpacity		= FillOpacity !SVGNumber//!SVGOpacityValue
						| FillOpacityInherit
:: SVGFillRule			= FillNonzero | FillEvenodd | FillInherit
:: SVGFuncIRI			= IRI String							// url(<IRI>)
:: SVGICCColor		  :== (String,[SVGNumber])					// (<color-profile-name>,<color-values>), the <color-values> list must not be empty
:: SVGLength		  :== (SVGNumber,SVGLengthUnit)
:: SVGLengthAdjust		= Spacing | SpacingAndGlyphs
:: SVGLengthUnit		= EM | EX | PX | IN | CM | MM | PT | PC | PERCENT
:: SVGLineCap			= CapButt | CapRound | CapSquare | CapInherit
:: SVGLineJoin			= JoinMiter | JoinRound | JoinBevel | JoinInherit
:: SVGMeetOrSlice		= SVGMeet
						| SVGSlice
:: SVGNumber		  :== String								// number ::= integer | [+-]? [0-9]* "." [0-9]+
:: SVGOpacityValue	  :== SVGNumber								// value between 0.0 (fully transparant) and 1.0 (fully opaque)
:: SVGPaint				= PaintNone
						| PaintCurrentColor
						| PaintColor SVGColor (Maybe SVGICCColor)
						| PaintFuncIRI SVGFuncIRI (Maybe SVGPaint)
						| PaintInherit
:: SVGStrokeDashArray	= NoDash
						| DashArray               ![String]		// non-empty list, an element is either an SVGNumber or a percentage
						| InheritDash
:: SVGStrokeDashOffset	= DashOffsetLength        !SVGLength	// <length> is allowed to be negative
						| DashOffsetInherit
:: SVGStrokeMiterLimit	= MiterLimit              !SVGNumber	// <miterlimit> >= 1.0
						| MiterLimitInherit
:: SVGStrokeWidth		= StrokeWidthLength       !SVGLength	// <length> >= 0.0
						| StrokeWidthInherit
:: SVGTransform			= MatrixTransform         !SVGNumber !SVGNumber !SVGNumber !SVGNumber !SVGNumber !SVGNumber		// matrix(<a> <b> <c> <d> <e> <f>)
						| TranslateTransform      !SVGNumber !SVGNumber													// translate(<tx> <ty>)
						| ScaleTransform          !SVGNumber !SVGNumber													// scale(<sx> <sy>)
						| RotateTransform         !SVGNumber !(Maybe (SVGNumber,SVGNumber))								// rotate(<rotate-angle> [<cx> <cy>])
						| SkewXTransform          !SVGNumber															// skewX(<skew-angle>)
						| SkewYTransform          !SVGNumber															// skewY(<skew-angle>)
:: SVGZoomAndPan		= Disable								// only sensible on outermost SVGElt: disable zooming and panning by user
						| Magnify								// only sensible on outermost SVGElt:  enable zooming and panning by user

svgEltSize  :: !SVGElt  -> Int
svgAttrSize :: !SVGAttr -> Int

writeSVGTag :: !{#Char} ![HtmlAttr] ![SVGAttr] ![SVGElt] !*{#Char} !Int -> (!*{#Char},!Int)

serializeAttr :: !HtmlAttr !*{#Char} !Int -> (!*{#Char}, !Int)
serializeSVGAttr :: !SVGAttr !*{#Char} !Int -> (!*{#Char}, !Int)

instance toString HtmlTag
instance toString SVGElt
instance toString SVGAlign
instance toString SVGColor
instance toString SVGICCColor
instance toString SVGFillOpacity
instance toString SVGFillRule
instance toString SVGFuncIRI
instance toString SVGLength
instance toString SVGLengthAdjust
instance toString SVGLengthUnit
instance toString SVGLineCap
instance toString SVGLineJoin
instance toString SVGMeetOrSlice
instance toString SVGStrokeMiterLimit
instance toString SVGPaint
instance toString SVGStrokeDashArray
instance toString SVGStrokeDashOffset
instance toString SVGStrokeWidth
instance toString SVGTransform
instance toString SVGZoomAndPan

/*
* This html class makes it possible to use either strings, or html as description/message/instruction
*/
class html a  
where
	html :: !a -> HtmlTag
	
instance html String
instance html HtmlTag
instance html [a] | html a
instance html (Maybe a) | html a

//BACKWARDS COMPATIBILITY
RawText :== Html

browserFriendlySVGEltToString :: !SVGElt -> String

escapeStr :: !String -> String
