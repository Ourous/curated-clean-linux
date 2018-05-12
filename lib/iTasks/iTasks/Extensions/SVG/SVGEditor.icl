implementation module iTasks.Extensions.SVG.SVGEditor

import Graphics.Scalable.Internal.Image`
import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.JS.Encoding
import StdArray, StdBool, StdEnum, StdInt, StdMisc, StdReal, StdTuple
from StdFunc import o
from Data.GenEq import generic gEq
import Data.List
import Data.Error
import Data.MapCollection
from Data.Map import :: Map, instance Functor (Map k)
from Data.Set import :: Set, instance == (Set a), instance < (Set a)
import qualified Data.Map as DM
import qualified Data.Set as DS
import Text
import Data.Matrix
import Text.HTML
import Text.GenJSON
import Math.Geometry

CLICK_DELAY :== 225
svgns =: "http://www.w3.org/2000/svg"

//Predefined object methods
(`addEventListener`)      obj args :== obj .# "addEventListener"      .$ args
(`setAttribute`)          obj args :== obj .# "setAttribute"          .$ args
(`setAttributeNS`)        obj args :== obj .# "setAttributeNS"        .$ args
(`createElementNS`)       obj args :== obj .# "createElementNS"       .$ args
(`appendChild`)           obj args :== obj .# "appendChild"           .$ args
(`removeChild`)           obj args :== obj .# "removeChild"           .$ args
(`getComputedTextLength`) obj args :== obj .# "getComputedTextLength" .$ args
(`createSVGPoint`)        obj args :== obj .# "createSVGPoint"        .$ args
(`getScreenCTM`)          obj args :== obj .# "getScreenCTM"          .$ args
(`inverse`)               obj args :== obj .# "inverse"               .$ args
(`matrixTransform`)       obj args :== obj .# "matrixTransform"       .$ args

:: ImageSpanReal :== (!Real, !Real)

:: DropTarget      = DropTarget
:: MousePos        = MouseUp | MouseDown
:: SVGDragState v  = 
  { svgMousePos     :: !MousePos
  , svgDropCallback :: !SVGDragFun v
  , svgTrueCoordsX  :: !Real
  , svgTrueCoordsY  :: !Real
  , svgGrabPointX   :: !Real
  , svgGrabPointY   :: !Real
  , svgDragTarget   :: !Maybe (JSObj DropTarget)
  }

derive gEq MousePos

fromSVGEditor :: (SVGEditor s v) -> Editor s | iTask s & JSEncode{|*|} s
fromSVGEditor svglet
  = { Editor
    | genUI     = withClientSideInit initUI genUI
    , onEdit    = onEdit
    , onRefresh = onRefresh
    }
where
	initUI :: !(JSObj ()) !*JSWorld -> *JSWorld
	initUI me world
// Set attributes
    # world                       = (me .# "clickCount" .= (toJSVal 0)) world
	# world                       = jsPutCleanVal "dragState" initDragState me world
// Set methods	
	# (jsOnAttributeChange,world) = jsWrapFun (onAttributeChange me) world
	# world                       = (me .# "onAttributeChange" .= jsOnAttributeChange) world
	# (jsInitDOMEl,world)         = jsWrapFun (initDOMEl me) world
	# world                       = (me .# "initDOMEl" .= jsInitDOMEl) world
	= world
	where
		initDragState = {SVGDragState | svgMousePos     = MouseUp
		                              , svgDropCallback = \_ _ v -> v
		                              , svgTrueCoordsX  = 0.0
		                              , svgTrueCoordsY  = 0.0
	                                  , svgGrabPointX   = 0.0
	                                  , svgGrabPointY   = 0.0
	                                  , svgDragTarget   = Nothing
	                    }
	
		initDOMEl me args world
			# (value,world) = .? (me .# "attributes.value") world
			# (value,world) = decodeOnClient value world
			= (jsNull,onNewState me svglet value world)
	
		onAttributeChange me args world
			| jsArgToString (args !! 0) == "stateChange"
				# (value,world) = decodeOnClient (toJSVal (args !! 1))world
				= (jsNull,onNewState me svglet value world)
			| otherwise
				= (jsNull,jsTrace "Unknown attribute change" world)

	genUI :: !DataPath !s !*VSt -> *(!MaybeErrorString (!UI,!EditMask), !*VSt) | iTask s & JSEncode{|*|} s
	genUI dp val world
		# attr = 'DM'.unions [sizeAttr FlexSize FlexSize, valueAttr (encodeOnServer val)]
		= (Ok (uia UIComponent attr,newFieldMask), world)

  	onEdit :: !DataPath !(!DataPath,!JSONNode) !s !EditMask !*VSt -> (!MaybeErrorString (!UIChange,!EditMask), !s, !*VSt) | iTask s & JSEncode{|*|} s
  	onEdit _ (_,json) st m vst 
		= case fromJSON json of 	
			Just nst = (Ok (NoChange,m),nst,vst)
			Nothing  = (Ok (NoChange,m),st, vst)
	
  	onRefresh :: !DataPath !s !s !EditMask !*VSt -> (!MaybeErrorString (!UIChange,!EditMask), !s, !*VSt) | iTask s & JSEncode{|*|} s
  	onRefresh _ new old mask vst 
		= (Ok (if (gEq{|*|} old new) NoChange (ChangeUI [SetAttribute "stateChange" (encodeOnServer new)] []),mask),new,vst)

imgTagSource :: !String -> *TagSource
imgTagSource cid
  = [(ImageTagUser no cid, ImageTagUser no cid) \\ no <- [0..]]

newImgTables :: ImgTables m
newImgTables
  = {ImgTables | imgEventhandlers = 'DM'.newMap
               , imgNewFonts      = 'DS'.newSet
               , imgNewTexts      = 'DM'.newMap
               , imgMasks         = 'DM'.newMap
               , imgLineMarkers   = 'DM'.newMap
               , imgPaths         = 'DM'.newMap
               , imgSpans         = 'DM'.newMap
               , imgGrids         = 'DM'.newMap
               , imgTags          = 'DM'.newMap
               , imgUniqIds       = 0
    }

onNewState :: !(JSVal a) !(SVGEditor s v) !s !*JSWorld -> *JSWorld | JSONEncode{|*|} s
onNewState me svglet=:{initView,renderImage} s world
  #! (cidJS,world)                 = .? (me .# "attributes.taskId") world
  #! cid                           = jsValToString cidJS
  #! v                             = initView s
  #! world                         = jsPutCleanVal "view"  v me world    // Store the view value on the component
  #! world                         = jsPutCleanVal "model" s me world    // Store the model value on the component
  #! (font_spans,text_spans,world) = loadCachedTextSpans world
  #! image`                        = renderImage s v (imgTagSource cid)
  #! (img,{ImgTables | imgEventhandlers=es,imgNewFonts=new_fonts,imgNewTexts=new_txts,imgMasks=masks,imgLineMarkers=markers,imgPaths=paths,imgSpans=spans,imgGrids=grids,imgTags=tags})
                                   = toImg image` font_spans text_spans newImgTables
  #! (font_spans,world)            = addNewFontSpans  new_fonts font_spans world
  #! (text_spans,world)            = addNewTextsSpans new_txts  text_spans world
  #! world                         = storeCachedTextSpans font_spans text_spans world
  = case resolve_all_spans tags font_spans text_spans img masks markers paths spans grids of
      Error error                  = abort error
      Ok (img,masks,markers,paths,spans,grids)
        #! mask_defs               = genSVGMasks masks cid ('DM'.keys es) markers paths spans grids
        #! svg_elems               = genSVGElts  img   cid ('DM'.keys es) markers paths spans grids
        #! (newSVG,world)          = updSVG (mask_defs ++ svg_elems) (getImgRootSize img spans) cid me world
        #! world                   = registerEventhandlers me svglet cid newSVG es tags world
        = world
where
//	retrieve the cached font/text spans, but for now it always yields the empty map
	loadCachedTextSpans :: !*JSWorld -> (!FontSpans,!TextSpans,!*JSWorld)
	loadCachedTextSpans world = ('DM'.newMap, 'DM'.newMap, world)

//	store the cached text spans, but for now it ignores it	
	storeCachedTextSpans :: !FontSpans !TextSpans !*JSWorld -> *JSWorld
	storeCachedTextSpans font_spans text_spans world = world

//	measure new font dimensions and add them to the known set of font dimensions:	
	addNewFontSpans :: !ImgFonts !FontSpans !*JSWorld -> (!FontSpans,!*JSWorld)
	addNewFontSpans newFonts font_spans world
	| 'DS'.null newFonts = (font_spans,world)
	= calcImgFontsSpans newFonts font_spans world

//	measure new text dimensions and add them to the known set of text dimensions:	
	addNewTextsSpans :: !ImgTexts !TextSpans !*JSWorld -> (!TextSpans,!*JSWorld)
	addNewTextsSpans newTexts text_spans world
	| 'DM'.null newTexts = (text_spans,world)
	= calcImgTextsLengths newTexts text_spans world

//	generate the svg-defs for the masks used in this image:
	genSVGMasks :: !ImgMasks !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGMasks masks cid es markers paths spans grids
		= [  DefsElt [] [] [MaskElt [IdAttr (mkMaskId cid no)] [] (genSVGElts m cid es markers paths spans grids)]
		  \\ (no,m) <- 'DM'.toList masks
		  ]

//	return the dimensions of the root image:
	getImgRootSize :: !Img !ImgSpans -> (!Real,!Real)
	getImgRootSize img=:{Img | uniqId} spans
		= case 'DM'.find uniqId spans of
		    (PxSpan w,PxSpan h) = (w,h)
		    _                   = abort "Unexpected error in module SVGEditor (local function getImgRootSize of onNewState): size of root image is undetermined."

//	update the DOM element with the new SVG content:
	updSVG :: ![SVGElt] !(!Real,!Real) !String !(JSVal a) !*JSWorld -> (!JSObj svg,!*JSWorld)
	updSVG elts (imXSp,imYSp) cid me world
	  #! imXSp           = to2decString imXSp
	  #! imYSp           = to2decString imYSp
	  #! svgStr          = browserFriendlySVGEltToString (SVGElt [WidthAttr imXSp, HeightAttr imYSp, XmlnsAttr svgns]
                                                                 [VersionAttr "1.1", ViewBoxAttr "0" "0" imXSp imYSp]
                                                                 elts)
      #! (parser, world) = new "DOMParser" () world
      #! (doc,    world) = (parser .# "parseFromString" .$ (svgStr, "image/svg+xml")) world
      #! (newSVG, world) = .? (doc .# "firstChild") world
      #! (domEl,  world) = .? (me .# "domEl") world
      #! (currSVG,world) = .? (domEl .# "firstChild") world
      #! (_,      world) = if (jsIsNull currSVG)
                              ((domEl `appendChild` newSVG) world)
                              ((domEl .# "replaceChild" .$ (newSVG, currSVG)) world)
      = (newSVG,world)

// compute the font dimensions of new fonts that are used in an image, and add them to the known font dimensions
calcImgFontsSpans :: !ImgFonts !FontSpans !*JSWorld -> (!FontSpans,!*JSWorld)
calcImgFontsSpans new_fonts font_spans world
  #! (svg, world) = (jsDocument `createElementNS` (svgns, "svg")) world
  #! (body,world) = .? (jsDocument .# "body") world
  #! (_,   world) = (body `appendChild` svg) world
  #! (elem,world) = (jsDocument `createElementNS` (svgns, "text")) world
  #! (_,   world) = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
  #! (_,   world) = (svg `appendChild` elem) world
  #! (res, world) = foldl (calcFontSpan elem) (font_spans,world) ('DS'.toList new_fonts)
  #! (_,   world) = (svg `removeChild` elem) world
  #! (_,   world) = (body `removeChild` svg) world
  = (res,  world)
where
	calcFontSpan :: !(JSVal (JSObject a)) !*(!FontSpans,!*JSWorld) !FontDef -> *(!FontSpans,!*JSWorld)
	calcFontSpan elem (font_spans, world) fontdef
	  #! fontAttrs   = [ ("font-family",  fontdef.fontfamily)
                       , ("font-size",    toString fontdef.fontysize)
                       , ("font-stretch", fontdef.fontstretch)
                       , ("font-style",   fontdef.fontstyle)
                       , ("font-variant", fontdef.fontvariant)
                       , ("font-weight",  fontdef.fontweight)
                       , ("alignment-baseline", "auto")
                       , ("dominant-baseline", "auto")
                       , ("x", "-10000")
                       , ("y", "-10000")
                       ]
	  #! world       = strictFoldl (\world args -> snd ((elem `setAttribute` args) world)) world fontAttrs
	  #! (fd, world) = calcFontDescent elem fontdef.fontysize world
	  = ('DM'.put fontdef fd font_spans, world)
	
	calcFontDescent :: !(JSVal (JSObject a)) !Real !*JSWorld -> (!Real, !*JSWorld)
	// same heuristic as used below (at function 'genSVGBasicImage'), must be replaced by proper determination of descent of current font
	calcFontDescent elem fontysize world
	  = (fontysize * 0.25,world)

// compute the string widths of new texts that are used in an image, and add them to the known collection of string widths
calcImgTextsLengths :: !ImgTexts !TextSpans !*JSWorld -> (!TextSpans, !*JSWorld)
calcImgTextsLengths texts text_spans world
  #! (svg, world) = (jsDocument `createElementNS` (svgns, "svg")) world
  #! (body,world) = .? (jsDocument .# "body") world
  #! (_,   world) = (body `appendChild` svg) world
  #! (elem,world) = (jsDocument `createElementNS` (svgns, "text")) world
  #! (_,   world) = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
  #! (_,   world) = (svg `appendChild` elem) world
  #! (res, world) = 'DM'.foldrWithKey (calcTextLengths elem) ('DM'.newMap, world) texts
  #! (_,   world) = (svg `removeChild` elem) world
  #! (_,   world) = (body `removeChild` svg) world
  = (res,  world)
where
	calcTextLengths :: !(JSVal (JSObject a)) !FontDef !(Set String) !*(!TextSpans, !*JSWorld) -> *(!TextSpans, !*JSWorld)
	calcTextLengths elem fontdef strs (text_spans, world)
	  #! fontAttrs   = [ ("font-family",  fontdef.fontfamily)
                       , ("font-size",    toString fontdef.fontysize)
                       , ("font-stretch", fontdef.fontstretch)
                       , ("font-style",   fontdef.fontstyle)
                       , ("font-variant", fontdef.fontvariant)
                       , ("font-weight",  fontdef.fontweight)
                       , ("alignment-baseline", "auto")
                       , ("dominant-baseline", "auto")
                       , ("x", "-10000")
                       , ("y", "-10000")
                       ]
	  #! world       = strictFoldl (\world args -> snd ((elem `setAttribute` args) world)) world fontAttrs
	  #! (ws, world) = 'DS'.fold (calcTextLength elem) ('DM'.newMap, world) strs
	  = ('DM'.alter (merge ws) fontdef text_spans, world)
	where
		merge :: !(Map String TextSpan) !(Maybe (Map String TextSpan)) -> Maybe (Map String TextSpan)
		merge ws` (Just ws) = Just ('DM'.union ws` ws)
		merge ws` nothing   = Just ws`
	
	calcTextLength :: !(JSVal (JSObject a)) !String !*(!Map String TextSpan, !*JSWorld) -> *(!Map String TextSpan, !*JSWorld)
	calcTextLength elem str (text_spans, world)
	  #! world        = (elem .# "textContent" .= str) world
	  #! (ctl, world) = (elem `getComputedTextLength` ()) world
	  = ('DM'.put str (jsValToReal ctl) text_spans, world)

//	register the event handlers of the img:
registerEventhandlers :: !(JSVal a) !(SVGEditor s v) !String !(JSObj svg) !(ImgEventhandlers v) !ImgTags !*JSWorld -> *JSWorld | JSONEncode{|*|} s
registerEventhandlers me svglet cid svg es tags world
  #! (domEl,  world)     = .? (me .# "domEl") world
  #! (svgRoot,world)     = .? (domEl .# "firstChild") world
  #! idMap               = invertToMapSet (fmap (mkUniqId cid) tags)
// all draggable elements share a common mousemove and mouseup event:
  #! (cbMove, world)     = jsWrapFun (doMouseDragMove me svglet svgRoot) world
  #! (cbUp,   world)     = jsWrapFun (doMouseDragUp me svglet svgRoot idMap) world
  #! (_,      world)     = (svgRoot `addEventListener` ("mousemove", cbMove, True)) world
  #! (_,      world)     = (svgRoot `addEventListener` ("mouseup",   cbUp,   True)) world
// register all individual event handlers:
  = 'DM'.foldrWithKey (registerEventhandler me svglet svg) world es
where
	registerEventhandler :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !ImgTagNo ![ImgEventhandler v] !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	registerEventhandler me svglet svg uniqId es world = foldr (register me svglet svg (mkUniqId cid uniqId)) world es
	where
		register :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !String !(ImgEventhandler v) !*JSWorld -> *JSWorld | JSONEncode{|*|} s
		register me svglet svg elemId (ImgEventhandlerOnClickAttr {OnClickAttr | local,onclick}) world
			= registerNClick me svglet svg elemId onclick local world
		register me svglet svg elemId (ImgEventhandlerOnMouseDownAttr {OnMouseDownAttr | local,onmousedown}) world
			= actuallyRegister me svglet svg elemId "mousedown" onmousedown local world
		register me svglet svg elemId (ImgEventhandlerOnMouseUpAttr {OnMouseUpAttr | local,onmouseup}) world
			= actuallyRegister me svglet svg elemId "mouseup" onmouseup local world
		register me svglet svg elemId (ImgEventhandlerOnMouseOverAttr {OnMouseOverAttr | local,onmouseover}) world
			= actuallyRegister me svglet svg elemId "mouseover" onmouseover local world
		register me svglet svg elemId (ImgEventhandlerOnMouseMoveAttr {OnMouseMoveAttr |local,onmousemove}) world
			= actuallyRegister me svglet svg elemId "mousemove" onmousemove local world
		register me svglet svg elemId (ImgEventhandlerOnMouseOutAttr  {OnMouseOutAttr |local,onmouseout}) world
			= actuallyRegister me svglet svg elemId "mouseout"  onmouseout  local world
		register me svglet svg elemId (ImgEventhandlerDraggableAttr {DraggableAttr | draggable}) world
			= registerDraggable me svglet svg elemId draggable world

actuallyRegister :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !String !String !(v -> v) !Bool! *JSWorld -> *JSWorld | JSONEncode{|*|} s
actuallyRegister me svglet svg elemId evt sttf local world
  #! (elem,world) = (svg .# "getElementById" .$ elemId) world
  #! (cb,  world) = jsWrapFun (doImageEvent me svglet svg elemId sttf local) world
  #! (_,   world) = (elem `addEventListener` (evt, cb, True)) world
  = world

doImageEvent :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !String !(v -> v) !Bool ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) | JSONEncode{|*|} s
doImageEvent me svglet svg elemId sttf local _ world
// Get model & view value 
  #! (view, world)    = jsGetCleanVal "view"  me world
  #! (model,world)    = jsGetCleanVal "model" me world
// Update the view & the model
  #! view             = sttf view
  #! model            = svglet.SVGEditor.updModel model view
  #! world            = jsPutCleanVal "view"  view  me world
  #! world            = jsPutCleanVal "model" model me world
// If not local, fire an itasks edit event 
  | local
// Don't trigger an event, just re-render
  	= (jsNull,onNewState me svglet model world)
// Send edit event
  #! (json,    world) = (jsWindow .# "JSON.parse" .$ (toString (toJSON model))) world //TODO: Should not really print+parse here
  #! (taskId,  world) = .? (me .# "attributes.taskId") world
  #! (editorId,world) = .? (me .# "attributes.editorId") world
  #! (_,       world) = (me .# "doEditEvent" .$ (taskId,editorId,json)) world
// Re-render
  = (jsNull,onNewState me svglet model world)

registerNClick :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !String !(Int v -> v) !Bool !*JSWorld -> *JSWorld | JSONEncode{|*|} s
registerNClick me svglet svg elemId sttf local world
  #! (elem,world) = (svg .# "getElementById" .$ elemId) world
  #! (cb,  world) = jsWrapFun (mkNClickCB me svglet svg elemId sttf local) world
  #! (_,   world) = (elem `addEventListener` ("click", cb, False)) world
  = world

mkNClickCB :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !String !(Int v -> v) !Bool ![JSArg] !*JSWorld-> *(!JSVal (), !*JSWorld) | JSONEncode{|*|} s
mkNClickCB me svglet svg elemId sttf local args world
  #! world           = case args of [a:_] = snd (((toJSVal a) .# "stopPropagation" .$ ()) world) ; _ = world
// If another click already registered a timeout, clear that timeout
  #! (to,world)      = .? (me .# "clickTimeOut") world
  #! world           = if (jsIsUndefined to || jsIsNull to) world (snd (("clearTimeout" .$ to) world))
// Register a callback for the click after a small timeout
  #! (cb,world)      = jsWrapFun (doNClickEvent me svglet svg elemId sttf local) world
  #! (to,world)  	 =  ("setTimeout" .$ (cb, CLICK_DELAY)) world
  #! world           = (me .# "clickTimeOut" .= to) world
// Increase click counter, so we can determine how many times the element was clicked when the timeout passes
  #! (nc,world)      = .? (me .# "clickCount") world
  #! world           = (me .# "clickCount" .= (toJSVal (jsValToInt nc + 1))) world
  = (jsNull,world)

doNClickEvent :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !String !(Int v -> v) !Bool ![JSArg] !*JSWorld-> *(!JSVal (), !*JSWorld) | JSONEncode{|*|} s
doNClickEvent me svglet svg elemId sttf local args world
// Get click count
  #! (nc,world)      = .? (me .# "clickCount") world
// Reset click count
  #! world           = (me .# "clickCount" .= (toJSVal 0)) world
  #! nc              = jsValToInt nc
  = doImageEvent me svglet svg elemId (sttf nc) local args world

registerDraggable :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !String !(SVGDragFun v) !*JSWorld -> *JSWorld
registerDraggable me svglet svg elemId f world
  #! (elem,  world) = (svg .# "getElementById" .$ elemId) world
  #! (cbDown,world) = jsWrapFun (doMouseDragDown me svglet svg f elemId elem) world
  #! (_,     world) = (elem `addEventListener` ("mousedown", cbDown, True)) world
  = world

doMouseDragDown :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !(SVGDragFun v) !String !(JSObj o) ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld)
doMouseDragDown me svglet svgRoot sttf elemId elem args world
  #! (ds,           world) = jsGetCleanVal "dragState" me world
  #! (targetElement,world) = (svgRoot .# "getElementById" .$ elemId) world
  #! (_,            world) = (targetElement .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (boundingRect, world) = (targetElement .# "getBoundingClientRect" .$ ()) world
  #! (left,         world) = .? (boundingRect .# "left") world
  #! (top,          world) = .? (boundingRect .# "top") world
  #! (p,            world) = (svgRoot `createSVGPoint` ()) world
  #!                world  = (p .# "x" .= left) world
  #!                world  = (p .# "y" .= top) world
  #! (m,            world) = (svgRoot `getScreenCTM` ()) world
  #! (inv,          world) = (m `inverse` ()) world
  #! (p,            world) = (p `matrixTransform` inv) world
  #! (px,           world) = .? (p .# "x") world
  #! (py,           world) = .? (p .# "y") world
  #! (e,f)                 = (jsValToReal px, jsValToReal py)
  #! ds                    = { SVGDragState 
                             | ds & svgDropCallback = sttf
                                  , svgMousePos     = MouseDown
                                  , svgDragTarget   = Just targetElement
                                  , svgGrabPointX   = ds.SVGDragState.svgTrueCoordsX - e
                                  , svgGrabPointY   = ds.SVGDragState.svgTrueCoordsY - f
                             }
  #!                world  = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

doMouseDragMove :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) 
doMouseDragMove me svglet svgRoot args world
  #! (ds,world)      = jsGetCleanVal "dragState" me world
  #! evt             = toJSVal (args !! 0)
  #! (newTrueCoordsX, newTrueCoordsY, world)
                     = getNewTrueCoords me evt world
  | not (gEq{|*|} ds.SVGDragState.svgMousePos MouseDown) || ds.SVGDragState.svgDragTarget =: Nothing
 	#! ds            = { SVGDragState 
 	                   | ds & svgTrueCoordsX = newTrueCoordsX
 	                        , svgTrueCoordsY = newTrueCoordsY
 	                   }
    #! world         = jsPutCleanVal "dragState" ds me world
    = (jsNull,world)
  #! dragTarget      = fromJust ds.SVGDragState.svgDragTarget
  #! (domEl,  world) = .? (me .# "domEl") world
  #! (svgRoot,world) = .? (domEl .# "firstChild") world
// Append the dragTarget to the root of the SVG element for two reasons:
//   1. To allow it to be dragged over all other elements
//   2. To not be bothered by the offsets of one or more groups it might initially be in
  #! (_, world)     = (svgRoot `appendChild` dragTarget) world
  #! newX           = newTrueCoordsX - ds.SVGDragState.svgGrabPointX
  #! newY           = newTrueCoordsY - ds.SVGDragState.svgGrabPointY
  #! (_, world)     = (dragTarget `setAttribute` ("transform", "translate(" +++ toString newX +++ "," +++ toString newY +++ ")")) world
  #! ds             = { SVGDragState
                      | ds & svgTrueCoordsX = newTrueCoordsX
                           , svgTrueCoordsY = newTrueCoordsY
                      }
  #! world          = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

doMouseDragUp :: !(JSVal a) !(SVGEditor s v) !(JSObj svg) !(Map String (Set ImageTag)) ![JSArg] !*JSWorld -> *(!JSVal (), !*JSWorld) 
doMouseDragUp me svglet svgRoot idMap args world
  #! evt               = toJSVal (args !! 0)
  #! (ds,world)        = jsGetCleanVal "dragState" me world
  | ds.SVGDragState.svgDragTarget =: Nothing
    #! ds              = { SVGDragState
                         | ds & svgMousePos   = MouseUp
                              , svgDragTarget = Nothing
                         }
    #! world           = jsPutCleanVal "dragState" ds me world
  	= (jsNull,world)
  #! (evtTarget,world) = .? (evt .# "target") world
  #! dragTarget        = fromJust ds.SVGDragState.svgDragTarget
  #! (_, world)        = (dragTarget .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
  #! (parentId, world) = firstIdentifiableParentId evtTarget world
// Get model & view value 
  #! (view, world)     = jsGetCleanVal "view" me world
  #! (model,world)     = jsGetCleanVal "model" me world
  #! xdiff             = ds.SVGDragState.svgTrueCoordsX - ds.SVGDragState.svgGrabPointX
  #! ydiff             = ds.SVGDragState.svgTrueCoordsY - ds.SVGDragState.svgGrabPointY
  #! view              = ds.SVGDragState.svgDropCallback ('DM'.findWithDefault 'DS'.newSet parentId idMap) (xdiff,ydiff) view 
  #! model             = svglet.SVGEditor.updModel model view
  #! ds                = { SVGDragState
                         | ds & svgMousePos   = MouseUp
                              , svgDragTarget = Nothing
                         }
  #! world             = jsPutCleanVal "view"  view  me world
  #! world             = jsPutCleanVal "model" model me world
  #! world             = jsPutCleanVal "dragState" ds me world
  = (jsNull,world)

firstIdentifiableParentId :: !(JSObj a) !*JSWorld -> *(!String, !*JSWorld)
firstIdentifiableParentId elem world
  #! (idval,world)      = .? (elem .# "id") world
  | jsIsNull idval
      #! (parent,world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  #! idval = jsValToString idval
  | idval == ""
      #! (parent,world) = .? (elem .# "parentNode") world
      = firstIdentifiableParentId parent world
  | otherwise
      = (idval, world)

getNewTrueCoords :: !(JSVal a) !(JSObj JSEvent) !*JSWorld -> *(!Real, !Real, !*JSWorld)
getNewTrueCoords me evt world
  #! (domEl,       world) = .? (me .# "domEl") world
  #! (svgRoot,     world) = .? (domEl .# "firstChild") world
  #! (newScale,    world) = .? (svgRoot .# "currentScale") world
  #! newScale             = jsValToReal newScale
  #! (translation, world) = .? (svgRoot .# "currentTranslate") world
  #! (translationX,world) = .? (translation .# "x") world
  #! (translationY,world) = .? (translation .# "y") world
  #! (clientX,     world) = .? (evt .# "clientX") world
  #! (clientY,     world) = .? (evt .# "clientY") world
  #! newTrueCoordsX       = ((jsValToReal clientX) - (jsValToReal translationX)) / newScale
  #! newTrueCoordsY       = ((jsValToReal clientY) - (jsValToReal translationY)) / newScale
  = (newTrueCoordsX, newTrueCoordsY, world)

point2Vec :: !(!Span, !Span) -> Vector Span
point2Vec (x, y) = {x, y, px 1.0}

appTF :: !(Matrix Span) !(!Span, !Span) -> (!Span, !Span)
appTF m p
  #! m = mulMatrixVec m (point2Vec p)
  = (m.[0].[0], m.[1].[0])

translateTF :: !Span !Span !(!Span, !Span) -> (!Span, !Span)
translateTF sx sy p
  = appTF { {px 1.0, px 0.0, sx}
          , {px 0.0, px 1.0, sy}
          , {px 0.0, px 0.0, px 1.0}
          } p

scaleTF :: !Span !Span !(!Span, !Span) -> (!Span, !Span)
scaleTF sx sy p
  = appTF { {sx,     px 0.0, px 0.0}
          , {px 0.0, sy,     px 0.0}
          , {px 0.0, px 0.0, px 1.0}
          } p

rotateTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
rotateTF a p
  #! a = toRad a
  = appTF { {px (cos a), px (0.0 - sin a), px 0.0}
          , {px (sin a), px (cos a),       px 0.0}
          , {px 0.0,     px 0.0,           px 1.0}
          } p

skewXTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewXTF a p
  = appTF { {px 1.0, px (tan (toRad a)), px 0.0}
          , {px 0.0, px 1.0,             px 0.0}
          , {px 0.0, px 0.0,             px 1.0}
          } p

skewYTF :: !Angle !(!Span, !Span) -> (!Span, !Span)
skewYTF a p
  = appTF { {px 1.0,             px 0.0, px 0.0}
          , {px (tan (toRad a)), px 1.0, px 0.0}
          , {px 0.0,             px 0.0, px 1.0}
          } p

mkMaskId :: !String !Int -> String
mkMaskId editletId uniqId = "maskId-" +++ editletId +++ toString uniqId

mkClipPathId :: !String !Int -> String
mkClipPathId editletId uniqId = "clipPathId-" +++ editletId +++ toString uniqId

mkMarkerId :: !String !Int -> String
mkMarkerId editletId uniqId = "markerId-" +++ editletId +++ toString uniqId

mkUniqId :: !String !Int -> String
mkUniqId editletId uniqId = "uniqId-" +++ editletId +++ toString uniqId

mkUrl :: !String -> String
mkUrl ref = "url(#" +++ ref +++ ")"

mkWH :: !ImageSpanReal -> [HtmlAttr]
mkWH (imXSp, imYSp) = [WidthAttr (to2decString imXSp), HeightAttr (to2decString imYSp)]

to2decString :: !Real -> String
to2decString r = toString (toReal (toInt (r * 100.0)) / 100.0)

genSVGElts :: !Img !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
genSVGElts {Img | uniqId, transform, host, overlays, offsets} cid es markers paths spans grids
	= mkGroup (if interactive [IdAttr (mkUniqId cid uniqId)] [])
	          (genSVGTransform host transform spans cid)
	          (  genSVGHost     uniqId   host    cid es markers paths spans grids 
	          ++ genSVGOverlays overlays offsets cid es markers paths spans grids
	          )
where
	interactive = isMember uniqId es
	
	genSVGTransform :: !HostImg !(Maybe ImgTransform) !ImgSpans !String -> [SVGAttr]
	genSVGTransform (CompositeImg img) (Just tf) spans cid
		= [genTransform isTextHost imgSpan tf cid]
	where
		isTextHost  = case img.Img.host of
		                BasicHostImg (TextImg _ _) _ = True
		                _                            = False
		imgSpan     = case 'DM'.get img.Img.uniqId spans of
			            Just (PxSpan w, PxSpan h) = (w,h)
			            Just _                    = abort ("Unexpected error in module SVGEditor (genSVGElts): " +++ unresolvedErrorMsg  "image")
			            nothing                   = abort ("Unexpected error in module SVGEditor (genSVGElts): " +++ unavailableErrorMsg "image")
		
		genTransform :: !Bool !ImageSpanReal !ImgTransform !String -> SVGAttr
		genTransform isText (xsp, ysp) (RotateImg imAn) _
		// FIXME: We currently divide ysp by 4.0 as an approximation of the text descent height. Text is transformed from the baseline, not top-left. The actual offset for text would be ~((fontyspan / 2) - descent), but we currently don't know the descent.
			#! yoff = if isText (~ (ysp / 4.0)) (ysp / 2.0)
			= TransformAttr [RotateTransform (to2decString (toDeg imAn)) (Just (to2decString (xsp / 2.0), to2decString yoff))]
		genTransform _ _ (SkewXImg imAn) _
			= TransformAttr [SkewXTransform (toString (toDeg imAn))]
		genTransform _ _ (SkewYImg imAn) _
			= TransformAttr [SkewYTransform (toString (toDeg imAn))]
		genTransform isText (xsp, ysp) (FitImg spx spy) _
			| isText     = TransformAttr [translate, scale]
			| otherwise  = TransformAttr            [scale]
		where
			(fx,fy)      = case (spx,spy) of
			                 (PxSpan rx, PxSpan ry) = (to2decString (rx / xsp), to2decString (ry / ysp))
			                 _                      = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg  "fit"))
			translate    = TranslateTransform "0" (toString ysp)
			scale        = ScaleTransform fx fy
		genTransform isText (xsp, ysp) (FitXImg sp) _
			| isText    = TransformAttr [translate, scale]
			| otherwise = TransformAttr            [scale]
		where
			fx          = case sp of
			                PxSpan rx = rx / xsp
			                _         = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg "fitx"))
			fxy         = if (xsp > 0.0) (to2decString fx) "1.0"
			translate   = TranslateTransform "0" (to2decString (ysp * 0.7 * fx))
			scale       = ScaleTransform fxy fxy
		genTransform isText (xsp, ysp) (FitYImg sp) _
			| isText    = TransformAttr [translate, scale]
			| otherwise = TransformAttr            [scale]
		where
			fy          = case sp of
			                PxSpan ry = ry / ysp
			                _         = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg "fity"))
			fxy         = if (ysp > 0.0) (to2decString fy) "1.0"
			translate   = TranslateTransform "0" (toString ysp)
			scale       = ScaleTransform fxy fxy
		genTransform isText (_, ysp) (ScaleImg fx fy) _
			| isText    = TransformAttr [translate, scale]
			| otherwise = TransformAttr            [scale]
		where
			translate   = TranslateTransform "0" (toString ysp)
			scale       = ScaleTransform (to2decString fx) (to2decString fy)
		genTransform isText (xsp, ysp) FlipXImg _
			= TransformAttr [TranslateTransform (to2decString xsp) "0", ScaleTransform "-1" "1"]
		genTransform isText (xsp, ysp) FlipYImg _
			= TransformAttr [TranslateTransform "0" (to2decString ysp`), ScaleTransform "1" "-1"]
		where
			ysp`        = if isText ((~ ysp) * 0.7) ysp
		genTransform _ _ (MaskImg uniqId) cid
			= MaskAttr (mkUrl (mkMaskId cid uniqId))
	genSVGTransform _ _ _ _
		= []
	
	genSVGHost :: !ImgTagNo !HostImg !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGHost no host=:(BasicHostImg basic attrs) cid es markers paths spans grids
		= genSVGBasicHostImg no basic (genSVGImageAttrs attrs) cid es markers paths spans grids
	where
		genSVGImageAttrs :: !(Set BasicImgAttr) -> [SVGAttr]
		genSVGImageAttrs atts = strictTRMap genSVGImageAttr ('DS'.toList atts)
		where
			genSVGImageAttr :: !BasicImgAttr -> SVGAttr
			genSVGImageAttr (BasicImgStrokeAttr      color)      = StrokeAttr (PaintColor color Nothing)
			genSVGImageAttr (BasicImgStrokeWidthAttr (PxSpan w)) = StrokeWidthAttr (StrokeWidthLength (toString w, PX))
			genSVGImageAttr (BasicImgXRadiusAttr     (PxSpan r)) = RxAttr (toString r, PX)
			genSVGImageAttr (BasicImgYRadiusAttr     (PxSpan r)) = RyAttr (toString r, PX)
			genSVGImageAttr (BasicImgStrokeOpacityAttr op)       = StrokeOpacityAttr (toString op)
			genSVGImageAttr (BasicImgFillOpacityAttr   op)       = FillOpacityAttr (FillOpacity (toString op))
			genSVGImageAttr (BasicImgFillAttr        color)      = FillAttr (PaintColor color Nothing)
			genSVGImageAttr (BasicImgDashAttr        dash)       = StrokeDashArrayAttr (DashArray (strictTRMap toString dash))
			genSVGImageAttr _ = abort "Unexpected error in module SVGEditor (local function genSVGImageAttr of genSVGElts): unresolved span value encountered."
	genSVGHost no host=:(RawHostImg content) cid es markers paths spans grids
		= [RawElt content]
	genSVGHost no host=:(CompositeImg img) cid es markers paths spans grids
		= genSVGElts img cid es markers paths spans grids
	
	genSVGBasicHostImg :: !ImgTagNo !BasicImg ![SVGAttr] !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGBasicHostImg no EmptyImg attrs cid es markers paths spans grids
		= []
	genSVGBasicHostImg no (TextImg {fontfamily,fontysize,fontstyle,fontstretch,fontvariant,fontweight} txt) attrs cid es markers paths spans grids
		= [TextElt [XmlspaceAttr "preserve"] (keepTransformAttrsTogether (TransformAttr [TranslateTransform (toString 0.0) (toString (fontysize * 0.75))]) (attrs ++ fontAttrs)) txt]
    where
		fontAttrs = [ AlignmentBaselineAttr "auto"
		            , DominantBaselineAttr  "auto"
		            , FontFamilyAttr        fontfamily
		            , FontSizeAttr          (toString fontysize)
		            , FontStyleAttr         fontstyle
		            , FontStretchAttr       fontstretch
		            , FontVariantAttr       fontvariant
		            , FontWeightAttr        fontweight
		            , TextRenderingAttr     "geometricPrecision"
		            ]
	genSVGBasicHostImg no RectImg attrs cid es markers paths spans grids
		= [RectElt sizeAtts attrs]
	where
		sizeAtts          = case 'DM'.get no spans of
		                      Just (PxSpan w, PxSpan h) = mkWH (w,h)
		                      Just _                    = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "rect"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "rect"))
	genSVGBasicHostImg no CircleImg attrs cid es markers paths spans grids
		= [CircleElt [] [RAttr (radius,PX), CxAttr (radius,PX), CyAttr (radius,PX) : attrs]]
	where
		radius            = case 'DM'.get no spans of
		                      Just (PxSpan w,h)         = to2decString (w / 2.0)
		                      Just (_,_)                = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "circle"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "circle"))
	genSVGBasicHostImg no EllipseImg attrs cid es markers paths spans grids
		= [EllipseElt [] [RxAttr (xradius,PX), CxAttr (xradius,PX), RyAttr (yradius,PX), CyAttr (yradius,PX) : attrs]]
	where
		(xradius,yradius) = case 'DM'.get no spans of
		                      Just (PxSpan w, PxSpan h) = (to2decString (w / 2.0), to2decString (h / 2.0))
		                      Just _                    = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "ellipse"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "ellipse"))
	genSVGBasicHostImg no PolylineImg attrs cid es markers` paths spans grids
		= [ PolylineElt [] [PointsAttr (strictTRMap (polypointToPointsAttr "polyline") points) : attrs ++ markerAttrs]
		  : map (\elt -> DefsElt [] [] [elt]) markerElts		// PA: this is different from first version in which all marker-elements were collected in a single DefsElt
		  ]
	where
		markers                   = case 'DM'.get no markers` of
		                              Just m  = m
		                              nothing = defaultLineMarkers
		points                    = case 'DM'.get no paths of
		                              Just ps = ps.ImgPath.pathPoints
		                              nothing = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "polyline"))
		(markerElts, markerAttrs) = unzip (genSVGLineMarkers "polyline" markers cid es markers` paths spans grids)
	genSVGBasicHostImg no PolygonImg attrs cid es markers` paths spans grids
		= [ PolygonElt [] [PointsAttr (strictTRMap (polypointToPointsAttr "polygon") points) : attrs ++ markerAttrs]
		  : map (\elt -> DefsElt [] [] [elt]) markerElts		// PA: this is different from first version in which all marker-elements were collected in a single DefsElt
		  ]
	where
		markers                   = case 'DM'.get no markers` of
		                              Just m  = m
		                              nothing = defaultLineMarkers
		points                    = case 'DM'.get no paths of
		                              Just ps = ps.ImgPath.pathPoints
		                              nothing = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "polygon"))
		(markerElts, markerAttrs) = unzip (genSVGLineMarkers "polygon" markers cid es markers` paths spans grids)
	
	genSVGLineMarkers :: !String !LineMarkers !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [(SVGElt,SVGAttr)]
	genSVGLineMarkers elt {LineMarkers | lineStart,lineMid,lineEnd} cid es markers paths spans grids
		= [  genSVGLineMarker elt img posAttr cid es markers paths spans grids 
		  \\ (Just img,posAttr) <- [ (lineStart,MarkerStartAttr)
		                           , (lineMid,  MarkerMidAttr)
		                           , (lineEnd,  MarkerEndAttr)
		                           ]
		  ]
	where
		genSVGLineMarker :: !String !Img !(String -> SVGAttr) !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> (!SVGElt,!SVGAttr)
		genSVGLineMarker elt img=:{Img | uniqId} posAttr cid es markers paths spans grids
			= ( MarkerElt [ IdAttr mid ]
			              [ OrientAttr       "auto"
                          , ViewBoxAttr      "0" "0" wStr hStr
                          , RefXAttr         (wStr, PX)
                          , RefYAttr         (to2decString (h / 2.0), PX)
                          , MarkerHeightAttr (hStr, PX)
                          , MarkerWidthAttr  (wStr, PX)
                          ]
                          (genSVGElts img cid es markers paths spans grids)
			  , posAttr (mkUrl mid)
			  )
		where
			mid   = mkMarkerId cid uniqId
			(w,h) = case 'DM'.get uniqId spans of
			          Just (PxSpan w, PxSpan h) = (w,h)
			          Just _                    = abort (lookupSpanErrorMsg "genSVGLineMarkers" (unresolvedErrorMsg  elt))
			          nothing                   = abort (lookupSpanErrorMsg "genSVGLineMarkers" (unavailableErrorMsg elt))
			wStr = to2decString w
        	hStr = to2decString h

	genSVGOverlays :: ![Img] ![ImageOffset] !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGOverlays overlays offsets cid es markers paths spans grids
		= flatten [mkGroup [] (mkTransformTranslateAttr off) (genSVGElts img cid es markers paths spans grids) \\ img <- overlays & off <- offsets]
	where
		mkTransformTranslateAttr :: !ImageOffset -> [SVGAttr]
		mkTransformTranslateAttr (PxSpan dx,PxSpan dy)
		| dx == 0.0 && dy == 0.0   = []
		| otherwise                = [TransformAttr [TranslateTransform (to2decString dx) (to2decString dy)]]
		mkTransformTranslateAttr _ = abort (lookupSpanErrorMsg "genSVGOverlays" (unresolvedErrorMsg "Img"))
	
	polypointToPointsAttr :: !String !ImageOffset -> (!String,!String)
	polypointToPointsAttr elt (PxSpan dx,PxSpan dy) = (to2decString dx, to2decString dy)
	polypointToPointsAttr elt _                     = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg elt))
		
	unresolvedErrorMsg :: !String -> String
	unresolvedErrorMsg elt = "unresolved span value of " +++ elt +++ " encountered."
	
	unavailableErrorMsg :: !String -> String
	unavailableErrorMsg elt = "no span value exists for " +++ elt +++ "."
	
	lookupSpanErrorMsg :: !String !String -> String
	lookupSpanErrorMsg local_fun error = "Unexpected error in module SVGEditor (local function " +++ local_fun +++ " of genSVGElts): " +++ error

mkGroup :: ![HtmlAttr] ![SVGAttr] ![SVGElt] -> [SVGElt]
mkGroup _      _      []                  = []
mkGroup []     []     xs                  = xs
mkGroup hattrs []     [GElt [] sattrs xs] = [GElt hattrs sattrs xs]
mkGroup []     sattrs [GElt hattrs [] xs] = [GElt hattrs sattrs xs]
mkGroup []     [tfattr=:(TransformAttr [TranslateTransform x y])] xs = map f xs
where
  f :: !SVGElt -> SVGElt
  f (GElt        hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = GElt       hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs) elts
  f (GElt        hattrs attrs elts)                                              = GElt       hattrs (keepTransformAttrsTogether tfattr attrs) elts
  f (TextElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs] elts) = TextElt    hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs) elts
  f (TextElt     hattrs attrs elts)                                              = TextElt    hattrs (keepTransformAttrsTogether tfattr attrs) elts
  f (EllipseElt  hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = EllipseElt hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs)
  f (EllipseElt  hattrs attrs)                                                   = EllipseElt hattrs (keepTransformAttrsTogether tfattr attrs)
  f (RectElt     hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = RectElt    hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs)
  f (RectElt     hattrs attrs)                                                   = RectElt    hattrs (keepTransformAttrsTogether tfattr attrs)
  f (CircleElt   hattrs [TransformAttr [TranslateTransform x` y`] : attrs])      = CircleElt  hattrs (keepTransformAttrsTogether (dualTransformTranslate x y x` y`) attrs)
  f (CircleElt   hattrs attrs)                                                   = CircleElt  hattrs (keepTransformAttrsTogether tfattr attrs)
  f (LineElt _ [X1Attr (x1, PX), X2Attr (x2, PX), Y1Attr (y1, PX), Y2Attr (y2, PX) : attrs]) = LineElt [] [X1Attr (lineAdd x1 x, PX), X2Attr (lineAdd x2 x, PX), Y1Attr (lineAdd y1 y, PX), Y2Attr (lineAdd y2 y, PX) : attrs]
  f elt                                                                                      = GElt    [] [tfattr] [elt]

  lineAdd :: !String !SVGNumber -> String
  lineAdd strVal n = to2decString (toReal strVal + toReal n)
mkGroup has    sas elts = [GElt has sas elts]

dualTransformTranslate :: !a !a !a !a -> SVGAttr | toReal a
dualTransformTranslate x y x` y` = TransformAttr [TranslateTransform (to2decString (toReal x + toReal x`)) (to2decString (toReal y + toReal y`))]

// PA: this is rather cumbersome;
// better plan is to keep the SVG transforms separate when creating the Img and in the end do put them at the end of the [SVGAttr]-list where they seem to end up
keepTransformAttrsTogether :: !SVGAttr ![SVGAttr] -> [SVGAttr]
keepTransformAttrsTogether (TransformAttr tfs) attrs
	= filter (not o isTransformAttr) attrs ++ [TransformAttr (tfs ++ flatten [tfs` \\ TransformAttr tfs` <- attrs])]
keepTransformAttrsTogether attr attrs
	= [attr : attrs]

isTransformAttr :: !SVGAttr -> Bool
isTransformAttr (TransformAttr _) = True
isTransformAttr _ = False
