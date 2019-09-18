implementation module iTasks.Extensions.SVG.SVGEditor

import iTasks.UI.JavaScript, iTasks.UI.Definition, iTasks.UI.Editor, iTasks.Internal.Serialization
import Graphics.Scalable.Image
import Graphics.Scalable.Internal.Image`
import StdEnv
import Data.Either
import Data.Func
import Data.Functor
import Data.GenEq
import Data.List
import qualified Data.Map
import Data.MapCollection
import Data.Maybe
import Data.Matrix
import qualified Data.Foldable
import qualified Data.Set
from Data.Map import :: Map, instance Functor (Map k)
from Data.Set import :: Set, instance == (Set a), instance < (Set a), instance Foldable Set
import Math.Geometry
import Text
import Text.GenJSON
import Text.HTML

import StdDebug
trace_n` a b :== /*trace_n a*/ b
trace`   a b :== /*trace   a*/ b
jsTrace` a b :== /*jsTrace a*/ b

timeTrace :: !String !*JSWorld -> *JSWorld
timeTrace msg world
  #! (ms,world) = getCurrentTimeInMilliseconds world
  #! ms`        = toString ms
  #! world      = jsTrace (ms`%(size ms`-7,size ms`-1) +++ "\tms:\t" +++ msg) world
  = world

from iTasks.Internal.Generic.Visualization import <+++, generic gText
class short a :: !a -> String
instance short FontDef where short fontdef = "{FontDef | " <+++ getfontfamily fontdef <+++ "," <+++ getfontysize fontdef <+++ "}"
instance short String  where short str     = "\"" +++ str +++ "\""
instance short Real    where short r       = toString r
instance short (a,b) | short a & short b where short (a,b) = "(" <+++ short a <+++ "," <+++ short b <+++ ")"

derive gEditor  EditMode
derive gText    EditMode
derive gDefault EditMode
derive gEq      EditMode

// JavaScript object attribute labels:
// Client side state (access via jsMakeCleanReference and jsGetCleanReference):
JS_ATTR_VIEW				:== "view"
JS_ATTR_MODEL        		:== "model"
FONT_WEB_STORAGE_KEY f		:== FontKeyWithoutDefaults f
TEXT_WEB_STORAGE_KEY f str	:== FontKeyWithoutDefaults f +++ "-" +++ str
jsLocalStorage				:== jsGlobal "localStorage"
FontKeyWithoutDefaults f	= "{FontDef` | " <+++ join "," (filter ((<>) "normal") [f.fontfamily`,toString f.fontysize`,f.fontstretch`,f.fontstyle`,f.fontvariant`,f.fontweight`]) <+++ "}"

// Server -> Client SVG attribute names (tag a serialized value of type ServerToClientAttr):
JS_ATTR_SVG					:== "svgPart"

CLICK_DELAY					:== 225
svgns						 =: "http://www.w3.org/2000/svg"

//Predefined object methods
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
(`setItem`)               obj args :== obj .# "setItem"               .$ args
(`getItem`)               obj args :== obj .# "getItem"               .$ args
(`now`)                   obj args :== obj .# "now"                   .$ args

:: EventCapture = EventBubbling | EventCapturing
instance toBool EventCapture
   where toBool EventBubbling  = False
         toBool EventCapturing = True

addEventListener :: !JSObj !String !EventCapture !({!JSVal} *JSWorld -> *JSWorld) !JSVal !*JSWorld -> *JSWorld
addEventListener svg event useCapture callback me world
  // add event listener
  #! (jsCallback,world)  = jsWrapFun callback me world
  #! world          = (svg .# "addEventListener" .$! (event,jsCallback,toBool useCapture)) world
  // store callback so that it can be freed in clientUpdateSVGString
  #! (refs,world)   = jsGetCleanReference (me .# "refs") world
  #! refs           = case refs of
      Nothing        -> ([], [jsCallback])
      Just (old,new) -> (old, [jsCallback:new])
  #! (jsRefs,world) = jsMakeCleanReference refs me world
  #! world          = (me .# "refs" .= jsRefs) world
  = world

getCurrentTimeInMilliseconds :: !*JSWorld -> (!Int,!*JSWorld)
getCurrentTimeInMilliseconds world
  #! (ms,world)     = (jsGlobal "Date.now" .$ ()) world
  = (jsValToInt` 0 ms,world)

:: ImageSpanReal :== (!Real, !Real)

:: DropTarget      = DropTarget
:: MousePos        = MouseUp | MouseDown
derive gEq MousePos
:: SVGDragState v  = 
  { svgMousePos     :: !MousePos
  , svgDropCallback :: !SVGDragFun v
  , svgTrueCoordsX  :: !Real
  , svgTrueCoordsY  :: !Real
  , svgGrabPointX   :: !Real
  , svgGrabPointY   :: !Real
  , svgDragTarget   :: !Maybe JSObj
  }

initDragState
  =: {SVGDragState | svgMousePos     = MouseUp
                   , svgDropCallback = \_ _ v -> v
                   , svgTrueCoordsX  = 0.0
                   , svgTrueCoordsY  = 0.0
                   , svgGrabPointX   = 0.0
                   , svgGrabPointY   = 0.0
                   , svgDragTarget   = Nothing
     }

:: MouseCallbackData                                      // information required for dealing with mouse events:
 = MouseOnClickData !Int                                  // mouse has been clicked n times
 | MouseNoData                                            // no additional data
// only for debugging:
instance toString MouseCallbackData
   where toString (MouseOnClickData n) = "(MouseOnClickData " +++ toString n +++ ")"
         toString MouseNoData          = "MouseNoData"
instance toString ViaImg
   where toString (ViaChild n) = "(ViaChild " +++ toString n +++ ")"
         toString ViaHost      = "ViaHost"
         toString ViaAttr      = "ViaAttr"

:: FontSpans` :== Map FontDef` FontDescent                // of each font, the font descent
:: TextSpans` :== Map FontDef` (Map String TextSpan)      // of each font, of each text of that font, the width
:: ImgFonts`  :== Set FontDef`                            // the collection of fonts used in the image for which no metrics are available
:: ImgTexts`  :== Map FontDef` (Set String)               // of each font, the collection of texts

// Client -> server communication (type parameter edit of LeafEditor):
:: ClientToServerMsg s
 = ClientNeedsSVG                                         // client is ready to receive SVG
 | ClientHasNewModel !s                                   // client has created a new public model value
 | ClientHasNewTextMetrics !FontSpans` !TextSpans`        // client has determined text metrics (reply to toSVGTextMetricsAttr server notifications)
derive JSONEncode ClientToServerMsg, Map
derive JSONDecode ClientToServerMsg, Map

//	SVG attribute for server -> client communication (use JSEncode/JSDecode for serialization)
:: ServerToClientAttr s
 = ServerNeedsTextMetrics !ImgFonts` !ImgTexts`                            // server needs fonts and texts metrics
 | ServerHasSVG           !String !ImgEventhandlers` !ImgTags !(Maybe s)   // server has computed new image and event handlers, (Just model) in case client-model needs to be updated

toUIAttributes :: !(ServerToClientAttr s) !*VSt -> (!UIAttributes, !*VSt)
toUIAttributes attr vst
  # (attr,vst) = serializeForClient attr vst
  = ('Data.Map'.fromList [(JS_ATTR_SVG,JSONString attr)], vst)

fromUIAttributes :: !*String !*JSWorld -> (!ServerToClientAttr s,!*JSWorld)
fromUIAttributes json world
  = jsDeserializeGraph json world

ensure_uniqueness :: !String -> *String
ensure_uniqueness _ = code {
	no_op
}

//	the server side state:
:: ServerSVGState s
   = { model :: !s
     , fonts :: !FontSpans
     , texts :: !TextSpans
     }
derive JSONEncode ServerSVGState
derive JSONDecode ServerSVGState

initServerSVGState :: !s -> ServerSVGState s
initServerSVGState model = {ServerSVGState | model = model, fonts = 'Data.Map'.newMap, texts = 'Data.Map'.newMap}

imgTagSource :: !String -> *TagSource
imgTagSource taskId
  = [(ImageTagUser no taskId, ImageTagUser no taskId) \\ no <- [0..]]

newImgTables :: ImgTables
newImgTables
  = {ImgTables | imgEventhandlers = 'Data.Map'.newMap
               , imgNewFonts      = 'Data.Set'.newSet
               , imgNewTexts      = 'Data.Map'.newMap
               , imgMasks         = 'Data.Map'.newMap
               , imgLineMarkers   = 'Data.Map'.newMap
               , imgPaths         = 'Data.Map'.newMap
               , imgSpans         = 'Data.Map'.newMap
               , imgGrids         = 'Data.Map'.newMap
               , imgTags          = 'Data.Map'.newMap
               , imgUniqIds       = 0
    }

//	PA: this is actually redundant code and should use functions from Text.HTML and svgFontDefAttrs below
svgFontDefAttrPairs :: !FontDef -> [(String,String)]
svgFontDefAttrPairs fontdef//{FontDef | fontfamily,fontysize,fontstyle,fontstretch,fontvariant,fontweight}
	= [ ("font-family",        (getfontfamily  fontdef))
      , ("font-size",          toString (getfontysize fontdef))
      , ("font-stretch",       (getfontstretch fontdef))
      , ("font-style",         (getfontstyle   fontdef))
      , ("font-variant",       (getfontvariant fontdef))
      , ("font-weight",        (getfontweight  fontdef))
      , ("alignment-baseline", "auto")
      , ("dominant-baseline",  "auto")
      ]

svgFontDefAttrs :: !FontDef -> [SVGAttr]
svgFontDefAttrs fontdef//{FontDef | fontfamily,fontysize,fontstyle,fontstretch,fontvariant,fontweight}
	= [ FontFamilyAttr        (getfontfamily  fontdef)
      , FontSizeAttr          (toString (getfontysize fontdef))
      , FontStretchAttr       (getfontstretch fontdef)
      , FontStyleAttr         (getfontstyle   fontdef)
      , FontVariantAttr       (getfontvariant fontdef)
      , FontWeightAttr        (getfontweight  fontdef)
      , AlignmentBaselineAttr "auto"
      , DominantBaselineAttr  "auto"
      , TextRenderingAttr     "geometricPrecision"
      ]


//	transform the functions of an SVGEditor into an Editor via a LeafEditor:
fromSVGEditor :: !(SVGEditor s v) -> Editor s | gEq{|*|}, gText{|*|}, JSONEncode{|*|}, JSONDecode{|*|} s
fromSVGEditor svglet = leafEditorToEditor
    { LeafEditor
    | genUI          = withClientSideInit (initClientSideUI svglet) initServerSideUI
    , onEdit         = serverHandleEditFromClient  svglet
    , onRefresh      = serverHandleEditFromContext svglet
    , valueFromState = valueFromState
    }
where
//	initServerSideUI is called first.
//	Its sole purpose is to tell the client which model value is being manipulated.
	initServerSideUI :: !UIAttributes !DataPath !(EditMode s) !*VSt -> *(!MaybeErrorString (!UI,!ServerSVGState s), !*VSt)
	initServerSideUI uiAttrs dp mode world=:{VSt | taskId}
	  = case editModeValue mode of
	      Nothing    = (Error "Error in module SVGEditor (fromSVGEditor/initServerSideUI): SVG editors cannot be used in Enter EditMode.",world)
	      Just model
	          #! (serializedModel,world) = serializeForClient model world
	          = trace_n` ("initServerSideUI of task with taskId = " +++ taskId)
	                   (Ok (uia UIComponent ('Data.Map'.union uiAttrs ('Data.Map'.union (valueAttr (JSONString serializedModel)) (sizeAttr FlexSize FlexSize))),initServerSVGState model),world)

//	initClientSideUI is called after initServerSideUI.
//	Information exchange from server -> client occurs via the attributes of the client object.
//  First clientInitDOMEl initialises the client. Subsequent changes are handled with clientHandleAttributeChange.
//	Information exchange from client -> server occurs via `doEditEvent` that emits a triplet (taskId,editId,json) in which json 
//	is the serialized data that the client sends to the server. The server receives this serialized data via serverHandleEditFromClient.
//  The first such `doEditEvent' is a request from the client to compute the SVG body (request generated by clientInitDOMEl).
	initClientSideUI :: !(SVGEditor s v) !JSObj !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	initClientSideUI svglet me world
	// Set attributes
      #! world                       = timeTrace "initClientSideUI started at " world
      #! world                       = (me .# "clickCount" .= 0) world
	  #! (jsDragState,world)         = jsMakeCleanReference initDragState me world
	  #! world                       = (me .# "dragState" .= jsDragState) world
	// Set methods	
	  #! (jsOnAttributeChange,world) = jsWrapFun (clientHandleAttributeChange svglet me) me world
	  #! world                       = (me .# "onAttributeChange" .= jsOnAttributeChange) world
	  #! (jsInitDOMEl,world)         = jsWrapFun (clientInitDOMEl svglet me) me world
	  #! world                       = (me .# "initDOMEl" .= jsInitDOMEl) world
	  #! world                       = timeTrace "initClientSideUI ended at " world
	  = world

//	serverHandleEditFromClient is called at the server side whenever the associated client component has evaluated `doEditEvent`.
//	The server component deserializes the received json data to determine the proper action.
 	serverHandleEditFromClient :: !(SVGEditor s v) !DataPath !(!DataPath,!ClientToServerMsg s) !(ServerSVGState s) !*VSt -> (!MaybeErrorString (!UIChange,!ServerSVGState s), !*VSt) | gText{|*|} s
  	serverHandleEditFromClient svglet _ (_,ClientHasNewModel new) mask=:{ServerSVGState | fonts,texts} vst
  	  #! (set_attrs,mask,vst) = serverHandleModel svglet {ServerSVGState | mask & model=new} False vst
  	  = trace_n` ("serverHandleEditFromClient (ClientHasNewModel " <+++ new <+++ ")")
  	    (Ok (attributesToUIChange set_attrs,mask),vst)
  	serverHandleEditFromClient svglet _ (_,ClientHasNewTextMetrics new_font_metrics new_texts_metrics) mask=:{ServerSVGState | model=old,fonts,texts} vst 
      #! font_spans           = 'Data.Map'.union                      new_font_metrics  fonts
      #! text_spans           = 'Data.Map'.unionWith 'Data.Map'.union new_texts_metrics texts
      #! mask                 = {ServerSVGState | mask & fonts=font_spans, texts=text_spans}
      #! (set_attrs,mask,vst) = serverHandleModel svglet mask True vst
      = trace_n` ("serverHandleEditFromClient (ClientHasNewTextMetrics [" +++ 
                 join "," (map short ('Data.Map'.keys new_font_metrics)) +++ 
                 "] ["                                                   +++ 
                 join "," (map short ('Data.Map'.toAscList ('Data.Map'.unions (map snd ('Data.Map'.toAscList new_texts_metrics))))) +++ 
                 "]"
                )
        (Ok (attributesToUIChange set_attrs,mask),vst)
    serverHandleEditFromClient svglet _ (_,ClientNeedsSVG) mask=:{ServerSVGState | model=old,fonts,texts} vst
	  #! (attrs,mask,vst) = serverHandleModel svglet mask False vst
	  = trace_n` ("serverHandleEditFromClient ClientNeedsSVG")
	    (Ok (attributesToUIChange attrs,mask),vst)

//	serverHandleEditFromContext is called at the server side whenever the context has acquired a new data model that needs to be rendered at the associated client component.	
//	This information is passed to the associated client via its attributes, and will be handled via the `onAttributeChange` function.
	serverHandleEditFromContext :: !(SVGEditor s v) !DataPath !s !(ServerSVGState s) !*VSt -> (!MaybeErrorString (!UIChange,!ServerSVGState s), !*VSt) | gEq{|*|} s
	serverHandleEditFromContext svglet _ new mask=:{ServerSVGState | model=old,fonts,texts} vst
  	| gEq{|*|} old new
  		= (Ok (NoChange,mask),vst)
  	#! (set_attrs,mask`,vst`) = serverHandleModel svglet {ServerSVGState | mask & model=new} True vst
  	= trace_n` ("serverHandleEditFromContext")
  	  (Ok (attributesToUIChange set_attrs,mask`),vst`)
	
//	valueFromState, using a LeafEditor always succeeds
	valueFromState :: !(ServerSVGState s) -> *Maybe s
	valueFromState {ServerSVGState | model} = Just model

//	serverHandleModel is called whenever a new model/view value has been obtained.
//	It computes the SVG rendering on the server side.
//	This may `fail' due to missing font/text metrics, in which case these are requested to the client via the attributes.
//  If it succeeds, then the client receives the fully evaluated SVG and the defunctionalized event handlers that need to be registered via the attributes.
//	The client handles these changes via clientHandleAttributeChange.
serverHandleModel :: !(SVGEditor s v) !(ServerSVGState s) !Bool !*VSt -> (!UIAttributes,!ServerSVGState s,!*VSt)
serverHandleModel svglet state=:{ServerSVGState | model,fonts=font_spans,texts=text_spans} model_is_new_for_client world=:{VSt | taskId}
  = case serverSVG svglet font_spans text_spans taskId model view of                            // start to generate the image server-side
      Left (img,tables=:{ImgTables | imgNewFonts=new_fonts,imgNewTexts=new_texts})              // image incomplete because of missing font/text-width information
	    #! (attrs,world) = toUIAttributes` svglet (ServerNeedsTextMetrics new_fonts new_texts) world
	    #! attrs = 'Data.Map'.union attrs size_and_model
	    = (attrs, state, world)
      Right (svg,es,tags)                                                                       // image complete, send it to client
	    #! string = toString svg
	    #! string = trace_n` ("serverHandleModel generates complete SVG string of size " +++ toString (size string) +++ " Bytes") string
	    | model_is_new_for_client
	      #! (attrs,world) = toUIAttributes` svglet (ServerHasSVG string es tags (Just model)) world
	      #! attrs = 'Data.Map'.union attrs size_and_model
	      = (attrs, state, world)
	    | otherwise
	      #! (attrs,world) = toUIAttributes` svglet (ServerHasSVG string es tags Nothing) world
	      #! attrs = 'Data.Map'.union attrs size_and_model
	      = (attrs, state, world)
where
	view           = svglet.initView model
	size_and_model = sizeAttr FlexSize FlexSize

// this auxiliary function is necessary to resolve otherwise internal overloading because of the type parameter s
toUIAttributes` :: (SVGEditor s v) !(ServerToClientAttr s) !*VSt -> (!UIAttributes, !*VSt)
toUIAttributes` svglet msg vst = toUIAttributes msg vst

attributesToUIChange :: !UIAttributes -> UIChange
attributesToUIChange set_attrs
  = trace_n` ("attributesToUIChange: attributes to set = [" +++ join "," ('Data.Map'.keys set_attrs) +++ "]") (
    ChangeUI [SetAttribute label value \\ (label,value) <- 'Data.Map'.toList set_attrs] []
    )

//	server side rendering of model value:
serverSVG :: !(SVGEditor s v) !FontSpans !TextSpans !String !s !v -> Either (!Img,!ImgTables) (!SVGElt,!ImgEventhandlers`,!ImgTags)
serverSVG {SVGEditor | renderImage} font_spans text_spans taskId s v
  #! image`               = trace_n` "serverSVG starts to render image" (renderImage s v (imgTagSource taskId))
  #! (img,tables=:{ImgTables | imgNewFonts=new_fonts,imgNewTexts=new_texts})
                          = trace_n` "serverSVG starts toImg" (toImg image` [] font_spans text_spans newImgTables)
  | not ('Data.Set'.null new_fonts) || not ('Data.Map'.null new_texts)                    // some font / text-width information is missing: need to ask the client
      = Left (img,tables)
  #! {ImgTables | imgEventhandlers=es,imgMasks=masks,imgLineMarkers=markers,imgPaths=paths,imgSpans=spans,imgGrids=grids,imgTags=tags}
                          = tables
  = case trace_n` "serverSVG starts resolve_all_spans" (resolve_all_spans tags font_spans text_spans img masks markers paths spans grids) of
      Error error         = abort error
      Ok (img,masks,markers,paths,spans,grids)
        #! svg            = trace_n` "serverSVG starts genSVGElt" (genSVGElt img taskId ('Data.Map'.keys es) masks markers paths spans grids)
        = Right (svg,es,tags)

clientGetTaskId :: !JSVal !*JSWorld -> (!String,!*JSWorld)
clientGetTaskId me world
  #! (cidJS,world) = me .# "attributes.taskId" .? world
  #! taskId        = jsValToString` "" cidJS
  = (taskId,world)

//	client side initialisation of DOM:
//  The client receives the model value via the .value attribute and stores it at the client side.
//  This makes the client ready to receive the SVG rendering that is computed at the server side (via `doEditEvent' and ClientNeedsSVG message).
clientInitDOMEl :: !(SVGEditor s v) !JSVal !{!JSVal} !*JSWorld -> *JSWorld | JSONEncode{|*|} s
clientInitDOMEl svglet me args world
  #! (model,  world) = me .# "attributes.value" .? world
  #! (model,  world) = jsDeserializeGraph (ensure_uniqueness (jsValToString` "" model)) world
  #! (jsView, world) = jsMakeCleanReference (svglet.initView model) me world
  #! (jsModel,world) = jsMakeCleanReference model me world
  #! world           = (me .# JS_ATTR_VIEW  .= jsView) world
  #! world           = (me .# JS_ATTR_MODEL .= jsModel) world
  #! (cidJS,  world) = me .# "attributes.taskId".? world
  #! (editId, world) = me .# "attributes.editorId" .? world
  #! (_,      world) = (me .# "doEditEvent" .$ (cidJS,editId,toJSON` svglet ClientNeedsSVG)) world
  = jsTrace` "clientInitDOMEl"
    world

//  this auxiliary function is necessary to resolve otherwise internal overloading because of the type parameter s
toJSON` :: (SVGEditor s v) !(ClientToServerMsg s) -> JSONNode | JSONEncode{|*|} s
toJSON` _ msg = toJSON msg

//	client side handling of server requests via attributes:
clientHandleAttributeChange :: !(SVGEditor s v) !JSVal !{!JSVal} !*JSWorld -> *JSWorld | JSONEncode{|*|} s
clientHandleAttributeChange svglet me args world
  #! world = timeTrace ("clientHandleAttributeChange [" +++ join "," (map fst nv_pairs) +++ "] started at ") world
  = case svg_or_text of
      Just json
        #! (request,world) = fromUIAttributes (ensure_uniqueness (jsValToString` "" json)) world
        = case request of
            (ServerNeedsTextMetrics new_fonts new_texts)
              #! world     = timeTrace "clientHandleAttributeChange (ServerNeedsTextMetrics) started at " world
              #! world     = clientHandlesTextMetrics svglet new_fonts new_texts me world
              #! world     = timeTrace "clientHandleAttributeChange (ServerNeedsTextMetrics) ended at " world
              = world
            (ServerHasSVG svg_body svg_handlers svg_tags new_model)
              #! world     = timeTrace "clientHandleAttributeChange (ServerHasSVG) started at " world
              #! world     = clientUpdateSVGString svg_body me world
              #! world     = clientRegisterEventhandlers svglet me svg_handlers svg_tags world
              = case new_model of
                  Nothing  = timeTrace "clientHandleAttributeChange (ServerHasSVG, no new model) ended at " world
                  Just model
                    #! (jsView, world) = jsMakeCleanReference (svglet.initView model) me world
                    #! (jsModel,world) = jsMakeCleanReference model me world
                    #! world           = (me .# JS_ATTR_VIEW  .= jsView) world
                    #! world           = (me .# JS_ATTR_MODEL .= jsModel) world
                    #! world           = timeTrace "clientHandleAttributeChange (ServerHasSVG, new model) ended at " world
                    = world
      _ = timeTrace "clientHandleAttributeChange (no action) ended at " world
where
	nv_pairs    = to_name_value_pairs [a \\ a <-: args]
	svg_or_text = lookup JS_ATTR_SVG nv_pairs
	
	to_name_value_pairs :: ![JSVal] -> [(String,JSVal)]
	to_name_value_pairs [n,v : nvs] = [(jsValToString` "" n,v) : to_name_value_pairs nvs]
	to_name_value_pairs _           = []

	clientHandlesTextMetrics :: !(SVGEditor s v) !ImgFonts !ImgTexts !JSVal !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	clientHandlesTextMetrics svglet new_fonts new_texts me world
	  #! world                  = timeTrace "clientHandlesTextMetrics started at " world
	  #! (new_font_spans,world) = getNewFontSpans  new_fonts me world                              // Get missing font spans
	  #! (new_text_spans,world) = getNewTextsSpans new_texts me world                              // Get missing text width spans
	  #! (cidJS,         world) = me .# "attributes.taskId" .? world
	  #! (editId,        world) = me .# "attributes.editorId" .? world
	  #! (_,             world) = (me .# "doEditEvent" .$ (cidJS,editId,toJSON` svglet (ClientHasNewTextMetrics new_font_spans new_text_spans))) world
	  #! world                  = timeTrace "clientHandlesTextMetrics ended at " world
	  = world
	
	clientRegisterEventhandlers :: !(SVGEditor s v) !JSVal !ImgEventhandlers` !ImgTags !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	clientRegisterEventhandlers svglet=:{SVGEditor | renderImage} me es tags world
	  #! (taskId,world) = clientGetTaskId me world
	  #! world          = clientRegisterEventhandlers` svglet me taskId es tags world
	  = world

//	generate the entire SVG element from an Img with all spans resolved:
genSVGElt :: !Img !String ![ImgTagNo] !ImgMasks !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> SVGElt
genSVGElt img taskId interactive_imgs masks markers paths spans grids
  #! mask_defs      = genSVGMasks masks taskId interactive_imgs markers paths spans grids
  #! svg_elems      = genSVGElts  img   taskId interactive_imgs markers paths spans grids
  #! (imXSp,imYSp)  = getImgRootSize img spans
  #! imXSp          = to2decString imXSp
  #! imYSp          = to2decString imYSp
  = SVGElt [WidthAttr imXSp, HeightAttr imYSp, XmlnsAttr svgns] [VersionAttr "1.1", ViewBoxAttr "0" "0" imXSp imYSp] (mask_defs ++ svg_elems)

//	update the DOM element with the new SVG content, represented as a string:
clientUpdateSVGString :: !String !JSVal !*JSWorld -> *JSWorld
clientUpdateSVGString svgStr me world
  #! world           = timeTrace "clientUpdateSVGString started at " world
  #! (parser, world) = jsNew "DOMParser" () world
  #! (doc,    world) = (parser .# "parseFromString" .$ (svgStr, "image/svg+xml")) world
  #! (newSVG, world) = doc .# "firstChild" .? world
  #! (domEl,  world) = me .# "domEl" .? world
  #! (currSVG,world) = domEl .# "firstChild" .? world
  #! (_,      world) = if (jsIsNull currSVG)
                          ((domEl `appendChild` newSVG) world)
                          ((domEl .# "replaceChild" .$ (newSVG, currSVG)) world)
  // Free old callbacks (see #298 for discussion)
  #! (refs,world)    = jsGetCleanReference (me .# "refs") world
  | isNothing refs
    #! world         = timeTrace "clientUpdateSVGString (no freeing of callbacks) ended at " world
    = world
  #! (old,new)       = fromJust refs
  #! world           = seqSt jsFreeCleanReference old world
  #! (jsRefs,world)  = jsMakeCleanReference (new,[]) me world
  #! world           = (me .# "refs" .= jsRefs) world
  #! world           = timeTrace "clientUpdateSVGString (with freeing of callbacks) ended at " world
  = world

//	return the dimensions of the root image:
getImgRootSize :: !Img !ImgSpans -> (!Real,!Real)
getImgRootSize img=:{Img | uniqId} spans
	= case 'Data.Map'.find uniqId spans of
	    (PxSpan w,PxSpan h) = (w,h)
	    _                   = abort "Unexpected error in module SVGEditor (getImgRootSize): size of root image is undetermined."

//	generate the svg-defs for the masks used in this image:
genSVGMasks :: !ImgMasks !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
genSVGMasks masks taskId es markers paths spans grids
	= [  DefsElt [] [] [MaskElt [IdAttr (mkMaskId taskId no)] [] (genSVGElts m taskId es markers paths spans grids)]
	  \\ (no,m) <- 'Data.Map'.toList masks
	  ]

//	measure font dimensions:
getNewFontSpans :: !ImgFonts !JSVal !*JSWorld -> (!FontSpans,!*JSWorld)
getNewFontSpans fonts me world
  #! (cached,new,world)    = loadCachedFontsSpans fonts world
  | 'Data.Set'.null new    = (cached,world)
  #! (measured, world)     = calcImgFontsSpans new world
  #! world                 = storeFontsSpansToCache measured world
  | otherwise              = ('Data.Map'.union cached measured,world)
where
// load cached font dimensions
	loadCachedFontsSpans :: !ImgFonts !*JSWorld -> (!FontSpans,!ImgFonts,!*JSWorld)
	loadCachedFontsSpans fonts world
	  #! (jsWebStorage,world) = jsLocalStorage .? world
	  = 'Data.Foldable'.foldl (loadCachedFontSpan jsWebStorage) ('Data.Map'.newMap,fonts,world) ('Data.Set'.toList fonts)
	where
		loadCachedFontSpan :: !JSVal !*(!FontSpans,!ImgFonts,!*JSWorld) !FontDef -> *(!FontSpans,!ImgFonts,!*JSWorld)
		loadCachedFontSpan jsWebStorage (cached,new,world) font
		  #! (v,world)     = (jsWebStorage `getItem` (FONT_WEB_STORAGE_KEY font)) (jsTrace` ("loadCachedFontSpan \"" +++ FONT_WEB_STORAGE_KEY font +++ "\"") world)
		  | jsIsNull v     = jsTrace` ("(loadCachedFontSpan " +++ FONT_WEB_STORAGE_KEY font +++ ") retrieved null value ") (cached,new,world)                                                              // font metric not in cache, need to measure (remains in new)
		  | otherwise      = ('Data.Map'.put font (jsValToReal` (getfontysize` font) v) cached,'Data.Set'.delete font new,world)   // font metric in cache, no need to measure (remove from new)

// store new font dimensions
	storeFontsSpansToCache :: !FontSpans !*JSWorld -> *JSWorld
	storeFontsSpansToCache fonts world
	  #! (jsWebStorage,world) = jsLocalStorage .? world
	  = 'Data.Foldable'.foldl (storeFontSpan jsWebStorage) world ('Data.Map'.toList fonts)
	where
		storeFontSpan :: !JSVal !*JSWorld !(!FontDef,!FontDescent) -> *JSWorld
		storeFontSpan jsWebStorage world (font,descent)
		  #! (_,world) = (jsWebStorage `setItem` (FONT_WEB_STORAGE_KEY font,descent)) (jsTrace` ("storeFontSpan (" +++ FONT_WEB_STORAGE_KEY font +++ "," +++ toString descent +++ ")") world)
		  = world

// compute the font dimensions of new fonts that are used in an image
	calcImgFontsSpans :: !ImgFonts !*JSWorld -> (!FontSpans,!*JSWorld)
	calcImgFontsSpans fonts world
	  #! (svg, world) = (jsDocument `createElementNS` (svgns, "svg")) world
	  #! (body,world) = jsDocument .# "body" .? world
	  #! (_,   world) = (body `appendChild` svg) world
	  #! (elem,world) = (jsDocument `createElementNS` (svgns, "text")) world
	  #! (_,   world) = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
	  #! (_,   world) = (svg `appendChild` elem) world
	  #! (res, world) = 'Data.Foldable'.foldl (calcFontSpan elem) ('Data.Map'.newMap,world) ('Data.Set'.toList fonts)
	  #! (_,   world) = (svg `removeChild` elem) world
	  #! (_,   world) = (body `removeChild` svg) world
	  = (res,  world)
	where
		calcFontSpan :: !JSVal !*(!FontSpans,!*JSWorld) !FontDef -> *(!FontSpans,!*JSWorld)
		calcFontSpan elem (font_spans,world) fontdef
		  #! world       = foldl (\world args -> snd ((elem `setAttribute` args) world)) world [("x", "-10000"), ("y", "-10000") : svgFontDefAttrPairs fontdef]
		  #! (fd, world) = calcFontDescent elem (getfontysize fontdef) world
		  = ('Data.Map'.put fontdef fd font_spans, world)
		
		calcFontDescent :: !JSVal !Real !*JSWorld -> (!Real, !*JSWorld)
		// same heuristic as used below (at function 'genSVGBasicHostImg'), must be replaced by proper determination of descent of current font
		calcFontDescent elem fontysize world
		  = (fontysize * 0.25,world)

//	measure text dimensions:
getNewTextsSpans :: !ImgTexts !JSVal !*JSWorld -> (!TextSpans,!*JSWorld)
getNewTextsSpans texts me world
  #! (cached,new,world) = loadCachedTextsSpans texts world
  | 'Data.Map'.null new = (cached,world)
  #! (measured,world)   = calcImgTextsLengths new world
  #! world              = storeTextsSpansToCache measured world
  | otherwise           = ('Data.Map'.unionWith 'Data.Map'.union measured cached,world)
where
//	load cached texts spans
	loadCachedTextsSpans :: !ImgTexts !*JSWorld -> (!TextSpans,!ImgTexts,!*JSWorld)
	loadCachedTextsSpans texts world
	  #! (jsWebStorage,world) = jsLocalStorage .? world
	  = 'Data.Foldable'.foldl (loadCachedTextSpans jsWebStorage) ('Data.Map'.newMap,texts,world) ('Data.Map'.toList texts)
	where
		loadCachedTextSpans :: !JSVal !*(!TextSpans,!ImgTexts,!*JSWorld) !(!FontDef,!Set String) -> *(!TextSpans,!ImgTexts,!*JSWorld)
		loadCachedTextSpans jsWebStorage (cached,new,world) (font,strs)
		  = 'Data.Foldable'.foldl (loadCachedTextSpan jsWebStorage font) (cached,new,world) ('Data.Set'.toList strs)
		where
			loadCachedTextSpan :: !JSVal !FontDef !*(!TextSpans,!ImgTexts,!*JSWorld) !String -> *(!TextSpans,!ImgTexts,!*JSWorld)
			loadCachedTextSpan jsWebStorage font (cached,new,world) str
			  #! (v,world) = (jsWebStorage `getItem` (TEXT_WEB_STORAGE_KEY font str)) (jsTrace` ("loadCachedTextSpan \"" +++ TEXT_WEB_STORAGE_KEY font str +++ "\"") world)
			  | jsIsNull v = jsTrace` ("(loadCachedTextSpan " +++ TEXT_WEB_STORAGE_KEY font str +++ ") retrieved null value ") (cached,new,world)
			  | otherwise  = ('Data.Map'.alter (merge ('Data.Map'.singleton str (jsValToReal` zero v))) font cached,'Data.Map'.alter (remove str) font new,world)
			where
				remove :: !String !(Maybe (Set String)) -> Maybe (Set String)
				remove str (Just set)
				  | 'Data.Set'.null set`
				                 = Nothing     // set = {str}, so this entry can be removed entirely from ImgTexts
				  | otherwise    = Just set`
				where
					set`         = 'Data.Set'.delete str set
				remove _ nothing = nothing
	
//  store new texts spans dimensions
	storeTextsSpansToCache :: !TextSpans !*JSWorld -> *JSWorld
	storeTextsSpansToCache texts world
	  #! (jsWebStorage,world) = jsLocalStorage .? world
	  = 'Data.Foldable'.foldl (storeFontTextsSpans jsWebStorage) world ('Data.Map'.toList texts)
	where
		storeFontTextsSpans :: !JSVal !*JSWorld !(!FontDef,!Map String TextSpan) -> *JSWorld
		storeFontTextsSpans jsWebStorage world (font,txt_widths)
		  = 'Data.Foldable'.foldl (storeTextSpan jsWebStorage font) world ('Data.Map'.toList txt_widths)
		where
			storeTextSpan :: !JSVal !FontDef !*JSWorld !(!String,!TextSpan) -> *JSWorld
			storeTextSpan jsWebStorage font world (str,width)
			  #! (_,world) = (jsWebStorage `setItem` (TEXT_WEB_STORAGE_KEY font str,width)) (jsTrace` ("storeTextSpan (" +++ TEXT_WEB_STORAGE_KEY font str +++ "," +++ toString width +++ ")") world)
			  = world

// compute the font-text dimensions of new font-texts that are used in an image
	calcImgTextsLengths :: !ImgTexts !*JSWorld -> (!TextSpans,!*JSWorld)
	calcImgTextsLengths texts world
	  #! (svg, world) = (jsDocument `createElementNS` (svgns, "svg")) world
	  #! (body,world) = jsDocument .# "body" .? world
	  #! (_,   world) = (body `appendChild` svg) world
	  #! (elem,world) = (jsDocument `createElementNS` (svgns, "text")) world
	  #! (_,   world) = (elem `setAttributeNS` ("http://www.w3.org/XML/1998/namespace", "xml:space", "preserve")) world
	  #! (_,   world) = (svg `appendChild` elem) world
	  #! (res, world) = 'Data.Map'.foldrWithKey (calcTextLengths elem) ('Data.Map'.newMap, world) texts
	  #! (_,   world) = (svg `removeChild` elem) world
	  #! (_,   world) = (body `removeChild` svg) world
	  = (res,  world) 
	where
		calcTextLengths :: !JSVal !FontDef !(Set String) !*(!TextSpans, !*JSWorld) -> *(!TextSpans,!*JSWorld)
		calcTextLengths elem fontdef strs (text_spans, world)
		  #! world       = foldl (\world args -> snd ((elem `setAttribute` args) world)) world [("x", "-10000"), ("y", "-10000") : svgFontDefAttrPairs fontdef]
		  #! (ws, world) = 'Data.Foldable'.foldr (calcTextLength elem) ('Data.Map'.newMap, world) strs
		  = ('Data.Map'.alter (merge ws) fontdef text_spans, world)
		
		calcTextLength :: !JSVal !String !*(!Map String TextSpan, !*JSWorld) -> *(!Map String TextSpan,!*JSWorld)
		calcTextLength elem str (text_spans, world)
		  #! world        = (elem .# "textContent" .= str) world
		  #! (ctl, world) = (elem `getComputedTextLength` ()) world
		  = ('Data.Map'.put str (jsValToReal` 0.0 ctl) text_spans, world)

	merge :: !(Map String TextSpan) !(Maybe (Map String TextSpan)) -> Maybe (Map String TextSpan)
	merge ws` (Just ws) = Just ('Data.Map'.union ws` ws)
	merge ws` nothing   = Just ws`

clientRootSVGElt :: !JSVal !*JSWorld -> (!JSObj,!*JSWorld)
clientRootSVGElt me world
  #! (domEl,  world) = me .# "domEl" .? world
  #! (currSVG,world) = domEl .# "firstChild" .? world
  = (currSVG, world)

//	register the defunctionalized event handlers of the image:
clientRegisterEventhandlers` :: !(SVGEditor s v) !JSVal !String !ImgEventhandlers` !ImgTags !*JSWorld -> *JSWorld | JSONEncode{|*|} s
clientRegisterEventhandlers` svglet me taskId es` tags world
  #! world               = timeTrace "clientRegisterEventhandlers` started at " world
  #! (svg,world)         = clientRootSVGElt me world
  #! world               = registerDragEventhandlers` svg me taskId es` tags world
  #! world               = 'Data.Map'.foldrWithKey (registerEventhandler` svglet me taskId svg) world es`
  = timeTrace "clientRegisterEventhandlers` ended at " world
where
//	register mousemove and mouseup listeners only when one of the draggable attributes are present in the image
	registerDragEventhandlers` :: !JSObj !JSVal !String !ImgEventhandlers` !ImgTags !*JSWorld -> *JSWorld
	registerDragEventhandlers` svg me taskId es` tags world
	  | isAnyMember draggableAttrs required
	// all draggable elements share a common mousemove and mouseup event:
	    #! idMap         = invertToMapSet (fmap (mkUniqId taskId) tags)
	    #! world         = addEventListener svg "mousemove" EventCapturing (doMouseDragMove svglet me svg)       me world
	    #! world         = addEventListener svg "mouseup"   EventCapturing (doMouseDragUp   svglet me svg idMap) me world
	    = world
	  | otherwise
	    = world
	where
		draggableAttrs   = [ImgEventhandlerOnMouseDownAttr`
		                   ,ImgEventhandlerOnMouseUpAttr`
		                   ,ImgEventhandlerOnMouseOverAttr`
		                   ,ImgEventhandlerOnMouseMoveAttr`
		                   ,ImgEventhandlerOnMouseOutAttr`
		                   ,ImgEventhandlerDraggableAttr`
		                   ]
		required         = removeDup (map (\(_,{ImgEventhandler` | handler}) = handler) (flatten ('Data.Map'.elems es`)))
	
	registerEventhandler` :: !(SVGEditor s v) !JSVal !String !JSObj !ImgTagNo ![(ImgNodePath,ImgEventhandler`)] !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	registerEventhandler` svglet me taskId svg uniqId es` world
		= 'Data.Foldable'.foldr (register` svglet me svg (mkUniqId taskId uniqId) uniqId) world es`
	where
		register` :: !(SVGEditor s v) !JSVal !JSObj !String !ImgTagNo !(ImgNodePath,ImgEventhandler`) !*JSWorld -> *JSWorld | JSONEncode{|*|} s
		register` svglet me svg elemId uniqId (p,{ImgEventhandler` | handler = ImgEventhandlerOnNClickAttr`,local}) world
			= registerNClick` svglet me svg elemId uniqId p local world
		register` svglet me svg elemId uniqId (p,{ImgEventhandler` | handler = ImgEventhandlerDraggableAttr`}) world
			= registerDraggable` svglet me svg elemId uniqId p world
		register` svglet me svg elemId uniqId (p,{ImgEventhandler` | handler,local}) world
			= registerMouse` svglet me svg elemId eventname capture uniqId p local world
		where
			(eventname,capture)	= case handler of
									ImgEventhandlerOnClickAttr`     = ("click",    EventBubbling)
									ImgEventhandlerOnMouseDownAttr` = ("mousedown",EventCapturing)
									ImgEventhandlerOnMouseUpAttr`   = ("mouseup",  EventCapturing)
									ImgEventhandlerOnMouseOverAttr` = ("mouseover",EventCapturing)
									ImgEventhandlerOnMouseMoveAttr` = ("mousemove",EventCapturing)
									ImgEventhandlerOnMouseOutAttr`  = ("mouseout", EventCapturing)
		
	registerNClick` :: !(SVGEditor s v) !JSVal !JSObj !String !ImgTagNo !ImgNodePath !Bool !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	registerNClick` svglet me svg elemId uniqId p local world
	  #! (elem,world) = (svg .# "getElementById" .$ elemId) world
	  #! world        = addEventListener elem "click" EventBubbling (mkNClickCB` svglet me svg elemId uniqId p local) me world
	  = world
	
	registerMouse` :: !(SVGEditor s v) !JSVal !JSObj !String !String !EventCapture !ImgTagNo !ImgNodePath !Bool !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	registerMouse` svglet me svg elemId evt capture uniqId p local world
	  #! (elem,world) = (svg .# "getElementById" .$ elemId) world
	  #! world        = addEventListener elem evt capture (doMouseEvent` svglet me svg elemId uniqId p MouseNoData local) me world
	  = world
	
	registerDraggable` :: !(SVGEditor s v) !JSVal !JSObj !String !ImgTagNo !ImgNodePath !*JSWorld -> *JSWorld
	registerDraggable` svglet me svg elemId uniqId p world
	  #! (elem,  world) = (svg .# "getElementById" .$ elemId) world
	  #! world          = addEventListener elem "mousedown" EventCapturing (doMouseDragEvent` svglet me svg uniqId p elemId elem) me world
	  = world
	
	mkNClickCB` :: !(SVGEditor s v) !JSVal !JSObj !String !ImgTagNo !ImgNodePath !Bool !{!JSVal} !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	mkNClickCB` svglet me svg elemId uniqId p local args world
	  #! world      = if (size args>0) ((args.[0] .# "stopPropagation" .$! ()) world) world
	// If another click already registered a timeout, clear that timeout
	  #! (to,world) = me .# "clickTimeOut" .? world
	  #! world      = if (jsIsUndefined to || jsIsNull to) world ((jsGlobal "clearTimeout" .$! to) world)
	// Register a callback for the click after a small timeout
	  #! (cb,world) = jsWrapFun (doNClickEvent` svglet me svg elemId uniqId p local) me world
	  #! world      = (me .# "clickHandler" .= cb) world
	  #! (to,world) = (jsGlobal "setTimeout" .$ (cb, CLICK_DELAY)) world
	  #! world      = (me .# "clickTimeOut" .= to) world
	// Increase click counter, so we can determine how many times the element was clicked when the timeout passes
	  #! (nc,world) = me .# "clickCount" .? world
	  #! world      = (me .# "clickCount" .= jsValToInt` 0 nc + 1) world
	  = world
	
	doNClickEvent` :: !(SVGEditor s v) !JSVal !JSObj !String !ImgTagNo !ImgNodePath !Bool !{!JSVal} !*JSWorld-> *JSWorld | JSONEncode{|*|} s
	doNClickEvent` svglet me svg elemId uniqId p local args world
	// Get click count
	  #! (nc,world) = me .# "clickCount" .? world
	// Reset click count
	  #! world      = (me .# "clickCount" .= 0) world
	  #! world      = (me .# "clickHandler" .= jsNull) world
	  #! nc         = jsValToInt` 0 nc
	  = doMouseEvent` svglet me svg elemId uniqId p (MouseOnClickData nc) local args world
	
	doMouseEvent` :: !(SVGEditor s v) !JSVal !JSObj !String !ImgTagNo !ImgNodePath !MouseCallbackData !Bool !{!JSVal} !*JSWorld -> *JSWorld | JSONEncode{|*|} s
	doMouseEvent` svglet=:{SVGEditor | initView,renderImage,updModel} me svg elemId uniqId p cb_data local _ world
	  #! world              = jsTrace` ("doMouseEvent` " +++ "[" +++ join "," (map toString p) +++ "] " +++ toString cb_data) world
	  #! world              = timeTrace "doMouseEvent` started at " world
	  #! (cidJS,world)      = me .# "attributes.taskId" .? world
	  #! taskId             = jsValToString` "" cidJS
	  #! (Just view, world) = jsGetCleanReference (me .# JS_ATTR_VIEW)  world
	  #! (Just model,world) = jsGetCleanReference (me .# JS_ATTR_MODEL) world
	  #! image`             = renderImage model view (imgTagSource taskId)
	  = case getImgEventhandler image` p of
	      Nothing           = timeTrace "doMouseEvent` reached illegal code section ended at " world		// this code should never be reached
	      Just f
	   // Update the view & the model
	      #! view           = applyImgEventhandler f cb_data view
	      #! model          = updModel model view
	      #! (jsView,world) = jsMakeCleanReference view me world
	      #! (jsModel,world)= jsMakeCleanReference model me world
	      #! world          = (me .# JS_ATTR_VIEW  .= jsView)  world
	      #! world          = (me .# JS_ATTR_MODEL .= jsModel) world
	      | local									// the new model value is rendered entirely local on client
	        #! world        = timeTrace "doMouseEvent` calls clientHandleModel started at " world
	        = clientHandleModel svglet me model view world
	      | otherwise           					// the new model value is rendered on the server
	      #! (editId,world) = me .# "attributes.editorId" .? world
	      #! (_,     world) = (me .# "doEditEvent" .$ (cidJS,editId,toJSON (ClientHasNewModel model))) world
	      #! world          = timeTrace "doMouseEvent` calls server round trip started at " world
	      = world							// rendering is completed by clientHandleAttributeChange
	where
		applyImgEventhandler :: !(ImgEventhandler m) !MouseCallbackData m -> m
		applyImgEventhandler (ImgEventhandlerOnClickAttr     {OnClickAttr     | onclick     = f}) _ m = f m
		applyImgEventhandler (ImgEventhandlerOnNClickAttr    {OnNClickAttr    | onNclick    = f}) (MouseOnClickData n) m = f n m
		applyImgEventhandler (ImgEventhandlerOnMouseDownAttr {OnMouseDownAttr | onmousedown = f}) _ m = f m
		applyImgEventhandler (ImgEventhandlerOnMouseUpAttr   {OnMouseUpAttr   | onmouseup   = f}) _ m = f m
		applyImgEventhandler (ImgEventhandlerOnMouseOverAttr {OnMouseOverAttr | onmouseover = f}) _ m = f m
		applyImgEventhandler (ImgEventhandlerOnMouseMoveAttr {OnMouseMoveAttr | onmousemove = f}) _ m = f m
		applyImgEventhandler (ImgEventhandlerOnMouseOutAttr  {OnMouseOutAttr  | onmouseout  = f}) _ m = f m
		applyImgEventhandler _ _ m = m		// this case should never be reached (including ImgEventhandlerDraggableAttr)
		
	//	client side entire rendering of model value:
		clientHandleModel :: !(SVGEditor s v) !JSVal !s !v !*JSWorld -> *JSWorld | JSONEncode{|*|} s
		clientHandleModel svglet=:{SVGEditor | initView,renderImage} me s v world
		  #! world                  = timeTrace "clientHandleModel started at " world
		  #! (taskId,world)         = clientGetTaskId me world
		  #! image`                 = renderImage s v (imgTagSource taskId)
		  #! (img,{ImgTables | imgEventhandlers=es,imgNewFonts=new_fonts,imgNewTexts=new_texts,imgMasks=masks,imgLineMarkers=markers,imgPaths=paths,imgSpans=spans,imgGrids=grids,imgTags=tags})
		                            = toImg image` [] 'Data.Map'.newMap 'Data.Map'.newMap newImgTables
		  #! world                  = timeTrace "clientHandleModel gets metrics started at " world
		  #! (new_font_spans,world) = getNewFontSpans  new_fonts me world                                   // Get missing font spans
		  #! (new_text_spans,world) = getNewTextsSpans new_texts me world                                   // Get missing text width spans
		  #! world                  = timeTrace "clientHandleModel gets metrics ended at " world
		  = case resolve_all_spans tags new_font_spans new_text_spans img masks markers paths spans grids of
		      Error error           = abort error
		      Ok (img,masks,markers,paths,spans,grids)
		        #! svg              = genSVGElt img taskId ('Data.Map'.keys es) masks markers paths spans grids
		        #! svgStr           = toString svg
		        #! world            = clientUpdateSVGString svgStr me world
		        #! world            = clientRegisterEventhandlers` svglet me taskId es tags world
		        #! world            = timeTrace "clientHandleModel ended at " world
		        = world
	
	doMouseDragEvent` :: !(SVGEditor s v) !JSVal !JSObj !ImgTagNo !ImgNodePath !String !JSObj !{!JSVal} !*JSWorld -> *JSWorld
	doMouseDragEvent` svglet=:{SVGEditor | renderImage} me svg uniqId p elemId elem args world
	  #! (Just ds,      world) = jsGetCleanReference (me .# "dragState") world
	  #! (targetElement,world) = (svg .# "getElementById" .$ elemId) world
	  #! (_,            world) = (targetElement .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
	  #! (boundingRect, world) = (targetElement .# "getBoundingClientRect" .$ ()) world
	  #! (left,         world) = boundingRect .# "left" .? world
	  #! (top,          world) = boundingRect .# "top" .? world
	  #! (point,        world) = (svg `createSVGPoint` ()) world
	  #!                world  = (point .# "x" .= left) world
	  #!                world  = (point .# "y" .= top) world
	  #! (m,            world) = (svg `getScreenCTM` ()) world
	  #! (inv,          world) = (m `inverse` ()) world
	  #! (point,        world) = (point `matrixTransform` inv) world
	  #! (px,           world) = point .# "x" .? world
	  #! (py,           world) = point .# "y" .? world
	  #! (e,f)                 = (jsValToReal` 0.0 px, jsValToReal` 0.0 py)

	  #! (taskId,       world) = clientGetTaskId me world
	  #! (Just view,    world) = jsGetCleanReference (me .# JS_ATTR_VIEW)  world
	  #! (Just model,   world) = jsGetCleanReference (me .# JS_ATTR_MODEL) world
	  #! image`                = renderImage model view (imgTagSource taskId)
	  = case getImgEventhandler image` p of
	      Just (ImgEventhandlerDraggableAttr {DraggableAttr | draggable})
	        #! ds              = { SVGDragState 
	                             | ds & svgDropCallback = draggable
	                                  , svgMousePos     = MouseDown
	                                  , svgDragTarget   = Just targetElement
	                                  , svgGrabPointX   = ds.SVGDragState.svgTrueCoordsX - e
	                                  , svgGrabPointY   = ds.SVGDragState.svgTrueCoordsY - f
	                             }
	        #! (jsDs,   world) = jsMakeCleanReference ds me world
	        #!          world  = (me .# "dragState" .= jsDs) world
	        = world
	      _ = world   // this code should never be reached
	
	doMouseDragMove :: !(SVGEditor s v) !JSVal !JSObj !{!JSVal} !*JSWorld -> *JSWorld
	doMouseDragMove svglet me svg args world
	  | size args == 0
	    = world
	  #! (Just ds,world) = jsGetCleanReference (me .# "dragState") world
	  #! evt             = args.[0]
	  #! (newTrueCoordsX, newTrueCoordsY, world)
	                     = getNewTrueCoords me evt world
	  | not (gEq{|*|} ds.SVGDragState.svgMousePos MouseDown) || ds.SVGDragState.svgDragTarget =: Nothing
	 	#! ds            = { SVGDragState 
	 	                   | ds & svgTrueCoordsX = newTrueCoordsX
	 	                        , svgTrueCoordsY = newTrueCoordsY
	 	                   }
	    #! (jsDs, world) = jsMakeCleanReference ds me world
	    #! world         = (me .# "dragState" .= jsDs) world
	    = world
	  #! dragTarget      = fromJust ds.SVGDragState.svgDragTarget
	// Append the dragTarget to the root of the SVG element for two reasons:
	//   1. To allow it to be dragged over all other elements
	//   2. To not be bothered by the offsets of one or more groups it might initially be in
	  #! (_, world)     = (svg `appendChild` dragTarget) world
	  #! newX           = newTrueCoordsX - ds.SVGDragState.svgGrabPointX
	  #! newY           = newTrueCoordsY - ds.SVGDragState.svgGrabPointY
	  #! (_, world)     = (dragTarget `setAttribute` ("transform", "translate(" +++ toString newX +++ "," +++ toString newY +++ ")")) world
	  #! ds             = { SVGDragState
	                      | ds & svgTrueCoordsX = newTrueCoordsX
	                           , svgTrueCoordsY = newTrueCoordsY
	                      }
	  #! (jsDs, world)  = jsMakeCleanReference ds me world
	  #! world          = (me .# "dragState" .= jsDs) world
	  = world
	
	doMouseDragUp :: !(SVGEditor s v) !JSVal !JSObj !(Map String (Set ImageTag)) !{!JSVal} !*JSWorld -> *JSWorld
	doMouseDragUp svglet me svg idMap args world
	  | size args == 0
	    = world
	  #! evt               = args.[0]
	  #! (Just ds,world)   = jsGetCleanReference (me .# "dragState") world
	  | ds.SVGDragState.svgDragTarget =: Nothing
	    #! ds              = { SVGDragState
	                         | ds & svgMousePos   = MouseUp
	                              , svgDragTarget = Nothing
	                         }
	    #! (jsDs, world)   = jsMakeCleanReference ds me world
	    #! world           = (me .# "dragState" .= jsDs) world
	    = world
	  #! (evtTarget,world) = evt .# "target" .? world
	  #! dragTarget        = fromJust ds.SVGDragState.svgDragTarget
	  #! (_, world)        = (dragTarget .# "setAttributeNS" .$ (jsNull, "pointer-events", "none")) world
	  #! (parentId, world) = firstIdentifiableParentId evtTarget world
	// Get model & view value 
	  #! (Just view, world) = jsGetCleanReference (me .# JS_ATTR_VIEW) world
	  #! (Just model,world) = jsGetCleanReference (me .# JS_ATTR_MODEL) world
	  #! xdiff             = ds.SVGDragState.svgTrueCoordsX - ds.SVGDragState.svgGrabPointX
	  #! ydiff             = ds.SVGDragState.svgTrueCoordsY - ds.SVGDragState.svgGrabPointY
	  #! view              = ds.SVGDragState.svgDropCallback ('Data.Map'.findWithDefault 'Data.Set'.newSet parentId idMap) (xdiff,ydiff) view 
	  #! model             = svglet.SVGEditor.updModel model view
	  #! ds                = { SVGDragState
	                         | ds & svgMousePos   = MouseUp
	                              , svgDragTarget = Nothing
	                         }
	  #! (jsView,world)    = jsMakeCleanReference view me world
	  #! (jsModel,world)   = jsMakeCleanReference model me world
	  #! (jsDs,world)      = jsMakeCleanReference ds me world
	  #! world             = (me .# JS_ATTR_VIEW  .= jsView)  world
	  #! world             = (me .# JS_ATTR_MODEL .= jsModel) world
	  #! world             = (me .# "dragState"   .= jsDs)    world
	  = world

firstIdentifiableParentId :: !JSObj !*JSWorld -> *(!String, !*JSWorld)
firstIdentifiableParentId elem world
  #! (idval,world)      = elem .# "id" .? world
  | jsIsNull idval
      #! (parent,world) = elem .# "parentNode" .? world
      = firstIdentifiableParentId parent world
  #! idval = jsValToString` "" idval
  | idval == ""
      #! (parent,world) = elem .# "parentNode" .? world
      = firstIdentifiableParentId parent world
  | otherwise
      = (idval, world)

getNewTrueCoords :: !JSVal !JSObj !*JSWorld -> *(!Real, !Real, !*JSWorld)
getNewTrueCoords me evt world
  #! (svg,         world) = clientRootSVGElt me world
  #! (newScale,    world) = svg .# "currentScale" .? world
  #! newScale             = jsValToReal` 0.0 newScale
  #! (translation, world) = svg .# "currentTranslate" .? world
  #! (translationX,world) = translation .# "x" .? world
  #! (translationY,world) = translation .# "y" .? world
  #! (clientX,     world) = evt .# "clientX" .? world
  #! (clientY,     world) = evt .# "clientY" .? world
  #! newTrueCoordsX       = ((jsValToReal` 0.0 clientX) - (jsValToReal` 0.0 translationX)) / newScale
  #! newTrueCoordsY       = ((jsValToReal` 0.0 clientY) - (jsValToReal` 0.0 translationY)) / newScale
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
to2decString r = toString (to2dec r)

genSVGElts :: !Img !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
genSVGElts {Img | uniqId, transform, host, overlays, offsets} taskId es markers paths spans grids
	= mkGroup (if interactive [IdAttr (mkUniqId taskId uniqId)] [])
	          (genSVGTransform host transform spans taskId)
	          (  genSVGHost     uniqId   host    taskId es markers paths spans grids 
	          ++ genSVGOverlays overlays offsets taskId es markers paths spans grids
	          )
where
	interactive = isMember uniqId es
	
	genSVGTransform :: !HostImg !(Maybe ImgTransform) !ImgSpans !String -> [SVGAttr]
	genSVGTransform (CompositeImg img) (Just tf) spans taskId
	  #! attr = genTransform imgSpan tf taskId
	  = [attr]
	where
		imgSpan     = case 'Data.Map'.get img.Img.uniqId spans of
			            Just (PxSpan w, PxSpan h) = (w,h)
			            Just _                    = abort ("Unexpected error in module SVGEditor (genSVGElts): " +++ unresolvedErrorMsg  "image")
			            nothing                   = abort ("Unexpected error in module SVGEditor (genSVGElts): " +++ unavailableErrorMsg "image")
		
		genTransform :: !ImageSpanReal !ImgTransform !String -> SVGAttr
		genTransform (xsp, ysp) (RotateImg imAn) _
		  #! attr = RotateTransform (to2decString (toDeg imAn)) (Just (to2decString (xsp / 2.0), to2decString (ysp / 2.0)))
		  = TransformAttr [attr]
		genTransform _ (SkewXImg imAn) _
		  #! attr = SkewXTransform (toString (toDeg imAn))
		  = TransformAttr [attr]
		genTransform _ (SkewYImg imAn) _
		  #! attr = SkewYTransform (toString (toDeg imAn))
		  = TransformAttr [attr]
		genTransform (xsp, ysp) (FitImg spx spy) _
		  #! attr = ScaleTransform fx fy
		  = TransformAttr [attr]
		where
			(fx,fy)      = case (spx,spy) of
			                 (PxSpan rx, PxSpan ry) = (to2decString (rx / xsp), to2decString (ry / ysp))
			                 _                      = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg  "fit"))
		genTransform (xsp, ysp) (FitXImg sp) _
		  #! attr = ScaleTransform fxy fxy
		  = TransformAttr [attr]
		where
			fx          = case sp of
			                PxSpan rx = rx / xsp
			                _         = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg "fitx"))
			fxy         = if (xsp > 0.0) (to2decString fx) "1.0"
		genTransform (xsp, ysp) (FitYImg sp) _
		  #! attr = ScaleTransform fxy fxy
		  = TransformAttr [attr]
		where
			fy          = case sp of
			                PxSpan ry = ry / ysp
			                _         = abort (lookupSpanErrorMsg "genTransform" (unresolvedErrorMsg "fity"))
			fxy         = if (ysp > 0.0) (to2decString fy) "1.0"
		genTransform (_, ysp) (ScaleImg fx fy) _
		  #! attr = ScaleTransform (to2decString fx) (to2decString fy)
		  = TransformAttr [attr]
		genTransform (xsp, ysp) FlipXImg _
		  #! attr0 = TranslateTransform (to2decString xsp) "0"
		  #! attr1 = ScaleTransform "-1" "1"
		  = TransformAttr [attr0, attr1]
		genTransform (xsp, ysp) FlipYImg _
		  #! attr0 = TranslateTransform "0" (to2decString ysp)
		  #! attr1 = ScaleTransform "1" "-1"
		  = TransformAttr [attr0, attr1]
		genTransform _ (MaskImg uniqId) taskId
		  = MaskAttr (mkUrl (mkMaskId taskId uniqId))
	genSVGTransform _ _ _ _
		= []
	
	genSVGHost :: !ImgTagNo !HostImg !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGHost no host=:(BasicHostImg basic attrs) taskId es markers paths spans grids
		= genSVGBasicHostImg no basic (genSVGImageAttrs attrs) taskId es markers paths spans grids
	where
		genSVGImageAttrs :: !(Set BasicImgAttr) -> [SVGAttr]
		genSVGImageAttrs atts = strictTRMap genSVGImageAttr ('Data.Set'.toList atts)
		where
			genSVGImageAttr :: !BasicImgAttr -> SVGAttr
			genSVGImageAttr (BasicImgStrokeAttr      color)
			  = StrokeAttr (PaintColor color Nothing)
			genSVGImageAttr (BasicImgStrokeWidthAttr (PxSpan w))
			  #! w` = toString w
			  = StrokeWidthAttr (StrokeWidthLength (w`, PX))
			genSVGImageAttr (BasicImgXRadiusAttr (PxSpan r))
			  #! r` = toString r
			  = RxAttr (r`, PX)
			genSVGImageAttr (BasicImgYRadiusAttr (PxSpan r))
			  #! r` = toString r
			  = RyAttr (r`, PX)
			genSVGImageAttr (BasicImgStrokeOpacityAttr op)
			  = StrokeOpacityAttr (toString op)
			genSVGImageAttr (BasicImgFillOpacityAttr op)
			  = FillOpacityAttr (FillOpacity (toString op))
			genSVGImageAttr (BasicImgFillAttr color)
			  = FillAttr (PaintColor color Nothing)
			genSVGImageAttr (BasicImgDashAttr dash)
			  = StrokeDashArrayAttr (DashArray (strictTRMap toString dash))
			genSVGImageAttr _
			  = abort "Unexpected error in module SVGEditor (local function genSVGImageAttr of genSVGElts): unresolved span value encountered."
	genSVGHost no host=:(RawHostImg content) taskId es markers paths spans grids
		= [RawElt content]
	genSVGHost no host=:(CompositeImg img) taskId es markers paths spans grids
		= genSVGElts img taskId es markers paths spans grids
	
	genSVGBasicHostImg :: !ImgTagNo !BasicImg ![SVGAttr] !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGBasicHostImg no EmptyImg attrs taskId es markers paths spans grids
	  = []
	genSVGBasicHostImg no (TextImg fontdef txt) attrs taskId es markers paths spans grids
	  #! elt = TextElt [XmlspaceAttr "preserve"] 
		           (keepTransformAttrsTogether (TransformAttr [TranslateTransform (toString 0.0) (toString (getfontysize fontdef * 0.75))]) (attrs ++ svgFontDefAttrs fontdef)) txt
	  = [elt]
	genSVGBasicHostImg no RectImg attrs taskId es markers paths spans grids
	  #! elt = RectElt sizeAtts attrs
	  = [elt]
	where
		sizeAtts          = case 'Data.Map'.get no spans of
		                      Just (PxSpan w, PxSpan h) = mkWH (w,h)
		                      Just _                    = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "rect"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "rect"))
	genSVGBasicHostImg no CircleImg attrs taskId es markers paths spans grids
	  #! elt = CircleElt [] [RAttr (radius,PX), CxAttr (radius,PX), CyAttr (radius,PX) : attrs]
	  = [elt]
	where
		radius            = case 'Data.Map'.get no spans of
		                      Just (PxSpan w,h)         = to2decString (w / 2.0)
		                      Just (_,_)                = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "circle"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "circle"))
	genSVGBasicHostImg no EllipseImg attrs taskId es markers paths spans grids
	  #! elt = EllipseElt [] [RxAttr (xradius,PX), CxAttr (xradius,PX), RyAttr (yradius,PX), CyAttr (yradius,PX) : attrs]
	  = [elt]
	where
		(xradius,yradius) = case 'Data.Map'.get no spans of
		                      Just (PxSpan w, PxSpan h) = (to2decString (w / 2.0), to2decString (h / 2.0))
		                      Just _                    = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unresolvedErrorMsg  "ellipse"))
		                      nothing                   = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "ellipse"))
	genSVGBasicHostImg no PolylineImg attrs taskId es markers` paths spans grids
	  #! attr = PointsAttr (strictTRMap (polypointToPointsAttr "polyline") points)
	  #! elt  = PolylineElt [] [attr : attrs ++ markerAttrs]
	  = [ elt : map (\elt -> DefsElt [] [] [elt]) markerElts ]		// PA: this is different from first version in which all marker-elements were collected in a single DefsElt
	where
		markers                   = case 'Data.Map'.get no markers` of
		                              Just m  = m
		                              nothing = defaultLineMarkers
		points                    = case 'Data.Map'.get no paths of
		                              Just ps = ps.ImgPath.pathPoints
		                              nothing = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "polyline"))
		(markerElts, markerAttrs) = unzip (genSVGLineMarkers "polyline" markers taskId es markers` paths spans grids)
	genSVGBasicHostImg no PolygonImg attrs taskId es markers` paths spans grids
	  #! attr = PointsAttr (strictTRMap (polypointToPointsAttr "polygon") points)
	  #! elt  = PolygonElt [] [attr : attrs ++ markerAttrs]
	  = [ elt : map (\elt -> DefsElt [] [] [elt]) markerElts ]		// PA: this is different from first version in which all marker-elements were collected in a single DefsElt
	where
		markers                   = case 'Data.Map'.get no markers` of
		                              Just m  = m
		                              nothing = defaultLineMarkers
		points                    = case 'Data.Map'.get no paths of
		                              Just ps = ps.ImgPath.pathPoints
		                              nothing = abort (lookupSpanErrorMsg "genSVGBasicHostImg" (unavailableErrorMsg "polygon"))
		(markerElts, markerAttrs) = unzip (genSVGLineMarkers "polygon" markers taskId es markers` paths spans grids)
	
	genSVGLineMarkers :: !String !LineMarkers !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [(SVGElt,SVGAttr)]
	genSVGLineMarkers elt {LineMarkers | lineStart,lineMid,lineEnd} taskId es markers paths spans grids
		= [  genSVGLineMarker elt img posAttr taskId es markers paths spans grids 
		  \\ (Just img,posAttr) <- [ (lineStart,MarkerStartAttr)
		                           , (lineMid,  MarkerMidAttr)
		                           , (lineEnd,  MarkerEndAttr)
		                           ]
		  ]
	where
		genSVGLineMarker :: !String !Img !(String -> SVGAttr) !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> (!SVGElt,!SVGAttr)
		genSVGLineMarker elt img=:{Img | uniqId} posAttr taskId es markers paths spans grids
			= ( MarkerElt [ IdAttr mid ]
			              [ OrientAttr       "auto"
                          , ViewBoxAttr      "0" "0" wStr hStr
                          , RefXAttr         (wStr, PX)
                          , RefYAttr         (to2decString (h / 2.0), PX)
                          , MarkerHeightAttr (hStr, PX)
                          , MarkerWidthAttr  (wStr, PX)
                          ]
                          (genSVGElts img taskId es markers paths spans grids)
			  , posAttr (mkUrl mid)
			  )
		where
			mid   = mkMarkerId taskId uniqId
			(w,h) = case 'Data.Map'.get uniqId spans of
			          Just (PxSpan w, PxSpan h) = (w,h)
			          Just _                    = abort (lookupSpanErrorMsg "genSVGLineMarkers" (unresolvedErrorMsg  elt))
			          nothing                   = abort (lookupSpanErrorMsg "genSVGLineMarkers" (unavailableErrorMsg elt))
			wStr = to2decString w
        	hStr = to2decString h

	genSVGOverlays :: ![Img] ![ImageOffset] !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans -> [SVGElt]
	genSVGOverlays overlays offsets taskId es markers paths spans grids
		= flatten (strictTRMap (genGroup taskId es markers paths spans grids) (zip2 overlays offsets))
	where
		genGroup :: !String ![ImgTagNo] !ImgLineMarkers !ImgPaths !ImgSpans !GridSpans !(!Img,!ImageOffset) -> [SVGElt]
		genGroup taskId es markers paths spans grids (img,offset)
		  #! attr = mkTransformTranslateAttr offset
		  #! elts = genSVGElts img taskId es markers paths spans grids
		  = mkGroup [] attr elts
		
		mkTransformTranslateAttr :: !ImageOffset -> [SVGAttr]
		mkTransformTranslateAttr (PxSpan dx,PxSpan dy)
		| dx == 0.0 && dy == 0.0   = []
		#! transform               = TranslateTransform (to2decString dx) (to2decString dy)
		| otherwise                = [TransformAttr [transform]]
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
mkGroup _ _ []
  = []
mkGroup [] [] xs
  = xs
mkGroup hattrs [] [GElt [] sattrs xs]
  #! elt = GElt hattrs sattrs xs
  = [elt]
mkGroup [] sattrs [GElt hattrs [] xs]
  #! elt = GElt hattrs sattrs xs
  = [elt]
mkGroup [] [tfattr=:(TransformAttr [TranslateTransform x y])] xs
  = strictTRMap f xs
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
mkGroup has    sas elts
  #! elt = GElt has sas elts
  = [elt]

dualTransformTranslate :: !a !a !a !a -> SVGAttr | toReal a
dualTransformTranslate x y x` y`
  #! transform = TranslateTransform (to2decString (toReal x + toReal x`)) (to2decString (toReal y + toReal y`))
  = TransformAttr [transform]

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
