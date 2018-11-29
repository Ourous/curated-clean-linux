implementation module iTasks.Extensions.GIS.Leaflet

import iTasks
import iTasks.UI.Definition, iTasks.UI.JS.Map, iTasks.UI.Editor, iTasks.UI.JS.Encoding
import StdMisc, Data.Tuple, Data.Error, Data.Func
import qualified Data.Map as DM
from Text.HTML import instance toString HtmlTag
from iTasks.UI.Editor.Common import diffChildren
from StdArray import class Array(uselect), instance Array {} a

LEAFLET_JS         :== "/leaflet-1.1.0/leaflet.js"
LEAFLET_JS_WINDOW  :== "leaflet-window.js"
LEAFLET_CSS        :== "/leaflet-1.1.0/leaflet.css"
LEAFLET_CSS_WINDOW :== "leaflet-window.css"

:: IconOptions =
    { iconUrl   :: !String
    , iconSize  :: ![Int]
    }
:: MapOptions =
    { attributionControl    :: !Bool
    , zoomControl           :: !Bool
    }
:: CursorOptions =
    { color     :: !String
    , opacity   :: !Real
    , radius    :: !Int
    }

derive JSONEncode IconOptions

derive JSEncode LeafletEdit, LeafletBounds, LeafletLatLng
derive JSDecode LeafletEdit, LeafletBounds, LeafletLatLng

CURSOR_OPTIONS  :== {color = "#00f", opacity = 1.0, radius = 3}
MAP_OPTIONS     :== {attributionControl = False, zoomControl = True}

:: LeafletEdit
    //Perspective
    = LDSetZoom         !Int
    | LDSetCenter       !LeafletLatLng
    | LDSetCursor       !LeafletLatLng
    | LDSetBounds       !LeafletBounds
	//Updating markers 
	| LDSelectMarker    !LeafletObjectID
    | LDRemoveWindow    !LeafletObjectID

openStreetMapTiles :: String
openStreetMapTiles = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
	
leafletEditor :: Editor LeafletMap
leafletEditor = leafEditorToEditor
	{ LeafEditor
    | genUI          = withClientSideInit initUI genUI
    , onEdit         = onEdit
    , onRefresh      = onRefresh
    , valueFromState = valueFromState
    }
where
	genUI dp mode world
		# val=:{LeafletMap|perspective={center,zoom,cursor},tilesUrls,objects,icons} =
			fromMaybe gDefault{|*|} $ editModeValue mode
		# mapAttr = 'DM'.fromList
			[("zoom", JSONInt zoom)
			,("center", JSONArray [JSONReal center.LeafletLatLng.lat, JSONReal center.LeafletLatLng.lng])
			,("cursor", maybe JSONNull toJSON cursor)
			,("tilesUrls", toJSON tilesUrls)
			,("icons", JSONArray [toJSON (iconId,{IconOptions|iconUrl=iconUrl,iconSize=[w,h]}) \\ {iconId,iconUrl,iconSize=(w,h)} <- icons])
			]
		# attr = 'DM'.unions [mapAttr, sizeAttr (ExactSize 500) (ExactSize 150)]
		# children = map encodeUI objects
		= (Ok (uiac UIHtmlView attr children, val), world)

	encodeUI (Marker o) = let (JSONObject attr) = toJSON o
                              dataMap = 'DM'.fromList [("type",JSONString "marker"):attr]
                              // translate HtmlTag of popup to HTML code
                              dataMap` = case o.popup of
                                  Nothing = dataMap
                                  Just html = 'DM'.put "popup" (JSONString (toString html)) dataMap
                          in uia UIData dataMap`
	encodeUI (Polyline o) = let (JSONObject attr) = toJSON o in uia UIData ('DM'.fromList [("type",JSONString "polyline"):attr])
	encodeUI (Polygon o) = let (JSONObject attr) = toJSON o in uia UIData ('DM'.fromList [("type",JSONString "polygon") : attr])
    encodeUI (Window o) = let (JSONObject attr) = toJSON o
                              dataMap = 'DM'.fromList [("type",JSONString "window"):attr]
                              // translate HtmlTag to HTML code
                              dataMap` = 'DM'.put "content" (JSONString (toString o.content)) dataMap
                          in uia UIData dataMap`

	initUI me world
		# (jsInitDOM,world) = jsWrapFun (initDOM me) world
		//Check if the leaflet library is loaded and either load it, 
		//and delay dom initialization or set the initDOM method to continue
		//as soon as the component's DOM element is available
        # (l, world) = findObject "L" world
        | jsIsUndefined l
            # world = addCSSFromUrl LEAFLET_CSS world
            # world = addCSSFromUrl LEAFLET_CSS_WINDOW world
            # world = addJSFromUrl LEAFLET_JS Nothing world
            # world = addJSFromUrl LEAFLET_JS_WINDOW (Just jsInitDOM) world
			= world
		| otherwise
			# world = (me .# "initDOMEl" .= jsInitDOM) world
			= world

	initDOM me args world
        # (l,world)         = findObject "L" world
		# (domEl,world) 	= .? (me .# "domEl") world
		//Create the map
		# (mapObj,world)    = (l .# "map" .$ (domEl,MAP_OPTIONS)) world
		# world 			= (me .# "map" .= mapObj) world
		//Set perspective
		# (center,world)    = .? (me .# "attributes.center") world
		# (zoom,world)      = .? (me .# "attributes.zoom") world
		# (cursor,world)    = .? (me .# "attributes.cursor") world
        # (_,world)         = (mapObj .# "setView" .$ (center,zoom)) world
		//Set initial cursor
        # world             = setMapCursor me mapObj cursor world
        //Add icons
		# (icons,world)     = .? (me .# "attributes.icons") world
		# world             = setMapIcons me mapObj icons world 
		//Create tile layer
		# (tilesUrls,world) = .? (me .# "attributes.tilesUrls") world
		# world             = forall (addMapTilesLayer me mapObj) tilesUrls world
		//Synchronize lat/lng bounds to server (they depend on the size of the map in the browser)
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (bounds,world)    = getMapBounds mapObj world
		# (edit,world)      = encodeOnClient [LDSetBounds bounds] world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,edit)) world
        //Add initial objects
		# (objects,world)   = .? (me .# "children") world
		# world             = createMapObjects me mapObj objects world
		//Add event handlers
		# (cb,world)       = jsWrapFun (\a w -> onResize me w) world	
		# world            = ((me .# "onResize") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onShow me w) world	
		# world            = ((me .# "onShow") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onAttributeChange me a w) world	
		# world            = ((me .# "onAttributeChange") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onAfterChildInsert me a w) world	
		# world            = ((me .# "afterChildInsert") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onBeforeChildRemove me a w) world	
		# world            = ((me .# "beforeChildRemove") .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onMapDragEnd me a w) world
        # (_,world)        = (mapObj .# "addEventListener" .$ ("dragend",cb)) world
		# (cb,world)       = jsWrapFun (\a w -> onMapZoomEnd me a w) world
        # (_,world)        = (mapObj .# "addEventListener" .$ ("zoomend",cb)) world
		# (cb,world)       = jsWrapFun (\a w -> onMapClick me a w) world
        # (_,world)        = (mapObj .# "addEventListener" .$ ("click",cb)) world
		= (jsNull,world)

	onResize me world
		# (mapObj,world) 	= .? (me .# "map") world
        # (_,world)         = (mapObj .# "invalidateSize" .$ ()) world
		= (jsNull,world)

	onShow me world
		# (mapObj,world) 	= .? (me .# "map") world
        # (_,world)         = (mapObj .# "invalidateSize" .$ ()) world
		= (jsNull,world)

	onMapDragEnd me args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (mapObj,world)    = .? (toJSVal (args !! 0) .# "target") world
        # (center,world)    = getMapCenter mapObj world
        # (bounds,world)    = getMapBounds mapObj world
		# (edit,world)      = encodeOnClient [LDSetCenter center,LDSetBounds bounds] world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,edit)) world
		= (jsNull,world)

	onMapZoomEnd me args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (mapObj,world)    = .? (toJSVal (args !! 0) .# "target") world
        # (zoom,world)      = getMapZoom mapObj world
        # (bounds,world)    = getMapBounds mapObj world
		# (edit,world)      = encodeOnClient [LDSetZoom zoom,LDSetBounds bounds] world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,edit)) world
		= (jsNull,world)

	onMapClick me args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
        # (mapObj,world)    = .? (toJSVal (args !! 0) .# "target") world
        # (clickPos,world)  = .? (toJSVal (args !! 0) .# "latlng") world
		# (cursor,world)    = toLatLng clickPos world 
		# (edit,world)      = encodeOnClient [LDSetCursor cursor] world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,edit)) world
		//Update cursor position on the map
		# world             = setMapCursor me mapObj (toJSVal cursor) world
		= (jsNull,world)

	onMarkerClick me markerId args world
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (edit,world)      = encodeOnClient [LDSelectMarker markerId] world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,edit)) world
		= (jsNull,world)

	onAttributeChange me args world
		# (mapObj,world)    = .? (me .# "map") world
		= case jsArgToString (args !! 0) of
			"center"  = (jsNull,setMapCenter mapObj (args !! 1) world)
			"zoom"    = (jsNull,setMapZoom mapObj (args !! 1) world)
			"cursor"  = (jsNull,setMapCursor me mapObj (toJSVal (args !! 1)) world)
			_ 		  = (jsNull,world)
		
	onAfterChildInsert me args world
		# (l, world)      	= findObject "L" world
		# (mapObj,world)    = .? (me .# "map") world
		= (jsNull,createMapObject me mapObj l (toJSVal (args !! 1)) world)

	onBeforeChildRemove me args world
		# (layer,world)     = .? (toJSVal (args !! 1) .# "layer") world
        // for windows, based on control class
        # (removeMethod, world) = .? (layer .# "remove") world
        | not (jsIsUndefined removeMethod) = (layer .# "remove" .$ ()) world
        // for all other objects
		# (mapObj,world)    = .? (me .# "map") world
        # (_,world)         = (mapObj.# "removeLayer" .$ layer) world
        # (popup, world)    = .? (layer .# "myPopup") world
        | jsIsUndefined popup = (jsNull, world)
        # (_,world)         = (mapObj.# "removeLayer" .$ popup) world
        = (jsNull, world)

    onWindowRemove me windowId _ world
        // remove children from iTasks component
        # (children,world)  = .? (me .# "children") world
        # world             = forall removeWindow children world
        // send edit event to server
        # (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (edit,world)      = encodeOnClient [LDRemoveWindow windowId] world
		# (_,world)         = ((me .# "doEditEvent") .$ (taskId,editorId,edit)) world
		= (jsNull, world)
    where
        removeWindow idx layer world
            # (layerWindowId, world)  = .? (layer .# "attributes.windowId") world
            | not (jsIsUndefined layerWindowId) && jsValToString layerWindowId == windowId =
                snd (((me .# "removeChild") .$ idx) world)
            = world

	//Map object access
	toLatLng obj world
		# (lat,world)     = .? (obj .# "lat") world
		# (lng,world)     = .? (obj .# "lng") world
		= ({LeafletLatLng|lat=jsValToReal lat,lng=jsValToReal lng}, world)

    getMapBounds mapObj env
        # (bounds,env)      = (mapObj .# "getBounds" .$ ()) env
        # (sw,env)          = (bounds .# "getSouthWest" .$ ()) env
        # (ne,env)          = (bounds .# "getNorthEast" .$ ()) env
		# (swpos,env)       = toLatLng sw env
		# (nepos,env)       = toLatLng ne env
        = ({southWest=swpos,northEast=nepos},env)

	getMapZoom mapObj world
		# (zoom,world) = (mapObj .# "getZoom" .$ ()) world
		= (jsValToInt zoom, world)

	setMapZoom mapObj zoom world
		# (_,world) = (mapObj .# "setZoom" .$ zoom) world
		= world

	getMapCenter mapObj world
        # (center,world)    = (mapObj .# "getCenter" .$ ()) world
        = toLatLng center world
	
	setMapCenter mapObj center world
		# (_,world) = (mapObj .# "panTo" .$ center) world
		= world

	setMapCursor me mapObj position world
		# (cursor,world) = .? (me .# "cursor") world
		| jsIsUndefined cursor
			| jsIsNull position //Nothing to do
				= world
			| otherwise
				//Create the cursor
				# (l, world)      = findObject "L" world
				# (cursor,world)  = (l .# "circleMarker" .$ (position, CURSOR_OPTIONS)) world
				# (_,world)       = (cursor .# "addTo" .$ mapObj) world
				# world           = ((me .# "cursor") .= cursor) world	
				= world
		| otherwise //Update the position
			| jsIsNull position
				//Destroy the cursor
				# (_,world)       = (mapObj .# "removeLayer" .$ cursor) world
				# world           = jsDeleteObjectAttr "cursor" me world
				= world
			| otherwise
        		# (_,world)       = (cursor .# "setLatLng" .$ position) world
				= world

	addMapTilesLayer me mapObj _ tilesUrl world
		| jsIsNull tilesUrl = world
		# (l, world)      	= findObject "L" world
        # (layer,world)     = (l .# "tileLayer" .$ tilesUrl) world
        # (_,world)         = (layer .# "addTo" .$ mapObj) world
		= world

	setMapIcons me mapObj icons world 
		# (l, world)      	= findObject "L" world
		# (index,world) 	= jsEmptyObject world
		# world 			= ((me .# "icons") .= index) world
		= forall (createMapIcon me mapObj l index) icons world
	where	
		createMapIcon me mapObj l index _ def world
			# (iconId,world)   = .? (def .# 0) world
			# (iconSpec,world) = .? (def .# 1) world
        	# (icon,world)     = (l .# "icon" .$ iconSpec) world
			# world            = ((index .# jsValToString iconId) .= icon) world
			= world

	createMapObjects me mapObj objects world
		# (l, world) = findObject "L" world
		= forall (\_ obj -> createMapObject me mapObj l obj) objects world

	createMapObject me mapObj l object world
		# (type,world) = .? (object .# "attributes.type") world
		= case jsValToString type of
			"marker"   = createMarker   me mapObj l object world
			"polyline" = createPolyline me mapObj l object world
			"polygon"  = createPolygon  me mapObj l object world
            "window"   = createWindow   me mapObj l object world
			_ 		   = world

	createMarker me mapObj  l object world
        # (markerId,world)    = .? (object .# "attributes.markerId") world
        # (options,world)     = jsEmptyObject world
		//Set title
		# (title,world)       = .? (object .# "attributes.title") world
		# world               = (options .# "title" .= title) world
		# world               = (options .# "alt" .= title) world
		//Optionally set icon
		# (iconId,world)      = .? (object .# "attributes.icon") world
		# (icons,world)       = .? (me .# "icons") world
		# world               = addIconOption iconId icons options world
		//Create marker
		# (position,world)    = .? (object .# "attributes.position") world
        # (layer,world)       = (l .# "marker" .$ (position,options) ) world
		# world               = (object .# "layer" .= layer) world
        //Optionally add popup
        # (popup,world)       = .? (object .# "attributes.popup") world
        # world               = addPopup popup position layer world
        //Store marker ID, needed for related markers of windows
        # world               = (layer .# "markerId" .= markerId) world
		//Set click handler
		# (cb,world)          = jsWrapFun (\a w -> onMarkerClick me (jsValToString markerId) a w) world
        # (_,world)           = (layer .# "addEventListener" .$ ("click",cb)) world
        //Add to map
        # (_,world)           = (layer .# "addTo" .$ (toJSArg mapObj)) world
		= world	
	where
		addIconOption iconId icons options world
			| jsIsUndefined iconId = world
			# (icon,world) = .? (icons .# (jsValToString iconId)) world
			| jsIsUndefined icon = world
			# world = (options .# "icon" .= icon) world
			= world

        addPopup content position marker world
            | jsIsUndefined content = world
            # (options,world) = jsEmptyObject world
            # world           = (options .# "maxWidth" .= 1000000) world // large nr to let content determine size
            # world           = (options .# "closeOnClick" .= False) world
            # (popup, world)  = (l .# "popup" .$ options) world
            # (_, world)      = (popup .# "setLatLng" .$ position) world
            # (_, world)      = (popup .# "setContent" .$ content) world
            # (_, world)      = (mapObj .# "addLayer" .$ popup) world
            // keep reference in marker object to remove popup if marker is removed
            # world           = (marker .# "myPopup" .= popup) world
            = world
	
	createPolyline me mapObj l object world 
		//Set options
        # (options,world)     = jsEmptyObject world
        # (style,world)       = .? (object .# "attributes.style") world
        # world               = forall (applyLineStyle options) style world
		# (points,world)      = .? (object .# "attributes.points") world
        # (layer,world)       = (l .# "polyline" .$ (points ,options)) world
        # (_,world)           = (layer .# "addTo" .$ (toJSArg mapObj)) world
		# world               = (object .# "layer" .= layer) world
		= world

	createPolygon me mapObj l object world 
		//Set options
        # (options,world)     = jsEmptyObject world
		# (style,world)       = .? (object .# "attributes.style") world
        # world               = forall (applyStyle options) style world
		# (points,world)      = .? (object .# "attributes.points") world
        # (layer,world)       = (l .# "polygon" .$ (points ,options)) world
        # (_,world)           = (layer .# "addTo" .$ (toJSArg mapObj)) world
		# world               = (object .# "layer" .= layer) world
		= world
    where
        applyStyle options _ style world
            # (styleType, world) = .? (style .# 0) world
            # styleType = jsValToString styleType
            | styleType == "Style"
                # (directStyle, world) = .? (style .# 1) world
                # (directStyleType, world) = .? (directStyle .# 0) world
                # (directStyleVal, world)  = .? (directStyle .# 1) world
                # directStyleType = jsValToString directStyleType
                | directStyleType == "PolygonLineStrokeColor" = (options .# "color"       .= directStyleVal) world
                | directStyleType == "PolygonLineStrokeWidth" = (options .# "weight"      .= directStyleVal) world
                | directStyleType == "PolygonLineOpacity"     = (options .# "opacity"     .= directStyleVal) world
                | directStyleType == "PolygonLineDashArray"   = (options .# "dashArray"   .= directStyleVal) world
                | directStyleType == "PolygonNoFill"          = (options .# "fill"        .= False)          world
                | directStyleType == "PolygonFillColor"       = (options .# "fillColor"   .= directStyleVal) world
                | directStyleType == "PolygonFillOpacity"     = (options .# "fillOpacity" .= directStyleVal) world
                = abort "unknown style"
            | styleType == "Class"
                # (cls, world) = .? (style .# 1) world
                = (options .# "className" .= cls) world
            = abort "unknown style"

    createWindow me mapObj l object world
        # (layer,world)      = (l .# "window" .$ () ) world
		# world              = (object .# "layer" .= layer) world
        # (position,world)   = .? (object .# "attributes.initPosition") world
        # (_, world)         = (layer .# "setInitPos" .$ position) world
        # (title,world)      = .? (object .# "attributes.title") world
        # (_, world)         = (layer .# "setTitle" .$ title) world
        # (content,world)    = .? (object .# "attributes.content") world
        # (_, world)         = (layer .# "setContent" .$ content) world
        # (relMarkers,world) = .? (object .# "attributes.relatedMarkers") world
        # world              = forall (addRelatedMarker layer) relMarkers world
        // inject function to send event on window remove
        # (windowId,world)   = .? (object .# "attributes.windowId") world
        # (onWRemove, world) = jsWrapFun (onWindowRemove me (jsValToString windowId)) world
        # world              = (layer .# "_onWindowClose" .= onWRemove) world
        // add to map
        # (_,world)          = (layer .# "addTo" .$ (toJSArg mapObj)) world
        = world
    where
        addRelatedMarker layer _ relMarker world
            # (markerId, world)   = .? (relMarker .# 0) world
            # (lineStyle, world)  = .? (relMarker .# 1) world
            # (lineOptions,world) = jsEmptyObject world
            # world               = forall (applyLineStyle lineOptions) lineStyle world
            # (_, world) = (layer .# "addRelatedMarker" .$ (markerId, lineOptions)) world
            = world

    applyLineStyle options _ style world
        # (styleType, world) = .? (style .# 0) world
        # styleType = jsValToString styleType
        | styleType == "Style"
            # (directStyle, world) = .? (style .# 1) world
            # (directStyleType, world) = .? (directStyle .# 0) world
            # (directStyleVal, world)  = .? (directStyle .# 1) world
            # directStyleType = jsValToString directStyleType
            | directStyleType == "LineStrokeColor" = (options .# "color"     .= directStyleVal) world
            | directStyleType == "LineStrokeWidth" = (options .# "weight"    .= directStyleVal) world
            | directStyleType == "LineOpacity"     = (options .# "opacity"   .= directStyleVal) world
            | directStyleType == "LineDashArray"   = (options .# "dashArray" .= directStyleVal) world
            = abort "unknown style"
        | styleType == "Class"
            # (cls, world) = .? (style .# 1) world
            = (options .# "className" .= cls) world
        = abort "unknown style"

	//Loop through a javascript array
    forall :: (Int (JSVal v11) *JSWorld -> *JSWorld) !(JSVal a) !*JSWorld -> *JSWorld
	forall f array world
		# (len,world) = .? (array .# "length") world
		= forall` 0 (jsValToInt len) world
	where
        forall` :: !Int !Int !*JSWorld -> *JSWorld
		forall` i len world
			| i >= len = world
			# (el,world) = .? (array .# i) world
			= forall` (i + 1) len (f i el world)

	//Process the edits received from the client
	onEdit dp ([], diffs) m vst = (Ok (NoChange, foldl app m diffs), vst)
	where
		app m (LDSetZoom zoom)          = {LeafletMap|m & perspective = {m.perspective & zoom = zoom}}
		app m (LDSetCenter center)      = {LeafletMap|m & perspective = {m.perspective & center = center}}
		app m (LDSetCursor cursor)      = {LeafletMap|m & perspective = {m.perspective & cursor = Just cursor}}
		app m (LDSetBounds bounds)      = {LeafletMap|m & perspective = {m.perspective & bounds = Just bounds}}
		app m (LDSelectMarker markerId) = {LeafletMap|m & objects = map (sel markerId) m.LeafletMap.objects}
		where
			sel x (Marker m=:{LeafletMarker|markerId}) = Marker {LeafletMarker|m & selected = markerId == x}
			sel x o = o
        app m (LDRemoveWindow idToRemove) = {LeafletMap|m & objects = filter notToRemove m.LeafletMap.objects}
        where
            notToRemove (Window {windowId}) = windowId <> idToRemove
            notToRemove _                   = True
		app m _ = m
	onEdit _ _ msk ust = (Ok (NoChange,msk),ust)

	//Check for changed objects and update the client
	onRefresh _ newMap oldMap vst
		//Determine attribute changes
		# attrChanges = diffAttributes oldMap newMap
		//Determine object changes
		# childChanges = diffChildren oldMap.LeafletMap.objects newMap.LeafletMap.objects encodeUI
		= (Ok (ChangeUI attrChanges childChanges, newMap),vst)
	where
		//Only center, zoom and cursor are synced to the client, bounds are only synced from client to server
		diffAttributes {LeafletMap|perspective=p1} {LeafletMap|perspective=p2}
			//Center
			# center = if (p2.LeafletPerspective.center === p1.LeafletPerspective.center) [] [SetAttribute "center" (toJSON p2.LeafletPerspective.center)]
			//Zoom
			# zoom = if (p2.LeafletPerspective.zoom === p1.LeafletPerspective.zoom) [] [SetAttribute "zoom" (toJSON p2.LeafletPerspective.zoom)]
			//Cursor
			# cursor = if (p2.LeafletPerspective.cursor === p1.LeafletPerspective.cursor) [] [SetAttribute "cursor" (maybe JSONNull toJSON p2.LeafletPerspective.cursor)]
			= center ++ zoom ++ cursor

	valueFromState m = Just m

gEditor{|LeafletMap|} = leafletEditor

gDefault{|LeafletMap|}
	= {LeafletMap|perspective=defaultValue, tilesUrls = [openStreetMapTiles], objects = [Marker homeMarker], icons = []}
where
	homeMarker = {markerId = "home", position= {LeafletLatLng|lat = 51.82, lng = 5.86}, title = Just "HOME", icon = Nothing, popup = Nothing, selected = False}

gDefault{|LeafletPerspective|}
    = {LeafletPerspective|center = {LeafletLatLng|lat = 51.82, lng = 5.86}, zoom = 7, cursor = Nothing, bounds = Nothing}

//Comparing reals may have unexpected results, especially when comparing constants to previously stored ones
gEq{|LeafletLatLng|} x y = (toString x.lat == toString y.lat) && (toString x.lng == toString y.lng)

derive JSONEncode LeafletMap, LeafletPerspective, LeafletLatLng
derive JSONDecode LeafletMap, LeafletPerspective, LeafletLatLng
derive gDefault   LeafletLatLng
derive gEq        LeafletMap, LeafletPerspective
derive gText      LeafletMap, LeafletPerspective, LeafletLatLng
derive gEditor    LeafletPerspective, LeafletLatLng
derive class iTask LeafletIcon, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit, LeafletWindow, LeafletWindowPos, LeafletLineStyle, LeafletStyleDef, LeafletPolygonStyle

