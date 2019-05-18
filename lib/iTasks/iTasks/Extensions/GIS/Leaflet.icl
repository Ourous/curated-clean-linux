implementation module iTasks.Extensions.GIS.Leaflet

import iTasks
import iTasks.UI.Definition, iTasks.UI.Editor, iTasks.UI.JavaScript
import StdMisc, Data.Tuple, Data.Error, Data.Func, Text, Data.Functor
import qualified Data.Map as DM
from Text.HTML import instance toString HtmlTag, instance toString SVGElt
from Text.Encodings.Base64 import base64Encode
from iTasks.UI.Editor.Common import diffChildren, :: ChildUpdate (..)
import StdArray

LEAFLET_JS           :== "/leaflet-1.3.4/leaflet.js"
LEAFLET_JS_WINDOW    :== "leaflet-window.js"
// https://github.com/Leaflet/Leaflet.Editable
LEAFLET_JS_EDITABLE  :== "Leaflet.Editable.js"
LEAFLET_CSS          :== "/leaflet-1.3.4/leaflet.css"
LEAFLET_CSS_WINDOW   :== "leaflet-window.css"

:: IconOptions =
    { iconUrl   :: !String
    , iconSize  :: ![Int]
    }
:: MapOptions =
    { attributionControl    :: !Bool
    , zoomControl           :: !Bool
    , editable              :: !Bool
    }
:: CursorOptions =
    { color     :: !String
    , opacity   :: !Real
    , radius    :: !Int
    }

derive JSONEncode IconOptions

derive gToJS CursorOptions, MapOptions, LeafletLatLng

CURSOR_OPTIONS  :== {color = "#00f", opacity = 1.0, radius = 3}
MAP_OPTIONS     :== {attributionControl = False, zoomControl = True, editable = True}

leafletObjectIdOf :: !LeafletObject -> LeafletObjectID
leafletObjectIdOf (Marker m)    = m.markerId
leafletObjectIdOf (Polyline p)  = p.polylineId
leafletObjectIdOf (Polygon p)   = p.polygonId
leafletObjectIdOf (Circle c)    = c.circleId
leafletObjectIdOf (Rectangle r) = r.rectangleId
leafletObjectIdOf (Window w)    = w.windowId

:: LeafletEdit
    //Perspective
    = LDSetZoom         !Int
    | LDSetCenter       !LeafletLatLng
    | LDSetCursor       !LeafletLatLng
    | LDSetBounds       !LeafletBounds
    //Updating markers
    | LDSelectMarker    !LeafletObjectID
    //Updating windows
    | LDRemoveWindow    !LeafletObjectID
    | LDUpdateObject    !LeafletObjectID !LeafletObjectUpdate

:: LeafletObjectUpdate
	= UpdatePolyline  ![LeafletLatLng]
	| UpdatePolygon   ![LeafletLatLng]
	| UpdateCircle    !LeafletLatLng !Real
	| UpdateRectangle !LeafletBounds

svgIconURL :: !SVGElt !(!Int,!Int) -> String
svgIconURL svgelt (width,height) = "data:image/svg+xml;base64," +++ base64Encode svg
where
    svg = concat ["<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
		, toString width, "\" height=\"", toString height, "\">", toString svgelt, "</svg>"]

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
	genUI attr dp mode world
		# val=:{LeafletMap|perspective={center,zoom,cursor},tilesUrls,objects,icons} =
			fromMaybe gDefault{|*|} $ editModeValue mode
		# mapAttr = 'DM'.fromList
			[("zoom", JSONInt zoom)
			,("center", JSONArray [JSONReal center.LeafletLatLng.lat, JSONReal center.LeafletLatLng.lng])
			,("cursor", maybe JSONNull toJSON cursor)
			,("tilesUrls", toJSON tilesUrls)
			,("icons", JSONArray [toJSON (iconId,{IconOptions|iconUrl=iconUrl,iconSize=[w,h]}) \\ {iconId,iconUrl,iconSize=(w,h)} <- icons])
			]
		# attr = 'DM'.unions [ mapAttr
		                     , sizeAttr (ExactSize 500) (ExactSize 150)
		                     , 'DM'.singleton "viewMode" $ JSONBool $ mode =: View _
		                     , attr
		                     ]
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
	encodeUI (Circle o) = let (JSONObject attr) = toJSON o in uia UIData ('DM'.fromList [("type",JSONString "circle"): attr])
	encodeUI (Rectangle o) = let (JSONObject attr) = toJSON o in uia UIData ('DM'.fromList [("type",JSONString "rectangle") : attr])
    encodeUI (Window o) = let (JSONObject attr) = toJSON o
                              dataMap = 'DM'.fromList [("type",JSONString "window"): attr]
                              // translate HtmlTag to HTML code
                              dataMap` = 'DM'.put "content" (JSONString (toString o.content)) dataMap
                          in uia UIData dataMap`

	initUI me world
		# (jsInitDOM,world) = jsWrapFun (initDOM me) me world
		//Check if the leaflet library is loaded and either load it,
		//and delay dom initialization or set the initDOM method to continue
		//as soon as the component's DOM element is available
        # (l, world) = jsTypeOf (jsGlobal "L") .? world
        | jsValToString` "undefined" l == "undefined"
            # world = addCSSFromUrl LEAFLET_CSS world
            # world = addCSSFromUrl LEAFLET_CSS_WINDOW world
            # world = addJSFromUrl LEAFLET_JS Nothing world
            # world = addJSFromUrl LEAFLET_JS_EDITABLE Nothing world
            # world = addJSFromUrl LEAFLET_JS_WINDOW (Just jsInitDOM) world
			= world
		| otherwise
			# world = (me .# "initDOMEl" .= jsInitDOM) world
			= world

	initDOM me args world
        # (l,world)         = jsGlobal "L" .? world
		# (domEl,world) 	= me .# "domEl" .? world
		# (viewMode, world) = me .# "attributes.viewMode" .? world
		# viewMode          = jsValToBool` False viewMode
		//Create the map
		# (mapObj,world)    = (l .# "map" .$ (domEl,MAP_OPTIONS)) world
		# world 			= (me .# "map" .= mapObj) world
		//Set perspective
		# (center,world)    = me .# "attributes.center" .? world
		# (zoom,world)      = me .# "attributes.zoom" .? world
		# (cursor,world)    = me .# "attributes.cursor" .? world
        # world             = (mapObj .# "setView" .$! (center,zoom)) world
		//Set initial cursor
        # world             = setMapCursor me mapObj cursor world
        //Add icons
		# world             = setMapIcons me mapObj (me .# "attributes.icons") world
		//Create tile layer
		# (tilesUrls,world) = me .# "attributes.tilesUrls" .? world
		# world             = forall (addMapTilesLayer me mapObj) tilesUrls world
		//Synchronize lat/lng bounds to server (they depend on the size of the map in the browser)
		# world = case viewMode of
			True
				= world
			False
				# (taskId,world)    = me .# "attributes.taskId" .? world
				# (editorId,world)  = me .# "attributes.editorId" .? world
				# (bounds,world)    = getMapBounds mapObj world
				# edit              = toJSON [LDSetBounds bounds]
				# world             = (me .# "doEditEvent" .$! (taskId,editorId,edit)) world
				= world
        //Add initial objects
		# (objects,world)   = me .# "children" .? world
		# world             = createMapObjects viewMode me mapObj objects world
		//Add event handlers
		# (cb,world)       = jsWrapFun (\a w -> onResize me w) me world
		# world            = (me .# "onResize" .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onShow me w) me world
		# world            = (me .# "onShow" .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onAttributeChange me a w) me world
		# world            = (me .# "onAttributeChange" .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onAfterChildInsert viewMode me a w) me world
		# world            = (me .# "afterChildInsert" .= cb) world
		# (cb,world)       = jsWrapFun (\a w -> onBeforeChildRemove me a w) me world
		# world            = (me .# "beforeChildRemove" .= cb) world
		# world = case viewMode of
            True
				= world
			False
				# (cb,world)       = jsWrapFun (\a w -> onMapDragEnd me a w) me world
				# world            = (mapObj .# "addEventListener" .$! ("dragend",cb)) world
				# (cb,world)       = jsWrapFun (\a w -> onMapZoomEnd me a w) me world
				# world            = (mapObj .# "addEventListener" .$! ("zoomend",cb)) world
				# (cb,world)       = jsWrapFun (\a w -> onMapClick me a w) me world
				# world            = (mapObj .# "addEventListener" .$! ("click",cb)) world
				= world
		= world

	onResize me world
		# (mapObj,world) 	= me .# "map" .? world
        # world             = (mapObj .# "invalidateSize" .$! ()) world
		= world

	onShow me world
		# (mapObj,world) 	= me .# "map" .? world
        # world             = (mapObj .# "invalidateSize" .$! ()) world
		= world

	onMapDragEnd me args world
		# (taskId,world)    = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
        # (mapObj,world)    = args.[0] .# "target" .? world
        # (center,world)    = getMapCenter mapObj world
        # (bounds,world)    = getMapBounds mapObj world
		# edit              = toJSON [LDSetCenter center,LDSetBounds bounds]
		# world             = (me .# "doEditEvent" .$! (taskId,editorId,edit)) world
		= world

	onMapZoomEnd me args world
		# (taskId,world)    = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
        # (mapObj,world)    = args.[0] .# "target" .? world
        # (zoom,world)      = getMapZoom mapObj world
        # (bounds,world)    = getMapBounds mapObj world
		# edit              = toJSON [LDSetZoom zoom,LDSetBounds bounds]
		# world             = (me .# "doEditEvent" .$! (taskId,editorId,edit)) world
		= world

	onMapClick me args world
		# (taskId,world)    = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
        # (mapObj,world)    = args.[0] .# "target" .? world
        # (clickPos,world)  = args.[0] .# "latlng" .? world
		# (cursor,world)    = toLatLng clickPos world
		# edit              = toJSON [LDSetCursor cursor]
		# world             = (me .# "doEditEvent" .$! (taskId,editorId,edit)) world
		//Update cursor position on the map
		# world             = setMapCursor me mapObj (toJS cursor) world
		= world

	onMarkerClick me markerId args world
		# (taskId,world)    = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
		# edit              = toJSON [LDSelectMarker markerId]
		# world             = (me .# "doEditEvent" .$! (taskId,editorId,edit)) world
		= world

	onAttributeChange me args world
		# (mapObj,world)    = me .# "map" .? world
		= case jsValToString args.[0] of
			Just "center"  = setMapCenter mapObj args.[1] world
			Just "zoom"    = setMapZoom mapObj args.[1] world
			Just "cursor"  = setMapCursor me mapObj args.[1] world
			_              = world

	onAfterChildInsert viewMode me args world
		# (l, world)      	= jsGlobal "L" .? world
		# (mapObj,world)    = me .# "map" .? world
		= createMapObject viewMode me mapObj l args.[1] world

	onBeforeChildRemove me args world
		# (layer,world)     = args.[1] .# "layer" .? world
        // for windows, based on control class
        # (removeMethod, world) = layer .# "remove" .? world
        | not (jsIsUndefined removeMethod) = (layer .# "remove" .$! ()) world
        // for all other objects
		# (mapObj,world)    = me .# "map" .? world
        # world             = (mapObj.# "removeLayer" .$! layer) world
        # (popup, world)    = layer .# "myPopup" .? world
        | jsIsUndefined popup = world
        # world             = (mapObj.# "removeLayer" .$! popup) world
        = world

    onWindowRemove me windowId _ world
        // remove children from iTasks component
        # (children,world)  = me .# "children" .? world
        # world             = forall removeWindow children world
        // send edit event to server
        # (taskId,world)    = me .# "attributes.taskId" .? world
		# (editorId,world)  = me .# "attributes.editorId" .? world
		# edit              = toJSON [LDRemoveWindow windowId]
		# world             = (me .# "doEditEvent" .$! (taskId,editorId,edit)) world
		= world
    where
        removeWindow idx layer world
            # (layerWindowId, world)  = layer .# "attributes.windowId" .? world
            | not (jsIsUndefined layerWindowId) && LeafletObjectID (jsValToString` "" layerWindowId) === windowId =
                ((me .# "removeChild" .$! idx) world)
            = world

	//Map object access
	toLatLng obj world
		# (lat,world)     = obj .# "lat" .? world
		# (lng,world)     = obj .# "lng" .? world
		= ({LeafletLatLng|lat=jsValToReal` 0.0 lat,lng=jsValToReal` 0.0 lng}, world)

	toBounds bounds env
        # (sw,env)          = (bounds .# "getSouthWest" .$ ()) env
        # (ne,env)          = (bounds .# "getNorthEast" .$ ()) env
		# (swpos,env)       = toLatLng sw env
		# (nepos,env)       = toLatLng ne env
        = ({southWest=swpos,northEast=nepos},env)

    getMapBounds mapObj env
        # (bounds,env)      = (mapObj .# "getBounds" .$ ()) env
		= toBounds bounds env

	getMapZoom mapObj world
		# (zoom,world) = (mapObj .# "getZoom" .$ ()) world
		= (jsValToInt` 1 zoom, world)

	setMapZoom mapObj zoom world
		# world     = (mapObj .# "setZoom" .$! zoom) world
		= world

	getMapCenter mapObj world
        # (center,world)    = (mapObj .# "getCenter" .$ ()) world
        = toLatLng center world

	setMapCenter mapObj center world
		# world     = (mapObj .# "panTo" .$! center) world
		= world

	setMapCursor me mapObj position world
		# (cursor,world) = me .# "cursor" .? world
		| jsIsUndefined cursor
			| jsIsNull position //Nothing to do
				= world
			| otherwise
				//Create the cursor
				# (l, world)      = jsGlobal "L" .? world
				# (cursor,world)  = (l .# "circleMarker" .$ (position, CURSOR_OPTIONS)) world
				# world           = (cursor .# "addTo" .$! mapObj) world
				# world           = (me .# "cursor" .= cursor) world
				= world
		| otherwise //Update the position
			| jsIsNull position
				//Destroy the cursor
				# world           = (mapObj .# "removeLayer" .$! cursor) world
				# world           = jsDelete (me .# "cursor") world
				= world
			| otherwise
        		# world           = (cursor .# "setLatLng" .$! position) world
				= world

	addMapTilesLayer me mapObj _ tilesUrl world
		| jsIsNull tilesUrl = world
		# (l, world)      	= jsGlobal "L" .? world
        # (layer,world)     = (l .# "tileLayer" .$ tilesUrl) world
        # world             = (layer .# "addTo" .$! mapObj) world
		= world

	setMapIcons me mapObj icons world
		# (l, world)      	= jsGlobal "L" .? world
		# (index,world) 	= jsEmptyObject world
		# world 			= (me .# "icons" .= index) world
		= forall (createMapIcon me mapObj l index) icons world
	where
		createMapIcon me mapObj l index _ def world
			# (iconId,world)   = def .# 0 .? world
			# (iconSpec,world) = def .# 1 .? world
        	# (icon,world)     = (l .# "icon" .$ iconSpec) world
			# world            = (index .# jsValToString` "" iconId .= icon) world
			= world

	createMapObjects viewMode me mapObj objects world
		# (l, world) = jsGlobal "L" .? world
		= forall (\_ obj -> createMapObject viewMode me mapObj l obj) objects world

	createMapObject viewMode me mapObj l object world
		# (type,world) = object .# "attributes.type" .? world
		= case jsValToString type of
			Just "marker"    = createMarker            me mapObj l object world
			Just "polyline"  = createPolyline          me mapObj l object world
			Just "polygon"   = createPolygon           me mapObj l object world
			Just "circle"    = createCircle            me mapObj l object world
			Just "rectangle" = createRectangle         me mapObj l object world
            Just "window"    = createWindow   viewMode me mapObj l object world
			_                = world

	createMarker me mapObj l object world
        # (markerId,world)    = object .# "attributes.markerId" .? world
        # (options,world)     = jsEmptyObject world
		//Set title
		# (title,world)       = object .# "attributes.title" .? world
		# world               = (options .# "title" .= title) world
		# world               = (options .# "alt" .= title) world
		//Optionally set icon
		# (iconId,world)      = object .# "attributes.icon" .? world
		# (icons,world)       = me .# "icons" .? world
		# world               = addIconOption iconId icons options world
		//Create marker
		# (position,world)    = object .# "attributes.position" .? world
        # (layer,world)       = (l .# "marker" .$ (position,options) ) world
		# world               = (object .# "layer" .= layer) world
        //Optionally add popup
        # (popup,world)       = object .# "attributes.popup" .? world
        # world               = addPopup popup position layer world
        //Store marker ID, needed for related markers of windows
        # world               = (layer .# "markerId" .= markerId) world
		//Set click handler
		# (cb,world)          = jsWrapFun (\a w -> onMarkerClick me (LeafletObjectID (jsValToString` "" markerId)) a w) me world
        # world               = (layer .# "addEventListener" .$! ("click",cb)) world
        //Add to map
        # world               = (layer .# "addTo" .$! mapObj) world
		= world
	where
		addIconOption iconId icons options world
			| jsIsUndefined iconId = world
			# (icon,world) = icons .# (jsValToString` "" iconId) .? world
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
		# (style,world)       = object .# "attributes.style" .? world
		# world               = forall (applyLineStyle options) style world
		# (points,world)      = object .# "attributes.points" .? world
		# (layer,world)       = (l .# "polyline" .$ (points ,options)) world
		# world               = (layer .# "addTo" .$! mapObj) world
		# world               = enableEdit "polylineId" me mapObj layer object getUpdate world
		# world               = (object .# "layer" .= layer) world
		= world
	where
		getUpdate layer world
			# (points, world) = (layer .# "getLatLngs" .$ ()) world
			# (points, world) = jsValToList` points id world
			# (points, world) = foldl (\(res, world) point = appFst (\latLng -> [latLng: res]) $ toLatLng point world)
			                          ([], world)
			                          points
			= (UpdatePolyline $ reverse points, world)

	createPolygon me mapObj l object world
		//Set options
		# (options,world)     = jsEmptyObject world
		# (style,world)       = object .# "attributes.style" .? world
		# world               = forall (applyAreaStyle options) style world
		# (points,world)      = object .# "attributes.points" .? world
		# (layer,world)       = (l .# "polygon" .$ (points ,options)) world
		# world               = (layer .# "addTo" .$! mapObj) world
		# world               = enableEdit "polygonId" me mapObj layer object getUpdate world
		# world               = (object .# "layer" .= layer) world
		= world
	where
		getUpdate layer world
			# (points, world) = (layer .# "getLatLngs" .$ ()) world
			# (points, world) = points .# 0 .? world
			# (points, world) = jsValToList` points id world
			# (points, world) = foldl (\(res, world) point = appFst (\latLng -> [latLng: res]) $ toLatLng point world)
			                          ([], world)
			                          points
			= (UpdatePolygon $ reverse points, world)

	createCircle me mapObj l object world
		//Set options
		# (options,world)     = jsEmptyObject world
		# (style,world)       = object .# "attributes.style" .? world
		# world               = forall (applyAreaStyle options) style world
		# (center,world)      = object .# "attributes.center" .? world
		# (radius,world)      = object .# "attributes.radius" .? world
		# world               = (options .# "radius" .= radius) world
		# (layer,world)       = (l .# "circle" .$ (center, options)) world
		# world               = (layer .# "addTo" .$! mapObj) world
		# world               = enableEdit "circleId" me mapObj layer object getUpdate world
		# world               = (object .# "layer" .= layer) world
		= world
	where
		getUpdate layer world
			# (radius,   world) = (layer .# "getRadius" .$ ()) world
			# (center,   world) = (layer .# "getLatLng" .$ ()) world
			# (center,   world) = toLatLng center world
			= (UpdateCircle center $ jsValToReal` 0.0 radius, world)

	createRectangle me mapObj l object world
		//Set options
		# (options,world)     = jsEmptyObject world
		# (style,world)       = object .# "attributes.style" .? world
		# world               = forall (applyAreaStyle options) style world
		# (sw,world)          = object .# "attributes.bounds.southWest" .? world
		# (ne,world)          = object .# "attributes.bounds.northEast" .? world
		# (layer,world)       = (l .# "rectangle" .$ ([sw, ne], options)) world
		# world               = (layer .# "addTo" .$! mapObj) world
		# world               = enableEdit "rectangleId" me mapObj layer object getUpdate world
		# world               = (object .# "layer" .= layer) world
		= world
	where
		getUpdate layer world
			# (bounds, world) = (layer .# "getBounds" .$ ()) world
			# (bounds, world) = toBounds bounds world
			= (UpdateRectangle bounds, world)

	enableEdit idFieldName me mapObj layer object getUpdate world
		# (isEditable,world)  = object .# "attributes.editable" .? world
		| not $ jsValToBool` False isEditable = world
		# (_, world)  = (layer .# "enableEdit" .$ ()) world
		# (cb, world) = jsWrapFun (onEditing layer) me world
		# (_, world)  = (layer .# "addEventListener" .$ ("editable:vertex:dragend", cb)) world
		# (_, world)  = (layer .# "addEventListener" .$ ("editable:vertex:new",     cb)) world
		# (_, world)  = (layer .# "addEventListener" .$ ("editable:vertex:deleted", cb)) world
		= world
	where
		onEditing layer _ world
			# (update,   world) = getUpdate layer world
			# (objectId, world) = object .# "attributes." +++ idFieldName .? world
			# edit              = toJSON [LDUpdateObject (LeafletObjectID (jsValToString` "" objectId)) update]
			# (taskId,   world) = me .# "attributes.taskId" .? world
			# (editorId, world) = me .# "attributes.editorId" .? world
			# (_,        world) = (me .# "doEditEvent" .$ (taskId, editorId, edit)) world
			= world

	applyAreaStyle options _ style world
		# (styleType, world) = style .# 0 .? world
		# styleType = jsValToString styleType
		| styleType == Just "Style"
			# (directStyle, world) = style .# 1 .? world
			# (directStyleType, world) = directStyle .# 0 .? world
			# (directStyleVal, world)  = directStyle .# 1 .? world
			# directStyleType = jsValToString directStyleType
			= case directStyleType of
				Just "AreaLineStrokeColor" = (options .# "color"       .= directStyleVal) world
				Just "AreaLineStrokeWidth" = (options .# "weight"      .= directStyleVal) world
				Just "AreaLineOpacity"     = (options .# "opacity"     .= directStyleVal) world
				Just "AreaLineDashArray"   = (options .# "dashArray"   .= directStyleVal) world
				Just "AreaNoFill"          = (options .# "fill"        .= False)          world
				Just "AreaFillColor"       = (options .# "fillColor"   .= directStyleVal) world
				Just "AreaFillOpacity"     = (options .# "fillOpacity" .= directStyleVal) world
				_                          = abort "unknown style"
		| styleType == Just "Class"
			# (cls, world) = style .# 1 .? world
			= (options .# "className" .= cls) world
		= abort "unknown style"

    createWindow viewMode me mapObj l object world
        # (layer,world)      = l .# "window" .$ () $ world
		# world              = (object .# "layer" .= layer) world
        # (position,world)   = object .# "attributes.initPosition" .? world
        # (_, world)         = (layer .# "setInitPos" .$ position) world
        # (title,world)      = object .# "attributes.title" .? world
        # (_, world)         = (layer .# "setTitle" .$ title) world
        # (content,world)    = object .# "attributes.content" .? world
        # (_, world)         = (layer .# "setContent" .$ content) world
        # (relMarkers,world) = object .# "attributes.relatedMarkers" .? world
        # world              = forall (\_ relMarker world -> layer .# "addRelatedMarker" .$! relMarker $ world)
                                      relMarkers
                                      world
        // inject function to send event on window remove
		# world = case viewMode of
			True
				= world
			False
                # (windowId,world)   = object .# "attributes.windowId" .? world
                # (onWRemove, world) = jsWrapFun (onWindowRemove me (LeafletObjectID (jsValToString` "" windowId))) me world
                = (layer .# "_onWindowClose" .= onWRemove) world
        // inject function to handle window update
        # (cb,world)         = jsWrapFun (onUIChange layer) me world
        # world              = ((object .# "onUIChange") .= cb) world
        // add to map
        # world              = (layer .# "addTo" .$! mapObj) world
        = world
    where
        onUIChange layer changes world
            # world = foldl doChange world [c \\ c <-: changes]
            = world
        where
            doChange world change
                # (attrUpdates, world) = change .# "attributes" .? world
                # world = forall updateAttr attrUpdates world
                = world

            updateAttr _ attrChange world
                # (name,  world) = attrChange .# "name" .? world
                # name           = jsValToString` "" name
                # (value, world) = attrChange .# "value" .? world
                = case name of
                    "content"        = layer .# "setContent"        .$! value $ world
                    "title"          = layer .# "setTitle"          .$! value $ world
                    "relatedMarkers" = layer .# "setRelatedMarkers" .$! value $ world
                    _                = abort $ concat ["unknown attribute of leaflet window: \"", name, "\"\n"]

    applyLineStyle options _ style world
        # (styleType, world) = style .# 0 .? world
        # styleType = jsValToString styleType
        | styleType == Just "Style"
            # (directStyle, world) = style .# 1 .? world
            # (directStyleType, world) = directStyle .# 0 .? world
            # (directStyleVal, world)  = directStyle .# 1 .? world
            # directStyleType = jsValToString directStyleType
            = case directStyleType of
                Just "LineStrokeColor" = (options .# "color"     .= directStyleVal) world
                Just "LineStrokeWidth" = (options .# "weight"    .= directStyleVal) world
                Just "LineOpacity"     = (options .# "opacity"   .= directStyleVal) world
                Just "LineDashArray"   = (options .# "dashArray" .= directStyleVal) world
                _                      = abort "unknown style"
        | styleType == Just "Class"
            # (cls, world) = style .# 1 .? world
            = (options .# "className" .= cls) world
        = abort "unknown style"

	//Loop through a javascript array
    forall :: !(Int JSVal *JSWorld -> *JSWorld) !JSVal !*JSWorld -> *JSWorld
	forall f array world
		# (len,world) = array .# "length" .? world
		= forall` 0 (jsValToInt` 0 len) world
	where
        forall` :: !Int !Int !*JSWorld -> *JSWorld
		forall` i len world
			| i >= len = world
			# (el,world) = array .# i .? world
			= forall` (i + 1) len (f i el world)

	//Process the edits received from the client
	onEdit dp ([], diffs) m vst = (Ok (NoChange, foldl app m diffs), vst)
	where
		app m (LDSetZoom zoom)          = {LeafletMap|m & perspective = {m.perspective & zoom = zoom}}
		app m (LDSetCenter center)      = {LeafletMap|m & perspective = {LeafletPerspective| m.perspective & center = center}}
		app m (LDSetCursor cursor)      = {LeafletMap|m & perspective = {m.perspective & cursor = Just cursor}}
		app m (LDSetBounds bounds)      = {LeafletMap|m & perspective = {LeafletPerspective| m.perspective & bounds = Just bounds}}
		app m (LDSelectMarker markerId) = {LeafletMap|m & objects = map (sel markerId) m.LeafletMap.objects}
		where
			sel x (Marker m=:{LeafletMarker|markerId}) = Marker {LeafletMarker|m & selected = markerId === x}
			sel x o = o
        app m (LDRemoveWindow idToRemove) = {LeafletMap|m & objects = filter notToRemove m.LeafletMap.objects}
        where
            notToRemove (Window {windowId}) = windowId =!= idToRemove
            notToRemove _                   = True
		app m (LDUpdateObject objectId upd) = {LeafletMap|m & objects = withUpdatedObject <$> m.LeafletMap.objects}
		where
			withUpdatedObject :: !LeafletObject -> LeafletObject
			withUpdatedObject obj | leafletObjectIdOf obj === objectId = case (obj, upd) of
				(Polyline polyline, UpdatePolyline points)
					= Polyline {LeafletPolyline| polyline & points = points}
				(Polygon polygon, UpdatePolygon points)
					= Polygon {LeafletPolygon| polygon & points = points}
				(Circle circle, UpdateCircle center radius)
					= Circle {LeafletCircle| circle & center = center, radius = radius}
				(Rectangle rect, UpdateRectangle bounds)
					= Rectangle {LeafletRectangle| rect & bounds = bounds}
			withUpdatedObject obj = obj
		app m _ = m
	onEdit _ _ msk ust = (Ok (NoChange,msk),ust)

	//Check for changed objects and update the client
	onRefresh _ newMap oldMap vst
		//Determine attribute changes
		# attrChanges = diffAttributes oldMap newMap
		//Determine object changes
		# childChanges = diffChildren oldMap.LeafletMap.objects newMap.LeafletMap.objects updateFromOldToNew encodeUI
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

		updateFromOldToNew :: !LeafletObject !LeafletObject -> ChildUpdate
		updateFromOldToNew (Window old) (Window new) | old.windowId === new.windowId && not (isEmpty changes) =
			ChildUpdate $ ChangeUI changes []
		where
			changes = catMaybes
				[ if (old.LeafletWindow.title == new.LeafletWindow.title)
				     Nothing
				     (Just $ SetAttribute "title" $ toJSON $ new.LeafletWindow.title)
				, if (old.content === new.content)
				     Nothing
				     (Just $ SetAttribute "content" $ toJSON $ toString new.content)
                , if (old.relatedMarkers === new.relatedMarkers)
                     Nothing
                     (Just $ SetAttribute "relatedMarkers" $ toJSON new.relatedMarkers)
				]
		updateFromOldToNew old new | old === new = NoChildUpdateRequired
		                           | otherwise   = ChildUpdateImpossible

	valueFromState m = Just m

gEditor{|LeafletMap|} = leafletEditor

gDefault{|LeafletMap|}
	= {LeafletMap|perspective=defaultValue, tilesUrls = [openStreetMapTiles], objects = [Marker homeMarker], icons = []}
where
	homeMarker = {markerId = LeafletObjectID "home", position= {LeafletLatLng|lat = 51.82, lng = 5.86}, title = Just "HOME", icon = Nothing, popup = Nothing, selected = False}

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
derive class iTask LeafletIcon, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletEdit, LeafletWindow, LeafletWindowPos, LeafletLineStyle, LeafletStyleDef, LeafletAreaStyle, LeafletObjectID, CSSClass, LeafletIconID, LeafletCircle, LeafletObjectUpdate, LeafletRectangle
