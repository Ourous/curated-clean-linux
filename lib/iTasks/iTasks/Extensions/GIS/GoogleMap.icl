implementation module iTasks.Extensions.GIS.GoogleMap

import iTasks
import iTasks.UI.Definition, iTasks.UI.JS.Interface, iTasks.UI.JS.Encoding, iTasks.UI.Editor
import iTasks.UI.JS.Map

import Data.Functor, Data.Error, Text, StdMisc

import qualified Data.Map as DM
from StdArray import class Array(uselect), instance Array {} a

GOOGLEMAP_JS = "http://maps.googleapis.com/maps/api/js?callback=googleMapsLoaded"

:: JSGM = JSGM

:: GoogleMapDiff
    = SetSettings GoogleMapSettings
    | SetPerspective GoogleMapPerspective
    | AddMarkers [GoogleMapMarker]
    | UpdateMarkers [GoogleMapMarker]
    | RemoveMarkers [String]

//--------------------------------------------------------------------------------------------------

:: GoogleMapState = {markers      :: [GoogleMapMarker]
					,nextMarkerId :: Int
					,markerMap	  :: JSMap String (JSObj GoogleMapMarker)
					}

// Parameter object for creating google.maps.Map
:: MapOptions = {center    			:: JSObj JSGM
          		,zoom      			:: Int
          		,mapTypeId 			:: JSObj JSGM
				,mapTypeControl		:: Bool
				,panControl			:: Bool
				,zoomControl		:: Bool
				,streetViewControl	:: Bool
				,scaleControl		:: Bool
				,scrollwheel		:: Bool
				,draggable			:: Bool
        		}

// Parameter object for creating google.maps.Marker
:: MarkerOptions = {map        :: JSObj JSGM
				   ,position   :: JSObj JSGM
				   ,title      :: String
				   ,draggable  :: Bool
				   ,icon       :: Maybe (JSObj JSGM)
				   ,id		   :: String
				   }

derive class iTask GoogleMapState, JSGM

derive JSEncode GoogleMap, GoogleMapPerspective, GoogleMapMarker, GoogleMapSettings, GoogleMapType, GoogleMapIcon, GoogleMapPosition, GoogleMapComplexIcon
derive JSEncode GoogleMapDiff
derive JSEncode Maybe, HtmlTag, HtmlAttr, SVGElt, SVGAttr, SVGZoomAndPan, SVGLengthUnit, SVGTransform, SVGStrokeWidth, SVGStrokeMiterLimit
derive JSEncode SVGLineJoin, SVGLineCap, SVGStrokeDashOffset, SVGStrokeDashArray, SVGPaint, SVGMeetOrSlice, SVGAlign, SVGDefer, SVGLengthAdjust, SVGFillRule
derive JSEncode SVGFillOpacity, SVGFuncIRI, SVGColor

derive JSDecode GoogleMap, GoogleMapPerspective, GoogleMapMarker, GoogleMapSettings, GoogleMapType, GoogleMapIcon, GoogleMapPosition, GoogleMapComplexIcon
derive JSDecode GoogleMapDiff
derive JSDecode Maybe, HtmlTag, HtmlAttr, SVGElt, SVGAttr, SVGZoomAndPan, SVGLengthUnit, SVGTransform, SVGStrokeWidth, SVGStrokeMiterLimit
derive JSDecode SVGLineJoin, SVGLineCap, SVGStrokeDashOffset, SVGStrokeDashArray, SVGPaint, SVGMeetOrSlice, SVGAlign, SVGDefer, SVGLengthAdjust, SVGFillRule
derive JSDecode SVGFillOpacity, SVGFuncIRI, SVGColor

googleMapEditor :: Editor GoogleMap
googleMapEditor
    = { Editor
      | genUI  = withClientSideInit initUI genUI
      , onEdit = onEdit
      , onRefresh = onRefresh
      }
where
	genUI dp val world
		# attr = 'DM'.unions [sizeAttr (ExactSize 500) (ExactSize 200), valueAttr (encodeOnServer val)]
		= (Ok (uia UIComponent attr,newFieldMask),world)

	initUI me world
		# (jsInitDOM, world)   = jsWrapFun (initDOM me) world
		# (gmaps_loaded,world) = findObject "googlemaps_loaded" world
		| jsIsUndefined gmaps_loaded
			//Check if another editlet has already loaded the javascript
			# (gmaps_callbacks,world) = findObject "googlemaps_callbacks" world
			| jsIsUndefined gmaps_callbacks
				//Create array of callback functions
				# (gmaps_callbacks,world)      = newJSArray world
				# (_,world)                    = jsArrayPush jsInitDOM gmaps_callbacks world
				# world                        = (jsWindow .# "googlemaps_callbacks" .= gmaps_callbacks) world
				//Prepare callback functions
				# (jsGoogleMapsCallback,world) = jsWrapFun googleMapsLoaded world
				# world                        = (jsWindow .# "googleMapsLoaded" .= jsGoogleMapsCallback) world
				//Load Google Maps library
				# world                        = addJSFromUrl GOOGLEMAP_JS Nothing world
				= world
			| otherwise
				//Just add a callback to the existing set of callbacks
				# (_,world) = jsArrayPush jsInitDOM gmaps_callbacks world
				= world
		| otherwise
			# world = (me .# "initDOMEl" .= jsInitDOM) world
			= world

	googleMapsLoaded args world
		# world = (jsWindow .# "googlemaps_loaded" .= (toJSVal True)) world
		# (callbacks,world)  = .? (jsWindow .# "googlemaps_callbacks") world
		# (callbacks, world) = fromJSArray callbacks id world
		//Call all the callbacks in the array
		# world = foldl (\w cb -> snd (jsApply cb jsWindow [] w)) world callbacks 
		= (jsNull, world)

	initDOM me args world
		//Get the initial data from the value attribute
		# (value,world)      = .? (me .# "attributes.value") world 
		# (st,world)         = decodeOnClient value world
		//Create the parameter object
		# (options,world)    = jsEmptyObject world
		# perspective        = st.GoogleMap.perspective
		# mapType            = toString perspective.GoogleMapPerspective.type
		# (mapTypeId,world)  = .? (jsWindow .# "google.maps.MapTypeId" .# mapType) world
		# world              = (options .# "mapTypeId" .= mapTypeId) world
		# {lat,lng}          = perspective.GoogleMapPerspective.center
	    # (center, world)    = jsNewObject "google.maps.LatLng" [toJSArg lat,toJSArg lng] world
		# world              = (options .# "center" .= center) world
		# world              = (options .# "zoom" .= perspective.GoogleMapPerspective.zoom) world
		# settings           = st.GoogleMap.settings
		# world              = (options .# "mapTypeControl" .= settings.GoogleMapSettings.mapTypeControl) world
		# world              = (options .# "panControl" .= settings.GoogleMapSettings.panControl) world
		# world              = (options .# "zoomControl" .= settings.GoogleMapSettings.zoomControl) world
		# world              = (options .# "streetViewControl" .= settings.GoogleMapSettings.streetViewControl) world
		# world              = (options .# "scaleControl" .= settings.GoogleMapSettings.scaleControl) world
		# world              = (options .# "scrollwheel" .= settings.GoogleMapSettings.scrollwheel) world
		# world              = (options .# "draggable" .= settings.GoogleMapSettings.draggable) world
		//Create the map object
		# (domEl,world)       = .? (me .# "domEl") world
	    # (mapobj, world)     = jsNewObject "google.maps.Map" [toJSArg domEl,toJSArg options] world
		# world               = (me .# "map" .= mapobj) world
		//Attach onAttributeChange
		# (cb,world) 		  = jsWrapFun (onAttributeChange me) world
		# world      	      = ((me .# "onAttributeChange") .= cb) world		
		//Attach event handlers
		# (jsOnShow,world)    = jsWrapFun (onShow me) world
		# world               = (me .# "onShow" .= jsOnShow) world
		# world				  = addListeners me world
		//Create initial markers
		= (jsNull,world)

	addListeners me world
		# (mapobj,world) = .? (me .# "map") world
		
		# (jsOnDragEnd,world) = jsWrapFun (onPerspectiveChanged me) world
		# (jsOnDragEndListener, world)          = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "dragend", jsOnDragEnd)) world
		# world = ((me .# "jsOnDragEndListener") .= jsOnDragEndListener) world
		
		# (jsOnMapTypeChanged,world) = jsWrapFun (onPerspectiveChanged me) world
		# (jsOnMapTypeChangedListener, world)   = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "maptypeid_changed", jsOnMapTypeChanged)) world
		# world = ((me .# "jsOnMapTypeChangedListener") .= jsOnMapTypeChangedListener) world

		# (jsOnZoomChanged,world) = jsWrapFun (onPerspectiveChanged me) world
		# (jsOnZoomChangedListener, world)       = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "zoom_changed", jsOnZoomChanged)) world
		# world = ((me .# "jsOnZoomChangedListener") .= jsOnZoomChangedListener) world
		
		# (jsOnMapClick,world) = jsWrapFun (onMapClick me) world
		# (jsOnMapClickListener, world)          = (jsWindow .# "google.maps.event.addListener" .$ (mapobj, "click", jsOnMapClick)) world
		# world = ((me .# "jsOnMapClickListener") .= jsOnMapClickListener) world

		= world
	
	removeListeners me world
		# (jsOnDragEndListener,world) = .? (me .# "jsOnDragEndListener") world			
		# (_, world)          = (jsWindow .# "google.maps.event.removeListener" .$ (jsOnDragEndListener)) world
		
		# (jsOnMapTypeChangedListener,world) = .? (me .# "jsOnMapTypeChangedListener") world
		# (_, world)          = (jsWindow .# "google.maps.event.removeListener" .$ (jsOnMapTypeChangedListener)) world

		# (jsOnZoomChangedListener,world) = .? (me .# "jsOnZoomChangedListener") world
		# (_, world)          = (jsWindow .# "google.maps.event.removeListener" .$ (jsOnZoomChangedListener)) world
		
		# (jsOnMapClickListener,world) = .? (me .# "jsOnMapClickListener") world		
		# (_, world)          = (jsWindow .# "google.maps.event.removeListener" .$ (jsOnMapClickListener)) world
		= world	
	
	onShow me args world
	    //Trigger a resize event for the map
		# (map,world) = .? (me .# "map") world
        # (_, world)  = (jsWindow .# "google.maps.event.trigger" .$ (map, "resize")) world
		//Correct center
		# (lat,world)        = .? (me .# "value.perspective.center.lat") world
		# (lng,world)        = .? (me .# "value.perspective.center.lng") world
	    # (center, world)    = jsNewObject "google.maps.LatLng" [toJSArg lat,toJSArg lng] world
        # (_, world)         = (map .# "setCenter" .$ center) world
		= (jsNull,world)

	onAttributeChange me [name,value] world
		# world = jsTrace name world
		# world = jsTrace value world
		| jsArgToString name == "diff"
			# (diff,world) = decodeOnClient (toJSVal value) world
			= (jsNull, applyDiffs me diff world)
		| otherwise
			= (jsNull, jsTrace "Unknown attribute change" world)

	applyDiffs :: (JSObj ()) [GoogleMapDiff] *JSWorld -> *JSWorld			
	applyDiffs me diffs world
		# world = removeListeners me world
		# (map, world) = .? (me .# "map") world
		# world = foldl (appDiff map) world diffs
		= addListeners me world
	where
		appDiff map world (SetPerspective {GoogleMapPerspective|type, center = {lat, lng}, zoom})
	    	# (center, world) 	= jsNewObject "google.maps.LatLng" [toJSArg lat,toJSArg lng] world
        	# (_, world)      	= (map .# "setCenter" .$ center) world	
        	# (_, world)      	= callObjectMethod "setZoom" [toJSArg zoom] map world
			# (mapTypeId, world)= findObject ("google.maps.MapTypeId." +++ toString type) world 
        	# (_, world)        = callObjectMethod "setMapTypeId" [toJSArg mapTypeId] map world			       	      	 		
			= jsTrace "SetPerspective" world
			
		appDiff map world (AddMarkers markers)
			# (st, world) 		= getState me world
			# (map,world) 		= .? (me .# "map") world	
			
			# world = foldl (\w m -> createMarker me map st.markerMap m w) world markers
			
			# world = setState me {GoogleMapState | st & markers = st.GoogleMapState.markers ++ markers} world						
			= jsTrace "AddMarkers" world

		appDiff map world (RemoveMarkers markers)
			# (st, world) 		= getState me world
			# (map,world) 		= .? (me .# "map") world
			
			# world = foldl (\w m -> removeMarker st.markerMap m w) world markers
							
			= jsTrace "RemoveMarkers" world	
				
		appDiff map world (UpdateMarkers markers)	
			# (st, world) 		= getState me world
			# (map,world) 		= .? (me .# "map") world
			
			# world = foldl (\w m -> updateMarker me map st.markerMap m w) world markers
			
			= jsTrace "UpdateMarkers" world				
				
		appDiff map world _
			= world

	getPespective map world
		# (zoom, world) = callObjectMethod "getZoom" [] map world
		# (latLng, world) = callObjectMethod "getCenter" [] map world
		# (typeId, world) = callObjectMethod "getMapTypeId" [] map world		
		# ((lat, lng), world) = getPos latLng world
		= ({GoogleMapPerspective|type = fromString (toUpperCase (jsValToString typeId)), center = {lat = lat, lng = lng}, zoom = jsValToInt zoom}, world)
		
	onPerspectiveChanged me args world
		# (map,world) = .? (me .# "map") world
		# (perspective, world) = getPespective map world
		
		# (taskId,world)    = .? (me .# "attributes.taskId") world
		# (editorId,world)  = .? (me .# "attributes.editorId") world
		# (diff,world)      = encodeOnClient [SetPerspective perspective] world
		# (_,world) = ((me .# "doEditEvent") .$ (taskId,editorId,diff)) world
		
		= (jsNull, world)

	onMapClick me args world
		# (st, world) 			= getState me world
		# (map,world) 			= .? (me .# "map") world		
		# (latlng, world)       = .? (toJSVal (args !! 0) .# "latLng") world
		# ((lat, lng), world)   = getPos latlng world
		# markrec 				= createMarkerRecord (toString st.GoogleMapState.nextMarkerId) lat lng Nothing		
		# markers 				= [markrec: st.GoogleMapState.markers]
		# world 				= createMarker me map st.markerMap markrec world
		# world					= setState me {st & nextMarkerId = st.nextMarkerId + 1, markers = markers} world

		# (taskId,world)  		= .? (me .# "attributes.taskId") world
		# (editorId,world)  	= .? (me .# "attributes.editorId") world		
		# (diff,world)          = encodeOnClient [AddMarkers [markrec]] world
		# (_,world) 			= ((me .# "doEditEvent") .$ (taskId,editorId,diff)) world
		
		= (jsNull, world)
		
	createMarker me map markerMap {GoogleMapMarker|markerId,position,title,draggable,icon} world
	    # (latlng, world) = jsNewObject "google.maps.LatLng"
	    				[toJSArg position.lat
	    				,toJSArg position.lng] world	
	    				
		# (mbIconObj, world) = case icon of
					Nothing = (Nothing, world)
					(Just (GoogleMapSimpleIcon name)) = (Just (toJSVal ("/icons/"+++name)), world)
					(Just (GoogleMapComplexIcon prop))
						# (size, world) = jsNewObject "google.maps.Size"
								[toJSArg (fst prop.GoogleMapComplexIcon.size), toJSArg (snd prop.GoogleMapComplexIcon.size)] world

						# (origin, world) = jsNewObject "google.maps.Point"
								[toJSArg (fst prop.origin), toJSArg (snd prop.origin)] world

						# (anchor, world) = jsNewObject "google.maps.Point"
								[toJSArg (fst prop.anchor), toJSArg (snd prop.anchor)] world						

						# (iconObj, world) = jsNewObject "google.maps.MarkerImage"
								[toJSArg ("/icons/"+++prop.image)
								,toJSArg size
								,toJSArg origin
								,toJSArg anchor] world
								
						= (Just iconObj, world)

		# (marker, world)
				= jsNewObject "google.maps.Marker"
						[toJSArg {MarkerOptions
								 | map = map
								 , position = latlng
								 , title = maybe "" id title
								 , draggable = draggable
								 , icon = mbIconObj
								 , id = markerId}]
						world
							
    	# (jsOnClick,world) = jsWrapFun (onMarkerClick me) world
		# (_, world)        = (jsWindow .# "google.maps.event.addListener" .$ (marker, "click", jsOnClick)) world	
    	# (jsOnDrag,world)  = jsWrapFun (onMarkerDrag me) world		    	
		# (_, world)        = (jsWindow .# "google.maps.event.addListener" .$ (marker, "dragend", jsOnDrag)) world			
								
		= jsPut markerId marker markerMap world
	where
        onMarkerClick me args world
			# (st, world) 		= getState me world
            //Toggle selection
            # markers 			= [{GoogleMapMarker|m & selected = (m.GoogleMapMarker.markerId == markerId)} \\ m <- st.GoogleMapState.markers]
			
			# (taskId,world)  	= .? (me .# "attributes.taskId") world
			# (editorId,world)  = .? (me .# "attributes.editorId") world		
			# (diff,world) 		= encodeOnClient [UpdateMarkers markers] world		
			# (_,world) 		= ((me .# "doEditEvent") .$ (taskId,editorId,diff)) world

			# world				= setState me {GoogleMapState | st & markers = markers} world					
			= (jsNull, world)

		onMarkerDrag me args world
			# (st, world) 		= getState me world
			
			# (latLng, world)       = .? (toJSVal (args !! 0) .# "latLng") world
			# ((lat, lng), world)   = getPos latLng world
            #  markers 				= [if (m.GoogleMapMarker.markerId == markerId) {GoogleMapMarker|m & position= {GoogleMapPosition | lat = lat, lng = lng}} m \\ m <- st.GoogleMapState.markers]
            
			# (taskId,world)  	= .? (me .# "attributes.taskId") world
			# (editorId,world)  = .? (me .# "attributes.editorId") world		
			# (diff,world) 	    = encodeOnClient [UpdateMarkers markers] world
			# (_,world) 		= ((me .# "doEditEvent") .$ (taskId,editorId,diff)) world					

			# world				= setState me {GoogleMapState | st & markers = markers} world		
			= (jsNull, world)

	getState me world
		# (jsState,world) = .? (me .# "st") world
		= case jsIsUndefined jsState of
				True	= defaultState world
				False   = jsGetCleanVal "st" me world
		
	setState me state world
		= jsPutCleanVal "st" state me world
			
	defaultState world 
		# (jsMap, world) = jsNewMap world
		= ({markers = [], nextMarkerId = 0, markerMap = jsMap}, world)
			
/*
    resizeMap cid event clval=:{val={perspective={GoogleMapPerspective|center}}, mbSt=Just {mapobj}} world
        //Resize map
        # (mapevent, world) = findObject "google.maps.event" world
		# (_, world)     	= callObjectMethod "trigger" [toJSArg mapobj, toJSArg "resize"] mapevent world
        //Correct center
        # (latlng, world)   = jsNewObject "google.maps.LatLng" [toJSArg center.lat,toJSArg center.lng] world	
        # (_, world)        = callObjectMethod "setCenter" [toJSArg latlng] mapobj world
        = (clval, NoDiff, world)
*/

	getPos obj world
		# (lat, world) = callObjectMethod "lat" [] obj world
		# (lng, world) = callObjectMethod "lng" [] obj world
		= ((jsValToReal lat,jsValToReal lng), world)

	createMarkerRecord markerId lat lng mbTitle =
	    { GoogleMapMarker
		| markerId	    = markerId
		, position      = {GoogleMapPosition | lat = lat, lng = lng}
		, title 		= mbTitle
		, icon 		    = Nothing
		, infoWindow    = Nothing
		, draggable     = True
		, selected      = False}

    removeMarker markerMap markerId world
        # (mbMarker,world) = jsGet markerId markerMap world
        = case mbMarker of
            Just marker
                # (_, world)    = callObjectMethod "setMap" [toJSArg jsNull] marker world
                # world         = jsDel markerId markerMap world
                = world
            Nothing         = world
	
	updateMarker me map markerMap marker=:{GoogleMapMarker|markerId,position,title,draggable,icon} world
        # world = removeMarker markerMap markerId world
        = createMarker me map markerMap marker world

    onScriptLoad args world
        # world                   = jsSetObjectAttr "googlemaps_loaded" (toJSVal True) jsWindow world
        # (gmaps_callbacks,world) = findObject "googlemaps_callbacks" world
        # (object,world)          = findObject "Object" world
        # (cids,world)            = callObjectMethod "keys" [toJSArg gmaps_callbacks] object world
        # (cids,world)            = fromJSArray cids jsValToString world
        # world                   = foldl (call gmaps_callbacks) world cids
        = (jsNull,world)
    where
        call callbacks world cid
            # (cb,world) = jsGetObjectAttr cid callbacks world
            # (_,world) = jsApply cb jsWindow [] world
            = world

	onRefresh _ g2 g1 mask vst = case settingsDiff ++ perspectiveDiff ++ remMarkersDiff ++ addMarkersDiff ++ updMarkersDiff of
        []      = (Ok (NoChange,mask),g2,vst)
        diffs   = (Ok (ChangeUI [SetAttribute "diff" (encodeOnServer diffs)] [],mask),g2,vst)
    where
        settingsDiff    = if (g1.GoogleMap.settings === g2.GoogleMap.settings) [] [SetSettings g2.GoogleMap.settings]
        perspectiveDiff = if (g1.GoogleMap.perspective === g2.GoogleMap.perspective) [] [SetPerspective g2.GoogleMap.perspective]
        remMarkersDiff = case [markerId \\ markerId <-oldMarkerIds | not (isMember markerId newMarkerIds)] of
            []          = []
            markerIds   = [RemoveMarkers markerIds]
        addMarkersDiff = case [marker \\ marker=:{GoogleMapMarker|markerId} <- g2.GoogleMap.markers | not (isMember markerId oldMarkerIds)] of
            []          = []
            markers     = [AddMarkers markers]
        updMarkersDiff = case [marker \\ marker <- g2.GoogleMap.markers | isUpdated marker] of
            []          = []
            markers     = [UpdateMarkers markers]
        where
            isUpdated marker = not (isEmpty [m \\ m <- g1.GoogleMap.markers | m.GoogleMapMarker.markerId == marker.GoogleMapMarker.markerId && m =!= marker])

        oldMarkerIds = [markerId \\ {GoogleMapMarker|markerId} <- g1.GoogleMap.markers]
        newMarkerIds = [markerId \\ {GoogleMapMarker|markerId} <- g2.GoogleMap.markers]

	onEdit dp ([],d) g msk ust = case decodeOnServer d of
		Just diffs = (Ok (NoChange,msk),foldl app g diffs,ust)
		Nothing    = (Ok (NoChange,msk),g,ust)
    where
        app g (SetSettings settings)        = {GoogleMap|g & settings = settings}
        app g (SetPerspective perspective)  = {GoogleMap|g & perspective = perspective}
        app g (AddMarkers m)                = {GoogleMap|g & markers = g.GoogleMap.markers ++ m}
        app g (UpdateMarkers m)             = {GoogleMap|g & markers = foldl upd g.GoogleMap.markers m}
        where
            upd markers updated = [if (m.GoogleMapMarker.markerId == updated.GoogleMapMarker.markerId) updated m \\ m <- markers]
        app g (RemoveMarkers m)             = {GoogleMap|g & markers = [marker \\ marker <- g.GoogleMap.markers | not (isMember marker.GoogleMapMarker.markerId m)]}
        app g _ = g
	onEdit _ d g msk ust = (Ok (NoChange,msk),g,ust)

//--------------------------------------------------------------------------------------------------
instance toString GoogleMapType
where
	toString ROADMAP    = "ROADMAP"
	toString SATELLITE  = "SATELLITE"
	toString HYBRID     = "HYBRID"
	toString TERRAIN    = "TERRAIN"

instance fromString GoogleMapType
where 
	fromString "ROADMAP"    = ROADMAP
	fromString "SATELLITE"  = SATELLITE
	fromString "HYBRID"     = HYBRID
	fromString "TERRAIN"    = TERRAIN			

gText{|GoogleMapPosition|} _ (Just {GoogleMapPosition|lat,lng}) = [toString lat + " " + toString lng]
gText{|GoogleMapPosition|} _ _ = [""]

gEditor{|GoogleMap|} = googleMapEditor

gDefault{|GoogleMapPerspective|} =
	{ GoogleMapPerspective
	| type				= ROADMAP
	, center 			= {GoogleMapPosition|lat = 51.82, lng = 5.86}
	, zoom				= 10
	}
gDefault{|GoogleMapSettings|} =
	{ GoogleMapSettings
	| mapTypeControl	= True
	, panControl		= True
	, streetViewControl	= True
	, zoomControl		= True
	, scaleControl		= True
	, scrollwheel		= True
	, draggable			= True
	}

derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gDefault			GoogleMap, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gText	        GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff
derive gEditor                     GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon,GoogleMapDiff

