definition module iTasks.Extensions.GIS.GoogleMap
import iTasks

:: GoogleMap =
	{ settings				:: GoogleMapSettings
	, perspective			:: GoogleMapPerspective
	, markers				:: [GoogleMapMarker]		// Markers placed on the map
	}
:: GoogleMapPerspective =
	{ type					:: GoogleMapType			// The map type
	, center				:: GoogleMapPosition 		// Coordinate of the center point (Required by maps)
	, zoom					:: Int	      				// The zoom level (Required by maps)
	}	
:: GoogleMapType = ROADMAP | SATELLITE | HYBRID | TERRAIN
:: GoogleMapSettings =
	{ mapTypeControl		:: Bool		  				// Show the control for switching between map types
	, panControl			:: Bool		  				// Show the control for panning
	, zoomControl			:: Bool						// Show the control for zooming
	, streetViewControl		:: Bool						// Show the control for street view
	, scaleControl			:: Bool		  				// Show the scale of the map
	, scrollwheel			:: Bool						// Scrollwheel zooming on the map
	, draggable				:: Bool						// Map can be dragged
	}
:: GoogleMapPosition =
	{ lat		:: !Real	//Lattitude
	, lng		:: !Real	//Longitude
	}	
:: GoogleMapMarker =
	{ markerId              :: !String                      // Unique identifier of the marker (to identify it a marker is dragged or selected)
    , position				:: !GoogleMapPosition			// Position of the marker
	, title					:: !Maybe String				// Title of the marker
	, icon					:: !Maybe GoogleMapIcon			// Name of an icon to use
	, infoWindow			:: !Maybe HtmlTag				// Information which is shown on click
	, draggable				:: !Bool						// Can the marker be dragged
	, selected				:: !Bool
	}

:: GoogleMapIcon
	= GoogleMapSimpleIcon String				//Just the name of a png file in Static/icons/ of your application
	| GoogleMapComplexIcon GoogleMapComplexIcon

:: GoogleMapComplexIcon =
	{ image		:: String		//Name of a png file in Static/icons of your application
	, size		:: (Int,Int)	//Dimensions width/height
	, origin	:: (Int,Int)	//Offset in the image x/y used for sprite icon
	, anchor	:: (Int,Int)	//Which x/y in the image is placed on the specified lat/lng in the map
	}

derive JSONEncode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive JSONDecode		GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gDefault			GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEq				GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gText	        GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
derive gEditor	        GoogleMap, GoogleMapSettings, GoogleMapPerspective, GoogleMapPosition, GoogleMapMarker, GoogleMapType, GoogleMapIcon, GoogleMapComplexIcon
