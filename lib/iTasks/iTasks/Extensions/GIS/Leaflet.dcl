definition module iTasks.Extensions.GIS.Leaflet

import iTasks

leafletEditor :: Editor LeafletMap

:: LeafletMap =
    { perspective   :: !LeafletPerspective
	, tilesUrls     :: ![String]
	, objects       :: ![LeafletObject]    //Markers, lines and polygon
    , icons         :: ![LeafletIcon]      //Custom icons used by markers. They are indexed by 'iconId' string and cannot be changed once the map is loaded
    }

:: LeafletPerspective =
    { center        :: !LeafletLatLng
    , zoom          :: !Int
    , cursor        :: !Maybe LeafletLatLng
    , bounds        :: !Maybe LeafletBounds
    }

:: LeafletIconID =: LeafletIconID String
:: LeafletIcon =
    { iconId        :: !LeafletIconID
    , iconUrl       :: !String
    , iconSize      :: !(!Int,!Int)
    }

:: LeafletLatLng =
    { lat :: !Real
    , lng :: !Real
    }

:: LeafletBounds =
    { southWest :: !LeafletLatLng
    , northEast :: !LeafletLatLng
    }

:: LeafletObject
    = Marker   !LeafletMarker
    | Polyline !LeafletPolyline
    | Polygon  !LeafletPolygon
    | Window   !LeafletWindow

:: LeafletObjectID =: LeafletObjectID String
:: LeafletMarker =
    { markerId      :: !LeafletObjectID
    , position      :: !LeafletLatLng
    , title         :: !Maybe String
    , icon          :: !Maybe LeafletIconID// Id of the list of icons defined for the map
    , popup         :: !Maybe HtmlTag
    , selected      :: !Bool
    }

:: LeafletPolyline =
    { polylineId    :: !LeafletObjectID
    , points        :: ![LeafletLatLng]
    , style         :: ![LeafletStyleDef LeafletLineStyle]
    }

:: LeafletPolygon =
    { polygonId     :: !LeafletObjectID
    , points        :: ![LeafletLatLng]
    , style         :: ![LeafletStyleDef LeafletPolygonStyle]
    }

:: LeafletWindow =
    { windowId       :: !LeafletObjectID
    , initPosition   :: !LeafletWindowPos
    , title          :: !String
    , content        :: !HtmlTag
    , relatedMarkers :: ![(!LeafletObjectID, ![LeafletStyleDef LeafletLineStyle])] // connecting lines are drawn between the window and the markers
                                                                                 // to visualise the relation
    }

:: LeafletLineStyle    = LineStrokeColor !String // html/css color definition
                       | LineStrokeWidth !Int
                       | LineOpacity     !Real   // between 0.0 and 1.0
                       | LineDashArray   !String // a list of comma separated lengths of alternating dashes and gaps (e.g. "1,5,2,5")

:: LeafletPolygonStyle = PolygonLineStrokeColor !String // html/css color definition
                       | PolygonLineStrokeWidth !Int
                       | PolygonLineOpacity     !Real   // between 0.0 and 1.0
                       | PolygonLineDashArray   !String // a list of comma separated lengths of alternating dashes and gaps (e.g. "1,5,2,5")
                       | PolygonNoFill                  // inside of polygone is not filled, all other fill options are ignored
                       | PolygonFillColor       !String // html/css color definition
                       | PolygonFillOpacity     !Real

:: CSSClass =: CSSClass String
:: LeafletStyleDef style = Style style
                         | Class CSSClass


:: LeafletWindowPos = { x :: !Int, y :: !Int }

//Public tileserver of openstreetmaps
openStreetMapTiles :: String

derive JSONEncode LeafletMap, LeafletPerspective, LeafletLatLng
derive JSONDecode LeafletMap, LeafletPerspective, LeafletLatLng
derive gDefault   LeafletMap, LeafletPerspective, LeafletLatLng
derive gEq        LeafletMap, LeafletPerspective
derive gText      LeafletMap, LeafletPerspective, LeafletLatLng
derive gEditor    LeafletMap, LeafletPerspective, LeafletLatLng
derive class iTask LeafletIcon, LeafletBounds, LeafletObject, LeafletMarker, LeafletPolyline, LeafletPolygon, LeafletWindow, LeafletWindowPos, LeafletLineStyle, LeafletStyleDef, LeafletPolygonStyle
