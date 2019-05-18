implementation module iTasks.Extensions.GIS.LeafletNavalIcons
/**
* This module provides a set of naval icons to use with leaflet maps
*/
from iTasks.Extensions.GIS.Leaflet import :: LeafletIcon(..), :: LeafletIconID(..), svgIconURL
from StdFunc import o
import StdString, StdInt, StdList, Data.Maybe, Text, Text.HTML

instance toString ShipIconColor
where
	toString GrayShip   = "gray"
	toString BlueShip   = "blue"
	toString OrangeShip = "orange"
	toString GreenShip  = "green"
	toString RedShip    = "red"

//Add this list to your leaflet map
shipIcons :: [LeafletIcon]
shipIcons =
	[let iconId = shipIconId heading color sel in
		{LeafletIcon|iconId=iconId, iconUrl= svgIconURL (shipIconSVG color heading sel) (24,24), iconSize = (24,24)}
		\\ color <- colors,  heading <- headings, sel <- selected]
where
	selected = [True,False]
	colors = [GrayShip,BlueShip,OrangeShip,GreenShip,RedShip]
	headings = [Nothing: map Just [0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345]]

shipIconSVG color mbHeading sel = GElt [WidthAttr "24",HeightAttr "24"] [] [maybe moored ship mbHeading: if sel [selection] []]
where
	ship heading = PolygonElt
		[WidthAttr "24",HeightAttr "24",StyleAttr ("fill:" +++ toString color +++ ";stroke:black;stroke-width:1")]
		[PointsAttr [(toString x, toString y) \\ (x,y) <- [(8,11),(11,3),(14,3),(17,11),(17,19),(8,19)]]
		,TransformAttr [RotateTransform (toString heading) (Just ("12","12"))]
		]
	moored = PolygonElt
		[WidthAttr "24",HeightAttr "24",StyleAttr ("fill:" +++ toString color +++ ";stroke:black;stroke-width:1")]
		[PointsAttr [(toString x, toString y) \\ (x,y) <- [(7,7),(17,7),(17,17),(7,17)]]
		]

	selection = PolygonElt
		[WidthAttr "24",HeightAttr "24",StyleAttr "fill:none;stroke:black;stroke-width:2"]
		[PointsAttr [(toString x,toString y) \\ (x,y) <- [(1,1),(1,23),(23,23),(23,1)]]
		]
/**
* Find the right icon based on a heading and color
*/
shipIconId :: (Maybe ShipIconHeading) ShipIconColor Bool -> LeafletIconID
shipIconId mbHeading color selected =
	LeafletIconID (concat [toString color, if selected "-sel" "", maybe "" toRoundedHeading mbHeading])
where	
	toRoundedHeading h = "-" +++ toString (((h rem 360) / 15) * 15)
