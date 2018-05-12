implementation module iTasks.Extensions.GIS.LeafletNavalIcons
/**
* This module provides a set of naval icons to use with leaflet maps
*/
from iTasks.Extensions.GIS.Leaflet import :: LeafletIcon(..), :: LeafletIconID(..)
from StdFunc import o
import StdString, StdInt, StdList, Data.Maybe

URL iconId :== "/leaflet-naval-icons/" +++ iconId +++ ".png"
SIZE  :== (24,24)

instance toString ShipIconColor
where
	toString GrayShip   = "gray"
	toString BlueShip   = "blue"
	toString OrangeShip = "orange"
	toString GreenShip  = "green"
	toString RedShip    = "red"

//Add this list to your leaflet map
shipIcons :: [LeafletIcon]
shipIcons = [let iconId = shipIconId heading color sel in {LeafletIcon|iconId=iconId, iconUrl=URL iconId,iconSize = SIZE}
             \\ color <- colors,  heading <- headings, sel <- selected]
where
	selected = [True,False]
	colors = [GrayShip,BlueShip,OrangeShip,GreenShip,RedShip]
	headings = [Nothing: map Just [0,15,30,45,60,75,90,105,120,135,150,165,180,195,210,225,240,255,270,285,300,315,330,345]]
/**
* Find the right icon based on a heading and color
*/
shipIconId :: (Maybe ShipIconHeading) ShipIconColor Bool -> LeafletIconID
shipIconId mbHeading color selected = toString color +++ if selected "-sel" "" +++ maybe "" toRoundedHeading mbHeading 
where	
	toRoundedHeading h = "-" +++ toString (((h rem 360) / 15) * 15)
