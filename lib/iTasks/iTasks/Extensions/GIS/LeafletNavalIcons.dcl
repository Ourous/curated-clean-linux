definition module iTasks.Extensions.GIS.LeafletNavalIcons
/**
* This module provides a set of naval icons to use with leaflet maps
*/
from iTasks.Extensions.GIS.Leaflet import :: LeafletIcon, :: LeafletIconID
from Data.Maybe import :: Maybe
from StdString import class toString

//Add this list to your leaflet map
shipIcons :: [LeafletIcon]

:: ShipIconHeading :== Int
:: ShipIconColor = GrayShip | BlueShip | OrangeShip | GreenShip | RedShip 

instance toString ShipIconColor
/**
* Determine the icon based on a heading, ship color and whether the icon is selected
*/
shipIconId :: (Maybe ShipIconHeading) ShipIconColor Bool -> LeafletIconID
