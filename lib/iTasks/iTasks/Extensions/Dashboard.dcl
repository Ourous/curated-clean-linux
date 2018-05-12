definition module iTasks.Extensions.Dashboard
/**
* This module provides some types for visuazing key indicator
* values in a dashboard style using simulated lights and gauges
*/
import iTasks

//LED-style visual indicator
:: ControlLight
    = LightOff
    | LightOnGreen
    | LightOnOrange
    | LightOnRed

//Speedometer-style gauge
:: Gauge =
    {val    :: Int    //Current value
    ,min    :: Int    //Maximum range value
    ,max    :: Int    //Minimum range value
    ,unit   :: String //Definition of the units of the scale
    ,label  :: String //Descriptive label of the indicator value
    }

derive JSONEncode       ControlLight
derive JSONDecode       ControlLight
derive gEditor          ControlLight
derive gText 			ControlLight
derive gDefault 		ControlLight
derive gEq              ControlLight
