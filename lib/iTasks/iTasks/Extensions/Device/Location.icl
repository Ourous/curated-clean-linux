implementation module iTasks.Extensions.Device.Location

import iTasks
import iTasks.Extensions.Device._Common
from Text import class Text, instance Text String
import qualified Text as T

derive class iTask Coordinates

getLocation :: Task (Maybe Coordinates)
getLocation
	= catchAll (deviceRequest "location" (\_ -> True)) (\_ -> return "")
	>>- \result -> unpack ('T'.split " " result)
where
	unpack :: [String] -> Task (Maybe Coordinates)
	unpack ["OK", lat, lon]	= return (Just (LatLon (toReal lat) (toReal lon)))
	unpack _		= return Nothing
