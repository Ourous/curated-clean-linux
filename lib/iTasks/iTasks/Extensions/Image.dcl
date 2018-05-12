definition module iTasks.Extensions.Image
/**
* This module provides support for displaying (interactive) images
*/
import iTasks

// Simple web images 
:: WebImage = 
	{ src :: String
    , alt :: String
    , width :: Int
    , height :: Int
    }

derive gText	        WebImage
derive gEditor			WebImage
derive JSONEncode		WebImage
derive JSONDecode		WebImage
derive gDefault			WebImage
derive gEq				WebImage
