implementation module Text.Encodings.UrlEncoding

import StdChar, StdString, StdList
import Text

urlEncode :: !String -> String
urlEncode s 							= toString (urlEncode` (fromString s))
where
	urlEncode` :: ![Char] -> [Char]
	urlEncode` []						= []
	urlEncode` [x:xs] 
	| isAlphanum x						= [x  : urlEncode` xs]
	| otherwise							= urlEncodeChar x ++ urlEncode` xs
	where
		urlEncodeChar ' '				= ['+']
		urlEncodeChar x					= ['%', c1 ,c2]
		
		(c1,c2)							= charToHex x

		charToHex :: !Char -> (!Char, !Char)
		charToHex c						= (toChar (digitToHex (i >> 4)), toChar (digitToHex (i bitand 15)))
		where
		        i						= toInt c
		        digitToHex :: !Int -> Int
		        digitToHex d
		                | d <= 9		= d + toInt '0'
		                | otherwise		= d + toInt 'A' - 10

urlDecode :: !String -> String
urlDecode s								= toString (urlDecode` (fromString s))
where
	urlDecode` :: ![Char] -> [Char]
	urlDecode` []						= []
	urlDecode` ['+':xs]				 	= [' ':urlDecode` xs]
	urlDecode` ['%',hex1,hex2:xs]		= [hexToChar(hex1, hex2):urlDecode` xs]
	urlDecode` [x:xs]				 	= [x:urlDecode` xs]

	hexToChar :: !(!Char, !Char) -> Char
	hexToChar (a, b)					= toChar ((hexToDigit (toInt a)) << 4 + hexToDigit (toInt b))

	hexToDigit :: !Int -> Int
	hexToDigit i
		| i <= toInt '9'	= i - (toInt '0')
		| otherwise			= 10 + (i - (toInt 'A'))


urlEncodePairs :: ![(String, String)] -> String
urlEncodePairs pairs = join "&" [ urlEncode k +++ "=" +++ urlEncode v \\ (k,v) <- pairs]

urlDecodePairs :: !String -> [(String,String)]
urlDecodePairs s = [(urlDecode k, urlDecode v) \\ [k,v] <- [split "=" part \\ part <- split "&" s ]]
