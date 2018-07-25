implementation module Text.Unicode.Encodings.JS

import StdString, StdArray, StdOverloaded, StdInt, StdChar, StdBool, StdFunc
import Data.List

import Text.Unicode
from Text.Unicode.UChar import instance fromInt UChar, instance toInt UChar, instance fromChar UChar
import qualified Text.Unicode.UChar

:: JSLit = JSLit !String

toJSLiteral :: !UString -> String
toJSLiteral ustr = encodeString ustr

instance fromUnicode JSLit
where
	fromUnicode :: !UString -> JSLit
	fromUnicode ustr = JSLit (encodeString ustr)
			
instance toUnicode JSLit 
where
	toUnicode :: !JSLit -> UString
	toUnicode (JSLit str) = decodeString str

instance fromString JSLit
where
	fromString :: !{#Char} -> JSLit
	fromString str = JSLit str
	 
instance toString JSLit 
where
	toString :: !JSLit -> {#Char}
	toString (JSLit str) = str

toHex 0
	= ['\\0']
toHex i 
	| i < 256
		= ['\\x'] ++ ['0' \\ a <- [1..2-length letters]] ++ reverse (toHex` i)
	| i <= 0xFFFF
	= ['\\u'] ++ ['0' \\ a <- [1..4-length letters]] ++ reverse (toHex` i)
	= toHex 0xFFFF
where
	letters = reverse (toHex` i)
	
	toHex` 0 = []
	toHex` i = [hex.[i bitand 15]:toHex` (i >> 4)] 
	where
		hex = "0123456789ABCDEF" 

encodeString :: UString -> String
encodeString us = {c \\ c <- concatMap (convert o toInt) us}
where
	convert cc 
		| cc == fromChar '\n' = ['\\n']
		| cc == fromChar '\r' = ['\\r']	
		| cc == fromChar '\t' = ['\\t']
		| cc == fromChar '\f' = ['\\f']
		| cc == fromChar '\v' = ['\\v']
		| cc == fromChar '\b' = ['\\b']
		| cc == fromChar '\'' = ['\\\'']	
		| cc == fromChar '"'  = ['\\"']	
		| cc == fromChar '\\' = ['\\\\']	
		| not ('Text.Unicode.UChar'.isControl c) && 'Text.Unicode.UChar'.isAscii c = [fromInt cc]
				= toHex cc
	where
		c :: UChar
		c = fromInt cc 

decodeString :: String -> UString
decodeString str = decode (fromString str)
where
	decode :: [Char] -> UString
	decode [] = []
	decode ['\\':chars] = let (c,chars2) = scanBSChar chars in [c: decode chars2]
	decode [c   :chars] = [fromChar c: decode chars] 

	scanBSChar ['\\':chars] = (fromChar '\\' , chars)
	scanBSChar ['0' :chars] = (fromInt 0, chars)
	scanBSChar ['n' :chars] = (fromChar '\n', chars)
	scanBSChar ['r' :chars] = (fromChar '\r', chars)
	scanBSChar ['f' :chars] = (fromChar '\f', chars)
	scanBSChar ['t' :chars] = (fromChar '\t', chars)
	scanBSChar ['v' :chars] = (fromChar '\v', chars)
	scanBSChar ['b' :chars] = (fromChar '\b', chars)			
	scanBSChar ['"' :chars] = (fromChar '"'  , chars)
	scanBSChar ['\'':chars] = (fromChar '\'' , chars)
	scanBSChar ['x':h1:h2:chars] | isHexDigit h1 && isHexDigit h2
			= (fromInt (dti h1 << 4 + dti h2), chars)
	scanBSChar ['u':h1:h2:h3:h4:chars] | isHexDigit h1 && isHexDigit h2 && isHexDigit h3 && isHexDigit h4
			= (fromInt (dti h1 << 12 + dti h2 << 8 + dti h3 << 4 + dti h4), chars)			
	// unrecognized escape sequense (e.g. less than 2 or 4 digits)
	scanBSChar [c: chars] = (fromChar c, chars)

	dti c 
		| c >= '0' && c <= '9' = toInt (c - '0')
		| c >= 'a' && c <= 'f' = 10 + toInt (c - 'a')
		| c >= 'A' && c <= 'F' = 10 + toInt (c - 'A')				
		
		

