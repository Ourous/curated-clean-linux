implementation module Text.Unicode.Encodings.UTF8

import StdFunc, StdBool
import Data.Word8, Data.List

from Text.Unicode.UChar import instance fromInt UChar, instance toInt UChar
import Text.Unicode

:: UTF8 = UTF8 !String

instance fromUnicode UTF8
where
	fromUnicode :: !UString -> UTF8
	fromUnicode ustr = UTF8 (encodeString ustr)
			
instance toUnicode UTF8 
where
	toUnicode :: !UTF8 -> UString
	toUnicode (UTF8 str) = decodeString str

instance fromString UTF8
where
	fromString :: !{#Char} -> UTF8
	fromString str = UTF8 str
	 
instance toString UTF8 
where
	toString :: !UTF8 -> {#Char}
	toString (UTF8 str) = str

// | Encode a string using 'encode' and store the result in a 'String'.
encodeString :: UString -> String
encodeString xs = bytesToString (encode xs)

// | Decode a string using 'decode' using a 'String' as input.
// | This is not safe but it is necessary if UTF-8 encoded text
// | has been loaded into a 'String' prior to being decoded.
decodeString :: String -> UString
decodeString xs = decode (stringToBytes xs)

replacement_character :: Int
replacement_character = 0xfffd

// | Encode a Haskell String to a list of Word8 values, in UTF8 format.
encode :: UString -> [Word8]
encode us = map fromInt (encode` (map toInt us))

encode` us = concatMap go us
	where
		go oc	| oc <= 0x7f    = [oc]
			 	| oc <= 0x7ff	= [ 0xc0 + (oc >> 6)
                		          , 0x80 + (oc bitand 0x3f)
                        		  ]

				| oc <= 0xffff  = [ 0xe0 + (oc >> 12)
                        		  , 0x80 + ((oc >> 6) bitand 0x3f)
                        		  , 0x80 + (oc bitand 0x3f)
                        		  ]
                        		  
   			       				= [ 0xf0 + (oc >> 18)
		                          , 0x80 + ((oc >> 12) bitand 0x3f)
              			          , 0x80 + ((oc >> 6) bitand 0x3f)
                        		  , 0x80 + (oc bitand 0x3f)
                        		  ]
                        		  
// | Decode a UTF8 string packed into a list of Word8 values, directly to String
decode :: [Word8] -> UString
decode ws = map fromInt (decode` (map toInt ws))

decode` [    ] = []
decode` [c:cs]
  | c < 0x80  = [c : decode` cs]
  | c < 0xc0  = [replacement_character : decode` cs]
  | c < 0xe0  = multi1 cs
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = [replacement_character : decode` cs]
  where
    multi1 [c1 : ds] | (c1 bitand 0xc0) == 0x80 =
        let d = ((c bitand 0x1f) << 6) bitor (c1 bitand 0x3f)
        in if (d >= 0x000080) [d : decode` ds]
                              [replacement_character : decode` ds]
    multi1 _ = [replacement_character : decode` cs]

    multi_byte :: Int Int Int -> [Int]
    multi_byte i mask overlong = aux i cs (c bitand mask)
      where
        aux 0 rs acc
          | (overlong <= acc) && (acc <= 0x10ffff) &&
            (acc < 0xd800 || 0xdfff < acc) &&
            (acc < 0xfffe || 0xffff < acc)      
        			= [acc : decode` rs]
					= [replacement_character : decode` rs]

        aux n [r:rs] acc | (r bitand 0xc0) == 0x80 
        		= aux (n-1) rs ((acc << 6) bitor (r bitand 0x3f))
        aux _ rs _ 
        		= [replacement_character : decode` rs]

        
        
        
        
