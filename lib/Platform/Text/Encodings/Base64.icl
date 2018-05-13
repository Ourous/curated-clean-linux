implementation module Text.Encodings.Base64

import StdChar, StdString, StdList, StdArray, StdMisc, StdBool

//65th character is padding-character
stdAlphabet :== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/="
urlAlphabet :== "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_="

decodeWithUrlAlphabet :: {#Char}
decodeWithUrlAlphabet =: { createArray 256 '\0' & [toInt urlAlphabet.[i]]=toChar i \\ i<-[0..size urlAlphabet-1]}

decodeWithStdAlphabet :: {#Char}
decodeWithStdAlphabet =: { createArray 256 '\0' & [toInt stdAlphabet.[i]]=toChar i \\ i<-[0..size stdAlphabet-1]}

base64Encode :: !.String -> .String
base64Encode s = encodeString s stdAlphabet

base64EncodeLen :: !.String !Length -> .String
base64EncodeLen s l = addLineBreaks (encodeString s stdAlphabet) l

base64URLEncode :: !.String -> .String
base64URLEncode s = encodeString s urlAlphabet

base64URLEncodeLen :: !.String !Length -> .String
base64URLEncodeLen s l = addLineBreaks (encodeString s urlAlphabet) l

encodeString :: !.{#Char} !Alphabet -> .{#Char}
encodeString s a
  #! destSize   = 4 * ((srcSize + 2) / 3)
  #! destString = createArray destSize '\0'
  = encodeString` destString 0 0 a
  where
  encodeString` :: !*{#Char} !Int !Int !Alphabet -> *{#Char}
  encodeString` dest src_o dest_o a
    #! r = srcSize - src_o
    | r >= 3
      #! oct = (toInt s.[src_o] << 16) + (toInt s.[src_o+1] << 8) + toInt s.[src_o+2]
      #! dest & [dest_o + 3] = a.[oct bitand 63]
      #! dest & [dest_o + 2] = a.[(oct >> 6)  bitand 63]
      #! dest & [dest_o + 1] = a.[(oct >> 12) bitand 63]
      #! dest & [dest_o]     = a.[(oct >> 18) bitand 63]
      = encodeString` dest (src_o + 3) (dest_o + 4) a
    | r == 2
      #! c1  = s.[src_o]
      #! c2  = s.[src_o+1]
      #! oct = (toInt c1 << 16) + (toInt c2 << 8)
      = encodeLastOctet oct 3 1 dest
    | r == 1
      #! c1  = s.[src_o]
      #! oct = toInt c1 << 16
      = encodeLastOctet oct 3 2 dest
    | r == 0 = dest
    where
    encodeLastOctet :: !Int !Offset !Padding !*{#Char} -> *{#Char}
    encodeLastOctet oct off p s
      | off < 0   = s
      | p > 0     = encodeLastOctet (oct >> 6) (off - 1) (p - 1) {s & [off + dest_o] = '='}
      | otherwise = encodeLastOctet (oct >> 6) (off - 1) p       {s & [off + dest_o] = a.[oct bitand 63]}
  srcSize = size s

addLineBreaks :: !.String Length -> .String
addLineBreaks s l
| l > 0 = addLineBreaks` s "" l
| otherwise = abort "Length cannot be 0 or less."
where
	addLineBreaks` :: !.String !.String !Length -> .String
	addLineBreaks` src dest len
	| len >= (size src) = dest +++. src
	| otherwise = addLineBreaks` (src % (len,(size src))) (dest+++(src % (0,len-1))+++"\n") len

base64Decode :: !.String -> .String
base64Decode s = decodeString (removeLineBreaks s) decodeWithStdAlphabet

base64URLDecode :: !.String -> .String
base64URLDecode s = decodeString (removeLineBreaks s) decodeWithUrlAlphabet

decodeString :: !.String !Alphabet -> .String
decodeString s a
  | srcSize bitand 3 <> 0 = abort "Base64: Invalid length, size of decoding string must be a multitude of 4."
  #! destString = createArray destSize '\0'
  = decodeString` destString 0 0
  where
  decodeString` :: !*{#Char} !Int !Int -> *{#Char}
  decodeString` dest src_o dest_o
    | src_o < srcSize - 4 = decodeString` (decodeCommonOctet dest src_o dest_o) (src_o + 4) (dest_o + 3)
    | src_o < srcSize     = decodeLastOctet dest
    | otherwise           = dest
    where
    decodeLastOctet :: !*{#Char} -> *{#Char}
    decodeLastOctet dest
      | s.[src_o + 2] == '='
        // lose the last four obsolete bits (2*6-8b)
        #! oct = (fromChar a.[toInt s.[src_o]]     << 2) +
                 (fromChar a.[toInt s.[src_o + 1]] >> 4)
        #! dest & [dest_o] = toChar oct
        = dest
      | s.[src_o + 3] == '='
        #! oct = (fromChar a.[toInt s.[src_o]]     << 10) +
                 (fromChar a.[toInt s.[src_o + 1]] << 4)  +
                 (fromChar a.[toInt s.[src_o + 2]] >> 2)
        // lose the last two obsolete bits (3*6-2*8b)
        #! dest & [dest_o+1] = toChar oct
        #! dest & [dest_o]   = toChar (oct >> 8)
        = dest
      | otherwise = decodeCommonOctet dest src_o dest_o
  decodeCommonOctet :: !*String !Int !Int -> *String
  decodeCommonOctet dest src_o dest_o
    #! oct = ((fromChar a.[toInt (s.[src_o])])     << 18) +
             ((fromChar a.[toInt (s.[src_o + 1])]) << 12) +
             ((fromChar a.[toInt (s.[src_o + 2])]) << 6)  +
              (fromChar a.[toInt (s.[src_o + 3])])
    #! dest & [dest_o + 2] = toChar oct
    #! dest & [dest_o + 1] = toChar (oct >> 8)
    #! dest & [dest_o]     = toChar (oct >> 16)
    = dest

  srcSize = size s
  destSize
    | srcSize == 0            = 0
    #! d = srcSize * 3 / 4
    | s.[srcSize - 2] == '=' = d - 2
    | s.[srcSize - 1] == '=' = d - 1
    | otherwise              = d

removeLineBreaks :: !{#Char} -> {#Char}
removeLineBreaks src
//	= {char \\ char <-: src | char <> '\n'}
	#! n_line_breaks = count_line_breaks 0 0 src
	| n_line_breaks==0
		= src
		#! s = createArray (size src-n_line_breaks) '\0';
		= copy_without_line_breaks 0 0 src s
where
	copy_without_line_breaks :: !Int !Int !{#Char} !*{#Char} -> *{#Char}
	copy_without_line_breaks s_i d_i s d
		| s_i<size s
			| s.[s_i]<>'\n'
				#! d & [d_i] = s.[s_i]
				= copy_without_line_breaks (s_i + 1) (d_i + 1) s d
				= copy_without_line_breaks (s_i + 1) d_i s d
			= d
	
	count_line_breaks :: !Int !Int !{#Char} -> Int
	count_line_breaks i n_line_breaks s
		| i<size s
			| s.[i]<>'\n'
				= count_line_breaks (i + 1) n_line_breaks s
				= count_line_breaks (i + 1) (n_line_breaks + 1) s
			= n_line_breaks

