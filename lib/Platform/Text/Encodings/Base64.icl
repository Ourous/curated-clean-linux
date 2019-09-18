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
    | otherwise = abort "error in encodeString\n"
    where
    encodeLastOctet :: !Int !Offset !Padding !*{#Char} -> *{#Char}
    encodeLastOctet oct off p s
      | off < 0   = s
      | p > 0     = encodeLastOctet (oct >> 6) (off - 1) (p - 1) {s & [off + dest_o] = '='}
      | otherwise = encodeLastOctet (oct >> 6) (off - 1) p       {s & [off + dest_o] = a.[oct bitand 63]}
  srcSize = size s

addLineBreaks :: !u:String !Length -> u:String
addLineBreaks s l
	| l <= 0
		= abort "Length cannot be 0 or less."
	# sz = size s
	| sz <= l
		= s
	# required = case sz rem l of
		0 -> (sz/l) * (l+1) - 1
		r -> (sz/l) * (l+1) + r
	= copy s 0 l (createArray required '\0') 0
where
	copy :: !.String !Int !Int !*String !Int -> .String
	copy src src_o remaining dest dest_o
	| src_o >= size src
		= dest
	| remaining == 0
		= copy src src_o     l             {dest & [dest_o]='\n'}        (dest_o+1)
		= copy src (src_o+1) (remaining-1) {dest & [dest_o]=src.[src_o]} (dest_o+1)

base64Decode :: !.String -> .String
base64Decode s = decodeString s decodeWithStdAlphabet

base64URLDecode :: !.String -> .String
base64URLDecode s = decodeString s decodeWithUrlAlphabet

decodeString :: !.String !Alphabet -> .String
decodeString s a
	#! (sz,s) = usize s
	#! (destSize,s) = decodedSize s
	#! destString = createArray destSize '\0'
	= decodeString` s sz a destString 0 0

base64DecodeUnique :: !*String -> .String
base64DecodeUnique s = decodeUnique s decodeWithStdAlphabet

base64URLDecodeUnique :: !*String -> .String
base64URLDecodeUnique s = decodeUnique s decodeWithUrlAlphabet

decodeUnique :: !*String !Alphabet -> .String
decodeUnique s a
	#! (destSize,s) = decodedSize s
	#! (src,dest) = duplicate s
	#! dest = decodeString` src (size src) a dest 0 0
	= setLength destSize dest
where
	duplicate :: !.String -> (!String, !.String)
	duplicate s = code {
		push_a 0
	}

	// This function destructively updates the length of the string. Because
	// the decoded value is always shorter than the original value, this can be
	// done safely (i.e., we don't have to worry about corrupting the elements
	// on the heap after this string). The leftover bytes will be ignored by
	// the garbage collector (it is comparable to the case where a thunk is
	// overwritten by a smaller head normal form).
	// Note that we cannot use `pushI -2; update INT 0 1` because this will
	// cause a runtime error when checking indexes (clm's -ci) is enabled.
	setLength :: !Int !.String -> .String
	setLength len s = code {
		fill_r _STRING_ 0 1 0 0 0
		pop_b 1
	}

decodedSize :: !u:String -> (!Int, !u:String)
decodedSize s
	#! (srcSize,s) = usize s
	| srcSize == 0 = (0,s)
	#! (nnl,s) = countNewlines (srcSize-1) 0 s
	#! (neq,s) = countEqualSigns (srcSize-1) 0 s
	= ((srcSize-nnl)*3/4-neq,s)
where
	countNewlines :: !Int !Int !u:String -> (!Int, !u:String)
	countNewlines -1 n s = (n,s)
	countNewlines i n s
		# (c,s) = s![i]
		= case c of
			'\n' -> countNewlines (i-1) (n+1) s
			_    -> countNewlines (i-1) n     s

	countEqualSigns :: !Int !Int !u:String -> (!Int, !u:String)
	countEqualSigns -1 n s = (n,s)
	countEqualSigns i n s
		# (c,s) = s![i]
		= case c of
			'='  -> countEqualSigns (i-1) (n+1) s
			'\n' -> countEqualSigns (i-1) n s
			_    -> (n,s)

decodeString` :: !.String !Int !Alphabet !*{#Char} !Int !Int -> *{#Char}
decodeString` s sz a dest src_o dest_o
	#! (c1,src_o,s) = nextChar s src_o sz
	#! (c2,src_o,s) = nextChar s src_o sz
	#! (c3,src_o,s) = nextChar s src_o sz
	#! (c4,src_o,s) = nextChar s src_o sz
	| c4 == '\0'
		| c1 == '\0'
			= dest
			= abort "invalid base64 input: not a multiple of 4\n"
	| c3 == '=' // lose the last four padding bits
		# oct =
			(toInt a.[toInt c1] << 2) +
			(toInt a.[toInt c2] >> 4)
		= {dest & [dest_o]=toChar oct}
	| c4 == '=' // lose the last two obsolete bits
		# oct =
			(toInt a.[toInt c1] << 10) +
			(toInt a.[toInt c2] << 4) +
			(toInt a.[toInt c3] >> 2)
		= {dest & [dest_o]=toChar (oct >> 8), [dest_o+1]=toChar oct}
	| otherwise
		# oct =
			(toInt a.[toInt c1] << 18) +
			(toInt a.[toInt c2] << 12) +
			(toInt a.[toInt c3] << 6) +
			(toInt a.[toInt c4])
		# dest = {dest & [dest_o]=toChar (oct >> 16), [dest_o+1]=toChar (oct >> 8), [dest_o+2]=toChar oct}
		= decodeString` s sz a dest src_o (dest_o+3)
where
	nextChar :: !u:String !Int !Int -> (!Char, !Int, !u:String)
	nextChar s i size
		| i >= size = ('\0', i, s)
		# (c,s) = s![i]
		| c == '\n' = nextChar s (i+1) size
		| otherwise = (c, i+1, s)
